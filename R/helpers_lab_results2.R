# helpers_lab_results2.R
# =====================================================================
# Canonical lab call handling + control parsing
# =====================================================================

#' Canonicalise lab result calls to standard format
#' @param x Vector of lab result calls
#' @return Standardized calls: "pos", "neg", "suspect", "repeat", or NA
canonicalise_call <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  dplyr::case_when(
    x_chr %in% c("p", "pos", "positive", "detected", "detect", "true", "yes") ~ "pos",
    x_chr %in% c("n", "neg", "negative", "not detected", "nd", "false", "no") ~ "neg",
    x_chr %in% c("s", "sus", "suspect", "suspected") ~ "suspect",
    x_chr %in% c("r", "rep", "repeat", "redo") ~ "repeat",
    TRUE ~ NA_character_
  )
}

#' Add canonical call and boolean positive flag to data frame
#' @param df Data frame
#' @param call_col Name of column containing calls
#' @param out_flag Name of output boolean column (default "is_pos")
#' @return Data frame with canonicalized call and boolean flag
add_is_pos <- function(df, call_col, out_flag = "is_pos") {
  if (!call_col %in% names(df)) {
    # Add dummy columns if column doesn't exist
    df[[call_col]] <- NA_character_
    df[[out_flag]] <- FALSE
    return(df)
  }
  
  df |>
    dplyr::mutate(
      "{call_col}" := canonicalise_call(.data[[call_col]]),
      "{out_flag}" := .data[[call_col]] == "pos"
    )
}

#' Parse and extract controls from lab data
#' @param labs Lab data frame
#' @return Controls data frame
extract_controls <- function(labs) {
  if (is.null(labs) || !nrow(labs)) {
    return(tibble::tibble(
      assay = character(),
      plate_id = character(),
      file_id = character(),
      run_dt = as.POSIXct(character()),
      control_type = character(),
      od = numeric(),
      cq = numeric()
    ))
  }
  
  # Try to identify control rows
  labs |>
    dplyr::mutate(
      # Detect control type from various possible columns
      control_type = dplyr::case_when(
        # Check lab_id column
        grepl("^pc$|^pc\\d|positive.*control", 
              tolower(lab_id %||% ""), perl = TRUE) ~ "PC",
        grepl("^nc$|^cn$|^cn\\d|negative.*control|no.*template", 
              tolower(lab_id %||% ""), perl = TRUE) ~ "NC",
        grepl("^cc$|conjugate.*control", 
              tolower(lab_id %||% ""), perl = TRUE) ~ "CC",
        
        # Check barcode column
        grepl("^pc$|^pc\\d|positive.*control", 
              tolower(barcode %||% ""), perl = TRUE) ~ "PC",
        grepl("^nc$|^cn$|^cn\\d|negative.*control", 
              tolower(barcode %||% ""), perl = TRUE) ~ "NC",
        grepl("^cc$|conjugate.*control", 
              tolower(barcode %||% ""), perl = TRUE) ~ "CC",
        
        # Check sample_type if it exists
        !is.null(sample_type) & grepl("^pc$|positive", 
                                       tolower(sample_type %||% ""), perl = TRUE) ~ "PC",
        !is.null(sample_type) & grepl("^nc$|negative", 
                                       tolower(sample_type %||% ""), perl = TRUE) ~ "NC",
        !is.null(sample_type) & grepl("^cc$|conjugate", 
                                       tolower(sample_type %||% ""), perl = TRUE) ~ "CC",
        
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(control_type)) |>
    dplyr::transmute(
      assay = dplyr::coalesce(assay_label, "Unknown"),
      plate_id = as.character(dplyr::coalesce(plate_id, plate, file_id, source_file)),
      file_id = as.character(source_file),
      run_dt = lubridate::ymd_hms(
        dplyr::coalesce(run_datetime, result_datetime, plate_datetime, NA_character_)
      ),
      control_type,
      # Extract OD values (for ELISA)
      od = suppressWarnings(as.numeric(dplyr::coalesce(
        d_od, delta_od, od_value, OD, NA_character_
      ))),
      # Extract Cq values (for PCR)
      cq = suppressWarnings(as.numeric(dplyr::coalesce(
        Cq_177T, Cq_18S2, cq_value, Cq, NA_character_
      )))
    )
}

#' Calculate QC bounds for controls
#' @param controls Controls data frame
#' @return QC bounds with mean and SD
calculate_qc_bounds <- function(controls) {
  if (is.null(controls) || !nrow(controls)) {
    return(tibble::tibble(
      assay = character(),
      control_type = character(),
      metric = numeric(),
      sdv = numeric()
    ))
  }
  
  controls |>
    dplyr::group_by(assay, control_type) |>
    dplyr::summarise(
      # Use Cq for PCR, OD for ELISA
      metric = ifelse(
        any(grepl("PCR", assay, ignore.case = TRUE)),
        mean(cq, na.rm = TRUE),
        mean(od, na.rm = TRUE)
      ),
      sdv = ifelse(
        any(grepl("PCR", assay, ignore.case = TRUE)),
        sd(cq, na.rm = TRUE),
        sd(od, na.rm = TRUE)
      ),
      n = dplyr::n(),
      .groups = "drop"
    )
}

#' Plot Levey-Jennings control chart
#' @param df_assay Controls data for one assay/control combination
#' @return ggplot object
plot_controls_lj <- function(df_assay) {
  if (is.null(df_assay) || !nrow(df_assay)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  
  # Determine metric column
  is_pcr <- any(grepl("PCR", df_assay$assay, ignore.case = TRUE))
  y_col <- if (is_pcr) "cq" else "od"
  
  # Get values
  m <- df_assay[[y_col]]
  m <- m[!is.na(m)]
  
  if (length(m) < 2) {
    return(ggplot2::ggplot() + 
             ggplot2::theme_void() +
             ggplot2::labs(title = "Insufficient data for QC plot"))
  }
  
  mu <- mean(m, na.rm = TRUE)
  s <- sd(m, na.rm = TRUE)
  
  ggplot2::ggplot(df_assay, ggplot2::aes(x = run_dt, y = .data[[y_col]])) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = mu, linetype = 1, color = "blue") +
    ggplot2::geom_hline(yintercept = mu + s, linetype = 2, color = "orange") +
    ggplot2::geom_hline(yintercept = mu - s, linetype = 2, color = "orange") +
    ggplot2::geom_hline(yintercept = mu + 2*s, linetype = 3, color = "red") +
    ggplot2::geom_hline(yintercept = mu - 2*s, linetype = 3, color = "red") +
    ggplot2::labs(
      x = NULL, 
      y = toupper(y_col), 
      title = paste(unique(df_assay$assay), unique(df_assay$control_type), "Control Chart"),
      subtitle = sprintf("Mean = %.2f | SD = %.2f", mu, s)
    ) +
    ggplot2::theme_minimal()
}

#' Safe coalesce that handles NULL
#' @param ... Values to coalesce
#' @return First non-NULL, non-NA value
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
