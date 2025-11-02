# utils_join.R
# Deterministic key joining with conflict detection
# ============================================================================

#' Standardize keys for joining
#' @param df Data frame
#' @param barcode_col Name of barcode column
#' @param labid_col Name of lab ID column
#' @return Data frame with standardized barcode and lab_id columns
#' @export
standardize_keys <- function(df, barcode_col = "barcode", labid_col = "lab_id") {
  if (is.null(df) || !nrow(df)) return(tibble::tibble())
  
  df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(
      barcode = dplyr::coalesce(
        dplyr::if_else(!!rlang::sym(barcode_col) == "", NA_character_, !!rlang::sym(barcode_col)),
        dplyr::if_else(!!rlang::sym(labid_col) == "", NA_character_, !!rlang::sym(labid_col))
      ),
      lab_id = dplyr::if_else(!!rlang::sym(labid_col) == "", NA_character_, !!rlang::sym(labid_col))
    ) %>%
    dplyr::select(barcode, lab_id, dplyr::everything())
}

#' Join with conflict detection and reporting
#' @param left Left data frame
#' @param right Right data frame (biobank keys)
#' @param by Join columns
#' @param label Descriptive label for the join operation
#' @return List with joined data and conflict report
#' @export
safe_join <- function(left, right, by = c("barcode", "lab_id"), label = "join") {
  if (is.null(left) || !nrow(left)) {
    return(list(
      data = tibble::tibble(),
      conflicts = tibble::tibble(),
      stats = list(
        n_left = 0,
        n_right = 0,
        n_matched = 0,
        match_rate = 0
      )
    ))
  }
  
  if (is.null(right) || !nrow(right)) {
    return(list(
      data = left,
      conflicts = tibble::tibble(),
      stats = list(
        n_left = nrow(left),
        n_right = 0,
        n_matched = 0,
        match_rate = 0
      )
    ))
  }
  
  # Ensure keys exist
  for (col in by) {
    if (!col %in% names(left)) left[[col]] <- NA_character_
    if (!col %in% names(right)) right[[col]] <- NA_character_
  }
  
  # Standardize keys
  left <- left %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(by), as.character))
  
  right <- right %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(by), as.character))
  
  # Check for many-to-many conflicts
  left_keys <- left %>%
    dplyr::count(dplyr::across(dplyr::all_of(by)), name = "n_left")
  
  right_keys <- right %>%
    dplyr::count(dplyr::across(dplyr::all_of(by)), name = "n_right")
  
  conflicts <- left_keys %>%
    dplyr::inner_join(right_keys, by = by) %>%
    dplyr::filter(n_left > 1 | n_right > 1) %>%
    dplyr::mutate(
      join_label = label,
      conflict_type = dplyr::case_when(
        n_left > 1 & n_right > 1 ~ "many-to-many",
        n_left > 1 ~ "many-to-one",
        n_right > 1 ~ "one-to-many",
        TRUE ~ "unknown"
      )
    )
  
  # Perform join
  joined <- left %>%
    dplyr::left_join(right, by = by, relationship = "many-to-many")
  
  # Calculate stats
  n_matched <- joined %>%
    dplyr::filter(!is.na(barcode) | !is.na(lab_id)) %>%
    nrow()
  
  stats <- list(
    n_left = nrow(left),
    n_right = nrow(right),
    n_matched = n_matched,
    match_rate = round(n_matched / nrow(left) * 100, 1)
  )
  
  list(
    data = joined,
    conflicts = conflicts,
    stats = stats
  )
}

#' Create biobank key reference
#' @param biobank_clean Cleaned biobank data frame
#' @return Distinct barcode-lab_id pairs
#' @export
extract_biobank_keys <- function(biobank_clean) {
  if (is.null(biobank_clean) || !nrow(biobank_clean)) {
    return(tibble::tibble(barcode = character(), lab_id = character()))
  }
  
  biobank_clean %>%
    dplyr::select(barcode, lab_id) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(barcode) | !is.na(lab_id))
}

#' Join extraction data to biobank
#' @param extractions Extraction data
#' @param biobank Biobank data
#' @return Joined data frame
#' @export
join_extractions_to_biobank <- function(extractions, biobank) {
  if (is.null(extractions) || !nrow(extractions)) return(tibble::tibble())
  
  # Ensure expected keys exist
  if (!"code_barres_kps" %in% names(extractions)) {
    extractions$code_barres_kps <- NA_character_
  }
  if (!"numero" %in% names(extractions)) {
    extractions$numero <- NA_character_
  }
  
  # If no biobank data, return extractions as-is
  if (is.null(biobank) || !nrow(biobank)) {
    return(extractions)
  }
  
  extractions %>%
    dplyr::mutate(
      numero = as.character(numero),
      code_barres_kps = as.character(code_barres_kps)
    ) %>%
    dplyr::left_join(
      biobank %>%
        dplyr::select(barcode, lab_id, zone, province, date_sample, 
                      date_received, date_result, study),
      by = c("code_barres_kps" = "barcode", "numero" = "lab_id"),
      relationship = "many-to-one"
    )
}
