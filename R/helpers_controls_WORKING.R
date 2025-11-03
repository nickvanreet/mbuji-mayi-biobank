# helpers_controls_WORKING.R
# Simplified, working control extraction
# =============================================================================

#' Extract PCR controls from PCR data
#' Simple, working version
extract_pcr_controls_simple <- function(pcr_data) {
  if (is.null(pcr_data) || !nrow(pcr_data)) {
    return(tibble::tibble())
  }
  
  # Ensure lab_id column exists
  if (!"lab_id" %in% names(pcr_data)) {
    warning("PCR data missing lab_id column")
    return(tibble::tibble())
  }
  
  message("Extracting PCR controls from ", nrow(pcr_data), " rows")
  
  # Identify controls by lab_id pattern
  controls <- pcr_data %>%
    dplyr::mutate(
      lab_id_upper = toupper(trimws(as.character(lab_id))),
      control_type = dplyr::case_when(
        grepl("^(CP|PC|POS)", lab_id_upper) ~ "CP",
        grepl("^(CN|NC|NEG|NTC)", lab_id_upper) ~ "CN",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(control_type))
  
  message("Found ", nrow(controls), " PCR controls")
  
  if (!nrow(controls)) return(tibble::tibble())
  
  # Add run date from source file
  controls <- controls %>%
    dplyr::mutate(
      run_date = extract_file_date(source_file %||% ""),
      run_id = extract_run_letter(source_file %||% "")
    )
  
  controls
}

#' Extract ELISA controls from Excel files
extract_elisa_controls_simple <- function(elisa_dir, assay_label = "ELISA") {
  if (is.null(elisa_dir) || !dir.exists(elisa_dir)) {
    return(tibble::tibble())
  }
  
  files <- list.files(elisa_dir, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) return(tibble::tibble())
  
  message("Reading ELISA controls from ", length(files), " files")
  
  # Read controls from each file
  all_controls <- purrr::map_dfr(files, function(f) {
    tryCatch({
      sheets <- readxl::excel_sheets(f)
      ctrl_sheet <- sheets[grepl("control", sheets, ignore.case = TRUE)]
      
      if (!length(ctrl_sheet)) {
        message("  No Controls sheet in ", basename(f))
        return(tibble::tibble())
      }
      
      # Read the controls sheet
      raw <- readxl::read_excel(f, sheet = ctrl_sheet[1]) %>%
        janitor::clean_names()
      
      # Identify control rows
      raw %>%
        dplyr::mutate(
          sample_upper = toupper(trimws(as.character(sample %||% sample_code %||% ""))),
          control_type = dplyr::case_when(
            grepl("^(PC|POS)", sample_upper) ~ "PC",
            grepl("^(NC|NEG)", sample_upper) ~ "NC",
            grepl("^(CC|CONJ)", sample_upper) ~ "CC",
            TRUE ~ NA_character_
          )
        ) %>%
        dplyr::filter(!is.na(control_type)) %>%
        dplyr::transmute(
          file = basename(f),
          run_date = extract_file_date(basename(f)),
          assay_label = assay_label,
          control_type,
          plate = suppressWarnings(as.integer(plate %||% elisa %||% 1)),
          d_od = parse_num_smart(d_od %||% delta_od %||% od),
          pp_percent = parse_num_smart(pp %||% pp_percent)
        )
    }, error = function(e) {
      warning("Failed to read ", basename(f), ": ", e$message)
      tibble::tibble()
    })
  })
  
  message("Found ", nrow(all_controls), " ELISA controls")
  all_controls
}

#' Extract iELISA controls from 450-600nm summary sheets
extract_ielisa_controls_simple <- function(ielisa_dir) {
  if (is.null(ielisa_dir) || !dir.exists(ielisa_dir)) {
    return(tibble::tibble())
  }
  
  files <- list.files(ielisa_dir, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) return(tibble::tibble())
  
  message("Reading iELISA controls from ", length(files), " files")
  
  # Read from each file
  all_controls <- purrr::map_dfr(files, function(f) {
    tryCatch({
      sheets <- readxl::excel_sheets(f)
      
      # Find 450-600 sheet
      sheet_450 <- sheets[grepl("450.*600|450-600", sheets, ignore.case = TRUE)]
      if (!length(sheet_450)) {
        message("  No 450-600nm sheet in ", basename(f))
        return(tibble::tibble())
      }
      
      # Read as matrix for flexible parsing
      raw <- readxl::read_excel(f, sheet = sheet_450[1], col_names = FALSE)
      M <- as.matrix(raw)
      
      # Find control values by text matching
      find_value <- function(pattern) {
        matches <- grep(pattern, M, ignore.case = TRUE)
        if (!length(matches)) return(NA_real_)
        
        # Get first match
        idx <- arrayInd(matches[1], dim(M))
        row <- idx[1, 1]
        col <- idx[1, 2]
        
        # Look for number below this cell
        for (r in (row+1):min(nrow(M), row+10)) {
          val <- suppressWarnings(as.numeric(M[r, col]))
          if (!is.na(val) && is.finite(val)) return(val)
        }
        
        NA_real_
      }
      
      tibble::tibble(
        file = basename(f),
        run_date = extract_file_date(basename(f)),
        assay_label = "iELISA",
        # LiTat 1.3
        L13_neg_mean = find_value("moyenne.*neg.*1\\.3|mean.*neg.*1\\.3"),
        L13_pos_mean = find_value("moyenne.*pos.*1\\.3|mean.*pos.*1\\.3"),
        L13_PI_CP = find_value("pi.*cp.*1\\.3|pi.*litat.*1\\.3"),
        # LiTat 1.5
        L15_neg_mean = find_value("moyenne.*neg.*1\\.5|mean.*neg.*1\\.5"),
        L15_pos_mean = find_value("moyenne.*pos.*1\\.5|mean.*pos.*1\\.5"),
        L15_PI_CP = find_value("pi.*cp.*1\\.5|pi.*litat.*1\\.5")
      )
    }, error = function(e) {
      warning("Failed to read ", basename(f), ": ", e$message)
      tibble::tibble()
    })
  })
  
  message("Found ", nrow(all_controls), " iELISA runs with control data")
  all_controls
}

#' Parse number smartly (handles European format)
parse_num_smart <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric())
  
  x_chr <- trimws(as.character(x))
  x_chr <- gsub("%", "", x_chr, fixed = TRUE)
  
  # Try standard parse
  val <- suppressWarnings(as.numeric(x_chr))
  
  # Fix European format if needed
  need_fix <- is.na(val) & nzchar(x_chr)
  if (any(need_fix)) {
    x_eur <- gsub("\\.", "", x_chr[need_fix])  # Remove thousands separator
    x_eur <- gsub(",", ".", x_eur)  # Decimal comma to dot
    val[need_fix] <- suppressWarnings(as.numeric(x_eur))
  }
  
  val
}

#' Extract date from filename
extract_file_date <- function(filename) {
  m <- regexpr("^(\\d{6})", filename)
  if (m[1] == -1) return(as.Date(NA))
  
  date_str <- regmatches(filename, m)
  if (length(date_str) == 0 || nchar(date_str) != 6) return(as.Date(NA))
  
  tryCatch({
    as.Date(date_str, format = "%y%m%d")
  }, error = function(e) as.Date(NA))
}

#' Extract run letter from filename (a, b, c for same-day repeats)
extract_run_letter <- function(filename) {
  m <- regexpr("^\\d{6}([a-z])", filename, ignore.case = TRUE)
  if (m[1] == -1) return(NA_character_)
  
  full_match <- regmatches(filename, m)
  sub("^\\d{6}", "", full_match)
}

#' Safe null coalesce
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
