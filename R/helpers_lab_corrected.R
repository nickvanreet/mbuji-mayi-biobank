# helpers_lab_corrected.R
# =====================================================================
# CORRECTED: PCR and iELISA parsing based on working original code
# =====================================================================

#' Parse PCR results from directory (CORRECTED VERSION)
#' @param dir Directory containing PCR Excel files
#' @return Tibble with PCR results
#' @export
parse_pcr_results <- function(dir) {
  dir <- safe_path(dir)
  if (is.null(dir) || !dir.exists(dir)) {
    warning("PCR directory not found: ", dir)
    return(tibble::tibble())
  }
  
  files <- list.files(dir, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) {
    warning("No PCR files found in: ", dir)
    return(tibble::tibble())
  }
  
  # Helper to summarize target (with mean Cq and SD)
  summarise_target_long <- function(path, sheet_name, tag) {
    tryCatch({
      readxl::read_excel(path, sheet = sheet_name) %>%
        janitor::clean_names() %>%
        dplyr::transmute(
          sample = as.character(sample),
          Cq = suppressWarnings(as.numeric(cq))
        ) %>%
        dplyr::mutate(Cq = dplyr::if_else(Cq < 0, NA_real_, Cq)) %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(
          !!rlang::sym(paste0("Cq_", tag)) := mean(Cq, na.rm = TRUE),
          !!rlang::sym(paste0("SD_", tag)) := sd(Cq, na.rm = TRUE),
          .groups = "drop"
        )
    }, error = function(e) {
      warning("Failed to read sheet ", sheet_name, " from ", basename(path))
      tibble::tibble(sample = character())
    })
  }
  
  # Process one PCR file
  make_qpcr_summary <- function(path) {
    tryCatch({
      # Read calls from Replicates sheet
      call_df <- readxl::read_excel(path, sheet = "Replicates") %>%
        janitor::clean_names() %>%
        dplyr::transmute(
          sample = as.character(sample),
          PCR_call = as.character(final)
        )
      
      # Read target sheets
      s177 <- summarise_target_long(path, "177T", "177T")
      s18  <- summarise_target_long(path, "18S2", "18S2")
      srna <- summarise_target_long(path, "RNAseP", "RNAseP")
      
      # Join all together
    result <- call_df %>%
      dplyr::left_join(s177, by = "sample") %>%
      dplyr::left_join(s18, by = "sample") %>%
      dplyr::left_join(srna, by = "sample") %>%
      dplyr::rename(lab_id = sample) %>%
      dplyr::mutate(
        source_file = basename(path),
        # ADD: Extract run date from filename
        run_date = extract_file_date(basename(path)),
        run_id = extract_run_letter(basename(path)),
        # Convert Cq values
        across(starts_with("Cq_"), ~ suppressWarnings(as.numeric(.))),
        across(starts_with("SD_"), ~ suppressWarnings(as.numeric(.)))
      )
    
    message("  Extracted ", nrow(result), " samples from ", basename(path), 
            " (date: ", run_date[1], ")")
    
    result
  }, error = function(e) {
    warning("Failed to process PCR file ", basename(path), ": ", e$message)
    tibble::tibble()
  })
}
  
  # Process all files
  pcr_data <- purrr::map_dfr(files, make_qpcr_summary)
  
  if (nrow(pcr_data) == 0) {
    warning("No PCR data extracted from files")
    return(tibble::tibble())
  }
  
  # Join with biobank to get barcodes (done in calling function)
  pcr_data
}

#' Parse iELISA results (CORRECTED VERSION)
#' @param dir Directory containing iELISA files
#' @return Tibble with iELISA results
#' @export
parse_ielisa <- function(dir) {
  dir <- safe_path(dir)
  if (is.null(dir) || !dir.exists(dir)) {
    warning("iELISA directory not found: ", dir)
    return(tibble::tibble())
  }
  
  files <- list.files(dir, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) {
    warning("No iELISA files found in: ", dir)
    return(tibble::tibble())
  }
  
  # Helper to get first available column
  first_present_chr <- function(.data, ...) {
    opts <- c(...)
    col <- opts[opts %in% names(.data)][1]
    if (is.na(col)) {
      rep(NA_character_, nrow(.data))
    } else {
      as.character(.data[[col]])
    }
  }
  
  # Process one iELISA file
  read_ielisa_results <- function(path, assay_label = "iELISA") {
    tryCatch({
      # Find the results sheet (handles accents)
      sheets <- readxl::excel_sheets(path)
      sheet <- sheets[stringi::stri_cmp_equiv(
        stringi::stri_trans_general(sheets, "Latin-ASCII"), 
        "Resultats"
      )]
      if (length(sheet) == 0) sheet <- sheets[1]
      
      # Probe for header row (look for "Numéro labo")
      probe <- readxl::read_excel(
        path, 
        sheet = sheet, 
        range = "A1:C40", 
        col_names = FALSE, 
        .name_repair = "minimal"
      )
      
      header_row <- which(
        grepl("num[eé]ro.*labo", probe[[2]], ignore.case = TRUE)
      )[1] - 1
      
      if (is.na(header_row)) header_row <- 0
      
      # Read the actual data
      raw <- readxl::read_excel(
        path, 
        sheet = sheet, 
        skip = header_row, 
        .name_repair = "minimal"
      ) %>%
        janitor::clean_names()
      
      # Find percentage inhibition columns
      pct_cols <- grep(
        "^pourcentage.*inhibition|^pct.*inh", 
        names(raw), 
        ignore.case = TRUE, 
        value = TRUE
      )
      
      # Find result columns
      res_cols <- grep(
        "^resultat($|_\\d+)", 
        names(raw), 
        ignore.case = TRUE, 
        value = TRUE
      )
      
      # Ensure we have at least 2 slots (for LiTat 1.3 and 1.5)
      pct_cols <- c(pct_cols, NA, NA)[1:2]
      res_cols <- c(res_cols, NA, NA)[1:2]
      
      # Build result
      tibble::tibble(
        file = basename(path),
        assay_label = assay_label,
        Barcode = first_present_chr(
          raw, 
          "code_barres_kps", 
          "code_echantillon_code_barres", 
          "barcode", 
          "code_barres"
        ),
        LabID = first_present_chr(
          raw, 
          "numero_labo", 
          "sample_id"
        ),
        pct_inh_13 = if (!is.na(pct_cols[1])) {
          suppressWarnings(as.numeric(raw[[pct_cols[1]]]))
        } else {
          NA_real_
        },
        res_13 = if (!is.na(res_cols[1])) {
          as.character(raw[[res_cols[1]]])
        } else {
          NA_character_
        },
        pct_inh_15 = if (!is.na(pct_cols[2])) {
          suppressWarnings(as.numeric(raw[[pct_cols[2]]]))
        } else {
          NA_real_
        },
        res_15 = if (!is.na(res_cols[2])) {
          as.character(raw[[res_cols[2]]])
        } else {
          NA_character_
        },
        res_final = first_present_chr(
          raw, 
          "resultat_final", 
          "x_2"
        )
      ) %>%
        dplyr::filter(!is.na(Barcode) | !is.na(LabID))
      
    }, error = function(e) {
      warning("Failed to process iELISA file ", basename(path), ": ", e$message)
      tibble::tibble()
    })
  }
  
  # Process all files
  purrr::map_dfr(files, read_ielisa_results, assay_label = "iELISA")
}

#' Parse ELISA PE results (UNCHANGED - working)
#' @param dir Directory containing ELISA PE files
#' @return Tibble with ELISA PE results
#' @export
parse_elisa_pe <- function(dir) {
  parse_elisa_generic(dir, "ELISA_PE")
}

#' Parse ELISA VSG results (UNCHANGED - working)
#' @param dir Directory containing ELISA VSG files
#' @return Tibble with ELISA VSG results
#' @export
parse_elisa_vsg <- function(dir) {
  parse_elisa_generic(dir, "ELISA_VSG")
}

#' Generic ELISA parser (UNCHANGED - working)
#' @param dir Directory containing ELISA files
#' @param assay_label Label for this assay
#' @return Tibble with ELISA results
parse_elisa_generic <- function(dir, assay_label) {
  dir <- safe_path(dir)
  if (is.null(dir) || !dir.exists(dir)) {
    warning(assay_label, " directory not found: ", dir)
    return(tibble::tibble())
  }
  
  files <- list.files(dir, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) {
    warning("No ", assay_label, " files found in: ", dir)
    return(tibble::tibble())
  }
  
  # Process one ELISA file
  process_elisa_file <- function(path) {
    tryCatch({
      sheets <- readxl::excel_sheets(path)
      result_sheet <- sheets[grepl("result", sheets, ignore.case = TRUE)]
      if (!length(result_sheet)) result_sheet <- sheets[1]
      
      raw <- readxl::read_excel(path, sheet = result_sheet, 
                                .name_repair = "minimal") %>%
        janitor::clean_names()
      
      # Helper to get first available column
      first_col <- function(.data, ...) {
        opts <- c(...)
        for (opt in opts) {
          if (opt %in% names(.data)) return(.data[[opt]])
        }
        rep(NA, nrow(.data))
      }
      
      tibble::tibble(
        lab_id = as.character(first_col(
          raw, 
          "numero_labo_dipumba", 
          "numero_labo", 
          "lab_id", 
          "sample"
        )),
        barcode = as.character(first_col(
          raw, 
          "code_barres_kps", 
          "code_echantillon_code_barres", 
          "barcode"
        )),
        d_od = suppressWarnings(as.numeric(first_col(
          raw, 
          "d_od", 
          "delta_od", 
          "od"
        ))),
        pp_percent = suppressWarnings(as.numeric(first_col(
          raw, 
          "pp", 
          "pp_percent", 
          "percentage"
        ))),
        result_pp50 = as.character(first_col(
          raw, 
          "result_pp50", 
          "resultat_pp50"
        )),
        result_pp60 = as.character(first_col(
          raw, 
          "result_pp60", 
          "resultat_pp60"
        )),
        assay_label = assay_label,
        source_file = basename(path)
      ) %>%
        dplyr::filter(!is.na(lab_id) | !is.na(barcode))
    }, error = function(e) {
      warning("Failed to process ", assay_label, " file ", 
              basename(path), ": ", e$message)
      tibble::tibble()
    })
  }
  
  purrr::map_dfr(files, process_elisa_file)
}

#' Merge lab data with biobank (UNCHANGED - working)
#' @param biobank Biobank data frame
#' @param lab List containing pcr, elisa_pe, elisa_vsg, ielisa
#' @return Merged data frame
#' @export
merge_lab_with_biobank <- function(biobank, lab) {
  if (is.null(biobank) || !nrow(biobank)) {
    warning("Cannot merge: biobank is empty")
    return(tibble::tibble())
  }
  
  # Start with biobank keys
  result <- biobank %>%
    dplyr::select(barcode, lab_id) %>%
    dplyr::distinct()
  
  # Join PCR
  if (!is.null(lab$pcr) && nrow(lab$pcr) > 0) {
    result <- result %>%
      dplyr::left_join(
        lab$pcr %>% dplyr::select(-source_file),
        by = "lab_id",
        relationship = "many-to-one"
      )
  }
  
  # Join ELISA PE
  if (!is.null(lab$elisa_pe) && nrow(lab$elisa_pe) > 0) {
    result <- result %>%
      dplyr::left_join(
        lab$elisa_pe %>%
          dplyr::select(
            lab_id, barcode, 
            d_od_pe = d_od, 
            pp_percent_pe = pp_percent, 
            result_pp50_pe = result_pp50,
            result_pp60_pe = result_pp60
          ),
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
  }
  
  # Join ELISA VSG
  if (!is.null(lab$elisa_vsg) && nrow(lab$elisa_vsg) > 0) {
    result <- result %>%
      dplyr::left_join(
        lab$elisa_vsg %>%
          dplyr::select(
            lab_id, barcode, 
            d_od_vsg = d_od, 
            pp_percent_vsg = pp_percent,
            result_pp50_vsg = result_pp50,
            result_pp60_vsg = result_pp60
          ),
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
  }
  
  # Join iELISA (aggregate first if multiple results per sample)
  if (!is.null(lab$ielisa) && nrow(lab$ielisa) > 0) {
    # Rename columns to match expected names
    ielisa_renamed <- lab$ielisa %>%
      dplyr::rename(
        lab_id = LabID,
        barcode = Barcode
      )
    
    ielisa_agg <- ielisa_renamed %>%
      dplyr::group_by(lab_id, barcode) %>%
      dplyr::summarise(
        pct_inh_13 = mean(pct_inh_13, na.rm = TRUE),
        pct_inh_15 = mean(pct_inh_15, na.rm = TRUE),
        res_13 = dplyr::first(stats::na.omit(res_13)),
        res_15 = dplyr::first(stats::na.omit(res_15)),
        res_final = dplyr::first(stats::na.omit(res_final)),
        .groups = "drop"
      )
    
    result <- result %>%
      dplyr::left_join(
        ielisa_agg,
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
  }
  
  result
}

#' Parse PCR controls (stub - implement if needed)
#' @param dir PCR directory
#' @return Tibble with control results
#' @export
parse_pcr_controls <- function(dir) {
  tibble::tibble(
    File = character(),
    Control_Type = character(),
    Cq_Value = numeric(),
    Status = character()
  )
}

#' Parse ELISA PE controls (stub - implement if needed)
#' @param dir ELISA PE directory
#' @return Tibble with control results
#' @export
parse_elisa_pe_controls <- function(dir) {
  tibble::tibble(
    File = character(),
    Control_Type = character(),
    OD_Value = numeric(),
    Status = character()
  )
}

#' Parse ELISA VSG controls (stub - implement if needed)
#' @param dir ELISA VSG directory
#' @return Tibble with control results
#' @export
parse_elisa_vsg_controls <- function(dir) {
  tibble::tibble(
    File = character(),
    Control_Type = character(),
    OD_Value = numeric(),
    Status = character()
  )
}

#' Parse iELISA controls (stub - implement if needed)
#' @param dir iELISA directory
#' @return Tibble with control results
#' @export
parse_ielisa_controls <- function(dir) {
  tibble::tibble(
    File = character(),
    Control_Type = character(),
    Inhibition = numeric(),
    Status = character()
  )
}
