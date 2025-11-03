# helpers_lab_complete.R
# =====================================================================
# HELPERS: Load & merge PCR, ELISA PE, ELISA VSG, iELISA, and controls
# =====================================================================

#' Parse PCR results from directory
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
  
  # Helper to read target sheet
  read_target_sheet <- function(path, sheet_name) {
    tryCatch({
      readxl::read_excel(path, sheet = sheet_name) %>%
        janitor::clean_names() %>%
        dplyr::transmute(
          sample = as.character(sample),
          Cq = suppressWarnings(as.numeric(cq))
        ) %>%
        dplyr::filter(!is.na(sample)) %>%
        dplyr::mutate(Cq = dplyr::if_else(Cq < 0, NA_real_, Cq))
    }, error = function(e) {
      warning("Failed to read sheet ", sheet_name, " from ", basename(path))
      tibble::tibble(sample = character(), Cq = numeric())
    })
  }
  
  # Process one PCR file
  process_pcr_file <- function(path) {
    tryCatch({
      # Read Replicates sheet for calls
      calls <- readxl::read_excel(path, sheet = "Replicates") %>%
        janitor::clean_names() %>%
        dplyr::transmute(
          lab_id = as.character(sample),
          pcr_call = as.character(dplyr::coalesce(final, call, result))
        )
      
      # Read target sheets
      t177 <- read_target_sheet(path, "177T") %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(
          Cq_177T = mean(Cq, na.rm = TRUE),
          SD_177T = sd(Cq, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::rename(lab_id = sample)
      
      t18s <- read_target_sheet(path, "18S2") %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(
          Cq_18S2 = mean(Cq, na.rm = TRUE),
          SD_18S2 = sd(Cq, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::rename(lab_id = sample)
      
      rnasep <- read_target_sheet(path, "RNAseP") %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(
          Cq_RNAseP = mean(Cq, na.rm = TRUE),
          SD_RNAseP = sd(Cq, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::rename(lab_id = sample)
      
      # Join all
      result <- calls %>%
        dplyr::left_join(t177, by = "lab_id") %>%
        dplyr::left_join(t18s, by = "lab_id") %>%
        dplyr::left_join(rnasep, by = "lab_id") %>%
        dplyr::mutate(source_file = basename(path))
      
      result
    }, error = function(e) {
      warning("Failed to process PCR file ", basename(path), ": ", e$message)
      tibble::tibble()
    })
  }
  
  # Process all files
  pcr_data <- purrr::map_dfr(files, process_pcr_file)
  
  if (nrow(pcr_data) == 0) {
    warning("No PCR data extracted from files")
    return(tibble::tibble())
  }
  
  pcr_data
}

#' Parse ELISA PE results
#' @param dir Directory containing ELISA PE files
#' @return Tibble with ELISA PE results
#' @export
parse_elisa_pe <- function(dir) {
  parse_elisa_generic(dir, "ELISA_PE")
}

#' Parse ELISA VSG results
#' @param dir Directory containing ELISA VSG files
#' @return Tibble with ELISA VSG results
#' @export
parse_elisa_vsg <- function(dir) {
  parse_elisa_generic(dir, "ELISA_VSG")
}

#' Generic ELISA parser
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
        lab_id = as.character(first_col(raw, "numero_labo_dipumba", "numero_labo", "lab_id", "sample")),
        barcode = as.character(first_col(raw, "code_barres_kps", "code_echantillon_code_barres", "barcode")),
        d_od = suppressWarnings(as.numeric(first_col(raw, "d_od", "delta_od", "od"))),
        pp_percent = suppressWarnings(as.numeric(first_col(raw, "pp", "pp_percent", "percentage"))),
        result_pp50 = as.character(first_col(raw, "result_pp50", "resultat_pp50")),
        result_pp60 = as.character(first_col(raw, "result_pp60", "resultat_pp60")),
        assay_label = assay_label,
        source_file = basename(path)
      ) %>%
        dplyr::filter(!is.na(lab_id) | !is.na(barcode))
    }, error = function(e) {
      warning("Failed to process ", assay_label, " file ", basename(path), ": ", e$message)
      tibble::tibble()
    })
  }
  
  purrr::map_dfr(files, process_elisa_file)
}

#' Parse iELISA results
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
  
  # Process one iELISA file
  process_ielisa_file <- function(path) {
    tryCatch({
      sheets <- readxl::excel_sheets(path)
      result_sheet <- sheets[grepl("résult|result", 
                                   stringi::stri_trans_general(sheets, "Latin-ASCII"), 
                                   ignore.case = TRUE)]
      if (!length(result_sheet)) result_sheet <- sheets[1]
      
      # Probe for header row
      probe <- readxl::read_excel(path, sheet = result_sheet, range = "A1:C40",
                                  col_names = FALSE, .name_repair = "minimal")
      header_row <- which(grepl("numéro.*labo", probe[[2]], ignore.case = TRUE))[1] - 1
      if (is.na(header_row)) header_row <- 0
      
      raw <- readxl::read_excel(path, sheet = result_sheet, skip = header_row,
                                .name_repair = "minimal") %>%
        janitor::clean_names()
      
      # Find percentage inhibition columns
      pct_cols <- grep("pourcentage.*inhibition|pct.*inh", names(raw), 
                       ignore.case = TRUE, value = TRUE)
      res_cols <- grep("resultat", names(raw), ignore.case = TRUE, value = TRUE)
      
      tibble::tibble(
        lab_id = as.character(dplyr::coalesce(raw$numero_labo, raw$sample_id, raw$lab_id)),
        barcode = as.character(dplyr::coalesce(raw$code_barres_kps, raw$barcode)),
        pct_inh_13 = if (length(pct_cols) >= 1) {
          suppressWarnings(as.numeric(raw[[pct_cols[1]]]))
        } else NA_real_,
        pct_inh_15 = if (length(pct_cols) >= 2) {
          suppressWarnings(as.numeric(raw[[pct_cols[2]]]))
        } else NA_real_,
        res_13 = if (length(res_cols) >= 1) {
          as.character(raw[[res_cols[1]]])
        } else NA_character_,
        res_15 = if (length(res_cols) >= 2) {
          as.character(raw[[res_cols[2]]])
        } else NA_character_,
        res_final = as.character(dplyr::coalesce(raw$resultat_final, raw$final_result)),
        source_file = basename(path)
      ) %>%
        dplyr::filter(!is.na(lab_id) | !is.na(barcode))
    }, error = function(e) {
      warning("Failed to process iELISA file ", basename(path), ": ", e$message)
      tibble::tibble()
    })
  }
  
  purrr::map_dfr(files, process_ielisa_file)
}

#' Merge lab data with biobank
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
          dplyr::select(lab_id, barcode, d_od_pe = d_od, 
                        pp_percent_pe = pp_percent, 
                        result_pp50_pe = result_pp50,
                        result_pp60_pe = result_pp60),
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
  }
  
  # Join ELISA VSG
  if (!is.null(lab$elisa_vsg) && nrow(lab$elisa_vsg) > 0) {
    result <- result %>%
      dplyr::left_join(
        lab$elisa_vsg %>%
          dplyr::select(lab_id, barcode, d_od_vsg = d_od, 
                        pp_percent_vsg = pp_percent,
                        result_pp50_vsg = result_pp50,
                        result_pp60_vsg = result_pp60),
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
  }
  
  # Join iELISA (aggregate first if multiple results per sample)
  if (!is.null(lab$ielisa) && nrow(lab$ielisa) > 0) {
    ielisa_agg <- lab$ielisa %>%
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

#' Compute positivity flags based on thresholds
#' @param df Merged data frame
#' @param pcr_cq_max Max Cq for PCR positivity (default 38)
#' @param elisa_pp_cut PP% cutoff for ELISA positivity (default 50)
#' @param ielisa_inh_cut % inhibition cutoff for iELISA positivity (default 35)
#' @return Data frame with flags
#' @export
compute_flags <- function(df, pcr_cq_max = 38, elisa_pp_cut = 50, ielisa_inh_cut = 35) {
  df %>%
    dplyr::mutate(
      # PCR positive if call says so OR if any target below threshold
      PCR_pos_call = grepl("pos|detect", tolower(pcr_call)),
      PCR_pos_cq = !is.na(Cq_177T) & Cq_177T <= pcr_cq_max | 
                   !is.na(Cq_18S2) & Cq_18S2 <= pcr_cq_max,
      PCR_pos = dplyr::coalesce(PCR_pos_call, PCR_pos_cq),
      
      # ELISA PE positive
      ELISA_PE_pos = !is.na(pp_percent_pe) & pp_percent_pe >= elisa_pp_cut,
      
      # ELISA VSG positive
      ELISA_VSG_pos = !is.na(pp_percent_vsg) & pp_percent_vsg >= elisa_pp_cut,
      
      # iELISA positive if either antigen or text result indicates positive
      iELISA_pos_numeric = !is.na(pct_inh_13) & pct_inh_13 >= ielisa_inh_cut |
                           !is.na(pct_inh_15) & pct_inh_15 >= ielisa_inh_cut,
      iELISA_pos_text = grepl("pos|positif|detect", 
                              paste(res_13, res_15, res_final), 
                              ignore.case = TRUE),
      iELISA_pos = iELISA_pos_numeric | iELISA_pos_text,
      
      # Overall concordance: any test positive
      Any_pos = PCR_pos | ELISA_PE_pos | ELISA_VSG_pos | iELISA_pos,
      
      # Full concordance: all tested methods agree
      Tested_count = sum(!is.na(PCR_pos), !is.na(ELISA_PE_pos), 
                         !is.na(ELISA_VSG_pos), !is.na(iELISA_pos)),
      Pos_count = sum(PCR_pos %in% TRUE, ELISA_PE_pos %in% TRUE,
                      ELISA_VSG_pos %in% TRUE, iELISA_pos %in% TRUE),
      
      Concordant = dplyr::case_when(
        Tested_count == 0 ~ NA,
        Pos_count == 0 ~ TRUE,  # All negative
        Pos_count == Tested_count ~ TRUE,  # All positive
        TRUE ~ FALSE  # Mixed results
      )
    ) %>%
    dplyr::select(-PCR_pos_call, -PCR_pos_cq, -iELISA_pos_numeric, 
                  -iELISA_pos_text, -Tested_count, -Pos_count)
}

#' Parse PCR controls
#' @param dir PCR directory
#' @return Tibble with control results
#' @export
parse_pcr_controls <- function(dir) {
  # Implementation would extract control rows from PCR files
  # For now, return empty tibble
  tibble::tibble(
    File = character(),
    Control_Type = character(),
    Cq_Value = numeric(),
    Status = character()
  )
}

#' Parse ELISA PE controls
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

#' Parse ELISA VSG controls
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

#' Parse iELISA controls
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
