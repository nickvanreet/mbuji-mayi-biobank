# helpers_lab_merge.R
# =====================================================================
# FIXED VERSION - Proper concordance + controls extraction
# VERSION 4.0 - 2025-11-03
# =====================================================================

#' Merge lab data with biobank (FIXED VERSION V4)
#' @param biobank Biobank data frame
#' @param lab List containing pcr, elisa_pe, elisa_vsg, ielisa
#' @param thresholds List with detailed thresholds for each marker
#' @return Merged data frame with canonical calls
#' @export
merge_lab_with_biobank_v2 <- function(biobank, lab, thresholds = NULL) {
  if (is.null(biobank) || !nrow(biobank)) {
    warning("Cannot merge: biobank is empty")
    return(tibble::tibble())
  }
  
  # Default thresholds with detailed markers
  if (is.null(thresholds)) {
    thresholds <- list(
      pcr_cq_177t = 38,
      pcr_cq_18s2 = 38,
      pcr_cq_rnasep = 38,
      elisa_pe_pp = 50,
      elisa_vsg_pp = 50,
      ielisa_inh_13 = 35,
      ielisa_inh_15 = 35
    )
  }
  
  # Source canonical call helpers
  if (!exists("canonicalise_call")) {
    source("R/helpers_lab_results2.R", local = TRUE)
  }
  
  # Start with biobank keys
  result <- biobank %>%
    dplyr::select(barcode, lab_id) %>%
    dplyr::distinct()
  
  message("Biobank keys: ", nrow(result), " samples")
  
  # === PCR ===
  if (!is.null(lab$pcr) && nrow(lab$pcr) > 0) {
    message("Processing PCR data: ", nrow(lab$pcr), " rows")
    
    # Ensure columns exist
    if (!"PCR_call" %in% names(lab$pcr)) {
      lab$pcr$PCR_call <- NA_character_
    }
    
    pcr_processed <- lab$pcr %>%
      dplyr::mutate(
        # Canonicalize PCR calls
        PCR_call = canonicalise_call(PCR_call),
        # Positive if: call is "pos" OR any Cq below respective threshold
        PCR_is_pos = PCR_call == "pos" | 
          (!is.na(Cq_177T) & Cq_177T <= thresholds$pcr_cq_177t) |
          (!is.na(Cq_18S2) & Cq_18S2 <= thresholds$pcr_cq_18s2),
        # Mark as tested if we have ANY Cq value
        PCR_tested = !is.na(Cq_177T) | !is.na(Cq_18S2) | !is.na(Cq_RNAseP) | !is.na(PCR_call)
      ) %>%
      # Deduplicate: keep latest/best per lab_id
      dplyr::arrange(lab_id, dplyr::desc(!is.na(Cq_177T))) %>%
      dplyr::distinct(lab_id, .keep_all = TRUE)
    
    message("  After dedup: ", nrow(pcr_processed), " rows")
    
    result <- result %>%
      dplyr::left_join(
        pcr_processed %>% 
          dplyr::select(-dplyr::any_of("source_file")),
        by = "lab_id",
        relationship = "many-to-one"
      )
    
    message("  Matched: ", sum(result$PCR_tested %||% FALSE), " samples")
  } else {
    message("No PCR data available")
    result$PCR_call <- NA_character_
    result$PCR_is_pos <- FALSE
    result$PCR_tested <- FALSE
  }
  
  # === ELISA PE ===
  if (!is.null(lab$elisa_pe) && nrow(lab$elisa_pe) > 0) {
    message("Processing ELISA PE data: ", nrow(lab$elisa_pe), " rows")
    
    elisa_pe_processed <- lab$elisa_pe %>%
      dplyr::mutate(
        pp_percent = suppressWarnings(as.numeric(pp_percent)),
        pp_percent = dplyr::if_else(
          !is.na(pp_percent) & pp_percent >= 0 & pp_percent <= 100,
          pp_percent,
          NA_real_
        ),
        elisa_pe_call = dplyr::case_when(
          !is.na(result_pp50) ~ canonicalise_call(result_pp50),
          !is.na(pp_percent) & pp_percent >= thresholds$elisa_pe_pp ~ "pos",
          !is.na(pp_percent) & pp_percent < thresholds$elisa_pe_pp ~ "neg",
          TRUE ~ NA_character_
        ),
        elisa_pe_is_pos = elisa_pe_call == "pos",
        elisa_pe_tested = !is.na(pp_percent) | !is.na(result_pp50)
      ) %>%
      dplyr::arrange(lab_id, barcode, dplyr::desc(!is.na(pp_percent))) %>%
      dplyr::distinct(lab_id, barcode, .keep_all = TRUE)
    
    message("  After dedup: ", nrow(elisa_pe_processed), " rows")
    
    result <- result %>%
      dplyr::left_join(
        elisa_pe_processed %>%
          dplyr::select(
            lab_id, barcode,
            d_od_pe = d_od,
            pp_percent_pe = pp_percent,
            elisa_pe_call,
            elisa_pe_is_pos,
            elisa_pe_tested
          ),
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
    
    message("  Matched: ", sum(result$elisa_pe_tested %||% FALSE), " samples")
  } else {
    message("No ELISA PE data available")
    result$elisa_pe_call <- NA_character_
    result$elisa_pe_is_pos <- FALSE
    result$elisa_pe_tested <- FALSE
  }
  
  # === ELISA VSG ===
  if (!is.null(lab$elisa_vsg) && nrow(lab$elisa_vsg) > 0) {
    message("Processing ELISA VSG data: ", nrow(lab$elisa_vsg), " rows")
    
    elisa_vsg_processed <- lab$elisa_vsg %>%
      dplyr::mutate(
        pp_percent = suppressWarnings(as.numeric(pp_percent)),
        pp_percent = dplyr::if_else(
          !is.na(pp_percent) & pp_percent >= 0 & pp_percent <= 100,
          pp_percent,
          NA_real_
        ),
        elisa_vsg_call = dplyr::case_when(
          !is.na(result_pp50) ~ canonicalise_call(result_pp50),
          !is.na(pp_percent) & pp_percent >= thresholds$elisa_vsg_pp ~ "pos",
          !is.na(pp_percent) & pp_percent < thresholds$elisa_vsg_pp ~ "neg",
          TRUE ~ NA_character_
        ),
        elisa_vsg_is_pos = elisa_vsg_call == "pos",
        elisa_vsg_tested = !is.na(pp_percent) | !is.na(result_pp50)
      ) %>%
      dplyr::arrange(lab_id, barcode, dplyr::desc(!is.na(pp_percent))) %>%
      dplyr::distinct(lab_id, barcode, .keep_all = TRUE)
    
    message("  After dedup: ", nrow(elisa_vsg_processed), " rows")
    
    result <- result %>%
      dplyr::left_join(
        elisa_vsg_processed %>%
          dplyr::select(
            lab_id, barcode,
            d_od_vsg = d_od,
            pp_percent_vsg = pp_percent,
            elisa_vsg_call,
            elisa_vsg_is_pos,
            elisa_vsg_tested
          ),
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
    
    message("  Matched: ", sum(result$elisa_vsg_tested %||% FALSE), " samples")
  } else {
    message("No ELISA VSG data available")
    result$elisa_vsg_call <- NA_character_
    result$elisa_vsg_is_pos <- FALSE
    result$elisa_vsg_tested <- FALSE
  }
  
  # === iELISA ===
  if (!is.null(lab$ielisa) && nrow(lab$ielisa) > 0) {
    message("Processing iELISA data: ", nrow(lab$ielisa), " rows")
    
    # Rename columns if needed
    if ("LabID" %in% names(lab$ielisa)) {
      lab$ielisa <- lab$ielisa %>% dplyr::rename(lab_id = LabID)
    }
    if ("Barcode" %in% names(lab$ielisa)) {
      lab$ielisa <- lab$ielisa %>% dplyr::rename(barcode = Barcode)
    }
    
    ielisa_processed <- lab$ielisa %>%
      dplyr::mutate(
        pct_inh_13 = suppressWarnings(as.numeric(pct_inh_13)),
        pct_inh_15 = suppressWarnings(as.numeric(pct_inh_15)),
        ielisa_call = dplyr::case_when(
          !is.na(res_final) ~ canonicalise_call(res_final),
          !is.na(res_13) & canonicalise_call(res_13) == "pos" ~ "pos",
          !is.na(res_15) & canonicalise_call(res_15) == "pos" ~ "pos",
          !is.na(pct_inh_13) & pct_inh_13 >= thresholds$ielisa_inh_13 ~ "pos",
          !is.na(pct_inh_15) & pct_inh_15 >= thresholds$ielisa_inh_15 ~ "pos",
          !is.na(pct_inh_13) | !is.na(pct_inh_15) ~ "neg",
          TRUE ~ NA_character_
        ),
        ielisa_is_pos = ielisa_call == "pos",
        ielisa_tested = !is.na(pct_inh_13) | !is.na(pct_inh_15) | !is.na(res_final)
      ) %>%
      dplyr::group_by(lab_id, barcode) %>%
      dplyr::summarise(
        pct_inh_13 = max(pct_inh_13, na.rm = TRUE),
        pct_inh_15 = max(pct_inh_15, na.rm = TRUE),
        ielisa_call = dplyr::first(ielisa_call[!is.na(ielisa_call)]),
        ielisa_is_pos = any(ielisa_is_pos, na.rm = TRUE),
        ielisa_tested = any(ielisa_tested, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        pct_inh_13 = dplyr::if_else(is.infinite(pct_inh_13), NA_real_, pct_inh_13),
        pct_inh_15 = dplyr::if_else(is.infinite(pct_inh_15), NA_real_, pct_inh_15)
      )
    
    message("  After aggregation: ", nrow(ielisa_processed), " rows")
    
    result <- result %>%
      dplyr::left_join(
        ielisa_processed,
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
    
    message("  Matched: ", sum(result$ielisa_tested %||% FALSE), " samples")
  } else {
    message("No iELISA data available")
    result$ielisa_call <- NA_character_
    result$ielisa_is_pos <- FALSE
    result$ielisa_tested <- FALSE
  }
  
  # === Add overall positivity flags ===
  result <- result %>%
    dplyr::mutate(
      Any_pos = (PCR_is_pos %||% FALSE) | 
                (elisa_pe_is_pos %||% FALSE) | 
                (elisa_vsg_is_pos %||% FALSE) | 
                (ielisa_is_pos %||% FALSE),
      
      Tests_done = sum(
        PCR_tested %||% FALSE,
        elisa_pe_tested %||% FALSE,
        elisa_vsg_tested %||% FALSE,
        ielisa_tested %||% FALSE
      ),
      
      Tests_pos = sum(
        PCR_is_pos %||% FALSE,
        elisa_pe_is_pos %||% FALSE,
        elisa_vsg_is_pos %||% FALSE,
        ielisa_is_pos %||% FALSE
      )
    )
  
  # === CRITICAL FIX: Apply concordance classification ONCE ===
  if (exists("classify_concordance")) {
    message("Applying concordance classification...")
    result <- classify_concordance(result)  # ✅ THIS is the final classification
  } else {
    # Fallback if classify_concordance not available
    tryCatch({
      source("R/helpers_concordance.R", local = TRUE)
      result <- classify_concordance(result)
    }, error = function(e) {
      warning("Could not load concordance classifier: ", e$message)
      # Simple fallback
      result <<- result %>%
        dplyr::mutate(
          concordance_class = dplyr::case_when(
            Tests_done == 0 ~ "No data",
            Tests_pos == 0 ~ "Negative concordant",
            Tests_pos == Tests_done ~ "True concordant",
            TRUE ~ "Discordant"
          )
        )
    })
  }
  
  message("Final merged data: ", nrow(result), " rows")
  message("  Any positive: ", sum(result$Any_pos %||% FALSE, na.rm = TRUE))
  
  result
}

#' Extract all controls from lab data
#' @param lab List containing pcr, elisa_pe, elisa_vsg, ielisa
#' @param config App configuration (for paths)
#' @return List with pcr, elisa, ielisa controls
#' @export
extract_all_controls <- function(lab, config = NULL) {
  
  message("Extracting controls from lab data...")
  
  controls_list <- list(
    pcr = tibble::tibble(),
    elisa = tibble::tibble(),
    ielisa = tibble::tibble()
  )
  
  # Source control helpers if needed
  if (!exists("extract_pcr_controls")) {
    tryCatch({
      source("R/helpers_controls.R", local = TRUE)
    }, error = function(e) {
      warning("Could not load helpers_controls.R: ", e$message)
      return(controls_list)
    })
  }
  
  # PCR controls
  if (!is.null(lab$pcr) && nrow(lab$pcr) > 0) {
    tryCatch({
      controls_list$pcr <- extract_pcr_controls(lab$pcr)
      message("  PCR controls: ", nrow(controls_list$pcr), " rows")
    }, error = function(e) {
      warning("Failed to extract PCR controls: ", e$message)
    })
  }
  
  # ELISA controls (from directories)
  if (!is.null(config) && "paths" %in% names(config)) {
    tryCatch({
      controls_list$elisa <- extract_elisa_controls(
        elisa_pe_data = lab$elisa_pe,
        elisa_vsg_data = lab$elisa_vsg,
        pe_dir = config$paths$elisa_pe_dir,
        vsg_dir = config$paths$elisa_vsg_dir
      )
      message("  ELISA controls: ", nrow(controls_list$elisa), " rows")
    }, error = function(e) {
      warning("Failed to extract ELISA controls: ", e$message)
    })
  }
  
  # iELISA controls
  if (!is.null(config) && "paths" %in% names(config)) {
    tryCatch({
      controls_list$ielisa <- extract_ielisa_controls(
        ielisa_dir = config$paths$ielisa_dir
      )
      message("  iELISA controls: ", nrow(controls_list$ielisa), " rows")
    }, error = function(e) {
      warning("Failed to extract iELISA controls: ", e$message)
    })
  }
  
  controls_list
}

#' Safe null coalesce operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
