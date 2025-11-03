# helpers_lab_merge.R
# =====================================================================
# Corrected lab merge with canonical calls and proper deduplication
# =====================================================================

#' Merge lab data with biobank (CORRECTED VERSION V3)
#' @param biobank Biobank data frame
#' @param lab List containing pcr, elisa_pe, elisa_vsg, ielisa
#' @param thresholds List with pcr_cq_max, elisa_pe_pp, elisa_vsg_pp, ielisa_inh
#' @return Merged data frame with canonical calls
#' @export
merge_lab_with_biobank_v2 <- function(biobank, lab, thresholds = NULL) {
  if (is.null(biobank) || !nrow(biobank)) {
    warning("Cannot merge: biobank is empty")
    return(tibble::tibble())
  }
  
  # Default thresholds
  if (is.null(thresholds)) {
    thresholds <- list(
      pcr_cq_max = 38,
      elisa_pe_pp = 50,
      elisa_vsg_pp = 50,
      ielisa_inh = 35
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
        # Positive if: call is "pos" OR any Cq below threshold
        PCR_is_pos = PCR_call == "pos" | 
          (!is.na(Cq_177T) & Cq_177T <= thresholds$pcr_cq_max) |
          (!is.na(Cq_18S2) & Cq_18S2 <= thresholds$pcr_cq_max),
        # Mark as tested if we have ANY Cq value
        PCR_tested = !is.na(Cq_177T) | !is.na(Cq_18S2) | !is.na(PCR_call)
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
    
    message("  Matched: ", sum(!is.na(result$PCR_call) | result$PCR_tested %||% FALSE), " samples")
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
        # pp_percent is already a percentage (0-100), not 0-1
        pp_percent = suppressWarnings(as.numeric(pp_percent)),
        # Create call from PP% or text result
        elisa_pe_call = dplyr::case_when(
          !is.na(result_pp50) ~ canonicalise_call(result_pp50),
          !is.na(pp_percent) & pp_percent >= thresholds$elisa_pe_pp ~ "pos",
          !is.na(pp_percent) ~ "neg",
          TRUE ~ NA_character_
        ),
        elisa_pe_is_pos = elisa_pe_call == "pos",
        elisa_pe_tested = !is.na(pp_percent) | !is.na(result_pp50)
      ) %>%
      # Deduplicate
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
        elisa_vsg_call = dplyr::case_when(
          !is.na(result_pp50) ~ canonicalise_call(result_pp50),
          !is.na(pp_percent) & pp_percent >= thresholds$elisa_vsg_pp ~ "pos",
          !is.na(pp_percent) ~ "neg",
          TRUE ~ NA_character_
        ),
        elisa_vsg_is_pos = elisa_vsg_call == "pos",
        elisa_vsg_tested = !is.na(pp_percent) | !is.na(result_pp50)
      ) %>%
      # Deduplicate
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
        # Create call from results or % inhibition
        ielisa_call = dplyr::case_when(
          !is.na(res_final) ~ canonicalise_call(res_final),
          !is.na(res_13) & canonicalise_call(res_13) == "pos" ~ "pos",
          !is.na(res_15) & canonicalise_call(res_15) == "pos" ~ "pos",
          !is.na(pct_inh_13) & pct_inh_13 >= thresholds$ielisa_inh ~ "pos",
          !is.na(pct_inh_15) & pct_inh_15 >= thresholds$ielisa_inh ~ "pos",
          !is.na(pct_inh_13) | !is.na(pct_inh_15) ~ "neg",
          TRUE ~ NA_character_
        ),
        ielisa_is_pos = ielisa_call == "pos",
        ielisa_tested = !is.na(pct_inh_13) | !is.na(pct_inh_15) | !is.na(res_final)
      ) %>%
      # Aggregate by barcode/lab_id (keep highest % inhibition)
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
      # Any test positive
      Any_pos = (PCR_is_pos %||% FALSE) | 
                (elisa_pe_is_pos %||% FALSE) | 
                (elisa_vsg_is_pos %||% FALSE) | 
                (ielisa_is_pos %||% FALSE),
      
      # Count tests performed
      Tests_done = sum(
        PCR_tested %||% FALSE,
        elisa_pe_tested %||% FALSE,
        elisa_vsg_tested %||% FALSE,
        ielisa_tested %||% FALSE
      ),
      
      # Count positive tests
      Tests_pos = sum(
        PCR_is_pos %||% FALSE,
        elisa_pe_is_pos %||% FALSE,
        elisa_vsg_is_pos %||% FALSE,
        ielisa_is_pos %||% FALSE
      ),
      
      # FIXED CONCORDANCE LOGIC:
      # Concordant = TRUE if all tested assays have same result
      # If only 1 test done, it's automatically concordant
      # If 0 tests done, it's NA
      Concordant = dplyr::case_when(
        Tests_done == 0 ~ NA,
        Tests_done == 1 ~ TRUE,  # Single test is always concordant
        Tests_pos == 0 ~ TRUE,    # All negative = concordant
        Tests_pos == Tests_done ~ TRUE,  # All positive = concordant
        TRUE ~ FALSE  # Mixed results = discordant
      )
    )
  
  message("Final merged data: ", nrow(result), " rows")
  message("  Any positive: ", sum(result$Any_pos %||% FALSE, na.rm = TRUE))
  
  result
}

#' Safe null coalesce operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
