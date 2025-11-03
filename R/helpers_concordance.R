# helpers_concordance.R
# =====================================================================
# Concordance classification with proper hierarchy
# =====================================================================

#' Classify concordance status with proper hierarchy
#' @param df Data frame with test results and _is_pos/_tested flags
#' @return Data frame with concordance_class and concordance_subtype
#' @export
#' Classify concordance with CORRECTED logic
#' @param df Data frame with test results and flags
#' @return Data frame with concordance_class
#' @export
classify_concordance <- function(df) {
  
  message("Classifying concordance for ", nrow(df), " samples")
  
  df %>%
    dplyr::mutate(
      # Determine which tests were run
      pcr_run = PCR_tested %||% FALSE,
      vsg_run = elisa_vsg_tested %||% FALSE,
      pe_run = elisa_pe_tested %||% FALSE,
      ielisa_run = ielisa_tested %||% FALSE,
      
      # Determine positivity
      pcr_pos = PCR_is_pos %||% FALSE,
      vsg_pos = elisa_vsg_is_pos %||% FALSE,
      pe_pos = elisa_pe_is_pos %||% FALSE,
      ielisa_pos = ielisa_is_pos %||% FALSE,
      
      # Any ELISA positive
      any_elisa_pos = vsg_pos | pe_pos | ielisa_pos,
      
      # Count tests
      n_tests_run = sum(pcr_run, vsg_run, pe_run, ielisa_run),
      n_tests_pos = sum(pcr_pos, vsg_pos, pe_pos, ielisa_pos),
      
      # CONCORDANCE CLASSIFICATION (hierarchical)
      concordance_class = dplyr::case_when(
        # 0. No tests run
        n_tests_run == 0 ~ "No data",
        
        # 1. All negative (all tests negative)
        n_tests_run > 0 & n_tests_pos == 0 ~ "Negative concordant",
        
        # 2. All positive (all tests that were run are positive)
        n_tests_run > 0 & n_tests_pos == n_tests_run ~ "All positive concordant",
        
        # 3. True concordant (PCR+ AND any ELISA+, but not all tests positive)
        pcr_pos & any_elisa_pos & n_tests_pos < n_tests_run ~ "True concordant (PCR+/ELISA+)",
        
        # 4. Serological concordant (iELISA+ AND VSG+ AND PE+, but PCR-)
        ielisa_pos & vsg_pos & pe_pos & !pcr_pos ~ "Serological concordant",
        
        # 5. Indirect ELISA concordant (VSG+ AND PE+, but iELISA- and PCR-)
        vsg_pos & pe_pos & !ielisa_pos & !pcr_pos ~ "Indirect ELISA concordant",
        
        # 6. Everything else is discordant
        TRUE ~ "Discordant"
      ),
      
      # Subtype for discordant
      concordance_subtype = dplyr::case_when(
        concordance_class != "Discordant" ~ NA_character_,
        
        # Single test positive
        n_tests_pos == 1 & pcr_pos ~ "PCR-only positive",
        n_tests_pos == 1 & vsg_pos ~ "VSG-only positive",
        n_tests_pos == 1 & pe_pos ~ "PE-only positive",
        n_tests_pos == 1 & ielisa_pos ~ "iELISA-only positive",
        
        # PE/VSG disagree
        pe_run & vsg_run & (pe_pos != vsg_pos) ~ "PE≠VSG",
        
        # Other patterns
        pcr_pos & !any_elisa_pos ~ "PCR+ but ELISA-",
        !pcr_pos & any_elisa_pos ~ "ELISA+ but PCR-",
        
        TRUE ~ "Other pattern"
      ),
      
      # Combined label
      concordance_label = dplyr::if_else(
        is.na(concordance_subtype),
        concordance_class,
        paste0(concordance_class, " (", concordance_subtype, ")")
      ),
      
      # Simple TRUE/FALSE concordant flag for calculations
      Concordant = concordance_class %in% c(
        "Negative concordant",
        "All positive concordant",
        "True concordant (PCR+/ELISA+)",
        "Serological concordant",
        "Indirect ELISA concordant"
      )
    ) %>%
    # Remove temporary columns
    dplyr::select(-pcr_run, -vsg_run, -pe_run, -ielisa_run,
                  -pcr_pos, -vsg_pos, -pe_pos, -ielisa_pos,
                  -any_elisa_pos, -n_tests_run, -n_tests_pos)
}

#' Get concordance summary statistics
#' @param df Data frame with concordance classifications
#' @return Summary tibble
#' @export
summarize_concordance <- function(df) {
  df %>%
    dplyr::count(concordance_class, concordance_subtype) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(
      percent = round(n / sum(n) * 100, 1),
      label = dplyr::if_else(
        is.na(concordance_subtype),
        concordance_class,
        paste0(concordance_class, " - ", concordance_subtype)
      )
    )
}

#' Create concordance visualization
#' @param df Data frame with concordance data
#' @return ggplot object
#' @export
plot_concordance <- function(df) {
  summary <- summarize_concordance(df)
  
  # Color palette
  colors <- c(
    "True concordant" = "#27AE60",
    "Serological concordant" = "#3498DB",
    "Indirect ELISA concordant" = "#9B59B6",
    "Negative concordant" = "#95A5A6",
    "Discordant" = "#E74C3C",
    "No data" = "#ECF0F1"
  )
  
  ggplot2::ggplot(summary, ggplot2::aes(x = "", y = n, fill = concordance_class)) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 1) +
    ggplot2::coord_polar("y") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = "Sample Concordance Classification",
      fill = "Category"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "right")
}

# Safe NULL coalesce
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
