# helpers_concordance.R
# =====================================================================
# Concordance classification with proper hierarchy
# =====================================================================

#' Classify concordance status with proper hierarchy
#' @param df Data frame with test results and _is_pos/_tested flags
#' @return Data frame with concordance_class and concordance_subtype
#' @export
classify_concordance <- function(df) {
  df %>%
    dplyr::mutate(
      # Determine which tests were actually run
      pcr_run = PCR_tested %||% FALSE,
      vsg_run = elisa_vsg_tested %||% FALSE,
      pe_run = elisa_pe_tested %||% FALSE,
      ielisa_run = ielisa_tested %||% FALSE,
      
      # Determine positivity
      pcr_pos = PCR_is_pos %||% FALSE,
      vsg_pos = elisa_vsg_is_pos %||% FALSE,
      pe_pos = elisa_pe_is_pos %||% FALSE,
      ielisa_pos = ielisa_is_pos %||% FALSE,
      
      # Any ELISA positive (VSG OR PE OR iELISA)
      any_elisa_pos = vsg_pos | pe_pos | ielisa_pos,
      
      # Count tests performed and positive
      n_tests_run = sum(pcr_run, vsg_run, pe_run, ielisa_run),
      n_tests_pos = sum(pcr_pos, vsg_pos, pe_pos, ielisa_pos),
      
      # Concordance classification (hierarchical)
      concordance_class = dplyr::case_when(
        # 0. No tests run
        n_tests_run == 0 ~ "No data",
        
        # 1. True concordant: PCR+ AND any ELISA+
        pcr_pos & any_elisa_pos ~ "True concordant",
        
        # 2. Serological concordant: iELISA+ AND (VSG+ AND PE+) AND PCR-/absent
        ielisa_pos & vsg_pos & pe_pos & !pcr_pos ~ "Serological concordant",
        
        # 3. Indirect ELISA concordant: (VSG+ AND PE+) AND iELISA-/absent AND PCR-/absent
        vsg_pos & pe_pos & !ielisa_pos & !pcr_pos ~ "Indirect ELISA concordant",
        
        # 4. Negative concordant: All run assays negative
        n_tests_pos == 0 & n_tests_run > 0 ~ "Negative concordant",
        
        # 5. Discordant: Everything else
        TRUE ~ "Discordant"
      ),
      
      # Subtype for discordant cases
      concordance_subtype = dplyr::case_when(
        concordance_class != "Discordant" ~ NA_character_,
        
        # Discordant subtypes
        pcr_pos & !any_elisa_pos ~ "PCR-only positive",
        !pcr_pos & n_tests_pos == 1 & vsg_pos ~ "VSG-only positive",
        !pcr_pos & n_tests_pos == 1 & pe_pos ~ "PE-only positive",
        !pcr_pos & n_tests_pos == 1 & ielisa_pos ~ "iELISA-only positive",
        
        # Serology discordant (PE ≠ VSG)
        pe_run & vsg_run & (pe_pos != vsg_pos) ~ "Serology discordant (PE≠VSG)",
        
        # iELISA vs indirect mismatch
        ielisa_pos & (!vsg_pos | !pe_pos) ~ "iELISA vs indirect mismatch",
        (vsg_pos | pe_pos) & ielisa_run & !ielisa_pos ~ "Indirect vs iELISA mismatch",
        
        # Other discordant
        TRUE ~ "Other discordant"
      ),
      
      # Combined label for display
      concordance_label = dplyr::if_else(
        is.na(concordance_subtype),
        concordance_class,
        paste0(concordance_class, " (", concordance_subtype, ")")
      )
    )
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
