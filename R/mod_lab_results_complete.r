# mod_lab_results.R
# Lab results module (PCR, ELISA PE/VSG, iELISA)
# =============================================================================

#' Lab Results Module - UI
#' @param id Module namespace ID
#' @export
mod_lab_results_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = c(4, 8),
    
    # Loader panel
    bslib::card(
      bslib::card_header("Load Lab Results"),
      
      shiny::textInput(ns("pcr_dir"), "PCR results folder", value = ""),
      shiny::textInput(ns("elisa_pe_dir"), "ELISA PE folder", value = ""),
      shiny::textInput(ns("elisa_vsg_dir"), "ELISA VSG folder", value = ""),
      shiny::textInput(ns("ielisa_dir"), "iELISA folder", value = ""),
      
      shiny::actionButton(ns("lab_load"), "Load Lab Results", 
                          class = "btn-primary w-100 mb-3"),
      
      shiny::div(class = "mt-2", shiny::textOutput(ns("lab_status")))
    ),
    
    # Results panel
    bslib::card(
      bslib::card_header("Lab Results"),
      
      bslib::navset_tab(
        bslib::nav_panel(
          "Overview",
          
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,
            
            bslib::value_box(
              title = "PCR Positive",
              value = shiny::textOutput(ns("vb_pcr_pos")),
              showcase = bsicons::bs_icon("virus"),
              theme = "danger"
            ),
            bslib::value_box(
              title = "iELISA Positive",
              value = shiny::textOutput(ns("vb_ielisa_pos")),
              showcase = bsicons::bs_icon("clipboard2-check"),
              theme = "info"
            ),
            bslib::value_box(
              title = "Concordant Results",
              value = shiny::textOutput(ns("vb_concordant")),
              showcase = bsicons::bs_icon("check-circle"),
              theme = "success"
            )
          ),
          
          bslib::layout_columns(
            col_widths = c(6, 6),
            
            bslib::card(
              bslib::card_header("PCR Cq Distributions"),
              shiny::plotOutput(ns("plot_pcr_cq"), height = 300)
            ),
            bslib::card(
              bslib::card_header("iELISA % Inhibition"),
              shiny::plotOutput(ns("plot_ielisa_inh"), height = 300)
            )
          )
        ),
        
        bslib::nav_panel(
          "PCR (per sample)",
          shiny::downloadButton(ns("dl_pcr"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_pcr"))
        ),
        
        bslib::nav_panel(
          "ELISA (PE/VSG)",
          shiny::downloadButton(ns("dl_elisa"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_elisa"))
        ),
        
        bslib::nav_panel(
          "iELISA (per sample)",
          shiny::downloadButton(ns("dl_ielisa"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_ielisa"))
        ),
        
        bslib::nav_panel(
          "Concordance",
          shiny::downloadButton(ns("dl_concordance"), "Download", 
                                class = "btn-sm mb-2"),
          
          bslib::layout_columns(
            col_widths = c(6, 6),
            
            bslib::card(
              bslib::card_header("Concordance Summary"),
              shiny::tableOutput(ns("tbl_concordance_summary"))
            ),
            bslib::card(
              bslib::card_header("Positivity Rates with 95% CI"),
              shiny::tableOutput(ns("tbl_positivity_rates"))
            )
          ),
          
          DT::DTOutput(ns("tbl_concordance"))
        )
      )
    )
  )
}

# helper to normalise directory paths using global safe_path when available
lab_normalise_path <- function(path) {
  if (is.null(path) || !length(path)) return(path)
  if (length(path) > 1) path <- paste(path, collapse = "")
  if (is.na(path)) return(NA_character_)
  path <- trimws(path)
  if (!nzchar(path)) return("")

  if (exists("safe_path", mode = "function")) {
    out <- tryCatch(safe_path(path), error = function(e) path)
    if (is.null(out) || !nzchar(out)) return(path)
    return(out)
  }
  path
}

lab_dir_label <- function(path) {
  if (is.null(path) || !length(path) || is.na(path)) return("not set")
  path <- trimws(path)
  if (!nzchar(path)) return("not set")
  path
}

#' Lab Results Module - Server
#' @param id Module namespace ID
#' @param biobank_clean Reactive containing cleaned biobank data
#' @param config Reactive containing app configuration
#' @export
mod_lab_results_server <- function(id, biobank_clean, config) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    lab_pcr <- shiny::reactiveVal(tibble::tibble())
    lab_elisa <- shiny::reactiveVal(tibble::tibble())
    lab_ielisa <- shiny::reactiveVal(tibble::tibble())

    # Initialize paths from config
    shiny::observe({
      cfg <- config()
      if (!is.null(cfg) && "paths" %in% names(cfg)) {
        shiny::updateTextInput(session, "pcr_dir", 
                               value = cfg$paths$pcr_dir)
        shiny::updateTextInput(session, "elisa_pe_dir", 
                               value = cfg$paths$elisa_pe_dir)
        shiny::updateTextInput(session, "elisa_vsg_dir", 
                               value = cfg$paths$elisa_vsg_dir)
        shiny::updateTextInput(session, "ielisa_dir", 
                               value = cfg$paths$ielisa_dir)
      }
    })
    
    # Load lab results
    shiny::observeEvent(input$lab_load, {
      shiny::showNotification("Loading lab results...", 
                              type = "message", duration = 2)
      
      bio <- biobank_clean()
      if (is.null(bio) || !nrow(bio)) {
        shiny::showNotification("Load biobank data first.", type = "error")
        output$lab_status <- shiny::renderText("Biobank not loaded.")
        return()
      }
      
      # Normalise directories from inputs/config
      dir_pcr <- lab_normalise_path(input$pcr_dir)
      dir_elisa_pe <- lab_normalise_path(input$elisa_pe_dir)
      dir_elisa_vsg <- lab_normalise_path(input$elisa_vsg_dir)
      dir_ielisa <- lab_normalise_path(input$ielisa_dir)

      # Extract keys for joining
      keys <- bio %>%
        dplyr::select(barcode, lab_id) %>%
        dplyr::distinct()

      missing_dirs <- character()

      # Read PCR
      has_dir <- function(path) {
        is.character(path) && length(path) == 1 && !is.na(path) && nzchar(path)
      }
      dir_is_valid <- function(path) {
        has_dir(path) && isTRUE(dir.exists(path))
      }

      pcr <- tibble::tibble()
      if (!dir_is_valid(dir_pcr)) {
        missing_dirs <- c(missing_dirs, sprintf("PCR (%s)", lab_dir_label(dir_pcr)))
      } else {
        pcr_files <- list.files(dir_pcr, pattern = "\\.xlsx?$", full.names = TRUE)
        if (!length(pcr_files)) {
          missing_dirs <- c(missing_dirs, sprintf("PCR (no Excel files in %s)", dir_pcr))
        }
        pcr <- read_pcr_dir(dir_pcr, biobank_df = keys)
      }
      lab_pcr(pcr)

      # Read ELISA
      elisa <- tibble::tibble()
      if (!has_dir(dir_elisa_pe) && !has_dir(dir_elisa_vsg)) {
        missing_dirs <- c(missing_dirs, "ELISA (no folder set)")
      } else {
        elisa <- read_elisa_dirs(dir_elisa_pe, dir_elisa_vsg)
        if (!nrow(elisa) && !dir_is_valid(dir_elisa_pe) && !dir_is_valid(dir_elisa_vsg)) {
          missing_dirs <- c(missing_dirs, sprintf("ELISA (%s | %s)",
                                                 lab_dir_label(dir_elisa_pe),
                                                 lab_dir_label(dir_elisa_vsg)))
        } else if (!nrow(elisa)) {
          missing <- character()
          if (dir_is_valid(dir_elisa_pe) && !length(list.files(dir_elisa_pe, pattern = "\\.xlsx?$"))) {
            missing <- c(missing, sprintf("PE (%s)", dir_elisa_pe))
          }
          if (dir_is_valid(dir_elisa_vsg) && !length(list.files(dir_elisa_vsg, pattern = "\\.xlsx?$"))) {
            missing <- c(missing, sprintf("VSG (%s)", dir_elisa_vsg))
          }
          if (length(missing)) {
            missing_dirs <- c(missing_dirs, paste("ELISA no Excel files:", paste(missing, collapse = "; ")))
          }
        }
      }
      # Map barcodes via lab_id if missing
      if (nrow(elisa) > 0) {
        elisa <- elisa %>%
          dplyr::left_join(keys, by = "lab_id",
                           suffix = c("_orig", "_mapped")) %>%
          dplyr::mutate(
            barcode = dplyr::coalesce(barcode_orig, barcode_mapped)
          ) %>%
          dplyr::select(-barcode_orig, -barcode_mapped)
      }
      lab_elisa(elisa)
      
      # Read iELISA
      iel <- tibble::tibble()
      if (!dir_is_valid(dir_ielisa)) {
        missing_dirs <- c(missing_dirs, sprintf("iELISA (%s)", lab_dir_label(dir_ielisa)))
      } else {
        if (!length(list.files(dir_ielisa, pattern = "\\.xlsx?$"))) {
          missing_dirs <- c(missing_dirs, sprintf("iELISA (no Excel files in %s)", dir_ielisa))
        }
        iel <- read_ielisa_dir(dir_ielisa)
      }
      lab_ielisa(iel)

      status_parts <- c(
        sprintf("PCR: %s", scales::comma(nrow(pcr))),
        sprintf("ELISA: %s", scales::comma(nrow(elisa))),
        sprintf("iELISA: %s", scales::comma(nrow(iel)))
      )

      if (length(missing_dirs)) {
        status_parts <- c(status_parts,
                          paste("⚠️ Missing/empty:", paste(missing_dirs, collapse = "; ")))
      }

      output$lab_status <- shiny::renderText(paste(status_parts, collapse = " | "))

      if ((nrow(pcr) + nrow(elisa) + nrow(iel)) > 0) {
        shiny::showNotification("Lab results loaded successfully.",
                                type = "message")
      } else {
        shiny::showNotification("Lab result folders were processed but no records were found.",
                                type = "warning")
      }
    })
    
    # Joined lab results (per barcode/lab_id)
    lab_joined <- shiny::reactive({
      bio <- biobank_clean()
      if (is.null(bio) || !nrow(bio)) return(tibble::tibble())
      
      keys <- bio %>% 
        dplyr::select(barcode, lab_id) %>% 
        dplyr::distinct()
      
      pcr <- lab_pcr()
      elisa <- lab_elisa()
      iel <- lab_ielisa()
      
      # Start with keys
      joined <- keys
      
      # Join PCR
      if (nrow(pcr) > 0) {
        joined <- joined %>%
          dplyr::left_join(
            pcr %>% dplyr::select(barcode, lab_id, pcr_call, is_pcr_pos,
                                  Cq_177T, Cq_18S2, Cq_RNAseP),
            by = c("barcode", "lab_id")
          )
      } else {
        joined <- joined %>%
          dplyr::mutate(pcr_call = NA_character_, is_pcr_pos = NA,
                        Cq_177T = NA_real_, Cq_18S2 = NA_real_, 
                        Cq_RNAseP = NA_real_)
      }
      
      # Join ELISA (aggregate by barcode/lab_id)
      if (nrow(elisa) > 0) {
        elisa_agg <- elisa %>%
          dplyr::group_by(barcode, lab_id, assay_label) %>%
          dplyr::summarise(
            n_plates = dplyr::n(),
            mean_pp = mean(pp_percent, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          tidyr::pivot_wider(
            names_from = assay_label,
            values_from = c(n_plates, mean_pp),
            names_glue = "{assay_label}_{.value}"
          )
        
        joined <- joined %>%
          dplyr::left_join(elisa_agg, by = c("barcode", "lab_id"))
      }
      
      # Join iELISA (aggregate by barcode/lab_id)
      if (nrow(iel) > 0) {
        iel_agg <- iel %>%
          dplyr::group_by(barcode, lab_id) %>%
          dplyr::summarise(
            n_ielisa = dplyr::n(),
            any_pos = any(is_positive_result(res_final) | 
                            is_positive_result(res_13) | 
                            is_positive_result(res_15), na.rm = TRUE),
            mean_inh13 = mean(pct_inh_13, na.rm = TRUE),
            mean_inh15 = mean(pct_inh_15, na.rm = TRUE),
            .groups = "drop"
          )
        
        joined <- joined %>%
          dplyr::left_join(iel_agg, by = c("barcode", "lab_id"))
      } else {
        joined <- joined %>%
          dplyr::mutate(n_ielisa = 0L, any_pos = NA,
                        mean_inh13 = NA_real_, mean_inh15 = NA_real_)
      }
      
      joined
    })
    
    # Compute summary statistics
    lab_summary <- shiny::reactive({
      joined <- lab_joined()
      if (is.null(joined) || !nrow(joined)) {
        return(list(
          pcr_tested = 0, pcr_pos = 0, pcr_neg = 0,
          iel_tested = 0, iel_pos = 0, iel_neg = 0,
          concordant = 0, discordant = 0
        ))
      }
      
      pcr_tested <- sum(!is.na(joined$is_pcr_pos))
      pcr_pos <- sum(joined$is_pcr_pos %in% TRUE, na.rm = TRUE)
      pcr_neg <- sum(joined$is_pcr_pos %in% FALSE, na.rm = TRUE)
      
      iel_tested <- sum(!is.na(joined$any_pos))
      iel_pos <- sum(joined$any_pos %in% TRUE, na.rm = TRUE)
      iel_neg <- sum(joined$any_pos %in% FALSE, na.rm = TRUE)
      
      # Concordance
      both_tested <- joined %>%
        dplyr::filter(!is.na(is_pcr_pos) & !is.na(any_pos))
      
      concordant <- sum(
        (both_tested$is_pcr_pos & both_tested$any_pos) |
          (!both_tested$is_pcr_pos & !both_tested$any_pos)
      )
      
      discordant <- nrow(both_tested) - concordant
      
      list(
        pcr_tested = pcr_tested,
        pcr_pos = pcr_pos,
        pcr_neg = pcr_neg,
        iel_tested = iel_tested,
        iel_pos = iel_pos,
        iel_neg = iel_neg,
        concordant = concordant,
        discordant = discordant,
        both_tested = nrow(both_tested)
      )
    })
    
    # Value boxes
    output$vb_pcr_pos <- shiny::renderText({
      s <- lab_summary()
      sprintf("%s / %s (%.1f%%)",
              scales::comma(s$pcr_pos),
              scales::comma(s$pcr_tested),
              if (s$pcr_tested > 0) s$pcr_pos / s$pcr_tested * 100 else 0)
    })
    
    output$vb_ielisa_pos <- shiny::renderText({
      s <- lab_summary()
      sprintf("%s / %s (%.1f%%)",
              scales::comma(s$iel_pos),
              scales::comma(s$iel_tested),
              if (s$iel_tested > 0) s$iel_pos / s$iel_tested * 100 else 0)
    })
    
    output$vb_concordant <- shiny::renderText({
      s <- lab_summary()
      sprintf("%s / %s (%.1f%%)",
              scales::comma(s$concordant),
              scales::comma(s$both_tested),
              if (s$both_tested > 0) s$concordant / s$both_tested * 100 else 0)
    })
    
    # PCR Cq distributions plot
    output$plot_pcr_cq <- shiny::renderPlot({
      pcr <- lab_pcr()
      if (!nrow(pcr)) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0, y = 0, 
                              label = "No PCR data loaded", size = 6) +
            ggplot2::theme_void()
        )
      }
      
      long <- pcr %>%
        dplyr::select(Cq_177T, Cq_18S2, Cq_RNAseP) %>%
        dplyr::mutate(row = dplyr::row_number()) %>%
        tidyr::pivot_longer(-row, names_to = "target", values_to = "Cq") %>%
        dplyr::filter(!is.na(Cq))
      
      ggplot2::ggplot(long, ggplot2::aes(x = Cq, fill = target)) +
        ggplot2::geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
        ggplot2::facet_wrap(~target, ncol = 1) +
        ggplot2::labs(x = "Cq Value", y = "Count", fill = "Target") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
    })
    
    # iELISA inhibition plot
    output$plot_ielisa_inh <- shiny::renderPlot({
      iel <- lab_ielisa()
      if (!nrow(iel)) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0, y = 0, 
                              label = "No iELISA data loaded", size = 6) +
            ggplot2::theme_void()
        )
      }
      
      long <- iel %>%
        dplyr::select(pct_inh_13, pct_inh_15) %>%
        dplyr::mutate(row = dplyr::row_number()) %>%
        tidyr::pivot_longer(-row, names_to = "antigen", values_to = "pct_inh") %>%
        dplyr::filter(!is.na(pct_inh)) %>%
        dplyr::mutate(
          antigen = dplyr::recode(antigen, 
                                  pct_inh_13 = "LiTat 1.3", 
                                  pct_inh_15 = "LiTat 1.5")
        )
      
      ggplot2::ggplot(long, ggplot2::aes(x = pct_inh, fill = antigen)) +
        ggplot2::geom_histogram(binwidth = 5, color = "white", alpha = 0.8) +
        ggplot2::facet_wrap(~antigen, ncol = 1) +
        ggplot2::geom_vline(xintercept = 50, linetype = "dashed", 
                            color = "red") +
        ggplot2::labs(x = "% Inhibition", y = "Count", fill = "Antigen") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
    })
    
    # PCR table
    output$tbl_pcr <- DT::renderDT({
      pcr <- lab_pcr()
      if (!nrow(pcr)) pcr <- tibble::tibble()
      
      DT::datatable(
        pcr %>%
          dplyr::select(barcode, lab_id, pcr_call, is_pcr_pos,
                        Cq_177T, SD_177T, Cq_18S2, SD_18S2, 
                        Cq_RNAseP, SD_RNAseP, source_file),
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Cq_177T", "SD_177T", "Cq_18S2", 
                                    "SD_18S2", "Cq_RNAseP", "SD_RNAseP"), 
                        digits = 2)
    })
    
    # ELISA table
    output$tbl_elisa <- DT::renderDT({
      elisa <- lab_elisa()
      if (!nrow(elisa)) elisa <- tibble::tibble()
      
      DT::datatable(
        elisa %>%
          dplyr::select(barcode, lab_id, assay_label, pp_percent, d_od,
                        result_pp50, result_pp60, source_file),
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("pp_percent", "d_od"), digits = 2)
    })
    
    # iELISA table
    output$tbl_ielisa <- DT::renderDT({
      iel <- lab_ielisa()
      if (!nrow(iel)) iel <- tibble::tibble()
      
      DT::datatable(
        iel %>%
          dplyr::select(barcode, lab_id, pct_inh_13, res_13, 
                        pct_inh_15, res_15, res_final, source_file),
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("pct_inh_13", "pct_inh_15"), digits = 1)
    })
    
    # Concordance matrix table
    concordance_data <- shiny::reactive({
      joined <- lab_joined()
      if (is.null(joined) || !nrow(joined)) return(tibble::tibble())
      
      joined %>%
        dplyr::mutate(
          PCR = dplyr::case_when(
            is_pcr_pos ~ "POS",
            is.na(is_pcr_pos) ~ "NA",
            TRUE ~ "NEG"
          ),
          iELISA = dplyr::case_when(
            any_pos ~ "POS",
            is.na(any_pos) ~ "NA",
            TRUE ~ "NEG"
          ),
          match = dplyr::case_when(
            PCR == "POS" & iELISA == "POS" ~ "POS/POS",
            PCR == "NEG" & iELISA == "NEG" ~ "NEG/NEG",
            PCR == "NA" | iELISA == "NA" ~ "INCOMPLETE",
            TRUE ~ "DISCORDANT"
          )
        ) %>%
        dplyr::select(barcode, lab_id, PCR, iELISA, match, 
                      Cq_177T, mean_inh13, mean_inh15)
    })
    
    output$tbl_concordance <- DT::renderDT({
      conc <- concordance_data()
      if (!nrow(conc)) conc <- tibble::tibble()
      
      DT::datatable(
        conc,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'match',
          backgroundColor = DT::styleEqual(
            c('POS/POS', 'NEG/NEG', 'DISCORDANT', 'INCOMPLETE'),
            c('#d4edda', '#d1ecf1', '#f8d7da', '#fff3cd')
          )
        ) %>%
        DT::formatRound(columns = c("Cq_177T", "mean_inh13", "mean_inh15"), 
                        digits = 2)
    })
    
    # Concordance summary
    output$tbl_concordance_summary <- shiny::renderTable({
      conc <- concordance_data()
      if (!nrow(conc)) {
        return(tibble::tibble(
          Category = "No data",
          Count = 0
        ))
      }
      
      conc %>%
        dplyr::count(match) %>%
        dplyr::mutate(
          Percent = round(n / sum(n) * 100, 1)
        ) %>%
        dplyr::rename(Category = match, Count = n) %>%
        dplyr::arrange(dplyr::desc(Count))
    }, striped = TRUE, hover = TRUE)
    
    # Positivity rates with confidence intervals
    output$tbl_positivity_rates <- shiny::renderTable({
      s <- lab_summary()
      
      # Calculate Wilson confidence intervals
      pcr_ci <- if (s$pcr_tested > 0) {
        binom::binom.confint(s$pcr_pos, s$pcr_tested, 
                             methods = "wilson")[, c("lower", "upper")]
      } else {
        data.frame(lower = 0, upper = 0)
      }
      
      iel_ci <- if (s$iel_tested > 0) {
        binom::binom.confint(s$iel_pos, s$iel_tested, 
                             methods = "wilson")[, c("lower", "upper")]
      } else {
        data.frame(lower = 0, upper = 0)
      }
      
      tibble::tibble(
        Assay = c("PCR", "iELISA"),
        Tested = c(s$pcr_tested, s$iel_tested),
        Positive = c(s$pcr_pos, s$iel_pos),
        `Rate (%)` = c(
          if (s$pcr_tested > 0) round(s$pcr_pos / s$pcr_tested * 100, 1) else 0,
          if (s$iel_tested > 0) round(s$iel_pos / s$iel_tested * 100, 1) else 0
        ),
        `95% CI Lower` = round(c(pcr_ci$lower, iel_ci$lower) * 100, 1),
        `95% CI Upper` = round(c(pcr_ci$upper, iel_ci$upper) * 100, 1)
      )
    }, striped = TRUE, hover = TRUE)
    
    # Downloads
    output$dl_pcr <- shiny::downloadHandler(
      filename = function() {
        paste0("pcr_results_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        readr::write_csv(lab_pcr(), file)
      }
    )
    
    output$dl_elisa <- shiny::downloadHandler(
      filename = function() {
        paste0("elisa_results_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        readr::write_csv(lab_elisa(), file)
      }
    )
    
    output$dl_ielisa <- shiny::downloadHandler(
      filename = function() {
        paste0("ielisa_results_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        readr::write_csv(lab_ielisa(), file)
      }
    )
    
    output$dl_concordance <- shiny::downloadHandler(
      filename = function() {
        paste0("concordance_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        readr::write_csv(concordance_data(), file)
      }
    )
    
    # Return lab_joined for use by other modules (e.g., map)
    return(list(
      lab_joined = lab_joined,
      lab_summary = lab_summary
    ))
  })
}

# === HELPER FUNCTIONS ===

#' Read PCR directory
#' @param dir_pcr Directory path
#' @param biobank_df Biobank keys for joining
#' @return Tibble with PCR data
read_pcr_dir <- function(dir_pcr, biobank_df = NULL) {
  dir_pcr <- lab_normalise_path(dir_pcr)
  if (is.null(dir_pcr) || is.na(dir_pcr) || !nzchar(dir_pcr)) {
    return(tibble::tibble())
  }
  if (!dir.exists(dir_pcr)) return(tibble::tibble())
  
  files <- list.files(dir_pcr, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) return(tibble::tibble())
  
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
          !!paste0("Cq_", tag) := mean(Cq, na.rm = TRUE),
          !!paste0("SD_", tag) := stats::sd(Cq, na.rm = TRUE),
          .groups = "drop"
        )
    }, error = function(e) {
      tibble::tibble(sample = character())
    })
  }
  
  make_qpcr_summary <- function(path) {
    call_df <- tryCatch({
      readxl::read_excel(path, sheet = "Replicates") %>%
        janitor::clean_names() %>%
        dplyr::transmute(
          lab_id = as.character(sample),
          pcr_call = as.character(final)
        )
    }, error = function(e) {
      tibble::tibble(lab_id = character(), pcr_call = character())
    })
    
    s177 <- summarise_target_long(path, "177T", "177T")
    s18 <- summarise_target_long(path, "18S2", "18S2")
    srp <- summarise_target_long(path, "RNAseP", "RNAseP")
    
    dplyr::reduce(
      list(
        call_df,
        dplyr::rename(s177, lab_id = sample),
        dplyr::rename(s18, lab_id = sample),
        dplyr::rename(srp, lab_id = sample)
      ),
      dplyr::left_join, by = "lab_id"
    )
  }
  
  pcr <- purrr::map_dfr(files, make_qpcr_summary, .id = "source_idx") %>%
    dplyr::mutate(source_file = basename(files[as.integer(source_idx)])) %>%
    dplyr::select(-source_idx) %>%
    dplyr::distinct(lab_id, .keep_all = TRUE) %>%
    dplyr::mutate(is_pcr_pos = is_positive_result(pcr_call))
  
  # Attach barcode via biobank if present
  if (!is.null(biobank_df) && nrow(biobank_df)) {
    pcr <- pcr %>%
      dplyr::left_join(
        biobank_df %>% dplyr::distinct(barcode, lab_id),
        by = "lab_id"
      ) %>%
      dplyr::relocate(barcode, lab_id)
  }
  
  pcr
}

#' Read ELISA directories (PE and VSG)
#' @param dir_pe ELISA PE directory
#' @param dir_vsg ELISA VSG directory
#' @return Tibble with ELISA data
read_elisa_dirs <- function(dir_pe, dir_vsg) {
  dir_pe <- lab_normalise_path(dir_pe)
  dir_vsg <- lab_normalise_path(dir_vsg)

  read_elisa_file <- function(path, assay_label) {
    tryCatch({
      sheets <- readxl::excel_sheets(path)
      sheet <- sheets[grepl("result", sheets, ignore.case = TRUE)]
      if (!length(sheet)) sheet <- sheets[1]
      
      raw <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal") %>%
        janitor::clean_names()
      
      first_present <- function(.data, ...) {
        opts <- c(...)
        col <- opts[opts %in% names(.data)][1]
        if (is.na(col)) rep(NA, nrow(.data)) else .data[[col]]
      }
      
      tibble::tibble(
        source_file = basename(path),
        assay_label = assay_label,
        barcode_raw = first_present(raw, "code_echantillon_code_barres",
                                    "code_barres_kps", "code_barres",
                                    "code_echantillon_barres"),
        lab_id_raw = first_present(raw, "numero_labo_dipumba", "numero_labo"),
        d_od = suppressWarnings(as.numeric(first_present(raw, "d_od", "delta_od", "od"))),
        pp_percent = suppressWarnings(as.numeric(first_present(raw, "pp", "pp_percent"))),
        result_pp60 = first_present(raw, "result_pp60"),
        result_pp50 = first_present(raw, "result_pp50")
      ) %>%
        dplyr::mutate(
          barcode = as.character(barcode_raw),
          lab_id = as.character(lab_id_raw)
        ) %>%
        dplyr::select(-barcode_raw, -lab_id_raw)
    }, error = function(e) {
      warning("Failed to read ELISA file: ", path, " - ", e$message)
      tibble::tibble()
    })
  }
  
  read_dir <- function(dir, label) {
    if (is.null(dir) || is.na(dir) || !nzchar(dir)) return(tibble::tibble())
    if (!dir.exists(dir)) return(tibble::tibble())
    files <- list.files(dir, pattern = "\\.xlsx?$", full.names = TRUE)
    if (!length(files)) return(tibble::tibble())
    purrr::map_dfr(files, read_elisa_file, assay_label = label)
  }
  
  dplyr::bind_rows(
    read_dir(dir_pe, "ELISA_PE"),
    read_dir(dir_vsg, "ELISA_VSG")
  )
}

#' Read iELISA directory
#' @param dir_ielisa Directory path
#' @return Tibble with iELISA data
read_ielisa_dir <- function(dir_ielisa) {
  dir_ielisa <- lab_normalise_path(dir_ielisa)
  if (is.null(dir_ielisa) || is.na(dir_ielisa) || !nzchar(dir_ielisa)) {
    return(tibble::tibble())
  }
  if (!dir.exists(dir_ielisa)) return(tibble::tibble())
  
  files <- list.files(dir_ielisa, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) return(tibble::tibble())
  
  read_ielisa_file <- function(path) {
    tryCatch({
      sheets <- readxl::excel_sheets(path)
      sheet <- sheets[grepl("résult|result", 
                            stringi::stri_trans_general(sheets, "Latin-ASCII"), 
                            ignore.case = TRUE)]
      if (!length(sheet)) sheet <- sheets[1]
      
      # Probe for header row
      probe <- readxl::read_excel(path, sheet = sheet, range = "A1:C40",
                                  col_names = FALSE, .name_repair = "minimal")
      header_row <- which(trimws(probe[[2]]) == "Numéro labo")[1] - 1
      if (is.na(header_row)) header_row <- 0
      
      raw <- readxl::read_excel(path, sheet = sheet, skip = header_row, 
                                .name_repair = "minimal") %>%
        janitor::clean_names()
      
      pct_cols <- grep("^pourcentage_d.?inhibition", names(raw), value = TRUE)
      res_cols <- grep("^resultat($|_\\d+)", names(raw), value = TRUE)
      pct_cols <- c(pct_cols, NA, NA)
      res_cols <- c(res_cols, NA, NA)
      
      tibble::tibble(
        source_file = basename(path),
        assay_label = "iELISA",
        barcode = dplyr::coalesce(
          raw$code_barres_kps,
          raw$code_echantillon_code_barres,
          raw$barcode,
          raw$code_barres
        ) %>% as.character(),
        lab_id = dplyr::coalesce(
          raw$numero_labo,
          raw$sample_id
        ) %>% as.character(),
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
        res_final = dplyr::coalesce(
          raw$resultat_final,
          raw$x_2
        ) %>% as.character()
      )
    }, error = function(e) {
      warning("Failed to read iELISA file: ", path, " - ", e$message)
      tibble::tibble()
    })
  }
  
  purrr::map_dfr(files, read_ielisa_file)
}