# mod_lab_results2.R
# =====================================================================
# MODULE: Lab Results v2 — integrates PCR, ELISA (PE + VSG), iELISA, QC
# =====================================================================

#' Lab Results Module - UI
#' @param id Module namespace ID
#' @export
mod_lab_results2_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = c(4, 8),
    
    # Control panel
    bslib::card(
      bslib::card_header("Load Lab Results"),
      
      shiny::textInput(ns("pcr_dir"), "PCR folder", value = ""),
      shiny::textInput(ns("elisa_pe_dir"), "ELISA PE folder", value = ""),
      shiny::textInput(ns("elisa_vsg_dir"), "ELISA VSG folder", value = ""),
      shiny::textInput(ns("ielisa_dir"), "iELISA folder", value = ""),
      
      shiny::actionButton(ns("lab_load"), "Load Lab Results", 
                          class = "btn-primary w-100 mb-3"),
      
      shiny::div(class = "mt-2", shiny::uiOutput(ns("lab_status_ui"))),
      
      shiny::hr(),
      
      shiny::h6("Positivity Thresholds"),
      shiny::numericInput(ns("threshold_pcr"), "PCR Cq max", 
                          value = 38, min = 1, max = 45, step = 1),
      shiny::numericInput(ns("threshold_elisa"), "ELISA PP% cutoff", 
                          value = 50, min = 0, max = 100, step = 5),
      shiny::numericInput(ns("threshold_ielisa"), "iELISA % inhibition cutoff", 
                          value = 35, min = 0, max = 100, step = 5),
      
      shiny::hr(),
      
      shiny::textInput(ns("barcode_search"), "Search Barcode/LabID", 
                       placeholder = "e.g. KPS001, 105")
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
              title = "PCR Tested",
              value = shiny::textOutput(ns("vb_pcr_tested")),
              showcase = bsicons::bs_icon("virus"),
              theme = "info"
            ),
            bslib::value_box(
              title = "PCR Positive",
              value = shiny::textOutput(ns("vb_pcr_pos")),
              showcase = bsicons::bs_icon("exclamation-triangle"),
              theme = "danger"
            ),
            bslib::value_box(
              title = "iELISA Positive",
              value = shiny::textOutput(ns("vb_ielisa_pos")),
              showcase = bsicons::bs_icon("clipboard2-check"),
              theme = "warning"
            )
          ),
          
          DT::DTOutput(ns("tbl_overview"))
        ),
        
        bslib::nav_panel(
          "PCR Details",
          shiny::downloadButton(ns("dl_pcr"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_pcr"))
        ),
        
        bslib::nav_panel(
          "ELISA PE",
          shiny::downloadButton(ns("dl_elisa_pe"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_elisa_pe"))
        ),
        
        bslib::nav_panel(
          "ELISA VSG",
          shiny::downloadButton(ns("dl_elisa_vsg"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_elisa_vsg"))
        ),
        
        bslib::nav_panel(
          "iELISA",
          shiny::downloadButton(ns("dl_ielisa"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_ielisa"))
        ),
        
        bslib::nav_panel(
          "Concordance",
          shiny::downloadButton(ns("dl_concordance"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_concordance"))
        ),
        
        bslib::nav_panel(
          "Controls / QC",
          shiny::h5("PCR Controls"),
          DT::DTOutput(ns("qc_pcr")),
          shiny::hr(),
          shiny::h5("ELISA PE Controls"),
          DT::DTOutput(ns("qc_elisa_pe")),
          shiny::hr(),
          shiny::h5("ELISA VSG Controls"),
          DT::DTOutput(ns("qc_elisa_vsg")),
          shiny::hr(),
          shiny::h5("iELISA Controls"),
          DT::DTOutput(ns("qc_ielisa"))
        )
      )
    )
  )
}

#' Lab Results Module - Server
#' @param id Module namespace ID
#' @param biobank_clean Reactive containing cleaned biobank data
#' @param config Reactive containing app configuration
#' @export
mod_lab_results2_server <- function(id, biobank_clean, config) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values
    lab_data_raw <- shiny::reactiveVal(list(
      pcr = tibble::tibble(),
      elisa_pe = tibble::tibble(),
      elisa_vsg = tibble::tibble(),
      ielisa = tibble::tibble()
    ))
    
    lab_status <- shiny::reactiveVal("No lab data loaded")
    
    # Initialize paths from config
    shiny::observe({
      cfg <- config()
      if (!is.null(cfg) && "paths" %in% names(cfg)) {
        shiny::updateTextInput(session, "pcr_dir", 
                               value = safe_path(cfg$paths$pcr_dir))
        shiny::updateTextInput(session, "elisa_pe_dir", 
                               value = safe_path(cfg$paths$elisa_pe_dir))
        shiny::updateTextInput(session, "elisa_vsg_dir", 
                               value = safe_path(cfg$paths$elisa_vsg_dir))
        shiny::updateTextInput(session, "ielisa_dir", 
                               value = safe_path(cfg$paths$ielisa_dir))
      }
    })
    
    # Load lab data
    shiny::observeEvent(input$lab_load, {
      shiny::withProgress(message = 'Loading lab results...', value = 0, {
        
        # Check biobank is loaded
        bio <- biobank_clean()
        if (is.null(bio) || !nrow(bio)) {
          lab_status("⚠️ Load biobank data first!")
          shiny::showNotification("Load biobank data first.", type = "error")
          return()
        }
        
        status_parts <- character()
        errors <- character()
        
        # Load PCR
        shiny::incProgress(0.25, detail = "Loading PCR...")
        pcr_data <- tryCatch({
          parse_pcr_results(input$pcr_dir)
        }, error = function(e) {
          errors <<- c(errors, paste("PCR:", e$message))
          tibble::tibble()
        })
        status_parts <- c(status_parts, sprintf("PCR: %d", nrow(pcr_data)))
        
        # Load ELISA PE
        shiny::incProgress(0.25, detail = "Loading ELISA PE...")
        elisa_pe_data <- tryCatch({
          parse_elisa_pe(input$elisa_pe_dir)
        }, error = function(e) {
          errors <<- c(errors, paste("ELISA PE:", e$message))
          tibble::tibble()
        })
        status_parts <- c(status_parts, sprintf("PE: %d", nrow(elisa_pe_data)))
        
        # Load ELISA VSG
        shiny::incProgress(0.25, detail = "Loading ELISA VSG...")
        elisa_vsg_data <- tryCatch({
          parse_elisa_vsg(input$elisa_vsg_dir)
        }, error = function(e) {
          errors <<- c(errors, paste("ELISA VSG:", e$message))
          tibble::tibble()
        })
        status_parts <- c(status_parts, sprintf("VSG: %d", nrow(elisa_vsg_data)))
        
        # Load iELISA
        shiny::incProgress(0.25, detail = "Loading iELISA...")
        ielisa_data <- tryCatch({
          parse_ielisa(input$ielisa_dir)
        }, error = function(e) {
          errors <<- c(errors, paste("iELISA:", e$message))
          tibble::tibble()
        })
        status_parts <- c(status_parts, sprintf("iELISA: %d", nrow(ielisa_data)))
        
        # Store data
        lab_data_raw(list(
          pcr = pcr_data,
          elisa_pe = elisa_pe_data,
          elisa_vsg = elisa_vsg_data,
          ielisa = ielisa_data
        ))
        
        # Build status message
        status_msg <- paste(status_parts, collapse = " | ")
        if (length(errors) > 0) {
          status_msg <- paste0(status_msg, "\n⚠️ ", paste(errors, collapse = "; "))
        }
        lab_status(status_msg)
        
        total_loaded <- nrow(pcr_data) + nrow(elisa_pe_data) + 
                        nrow(elisa_vsg_data) + nrow(ielisa_data)
        
        if (total_loaded > 0) {
          shiny::showNotification(
            sprintf("Loaded %d lab records", total_loaded),
            type = "message"
          )
        } else {
          shiny::showNotification(
            "No lab results loaded. Check folder paths and file formats.",
            type = "warning"
          )
        }
      })
    })
    
    # Status UI with color coding
    output$lab_status_ui <- shiny::renderUI({
      status <- lab_status()
      has_warning <- grepl("⚠️", status)
      
      shiny::div(
        class = if (has_warning) "alert alert-warning" else "alert alert-info",
        shiny::HTML(gsub("\n", "<br/>", status))
      )
    })
    
    # Merged and flagged data
    joined_data <- shiny::reactive({
      bio <- biobank_clean()
      lab <- lab_data_raw()
      
      if (is.null(bio) || !nrow(bio)) return(tibble::tibble())
      
      tryCatch({
        merged <- merge_lab_with_biobank(bio, lab)
        flagged <- compute_flags(
          merged,
          pcr_cq_max = input$threshold_pcr,
          elisa_pp_cut = input$threshold_elisa,
          ielisa_inh_cut = input$threshold_ielisa
        )
        flagged
      }, error = function(e) {
        warning("Failed to merge lab data: ", e$message)
        tibble::tibble()
      })
    })
    
    # Filtered by search
    filtered_data <- shiny::reactive({
      df <- joined_data()
      if (!nrow(df)) return(df)
      
      search <- input$barcode_search
      if (!is.null(search) && nzchar(trimws(search))) {
        search_clean <- toupper(trimws(search))
        df <- df %>%
          dplyr::filter(
            grepl(search_clean, toupper(barcode), fixed = TRUE) |
            grepl(search_clean, toupper(lab_id), fixed = TRUE)
          )
      }
      
      df
    })
    
    # === VALUE BOXES ===
    
    output$vb_pcr_tested <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0")
      n_tested <- sum(!is.na(df$Cq_177T) | !is.na(df$Cq_18S2))
      scales::comma(n_tested)
    })
    
    output$vb_pcr_pos <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0 / 0 (0%)")
      n_pos <- sum(df$PCR_pos %in% TRUE, na.rm = TRUE)
      n_tested <- sum(!is.na(df$Cq_177T) | !is.na(df$Cq_18S2))
      pct <- if (n_tested > 0) round(100 * n_pos / n_tested, 1) else 0
      sprintf("%s / %s (%s%%)", scales::comma(n_pos), scales::comma(n_tested), pct)
    })
    
    output$vb_ielisa_pos <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0 / 0 (0%)")
      n_pos <- sum(df$iELISA_pos %in% TRUE, na.rm = TRUE)
      n_tested <- sum(!is.na(df$pct_inh_13) | !is.na(df$pct_inh_15))
      pct <- if (n_tested > 0) round(100 * n_pos / n_tested, 1) else 0
      sprintf("%s / %s (%s%%)", scales::comma(n_pos), scales::comma(n_tested), pct)
    })
    
    # === TABLES ===
    
    # Overview with detailed values
    output$tbl_overview <- DT::renderDT({
      df <- filtered_data()
      
      if (!nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No lab results available")))
      }
      
      display_df <- df %>%
        dplyr::select(
          barcode, lab_id,
          # PCR
          PCR_Call = pcr_call,
          `177T Cq` = Cq_177T,
          `18S2 Cq` = Cq_18S2,
          `RNAseP Cq` = Cq_RNAseP,
          PCR_Status = PCR_pos,
          # ELISA PE
          `PE PP%` = pp_percent_pe,
          `PE ΔOD` = d_od_pe,
          PE_Status = ELISA_PE_pos,
          # ELISA VSG
          `VSG PP%` = pp_percent_vsg,
          `VSG ΔOD` = d_od_vsg,
          VSG_Status = ELISA_VSG_pos,
          # iELISA
          `iELISA 1.3%` = pct_inh_13,
          `iELISA 1.5%` = pct_inh_15,
          iELISA_Status = iELISA_pos,
          # Overall
          Concordant
        ) %>%
        dplyr::mutate(
          dplyr::across(c(PCR_Status, PE_Status, VSG_Status, iELISA_Status, Concordant),
                        ~dplyr::case_when(
                          . == TRUE ~ "POS",
                          . == FALSE ~ "NEG",
                          TRUE ~ "NA"
                        ))
        )
      
      DT::datatable(
        display_df,
        options = list(pageLength = 20, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("177T Cq", "18S2 Cq", "RNAseP Cq",
                                    "PE PP%", "PE ΔOD", "VSG PP%", "VSG ΔOD",
                                    "iELISA 1.3%", "iELISA 1.5%"),
                        digits = 2) %>%
        DT::formatStyle(
          c("PCR_Status", "PE_Status", "VSG_Status", "iELISA_Status"),
          backgroundColor = DT::styleEqual(
            c("POS", "NEG", "NA"),
            c("#f8d7da", "#d4edda", "#e2e3e5")
          )
        ) %>%
        DT::formatStyle(
          "Concordant",
          backgroundColor = DT::styleEqual(
            c("POS", "NEG", "NA"),
            c("#d4edda", "#fff3cd", "#e2e3e5")
          )
        )
    })
    
    # PCR detail table
    output$tbl_pcr <- DT::renderDT({
      pcr <- lab_data_raw()$pcr
      
      if (!nrow(pcr)) {
        return(DT::datatable(tibble::tibble(Message = "No PCR data loaded")))
      }
      
      DT::datatable(
        pcr,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Cq_177T", "SD_177T", "Cq_18S2", 
                                    "SD_18S2", "Cq_RNAseP", "SD_RNAseP"),
                        digits = 2)
    })
    
    # ELISA PE table
    output$tbl_elisa_pe <- DT::renderDT({
      elisa_pe <- lab_data_raw()$elisa_pe
      
      if (!nrow(elisa_pe)) {
        return(DT::datatable(tibble::tibble(Message = "No ELISA PE data loaded")))
      }
      
      DT::datatable(
        elisa_pe,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("d_od", "pp_percent"), digits = 2)
    })
    
    # ELISA VSG table
    output$tbl_elisa_vsg <- DT::renderDT({
      elisa_vsg <- lab_data_raw()$elisa_vsg
      
      if (!nrow(elisa_vsg)) {
        return(DT::datatable(tibble::tibble(Message = "No ELISA VSG data loaded")))
      }
      
      DT::datatable(
        elisa_vsg,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("d_od", "pp_percent"), digits = 2)
    })
    
    # iELISA table
    output$tbl_ielisa <- DT::renderDT({
      ielisa <- lab_data_raw()$ielisa
      
      if (!nrow(ielisa)) {
        return(DT::datatable(tibble::tibble(Message = "No iELISA data loaded")))
      }
      
      DT::datatable(
        ielisa,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("pct_inh_13", "pct_inh_15"), digits = 1)
    })
    
    # Concordance table
    output$tbl_concordance <- DT::renderDT({
      df <- joined_data()
      
      if (!nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No data for concordance")))
      }
      
      concordance_summary <- df %>%
        dplyr::mutate(
          PCR = dplyr::case_when(
            PCR_pos ~ "POS",
            is.na(PCR_pos) ~ "Not Tested",
            TRUE ~ "NEG"
          ),
          iELISA = dplyr::case_when(
            iELISA_pos ~ "POS",
            is.na(iELISA_pos) ~ "Not Tested",
            TRUE ~ "NEG"
          )
        ) %>%
        dplyr::count(PCR, iELISA, name = "Count") %>%
        dplyr::arrange(PCR, iELISA)
      
      DT::datatable(
        concordance_summary,
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    # === QC TABLES ===
    
    output$qc_pcr <- DT::renderDT({
      DT::datatable(
        parse_pcr_controls(input$pcr_dir),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    output$qc_elisa_pe <- DT::renderDT({
      DT::datatable(
        parse_elisa_pe_controls(input$elisa_pe_dir),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    output$qc_elisa_vsg <- DT::renderDT({
      DT::datatable(
        parse_elisa_vsg_controls(input$elisa_vsg_dir),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    output$qc_ielisa <- DT::renderDT({
      DT::datatable(
        parse_ielisa_controls(input$ielisa_dir),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    # === DOWNLOADS ===
    
    output$dl_pcr <- shiny::downloadHandler(
      filename = function() paste0("pcr_results_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) readr::write_csv(lab_data_raw()$pcr, file)
    )
    
    output$dl_elisa_pe <- shiny::downloadHandler(
      filename = function() paste0("elisa_pe_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) readr::write_csv(lab_data_raw()$elisa_pe, file)
    )
    
    output$dl_elisa_vsg <- shiny::downloadHandler(
      filename = function() paste0("elisa_vsg_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) readr::write_csv(lab_data_raw()$elisa_vsg, file)
    )
    
    output$dl_ielisa <- shiny::downloadHandler(
      filename = function() paste0("ielisa_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) readr::write_csv(lab_data_raw()$ielisa, file)
    )
    
    output$dl_concordance <- shiny::downloadHandler(
      filename = function() paste0("concordance_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) readr::write_csv(joined_data(), file)
    )
    
    # Return joined data for use by other modules (e.g., map)
    return(list(
      lab_joined = joined_data
    ))
  })
}