# mod_lab_results_complete.R
# =====================================================================
# MODULE: Lab Results — Complete with ALL FIXES APPLIED
# VERSION: 3.0 - 2025-11-03
# =====================================================================
# FIXES INCLUDED:
# ✅ Canonical call handling (P/F/S/R → pos/neg/suspect/repeat)
# ✅ Proper deduplication before joining
# ✅ Reactive thresholds
# ✅ Correct tested flags for all assays
# ✅ Fixed concordance logic (all positive = TRUE)
# ✅ ELISA box combines PE + VSG
# ✅ Controls QC tab with Levey-Jennings plots
# =====================================================================

#' Lab Results Module - UI
#' @param id Module namespace ID
#' @export
mod_lab_results_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = c(4, 8),
    
    # Control panel
    bslib::card(
      bslib::card_header("Lab Results Settings"),
      
      shiny::h6("Folders"),
      shiny::textInput(ns("pcr_dir"), "PCR folder", value = ""),
      shiny::textInput(ns("elisa_pe_dir"), "ELISA PE folder", value = ""),
      shiny::textInput(ns("elisa_vsg_dir"), "ELISA VSG folder", value = ""),
      shiny::textInput(ns("ielisa_dir"), "iELISA folder", value = ""),
      
      shiny::actionButton(ns("lab_load"), "Load Lab Results", 
                          class = "btn-primary w-100 mb-3"),
      
      shiny::div(class = "mt-2", shiny::uiOutput(ns("lab_status_ui"))),
      
      shiny::hr(),
      
      shiny::h6("Positivity Thresholds"),
      
      shiny::strong("PCR Targets (Cq max)"),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shiny::numericInput(ns("threshold_pcr_177t"), "177T", 
                            value = 38, min = 1, max = 45, step = 0.5, width = "100%"),
        shiny::numericInput(ns("threshold_pcr_18s2"), "18S2", 
                            value = 38, min = 1, max = 45, step = 0.5, width = "100%"),
        shiny::numericInput(ns("threshold_pcr_rnasep"), "RNAseP", 
                            value = 38, min = 1, max = 45, step = 0.5, width = "100%")
      ),
      
      shiny::strong("ELISA PP% cutoffs"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::numericInput(ns("threshold_elisa_pe"), "PE", 
                            value = 50, min = 0, max = 100, step = 5, width = "100%"),
        shiny::numericInput(ns("threshold_elisa_vsg"), "VSG", 
                            value = 50, min = 0, max = 100, step = 5, width = "100%")
      ),
      
      shiny::strong("iELISA % inhibition cutoffs"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::numericInput(ns("threshold_ielisa_13"), "LiTat 1.3", 
                            value = 35, min = 0, max = 100, step = 5, width = "100%"),
        shiny::numericInput(ns("threshold_ielisa_15"), "LiTat 1.5", 
                            value = 35, min = 0, max = 100, step = 5, width = "100%")
      ),
      
      shiny::hr(),
      
      shiny::h6("Search"),
      shiny::textInput(ns("barcode_search"), "Search Barcode/LabID", 
                       placeholder = "e.g. KPS001, 105"),
      shiny::actionButton(ns("clear_search"), "Clear", 
                          class = "btn-outline-secondary btn-sm w-100")
    ),
    
    # Results panel
    bslib::card(
      bslib::card_header("Lab Results"),
      
      bslib::navset_tab(
        # === OVERVIEW TAB ===
        bslib::nav_panel(
          "Overview",
          
          bslib::layout_columns(
            col_widths = c(2, 2, 2, 2, 2, 2),
            fill = FALSE,
            
            bslib::value_box(
              title = "Samples Tested",
              value = shiny::textOutput(ns("vb_total_tested")),
              showcase = bsicons::bs_icon("clipboard-data"),
              theme = "primary"
            ),
            bslib::value_box(
              title = "PCR Positive",
              value = shiny::textOutput(ns("vb_pcr_pos")),
              showcase = bsicons::bs_icon("virus"),
              theme = "danger"
            ),
            bslib::value_box(
              title = "ELISA PE Positive",
              value = shiny::textOutput(ns("vb_elisa_pe_pos")),
              showcase = bsicons::bs_icon("clipboard-check"),
              theme = "warning"
            ),
            bslib::value_box(
              title = "ELISA VSG Positive",
              value = shiny::textOutput(ns("vb_elisa_vsg_pos")),
              showcase = bsicons::bs_icon("clipboard-pulse"),
              theme = "warning"
            ),
            bslib::value_box(
              title = "iELISA Positive",
              value = shiny::textOutput(ns("vb_ielisa_pos")),
              showcase = bsicons::bs_icon("clipboard2-check"),
              theme = "info"
            ),
            bslib::value_box(
              title = "Any Positive",
              value = shiny::textOutput(ns("vb_any_pos")),
              showcase = bsicons::bs_icon("exclamation-triangle"),
              theme = "success"
            )
          ),
          
          shiny::h5("Test Coverage by Assay", class = "mt-3"),
          DT::DTOutput(ns("tbl_test_coverage")),
          
          shiny::hr(),
          
          shiny::h5("Sample Results (Filtered)", class = "mt-3"),
          shiny::downloadButton(ns("dl_overview"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_overview"))
        ),
        
        # === PCR TAB ===
        bslib::nav_panel(
          "PCR Details",
          shiny::downloadButton(ns("dl_pcr"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_pcr"))
        ),
        
        # === ELISA PE TAB ===
        bslib::nav_panel(
          "ELISA PE",
          shiny::downloadButton(ns("dl_elisa_pe"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_elisa_pe"))
        ),
        
        # === ELISA VSG TAB ===
        bslib::nav_panel(
          "ELISA VSG",
          shiny::downloadButton(ns("dl_elisa_vsg"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_elisa_vsg"))
        ),
        
        # === iELISA TAB ===
        bslib::nav_panel(
          "iELISA",
          shiny::downloadButton(ns("dl_ielisa"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_ielisa"))
        ),
        
        # === CONCORDANCE TAB ===
        bslib::nav_panel(
          "Concordance",
          shiny::downloadButton(ns("dl_concordance"), "Download", class = "btn-sm mb-2"),
          
          bslib::layout_columns(
            col_widths = c(6, 6),
            
            bslib::card(
              bslib::card_header("Agreement Summary"),
              shiny::tableOutput(ns("tbl_concordance_summary"))
            ),
            bslib::card(
              bslib::card_header("Positivity Rates"),
              shiny::tableOutput(ns("tbl_positivity_rates"))
            )
          ),
          
          DT::DTOutput(ns("tbl_concordance"))
        ),
        
        # === CONTROLS QC TAB (NEW) ===
        bslib::nav_panel(
          "Controls QC",
          
          bslib::layout_columns(
            col_widths = c(3, 9),
            
            # Filters
            bslib::card(
              bslib::card_header("QC Filters"),
              shiny::selectInput(
                ns("qc_assay"),
                "Assay",
                choices = c("All", "PCR", "ELISA_PE", "ELISA_VSG", "iELISA"),
                selected = "All"
              ),
              shiny::selectInput(
                ns("qc_control"),
                "Control Type",
                choices = c("All", "PC", "NC", "CC"),
                selected = "All"
              ),
              shiny::checkboxInput(
                ns("qc_flag_outliers"),
                "Flag outliers (±2 SD)",
                value = TRUE
              ),
              shiny::hr(),
              shiny::downloadButton(ns("dl_controls"), "Download Controls", 
                                    class = "btn-sm w-100")
            ),
            
            # Results
            bslib::card(
              bslib::card_header("Control Performance"),
              bslib::navset_tab(
                bslib::nav_panel(
                  "Summary",
                  DT::DTOutput(ns("qc_controls_summary"))
                ),
                bslib::nav_panel(
                  "Levey-Jennings Chart",
                  shiny::plotOutput(ns("qc_lj_plot"), height = 500)
                ),
                bslib::nav_panel(
                  "Raw Data",
                  DT::DTOutput(ns("qc_controls_raw"))
                )
              )
            )
          )
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
mod_lab_results_server <- function(id, biobank_clean, config) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Ensure all helpers are loaded at module start
    tryCatch({
      if (!exists("canonicalise_call")) {
        source("R/helpers_lab_results2.R", local = TRUE)
      }
    }, error = function(e) {
      warning("Could not load helpers_lab_results2.R: ", e$message)
    })
    
    tryCatch({
      if (!exists("merge_lab_with_biobank_v2")) {
        source("R/helpers_lab_merge.R", local = TRUE)
      }
    }, error = function(e) {
      warning("Could not load helpers_lab_merge.R: ", e$message)
    })
    
    tryCatch({
      if (!exists("classify_concordance")) {
        source("R/helpers_concordance.R", local = TRUE)
      }
    }, error = function(e) {
      warning("Could not load helpers_concordance.R: ", e$message)
    })
    
    tryCatch({
      if (!exists("extract_pcr_controls")) {
        source("R/helpers_controls.R", local = TRUE)
      }
    }, error = function(e) {
      warning("Could not load helpers_controls.R: ", e$message)
    })
    
    
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
        if (exists("safe_path")) {
          shiny::updateTextInput(session, "pcr_dir", 
                                 value = safe_path(cfg$paths$pcr_dir))
          shiny::updateTextInput(session, "elisa_pe_dir", 
                                 value = safe_path(cfg$paths$elisa_pe_dir))
          shiny::updateTextInput(session, "elisa_vsg_dir", 
                                 value = safe_path(cfg$paths$elisa_vsg_dir))
          shiny::updateTextInput(session, "ielisa_dir", 
                                 value = safe_path(cfg$paths$ielisa_dir))
        } else {
          shiny::updateTextInput(session, "pcr_dir", value = cfg$paths$pcr_dir)
          shiny::updateTextInput(session, "elisa_pe_dir", value = cfg$paths$elisa_pe_dir)
          shiny::updateTextInput(session, "elisa_vsg_dir", value = cfg$paths$elisa_vsg_dir)
          shiny::updateTextInput(session, "ielisa_dir", value = cfg$paths$ielisa_dir)
        }
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
          dat <- parse_pcr_results(input$pcr_dir)
          # Verify structure
          if (nrow(dat) > 0) {
            if (!"lab_id" %in% names(dat)) {
              warning("PCR data missing lab_id column")
              dat$lab_id <- NA_character_
            }
          }
          dat
        }, error = function(e) {
          errors <<- c(errors, paste("PCR:", e$message))
          tibble::tibble()
        })
        status_parts <- c(status_parts, sprintf("PCR: %d", nrow(pcr_data)))
        
        # Load ELISA PE
        shiny::incProgress(0.25, detail = "Loading ELISA PE...")
        elisa_pe_data <- tryCatch({
          dat <- parse_elisa_pe(input$elisa_pe_dir)
          if (nrow(dat) > 0) {
            required <- c("lab_id", "barcode")
            missing <- required[!required %in% names(dat)]
            if (length(missing)) {
              warning("ELISA PE missing columns: ", paste(missing, collapse = ", "))
              for (col in missing) dat[[col]] <- NA_character_
            }
          }
          dat
        }, error = function(e) {
          errors <<- c(errors, paste("ELISA PE:", e$message))
          tibble::tibble()
        })
        status_parts <- c(status_parts, sprintf("PE: %d", nrow(elisa_pe_data)))
        
        # Load ELISA VSG
        shiny::incProgress(0.25, detail = "Loading ELISA VSG...")
        elisa_vsg_data <- tryCatch({
          dat <- parse_elisa_vsg(input$elisa_vsg_dir)
          if (nrow(dat) > 0) {
            required <- c("lab_id", "barcode")
            missing <- required[!required %in% names(dat)]
            if (length(missing)) {
              warning("ELISA VSG missing columns: ", paste(missing, collapse = ", "))
              for (col in missing) dat[[col]] <- NA_character_
            }
          }
          dat
        }, error = function(e) {
          errors <<- c(errors, paste("ELISA VSG:", e$message))
          tibble::tibble()
        })
        status_parts <- c(status_parts, sprintf("VSG: %d", nrow(elisa_vsg_data)))
        
        # Load iELISA
        shiny::incProgress(0.25, detail = "Loading iELISA...")
        ielisa_data <- tryCatch({
          dat <- parse_ielisa(input$ielisa_dir)
          # Standardize column names
          if (nrow(dat) > 0) {
            if ("LabID" %in% names(dat) && !"lab_id" %in% names(dat)) {
              dat <- dat %>% dplyr::rename(lab_id = LabID)
            }
            if ("Barcode" %in% names(dat) && !"barcode" %in% names(dat)) {
              dat <- dat %>% dplyr::rename(barcode = Barcode)
            }
          }
          dat
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
    
    # Clear search
    shiny::observeEvent(input$clear_search, {
      shiny::updateTextInput(session, "barcode_search", value = "")
    })
    
    # Merged and flagged data (UPDATED WITH THRESHOLDS)
    joined_data <- shiny::reactive({
      bio <- biobank_clean()
      lab <- lab_data_raw()
      
      if (is.null(bio) || !nrow(bio)) return(tibble::tibble())
      
      # Get current thresholds with detailed markers
      thresholds <- list(
        pcr_cq_177t = input$threshold_pcr_177t,
        pcr_cq_18s2 = input$threshold_pcr_18s2,
        pcr_cq_rnasep = input$threshold_pcr_rnasep,
        elisa_pe_pp = input$threshold_elisa_pe,
        elisa_vsg_pp = input$threshold_elisa_vsg,
        ielisa_inh_13 = input$threshold_ielisa_13,
        ielisa_inh_15 = input$threshold_ielisa_15
      )
      
      tryCatch({
        # Use corrected merge function with thresholds
        merged <- merge_lab_with_biobank_v2(bio, lab, thresholds)
        
        # Merge function already adds canonical calls and is_pos flags
        merged
        
      }, error = function(e) {
        warning("Failed to merge lab data: ", e$message)
        shiny::showNotification(
          paste("Lab merge error:", e$message),
          type = "error",
          duration = 10
        )
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
    
    # === VALUE BOXES (FIXED TO USE TESTED FLAGS) ===
    
    output$vb_total_tested <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0")
      
      # Count samples with ANY test data
      n_tested <- sum(
        (df$PCR_tested | df$elisa_pe_tested | df$elisa_vsg_tested | df$ielisa_tested) %||% FALSE,
        na.rm = TRUE
      )
      
      scales::comma(n_tested)
    })
    
    output$vb_pcr_pos <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0 / 0 (0%)")
      
      # Use canonical flag AND tested flag
      n_pos <- sum(df$PCR_is_pos %||% FALSE, na.rm = TRUE)
      n_tested <- sum(df$PCR_tested %||% FALSE, na.rm = TRUE)
      pct <- if (n_tested > 0) round(100 * n_pos / n_tested, 1) else 0
      
      sprintf("%s / %s (%s%%)", scales::comma(n_pos), scales::comma(n_tested), pct)
    })
    
    output$vb_elisa_pe_pos <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0 / 0 (0%)")
      
      n_pos <- sum(df$elisa_pe_is_pos %||% FALSE, na.rm = TRUE)
      n_tested <- sum(df$elisa_pe_tested %||% FALSE, na.rm = TRUE)
      pct <- if (n_tested > 0) round(100 * n_pos / n_tested, 1) else 0
      
      sprintf("%s / %s (%s%%)", scales::comma(n_pos), scales::comma(n_tested), pct)
    })
    
    output$vb_elisa_vsg_pos <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0 / 0 (0%)")
      
      n_pos <- sum(df$elisa_vsg_is_pos %||% FALSE, na.rm = TRUE)
      n_tested <- sum(df$elisa_vsg_tested %||% FALSE, na.rm = TRUE)
      pct <- if (n_tested > 0) round(100 * n_pos / n_tested, 1) else 0
      
      sprintf("%s / %s (%s%%)", scales::comma(n_pos), scales::comma(n_tested), pct)
    })
    
    output$vb_ielisa_pos <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0 / 0 (0%)")
      
      n_pos <- sum(df$ielisa_is_pos %||% FALSE, na.rm = TRUE)
      n_tested <- sum(df$ielisa_tested %||% FALSE, na.rm = TRUE)
      pct <- if (n_tested > 0) round(100 * n_pos / n_tested, 1) else 0
      
      sprintf("%s / %s (%s%%)", scales::comma(n_pos), scales::comma(n_tested), pct)
    })
    
    output$vb_any_pos <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0 / 0 (0%)")
      
      # Any test positive
      n_pos <- sum(df$Any_pos %||% FALSE, na.rm = TRUE)
      # Total samples with at least one test
      n_tested <- sum(
        (df$PCR_tested | df$elisa_pe_tested | df$elisa_vsg_tested | df$ielisa_tested) %||% FALSE,
        na.rm = TRUE
      )
      pct <- if (n_tested > 0) round(100 * n_pos / n_tested, 1) else 0
      
      sprintf("%s / %s (%s%%)", scales::comma(n_pos), scales::comma(n_tested), pct)
    })
    
    # === TABLES ===
    
    # Test coverage table
    output$tbl_test_coverage <- DT::renderDT({
      df <- joined_data()
      
      if (!nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No lab data loaded")))
      }
      
      coverage <- tibble::tibble(
        Assay = c("PCR", "ELISA PE", "ELISA VSG", "iELISA"),
        `Samples Tested` = c(
          sum(df$PCR_tested %||% FALSE, na.rm = TRUE),
          sum(df$elisa_pe_tested %||% FALSE, na.rm = TRUE),
          sum(df$elisa_vsg_tested %||% FALSE, na.rm = TRUE),
          sum(df$ielisa_tested %||% FALSE, na.rm = TRUE)
        ),
        `Positive Results` = c(
          sum(df$PCR_is_pos %||% FALSE, na.rm = TRUE),
          sum(df$elisa_pe_is_pos %||% FALSE, na.rm = TRUE),
          sum(df$elisa_vsg_is_pos %||% FALSE, na.rm = TRUE),
          sum(df$ielisa_is_pos %||% FALSE, na.rm = TRUE)
        )
      ) %>%
        dplyr::mutate(
          `% Positive` = dplyr::if_else(
            `Samples Tested` > 0,
            round(`Positive Results` / `Samples Tested` * 100, 1),
            0
          )
        )
      
      DT::datatable(
        coverage,
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          '% Positive',
          background = DT::styleColorBar(c(0, 100), '#27AE60'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })
    
    # Overview table with canonical flags
    output$tbl_overview <- DT::renderDT({
      df <- filtered_data()
      
      if (!nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No results for current filters")))
      }
      
      display_df <- df %>%
        dplyr::select(
          barcode, lab_id,
          # PCR
          PCR_Call = PCR_call,
          `177T Cq` = Cq_177T,
          `18S2 Cq` = Cq_18S2,
          `RNAseP Cq` = Cq_RNAseP,
          PCR_Status = PCR_is_pos,
          # ELISA PE
          `PE OD` = d_od_pe,
          `PE PP%` = pp_percent_pe,
          PE_Status = elisa_pe_is_pos,
          # ELISA VSG
          `VSG OD` = d_od_vsg,
          `VSG PP%` = pp_percent_vsg,
          VSG_Status = elisa_vsg_is_pos,
          # iELISA
          `iELISA 1.3%` = pct_inh_13,
          `iELISA 1.5%` = pct_inh_15,
          iELISA_Status = ielisa_is_pos,
          # Overall
          Any_Positive = Any_pos,
          Concordant
        ) %>%
        dplyr::mutate(
          dplyr::across(
            c(PCR_Status, PE_Status, VSG_Status, iELISA_Status, Any_Positive),
            ~dplyr::case_when(
              . == TRUE ~ "POS",
              . == FALSE ~ "NEG",
              TRUE ~ "NA"
            )
          ),
          Concordant = dplyr::case_when(
            Concordant == TRUE ~ "YES",
            Concordant == FALSE ~ "NO",
            TRUE ~ "NA"
          )
        )
      
      DT::datatable(
        display_df,
        options = list(pageLength = 20, scrollX = TRUE),
        filter = "top",
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-weight: bold;",
          sprintf("Showing %s of %s samples with lab results", 
                  scales::comma(nrow(display_df)), 
                  scales::comma(nrow(joined_data())))
        )
      ) %>%
        DT::formatRound(columns = c("177T Cq", "18S2 Cq", "RNAseP Cq",
                                    "PE OD", "PE PP%", "VSG OD", "VSG PP%",
                                    "iELISA 1.3%", "iELISA 1.5%"),
                        digits = 2) %>%
        DT::formatStyle(
          c("PCR_Status", "PE_Status", "VSG_Status", "iELISA_Status", "Any_Positive"),
          backgroundColor = DT::styleEqual(
            c("POS", "NEG", "NA"),
            c("#f8d7da", "#d4edda", "#e2e3e5")
          )
        ) %>%
        DT::formatStyle(
          "Concordant",
          backgroundColor = DT::styleEqual(
            c("YES", "NO", "NA"),
            c("#d4edda", "#fff3cd", "#e2e3e5")
          )
        )
    })
    
    # PCR detail table
    output$tbl_pcr <- DT::renderDT({
      pcr <- lab_data_raw()$pcr
      
      if (is.null(pcr) || !nrow(pcr)) {
        return(DT::datatable(tibble::tibble(Message = "No PCR data loaded")))
      }
      
      # Select only columns that exist
      available_cols <- names(pcr)
      numeric_cols <- available_cols[available_cols %in% c("Cq_177T", "SD_177T", "Cq_18S2", 
                                                             "SD_18S2", "Cq_RNAseP", "SD_RNAseP")]
      
      DT::datatable(
        pcr,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        {if (length(numeric_cols) > 0) DT::formatRound(., columns = numeric_cols, digits = 2) else .}
    })
    
    # ELISA PE table
    output$tbl_elisa_pe <- DT::renderDT({
      elisa_pe <- lab_data_raw()$elisa_pe
      
      if (is.null(elisa_pe) || !nrow(elisa_pe)) {
        return(DT::datatable(tibble::tibble(Message = "No ELISA PE data loaded")))
      }
      
      available_cols <- names(elisa_pe)
      numeric_cols <- available_cols[available_cols %in% c("d_od", "pp_percent", "od_value", 
                                                             "od_sample", "od_nc", "od_pc")]
      
      DT::datatable(
        elisa_pe,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        {if (length(numeric_cols) > 0) DT::formatRound(., columns = numeric_cols, digits = 2) else .}
    })
    
    # ELISA VSG table
    output$tbl_elisa_vsg <- DT::renderDT({
      elisa_vsg <- lab_data_raw()$elisa_vsg
      
      if (is.null(elisa_vsg) || !nrow(elisa_vsg)) {
        return(DT::datatable(tibble::tibble(Message = "No ELISA VSG data loaded")))
      }
      
      available_cols <- names(elisa_vsg)
      numeric_cols <- available_cols[available_cols %in% c("d_od", "pp_percent", "od_value",
                                                             "od_sample", "od_nc", "od_pc")]
      
      DT::datatable(
        elisa_vsg,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        {if (length(numeric_cols) > 0) DT::formatRound(., columns = numeric_cols, digits = 2) else .}
    })
    
    # iELISA table
    output$tbl_ielisa <- DT::renderDT({
      ielisa <- lab_data_raw()$ielisa
      
      if (is.null(ielisa) || !nrow(ielisa)) {
        return(DT::datatable(tibble::tibble(Message = "No iELISA data loaded")))
      }
      
      available_cols <- names(ielisa)
      numeric_cols <- available_cols[available_cols %in% c("pct_inh_13", "pct_inh_15", 
                                                             "od_13", "od_15", "od_sample")]
      
      DT::datatable(
        ielisa,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        {if (length(numeric_cols) > 0) DT::formatRound(., columns = numeric_cols, digits = 1) else .}
    })
    
    # Concordance table
    output$tbl_concordance <- DT::renderDT({
      df <- joined_data()
      
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No data for concordance")))
      }
      
      concordance_details <- df %>%
        dplyr::mutate(
          PCR = dplyr::case_when(
            is.na(PCR_tested) | !PCR_tested ~ "Not Tested",
            PCR_is_pos %||% FALSE ~ "POS",
            TRUE ~ "NEG"
          ),
          ELISA_PE = dplyr::case_when(
            is.na(elisa_pe_tested) | !elisa_pe_tested ~ "Not Tested",
            elisa_pe_is_pos %||% FALSE ~ "POS",
            TRUE ~ "NEG"
          ),
          ELISA_VSG = dplyr::case_when(
            is.na(elisa_vsg_tested) | !elisa_vsg_tested ~ "Not Tested",
            elisa_vsg_is_pos %||% FALSE ~ "POS",
            TRUE ~ "NEG"
          ),
          iELISA = dplyr::case_when(
            is.na(ielisa_tested) | !ielisa_tested ~ "Not Tested",
            ielisa_is_pos %||% FALSE ~ "POS",
            TRUE ~ "NEG"
          ),
          Concordant_Status = dplyr::case_when(
            is.na(Concordant) ~ "Insufficient Data",
            Concordant == TRUE ~ "YES",
            TRUE ~ "NO"
          )
        ) %>%
        dplyr::select(barcode, lab_id, PCR, ELISA_PE, ELISA_VSG, iELISA, Concordant = Concordant_Status) %>%
        dplyr::arrange(barcode)
      
      DT::datatable(
        concordance_details,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'Concordant',
          backgroundColor = DT::styleEqual(
            c("YES", "NO", "Insufficient Data"),
            c('#d4edda', '#f8d7da', '#e2e3e5')
          )
        )
    })
    
    # Concordance summary
    output$tbl_concordance_summary <- shiny::renderTable({
      df <- joined_data()
      
      if (!nrow(df)) {
        return(tibble::tibble(Category = "No data", Count = 0))
      }
      
      df %>%
        dplyr::mutate(
          Agreement = dplyr::case_when(
            Concordant == TRUE ~ "All tests agree",
            Concordant == FALSE ~ "Tests disagree",
            TRUE ~ "Insufficient data"
          )
        ) %>%
        dplyr::count(Agreement) %>%
        dplyr::mutate(
          Percent = round(n / sum(n) * 100, 1)
        ) %>%
        dplyr::rename(Category = Agreement, Count = n) %>%
        dplyr::arrange(dplyr::desc(Count))
    }, striped = TRUE, hover = TRUE)
    
    # Positivity rates
    output$tbl_positivity_rates <- shiny::renderTable({
      df <- joined_data()
      
      if (!nrow(df)) {
        return(tibble::tibble(Assay = "No data", Tested = 0, Positive = 0))
      }
      
      tibble::tibble(
        Assay = c("PCR", "ELISA PE", "ELISA VSG", "iELISA"),
        Tested = c(
          sum(df$PCR_tested %||% FALSE, na.rm = TRUE),
          sum(df$elisa_pe_tested %||% FALSE, na.rm = TRUE),
          sum(df$elisa_vsg_tested %||% FALSE, na.rm = TRUE),
          sum(df$ielisa_tested %||% FALSE, na.rm = TRUE)
        ),
        Positive = c(
          sum(df$PCR_is_pos %||% FALSE, na.rm = TRUE),
          sum(df$elisa_pe_is_pos %||% FALSE, na.rm = TRUE),
          sum(df$elisa_vsg_is_pos %||% FALSE, na.rm = TRUE),
          sum(df$ielisa_is_pos %||% FALSE, na.rm = TRUE)
        )
      ) %>%
        dplyr::mutate(
          `Rate (%)` = dplyr::if_else(
            Tested > 0,
            round(Positive / Tested * 100, 1),
            0
          )
        )
    }, striped = TRUE, hover = TRUE)
    
    # === CONTROLS QC (NEW SECTION) ===
    
    # Extract controls from lab data
controls_data <- shiny::reactive({
        lab <- lab_data_raw()
        cfg <- config()
        
        if (is.null(lab)) {
          return(list(pcr = tibble::tibble(), elisa = tibble::tibble(), ielisa = tibble::tibble()))
        }
        
        # Call the centralized extraction function
        extract_all_controls(lab, cfg)
      })    
    # Filter controls
# In controls_filtered reactive:
      controls_filtered <- shiny::reactive({
          ctrl <- controls_data()  # This is now a list!
      
      # Combine all controls into one dataframe for filtering
      all_controls <- dplyr::bind_rows(
        if (nrow(ctrl$pcr) > 0) ctrl$pcr %>% mutate(assay = "PCR") else NULL,
        if (nrow(ctrl$elisa) > 0) ctrl$elisa else NULL,
        if (nrow(ctrl$ielisa) > 0) ctrl$ielisa %>% mutate(assay = "iELISA") else NULL
      )
      
      if (is.null(controls) || !nrow(controls)) {
        return(tibble::tibble())
      }
      
      tryCatch({
        if (!identical(input$qc_assay, "All")) {
          controls <- controls %>% dplyr::filter(assay == input$qc_assay)
        }
        
        if (!identical(input$qc_control, "All")) {
          controls <- controls %>% dplyr::filter(control_type == input$qc_control)
        }
        
        controls
      }, error = function(e) {
        warning("Failed to filter controls: ", e$message)
        tibble::tibble()
      })
    })
    
    # QC bounds
    qc_bounds <- shiny::reactive({
      tryCatch({
        calculate_qc_bounds(controls_data())
      }, error = function(e) {
        warning("Failed to calculate QC bounds: ", e$message)
        tibble::tibble()
      })
    })
    
    # Controls summary table
    output$qc_controls_summary <- DT::renderDT({
      controls <- controls_data()
      
      if (is.null(controls) || (nrow(controls$pcr) == 0 && nrow(controls$elisa) == 0 && nrow(controls$ielisa) == 0)) {
        return(DT::datatable(tibble::tibble(Message = "No control data available")))
      }
      
      tryCatch({
        # PCR summary
        pcr_summary <- if (nrow(controls$pcr) > 0) {
          qc_pcr_controls_summary(controls$pcr, cp_max_cq = 35)
        } else {
          tibble::tibble()
        }
        
        # ELISA summary
        elisa_summary <- if (nrow(controls$elisa) > 0) {
          qc_elisa_controls(controls$elisa) %>%
            dplyr::group_by(assay_label, control) %>%
            dplyr::summarise(
              n = dplyr::n(),
              mean_pp = mean(pp_percent, na.rm = TRUE),
              sd_pp = sd(pp_percent, na.rm = TRUE),
              passes = sum(pass, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              assay = assay_label,
              control_type = control
            ) %>%
            dplyr::select(assay, control_type, n, mean_pp, sd_pp, passes)
        } else {
          tibble::tibble()
        }
        
        # iELISA summary  
        ielisa_summary <- if (nrow(controls$ielisa) > 0) {
          qc_ielisa_controls(controls$ielisa) %>%
            dplyr::transmute(
              assay = "iELISA",
              control_type = "Summary",
              n = dplyr::n(),
              mean_PI_13 = mean(iL13_PI_CP, na.rm = TRUE),
              mean_PI_15 = mean(iL15_PI_CP, na.rm = TRUE),
              passes = sum(pass_overall, na.rm = TRUE)
            )
        } else {
          tibble::tibble()
        }
        
        # Combine
        if (nrow(pcr_summary) > 0) {
          DT::datatable(
            pcr_summary,
            options = list(pageLength = 20),
            rownames = FALSE
          )
        } else if (nrow(elisa_summary) > 0) {
          DT::datatable(
            elisa_summary,
            options = list(pageLength = 20),
            rownames = FALSE
          ) %>%
            DT::formatRound(columns = c("mean_pp", "sd_pp"), digits = 1)
        } else if (nrow(ielisa_summary) > 0) {
          DT::datatable(
            ielisa_summary,
            options = list(pageLength = 20),
            rownames = FALSE
          ) %>%
            DT::formatRound(columns = c("mean_PI_13", "mean_PI_15"), digits = 1)
        } else {
          DT::datatable(tibble::tibble(Message = "No control summaries available"))
        }
        
      }, error = function(e) {
        DT::datatable(tibble::tibble(Error = paste("Failed to summarize:", e$message)))
      })
    })
    
    # Levey-Jennings plot
    output$qc_lj_plot <- shiny::renderPlot({
      controls <- controls_data()
      
      if (is.null(controls)) {
        plot.new()
        text(0.5, 0.5, "No control data available", cex = 1.2, col = "gray50")
        return()
      }
      
      tryCatch({
        # Determine which plot to show based on filter
        if (input$qc_assay %in% c("All", "PCR") && nrow(controls$pcr) > 0) {
          plot_pcr_controls(controls$pcr, cp_max_cq = 35)
        } else if (grepl("ELISA", input$qc_assay) && nrow(controls$elisa) > 0) {
          elisa_qc <- qc_elisa_controls(controls$elisa)
          if (input$qc_assay != "All") {
            elisa_qc <- elisa_qc %>% dplyr::filter(assay_label == input$qc_assay)
          }
          if (input$qc_control != "All") {
            elisa_qc <- elisa_qc %>% dplyr::filter(control == input$qc_control)
          }
          plot_elisa_controls(elisa_qc)
        } else if (input$qc_assay == "iELISA" && nrow(controls$ielisa) > 0) {
          ielisa_qc <- qc_ielisa_controls(controls$ielisa, min_pi = 60)
          plot_ielisa_controls(ielisa_qc, min_pi = 60)
        } else {
          plot.new()
          text(0.5, 0.5, "No data for selected filters", cex = 1.2, col = "gray50")
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
      })
    })
    
    # Raw controls data
    output$qc_controls_raw <- DT::renderDT({
      controls <- controls_data()
      
      if (is.null(controls)) {
        return(DT::datatable(tibble::tibble(Message = "No control data available")))
      }
      
      tryCatch({
        # Combine all control data for display
        all_controls <- dplyr::bind_rows(
          if (nrow(controls$pcr) > 0) {
            controls$pcr %>% dplyr::mutate(assay = "PCR")
          } else NULL,
          if (nrow(controls$elisa) > 0) {
            controls$elisa %>% dplyr::select(
              file, assay = assay_label, plate, 
              control_type = control, d_od, pp_percent
            ) %>%
              dplyr::mutate(assay = as.character(assay))
          } else NULL
        )
        
        if (is.null(all_controls) || !nrow(all_controls)) {
          return(DT::datatable(tibble::tibble(Message = "No control data to display")))
        }
        
        # Apply filters
        if (input$qc_assay != "All") {
          all_controls <- all_controls %>% 
            dplyr::filter(grepl(input$qc_assay, assay, ignore.case = TRUE))
        }
        
        if (input$qc_control != "All") {
          all_controls <- all_controls %>%
            dplyr::filter(control_type == input$qc_control | control == input$qc_control)
        }
        
        available_cols <- names(all_controls)
        numeric_cols <- available_cols[available_cols %in% c("d_od", "pp_percent", "cq", 
                                                               "Cq_177T", "Cq_18S2", "Cq_RNAseP", 
                                                               "min_Cq_tryp")]
        
        DT::datatable(
          all_controls,
          options = list(pageLength = 25, scrollX = TRUE),
          filter = "top",
          rownames = FALSE
        ) %>%
          {if (length(numeric_cols) > 0) DT::formatRound(., columns = numeric_cols, digits = 3) else .}
      }, error = function(e) {
        DT::datatable(tibble::tibble(Error = paste("Failed to display controls:", e$message)))
      })
    })
    
    # === DOWNLOADS ===
    
    output$dl_overview <- shiny::downloadHandler(
      filename = function() paste0("lab_overview_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) readr::write_csv(filtered_data(), file)
    )
    
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
    
    output$dl_controls <- shiny::downloadHandler(
      filename = function() paste0("controls_qc_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        controls <- controls_data()
        if (!is.null(controls)) {
          all_ctrls <- dplyr::bind_rows(
            if (nrow(controls$pcr) > 0) controls$pcr %>% dplyr::mutate(assay = "PCR") else NULL,
            if (nrow(controls$elisa) > 0) controls$elisa else NULL
          )
          readr::write_csv(all_ctrls, file)
        }
      }
    )
    
    # Return joined data for use by other modules (e.g., map)
    return(list(
      lab_joined = joined_data
    ))
  })
}

# Safe NULL coalesce operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
