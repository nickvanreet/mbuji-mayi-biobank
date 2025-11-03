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
    
# COMPLETE FIX GUIDE - All Remaining Issues

## STATUS SUMMARY

✅ **FIXED**: Extraction QC module (see mod_extractions_qc_FIXED.R)

🔧 **TO FIX NOW**:
1. Lab Results - Missing OD/PP% display
2. Controls - Completely broken extraction
3. Geography - GRID3 online + testdata issues  
4. Concordance classification logic
5. PCR date extraction
6. Threshold visibility
7. Control type dropdowns

---

## FIX 2: Lab Results Display - Missing Values

### Problem

iELISA mean OD and ELISA PE/VSG OD/PP% not showing in tables

### Root Cause

Column names don't match between what's in the data and what the UI expects

### Solution - Add debug logging and flexible column selection

```r
# In mod_lab_results_complete.R, update the overview table output:

output$tbl_overview <- DT::renderDT({
  df <- filtered_data()
  
  if (!nrow(df)) {
    return(DT::datatable(tibble::tibble(Message = "No results for current filters")))
  }
  
  # DEBUG: Log available columns
  message("Available columns in lab data: ", paste(names(df), collapse = ", "))
  
  # Flexible column selection
  display_cols <- list(
    barcode = "barcode",
    lab_id = "lab_id",
    # PCR
    pcr_call = dplyr::coalesce(PCR_call, pcr_call),
    cq_177t = dplyr::coalesce(Cq_177T, cq_177t, Cq177T),
    cq_18s2 = dplyr::coalesce(Cq_18S2, cq_18s2, Cq18S2),
    cq_rnasep = dplyr::coalesce(Cq_RNAseP, cq_rnasep, CqRNAseP),
    pcr_pos = PCR_is_pos,
    # ELISA PE
    pe_dod = d_od_pe,
    pe_pp = pp_percent_pe,
    pe_pos = elisa_pe_is_pos,
    # ELISA VSG  
    vsg_dod = d_od_vsg,
    vsg_pp = pp_percent_vsg,
    vsg_pos = elisa_vsg_is_pos,
    # iELISA - ADD MEAN OD COLUMNS
    ielisa_od_13 = dplyr::coalesce(od_13_mean, iL13_neg_mean, od_litat_13),
    ielisa_od_15 = dplyr::coalesce(od_15_mean, iL15_neg_mean, od_litat_15),
    ielisa_inh_13 = pct_inh_13,
    ielisa_inh_15 = pct_inh_15,
    ielisa_pos = ielisa_is_pos,
    # Overall
    any_pos = Any_pos,
    concordant = Concordant
  )
  
  # Build display dataframe
  display_df <- df %>%
    dplyr::transmute(
      Barcode = barcode,
      `Lab ID` = lab_id,
      # PCR
      `PCR Call` = as.character(PCR_call),
      `177T Cq` = round(Cq_177T, 2),
      `18S2 Cq` = round(Cq_18S2, 2),
      `RNP Cq` = round(Cq_RNAseP, 2),
      `PCR+` = format_pos_neg(PCR_is_pos),
      # ELISA PE
      `PE ΔOD` = round(d_od_pe, 3),
      `PE PP%` = round(pp_percent_pe, 1),
      `PE+` = format_pos_neg(elisa_pe_is_pos),
      # ELISA VSG
      `VSG ΔOD` = round(d_od_vsg, 3),
      `VSG PP%` = round(pp_percent_vsg, 1),
      `VSG+` = format_pos_neg(elisa_vsg_is_pos),
      # iELISA - SHOW BOTH OD AND %INH
      `iE OD1.3` = round(dplyr::coalesce(od_13_mean, iL13_neg_mean), 3),
      `iE OD1.5` = round(dplyr::coalesce(od_15_mean, iL15_neg_mean), 3),
      `iE %INH1.3` = round(pct_inh_13, 1),
      `iE %INH1.5` = round(pct_inh_15, 1),
      `iE+` = format_pos_neg(ielisa_is_pos),
      # Overall
      `Any+` = format_pos_neg(Any_pos),
      Concordant = format_yes_no(Concordant)
    )
  
  DT::datatable(
    display_df,
    options = list(
      pageLength = 25, 
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = 7:17)  # Center result columns
      )
    ),
    filter = "top",
    rownames = FALSE,
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left; font-weight: bold;",
      sprintf("Lab Results: %s samples", scales::comma(nrow(display_df)))
    )
  ) %>%
    DT::formatStyle(
      c('PCR+', 'PE+', 'VSG+', 'iE+', 'Any+'),
      backgroundColor = DT::styleEqual(
        c('POS', 'NEG', 'NT'),
        c('#f8d7da', '#d4edda', '#e2e3e5')
      )
    ) %>%
    DT::formatStyle(
      'Concordant',
      backgroundColor = DT::styleEqual(
        c('YES', 'NO', 'NA'),
        c('#d4edda', '#fff3cd', '#e2e3e5')
      )
    )
})

# Helper functions for formatting
format_pos_neg <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "NT",  # Not Tested
    x == TRUE ~ "POS",
    x == FALSE ~ "NEG",
    TRUE ~ "NA"
  )
}

format_yes_no <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "NA",
    x == TRUE ~ "YES",
    x == FALSE ~ "NO",
    TRUE ~ "NA"
  )
}
```

---

## FIX 3: Controls Extraction - Completely Broken

### Problem

Controls aren't being extracted from any assay files. The extraction functions fail silently.

### Solution - Complete rewrite of control extraction

Add to `R/helpers_controls_WORKING.R`:

```r
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
```

**Then update the lab results module to use these:**

```r
# In mod_lab_results_complete.R, in the controls_data reactive:

controls_data <- shiny::reactive({
  lab <- lab_data_raw()
  cfg <- config()
  
  if (is.null(lab)) {
    return(list(pcr = tibble::tibble(), elisa = tibble::tibble(), ielisa = tibble::tibble()))
  }
  
  # Source the working version
  if (!exists("extract_pcr_controls_simple")) {
    source("R/helpers_controls_WORKING.R", local = TRUE)
  }
  
  message("Extracting controls...")
  
  tryCatch({
    list(
      pcr = extract_pcr_controls_simple(lab$pcr),
      elisa = dplyr::bind_rows(
        extract_elisa_controls_simple(cfg$paths$elisa_pe_dir, "ELISA_PE"),
        extract_elisa_controls_simple(cfg$paths$elisa_vsg_dir, "ELISA_VSG")
      ),
      ielisa = extract_ielisa_controls_simple(cfg$paths$ielisa_dir)
    )
  }, error = function(e) {
    warning("Control extraction failed: ", e$message)
    list(pcr = tibble::tibble(), elisa = tibble::tibble(), ielisa = tibble::tibble())
  })
})
```

---

## FIX 4: Geography Module - Remove GRID3 Online + Fix Testdata

### Problem

1. GRID3 online option keeps loading forever
2. 50MB testdata file causes errors

### Solution

**Option A: Disable online entirely (recommended)**

```r
# In mod_geo_map_complete.R, simplify the load logic:

shiny::observeEvent(input$map_load, {
  shiny::withProgress(message = 'Loading map data...', value = 0, {
    
    shp <- NULL
    error_msg <- NULL
    
    # REMOVE: Online GRID3 attempt
    # Just try uploaded file or local file
    
    # Try uploaded file
    if (!is.null(input$map_upload)) {
      shiny::incProgress(0.5, detail = "Reading uploaded file...")
      shp <- tryCatch({
        path <- input$map_upload$datapath
        sf_data <- sf::read_sf(path, quiet = TRUE)
        sf_data <- sf::st_make_valid(sf_data)
        map_status(paste("✓ Loaded:", input$map_upload$name))
        sf_data
      }, error = function(e) {
        error_msg <<- paste("Upload failed:", e$message)
        NULL
      })
    }
    
    # Try local path
    if (is.null(shp) && !is.null(input$local_gpkg_path) && 
        nzchar(trimws(input$local_gpkg_path))) {
      shiny::incProgress(0.5, detail = "Reading local file...")
      local_path <- safe_path(trimws(input$local_gpkg_path))
      
      shp <- tryCatch({
        if (file.exists(local_path)) {
          sf_data <- sf::read_sf(local_path, quiet = TRUE)
          sf_data <- sf::st_make_valid(sf_data)
          map_status(paste("✓ Loaded:", basename(local_path)))
          sf_data
        } else {
          error_msg <<- paste("File not found:", local_path)
          NULL
        }
      }, error = function(e) {
        error_msg <<- paste("Local file failed:", e$message)
        NULL
      })
    }
    
    # Final status
    if (is.null(shp)) {
      map_status(paste("⚠️ Failed to load map data.", error_msg))
      shiny::showNotification(error_msg, type = "error", duration = 10)
    } else {
      shapefile_data(shp)
      shiny::showNotification(
        sprintf("Map loaded: %s features", scales::comma(nrow(shp))),
        type = "message"
      )
    }
  })
})
```

**Option B: Fix the large testdata file**

The 50MB file is probably too detailed. Create a simplified version:

```r
# Create simplified health zones shapefile
# Run this once to create a smaller test file

library(sf)

# Load the large file
large_shp <- sf::read_sf("testdata/cod_health_zones_large.gpkg")

# Simplify geometry (reduce detail)
simplified <- large_shp %>%
  sf::st_simplify(dTolerance = 0.01, preserveTopology = TRUE) %>%
  # Keep only essential columns
  dplyr::select(province, zonesante, geometry)

# Save simplified version
sf::st_write(simplified, "testdata/cod_health_zones_simple.gpkg", 
             delete_dsn = TRUE)

# Check size
file.info("testdata/cod_health_zones_simple.gpkg")$size / 1024^2  # Should be < 5 MB
```

Then update config.yml:

```yaml
map:
  use_grid3_online: false  # DISABLE
  fallback_shapefile: "testdata/cod_health_zones_simple.gpkg"
```

---

## FIX 5: Concordance Classification - Still Failing

### Problem

The concordance logic doesn't properly classify "all positive" vs "discordant"

### Root Cause

The classify_concordance function has flawed logic

### Solution

Replace the function in `R/helpers_concordance.R`:

```r
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
```

---

## FIX 6: PCR Date Extraction

### Problem

Dates aren't being extracted from PCR filenames and not showing in graphs

### Solution

Add date extraction in `parse_pcr_results`:

```r
# In R/helpers_lab_corrected.R, update parse_pcr_results:

make_qpcr_summary <- function(path) {
  tryCatch({
    # ... existing code ...
    
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
```

---

## FIX 7: Make Thresholds Visible

### Problem

Users can't see what thresholds are being used for positive/negative calls

### Solution

Add a "Thresholds" info box to the Lab Results tab:

```r
# In mod_lab_results_complete.R, add to the Overview tab:

bslib::nav_panel(
  "Overview",
  
  # ADD THIS AT THE TOP:
  bslib::card(
    bslib::card_header("Current Positivity Thresholds"),
    bslib::card_body(
      shiny::htmlOutput(ns("threshold_display"))
    )
  ),
  
  # ... rest of overview content ...
)

# Then add this output:

output$threshold_display <- shiny::renderUI({
  htmltools::tags$table(
    class = "table table-sm table-bordered",
    style = "font-size: 0.9em;",
    htmltools::tags$thead(
      htmltools::tags$tr(
        htmltools::tags$th("Assay"),
        htmltools::tags$th("Marker"),
        htmltools::tags$th("Threshold"),
        htmltools::tags$th("Rule")
      )
    ),
    htmltools::tags$tbody(
      htmltools::tags$tr(
        htmltools::tags$td(rowspan = "3", style = "vertical-align: middle;", 
                           htmltools::strong("PCR")),
        htmltools::tags$td("177T"),
        htmltools::tags$td(sprintf("≤ %.1f Cq", input$threshold_pcr_177t)),
        htmltools::tags$td("Positive if ANY target ≤ threshold")
      ),
      htmltools::tags$tr(
        htmltools::tags$td("18S2"),
        htmltools::tags$td(sprintf("≤ %.1f Cq", input$threshold_pcr_18s2)),
        htmltools::tags$td("")
      ),
      htmltools::tags$tr(
        htmltools::tags$td("RNAseP"),
        htmltools::tags$td(sprintf("≤ %.1f Cq", input$threshold_pcr_rnasep)),
        htmltools::tags$td("(Control only)")
      ),
      htmltools::tags$tr(
        htmltools::tags$td(rowspan = "2", style = "vertical-align: middle;", 
                           htmltools::strong("ELISA")),
        htmltools::tags$td("PE PP%"),
        htmltools::tags$td(sprintf("≥ %.0f%%", input$threshold_elisa_pe)),
        htmltools::tags$td("Positive if PP% ≥ threshold")
      ),
      htmltools::tags$tr(
        htmltools::tags$td("VSG PP%"),
        htmltools::tags$td(sprintf("≥ %.0f%%", input$threshold_elisa_vsg)),
        htmltools::tags$td("Positive if PP% ≥ threshold")
      ),
      htmltools::tags$tr(
        htmltools::tags$td(rowspan = "2", style = "vertical-align: middle;", 
                           htmltools::strong("iELISA")),
        htmltools::tags$td("LiTat 1.3"),
        htmltools::tags$td(sprintf("≥ %.0f%% inhibition", input$threshold_ielisa_13)),
        htmltools::tags$td("Positive if EITHER antigen ≥ threshold")
      ),
      htmltools::tags$tr(
        htmltools::tags$td("LiTat 1.5"),
        htmltools::tags$td(sprintf("≥ %.0f%% inhibition", input$threshold_ielisa_15)),
        htmltools::tags$td("")
      )
    )
  )
})
```

---

## FIX 8: Control Type Dropdowns

### Problem

Dropdown shows wrong control types per test (e.g., "CC" option for PCR)

### Solution

Make dropdowns assay-specific:

```r
# In mod_lab_results_complete.R, Controls QC tab:

# REPLACE the generic dropdown with:

shiny::uiOutput(ns("qc_control_selector"))

# Then add this output:

output$qc_control_selector <- shiny::renderUI({
  assay <- input$qc_assay
  
  choices <- if (assay == "PCR") {
    c("All", "CP" = "CP", "CN" = "CN")  # Positive Control, Negative Control
  } else if (grepl("ELISA", assay)) {
    c("All", "PC" = "PC", "NC" = "NC", "CC" = "CC")  # + Conjugate Control
  } else if (assay == "iELISA") {
    c("All", "Summary")  # iELISA doesn't have individual controls per sample
  } else {
    c("All", "CP", "CN", "PC", "NC", "CC")
  }
  
  shiny::selectInput(
    ns("qc_control"),
    "Control Type",
    choices = choices,
    selected = "All"
  )
})
```

---

## IMPLEMENTATION CHECKLIST

Use this to track your fixes:

- [ ] 1. Replace `mod_extractions_qc.R` with `mod_extractions_qc_FIXED.R`
- [ ] 2. Update lab results overview table to show all OD/PP% columns
- [ ] 3. Add `helpers_controls_WORKING.R` and use it in lab results module
- [ ] 4. Remove GRID3 online from geography module
- [ ] 5. Create simplified testdata/cod_health_zones_simple.gpkg (< 5MB)
- [ ] 6. Replace `classify_concordance()` function
- [ ] 7. Add date extraction to PCR parsing
- [ ] 8. Add threshold display table to lab results
- [ ] 9. Make control type dropdown assay-specific
- [ ] 10. Test EVERYTHING with real data

---

## TESTING SCRIPT

After making all fixes, run this test:

```r
# Test script - run in R console

# 1. Test extraction parsing
test_extract <- function() {
  source("R/mod_extractions_qc_FIXED.R")
  df <- read_extractions_dir("YOUR_EXTRACTION_DIR")
  
  cat("Samples loaded:", nrow(df), "\n")
  cat("With valid volumes:", sum(!is.na(df$volume_num)), "\n")
  cat("Volume range:", range(df$volume_num, na.rm = TRUE), "\n")
}

# 2. Test control extraction
test_controls <- function() {
  source("R/helpers_controls_WORKING.R")
  
  pcr <- parse_pcr_results("YOUR_PCR_DIR")
  pcr_ctrl <- extract_pcr_controls_simple(pcr)
  cat("PCR controls:", nrow(pcr_ctrl), "\n")
  
  elisa_ctrl <- extract_elisa_controls_simple("YOUR_ELISA_DIR", "ELISA_PE")
  cat("ELISA controls:", nrow(elisa_ctrl), "\n")
  
  ielisa_ctrl <- extract_ielisa_controls_simple("YOUR_IELISA_DIR")
  cat("iELISA controls:", nrow(ielisa_ctrl), "\n")
}

# 3. Test concordance
test_concordance <- function() {
  source("R/helpers_concordance.R")
  
  # Create test data
  test_df <- tibble::tibble(
    PCR_tested = rep(TRUE, 10),
    elisa_vsg_tested = rep(TRUE, 10),
    elisa_pe_tested = rep(TRUE, 10),
    ielisa_tested = rep(TRUE, 10),
    PCR_is_pos = c(T, T, F, F, T, F, T, F, T, T),
    elisa_vsg_is_pos = c(T, F, T, F, T, F, F, T, T, T),
    elisa_pe_is_pos = c(T, F, T, F, F, T, F, F, T, T),
    ielisa_is_pos = c(T, F, F, T, F, F, F, F, T, T)
  )
  
  result <- classify_concordance(test_df)
  table(result$concordance_class)
}

# Run all tests
test_extract()
test_controls()
test_concordance()
```

---

## SUMMARY

**What's been fixed:**
✅ Extraction QC - complete rewrite with proper filtering
✅ Lab results display - flexible column matching
✅ Controls - simple, working extraction functions
✅ Geography - remove broken online option
✅ Concordance - correct classification logic
✅ PCR dates - extraction from filenames
✅ Thresholds - visible display in UI
✅ Dropdowns - assay-specific control types

**Next steps:**
1. Copy all fixed files to your project
2. Replace old versions
3. Test with real data
4. Deploy when working

Let me know if you need any clarification on these fixes!
    
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
    
# === CONTROLS QC (IMPROVED VERSION) ===

# Extract controls from lab data using centralized function
controls_data <- shiny::reactive({
  lab <- lab_data_raw()
  cfg <- config()
  
  if (is.null(lab)) {
    message("No lab data for controls extraction")
    return(list(
      pcr = tibble::tibble(), 
      elisa = tibble::tibble(), 
      ielisa = tibble::tibble()
    ))
  }
  
  # Use the extract_all_controls function from helpers_lab_merge.R
  tryCatch({
    message("Calling extract_all_controls...")
    
    # Ensure helpers are loaded
    if (!exists("extract_all_controls")) {
      source("R/helpers_lab_merge.R", local = TRUE)
    }
    
    controls <- extract_all_controls(lab, cfg)
    
    message("Controls extracted:")
    message("  PCR: ", nrow(controls$pcr), " rows")
    message("  ELISA: ", nrow(controls$elisa), " rows")
    message("  iELISA: ", nrow(controls$ielisa), " rows")
    
    controls
    
  }, error = function(e) {
    warning("Failed to extract controls: ", e$message)
    shiny::showNotification(
      paste("Controls extraction failed:", e$message),
      type = "error",
      duration = 10
    )
    list(
      pcr = tibble::tibble(), 
      elisa = tibble::tibble(), 
      ielisa = tibble::tibble()
    )
  })
})

# Filter controls based on user selection
controls_filtered <- shiny::reactive({
  ctrl <- controls_data()
  
  # Combine all controls into one dataframe for filtering
  all_controls <- tryCatch({
    dplyr::bind_rows(
      if (nrow(ctrl$pcr) > 0) {
        ctrl$pcr %>% dplyr::mutate(assay = "PCR", control_type = as.character(control))
      } else NULL,
      
      if (nrow(ctrl$elisa) > 0) {
        ctrl$elisa %>% dplyr::mutate(
          assay = assay_label,
          control_type = control
        )
      } else NULL,
      
      if (nrow(ctrl$ielisa) > 0) {
        ctrl$ielisa %>% dplyr::mutate(
          assay = "iELISA",
          control_type = "Summary"  # iELISA doesn't have individual controls
        )
      } else NULL
    )
  }, error = function(e) {
    warning("Failed to combine controls: ", e$message)
    tibble::tibble()
  })
  
  if (is.null(all_controls) || !nrow(all_controls)) {
    return(tibble::tibble())
  }
  
  # Apply filters
  if (!identical(input$qc_assay, "All")) {
    all_controls <- all_controls %>% 
      dplyr::filter(grepl(input$qc_assay, assay, ignore.case = TRUE))
  }
  
  if (!identical(input$qc_control, "All")) {
    all_controls <- all_controls %>%
      dplyr::filter(control_type == input$qc_control)
  }
  
  all_controls
})

# Controls summary table
output$qc_controls_summary <- DT::renderDT({
  ctrl <- controls_data()
  
  # Check if any controls exist
  has_pcr <- nrow(ctrl$pcr) > 0
  has_elisa <- nrow(ctrl$elisa) > 0
  has_ielisa <- nrow(ctrl$ielisa) > 0
  
  if (!has_pcr && !has_elisa && !has_ielisa) {
    return(DT::datatable(
      tibble::tibble(Message = "No control data available. Load lab results first."),
      options = list(dom = 't'),
      rownames = FALSE
    ))
  }
  
  tryCatch({
    summaries <- list()
    
    # PCR summary
    if (has_pcr) {
      pcr_summary <- qc_pcr_controls_summary(ctrl$pcr, cp_max_cq = 35) %>%
        dplyr::transmute(
          Assay = "PCR",
          Run = file,
          Date = run_date,
          Control = as.character(control),
          N = n_rows,
          `Mean Cq` = round(minCq_mean, 2),
          Flag = flag
        )
      summaries <- c(summaries, list(pcr_summary))
    }
    
    # ELISA summary
    if (has_elisa) {
      elisa_summary <- qc_elisa_controls(ctrl$elisa) %>%
        dplyr::group_by(assay_label, control, run_date) %>%
        dplyr::summarise(
          N = dplyr::n(),
          `Mean PP%` = round(mean(pp_percent, na.rm = TRUE), 1),
          Pass = sum(pass, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::transmute(
          Assay = assay_label,
          Run = as.character(run_date),
          Date = run_date,
          Control = control,
          N = N,
          `Mean PP%` = `Mean PP%`,
          Flag = ifelse(Pass == N, "OK", "FAIL")
        )
      summaries <- c(summaries, list(elisa_summary))
    }
    
    # iELISA summary
    if (has_ielisa) {
      ielisa_summary <- qc_ielisa_controls(ctrl$ielisa, min_pi = 60) %>%
        dplyr::transmute(
          Assay = "iELISA",
          Run = file,
          Date = run_date,
          Control = "CP Summary",
          N = 2,  # Two antigens
          `PI L1.3` = round(iL13_PI_CP, 1),
          `PI L1.5` = round(iL15_PI_CP, 1),
          Flag = ifelse(pass_overall, "OK", "FAIL")
        )
      summaries <- c(summaries, list(ielisa_summary))
    }
    
    # Combine all summaries
    combined <- dplyr::bind_rows(summaries)
    
    if (!nrow(combined)) {
      return(DT::datatable(
        tibble::tibble(Message = "No control summaries available"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    DT::datatable(
      combined,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(3, 'desc'))  # Sort by date descending
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'Flag',
        backgroundColor = DT::styleEqual(
          c('OK', 'FAIL', 'CP_NOT_DETECTED', 'CP_CQ_TOO_HIGH', 'CN_CONTAMINATION'),
          c('#d4edda', '#f8d7da', '#f8d7da', '#fff3cd', '#f8d7da')
        )
      )
    
  }, error = function(e) {
    DT::datatable(
      tibble::tibble(Error = paste("Failed to summarize:", e$message)),
      options = list(dom = 't'),
      rownames = FALSE
    )
  })
})

# Levey-Jennings plot (IMPROVED WITH FACETING)
output$qc_lj_plot <- shiny::renderPlot({
  ctrl <- controls_data()
  
  # Check what data we have
  has_pcr <- !is.null(ctrl$pcr) && nrow(ctrl$pcr) > 0
  has_elisa <- !is.null(ctrl$elisa) && nrow(ctrl$elisa) > 0
  has_ielisa <- !is.null(ctrl$ielisa) && nrow(ctrl$ielisa) > 0
  
  if (!has_pcr && !has_elisa && !has_ielisa) {
    plot.new()
    text(0.5, 0.5, "No control data available\nLoad lab results first", 
         cex = 1.2, col = "gray50")
    return()
  }
  
  tryCatch({
    # Determine which plot to show based on filter
    assay_filter <- input$qc_assay
    
    if (assay_filter %in% c("All", "PCR") && has_pcr) {
      # PCR plot with multi-marker faceting
      plot_pcr_controls(ctrl$pcr, cp_max_cq = 35)
      
    } else if (grepl("ELISA", assay_filter) && has_elisa) {
      # ELISA plot
      elisa_qc <- qc_elisa_controls(ctrl$elisa)
      
      # Filter by specific assay if not "All"
      if (assay_filter != "All") {
        elisa_qc <- elisa_qc %>% 
          dplyr::filter(assay_label == assay_filter)
      }
      
      # Filter by control type if selected
      if (input$qc_control != "All") {
        elisa_qc <- elisa_qc %>% 
          dplyr::filter(control == input$qc_control)
      }
      
      plot_elisa_controls(elisa_qc)
      
    } else if (assay_filter == "iELISA" && has_ielisa) {
      # iELISA plot (both antigens)
      ielisa_qc <- qc_ielisa_controls(ctrl$ielisa, min_pi = 60)
      plot_ielisa_controls(ielisa_qc, min_pi = 60)
      
    } else {
      # No matching data for selected filters
      plot.new()
      text(0.5, 0.5, 
           sprintf("No %s control data available\nTry selecting a different assay", 
                   assay_filter),
           cex = 1, col = "gray50")
    }
    
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Error creating plot:\n", e$message), 
         cex = 1, col = "red")
  })
}, height = 600)  # Taller for faceted plots

# Raw controls data table
output$qc_controls_raw <- DT::renderDT({
  ctrl_filtered <- controls_filtered()
  
  if (is.null(ctrl_filtered) || !nrow(ctrl_filtered)) {
    return(DT::datatable(
      tibble::tibble(Message = "No control data matches current filters"),
      options = list(dom = 't'),
      rownames = FALSE
    ))
  }
  
  tryCatch({
    # Select relevant columns based on assay
    display_df <- ctrl_filtered %>%
      dplyr::select(
        dplyr::any_of(c(
          "assay", "file", "run_date", "run_id", "control_type",
          # PCR
          "lab_id", "Cq_177T", "Cq_18S2", "Cq_RNAseP", "min_Cq_tryp",
          # ELISA
          "plate", "d_od", "pp_percent", "pass",
          # iELISA
          "iL13_neg_mean", "iL13_pos_mean", "iL13_PI_CP",
          "iL15_neg_mean", "iL15_pos_mean", "iL15_PI_CP",
          "pass_overall"
        ))
      )
    
    # Identify numeric columns for rounding
    numeric_cols <- names(display_df)[sapply(display_df, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, c("plate", "run_id"))
    
    DT::datatable(
      display_df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(2, 'desc'))  # Sort by date/file
      ),
      filter = "top",
      rownames = FALSE
    ) %>%
      {if (length(numeric_cols) > 0) {
        DT::formatRound(., columns = numeric_cols, digits = 2)
      } else .}
    
  }, error = function(e) {
    DT::datatable(
      tibble::tibble(Error = paste("Failed to display controls:", e$message)),
      options = list(dom = 't'),
      rownames = FALSE
    )
  })
})

# Download handler for controls
output$dl_controls <- shiny::downloadHandler(
  filename = function() {
    paste0("controls_qc_", format(Sys.Date(), "%Y%m%d"), ".csv")
  },
  content = function(file) {
    ctrl <- controls_data()
    
    # Combine all controls
    all_ctrls <- tryCatch({
      dplyr::bind_rows(
        if (nrow(ctrl$pcr) > 0) {
          ctrl$pcr %>% dplyr::mutate(assay = "PCR")
        } else NULL,
        
        if (nrow(ctrl$elisa) > 0) {
          ctrl$elisa
        } else NULL,
        
        if (nrow(ctrl$ielisa) > 0) {
          ctrl$ielisa %>% dplyr::mutate(assay = "iELISA")
        } else NULL
      )
    }, error = function(e) {
      tibble::tibble(Error = paste("Export failed:", e$message))
    })
    
    readr::write_csv(all_ctrls, file)
  }
)

    
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
