# =====================================================================
# MODULE: Lab Results v2 — integrates PCR, ELISA (PE + VSG), iELISA, QC
# =====================================================================

mod_lab_results2_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_columns(
    col_widths = c(4, 8),

    bslib::card(
      bslib::card_header("Lab Results Controls"),

      h6("Source folders"),
      helpText("Update any folder paths that have moved before loading the lab results."),
      textInput(ns("pcr_dir"), "PCR folder", placeholder = "Path to PCR Excel files"),
      textInput(ns("elisa_pe_dir"), "ELISA PE folder", placeholder = "Path to ELISA PE Excel files"),
      textInput(ns("elisa_vsg_dir"), "ELISA VSG folder", placeholder = "Path to ELISA VSG Excel files"),
      textInput(ns("ielisa_dir"), "iELISA folder", placeholder = "Path to iELISA Excel files"),

      bslib::layout_columns(
        col_widths = c(6, 6),
        actionButton(ns("reload_lab"), "Load lab data", icon = bsicons::bs_icon("clipboard"),
                     class = "btn-primary w-100"),
        uiOutput(ns("lab_loaded_at"))
      ),

      hr(),
      h6("Positivity thresholds"),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        numericInput(ns("threshold_pcr"), "PCR Cq max", value = 38, min = 1, max = 45, step = 1),
        numericInput(ns("threshold_elisa"), "ELISA PP%", value = 50, min = 0, max = 100, step = 1),
        numericInput(ns("threshold_ielisa"), "iELISA % inhibition", value = 35, min = 0, max = 100, step = 1)
      ),

      hr(),
      h6("Search"),
      bslib::layout_columns(
        col_widths = c(8, 4),
        textInput(ns("barcode_search"), "Barcode / Lab ID", placeholder = "e.g. KPS001, 2401006"),
        actionButton(ns("clear_search"), NULL, icon = bsicons::bs_icon("clipboard"),
                     class = "btn-outline-secondary mt-4", title = "Clear search")
      ),

      hr(),
      uiOutput(ns("lab_status_ui"))
    ),

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
              value = textOutput(ns("vb_pcr_pos")),
              showcase = bsicons::bs_icon("clipboard"),
              theme = "danger"
            ),
            bslib::value_box(
              title = "iELISA Positive",
              value = textOutput(ns("vb_ielisa_pos")),
              showcase = bsicons::bs_icon("clipboard"),
              theme = "info"
            ),
            bslib::value_box(
              title = "Concordant Results",
              value = textOutput(ns("vb_concordant")),
              showcase = bsicons::bs_icon("clipboard"),
              theme = "success"
            )
          ),
          DT::DTOutput(ns("tbl_overview"))
        ),
        bslib::nav_panel(
          "PCR",
          downloadButton(ns("dl_pcr"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_pcr"))
        ),
        bslib::nav_panel(
          "ELISA PE",
          downloadButton(ns("dl_elisa_pe"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_elisa_pe"))
        ),
        bslib::nav_panel(
          "ELISA VSG",
          downloadButton(ns("dl_elisa_vsg"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_elisa_vsg"))
        ),
        bslib::nav_panel(
          "iELISA",
          downloadButton(ns("dl_ielisa"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_ielisa"))
        ),
        bslib::nav_panel(
          "Concordance",
          downloadButton(ns("dl_concordance"), "Download", class = "btn-sm mb-2"),
          DT::DTOutput(ns("tbl_concordance"))
        ),
        bslib::nav_panel(
          "Controls / QC",
          h5("PCR Controls"),
          DT::DTOutput(ns("qc_pcr")),
          hr(),
          h5("ELISA PE Controls"),
          DT::DTOutput(ns("qc_elisa_pe")),
          hr(),
          h5("ELISA VSG Controls"),
          DT::DTOutput(ns("qc_elisa_vsg")),
          hr(),
          h5("iELISA Controls"),
          DT::DTOutput(ns("qc_ielisa"))
        )
      )
    )
  )
}

mod_lab_results2_server <- function(id, biobank_clean, config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    normalise_dir_input <- function(value) {
      if (is.null(value) || !length(value)) return("")
      val <- as.character(value)[1]
      if (is.na(val)) return("")
      val <- trimws(val)
      if (!nzchar(val)) return("")
      if (exists("safe_path", mode = "function")) {
        val <- tryCatch(safe_path(val), error = function(e) val)
      }
      if (is.null(val) || !nzchar(val)) return("")
      val
    }

    update_dir_inputs <- function(dirs) {
      defaults <- list(pcr = "", elisa_pe = "", elisa_vsg = "", ielisa = "")
      dirs <- utils::modifyList(defaults, dirs)
      updateTextInput(session, "pcr_dir", value = dirs$pcr)
      updateTextInput(session, "elisa_pe_dir", value = dirs$elisa_pe)
      updateTextInput(session, "elisa_vsg_dir", value = dirs$elisa_vsg)
      updateTextInput(session, "ielisa_dir", value = dirs$ielisa)
    }

    lab_dirs <- reactiveVal(list(pcr = "", elisa_pe = "", elisa_vsg = "", ielisa = ""))

    thresholds_rv <- reactiveValues(
      pcr = 38,
      elisa = 50,
      ielisa = 35
    )

    observeEvent(input$threshold_pcr, {
      if (!is.null(input$threshold_pcr) && is.finite(input$threshold_pcr)) {
        thresholds_rv$pcr <- input$threshold_pcr
      }
    })
    observeEvent(input$threshold_elisa, {
      if (!is.null(input$threshold_elisa) && is.finite(input$threshold_elisa)) {
        thresholds_rv$elisa <- input$threshold_elisa
      }
    })
    observeEvent(input$threshold_ielisa, {
      if (!is.null(input$threshold_ielisa) && is.finite(input$threshold_ielisa)) {
        thresholds_rv$ielisa <- input$threshold_ielisa
      }
    })

    thresholds <- shiny::reactive(list(
      pcr_cq_max = input$threshold_pcr,
      elisa_pp_cut = input$threshold_elisa,
      ielisa_inh_cut = input$threshold_ielisa
    ))
    
    return(list(
      lab_joined   = joined_reactive,
      joined_data  = joined_reactive,   # <- alias so other modules find it
      thresholds   = thresholds
    ))

    lab_reload <- reactiveVal(0)

    observeEvent(config(), {
      cfg <- config()
      if (is.null(cfg) || is.null(cfg$paths)) return()

      dirs <- list(
        pcr = normalise_dir_input(cfg$paths$pcr_dir),
        elisa_pe = normalise_dir_input(cfg$paths$elisa_pe_dir),
        elisa_vsg = normalise_dir_input(cfg$paths$elisa_vsg_dir),
        ielisa = normalise_dir_input(cfg$paths$ielisa_dir)
      )

      update_dir_inputs(dirs)
      lab_dirs(dirs)
      lab_reload(lab_reload() + 1)
    }, ignoreNULL = FALSE)

    observeEvent(input$reload_lab, {
      dirs <- list(
        pcr = normalise_dir_input(input$pcr_dir),
        elisa_pe = normalise_dir_input(input$elisa_pe_dir),
        elisa_vsg = normalise_dir_input(input$elisa_vsg_dir),
        ielisa = normalise_dir_input(input$ielisa_dir)
      )

      update_dir_inputs(dirs)
      lab_dirs(dirs)
      lab_reload(lab_reload() + 1)
    })

    lab_state <- eventReactive(lab_reload(), {
      dirs <- lab_dirs()
      defaults <- list(pcr = "", elisa_pe = "", elisa_vsg = "", ielisa = "")
      dirs <- utils::modifyList(defaults, dirs)

      empty_data <- list(
        pcr = tibble::tibble(),
        elisa_pe = tibble::tibble(),
        elisa_vsg = tibble::tibble(),
        ielisa = tibble::tibble()
      )

      if (!any(vapply(dirs, function(x) nzchar(x), logical(1)))) {
        return(list(
          data = empty_data,
          dirs = dirs,
          messages = "No lab result folders configured. Update the folder paths and reload.",
          timestamp = Sys.time()
        ))
      }

      withProgress(message = "Loading lab results...", value = 0, {
        incProgress(0.25, detail = "PCR")
        pcr_tbl <- tryCatch(parse_pcr_results(dirs$pcr), error = function(e) {
          attr <- tibble::tibble()
          attr(attr, "messages") <- sprintf("PCR error: %s", e$message)
          attr
        })

        incProgress(0.25, detail = "ELISA PE")
        elisa_pe_tbl <- tryCatch(parse_elisa_pe(dirs$elisa_pe), error = function(e) {
          attr <- tibble::tibble()
          attr(attr, "messages") <- sprintf("ELISA PE error: %s", e$message)
          attr
        })

        incProgress(0.2, detail = "ELISA VSG")
        elisa_vsg_tbl <- tryCatch(parse_elisa_vsg(dirs$elisa_vsg), error = function(e) {
          attr <- tibble::tibble()
          attr(attr, "messages") <- sprintf("ELISA VSG error: %s", e$message)
          attr
        })

        incProgress(0.15, detail = "iELISA")
        ielisa_tbl <- tryCatch(parse_ielisa(dirs$ielisa), error = function(e) {
          attr <- tibble::tibble()
          attr(attr, "messages") <- sprintf("iELISA error: %s", e$message)
          attr
        })

        incProgress(0.15, detail = "Merging results")

        messages <- unique(stats::na.omit(unlist(list(
          attr(pcr_tbl, "messages"),
          attr(elisa_pe_tbl, "messages"),
          attr(elisa_vsg_tbl, "messages"),
          attr(ielisa_tbl, "messages")
        ))))

        missing_dirs <- names(dirs)[!nzchar(unlist(dirs))]
        if (length(missing_dirs)) {
          friendly <- c(pcr = "PCR", elisa_pe = "ELISA PE", elisa_vsg = "ELISA VSG", ielisa = "iELISA")
          messages <- c(messages, sprintf("No folder provided for %s.", friendly[missing_dirs]))
        }

        list(
          data = list(
            pcr = tibble::as_tibble(pcr_tbl),
            elisa_pe = tibble::as_tibble(elisa_pe_tbl),
            elisa_vsg = tibble::as_tibble(elisa_vsg_tbl),
            ielisa = tibble::as_tibble(ielisa_tbl)
          ),
          messages = unique(stats::na.omit(messages)),
          timestamp = Sys.time(),
          dirs = dirs
        )
      })
    }, ignoreNULL = FALSE)

    lab_data <- reactive({
      state <- lab_state()
      state$data
    })

    lab_messages <- reactive({
      state <- lab_state()
      if (is.null(state$messages)) character() else state$messages
    })

    output$lab_loaded_at <- renderUI({
      state <- lab_state()
      if (is.null(state$timestamp)) {
        div(class = "text-muted mt-3", "Not loaded yet")
      } else {
        div(class = "text-muted mt-3", paste("Last loaded:", format(state$timestamp, "%Y-%m-%d %H:%M")))
      }
    })

    output$lab_status_ui <- renderUI({
      state <- lab_state()
      data <- state$data
      counts <- vapply(data, nrow, integer(1), USE.NAMES = TRUE)

      dirs <- state$dirs
      dir_labels <- c(pcr = "PCR", elisa_pe = "ELISA PE", elisa_vsg = "ELISA VSG", ielisa = "iELISA")
      dir_list <- NULL
      if (!is.null(dirs)) {
        dir_list <- tags$ul(
          class = "list-unstyled small mb-3",
          lapply(names(dir_labels), function(nm) {
            value <- dirs[[nm]]
            display <- if (!is.null(value) && nzchar(value)) value else tags$em("Not set")
            tags$li(tags$strong(dir_labels[[nm]]), ": ", display)
          })
        )
      }

      status_list <- tags$ul(
        class = "list-unstyled mb-0",
        lapply(names(counts), function(nm) {
          tags$li(tags$strong(toupper(nm)), ": ", scales::comma(counts[[nm]]))
        })
      )

      messages <- lab_messages()
      msg_ui <- NULL
      if (length(messages)) {
        msg_ui <- bslib::callout(
          title = "Warnings",
          color = "warning",
          lapply(messages, function(m) tags$p(m, class = "mb-1"))
        )
      }

      tagList(dir_list, status_list, msg_ui)
    })

    observeEvent(input$clear_search, {
      updateTextInput(session, "barcode_search", value = "")
    })

    joined <- reactive({
      bio <- biobank_clean()
      state <- lab_state()
      if (is.null(bio) || !nrow(bio)) return(tibble::tibble())

      merged <- merge_lab_with_biobank(bio, state$data)
      compute_flags(merged,
                    pcr_cq_max = thresholds()$pcr,
                    elisa_pp_cut = thresholds()$elisa,
                    ielisa_inh_cut = thresholds()$ielisa)
    })

    joined_filtered <- reactive({
      df <- joined()
      if (!nrow(df)) return(df)
      search_input <- input$barcode_search
      if (is.null(search_input)) search_input <- ""
      term <- tolower(trimws(search_input))
      if (!nzchar(term)) return(df)
      df %>%
        dplyr::mutate(
          barcode_chr = tolower(trimws(as.character(dplyr::coalesce(barcode, "")))),
          lab_id_chr = tolower(trimws(as.character(dplyr::coalesce(lab_id, ""))))
        ) %>%
        dplyr::filter(
          grepl(term, barcode_chr, fixed = TRUE) |
            grepl(term, lab_id_chr, fixed = TRUE)
        ) %>%
        dplyr::select(-barcode_chr, -lab_id_chr)
    })

    output$vb_pcr_pos <- renderText({
      df <- joined()
      if (!nrow(df)) return("0 / 0 (0%)")
      n_pos <- sum(df$PCR_pos %in% TRUE, na.rm = TRUE)
      n_tested <- sum(df$PCR_tested %in% TRUE, na.rm = TRUE)
      pct <- if (n_tested > 0) round(100 * n_pos / n_tested, 1) else 0
      sprintf("%s / %s (%s%%)", scales::comma(n_pos), scales::comma(n_tested), pct)
    })

    output$vb_ielisa_pos <- shiny::renderText({
      df <- joined_data()
      if (!nrow(df)) return("0 / 0 (0%)")
      n_pos <- sum(df$iELISA_pos %in% TRUE, na.rm = TRUE)
      n_tested <- sum(!is.na(df$iELISA_pct_13) | !is.na(df$iELISA_pct_15))
      pct <- if (n_tested > 0) round(100 * n_pos / n_tested, 1) else 0
      sprintf("%s / %s (%s%%)", scales::comma(n_pos), scales::comma(n_tested), pct)
    })

    output$vb_concordant <- renderText({
      df <- joined()
      if (!nrow(df)) return("0 / 0 (0%)")
      n_conc <- sum(df$Concordant %in% TRUE, na.rm = TRUE)
      n_eval <- sum(!is.na(df$Concordant))
      pct <- if (n_eval > 0) round(100 * n_conc / n_eval, 1) else 0
      sprintf("%s / %s (%s%%)", scales::comma(n_conc), scales::comma(n_eval), pct)
    })

    # === TABLES ===
    output$tbl_overview <- DT::renderDT({
      df <- filtered_data()
      if (!nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No lab results available")))
      }
      
      display_df <- df %>%
        dplyr::select(
          barcode, lab_id,
          # PCR
          PCR_Call = PCR_call,                # <-- this name exists in helpers as PCR_call
          `177T Cq` = Cq_177T,
          `18S2 Cq` = Cq_18S2,
          `RNAseP Cq` = Cq_RNAseP,
          PCR_Status = PCR_pos,
          # ELISA PE
          `PE PP%` = PP_percent_PE,
          `PE ΔOD` = OD_PE,
          PE_Status = ELISA_PE_pos,
          # ELISA VSG
          `VSG PP%` = PP_percent_VSG,
          `VSG ΔOD` = OD_VSG,
          VSG_Status = ELISA_VSG_pos,
          # iELISA
          `iELISA 1.3%` = iELISA_pct_13,
          `iELISA 1.5%` = iELISA_pct_15,
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
    

    output$tbl_pcr <- DT::renderDT({
      pcr <- lab_data()$pcr
      shiny::validate(shiny::need(nrow(pcr) > 0, "No PCR files parsed."))
      DT::datatable(pcr, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })

    output$tbl_elisa_pe <- DT::renderDT({
      elisa <- lab_data()$elisa_pe
      shiny::validate(shiny::need(nrow(elisa) > 0, "No ELISA PE files parsed."))
      DT::datatable(elisa, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })

    output$tbl_elisa_vsg <- DT::renderDT({
      elisa <- lab_data()$elisa_vsg
      shiny::validate(shiny::need(nrow(elisa) > 0, "No ELISA VSG files parsed."))
      DT::datatable(elisa, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })

    output$tbl_ielisa <- DT::renderDT({
      iel <- lab_data()$ielisa
      shiny::validate(shiny::need(nrow(iel) > 0, "No iELISA files parsed."))
      DT::datatable(iel, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })

    output$tbl_concordance <- DT::renderDT({
      df <- joined()
      shiny::validate(shiny::need(nrow(df) > 0, "No lab data available to compute concordance."))
      conc <- df %>%
        dplyr::count(PCR_pos, ELISA_PE_pos, ELISA_VSG_pos, iELISA_pos, Concordant, name = "n")
      DT::datatable(conc, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })

    status_palette <- function(status) {
      unique_status <- unique(status)
      colors <- ifelse(unique_status == "OK", "#e8f5e9", "#fdecea")
      names(colors) <- unique_status
      list(status = unique_status, colors = colors)
    }

    output$qc_pcr <- DT::renderDT({
      DT::datatable(
        summarise_pcr_controls(lab_data_raw()$pcr, input$threshold_pcr),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    output$qc_elisa_pe <- DT::renderDT({
      DT::datatable(
        summarise_elisa_controls(lab_data_raw()$elisa_pe, input$threshold_elisa, "ELISA_PE"),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    output$qc_elisa_vsg <- DT::renderDT({
      DT::datatable(
        summarise_elisa_controls(lab_data_raw()$elisa_vsg, input$threshold_elisa, "ELISA_VSG"),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    output$qc_ielisa <- DT::renderDT({
      DT::datatable(
        summarise_ielisa_controls(lab_data_raw()$ielisa, input$threshold_ielisa),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    output$dl_pcr <- downloadHandler(
      filename = function() paste0("pcr_results_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        readr::write_csv(lab_data()$pcr, file)
      }
    )
    output$dl_elisa_pe <- downloadHandler(
      filename = function() paste0("elisa_pe_results_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        readr::write_csv(lab_data()$elisa_pe, file)
      }
    )
    output$dl_elisa_vsg <- downloadHandler(
      filename = function() paste0("elisa_vsg_results_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        readr::write_csv(lab_data()$elisa_vsg, file)
      }
    )
    output$dl_ielisa <- downloadHandler(
      filename = function() paste0("ielisa_results_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        readr::write_csv(lab_data()$ielisa, file)
      }
    )
    output$dl_concordance <- downloadHandler(
      filename = function() paste0("lab_concordance_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        readr::write_csv(joined(), file)
      }
    )

    return(list(
      lab_joined = joined,
      thresholds = thresholds,
      messages = lab_messages
    ))
  })
}
