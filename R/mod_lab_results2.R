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
        actionButton(ns("reload_lab"), "Load lab data", icon = bsicons::bs_icon("arrow-repeat"),
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
        actionButton(ns("clear_search"), NULL, icon = bsicons::bs_icon("x-circle"),
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
              showcase = bsicons::bs_icon("virus"),
              theme = "danger"
            ),
            bslib::value_box(
              title = "iELISA Positive",
              value = textOutput(ns("vb_ielisa_pos")),
              showcase = bsicons::bs_icon("clipboard2-check"),
              theme = "info"
            ),
            bslib::value_box(
              title = "Concordant Results",
              value = textOutput(ns("vb_concordant")),
              showcase = bsicons::bs_icon("check-circle"),
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

    thresholds <- reactive({
      list(
        pcr = thresholds_rv$pcr,
        elisa = thresholds_rv$elisa,
        ielisa = thresholds_rv$ielisa
      )
    })

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

    output$vb_ielisa_pos <- renderText({
      df <- joined()
      if (!nrow(df)) return("0 / 0 (0%)")
      n_pos <- sum(df$iELISA_pos %in% TRUE, na.rm = TRUE)
      n_tested <- sum(df$iELISA_tested %in% TRUE, na.rm = TRUE)
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

    output$tbl_overview <- DT::renderDT({
      df <- joined_filtered()
      shiny::validate(shiny::need(nrow(df) > 0, "No lab results available for the current filters."))

      overview <- df %>%
        dplyr::mutate(
          PCR_result = dplyr::case_when(PCR_pos %in% TRUE ~ "POS", PCR_pos %in% FALSE ~ "NEG", TRUE ~ ""),
          ELISA_PE_result = dplyr::case_when(ELISA_PE_pos %in% TRUE ~ "POS", ELISA_PE_pos %in% FALSE ~ "NEG", TRUE ~ ""),
          ELISA_VSG_result = dplyr::case_when(ELISA_VSG_pos %in% TRUE ~ "POS", ELISA_VSG_pos %in% FALSE ~ "NEG", TRUE ~ ""),
          iELISA_result = dplyr::case_when(iELISA_pos %in% TRUE ~ "POS", iELISA_pos %in% FALSE ~ "NEG", TRUE ~ ""),
          Any_positive = dplyr::case_when(Any_positive %in% TRUE ~ "Yes", Any_positive %in% FALSE ~ "No", TRUE ~ ""),
          Concordant = dplyr::case_when(Concordant %in% TRUE ~ "Yes", Concordant %in% FALSE ~ "No", TRUE ~ "")
        ) %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(c("Cq_177T", "Cq_18S2", "Cq_RNAseP", "OD_PE", "PP_percent_PE",
                           "OD_VSG", "PP_percent_VSG", "iELISA_pct_13", "iELISA_pct_15")),
          ~round(.x, 2)
        )) %>%
        dplyr::select(
          barcode, lab_id,
          PCR_result, PCR_call, Cq_177T, Cq_18S2, Cq_RNAseP,
          ELISA_PE_result, OD_PE, PP_percent_PE,
          ELISA_VSG_result, OD_VSG, PP_percent_VSG,
          iELISA_result, iELISA_pct_13, iELISA_pct_15,
          Any_positive, Concordant
        )

      DT::datatable(
        overview,
        options = list(pageLength = 20, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        filter = "top",
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left;",
          "Exact PCR Cq values and ELISA/iELISA metrics shown per sample."
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
      df <- summarise_pcr_controls(lab_data()$pcr, thresholds()$pcr)
      shiny::validate(shiny::need(nrow(df) > 0, "No PCR control rows detected."))
      pal <- status_palette(df$Status)
      DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
        DT::formatStyle("Status", target = "row",
                        backgroundColor = DT::styleEqual(pal$status, pal$colors))
    })

    output$qc_elisa_pe <- DT::renderDT({
      df <- summarise_elisa_controls(lab_data()$elisa_pe, thresholds()$elisa, "ELISA PE")
      shiny::validate(shiny::need(nrow(df) > 0, "No ELISA PE control rows detected."))
      pal <- status_palette(df$Status)
      DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
        DT::formatStyle("Status", target = "row",
                        backgroundColor = DT::styleEqual(pal$status, pal$colors))
    })

    output$qc_elisa_vsg <- DT::renderDT({
      df <- summarise_elisa_controls(lab_data()$elisa_vsg, thresholds()$elisa, "ELISA VSG")
      shiny::validate(shiny::need(nrow(df) > 0, "No ELISA VSG control rows detected."))
      pal <- status_palette(df$Status)
      DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
        DT::formatStyle("Status", target = "row",
                        backgroundColor = DT::styleEqual(pal$status, pal$colors))
    })

    output$qc_ielisa <- DT::renderDT({
      df <- summarise_ielisa_controls(joined(), thresholds()$ielisa)
      shiny::validate(shiny::need(nrow(df) > 0, "No iELISA control rows detected."))
      pal <- status_palette(df$Status)
      DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
        DT::formatStyle("Status", target = "row",
                        backgroundColor = DT::styleEqual(pal$status, pal$colors))
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
