# =====================================================================
# MODULE: Lab Results v2 — integrates PCR, ELISA (PE + VSG), iELISA, QC
# =====================================================================

mod_lab_results2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        textInput(ns("barcode_search"), "Search Barcode or LabID", placeholder = "e.g. 2401006, 105, ..."),
      ),
      column(
        width = 2,
        actionButton(ns("clear_search"), "Clear", icon = icon("times")),
      ),
      column(
        width = 3,
        actionButton(ns("set_thresholds"), "⚙ Thresholds", class = "btn-secondary")
      )
    ),
    hr(),
    tabsetPanel(
      tabPanel("Overview",    DTOutput(ns("tbl_overview"))),
      tabPanel("PCR",         DTOutput(ns("tbl_pcr"))),
      tabPanel("ELISA PE",    DTOutput(ns("tbl_elisa_pe"))),
      tabPanel("ELISA VSG",   DTOutput(ns("tbl_elisa_vsg"))),
      tabPanel("iELISA",      DTOutput(ns("tbl_ielisa"))),
      tabPanel("Concordance", DTOutput(ns("tbl_concordance"))),
      tabPanel("Controls / QC",
               h4("PCR Controls"),    DTOutput(ns("qc_pcr")),
               h4("ELISA PE Controls"),  DTOutput(ns("qc_elisa_pe")),
               h4("ELISA VSG Controls"), DTOutput(ns("qc_elisa_vsg")),
               h4("iELISA Controls"),    DTOutput(ns("qc_ielisa"))
      )
    )
  )
}

mod_lab_results2_server <- function(id, biobank_clean, config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ======= 1. Load and cache lab data =======
    lab_data <- reactive({
      req(config)
      list(
        pcr     = parse_pcr_results(config$paths$pcr_dir),
        elisa_pe  = parse_elisa_pe(config$paths$elisa_pe_dir),
        elisa_vsg = parse_elisa_vsg(config$paths$elisa_vsg_dir),
        ielisa  = parse_ielisa(config$paths$ielisa_dir)
      )
    })

    # ======= 2. Merge lab data by barcode/LabID =======
    joined <- reactive({
      merge_lab_with_biobank(biobank_clean(), lab_data()) %>%
        compute_flags(
          pcr_cq_max  = input$threshold_pcr,
          elisa_pp_cut = input$threshold_elisa,
          ielisa_inh_cut = input$threshold_ielisa
        )
    })

    # ======= 3. Threshold modal =======
    observeEvent(input$set_thresholds, {
      showModal(
        modalDialog(
          title = "Set Thresholds",
          numericInput(ns("threshold_pcr"), "PCR Cq max", value = 38, min = 1, max = 45),
          numericInput(ns("threshold_elisa"), "ELISA PP% cut-off", value = 50, min = 0, max = 100),
          numericInput(ns("threshold_ielisa"), "iELISA % inhibition cut-off", value = 35, min = 0, max = 100),
          footer = modalButton("Close")
        )
      )
    })

    # ======= 4. Overview table =======
    output$tbl_overview <- renderDT({
      t <- joined()
      validate(need(nrow(t) > 0, "No lab results found"))
      t %>%
        select(barcode, lab_id, PCR_call, ELISA_PE_pos, ELISA_VSG_pos, iELISA_pos, Concordant) %>%
        datatable(options = list(pageLength = 20, scrollX = TRUE))
    })

    # ======= 5. Each individual table =======
    output$tbl_pcr <- renderDT({
      lab_data()$pcr %>% datatable(options = list(pageLength = 15, scrollX = TRUE))
    })
    output$tbl_elisa_pe <- renderDT({
      lab_data()$elisa_pe %>% datatable(options = list(pageLength = 15, scrollX = TRUE))
    })
    output$tbl_elisa_vsg <- renderDT({
      lab_data()$elisa_vsg %>% datatable(options = list(pageLength = 15, scrollX = TRUE))
    })
    output$tbl_ielisa <- renderDT({
      lab_data()$ielisa %>% datatable(options = list(pageLength = 15, scrollX = TRUE))
    })

    # ======= 6. Concordance tab =======
    output$tbl_concordance <- renderDT({
      joined() %>%
        count(Concordant, PCR_call, ELISA_PE_pos, ELISA_VSG_pos, iELISA_pos) %>%
        datatable(options = list(pageLength = 20))
    })

    # ======= 7. Controls / QC =======
    output$qc_pcr        <- renderDT({ parse_pcr_controls(config$paths$pcr_dir) })
    output$qc_elisa_pe   <- renderDT({ parse_elisa_pe_controls(config$paths$elisa_pe_dir) })
    output$qc_elisa_vsg  <- renderDT({ parse_elisa_vsg_controls(config$paths$elisa_vsg_dir) })
    output$qc_ielisa     <- renderDT({ parse_ielisa_controls(config$paths$ielisa_dir) })
  })
}
