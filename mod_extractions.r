# mod_extractions_qc.R
# Module for DRS extraction quality control
# ============================================================================

#' Extraction QC Module - UI
#' @param id Module namespace ID
#' @export
mod_extractions_qc_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = c(4, 8),
    
    # Settings card
    bslib::card(
      bslib::card_header("Settings"),
      shiny::textInput(ns("qc_dir"), "Extractions directory", value = ""),
      shiny::actionButton(ns("qc_refresh"), "Load extractions", 
                          class = "btn-primary w-100"),
      shiny::div(class = "mt-2", shiny::textOutput(ns("qc_status"))),
      shiny::hr(),
      
      shiny::sliderInput(
        ns("qc_rng"), "Acceptable volume range (mL)",
        min = 0.5, max = 3.5, value = c(1.5, 2.5), step = 0.1
      ),
      shiny::selectInput(ns("qc_prov"), "Province", choices = "All", selected = "All"),
      shiny::selectInput(ns("qc_zone"), "Zone", choices = "All", selected = "All"),
      shiny::dateRangeInput(
        ns("qc_date_rng"), "Sample date filter",
        start = NULL, end = NULL, format = "yyyy-mm-dd", weekstart = 1
      ),
      shiny::radioButtons(
        ns("qc_agg"), "Aggregation",
        choices = c("Day" = "day", "Month" = "month", "Year" = "year"),
        selected = "day", inline = TRUE
      ),
      shiny::checkboxInput(ns("qc_show_out"), "Show out-of-range only", value = FALSE),
      shiny::hr(),
      
      shiny::downloadButton(ns("qc_dl_zone"), "Download Zone Summary", 
                            class = "btn-sm w-100 mb-2"),
      shiny::downloadButton(ns("qc_dl_out"), "Download Outliers", 
                            class = "btn-sm w-100 mb-2"),
      shiny::downloadButton(ns("qc_dl_dup"), "Download Duplicates", 
                            class = "btn-sm w-100 mb-2"),
      shiny::downloadButton(ns("qc_dl_filt"), "Download Filtered Data", 
                            class = "btn-sm w-100 mb-2"),
      shiny::downloadButton(ns("qc_dl_agg"), "Download Aggregated Data", 
                            class = "btn-sm w-100")
    ),
    
    # Insights card
    bslib::card(
      bslib::card_header("Extraction Insights"),
      bslib::navset_tab(
        bslib::nav_panel(
          "Overview",
          shiny::uiOutput(ns("qc_overview_status")),
          shiny::plotOutput(ns("qc_hist_vol"), height = 320),
          shiny::plotOutput(ns("qc_box_vol"), height = 260)
        ),
        bslib::nav_panel(
          "By Zone",
          shiny::uiOutput(ns("qc_zone_status")),
          shiny::plotOutput(ns("qc_trend_filedate"), height = 320),
          DT::DTOutput(ns("qc_zone_tbl"))
        ),
        bslib::nav_panel(
          "Time Series",
          shiny::uiOutput(ns("qc_timeseries_status")),
          shiny::plotOutput(ns("qc_agg_count_plot"), height = 280),
          shiny::plotOutput(ns("qc_agg_volume_plot"), height = 280),
          DT::DTOutput(ns("qc_agg_tbl"))
        ),
        bslib::nav_panel(
          "Per sample",
          shiny::uiOutput(ns("qc_sample_status")),
          DT::DTOutput(ns("qc_per_sample_tbl"))
        ),
        bslib::nav_panel(
          "Outliers",
          shiny::uiOutput(ns("qc_outliers_status")),
          DT::DTOutput(ns("qc_out_tbl"))
        ),
        bslib::nav_panel(
          "Duplicates",
          shiny::uiOutput(ns("qc_duplicates_status")),
          DT::DTOutput(ns("qc_dup_tbl"))
        )
      )
    )
  )
}

#' Extraction QC Module - Server
#' @param id Module namespace ID
#' @param biobank_clean Reactive containing cleaned biobank data
#' @param config Reactive containing app configuration
#' @export
mod_extractions_qc_server <- function(id, biobank_clean, config) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values
    extraction_raw <- shiny::reactiveVal(tibble::tibble())
    extraction_status <- shiny::reactiveVal("No extraction data loaded.")
    
    # Initialize directory from config
    shiny::observe({
      cfg <- config()
      if (!is.null(cfg) && "paths" %in% names(cfg)) {
        shiny::updateTextInput(session, "qc_dir", 
                               value = cfg$paths$extractions_dir)
      }
    })
    
    # Load extractions
    shiny::observeEvent(input$qc_refresh, {
      shiny::withProgress(message = 'Loading extractions...', value = 0, {
        dir <- input$qc_dir
        
        if (!dir.exists(dir)) {
          extraction_status("Directory not found. Check the path.")
          extraction_raw(tibble::tibble())
          shiny::showNotification("Extraction directory does not exist.", 
                                   type = "error")
          return()
        }
        
        shiny::incProgress(0.3, detail = "Reading files")
        dat <- read_extractions_dir(dir)
        
        if (!nrow(dat)) {
          extraction_status("No extraction files found in this directory.")
          extraction_raw(tibble::tibble())
          shiny::showNotification("No extraction files found.", type = "warning")
          return()
        }
        
        shiny::incProgress(0.3, detail = "Processing data")
        
        # Process volume data
        dat <- dat %>%
          dplyr::mutate(
            volume_num = parse_decimal_number(volume_ml),
            # Scale down if values > 10 (likely in deci-mL)
            volume_num = dplyr::if_else(
              !is.na(volume_num) & volume_num > 10, 
              volume_num / 10, 
              volume_num
            ),
            # Remove negatives and extreme outliers
            volume_num = dplyr::if_else(
              !is.na(volume_num) & (volume_num < 0 | volume_num > 10),
              NA_real_,
              volume_num
            ),
            file_date = as.Date(file_date)
          )
        
        extraction_raw(dat)
        
        file_count <- length(unique(stats::na.omit(dat$source_file)))
        sample_count <- nrow(dat)
        
        shiny::incProgress(0.4, detail = "Updating display")
        extraction_status(sprintf("%s files · %s samples",
                                  scales::comma(file_count),
                                  scales::comma(sample_count)))
      })
    })
    
    # Join with biobank
    extractions_joined <- shiny::reactive({
      join_extractions_to_biobank(extraction_raw(), biobank_clean())
    })
    
    # Flag duplicates and issues
    extractions_flagged <- shiny::reactive({
      flag_extractions(extractions_joined())
    })
    
    # Deduplicated data
    extractions_dedup <- shiny::reactive({
      res <- extractions_flagged()
      if (is.null(res$dedup)) tibble::tibble() else res$dedup
    })
    
    # Update filter choices
    shiny::observe({
      df <- extractions_dedup()
      if (is.null(df) || !nrow(df) || !"province" %in% names(df)) {
        shiny::updateSelectInput(session, "qc_prov", choices = "All", 
                                 selected = "All")
        shiny::updateSelectInput(session, "qc_zone", choices = "All", 
                                 selected = "All")
        return()
      }
      
      provs <- sort(unique(stats::na.omit(df$province)))
      sel_prov <- if (input$qc_prov %in% c("All", provs)) input$qc_prov else "All"
      shiny::updateSelectInput(session, "qc_prov", 
                               choices = c("All", provs), 
                               selected = sel_prov)
      
      rng <- suppressWarnings(range(df$date_prelev, na.rm = TRUE))
      if (all(is.finite(rng))) {
        shiny::updateDateRangeInput(session, "qc_date_rng",
                                    start = rng[1], end = rng[2],
                                    min = rng[1], max = rng[2])
      }
    })
    
    # Update zone choices based on province
    shiny::observe({
      df <- extractions_dedup()
      if (is.null(df) || !nrow(df) || !"zone" %in% names(df)) {
        shiny::updateSelectInput(session, "qc_zone", choices = "All", 
                                 selected = "All")
        return()
      }
      
      zones <- sort(unique(stats::na.omit(df$zone)))
      if (!is.null(input$qc_prov) && input$qc_prov != "All" && 
          "province" %in% names(df)) {
        zones <- sort(unique(stats::na.omit(
          df$zone[df$province == input$qc_prov]
        )))
      }
      sel_zone <- if (input$qc_zone %in% c("All", zones)) input$qc_zone else "All"
      shiny::updateSelectInput(session, "qc_zone", choices = c("All", zones), 
                               selected = sel_zone)
    })
    
    # Apply filters
    extractions_filtered <- shiny::reactive({
      df <- extractions_dedup()
      shiny::req(df)
      
      if (!nrow(df)) return(tibble::tibble())
      
      # Apply province filter
      if (!is.null(input$qc_prov) && input$qc_prov != "All") {
        df <- df %>% dplyr::filter(!is.na(province) & province == input$qc_prov)
      }
      
      # Apply zone filter
      if (!is.null(input$qc_zone) && input$qc_zone != "All") {
        df <- df %>% dplyr::filter(!is.na(zone) & zone == input$qc_zone)
      }
      
      # Apply date filter
      if (!is.null(input$qc_date_rng) && length(input$qc_date_rng) == 2) {
        df <- df %>%
          dplyr::filter(!is.na(date_prelev) &
                          date_prelev >= input$qc_date_rng[1] &
                          date_prelev <= input$qc_date_rng[2])
      }
      
      # Apply outlier filter
      if (isTRUE(input$qc_show_out)) {
        rng <- input$qc_rng
        df <- df %>%
          dplyr::filter(!is.na(volume_num) &
                          (volume_num < rng[1] | volume_num > rng[2]))
      }
      
      df
    })
    
    # Aggregated data
    qc_agg_data <- shiny::reactive({
      df <- extractions_filtered()
      if (is.null(df) || !nrow(df) || 
          !"date_prelev" %in% names(df) || 
          !"volume_num" %in% names(df)) {
        return(tibble::tibble())
      }
      
      df <- df %>% dplyr::filter(!is.na(date_prelev))
      if (!nrow(df)) return(tibble::tibble())
      
      agg <- input$qc_agg
      df <- df %>%
        dplyr::mutate(
          period = dplyr::case_when(
            agg == "day" ~ date_prelev,
            agg == "month" ~ lubridate::floor_date(date_prelev, unit = "month"),
            agg == "year" ~ as.Date(sprintf("%s-01-01", 
                                            lubridate::year(date_prelev))),
            TRUE ~ date_prelev
          )
        )
      
      rng <- input$qc_rng
      df %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(
          N = dplyr::n(),
          Median = suppressWarnings(stats::median(volume_num, na.rm = TRUE)),
          P10 = suppressWarnings(stats::quantile(volume_num, 0.10, 
                                                  na.rm = TRUE, names = FALSE)),
          P90 = suppressWarnings(stats::quantile(volume_num, 0.90, 
                                                  na.rm = TRUE, names = FALSE)),
          Out = sum(!is.na(volume_num) & 
                      (volume_num < rng[1] | volume_num > rng[2])),
          `% Out` = round(100 * Out / pmax(N, 1), 1),
          .groups = "drop"
        ) %>%
        dplyr::arrange(period)
    })
    
    # Outputs
    output$qc_status <- shiny::renderText({ extraction_status() })
    
    # [Continue in next message - outputs for plots and tables]
    
    # Return reactive values for use by other modules
    return(list(
      data = extractions_filtered,
      dedup = extractions_dedup,
      flagged = shiny::reactive({ extractions_flagged()$flagged })
    ))
  })
}

# Helper function: Read extraction directory
read_extractions_dir <- function(dir_extraction) {
  if (!dir.exists(dir_extraction)) return(tibble::tibble())
  
  files <- list.files(dir_extraction, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) return(tibble::tibble())
  
  read_one <- function(f) {
    sneak <- suppressMessages(readxl::read_excel(f, n_max = 1))
    col_types <- rep("text", ncol(sneak))
    
    suppressMessages(
      readxl::read_excel(f, col_types = col_types, .name_repair = "minimal") %>%
        janitor::clean_names()
    ) %>%
      dplyr::mutate(
        source_file = basename(f),
        file_date = {
          m <- stringr::str_match(basename(f), "^(\\d{6})")
          if (!is.na(m[1, 2])) as.Date(m[1, 2], format = "%y%m%d") else as.Date(NA)
        }
      )
  }
  
  raw <- purrr::map_dfr(files, read_one)
  
  if (!nrow(raw)) return(tibble::tibble())
  
  n <- nrow(raw)
  
  get_chr <- function(name) {
    if (name %in% names(raw)) {
      as.character(raw[[name]])
    } else {
      rep(NA_character_, n)
    }
  }
  
  get_date <- function(name) {
    parse_any_date(get_chr(name))
  }
  
  raw %>%
    dplyr::mutate(
      date_prelev = get_date("date_de_prelevement_jj_mm_aaaa"),
      date_env_cpltha = get_date("date_envoi_vers_cpltha_jj_mm_aaaa"),
      date_rec_cpltha = get_date("date_de_reception_cpltha_jj_mm_aaaa"),
      date_env_inrb = get_date("date_denvoi_inrb"),
      volume_raw = get_chr("volume_total_echantillon_sang_drs_ml"),
      volume_ml = parse_decimal_number(volume_raw)
    ) %>%
    dplyr::filter(!(is.na(code_barres_kps) | code_barres_kps == ""))
}

# Helper function: Flag extraction issues
flag_extractions <- function(extractions_joined) {
  if (is.null(extractions_joined) || !nrow(extractions_joined)) {
    return(list(flagged = tibble::tibble(), dedup = tibble::tibble()))
  }
  
  # [Flagging logic continues...]
  # This is complex - keeping the original logic from app.R
  
  list(
    flagged = extractions_joined,  # Placeholder
    dedup = extractions_joined     # Placeholder
  )
}
