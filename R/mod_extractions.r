# mod_extractions_qc.R
# Complete DRS extraction quality control module
# =============================================================================

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
                          class = "btn-primary w-100 mb-3"),
      shiny::div(class = "mt-2", shiny::textOutput(ns("qc_status"))),
      
      shiny::hr(),
      
      shiny::sliderInput(
        ns("qc_rng"), "Acceptable volume range (mL)",
        min = 0.5, max = 3.5, value = c(1.5, 2.5), step = 0.1
      ),
      
      shiny::selectInput(ns("qc_prov"), "Province", 
                         choices = "All", selected = "All"),
      shiny::selectInput(ns("qc_zone"), "Zone", 
                         choices = "All", selected = "All"),
      
      shiny::dateRangeInput(
        ns("qc_date_rng"), "Sample date filter",
        start = NULL, end = NULL, format = "yyyy-mm-dd", weekstart = 1
      ),
      
      shiny::radioButtons(
        ns("qc_agg"), "Aggregation",
        choices = c("Day" = "day", "Month" = "month", "Year" = "year"),
        selected = "day", inline = TRUE
      ),
      
      shiny::checkboxInput(ns("qc_show_out"), 
                           "Show out-of-range only", value = FALSE),
      
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
qc_normalise_path <- function(path) {
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

mod_extractions_qc_server <- function(id, biobank_clean, config) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    extraction_raw <- shiny::reactiveVal(tibble::tibble())
    extraction_status <- shiny::reactiveVal("No extraction data loaded.")
    
    # Initialize from config
    shiny::observe({
      cfg <- config()
      if (!is.null(cfg) && "paths" %in% names(cfg)) {
        shiny::updateTextInput(session, "qc_dir",
                               value = qc_normalise_path(cfg$paths$extractions_dir))
      }
      if (!is.null(cfg) && "qc" %in% names(cfg)) {
        shiny::updateSliderInput(session, "qc_rng",
                                 value = c(cfg$qc$drs_accept_min_ml,
                                           cfg$qc$drs_accept_max_ml))
      }
    })
    
    # Load extractions
    shiny::observeEvent(input$qc_refresh, {
      shiny::withProgress(message = 'Loading extractions...', value = 0, {
        dir <- qc_normalise_path(input$qc_dir)

        if (is.null(dir) || is.na(dir) || !nzchar(dir) || !dir.exists(dir)) {
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
        
        shiny::incProgress(0.3, detail = "Processing volumes")
        
        # Process volume data with robust parsing
        dat <- dat %>%
          dplyr::mutate(
            # Parse volume
            volume_num = parse_decimal_number(volume_ml),
            # Scale down if > 10 (likely deci-mL)
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
        
        shiny::incProgress(0.4, detail = "Complete")
        extraction_status(sprintf("%s files · %s samples",
                                  scales::comma(file_count),
                                  scales::comma(sample_count)))
        
        shiny::showNotification(
          sprintf("Loaded %s extraction records", scales::comma(sample_count)),
          type = "message"
        )
      })
    })
    
    # Join with biobank
    extractions_joined <- shiny::reactive({
      extr <- extraction_raw()
      bio <- biobank_clean()
      join_extractions_to_biobank(extr, bio)
    })
    
    # Flag duplicates and issues
    extractions_flagged <- shiny::reactive({
      flag_extractions(extractions_joined())
    })
    
    # Deduplicated data
    extractions_dedup <- shiny::reactive({
      res <- extractions_flagged()

      if (!is.null(res$dedup) && nrow(res$dedup)) {
        return(res$dedup)
      }

      flagged <- res$flagged
      if (!is.null(flagged) && nrow(flagged)) {
        return(flagged)
      }

      tibble::tibble()
    })
    
    # Update filter choices
    shiny::observe({
      df <- extractions_dedup()
      if (is.null(df) || !nrow(df) || !"province" %in% names(df)) {
        shiny::updateSelectInput(session, "qc_prov", 
                                 choices = "All", selected = "All")
        shiny::updateSelectInput(session, "qc_zone", 
                                 choices = "All", selected = "All")
        return()
      }
      
      provs <- sort(unique(stats::na.omit(df$province)))
      sel_prov <- if (input$qc_prov %in% c("All", provs)) {
        input$qc_prov
      } else {
        "All"
      }
      
      shiny::updateSelectInput(session, "qc_prov", 
                               choices = c("All", provs), 
                               selected = sel_prov)
      
      # Update date range
      rng <- suppressWarnings(range(df$date_prelev, na.rm = TRUE))
      if (all(is.finite(rng))) {
        shiny::updateDateRangeInput(session, "qc_date_rng",
                                    start = rng[1], end = rng[2],
                                    min = rng[1], max = rng[2])
      }
    })
    
    # Update zones based on province
    shiny::observe({
      df <- extractions_dedup()
      if (is.null(df) || !nrow(df) || !"zone" %in% names(df)) {
        shiny::updateSelectInput(session, "qc_zone", 
                                 choices = "All", selected = "All")
        return()
      }
      
      zones <- sort(unique(stats::na.omit(df$zone)))
      if (!is.null(input$qc_prov) && input$qc_prov != "All" && 
          "province" %in% names(df)) {
        zones <- sort(unique(stats::na.omit(
          df$zone[df$province == input$qc_prov]
        )))
      }
      
      sel_zone <- if (input$qc_zone %in% c("All", zones)) {
        input$qc_zone
      } else {
        "All"
      }
      
      shiny::updateSelectInput(session, "qc_zone", 
                               choices = c("All", zones), 
                               selected = sel_zone)
    })
    
    # Apply filters
    extractions_filtered <- shiny::reactive({
      df <- extractions_dedup()
      shiny::req(df)

      if (!nrow(df)) return(tibble::tibble())
      
      # Province filter
      if (!is.null(input$qc_prov) && input$qc_prov != "All") {
        df <- df %>% 
          dplyr::filter(!is.na(province) & province == input$qc_prov)
      }
      
      # Zone filter
      if (!is.null(input$qc_zone) && input$qc_zone != "All") {
        df <- df %>% 
          dplyr::filter(!is.na(zone) & zone == input$qc_zone)
      }
      
      # Date filter
      if (!is.null(input$qc_date_rng) && length(input$qc_date_rng) == 2) {
        dr <- as.Date(input$qc_date_rng)
        if (all(!is.na(dr))) {
          df <- df %>%
            dplyr::filter(!is.na(date_prelev) &
                            date_prelev >= dr[1] &
                            date_prelev <= dr[2])
        }
      }
      
      # Outlier filter
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
      cfg <- config()
      target <- if (!is.null(cfg) && "qc" %in% names(cfg)) {
        cfg$qc$drs_target_ml
      } else {
        2.0
      }
      
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
    
    # === OUTPUTS ===
    
    output$qc_status <- shiny::renderText({ extraction_status() })
    
    # Status messages for each tab
    output$qc_overview_status <- shiny::renderUI({
      df <- extractions_filtered()
      if (is.null(df) || !nrow(df)) {
        shiny::div(class = "alert alert-warning mb-3",
                   "No extraction data available with current filters.")
      }
    })
    
    output$qc_zone_status <- shiny::renderUI({
      df <- extractions_filtered()
      if (is.null(df) || !nrow(df)) {
        shiny::div(class = "alert alert-warning mb-3",
                   "No extraction data available with current filters.")
      }
    })
    
    output$qc_timeseries_status <- shiny::renderUI({
      df <- extractions_filtered()
      if (is.null(df) || !nrow(df)) {
        shiny::div(class = "alert alert-warning mb-3",
                   "No extraction data available with current filters.")
      }
    })
    
    output$qc_sample_status <- shiny::renderUI({
      df <- extractions_filtered()
      if (is.null(df) || !nrow(df)) {
        shiny::div(class = "alert alert-warning mb-3",
                   "No extraction data available with current filters.")
      }
    })
    
    output$qc_outliers_status <- shiny::renderUI({
      df <- extractions_filtered()
      if (is.null(df) || !nrow(df)) {
        shiny::div(class = "alert alert-warning mb-3",
                   "No extraction data available with current filters.")
      }
    })
    
    output$qc_duplicates_status <- shiny::renderUI({
      df <- extractions_filtered()
      if (is.null(df) || !nrow(df)) {
        shiny::div(class = "alert alert-warning mb-3",
                   "No extraction data available with current filters.")
      }
    })
    
    # Histogram
    output$qc_hist_vol <- shiny::renderPlot({
      df <- extractions_filtered()
      shiny::req(df, nrow(df) > 0, "volume_num" %in% names(df))
      
      rng <- input$qc_rng
      cfg <- config()
      target <- if (!is.null(cfg) && "qc" %in% names(cfg)) {
        cfg$qc$drs_target_ml
      } else {
        2.0
      }
      
      ggplot2::ggplot(df, ggplot2::aes(
        x = volume_num,
        fill = volume_num < rng[1] | volume_num > rng[2]
      )) +
        ggplot2::geom_histogram(binwidth = 0.1, colour = "black") +
        ggplot2::geom_vline(xintercept = target, 
                            linetype = "dashed", colour = "red", size = 1) +
        ggplot2::geom_vline(xintercept = rng, 
                            linetype = "dotted", colour = "grey40", size = 0.8) +
        ggplot2::scale_fill_manual(
          values = c(`FALSE` = "steelblue", `TRUE` = "tomato"),
          guide = "none"
        ) +
        ggplot2::labs(
          title = "Distribution of Sample Volumes",
          subtitle = sprintf(
            "Dashed line = %.1f mL target | Dotted = %.1f–%.1f mL acceptable range",
            target, rng[1], rng[2]
          ),
          x = "Volume (mL)",
          y = "Count"
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })
    
    # Box plot
    output$qc_box_vol <- shiny::renderPlot({
      df <- extractions_filtered()
      shiny::req(df, nrow(df) > 0, "volume_num" %in% names(df))
      
      rng <- input$qc_rng
      cfg <- config()
      target <- if (!is.null(cfg) && "qc" %in% names(cfg)) {
        cfg$qc$drs_target_ml
      } else {
        2.0
      }
      
      ggplot2::ggplot(df, ggplot2::aes(y = volume_num)) +
        ggplot2::geom_boxplot(fill = "grey90", outlier.colour = "tomato") +
        ggplot2::geom_jitter(height = 0, width = 0.1, alpha = 0.4) +
        ggplot2::geom_hline(yintercept = target, 
                            linetype = "dashed", colour = "red", size = 1) +
        ggplot2::geom_hline(yintercept = rng, 
                            linetype = "dotted", colour = "grey40", size = 0.8) +
        ggplot2::labs(
          title = "Volume Distribution with Outliers",
          y = "Volume (mL)"
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })
    
    # Trend by file date
    output$qc_trend_filedate <- shiny::renderPlot({
      df <- extractions_filtered()
      shiny::req(df, nrow(df) > 0,
                 all(c("file_date", "volume_num", "zone") %in% names(df)))
      
      rng <- input$qc_rng
      cfg <- config()
      target <- if (!is.null(cfg) && "qc" %in% names(cfg)) {
        cfg$qc$drs_target_ml
      } else {
        2.0
      }
      
      ggplot2::ggplot(df, ggplot2::aes(x = file_date, y = volume_num, 
                                       colour = zone)) +
        ggplot2::geom_jitter(alpha = 0.5, width = 1, height = 0) +
        ggplot2::geom_hline(yintercept = target, 
                            linetype = "dashed", colour = "red", size = 1) +
        ggplot2::geom_hline(yintercept = rng, 
                            linetype = "dotted", colour = "grey40", size = 0.8) +
        ggplot2::labs(
          title = "Sample Volumes by Extraction Batch",
          x = "Extraction File Date",
          y = "Volume (mL)",
          colour = "Zone"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    })
    
    # Zone table
    output$qc_zone_tbl <- DT::renderDT({
      df <- extractions_filtered()
      if (!nrow(df) || !all(c("province", "zone", "volume_num") %in% names(df))) {
        return(DT::datatable(tibble::tibble(), 
                             options = list(pageLength = 15, scrollX = TRUE)))
      }
      
      rng <- input$qc_rng
      summ <- df %>%
        dplyr::group_by(province, zone) %>%
        dplyr::summarise(
          N = dplyr::n(),
          Out = sum(!is.na(volume_num) & 
                      (volume_num < rng[1] | volume_num > rng[2])),
          `% Out` = round(100 * Out / pmax(N, 1), 1),
          Mean = round(mean(volume_num, na.rm = TRUE), 2),
          Median = round(stats::median(volume_num, na.rm = TRUE), 2),
          SD = round(stats::sd(volume_num, na.rm = TRUE), 2),
          P10 = round(stats::quantile(volume_num, 0.10, 
                                      na.rm = TRUE, names = FALSE), 2),
          P90 = round(stats::quantile(volume_num, 0.90, 
                                      na.rm = TRUE, names = FALSE), 2),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(`% Out`), dplyr::desc(N))
      
      DT::datatable(summ,
                    options = list(pageLength = 15, scrollX = TRUE),
                    rownames = FALSE,
                    class = 'cell-border stripe') %>%
        DT::formatStyle(
          '% Out',
          backgroundColor = DT::styleInterval(c(10, 20), 
                                              c('white', '#fff3cd', '#f8d7da'))
        )
    })
    
    # Time series plots
    output$qc_agg_count_plot <- shiny::renderPlot({
      ad <- qc_agg_data()
      shiny::req(ad, nrow(ad) > 0)
      
      ggplot2::ggplot(ad, ggplot2::aes(x = period, y = N)) +
        ggplot2::geom_col(fill = "steelblue") +
        ggplot2::labs(
          title = sprintf("Samples per %s", 
                          switch(input$qc_agg, 
                                 day = "day", 
                                 month = "month", 
                                 year = "year")),
          x = "Period",
          y = "Sample Count"
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })
    
    output$qc_agg_volume_plot <- shiny::renderPlot({
      ad <- qc_agg_data()
      shiny::req(ad, nrow(ad) > 0)
      
      cfg <- config()
      target <- if (!is.null(cfg) && "qc" %in% names(cfg)) {
        cfg$qc$drs_target_ml
      } else {
        2.0
      }
      
      ggplot2::ggplot(ad, ggplot2::aes(x = period, y = Median)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = P10, ymax = P90), 
                             alpha = 0.2, fill = "steelblue") +
        ggplot2::geom_line(color = "steelblue", size = 1) +
        ggplot2::geom_point(color = "steelblue", size = 2) +
        ggplot2::geom_hline(yintercept = target, 
                            linetype = "dashed", colour = "red") +
        ggplot2::labs(
          title = sprintf("Median Volume per %s (P10–P90 ribbon)",
                          switch(input$qc_agg, 
                                 day = "day", 
                                 month = "month", 
                                 year = "year")),
          x = "Period",
          y = "Volume (mL)"
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })
    
    output$qc_agg_tbl <- DT::renderDT({
      ad <- qc_agg_data()
      DT::datatable(ad, options = list(pageLength = 20, scrollX = TRUE),
                    rownames = FALSE)
    })
    
    # Per sample table
    output$qc_per_sample_tbl <- DT::renderDT({
      df <- extractions_filtered()
      if (!nrow(df)) df <- tibble::tibble()
      
      keep <- c("province", "zone", "numero", "code_barres_kps", 
                "date_prelev", "date_rec_cpltha", "date_env_cpltha", 
                "date_env_inrb", "volume_ml", "volume_num", 
                "file_date", "source_file", "flag")
      
      DT::datatable(
        df %>%
          dplyr::select(dplyr::any_of(keep)) %>%
          dplyr::arrange(date_prelev, zone, numero),
        options = list(pageLength = 20, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      )
    })
    
    # Outliers table
    output$qc_out_tbl <- DT::renderDT({
      df <- extractions_filtered()
      if (!nrow(df) || !"volume_num" %in% names(df)) {
        df <- tibble::tibble()
      } else {
        rng <- input$qc_rng
        df <- df %>%
          dplyr::filter(!is.na(volume_num) & 
                          (volume_num < rng[1] | volume_num > rng[2])) %>%
          dplyr::arrange(volume_num)
      }
      
      keep <- c("province", "zone", "numero", "code_barres_kps", 
                "volume_ml", "volume_num", "date_prelev", 
                "file_date", "source_file")
      
      DT::datatable(
        df %>% dplyr::select(dplyr::any_of(keep)),
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # Duplicates table
    output$qc_dup_tbl <- DT::renderDT({
      flagged <- extractions_flagged()
      flagged <- flagged$flagged
      
      if (is.null(flagged) || !nrow(flagged) || !"flag" %in% names(flagged)) {
        flagged <- tibble::tibble()
      } else {
        flagged <- flagged %>% dplyr::filter(flag != "OK")
      }
      
      keep <- c("flag", "province", "zone", "numero", "code_barres_kps", 
                "volume_ml", "volume_num", "date_prelev", 
                "file_date", "source_file")
      
      DT::datatable(
        flagged %>% dplyr::select(dplyr::any_of(keep)),
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'flag',
          backgroundColor = DT::styleEqual(
            c('EXACT_DUPLICATE', 'RE_EXTRACTION_SAME_BARCODE_DIFF_NUM_DIFF_VOL',
              'SUSPECT_SAME_BARCODE_SAME_VOL_NEW_NUM', 'SUSPECT_SAME_NUM_DIFF_BARCODE'),
            c('#f8d7da', '#fff3cd', '#d1ecf1', '#d1ecf1')
          )
        )
    })
    
    # === DOWNLOADS ===
    
    output$qc_dl_zone <- shiny::downloadHandler(
      filename = function() {
        paste0("zone_summary_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- extractions_filtered()
        if (!nrow(df) || 
            !all(c("province", "zone", "volume_num") %in% names(df))) {
          readr::write_csv(tibble::tibble(), file)
          return()
        }
        
        rng <- input$qc_rng
        summ <- df %>%
          dplyr::group_by(province, zone) %>%
          dplyr::summarise(
            N = dplyr::n(),
            Out = sum(!is.na(volume_num) & 
                        (volume_num < rng[1] | volume_num > rng[2])),
            `% Out` = round(100 * Out / pmax(N, 1), 1),
            Median = round(stats::median(volume_num, na.rm = TRUE), 2),
            P10 = round(stats::quantile(volume_num, 0.10, 
                                        na.rm = TRUE, names = FALSE), 2),
            P90 = round(stats::quantile(volume_num, 0.90, 
                                        na.rm = TRUE, names = FALSE), 2),
            .groups = "drop"
          )
        readr::write_csv(summ, file)
      }
    )
    
    output$qc_dl_out <- shiny::downloadHandler(
      filename = function() {
        paste0("outliers_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- extractions_filtered()
        if (!nrow(df) || !"volume_num" %in% names(df)) {
          readr::write_csv(tibble::tibble(), file)
          return()
        }
        
        rng <- input$qc_rng
        out <- df %>%
          dplyr::filter(!is.na(volume_num) & 
                          (volume_num < rng[1] | volume_num > rng[2]))
        readr::write_csv(out, file)
      }
    )
    
    output$qc_dl_dup <- shiny::downloadHandler(
      filename = function() {
        paste0("duplicates_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        flagged <- extractions_flagged()
        flagged <- flagged$flagged
        if (is.null(flagged) || !nrow(flagged)) {
          readr::write_csv(tibble::tibble(), file)
          return()
        }
        readr::write_csv(flagged %>% dplyr::filter(flag != "OK"), file)
      }
    )
    
    output$qc_dl_filt <- shiny::downloadHandler(
      filename = function() {
        paste0("filtered_extractions_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- extractions_filtered()
        if (!nrow(df)) df <- tibble::tibble()
        readr::write_csv(df, file)
      }
    )
    
    output$qc_dl_agg <- shiny::downloadHandler(
      filename = function() {
        paste0("extraction_timeseries_", input$qc_agg, "_", 
               format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        readr::write_csv(qc_agg_data(), file)
      }
    )
    
    # Return reactive values
    return(list(
      data = extractions_filtered,
      dedup = extractions_dedup,
      flagged = shiny::reactive({ extractions_flagged()$flagged })
    ))
  })
}

# === HELPER FUNCTIONS ===

#' Read extraction directory
#' @param dir_extraction Directory path
#' @return Tibble with extraction data
read_extractions_dir <- function(dir_extraction) {
  dir_extraction <- qc_normalise_path(dir_extraction)
  if (is.null(dir_extraction) || is.na(dir_extraction) || !nzchar(dir_extraction)) {
    return(tibble::tibble())
  }
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
          if (!is.na(m[1, 2])) {
            as.Date(m[1, 2], format = "%y%m%d")
          } else {
            as.Date(NA)
          }
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
      volume_ml = volume_raw
    ) %>%
    dplyr::filter(!(is.na(code_barres_kps) | code_barres_kps == ""))
}

#' Flag extraction issues
#' @param extractions_joined Joined extraction data
#' @return List with flagged and deduplicated data
flag_extractions <- function(extractions_joined) {
  if (is.null(extractions_joined) || !nrow(extractions_joined)) {
    return(list(flagged = tibble::tibble(), dedup = tibble::tibble()))
  }
  
  # Ensure volume_num exists
  if (!"volume_num" %in% names(extractions_joined)) {
    extractions_joined$volume_num <- NA_real_
  }
  
  # Create volume key for matching
  extractions_joined <- extractions_joined %>%
    dplyr::mutate(vol_key = round(volume_num, 2))
  
  # Find exact duplicates
  dups_exact <- extractions_joined %>%
    dplyr::group_by(code_barres_kps, numero, vol_key) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()
  
  # Same barcode, different numero
  same_barcode_diff_num <- extractions_joined %>%
    dplyr::filter(!is.na(code_barres_kps)) %>%
    dplyr::group_by(code_barres_kps) %>%
    dplyr::filter(dplyr::n_distinct(numero, na.rm = TRUE) > 1) %>%
    dplyr::ungroup()
  
  # Re-extractions (different volumes)
  reextractions <- same_barcode_diff_num %>%
    dplyr::group_by(code_barres_kps) %>%
    dplyr::filter(dplyr::n_distinct(vol_key, na.rm = TRUE) > 1) %>%
    dplyr::ungroup()
  
  # Same barcode, same volume, different numero (suspect)
  same_barcode_same_vol_new_num <- same_barcode_diff_num %>%
    dplyr::group_by(code_barres_kps) %>%
    dplyr::filter(dplyr::n_distinct(vol_key, na.rm = TRUE) == 1) %>%
    dplyr::ungroup()
  
  # Same numero, different barcode
  same_numero_diff_barcode <- extractions_joined %>%
    dplyr::filter(!is.na(numero)) %>%
    dplyr::group_by(numero) %>%
    dplyr::filter(dplyr::n_distinct(code_barres_kps, na.rm = TRUE) > 1) %>%
    dplyr::ungroup()
  
  # Apply flags
  extractions_flagged <- extractions_joined %>%
    dplyr::mutate(
      flag = dplyr::case_when(
        paste(code_barres_kps, numero, vol_key) %in%
          paste(dups_exact$code_barres_kps, dups_exact$numero, dups_exact$vol_key)
        ~ "EXACT_DUPLICATE",
        
        code_barres_kps %in% reextractions$code_barres_kps &
          numero %in% reextractions$numero
        ~ "RE_EXTRACTION_SAME_BARCODE_DIFF_NUM_DIFF_VOL",
        
        code_barres_kps %in% same_barcode_same_vol_new_num$code_barres_kps &
          numero %in% same_barcode_same_vol_new_num$numero
        ~ "SUSPECT_SAME_BARCODE_SAME_VOL_NEW_NUM",
        
        numero %in% same_numero_diff_barcode$numero &
          code_barres_kps %in% same_numero_diff_barcode$code_barres_kps
        ~ "SUSPECT_SAME_NUM_DIFF_BARCODE",
        
        TRUE ~ "OK"
      )
    )
  
  # Deduplicate: keep latest file_date for exact duplicates
  extractions_dedup <- extractions_flagged %>%
    dplyr::arrange(code_barres_kps, numero, dplyr::desc(file_date)) %>%
    dplyr::group_by(code_barres_kps, numero, vol_key) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  list(
    flagged = extractions_flagged,
    dedup = extractions_dedup
  )
}
