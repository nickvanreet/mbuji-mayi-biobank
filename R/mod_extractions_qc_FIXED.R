# mod_extractions_qc_FIXED.R
# FIXED VERSION - Addresses all extraction QC issues
# =============================================================================

#' Extraction QC Module - UI (FIXED)
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
      
      shiny::h6("Volume QC Thresholds (mL)"),
      shiny::numericInput(ns("qc_min"), "Minimum acceptable", 
                          value = 1.5, min = 0, max = 5, step = 0.1, width = "100%"),
      shiny::numericInput(ns("qc_max"), "Maximum acceptable", 
                          value = 2.5, min = 0, max = 5, step = 0.1, width = "100%"),
      shiny::numericInput(ns("qc_target"), "Target volume", 
                          value = 2.0, min = 0, max = 5, step = 0.1, width = "100%"),
      
      shiny::hr(),
      
      shiny::h6("Filters (Optional)"),
      shiny::selectInput(ns("qc_prov"), "Province", 
                         choices = "All", selected = "All"),
      shiny::selectInput(ns("qc_zone"), "Zone", 
                         choices = "All", selected = "All"),
      shiny::dateRangeInput(
        ns("qc_date_rng"), "Sample date filter",
        start = NULL, end = NULL, format = "yyyy-mm-dd", weekstart = 1
      ),
      shiny::checkboxInput(ns("qc_show_out"), 
                           "Show only out-of-range volumes", value = FALSE),
      
      shiny::hr(),
      
      shiny::radioButtons(
        ns("qc_agg"), "Time Aggregation",
        choices = c("Day" = "day", "Week" = "week", "Month" = "month"),
        selected = "week", inline = TRUE
      ),
      
      shiny::hr(),
      
      shiny::downloadButton(ns("qc_dl_all"), "Download All Data", 
                            class = "btn-sm w-100 mb-2"),
      shiny::downloadButton(ns("qc_dl_outliers"), "Download Outliers Only", 
                            class = "btn-sm w-100")
    ),
    
    # Results card
    bslib::card(
      bslib::card_header("Extraction Insights"),
      bslib::navset_tab(
        bslib::nav_panel(
          "Overview",
          shiny::plotOutput(ns("qc_hist_vol"), height = 320),
          shiny::plotOutput(ns("qc_box_vol"), height = 260),
          shiny::tableOutput(ns("qc_summary_stats"))
        ),
        bslib::nav_panel(
          "By Zone",
          shiny::plotOutput(ns("qc_zone_plot"), height = 400),
          DT::DTOutput(ns("qc_zone_tbl"))
        ),
        bslib::nav_panel(
          "Time Series",
          shiny::plotOutput(ns("qc_timeseries_plot"), height = 350),
          DT::DTOutput(ns("qc_timeseries_tbl"))
        ),
        bslib::nav_panel(
          "Sample Details",
          DT::DTOutput(ns("qc_sample_tbl"))
        )
      )
    )
  )
}

#' Extraction QC Module - Server (FIXED)
#' @param id Module namespace ID
#' @param biobank_clean Reactive containing cleaned biobank data
#' @param config Reactive containing app configuration
#' @export
mod_extractions_qc_server <- function(id, biobank_clean, config) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values
    extraction_raw <- shiny::reactiveVal(tibble::tibble())
    extraction_status <- shiny::reactiveVal("No extraction data loaded.")
    
    # Initialize from config
    shiny::observe({
      cfg <- config()
      if (!is.null(cfg) && "paths" %in% names(cfg)) {
        if (exists("safe_path")) {
          shiny::updateTextInput(session, "qc_dir",
                                 value = safe_path(cfg$paths$extractions_dir))
        } else {
          shiny::updateTextInput(session, "qc_dir",
                                 value = cfg$paths$extractions_dir)
        }
      }
      if (!is.null(cfg) && "qc" %in% names(cfg)) {
        shiny::updateNumericInput(session, "qc_min", 
                                   value = cfg$qc$drs_accept_min_ml)
        shiny::updateNumericInput(session, "qc_max", 
                                   value = cfg$qc$drs_accept_max_ml)
        shiny::updateNumericInput(session, "qc_target", 
                                   value = cfg$qc$drs_target_ml)
      }
    })
    
    # Load extractions
    shiny::observeEvent(input$qc_refresh, {
      shiny::withProgress(message = 'Loading extractions...', value = 0, {
        
        dir_path <- input$qc_dir
        if (exists("safe_path")) {
          dir_path <- safe_path(dir_path)
        }
        
        if (is.null(dir_path) || is.na(dir_path) || 
            !nzchar(dir_path) || !dir.exists(dir_path)) {
          extraction_status("⚠️ Directory not found. Check the path.")
          extraction_raw(tibble::tibble())
          shiny::showNotification("Extraction directory does not exist.",
                                   type = "error")
          return()
        }
        
        shiny::incProgress(0.3, detail = "Reading files")
        
        # Read extraction files
        files <- list.files(dir_path, pattern = "\\.xlsx?$", full.names = TRUE)
        if (!length(files)) {
          extraction_status("No Excel files found in directory.")
          extraction_raw(tibble::tibble())
          shiny::showNotification("No extraction files found.", type = "warning")
          return()
        }
        
        # Read all files
        dat <- purrr::map_dfr(files, function(f) {
          tryCatch({
            # Read with text types first
            sneak <- suppressMessages(readxl::read_excel(f, n_max = 1))
            col_types <- rep("text", ncol(sneak))
            
            suppressMessages(
              readxl::read_excel(f, col_types = col_types, 
                                 .name_repair = "minimal") %>%
                janitor::clean_names()
            ) %>%
              dplyr::mutate(
                source_file = basename(f),
                file_date = extract_file_date(basename(f))
              )
          }, error = function(e) {
            warning("Failed to read file ", basename(f), ": ", e$message)
            tibble::tibble()
          })
        })
        
        if (!nrow(dat)) {
          extraction_status("Failed to read any extraction files.")
          extraction_raw(tibble::tibble())
          return()
        }
        
        shiny::incProgress(0.3, detail = "Processing volumes")
        
        # Process volume data - FIXED PARSING
        dat <- dat %>%
          dplyr::mutate(
            # Find volume column
            volume_raw = dplyr::coalesce(
              volume_total_echantillon_sang_drs_ml,
              NA_character_
            ),
            
            # Parse volume with robust function
            volume_num = parse_volume_smart(volume_raw),
            
            # Parse dates
            date_prelev = parse_any_date(
              dplyr::coalesce(
                date_de_prelevement_jj_mm_aaaa,
                date_prelevement,
                date_sample,
                NA_character_
              )
            ),
            
            # Get barcode
            barcode = dplyr::coalesce(
              code_barres_kps,
              code_barres,
              barcode,
              NA_character_
            ),
            
            # Get numero
            numero = dplyr::coalesce(
              numero_labo_dipumba,
              numero_labo,
              numero,
              lab_id,
              NA_character_
            )
          ) %>%
          dplyr::filter(!is.na(barcode) | !is.na(numero))
        
        extraction_raw(dat)
        
        file_count <- length(unique(stats::na.omit(dat$source_file)))
        sample_count <- nrow(dat)
        valid_volumes <- sum(!is.na(dat$volume_num))
        
        shiny::incProgress(0.4, detail = "Complete")
        extraction_status(sprintf(
          "✓ %s files · %s samples · %s with valid volumes",
          scales::comma(file_count),
          scales::comma(sample_count),
          scales::comma(valid_volumes)
        ))
        
        shiny::showNotification(
          sprintf("Loaded %s extraction records (%s with valid volumes)", 
                  scales::comma(sample_count),
                  scales::comma(valid_volumes)),
          type = "message"
        )
      })
    })
    
    # Join with biobank
    extractions_joined <- shiny::reactive({
      extr <- extraction_raw()
      bio <- biobank_clean()
      
      if (is.null(extr) || !nrow(extr)) return(tibble::tibble())
      if (is.null(bio) || !nrow(bio)) return(extr)
      
      # Join to get zone/province
      extr %>%
        dplyr::left_join(
          bio %>%
            dplyr::select(barcode, lab_id, zone, province) %>%
            dplyr::distinct(),
          by = c("barcode", "numero" = "lab_id")
        )
    })
    
    # Update filter choices
    shiny::observe({
      df <- extractions_joined()
      if (is.null(df) || !nrow(df)) return()
      
      # Province choices
      if ("province" %in% names(df)) {
        provs <- sort(unique(stats::na.omit(df$province)))
        if (length(provs) > 0) {
          shiny::updateSelectInput(session, "qc_prov", 
                                   choices = c("All", provs), 
                                   selected = "All")
        }
      }
      
      # Zone choices
      if ("zone" %in% names(df)) {
        zones <- sort(unique(stats::na.omit(df$zone)))
        if (length(zones) > 0) {
          shiny::updateSelectInput(session, "qc_zone", 
                                   choices = c("All", zones), 
                                   selected = "All")
        }
      }
      
      # Date range
      if ("date_prelev" %in% names(df)) {
        rng <- suppressWarnings(range(df$date_prelev, na.rm = TRUE))
        if (all(is.finite(rng))) {
          shiny::updateDateRangeInput(session, "qc_date_rng",
                                      start = rng[1], end = rng[2],
                                      min = rng[1], max = rng[2])
        }
      }
    })
    
    # Apply filters - FIXED LOGIC
    extractions_filtered <- shiny::reactive({
      df <- extractions_joined()
      
      # CRITICAL: Return data even if empty to avoid "no data" message
      if (is.null(df)) return(tibble::tibble())
      if (!nrow(df)) {
        message("No extraction data available after joining")
        return(tibble::tibble())
      }
      
      message("Starting with ", nrow(df), " extraction records")
      
      # Apply filters only if they're set and columns exist
      
      # Province filter
      if (!identical(input$qc_prov, "All") && "province" %in% names(df)) {
        df <- df %>% dplyr::filter(!is.na(province) & province == input$qc_prov)
        message("After province filter: ", nrow(df), " records")
      }
      
      # Zone filter
      if (!identical(input$qc_zone, "All") && "zone" %in% names(df)) {
        df <- df %>% dplyr::filter(!is.na(zone) & zone == input$qc_zone)
        message("After zone filter: ", nrow(df), " records")
      }
      
      # Date filter
      if (!is.null(input$qc_date_rng) && length(input$qc_date_rng) == 2 &&
          "date_prelev" %in% names(df)) {
        dr <- as.Date(input$qc_date_rng)
        if (all(!is.na(dr))) {
          df <- df %>%
            dplyr::filter(!is.na(date_prelev) &
                            date_prelev >= dr[1] &
                            date_prelev <= dr[2])
          message("After date filter: ", nrow(df), " records")
        }
      }
      
      # Outlier filter - only if checkbox is checked
      if (isTRUE(input$qc_show_out) && "volume_num" %in% names(df)) {
        qc_min <- input$qc_min
        qc_max <- input$qc_max
        df <- df %>%
          dplyr::filter(!is.na(volume_num) &
                          (volume_num < qc_min | volume_num > qc_max))
        message("After outlier filter: ", nrow(df), " records")
      }
      
      message("Returning ", nrow(df), " filtered records")
      df
    })
    
    # Output status
    output$qc_status <- shiny::renderText({
      extraction_status()
    })
    
    # PLOTS - All fixed to handle empty data gracefully
    
    output$qc_hist_vol <- shiny::renderPlot({
      df <- extractions_filtered()
      
      if (is.null(df) || !nrow(df) || !"volume_num" %in% names(df)) {
        plot.new()
        text(0.5, 0.5, "No volume data available\nLoad extraction files to see histogram", 
             cex = 1.2, col = "gray50")
        return()
      }
      
      df_plot <- df %>% dplyr::filter(!is.na(volume_num))
      
      if (!nrow(df_plot)) {
        plot.new()
        text(0.5, 0.5, "No valid volume measurements\nCheck volume column in Excel files", 
             cex = 1.2, col = "gray50")
        return()
      }
      
      qc_min <- input$qc_min
      qc_max <- input$qc_max
      qc_target <- input$qc_target
      
      ggplot2::ggplot(df_plot, ggplot2::aes(
        x = volume_num,
        fill = volume_num < qc_min | volume_num > qc_max
      )) +
        ggplot2::geom_histogram(binwidth = 0.1, colour = "black") +
        ggplot2::geom_vline(xintercept = qc_target, 
                            linetype = "dashed", colour = "red", size = 1) +
        ggplot2::geom_vline(xintercept = c(qc_min, qc_max), 
                            linetype = "dotted", colour = "grey40", size = 0.8) +
        ggplot2::scale_fill_manual(
          values = c(`FALSE` = "steelblue", `TRUE` = "tomato"),
          guide = "none"
        ) +
        ggplot2::labs(
          title = sprintf("Distribution of Sample Volumes (n=%s)", 
                          scales::comma(nrow(df_plot))),
          subtitle = sprintf(
            "Target = %.1f mL | Acceptable range = %.1f–%.1f mL",
            qc_target, qc_min, qc_max
          ),
          x = "Volume (mL)",
          y = "Count"
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })
    
    output$qc_box_vol <- shiny::renderPlot({
      df <- extractions_filtered()
      
      if (is.null(df) || !nrow(df) || !"volume_num" %in% names(df)) {
        plot.new()
        text(0.5, 0.5, "No volume data available", cex = 1.2, col = "gray50")
        return()
      }
      
      df_plot <- df %>% dplyr::filter(!is.na(volume_num))
      
      if (!nrow(df_plot)) {
        plot.new()
        text(0.5, 0.5, "No valid volume measurements", cex = 1.2, col = "gray50")
        return()
      }
      
      qc_min <- input$qc_min
      qc_max <- input$qc_max
      qc_target <- input$qc_target
      
      ggplot2::ggplot(df_plot, ggplot2::aes(y = volume_num)) +
        ggplot2::geom_boxplot(fill = "grey90", outlier.colour = "tomato") +
        ggplot2::geom_jitter(height = 0, width = 0.1, alpha = 0.4) +
        ggplot2::geom_hline(yintercept = qc_target, 
                            linetype = "dashed", colour = "red", size = 1) +
        ggplot2::geom_hline(yintercept = c(qc_min, qc_max), 
                            linetype = "dotted", colour = "grey40", size = 0.8) +
        ggplot2::labs(
          title = sprintf("Volume Distribution (n=%s)", scales::comma(nrow(df_plot))),
          y = "Volume (mL)"
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })
    
    output$qc_summary_stats <- shiny::renderTable({
      df <- extractions_filtered()
      
      if (is.null(df) || !nrow(df) || !"volume_num" %in% names(df)) {
        return(tibble::tibble(Metric = "No data", Value = ""))
      }
      
      df_vol <- df %>% dplyr::filter(!is.na(volume_num))
      
      if (!nrow(df_vol)) {
        return(tibble::tibble(Metric = "No volume data", Value = ""))
      }
      
      qc_min <- input$qc_min
      qc_max <- input$qc_max
      
      n_outliers <- sum(df_vol$volume_num < qc_min | df_vol$volume_num > qc_max)
      
      tibble::tibble(
        Metric = c(
          "Total samples",
          "With valid volumes",
          "Median volume",
          "Mean volume",
          "SD",
          "Range",
          "Out of range",
          "% Out of range"
        ),
        Value = c(
          scales::comma(nrow(df)),
          scales::comma(nrow(df_vol)),
          sprintf("%.2f mL", median(df_vol$volume_num, na.rm = TRUE)),
          sprintf("%.2f mL", mean(df_vol$volume_num, na.rm = TRUE)),
          sprintf("%.2f mL", sd(df_vol$volume_num, na.rm = TRUE)),
          sprintf("%.2f - %.2f mL", 
                  min(df_vol$volume_num, na.rm = TRUE),
                  max(df_vol$volume_num, na.rm = TRUE)),
          scales::comma(n_outliers),
          sprintf("%.1f%%", 100 * n_outliers / nrow(df_vol))
        )
      )
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    # Zone plot
    output$qc_zone_plot <- shiny::renderPlot({
      df <- extractions_filtered()
      
      if (is.null(df) || !nrow(df) || 
          !"volume_num" %in% names(df) || !"zone" %in% names(df)) {
        plot.new()
        text(0.5, 0.5, "No zone data available", cex = 1.2, col = "gray50")
        return()
      }
      
      df_plot <- df %>% 
        dplyr::filter(!is.na(volume_num) & !is.na(zone)) %>%
        dplyr::group_by(zone) %>%
        dplyr::filter(dplyr::n() >= 3) %>%
        dplyr::ungroup()
      
      if (!nrow(df_plot)) {
        plot.new()
        text(0.5, 0.5, "Not enough data per zone\n(need ≥3 samples per zone)", 
             cex = 1.2, col = "gray50")
        return()
      }
      
      qc_min <- input$qc_min
      qc_max <- input$qc_max
      qc_target <- input$qc_target
      
      ggplot2::ggplot(df_plot, ggplot2::aes(x = reorder(zone, volume_num), 
                                             y = volume_num)) +
        ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.7) +
        ggplot2::geom_hline(yintercept = qc_target, 
                            linetype = "dashed", colour = "red", size = 1) +
        ggplot2::geom_hline(yintercept = c(qc_min, qc_max), 
                            linetype = "dotted", colour = "grey40", size = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = "Volume Distribution by Health Zone",
          x = "Health Zone",
          y = "Volume (mL)"
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })
    
    # Zone table
    output$qc_zone_tbl <- DT::renderDT({
      df <- extractions_filtered()
      
      if (is.null(df) || !nrow(df) || 
          !"volume_num" %in% names(df) || !"zone" %in% names(df)) {
        return(DT::datatable(
          tibble::tibble(Message = "No zone data available"),
          options = list(pageLength = 10, dom = 't'),
          rownames = FALSE
        ))
      }
      
      qc_min <- input$qc_min
      qc_max <- input$qc_max
      
      summ <- df %>%
        dplyr::filter(!is.na(zone) & !is.na(volume_num)) %>%
        dplyr::group_by(zone) %>%
        dplyr::summarise(
          N = dplyr::n(),
          Mean = round(mean(volume_num, na.rm = TRUE), 2),
          Median = round(stats::median(volume_num, na.rm = TRUE), 2),
          SD = round(stats::sd(volume_num, na.rm = TRUE), 2),
          Min = round(min(volume_num, na.rm = TRUE), 2),
          Max = round(max(volume_num, na.rm = TRUE), 2),
          `Out of Range` = sum(volume_num < qc_min | volume_num > qc_max),
          `% Out` = round(100 * `Out of Range` / N, 1),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(`% Out`))
      
      DT::datatable(
        summ,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle(
          '% Out',
          backgroundColor = DT::styleInterval(
            c(10, 20), 
            c('white', '#fff3cd', '#f8d7da')
          )
        )
    })
    
    # Time series plot
    output$qc_timeseries_plot <- shiny::renderPlot({
      df <- extractions_filtered()
      
      if (is.null(df) || !nrow(df) || 
          !"volume_num" %in% names(df) || !"date_prelev" %in% names(df)) {
        plot.new()
        text(0.5, 0.5, "No time series data available", cex = 1.2, col = "gray50")
        return()
      }
      
      df_plot <- df %>% dplyr::filter(!is.na(date_prelev) & !is.na(volume_num))
      
      if (!nrow(df_plot)) {
        plot.new()
        text(0.5, 0.5, "No data with dates", cex = 1.2, col = "gray50")
        return()
      }
      
      # Aggregate by time period
      agg_period <- input$qc_agg
      df_agg <- df_plot %>%
        dplyr::mutate(
          period = dplyr::case_when(
            agg_period == "day" ~ date_prelev,
            agg_period == "week" ~ lubridate::floor_date(date_prelev, "week"),
            agg_period == "month" ~ lubridate::floor_date(date_prelev, "month"),
            TRUE ~ date_prelev
          )
        ) %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(
          n = dplyr::n(),
          median_vol = median(volume_num, na.rm = TRUE),
          q10 = stats::quantile(volume_num, 0.10, na.rm = TRUE),
          q90 = stats::quantile(volume_num, 0.90, na.rm = TRUE),
          .groups = "drop"
        )
      
      qc_target <- input$qc_target
      
      ggplot2::ggplot(df_agg, ggplot2::aes(x = period, y = median_vol)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = q10, ymax = q90), 
                             alpha = 0.2, fill = "steelblue") +
        ggplot2::geom_line(color = "steelblue", size = 1) +
        ggplot2::geom_point(ggplot2::aes(size = n), color = "steelblue") +
        ggplot2::geom_hline(yintercept = qc_target, 
                            linetype = "dashed", colour = "red") +
        ggplot2::scale_size_continuous(range = c(2, 6)) +
        ggplot2::labs(
          title = sprintf("Median Volume Over Time (by %s)", agg_period),
          subtitle = "Ribbon shows P10-P90 range | Point size = sample count",
          x = "Date",
          y = "Median Volume (mL)",
          size = "N samples"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    })
    
    # Time series table
    output$qc_timeseries_tbl <- DT::renderDT({
      df <- extractions_filtered()
      
      if (is.null(df) || !nrow(df) || 
          !"volume_num" %in% names(df) || !"date_prelev" %in% names(df)) {
        return(DT::datatable(
          tibble::tibble(Message = "No time series data available"),
          options = list(pageLength = 10, dom = 't'),
          rownames = FALSE
        ))
      }
      
      df_plot <- df %>% dplyr::filter(!is.na(date_prelev) & !is.na(volume_num))
      
      if (!nrow(df_plot)) {
        return(DT::datatable(
          tibble::tibble(Message = "No data with dates"),
          options = list(pageLength = 10, dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Aggregate by time period
      agg_period <- input$qc_agg
      qc_min <- input$qc_min
      qc_max <- input$qc_max
      
      df_agg <- df_plot %>%
        dplyr::mutate(
          period = dplyr::case_when(
            agg_period == "day" ~ date_prelev,
            agg_period == "week" ~ lubridate::floor_date(date_prelev, "week"),
            agg_period == "month" ~ lubridate::floor_date(date_prelev, "month"),
            TRUE ~ date_prelev
          )
        ) %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(
          N = dplyr::n(),
          Mean = round(mean(volume_num, na.rm = TRUE), 2),
          Median = round(stats::median(volume_num, na.rm = TRUE), 2),
          SD = round(stats::sd(volume_num, na.rm = TRUE), 2),
          P10 = round(stats::quantile(volume_num, 0.10, na.rm = TRUE), 2),
          P90 = round(stats::quantile(volume_num, 0.90, na.rm = TRUE), 2),
          `Out of Range` = sum(volume_num < qc_min | volume_num > qc_max),
          `% Out` = round(100 * `Out of Range` / N, 1),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(period))
      
      DT::datatable(
        df_agg,
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # Sample details table
    output$qc_sample_tbl <- DT::renderDT({
      df <- extractions_filtered()
      
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(
          tibble::tibble(Message = "No extraction data available"),
          options = list(pageLength = 10, dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Select relevant columns
      display_cols <- c(
        "barcode", "numero", "volume_raw", "volume_num", 
        "date_prelev", "zone", "province", "source_file", "file_date"
      )
      
      available_cols <- intersect(display_cols, names(df))
      
      df_display <- df %>%
        dplyr::select(dplyr::all_of(available_cols))
      
      # Add QC flag
      if ("volume_num" %in% names(df_display)) {
        qc_min <- input$qc_min
        qc_max <- input$qc_max
        df_display <- df_display %>%
          dplyr::mutate(
            QC_Status = dplyr::case_when(
              is.na(volume_num) ~ "No volume",
              volume_num < qc_min ~ "Too low",
              volume_num > qc_max ~ "Too high",
              TRUE ~ "OK"
            )
          )
      }
      
      DT::datatable(
        df_display,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'QC_Status',
          backgroundColor = DT::styleEqual(
            c('OK', 'Too low', 'Too high', 'No volume'),
            c('#d4edda', '#fff3cd', '#f8d7da', '#e2e3e5')
          )
        )
    })
    
    # Download handlers
    output$qc_dl_all <- shiny::downloadHandler(
      filename = function() {
        paste0("extractions_all_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- extractions_filtered()
        if (!is.null(df) && nrow(df) > 0) {
          readr::write_csv(df, file)
        }
      }
    )
    
    output$qc_dl_outliers <- shiny::downloadHandler(
      filename = function() {
        paste0("extractions_outliers_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- extractions_filtered()
        if (!is.null(df) && nrow(df) > 0 && "volume_num" %in% names(df)) {
          qc_min <- input$qc_min
          qc_max <- input$qc_max
          outliers <- df %>%
            dplyr::filter(!is.na(volume_num) &
                            (volume_num < qc_min | volume_num > qc_max))
          readr::write_csv(outliers, file)
        }
      }
    )
    
    # Return reactive values
    return(list(
      data = extractions_filtered
    ))
  })
}

# === HELPER FUNCTIONS ===

#' Parse volume with smart detection
parse_volume_smart <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric())
  
  # Convert to character
  x_chr <- trimws(as.character(x))
  
  # Clean common issues
  x_chr <- gsub("[^0-9.,]", "", x_chr)  # Remove non-numeric except .,
  
  # Parse
  vol <- suppressWarnings(as.numeric(gsub(",", ".", x_chr)))
  
  # Scale down if in deci-mL (> 10)
  vol <- ifelse(!is.na(vol) & vol > 10, vol / 10, vol)
  
  # Remove impossible values
  vol <- ifelse(!is.na(vol) & (vol < 0 | vol > 10), NA_real_, vol)
  
  vol
}

#' Extract date from filename (YYMMDD format)
extract_file_date <- function(filename) {
  m <- regexpr("^(\\d{6})", filename)
  if (m[1] == -1) return(as.Date(NA))
  
  date_str <- regmatches(filename, m)
  if (length(date_str) == 0 || nchar(date_str) != 6) return(as.Date(NA))
  
  as.Date(date_str, format = "%y%m%d")
}
