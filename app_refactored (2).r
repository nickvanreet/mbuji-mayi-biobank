# MBUJI-MAYI BIOBANK DASHBOARD - COMPLETE VERSION
# Production-ready version with all modules
# =============================================================================

# === SETUP ===
library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(DT)
library(sf)
library(leaflet)
library(stringi)
library(bsicons)
library(scales)
library(binom)

# Load configuration
load_config <- function() {
  if (file.exists("config.yml")) {
    cfg <- yaml::read_yaml("config.yml")
  } else {
    # Default configuration
    cfg <- list(
      paths = list(
        biobank_dir = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/01 - Biobanque",
        extractions_dir = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/02 - Extractions",
        pcr_dir = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/03 - Biologie Moléculaire/0302 - Résultats qPCR",
        elisa_pe_dir = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/04 - ELISA indirect PE/0402 - Résultats ELISA indirect PE",
        elisa_vsg_dir = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/05 - ELISA indirect VSG/0502 - Résultats ELISA indirect VSG",
        ielisa_dir = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/06 - iELISA/0602 - Résultats iELISA"
      ),
      map = list(
        use_grid3_online = TRUE,
        grid3_url = "https://services3.arcgis.com/BU6Aadhn6tbBEdyk/arcgis/rest/services/GRID3_COD_health_zones_v7_0/FeatureServer/0/query?where=1%3D1&outFields=province,zonesante&outSR=4326&f=geojson",
        province_field_regex = "(?i)prov",
        zone_field_regex = "(?i)zone|zs|zonesante"
      ),
      qc = list(
        drs_target_ml = 2.0,
        drs_accept_min_ml = 1.5,
        drs_accept_max_ml = 2.5,
        max_days_field_hs = 30,
        max_days_hs_lsd = 30,
        max_days_lsd_inrb = 90
      ),
      ui = list(
        theme_primary = "#2C3E50",
        default_date_range_days = 180,
        min_zone_n_for_pyramid = 15
      )
    )
  }
  cfg
}

# Source utility functions and modules
source("R/utils_parse.R", local = TRUE)
source("R/utils_join.R", local = TRUE)
source("R/mod_geo_map.R", local = TRUE)
source("R/mod_extractions_qc.R", local = TRUE)
source("R/mod_lab_results.R", local = TRUE)

# === UI ===
ui <- page_navbar(
  title = "Mbuji-Mayi Biobank",
  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#2C3E50",
    success = "#27AE60",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C"
  ),
  
  sidebar = sidebar(
    width = 280,
    
    h5("Data Source"),
    textInput("data_dir", "Directory", value = ""),
    uiOutput("file_selector"),
    actionButton("load_data", "Load Data", class = "btn-primary w-100 mb-3"),
    textOutput("data_status"),
    
    hr(),
    
    h5("Filters"),
    dateRangeInput("date_range", "Sample Date", 
                   start = Sys.Date() - 180, end = Sys.Date()),
    selectInput("filter_study", "Study", choices = c("All" = "all")),
    selectInput("filter_province", "Province", choices = c("All" = "all")),
    selectInput("filter_zone", "Zone", choices = c("All" = "all")),
    selectInput("filter_structure", "Structure", choices = c("All" = "all")),
    checkboxGroupInput("filter_sex", "Sex", 
                       choices = c("M", "F"), selected = c("M", "F"), inline = TRUE)
  ),
  
  # === OVERVIEW TAB ===
  nav_panel(
    title = "Overview",
    
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),
      
      value_box(
        title = "Total Samples",
        value = textOutput("vb_total"),
        showcase = bs_icon("clipboard-data"),
        theme = "primary"
      ),
      value_box(
        title = "DA Samples",
        value = textOutput("vb_da"),
        showcase = bs_icon("clipboard-check"),
        theme = "info"
      ),
      value_box(
        title = "DP Samples",
        value = textOutput("vb_dp"),
        showcase = bs_icon("clipboard-pulse"),
        theme = "success"
      ),
      value_box(
        title = "Sites Active",
        value = textOutput("vb_sites"),
        showcase = bs_icon("hospital"),
        theme = "warning"
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Sample Collection Over Time"),
        plotOutput("plot_timeline", height = 300)
      ),
      card(
        card_header("Study Distribution"),
        plotOutput("plot_study_dist", height = 300)
      )
    ),
    
    layout_columns(
      col_widths = c(4, 4, 4),
      
      card(
        card_header("Demographics Summary"),
        tableOutput("table_demographics")
      ),
      card(
        card_header("Geographic Coverage"),
        tableOutput("table_geography")
      ),
      card(
        card_header("Top Contributing Sites"),
        tableOutput("table_top_sites")
      )
    )
  ),
  
  # === DEMOGRAPHICS TAB ===
  nav_panel(
    title = "Demographics",
    
    layout_columns(
      col_widths = c(12),
      
      card(
        card_header("Age-Sex Pyramid by Zone"),
        card_body(
          fluidRow(
            column(3,
                   numericInput("pyramid_age_width", "Age group width", 
                                5, min = 1, max = 10),
                   numericInput("pyramid_min_n", "Min samples per zone", 
                                15, min = 1),
                   checkboxInput("pyramid_split_study", 
                                 "Split by study (DA/DP)", FALSE),
                   checkboxInput("pyramid_by_structure", 
                                 "Facet by structure", FALSE)
            ),
            column(9,
                   plotOutput("plot_pyramid", height = 600)
            )
          )
        )
      )
    )
  ),
  
  # === GEOGRAPHY TAB ===
  nav_panel(
    title = "Geography",
    mod_geo_map_ui("geo_map")
  ),
  
  # === EXTRACTION QC TAB ===
  nav_panel(
    title = "Extraction QC",
    mod_extractions_qc_ui("extractions_qc")
  ),
  
  # === LAB RESULTS TAB ===
  nav_panel(
    title = "Lab Results",
    mod_lab_results_ui("lab_results")
  ),
  
  # === DATA TAB ===
  nav_panel(
    title = "Data",
    
    card(
      card_header(
        "Sample Records",
        class = "d-flex justify-content-between align-items-center",
        downloadButton("download_data", "Download CSV", class = "btn-sm")
      ),
      DTOutput("data_table")
    )
  ),
  
  # === DEBUG TAB ===
  nav_panel(
    title = "Debug",
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Configuration"),
        verbatimTextOutput("debug_config")
      ),
      card(
        card_header("Data Info"),
        verbatimTextOutput("debug_data_info")
      )
    ),
    
    card(
      card_header("Key Conflicts"),
      DTOutput("debug_conflicts")
    )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  
  # Load configuration
  cfg <- shiny::reactiveVal(load_config())
  
  # Reactive values
  raw_data <- shiny::reactiveVal(NULL)
  clean_data <- shiny::reactiveVal(NULL)
  
  # Initialize directory from config
  shiny::observe({
    config <- cfg()
    if (!is.null(config) && "paths" %in% names(config)) {
      shiny::updateTextInput(session, "data_dir", 
                             value = config$paths$biobank_dir)
    }
  })
  
  # File selector
  output$file_selector <- renderUI({
    req(input$data_dir)
    files <- list.files(input$data_dir, pattern = "\\.xlsx?$", full.names = TRUE)
    if (!length(files)) return(helpText("No Excel files found"))
    
    names(files) <- basename(files)
    selectInput("selected_file", "Select file", choices = files)
  })
  
  # Load data
  observeEvent(input$load_data, {
    req(input$selected_file)
    
    showNotification("Loading data...", duration = 2, type = "message")
    
    df_raw <- tryCatch({
      readxl::read_excel(input$selected_file, .name_repair = "minimal") %>%
        janitor::clean_names() %>%
        mutate(across(everything(), as.character))
    }, error = function(e) {
      showNotification(paste("Failed to load file:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(df_raw)) return()
    
    raw_data(df_raw)
    
    # Clean data
    df_clean <- tryCatch({
      clean_biobank_data(df_raw)
    }, error = function(e) {
      showNotification(paste("Clean failed:", e$message), type = "error")
      return(tibble())
    })
    
    clean_data(df_clean)
    
    if (nrow(df_clean) == 0) {
      showNotification("Loaded file, but 0 usable rows.", type = "warning")
      return()
    }
    
    # Update filters
    studies <- sort(unique(na.omit(df_clean$study)))
    provinces <- sort(unique(na.omit(df_clean$province)))
    zones <- sort(unique(na.omit(df_clean$zone)))
    structures <- sort(unique(na.omit(df_clean$structure)))
    
    updateSelectInput(session, "filter_study", 
                      choices = c("All" = "all", studies))
    updateSelectInput(session, "filter_province", 
                      choices = c("All" = "all", provinces))
    updateSelectInput(session, "filter_zone", 
                      choices = c("All" = "all", zones))
    updateSelectInput(session, "filter_structure", 
                      choices = c("All" = "all", structures))
    
    # Update date range
    dr <- range(df_clean$date_sample, na.rm = TRUE)
    start <- max(dr[1], dr[2] - 180)
    updateDateRangeInput(session, "date_range",
                         start = start, end = dr[2],
                         min = dr[1], max = dr[2])
    
    showNotification(sprintf("Loaded %s samples", scales::comma(nrow(df_clean))), 
                     type = "message")
  })
  
  output$data_status <- renderText({
    df <- clean_data()
    if (is.null(df)) return("No data loaded")
    sprintf("%s samples loaded", scales::comma(nrow(df)))
  })
  
  # Filtered data
  filtered_data <- reactive({
    df <- clean_data()
    req(df)
    if (!nrow(df)) return(df)
    
    # Date filter
    dr <- input$date_range
    if (length(dr) == 2 && all(!is.na(dr))) {
      df <- df %>% filter(date_sample >= dr[1], date_sample <= dr[2])
    }
    
    # Study filter
    if (!identical(input$filter_study, "all")) {
      df <- df %>% filter(study == input$filter_study)
    }
    
    # Province filter
    if (!identical(input$filter_province, "all")) {
      df <- df %>% filter(province == input$filter_province)
    }
    
    # Zone filter
    if (!identical(input$filter_zone, "all")) {
      df <- df %>% filter(zone == input$filter_zone)
    }
    
    # Structure filter
    if (!identical(input$filter_structure, "all")) {
      df <- df %>% filter(structure == input$filter_structure)
    }
    
    # Sex filter
    if (length(input$filter_sex)) {
      df <- df %>% filter(sex %in% input$filter_sex)
    }
    
    df
  })
  
  # === CALL MODULES ===
  
  # Geography module
  geo_results <- mod_geo_map_server(
    "geo_map",
    biobank_filtered = filtered_data,
    lab_joined = reactive({ 
      lab_mod$lab_joined() 
    }),
    config = cfg
  )
  
  # Extraction QC module
  extraction_results <- mod_extractions_qc_server(
    "extractions_qc",
    biobank_clean = clean_data,
    config = cfg
  )
  
  # Lab results module
  lab_mod <- mod_lab_results_server(
    "lab_results",
    biobank_clean = clean_data,
    config = cfg
  )
  
  # === VALUE BOXES ===
  
  output$vb_total <- renderText({
    df <- filtered_data()
    req(df)
    scales::comma(nrow(df))
  })
  
  output$vb_da <- renderText({
    df <- filtered_data()
    req(df)
    if (!nrow(df)) return("0 (0%)")
    n_da <- sum(df$study == "DA", na.rm = TRUE)
    pct <- round(100 * n_da / nrow(df), 1)
    sprintf("%s (%s%%)", scales::comma(n_da), pct)
  })
  
  output$vb_dp <- renderText({
    df <- filtered_data()
    req(df)
    if (!nrow(df)) return("0 (0%)")
    n_dp <- sum(df$study == "DP", na.rm = TRUE)
    pct <- round(100 * n_dp / nrow(df), 1)
    sprintf("%s (%s%%)", scales::comma(n_dp), pct)
  })
  
  output$vb_sites <- renderText({
    df <- filtered_data()
    req(df)
    structures <- df %>% filter(!is.na(structure) & structure != "") %>% 
      pull(structure)
    units <- df %>% filter(!is.na(unit) & unit != "") %>% pull(unit)
    sprintf("%s structures, %s units", 
            scales::comma(n_distinct(structures)), 
            scales::comma(n_distinct(units)))
  })
  
  # === PLOTS ===
  
  output$plot_timeline <- renderPlot({
    df <- filtered_data()
    req(df)
    
    df %>%
      filter(!is.na(date_sample)) %>%
      count(week = floor_date(date_sample, "week"), study) %>%
      ggplot(aes(week, n, fill = study)) +
      geom_col() +
      scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60"),
                        na.value = "grey70") +
      labs(x = NULL, y = "Samples", fill = "Study") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$plot_study_dist <- renderPlot({
    df <- filtered_data()
    req(df)
    
    tab <- df %>%
      mutate(study = ifelse(is.na(study), "Unknown", study)) %>%
      count(study) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(tab, aes(x = "", y = n, fill = study)) +
      geom_col(width = 1, color = "white", size = 2) +
      geom_text(aes(label = sprintf("%s\n%.1f%%", scales::comma(n), pct)),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 5) +
      coord_polar("y") +
      scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60", 
                                    Unknown = "grey70")) +
      theme_void() +
      theme(legend.position = "right")
  })
  
  # Age-sex pyramid
  output$plot_pyramid <- renderPlot({
    df <- filtered_data()
    req(df, nrow(df) > 0)
    
    # Filter complete cases
    df_plot <- df %>%
      filter(!is.na(age_num), !is.na(sex), !is.na(zone)) %>%
      mutate(age_group = make_age_groups(age_num, input$pyramid_age_width))
    
    # Filter zones with min samples
    if (input$pyramid_by_structure && !identical(input$filter_structure, "all")) {
      keep <- df_plot %>%
        count(province, structure) %>%
        filter(n >= input$pyramid_min_n)
      
      if (nrow(keep) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0, y = 0,
                     label = "No structures meet minimum sample threshold",
                     size = 6) +
            theme_void()
        )
      }
      
      df_plot <- df_plot %>%
        semi_join(keep, by = c("province", "structure"))
    } else {
      zones_keep <- df_plot %>%
        count(province, zone) %>%
        filter(n >= input$pyramid_min_n)
      
      if (nrow(zones_keep) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0, y = 0,
                     label = "No zones meet minimum sample threshold",
                     size = 6) +
            theme_void()
        )
      }
      
      df_plot <- df_plot %>%
        semi_join(zones_keep, by = c("province", "zone"))
    }
    
    # Aggregate
    df_agg <- df_plot %>%
      count(province, zone, structure, age_group, sex, study) %>%
      complete(province, zone, structure, age_group, sex, study, 
               fill = list(n = 0)) %>%
      mutate(n_signed = if_else(sex == "M", -n, n))
    
    # Plot
    p <- ggplot(df_agg, aes(x = n_signed, y = age_group, fill = sex)) +
      geom_col(width = 0.9, color = "grey40") +
      geom_vline(xintercept = 0, color = "grey30") +
      scale_x_continuous("Count (M left, F right)", labels = abs) +
      scale_fill_manual(values = c(M = "#3498DB", F = "#E74C3C")) +
      labs(y = "Age group", fill = "Sex") +
      theme_minimal(base_size = 12)
    
    # Faceting
    if (input$pyramid_by_structure) {
      if (input$pyramid_split_study) {
        p + facet_grid(study + province ~ structure, 
                       scales = "free_x", space = "free_x")
      } else {
        p + facet_grid(province ~ structure, 
                       scales = "free_x", space = "free_x")
      }
    } else {
      if (input$pyramid_split_study) {
        p + facet_grid(study + province ~ zone, 
                       scales = "free_x", space = "free_x")
      } else {
        p + facet_grid(province ~ zone, 
                       scales = "free_x", space = "free_x")
      }
    }
  }, res = 96)
  
  # === TABLES ===
  
  output$table_demographics <- renderTable({
    df <- filtered_data()
    req(df)
    
    tibble(
      Metric = c("Median age", "% Female", "Age range"),
      Value = c(
        sprintf("%.1f years", median(df$age_num, na.rm = TRUE)),
        sprintf("%.1f%%", mean(df$sex == "F", na.rm = TRUE) * 100),
        sprintf("%.0f - %.0f", 
                min(df$age_num, na.rm = TRUE),
                max(df$age_num, na.rm = TRUE))
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_geography <- renderTable({
    df <- filtered_data()
    req(df)
    
    tibble(
      Metric = c("Provinces", "Health zones", "Structures", "Mobile units"),
      Count = c(
        n_distinct(df$province, na.rm = TRUE),
        n_distinct(df$zone, na.rm = TRUE),
        n_distinct(df$structure, na.rm = TRUE),
        n_distinct(df$unit, na.rm = TRUE)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_top_sites <- renderTable({
    df <- filtered_data()
    req(df)
    
    df %>%
      filter(!is.na(structure)) %>%
      count(structure, sort = TRUE) %>%
      head(5) %>%
      mutate(n = scales::comma(n)) %>%
      rename(Structure = structure, Samples = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Data table
  output$data_table <- renderDT({
    df <- filtered_data()
    req(df)
    
    df %>%
      select(barcode, lab_id, date_sample, date_received, date_result,
             study, sex, age_num, province, zone, structure, unit) %>%
      datatable(
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      sprintf("biobank_export_%s.csv", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  # Debug outputs
  output$debug_config <- renderPrint({
    cfg()
  })
  
  output$debug_data_info <- renderPrint({
    df <- clean_data()
    if (is.null(df)) return("No data loaded")
    
    list(
      rows = nrow(df),
      columns = ncol(df),
      column_names = names(df),
      study_breakdown = table(df$study, useNA = "ifany"),
      date_range = range(df$date_sample, na.rm = TRUE)
    )
  })
  
  output$debug_conflicts <- renderDT({
    # Placeholder for key conflicts
    tibble(
      Message = "Key conflict detection coming soon"
    ) %>%
      datatable(options = list(pageLength = 10))
  })
}

# === HELPER FUNCTIONS ===

# Helper function: Clean biobank data
clean_biobank_data <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(tibble())
  }
  
  # Rename columns
  rename_first <- function(df, new_name, pattern) {
    hits <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hits) >= 1) {
      df <- df %>% rename(!!new_name := all_of(hits[1]))
    }
    df
  }
  
  df <- df %>%
    rename_first("barcode", "code.*barr|barcode") %>%
    rename_first("lab_id", "num[eé]ro|id.*lab") %>%
    rename_first("date_raw", "date.*pr[eé]lev|date.*sample|^date$") %>%
    rename_first("age", "^age") %>%
    rename_first("sex", "^sex|^sexe|^gender") %>%
    rename_first("zone", "zone.*sant[eé]|health.*zone|^zs$") %>%
    rename_first("province", "^province") %>%
    rename_first("study", "etude|study|passif|actif") %>%
    rename_first("structure", "structure.*sanit|facility") %>%
    rename_first("unit", "unit[eé].*mobile|mobile.*unit") %>%
    rename_first("date_received_raw", "date.*recept|date.*arriv") %>%
    rename_first("date_result_raw", "date.*result|result.*date") %>%
    rename_first("date_env_cpltha_raw", "date.*env.*cpltha") %>%
    rename_first("date_rec_cpltha_raw", "date.*recept.*cpltha") %>%
    rename_first("date_env_inrb_raw", "date.*env.*inrb") %>%
    rename_first("temp_transport_raw", "temp.*transport") %>%
    rename_first("temp_cpltha_raw", "temp.*stockage|temp.*cpltha")
  
  # Ensure required columns exist
  required <- c("barcode", "lab_id", "date_raw", "age", "sex", "zone", "province",
                "study", "structure", "unit", "date_received_raw", "date_result_raw",
                "date_env_cpltha_raw", "date_rec_cpltha_raw", "date_env_inrb_raw",
                "temp_transport_raw", "temp_cpltha_raw")
  for (col in required) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  
  # Parse and standardize
  df %>%
    mutate(
      date_sample = parse_any_date(date_raw),
      date_received = parse_any_date(date_received_raw),
      date_result = parse_any_date(date_result_raw),
      date_env_cpltha = parse_any_date(date_env_cpltha_raw),
      date_rec_cpltha = parse_any_date(date_rec_cpltha_raw),
      date_env_inrb = parse_any_date(date_env_inrb_raw),
      
      age_num = parse_age(age),
      sex = parse_sex_code(sex),
      study = parse_study_code(study),
      
      temp_field = parse_temp_code(temp_transport_raw),
      temp_hs = parse_temp_code(temp_cpltha_raw),
      
      # Transport segments
      transport_field_hs = safe_days_between(date_env_cpltha, date_sample, 30),
      transport_hs_lsd = safe_days_between(date_rec_cpltha, date_env_cpltha, 30),
      transport_lsd_inrb = safe_days_between(date_env_inrb, date_rec_cpltha, 90),
      
      zone = str_squish(as.character(zone)),
      province = str_squish(as.character(province)),
      structure = str_squish(as.character(structure)),
      unit = str_squish(as.character(unit))
    ) %>%
    filter(!is.na(date_sample)) %>%
    distinct(barcode, lab_id, .keep_all = TRUE)
}

# Helper function: Make age groups
make_age_groups <- function(age, width = 5) {
  if (all(is.na(age))) return(factor(levels = character()))
  hi <- suppressWarnings(ceiling(max(age, na.rm = TRUE) / width) * width)
  if (!is.finite(hi) || hi <= 0) hi <- width
  cut(age, breaks = seq(0, hi, by = width), right = FALSE, include.lowest = TRUE)
}

shinyApp(ui, server)C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/03 - Biologie Moléculaire/0302 - Résultats qPCR"
      ),
      qc = list(
        drs_target_ml = 2.0,
        drs_accept_min_ml = 1.5,
        drs_accept_max_ml = 2.5
      ),
      ui = list(
        theme_primary = "#2C3E50",
        default_date_range_days = 180
      )
    )
  }
  cfg
}

# Source utility functions
source("R/utils_parse.R", local = TRUE)
source("R/utils_join.R", local = TRUE)

# === UI ===
ui <- page_navbar(
  title = "Mbuji-Mayi Biobank",
  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#2C3E50",
    success = "#27AE60",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C"
  ),
  
  sidebar = sidebar(
    width = 280,
    
    h5("Data Source"),
    textInput("data_dir", "Directory", value = ""),
    uiOutput("file_selector"),
    actionButton("load_data", "Load Data", class = "btn-primary w-100 mb-3"),
    textOutput("data_status"),
    
    hr(),
    
    h5("Filters"),
    dateRangeInput("date_range", "Sample Date", 
                   start = Sys.Date() - 180, end = Sys.Date()),
    selectInput("filter_study", "Study", choices = c("All" = "all")),
    selectInput("filter_province", "Province", choices = c("All" = "all")),
    selectInput("filter_zone", "Zone", choices = c("All" = "all")),
    checkboxGroupInput("filter_sex", "Sex", 
                       choices = c("M", "F"), selected = c("M", "F"), inline = TRUE)
  ),
  
  # Overview tab
  nav_panel(
    title = "Overview",
    
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),
      
      value_box(
        title = "Total Samples",
        value = textOutput("vb_total"),
        showcase = bs_icon("clipboard-data"),
        theme = "primary"
      ),
      value_box(
        title = "DA Samples",
        value = textOutput("vb_da"),
        showcase = bs_icon("clipboard-check"),
        theme = "info"
      ),
      value_box(
        title = "DP Samples",
        value = textOutput("vb_dp"),
        showcase = bs_icon("clipboard-pulse"),
        theme = "success"
      ),
      value_box(
        title = "Sites Active",
        value = textOutput("vb_sites"),
        showcase = bs_icon("hospital"),
        theme = "warning"
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Sample Collection Over Time"),
        plotOutput("plot_timeline", height = 300)
      ),
      card(
        card_header("Study Distribution"),
        plotOutput("plot_study_dist", height = 300)
      )
    )
  ),
  
  # Data table tab
  nav_panel(
    title = "Data",
    
    card(
      card_header(
        "Sample Records",
        class = "d-flex justify-content-between align-items-center",
        downloadButton("download_data", "Download CSV", class = "btn-sm")
      ),
      DTOutput("data_table")
    )
  ),
  
  # Debug tab
  nav_panel(
    title = "Debug",
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Configuration"),
        verbatimTextOutput("debug_config")
      ),
      card(
        card_header("Data Info"),
        verbatimTextOutput("debug_data_info")
      )
    )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  
  # Load configuration
  cfg <- shiny::reactiveVal(load_config())
  
  # Reactive values
  raw_data <- shiny::reactiveVal(NULL)
  clean_data <- shiny::reactiveVal(NULL)
  
  # Initialize directory from config
  shiny::observe({
    config <- cfg()
    if (!is.null(config) && "paths" %in% names(config)) {
      shiny::updateTextInput(session, "data_dir", 
                             value = config$paths$biobank_dir)
    }
  })
  
  # File selector
  output$file_selector <- renderUI({
    req(input$data_dir)
    files <- list.files(input$data_dir, pattern = "\\.xlsx?$", full.names = TRUE)
    if (!length(files)) return(helpText("No Excel files found"))
    
    names(files) <- basename(files)
    selectInput("selected_file", "Select file", choices = files)
  })
  
  # Load data
  observeEvent(input$load_data, {
    req(input$selected_file)
    
    showNotification("Loading data...", duration = 2, type = "message")
    
    df_raw <- tryCatch({
      readxl::read_excel(input$selected_file, .name_repair = "minimal") %>%
        janitor::clean_names() %>%
        mutate(across(everything(), as.character))
    }, error = function(e) {
      showNotification(paste("Failed to load file:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(df_raw)) return()
    
    raw_data(df_raw)
    
    # Clean data
    df_clean <- tryCatch({
      clean_biobank_data(df_raw)
    }, error = function(e) {
      showNotification(paste("Clean failed:", e$message), type = "error")
      return(tibble())
    })
    
    clean_data(df_clean)
    
    if (nrow(df_clean) == 0) {
      showNotification("Loaded file, but 0 usable rows.", type = "warning")
      return()
    }
    
    # Update filters
    studies <- sort(unique(na.omit(df_clean$study)))
    provinces <- sort(unique(na.omit(df_clean$province)))
    zones <- sort(unique(na.omit(df_clean$zone)))
    
    updateSelectInput(session, "filter_study", choices = c("All" = "all", studies))
    updateSelectInput(session, "filter_province", choices = c("All" = "all", provinces))
    updateSelectInput(session, "filter_zone", choices = c("All" = "all", zones))
    
    # Update date range
    dr <- range(df_clean$date_sample, na.rm = TRUE)
    start <- max(dr[1], dr[2] - 180)
    updateDateRangeInput(session, "date_range",
                         start = start, end = dr[2],
                         min = dr[1], max = dr[2])
    
    showNotification(sprintf("Loaded %s samples", scales::comma(nrow(df_clean))), 
                     type = "message")
  })
  
  output$data_status <- renderText({
    df <- clean_data()
    if (is.null(df)) return("No data loaded")
    sprintf("%s samples loaded", scales::comma(nrow(df)))
  })
  
  # Filtered data
  filtered_data <- reactive({
    df <- clean_data()
    req(df)
    if (!nrow(df)) return(df)
    
    # Date filter
    dr <- input$date_range
    if (length(dr) == 2 && all(!is.na(dr))) {
      df <- df %>% filter(date_sample >= dr[1], date_sample <= dr[2])
    }
    
    # Study filter
    if (!identical(input$filter_study, "all")) {
      df <- df %>% filter(study == input$filter_study)
    }
    
    # Province filter
    if (!identical(input$filter_province, "all")) {
      df <- df %>% filter(province == input$filter_province)
    }
    
    # Zone filter
    if (!identical(input$filter_zone, "all")) {
      df <- df %>% filter(zone == input$filter_zone)
    }
    
    # Sex filter
    if (length(input$filter_sex)) {
      df <- df %>% filter(sex %in% input$filter_sex)
    }
    
    df
  })
  
  # Value boxes
  output$vb_total <- renderText({
    df <- filtered_data()
    req(df)
    scales::comma(nrow(df))
  })
  
  output$vb_da <- renderText({
    df <- filtered_data()
    req(df)
    if (!nrow(df)) return("0 (0%)")
    n_da <- sum(df$study == "DA", na.rm = TRUE)
    pct <- round(100 * n_da / nrow(df), 1)
    sprintf("%s (%s%%)", scales::comma(n_da), pct)
  })
  
  output$vb_dp <- renderText({
    df <- filtered_data()
    req(df)
    if (!nrow(df)) return("0 (0%)")
    n_dp <- sum(df$study == "DP", na.rm = TRUE)
    pct <- round(100 * n_dp / nrow(df), 1)
    sprintf("%s (%s%%)", scales::comma(n_dp), pct)
  })
  
  output$vb_sites <- renderText({
    df <- filtered_data()
    req(df)
    structures <- df %>% filter(!is.na(structure) & structure != "") %>% pull(structure)
    units <- df %>% filter(!is.na(unit) & unit != "") %>% pull(unit)
    sprintf("%s structures, %s units", 
            scales::comma(n_distinct(structures)), 
            scales::comma(n_distinct(units)))
  })
  
  # Plots
  output$plot_timeline <- renderPlot({
    df <- filtered_data()
    req(df)
    
    df %>%
      filter(!is.na(date_sample)) %>%
      count(week = floor_date(date_sample, "week"), study) %>%
      ggplot(aes(week, n, fill = study)) +
      geom_col() +
      scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60"),
                        na.value = "grey70") +
      labs(x = NULL, y = "Samples", fill = "Study") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$plot_study_dist <- renderPlot({
    df <- filtered_data()
    req(df)
    
    tab <- df %>%
      mutate(study = ifelse(is.na(study), "Unknown", study)) %>%
      count(study) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(tab, aes(x = "", y = n, fill = study)) +
      geom_col(width = 1, color = "white", size = 2) +
      geom_text(aes(label = sprintf("%s\n%.1f%%", scales::comma(n), pct)),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 5) +
      coord_polar("y") +
      scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60", Unknown = "grey70")) +
      theme_void() +
      theme(legend.position = "right")
  })
  
  # Data table
  output$data_table <- renderDT({
    df <- filtered_data()
    req(df)
    
    df %>%
      select(barcode, lab_id, date_sample, date_received, date_result,
             study, sex, age_num, province, zone, structure, unit) %>%
      datatable(
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      sprintf("biobank_export_%s.csv", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  # Debug outputs
  output$debug_config <- renderPrint({
    cfg()
  })
  
  output$debug_data_info <- renderPrint({
    df <- clean_data()
    if (is.null(df)) return("No data loaded")
    
    list(
      rows = nrow(df),
      columns = ncol(df),
      column_names = names(df),
      study_breakdown = table(df$study, useNA = "ifany"),
      date_range = range(df$date_sample, na.rm = TRUE)
    )
  })
}

# Helper function: Clean biobank data
clean_biobank_data <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(tibble())
  }
  
  # Rename columns (simplified version)
  rename_first <- function(df, new_name, pattern) {
    hits <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hits) >= 1) {
      df <- df %>% rename(!!new_name := all_of(hits[1]))
    }
    df
  }
  
  df <- df %>%
    rename_first("barcode", "code.*barr|barcode") %>%
    rename_first("lab_id", "num[eé]ro|id.*lab") %>%
    rename_first("date_raw", "date.*pr[eé]lev|date.*sample|^date$") %>%
    rename_first("age", "^age") %>%
    rename_first("sex", "^sex|^sexe|^gender") %>%
    rename_first("zone", "zone.*sant[eé]|health.*zone|^zs$") %>%
    rename_first("province", "^province") %>%
    rename_first("study", "etude|study|passif|actif") %>%
    rename_first("structure", "structure.*sanit|facility") %>%
    rename_first("unit", "unit[eé].*mobile|mobile.*unit") %>%
    rename_first("date_received_raw", "date.*recept|date.*arriv") %>%
    rename_first("date_result_raw", "date.*result|result.*date")
  
  # Ensure required columns exist
  required <- c("barcode", "lab_id", "date_raw", "age", "sex", "zone", "province", 
                "study", "structure", "unit", "date_received_raw", "date_result_raw")
  for (col in required) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  
  # Parse and standardize
  df %>%
    mutate(
      date_sample = parse_any_date(date_raw),
      date_received = parse_any_date(date_received_raw),
      date_result = parse_any_date(date_result_raw),
      age_num = parse_age(age),
      sex = parse_sex_code(sex),
      study = parse_study_code(study),
      zone = str_squish(as.character(zone)),
      province = str_squish(as.character(province)),
      structure = str_squish(as.character(structure)),
      unit = str_squish(as.character(unit))
    ) %>%
    filter(!is.na(date_sample)) %>%
    distinct(barcode, lab_id, .keep_all = TRUE)
}

shinyApp(ui, server)
