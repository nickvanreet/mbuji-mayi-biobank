# MBUJI-MAYI BIOBANK DASHBOARD — PRODUCTION APP
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
library(yaml)

# --- CONFIG LOADER -----------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

default_config <- function() {
  list(
    paths = list(
      biobank_dir   = "data/biobank",
      extractions_dir = "data/extractions",
      pcr_dir         = "data/pcr",
      elisa_pe_dir    = "data/elisa_pe",
      elisa_vsg_dir   = "data/elisa_vsg",
      ielisa_dir      = "data/ielisa"
    ),
    map = list(
      use_grid3_online = TRUE,
      grid3_url = "https://services3.arcgis.com/BU6Aadhn6tbBEdyk/arcgis/rest/services/GRID3_COD_health_zones_v7_0/FeatureServer/0/query?where=1%3D1&outFields=province,zonesante&outSR=4326&f=geojson",
      province_field_regex = "(?i)prov",
      zone_field_regex = "(?i)zone|zs|zonesante",
      fallback_shapefile = "testdata/cod_health_zones.gpkg"
    ),
    qc = list(
      drs_target_ml = 2.0,
      drs_accept_min_ml = 1.5,
      drs_accept_max_ml = 2.5,
      max_transport_field_hs_days = 30,
      max_transport_hs_lsd_days = 30,
      max_transport_lsd_inrb_days = 90,
      max_doorlooptijd_days = 90
    ),
    ui = list(
      theme_primary = "#2C3E50",
      theme_success = "#27AE60",
      theme_info    = "#3498DB",
      theme_warning = "#F39C12",
      theme_danger  = "#E74C3C",
      cache_minutes = 10,
      default_date_range_days = 180
    ),
    app = list(
      title = "Mbuji-Mayi Biobank Dashboard",
      version = "0.0.0",
      institution = "Institute of Tropical Medicine, Antwerp",
      last_updated = as.character(Sys.Date())
    )
  )
}

# ----- ROBUST WINDOWS PATH HANDLING -----------------------------------------
is_windows <- function() .Platform$OS.type == "windows"

# Try several encodings & representations; return first that exists (or last tried)
resolve_windows_path <- function(p) {
  if (is.null(p) || !nzchar(p)) return(p)
  
  candidates <- character(0)
  
  # raw & normalized slashes
  candidates <- c(candidates, p, gsub("\\\\", "/", p), gsub("/", "\\\\", p))
  
  # encoding juggling
  candidates <- c(candidates, enc2native(p), enc2utf8(p))
  
  # normalizePath attempts (may not exist yet)
  for (cand in unique(candidates)) {
    try({
      norm <- normalizePath(cand, winslash = "/", mustWork = FALSE)
      candidates <- c(candidates, norm)
    }, silent = TRUE)
  }
  
  # long path prefix (\\?\C:\...)
  if (grepl("^[A-Za-z]:[\\/]", p)) {
    longp <- paste0("\\\\?\\", gsub("/", "\\\\", p))
    candidates <- c(candidates, longp)
    try({
      norm_long <- normalizePath(longp, winslash = "/", mustWork = FALSE)
      candidates <- c(candidates, norm_long)
    }, silent = TRUE)
  }
  
  # 8.3 short path (bypasses diacritics) — only if it exists
  if (is_windows()) {
    # shortPathName errors if not found; so try safely
    for (cand in unique(candidates)) {
      sp <- try(utils::shortPathName(cand), silent = TRUE)
      if (!inherits(sp, "try-error") && nzchar(sp)) {
        candidates <- c(candidates, sp, gsub("\\\\", "/", sp))
      }
    }
  }
  
  candidates <- unique(candidates)
  
  # pick first that exists as *directory*
  for (cand in candidates) {
    if (dir.exists(cand)) return(cand)
  }
  
  # nothing exists — return best-normalized guess (first normalizePath result if any)
  if (length(candidates)) return(candidates[[length(candidates)]])
  p
}

normalize_paths <- function(cfg) {
  if (!is.list(cfg$paths)) return(cfg)
  cfg$paths <- lapply(cfg$paths, function(p) {
    if (is.null(p) || !nzchar(p)) return(p)
    if (is_windows()) resolve_windows_path(p) else normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  })
  cfg
}

load_config_once <- function(path = "config.yml") {
  base <- default_config()
  if (!file.exists(path)) {
    message("config.yml not found; using defaults.")
    return(normalize_paths(base))
  }
  user <- yaml::read_yaml(path)
  cfg  <- modifyList(base, user, keep.null = TRUE)
  normalize_paths(cfg)
}

# Hot-reload wrapper for Shiny
reactive_config <- function(session, path = "config.yml", interval_ms = 3000) {
  shiny::reactiveFileReader(interval_ms, session, path, function(p) load_config_once(p))
}

# --- HELPERS & MODULES -------------------------------------------------------
source("R/utils_parse.R", local = TRUE)       # parse_any_date, parse_age, parse_* codes, safe_days_between
source("R/utils_join.R",  local = TRUE)
source("R/mod_geo_map_complete.R",     local = TRUE)
source("R/mod_extractions_complete.R", local = TRUE)
source("R/mod_lab_results_complete.R", local = TRUE)

# One canonical cleaner (used everywhere)
clean_biobank_data <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(tibble())
  
  rename_first <- function(df, new_name, pattern) {
    hits <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hits) >= 1) df <- dplyr::rename(df, !!new_name := dplyr::all_of(hits[1]))
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
  
  required <- c(
    "barcode","lab_id","date_raw","age","sex","zone","province","study","structure","unit",
    "date_received_raw","date_result_raw","date_env_cpltha_raw","date_rec_cpltha_raw","date_env_inrb_raw",
    "temp_transport_raw","temp_cpltha_raw"
  )
  for (col in required) if (!col %in% names(df)) df[[col]] <- NA_character_
  
  df %>%
    mutate(
      date_sample     = parse_any_date(date_raw),
      date_received   = parse_any_date(date_received_raw),
      date_result     = parse_any_date(date_result_raw),
      date_env_cpltha = parse_any_date(date_env_cpltha_raw),
      date_rec_cpltha = parse_any_date(date_rec_cpltha_raw),
      date_env_inrb   = parse_any_date(date_env_inrb_raw),
      
      age_num = parse_age(age),
      sex     = parse_sex_code(sex),
      study   = parse_study_code(study),
      
      temp_field = parse_temp_code(temp_transport_raw),
      temp_hs    = parse_temp_code(temp_cpltha_raw),
      
      transport_field_hs = safe_days_between(date_env_cpltha, date_sample, 30),
      transport_hs_lsd   = safe_days_between(date_rec_cpltha, date_env_cpltha, 30),
      transport_lsd_inrb = safe_days_between(date_env_inrb, date_rec_cpltha, 90),
      
      zone      = str_squish(as.character(zone)),
      province  = str_squish(as.character(province)),
      structure = str_squish(as.character(structure)),
      unit      = str_squish(as.character(unit))
    ) %>%
    filter(!is.na(date_sample)) %>%
    distinct(barcode, lab_id, .keep_all = TRUE)
}

make_age_groups <- function(age, width = 5) {
  if (all(is.na(age))) return(factor(levels = character()))
  hi <- suppressWarnings(ceiling(max(age, na.rm = TRUE) / width) * width)
  if (!is.finite(hi) || hi <= 0) hi <- width
  cut(age, breaks = seq(0, hi, by = width), right = FALSE, include.lowest = TRUE)
}

# === UI ======================================================================
ui <- page_navbar(
  title = "Mbuji-Mayi Biobank",
  theme = bs_theme(
    version = 5, preset = "bootstrap",
    primary = "#2C3E50", success = "#27AE60", info = "#3498DB",
    warning = "#F39C12", danger = "#E74C3C"
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
      fill = FALSE, col_widths = c(3,3,3,3),
      value_box("Total Samples", textOutput("vb_total"), bs_icon("clipboard-data"),  theme = "primary"),
      value_box("DA Samples",    textOutput("vb_da"),    bs_icon("clipboard-check"), theme = "info"),
      value_box("DP Samples",    textOutput("vb_dp"),    bs_icon("clipboard-pulse"), theme = "success"),
      value_box("Sites Active",  textOutput("vb_sites"), bs_icon("hospital"),        theme = "warning")
    ),
    layout_columns(
      col_widths = c(6,6),
      card(card_header("Sample Collection Over Time"), plotOutput("plot_timeline", height = 300)),
      card(card_header("Study Distribution"),          plotOutput("plot_study_dist", height = 300))
    ),
    layout_columns(
      col_widths = c(4,4,4),
      card(card_header("Demographics Summary"), tableOutput("table_demographics")),
      card(card_header("Geographic Coverage"),  tableOutput("table_geography")),
      card(card_header("Top Contributing Sites"), tableOutput("table_top_sites"))
    )
  ),
  
  # === DEMOGRAPHICS TAB ===
  nav_panel(
    title = "Demographics",
    card(
      card_header("Age-Sex Pyramid by Zone"),
      card_body(
        fluidRow(
          column(3,
                 numericInput("pyramid_age_width", "Age group width", 5, min = 1, max = 10),
                 numericInput("pyramid_min_n", "Min samples per zone", 15, min = 1),
                 checkboxInput("pyramid_split_study", "Split by study (DA/DP)", FALSE),
                 checkboxInput("pyramid_by_structure", "Facet by structure", FALSE)
          ),
          column(9, plotOutput("plot_pyramid", height = 600))
        )
      )
    )
  ),
  
  # === GEOGRAPHY / QC / LAB RESULTS ===
  nav_panel(title = "Geography",     mod_geo_map_ui("geo_map")),
  nav_panel(title = "Extraction QC", mod_extractions_qc_ui("extractions_qc")),
  nav_panel(title = "Lab Results",   mod_lab_results_ui("lab_results")),
  
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
      col_widths = c(6,6),
      card(card_header("Configuration"), verbatimTextOutput("debug_config")),
      card(card_header("Data Info"),     verbatimTextOutput("debug_data_info"))
    ),
    card(card_header("Key Conflicts"), DTOutput("debug_conflicts"))
  )
)

# === SERVER ==================================================================
server <- function(input, output, session) {
  
  # Config (hot-reload on file change)
  cfg <- reactive_config(session, "config.yml", interval_ms = 3000)
  
  # Reactives
  raw_data   <- reactiveVal(NULL)
  data_clean <- reactiveVal(NULL)
  
  # Initialize directory from config
  observe({
    conf <- cfg()
    if (!is.null(conf$paths$biobank_dir)) {
      updateTextInput(session, "data_dir", value = conf$paths$biobank_dir)
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
    showNotification("Loading data...", duration = 1.5, type = "message")
    
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
    
    df_clean <- tryCatch(clean_biobank_data(df_raw), error = function(e) {
      showNotification(paste("Clean failed:", e$message), type = "error")
      tibble()
    })
    data_clean(df_clean)
    
    if (!nrow(df_clean)) {
      showNotification("Loaded file, but 0 usable rows.", type = "warning")
      return()
    }
    
    # Update filters
    studies    <- sort(unique(na.omit(df_clean$study)))
    provinces  <- sort(unique(na.omit(df_clean$province)))
    zones      <- sort(unique(na.omit(df_clean$zone)))
    structures <- sort(unique(na.omit(df_clean$structure)))
    
    updateSelectInput(session, "filter_study",     choices = c("All" = "all", studies))
    updateSelectInput(session, "filter_province",  choices = c("All" = "all", provinces))
    updateSelectInput(session, "filter_zone",      choices = c("All" = "all", zones))
    updateSelectInput(session, "filter_structure", choices = c("All" = "all", structures))
    
    # Date range
    dr <- range(df_clean$date_sample, na.rm = TRUE)
    start <- max(dr[1], dr[2] - cfg()$ui$default_date_range_days)
    updateDateRangeInput(session, "date_range", start = start, end = dr[2], min = dr[1], max = dr[2])
    
    showNotification(sprintf("Loaded %s samples", scales::comma(nrow(df_clean))), type = "message")
  })
  
  output$data_status <- renderText({
    df <- data_clean()
    if (is.null(df)) return("No data loaded")
    sprintf("%s samples loaded", scales::comma(nrow(df)))
  })
  
  output$debug_paths <- renderTable({
    conf <- cfg()
    paths <- conf$paths
    tibble::tibble(
      key   = names(paths),
      value = unname(unlist(paths)),
      exists = vapply(paths, dir.exists, logical(1)),
      files_example = vapply(paths, function(d) {
        if (dir.exists(d)) {
          # show 1 example file if present (helpful to see encoding)
          f <- list.files(d, full.names = TRUE, n = 1)
          if (length(f)) f else ""
        } else ""
      }, character(1))
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Filtered data
  filtered_data <- reactive({
    df <- data_clean(); req(df)
    if (!nrow(df)) return(df)
    
    dr <- input$date_range
    if (length(dr) == 2 && all(!is.na(dr))) df <- df %>% filter(date_sample >= dr[1], date_sample <= dr[2])
    if (!identical(input$filter_study, "all"))     df <- df %>% filter(study     == input$filter_study)
    if (!identical(input$filter_province, "all"))  df <- df %>% filter(province  == input$filter_province)
    if (!identical(input$filter_zone, "all"))      df <- df %>% filter(zone      == input$filter_zone)
    if (!identical(input$filter_structure, "all")) df <- df %>% filter(structure == input$filter_structure)
    if (length(input$filter_sex))                  df <- df %>% filter(sex %in% input$filter_sex)
    df
  })
  
  # --- MODULES (order matters) ----------------------------------------------
  lab_mod <- mod_lab_results_server(
    "lab_results",
    biobank_clean = data_clean,
    config = cfg
  )
  
  extraction_results <- mod_extractions_qc_server(
    "extractions_qc",
    biobank_clean = data_clean,
    config = cfg
  )
  
  geo_results <- mod_geo_map_server(
    "geo_map",
    biobank_filtered = filtered_data,
    lab_joined = reactive(lab_mod$lab_joined()),
    config = cfg
  )
  
  # === VALUE BOXES ===
  output$vb_total <- renderText({
    df <- filtered_data(); req(df); scales::comma(nrow(df))
  })
  output$vb_da <- renderText({
    df <- filtered_data(); req(df)
    if (!nrow(df)) return("0 (0%)")
    n_da <- sum(df$study == "DA", na.rm = TRUE)
    sprintf("%s (%.1f%%)", scales::comma(n_da), 100 * n_da / nrow(df))
  })
  output$vb_dp <- renderText({
    df <- filtered_data(); req(df)
    if (!nrow(df)) return("0 (0%)")
    n_dp <- sum(df$study == "DP", na.rm = TRUE)
    sprintf("%s (%.1f%%)", scales::comma(n_dp), 100 * n_dp / nrow(df))
  })
  output$vb_sites <- renderText({
    df <- filtered_data(); req(df)
    structures <- df %>% filter(!is.na(structure) & structure != "") %>% pull(structure)
    units      <- df %>% filter(!is.na(unit) & unit != "") %>% pull(unit)
    sprintf("%s structures, %s units",
            scales::comma(dplyr::n_distinct(structures)),
            scales::comma(dplyr::n_distinct(units)))
  })
  
  # === PLOTS ===
  output$plot_timeline <- renderPlot({
    df <- filtered_data(); req(df)
    df %>%
      filter(!is.na(date_sample)) %>%
      count(week = floor_date(date_sample, "week"), study) %>%
      ggplot(aes(week, n, fill = study)) +
      geom_col() +
      scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60"), na.value = "grey70") +
      labs(x = NULL, y = "Samples", fill = "Study") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$plot_study_dist <- renderPlot({
    df <- filtered_data(); req(df)
    tab <- df %>% mutate(study = ifelse(is.na(study), "Unknown", study)) %>%
      count(study) %>% mutate(pct = n / sum(n) * 100)
    ggplot(tab, aes(x = "", y = n, fill = study)) +
      geom_col(width = 1, color = "white", size = 2) +
      geom_text(aes(label = sprintf("%s\n%.1f%%", scales::comma(n), pct)),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 5) +
      coord_polar("y") +
      scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60", Unknown = "grey70")) +
      theme_void() + theme(legend.position = "right")
  })
  
  output$plot_pyramid <- renderPlot({
    df <- filtered_data(); req(df, nrow(df) > 0)
    df_plot <- df %>%
      filter(!is.na(age_num), !is.na(sex), !is.na(zone)) %>%
      mutate(age_group = make_age_groups(age_num, input$pyramid_age_width))
    
    if (input$pyramid_by_structure && !identical(input$filter_structure, "all")) {
      keep <- df_plot %>% count(province, structure) %>% filter(n >= input$pyramid_min_n)
      if (!nrow(keep)) return(ggplot() + annotate("text", x = 0, y = 0, label = "No structures meet minimum sample threshold", size = 6) + theme_void())
      df_plot <- df_plot %>% semi_join(keep, by = c("province", "structure"))
    } else {
      zones_keep <- df_plot %>% count(province, zone) %>% filter(n >= input$pyramid_min_n)
      if (!nrow(zones_keep)) return(ggplot() + annotate("text", x = 0, y = 0, label = "No zones meet minimum sample threshold", size = 6) + theme_void())
      df_plot <- df_plot %>% semi_join(zones_keep, by = c("province", "zone"))
    }
    
    df_agg <- df_plot %>%
      count(province, zone, structure, age_group, sex, study) %>%
      tidyr::complete(province, zone, structure, age_group, sex, study, fill = list(n = 0)) %>%
      mutate(n_signed = if_else(sex == "M", -n, n))
    
    p <- ggplot(df_agg, aes(x = n_signed, y = age_group, fill = sex)) +
      geom_col(width = 0.9, color = "grey40") +
      geom_vline(xintercept = 0, color = "grey30") +
      scale_x_continuous("Count (M left, F right)", labels = abs) +
      scale_fill_manual(values = c(M = "#3498DB", F = "#E74C3C")) +
      labs(y = "Age group", fill = "Sex") +
      theme_minimal(base_size = 12)
    
    if (input$pyramid_by_structure) {
      if (input$pyramid_split_study) p + facet_grid(study + province ~ structure, scales = "free_x", space = "free_x")
      else                          p + facet_grid(province ~ structure,          scales = "free_x", space = "free_x")
    } else {
      if (input$pyramid_split_study) p + facet_grid(study + province ~ zone, scales = "free_x", space = "free_x")
      else                          p + facet_grid(province ~ zone,         scales = "free_x", space = "free_x")
    }
  }, res = 96)
  
  # === TABLES ===
  output$table_demographics <- renderTable({
    df <- filtered_data(); req(df)
    tibble(
      Metric = c("Median age", "% Female", "Age range"),
      Value = c(
        sprintf("%.1f years", median(df$age_num, na.rm = TRUE)),
        sprintf("%.1f%%", mean(df$sex == "F", na.rm = TRUE) * 100),
        sprintf("%.0f - %.0f", min(df$age_num, na.rm = TRUE), max(df$age_num, na.rm = TRUE))
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_geography <- renderTable({
    df <- filtered_data(); req(df)
    tibble(
      Metric = c("Provinces", "Health zones", "Structures", "Mobile units"),
      Count = c(
        dplyr::n_distinct(df$province,  na.rm = TRUE),
        dplyr::n_distinct(df$zone,      na.rm = TRUE),
        dplyr::n_distinct(df$structure, na.rm = TRUE),
        dplyr::n_distinct(df$unit,      na.rm = TRUE)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_top_sites <- renderTable({
    df <- filtered_data(); req(df)
    df %>%
      filter(!is.na(structure)) %>%
      count(structure, sort = TRUE) %>%
      head(5) %>%
      mutate(n = scales::comma(n)) %>%
      rename(Structure = structure, Samples = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # === DATA TABLE ===
  output$data_table <- renderDT({
    df <- filtered_data(); req(df)
    df %>%
      select(barcode, lab_id, date_sample, date_received, date_result,
             study, sex, age_num, province, zone, structure, unit) %>%
      datatable(options = list(pageLength = 25, scrollX = TRUE),
                filter = "top", rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = function() sprintf("biobank_export_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content  = function(file) readr::write_csv(filtered_data(), file)
  )
  
  # === DEBUG ===
  output$debug_config <- renderPrint({ cfg() })
  output$debug_data_info <- renderPrint({
    df <- data_clean(); if (is.null(df)) return("No data loaded")
    list(
      rows = nrow(df),
      columns = ncol(df),
      column_names = names(df),
      study_breakdown = table(df$study, useNA = "ifany"),
      date_range = range(df$date_sample, na.rm = TRUE)
    )
  })
  output$debug_conflicts <- renderDT({
    tibble(Message = "Key conflict detection coming soon") %>%
      datatable(options = list(pageLength = 10))
  })
}

# --- BOOTSTRAP ---------------------------------------------------------------
shinyApp(ui, server)
