# MBUJI-MAYI BIOBANK DASHBOARD — CLEAN + WORKING
# =============================================================================
# --- SETUP -------------------------------------------------------------------
suppressPackageStartupMessages({
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
})
options(shiny.fullstacktrace = TRUE)
# Allow larger uploads (e.g. shapefiles packaged as ZIP)
options(shiny.maxRequestSize = 50 * 1024^2)
# Use Bootstrap Icons safely *without* hitting validateIcon()
bi_tag <- function(name, ...) htmltools::tagList(bsicons::bs_icon(name, ...))
# For any component that *forces* icon=, move the icon into the label/text
with_icon <- function(tag, icon) htmltools::tagList(icon, tag)

# Windows locale nudge for accented folders (harmless on other OSes)
try({
  if (.Platform$OS.type == "windows") {
    Sys.setlocale("LC_CTYPE", "English_United States.utf8")
  }
}, silent = TRUE)

# --- PATH NORMALISER ---------------------------------------------------------
safe_path <- function(p) {
  if (is.null(p) || !length(p)) return(p)
  if (length(p) > 1) p <- paste(p, collapse = "")
  if (is.na(p)) return(NA_character_)

  p_clean <- trimws(gsub("[\r\n]+", "", p))
  if (!nzchar(p_clean)) return(p_clean)

  # If the raw value already resolves on this system, keep it.
  if (isTRUE(dir.exists(p_clean)) || isTRUE(file.exists(p_clean))) {
    p_norm <- tryCatch(normalizePath(p_clean, winslash = "/", mustWork = FALSE),
                      error = function(e) p_clean)
    if (length(p_norm) == 1 && nzchar(p_norm)) return(p_norm)
    return(p_clean)
  }

  # Convert to native encoding, but fall back to the original if it fails.
  p_native <- tryCatch(enc2native(p_clean), error = function(e) p_clean)
  if (length(p_native) != 1 || is.na(p_native) || !nzchar(p_native)) {
    p_native <- p_clean
  }

  p_norm <- tryCatch(normalizePath(p_native, winslash = "/", mustWork = FALSE),
                     error = function(e) p_native)
  if (isTRUE(dir.exists(p_norm)) || isTRUE(file.exists(p_norm))) return(p_norm)

  sp <- try(utils::shortPathName(p_native), silent = TRUE)
  if (!inherits(sp, "try-error") && length(sp) == 1 && nzchar(sp)) {
    sp_norm <- tryCatch(normalizePath(sp, winslash = "/", mustWork = FALSE),
                        error = function(e) sp)
    if (isTRUE(dir.exists(sp_norm)) || isTRUE(file.exists(sp_norm))) return(sp_norm)
  }

  if (.Platform$OS.type == "windows") {
    unc <- paste0("\\\\?\\", gsub("/", "\\\\", p_native))
    unc_norm <- tryCatch(normalizePath(unc, winslash = "/", mustWork = FALSE),
                         error = function(e) unc)
    if (isTRUE(dir.exists(unc_norm)) || isTRUE(file.exists(unc_norm))) return(unc_norm)
  }

  p_clean
}

# --- CONFIG LOADER -----------------------------------------------------------
load_config <- function() {
  cfg <- if (file.exists("config.yml")) {
    yaml::read_yaml("config.yml")
  } else {
    # sensible defaults if YAML missing
    list(
      paths = list(
        biobank_dir   = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/01 - Biobanque",
        extractions_dir = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/02 - Extractions",
        pcr_dir       = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/03 - Biologie Moléculaire/0302 - Résultats qPCR",
        elisa_pe_dir  = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/04 - ELISA indirect PE/0402 - Résultats ELISA indirect PE",
        elisa_vsg_dir = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/05 - ELISA indirect VSG/0502 - Résultats ELISA indirect VSG",
        ielisa_dir    = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/06 - iELISA/0602 - Résultats iELISA"
      ),
      map = list(
        use_grid3_online = FALSE,
        grid3_url = "https://services3.arcgis.com/BU6Aadhn6tbBEdyk/arcgis/rest/services/GRID3_COD_health_zones_v7_0/FeatureServer/0/query?where=1%3D1&outFields=province,zonesante&outSR=4326&f=geojson",
        province_field_regex = "(?i)prov",
        zone_field_regex     = "(?i)zone|zs|zonesante",
        fallback_shapefile   = "testdata/cod_kasai_lomami_health_zones.gpkg"
      ),
      qc = list(
        drs_target_ml = 2.0, drs_accept_min_ml = 1.5, drs_accept_max_ml = 2.5,
        max_transport_field_hs_days = 30, max_transport_hs_lsd_days = 30,
        max_transport_lsd_inrb_days = 90, max_doorlooptijd_days = 90
      ),
      ui = list(
        theme_primary = "#2C3E50", theme_success = "#27AE60",
        theme_info = "#3498DB", theme_warning = "#F39C12", theme_danger = "#E74C3C",
        cache_minutes = 10, default_date_range_days = 180
      ),
      app = list(
        title = "Mbuji-Mayi Biobank Dashboard", version = "2.0.0",
        institution = "Institute of Tropical Medicine, Antwerp",
        last_updated = "2025-11-02"
      )
    )
  }
  
  # normalise every path-like entry
  if (!is.null(cfg$paths) && length(cfg$paths)) {
    cfg$paths <- lapply(cfg$paths, safe_path)
  }
  if (!is.null(cfg$map$fallback_shapefile)) {
    cfg$map$fallback_shapefile <- safe_path(cfg$map$fallback_shapefile)
  }
  cfg
}

# --- UTILITIES / MODULES (must be available before UI) ----------------------
source("R/utils_parse.R", local = TRUE)
source("R/utils_join.R", local = TRUE)
source("R/mod_lab_results_complete_v2.R", local = TRUE)  # This module
source("R/helpers_lab_corrected.R", local = TRUE)  # PCR/iELISA parsing
source("R/helpers_controls_WORKING.R", local = TRUE) #N EW                         
source("R/helpers_lab_results2.R", local = TRUE)  # Canonical calls
source("R/helpers_lab_merge_FIXED.R", local = TRUE)     # Corrected merge
source("R/mod_geo_map_complete.R", local = TRUE)
source("R/mod_extractions.R", local = TRUE)
source("R/helpers_concordance.R", local = TRUE)   # NEW
source("R/helpers_dates.R", local = TRUE)         # NEW
source("R/helpers_controls_v2.R", local = TRUE)

                         
# --- HELPERS FOR THIS APP (FIXED VERSION) ------------------------------------
# Enhanced clean_biobank_data function with new fields
clean_biobank_data <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(tibble())
  
  rename_first <- function(df, new_name, pattern) {
    hits <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hits) >= 1) df <- df %>% dplyr::rename(!!new_name := dplyr::all_of(hits[1]))
    df
  }
  
  df <- df %>%
    rename_first("barcode", "code.*barr|barcode") %>%
    rename_first("lab_id", "num[eé]ro|id.*lab") %>%
    rename_first("date_raw", "date.*pr[eé]lev|date.*sample|^date$|date_de_prelevement") %>%
    rename_first("age", "^age") %>%
    rename_first("sex", "^sex|^sexe|^gender") %>%
    rename_first("zone", "zone.*sant[eé]|health.*zone|^zs$") %>%
    rename_first("province", "^province") %>%
    rename_first("study", "[eé]tude|study|passif|actif") %>%
    rename_first("structure", "structure.*sanit|facility") %>%
    rename_first("unit", "unit[eé].*mobile|mobile.*unit") %>%
    rename_first("date_received_raw", "date.*recept|date.*arriv") %>%
    rename_first("date_result_raw", "date.*result|result.*date") %>%
    rename_first("date_env_cpltha_raw", "date.*env.*cpltha|date_envoi_vers_cpltha") %>%
    rename_first("date_rec_cpltha_raw", "date.*recept.*cpltha") %>%
    rename_first("date_env_inrb_raw", "date.*env.*inrb") %>%
    rename_first("temp_transport_raw", "temp.*transport") %>%
    rename_first("temp_cpltha_raw", "temp.*stockage|temp.*cpltha") %>%
    # ✅ NEW: Add ancien_cas and traite columns
    rename_first("ancien_cas_raw", "ancien.*cas|previous.*case|old.*case") %>%
    rename_first("traite_raw", "trait[eé]|treated|treatment")
  
  required <- c(
    "barcode","lab_id","date_raw","age","sex","zone","province","study",
    "structure","unit","date_received_raw","date_result_raw",
    "date_env_cpltha_raw","date_rec_cpltha_raw","date_env_inrb_raw",
    "temp_transport_raw","temp_cpltha_raw",
    "ancien_cas_raw","traite_raw"  # ✅ NEW
  )
  for (col in required) if (!col %in% names(df)) df[[col]] <- NA_character_
  
  df %>%
    mutate(
      # === Date parsing ===
      # Date prélèvement (sample collection date)
      date_sample     = parse_any_date(date_raw),
      date_received   = parse_any_date(date_received_raw),
      date_result     = parse_any_date(date_result_raw),
      # Date envoi vers CPLTHA (shipment to lab)
      date_env_cpltha = parse_any_date(date_env_cpltha_raw),
      date_rec_cpltha = parse_any_date(date_rec_cpltha_raw),
      date_env_inrb   = parse_any_date(date_env_inrb_raw),
      
      # === Demographic parsing ===
      age_num = parse_age(age),
      sex     = parse_sex_code(sex),
      study   = parse_study_code(study),
      
      # === Temperature codes ===
      temp_field = parse_temp_code(temp_transport_raw),
      temp_hs    = parse_temp_code(temp_cpltha_raw),
      
      # === Transport times (days between dates) ===
      # Field → Health Structure (max 30 days)
      transport_field_hs = safe_days_between(date_env_cpltha, date_sample, 30),
      # Health Structure → Lab Surveillance District (max 30 days)
      transport_hs_lsd   = safe_days_between(date_rec_cpltha, date_env_cpltha, 30),
      # Lab Surveillance District → INRB (max 90 days)
      transport_lsd_inrb = safe_days_between(date_env_inrb,   date_rec_cpltha, 90),
      
      # ✅ NEW: Transport time from field to CPLTHA
      # = Date envoi vers CPLTHA - Date prélèvement
      # This is the time from sample collection to shipping to central lab
      # Critical for cold chain quality monitoring
      transport_field_cpltha = safe_days_between(date_env_cpltha, date_sample, max_ok = 90),
      
      # === Text standardization ===
      zone      = stringr::str_squish(as.character(zone)),
      province  = stringr::str_squish(as.character(province)),
      structure = stringr::str_squish(as.character(structure)),
      unit      = stringr::str_squish(as.character(unit)),
      
      # ✅ NEW: Parse ancien_cas (previous case status)
      # Oui = Previously diagnosed case
      # Non = New case
      # Incertain = Uncertain/Unknown
      # NA = No information provided
      ancien_cas = parse_yes_no_uncertain(ancien_cas_raw),
      
      # ✅ NEW: Parse traite (treatment status)
      # Oui = Previously treated
      # Non = Treatment-naive
      # Incertain = Uncertain/Unknown
      # NA = No information provided
      traite = parse_yes_no_uncertain(traite_raw)
    ) %>%
    # CRITICAL: Remove samples without valid collection date
    # This is the primary deduplication and QC filter
    # Samples loaded counter = rows remaining after this filter
    dplyr::filter(!is.na(date_sample)) %>%
    # CRITICAL: Deduplicate by (barcode, lab_id) pair
    # Keeps only the first occurrence of each unique sample
    # Handles cases where same sample has multiple specimen types
    # Example: Same patient sample with serum + plasma = counted as 1 sample
    dplyr::distinct(barcode, lab_id, .keep_all = TRUE)
}

make_age_groups <- function(age, width = 5) {
  if (all(is.na(age))) return(factor(levels = character()))
  hi <- suppressWarnings(ceiling(max(age, na.rm = TRUE) / width) * width)
  if (!is.finite(hi) || hi <= 0) hi <- width
  cut(age, breaks = seq(0, hi, by = width), right = FALSE, include.lowest = TRUE)
}

# --- UI ---------------------------------------------------------------------
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
    dateRangeInput(
      "date_range", "Sample Date",
      start = Sys.Date() - 180, end = Sys.Date()
    ),
    selectInput("filter_study",     "Study",     choices = c("All" = "all")),
    selectInput("filter_province",  "Province",  choices = c("All" = "all")),
    selectInput("filter_zone",      "Zone",      choices = c("All" = "all")),
    selectInput("filter_structure", "Structure", choices = c("All" = "all")),
    checkboxGroupInput(
      "filter_sex", "Sex",
      choices = c("M","F"), selected = c("M","F"), inline = TRUE
    )
  ),
  
  # === OVERVIEW TAB ===
  nav_panel(
    title = "Overview",
    layout_columns(
      fill = FALSE, col_widths = c(3,3,3,3),
      value_box(title = "Total Samples", value = textOutput("vb_total"),
                showcase = bs_icon("clipboard"), theme = "primary"),
      value_box(title = "DA Samples", value = textOutput("vb_da"),
                showcase = bs_icon("clipboard"), theme = "info"),
      value_box(title = "DP Samples", value = textOutput("vb_dp"),
                showcase = bs_icon("clipboard"), theme = "success"),
      value_box(title = "Sites Active", value = textOutput("vb_sites"),
                showcase = bs_icon("hospital"), theme = "warning")
    ),
    layout_columns(
      col_widths = c(6,6),
      card(card_header("Sample Collection Over Time"),
           plotOutput("plot_timeline", height = 300)),
      card(card_header("Study Distribution"),
           plotOutput("plot_study_dist", height = 300))
    ),
    layout_columns(
      col_widths = c(4,4,4),
      card(card_header("Demographics Summary"), tableOutput("table_demographics")),
      card(card_header("Geographic Coverage"),  tableOutput("table_geography")),
      card(card_header("Top Contributing Sites"), tableOutput("table_top_sites"))
    ),
    # After the Overview value boxes, add new metadata summary card:
    
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Sample Metadata Summary"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
           
          # Ancien cas breakdown
          card(
            card_header("Ancien Cas (Previous Cases)", class = "bg-info text-white"),
            tableOutput("table_ancien_cas")
          ),
          
          # Traité breakdown
          card(
            card_header("Traité (Treatment Status)", class = "bg-success text-white"),
            tableOutput("table_traite")
          ),
          
          # Transport time summary
          card(
            card_header("Transport Time (Field→CPLTHA)", class = "bg-warning text-white"),
            tableOutput("table_transport_time")
          ),

# Insert this as a new card in the Overview tab:
 
     card(
       card_header("Transport Time Distribution (Field → CPLTHA)"),
       plotOutput("plot_transport_time", height = 300)
     ),
          
          # Latest sample date
          card(
            card_header("Latest Sample Date", class = "bg-primary text-white"),
            div(
              style = "padding: 20px; text-align: center;",
              h3(textOutput("latest_sample_date"), style = "margin: 0;")
            )
          )
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
                   numericInput("pyramid_age_width", "Age group width", 5, min = 1, max = 10),
                   numericInput("pyramid_min_n", "Min samples per zone", 15, min = 1),
                   checkboxInput("pyramid_split_study", "Split by study (DA/DP)", FALSE),
                   checkboxInput("pyramid_by_structure", "Facet by structure", FALSE)
            ),
            column(9, plotOutput("plot_pyramid", height = 600))
          )
        )
      )
    )
  ),
  
  # === GEOGRAPHY TAB ===
  nav_panel(title = "Geography",     mod_geo_map_ui("geo_map")),
  
  # === EXTRACTION QC TAB ===
  nav_panel(title = "Extraction QC", mod_extractions_qc_ui("extractions_qc")),
  
  # === LAB RESULTS TAB ===
  nav_panel(title = "Lab Results", mod_lab_results_ui("lab_results")),
  
  # === DATA TAB ===
  nav_panel(
    title = "Data",
    card(
      card_header("Sample Records",
                  class = "d-flex justify-content-between align-items-center",
                  downloadButton("download_data", "Download CSV", class = "btn-sm")),
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

# --- SERVER -----------------------------------------------------------------
server <- function(input, output, session) {
  
  cfg <- reactiveVal(load_config())
  
  # Report missing/unreadable dirs from YAML (normalised)
  missing_paths <- reactive({
    conf <- cfg(); out <- character()
    if (!is.null(conf$paths)) {
      for (nm in names(conf$paths)) {
        p <- safe_path(conf$paths[[nm]])
        if (is.character(p) && nzchar(p) && !dir.exists(p)) {
          out <- c(out, sprintf("%s: %s", nm, p))
        }
      }
    }
    out
  })
  
  raw_data   <- reactiveVal(NULL)
  clean_data <- reactiveVal(NULL)
  
  # Seed data_dir with normalised biobank dir
  observe({
    conf <- cfg()
    if (!is.null(conf$paths$biobank_dir)) {
      updateTextInput(session, "data_dir", value = safe_path(conf$paths$biobank_dir))
    }
  })
  
  # Sidebar status line
  output$data_status <- renderText({
    miss <- missing_paths()
    if (length(miss)) {
      paste(c("⚠️ Missing/unreadable folders:", miss), collapse = "\n")
    } else {
      df <- clean_data()
      if (is.null(df)) "No data loaded" else sprintf("%s samples loaded", scales::comma(nrow(df)))
    }
  })
  
  # File picker uses normalised path
  output$file_selector <- renderUI({
    req(input$data_dir)
    dir_resolved <- safe_path(input$data_dir)
    
    if (!dir.exists(dir_resolved)) {
      return(helpText(paste0("⚠️ Folder not found or unreadable:\n", dir_resolved)))
    }
    
    files <- list.files(dir_resolved, pattern = "\\.xlsx?$", full.names = TRUE)
    if (!length(files)) return(helpText("No Excel files found in folder"))
    
    names(files) <- basename(files)
    selectInput("selected_file", "Select file", choices = files)
  })
  
  # Load + clean
  # ✅ FIXED: Load data with proper sample counting
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
    
    df_clean <- tryCatch({
      clean_biobank_data(df_raw)
    }, error = function(e) {
      showNotification(paste("Clean failed:", e$message), type = "error")
      return(tibble())
    })
    
    clean_data(df_clean)
    
    if (!nrow(df_clean)) {
      showNotification("Loaded file, but 0 usable rows.", type = "warning")
      return()
    }
    
    # ✅ FIXED: Update filters with actual unique values
    studies    <- sort(unique(na.omit(as.character(df_clean$study))))
    provinces  <- sort(unique(na.omit(df_clean$province)))
    zones      <- sort(unique(na.omit(df_clean$zone)))
    structures <- sort(unique(na.omit(df_clean$structure)))
    
    updateSelectInput(session, "filter_study",     
                      choices = c("All" = "all", setNames(studies, studies)))
    updateSelectInput(session, "filter_province",  
                      choices = c("All" = "all", provinces))
    updateSelectInput(session, "filter_zone",      
                      choices = c("All" = "all", zones))
    updateSelectInput(session, "filter_structure", 
                      choices = c("All" = "all", structures))
    
    # Date range
    dr <- range(df_clean$date_sample, na.rm = TRUE)
    start <- max(dr[1], dr[2] - 180)
    updateDateRangeInput(session, "date_range",
                         start = start, end = dr[2], min = dr[1], max = dr[2])
    
    # ✅ FIXED: Show actual unique sample count
    showNotification(
      sprintf("Loaded %s samples (unique by barcode/lab_id)", 
              scales::comma(nrow(df_clean))), 
      type = "message"
    )
  })
  
  # ✅ FIXED: Cascading filters - Zone updates based on Province
  observe({
    df <- clean_data()
    req(df, nrow(df) > 0)
    
    # If a province is selected, filter zones to only those in that province
    if (!is.null(input$filter_province) && input$filter_province != "all") {
      zones_in_province <- df %>%
        filter(province == input$filter_province) %>%
        pull(zone) %>%
        unique() %>%
        sort() %>%
        na.omit()
      
      # Preserve current zone selection if it's valid
      current_zone <- input$filter_zone
      new_selection <- if (current_zone %in% c("all", zones_in_province)) {
        current_zone
      } else {
        "all"
      }
      
      updateSelectInput(session, "filter_zone", 
                        choices = c("All" = "all", zones_in_province),
                        selected = new_selection)
    } else {
      # No province filter - show all zones
      all_zones <- sort(unique(na.omit(df$zone)))
      updateSelectInput(session, "filter_zone", 
                        choices = c("All" = "all", all_zones))
    }
  })
  
  # ✅ FIXED: Structure updates based on Province and Zone
  observe({
    df <- clean_data()
    req(df, nrow(df) > 0)
    
    # Start with all data
    df_filtered <- df
    
    # Filter by province if selected
    if (!is.null(input$filter_province) && input$filter_province != "all") {
      df_filtered <- df_filtered %>% filter(province == input$filter_province)
    }
    
    # Filter by zone if selected
    if (!is.null(input$filter_zone) && input$filter_zone != "all") {
      df_filtered <- df_filtered %>% filter(zone == input$filter_zone)
    }
    
    # Get structures available in filtered data
    structures_available <- df_filtered %>%
      pull(structure) %>%
      unique() %>%
      sort() %>%
      na.omit()
    
    # Preserve current structure selection if valid
    current_structure <- input$filter_structure
    new_selection <- if (current_structure %in% c("all", structures_available)) {
      current_structure
    } else {
      "all"
    }
    
    updateSelectInput(session, "filter_structure", 
                      choices = c("All" = "all", structures_available),
                      selected = new_selection)
  })
  
  # Filtered view
  filtered_data <- reactive({
    df <- clean_data(); req(df)
    if (!nrow(df)) return(df)
    
    dr <- input$date_range
    if (length(dr) == 2 && all(!is.na(dr))) {
      df <- df %>% dplyr::filter(date_sample >= dr[1], date_sample <= dr[2])
    }
    if (!identical(input$filter_study, "all"))     df <- df %>% dplyr::filter(study == input$filter_study)
    if (!identical(input$filter_province, "all"))  df <- df %>% dplyr::filter(province == input$filter_province)
    if (!identical(input$filter_zone, "all"))      df <- df %>% dplyr::filter(zone == input$filter_zone)
    if (!identical(input$filter_structure, "all")) df <- df %>% dplyr::filter(structure == input$filter_structure)
    if (length(input$filter_sex))                  df <- df %>% dplyr::filter(sex %in% input$filter_sex)
    df
  })

output$table_ancien_cas <- renderTable({
  df <- filtered_data()
  req(df, nrow(df) > 0)
  
  # Count breakdown
  summary <- df %>%
    mutate(
      status = case_when(
        is.na(ancien_cas) ~ "Unknown (no data)",
        ancien_cas == "Oui" ~ "Oui (Previous case)",
        ancien_cas == "Non" ~ "Non (New case)",
        ancien_cas == "Incertain" ~ "Incertain (Uncertain)",
        TRUE ~ "Other"
      )
    ) %>%
    count(status) %>%
    mutate(
      Percent = sprintf("%.1f%%", n / sum(n) * 100)
    ) %>%
    arrange(desc(n)) %>%
    rename(Status = status, Count = n)
  
  summary
}, striped = TRUE, hover = TRUE, bordered = TRUE)


output$table_traite <- renderTable({
  df <- filtered_data()
  req(df, nrow(df) > 0)
  
  summary <- df %>%
    mutate(
      status = case_when(
        is.na(traite) ~ "Unknown (no data)",
        traite == "Oui" ~ "Oui (Treated)",
        traite == "Non" ~ "Non (Treatment-naive)",
        traite == "Incertain" ~ "Incertain (Uncertain)",
        TRUE ~ "Other"
      )
    ) %>%
    count(status) %>%
    mutate(
      Percent = sprintf("%.1f%%", n / sum(n) * 100)
    ) %>%
    arrange(desc(n)) %>%
    rename(Status = status, Count = n)
  
  summary
}, striped = TRUE, hover = TRUE, bordered = TRUE)


output$table_transport_time <- renderTable({
  df <- filtered_data()
  req(df, nrow(df) > 0)
  
  # Calculate summary stats for transport time
  transport_times <- df %>%
    filter(!is.na(transport_field_cpltha)) %>%
    pull(transport_field_cpltha)
  
  if (length(transport_times) == 0) {
    return(tibble(Metric = "No data", Value = "—"))
  }
  
  tibble(
    Metric = c("Samples with data", "Minimum", "Median", "Maximum", "Mean"),
    Value = c(
      scales::comma(length(transport_times)),
      sprintf("%.1f days", min(transport_times, na.rm = TRUE)),
      sprintf("%.1f days", median(transport_times, na.rm = TRUE)),
      sprintf("%.1f days", max(transport_times, na.rm = TRUE)),
      sprintf("%.1f days", mean(transport_times, na.rm = TRUE))
    )
  )
}, striped = TRUE, hover = TRUE, bordered = TRUE)


output$latest_sample_date <- renderText({
  df <- clean_data()  # Use full dataset, not filtered
  req(df, nrow(df) > 0)
  
  latest <- max(df$date_sample, na.rm = TRUE)
  
  if (is.finite(latest) && !is.na(latest)) {
    format(latest, "%d %b %Y")
  } else {
    "No valid dates"
  }
})


# Enhanced status line with latest sample date
output$data_status <- renderText({
  miss <- missing_paths()
  df <- clean_data()
  
  if (length(miss)) {
    paste(c("⚠️ Missing/unreadable folders:", miss), collapse = "\n")
  } else if (is.null(df) || !nrow(df)) {
    "No data loaded"
  } else {
    latest <- max(df$date_sample, na.rm = TRUE)
    latest_str <- if (is.finite(latest) && !is.na(latest)) {
      sprintf(" | Latest: %s", format(latest, "%d %b %Y"))
    } else {
      ""
    }
    sprintf("%s samples loaded%s", scales::comma(nrow(df)), latest_str)
  }
})


# ✅ ENHANCED: Age-Sex pyramid with overlay for filtered vs full dataset
output$plot_study_dist <- renderPlot({
  df_full <- clean_data()       # Full dataset
  df_filtered <- filtered_data()  # Filtered dataset
  
  req(df_full, nrow(df_full) > 0)
  req(df_filtered, nrow(df_filtered) > 0)
  
  # Check if overlay is enabled (default = TRUE)
  show_overlay <- TRUE  # You can add a toggle input if desired
  
  # Prepare full dataset pyramid
  pyramid_full <- df_full %>%
    filter(!is.na(age_num), !is.na(sex), sex %in% c("M", "F")) %>%
    mutate(
      age_group = make_age_groups(age_num, width = 5),
      sex_label = recode(sex, M = "Male", F = "Female")
    ) %>%
    filter(!is.na(age_group)) %>%
    count(age_group, sex_label, name = "n") %>%
    mutate(
      dataset = "Full",
      count = if_else(sex_label == "Male", -n, n)
    )
  
  # Prepare filtered dataset pyramid
  pyramid_filtered <- df_filtered %>%
    filter(!is.na(age_num), !is.na(sex), sex %in% c("M", "F")) %>%
    mutate(
      age_group = make_age_groups(age_num, width = 5),
      sex_label = recode(sex, M = "Male", F = "Female")
    ) %>%
    filter(!is.na(age_group)) %>%
    count(age_group, sex_label, name = "n") %>%
    mutate(
      dataset = "Filtered",
      count = if_else(sex_label == "Male", -n, n)
    )
  
  # Check if we have enough filtered samples
  n_filtered <- nrow(df_filtered %>% filter(!is.na(age_num), !is.na(sex)))
  low_sample_warning <- n_filtered < 20
  
  if (!nrow(pyramid_full) && !nrow(pyramid_filtered)) {
    # Fallback to simple study distribution
    plot.new()
    text(0.5, 0.5, "No age/sex data available", cex = 1.5, col = "gray50")
    return()
  }
  
  # Combine datasets
  plot_data <- bind_rows(pyramid_full, pyramid_filtered)
  
  max_count <- max(abs(plot_data$count))
  
  # Create plot
  p <- ggplot(plot_data, aes(x = age_group, y = count, fill = sex_label, alpha = dataset)) +
    geom_col(width = 0.9, position = "identity", 
             color = "black", linewidth = 0.3) +
    coord_flip() +
    scale_y_continuous(
      labels = abs, 
      limits = c(-max_count, max_count),
      breaks = pretty(c(-max_count, max_count), n = 5)
    ) +
    scale_fill_manual(values = c("Male" = "#3498DB", "Female" = "#E91E63")) +
    scale_alpha_manual(
      values = c("Full" = 0.3, "Filtered" = 1.0),
      guide = guide_legend(override.aes = list(fill = "gray"))
    ) +
    labs(
      title = "Age-Sex Distribution: Filtered vs Full Dataset",
      subtitle = sprintf(
        "Filtered: %s samples | Full: %s samples%s",
        scales::comma(sum(abs(pyramid_filtered$count))),
        scales::comma(sum(abs(pyramid_full$count))),
        if (low_sample_warning) " ⚠️ Low filtered sample size" else ""
      ),
      x = "Age group", 
      y = "Sample count", 
      fill = "Sex",
      alpha = "Dataset"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.subtitle = element_text(
        color = if (low_sample_warning) "#E74C3C" else "black"
      )
    )
  
  p
})


# ✅ IMPROVED: Site metrics with clearer mobile unit detection
output$vb_sites <- renderText({
  df <- filtered_data()
  req(df, nrow(df) > 0)
  
  # More robust mobile unit detection
  structures <- df %>%
    mutate(structure = trimws(as.character(structure))) %>%
    filter(!is.na(structure) & nzchar(structure)) %>%
    # ✅ ENHANCED: Detect mobile units more comprehensively
    filter(!grepl("^(UM|MUM|MOBILE|UNIT[ÉE]\\s*MOBILE)", toupper(structure))) %>%
    pull(structure)
  
  sprintf("%s fixed sites", scales::comma(n_distinct(structures)))
})


# ✅ IMPROVED: Geographic coverage with split mobile/fixed
output$table_geography <- renderTable({
  df <- filtered_data()
  req(df, nrow(df) > 0)
  
  # Detect mobile structures
  structures_classified <- df %>%
    mutate(
      structure = trimws(as.character(structure)),
      is_mobile = grepl("^(UM|MUM|MOBILE|UNIT[ÉE]\\s*MOBILE)", toupper(structure))
    ) %>%
    filter(!is.na(structure) & nzchar(structure))
  
  n_fixed <- n_distinct(structures_classified$structure[!structures_classified$is_mobile])
  n_mobile_structures <- n_distinct(structures_classified$structure[structures_classified$is_mobile])
  n_mobile_units <- n_distinct(df$unit[!is.na(df$unit) & nzchar(df$unit)])
  
  tibble(
    Metric = c(
      "Provinces", 
      "Health zones", 
      "Fixed structures",      # ✅ CLARIFIED
      "Mobile structures",     # ✅ NEW
      "Mobile unit IDs"        # ✅ CLARIFIED
    ),
    Count = c(
      n_distinct(df$province, na.rm = TRUE),
      n_distinct(df$zone, na.rm = TRUE),
      n_fixed,
      n_mobile_structures,
      n_mobile_units
    )
  )
}, striped = TRUE, hover = TRUE, bordered = TRUE)

  
  # === LAB RESULTS MODULE (replaces old version) ===
  lab_modules <- mod_lab_results_server(
    "lab_results",
    biobank_clean = clean_data,
    config = cfg
  )
  
  # === EXTRACTIONS MODULE ===
  mod_extractions_qc_server(
    "extractions_qc",
    biobank_clean = clean_data,
    config = cfg
  )
  
  # === GEOGRAPHY MODULE (uses lab results) ===
  mod_geo_map_server(
    "geo_map",
    biobank_filtered = filtered_data,
    lab_joined = lab_modules$lab_joined,  # <-- Important: uses lab data
    config = cfg
  )
  
  # KPIs
  output$vb_total <- renderText({
    df <- filtered_data(); req(df); scales::comma(nrow(df))
  })
  
  output$vb_da <- renderText({
    df <- filtered_data(); req(df)
    if (!nrow(df)) return("0 (0%)")
    n_da <- sum(df$study == "DA", na.rm = TRUE)
    pct  <- round(100 * n_da / nrow(df), 1)
    sprintf("%s (%s%%)", scales::comma(n_da), pct)
  })
  
  output$vb_dp <- renderText({
    df <- filtered_data(); req(df)
    if (!nrow(df)) return("0 (0%)")
    n_dp <- sum(df$study == "DP", na.rm = TRUE)
    pct  <- round(100 * n_dp / nrow(df), 1)
    sprintf("%s (%s%%)", scales::comma(n_dp), pct)
  })
  
  output$vb_sites <- renderText({
    df <- filtered_data(); req(df)
    structures <- df %>%
      dplyr::mutate(structure = trimws(as.character(structure))) %>%
      dplyr::filter(!is.na(structure) & nzchar(structure)) %>%
      dplyr::filter(!grepl("^(UM|MUM)", toupper(structure))) %>%
      dplyr::pull(structure)
    sprintf("%s structures", scales::comma(dplyr::n_distinct(structures)))
  })

  # Plots
  output$plot_timeline <- renderPlot({
    df <- filtered_data(); req(df)
    df %>%
      dplyr::filter(!is.na(date_sample)) %>%
      dplyr::count(week = floor_date(date_sample, "week"), study) %>%
      ggplot(aes(week, n, fill = study)) +
      geom_col() +
      scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60"), na.value = "grey70") +
      labs(x = NULL, y = "Samples", fill = "Study") +
      theme_minimal() + theme(legend.position = "top")
  })
  
  # ✅ FIXED: Replace pie chart with Age-Sex pyramid
  output$plot_study_dist <- renderPlot({
    df <- filtered_data()
    req(df, nrow(df) > 0)
    
    # Prepare data for pyramid
    pyramid_data <- df %>%
      filter(!is.na(age_num), !is.na(sex), sex %in% c("M", "F")) %>%
      mutate(
        age_group = make_age_groups(age_num, width = 5),
        sex_label = recode(sex, M = "Male", F = "Female")
      ) %>%
      filter(!is.na(age_group)) %>%
      count(age_group, sex_label, name = "n")
    
    if (!nrow(pyramid_data)) {
      # Fallback to simple study distribution if no age/sex data
      tab <- df %>%
        mutate(
          study = as.character(study),
          study = case_when(
            is.na(study) | !nzchar(study) ~ "Unknown",
            study %in% c("DA", "DP") ~ study,
            TRUE ~ "Other"
          )
        ) %>%
        count(study, name = "n") %>%
        mutate(pct = n / sum(n) * 100)
      
      ggplot(tab, aes(x = "", y = n, fill = study)) +
        geom_col(width = 1, color = "white", linewidth = 1.2) +
        geom_text(
          aes(label = sprintf("%s\n%.1f%%", scales::comma(n), pct)),
          position = position_stack(vjust = 0.5),
          color = "white", fontface = "bold", size = 4
        ) +
        coord_polar("y") +
        scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60", 
                                     Other = "#8E44AD", Unknown = "#95A5A6")) +
        labs(title = "Study Distribution", fill = "Study") +
        theme_void() +
        theme(legend.position = "right")
    } else {
      # Create age-sex pyramid
      plot_data <- pyramid_data %>%
        mutate(count = if_else(sex_label == "Male", -n, n))
      
      max_count <- max(abs(plot_data$count))
      
      ggplot(plot_data, aes(x = age_group, y = count, fill = sex_label)) +
        geom_col(width = 0.9) +
        coord_flip() +
        scale_y_continuous(
          labels = abs, 
          limits = c(-max_count, max_count),
          breaks = pretty(c(-max_count, max_count), n = 5)
        ) +
        scale_fill_manual(values = c("Male" = "#3498DB", "Female" = "#E91E63")) +
        labs(
          title = "Age-Sex Distribution",
          subtitle = sprintf("Total: %s samples with age/sex data", 
                             scales::comma(sum(pyramid_data$n))),
          x = "Age group", 
          y = "Sample count", 
          fill = "Sex"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        )
    }
  })

  output$plot_pyramid <- renderPlot({
    df <- filtered_data(); req(df)

    shiny::validate(
      shiny::need("zone" %in% names(df), "Zone information is missing."),
      shiny::need("sex" %in% names(df), "Sex information is missing."),
      shiny::need("age_num" %in% names(df), "Age data is missing.")
    )

    df <- df %>%
      dplyr::mutate(
        zone = trimws(as.character(zone)),
        sex = as.character(sex),
        age_num = suppressWarnings(as.numeric(age_num))
      ) %>%
      dplyr::filter(
        !is.na(zone) & nzchar(zone),
        sex %in% c("M", "F"),
        !is.na(age_num)
      )

    shiny::validate(shiny::need(nrow(df) > 0, "No age/sex data available for the selected filters."))

    min_n_input <- input$pyramid_min_n
    if (is.null(min_n_input) || !is.numeric(min_n_input) || min_n_input < 1) {
      min_n <- 1
    } else {
      min_n <- floor(min_n_input)
    }
    zones_keep <- df %>% dplyr::count(zone, name = "n") %>% dplyr::filter(n >= min_n) %>% dplyr::pull(zone)

    shiny::validate(shiny::need(length(zones_keep) > 0, "Not enough samples per zone to draw the pyramid."))

    age_width_input <- input$pyramid_age_width
    if (is.null(age_width_input) || !is.numeric(age_width_input) || age_width_input < 1) {
      age_width <- 5
    } else {
      age_width <- floor(age_width_input)
    }

    df <- df %>%
      dplyr::filter(zone %in% zones_keep) %>%
      dplyr::mutate(
        age_group = make_age_groups(age_num, width = age_width),
        sex_label = dplyr::recode(sex, M = "Male", F = "Female"),
        structure_label = if (isTRUE(input$pyramid_by_structure) && "structure" %in% names(.)) {
          structure_clean <- trimws(as.character(structure))
          structure_clean[is.na(structure_clean) | !nzchar(structure_clean)] <- "Unknown structure"
          structure_clean
        } else {
          ""
        },
        study_label = if (isTRUE(input$pyramid_split_study) && "study" %in% names(.)) {
          study_clean <- as.character(study)
          study_clean[is.na(study_clean) | !nzchar(study_clean)] <- "Unknown study"
          study_clean
        } else {
          ""
        }
      )

    shiny::validate(shiny::need(!all(is.na(df$age_group)), "Unable to form age groups with the current data."))

    df <- df %>% dplyr::filter(!is.na(age_group))

    df <- df %>%
      dplyr::mutate(
        facet_label = zone,
        facet_label = ifelse(nzchar(structure_label), paste0(facet_label, " • ", structure_label), facet_label),
        facet_label = ifelse(nzchar(study_label), paste0(facet_label, "\n", study_label), facet_label)
      )

    summary_cols <- c("facet_label", "age_group", "sex_label")
    summary_df <- df %>%
      dplyr::group_by(dplyr::across(all_of(summary_cols))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    shiny::validate(shiny::need(nrow(summary_df) > 0, "No samples remain after applying the pyramid settings."))

    plot_df <- summary_df %>%
      dplyr::mutate(
        count = dplyr::if_else(sex_label == "Male", -n, n),
        facet_label = factor(facet_label, levels = unique(facet_label))
      )

    max_count <- max(abs(plot_df$count))
    if (!is.finite(max_count) || max_count <= 0) {
      max_count <- max(summary_df$n)
    }
    if (!is.finite(max_count) || max_count <= 0) {
      max_count <- 1
    }

    ggplot(plot_df, aes(x = age_group, y = count, fill = sex_label)) +
      geom_col(width = 0.9) +
      coord_flip() +
      facet_wrap(~ facet_label, scales = "free_y") +
      scale_y_continuous(labels = abs, limits = c(-max_count, max_count)) +
      scale_fill_manual(values = c("Male" = "#3498DB", "Female" = "#E91E63")) +
      labs(x = "Age group", y = "Sample count", fill = "Sex") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
output$plot_transport_time <- renderPlot({
  df <- filtered_data()
  req(df, nrow(df) > 0)
  
  transport_data <- df %>%
    filter(!is.na(transport_field_cpltha)) %>%
    select(transport_field_cpltha)
  
  if (!nrow(transport_data)) {
    plot.new()
    text(0.5, 0.5, "No transport time data available", cex = 1.2, col = "gray50")
    return()
  }
  
  ggplot(transport_data, aes(x = transport_field_cpltha)) +
    geom_histogram(binwidth = 1, fill = "#3498DB", color = "black", alpha = 0.7) +
    geom_vline(xintercept = median(transport_data$transport_field_cpltha, na.rm = TRUE),
               linetype = "dashed", color = "red", size = 1) +
    labs(
      title = "Transport Time: Sample Collection → Shipment to CPLTHA",
      subtitle = sprintf(
        "Median: %.1f days | Range: %.1f - %.1f days | n = %s",
        median(transport_data$transport_field_cpltha, na.rm = TRUE),
        min(transport_data$transport_field_cpltha, na.rm = TRUE),
        max(transport_data$transport_field_cpltha, na.rm = TRUE),
        scales::comma(nrow(transport_data))
      ),
      x = "Days from collection to shipment",
      y = "Number of samples"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())
})

      
  # Tables
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
      Count  = c(
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
      dplyr::filter(!is.na(structure)) %>%
      dplyr::count(structure, sort = TRUE) %>%
      head(5) %>%
      mutate(n = scales::comma(n)) %>%
      dplyr::rename(Structure = structure, Samples = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Data table + download
  output$data_table <- renderDT({
    df <- filtered_data(); req(df)
    df %>%
      dplyr::select(barcode, lab_id, date_sample, date_received, date_result,
                    study, sex, age_num, province, zone, structure, unit) %>%
      datatable(options = list(pageLength = 25, scrollX = TRUE),
                filter = "top", rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = function() sprintf("biobank_export_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content  = function(file) readr::write_csv(filtered_data(), file)
  )
  
  # Debug
  output$debug_config <- renderPrint({ cfg() })
  output$debug_data_info <- renderPrint({
    df <- clean_data(); if (is.null(df)) return("No data loaded")
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

# --- RUN --------------------------------------------------------------------
shinyApp(ui, server)
