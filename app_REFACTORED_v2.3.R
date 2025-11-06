# MBUJI-MAYI BIOBANK DASHBOARD — REFACTORED v2.3
# =============================================================================
# Clean Overview + Dedicated Transport Tab
# v2.3: Refined UI - smaller icons, maximized plot space
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
  library(memoise)  # For caching
})

options(shiny.fullstacktrace = TRUE)
options(shiny.maxRequestSize = 50 * 1024^2)

# Windows locale
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

  if (isTRUE(dir.exists(p_clean)) || isTRUE(file.exists(p_clean))) {
    p_norm <- tryCatch(normalizePath(p_clean, winslash = "/", mustWork = FALSE),
                      error = function(e) p_clean)
    if (length(p_norm) == 1 && nzchar(p_norm)) return(p_norm)
    return(p_clean)
  }

  p_native <- tryCatch(enc2native(p_clean), error = function(e) p_clean)
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
        title = "Mbuji-Mayi Biobank Dashboard", version = "2.3.0",
        institution = "Institute of Tropical Medicine, Antwerp",
        last_updated = "2025-11-06"
      )
    )
  }
  
  if (!is.null(cfg$paths) && length(cfg$paths)) {
    cfg$paths <- lapply(cfg$paths, safe_path)
  }
  if (!is.null(cfg$map$fallback_shapefile)) {
    cfg$map$fallback_shapefile <- safe_path(cfg$map$fallback_shapefile)
  }
  cfg
}

# --- SOURCE MODULES ----------------------------------------------------------
source_if_exists <- function(file, ...) {
  if (file.exists(file)) {
    tryCatch({
      source(file, ...)
      return(TRUE)
    }, error = function(e) {
      message(sprintf("Warning: Could not source %s: %s", file, e$message))
      return(FALSE)
    })
  } else {
    message(sprintf("Warning: File not found: %s", file))
    return(FALSE)
  }
}

source_if_exists("R/utils_parse.R", local = TRUE)
source_if_exists("R/utils_join.R", local = TRUE)
source_if_exists("R/mod_lab_results_complete_v2.R", local = TRUE)
source_if_exists("R/helpers_lab_corrected.R", local = TRUE)
source_if_exists("R/helpers_controls_WORKING.R", local = TRUE)
source_if_exists("R/helpers_lab_results2.R", local = TRUE)
source_if_exists("R/helpers_lab_merge_FIXED.R", local = TRUE)
source_if_exists("R/mod_geo_map_complete.R", local = TRUE)
source_if_exists("R/mod_extractions.R", local = TRUE)
source_if_exists("R/helpers_concordance.R", local = TRUE)
source_if_exists("R/helpers_dates.R", local = TRUE)
source_if_exists("R/helpers_controls_v2.R", local = TRUE)

# --- STUB FUNCTIONS FOR MISSING MODULES --------------------------------------
if (!exists("mod_lab_results_ui")) {
  mod_lab_results_ui <- function(id) {
    ns <- NS(id)
    card(card_header("Lab Results Module Not Available"),
         p("Please ensure R/mod_lab_results_complete_v2.R exists"))
  }
}

if (!exists("mod_lab_results_server")) {
  mod_lab_results_server <- function(id, biobank_clean, config) {
    moduleServer(id, function(input, output, session) {
      list(lab_joined = reactive(tibble()))
    })
  }
}

if (!exists("mod_extractions_qc_ui")) {
  mod_extractions_qc_ui <- function(id) {
    ns <- NS(id)
    card(card_header("Extractions Module Not Available"),
         p("Please ensure R/mod_extractions.R exists"))
  }
}

if (!exists("mod_extractions_qc_server")) {
  mod_extractions_qc_server <- function(id, biobank_clean, config) {
    moduleServer(id, function(input, output, session) {})
  }
}

if (!exists("mod_geo_map_ui")) {
  mod_geo_map_ui <- function(id) {
    ns <- NS(id)
    card(card_header("Geography Module Not Available"),
         p("Please ensure R/mod_geo_map_complete.R exists"))
  }
}

if (!exists("mod_geo_map_server")) {
  mod_geo_map_server <- function(id, biobank_filtered, lab_joined, config) {
    moduleServer(id, function(input, output, session) {})
  }
}

# --- FALLBACK PARSE FUNCTIONS ------------------------------------------------
if (!exists("parse_any_date")) {
  parse_any_date <- function(x) {
    lubridate::ymd(x, quiet = TRUE)
  }
}

if (!exists("parse_age")) {
  parse_age <- function(x) {
    as.numeric(x)
  }
}

if (!exists("parse_sex_code")) {
  parse_sex_code <- function(x) {
    x_clean <- toupper(trimws(as.character(x)))
    case_when(
      x_clean %in% c("M", "H", "MALE", "HOMME") ~ "M",
      x_clean %in% c("F", "FEMALE", "FEMME") ~ "F",
      TRUE ~ NA_character_
    )
  }
}

if (!exists("parse_study_code")) {
  parse_study_code <- function(x) {
    x_clean <- toupper(trimws(as.character(x)))
    case_when(
      grepl("DA|ACTIF|ACTIVE", x_clean) ~ "DA",
      grepl("DP|PASSIF|PASSIVE", x_clean) ~ "DP",
      TRUE ~ NA_character_
    )
  }
}

if (!exists("parse_temp_code")) {
  parse_temp_code <- function(x) {
    x_clean <- toupper(trimws(as.character(x)))
    case_when(
      grepl("OK|GOOD|BON", x_clean) ~ "OK",
      grepl("BROKEN|CASS|ROMPU", x_clean) ~ "Broken",
      grepl("AMB|ROOM", x_clean) ~ "Ambiante",
      grepl("FRIG|REFR", x_clean) ~ "Frigo",
      grepl("CONG|FREEZ", x_clean) ~ "Congelateur",
      TRUE ~ NA_character_
    )
  }
}

if (!exists("parse_yes_no_uncertain")) {
  parse_yes_no_uncertain <- function(x) {
    x_clean <- toupper(trimws(as.character(x)))
    result <- case_when(
      is.na(x_clean) | x_clean == "" | x_clean %in% c("NA", "N/A") ~ NA_character_,
      x_clean %in% c("OUI", "YES", "Y", "O", "1") ~ "Oui",
      x_clean %in% c("NON", "NO", "N", "0") ~ "Non",
      x_clean %in% c("INCERTAIN", "UNCERTAIN", "?") ~ "Incertain",
      TRUE ~ NA_character_
    )
    as.character(result)
  }
}

if (!exists("safe_days_between")) {
  safe_days_between <- function(end_date, start_date, max_ok = NULL) {
    days_diff <- as.numeric(difftime(end_date, start_date, units = "days"))
    if (!is.null(max_ok)) {
      days_diff <- ifelse(days_diff < 0 | days_diff > max_ok, NA_real_, days_diff)
    } else {
      days_diff <- ifelse(days_diff < 0, NA_real_, days_diff)
    }
    days_diff
  }
}

if (!exists("calc_conservation_days")) {
  calc_conservation_days <- function(date_treatment, date_sample, 
                                      date_received = NULL, date_inrb = NULL, 
                                      max_ok = 365) {
    days <- safe_days_between(date_treatment, date_sample, max_ok)
    
    if (!is.null(date_received)) {
      missing <- is.na(days) & !is.na(date_received)
      if (any(missing)) {
        days[missing] <- safe_days_between(date_received[missing], 
                                            date_sample[missing], max_ok)
      }
    }
    
    if (!is.null(date_inrb)) {
      missing <- is.na(days) & !is.na(date_inrb)
      if (any(missing)) {
        days[missing] <- safe_days_between(date_inrb[missing], 
                                            date_sample[missing], max_ok)
      }
    }
    
    days
  }
}

if (!exists("parse_shipped_to_inrb")) {
  parse_shipped_to_inrb <- function(date_env_inrb, status_field = NULL) {
    shipped <- !is.na(date_env_inrb)
    
    if (!is.null(status_field)) {
      status_clean <- toupper(trimws(as.character(status_field)))
      status_indicates_shipped <- grepl("INRB|ENV.*INRB|SHIP.*INRB|SENT", status_clean)
      shipped <- shipped | status_indicates_shipped
    }
    
    shipped
  }
}

# --- DATA CLEANING -----------------------------------------------------------
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
    rename_first("date_treatment_raw", "date.*traite|date.*treatment|date.*labo") %>%
    rename_first("temp_transport_raw", "temp.*transport") %>%
    rename_first("temp_cpltha_raw", "temp.*stockage|temp.*cpltha") %>%
    rename_first("ancien_cas_raw", "ancien.*cas|previous.*case|old.*case") %>%
    rename_first("traite_raw", "trait[eé]|treated|treatment")
  
  required <- c(
    "barcode","lab_id","date_raw","age","sex","zone","province","study",
    "structure","unit","date_received_raw","date_result_raw",
    "date_env_cpltha_raw","date_rec_cpltha_raw","date_env_inrb_raw",
    "date_treatment_raw","temp_transport_raw","temp_cpltha_raw",
    "ancien_cas_raw","traite_raw"
  )
  for (col in required) if (!col %in% names(df)) df[[col]] <- NA_character_
  
  df %>%
    mutate(
      # Parse dates
      date_sample     = parse_any_date(date_raw),
      date_received   = parse_any_date(date_received_raw),
      date_result     = parse_any_date(date_result_raw),
      date_env_cpltha = parse_any_date(date_env_cpltha_raw),
      date_rec_cpltha = parse_any_date(date_rec_cpltha_raw),
      date_env_inrb   = parse_any_date(date_env_inrb_raw),
      date_treatment  = parse_any_date(date_treatment_raw),
      
      # Parse demographics
      age_num = parse_age(age),
      sex     = parse_sex_code(sex),
      study   = parse_study_code(study),
      
      # Parse temperature
      temp_field = parse_temp_code(temp_transport_raw),
      temp_hs    = parse_temp_code(temp_cpltha_raw),
      
      # Calculate transport times
      transport_field_hs = safe_days_between(date_env_cpltha, date_sample, 30),
      transport_hs_lsd   = safe_days_between(date_rec_cpltha, date_env_cpltha, 30),
      transport_lsd_inrb = safe_days_between(date_env_inrb, date_rec_cpltha, 90),
      transport_field_cpltha = safe_days_between(date_env_cpltha, date_sample, 90),
      
      # Calculate conservation time with fallbacks
      conservation_days = calc_conservation_days(
        date_treatment, date_sample,
        date_received, date_env_inrb,
        max_ok = 365
      ),
      
      # Determine INRB shipment status
      shipped_to_inrb = parse_shipped_to_inrb(date_env_inrb),
      
      # Clean text fields
      zone      = stringr::str_squish(as.character(zone)),
      province  = stringr::str_squish(as.character(province)),
      structure = stringr::str_squish(as.character(structure)),
      unit      = stringr::str_squish(as.character(unit)),
      
      # Parse metadata - keep as character to preserve Unknown status
      ancien_cas = as.character(parse_yes_no_uncertain(ancien_cas_raw)),
      traite = as.character(parse_yes_no_uncertain(traite_raw))
    ) %>%
    dplyr::filter(!is.na(date_sample)) %>%
    dplyr::distinct(barcode, lab_id, .keep_all = TRUE)
}

make_age_groups <- function(age, width = 5) {
  if (all(is.na(age))) return(factor(levels = character()))
  hi <- suppressWarnings(ceiling(max(age, na.rm = TRUE) / width) * width)
  if (!is.finite(hi) || hi <= 0) hi <- width
  cut(age, breaks = seq(0, hi, by = width), right = FALSE, include.lowest = TRUE)
}

# --- PRECOMPUTE SUMMARIES ----------------------------------------------------
compute_kpi_summary <- memoise::memoise(function(df) {
  list(
    n_total = nrow(df),
    n_da = sum(df$study == "DA", na.rm = TRUE),
    n_dp = sum(df$study == "DP", na.rm = TRUE),
    n_sites = n_distinct(df$structure[!grepl("^(UM|MUM|MOBILE)", toupper(df$structure))]),
    n_provinces = n_distinct(df$province, na.rm = TRUE),
    n_zones = n_distinct(df$zone, na.rm = TRUE),
    median_transport = median(df$transport_field_cpltha, na.rm = TRUE),
    p95_transport = quantile(df$transport_field_cpltha, 0.95, na.rm = TRUE),
    median_conservation = median(df$conservation_days, na.rm = TRUE),
    pct_shipped_inrb = mean(df$shipped_to_inrb, na.rm = TRUE) * 100,
    latest_sample = max(df$date_sample, na.rm = TRUE),
    latest_arrival = max(df$date_rec_cpltha, na.rm = TRUE, default = NA)
  )
}, cache = cachem::cache_mem(max_age = 600))  # 10 min cache

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
    div(
      style = "font-size: 13px; color: #555;",
      textOutput("data_status"),
      hr(style = "margin: 8px 0;"),
      textOutput("latest_arrival_date")
    ),
    
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
  
  # === OVERVIEW TAB (STREAMLINED) ==========================================
  nav_panel(
    title = "Overview",
    # Reorganized KPI strip: Total → DA → DP → Provinces → Zones → Sites
    layout_columns(
      fill = FALSE, col_widths = c(2,2,2,2,2,2),
      value_box(
        title = "Total Samples",
        value = textOutput("vb_total"),
        showcase = bs_icon("clipboard-data", size = "2rem"),
        theme = "primary",
        height = "120px"
      ),
      value_box(
        title = "DA Samples",
        value = textOutput("vb_da"),
        showcase = bs_icon("fingerprint", size = "2rem"),
        theme = "info",
        height = "120px"
      ),
      value_box(
        title = "DP Samples",
        value = textOutput("vb_dp"),
        showcase = bs_icon("person-lines-fill", size = "2rem"),
        theme = "success",
        height = "120px"
      ),
      value_box(
        title = "Provinces",
        value = textOutput("vb_provinces"),
        showcase = bs_icon("geo-alt", size = "2rem"),
        theme = "warning",
        height = "120px"
      ),
      value_box(
        title = "Health Zones",
        value = textOutput("vb_zones"),
        showcase = bs_icon("pin-map", size = "2rem"),
        theme = "danger",
        height = "120px"
      ),
      value_box(
        title = "Sites Active",
        value = textOutput("vb_sites"),
        showcase = bs_icon("hospital", size = "2rem"),
        theme = "secondary",
        height = "120px"
      )
    ),
    
    # Full-height visualizations row - maximized space
    layout_columns(
      col_widths = c(6,6),
      card(
        card_header("Sample Collection Over Time"),
        plotOutput("plot_timeline", height = 550)
      ),
      card(
        card_header(
          "Age-Sex Distribution",
          popover(
            bs_icon("info-circle"),
            "Outlined bars = full dataset | Filled bars = current filters"
          )
        ),
        fluidRow(
          column(6, checkboxInput("show_overlay", "Show full dataset overlay", value = TRUE)),
          column(6, checkboxInput("show_case_status", "Show case status (ancien cas/traité)", value = FALSE))
        ),
        plotOutput("plot_age_sex_pyramid", height = 500)
      )
    )
  ),
  
  # === TRANSPORT TAB (NEW) =================================================
  nav_panel(
    title = "Transport",
    # Transport KPIs
    layout_columns(
      fill = FALSE, col_widths = c(3,3,3,3),
      value_box(
        title = "Median Transport Time",
        value = textOutput("vb_transport_median"),
        showcase = bs_icon("truck"),
        theme = "info",
        p("Sample → Shipment to CPLTHA", style = "font-size: 12px; margin: 0;")
      ),
      value_box(
        title = "95th Percentile",
        value = textOutput("vb_transport_p95"),
        showcase = bs_icon("clock-history"),
        theme = "warning",
        p("Transport time P95", style = "font-size: 12px; margin: 0;")
      ),
      value_box(
        title = "Median Conservation",
        value = textOutput("vb_conservation_median"),
        showcase = bs_icon("thermometer-snow"),
        theme = "primary",
        p("Sample → Lab treatment", style = "font-size: 12px; margin: 0;")
      ),
      value_box(
        title = "Shipped to INRB",
        value = textOutput("vb_shipped_inrb"),
        showcase = bs_icon("box-seam"),
        theme = "success",
        p("% samples sent to INRB", style = "font-size: 12px; margin: 0;")
      )
    ),
    
    # Transport visualizations
    layout_columns(
      col_widths = c(6,6),
      card(
        card_header("Transport Time Distribution"),
        plotOutput("plot_transport_dist", height = 350)
      ),
      card(
        card_header("Conservation Time Distribution"),
        plotOutput("plot_conservation_dist", height = 350)
      )
    ),
    
    # Temperature & shipment status
    layout_columns(
      col_widths = c(3,3,3,3),
      card(
        card_header("Temperature Status"),
        tableOutput("table_temperature")
      ),
      card(
        card_header("Top Contributing Sites"),
        tableOutput("table_top_sites")
      ),
      card(
        card_header("Transport Summary by Province"),
        tableOutput("table_transport_province")
      ),
      card(
        card_header("Shipment Status by Site"),
        DTOutput("table_shipment_status")
      )
    )
  ),
  
  # === DEMOGRAPHICS TAB ====================================================
  nav_panel(
    title = "Demographics",
    # Enhanced demographics summary with case history
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Demographics & Case History Summary"),
        layout_columns(
          col_widths = c(6,6),
          tableOutput("table_demographics_detailed"),
          tableOutput("table_case_history_detailed")
        )
      )
    ),
    
    # Age-sex pyramid by zone
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
  
  # === OTHER TABS ==========================================================
  nav_panel(title = "Geography", mod_geo_map_ui("geo_map")),
  nav_panel(title = "Extraction QC", mod_extractions_qc_ui("extractions_qc")),
  nav_panel(title = "Lab Results", mod_lab_results_ui("lab_results")),
  
  nav_panel(
    title = "Data",
    card(
      card_header("Sample Records",
                  class = "d-flex justify-content-between align-items-center",
                  downloadButton("download_data", "Download CSV", class = "btn-sm")),
      DTOutput("data_table")
    )
  ),
  
  nav_panel(
    title = "Debug",
    layout_columns(
      col_widths = c(6,6),
      card(card_header("Configuration"), verbatimTextOutput("debug_config")),
      card(card_header("Data Info"), verbatimTextOutput("debug_data_info"))
    )
  )
)

# --- SERVER -----------------------------------------------------------------
server <- function(input, output, session) {
  
  cfg <- reactiveVal(load_config())
  
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
  
  observe({
    conf <- cfg()
    if (!is.null(conf$paths$biobank_dir)) {
      updateTextInput(session, "data_dir", value = safe_path(conf$paths$biobank_dir))
    }
  })
  
  # Status messages
  output$data_status <- renderText({
    miss <- missing_paths()
    df <- clean_data()
    
    if (length(miss)) {
      paste(c("⚠️ Missing folders:", miss), collapse = "\n")
    } else if (is.null(df) || !nrow(df)) {
      "No data loaded"
    } else {
      latest <- max(df$date_sample, na.rm = TRUE)
      latest_str <- if (is.finite(latest)) format(latest, "%d %b %Y") else "—"
      sprintf("%s samples | Latest: %s", scales::comma(nrow(df)), latest_str)
    }
  })
  
  output$latest_arrival_date <- renderText({
    df <- clean_data()
    req(df, nrow(df) > 0)
    
    latest_arrival <- max(df$date_rec_cpltha, na.rm = TRUE, default = NA)
    if (is.finite(latest_arrival) && !is.na(latest_arrival)) {
      sprintf("Latest arrival: %s", format(latest_arrival, "%d %b %Y"))
    } else {
      "Latest arrival: —"
    }
  })
  
  output$file_selector <- renderUI({
    req(input$data_dir)
    dir_resolved <- safe_path(input$data_dir)
    
    if (!dir.exists(dir_resolved)) {
      return(helpText(paste0("⚠️ Folder not found:\n", dir_resolved)))
    }
    
    files <- list.files(dir_resolved, pattern = "\\.xlsx?$", full.names = TRUE)
    if (!length(files)) return(helpText("No Excel files found"))
    
    names(files) <- basename(files)
    selectInput("selected_file", "Select file", choices = files)
  })
  
  observeEvent(input$load_data, {
    req(input$selected_file)
    showNotification("Loading data...", duration = 2, type = "message")
    
    df_raw <- tryCatch({
      readxl::read_excel(input$selected_file, .name_repair = "minimal") %>%
        janitor::clean_names() %>%
        mutate(across(everything(), as.character))
    }, error = function(e) {
      showNotification(paste("Load error:", e$message), type = "error")
      return(NULL)
    })
    if (is.null(df_raw)) return()
    
    raw_data(df_raw)
    
    df_clean <- tryCatch({
      clean_biobank_data(df_raw)
    }, error = function(e) {
      showNotification(paste("Clean error:", e$message), type = "error")
      return(tibble())
    })
    
    clean_data(df_clean)
    
    if (!nrow(df_clean)) {
      showNotification("No usable rows after cleaning", type = "warning")
      return()
    }
    
    # Update filter choices
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
    
    dr <- range(df_clean$date_sample, na.rm = TRUE)
    start <- max(dr[1], dr[2] - 180)
    updateDateRangeInput(session, "date_range",
                         start = start, end = dr[2], min = dr[1], max = dr[2])
    
    showNotification(
      sprintf("✓ Loaded %s unique samples", scales::comma(nrow(df_clean))), 
      type = "message"
    )
  })
  
  # Cascading filters
  observe({
    df <- clean_data()
    req(df, nrow(df) > 0)
    
    if (!is.null(input$filter_province) && input$filter_province != "all") {
      zones_in_province <- df %>%
        filter(province == input$filter_province) %>%
        pull(zone) %>% unique() %>% sort() %>% na.omit()
      
      current_zone <- input$filter_zone
      new_selection <- if (current_zone %in% c("all", zones_in_province)) current_zone else "all"
      
      updateSelectInput(session, "filter_zone", 
                        choices = c("All" = "all", zones_in_province),
                        selected = new_selection)
    } else {
      all_zones <- sort(unique(na.omit(df$zone)))
      updateSelectInput(session, "filter_zone", choices = c("All" = "all", all_zones))
    }
  })
  
  observe({
    df <- clean_data()
    req(df, nrow(df) > 0)
    
    df_filtered <- df
    
    if (!is.null(input$filter_province) && input$filter_province != "all") {
      df_filtered <- df_filtered %>% filter(province == input$filter_province)
    }
    
    if (!is.null(input$filter_zone) && input$filter_zone != "all") {
      df_filtered <- df_filtered %>% filter(zone == input$filter_zone)
    }
    
    structures_available <- df_filtered %>% pull(structure) %>% unique() %>% sort() %>% na.omit()
    
    current_structure <- input$filter_structure
    new_selection <- if (current_structure %in% c("all", structures_available)) current_structure else "all"
    
    updateSelectInput(session, "filter_structure", 
                      choices = c("All" = "all", structures_available),
                      selected = new_selection)
  })
  
  # Filtered dataset
  filtered_data <- reactive({
    df <- clean_data(); req(df)
    if (!nrow(df)) return(df)
    
    dr <- input$date_range
    if (length(dr) == 2 && all(!is.na(dr))) {
      df <- df %>% filter(date_sample >= dr[1], date_sample <= dr[2])
    }
    if (!identical(input$filter_study, "all"))     df <- df %>% filter(study == input$filter_study)
    if (!identical(input$filter_province, "all"))  df <- df %>% filter(province == input$filter_province)
    if (!identical(input$filter_zone, "all"))      df <- df %>% filter(zone == input$filter_zone)
    if (!identical(input$filter_structure, "all")) df <- df %>% filter(structure == input$filter_structure)
    if (length(input$filter_sex))                  df <- df %>% filter(sex %in% input$filter_sex)
    df
  })
  
  # Cached KPI summary
  kpi_summary <- reactive({
    df <- filtered_data(); req(df, nrow(df) > 0)
    compute_kpi_summary(df)
  })

  # === VALUE BOXES =========================================================
  output$vb_total <- renderText({
    kpi <- kpi_summary(); scales::comma(kpi$n_total)
  })
  
  output$vb_da <- renderText({
    kpi <- kpi_summary()
    pct <- if (kpi$n_total > 0) round(100 * kpi$n_da / kpi$n_total, 1) else 0
    sprintf("%s (%s%%)", scales::comma(kpi$n_da), pct)
  })
  
  output$vb_dp <- renderText({
    kpi <- kpi_summary()
    pct <- if (kpi$n_total > 0) round(100 * kpi$n_dp / kpi$n_total, 1) else 0
    sprintf("%s (%s%%)", scales::comma(kpi$n_dp), pct)
  })
  
  output$vb_sites <- renderText({
    kpi <- kpi_summary(); scales::comma(kpi$n_sites)
  })
  
  output$vb_provinces <- renderText({
    kpi <- kpi_summary(); scales::comma(kpi$n_provinces)
  })
  
  output$vb_zones <- renderText({
    kpi <- kpi_summary(); scales::comma(kpi$n_zones)
  })
  
  # Transport KPIs
  output$vb_transport_median <- renderText({
    kpi <- kpi_summary()
    if (is.finite(kpi$median_transport)) {
      sprintf("%.1f days", kpi$median_transport)
    } else {
      "—"
    }
  })
  
  output$vb_transport_p95 <- renderText({
    kpi <- kpi_summary()
    if (is.finite(kpi$p95_transport)) {
      sprintf("%.1f days", kpi$p95_transport)
    } else {
      "—"
    }
  })
  
  output$vb_conservation_median <- renderText({
    kpi <- kpi_summary()
    if (is.finite(kpi$median_conservation)) {
      sprintf("%.1f days", kpi$median_conservation)
    } else {
      "—"
    }
  })
  
  output$vb_shipped_inrb <- renderText({
    kpi <- kpi_summary()
    sprintf("%.1f%%", kpi$pct_shipped_inrb)
  })

  # === PLOTS ===============================================================
  output$plot_timeline <- renderPlot({
    df <- filtered_data(); req(df, nrow(df) > 0)
    df %>%
      filter(!is.na(date_sample)) %>%
      count(week = floor_date(date_sample, "week"), study) %>%
      ggplot(aes(week, n, fill = study)) +
      geom_col() +
      scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60"), na.value = "grey70") +
      labs(x = NULL, y = "Samples", fill = "Study") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
  })
  
  output$plot_age_sex_pyramid <- renderPlot({
    df_full <- clean_data()
    df_filtered <- filtered_data()
    
    req(df_full, nrow(df_full) > 0)
    req(df_filtered, nrow(df_filtered) > 0)
    
    show_full <- isTRUE(input$show_overlay)
    show_case <- isTRUE(input$show_case_status)
    
    # Prepare data with case status if requested
    if (show_case) {
      # Filter for case status visualization
      df_viz <- df_filtered %>%
        filter(!is.na(age_num), !is.na(sex), sex %in% c("M", "F")) %>%
        mutate(
          age_group = make_age_groups(age_num, width = 5),
          sex_label = recode(sex, M = "Male", F = "Female"),
          case_status = case_when(
            ancien_cas == "Oui" | traite == "Oui" ~ "Previous/Treated",
            ancien_cas == "Non" & traite == "Non" ~ "New",
            TRUE ~ "Unknown"
          )
        ) %>%
        filter(!is.na(age_group)) %>%
        count(age_group, sex_label, case_status) %>%
        mutate(count = if_else(sex_label == "Male", -n, n))
      
      if (!nrow(df_viz)) {
        plot.new()
        text(0.5, 0.5, "No age/sex data", cex = 1.5, col = "gray50")
        return()
      }
      
      max_count <- max(abs(df_viz$count))
      
      ggplot(df_viz, aes(x = age_group, y = count, fill = interaction(sex_label, case_status))) +
        geom_col(width = 0.9, position = "stack") +
        coord_flip() +
        scale_y_continuous(labels = abs, limits = c(-max_count, max_count)) +
        scale_fill_manual(
          values = c(
            "Male.New" = "#3498DB",
            "Female.New" = "#E91E63",
            "Male.Previous/Treated" = "#1ABC9C",
            "Female.Previous/Treated" = "#F39C12",
            "Male.Unknown" = "#95A5A6",
            "Female.Unknown" = "#BDC3C7"
          ),
          labels = function(x) {
            gsub("\\.", " - ", x)
          }
        ) +
        labs(
          title = "Age-Sex Distribution by Case Status",
          subtitle = sprintf("Total: %s samples", scales::comma(sum(abs(df_viz$count)))),
          x = "Age group", y = "Sample count", fill = "Sex - Status"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        )
      
    } else {
      # Standard overlay view
      if (show_full) {
        pyramid_full <- df_full %>%
          filter(!is.na(age_num), !is.na(sex), sex %in% c("M", "F")) %>%
          mutate(
            age_group = make_age_groups(age_num, width = 5),
            sex_label = recode(sex, M = "Male", F = "Female")
          ) %>%
          filter(!is.na(age_group)) %>%
          count(age_group, sex_label) %>%
          mutate(dataset = "Full", count = if_else(sex_label == "Male", -n, n))
      } else {
        pyramid_full <- tibble()
      }
      
      pyramid_filtered <- df_filtered %>%
        filter(!is.na(age_num), !is.na(sex), sex %in% c("M", "F")) %>%
        mutate(
          age_group = make_age_groups(age_num, width = 5),
          sex_label = recode(sex, M = "Male", F = "Female")
        ) %>%
        filter(!is.na(age_group)) %>%
        count(age_group, sex_label) %>%
        mutate(dataset = "Filtered", count = if_else(sex_label == "Male", -n, n))
      
      n_filtered <- sum(abs(pyramid_filtered$count))
      low_sample_warning <- n_filtered < 5
      
      if (!nrow(pyramid_filtered)) {
        plot.new()
        text(0.5, 0.5, "No age/sex data", cex = 1.5, col = "gray50")
        return()
      }
      
      plot_data <- bind_rows(pyramid_full, pyramid_filtered)
      max_count <- max(abs(plot_data$count))
      
      ggplot(plot_data, aes(x = age_group, y = count, fill = sex_label, alpha = dataset)) +
        geom_col(width = 0.9, position = "identity", color = "black", linewidth = 0.3) +
        coord_flip() +
        scale_y_continuous(labels = abs, limits = c(-max_count, max_count)) +
        scale_fill_manual(values = c("Male" = "#3498DB", "Female" = "#E91E63")) +
        scale_alpha_manual(values = c("Full" = 0.3, "Filtered" = 1.0)) +
        labs(
          title = "Age-Sex Distribution: Filtered vs Full Dataset",
          subtitle = if (low_sample_warning) "⚠️ Low sample size (<5)" else 
            sprintf("Filtered: %s | Full: %s samples", 
                    scales::comma(n_filtered),
                    scales::comma(sum(abs(pyramid_full$count)))),
          x = "Age group", y = "Sample count", fill = "Sex", alpha = "Dataset"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.subtitle = element_text(color = if (low_sample_warning) "#E74C3C" else "black")
        )
    }
  })
  
  output$plot_transport_dist <- renderPlot({
    df <- filtered_data()
    req(df, nrow(df) > 0)
    
    transport_data <- df %>% filter(!is.na(transport_field_cpltha))
    
    if (!nrow(transport_data)) {
      plot.new()
      text(0.5, 0.5, "No transport data", cex = 1.2, col = "gray50")
      return()
    }
    
    med <- median(transport_data$transport_field_cpltha, na.rm = TRUE)
    p95 <- quantile(transport_data$transport_field_cpltha, 0.95, na.rm = TRUE)
    
    ggplot(transport_data, aes(x = transport_field_cpltha)) +
      geom_histogram(binwidth = 1, fill = "#3498DB", color = "black", alpha = 0.7) +
      geom_vline(xintercept = med, linetype = "dashed", color = "red", linewidth = 1) +
      geom_vline(xintercept = p95, linetype = "dotted", color = "orange", linewidth = 1) +
      labs(
        title = "Transport Time: Sample → Shipment to CPLTHA",
        subtitle = sprintf("Median: %.1f d | P95: %.1f d | n = %s",
                           med, p95, scales::comma(nrow(transport_data))),
        x = "Days", y = "Count"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })
  
  output$plot_conservation_dist <- renderPlot({
    df <- filtered_data()
    req(df, nrow(df) > 0)
    
    cons_data <- df %>% filter(!is.na(conservation_days))
    
    if (!nrow(cons_data)) {
      plot.new()
      text(0.5, 0.5, "No conservation data", cex = 1.2, col = "gray50")
      return()
    }
    
    med <- median(cons_data$conservation_days, na.rm = TRUE)
    
    ggplot(cons_data, aes(x = conservation_days)) +
      geom_histogram(binwidth = 5, fill = "#27AE60", color = "black", alpha = 0.7) +
      geom_vline(xintercept = med, linetype = "dashed", color = "red", linewidth = 1) +
      labs(
        title = "Conservation Time: Sample → Lab Treatment",
        subtitle = sprintf("Median: %.1f days | n = %s", med, scales::comma(nrow(cons_data))),
        x = "Days", y = "Count"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })
  
  output$plot_pyramid <- renderPlot({
    df <- filtered_data(); req(df)
    
    validate(
      need("zone" %in% names(df), "Zone missing"),
      need("sex" %in% names(df), "Sex missing"),
      need("age_num" %in% names(df), "Age missing")
    )
    
    df <- df %>%
      mutate(zone = trimws(as.character(zone)), sex = as.character(sex),
             age_num = suppressWarnings(as.numeric(age_num))) %>%
      filter(!is.na(zone) & nzchar(zone), sex %in% c("M", "F"), !is.na(age_num))
    
    validate(need(nrow(df) > 0, "No age/sex data"))
    
    min_n <- floor(input$pyramid_min_n %||% 1)
    zones_keep <- df %>% count(zone) %>% filter(n >= min_n) %>% pull(zone)
    
    validate(need(length(zones_keep) > 0, "Not enough samples per zone"))
    
    age_width <- floor(input$pyramid_age_width %||% 5)
    
    df <- df %>%
      filter(zone %in% zones_keep) %>%
      mutate(
        age_group = make_age_groups(age_num, width = age_width),
        sex_label = recode(sex, M = "Male", F = "Female"),
        facet_label = zone
      )
    
    if (isTRUE(input$pyramid_by_structure) && "structure" %in% names(df)) {
      df <- df %>%
        mutate(structure_clean = trimws(as.character(structure)),
               structure_clean = ifelse(is.na(structure_clean) | !nzchar(structure_clean), 
                                        "Unknown", structure_clean),
               facet_label = paste0(facet_label, " • ", structure_clean))
    }
    
    if (isTRUE(input$pyramid_split_study) && "study" %in% names(df)) {
      df <- df %>%
        mutate(study_clean = as.character(study),
               study_clean = ifelse(is.na(study_clean) | !nzchar(study_clean), 
                                    "Unknown", study_clean),
               facet_label = paste0(facet_label, "\n", study_clean))
    }
    
    validate(need(!all(is.na(df$age_group)), "Cannot form age groups"))
    
    df <- df %>% filter(!is.na(age_group))
    
    summary_df <- df %>%
      group_by(facet_label, age_group, sex_label) %>%
      summarise(n = n(), .groups = "drop")
    
    validate(need(nrow(summary_df) > 0, "No samples after settings"))
    
    plot_df <- summary_df %>%
      mutate(count = if_else(sex_label == "Male", -n, n),
             facet_label = factor(facet_label, levels = unique(facet_label)))
    
    max_count <- max(abs(plot_df$count), 1)
    
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

  # === TABLES ==============================================================
  output$table_demographics_detailed <- renderTable({
    df <- filtered_data(); req(df, nrow(df) > 0)
    
    # Calculate detailed age statistics
    age_valid <- df %>% filter(!is.na(age_num)) %>% pull(age_num)
    sex_counts <- table(df$sex, useNA = "ifany")
    
    tibble(
      Metric = c(
        "Total samples",
        "Samples with age data",
        "Median age (years)",
        "Mean age (years)",
        "Age range",
        "Age std. deviation",
        "─────────────",
        "Male samples",
        "Female samples",
        "Unknown sex",
        "% Female (of known)"
      ),
      Value = c(
        scales::comma(nrow(df)),
        scales::comma(length(age_valid)),
        sprintf("%.1f", median(age_valid, na.rm = TRUE)),
        sprintf("%.1f", mean(age_valid, na.rm = TRUE)),
        sprintf("%.0f - %.0f", min(age_valid, na.rm = TRUE), max(age_valid, na.rm = TRUE)),
        sprintf("%.1f", sd(age_valid, na.rm = TRUE)),
        "─────────────",
        scales::comma(sum(df$sex == "M", na.rm = TRUE)),
        scales::comma(sum(df$sex == "F", na.rm = TRUE)),
        scales::comma(sum(is.na(df$sex))),
        sprintf("%.1f%%", mean(df$sex == "F", na.rm = TRUE) * 100)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_case_history_detailed <- renderTable({
    df <- filtered_data(); req(df, nrow(df) > 0)
    
    # Treat NA/empty as "Unknown" for ancien_cas and traite
    df <- df %>%
      mutate(
        ancien_cas_clean = case_when(
          is.na(ancien_cas) | ancien_cas == "" ~ "Unknown",
          TRUE ~ as.character(ancien_cas)
        ),
        traite_clean = case_when(
          is.na(traite) | traite == "" ~ "Unknown",
          TRUE ~ as.character(traite)
        ),
        # Combined status
        case_combo = case_when(
          ancien_cas_clean == "Oui" | traite_clean == "Oui" ~ "Previous/Treated",
          ancien_cas_clean == "Non" & traite_clean == "Non" ~ "New patient",
          TRUE ~ "Unknown"
        )
      )
    
    # Count breakdowns
    ancien_cas_summary <- df %>%
      count(ancien_cas_clean) %>%
      mutate(pct = sprintf("%.1f%%", n / sum(n) * 100)) %>%
      rename(Status = ancien_cas_clean, Count = n, Percent = pct)
    
    traite_summary <- df %>%
      count(traite_clean) %>%
      mutate(pct = sprintf("%.1f%%", n / sum(n) * 100)) %>%
      rename(Status = traite_clean, Count = n, Percent = pct)
    
    combined_summary <- df %>%
      count(case_combo) %>%
      mutate(pct = sprintf("%.1f%%", n / sum(n) * 100)) %>%
      rename(Status = case_combo, Count = n, Percent = pct)
    
    # Create unified table
    bind_rows(
      tibble(Category = "ANCIEN CAS", Status = "───────────", Count = "───", Percent = "───"),
      ancien_cas_summary %>% mutate(Category = ""),
      tibble(Category = "", Status = "", Count = "", Percent = ""),
      tibble(Category = "TRAITÉ", Status = "───────────", Count = "───", Percent = "───"),
      traite_summary %>% mutate(Category = ""),
      tibble(Category = "", Status = "", Count = "", Percent = ""),
      tibble(Category = "COMBINED", Status = "───────────", Count = "───", Percent = "───"),
      combined_summary %>% mutate(Category = "")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_top_sites <- renderTable({
    df <- filtered_data(); req(df, nrow(df) > 0)
    df %>%
      filter(!is.na(structure)) %>%
      count(structure, sort = TRUE) %>%
      head(5) %>%
      mutate(n = scales::comma(n)) %>%
      rename(Site = structure, Samples = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_temperature <- renderTable({
    df <- filtered_data(); req(df, nrow(df) > 0)
    
    temp_summary <- df %>%
      mutate(
        temp_status = case_when(
          is.na(temp_hs) ~ "Unknown",
          temp_hs == "Frigo" ~ "Refrigerated",
          temp_hs == "Congelateur" ~ "Frozen",
          temp_hs == "Ambiante" ~ "Ambient",
          TRUE ~ "Other"
        )
      ) %>%
      count(temp_status) %>%
      mutate(Percent = sprintf("%.1f%%", n / sum(n) * 100)) %>%
      rename(Status = temp_status, Count = n)
    
    temp_summary
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_transport_province <- renderTable({
    df <- filtered_data(); req(df, nrow(df) > 0)
    
    df %>%
      filter(!is.na(province), !is.na(transport_field_cpltha)) %>%
      group_by(province) %>%
      summarise(
        n = n(),
        Median = median(transport_field_cpltha, na.rm = TRUE),
        P95 = quantile(transport_field_cpltha, 0.95, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        n = scales::comma(n),
        Median = sprintf("%.1f d", Median),
        P95 = sprintf("%.1f d", P95)
      ) %>%
      rename(Province = province, Samples = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_shipment_status <- renderDT({
    df <- filtered_data(); req(df, nrow(df) > 0)
    
    df %>%
      filter(!is.na(structure)) %>%
      group_by(structure) %>%
      summarise(
        Total = n(),
        Shipped = sum(shipped_to_inrb, na.rm = TRUE),
        `% Shipped` = sprintf("%.1f%%", mean(shipped_to_inrb, na.rm = TRUE) * 100),
        `Median Transport (d)` = sprintf("%.1f", median(transport_field_cpltha, na.rm = TRUE)),
        `Outliers (>P95)` = sum(transport_field_cpltha > 
                                  quantile(df$transport_field_cpltha, 0.95, na.rm = TRUE), 
                                na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Total)) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        filter = "top", rownames = FALSE
      )
  })

  # === MODULES =============================================================
  lab_modules <- mod_lab_results_server("lab_results", biobank_clean = clean_data, config = cfg)
  mod_extractions_qc_server("extractions_qc", biobank_clean = clean_data, config = cfg)
  mod_geo_map_server("geo_map", biobank_filtered = filtered_data, 
                     lab_joined = lab_modules$lab_joined, config = cfg)

  # === DATA EXPORT =========================================================
  output$data_table <- renderDT({
    df <- filtered_data(); req(df)
    df %>%
      select(barcode, lab_id, date_sample, date_received, study, sex, age_num,
             province, zone, structure, transport_field_cpltha, conservation_days,
             shipped_to_inrb, ancien_cas, traite) %>%
      datatable(options = list(pageLength = 25, scrollX = TRUE),
                filter = "top", rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = function() sprintf("biobank_export_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content  = function(file) readr::write_csv(filtered_data(), file)
  )
  
  # === DEBUG ===============================================================
  output$debug_config <- renderPrint({ cfg() })
  output$debug_data_info <- renderPrint({
    df <- clean_data(); if (is.null(df)) return("No data loaded")
    list(
      rows = nrow(df),
      columns = ncol(df),
      column_names = names(df),
      study_breakdown = table(df$study, useNA = "ifany"),
      date_range = range(df$date_sample, na.rm = TRUE),
      transport_stats = summary(df$transport_field_cpltha),
      conservation_stats = summary(df$conservation_days)
    )
  })
}

# --- RUN --------------------------------------------------------------------
shinyApp(ui, server)
