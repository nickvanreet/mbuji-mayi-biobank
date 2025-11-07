# MBUJI-MAYI BIOBANK DASHBOARD — COMPLETE v2.6
# =============================================================================
# v2.6: Enhanced QC + All original features restored
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
  library(memoise)
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
        title = "Mbuji-Mayi Biobank Dashboard", version = "2.6.0",
        institution = "Institute of Tropical Medicine, Antwerp",
        last_updated = "2025-11-07"
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

# --- DATA QUALITY CHECKING (NEW) ---------------------------------------------
check_data_quality <- function(df_raw) {
  if (is.null(df_raw) || !is.data.frame(df_raw) || nrow(df_raw) == 0) {
    return(list(
      raw_data = df_raw,
      qc_report = tibble(check = "No data", result = "ERROR"),
      valid_data = tibble(),
      duplicates = tibble(),
      qc_summary = tibble(),
      field_completeness = tibble(),
      exclusion_reasons = tibble(),
      qc_checks = list()
    ))
  }
  
  # CRITICAL: Use actual column names after clean_names
  # After janitor::clean_names(), columns are already cleaned
  df <- df_raw
  
  # Map to our standard names - handle the actual cleaned names
  rename_first <- function(df, new_name, pattern) {
    hits <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hits) >= 1) {
      df <- df %>% dplyr::rename(!!new_name := dplyr::all_of(hits[1]))
    }
    df
  }
  
  df <- df %>%
    rename_first("numero", "num[eé]ro") %>%
    rename_first("barcode", "code.*barr|barcode") %>%
    rename_first("date_raw", "date.*pr[eé]lev|date.*sample|^date$") %>%
    rename_first("age", "^age|ann[eé]e.*naissance") %>%
    rename_first("sex", "^sex|^sexe") %>%
    rename_first("zone", "zone.*sant[eé]|^zs$") %>%
    rename_first("province", "^province") %>%
    rename_first("study", "[eé]tude|study") %>%
    rename_first("structure", "structure.*sanit") %>%
    rename_first("drs_present", "pr[eé]sence.*drs") %>%
    rename_first("dbs_present", "pr[eé]sence.*dbs") %>%
    rename_first("dbs_count", "nombre.*dbs")
  
  # QC Checks
  total_rows <- nrow(df)
  
  # Completely empty rows
  empty_rows_mask <- df %>%
    select(where(~ !all(is.na(.) | . == ""))) %>%
    {nrow(df) - nrow(.)} > 0
  
  if (empty_rows_mask) {
    df_non_empty <- df %>% filter(if_any(everything(), ~ !is.na(.) & . != ""))
  } else {
    df_non_empty <- df
  }
  
  n_empty_rows <- total_rows - nrow(df_non_empty)
  
  # Rows with minimum required fields
  has_numero <- !is.na(df_non_empty$numero) & df_non_empty$numero != ""
  has_barcode <- !is.na(df_non_empty$barcode) & df_non_empty$barcode != ""
  has_minimum <- has_numero & has_barcode
  
  n_with_minimum <- sum(has_minimum)
  
  # Get valid data
  df_valid <- df_non_empty[has_minimum, ]
  
  # Field completeness
  field_completeness <- tibble(
    Field = c("Numéro", "Code-barres KPS", "Date prélèvement", "Étude", 
              "Structure sanitaire", "Zone de santé", "Province",
              "Age", "Sexe", "Présence DRS", "Présence DBS", "Nombre DBS"),
    Column = c("numero", "barcode", "date_raw", "study", "structure", 
               "zone", "province", "age", "sex", "drs_present", "dbs_present", "dbs_count"),
    Present = 0,
    Missing = 0,
    `%Complete` = 0
  )
  
  if (nrow(df_valid) > 0) {
    for (i in 1:nrow(field_completeness)) {
      col <- field_completeness$Column[i]
      if (col %in% names(df_valid)) {
        present <- sum(!is.na(df_valid[[col]]) & df_valid[[col]] != "", na.rm = TRUE)
        missing <- nrow(df_valid) - present
        pct <- round(100 * present / nrow(df_valid), 1)
        
        field_completeness$Present[i] <- present
        field_completeness$Missing[i] <- missing
        field_completeness$`%Complete`[i] <- pct
      }
    }
  }
  
  # Duplicate detection
  duplicates_info <- tibble()
  n_dup_numero <- 0
  n_dup_barcode <- 0
  
  if (nrow(df_valid) > 0) {
    dup_numero <- df_valid %>%
      group_by(numero) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      arrange(numero) %>%
      select(any_of(c("numero", "barcode", "date_raw", "study", "structure", "zone"))) %>%
      mutate(duplicate_type = "Duplicate Numéro")
    
    dup_barcode <- df_valid %>%
      group_by(barcode) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      arrange(barcode) %>%
      select(any_of(c("numero", "barcode", "date_raw", "study", "structure", "zone"))) %>%
      mutate(duplicate_type = "Duplicate Barcode")
    
    duplicates_info <- bind_rows(dup_numero, dup_barcode) %>% distinct()
    
    n_dup_numero <- if (nrow(dup_numero) > 0) n_distinct(dup_numero$numero) else 0
    n_dup_barcode <- if (nrow(dup_barcode) > 0) n_distinct(dup_barcode$barcode) else 0
  }
  
  # Exclusion reasons
  df_excluded <- df_non_empty[!has_minimum, ]
  
  exclusion_reasons <- tibble()
  if (nrow(df_excluded) > 0) {
    exclusion_reasons <- df_excluded %>%
      mutate(
        row_number = row_number(),
        missing_numero = is.na(numero) | numero == "",
        missing_barcode = is.na(barcode) | barcode == "",
        reason = case_when(
          missing_numero & missing_barcode ~ "Missing both Numéro and Barcode",
          missing_numero ~ "Missing Numéro",
          missing_barcode ~ "Missing Barcode",
          TRUE ~ "Other"
        )
      ) %>%
      select(any_of(c("row_number", "numero", "barcode", "date_raw", "study", "structure", "reason")))
  }
  
  # QC Summary
  qc_summary <- tibble(
    Metric = c(
      "Total rows in Excel file",
      "Completely empty rows",
      "Rows with data (any field)",
      "─────────────────────────",
      "✓ Rows with Numéro + Barcode",
      "✗ Rows excluded (missing required)",
      "─────────────────────────",
      "Duplicate Numéros detected",
      "Duplicate Barcodes detected"
    ),
    Count = c(
      total_rows,
      n_empty_rows,
      nrow(df_non_empty),
      NA_integer_,
      n_with_minimum,
      nrow(df_excluded),
      NA_integer_,
      n_dup_numero,
      n_dup_barcode
    ),
    `% of Total` = c(
      100,
      round(100 * n_empty_rows / total_rows, 1),
      round(100 * nrow(df_non_empty) / total_rows, 1),
      NA_real_,
      round(100 * n_with_minimum / total_rows, 1),
      round(100 * nrow(df_excluded) / total_rows, 1),
      NA_real_,
      NA_real_,
      NA_real_
    )
  )
  
  list(
    raw_data = df_raw,
    valid_data = df_valid,
    qc_summary = qc_summary,
    field_completeness = field_completeness,
    duplicates = duplicates_info,
    exclusion_reasons = exclusion_reasons,
    qc_checks = list(
      total_rows = total_rows,
      empty_rows = n_empty_rows,
      rows_with_minimum = n_with_minimum,
      n_dup_numero = n_dup_numero,
      n_dup_barcode = n_dup_barcode
    )
  )
}

# --- DATA CLEANING (ORIGINAL LOGIC RESTORED) ---------------------------------
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
    rename_first("age", "^age|ann[eé]e.*naissance") %>%
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
    rename_first("traite_raw", "trait[eé]|treated|treatment") %>%
    rename_first("drs_present", "pr[eé]sence.*drs") %>%
    rename_first("dbs_present", "pr[eé]sence.*dbs") %>%
    rename_first("dbs_count", "nombre.*dbs")
  
  required <- c(
    "barcode","lab_id","date_raw","age","sex","zone","province","study",
    "structure","unit","date_received_raw","date_result_raw",
    "date_env_cpltha_raw","date_rec_cpltha_raw","date_env_inrb_raw",
    "date_treatment_raw","temp_transport_raw","temp_cpltha_raw",
    "ancien_cas_raw","traite_raw","drs_present","dbs_present","dbs_count"
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
      
      # Calculate conservation time
      conservation_days = calc_conservation_days(
        date_treatment, date_sample,
        date_received, date_env_inrb,
        max_ok = 365
      ),
      
      # INRB shipment
      shipped_to_inrb = parse_shipped_to_inrb(date_env_inrb),
      
      # Clean text
      zone      = stringr::str_squish(as.character(zone)),
      province  = stringr::str_squish(as.character(province)),
      structure = stringr::str_squish(as.character(structure)),
      unit      = stringr::str_squish(as.character(unit)),
      
      # Parse metadata
      ancien_cas = as.character(parse_yes_no_uncertain(ancien_cas_raw)),
      traite = as.character(parse_yes_no_uncertain(traite_raw)),
      
      # Parse DRS/DBS
      drs_present = parse_yes_no_uncertain(drs_present),
      dbs_present = parse_yes_no_uncertain(dbs_present),
      dbs_count = as.numeric(dbs_count)
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
    n_sites = n_distinct(df$structure, na.rm = TRUE),
    n_provinces = n_distinct(df$province, na.rm = TRUE),
    n_zones = n_distinct(df$zone, na.rm = TRUE),
    median_transport = median(df$transport_field_cpltha, na.rm = TRUE),
    p95_transport = quantile(df$transport_field_cpltha, 0.95, na.rm = TRUE),
    median_conservation = median(df$conservation_days, na.rm = TRUE),
    pct_shipped_inrb = mean(df$shipped_to_inrb, na.rm = TRUE) * 100,
    latest_sample = max(df$date_sample, na.rm = TRUE),
    latest_arrival = max(df$date_rec_cpltha, na.rm = TRUE, default = NA)
  )
}, cache = cachem::cache_mem(max_age = 600))

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
    h5("Quick QC Summary"),
    card(
      card_body(
        div(
          style = "display:flex; justify-content:space-between; align-items:baseline; margin:.25rem 0;",
          span("Rows in file"),
          strong(textOutput("qc_total_rows"))
        ),
        div(
          style = "display:flex; justify-content:space-between; align-items:baseline; margin:.25rem 0;",
          span("Valid samples"),
          strong(style = "color:#27AE60;", textOutput("qc_valid_samples"))
        ),
        div(
          style = "display:flex; justify-content:space-between; align-items:baseline; margin:.25rem 0;",
          span("⚠ Duplicates"),
          strong(style = "color:#F39C12;", textOutput("qc_duplicates"))
        )
      )
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
  
  # === DATA QUALITY TAB (NEW) ==============================================
  nav_panel(
    title = "📊 Data Quality",
    
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            span("Quality Control Summary"),
            downloadButton("download_qc_report", "Download QC Report", 
                          class = "btn-sm btn-outline-primary")
          )
        ),
        tableOutput("table_qc_summary")
      )
    ),
    
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Field Completeness (Valid Samples Only)"),
        p("Completeness of important fields among samples with Numéro + Barcode",
          style = "padding: 10px; color: #7F8C8D; margin: 0;"),
        tableOutput("table_field_completeness")
      )
    ),
    
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center; background-color: #F39C12; color: white; padding: 10px;",
            span("⚠ Duplicate Detection"),
            downloadButton("download_duplicates", "Download Duplicates", 
                          class = "btn-sm btn-outline-light")
          )
        ),
        uiOutput("duplicates_alert"),
        DTOutput("table_duplicates")
      )
    ),
    
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Excluded Rows (Missing Required Fields)"),
        p("Rows that could not be included because they lack Numéro and/or Barcode",
          style = "padding: 10px; color: #7F8C8D; margin: 0;"),
        DTOutput("table_exclusions")
      )
    )
  ),
  
  # === OVERVIEW TAB (ORIGINAL) =============================================
  nav_panel(
    title = "Overview",
    layout_columns(
      fill = FALSE, col_widths = c(2,2,2,2,2,2),
      value_box(
        title = "Total Samples",
        value = textOutput("vb_total"),
        showcase = bs_icon("clipboard-data", size = "2rem"),
        showcase_layout = "top right",
        theme = "primary",
        height = "120px"
      ),
      value_box(
        title = "DA Samples",
        value = textOutput("vb_da"),
        showcase = bs_icon("fingerprint", size = "2rem"),
        showcase_layout = "top right",
        theme = "info",
        height = "120px"
      ),
      value_box(
        title = "DP Samples",
        value = textOutput("vb_dp"),
        showcase = bs_icon("person-lines-fill", size = "2rem"),
        showcase_layout = "top right",
        theme = "success",
        height = "120px"
      ),
      value_box(
        title = "Provinces",
        value = textOutput("vb_provinces"),
        showcase = bs_icon("geo-alt", size = "2rem"),
        showcase_layout = "top right",
        theme = "warning",
        height = "120px"
      ),
      value_box(
        title = "Health Zones",
        value = textOutput("vb_zones"),
        showcase = bs_icon("pin-map", size = "2rem"),
        showcase_layout = "top right",
        theme = "danger",
        height = "120px"
      ),
      value_box(
        title = "Sites Active",
        value = textOutput("vb_sites"),
        showcase = bs_icon("hospital", size = "2rem"),
        showcase_layout = "top right",
        theme = "secondary",
        height = "120px"
      )
    ),
    
    layout_columns(
      col_widths = c(6,6),
      card(
        card_header("Sample Collection Over Time"),
        plotOutput("plot_timeline", height = 550)
      ),
      card(
        card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            span("Age-Sex Distribution"),
            downloadButton("download_pyramid", "Download Plot", class = "btn-sm btn-outline-primary")
          ),
        ),
        fluidRow(
          column(6, checkboxInput("show_overlay", "Show full dataset overlay", value = TRUE)),
          column(6, checkboxInput("show_case_status", "Show case status", value = FALSE))
        ),
        plotOutput("plot_age_sex_pyramid", height = 500)
      )
    )
  ),
  
  # === TRANSPORT TAB (ORIGINAL) ============================================
  nav_panel(
    title = "Transport",
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
  
  # === DEMOGRAPHICS TAB (ORIGINAL) =========================================
  nav_panel(
    title = "Demographics",
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            span("Demographics & Case History"),
            downloadButton("download_demog_case", "Download Table", class = "btn-sm btn-outline-primary")
          )
        ),
        p("Combined summary of age/sex and case history (current filters vs full).",
          style = "padding:10px; color:#7F8C8D; margin:0;"),
        tableOutput("table_demog_case_full")
      )
    )
  ),
  
  # === OTHER TABS (ORIGINAL) ===============================================
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

# --- SERVER FUNCTION ---------------------------------------------------------
server <- function(input, output, session) {
  
  cfg <- reactiveVal(load_config())
  
  # Store QC and clean data
  qc_results <- reactiveVal(NULL)
  clean_data <- reactiveVal(NULL)
  
  observe({
    conf <- cfg()
    if (!is.null(conf$paths$biobank_dir)) {
      updateTextInput(session, "data_dir", value = safe_path(conf$paths$biobank_dir))
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
  
  # === DATA LOADING WITH QC ================================================
  observeEvent(input$load_data, {
    req(input$selected_file)
    showNotification("Loading and checking data quality...", duration = 3, type = "message")
    
    df_raw <- tryCatch({
      readxl::read_excel(input$selected_file, .name_repair = "minimal") %>%
        janitor::clean_names() %>%
        mutate(across(everything(), as.character))
    }, error = function(e) {
      showNotification(paste("Load error:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(df_raw)) return()
    
    # Perform QC
    qc <- tryCatch({
      check_data_quality(df_raw)
    }, error = function(e) {
      showNotification(paste("QC error:", e$message), type = "error", duration = 10)
      return(NULL)
    })
    
    if (is.null(qc)) return()
    qc_results(qc)
    
    # Clean valid data
    df_clean <- tryCatch({
      clean_biobank_data(qc$valid_data)
    }, error = function(e) {
      showNotification(paste("Clean error:", e$message), type = "error", duration = 10)
      return(tibble())
    })
    
    clean_data(df_clean)
    
    # Show summary
    n_valid <- qc$qc_checks$rows_with_minimum
    n_dup <- qc$qc_checks$n_dup_numero + qc$qc_checks$n_dup_barcode
    
    msg <- sprintf("✓ Loaded %s valid samples", scales::comma(nrow(df_clean)))
    if (n_dup > 0) {
      msg <- paste0(msg, sprintf(" | ⚠ %d duplicates found", n_dup))
    }
    
    showNotification(msg, type = if (n_dup > 0) "warning" else "message", duration = 5)
    
    # Update filters
    if (nrow(df_clean) > 0) {
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
    }
  })
  
  # === QC OUTPUTS ==========================================================
  output$qc_total_rows <- renderText({
    qc <- qc_results()
    if (is.null(qc)) return("—")
    scales::comma(qc$qc_checks$total_rows)
  })
  
  output$qc_valid_samples <- renderText({
    qc <- qc_results()
    if (is.null(qc)) return("—")
    scales::comma(qc$qc_checks$rows_with_minimum)
  })
  
  output$qc_duplicates <- renderText({
    qc <- qc_results()
    if (is.null(qc)) return("—")
    n_dup <- qc$qc_checks$n_dup_numero + qc$qc_checks$n_dup_barcode
    if (n_dup == 0) return("None ✓")
    scales::comma(n_dup)
  })
  
  output$table_qc_summary <- renderTable({
    qc <- qc_results()
    req(qc)
    qc$qc_summary
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "—")
  
  output$table_field_completeness <- renderTable({
    qc <- qc_results()
    req(qc)
    qc$field_completeness %>%
      select(-Column) %>%
      mutate(
        Present = scales::comma(Present),
        Missing = scales::comma(Missing),
        `%Complete` = sprintf("%.1f%%", `%Complete`)
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$duplicates_alert <- renderUI({
    qc <- qc_results()
    req(qc)
    
    n_dup <- qc$qc_checks$n_dup_numero + qc$qc_checks$n_dup_barcode
    
    if (n_dup == 0) {
      div(
        class = "alert alert-success",
        style = "margin: 10px;",
        HTML("<strong>✓ No duplicates detected!</strong> All Numéros and Barcodes are unique.")
      )
    } else {
      div(
        class = "alert alert-warning",
        style = "margin: 10px;",
        HTML(sprintf(
          "<strong>⚠ Warning: %d duplicate(s) detected!</strong><br/>",
          n_dup
        )),
        HTML(sprintf(
          "Duplicate Numéros: %d | Duplicate Barcodes: %d<br/>",
          qc$qc_checks$n_dup_numero,
          qc$qc_checks$n_dup_barcode
        )),
        HTML("<em>Review the table below and resolve duplicates before analysis.</em>")
      )
    }
  })
  
  output$table_duplicates <- renderDT({
    qc <- qc_results()
    req(qc)
    
    if (nrow(qc$duplicates) == 0) {
      return(datatable(
        tibble(Message = "No duplicates found!"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    qc$duplicates %>%
      mutate(date_raw = as.character(date_raw)) %>%
      datatable(
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      ) %>%
      formatStyle(
        'duplicate_type',
        backgroundColor = styleEqual(
          c("Duplicate Numéro", "Duplicate Barcode"),
          c("#FFE5E5", "#FFF4E5")
        )
      )
  })
  
  output$table_exclusions <- renderDT({
    qc <- qc_results()
    req(qc)
    
    if (nrow(qc$exclusion_reasons) == 0) {
      return(datatable(
        tibble(Message = "All rows with data have required fields!"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    qc$exclusion_reasons %>%
      mutate(date_raw = as.character(date_raw)) %>%
      datatable(
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      )
  })
  
  # === DOWNLOAD HANDLERS ===================================================
  output$download_qc_report <- downloadHandler(
    filename = function() sprintf("QC_report_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      qc <- qc_results()
      req(qc)
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Summary")
      openxlsx::writeData(wb, "Summary", qc$qc_summary)
      openxlsx::addWorksheet(wb, "Field_Completeness")
      openxlsx::writeData(wb, "Field_Completeness", qc$field_completeness)
      openxlsx::addWorksheet(wb, "Duplicates")
      openxlsx::writeData(wb, "Duplicates", if (nrow(qc$duplicates) > 0) qc$duplicates else tibble(Note = "No duplicates"))
      openxlsx::addWorksheet(wb, "Exclusions")
      openxlsx::writeData(wb, "Exclusions", if (nrow(qc$exclusion_reasons) > 0) qc$exclusion_reasons else tibble(Note = "No exclusions"))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_duplicates <- downloadHandler(
    filename = function() sprintf("duplicates_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      qc <- qc_results()
      req(qc)
      readr::write_csv(qc$duplicates, file)
    }
  )
  
  # === CASCADING FILTERS ===================================================
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
  
  # === FILTERED DATA =======================================================
  filtered_data <- reactive({
    df <- clean_data()
    req(df, nrow(df) > 0)
    
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
  
  kpi_summary <- reactive({
    df <- filtered_data()
    req(df, nrow(df) > 0)
    compute_kpi_summary(df)
  })
  
  # === VALUE BOXES =========================================================
  output$vb_total <- renderText({
    kpi <- kpi_summary()
    scales::comma(kpi$n_total)
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
    kpi <- kpi_summary()
    scales::comma(kpi$n_sites)
  })
  
  output$vb_provinces <- renderText({
    kpi <- kpi_summary()
    scales::comma(kpi$n_provinces)
  })
  
  output$vb_zones <- renderText({
    kpi <- kpi_summary()
    scales::comma(kpi$n_zones)
  })
  
  output$vb_transport_median <- renderText({
    kpi <- kpi_summary()
    if (is.finite(kpi$median_transport)) sprintf("%.1f days", kpi$median_transport) else "—"
  })
  
  output$vb_transport_p95 <- renderText({
    kpi <- kpi_summary()
    if (is.finite(kpi$p95_transport)) sprintf("%.1f days", kpi$p95_transport) else "—"
  })
  
  output$vb_conservation_median <- renderText({
    kpi <- kpi_summary()
    if (is.finite(kpi$median_conservation)) sprintf("%.1f days", kpi$median_conservation) else "—"
  })
  
  output$vb_shipped_inrb <- renderText({
    kpi <- kpi_summary()
    sprintf("%.1f%%", kpi$pct_shipped_inrb)
  })
  
  # === PLOTS ===============================================================
  output$plot_timeline <- renderPlot({
    df <- filtered_data()
    req(df, nrow(df) > 0)
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
    
    if (show_case) {
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
          labels = function(x) gsub("\\.", " - ", x)
        ) +
        labs(
          title = "Age-Sex Distribution by Case Status",
          subtitle = sprintf("Total: %s samples", scales::comma(sum(abs(df_viz$count)))),
          x = "Age group", y = "Sample count", fill = "Sex - Status"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
      
    } else {
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
          subtitle = sprintf("Filtered: %s | Full: %s samples", 
                             scales::comma(sum(abs(pyramid_filtered$count))),
                             scales::comma(sum(abs(pyramid_full$count)))),
          x = "Age group", y = "Sample count", fill = "Sex", alpha = "Dataset"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
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
  
  # === TABLES ==============================================================
  make_demog_case_tbl_compare <- function(df_filtered, df_full) {
    if (is.null(df_full) || !nrow(df_full)) return(tibble())
    
    sex_counts <- function(df) {
      tibble(
        Male   = sum(df$sex == "M", na.rm = TRUE),
        Female = sum(df$sex == "F", na.rm = TRUE),
        Sex_Unknown = sum(is.na(df$sex))
      )
    }
    
    case_counts <- function(df) {
      df %>%
        mutate(
          ancien_cas_clean = ifelse(is.na(ancien_cas) | ancien_cas == "", "Unknown", as.character(ancien_cas)),
          traite_clean = ifelse(is.na(traite) | traite == "", "Unknown", as.character(traite)),
          case_combo = case_when(
            ancien_cas_clean == "Oui" | traite_clean == "Oui" ~ "Previous/Treated",
            ancien_cas_clean == "Non" & traite_clean == "Non" ~ "New patient",
            TRUE ~ "Unknown"
          )
        ) %>%
        summarise(
          Previous_Treated = sum(case_combo == "Previous/Treated", na.rm = TRUE),
          New_Patient      = sum(case_combo == "New patient", na.rm = TRUE),
          Case_Unknown     = sum(case_combo == "Unknown", na.rm = TRUE)
        )
    }
    
    df_f <- df_filtered
    if (is.null(df_f)) df_f <- df_full
    
    a_full <- df_full$age_num[!is.na(df_full$age_num)]
    a_filt <- df_f$age_num[!is.na(df_f$age_num)]
    
    sex_full <- sex_counts(df_full)
    sex_filt <- sex_counts(df_f)
    
    case_full <- case_counts(df_full)
    case_filt <- case_counts(df_f)
    
    pct_of_total <- function(x_filt, x_full) ifelse(x_full > 0, x_filt / x_full, NA_real_)
    
    tibble::tibble(
      Metric = c(
        "Total samples",
        "Samples with age",
        "Median age (y)",
        "Mean age (y)",
        "Age range",
        "SD age (y)",
        "Male",
        "Female",
        "Sex unknown",
        "% Female (of known)",
        "Previous/Treated",
        "New patient",
        "Case status unknown"
      ),
      Filtered = c(
        nrow(df_f),
        length(a_filt),
        if (length(a_filt)) sprintf("%.1f", median(a_filt, na.rm = TRUE)) else "—",
        if (length(a_filt)) sprintf("%.1f", mean(a_filt, na.rm = TRUE)) else "—",
        if (length(a_filt)) sprintf("%.0f–%.0f", min(a_filt), max(a_filt)) else "—",
        if (length(a_filt)) sprintf("%.1f", sd(a_filt, na.rm = TRUE)) else "—",
        scales::comma(sex_filt$Male),
        scales::comma(sex_filt$Female),
        scales::comma(sex_filt$Sex_Unknown),
        {
          denom <- sex_filt$Male + sex_filt$Female
          if (denom > 0) scales::percent(sex_filt$Female / denom, accuracy = 0.1) else "—"
        },
        scales::comma(case_filt$Previous_Treated),
        scales::comma(case_filt$New_Patient),
        scales::comma(case_filt$Case_Unknown)
      ),
      `Full Dataset` = c(
        nrow(df_full),
        length(a_full),
        if (length(a_full)) sprintf("%.1f", median(a_full, na.rm = TRUE)) else "—",
        if (length(a_full)) sprintf("%.1f", mean(a_full, na.rm = TRUE)) else "—",
        if (length(a_full)) sprintf("%.0f–%.0f", min(a_full), max(a_full)) else "—",
        if (length(a_full)) sprintf("%.1f", sd(a_full, na.rm = TRUE)) else "—",
        scales::comma(sex_full$Male),
        scales::comma(sex_full$Female),
        scales::comma(sex_full$Sex_Unknown),
        {
          denom <- sex_full$Male + sex_full$Female
          if (denom > 0) scales::percent(sex_full$Female / denom, accuracy = 0.1) else "—"
        },
        scales::comma(case_full$Previous_Treated),
        scales::comma(case_full$New_Patient),
        scales::comma(case_full$Case_Unknown)
      ),
      `% of Total` = c(
        scales::percent(pct_of_total(nrow(df_f), nrow(df_full)), accuracy = 0.1),
        scales::percent(pct_of_total(length(a_filt), length(a_full)), accuracy = 0.1),
        "—","—","—","—",
        scales::percent(pct_of_total(sex_filt$Male, sex_full$Male), accuracy = 0.1),
        scales::percent(pct_of_total(sex_filt$Female, sex_full$Female), accuracy = 0.1),
        if (sex_full$Sex_Unknown > 0)
          scales::percent(pct_of_total(sex_filt$Sex_Unknown, sex_full$Sex_Unknown), accuracy = 0.1)
        else "—",
        "—",
        scales::percent(pct_of_total(case_filt$Previous_Treated, case_full$Previous_Treated), accuracy = 0.1),
        scales::percent(pct_of_total(case_filt$New_Patient, case_full$New_Patient), accuracy = 0.1),
        scales::percent(pct_of_total(case_filt$Case_Unknown, case_full$Case_Unknown), accuracy = 0.1)
      )
    )
  }
  
  output$table_demog_case_full <- renderTable({
    df_full <- clean_data()
    df_filt <- filtered_data()
    req(df_full, nrow(df_full) > 0, df_filt)
    make_demog_case_tbl_compare(df_filt, df_full)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  output$download_demog_case <- downloadHandler(
    filename = function() sprintf("demographics_case_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      df_full <- clean_data()
      df_filt <- filtered_data()
      req(df_full, df_filt)
      readr::write_csv(make_demog_case_tbl_compare(df_filt, df_full), file)
    }
  )
  
  output$table_top_sites <- renderTable({
    df <- filtered_data()
    req(df, nrow(df) > 0)
    df %>%
      filter(!is.na(structure)) %>%
      count(structure, sort = TRUE) %>%
      head(5) %>%
      mutate(n = scales::comma(n)) %>%
      rename(Site = structure, Samples = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_temperature <- renderTable({
    df <- filtered_data()
    req(df, nrow(df) > 0)
    
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
    df <- filtered_data()
    req(df, nrow(df) > 0)
    
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
    df <- filtered_data()
    req(df, nrow(df) > 0)
    
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
  
  # === DATA TABLE & EXPORT =================================================
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
    filename = function() sprintf("biobank_export_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      readr::write_csv(filtered_data(), file)
    }
  )
  
  output$download_pyramid <- downloadHandler(
    filename = function() sprintf("age_sex_pyramid_%s.png", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      p <- last_plot()
      ggsave(file, plot = p, width = 10, height = 8, dpi = 300)
    }
  )
  
  # === MODULES =============================================================
  lab_modules <- mod_lab_results_server("lab_results", biobank_clean = clean_data, config = cfg)
  mod_extractions_qc_server("extractions_qc", biobank_clean = clean_data, config = cfg)
  mod_geo_map_server("geo_map", biobank_filtered = filtered_data, 
                     lab_joined = lab_modules$lab_joined, config = cfg)
  
  # === STATUS MESSAGES =====================================================
  output$data_status <- renderText({
    df <- clean_data()
    qc <- qc_results()
    
    if (is.null(qc)) {
      "No data loaded"
    } else if (is.null(df) || !nrow(df)) {
      sprintf("⚠️ No valid samples")
    } else {
      n_valid <- qc$qc_checks$rows_with_minimum
      n_dup <- qc$qc_checks$n_dup_numero + qc$qc_checks$n_dup_barcode
      
      msg <- sprintf("%s valid samples", scales::comma(n_valid))
      if (n_dup > 0) msg <- paste0(msg, sprintf(" | ⚠ %d dup", n_dup))
      
      latest <- max(df$date_sample, na.rm = TRUE)
      latest_str <- if (is.finite(latest)) format(latest, "%d %b %Y") else "—"
      
      sprintf("%s | Latest: %s", msg, latest_str)
    }
  })
  
  output$latest_arrival_date <- renderText({
    df <- clean_data()
    if (is.null(df) || !nrow(df)) return("")
    
    latest_arrival <- max(df$date_rec_cpltha, na.rm = TRUE)
    if (is.finite(latest_arrival)) {
      sprintf("Latest arrival: %s", format(latest_arrival, "%d %b %Y"))
    } else {
      ""
    }
  })
  
  # === DEBUG ===============================================================
  output$debug_config <- renderPrint({
    cfg()
  })
  
  output$debug_data_info <- renderPrint({
    df <- clean_data()
    qc <- qc_results()
    if (is.null(df) && is.null(qc)) return("No data loaded")
    
    list(
      qc_summary = if (!is.null(qc)) qc$qc_checks else "No QC run",
      clean_data_info = if (!is.null(df)) list(
        rows = nrow(df),
        columns = ncol(df),
        column_names = names(df),
        study_breakdown = table(df$study, useNA = "ifany"),
        date_range = range(df$date_sample, na.rm = TRUE),
        transport_stats = summary(df$transport_field_cpltha),
        conservation_stats = summary(df$conservation_days)
      ) else "No clean data"
    )
  })
}

# --- RUN --------------------------------------------------------------------
shinyApp(ui, server)
