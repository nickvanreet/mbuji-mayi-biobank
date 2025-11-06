# utils_parse.R
# Robust parsing utilities for biobank data
# ============================================================================

#' Parse dates from various formats (Excel serials, ISO, DMY, MDY)
#' @param x Vector of date values (character, numeric, or Date)
#' @return Date vector with NA for unparseable values
#' @export
parse_any_date <- function(x) {
  if (is.null(x) || length(x) == 0) return(as.Date(character()))
  
  # Convert to character and clean
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "na", "NaN", "NULL")] <- NA_character_
  
  # Initialize output
  out <- as.Date(rep(NA_integer_, length(x_chr)), origin = "1970-01-01")
  
  tryCatch({
    # Handle numeric Excel serials (including decimals)
    is_num <- grepl("^\\d+(?:[.,]\\d+)?$", x_chr)
    if (any(is_num, na.rm = TRUE)) {
      ser <- sub(",", ".", x_chr[is_num], fixed = TRUE)
      suppressWarnings({
        out[is_num] <- as.Date(as.numeric(ser), origin = "1899-12-30")
      })
    }
    
    # Handle text dates
    idx <- !is_num & !is.na(x_chr)
    if (any(idx)) {
      # Strip time components
      tmp <- sub("[ T].*$", "", x_chr[idx])
      # Normalize separators
      tmp <- gsub("[.\\-]", "/", tmp)
      
      # Try DMY first (European format)
      p <- suppressWarnings(lubridate::dmy(tmp))
      # Fallback to YMD
      miss <- is.na(p)
      if (any(miss)) p[miss] <- suppressWarnings(lubridate::ymd(tmp[miss]))
      # Last resort: MDY
      miss <- is.na(p)
      if (any(miss)) p[miss] <- suppressWarnings(lubridate::mdy(tmp[miss]))
      
      out[idx] <- p
    }
  }, error = function(e) {
    warning("Date parsing error: ", e$message, call. = FALSE)
  })
  
  out
}

#' Parse decimal numbers from various formats
#' @param x Vector of numeric values (character or numeric)
#' @return Numeric vector with NA for unparseable values
#' @export
parse_decimal_number <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric())
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "na", "NaN", "NULL")] <- NA_character_
  
  parsed <- rep(NA_real_, length(x_chr))
  
  tryCatch({
    # Try standard parsing first
    parsed <- readr::parse_number(x_chr)
    
    # Handle European format (comma as decimal)
    miss <- is.na(parsed) & !is.na(x_chr)
    if (any(miss)) {
      parsed[miss] <- readr::parse_number(
        x_chr[miss],
        locale = readr::locale(decimal_mark = ",", grouping_mark = " ")
      )
    }
    
    # Last resort: manual cleaning
    miss <- is.na(parsed) & !is.na(x_chr)
    if (any(miss)) {
      cleaned <- gsub("[^0-9,.-]", "", x_chr[miss])
      cleaned <- gsub(",", ".", cleaned, fixed = TRUE)
      parsed[miss] <- suppressWarnings(as.numeric(cleaned))
    }
  }, error = function(e) {
    warning("Number parsing error: ", e$message, call. = FALSE)
  })
  
  parsed
}

#' Standardize study codes (DA/DP)
#' @param x Vector of study identifiers
#' @return Factor with levels DA, DP, or NA
#' @export
parse_study_code <- function(x) {
  if (is.null(x) || length(x) == 0) return(factor(levels = c("DA", "DP")))
  
  x_clean <- toupper(trimws(as.character(x)))
  
  result <- dplyr::case_when(
    x_clean == "DA" ~ "DA",
    x_clean == "DP" ~ "DP",
    grepl("ACTIF", x_clean) ~ "DA",
    grepl("PASSIF|PASSIVE", x_clean) ~ "DP",
    grepl("^D\\.?A\\.?$", x_clean) ~ "DA",
    grepl("^D\\.?P\\.?$", x_clean) ~ "DP",
    TRUE ~ NA_character_
  )
  
  factor(result, levels = c("DA", "DP"))
}

#' Standardize sex codes (M/F)
#' @param x Vector of sex identifiers
#' @return Factor with levels M, F, or NA
#' @export
parse_sex_code <- function(x) {
  if (is.null(x) || length(x) == 0) return(factor(levels = c("M", "F")))
  
  x_clean <- toupper(trimws(as.character(x)))
  
  result <- dplyr::case_when(
    x_clean %in% c("M", "MALE", "H", "HOMME") ~ "M",
    x_clean %in% c("F", "FEMALE", "FEMME") ~ "F",
    TRUE ~ NA_character_
  )
  
  factor(result, levels = c("M", "F"))
}

#' Clean temperature storage codes
#' @param x Vector of temperature codes
#' @return Factor with levels Ambiante, Frigo, Congelateur, or NA
#' @export
parse_temp_code <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(factor(levels = c("Ambiante", "Frigo", "Congelateur")))
  }
  
  tryCatch({
    lab <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
    lab <- toupper(trimws(lab))
    
    result <- dplyr::case_when(
      lab == "" ~ NA_character_,
      grepl("^A", lab) ~ "Ambiante",
      grepl("^F", lab) ~ "Frigo",
      grepl("^C|CONG|FREEZ", lab) ~ "Congelateur",
      TRUE ~ NA_character_
    )
    
    factor(result, levels = c("Ambiante", "Frigo", "Congelateur"))
  }, error = function(e) {
    warning("Temperature code parsing error: ", e$message, call. = FALSE)
    factor(rep(NA_character_, length(x)), levels = c("Ambiante", "Frigo", "Congelateur"))
  })
}

#' Parse age values (handle birth years)
#' @param x Vector of age values
#' @return Numeric vector of ages (0-110)
#' @export
parse_age <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric())
  
  age_num <- suppressWarnings(as.numeric(x))
  
  dplyr::case_when(
    is.na(age_num) ~ NA_real_,
    age_num > 1900 ~ as.numeric(lubridate::year(Sys.Date())) - age_num,
    dplyr::between(age_num, 0, 110) ~ age_num,
    TRUE ~ NA_real_
  )
}

#' Calculate days between dates with validation
#' @param to End date
#' @param from Start date
#' @param max_ok Maximum reasonable number of days
#' @return Numeric vector of days (negative or excessive values become NA)
#' @export
safe_days_between <- function(to, from, max_ok = 90) {
  if (is.null(to) || is.null(from)) return(numeric())
  
  tryCatch({
    d <- as.numeric(difftime(to, from, units = "days"))
    d[!is.finite(d) | d < 0 | d > max_ok] <- NA_real_
    d
  }, error = function(e) {
    warning("Days calculation error: ", e$message, call. = FALSE)
    rep(NA_real_, length(to))
  })
}

#' Normalize text for fuzzy matching
#' @param x Character vector
#' @return Normalized character vector (lowercase, no accents, trimmed)
#' @export
normalize_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(character())
  
  x_norm <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  tolower(trimws(x_norm))
}

#' Detect positive lab results from text
#' @param x Vector of result calls
#' @return Logical vector
#' @export
is_positive_result <- function(x) {
  if (is.null(x) || length(x) == 0) return(logical())
  
  x_clean <- toupper(trimws(as.character(x)))
  grepl("POS|POSITIF|DETECT", x_clean)
}

#' Parse yes/no/uncertain responses
#' @param x Vector of response text
#' @return Factor with levels Oui, Non, Incertain
#' @export
parse_yes_no_uncertain <- function(x) {
  if (is.null(x) || length(x) == 0) return(factor(levels = c("Oui", "Non", "Incertain")))
  
  x_clean <- toupper(trimws(as.character(x)))
  x_clean[x_clean %in% c("", "NA", "N/A")] <- NA_character_
  
  result <- dplyr::case_when(
    x_clean %in% c("OUI", "YES", "Y", "O", "1", "TRUE") ~ "Oui",
    x_clean %in% c("NON", "NO", "N", "0", "FALSE") ~ "Non",
    x_clean %in% c("INCERTAIN", "UNCERTAIN", "UNKNOWN", "?", "UNKN") ~ "Incertain",
    TRUE ~ NA_character_
  )
  
  factor(result, levels = c("Oui", "Non", "Incertain"))
}

#' Calculate conservation time with fallback dates
#' Priority: date_treatment → date_received → date_inrb
#' @param date_treatment Lab treatment date (primary)
#' @param date_sample Sample collection date
#' @param date_received Fallback: reception date
#' @param date_inrb Fallback: INRB arrival date
#' @param max_ok Maximum reasonable days
#' @return Numeric vector of conservation days
#' @export
calc_conservation_days <- function(date_treatment, date_sample, 
                                    date_received = NULL, date_inrb = NULL, 
                                    max_ok = 365) {
  if (is.null(date_treatment) || is.null(date_sample)) {
    return(rep(NA_real_, max(length(date_treatment), length(date_sample))))
  }
  
  # Primary calculation: treatment - collection
  days <- safe_days_between(date_treatment, date_sample, max_ok)
  
  # Fallback 1: Use reception date if treatment missing
  if (!is.null(date_received)) {
    missing <- is.na(days) & !is.na(date_received)
    if (any(missing)) {
      days[missing] <- safe_days_between(date_received[missing], 
                                          date_sample[missing], max_ok)
    }
  }
  
  # Fallback 2: Use INRB arrival if still missing
  if (!is.null(date_inrb)) {
    missing <- is.na(days) & !is.na(date_inrb)
    if (any(missing)) {
      days[missing] <- safe_days_between(date_inrb[missing], 
                                          date_sample[missing], max_ok)
    }
  }
  
  days
}

#' Detect if sample was shipped to INRB
#' @param date_env_inrb Date sent to INRB
#' @param status_field Optional status text field
#' @return Logical vector
#' @export
parse_shipped_to_inrb <- function(date_env_inrb, status_field = NULL) {
  if (is.null(date_env_inrb)) {
    return(rep(NA, length(status_field) %||% 0))
  }
  
  # Primary: check if shipment date exists
  shipped <- !is.na(date_env_inrb)
  
  # Secondary: check status text if provided
  if (!is.null(status_field)) {
    status_clean <- toupper(trimws(as.character(status_field)))
    status_indicates_shipped <- grepl("INRB|ENV.*INRB|SHIP.*INRB|SENT", status_clean)
    shipped <- shipped | status_indicates_shipped
  }
  
  shipped
}
