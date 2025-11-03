# helpers_controls.R
# =====================================================================
# PRODUCTION-READY CONTROLS QC WITH LEVEY-JENNINGS
# Supports: PCR (CP/CN), ELISA PE/VSG (PC/NC/CC), iELISA (LiTat 1.3/1.5)
# Features: Multi-marker faceting, robust parsing, comprehensive QC
# Version: 2.0 - 2025-11-03
# =====================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringi)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Normalize text for fuzzy matching
norm_txt <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9\\. ]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Smart number parser (handles European format with comma as decimal)
parse_num_smart <- function(x) {
  x0 <- trimws(as.character(x))
  x0 <- gsub("\u00A0", " ", x0, fixed = TRUE)  # Non-breaking space
  x0 <- gsub("%", "", x0, fixed = TRUE)
  
  # Try standard parsing first
  out <- suppressWarnings(as.numeric(x0))
  
  # Fix European format for NA values
  need <- is.na(out) & nzchar(x0)
  if (any(need)) {
    y <- x0[need]
    # If both . and , present, . is thousands separator
    both <- grepl("\\.", y) & grepl(",", y)
    y[both] <- gsub("\\.", "", y[both])
    # Replace comma with dot
    y <- gsub(",", ".", y, fixed = TRUE)
    out[need] <- suppressWarnings(as.numeric(y))
  }
  
  out
}

#' Parse YYMMDD filename date with optional letter suffix
parse_date_from_fname <- function(fname) {
  m <- regexpr("^([0-9]{6})([a-z])?", basename(fname), perl = TRUE, ignore.case = TRUE)
  if (m[1] == -1) return(tibble(run_date = as.Date(NA), run_id = NA_character_))
  
  raw <- regmatches(basename(fname), m)
  yymmdd <- sub("^([0-9]{6}).*$", "\\1", raw)
  suffix <- sub("^[0-9]{6}([a-z])$", "\\1", raw, perl = TRUE)
  
  yy <- as.integer(substr(yymmdd, 1, 2))
  mm <- as.integer(substr(yymmdd, 3, 4))
  dd <- as.integer(substr(yymmdd, 5, 6))
  
  year <- ifelse(yy >= 90, 1900 + yy, 2000 + yy)
  
  tibble(
    run_date = as.Date(sprintf("%04d-%02d-%02d", year, mm, dd)),
    run_id = ifelse(nchar(suffix) == 1 & grepl("^[a-z]$", suffix, ignore.case = TRUE), 
                    suffix, NA_character_)
  )
}

#' Safe NULL coalesce
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

#' Find first matching column name
first_match <- function(nms, patterns) {
  hits <- unique(unlist(lapply(patterns, function(p) {
    nms[grepl(p, nms, ignore.case = TRUE)]
  })))
  if (length(hits)) hits[1] else NA_character_
}

# ============================================================================
# PCR CONTROLS EXTRACTION
# ============================================================================

#' Detect PCR control type from sample ID
#' @param x Character vector of sample IDs
#' @return Factor with levels CP (positive control), CN (negative control)
detect_pcr_ctrl <- function(x) {
  x2 <- toupper(trimws(as.character(x)))
  
  out <- dplyr::case_when(
    grepl("^(CP|PC|POS|POSITIVE)(\\b|\\d)", x2) ~ "CP",
    grepl("POS(\\s)?CTRL|POSITIVE\\s*CONTROL", x2) ~ "CP",
    grepl("^(CN|NC|NEG)(\\b|\\d)", x2) ~ "CN",
    grepl("NTC|NO\\s*TEMPLATE", x2) ~ "CN",
    TRUE ~ NA_character_
  )
  
  factor(out, levels = c("CP", "CN"))
}

#' Extract PCR controls from loaded PCR data
#' @param pcr_data PCR results data frame with lab_id, Cq columns
#' @return Data frame with control information including all markers
#' @export
extract_pcr_controls <- function(pcr_data) {
  if (is.null(pcr_data) || !nrow(pcr_data)) {
    return(tibble::tibble())
  }
  
  # Ensure required columns exist
  if (!"lab_id" %in% names(pcr_data)) {
    warning("PCR data missing lab_id column")
    return(tibble::tibble())
  }
  
  pcr_data %>%
    dplyr::mutate(
      control = detect_pcr_ctrl(lab_id),
      PCR_call_up = toupper(trimws(as.character(PCR_call %||% "")))
    ) %>%
    dplyr::filter(!is.na(control)) %>%
    dplyr::mutate(
      # Ensure numeric Cq values
      Cq_177T = suppressWarnings(as.numeric(Cq_177T)),
      Cq_18S2 = suppressWarnings(as.numeric(Cq_18S2)),
      Cq_RNAseP = suppressWarnings(as.numeric(Cq_RNAseP)),
      
      # Minimum Cq of parasite targets
      min_Cq_tryp = pmin(Cq_177T, Cq_18S2, na.rm = TRUE),
      min_Cq_tryp = ifelse(is.infinite(min_Cq_tryp), NA_real_, min_Cq_tryp),
      
      # Extract file info
      file = basename(source_file %||% "unknown")
    ) %>%
    # Add run date from filename
    dplyr::bind_cols(parse_date_from_fname(.$file[1])) %>%
    dplyr::select(
      file, run_date, run_id, control, lab_id, PCR_call_up,
      Cq_177T, Cq_18S2, Cq_RNAseP, min_Cq_tryp
    )
}

#' Create long-format PCR control data for multi-marker plotting
#' @param pcr_ctrl_raw Raw control data from extract_pcr_controls
#' @return Long-format tibble with one row per control-marker combination
#' @export
pcr_controls_long <- function(pcr_ctrl_raw) {
  if (is.null(pcr_ctrl_raw) || !nrow(pcr_ctrl_raw)) {
    return(tibble::tibble())
  }
  
  pcr_ctrl_raw %>%
    dplyr::filter(control == "CP") %>%
    dplyr::select(file, run_date, run_id, Cq_177T, Cq_18S2, Cq_RNAseP) %>%
    tidyr::pivot_longer(
      cols = starts_with("Cq_"),
      names_to = "marker",
      values_to = "Cq"
    ) %>%
    dplyr::mutate(
      marker = dplyr::recode(marker,
                             "Cq_177T" = "177T",
                             "Cq_18S2" = "18S2",
                             "Cq_RNAseP" = "RNAseP")
    ) %>%
    dplyr::arrange(run_date, run_id, file) %>%
    dplyr::group_by(marker) %>%
    dplyr::mutate(run_order = dplyr::row_number()) %>%
    dplyr::ungroup()
}

#' Summarize PCR controls per run with QC flags
#' @param pcr_ctrl_raw Raw control data
#' @param cp_max_cq Maximum acceptable Cq for positive control (default 35)
#' @return Summary with QC flags
#' @export
qc_pcr_controls_summary <- function(pcr_ctrl_raw, cp_max_cq = 35) {
  if (is.null(pcr_ctrl_raw) || !nrow(pcr_ctrl_raw)) {
    return(tibble::tibble())
  }
  
  summary <- pcr_ctrl_raw %>%
    dplyr::group_by(file, run_date, run_id, control) %>%
    dplyr::summarise(
      n_rows = dplyr::n(),
      pos_calls = sum(PCR_call_up %in% c("POS", "POSITIVE", "DETECTED", "TRUE", "YES"), 
                      na.rm = TRUE),
      any_pos = pos_calls > 0,
      minCq_mean = mean(min_Cq_tryp, na.rm = TRUE),
      minCq_min = suppressWarnings(min(min_Cq_tryp, na.rm = TRUE)),
      Cq_177T_mean = mean(Cq_177T, na.rm = TRUE),
      Cq_18S2_mean = mean(Cq_18S2, na.rm = TRUE),
      Cq_RNAseP_mean = mean(Cq_RNAseP, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      minCq_min = ifelse(is.infinite(minCq_min), NA_real_, minCq_min)
    )
  
  # Add QC flags
  summary %>%
    dplyr::mutate(
      flag = dplyr::case_when(
        control == "CP" & (is.na(any_pos) | !any_pos) ~ "CP_NOT_DETECTED",
        control == "CP" & is.finite(minCq_min) & minCq_min > cp_max_cq ~ "CP_CQ_TOO_HIGH",
        control == "CN" & any_pos ~ "CN_CONTAMINATION",
        TRUE ~ "OK"
      )
    )
}

# ============================================================================
# ELISA CONTROLS EXTRACTION (PE + VSG)
# ============================================================================

#' Map control codes to standard names
ctrl_map_elisa <- function(x) {
  x2 <- toupper(trimws(as.character(x)))
  dplyr::case_when(
    x2 %in% c("CP", "PC", "POS", "POSITIVE", "POS CTRL", "POSITIVE CONTROL", "PC3") ~ "PC",
    x2 %in% c("CN", "NC", "NEG", "NEGATIVE", "NEGATIVE CONTROL") ~ "NC",
    x2 %in% c("CC", "CONJ", "CONJUGATE CONTROL", "CONJ CTRL") ~ "CC",
    TRUE ~ NA_character_
  )
}

#' Read ELISA controls from a file
#' @param path Path to Excel file
#' @param assay_label "ELISA_PE" or "ELISA_VSG"
#' @return Data frame with control data
read_elisa_controls <- function(path, assay_label) {
  sheets <- readxl::excel_sheets(path)
  ctrl_sheet <- sheets[tolower(sheets) == "controls"]
  if (!length(ctrl_sheet)) return(tibble::tibble())
  
  raw <- readxl::read_excel(path, sheet = ctrl_sheet[1], .name_repair = "minimal") %>%
    janitor::clean_names()
  cols <- names(raw)
  
  # Find columns
  col_plate <- first_match(cols, c("^elisa$", "^plate$", "^plate_?id$"))
  col_sample <- first_match(cols, c("^sample$", "^name$", "^controle?$"))
  col_code <- first_match(cols, c("^sample_?code$", "^code$", "^type$"))
  col_dod <- first_match(cols, c("^d_?od$", "^delta_?od$", "od$"))
  col_pp <- first_match(cols, c("^pp_?%$", "^pp_percent$", "^pp$", "^percent_?pos(itive)?$"))
  col_result <- first_match(cols, c("^result$"))
  
  # Need at least one identifier
  if (is.na(col_code) && is.na(col_sample)) return(tibble::tibble())
  
  # Build control label
  control_label <- if (!is.na(col_code)) raw[[col_code]] else raw[[col_sample]]
  
  d_od <- if (!is.na(col_dod)) parse_num_smart(raw[[col_dod]]) else NA_real_
  pp <- if (!is.na(col_pp)) parse_num_smart(raw[[col_pp]]) else NA_real_
  
  # If PP looks like 0–1, rescale to 0–100
  if (any(!is.na(pp))) {
    rng <- range(pp, na.rm = TRUE)
    if (is.finite(rng[2]) && rng[2] <= 1.5 && rng[1] >= -1) pp <- pp * 100
  }
  
  tibble::tibble(
    file = basename(path),
    assay_label = assay_label,
    plate = if (!is.na(col_plate)) {
      suppressWarnings(as.integer(raw[[col_plate]]))
    } else 1L,
    control = ctrl_map_elisa(control_label),
    d_od = d_od,
    pp_percent = pp,
    result_raw = if (!is.na(col_result)) as.character(raw[[col_result]]) else NA_character_
  ) %>%
    dplyr::filter(!is.na(control)) %>%
    dplyr::bind_cols(parse_date_from_fname(path))
}

#' Extract ELISA controls from loaded data or directories
#' @param elisa_pe_data ELISA PE data (optional)
#' @param elisa_vsg_data ELISA VSG data (optional)
#' @param pe_dir Directory with PE files
#' @param vsg_dir Directory with VSG files
#' @return Combined control data
#' @export
extract_elisa_controls <- function(elisa_pe_data = NULL, elisa_vsg_data = NULL,
                                   pe_dir = NULL, vsg_dir = NULL) {
  
  read_safe <- purrr::possibly(read_elisa_controls, otherwise = tibble::tibble())
  
  pe_ctrls <- if (!is.null(pe_dir) && dir.exists(pe_dir)) {
    list.files(pe_dir, "\\.xlsx?$", full.names = TRUE) %>%
      purrr::map_dfr(~ read_safe(.x, "ELISA_PE"))
  } else {
    tibble::tibble()
  }
  
  vsg_ctrls <- if (!is.null(vsg_dir) && dir.exists(vsg_dir)) {
    list.files(vsg_dir, "\\.xlsx?$", full.names = TRUE) %>%
      purrr::map_dfr(~ read_safe(.x, "ELISA_VSG"))
  } else {
    tibble::tibble()
  }
  
  dplyr::bind_rows(pe_ctrls, vsg_ctrls)
}

#' QC ELISA controls with configurable thresholds
#' @param elisa_ctrl Control data
#' @param qc_rules List with PC_min_dod, PC_min_pp, NEG_max_dod, NEG_max_pp
#' @return Data with pass/fail flags
#' @export
qc_elisa_controls <- function(elisa_ctrl, qc_rules = NULL) {
  if (is.null(qc_rules)) {
    qc_rules <- list(
      PC_min_dod = 0.30,
      PC_min_pp = 50,
      NEG_max_dod = 0.05,
      NEG_max_pp = 5
    )
  }
  
  elisa_ctrl %>%
    dplyr::mutate(
      pass_dod = dplyr::case_when(
        control == "PC" ~ !is.na(d_od) & d_od >= qc_rules$PC_min_dod,
        control %in% c("NC", "CC") ~ !is.na(d_od) & d_od <= qc_rules$NEG_max_dod,
        TRUE ~ NA
      ),
      pass_pp = dplyr::case_when(
        control == "PC" ~ !is.na(pp_percent) & pp_percent >= qc_rules$PC_min_pp,
        control %in% c("NC", "CC") ~ !is.na(pp_percent) & pp_percent <= qc_rules$NEG_max_pp,
        TRUE ~ NA
      ),
      pass = pass_dod & pass_pp
    )
}

# ============================================================================
# iELISA CONTROLS EXTRACTION (LiTat 1.3/1.5)
# ============================================================================

#' Read sheet as multi-format matrices for flexible parsing
.read_sheet <- function(path, sheet) {
  df <- tryCatch(
    openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE,
                        na.strings = c("", "NA"), skipEmptyRows = FALSE, detectDates = FALSE),
    error = function(e) {
      readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
    }
  )
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (ncol(df) == 0) df <- data.frame(V1 = rep(NA, nrow(df)))
  
  M_chr <- apply(df, 2, as.character)
  if (is.null(dim(M_chr))) M_chr <- matrix(M_chr, ncol = 1)
  
  M_num0 <- suppressWarnings(apply(df, 2, as.numeric))
  if (is.null(dim(M_num0))) M_num0 <- matrix(M_num0, ncol = 1)
  
  # Apply smart parsing to failed numeric conversions
  M_num <- M_num0
  mask <- is.na(M_num0) & !is.na(M_chr)
  if (any(mask)) M_num[mask] <- parse_num_smart(M_chr[mask])
  
  M_norm <- apply(M_chr, 2, norm_txt)
  if (is.null(dim(M_norm))) M_norm <- matrix(M_norm, ncol = 1)
  
  list(c = M_chr, n = M_num, norm = M_norm)
}

#' Find first numeric cell within N rows below a text label
find_numeric_below <- function(M_norm, M_num, patterns, max_down = 10) {
  dims <- dim(M_norm)
  if (is.null(dims)) return(NA_real_)
  
  # Create masks for each pattern
  masks <- lapply(patterns, function(p) {
    m <- grepl(p, M_norm, ignore.case = TRUE)
    dim(m) <- dims
    m
  })
  
  # Combine all masks
  ms <- Reduce(`|`, masks)
  if (!any(ms, na.rm = TRUE)) return(NA_real_)
  
  # Get first matching position
  idx <- which(ms, arr.ind = TRUE)[1, , drop = TRUE]
  r <- idx[1]
  c <- idx[2]
  
  # Look below for numeric value
  r2 <- min(nrow(M_num), r + max_down)
  vals <- suppressWarnings(as.numeric(M_num[seq.int(min(nrow(M_num), r + 1), r2), c]))
  vals <- vals[is.finite(vals)]
  
  if (length(vals)) vals[1] else NA_real_
}

#' Resolve 450-600nm sheet name (handles variations)
resolve_450_sheet <- function(path) {
  shs <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  if (!length(shs)) return(NA_character_)
  
  s_norm <- norm_txt(shs)
  targets <- c("450 600 nm", "450-600 nm", "450 600nm", "450-600nm")
  
  hit <- which(s_norm %in% targets)
  if (length(hit)) return(shs[hit[1]])
  
  # Fuzzy match
  hit2 <- which(grepl("450", s_norm) & grepl("600", s_norm))
  if (length(hit2)) return(shs[hit2[1]])
  
  NA_character_
}

#' Plausibility guards for OD and PI values
cap_to_od <- function(v) {
  v[!is.na(v) & (v < 0 | v > 6)] <- NA_real_
  v
}

cap_to_pi <- function(v) {
  v[!is.na(v) & (v < -150 | v > 150)] <- NA_real_
  v
}

#' Read iELISA controls from file (extracts summary metrics)
#' @param path Path to iELISA Excel file
#' @return Tibble with control summaries for LiTat 1.3 and 1.5
read_ielisa_controls_450 <- function(path) {
  sh <- resolve_450_sheet(path)
  if (is.na(sh)) return(tibble::tibble())
  
  M <- .read_sheet(path, sh)
  
  # Patterns for French/English labels
  pat_L13_neg <- c("moyenne controle neg.*1\\.3", "mean .*neg.*1\\.3")
  pat_L13_pos <- c("moyenne controle pos.*1\\.3", "mean .*pos.*1\\.3")
  pat_L13_pi <- c("^pi.*(cp|c).*1\\.3", "pi .*litat.*1\\.3")
  pat_L15_neg <- c("moyenne controle neg.*1\\.5", "mean .*neg.*1\\.5")
  pat_L15_pos <- c("moyenne controle pos.*1\\.5", "mean .*pos.*1\\.5")
  pat_L15_pi <- c("^pi.*(cp|c).*1\\.5", "pi .*litat.*1\\.5")
  
  tibble::tibble(
    file = basename(path),
    sheet = sh,
    iL13_neg_mean = cap_to_od(find_numeric_below(M$norm, M$n, pat_L13_neg)),
    iL13_pos_mean = cap_to_od(find_numeric_below(M$norm, M$n, pat_L13_pos)),
    iL13_PI_CP = cap_to_pi(find_numeric_below(M$norm, M$n, pat_L13_pi)),
    iL15_neg_mean = cap_to_od(find_numeric_below(M$norm, M$n, pat_L15_neg)),
    iL15_pos_mean = cap_to_od(find_numeric_below(M$norm, M$n, pat_L15_pos)),
    iL15_PI_CP = cap_to_pi(find_numeric_below(M$norm, M$n, pat_L15_pi))
  ) %>%
    dplyr::bind_cols(parse_date_from_fname(path))
}

#' Extract iELISA controls from directory
#' @param ielisa_dir Directory with iELISA files
#' @return Control summaries
#' @export
extract_ielisa_controls <- function(ielisa_dir = NULL) {
  if (is.null(ielisa_dir) || !dir.exists(ielisa_dir)) {
    return(tibble::tibble())
  }
  
  list.files(ielisa_dir, "\\.xlsx?$", full.names = TRUE) %>%
    purrr::map_dfr(read_ielisa_controls_450)
}

#' QC iELISA controls
#' @param ielisa_ctrl Control data
#' @param min_pi Minimum % inhibition for positive control (default 60)
#' @return Data with QC flags
#' @export
qc_ielisa_controls <- function(ielisa_ctrl, min_pi = 60) {
  ielisa_ctrl %>%
    dplyr::mutate(
      pass_L13_negpos = !is.na(iL13_neg_mean) & !is.na(iL13_pos_mean) & 
        iL13_neg_mean > iL13_pos_mean,
      pass_L15_negpos = !is.na(iL15_neg_mean) & !is.na(iL15_pos_mean) & 
        iL15_neg_mean > iL15_pos_mean,
      pass_L13_PI = !is.na(iL13_PI_CP) & iL13_PI_CP >= min_pi,
      pass_L15_PI = !is.na(iL15_PI_CP) & iL15_PI_CP >= min_pi,
      pass_LiTat13 = pass_L13_negpos & pass_L13_PI,
      pass_LiTat15 = pass_L15_negpos & pass_L15_PI,
      pass_overall = pass_LiTat13 & pass_LiTat15
    )
}

# ============================================================================
# LEVEY-JENNINGS PLOTTING
# ============================================================================

#' Generic Levey-Jennings plot function
#' @param df Data frame with run_order and value columns
#' @param x Column name for x-axis (usually "run_order")
#' @param y Column name for y-axis (the measured value)
#' @param title Plot title
#' @param ylab Y-axis label
#' @return ggplot object
make_lj <- function(df, x, y, title, ylab) {
  if (is.null(df) || !nrow(df)) {
    return(ggplot() + theme_void() + 
             labs(title = paste(title, "(no data)")))
  }
  
  mu <- mean(df[[y]], na.rm = TRUE)
  sdv <- stats::sd(df[[y]], na.rm = TRUE)
  
  ggplot2::ggplot(df, ggplot2::aes_string(x = x, y = y)) +
    # Mean line
    ggplot2::geom_hline(yintercept = mu, linetype = 1, alpha = 0.6, color = "blue") +
    # ±1 SD
    ggplot2::geom_hline(yintercept = mu + 1*sdv, linetype = 2, alpha = 0.4, color = "orange") +
    ggplot2::geom_hline(yintercept = mu - 1*sdv, linetype = 2, alpha = 0.4, color = "orange") +
    # ±2 SD
    ggplot2::geom_hline(yintercept = mu + 2*sdv, linetype = 3, alpha = 0.3, color = "red") +
    ggplot2::geom_hline(yintercept = mu - 2*sdv, linetype = 3, alpha = 0.3, color = "red") +
    # Data points
    ggplot2::geom_point(na.rm = TRUE, size = 2) +
    ggplot2::geom_line(ggplot2::aes(group = 1), alpha = 0.6, na.rm = TRUE) +
    ggplot2::labs(title = title, x = "Run order", y = ylab) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = element_blank())
}

#' Plot PCR control Cq values (faceted by marker)
#' @param pcr_ctrl_raw PCR control data
#' @param cp_max_cq Maximum acceptable Cq for positive control
#' @return ggplot object with facets for each marker
#' @export
plot_pcr_controls <- function(pcr_ctrl_raw, cp_max_cq = 35) {
  if (is.null(pcr_ctrl_raw) || !nrow(pcr_ctrl_raw)) {
    return(ggplot() + theme_void() + labs(title = "No PCR control data"))
  }
  
  # Create long format
  pcr_long <- pcr_controls_long(pcr_ctrl_raw)
  
  if (!nrow(pcr_long)) {
    return(ggplot() + theme_void() + labs(title = "No PCR CP data for plotting"))
  }
  
  # Calculate stats per marker for reference lines
  mu_sd <- pcr_long %>%
    dplyr::group_by(marker) %>%
    dplyr::summarise(
      mu = mean(Cq, na.rm = TRUE),
      sd = sd(Cq, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Threshold lines per marker (different for RNAseP)
  thr_df <- tibble::tibble(
    marker = c("177T", "18S2", "RNAseP"),
    thr = c(cp_max_cq, cp_max_cq, 40)  # RNAseP has higher threshold
  )
  
  ggplot(pcr_long, aes(x = run_order, y = Cq)) +
    # Threshold line
    geom_hline(data = thr_df, aes(yintercept = thr), 
               linetype = 2, alpha = 0.6, color = "red") +
    # Mean ± SD lines
    geom_hline(data = mu_sd, aes(yintercept = mu), 
               linetype = 1, alpha = 0.6, color = "blue") +
    geom_hline(data = mu_sd, aes(yintercept = mu + sd), 
               linetype = 2, alpha = 0.4, color = "orange") +
    geom_hline(data = mu_sd, aes(yintercept = mu - sd), 
               linetype = 2, alpha = 0.4, color = "orange") +
    geom_hline(data = mu_sd, aes(yintercept = mu + 2*sd), 
               linetype = 3, alpha = 0.3, color = "darkred") +
    geom_hline(data = mu_sd, aes(yintercept = mu - 2*sd), 
               linetype = 3, alpha = 0.3, color = "darkred") +
    # Data
    geom_point(size = 2, na.rm = TRUE) +
    geom_line(aes(group = 1), alpha = 0.6, na.rm = TRUE) +
    # Facet by marker
    facet_wrap(~ marker, scales = "free_y", ncol = 1) +
    labs(
      title = "Levey-Jennings: PCR Positive Control (CP) per Marker",
      subtitle = "Dashed red = threshold | Solid blue = mean | Orange/Red = ±1/2 SD",
      x = "Run order",
      y = "Cq value"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

#' Plot ELISA controls
#' @param elisa_ctrl_qc ELISA control data with QC flags
#' @param qc_rules QC threshold rules
#' @return ggplot object
#' @export
plot_elisa_controls <- function(elisa_ctrl_qc, qc_rules = NULL) {
  if (is.null(elisa_ctrl_qc) || !nrow(elisa_ctrl_qc)) {
    return(ggplot() + theme_void() + labs(title = "No ELISA control data"))
  }
  
  if (is.null(qc_rules)) {
    qc_rules <- list(PC_min_pp = 50, NEG_max_pp = 5)
  }
  
  # Add run order within each assay-control combination
  elisa_ordered <- elisa_ctrl_qc %>%
    dplyr::arrange(run_date, run_id, file) %>%
    dplyr::group_by(assay_label, control) %>%
    dplyr::mutate(run_order = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  # Calculate stats for reference lines
  mu_sd <- elisa_ordered %>%
    dplyr::group_by(assay_label, control) %>%
    dplyr::summarise(
      mu = mean(pp_percent, na.rm = TRUE),
      sd = sd(pp_percent, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(elisa_ordered, aes(x = run_order, y = pp_percent, color = control, group = control)) +
    geom_hline(data = mu_sd, aes(yintercept = mu, color = control), 
               linetype = 1, alpha = 0.6) +
    geom_hline(yintercept = qc_rules$PC_min_pp, linetype = 2, alpha = 0.5, color = "red") +
    geom_hline(yintercept = qc_rules$NEG_max_pp, linetype = 3, alpha = 0.5, color = "red") +
    geom_point(size = 2, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    facet_wrap(~ assay_label, scales = "free", ncol = 1) +
    scale_color_manual(values = c(PC = "#27AE60", NC = "#E74C3C", CC = "#3498DB")) +
    labs(
      title = "Levey-Jennings: ELISA Controls (PE & VSG)",
      subtitle = "Positive Control (PC) should be high, Negative/Conjugate low",
      x = "Run order",
      y = "PP %",
      color = "Control"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom")
}

#' Plot iELISA controls (both antigens)
#' @param ielisa_ctrl_qc iELISA control data with QC flags
#' @param min_pi Minimum PI threshold
#' @return ggplot object
#' @export
plot_ielisa_controls <- function(ielisa_ctrl_qc, min_pi = 60) {
  if (is.null(ielisa_ctrl_qc) || !nrow(ielisa_ctrl_qc)) {
    return(ggplot() + theme_void() + labs(title = "No iELISA control data"))
  }
  
  # Order by date
  file_order <- ielisa_ctrl_qc %>%
    dplyr::mutate(.date = suppressWarnings(as.integer(substr(file, 1, 6)))) %>%
    dplyr::arrange(.date) %>%
    dplyr::pull(file) %>%
    unique()
  
  # Long format for both antigens
  pi_long <- ielisa_ctrl_qc %>%
    dplyr::mutate(file = factor(file, levels = file_order)) %>%
    tidyr::pivot_longer(
      cols = c(iL13_PI_CP, iL15_PI_CP),
      names_to = "antigen",
      values_to = "PI"
    ) %>%
    dplyr::mutate(
      antigen = dplyr::if_else(grepl("iL13", antigen), "LiTat 1.3", "LiTat 1.5")
    ) %>%
    dplyr::arrange(run_date, run_id, file) %>%
    dplyr::group_by(antigen) %>%
    dplyr::mutate(run_order = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  # Calculate stats
  mu_sd <- pi_long %>%
    dplyr::group_by(antigen) %>%
    dplyr::summarise(
      mu = mean(PI, na.rm = TRUE),
      sd = sd(PI, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(pi_long, aes(x = run_order, y = PI, color = antigen, group = antigen)) +
    # Threshold
    geom_hline(yintercept = min_pi, linetype = 2, alpha = 0.6, color = "red") +
    # Mean ± SD per antigen
    geom_hline(data = mu_sd, aes(yintercept = mu, color = antigen), 
               linetype = 1, alpha = 0.6) +
    # Data
    geom_point(size = 2, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    facet_wrap(~ antigen, scales = "free_y", ncol = 1) +
    scale_color_manual(values = c("LiTat 1.3" = "#9B59B6", "LiTat 1.5" = "#E67E22")) +
    labs(
      title = "Levey-Jennings: iELISA Positive Control (% Inhibition)",
      subtitle = sprintf("Red dashed line = %d%% minimum threshold", min_pi),
      x = "Run order",
      y = "% Inhibition (CP)"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom")
}
