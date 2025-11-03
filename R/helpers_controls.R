# helpers_controls.R
# =====================================================================
# Control extraction and QC - Based on working user script
# =====================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================================
# PCR CONTROLS (CP/CN detection from QuantStudio files)
# ============================================================================

#' Detect PCR control type from sample ID
#' @param x Character vector of sample IDs
#' @return Factor with levels CP, CN
detect_pcr_ctrl <- function(x){
  x2 <- toupper(trimws(as.character(x)))
  out <- ifelse(
    grepl("^(CP|PC|POS|POSITIVE)(\\b|\\d)", x2) | grepl("POS(\\s)?CTRL|POSITIVE\\s*CONTROL", x2),
    "CP",
    ifelse(
      grepl("^(CN|NC|NEG)(\\b|\\d)", x2) | grepl("NTC|NO\\s*TEMPLATE", x2),
      "CN",
      NA_character_
    )
  )
  factor(out, levels = c("CP","CN"))
}

#' Extract PCR controls from loaded PCR data
#' @param pcr_data PCR results data frame with lab_id, Cq columns
#' @return Data frame with control information
extract_pcr_controls <- function(pcr_data) {
  if (is.null(pcr_data) || !nrow(pcr_data)) {
    return(tibble::tibble())
  }
  
  pcr_data %>%
    dplyr::mutate(
      control = detect_pcr_ctrl(lab_id),
      PCR_call_up = toupper(trimws(as.character(PCR_call)))
    ) %>%
    dplyr::filter(!is.na(control)) %>%
    dplyr::mutate(
      # Minimum Cq of parasite targets (177T or 18S2)
      min_Cq_tryp = pmin(Cq_177T, Cq_18S2, na.rm = TRUE),
      min_Cq_tryp = ifelse(is.infinite(min_Cq_tryp), NA_real_, min_Cq_tryp),
      # File date if available
      file = basename(source_file %||% "unknown")
    ) %>%
    dplyr::select(
      file, control, lab_id, PCR_call_up,
      Cq_177T, Cq_18S2, Cq_RNAseP, min_Cq_tryp
    )
}

#' Summarize PCR controls per run
#' @param pcr_ctrl_raw Raw control data from extract_pcr_controls
#' @return Summary with QC flags
qc_pcr_controls_summary <- function(pcr_ctrl_raw, cp_max_cq = 35) {
  if (is.null(pcr_ctrl_raw) || !nrow(pcr_ctrl_raw)) {
    return(tibble::tibble())
  }
  
  summary <- pcr_ctrl_raw %>%
    dplyr::group_by(file, control) %>%
    dplyr::summarise(
      n_rows     = dplyr::n(),
      pos_calls  = sum(PCR_call_up %in% c("POS","POSITIVE","DETECTED","TRUE","YES"), na.rm = TRUE),
      any_pos    = pos_calls > 0,
      minCq_mean = mean(min_Cq_tryp, na.rm = TRUE),
      minCq_min  = suppressWarnings(min(min_Cq_tryp, na.rm = TRUE)),
      rnp_mean   = mean(Cq_RNAseP, na.rm = TRUE),
      .groups    = "drop"
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
# ELISA CONTROLS (PC/NC/CC detection from Controls sheet)
# ============================================================================

#' Helper: find first matching column name
first_match <- function(nms, patterns){
  hits <- unique(unlist(lapply(patterns, function(p) {
    nms[grepl(p, nms, ignore.case = TRUE)]
  })))
  if (length(hits)) hits[1] else NA_character_
}

#' Helper: parse European number format
parse_num_eu <- function(x){
  x <- as.character(x)
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub("\\s+", "", x)
  both <- grepl("\\.", x) & grepl(",", x)
  x[both] <- gsub("\\.", "", x[both])       # "1.234,5" -> "1234,5"
  x <- gsub(",", ".", x)                    # decimal comma -> dot
  suppressWarnings(as.numeric(x))
}

#' Map control codes to standard names
ctrl_map <- function(x){
  x2 <- toupper(trimws(as.character(x)))
  dplyr::case_when(
    x2 %in% c("CP","PC","POS","POSITIVE","POS CTRL","POSITIVE CONTROL") ~ "PC",
    x2 %in% c("CN","NC","NEG","NEGATIVE","NEGATIVE CONTROL") ~ "NC",
    x2 %in% c("CC","CONJ","CONJUGATE CONTROL","CONJ CTRL") ~ "CC",
    TRUE ~ NA_character_
  )
}

#' Read ELISA controls from a file
#' @param path Path to Excel file
#' @param assay_label "ELISA_PE" or "ELISA_VSG"
#' @return Data frame with control data
read_elisa_controls <- function(path, assay_label){
  sheets <- readxl::excel_sheets(path)
  ctrl_sheet <- sheets[tolower(sheets) == "controls"]
  if (!length(ctrl_sheet)) return(tibble::tibble())
  
  raw <- readxl::read_excel(path, sheet = ctrl_sheet[1], .name_repair = "minimal") %>%
    janitor::clean_names()
  cols <- names(raw)
  
  # Find columns
  col_plate  <- first_match(cols, c("^elisa$", "^plate$", "^plate_?id$"))
  col_sample <- first_match(cols, c("^sample$", "^name$", "^controle?$"))
  col_code   <- first_match(cols, c("^sample_?code$", "^code$", "^type$"))
  col_dod    <- first_match(cols, c("^d_?od$", "^delta_?od$", "od$"))
  col_pp     <- first_match(cols, c("^pp_?%$", "^pp_percent$", "^pp$", "^percent_?pos(itive)?$"))
  col_result <- first_match(cols, c("^result$"))
  
  # Need at least one identifier
  if (is.na(col_code) && is.na(col_sample)) return(tibble::tibble())
  
  # Build control label
  control_label <- if (!is.na(col_code)) raw[[col_code]] else raw[[col_sample]]
  
  d_od <- if (!is.na(col_dod)) parse_num_eu(raw[[col_dod]]) else NA_real_
  pp   <- if (!is.na(col_pp))  parse_num_eu(raw[[col_pp]])  else NA_real_
  
  # If PP looks like 0–1, rescale to 0–100
  if (any(!is.na(pp))) {
    rng <- range(pp, na.rm = TRUE)
    if (is.finite(rng[2]) && rng[2] <= 1.5 && rng[1] >= -1) pp <- pp * 100
  }
  
  tibble::tibble(
    file        = basename(path),
    assay_label = assay_label,
    plate       = if (!is.na(col_plate)) {
      suppressWarnings(as.integer(raw[[col_plate]]))
    } else 1L,
    control     = ctrl_map(control_label),
    d_od        = d_od,
    pp_percent  = pp,
    result_raw  = if (!is.na(col_result)) as.character(raw[[col_result]]) else NA_character_
  ) %>%
    dplyr::filter(!is.na(control))
}

#' Extract ELISA controls from loaded data
#' @param elisa_pe_data ELISA PE data
#' @param elisa_vsg_data ELISA VSG data
#' @param pe_dir Directory with PE files
#' @param vsg_dir Directory with VSG files
#' @return Combined control data
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

#' QC ELISA controls
#' @param elisa_ctrl Control data
#' @param qc_rules List with PC_min_dod, PC_min_pp, NEG_max_dod, NEG_max_pp
#' @return Data with pass/fail flags
qc_elisa_controls <- function(elisa_ctrl, qc_rules = NULL) {
  if (is.null(qc_rules)) {
    qc_rules <- list(
      PC_min_dod = 0.30,
      PC_min_pp = 90,
      NEG_max_dod = 0.05,
      NEG_max_pp = 5
    )
  }
  
  elisa_ctrl %>%
    dplyr::mutate(
      pass_dod = dplyr::case_when(
        control == "PC" ~ !is.na(d_od) & d_od >= qc_rules$PC_min_dod,
        control %in% c("NC","CC") ~ !is.na(d_od) & d_od <= qc_rules$NEG_max_dod,
        TRUE ~ NA
      ),
      pass_pp = dplyr::case_when(
        control == "PC" ~ !is.na(pp_percent) & pp_percent >= qc_rules$PC_min_pp,
        control %in% c("NC","CC") ~ !is.na(pp_percent) & pp_percent <= qc_rules$NEG_max_pp,
        TRUE ~ NA
      ),
      pass = pass_dod & pass_pp
    )
}

# ============================================================================
# iELISA CONTROLS (from 450-600nm summary sheet)
# ============================================================================

#' Helper: normalize text for matching
norm_txt <- function(x){
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9\\. ]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Parse smart numbers (handles EU format)
parse_num_smart <- function(x){
  x0 <- trimws(as.character(x))
  x0 <- gsub("\u00A0", " ", x0, fixed = TRUE)
  x0 <- gsub("%", "", x0, fixed = TRUE)
  
  silent_num <- suppressWarnings(as.numeric(x0))
  needs_fix  <- is.na(silent_num) & nzchar(x0)
  
  has_comma  <- grepl(",", x0, fixed = TRUE)
  has_dot    <- grepl("\\.", x0)
  has_e      <- grepl("[eE][+-]?[0-9]+$", x0)
  
  out <- silent_num
  
  # EU style "1.234,56"
  m1 <- needs_fix & has_comma & has_dot & !has_e
  if (any(m1)) {
    y <- x0[m1]
    y <- gsub("\\.", "", y)
    y <- gsub(",", ".", y, fixed=TRUE)
    out[m1] <- suppressWarnings(as.numeric(y))
  }
  
  # Only comma "123,45"
  m2 <- needs_fix & has_comma & !has_dot
  if (any(m2)) {
    y <- gsub(",", ".", x0[m2], fixed=TRUE)
    out[m2] <- suppressWarnings(as.numeric(y))
  }
  
  out
}

# Plausibility guards
cap_to_od <- function(v) { v[!is.na(v) & (v < 0 | v > 6)] <- NA_real_; v }
cap_to_pi <- function(v) { v[!is.na(v) & (v < -150 | v > 150)] <- NA_real_; v }

#' Read iELISA controls from file
#' @param path Path to iELISA Excel file
#' @return Tibble with control summaries
read_ielisa_controls_450 <- function(path){
  # Find 450-600nm sheet
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  if (!length(sheets)) return(tibble::tibble())
  
  s_norm <- norm_txt(sheets)
  targets <- c("450 600 nm","450-600 nm","450 600nm","450-600nm")
  hit <- which(s_norm %in% targets)
  if (!length(hit)) {
    hit <- which(grepl("450", s_norm) & grepl("600", s_norm))
  }
  if (!length(hit)) return(tibble::tibble())
  
  sh <- sheets[hit[1]]
  
  # Read as matrix for flexible parsing
  df <- tryCatch({
    openxlsx::read.xlsx(path, sheet = sh, colNames = FALSE,
                        na.strings = c("", "NA"), skipEmptyRows = FALSE, detectDates = FALSE)
  }, error = function(e) {
    readxl::read_excel(path, sheet = sh, col_names = FALSE, .name_repair = "minimal")
  })
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (ncol(df) == 0) return(tibble::tibble())
  
  M_chr <- apply(df, 2, as.character)
  M_num <- suppressWarnings(apply(df, 2, as.numeric))
  M_norm <- apply(M_chr, 2, norm_txt)
  
  # Find numeric values below labels
  find_below <- function(pattern, max_down = 10) {
    matches <- grepl(pattern, M_norm, ignore.case = TRUE)
    if (!any(matches)) return(NA_real_)
    idx <- which(matches, arr.ind = TRUE)[1,]
    r <- idx[1]; c <- idx[2]
    vals <- M_num[(r+1):min(nrow(M_num), r+max_down), c]
    vals <- vals[is.finite(vals)]
    if (length(vals)) vals[1] else NA_real_
  }
  
  tibble::tibble(
    file = basename(path),
    sheet = sh,
    iL13_neg_mean = cap_to_od(find_below("moyenne controle neg.*1\\.3|mean .*neg.*1\\.3")),
    iL13_pos_mean = cap_to_od(find_below("moyenne controle pos.*1\\.3|mean .*pos.*1\\.3")),
    iL13_PI_CP    = cap_to_pi(find_below("^pi.*(cp|c).*1\\.3|pi .*litat.*1\\.3")),
    iL15_neg_mean = cap_to_od(find_below("moyenne controle neg.*1\\.5|mean .*neg.*1\\.5")),
    iL15_pos_mean = cap_to_od(find_below("moyenne controle pos.*1\\.5|mean .*pos.*1\\.5")),
    iL15_PI_CP    = cap_to_pi(find_below("^pi.*(cp|c).*1\\.5|pi .*litat.*1\\.5"))
  )
}

#' Extract iELISA controls
#' @param ielisa_dir Directory with iELISA files
#' @return Control summaries
extract_ielisa_controls <- function(ielisa_dir = NULL) {
  if (is.null(ielisa_dir) || !dir.exists(ielisa_dir)) {
    return(tibble::tibble())
  }
  
  list.files(ielisa_dir, "\\.xlsx?$", full.names = TRUE) %>%
    purrr::map_dfr(read_ielisa_controls_450)
}

#' QC iELISA controls
#' @param ielisa_ctrl Control data
#' @param min_pi Minimum % inhibition for positive control
#' @return Data with QC flags
qc_ielisa_controls <- function(ielisa_ctrl, min_pi = 60) {
  ielisa_ctrl %>%
    dplyr::mutate(
      pass_L13_negpos = !is.na(iL13_neg_mean) & !is.na(iL13_pos_mean) & iL13_neg_mean > iL13_pos_mean,
      pass_L15_negpos = !is.na(iL15_neg_mean) & !is.na(iL15_pos_mean) & iL15_neg_mean > iL15_pos_mean,
      pass_L13_PI = !is.na(iL13_PI_CP) & iL13_PI_CP >= min_pi,
      pass_L15_PI = !is.na(iL15_PI_CP) & iL15_PI_CP >= min_pi,
      pass_LiTat13 = pass_L13_negpos & pass_L13_PI,
      pass_LiTat15 = pass_L15_negpos & pass_L15_PI,
      pass_overall = pass_LiTat13 & pass_LiTat15
    )
}

# ============================================================================
# PLOTTING
# ============================================================================

#' Plot PCR control Cq values over time
#' @param pcr_ctrl_raw PCR control data
#' @param cp_max_cq Maximum acceptable Cq for positive control
#' @return ggplot object
plot_pcr_controls <- function(pcr_ctrl_raw, cp_max_cq = 35) {
  if (is.null(pcr_ctrl_raw) || !nrow(pcr_ctrl_raw)) {
    return(ggplot() + theme_void() + labs(title = "No PCR control data"))
  }
  
  # Order files by date if possible
  file_order <- unique(pcr_ctrl_raw$file)
  
  # Long format for faceting by marker
  cp_long <- pcr_ctrl_raw %>%
    dplyr::filter(control == "CP") %>%
    tidyr::pivot_longer(
      cols = c(Cq_177T, Cq_18S2, Cq_RNAseP),
      names_to = "marker", values_to = "Cq"
    ) %>%
    dplyr::mutate(
      marker = dplyr::recode(marker,
                             "Cq_177T" = "177T",
                             "Cq_18S2" = "18S2",
                             "Cq_RNAseP" = "RNAseP"),
      file = factor(file, levels = file_order)
    )
  
  # Threshold lines per marker
  thr_df <- tibble::tibble(
    marker = c("177T", "18S2", "RNAseP"),
    thr = c(cp_max_cq, cp_max_cq, 40)  # RNAseP has higher threshold
  )
  
  ggplot(cp_long, aes(x = file, y = Cq)) +
    geom_hline(data = thr_df, aes(yintercept = thr), linetype = 2, alpha = 0.6) +
    geom_point(na.rm = TRUE, size = 2) +
    geom_line(aes(group = marker), alpha = 0.6, na.rm = TRUE) +
    coord_flip() +
    facet_wrap(~ marker, ncol = 1, scales = "free_y") +
    labs(
      title = "PCR Positive Control (CP) — Cq per marker",
      x = "Run file",
      y = "Cq value"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

#' Plot ELISA controls
#' @param elisa_ctrl_qc ELISA control data with QC flags
#' @param qc_rules QC threshold rules
#' @return ggplot object
plot_elisa_controls <- function(elisa_ctrl_qc, qc_rules = NULL) {
  if (is.null(elisa_ctrl_qc) || !nrow(elisa_ctrl_qc)) {
    return(ggplot() + theme_void() + labs(title = "No ELISA control data"))
  }
  
  if (is.null(qc_rules)) {
    qc_rules <- list(PC_min_pp = 90, NEG_max_pp = 5)
  }
  
  ggplot(elisa_ctrl_qc, aes(x = factor(plate), y = pp_percent, color = control, group = control)) +
    geom_point(size = 2) +
    geom_line() +
    geom_hline(yintercept = qc_rules$PC_min_pp, linetype = 2, alpha = 0.5) +
    geom_hline(yintercept = qc_rules$NEG_max_pp, linetype = 3, alpha = 0.5) +
    facet_wrap(~ assay_label + file, scales = "free_y") +
    labs(
      title = "ELISA Controls: PP% per plate",
      x = "Plate",
      y = "PP %"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

#' Plot iELISA controls
#' @param ielisa_ctrl_qc iELISA control data with QC flags
#' @param min_pi Minimum PI threshold
#' @return ggplot object
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
  
  pi_long <- ielisa_ctrl_qc %>%
    dplyr::mutate(file = factor(file, levels = file_order)) %>%
    tidyr::pivot_longer(
      cols = c(iL13_PI_CP, iL15_PI_CP),
      names_to = "antigen", values_to = "PI"
    ) %>%
    dplyr::mutate(
      antigen = dplyr::if_else(grepl("iL13", antigen), "LiTat 1.3", "LiTat 1.5")
    )
  
  ggplot(pi_long, aes(x = file, y = PI, color = antigen, group = antigen)) +
    geom_hline(yintercept = min_pi, linetype = 2, alpha = 0.6) +
    geom_point(size = 2, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    coord_flip() +
    labs(
      title = "iELISA Positive Control — % Inhibition",
      x = "File",
      y = "% Inhibition"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom")
}

# Safe NULL coalesce
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
