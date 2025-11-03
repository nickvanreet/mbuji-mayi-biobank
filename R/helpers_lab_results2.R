# =====================================================================
# HELPERS: Load & merge PCR, ELISA (PE + VSG), iELISA, and controls
# =====================================================================

lab_result_tbl <- function(data, messages = character()) {
  attr(data, "messages") <- unique(stats::na.omit(messages))
  data
}

normalise_dir <- function(dir) {
  d <- safe_path(dir)
  if (is.null(d) || !nzchar(d)) return(NA_character_)
  d
}

collect_excel_files <- function(dir, label) {
  if (is.na(dir) || !dir.exists(dir)) {
    return(list(files = character(), messages = sprintf("%s directory not found: %s", label, dir)))
  }
  files <- list.files(dir, pattern = "\\.xlsx?$", full.names = TRUE)
  msgs <- character()
  if (!length(files)) {
    msgs <- sprintf("No %s Excel files found in %s", label, dir)
  }
  list(files = files, messages = msgs)
}

parse_pcr_results <- function(dir) {
  dir <- normalise_dir(dir)
  collected <- collect_excel_files(dir, "PCR")
  files <- collected$files
  notes <- collected$messages

  if (!length(files)) {
    return(lab_result_tbl(tibble::tibble(), notes))
  }

  read_target_sheet <- function(path, sheet_name) {
    tryCatch({
      readxl::read_excel(path, sheet = sheet_name) %>%
        janitor::clean_names() %>%
        dplyr::transmute(
          sample = as.character(sample),
          cq = suppressWarnings(as.numeric(cq))
        ) %>%
        dplyr::filter(!is.na(sample)) %>%
        dplyr::mutate(cq = dplyr::if_else(is.finite(cq) & cq >= 0, cq, NA_real_))
    }, error = function(e) {
      tibble::tibble(sample = character(), cq = numeric())
    })
  }

  messages <- notes

  process_file <- function(path) {
    tryCatch({
      sheets <- suppressWarnings(readxl::excel_sheets(path))
      if (!length(sheets)) {
        messages <<- c(messages, sprintf("No sheets found in %s", basename(path)))
        return(tibble::tibble())
      }

      calls <- tryCatch({
        readxl::read_excel(path, sheet = "Replicates") %>%
          janitor::clean_names() %>%
          dplyr::transmute(
            lab_id = as.character(sample),
            PCR_call = as.character(dplyr::coalesce(final, call, result, status))
          )
      }, error = function(e) {
        messages <<- c(messages, sprintf("Missing 'Replicates' sheet in %s", basename(path)))
        tibble::tibble(lab_id = character(), PCR_call = character())
      })

      t177 <- read_target_sheet(path, "177T") %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(
          Cq_177T = mean(cq, na.rm = TRUE),
          SD_177T = stats::sd(cq, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::rename(lab_id = sample)

      t18s <- read_target_sheet(path, "18S2") %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(
          Cq_18S2 = mean(cq, na.rm = TRUE),
          SD_18S2 = stats::sd(cq, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::rename(lab_id = sample)

      rnasep <- read_target_sheet(path, "RNAseP") %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(
          Cq_RNAseP = mean(cq, na.rm = TRUE),
          SD_RNAseP = stats::sd(cq, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::rename(lab_id = sample)

      calls %>%
        dplyr::left_join(t177, by = "lab_id") %>%
        dplyr::left_join(t18s, by = "lab_id") %>%
        dplyr::left_join(rnasep, by = "lab_id") %>%
        dplyr::mutate(source_file = basename(path))
    }, error = function(e) {
      messages <<- c(messages, sprintf("Failed to parse PCR file %s: %s", basename(path), e$message))
      tibble::tibble()
    })
  }

  pcr_data <- purrr::map_dfr(files, process_file)
  lab_result_tbl(pcr_data, messages)
}

parse_elisa_generic <- function(dir, assay_label) {
  dir <- normalise_dir(dir)
  collected <- collect_excel_files(dir, assay_label)
  files <- collected$files
  messages <- collected$messages

  if (!length(files)) {
    return(lab_result_tbl(tibble::tibble(), messages))
  }

  process_file <- function(path) {
    tryCatch({
      sheets <- suppressWarnings(readxl::excel_sheets(path))
      sheet_idx <- which(grepl("result", sheets, ignore.case = TRUE))
      sheet_name <- if (length(sheet_idx)) sheets[sheet_idx[1]] else sheets[1]

      raw <- readxl::read_excel(path, sheet = sheet_name, .name_repair = "minimal")
      if (nrow(raw) == 0) {
        messages <<- c(messages, sprintf("Empty sheet in %s", basename(path)))
        return(tibble::tibble())
      }

      raw <- janitor::clean_names(raw)

      get_first <- function(cols) {
        for (col in cols) {
          if (!is.null(raw[[col]])) return(raw[[col]])
        }
        rep(NA, nrow(raw))
      }

      tibble::tibble(
        lab_id = as.character(get_first(c("numero_labo_dipumba", "numero_labo", "lab_id", "sample"))),
        barcode = as.character(get_first(c("code_barres_kps", "code_barres", "barcode", "code_echantillon_code_barres"))),
        OD = suppressWarnings(as.numeric(get_first(c("d_od", "delta_od", "od")))),
        PP_percent = suppressWarnings(as.numeric(get_first(c("pp_percent", "pp", "percent_pp", "pp_50")))),
        Result_pp50 = as.character(get_first(c("result_pp50", "resultat_pp50", "interpretation_pp50"))),
        Result_pp60 = as.character(get_first(c("result_pp60", "resultat_pp60", "interpretation_pp60"))),
        assay = assay_label,
        source_file = basename(path)
      ) %>%
        dplyr::filter(!is.na(lab_id) | !is.na(barcode))
    }, error = function(e) {
      messages <<- c(messages, sprintf("Failed to parse %s file %s: %s", assay_label, basename(path), e$message))
      tibble::tibble()
    })
  }

  lab_result_tbl(purrr::map_dfr(files, process_file), messages)
}

parse_elisa_pe <- function(dir) {
  parse_elisa_generic(dir, "ELISA_PE")
}

parse_elisa_vsg <- function(dir) {
  parse_elisa_generic(dir, "ELISA_VSG")
}

parse_ielisa <- function(dir) {
  dir <- normalise_dir(dir)
  collected <- collect_excel_files(dir, "iELISA")
  files <- collected$files
  messages <- collected$messages

  if (!length(files)) {
    return(lab_result_tbl(tibble::tibble(), messages))
  }

  process_file <- function(path) {
    tryCatch({
      sheets <- suppressWarnings(readxl::excel_sheets(path))
      if (!length(sheets)) {
        messages <<- c(messages, sprintf("No sheets in %s", basename(path)))
        return(tibble::tibble())
      }

      latin_sheets <- stringi::stri_trans_general(sheets, "Latin-ASCII")
      sheet_idx <- which(grepl("result", latin_sheets, ignore.case = TRUE))
      sheet_name <- if (length(sheet_idx)) sheets[sheet_idx[1]] else sheets[1]

      probe <- suppressWarnings(readxl::read_excel(path, sheet = sheet_name, range = "A1:C40", col_names = FALSE, .name_repair = "minimal"))
      header_row <- which(grepl("num[eé]ro.*labo", probe[[2]], ignore.case = TRUE))[1] - 1
      if (is.na(header_row)) header_row <- 0

      raw <- readxl::read_excel(path, sheet = sheet_name, skip = header_row, .name_repair = "minimal") %>%
        janitor::clean_names()

      pct_cols <- grep("pourcentage.*inhibition|pct.*inh|percent.*inhib", names(raw), ignore.case = TRUE, value = TRUE)
      res_cols <- grep("result", names(raw), ignore.case = TRUE, value = TRUE)

      tibble::tibble(
        lab_id = as.character(dplyr::coalesce(raw$numero_labo, raw$numero_labo_dipumba, raw$sample_id, raw$lab_id)),
        barcode = as.character(dplyr::coalesce(raw$code_barres_kps, raw$barcode, raw$code_barre)),
        pct_inh_13 = if (length(pct_cols) >= 1) suppressWarnings(as.numeric(raw[[pct_cols[1]]])) else NA_real_,
        pct_inh_15 = if (length(pct_cols) >= 2) suppressWarnings(as.numeric(raw[[pct_cols[2]]])) else NA_real_,
        res_13 = if (length(res_cols) >= 1) as.character(raw[[res_cols[1]]]) else NA_character_,
        res_15 = if (length(res_cols) >= 2) as.character(raw[[res_cols[2]]]) else NA_character_,
        res_final = as.character(dplyr::coalesce(raw$resultat_final, raw$final_result, raw$resultat_final_litat, raw$interpretation)),
        source_file = basename(path)
      ) %>%
        dplyr::filter(!is.na(lab_id) | !is.na(barcode))
    }, error = function(e) {
      messages <<- c(messages, sprintf("Failed to parse iELISA file %s: %s", basename(path), e$message))
      tibble::tibble()
    })
  }

  lab_result_tbl(purrr::map_dfr(files, process_file), messages)
}

merge_lab_with_biobank <- function(biobank, lab) {
  if (is.null(biobank) || !nrow(biobank)) {
    return(tibble::tibble())
  }

  result <- biobank %>%
    dplyr::select(barcode, lab_id) %>%
    dplyr::distinct()

  if (!is.null(lab$pcr) && nrow(lab$pcr) > 0) {
    result <- result %>%
      dplyr::left_join(
        lab$pcr %>% dplyr::select(lab_id, PCR_call, Cq_177T, Cq_18S2, Cq_RNAseP, SD_177T, SD_18S2, SD_RNAseP),
        by = "lab_id",
        relationship = "many-to-one"
      )
  }

  if (!is.null(lab$elisa_pe) && nrow(lab$elisa_pe) > 0) {
    result <- result %>%
      dplyr::left_join(
        lab$elisa_pe %>%
          dplyr::select(lab_id, barcode, OD_PE = OD, PP_percent_PE = PP_percent,
                        Result_pp50_PE = Result_pp50, Result_pp60_PE = Result_pp60),
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
  }

  if (!is.null(lab$elisa_vsg) && nrow(lab$elisa_vsg) > 0) {
    result <- result %>%
      dplyr::left_join(
        lab$elisa_vsg %>%
          dplyr::select(lab_id, barcode, OD_VSG = OD, PP_percent_VSG = PP_percent,
                        Result_pp50_VSG = Result_pp50, Result_pp60_VSG = Result_pp60),
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
  }

  if (!is.null(lab$ielisa) && nrow(lab$ielisa) > 0) {
    ielisa_agg <- lab$ielisa %>%
      dplyr::group_by(lab_id, barcode) %>%
      dplyr::summarise(
        iELISA_pct_13 = suppressWarnings(mean(pct_inh_13, na.rm = TRUE)),
        iELISA_pct_15 = suppressWarnings(mean(pct_inh_15, na.rm = TRUE)),
        iELISA_result_13 = dplyr::first(stats::na.omit(res_13)),
        iELISA_result_15 = dplyr::first(stats::na.omit(res_15)),
        iELISA_result_final = dplyr::first(stats::na.omit(res_final)),
        .groups = "drop"
      )

    result <- result %>%
      dplyr::left_join(
        ielisa_agg,
        by = c("lab_id", "barcode"),
        relationship = "many-to-one"
      )
  }

  result
}

compute_flags <- function(df, pcr_cq_max = 38, elisa_pp_cut = 50, ielisa_inh_cut = 35) {
  if (is.null(df) || !nrow(df)) return(tibble::tibble())

  pcr_pos_from_call <- function(x) {
    ifelse(
      is.na(x) | !nzchar(x), NA,
      dplyr::case_when(
        grepl("pos", x, ignore.case = TRUE) ~ TRUE,
        grepl("neg", x, ignore.case = TRUE) ~ FALSE,
        TRUE ~ NA
      )
    )
  }

  min_target_cq <- purrr::pmap_dbl(list(df$Cq_177T, df$Cq_18S2), function(a, b) {
    vals <- suppressWarnings(as.numeric(c(a, b)))
    vals <- vals[is.finite(vals)]
    if (!length(vals)) return(NA_real_)
    min(vals)
  })

  concordant_vals <- purrr::pmap(
    list(df$PCR_pos, df$ELISA_PE_pos, df$ELISA_VSG_pos, df$iELISA_pos),
    function(a, b, c, d) {
      vals <- c(a, b, c, d)
      vals <- vals[!is.na(vals)]
      if (!length(vals)) return(NA)
      all(vals == vals[1])
    }
  ) %>% unlist(use.names = FALSE)

  df %>%
    dplyr::mutate(
      PCR_pos_call = pcr_pos_from_call(PCR_call),
      PCR_pos_threshold = dplyr::case_when(
        !is.na(min_target_cq) & min_target_cq <= pcr_cq_max ~ TRUE,
        !is.na(min_target_cq) ~ FALSE,
        TRUE ~ NA
      ),
      PCR_pos = dplyr::coalesce(PCR_pos_call, PCR_pos_threshold),
      PCR_tested = !is.na(PCR_pos_call) | !is.na(min_target_cq) | !is.na(Cq_RNAseP),

      ELISA_PE_pos = ifelse(!is.na(PP_percent_PE), PP_percent_PE >= elisa_pp_cut, NA),
      ELISA_PE_tested = !is.na(PP_percent_PE),

      ELISA_VSG_pos = ifelse(!is.na(PP_percent_VSG), PP_percent_VSG >= elisa_pp_cut, NA),
      ELISA_VSG_tested = !is.na(PP_percent_VSG),

      iELISA_pos_numeric = ifelse(!is.na(iELISA_pct_13) | !is.na(iELISA_pct_15),
                                  (dplyr::coalesce(iELISA_pct_13, -Inf) >= ielisa_inh_cut) |
                                    (dplyr::coalesce(iELISA_pct_15, -Inf) >= ielisa_inh_cut),
                                  NA),
      iELISA_pos_text = ifelse(
        is.na(iELISA_result_13) & is.na(iELISA_result_15) & is.na(iELISA_result_final),
        NA,
        grepl("pos", paste(iELISA_result_13, iELISA_result_15, iELISA_result_final), ignore.case = TRUE)
      ),
      iELISA_pos = dplyr::coalesce(iELISA_pos_numeric, iELISA_pos_text),
      iELISA_tested = !is.na(iELISA_pos_numeric) | !is.na(iELISA_pos_text),

      Any_positive = dplyr::coalesce(PCR_pos, FALSE) |
        dplyr::coalesce(ELISA_PE_pos, FALSE) |
        dplyr::coalesce(ELISA_VSG_pos, FALSE) |
        dplyr::coalesce(iELISA_pos, FALSE),

      Concordant = concordant_vals
    ) %>%
    dplyr::select(-PCR_pos_call, -PCR_pos_threshold, -iELISA_pos_numeric, -iELISA_pos_text)
}

detect_control_type <- function(x) {
  lab <- tolower(trimws(x))
  dplyr::case_when(
    !nzchar(lab) ~ "Unknown",
    grepl("neg|nc|blank|ntc", lab) ~ "Negative",
    grepl("pos|pc|control\\+", lab) ~ "Positive",
    TRUE ~ "Unknown"
  )
}

summarise_pcr_controls <- function(pcr_df, threshold) {
  if (is.null(pcr_df) || !nrow(pcr_df)) {
    return(tibble::tibble())
  }

  controls <- pcr_df %>%
    dplyr::filter(grepl("ctrl|control|pc|nc|pos|neg", lab_id, ignore.case = TRUE)) %>%
    dplyr::mutate(
      Control = detect_control_type(lab_id),
      Min_Cq = purrr::pmap_dbl(list(Cq_177T, Cq_18S2), function(a, b) {
        vals <- suppressWarnings(as.numeric(c(a, b)))
        vals <- vals[is.finite(vals)]
        if (!length(vals)) return(NA_real_)
        min(vals)
      }),
      Status = dplyr::case_when(
        Control == "Positive" & !is.na(Min_Cq) & Min_Cq <= threshold ~ "OK",
        Control == "Positive" & is.na(Min_Cq) ~ "Check: missing Cq",
        Control == "Positive" ~ "Check: high Cq",
        Control == "Negative" & (is.na(Min_Cq) || Min_Cq > threshold) ~ "OK",
        Control == "Negative" ~ "Check: amplification",
        TRUE ~ "Review"
      )
    ) %>%
    dplyr::select(lab_id, Control, Cq_177T, Cq_18S2, Cq_RNAseP, Status)

  controls
}

summarise_elisa_controls <- function(elisa_df, threshold, assay_label) {
  if (is.null(elisa_df) || !nrow(elisa_df)) {
    return(tibble::tibble())
  }

  elisa_df %>%
    dplyr::filter(grepl("ctrl|control|pc|nc|pos|neg", lab_id, ignore.case = TRUE)) %>%
    dplyr::mutate(
      Control = detect_control_type(lab_id),
      Status = dplyr::case_when(
        Control == "Positive" & !is.na(PP_percent) & PP_percent >= threshold ~ "OK",
        Control == "Positive" & is.na(PP_percent) ~ "Check: missing %PP",
        Control == "Positive" ~ "Check: low %PP",
        Control == "Negative" & (!is.na(PP_percent) & PP_percent < threshold) ~ "OK",
        Control == "Negative" & is.na(PP_percent) ~ "Check: missing %PP",
        Control == "Negative" ~ "Check: high %PP",
        TRUE ~ "Review"
      ),
      Assay = assay_label
    ) %>%
    dplyr::select(lab_id, Assay, Control, OD, PP_percent, Status)
}

summarise_ielisa_controls <- function(ielisa_df, threshold) {
  if (is.null(ielisa_df) || !nrow(ielisa_df)) {
    return(tibble::tibble())
  }

  get_col <- function(df, name) {
    if (name %in% names(df)) df[[name]] else rep(NA, nrow(df))
  }

  filtered <- ielisa_df %>%
    dplyr::filter(grepl("ctrl|control|pc|nc|pos|neg", lab_id, ignore.case = TRUE))

  if (!nrow(filtered)) {
    return(tibble::tibble())
  }

  primary13 <- suppressWarnings(as.numeric(get_col(filtered, "iELISA_pct_13")))
  fallback13 <- suppressWarnings(as.numeric(get_col(filtered, "pct_inh_13")))
  pct_13 <- ifelse(!is.na(primary13), primary13, fallback13)

  primary15 <- suppressWarnings(as.numeric(get_col(filtered, "iELISA_pct_15")))
  fallback15 <- suppressWarnings(as.numeric(get_col(filtered, "pct_inh_15")))
  pct_15 <- ifelse(!is.na(primary15), primary15, fallback15)

  res_primary <- as.character(get_col(filtered, "iELISA_result_final"))
  res_fallback <- as.character(get_col(filtered, "res_final"))
  res_final <- ifelse(!is.na(res_primary) & nzchar(res_primary), res_primary, res_fallback)

  filtered %>%
    dplyr::mutate(
      Control = detect_control_type(lab_id),
      iELISA_pct_13 = pct_13,
      iELISA_pct_15 = pct_15,
      iELISA_result_final = res_final,
      Max_pct = purrr::pmap_dbl(list(iELISA_pct_13, iELISA_pct_15), function(a, b) {
        vals <- suppressWarnings(as.numeric(c(a, b)))
        vals <- vals[is.finite(vals)]
        if (!length(vals)) return(NA_real_)
        max(vals)
      }),
      Status = dplyr::case_when(
        Control == "Positive" & !is.na(Max_pct) & Max_pct >= threshold ~ "OK",
        Control == "Positive" & is.na(Max_pct) ~ "Check: missing % inhibition",
        Control == "Positive" ~ "Check: low % inhibition",
        Control == "Negative" & (!is.na(Max_pct) && Max_pct < threshold) ~ "OK",
        Control == "Negative" & is.na(Max_pct) ~ "Check: missing % inhibition",
        Control == "Negative" ~ "Check: high % inhibition",
        TRUE ~ "Review"
      )
    ) %>%
    dplyr::select(lab_id, Control, iELISA_pct_13, iELISA_pct_15, iELISA_result_final, Status)
}
