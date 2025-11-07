# MBUJI-MAYI BIOBANK DASHBOARD — REFACTORED v2.5
# =============================================================================
# Refocused Biobank import & data quality workflow with navbar UI
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(lubridate)
  library(purrr)
  library(DT)
  library(janitor)
  library(scales)
})

options(shiny.fullstacktrace = TRUE)
options(shiny.maxRequestSize = 50 * 1024^2)

blank_tokens <- c("NA", "N/A", ".", "-")

to_empty_na <- function(x) {
  if (is.null(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) return(x)
  x <- str_trim(x)
  x[x %in% blank_tokens] <- NA_character_
  x[x == ""] <- NA_character_
  x
}

is_blank <- function(x) {
  if (is.null(x)) return(TRUE)
  if (inherits(x, c("POSIXct", "POSIXt", "Date"))) {
    return(is.na(x))
  }
  if (is.numeric(x) || is.integer(x) || is.logical(x)) {
    return(is.na(x))
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- str_trim(x)
    x[x %in% blank_tokens] <- ""
    return(is.na(x) | x == "")
  }
  is.na(x)
}

canonicalise_name <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

parse_date_prelevement <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as_date(x))
  if (is.numeric(x)) {
    suppressWarnings(return(as_date(x, origin = "1899-12-30")))
  }
  if (is.character(x)) {
    x <- to_empty_na(x)
    if (all(is.na(x))) return(as_date(x))
    parsed <- suppressWarnings(parse_date_time(
      x,
      orders = c(
        "Ymd", "dmY", "dmy", "mdy", "Y-m-d", "d/m/Y",
        "m/d/Y", "d.m.Y", "Y/m/d", "d-b-Y", "d-B-Y",
        "Ymd HMS", "dmY HMS", "mdy HMS"
      ),
      tz = "UTC"
    ))
    return(as_date(parsed))
  }
  suppressWarnings(as_date(x))
}

min_date_safe <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(as.Date(NA))
  }
  suppressWarnings(min(x, na.rm = TRUE))
}

prepare_biobank <- function(path, sheet = NULL) {
  raw_df <- read_excel(path, sheet = sheet, guess_max = 50000, .name_repair = "minimal")
  raw_df <- tibble::as_tibble(raw_df, .name_repair = "minimal")

  if (!nrow(raw_df)) {
    return(list(
      rows_read = 0,
      samples_used = 0,
      excluded = 0,
      reason_breakdown = tibble::tibble(),
      used_clean = tibble::tibble(),
      used_full = tibble::tibble(),
      excluded_full = tibble::tibble(),
      field_stats = tibble::tibble(),
      row_completeness = tibble::tibble(),
      dup_numero_summary = tibble::tibble(),
      dup_numero_details = tibble::tibble(),
      dup_kps_summary = tibble::tibble(),
      dup_kps_details = tibble::tibble(),
      normalized_preview = tibble::tibble()
    ))
  }

  normalized <- raw_df %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(across(where(is.character), to_empty_na))

  blank_rows <- normalized %>%
    mutate(.blank_row = if_all(everything(), ~ is_blank(.x))) %>%
    pull(.blank_row)

  if (all(blank_rows)) {
    return(list(
      rows_read = 0,
      samples_used = 0,
      excluded = 0,
      reason_breakdown = tibble::tibble(),
      used_clean = tibble::tibble(),
      used_full = tibble::tibble(),
      excluded_full = tibble::tibble(),
      field_stats = tibble::tibble(),
      row_completeness = tibble::tibble(),
      dup_numero_summary = tibble::tibble(),
      dup_numero_details = tibble::tibble(),
      dup_kps_summary = tibble::tibble(),
      dup_kps_details = tibble::tibble(),
      normalized_preview = tibble::tibble()
    ))
  }

  keep_idx <- which(!blank_rows)
  source_rows <- keep_idx + 1L

  normalized_nonblank <- normalized[keep_idx, , drop = FALSE]
  normalized_nonblank <- mutate(normalized_nonblank, source_row = source_rows)

  cleaned <- normalized_nonblank
  names(cleaned) <- canonicalise_name(names(cleaned))

  required_cols <- c(
    "numero", "etude", "structure_sanitaire", "zone_de_sante",
    "province", "code_barres_kps", "date_prelevement", "age",
    "sex", "presence_drs", "presence_dbs", "nombre_dbs"
  )

  for (col in required_cols) {
    if (!col %in% names(cleaned)) {
      cleaned[[col]] <- NA
    }
  }

  character_cols <- c(
    "numero", "etude", "structure_sanitaire", "zone_de_sante",
    "province", "code_barres_kps", "sex", "presence_drs", "presence_dbs"
  )

  cleaned <- cleaned %>%
    mutate(across(all_of(character_cols), ~ to_empty_na(as.character(.x)))) %>%
    mutate(
      age = suppressWarnings(as.numeric(as.character(age))),
      nombre_dbs = suppressWarnings(as.numeric(as.character(nombre_dbs))),
      date_prelevement = parse_date_prelevement(date_prelevement)
    )

  cleaned <- cleaned %>%
    mutate(
      has_numero = !is_blank(numero),
      has_kps = !is_blank(code_barres_kps),
      used = has_numero & has_kps,
      exclusion_reason = dplyr::case_when(
        !has_numero & !has_kps ~ "Missing Numéro & code-barres KPS",
        !has_numero ~ "Missing Numéro",
        !has_kps ~ "Missing code-barres KPS",
        TRUE ~ NA_character_
      )
    )

  rows_read <- nrow(cleaned)
  samples_used <- sum(cleaned$used, na.rm = TRUE)
  excluded <- rows_read - samples_used

  normalized_preview <- normalized_nonblank %>%
    mutate(across(where(is.character), to_empty_na))

  normalized_preview$used <- cleaned$used
  normalized_preview$exclusion_reason <- cleaned$exclusion_reason

  used_clean <- cleaned %>% filter(used)
  used_full <- normalized_preview %>% filter(used)
  excluded_full <- normalized_preview %>% filter(!used)

  reason_breakdown <- cleaned %>%
    filter(!used) %>%
    count(exclusion_reason, name = "count") %>%
    mutate(
      exclusion_reason = if_else(is.na(exclusion_reason), "Other", exclusion_reason),
      percent = if (rows_read > 0) count / rows_read * 100 else 0
    ) %>%
    arrange(desc(count))

  field_labels <- c(
    numero = "Numéro",
    etude = "étude",
    structure_sanitaire = "structure sanitaire",
    zone_de_sante = "zone de santé",
    province = "province",
    code_barres_kps = "code-barres KPS",
    date_prelevement = "date de prélèvement",
    age = "age",
    sex = "sex",
    presence_drs = "présence de DRS",
    presence_dbs = "présence de DBS",
    nombre_dbs = "nombre de DBS"
  )

  total_used <- nrow(used_clean)
  field_stats <- tibble::tibble(field = required_cols) %>%
    mutate(
      label = field_labels[field],
      non_missing = map_int(field, ~ sum(!is.na(used_clean[[.x]]))),
      total = total_used,
      percent = if (total_used > 0) non_missing / total_used * 100 else 0
    )

  row_completeness <- used_clean %>%
    mutate(
      completeness = if (length(required_cols) > 0) {
        rowSums(!is.na(select(cur_data(), all_of(required_cols)))) / length(required_cols) * 100
      } else {
        0
      }
    ) %>%
    transmute(
      numero,
      code_barres_kps,
      source_row,
      completeness
    )

  dup_numero_summary <- used_clean %>%
    filter(!is_blank(numero)) %>%
    group_by(numero) %>%
    summarise(
      n_occurrences = dplyr::n(),
      first_date_prelevement = min_date_safe(date_prelevement),
      .groups = "drop"
    ) %>%
    filter(n_occurrences > 1) %>%
    arrange(desc(n_occurrences), numero)

  dup_numero_details <- used_clean %>%
    filter(numero %in% dup_numero_summary$numero) %>%
    arrange(numero, code_barres_kps, source_row) %>%
    transmute(
      numero,
      code_barres_kps,
      etude,
      structure_sanitaire,
      zone_de_sante,
      province,
      date_prelevement,
      age,
      sex,
      source_row
    )

  dup_kps_summary <- used_clean %>%
    filter(!is_blank(code_barres_kps)) %>%
    group_by(code_barres_kps) %>%
    summarise(
      n_occurrences = dplyr::n(),
      first_date_prelevement = min_date_safe(date_prelevement),
      .groups = "drop"
    ) %>%
    filter(n_occurrences > 1) %>%
    arrange(desc(n_occurrences), code_barres_kps)

  dup_kps_details <- used_clean %>%
    filter(code_barres_kps %in% dup_kps_summary$code_barres_kps) %>%
    arrange(code_barres_kps, numero, source_row) %>%
    transmute(
      code_barres_kps,
      numero,
      etude,
      structure_sanitaire,
      zone_de_sante,
      province,
      date_prelevement,
      age,
      sex,
      source_row
    )

  list(
    rows_read = rows_read,
    samples_used = samples_used,
    excluded = excluded,
    reason_breakdown = reason_breakdown,
    used_clean = used_clean,
    used_full = used_full,
    excluded_full = excluded_full,
    field_stats = field_stats,
    row_completeness = row_completeness,
    dup_numero_summary = dup_numero_summary,
    dup_numero_details = dup_numero_details,
    dup_kps_summary = dup_kps_summary,
    dup_kps_details = dup_kps_details,
    normalized_preview = normalized_preview
  )
}

overview_table_data <- function(df) {
  df %>%
    transmute(
      `Numéro` = numero,
      `étude` = etude,
      `structure sanitaire` = structure_sanitaire,
      `zone de santé` = zone_de_sante,
      `province` = province,
      `code-barres KPS` = code_barres_kps,
      `date de prélèvement` = date_prelevement,
      `age` = age,
      `sex` = sex,
      `présence de DRS` = presence_drs,
      `présence de DBS` = presence_dbs,
      `nombre de DBS` = nombre_dbs
    )
}

default_example_path <- "testdata/20251023 Registre KPS reçus_labo Dipumba.xlsx"

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

  nav_panel(
    "Import & Status",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Biobank file"),
        fileInput("biobank_file", "Upload Excel", accept = c(".xlsx", ".xls")),
        textInput("biobank_path", "or load from path", value = default_example_path),
        actionButton("load_path", "Load from path", class = "btn-primary"),
        selectInput("sheet", "Sheet", choices = character()),
        div(
          class = "mt-2 text-muted",
          textOutput("file_status")
        )
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title = "Rows read",
          value = textOutput("vb_rows_read"),
          showcase = bs_icon("table"),
          theme = "primary"
        ),
        value_box(
          title = "Samples used",
          value = textOutput("vb_samples_used"),
          showcase = bs_icon("check2-circle"),
          theme = "success"
        ),
        value_box(
          title = "Excluded",
          value = textOutput("vb_excluded"),
          showcase = bs_icon("x-octagon"),
          theme = "danger"
        )
      ),
      card(
        card_header("Exclusion reasons"),
        DTOutput("tbl_reasons")
      )
    )
  ),

  nav_panel(
    "Overview",
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Samples overview"),
        DTOutput("tbl_overview")
      ),
      card(
        card_header("Column completeness"),
        DTOutput("tbl_overview_completeness")
      )
    )
  ),

  nav_panel(
    "Duplicates",
    navset_card_pill(
      nav_panel(
        "Numéro",
        card(
          card_header("Duplicate Numéro summary"),
          DTOutput("tbl_dup_numero_summary")
        ),
        card(
          card_header("Duplicate Numéro details"),
          DTOutput("tbl_dup_numero_details")
        )
      ),
      nav_panel(
        "code-barres KPS",
        card(
          card_header("Duplicate code-barres KPS summary"),
          DTOutput("tbl_dup_kps_summary")
        ),
        card(
          card_header("Duplicate code-barres KPS details"),
          DTOutput("tbl_dup_kps_details")
        )
      )
    )
  ),

  nav_panel(
    "Field Completeness",
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Required field completeness"),
        DTOutput("tbl_field_completeness")
      ),
      card(
        card_header("Per-row completeness"),
        DTOutput("tbl_row_completeness")
      )
    )
  ),

  nav_panel(
    "Raw Preview",
    card(
      card_header("First 50 rows (normalized)"),
      DTOutput("tbl_raw_preview")
    )
  )
)

server <- function(input, output, session) {
  active_path <- reactiveVal(default_example_path)

  observeEvent(input$biobank_file, {
    req(input$biobank_file)
    active_path(input$biobank_file$datapath)
    updateTextInput(session, "biobank_path", value = input$biobank_file$name)
  })

  observeEvent(input$load_path, {
    path <- str_trim(input$biobank_path)
    if (nzchar(path)) {
      active_path(path)
    }
  })

  observeEvent(active_path(), {
    path <- active_path()
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      updateSelectInput(session, "sheet", choices = character())
      return()
    }
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
    if (length(sheets)) {
      updateSelectInput(session, "sheet", choices = sheets, selected = sheets[1])
    } else {
      updateSelectInput(session, "sheet", choices = character())
    }
  }, ignoreNULL = FALSE)

  biobank_data <- reactive({
    path <- active_path()
    req(path)
    validate(need(file.exists(path), "File not found"))
    sheet <- input$sheet
    validate(need(length(sheet) == 1 && nzchar(sheet), "Select a sheet"))
    prepare_biobank(path, sheet)
  })

  output$file_status <- renderText({
    path <- active_path()
    if (is.null(path) || !nzchar(path)) {
      return("No file selected")
    }
    if (!file.exists(path)) {
      return("⚠️ File not found")
    }
    paste0("Using: ", path)
  })

  output$vb_rows_read <- renderText({
    data <- biobank_data()
    comma(data$rows_read)
  })

  output$vb_samples_used <- renderText({
    data <- biobank_data()
    comma(data$samples_used)
  })

  output$vb_excluded <- renderText({
    data <- biobank_data()
    pct <- if (data$rows_read > 0) data$excluded / data$rows_read * 100 else 0
    paste0(comma(data$excluded), " (", sprintf("%.1f%%", pct), ")")
  })

  output$tbl_reasons <- renderDT({
    data <- biobank_data()
    tbl <- data$reason_breakdown
    if (!nrow(tbl)) {
      tbl <- tibble::tibble(
        `Reason` = "No exclusions",
        `Count` = 0,
        `Percent of rows read` = "0.0%"
      )
      return(datatable(tbl, options = list(dom = "t", paging = FALSE), rownames = FALSE))
    }
    tbl <- tbl %>%
      transmute(
        Reason = exclusion_reason,
        Count = count,
        `Percent of rows read` = sprintf("%.1f%%", percent)
      )
    datatable(
      tbl,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      rownames = FALSE
    )
  })

  output$tbl_overview <- renderDT({
    data <- biobank_data()
    tbl <- overview_table_data(data$used_clean)
    datatable(
      tbl,
      extensions = "Buttons",
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$tbl_overview_completeness <- renderDT({
    data <- biobank_data()
    tbl <- data$field_stats %>%
      transmute(
        Field = label,
        `Non-missing` = non_missing,
        Total = total,
        Percent = percent
      )
    dt <- datatable(
      tbl,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      rownames = FALSE
    )
    formatStyle(
      dt,
      "Percent",
      background = styleColorBar(c(0, 100), "#3498DB"),
      backgroundSize = "98% 70%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    ) %>%
      formatRound("Percent", 1)
  })

  output$tbl_dup_numero_summary <- renderDT({
    data <- biobank_data()
    tbl <- data$dup_numero_summary
    if (!nrow(tbl)) {
      tbl <- tibble::tibble(`Numéro` = character(), `Occurrences` = integer(), `Première date de prélèvement` = as.Date(character()))
    } else {
      tbl <- tbl %>%
        transmute(
          `Numéro` = numero,
          `Occurrences` = n_occurrences,
          `Première date de prélèvement` = first_date_prelevement
        )
    }
    datatable(tbl, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
  })

  output$tbl_dup_numero_details <- renderDT({
    data <- biobank_data()
    tbl <- data$dup_numero_details
    datatable(tbl, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tbl_dup_kps_summary <- renderDT({
    data <- biobank_data()
    tbl <- data$dup_kps_summary
    if (!nrow(tbl)) {
      tbl <- tibble::tibble(`code-barres KPS` = character(), `Occurrences` = integer(), `Première date de prélèvement` = as.Date(character()))
    } else {
      tbl <- tbl %>%
        transmute(
          `code-barres KPS` = code_barres_kps,
          `Occurrences` = n_occurrences,
          `Première date de prélèvement` = first_date_prelevement
        )
    }
    datatable(tbl, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
  })

  output$tbl_dup_kps_details <- renderDT({
    data <- biobank_data()
    tbl <- data$dup_kps_details
    datatable(tbl, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tbl_field_completeness <- renderDT({
    data <- biobank_data()
    tbl <- data$field_stats %>%
      transmute(
        Field = label,
        `Non-missing` = non_missing,
        Total = total,
        Percent = percent
      )
    dt <- datatable(
      tbl,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      rownames = FALSE
    )
    formatStyle(
      dt,
      "Percent",
      background = styleColorBar(c(0, 100), "#2C3E50"),
      backgroundSize = "98% 70%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    ) %>%
      formatRound("Percent", 1)
  })

  output$tbl_row_completeness <- renderDT({
    data <- biobank_data()
    tbl <- data$row_completeness %>%
      transmute(
        `Numéro` = numero,
        `code-barres KPS` = code_barres_kps,
        `Source row` = source_row,
        `Completeness (%)` = completeness
      )
    dt <- datatable(
      tbl,
      options = list(pageLength = 10, dom = "tip"),
      rownames = FALSE
    )
    formatStyle(
      dt,
      "Completeness (%)",
      background = styleColorBar(c(0, 100), "#27AE60"),
      backgroundSize = "98% 70%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    ) %>%
      formatRound("Completeness (%)", 1)
  })

  output$tbl_raw_preview <- renderDT({
    data <- biobank_data()
    tbl <- head(data$normalized_preview, 50)
    datatable(
      tbl,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
