# helpers_dates.R
# =====================================================================
# Date extraction from file names
# =====================================================================

#' Extract date from filename (YYMMDD format)
#' @param filename Character vector of filenames
#' @return Data frame with extracted date components
#' @export
extract_file_date <- function(filename) {
  # Extract first 6 digits as YYMMDD
  date_str <- stringr::str_extract(filename, "^\\d{6}")
  
  # Extract trailing letter for same-day repeats (a, b, c)
  repeat_letter <- stringr::str_extract(filename, "(?<=^\\d{6})([a-z])")
  
  tibble::tibble(
    filename = filename,
    date_str = date_str,
    repeat_letter = repeat_letter,
    run_date = dplyr::if_else(
      !is.na(date_str) & nchar(date_str) == 6,
      lubridate::ymd(paste0("20", date_str)),
      as.Date(NA)
    ),
    run_sequence = dplyr::case_when(
      is.na(repeat_letter) ~ 1L,
      repeat_letter == "a" ~ 1L,
      repeat_letter == "b" ~ 2L,
      repeat_letter == "c" ~ 3L,
      repeat_letter == "d" ~ 4L,
      TRUE ~ as.integer(match(repeat_letter, letters))
    ),
    run_datetime = dplyr::if_else(
      !is.na(run_date),
      run_date + lubridate::hours(run_sequence - 1),
      as.POSIXct(NA)
    )
  )
}

#' Add file date metadata to lab data
#' @param df Data frame with source_file column
#' @return Data frame with date columns added
#' @export
add_file_dates <- function(df) {
  if (!"source_file" %in% names(df)) {
    df$source_file <- NA_character_
  }
  
  # Extract dates
  date_info <- extract_file_date(df$source_file)
  
  # Add to original data
  df %>%
    dplyr::mutate(
      file_date_str = date_info$date_str,
      file_date = date_info$run_date,
      file_sequence = date_info$run_sequence,
      file_datetime = date_info$run_datetime
    )
}

#' Order files chronologically
#' @param files Character vector of filenames
#' @return Ordered character vector
#' @export
order_files_by_date <- function(files) {
  date_info <- extract_file_date(files)
  
  # Order by date, then by sequence
  ordered_idx <- order(
    date_info$run_date,
    date_info$run_sequence,
    files,
    na.last = TRUE
  )
  
  files[ordered_idx]
}
