#' Extend a Time Series into the Future
#'
#' Appends future-dated rows to a time-series data frame by filling predictor
#' variables with the rolling mean of recent historical observations that share
#' the same sub-period (month, quarter, semester, or year).  The response
#' variable column is set to `NA` in all new rows.
#'
#' @param data A data frame containing historical records.  Must include a
#'   `date` column of class `Date` and numeric predictor columns.
#' @param startdate A scalar coercible to `Date` via [lubridate::ymd()].  The
#'   first future date to generate.
#' @param period Character; one of `"year"`, `"semester"`, `"quarter"`, or
#'   `"month"`.  Passed to [base::seq.Date()] as the `by` argument and used to
#'   match historical sub-periods.
#' @param length Integer.  Number of future periods to generate.
#' @param round_dates Character; one of `"ceiling"` (default), `"floor"`, or
#'   `"none"`.  When not `"none"`, each generated date is rounded to the
#'   boundary of `round_unit`.
#' @param round_unit Character scalar passed to [lubridate::ceiling_date()] or
#'   [lubridate::floor_date()] when `round_dates != "none"` (e.g. `"month"`).
#' @param fillyears Integer.  Number of most-recent historical years to use when
#'   computing rolling means.  Ignored when `minyear` is supplied.  Default `3`.
#' @param minyear Integer or `NULL`.  Earliest year to include in the historical
#'   window.  If `NULL`, derived as `maxyear - fillyears`.
#' @param maxyear Integer or `NULL`.  Latest year to include in the historical
#'   window.  If `NULL`, derived from `max(year(data$date))`.
#' @param present_vars Character vector of predictor column names, **or**
#'   `"detect"` to auto-select all numeric columns that do not contain `"_l"`
#'   in their name (i.e., exclude pre-existing lag columns).
#' @param response_var Character scalar.  Name of the response/outcome column.
#'   This column is excluded from predictor filling and set to `NA_real_` in
#'   the returned rows.
#'
#' @return A tibble of future-dated rows (from `startdate` onwards) whose
#'   columns match those of `data`.  Predictor columns are filled with rolling
#'   historical means; `response_var` is `NA_real_`.
#'
#' @details
#' The function builds a running `fill_data` table.  For each future date, it
#' finds the most recent 3 historical observations sharing the same sub-period,
#' averages them, and appends a new row.  Lag columns (suffix `_l<n>`) are then
#' computed over the combined table and the function returns only the
#' future-dated rows, selecting the same columns as `data`.
#'
#' If `oni_anom` is detected among the predictor columns, an `oni_phenom`
#' classification column (`"La Niña"`, `"El Niño"`, `"Normal"`) is appended
#' automatically.
#'
#' @examples
#' \dontrun{
#' future_rows <- extend_time_series(
#'   data         = historical_df,
#'   startdate    = "2025-01-01",
#'   period       = "quarter",
#'   length       = 4,
#'   round_dates  = "ceiling",
#'   round_unit   = "month",
#'   present_vars = "detect",
#'   response_var = "riceProd"
#' )
#' }
#'
#' @importFrom dplyr select where contains filter mutate bind_rows bind_cols
#'   arrange desc slice_head summarise across all_of any_of sym
#' @importFrom lubridate ymd year month quarter ceiling_date floor_date days
#' @importFrom purrr map_dfc
#' @importFrom tibble tibble
#' @importFrom rlang `!!` `!!!`
#' @export
extend_time_series <- function(
  data,
  startdate,
  period = c("year", "semester", "quarter", "month"),
  length,
  round_dates = c("ceiling", "floor", "none"),
  round_unit = NULL,
  fillyears = 3,
  minyear = NULL,
  maxyear = NULL,
  present_vars = c("stored", "detect"),
  response_var
) {
  # ---- helpers ----
  semester <- function(x) dplyr::if_else(lubridate::month(x) <= 6, 1L, 2L)

  period <- match.arg(period)
  round_dates <- match.arg(round_dates)
  start.date <- ymd(startdate)

  # ---- detect present vars ----
  if ("detect" %in% present_vars) {
    present_vars <- data |>
      select(where(is.numeric)) |>
      select(!contains("_l")) |>
      names()
  }

  present_vars <- setdiff(present_vars, response_var)

  # ---- future dates ----
  future_dates <- seq.Date(
    from = start.date,
    by = period,
    length.out = length
  )

  if (round_dates != "none") {
    future_dates <- switch(
      round_dates,
      ceiling = ceiling_date(future_dates, round_unit) - days(1),
      floor = floor_date(future_dates, round_unit)
    )
  }

  # ---- historical window ----
  if (is.null(maxyear)) {
    maxyear <- max(year(data$date))
  }
  if (is.null(minyear)) {
    minyear <- maxyear - fillyears
  }

  fill_data <- data |>
    filter(year(date) %in% minyear:maxyear) |>
    select(all_of(c("date", present_vars))) |>
    mutate(
      year = year(date),
      quarter = quarter(date),
      semester = semester(date),
      month = month(date)
    )

  # ---- recursive fill ----
  for (i in seq_along(future_dates)) {
    fd <- future_dates[i]

    selector <- switch(
      period,
      year = fill_data |> arrange(desc(year)),
      semester = fill_data |>
        filter(semester == semester(fd)) |>
        arrange(desc(year)),
      quarter = fill_data |>
        filter(quarter == quarter(fd)) |>
        arrange(desc(year)),
      month = fill_data |> filter(month == month(fd)) |> arrange(desc(year))
    )

    last_vals <- selector |>
      slice_head(n = 3) |>
      summarise(across(all_of(present_vars), mean, na.rm = TRUE)) |>
      as.list()

    new_row <- tibble(
      date = fd,
      year = year(fd),
      quarter = quarter(fd),
      semester = semester(fd),
      month = month(fd)
    ) |>
      mutate(!!!last_vals)

    fill_data <- bind_rows(fill_data, new_row)
  }

  # ---- optional ENSO classification ----
  if ("oni_anom" %in% names(fill_data)) {
    fill_data <- fill_data |>
      mutate(
        oni_phenom = case_when(
          oni_anom <= -0.5 ~ "La Ni\u00f1a",
          oni_anom >= 0.5 ~ "El Ni\u00f1o",
          TRUE ~ "Normal"
        )
      )
  }

  # ---- lag creation (SAFE) ----
  max_lag <- switch(
    period,
    year = 1,
    semester = 2,
    quarter = 4,
    month = 6
  )

  lag_data <- purrr::map_dfc(
    seq_len(max_lag),
    function(lag_i) {
      purrr::map_dfc(
        present_vars,
        function(v) {
          tibble(!!paste0(v, "_l", lag_i) := dplyr::lag(fill_data[[v]], lag_i))
        }
      )
    }
  )

  stopifnot(nrow(lag_data) == nrow(fill_data))
  stopifnot(!anyDuplicated(names(lag_data)))

  finaldata <- bind_cols(fill_data, lag_data) |>
    filter(date >= start.date) |>
    select(any_of(names(data))) |>
    mutate(!!sym(response_var) := NA_real_)

  return(finaldata)
}
