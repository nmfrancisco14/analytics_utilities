#' Retrieve PRiSM Production, Area, and Yield Records from a Supabase REST API
#'
#' Queries a Supabase REST endpoint for PRiSM Production, Area, and Yield
#' (PAY) data, filters rows by a location value and by valid English month
#' names, and optionally appends a month-end `date` column.
#'
#' @param supabase_url Character. Base Supabase project URL (e.g.
#'   `"https://<project-ref>.supabase.co"`).  Do **not** include the
#'   `/rest/v1` suffix.
#' @param supabase_key Character. Supabase `anon` or `service_role` API key
#'   used for the `apikey` and `Authorization: Bearer` headers.
#' @param table Character. REST table to query. Default: `"pay_prism"`.
#' @param location_field Character. Column name to filter by location.
#'   Default: `"location"`.
#' @param location_value Character. Value to match in `location_field`.
#'   Default: `"Philippines"`.
#' @param period_col Character. Column containing month names (e.g.
#'   `"January"`). Default: `"PERIOD"`.
#' @param year_col Character. Column containing the integer year. Default:
#'   `"YEAR"`.
#' @param parse_month_end Logical. If `TRUE` (default), append a `date` column
#'   with the last day of the month derived from `year_col` and `period_col`.
#' @param verbose Logical. If `TRUE`, print the request URL to the console.
#'   Default: `FALSE`.
#'
#' @return A tibble of the parsed JSON response, filtered to rows where
#'   `period_col` is one of [base::month.name].  If `parse_month_end = TRUE`
#'   and the result is non-empty, a `date` column of class `Date` is appended.
#'
#' @details
#' The function constructs a PostgREST equality filter (`?<field>=eq.<value>`)
#' and sets the required Supabase authentication headers.  An HTTP error will
#' cause [httr::stop_for_status()] to stop with an informative message.
#'
#' @examples
#' \dontrun{
#' df <- sbget_payPrism(
#'   supabase_url   = "https://xyzabc.supabase.co",
#'   supabase_key   = Sys.getenv("SUPABASE_KEY"),
#'   location_value = "Philippines"
#' )
#' head(df)
#' }
#'
#' @importFrom httr GET add_headers stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble filter mutate
#' @importFrom lubridate make_date ceiling_date days
#' @importFrom utils URLencode
#' @importFrom rlang .data
#' @export
sbget_payPrism <- function(
    supabase_url,
    supabase_key,
    table           = "pay_prism",
    location_field  = "location",
    location_value  = "Philippines",
    period_col      = "PERIOD",
    year_col        = "YEAR",
    parse_month_end = TRUE,
    verbose         = FALSE
) {
  stopifnot(is.character(supabase_url), length(supabase_url) == 1L)
  stopifnot(is.character(supabase_key), length(supabase_key) == 1L)

  location_enc <- utils::URLencode(location_value, reserved = TRUE)
  query_url <- paste0(
    supabase_url, "/rest/v1/", table,
    "?", location_field, "=eq.", location_enc
  )

  if (verbose) message("GET ", query_url)

  resp <- httr::GET(
    query_url,
    httr::add_headers(
      apikey        = supabase_key,
      Authorization = paste("Bearer", supabase_key)
    )
  )
  httr::stop_for_status(resp)

  df <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    flatten = TRUE
  ) |>
    dplyr::as_tibble() |>
    dplyr::filter(.data[[period_col]] %in% month.name)

  if (parse_month_end && nrow(df) > 0L) {
    df <- df |>
      dplyr::mutate(
        date = lubridate::make_date(
          year  = as.integer(.data[[year_col]]),
          month = match(.data[[period_col]], month.name),
          day   = 1L
        ) |>
          lubridate::ceiling_date(unit = "month") - lubridate::days(1)
      )
  }

  df
}
