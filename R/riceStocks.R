#' Fetch Rice Stocks Inventory from the PSA PXWeb API
#'
#' Downloads and tidies monthly rice stocks inventory data from the Philippine
#' Statistics Authority (PSA) OpenStat PXWeb endpoint, and returns a tibble
#' with a computed month-end date column.
#'
#' The function queries all sectors (0–3), a wide range of years (1980–2026),
#' and all months; uses `pxweb_advanced_get()` with metadata-based elimination
#' to handle the API's response structure; renames the value column to
#' `riceStocks`; and derives a `date` column for the last day of each reported
#' month.
#'
#' @return A tibble with at least the following columns:
#'   \describe{
#'     \item{`Sector`}{character — type of stock (e.g. Household, Commercial,
#'       Government/NFA, Total).}
#'     \item{`Year`}{character — reported year.}
#'     \item{`Month`}{character — reported month name (e.g. `"January"`).}
#'     \item{`riceStocks`}{numeric — rice stocks at end of month (metric tons).}
#'     \item{`date`}{Date — last calendar day of the reported month.}
#'   }
#'
#' @details
#' The function calls `pxweb::pxweb_advanced_get()` with an intermediate
#' metadata object to set `elimination = TRUE` on the Year variable, which is
#' required by this particular PSA endpoint.  The `date` column is derived by
#' matching `Month` against [base::month.name] and computing the month-end date
#' via [lubridate::ceiling_date()].
#'
#' @references
#' Philippine Statistics Authority. *Rice and Corn: Monthly Total Stocks
#' Inventory by Sector.* PSA OpenStat PXWeb API.
#' <https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2E/CS/0032E4ECNV0.px>
#'
#' @examples
#' \dontrun{
#' df <- pxget_riceStocks()
#' head(df)
#' }
#'
#' @importFrom pxweb pxweb_get pxweb_advanced_get
#' @importFrom lubridate ceiling_date make_date days
#' @importFrom dplyr mutate rename last_col
#' @export
pxget_riceStocks <- function() {
  riceStocks_url <- "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2E/CS/0032E4ECNV0.px"

  riceStocks_query <- list(
    "Sector" = as.character(0:3),
    "Year"   = as.character(1980:2026),
    "Month"  = as.character(0:11)
  )

  # Fetch metadata and enable elimination on Year so the API accepts the query
  riceStocks_pxmd <- pxweb::pxweb_get(riceStocks_url)
  riceStocks_pxmd$variables[[2]]$elimination <- TRUE

  riceStocks_pxdata <- pxweb::pxweb_advanced_get(
    url    = riceStocks_url,
    pxq    = riceStocks_query,
    pxmdo  = riceStocks_pxmd
  )

  as.data.frame(riceStocks_pxdata,
                column.name.type    = "text",
                variable.value.type = "text") |>
    dplyr::rename(riceStocks = dplyr::last_col()) |>
    dplyr::mutate(
      date = lubridate::ceiling_date(
        lubridate::make_date(
          year  = as.integer(Year),
          month = match(Month, month.name),
          day   = 1L
        ),
        unit = "month"
      ) - lubridate::days(1)
    )
}
