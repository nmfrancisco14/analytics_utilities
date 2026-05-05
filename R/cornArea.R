#' Fetch Corn Area Data from the PSA PXWeb API
#'
#' Downloads and tidies corn cropped-area data from the Philippine Statistics
#' Authority (PSA) OpenStat PXWeb endpoint, and returns a tibble with a
#' computed period-end date column.
#'
#' The function issues a PXWeb query for corn (Ecosystem/Croptype = 5) across
#' all geolocations, years, and reporting periods; coerces the PXWeb result to
#' a data frame; strips leading/trailing dots from the `Geolocation` field;
#' renames the value column to `cornArea`; and derives a `date` column
#' representing the last calendar day of each reporting period.
#'
#' @return A tibble with at least the following columns:
#'   \describe{
#'     \item{`Geolocation`}{character — geolocation name (dots removed).}
#'     \item{`Year`}{character — reported year.}
#'     \item{`Period`}{character — reporting period (e.g. `"Quarter 1"`,
#'       `"Semester 1"`).}
#'     \item{`cornArea`}{numeric — reported corn area (hectares).}
#'     \item{`date`}{Date — last day of the reporting period.}
#'   }
#'
#' @details
#' Reporting periods are mapped to end-months as follows:
#' * Quarter 1 → March, Quarter 2 → June, Quarter 3 → September,
#'   Quarter 4 → December.
#' * Semester 1 → June, Semester 2 → December.
#'
#' The `date` value is the last calendar day of the corresponding month.
#'
#' @references
#' Philippine Statistics Authority. *Palay and Corn: Area Harvested in
#' Hectares by Ecosystem/Croptype, Quarter, Semester, Region and Province,
#' 1987–2025.* PSA OpenStat PXWeb API.
#' <https://openstat.psa.gov.ph/PXWeb/api/v1/en/DB/2E/CS/0022E4EAHC0.px>
#'
#' @examples
#' \dontrun{
#' df <- pxget_cornArea()
#' head(df)
#' }
#'
#' @importFrom pxweb pxweb_get
#' @importFrom lubridate ceiling_date make_date days
#' @importFrom dplyr mutate rename case_when
#' @importFrom stringr str_remove_all
#' @export
pxget_cornArea <- function() {
  cornArea_url <- "https://openstat.psa.gov.ph/PXWeb/api/v1/en/DB/2E/CS/0022E4EAHC0.px"

  cornArea_query <- list(
    "Ecosystem/Croptype" = c("5"),
    "Geolocation" = "0",
    "Year" = as.character(0:38),
    "Period" = as.character(0:6)
  )

  cornArea_pxdata <- pxweb::pxweb_get(
    url   = cornArea_url,
    query = cornArea_query
  )

  as.data.frame(cornArea_pxdata,
                column.name.type   = "text",
                variable.value.type = "text") |>
    dplyr::mutate(
      Geolocation = stringr::str_remove_all(Geolocation, "\\.+")
    ) |>
    dplyr::rename(cornArea = dplyr::last_col()) |>
    dplyr::mutate(
      date = lubridate::ceiling_date(
        lubridate::make_date(
          year  = as.integer(Year),
          month = dplyr::case_when(
            Period == "Quarter 1"  ~ 3L,
            Period == "Quarter 2"  ~ 6L,
            Period == "Quarter 3"  ~ 9L,
            Period == "Quarter 4"  ~ 12L,
            Period == "Semester 1" ~ 6L,
            Period == "Semester 2" ~ 12L,
            .default = NA_integer_
          ),
          day = 1L
        ),
        unit = "month"
      ) - lubridate::days(1)
    )
}
