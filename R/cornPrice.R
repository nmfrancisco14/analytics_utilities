#' Fetch Farmgate Corn Price Data from the PSA PXWeb API
#'
#' Downloads and tidies farmgate corn price (PhP/kg) data from the Philippine
#' Statistics Authority (PSA) OpenStat PXWeb endpoint, combining the old and
#' new price series, and returns a tibble with a computed period-end date column.
#'
#' The function queries two PSA PXWeb endpoints (old series up to 2009; new
#' series from 2010 onwards) for yellow-matured corn prices across all
#' geolocations and reporting months; combines them via a full join; strips
#' dots from `Geolocation`; renames the value column to `cornPrice`; and
#' derives a `date` column representing the last calendar day of each
#' reporting month.
#'
#' @return A tibble with at least the following columns:
#'   \describe{
#'     \item{`Geolocation`}{character — geolocation name (dots removed).}
#'     \item{`Year`}{character — reported year.}
#'     \item{`Period`}{character — reporting month name (e.g. `"January"`).}
#'     \item{`cornPrice`}{numeric — reported farmgate corn price (PhP/kg).}
#'     \item{`date`}{Date — last day of the reporting month.}
#'   }
#'
#' @details
#' The old series covers 1979–2009; the new series covers 2010 onwards.  Rows
#' from the old series with `Year >= 2010` are dropped before joining to avoid
#' duplication.  The `date` value is the last calendar day of the month named
#' in `Period`.
#'
#' @references
#' Philippine Statistics Authority. *Farmgate Prices of Palay and Corn
#' (Old Series, 1979–2009).* PSA OpenStat PXWeb API.
#' <https://openstat.psa.gov.ph/PXWeb/api/v1/en/DB/2M/FG/0032M4AFP01.px>
#'
#' Philippine Statistics Authority. *Farmgate Prices of Palay and Corn
#' (New Series, 2010–present).* PSA OpenStat PXWeb API.
#' <https://openstat.psa.gov.ph/PXWeb/api/v1/en/DB/2M/NFG/0032M4AFN01.px>
#'
#' @examples
#' \dontrun{
#' df <- pxget_cornPrice()
#' head(df)
#' }
#'
#' @importFrom pxweb pxweb_get
#' @importFrom lubridate ceiling_date make_date days
#' @importFrom dplyr mutate rename filter full_join last_col
#' @importFrom stringr str_remove_all
#' @export
pxget_cornPrice <- function() {
  # ---- Old series (1979–2009) -----------------------------------------------
  cornprice_url_old <- "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2M/FG/0032M4AFP01.px"

  cornprice_query_old <- list(
    "Geolocation" = c("0"),
    "Commodity"   = c("2"),            # yellow matured
    "Year"        = as.character(0:30),
    "Period"      = as.character(0:12)
  )

  # ---- New series (2010–present) --------------------------------------------
  cornprice_url_new <- "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2M/NFG/0032M4AFN01.px"

  cornprice_query_new <- list(
    "Geolocation" = c("000000000"),
    "Commodity"   = "3",               # yellow matured
    "Year"        = as.character(0:15),
    "Period"      = as.character(0:12)
  )

  # ---- Fetch ----------------------------------------------------------------
  cornprice_pxdata_old <- pxweb::pxweb_get(url = cornprice_url_old,
                                            query = cornprice_query_old)
  cornprice_pxdata_new <- pxweb::pxweb_get(url = cornprice_url_new,
                                            query = cornprice_query_new)

  # ---- Combine and tidy -----------------------------------------------------
  old_df <- as.data.frame(cornprice_pxdata_old,
                           column.name.type    = "text",
                           variable.value.type = "text") |>
    dplyr::filter(as.integer(Year) < 2010L) |>
    dplyr::rename(cornPrice = dplyr::last_col())

  new_df <- as.data.frame(cornprice_pxdata_new,
                           column.name.type    = "text",
                           variable.value.type = "text") |>
    dplyr::rename(cornPrice = dplyr::last_col())

  dplyr::full_join(old_df, new_df) |>
    dplyr::mutate(
      Geolocation = stringr::str_remove_all(Geolocation, "\\.+"),
      date = lubridate::ceiling_date(
        lubridate::make_date(
          year  = as.integer(Year),
          month = match(Period, month.name),
          day   = 1L
        ),
        unit = "month"
      ) - lubridate::days(1)
    )
}
