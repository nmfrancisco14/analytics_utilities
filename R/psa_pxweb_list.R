#' List of PSA OpenStat PXWeb API URLs
#'
#' Returns a named list of character URLs for the Philippine Statistics
#' Authority (PSA) OpenStat PXWeb API endpoints used by the `psatools`
#' package functions.
#'
#' @return A named list of character scalars:
#'   \describe{
#'     \item{`ricecorn_prod`}{Rice and corn volume of production.}
#'     \item{`ricecorn_area`}{Rice and corn area harvested.}
#'     \item{`ricecorn_stocks`}{Rice and corn monthly stocks inventory.}
#'     \item{`palay_fertPrices`}{Palay fertiliser prices.}
#'     \item{`ricecorn_fgateprices_oldseries`}{Farmgate prices, old series.}
#'     \item{`ricecorn_fgateprices_newseries`}{Farmgate prices, new series.}
#'   }
#'
#' @examples
#' urls <- psa_pxweb_urls()
#' urls$ricecorn_prod
#'
#' @export
psa_pxweb_urls <- function() {
  list(
    ricecorn_prod                  = "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2E/CS/0012E4EVCP0.px",
    ricecorn_area                  = "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2E/CS/0022E4EAHC0.px",
    ricecorn_stocks                = "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2E/CS/0032E4ECNV0.px",
    palay_fertPrices               = "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2E/CS/0042E4EPFU0.px",
    ricecorn_fgateprices_oldseries = "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2M/FG/0032M4AFP01.px",
    ricecorn_fgateprices_newseries = "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2M/NFG/0032M4AFN01.px"
  )
}
