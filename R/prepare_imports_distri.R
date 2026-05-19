#' Prepare Import Distribution Data for a Reporting Period
#'
#' Reads a CSV file of port-level import distribution records, enriches it with
#' PSGC province codes derived from the API, aligns column names to the existing
#' `importsDistributionByPortQ1` schema, and prints a reconciliation summary
#' comparing total allocated volumes against actual import data.
#'
#' @param csv_file Character scalar. Path to a CSV file containing import
#'   distribution records.  Must include at least the columns `port_name`,
#'   `def_prov`, `def_prov_psgc`, `alloc_vol_mt`, `pre_import_vol_mt`,
#'   `post_import_vol_mt`, and `iteration`.
#' @param date A scalar coercible to `Date` via [lubridate::ymd()].  Used to
#'   populate the `as_of_date` column in the output (e.g. `"2026-06-30"`).
#'
#' @return A tibble with the same column set as the existing
#'   `importsDistributionByPortQ1` API table (excluding `id`, `created_at`, and
#'   `updated_at`).  Each row represents a port–province distribution record
#'   enriched with:
#'   \describe{
#'     \item{`fid`}{Integer row sequence.}
#'     \item{`as_of_date`}{Date derived from `date`.}
#'     \item{`period`}{Fixed string `"Quarter 2"`.}
#'     \item{`year`}{Fixed integer `2026`.}
#'     \item{`port_psgc`}{PSGC code of the port, joined from `riceImportsByStation`.}
#'     \item{`def_prov_psgc`}{Province PSGC code, joined from the SUA combined data.}
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Fetches existing distribution schema and import-by-station data from the
#'    DAC Lake API via `daclakeapi::get_api_data()`.
#' 2. Builds a port → PSGC lookup table, overriding known mismatches for
#'    SAN FERNANDO, MICP, and POM.
#' 3. Fetches SUA combined data (FIES, 2025) to obtain province PSGC codes,
#'    and derives an island-group flag (`LZN`, `VIS`, `MIN`).
#' 4. Reads `csv_file`, standardises `port_name` to upper-case and `def_prov`
#'    to lower-case (with a manual fix for `"Samar"`), then joins the port and
#'    province PSGC lookups.
#' 5. Prints a reconciliation table of total imports, total allocated volume,
#'    pre-distribution volume (iteration = 0), and post-distribution volume
#'    (max iteration).
#'
#' @examples
#' \dontrun{
#' result <- prepare_imports_distri(
#'   csv_file = "data/import_distri_q2_2026.csv",
#'   date     = "2026-06-30"
#' )
#' }
#'
#' @importFrom daclakeapi get_api_data
#' @importFrom dplyr distinct mutate arrange filter select left_join pull
#'   group_by ungroup rename if_else case_when n
#' @importFrom stringr str_detect str_to_upper str_to_lower
#' @importFrom readr read_csv
#' @importFrom lubridate ymd
#' @export
prepare_imports_distri <- function(csv_file, date) {

  existing_importDistri <- daclakeapi::get_api_data("importsDistributionByPortQ1")

  imports_data <- daclakeapi::get_api_data("riceImportsByStation")

  port_psgc <-
    imports_data |>
    dplyr::distinct(port_name, psgc_code) |>
    dplyr::mutate(
      psgc_code = dplyr::case_when(
        port_name == "SAN FERNANDO" ~ "0305416000",
        port_name == "MICP"         ~ "1380600001",
        port_name == "POM"          ~ "1380600002",
        .default = psgc_code
      )
    ) |>
    dplyr::distinct()

  psgcs <- daclakeapi::get_api_data(
    "suaCombined",
    filters = list(
      list(column = "percap_type", operator = "=", value = "FIES"),
      list(column = "year",        operator = "=", value = 2025)
    )
  ) |>
    dplyr::ungroup() |>
    dplyr::mutate(region_id = as.numeric(region_id)) |>
    dplyr::arrange(region_id, sub_id) |>
    dplyr::distinct(region_psgc, province_psgc, psgc_code, location, loc_type) |>
    dplyr::mutate(
      province_psgc = dplyr::if_else(is.na(province_psgc), psgc_code, province_psgc)
    ) |>
    dplyr::mutate(
      island_psgc = dplyr::case_when(
        stringr::str_detect(region_psgc, "^01|^02|^03|^04|^17|05|^14|^13") ~ "LZN",
        stringr::str_detect(region_psgc, "^06|^07|^08|^18")                ~ "VIS",
        region_psgc %in% c("PHL", "LZN", "VIS", "MIN")                    ~ region_psgc,
        .default = "MIN"
      )
    )

  import_distri <-
    readr::read_csv(csv_file) |>
    dplyr::mutate(
      port_name = stringr::str_to_upper(port_name),
      def_prov  = dplyr::case_when(
        def_prov == "Samar" ~ "samar (western samar)",
        .default = stringr::str_to_lower(def_prov)
      )
    ) |>
    dplyr::left_join(
      port_psgc |> dplyr::rename(port_psgc = psgc_code)
    ) |>
    dplyr::select(-def_prov_psgc) |>
    dplyr::left_join(
      psgcs |>
        dplyr::filter(loc_type == "Province") |>
        dplyr::mutate(location = stringr::str_to_lower(location)) |>
        dplyr::select(location, def_prov_psgc = province_psgc),
      by = c("def_prov" = "location")
    ) |>
    dplyr::mutate(
      fid         = 1:dplyr::n(),
      as_of_date  = lubridate::ymd(date),
      period      = "Quarter 2",
      year        = 2026
    ) |>
    dplyr::select(
      names(existing_importDistri |> dplyr::select(-c(id, created_at, updated_at)))
    )

  import_fromdata <-
    imports_data |>
    dplyr::filter(
      psgc_code      == "PHL",
      year           == 2026,
      as_of_date     == max(as_of_date),
      country_origin == "All Countries",
      month          %in% c("APRIL", "MAY", "JUNE")
    ) |>
    dplyr::pull(volume_imported) |>
    as.numeric() |>
    sum(na.rm = TRUE)

  import_distri_alloc <- sum(import_distri$alloc_vol_mt, na.rm = TRUE)

  import_distri_preimport <-
    import_distri |>
    dplyr::filter(iteration == 0) |>
    dplyr::pull(pre_import_vol_mt) |>
    sum(na.rm = TRUE)

  import_distri_postimport <-
    import_distri |>
    dplyr::group_by(port_name, port_psgc) |>
    dplyr::filter(iteration == max(iteration)) |>
    dplyr::pull(post_import_vol_mt) |>
    sum(na.rm = TRUE)

  cat(
    "=== IMPORT COMPARISONS ===",
    "\nTotal imports from Import data:           ", import_fromdata,
    "\nTotal imports distributed:                ", import_distri_alloc,
    "\nTotal imports from pre distri at iter=0:  ", import_distri_preimport,
    "\nTotal imports from post distri at max iter:", import_distri_postimport,
    "\n"
  )

  return(import_distri)
}
