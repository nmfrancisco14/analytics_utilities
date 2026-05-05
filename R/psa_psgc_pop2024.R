#' Build Tidy PSGC 2024 Geographic Tables from a PSA Excel File
#'
#' Reads the official Philippine Standard Geographic Code (PSGC) Excel workbook
#' published by the Philippine Statistics Authority (PSA) and returns a named
#' list of four tidy tibbles: regions, provinces/HUCs, municipalities/cities,
#' and barangays — each enriched with parent-level codes and names.
#'
#' @param file_path Character scalar. Path to the PSGC Excel workbook (the
#'   sheet named `"PSGC"` is used).  The file should be the official PSA
#'   publication dated 30 September 2025 or later.
#'
#' @return A named list with four tibbles:
#'   \describe{
#'     \item{`regions`}{Region-level records with columns `reg_psgc`,
#'       `reg_ccode`, `region`, `x2024_population`.}
#'     \item{`provinces`}{Province/HUC records joined to their parent region.}
#'     \item{`municities`}{Municipality/city/sub-municipality records joined
#'       to their parent region and province.}
#'     \item{`barangays`}{Barangay records joined to their parent region,
#'       province, and municipality/city.}
#'   }
#'
#' @details
#' The function uses [openxlsx::read.xlsx()] to read the workbook and
#' [janitor::clean_names()] to snake-case column names.  Parent codes are
#' derived by slicing the 10-digit PSGC code:
#' * Region code — first 2 digits padded to 10 digits with zeros.
#' * Province code — first 5 digits padded to 10 digits with zeros.
#' * Municipality code — first 7 digits padded to 10 digits with zeros.
#'
#' Highly Urbanised Cities (HUCs) are treated as province-equivalent units and
#' are included in the `provinces` table.
#'
#' @references
#' Philippine Statistics Authority. *Philippine Standard Geographic Code
#' (PSGC), Publication Date: 30 September 2025.*
#' <https://psa.gov.ph/classification/psgc>
#'
#' @examples
#' \dontrun{
#' psgc <- get_psgc_pop2024("path/to/psgc_pop2024.xlsx")
#' head(psgc$regions)
#' head(psgc$provinces)
#' }
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom janitor clean_names
#' @importFrom dplyr filter select mutate left_join relocate if_else
#' @importFrom stringr str_to_lower str_squish str_sub
#' @export
get_psgc_pop2024 <- function(file_path) {
  stopifnot(is.character(file_path), length(file_path) == 1L, file.exists(file_path))

  psgc_pop2024 <- openxlsx::read.xlsx(file_path, sheet = "PSGC") |>
    janitor::clean_names() |>
    dplyr::mutate(
      name      = stringr::str_to_lower(stringr::str_squish(name)),
      old_names = stringr::str_to_lower(stringr::str_squish(old_names))
    )

  # ---- Helper: pad PSGC prefix with trailing zeros -------------------------
  pad_psgc <- function(x, prefix_len) {
    paste0(stringr::str_sub(x, 1, prefix_len),
           strrep("0", 10L - prefix_len))
  }

  # ---- Regions ---------------------------------------------------------------
  reg_psgc_pop <- psgc_pop2024 |>
    dplyr::filter(geographic_level == "Reg") |>
    dplyr::select(
      reg_psgc       = x10_digit_psgc,
      reg_ccode      = correspondence_code,
      region         = name,
      x2024_population
    )

  # ---- Provinces / HUCs -----------------------------------------------------
  prov_psgc_pop <- psgc_pop2024 |>
    dplyr::filter(
      geographic_level == "Prov" |
        (geographic_level == "City" & city_class == "HUC")
    ) |>
    dplyr::mutate(
      class = dplyr::if_else(geographic_level == "City", "City (HUC)",
                             geographic_level)
    ) |>
    dplyr::select(
      prov_psgc        = x10_digit_psgc,
      prov_ccode       = correspondence_code,
      province         = name,
      province_oldname = old_names,
      prov_type        = class,
      income_class     = income_classification_dof_do_no_074_2024,
      x2024_population
    ) |>
    dplyr::mutate(reg_psgc = pad_psgc(prov_psgc, 2L)) |>
    dplyr::left_join(
      dplyr::select(reg_psgc_pop, -x2024_population),
      by = "reg_psgc"
    ) |>
    dplyr::relocate(reg_psgc, reg_ccode, .before = 1L) |>
    dplyr::relocate(region, .before = province)

  # ---- Municipalities / cities ----------------------------------------------
  muni_psgc_pop <- psgc_pop2024 |>
    dplyr::filter(geographic_level %in% c("City", "Mun", "SubMun")) |>
    dplyr::select(
      municity_psgc    = x10_digit_psgc,
      municity_ccode   = correspondence_code,
      municity         = name,
      municity_oldname = old_names,
      city_type        = geographic_level,
      city_class,
      income_class     = income_classification_dof_do_no_074_2024,
      x2024_population
    ) |>
    dplyr::mutate(
      reg_psgc  = pad_psgc(municity_psgc, 2L),
      prov_psgc = pad_psgc(municity_psgc, 5L)
    ) |>
    dplyr::left_join(
      dplyr::select(reg_psgc_pop, -x2024_population),
      by = "reg_psgc"
    ) |>
    dplyr::left_join(
      dplyr::select(prov_psgc_pop, prov_psgc, prov_ccode, province,
                    province_oldname),
      by = "prov_psgc"
    ) |>
    dplyr::relocate(reg_psgc, reg_ccode, prov_psgc, prov_ccode, .before = 1L) |>
    dplyr::relocate(region, province, province_oldname, .before = municity)

  # ---- Barangays ------------------------------------------------------------
  brgy_psgc_pop <- psgc_pop2024 |>
    dplyr::filter(geographic_level == "Bgy") |>
    dplyr::select(
      brgy_psgc    = x10_digit_psgc,
      brgy_ccode   = correspondence_code,
      brgy         = name,
      brgy_oldname = old_names,
      brgy_class   = urban_rural_based_on_2020_cph,
      x2024_population
    ) |>
    dplyr::mutate(
      reg_psgc      = pad_psgc(brgy_psgc, 2L),
      prov_psgc     = pad_psgc(brgy_psgc, 5L),
      municity_psgc = pad_psgc(brgy_psgc, 7L)
    ) |>
    dplyr::left_join(
      dplyr::select(reg_psgc_pop, -x2024_population),
      by = "reg_psgc"
    ) |>
    dplyr::left_join(
      dplyr::select(prov_psgc_pop, prov_psgc, prov_ccode, province,
                    province_oldname),
      by = "prov_psgc"
    ) |>
    dplyr::left_join(
      dplyr::select(muni_psgc_pop, municity_psgc, municity_ccode, municity,
                    municity_oldname),
      by = "municity_psgc"
    ) |>
    dplyr::relocate(reg_psgc, reg_ccode, prov_psgc, prov_ccode,
                    municity_psgc, municity_ccode, .before = 1L) |>
    dplyr::relocate(region, province, province_oldname, municity,
                    municity_oldname, .before = brgy)

  list(
    regions    = reg_psgc_pop,
    provinces  = prov_psgc_pop,
    municities = muni_psgc_pop,
    barangays  = brgy_psgc_pop
  )
}
