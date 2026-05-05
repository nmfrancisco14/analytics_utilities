#' analyticsutilities: Analytics Utility Functions
#'
#' A collection of utility functions for Philippine agricultural and
#' geospatial data analytics, including PSA PXWeb API wrappers, Supabase
#' REST helpers, PSGC geographic table builders, time-series extension,
#' and location-name standardisation.
#'
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check NOTEs for NSE column names used across the package
utils::globalVariables(c(
  # pxget_* functions (PXWeb data frames)
  "Geolocation", "Year", "Period", "Month",
  # get_psgc_pop2024 (janitor-cleaned names)
  "name", "old_names", "geographic_level", "x10_digit_psgc",
  "correspondence_code", "x2024_population", "city_class",
  "income_classification_dof_do_no_074_2024",
  "prov_psgc", "reg_psgc", "reg_ccode", "region",
  "province", "province_oldname", "prov_ccode",
  "municity_psgc", "municity_ccode", "municity", "municity_oldname",
  "urban_rural_based_on_2020_cph", "brgy_psgc", "brgy",
  # rlang walrus / tidy-eval
  ":="
))
