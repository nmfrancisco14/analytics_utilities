#' Standardise Philippine Administrative Region and Province Names
#'
#' Coerces a character vector of Philippine geolocation names to a canonical
#' form used consistently across PSA datasets.  The matching is
#' case-insensitive and collapses internal whitespace before comparing, so
#' common typographical variants (Roman/Arabic numerals, abbreviated names,
#' alternative spellings) are all resolved to the same canonical string.
#'
#' @param variable A character vector of location names to standardise.
#'
#' @return A character vector of the same length as `variable`.  Recognised
#'   names are replaced by their canonical form (e.g. `"Region I (Ilocos Region)"`).
#'   Unrecognised names are returned as `stringr::str_to_lower(variable)` (i.e.
#'   lowercased but otherwise unchanged).
#'
#' @details
#' Canonical region names follow the PSA OpenStat convention.  The lookup table
#' covers all 18 administrative regions (including NCR, CAR, NIR, and BARMM),
#' plus a selection of frequently mis-spelled or renamed provinces.
#'
#' @examples
#' clean_location(c("region 1", "NCR", "CARAGA", "davao norte"))
#'
#' @importFrom dplyr case_match
#' @importFrom stringr str_to_lower str_squish
#' @export
clean_location <- function(variable) {
  dplyr::case_match(
    stringr::str_to_lower(stringr::str_squish(variable)),
    c("region i - ilocos region", "region i", "ilocos", "region 1",
      "ilocos region", "region i (ilocos region)", "region 1 (ilocos region)")
      ~ "Region I (Ilocos Region)",
    c("region ii", "region 2", "cagayan valley", "region ii (cagayan valley)",
      "region 2 (cagayan valley)", "region ii - cagayan valley")
      ~ "Region II (Cagayan Valley)",
    c("region iii", "region 3", "central luzon", "region iii (central luzon)",
      "region 3 (central luzon)", "region iii - central luzon")
      ~ "Region III (Central Luzon)",
    c("region iv-a", "southern tagalog", "region 4a", "calabarzon",
      "region iv-a (calabarzon)", "region 4-a (calabarzon)")
      ~ "Region IV-A (CALABARZON)",
    c("region v", "bicol", "region 5", "bicol region",
      "region v (bicol region)", "region 5 (bicol region)",
      "region v - bicol region")
      ~ "Region V (Bicol Region)",
    c("region vi", "region 6", "western visayas", "region vi (western visayas)",
      "region 6 (western visayas)", "region vi - western visayas")
      ~ "Region VI (Western Visayas)",
    c("region vii", "region 7", "central visayas",
      "region vii (central visayas)", "region 7 (central visayas)",
      "region vii - central visayas")
      ~ "Region VII (Central Visayas)",
    c("region viii", "region 8", "eastern visayas",
      "region viii (eastern visayas)", "region 8 (eastern visayas)",
      "region viii - eastern visayas")
      ~ "Region VIII (Eastern Visayas)",
    c("region ix", "region 9", "zamboanga peninsula",
      "region ix (zamboanga peninsula)", "region 9 (zamboanga peninsula)",
      "region ix - western mindanao")
      ~ "Region IX (Zamboanga Peninsula)",
    c("region x", "region 10", "northern mindanao",
      "region x (northern mindanao)", "region 10 (northern mindanao)",
      "region x - northern mindanao")
      ~ "Region X (Northern Mindanao)",
    c("region xi", "region 11", "davao region", "region xi (davao region)",
      "region 11 (davao region)", "region xi - southern mindanao")
      ~ "Region XI (Davao Region)",
    c("region xii", "region 12", "soccsksargen",
      "region xii (soccsksargen)", "region 12 (soccsksargen)",
      "region xii - central mindanao")
      ~ "Region XII (SOCCSKSARGEN)",
    c("region xiii", "region 13", "caraga", "region xiii (caraga)",
      "region 13 (caraga region)", "region xiii (caraga) ",
      "region- caraga")
      ~ "Region XIII (CARAGA)",
    c("region iv-b", "mimaropa", "region iv-b (mimaropa)", "mimaropa region",
      "region 4-b (mimaropa region)")
      ~ "Region IV-B (MIMAROPA)",
    c("cordillera administrative region (car)", "car",
      "cordillera administrative region")
      ~ "Cordillera Administrative Region (CAR)",
    c("negros island region", "negros island region (nir)", "nir")
      ~ "Negros Island Region (NIR)",
    c("ncr", "national capital region (ncr)")
      ~ "National Capital Region (NCR)",
    c("autonomous region in muslim mindanao (armm)", "armm", "barmm",
      "bangsamoro autonomous region in muslim mindanao (barmm)",
      "bangsamoro autonomous region in muslim mindanao", "region- barmm")
      ~ "Bangsamoro Autonomous Region In Muslim Mindanao (BARMM)",
    c("city of davao", "davao city")           ~ "davao city",
    c("cotabato (north cotabato)", "north cotabato") ~ "north cotabato",
    c("compostela valley", "davao de oro",
      "davao de oro (compostela valley)")       ~ "davao de oro (compostela valley)",
    c("western samar", "samar", "samar (western samar")  ~ "samar (western samar)",
    c("maguindanao (excluding cotabato city)")  ~ "maguindanao",
    c("basilan (excluding city of isabela)")    ~ "basilan",
    c("agusan norte")     ~ "agusan del norte",
    c("agusan sur")       ~ "agusan del sur",
    c("davao norte")      ~ "davao del norte",
    c("davao sur")        ~ "davao del sur",
    c("lanao norte")      ~ "lanao del norte",
    c("lanao sur")        ~ "lanao del sur",
    c("mt. province")     ~ "mountain province",
    c("mindoro occidental") ~ "occidental mindoro",
    c("mindoro oriental")   ~ "oriental mindoro",
    c("surigao norte")    ~ "surigao del norte",
    c("surigao sur")      ~ "surigao del sur",
    c("zamboanga norte")  ~ "zamboanga del norte",
    c("zamboanga sur")    ~ "zamboanga del sur",
    .default = stringr::str_to_lower(variable)
  )
}
