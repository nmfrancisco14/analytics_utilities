#' Return the Last Day of Each Month in a Date Vector
#'
#' Computes the last calendar day of the month for each element of a `Date`
#' (or date-time) vector using [lubridate::ceiling_date()] minus one day.
#'
#' @param dateX A `Date`, `POSIXct`, or `POSIXlt` vector (scalar or length > 1).
#'
#' @return A `Date` vector of the same length as `dateX`, where each element is
#'   the last day of the corresponding month (e.g. `2024-01-15` → `2024-01-31`).
#'
#' @examples
#' end_of_month(as.Date("2024-02-10"))  # 2024-02-29 (leap year)
#' end_of_month(as.Date("2023-02-10"))  # 2023-02-28
#' end_of_month(Sys.Date())
#'
#' @importFrom lubridate ceiling_date days
#' @export
end_of_month <- function(dateX) {
  lubridate::ceiling_date(dateX, unit = "months") - lubridate::days(1)
}
