#' Compute the Number of Days in a Reporting Period
#'
#' Given a date and a reporting period type, returns the total number of calendar
#' days that fall within that period (month, quarter, semester, or year).
#'
#' @param date_val A scalar that can be coerced to `Date` via [base::as.Date()].
#'   The year and month components of this value are used to identify the
#'   enclosing period.
#' @param period_type_val Character scalar; one of `"Annual"`, `"Semestral"`,
#'   `"Quarterly"`, or `"Monthly"`. Case-sensitive.
#'
#' @return An integer scalar: the number of calendar days in the specified
#'   reporting period.  If `period_type_val` does not match any of the four
#'   recognised values, the function returns `365` as a fallback.
#'
#' @details
#' * **Annual** — the full calendar year of `date_val`, accounting for leap years
#'   (366 days) and common years (365 days).
#' * **Semestral** — the first semester runs 1 January – 30 June (181 or 182 days);
#'   the second semester runs 1 July – 31 December (184 days).
#' * **Quarterly** — the three-month calendar quarter containing `date_val`
#'   (Q1: Jan–Mar, Q2: Apr–Jun, Q3: Jul–Sep, Q4: Oct–Dec).
#' * **Monthly** — the calendar month containing `date_val`.
#'
#' @examples
#' # Leap-year annual
#' calculate_period_days("2024-07-15", "Annual")   # 366
#'
#' # Common-year annual
#' calculate_period_days("2023-01-01", "Annual")   # 365
#'
#' # Q1 of a non-leap year
#' calculate_period_days("2023-02-28", "Quarterly") # 90
#'
#' # Month of February in a leap year
#' calculate_period_days("2024-02-10", "Monthly")   # 29
#'
#' @importFrom lubridate year month
#' @export
calculate_period_days <- function(date_val, period_type_val) {
  date_val  <- as.Date(date_val)
  year_val  <- lubridate::year(date_val)
  month_val <- lubridate::month(date_val)

  if (period_type_val == "Annual") {
    return(ifelse(
      (year_val %% 4 == 0 & year_val %% 100 != 0) | (year_val %% 400 == 0),
      366L, 365L
    ))
  }

  if (period_type_val == "Semestral") {
    semester <- ifelse(month_val <= 6, 1, 2)
    if (semester == 1) {
      start_date <- as.Date(paste0(year_val, "-01-01"))
      end_date   <- as.Date(paste0(year_val, "-06-30"))
    } else {
      start_date <- as.Date(paste0(year_val, "-07-01"))
      end_date   <- as.Date(paste0(year_val, "-12-31"))
    }
    return(as.integer(end_date - start_date) + 1L)
  }

  if (period_type_val == "Quarterly") {
    quarter     <- ceiling(month_val / 3)
    start_month <- (quarter - 1L) * 3L + 1L
    end_month   <- start_month + 2L
    start_date  <- as.Date(paste0(year_val, "-", sprintf("%02d", start_month), "-01"))
    if (end_month == 12L) {
      end_date <- as.Date(paste0(year_val, "-12-31"))
    } else {
      end_date <- as.Date(paste0(year_val, "-", sprintf("%02d", end_month + 1L), "-01")) - 1L
    }
    return(as.integer(end_date - start_date) + 1L)
  }

  if (period_type_val == "Monthly") {
    start_date <- as.Date(paste0(year_val, "-", sprintf("%02d", month_val), "-01"))
    if (month_val == 12L) {
      end_date <- as.Date(paste0(year_val, "-12-31"))
    } else {
      end_date <- as.Date(paste0(year_val, "-", sprintf("%02d", month_val + 1L), "-01")) - 1L
    }
    return(as.integer(end_date - start_date) + 1L)
  }

  return(365L)  # fallback
}
