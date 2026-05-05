#' Import a File from Google Drive
#'
#' Downloads a file from Google Drive (by share link or file ID) to a temporary
#' path and reads it into R.  Supports CSV, Excel (`.xlsx`), and RDS formats.
#'
#' @param file_link Character scalar.  A Google Drive share link or file ID
#'   accepted by [googledrive::as_id()].
#' @param authorize Logical.  If `TRUE` (default), authenticate via
#'   [googledrive::drive_auth()].  Set to `FALSE` to use a previously cached
#'   token or public files via [googledrive::drive_deauth()].
#' @param email Character scalar or `NULL`.  Google account email passed to
#'   [googledrive::drive_auth()].  Ignored when `authorize = FALSE`.
#' @param ... Additional arguments forwarded to the underlying reader function
#'   ([readr::read_csv()], [openxlsx::read.xlsx()], or [base::readRDS()]).
#'
#' @return The parsed file contents:
#'   * `.csv` → a tibble (via [readr::read_csv()])
#'   * `.xlsx` → a data frame (via [openxlsx::read.xlsx()])
#'   * `.rds` → the object stored in the RDS file
#'
#' @details
#' The file is streamed to a temporary file (cleaned up on function exit via
#' [base::on.exit()]).  File type is determined from the Drive file's name
#' extension.  Unsupported extensions raise an error.
#'
#' @examples
#' \dontrun{
#' df <- import_from_gdrive(
#'   file_link = "https://drive.google.com/file/d/FILEID/view?usp=sharing",
#'   email     = "you@example.com"
#' )
#' }
#'
#' @importFrom googledrive drive_auth drive_deauth as_id drive_get drive_download
#' @importFrom readr read_csv
#' @importFrom openxlsx read.xlsx
#' @export
import_from_gdrive <- function(file_link, authorize = TRUE, email = NULL, ...) {
    if (authorize) {
        googledrive::drive_auth(email = email)
    } else {
        googledrive::drive_deauth()
    }

    file_id <- googledrive::as_id(file_link)
    file_meta <- googledrive::drive_get(file_id)
    file_name <- file_meta$name[[1]]
    file_ext <- tolower(tools::file_ext(file_name))

    tmp_path <- tempfile(fileext = paste0(".", file_ext))
    on.exit(unlink(tmp_path), add = TRUE)

    googledrive::drive_download(
        file = file_id,
        path = tmp_path,
        overwrite = TRUE,
        verbose = FALSE
    )

    switch(
        file_ext,
        csv = readr::read_csv(tmp_path, ...),
        xlsx = openxlsx::read.xlsx(tmp_path, ...),
        rds = readRDS(tmp_path),
        stop("Unsupported file type. Use csv, xlsx, or rds.")
    )
}