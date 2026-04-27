#' Read a SAS XPT file (v5 or v8)
#'
#' Reads a SAS Transport (XPORT) file and returns a data frame with all
#' variable metadata preserved as attributes. Supports both XPT version 5
#' (column names <= 8 characters) and version 8 (column names <= 32 characters).
#'
#' @param path Character. Path to the `.xpt` file.
#' @param col_select Optional character vector of column names to return. If
#'   `NULL` (default), all columns are returned.
#' @param apply_formats Logical. If `TRUE`, columns with a `DATE`, `DATETIME`,
#'   or `TIME` format are converted to R Date/POSIXct/hms classes. Default
#'   `TRUE`.
#' @param encoding Character. String encoding of character variables. Default
#'   `"UTF-8"`.
#'
#' @return A `data.frame` with the following extra attributes:
#'   \describe{
#'     \item{`var_labels`}{Named character vector of variable labels.}
#'     \item{`var_formats`}{Named character vector of SAS format strings.}
#'     \item{`var_lengths`}{Named integer vector of variable byte lengths.}
#'     \item{`dataset_name`}{Character. SAS dataset name from the file header.}
#'     \item{`dataset_label`}{Character. SAS dataset label.}
#'   }
#'
#' @examples
#' \dontrun{
#' dm <- read_xpt("path/to/dm.xpt")
#' attr(dm, "var_labels")
#' }
#'
#' @seealso [write_xpt()], [read_sas()], [apply_spec()]
#' @export
read_xpt <- function(path,
                     col_select    = NULL,
                     apply_formats = TRUE,
                     encoding      = "UTF-8") {
  path <- normalizePath(path, mustWork = TRUE)
  if (!grepl("\\.xpt$", path, ignore.case = TRUE)) {
    warning("File does not have an .xpt extension: ", path)
  }

  df <- cx_read_xpt(path)

  # Subset columns
  if (!is.null(col_select)) {
    keep <- intersect(col_select, names(df))
    if (length(keep) == 0L) stop("None of the requested columns found in ", path)
    meta_attrs <- c("var_labels", "var_formats", "var_lengths", "dataset_name", "dataset_label")
    saved <- lapply(meta_attrs, function(a) attr(df, a))
    df <- df[, keep, drop = FALSE]
    for (j in seq_along(meta_attrs)) {
      v <- saved[[j]]
      if (!is.null(names(v))) v <- v[names(v) %in% keep]
      attr(df, meta_attrs[j]) <- v
    }
  }

  # Apply format conversions
  if (isTRUE(apply_formats)) {
    fmts <- attr(df, "var_formats")
    if (!is.null(fmts)) {
      df <- .apply_sas_formats(df, fmts)
    }
  }

  df
}
