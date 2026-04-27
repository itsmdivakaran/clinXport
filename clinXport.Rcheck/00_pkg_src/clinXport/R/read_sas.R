#' Read a SAS7BDAT file
#'
#' Reads a SAS7BDAT binary file and returns a data frame with all variable
#' metadata (labels, formats, lengths) preserved as attributes. Uses a
#' built-in C++ parser; does not require SAS, \pkg{haven}, or \pkg{xportr}.
#'
#' @param path Character. Path to the `.sas7bdat` file.
#' @param col_select Optional character vector of column names to return.
#' @param apply_formats Logical. Convert DATE/DATETIME/TIME formatted columns
#'   to R Date/POSIXct classes. Default `TRUE`.
#'
#' @return A `data.frame` with attributes `var_labels`, `var_formats`,
#'   `var_lengths`, `dataset_name`.
#'
#' @note
#' The built-in reader supports standard SAS7BDAT files with uncompressed or
#' RLE-compressed data pages (SAS9 on Linux/Windows, little-endian). Files
#' created by SAS on mainframe (EBCDIC, big-endian IBM float) are also
#' handled. Bitmask-compressed pages are not currently supported.
#'
#' @examples
#' \dontrun{
#' adsl <- read_sas("path/to/adsl.sas7bdat")
#' }
#'
#' @seealso [write_sas()], [read_xpt()]
#' @export
read_sas <- function(path,
                     col_select    = NULL,
                     apply_formats = TRUE) {
  path <- normalizePath(path, mustWork = TRUE)
  df   <- cx_read_sas7bdat(path)

  if (!is.null(col_select)) {
    keep <- intersect(col_select, names(df))
    if (length(keep) == 0L) stop("None of the requested columns found.")
    meta <- c("var_labels", "var_formats", "var_lengths", "dataset_name")
    saved <- lapply(meta, function(a) attr(df, a))
    df <- df[, keep, drop = FALSE]
    for (j in seq_along(meta)) {
      v <- saved[[j]]
      if (!is.null(names(v))) v <- v[names(v) %in% keep]
      attr(df, meta[j]) <- v
    }
  }

  if (isTRUE(apply_formats)) {
    fmts <- attr(df, "var_formats")
    if (!is.null(fmts)) df <- .apply_sas_formats(df, fmts)
  }

  df
}
