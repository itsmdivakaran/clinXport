#' Write a data frame to SAS7BDAT format
#'
#' Creates an uncompressed SAS7BDAT file compatible with SAS 9.4 and later.
#' Variable metadata (labels, SAS formats, byte lengths) is embedded in the
#' file's metadata subheaders so that the file round-trips cleanly through SAS
#' or [read_sas()].
#'
#' @param df            A `data.frame` to export.
#' @param path          Character. Output file path (`.sas7bdat`).
#' @param dataset_name  Character. SAS dataset name (<= 64 chars).
#' @param var_labels    Named character vector of variable labels.
#' @param var_formats   Named character vector of SAS format strings.
#' @param var_lengths   Named integer vector of character column byte lengths.
#'
#' @return Invisibly returns `path`.
#'
#' @note
#' Only uncompressed data is written. If you need PROC COMPRESS or CHAR/BINARY
#' compression, export to XPT and convert inside SAS.
#'
#' @examples
#' \dontrun{
#' write_sas(adsl, "adsl.sas7bdat",
#'           dataset_name = "ADSL",
#'           var_labels   = c(USUBJID = "Unique Subject ID", AGE = "Age"))
#' }
#'
#' @seealso [read_sas()], [write_xpt()]
#' @export
write_sas <- function(df,
                      path,
                      dataset_name = NULL,
                      var_labels   = NULL,
                      var_formats  = NULL,
                      var_lengths  = NULL) {
  stopifnot(is.data.frame(df))

  if (is.null(dataset_name)) {
    dataset_name <- toupper(deparse(substitute(df)))
    dataset_name <- gsub("[^A-Z0-9_]", "", dataset_name)
    if (nchar(dataset_name) == 0L) dataset_name <- "DATASET"
    dataset_name <- substr(dataset_name, 1L, 64L)
  }

  if (!grepl("\\.sas7bdat$", path, ignore.case = TRUE)) {
    path <- paste0(tools::file_path_sans_ext(path), ".sas7bdat")
  }

  labels  <- .resolve_meta(df, var_labels,  "var_labels",  "label")
  formats <- .resolve_meta(df, var_formats, "var_formats", "format")
  lengths <- .resolve_meta_int(df, var_lengths, "var_lengths")

  invisible(
    cx_write_sas7bdat(
      df           = df,
      path         = path,
      dataset_name = dataset_name,
      var_labels   = if (length(labels)  > 0) labels  else NULL,
      var_formats  = if (length(formats) > 0) formats else NULL,
      var_lengths  = if (length(lengths) > 0) lengths else NULL
    )
  )
}
