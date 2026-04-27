#' Write a data frame to SAS XPT format
#'
#' Exports a data frame to SAS Transport format (XPORT engine), version 5 or 8.
#' Variable metadata (labels, formats, lengths) can be passed directly or
#' inferred from the data frame's own attributes (as set by [set_var_label()],
#' [set_var_format()], and [set_var_length()]).
#'
#' @param df            A `data.frame` to export.
#' @param path          Character. Output file path (`.xpt`).
#' @param version       Integer. XPT version: `5` (default) or `8`.
#'   Version 5 restricts column names to <= 8 uppercase alphanumeric characters.
#'   Version 8 allows up to 32 characters.
#' @param dataset_name  Character. SAS dataset name (<= 8 chars for v5). If not
#'   supplied, the object name of `df` is used (truncated as needed).
#' @param dataset_label Character. Dataset label (<= 40 chars). Default `""`.
#' @param var_labels    Named character vector overriding variable labels.
#'   Falls back to `attr(df, "var_labels")` then `attr(col, "label")`.
#' @param var_formats   Named character vector overriding SAS format strings
#'   (e.g. `c(RFSTDTC = "DATE9.", AGE = "3.")`).
#'   Falls back to `attr(df, "var_formats")`.
#' @param var_lengths   Named integer vector overriding character column byte
#'   lengths. Numeric columns are always 8 bytes. Falls back to
#'   `attr(df, "var_lengths")`.
#' @param sanitize_names Logical. If `TRUE` (default), column names are
#'   automatically sanitised for XPT compliance (uppercase, <= 8/32 chars,
#'   alphanumeric + underscore only).
#'
#' @return Invisibly returns `path`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(USUBJID = "STUDY-001-001", AGE = 34L, SEX = "M")
#' df <- set_var_label(df, c(USUBJID = "Unique Subject Identifier",
#'                           AGE = "Age at Screening",
#'                           SEX = "Sex"))
#' df <- set_var_format(df, c(AGE = "3."))
#' write_xpt(df, "dm.xpt", dataset_name = "DM", dataset_label = "Demographics")
#' }
#'
#' @seealso [read_xpt()], [set_var_label()], [set_var_format()], [apply_spec()]
#' @export
write_xpt <- function(df,
                      path,
                      version       = 5L,
                      dataset_name  = NULL,
                      dataset_label = "",
                      var_labels    = NULL,
                      var_formats   = NULL,
                      var_lengths   = NULL,
                      sanitize_names = TRUE) {
  stopifnot(is.data.frame(df))
  version <- as.integer(version)
  if (!version %in% c(5L, 8L)) stop("`version` must be 5 or 8.")

  # Dataset name: use argument, then object name, then "DATASET"
  if (is.null(dataset_name)) {
    dataset_name <- toupper(deparse(substitute(df)))
    dataset_name <- gsub("[^A-Z0-9_]", "", dataset_name)
    if (nchar(dataset_name) == 0L) dataset_name <- "DATASET"
    dataset_name <- substr(dataset_name, 1L, if (version == 5L) 8L else 32L)
  }

  # Sanitise column names
  if (isTRUE(sanitize_names)) {
    df <- sanitize_col_names(df, version = version)
  }

  # Resolve metadata: explicit args > df attributes > per-column attributes
  labels  <- .resolve_meta(df, var_labels,  "var_labels",  "label")
  formats <- .resolve_meta(df, var_formats, "var_formats", "format")
  lengths <- .resolve_meta_int(df, var_lengths, "var_lengths")

  # Ensure path ends with .xpt
  if (!grepl("\\.xpt$", path, ignore.case = TRUE)) {
    path <- paste0(tools::file_path_sans_ext(path), ".xpt")
  }

  invisible(
    cx_write_xpt(
      df            = df,
      path          = path,
      version       = version,
      dataset_name  = dataset_name,
      dataset_label = dataset_label,
      var_labels    = if (length(labels) > 0) labels else NULL,
      var_formats   = if (length(formats) > 0) formats else NULL,
      var_lengths   = if (length(lengths) > 0) lengths else NULL
    )
  )
}

# -- Internal helpers ----------------------------------------------------------

.resolve_meta <- function(df, explicit, attr_name, col_attr) {
  nms <- names(df)
  out <- stats::setNames(character(length(nms)), nms)

  # From per-column attributes
  for (nm in nms) {
    v <- attr(df[[nm]], col_attr)
    if (!is.null(v) && !is.na(v)) out[nm] <- as.character(v)
  }

  # From data-frame attribute
  df_attr <- attr(df, attr_name)
  if (!is.null(df_attr) && length(df_attr) > 0) {
    shared <- intersect(names(df_attr), nms)
    out[shared] <- as.character(df_attr[shared])
  }

  # From explicit argument (highest priority)
  if (!is.null(explicit)) {
    shared <- intersect(names(explicit), nms)
    out[shared] <- as.character(explicit[shared])
  }

  out
}

.resolve_meta_int <- function(df, explicit, attr_name) {
  nms  <- names(df)
  out  <- stats::setNames(integer(length(nms)), nms)

  df_attr <- attr(df, attr_name)
  if (!is.null(df_attr) && length(df_attr) > 0) {
    shared <- intersect(names(df_attr), nms)
    out[shared] <- as.integer(df_attr[shared])
  }
  if (!is.null(explicit)) {
    shared <- intersect(names(explicit), nms)
    out[shared] <- as.integer(explicit[shared])
  }
  out
}
