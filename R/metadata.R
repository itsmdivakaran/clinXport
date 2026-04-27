#' Set variable labels on a data frame
#'
#' Attaches variable labels both as per-column `"label"` attributes and as the
#' data-frame-level `"var_labels"` attribute. Both storage locations are
#' checked by [write_xpt()], [write_sas()], and [write_pharma_excel()].
#'
#' @param df     A `data.frame`.
#' @param labels Named character vector mapping column names to labels.
#'   Names not found in `df` are silently ignored.
#'
#' @return The modified `data.frame` (invisibly).
#'
#' @examples
#' df <- data.frame(USUBJID = "001", AGE = 30L, SEX = "M")
#' df <- set_var_label(df, c(
#'   USUBJID = "Unique Subject Identifier",
#'   AGE     = "Age at Screening",
#'   SEX     = "Sex"
#' ))
#' get_var_label(df)
#'
#' @seealso [get_var_label()], [set_var_format()], [set_var_length()]
#' @export
set_var_label <- function(df, labels) {
  stopifnot(is.data.frame(df), is.character(labels), !is.null(names(labels)))

  # Ensure names are not dropped by any coercion
  lbl_names <- names(labels)
  labels     <- stats::setNames(as.character(labels), lbl_names)

  existing <- attr(df, "var_labels")
  if (is.null(existing)) existing <- stats::setNames(character(ncol(df)), names(df))

  for (nm in intersect(lbl_names, names(df))) {
    # Replace the column so the per-column attribute is properly stored
    col <- df[[nm]]
    attr(col, "label") <- labels[[nm]]
    df[[nm]] <- col
    # Mirror into the data-frame-level attribute
    existing[[nm]] <- labels[[nm]]
  }
  attr(df, "var_labels") <- existing
  invisible(df)
}

#' Get variable labels from a data frame
#'
#' Returns a named character vector of variable labels, checking both
#' per-column attributes and the data-frame-level `"var_labels"` attribute.
#'
#' @param df A `data.frame`.
#' @return Named character vector (same length as `ncol(df)`).
#'
#' @examples
#' df <- data.frame(A = 1, B = "x")
#' df <- set_var_label(df, c(A = "Variable A", B = "Variable B"))
#' get_var_label(df)
#'
#' @export
get_var_label <- function(df) {
  stopifnot(is.data.frame(df))
  nms    <- names(df)
  df_lbl <- attr(df, "var_labels")
  out    <- stats::setNames(character(length(nms)), nms)

  for (nm in nms) {
    v <- attr(df[[nm]], "label")
    if (!is.null(v) && !is.na(v)) { out[nm] <- as.character(v); next }
    if (!is.null(df_lbl) && nm %in% names(df_lbl)) out[nm] <- as.character(df_lbl[[nm]])
  }
  out
}

#' Set SAS format strings on a data frame
#'
#' Attaches SAS format strings (e.g. `"DATE9."`, `"$CHAR200."`, `"3."`) to
#' columns. These are written verbatim into the XPT/SAS7BDAT format field.
#'
#' @param df      A `data.frame`.
#' @param formats Named character vector, e.g.
#'   `c(RFSTDTC = "DATE9.", AGE = "3.", USUBJID = "$CHAR20.")`.
#'
#' @return The modified `data.frame` (invisibly).
#'
#' @examples
#' df <- data.frame(RFSTDTC = as.Date("2024-01-15"), AGE = 42L)
#' df <- set_var_format(df, c(RFSTDTC = "DATE9.", AGE = "3."))
#' get_var_format(df)
#'
#' @seealso [get_var_format()], [set_var_label()]
#' @export
set_var_format <- function(df, formats) {
  stopifnot(is.data.frame(df), is.character(formats), !is.null(names(formats)))

  fmt_names <- names(formats)
  formats    <- stats::setNames(as.character(formats), fmt_names)

  existing <- attr(df, "var_formats")
  if (is.null(existing)) existing <- stats::setNames(character(ncol(df)), names(df))

  for (nm in intersect(fmt_names, names(df))) {
    col <- df[[nm]]
    attr(col, "format") <- formats[[nm]]
    df[[nm]] <- col
    existing[[nm]] <- formats[[nm]]
  }
  attr(df, "var_formats") <- existing
  invisible(df)
}

#' Get SAS format strings from a data frame
#'
#' @param df A `data.frame`.
#' @return Named character vector of SAS format strings.
#' @export
get_var_format <- function(df) {
  stopifnot(is.data.frame(df))
  nms  <- names(df)
  fmts <- attr(df, "var_formats")
  out  <- stats::setNames(character(length(nms)), nms)
  for (nm in nms) {
    v <- attr(df[[nm]], "format")
    if (!is.null(v) && !is.na(v)) { out[nm] <- as.character(v); next }
    if (!is.null(fmts) && nm %in% names(fmts)) out[nm] <- as.character(fmts[[nm]])
  }
  out
}

#' Set variable byte lengths on a data frame
#'
#' Used to control the width written to XPT/SAS7BDAT for character columns.
#' Numeric columns are always 8 bytes in XPT and are silently unaffected.
#'
#' @param df      A `data.frame`.
#' @param lengths Named integer vector of byte lengths (e.g.
#'   `c(USUBJID = 20L, RACE = 200L)`).
#'
#' @return The modified `data.frame` (invisibly).
#'
#' @examples
#' df <- data.frame(USUBJID = "001-001", RACE = "WHITE")
#' df <- set_var_length(df, c(USUBJID = 20L, RACE = 200L))
#' get_var_length(df)
#'
#' @export
set_var_length <- function(df, lengths) {
  stopifnot(is.data.frame(df), !is.null(names(lengths)))

  # Explicitly preserve names before any coercion - as.integer() can drop them
  # in some R builds when the input is already an integer vector.
  len_names <- names(lengths)
  lengths    <- stats::setNames(as.integer(lengths), len_names)

  existing <- attr(df, "var_lengths")
  if (is.null(existing)) existing <- stats::setNames(integer(ncol(df)), names(df))

  for (nm in intersect(len_names, names(df))) {
    # Replace the column so the per-column "width" attribute is properly stored
    col <- df[[nm]]
    attr(col, "width") <- lengths[[nm]]
    df[[nm]] <- col
    # Mirror into the data-frame-level attribute
    existing[[nm]] <- lengths[[nm]]
  }
  attr(df, "var_lengths") <- existing
  invisible(df)
}

#' Get variable byte lengths from a data frame
#'
#' @param df A `data.frame`.
#' @return Named integer vector of variable byte lengths.
#' @export
get_var_length <- function(df) {
  stopifnot(is.data.frame(df))
  nms  <- names(df)
  lens <- attr(df, "var_lengths")
  out  <- stats::setNames(integer(length(nms)), nms)
  for (nm in nms) {
    v <- attr(df[[nm]], "width")
    if (!is.null(v) && !is.na(v)) { out[nm] <- as.integer(v); next }
    if (!is.null(lens) && nm %in% names(lens)) out[nm] <- as.integer(lens[[nm]])
  }
  out
}

#' Get a metadata summary table for a data frame
#'
#' Returns a `data.frame` with one row per column summarising name, label,
#' type, length, and SAS format. Useful for review and for building spec sheets.
#'
#' @param df           A `data.frame`.
#' @param dataset_name Character. Dataset name to include in the table.
#'
#' @return A `data.frame` with columns: `dataset`, `variable`, `label`,
#'   `type`, `length`, `format`.
#'
#' @examples
#' df <- data.frame(USUBJID = "001", AGE = 30L)
#' df <- set_var_label(df, c(USUBJID = "Unique Subject Identifier", AGE = "Age"))
#' get_metadata(df, dataset_name = "DM")
#'
#' @export
get_metadata <- function(df, dataset_name = "") {
  stopifnot(is.data.frame(df))
  .build_spec_table(df, dataset_name = dataset_name)
}

# -- Internal: SAS format -> R class conversion ---------------------------------

.apply_sas_formats <- function(df, fmts) {
  # SAS epoch: Jan 1, 1960
  sas_origin <- as.Date("1960-01-01")

  for (nm in names(fmts)) {
    if (!nm %in% names(df)) next
    fmt <- toupper(trimws(fmts[[nm]]))
    if (nchar(fmt) == 0L) next
    col <- df[[nm]]

    if (is.numeric(col)) {
      if (grepl("^DATE", fmt)) {
        df[[nm]] <- structure(as.integer(col), class = "Date",
                              origin = sas_origin) + 0L
        # Actually:
        df[[nm]] <- sas_origin + as.integer(col)
      } else if (grepl("^DATETIME", fmt)) {
        df[[nm]] <- as.POSIXct(col, origin = sas_origin, tz = "UTC")
      } else if (grepl("^TIME", fmt)) {
        # Keep as numeric seconds
        class(df[[nm]]) <- c("hms", "difftime")
        attr(df[[nm]], "units") <- "secs"
      }
    }
  }
  df
}
