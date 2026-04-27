#' Import a variable specification from a file
#'
#' Reads a specification table from an Excel or CSV file and returns a
#' standardised `data.frame` that can be passed to [apply_spec()].
#'
#' The spec file must contain at least a `variable` column. All other columns
#' are optional but recognised:
#' \describe{
#'   \item{`variable`}{Column name in the target dataset (required).}
#'   \item{`label`}{Variable label.}
#'   \item{`type`}{`"Char"` / `"C"` / `"character"` or `"Num"` / `"N"` / `"numeric"`.}
#'   \item{`length`}{Byte length for character variables.}
#'   \item{`format`}{SAS format string, e.g. `"DATE9."`, `"$CHAR200."`.}
#'   \item{`dataset`}{Dataset name (optional; used to filter when one spec
#'     file covers multiple domains).}
#' }
#'
#' @param path    Character. Path to the spec file (`.xlsx`, `.xls`, or `.csv`).
#' @param sheet   Sheet name or index for Excel files. Default `1`.
#' @param dataset Character. If the spec covers multiple datasets, filter to
#'   this dataset name. If `NULL`, all rows are returned.
#'
#' @return A standardised spec `data.frame`.
#'
#' @examples
#' \dontrun{
#' spec <- import_spec("SDTM_spec.xlsx", sheet = "DM", dataset = "DM")
#' dm   <- apply_spec(dm_raw, spec)
#' }
#'
#' @seealso [apply_spec()], [get_metadata()]
#' @export
import_spec <- function(path, sheet = 1L, dataset = NULL) {
  path <- normalizePath(path, mustWork = TRUE)
  ext  <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    spec <- as.data.frame(
      readxl::read_excel(path, sheet = sheet, na = c("", "NA", ".")),
      stringsAsFactors = FALSE
    )
  } else if (ext == "csv") {
    spec <- data.table::fread(path, data.table = FALSE, na.strings = c("", "NA", "."))
  } else {
    stop("Unsupported spec file format: ", ext, ". Use .xlsx, .xls, or .csv.")
  }

  # Normalise column names (lowercase)
  names(spec) <- tolower(trimws(names(spec)))

  if (!"variable" %in% names(spec)) {
    stop("Spec file must have a 'variable' column.")
  }

  spec$variable <- trimws(as.character(spec$variable))

  # Filter to dataset
  if (!is.null(dataset) && "dataset" %in% names(spec)) {
    spec <- spec[toupper(trimws(spec$dataset)) == toupper(dataset), , drop = FALSE]
  }

  spec
}

#' Apply a variable specification to a data frame
#'
#' Applies metadata from a spec table to a data frame: attaches labels, SAS
#' format strings, and byte lengths; optionally coerces column types; optionally
#' reorders or selects columns to match the spec order.
#'
#' @param df      A `data.frame`.
#' @param spec    A spec `data.frame` as returned by [import_spec()], or any
#'   `data.frame` with at least a `variable` column.
#' @param coerce_types Logical. If `TRUE`, coerce column types to match the
#'   `type` column of the spec (`"Char"` -> `character`, `"Num"` ->
#'   `numeric`). Default `TRUE`.
#' @param reorder Logical. If `TRUE`, reorder `df` columns to match spec order.
#'   Columns in `df` but not in the spec are appended at the end. Default
#'   `FALSE`.
#' @param select_only Logical. If `TRUE` and `reorder = TRUE`, only columns
#'   present in the spec are returned. Default `FALSE`.
#' @param quiet   Logical. Suppress informational messages. Default `FALSE`.
#'
#' @return The modified `data.frame`.
#'
#' @examples
#' \dontrun{
#' spec <- import_spec("dm_spec.xlsx")
#' dm   <- apply_spec(dm_raw, spec, coerce_types = TRUE, reorder = TRUE)
#' }
#'
#' @seealso [import_spec()], [set_var_label()], [set_var_format()]
#' @export
apply_spec <- function(df,
                       spec,
                       coerce_types = TRUE,
                       reorder      = FALSE,
                       select_only  = FALSE,
                       quiet        = FALSE) {
  stopifnot(is.data.frame(df), is.data.frame(spec))

  # Normalise spec column names
  names(spec) <- tolower(trimws(names(spec)))
  if (!"variable" %in% names(spec)) stop("Spec must have a 'variable' column.")
  spec$variable <- trimws(as.character(spec$variable))

  matched_vars <- intersect(spec$variable, names(df))
  if (length(matched_vars) == 0L) {
    if (!quiet) message("No spec variables matched columns in df.")
    return(df)
  }

  if (!quiet) {
    n_missing <- sum(!spec$variable %in% names(df))
    if (n_missing > 0L)
      message(n_missing, " spec variable(s) not found in df: ",
              paste(setdiff(spec$variable, names(df)), collapse = ", "))
  }

  # -- Apply labels ------------------------------------------
  if ("label" %in% names(spec)) {
    lbl_map <- stats::setNames(
      as.character(spec$label[spec$variable %in% matched_vars]),
      spec$variable[spec$variable %in% matched_vars]
    )
    lbl_map <- lbl_map[!is.na(lbl_map)]
    if (length(lbl_map) > 0L) df <- set_var_label(df, lbl_map)
  }

  # -- Apply formats -----------------------------------------
  if ("format" %in% names(spec)) {
    fmt_map <- stats::setNames(
      as.character(spec$format[spec$variable %in% matched_vars]),
      spec$variable[spec$variable %in% matched_vars]
    )
    fmt_map <- fmt_map[!is.na(fmt_map) & nzchar(fmt_map)]
    fmt_map <- stats::setNames(
      ifelse(grepl(".", fmt_map, fixed = TRUE), fmt_map, paste0(fmt_map, ".")),
      names(fmt_map)
    )
    if (length(fmt_map) > 0L) df <- set_var_format(df, fmt_map)
  }

  # -- Apply lengths -----------------------------------------
  if ("length" %in% names(spec)) {
    raw_len  <- spec$length[spec$variable %in% matched_vars]
    len_vals <- suppressWarnings(as.integer(raw_len))
    len_map  <- stats::setNames(len_vals,
                                spec$variable[spec$variable %in% matched_vars])
    len_map  <- len_map[!is.na(len_map)]
    if (length(len_map) > 0L) df <- set_var_length(df, len_map)
  }

  # -- Coerce types ------------------------------------------
  if (isTRUE(coerce_types) && "type" %in% names(spec)) {
    for (i in seq_len(nrow(spec))) {
      nm  <- spec$variable[i]
      typ <- tolower(trimws(as.character(spec$type[i])))
      if (!nm %in% names(df) || is.na(typ)) next

      is_char <- typ %in% c("char", "c", "character", "text", "$")
      is_num  <- typ %in% c("num", "n", "numeric", "number", "float", "double", "int", "integer")

      col <- df[[nm]]
      if (is_char && !is.character(col)) {
        df[[nm]] <- as.character(col)
        if (!quiet) message("Coerced '", nm, "' to character.")
      } else if (is_num && !is.numeric(col)) {
        df[[nm]] <- suppressWarnings(as.numeric(col))
        if (!quiet) message("Coerced '", nm, "' to numeric.")
      }
    }
  }

  # -- Reorder columns ---------------------------------------
  if (isTRUE(reorder)) {
    spec_order <- spec$variable[spec$variable %in% names(df)]
    other_cols <- setdiff(names(df), spec_order)
    new_order  <- if (isTRUE(select_only)) spec_order
                  else c(spec_order, other_cols)
    df <- df[, new_order, drop = FALSE]
  }

  df
}
