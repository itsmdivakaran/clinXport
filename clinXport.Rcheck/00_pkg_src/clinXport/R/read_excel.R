#' Read an Excel file with clinical metadata support
#'
#' Reads an `.xlsx` file using [readxl::read_excel()] and optionally converts
#' date columns, coerces types, and restores variable labels from a companion
#' spec sheet within the same workbook.
#'
#' @param path    Character. Path to the `.xlsx` / `.xls` file.
#' @param sheet   Sheet name or index. Default `1` (first sheet).
#' @param spec_sheet Character. Name of an optional sheet inside the workbook
#'   containing a variable specification (columns: `variable`, `label`,
#'   `format`, `length`, `type`). Set to `NULL` (default) to skip.
#' @param col_types Named character vector overriding column types. Passed to
#'   [readxl::read_excel()]. Use `"text"`, `"numeric"`, `"date"`, `"logical"`.
#' @param label_row Logical or `NULL`. If `TRUE`, treat the first row after the
#'   column names as variable labels and remove it from the data. If `NULL`
#'   (default), this is detected when the row matches labels in `spec_sheet`.
#' @param na     Character vector of strings to treat as `NA`. Default
#'   `c("", "NA", ".", " ")`.
#' @param skip   Integer. Number of rows to skip before reading data. Default 0.
#' @param ...    Additional arguments passed to [readxl::read_excel()].
#'
#' @return A `data.frame` (not a tibble). If a spec sheet is found, variable
#'   labels are attached via [set_var_label()].
#'
#' @examples
#' \dontrun{
#' adsl <- read_pharma_excel("adsl.xlsx", sheet = "ADSL")
#' }
#'
#' @seealso [write_pharma_excel()], [read_xpt()], [apply_spec()]
#' @export
read_pharma_excel <- function(path,
                              sheet      = 1L,
                              spec_sheet = NULL,
                              col_types  = NULL,
                              label_row  = NULL,
                              na         = c("", "NA", ".", " "),
                              skip       = 0L,
                              ...) {
  path <- normalizePath(path, mustWork = TRUE)

  args <- list(path = path, sheet = sheet, na = na, skip = skip, ...)
  if (!is.null(col_types)) args$col_types <- col_types

  df <- do.call(readxl::read_excel, args)
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  # Load spec sheet if requested
  spec <- NULL
  if (!is.null(spec_sheet)) {
    sheets <- readxl::excel_sheets(path)
    if (spec_sheet %in% sheets) {
      spec <- as.data.frame(
        readxl::read_excel(path, sheet = spec_sheet, na = c("", "NA")),
        stringsAsFactors = FALSE
      )
    } else {
      warning("Spec sheet '", spec_sheet, "' not found in ", path)
    }
  }

  if (is.null(label_row)) {
    label_row <- FALSE
    if (!is.null(spec) && nrow(df) > 0L &&
        all(c("variable", "label") %in% tolower(names(spec)))) {
      spec2 <- spec
      names(spec2) <- tolower(names(spec2))
      spec_labels <- stats::setNames(as.character(spec2$label), spec2$variable)
      shared <- intersect(names(df), names(spec_labels))
      first_row <- as.character(unlist(df[1L, shared, drop = FALSE], use.names = FALSE))
      expected <- as.character(spec_labels[shared])
      label_row <- length(shared) > 0L &&
        all(is.na(first_row) | first_row == expected | !nzchar(expected))
    }
  }

  if (isTRUE(label_row) && nrow(df) > 0L) {
    labels <- as.character(unlist(df[1L, , drop = FALSE], use.names = FALSE))
    names(labels) <- names(df)
    labels <- labels[!is.na(labels) & nzchar(labels)]
    df <- df[-1L, , drop = FALSE]
    row.names(df) <- NULL
    if (length(labels) > 0L) df <- set_var_label(df, labels)
  }

  if (!is.null(spec)) {
    df <- apply_spec(df, spec, quiet = TRUE)
  }

  df
}
