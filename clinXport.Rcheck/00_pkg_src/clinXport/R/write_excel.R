#' Write a data frame (or list of data frames) to an Excel file
#'
#' Exports one or more data frames to a formatted `.xlsx` workbook using
#' \pkg{openxlsx2}. Variable labels appear as a frozen header row in a
#' distinct colour, column widths are auto-fitted, and an optional metadata
#' summary sheet is appended.
#'
#' @param x         A `data.frame` or a named list of data frames. Each
#'   list element becomes a separate sheet.
#' @param path      Character. Output `.xlsx` file path.
#' @param sheet     Character. Sheet name(s). Recycled / truncated to match
#'   the length of `x`. Default `"Sheet1"` (or the names of the list).
#' @param header_style Character. One of `"bold"` (default), `"clinical"`
#'   (blue header, white text), or `"none"`.
#' @param include_labels Logical. If `TRUE` (default), a second frozen row
#'   with variable labels is written below the header.
#' @param add_spec_sheet Logical. If `TRUE`, an extra sheet called `"SPEC"` is
#'   appended with a variable-level metadata table. Default `FALSE`.
#' @param overwrite Logical. Overwrite the file if it already exists. Default
#'   `TRUE`.
#' @param ...       Additional arguments (currently unused).
#'
#' @return Invisibly returns `path`.
#'
#' @examples
#' \dontrun{
#' write_pharma_excel(adsl, "adsl.xlsx",
#'                   header_style  = "clinical",
#'                   include_labels = TRUE,
#'                   add_spec_sheet = TRUE)
#'
#' # Multiple sheets
#' write_pharma_excel(list(DM = dm, AE = ae), "datasets.xlsx")
#' }
#'
#' @seealso [read_pharma_excel()], [write_xpt()]
#' @export
write_pharma_excel <- function(x,
                               path,
                               sheet          = NULL,
                               header_style   = c("bold", "clinical", "none"),
                               include_labels = TRUE,
                               add_spec_sheet = FALSE,
                               overwrite      = TRUE,
                               ...) {
  header_style <- match.arg(header_style)
  if (is.data.frame(x)) x <- list(x)
  if (!is.list(x)) stop("`x` must be a data.frame or named list of data.frames.")

  # Sheet names
  if (is.null(sheet)) {
    sheet <- if (!is.null(names(x))) names(x) else paste0("Sheet", seq_along(x))
  }
  sheet <- rep_len(sheet, length(x))
  # Truncate to Excel's 31-char limit
  sheet <- substr(sheet, 1L, 31L)

  wb <- openxlsx2::wb_workbook()

  # Header styles
  hs_fill   <- switch(header_style,
    bold     = "FFDDDDDD",
    clinical = "FF003087",
    none     = "FFFFFFFF")
  hs_fcolor <- switch(header_style,
    bold     = "FF000000",
    clinical = "FFFFFFFF",
    none     = "FF000000")
  hs_bold   <- header_style != "none"

  for (i in seq_along(x)) {
    df  <- x[[i]]
    sht <- sheet[i]

    if (!is.data.frame(df)) {
      warning("Element '", sht, "' is not a data.frame; skipping.")
      next
    }

    nms  <- names(df)
    lbls <- .get_labels(df)

    wb <- openxlsx2::wb_add_worksheet(wb, sheet = sht)

    # --- Write data (including header) ---
    if (include_labels && any(nzchar(lbls))) {
      # Write variable names as row 1, labels as row 2, data from row 3
      header_df <- as.data.frame(
        matrix(c(nms, lbls), nrow = 2L, byrow = TRUE,
               dimnames = list(NULL, nms)),
        stringsAsFactors = FALSE
      )
      combined <- rbind(header_df, df)
      wb <- openxlsx2::wb_add_data(wb, sheet = sht, x = combined,
                                   col_names = FALSE, start_row = 1L)
      # Style label row
      wb <- openxlsx2::wb_add_font(
        wb, sheet = sht,
        dims = openxlsx2::wb_dims(rows = 2L, cols = seq_along(nms)),
        italic = TRUE,
        color = openxlsx2::wb_color(hex = "FF666666"),
        update = TRUE
      )
      wb <- openxlsx2::wb_freeze_pane(wb, sheet = sht, first_active_row = 3L)
    } else {
      wb <- openxlsx2::wb_add_data(wb, sheet = sht, x = df, col_names = TRUE,
                                   start_row = 1L)
      wb <- openxlsx2::wb_freeze_pane(wb, sheet = sht, first_active_row = 2L)
    }

    # Style header row
    if (hs_bold || header_style != "none") {
      dims <- openxlsx2::wb_dims(rows = 1L, cols = seq_along(nms))
      wb <- openxlsx2::wb_add_font(
        wb, sheet = sht,
        dims = dims,
        bold = hs_bold,
        color = openxlsx2::wb_color(hex = hs_fcolor),
        update = TRUE
      )
      wb <- openxlsx2::wb_add_fill(
        wb, sheet = sht,
        dims = dims,
        color = openxlsx2::wb_color(hex = hs_fill)
      )
    }

    # Auto-fit column widths
    wb <- openxlsx2::wb_set_col_widths(
      wb, sheet = sht,
      cols = seq_along(nms),
      widths = "auto"
    )
  }

  # Spec sheet
  if (isTRUE(add_spec_sheet)) {
    all_specs <- lapply(seq_along(x), function(i) {
      df <- x[[i]]
      .build_spec_table(df, dataset_name = sheet[i])
    })
    spec_df <- do.call(rbind, all_specs)
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = "SPEC")
    wb <- openxlsx2::wb_add_data(wb, sheet = "SPEC", x = spec_df,
                                 col_names = TRUE)
    wb <- openxlsx2::wb_set_col_widths(wb, sheet = "SPEC",
                                       cols = seq_len(ncol(spec_df)),
                                       widths = "auto")
  }

  openxlsx2::wb_save(wb, file = path, overwrite = overwrite)
  invisible(path)
}

# -- Internal ------------------------------------------------------------------

.get_labels <- function(df) {
  df_lbl <- attr(df, "var_labels")
  nms    <- names(df)
  out    <- character(length(nms))
  for (i in seq_along(nms)) {
    nm <- nms[i]
    # Per-column attribute first
    v  <- attr(df[[i]], "label")
    if (!is.null(v) && !is.na(v) && nzchar(v)) { out[i] <- as.character(v); next }
    # Then data-frame attribute
    if (!is.null(df_lbl) && nm %in% names(df_lbl)) {
      v2 <- df_lbl[[nm]]
      if (!is.null(v2) && !is.na(v2) && nzchar(v2)) out[i] <- as.character(v2)
    }
  }
  out
}

.build_spec_table <- function(df, dataset_name = "") {
  nms    <- names(df)
  lbls   <- .get_labels(df)
  fmts   <- attr(df, "var_formats")
  lens   <- attr(df, "var_lengths")

  data.frame(
    dataset  = dataset_name,
    variable = nms,
    label    = lbls,
    type     = vapply(df, function(col) {
      if (is.character(col) || is.factor(col)) "Char" else "Num"
    }, character(1L)),
    length   = vapply(nms, function(nm) {
      if (!is.null(lens) && nm %in% names(lens)) lens[[nm]] else NA_integer_
    }, integer(1L)),
    format   = vapply(nms, function(nm) {
      if (!is.null(fmts) && nm %in% names(fmts)) fmts[[nm]] else ""
    }, character(1L)),
    stringsAsFactors = FALSE
  )
}
