#' Read a CSV file with clinical metadata support
#'
#' A thin wrapper around [data.table::fread()] that returns a plain `data.frame`
#' and optionally reads a companion `.spec.csv` sidecar file to restore variable
#' labels, formats, lengths, and types.
#'
#' @param path        Character. Path to the CSV file.
#' @param col_select  Optional character vector of column names to return.
#' @param na_strings  Character vector of strings treated as `NA`. Default
#'   `c("", "NA", ".", " ")`.
#' @param encoding    Character. Encoding passed to [data.table::fread()].
#'   Default `"UTF-8"`.
#' @param restore_spec Logical. If `TRUE` (default), read and apply a companion
#'   `<basename>.spec.csv` file when it exists.
#' @param spec_path Optional character. Explicit path to a spec sidecar file.
#' @param ...         Additional arguments passed to [data.table::fread()].
#'
#' @return A `data.frame`.
#'
#' @examples
#' \dontrun{
#' adsl <- read_pharma_csv("adsl.csv")
#' }
#'
#' @seealso [write_pharma_csv()], [read_xpt()]
#' @export
read_pharma_csv <- function(path,
                            col_select  = NULL,
                            na_strings  = c("", "NA", ".", " "),
                            encoding    = "UTF-8",
                            restore_spec = TRUE,
                            spec_path    = NULL,
                            ...) {
  path <- normalizePath(path, mustWork = TRUE)
  na_strings <- unique(na_strings[!grepl("^\\s+$", na_strings)])

  dt <- data.table::fread(
    file        = path,
    na.strings  = na_strings,
    encoding    = encoding,
    data.table  = FALSE,
    ...
  )

  if (isTRUE(restore_spec)) {
    if (is.null(spec_path)) {
      spec_path <- paste0(tools::file_path_sans_ext(path), ".spec.csv")
    }
    if (file.exists(spec_path)) {
      spec <- data.table::fread(spec_path, data.table = FALSE,
                                na.strings = c("", "NA", "."))
      dt <- apply_spec(dt, spec, quiet = TRUE)
    }
  }

  if (!is.null(col_select)) {
    keep <- intersect(col_select, names(dt))
    if (length(keep) == 0L) stop("None of the requested columns found.")
    saved <- lapply(c("var_labels", "var_formats", "var_lengths"), function(a) attr(dt, a))
    dt <- dt[, keep, drop = FALSE]
    meta_attrs <- c("var_labels", "var_formats", "var_lengths")
    for (j in seq_along(meta_attrs)) {
      v <- saved[[j]]
      if (!is.null(v) && !is.null(names(v))) {
        attr(dt, meta_attrs[j]) <- v[names(v) %in% keep]
      }
    }
  }

  dt
}
