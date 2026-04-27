#' Write a data frame to CSV with metadata sidecar
#'
#' Writes a CSV via [data.table::fwrite()] (very fast). Optionally writes a
#' companion `.spec.csv` sidecar file containing variable-level metadata so that
#' labels, formats, and types survive the round trip.
#'
#' @param df           A `data.frame` to export.
#' @param path         Character. Output `.csv` file path.
#' @param sep          Character. Field separator. Default `","`.
#' @param na           Character. String used for `NA` values. Default `""`.
#' @param write_spec   Logical. Write a `<basename>.spec.csv` sidecar file with
#'   variable metadata. Default `FALSE`.
#' @param date_format  Character. Format string for date columns (passed to
#'   [data.table::fwrite()]). Default `"%Y-%m-%d"`.
#' @param ...          Additional arguments passed to [data.table::fwrite()].
#'
#' @return Invisibly returns `path`.
#'
#' @examples
#' \dontrun{
#' write_pharma_csv(adsl, "adsl.csv", write_spec = TRUE)
#' }
#'
#' @seealso [read_pharma_csv()], [write_xpt()]
#' @export
write_pharma_csv <- function(df,
                             path,
                             sep         = ",",
                             na          = "",
                             write_spec  = FALSE,
                             date_format = "%Y-%m-%d",
                             ...) {
  stopifnot(is.data.frame(df))

  out <- df
  if (!is.null(date_format)) {
    out[] <- lapply(out, function(col) {
      if (inherits(col, "Date")) {
        format(col, date_format)
      } else {
        col
      }
    })
  }

  data.table::fwrite(
    x           = out,
    file        = path,
    sep         = sep,
    na          = na,
    dateTimeAs  = "write.csv",
    ...
  )

  if (isTRUE(write_spec)) {
    spec_path <- paste0(tools::file_path_sans_ext(path), ".spec.csv")
    spec_df   <- .build_spec_table(df, dataset_name = basename(path))
    data.table::fwrite(spec_df, file = spec_path)
    message("Spec sidecar written to: ", spec_path)
  }

  invisible(path)
}
