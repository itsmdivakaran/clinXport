#' clinXport: Clinical Data I/O for SAS XPT, SAS7BDAT, Excel, and CSV
#'
#' @description
#' **clinXport** provides high-performance read/write capabilities for clinical
#' data formats used in pharmaceutical regulatory submissions. It does **not**
#' depend on \pkg{haven} or \pkg{xportr}.
#'
#' ## Key features
#'
#' ### Format support
#' | Function | Format | Read | Write |
#' |----------|--------|------|-------|
#' | `read_xpt()` / `write_xpt()` | SAS XPT v5/v8 | yes | yes |
#' | `read_sas()` / `write_sas()` | SAS7BDAT | yes | yes |
#' | `read_pharma_excel()` / `write_pharma_excel()` | Excel (.xlsx) | yes | yes |
#' | `read_pharma_csv()` / `write_pharma_csv()` | CSV | yes | yes |
#'
#' ### Metadata management
#' - [set_var_label()] / [get_var_label()] - variable labels
#' - [set_var_format()] / [get_var_format()] - SAS format strings (e.g. `DATE9.`)
#' - [set_var_length()] / [get_var_length()] - byte lengths
#' - [get_metadata()] - variable-level metadata summary
#'
#' ### Spec-driven workflow
#' - [import_spec()] - load variable spec from Excel or CSV
#' - [apply_spec()] - apply labels, formats, lengths, and type coercions
#'
#' ### Submission validation
#' - [submission_check()] - full FDA/PMDA submission-readiness report
#' - [check_xpt_names()] - XPT v5/v8 name compliance
#' - [check_var_lengths()] - byte-length limit check
#' - [sanitize_col_names()] - auto-correct column names
#'
#' ## Quick start
#'
#' ```r
#' library(clinXport)
#'
#' # Read XPT
#' dm <- read_xpt("dm.xpt")
#' attr(dm, "var_labels")
#'
#' # Attach metadata and write XPT
#' dm <- set_var_label(dm, c(USUBJID = "Unique Subject Identifier", AGE = "Age"))
#' dm <- set_var_format(dm, c(RFSTDTC = "DATE9."))
#' write_xpt(dm, "dm_out.xpt", dataset_name = "DM", dataset_label = "Demographics")
#'
#' # Spec-driven workflow
#' spec <- import_spec("dm_spec.xlsx", dataset = "DM")
#' dm   <- apply_spec(dm, spec, coerce_types = TRUE, reorder = TRUE)
#'
#' # Submission check
#' submission_check(dm, domain = "DM")
#' ```
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom Rcpp sourceCpp
#' @useDynLib clinXport, .registration = TRUE
NULL
