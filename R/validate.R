#' Run a full submission-readiness check on a dataset
#'
#' Performs a battery of checks relevant to FDA eCTD / PMDA submissions:
#' XPT v5 naming rules, variable label completeness, format string validity,
#' character length limits, numeric precision, and CDISC domain conventions
#' (STUDYID, DOMAIN, USUBJID presence, etc.).
#'
#' @param df          A `data.frame`.
#' @param domain      Character. CDISC domain code (e.g. `"DM"`, `"AE"`).
#'   Pass `NULL` to skip domain-specific checks.
#' @param xpt_version Integer. XPT version used for naming rules: `5` or `8`.
#'   Default `5`.
#' @param check_labels    Logical. Warn on missing labels. Default `TRUE`.
#' @param check_formats   Logical. Warn on missing formats for date columns.
#'   Default `TRUE`.
#' @param check_lengths   Logical. Error if any character variable exceeds the
#'   200-byte XPT v5 limit. Default `TRUE`.
#' @param verbose     Logical. Print the report to the console. Default `TRUE`.
#'
#' @return A `data.frame` (invisibly) with columns `level` (`"ERROR"`,
#'   `"WARNING"`, `"INFO"`), `variable`, `check`, `message`.
#'
#' @examples
#' \dontrun{
#' dm <- read_xpt("dm.xpt")
#' report <- submission_check(dm, domain = "DM")
#' }
#'
#' @seealso [check_xpt_names()], [check_var_lengths()]
#' @export
submission_check <- function(df,
                             domain        = NULL,
                             xpt_version   = 5L,
                             check_labels  = TRUE,
                             check_formats = TRUE,
                             check_lengths = TRUE,
                             verbose       = TRUE) {
  stopifnot(is.data.frame(df))
  xpt_version <- as.integer(xpt_version)

  issues <- list()

  add_issue <- function(level, variable, check, msg) {
    issues[[length(issues) + 1L]] <<- data.frame(
      level    = level,
      variable = variable,
      check    = check,
      message  = msg,
      stringsAsFactors = FALSE
    )
  }

  nms  <- names(df)
  lbls <- get_var_label(df)
  fmts <- get_var_format(df)

  # -- 1. Column name compliance ------------------------------
  name_report <- check_xpt_names(df, version = xpt_version)
  if (nrow(name_report) > 0L) {
    for (i in seq_len(nrow(name_report))) {
      add_issue("ERROR", name_report$variable[i], "xpt_name",
                name_report$issue[i])
    }
  }

  # -- 2. Label completeness ----------------------------------
  if (isTRUE(check_labels)) {
    missing_lbl <- nms[!nzchar(lbls[nms])]
    for (nm in missing_lbl) {
      add_issue("WARNING", nm, "label_missing",
                paste0("'", nm, "' has no variable label."))
    }
  }

  # -- 3. Format strings -------------------------------------
  if (isTRUE(check_formats)) {
    for (nm in nms) {
      col <- df[[nm]]
      fmt <- fmts[nm]

      # Date columns should have a DATE format
      if (inherits(col, "Date") && !grepl("DATE", toupper(fmt))) {
        add_issue("WARNING", nm, "format_date",
                  paste0("'", nm, "' is a Date but has no DATE* format. Suggest 'DATE9.'."))
      }
      if (inherits(col, "POSIXct") && !grepl("DATETIME", toupper(fmt))) {
        add_issue("WARNING", nm, "format_datetime",
                  paste0("'", nm, "' is POSIXct but has no DATETIME* format. Suggest 'DATETIME20.'."))
      }

      # Validate format string syntax (must end with '.' or digits)
      if (nzchar(fmt) && !grepl("\\.$", fmt) && !grepl("[0-9]$", fmt)) {
        add_issue("WARNING", nm, "format_syntax",
                  paste0("'", nm, "' format '", fmt, "' may be malformed (should end with '.')."))
      }
    }
  }

  # -- 4. Character lengths -----------------------------------
  if (isTRUE(check_lengths)) {
    max_char <- if (xpt_version == 5L) 200L else 32767L
    lens <- get_var_length(df)

    for (nm in nms) {
      col <- df[[nm]]
      if (is.character(col) || is.factor(col)) {
        # Declared length
        declared <- lens[nm]
        if (!is.na(declared) && declared > 0L && declared > max_char) {
          add_issue("ERROR", nm, "length_exceeded",
                    sprintf("'%s' declared length %d exceeds XPT v%d limit of %d.",
                            nm, declared, xpt_version, max_char))
        }
        # Actual max width in data
        if (is.character(col)) {
          actual_max <- max(nchar(col, type = "bytes", allowNA = TRUE), na.rm = TRUE)
        } else {
          actual_max <- max(nchar(as.character(col), type = "bytes",
                                  allowNA = TRUE), na.rm = TRUE)
        }
        if (!is.infinite(actual_max) && actual_max > max_char) {
          add_issue("ERROR", nm, "value_too_long",
                    sprintf("'%s' contains values up to %d bytes; XPT v%d max is %d.",
                            nm, actual_max, xpt_version, max_char))
        }
      }
    }
  }

  # -- 5. CDISC domain checks --------------------------------
  if (!is.null(domain)) {
    domain_upper <- toupper(domain)

    # Required keys for all CDISC datasets
    req_vars <- c("STUDYID", "DOMAIN", "USUBJID")
    for (rv in req_vars) {
      if (!rv %in% toupper(nms)) {
        add_issue("ERROR", rv, "cdisc_required",
                  sprintf("Required CDISC variable '%s' not found in domain %s.",
                          rv, domain_upper))
      }
    }

    # DOMAIN value should match the domain code
    if ("DOMAIN" %in% toupper(nms)) {
      dm_col <- df[[ nms[toupper(nms) == "DOMAIN"][1L] ]]
      unique_domains <- unique(dm_col[!is.na(dm_col)])
      if (length(unique_domains) > 1L) {
        add_issue("WARNING", "DOMAIN", "cdisc_domain_value",
                  paste0("Multiple DOMAIN values found: ",
                         paste(unique_domains, collapse = ", ")))
      } else if (length(unique_domains) == 1L && unique_domains != domain_upper) {
        add_issue("WARNING", "DOMAIN", "cdisc_domain_value",
                  sprintf("DOMAIN value '%s' does not match expected '%s'.",
                          unique_domains, domain_upper))
      }
    }

    # Sequence variable
    seq_var <- paste0(domain_upper, "SEQ")
    if (!seq_var %in% toupper(nms)) {
      add_issue("INFO", seq_var, "cdisc_seq",
                sprintf("Sequence variable '%s' not found.", seq_var))
    }
  }

  # -- 6. Row count ------------------------------------------
  if (nrow(df) == 0L) {
    add_issue("WARNING", "", "empty_dataset", "Dataset has zero rows.")
  }

  # -- 7. Numeric overflow -----------------------------------
  for (nm in nms) {
    col <- df[[nm]]
    if (is.numeric(col) && any(is.infinite(col), na.rm = TRUE)) {
      add_issue("ERROR", nm, "infinite_values",
                paste0("'", nm, "' contains Inf or -Inf values. SAS cannot store these."))
    }
  }

  # -- Build report ------------------------------------------
  if (length(issues) == 0L) {
    report <- data.frame(
      level = character(), variable = character(),
      check = character(), message = character(),
      stringsAsFactors = FALSE
    )
  } else {
    report <- do.call(rbind, issues)
    row.names(report) <- NULL
  }

  if (isTRUE(verbose)) {
    .print_check_report(report)
  }

  invisible(report)
}

#' Check variable byte lengths against XPT limits
#'
#' A focused check that returns only variables whose declared or actual content
#' exceeds XPT v5 or v8 character length limits.
#'
#' @param df          A `data.frame`.
#' @param xpt_version Integer. `5` (default) or `8`.
#'
#' @return A `data.frame` with columns `variable`, `max_actual`, `declared`,
#'   `limit`, `exceeds`.
#'
#' @export
check_var_lengths <- function(df, xpt_version = 5L) {
  xpt_version <- as.integer(xpt_version)
  limit       <- if (xpt_version == 5L) 200L else 32767L
  lens        <- get_var_length(df)
  nms         <- names(df)

  out <- lapply(nms, function(nm) {
    col <- df[[nm]]
    if (!(is.character(col) || is.factor(col))) return(NULL)
    actual_max <- if (is.character(col))
      max(nchar(col, type = "bytes", allowNA = TRUE), na.rm = TRUE)
    else
      max(nchar(as.character(col), type = "bytes", allowNA = TRUE), na.rm = TRUE)
    if (is.infinite(actual_max)) actual_max <- 0L
    declared <- if (!is.na(lens[nm]) && lens[nm] > 0L) lens[nm] else NA_integer_
    data.frame(
      variable   = nm,
      max_actual = as.integer(actual_max),
      declared   = declared,
      limit      = limit,
      exceeds    = actual_max > limit || (!is.na(declared) && declared > limit),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), out))
}

# -- Internal ------------------------------------------------------------------

.print_check_report <- function(report) {
  if (nrow(report) == 0L) {
    message("\u2714 Submission check passed: no issues found.")
    return(invisible(NULL))
  }
  errors   <- sum(report$level == "ERROR")
  warnings <- sum(report$level == "WARNING")
  infos    <- sum(report$level == "INFO")

  message(sprintf(
    "\n\u2500\u2500 Submission Check Report \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n%d ERROR(s)  |  %d WARNING(s)  |  %d INFO(s)\n",
    errors, warnings, infos))

  for (lvl in c("ERROR", "WARNING", "INFO")) {
    sub <- report[report$level == lvl, , drop = FALSE]
    if (nrow(sub) == 0L) next
    icon <- switch(lvl, ERROR = "\u2716", WARNING = "\u26a0", INFO = "\u2139")
    for (i in seq_len(nrow(sub))) {
      message(sprintf("%s [%s] %s: %s",
                      icon, lvl, sub$variable[i], sub$message[i]))
    }
  }
  message("")
}
