#' Sanitise data frame column names for XPT compliance
#'
#' Transforms column names to satisfy SAS XPT naming rules:
#' \itemize{
#'   \item Uppercase only.
#'   \item Contains only letters, digits, and underscores.
#'   \item Starts with a letter or underscore.
#'   \item Maximum 8 characters for XPT v5, 32 for XPT v8.
#'   \item All names unique after truncation.
#' }
#'
#' @param df      A `data.frame`.
#' @param version Integer. XPT version (`5` or `8`). Controls max name length.
#' @param quiet   Logical. If `FALSE` (default), a message is printed for any
#'   column that was renamed.
#'
#' @return The `data.frame` with sanitised column names (metadata attributes
#'   are updated to reflect the new names).
#'
#' @examples
#' df <- data.frame(`subject id` = 1, `visit.date` = "2024-01-01",
#'                  check.names = FALSE)
#' sanitize_col_names(df, version = 5)
#'
#' @seealso [check_xpt_names()], [write_xpt()]
#' @export
sanitize_col_names <- function(df, version = 5L, quiet = FALSE) {
  stopifnot(is.data.frame(df))
  version  <- as.integer(version)
  max_len  <- if (version == 5L) 8L else 32L
  old_nms  <- names(df)
  new_nms  <- character(length(old_nms))

  for (i in seq_along(old_nms)) {
    nm <- old_nms[i]
    # Step 1: uppercase
    clean <- toupper(nm)
    # Step 2: replace non-alphanumeric/underscore with underscore
    clean <- gsub("[^A-Z0-9_]", "_", clean)
    # Step 3: must start with letter or underscore
    if (nchar(clean) == 0L || (!grepl("^[A-Z_]", clean))) {
      clean <- paste0("V", clean)
    }
    # Step 4: truncate
    if (nchar(clean) > max_len) clean <- substr(clean, 1L, max_len)
    new_nms[i] <- clean
  }

  # Step 5: deduplicate (append numeric suffix)
  seen <- character(0)
  for (i in seq_along(new_nms)) {
    nm <- new_nms[i]
    if (nm %in% seen) {
      base  <- substr(nm, 1L, max_len - 2L)
      sufx  <- 1L
      candidate <- paste0(base, sufx)
      while (candidate %in% seen) {
        sufx      <- sufx + 1L
        candidate <- paste0(base, sufx)
      }
      new_nms[i] <- candidate
    }
    seen <- c(seen, new_nms[i])
  }

  # Report changes
  changed <- old_nms != new_nms
  if (!quiet && any(changed)) {
    pairs <- paste(sprintf("  '%s' -> '%s'",
                           old_nms[changed], new_nms[changed]),
                   collapse = "\n")
    message("sanitize_col_names: ", sum(changed),
            " column(s) renamed:\n", pairs)
  }

  # Update names and synchronise metadata attributes
  names(df) <- new_nms

  meta_attrs <- c("var_labels", "var_formats", "var_lengths")
  for (a in meta_attrs) {
    v <- attr(df, a)
    if (!is.null(v) && !is.null(names(v))) {
      map <- stats::setNames(new_nms, old_nms)
      names(v) <- ifelse(names(v) %in% names(map), map[names(v)], names(v))
      attr(df, a) <- v
    }
  }

  df
}

#' Check column names for XPT compliance
#'
#' Returns a `data.frame` reporting each column name that violates XPT v5 or
#' v8 naming rules, along with the nature of the violation.
#'
#' @param df      A `data.frame` (or character vector of column names).
#' @param version Integer. XPT version: `5` or `8`.
#'
#' @return A `data.frame` with columns `variable`, `issue`, `suggested_name`.
#'   Returns an empty data frame if all names are compliant.
#'
#' @examples
#' df <- data.frame(`subject id` = 1, longcolumnname = 2, check.names = FALSE)
#' check_xpt_names(df, version = 5)
#'
#' @export
check_xpt_names <- function(df, version = 5L) {
  nms     <- if (is.character(df)) df else names(df)
  version <- as.integer(version)
  max_len <- if (version == 5L) 8L else 32L

  issues  <- list()

  for (nm in nms) {
    probs <- character(0)
    if (nchar(nm) > max_len)
      probs <- c(probs, sprintf("too long (%d > %d)", nchar(nm), max_len))
    if (nm != toupper(nm))
      probs <- c(probs, "not uppercase")
    if (!grepl("^[A-Z_]", toupper(nm)))
      probs <- c(probs, "must start with letter or underscore")
    if (grepl("[^A-Z0-9_]", toupper(nm)))
      probs <- c(probs, "contains invalid characters")

    if (length(probs) > 0L) {
      issues[[length(issues) + 1L]] <- data.frame(
        variable       = nm,
        issue          = paste(probs, collapse = "; "),
        suggested_name = .suggest_name(nm, max_len),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(issues) == 0L) {
    return(data.frame(variable = character(), issue = character(),
                      suggested_name = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, issues)
}

.suggest_name <- function(nm, max_len) {
  clean <- toupper(nm)
  clean <- gsub("[^A-Z0-9_]", "_", clean)
  if (!grepl("^[A-Z_]", clean)) clean <- paste0("V", clean)
  substr(clean, 1L, max_len)
}
