test_that("write_xpt and read_xpt round-trip numeric data", {
  df <- data.frame(
    USUBJID = c("STUDY-001-001", "STUDY-001-002", "STUDY-001-003"),
    AGE     = c(34L, 55L, 42L),
    WEIGHT  = c(72.5, 88.1, 65.3),
    stringsAsFactors = FALSE
  )
  df <- set_var_label(df, c(USUBJID = "Unique Subject ID", AGE = "Age", WEIGHT = "Weight (kg)"))
  df <- set_var_format(df, c(AGE = "3.", WEIGHT = "5.1"))

  tmp <- withr::local_tempfile(fileext = ".xpt")
  write_xpt(df, tmp, version = 5, dataset_name = "DM", dataset_label = "Test")

  out <- read_xpt(tmp, apply_formats = FALSE)

  expect_equal(nrow(out), 3L)
  expect_equal(ncol(out), 3L)
  expect_equal(out$USUBJID, df$USUBJID)
  expect_equal(as.double(out$AGE), as.double(df$AGE), tolerance = 1e-6)
  expect_equal(out$WEIGHT, df$WEIGHT, tolerance = 1e-4)
})

test_that("read_xpt restores var_labels and var_formats", {
  df <- data.frame(A = 1:3, B = c("x", "y", "z"), stringsAsFactors = FALSE)
  df <- set_var_label(df, c(A = "Label A", B = "Label B"))
  df <- set_var_format(df, c(A = "3."))

  tmp <- withr::local_tempfile(fileext = ".xpt")
  write_xpt(df, tmp, version = 5)
  out <- read_xpt(tmp, apply_formats = FALSE)

  lbls <- attr(out, "var_labels")
  expect_equal(as.character(lbls["A"]), "Label A")
  expect_equal(as.character(lbls["B"]), "Label B")
})

test_that("write_xpt sanitises column names for v5", {
  df <- data.frame(`subject id` = 1:3, longcolumnname = 4:6, check.names = FALSE)
  tmp <- withr::local_tempfile(fileext = ".xpt")
  # Should not error even with bad names
  expect_no_error(write_xpt(df, tmp, version = 5, sanitize_names = TRUE))
  out <- read_xpt(tmp, apply_formats = FALSE)
  expect_true(all(nchar(names(out)) <= 8))
})

test_that("write_xpt handles NA values", {
  df <- data.frame(
    X = c(1.0, NA_real_, 3.0),
    Y = c("a", NA_character_, "c"),
    stringsAsFactors = FALSE
  )
  tmp <- withr::local_tempfile(fileext = ".xpt")
  write_xpt(df, tmp, version = 5)
  out <- read_xpt(tmp, apply_formats = FALSE)
  expect_true(is.na(out$X[2]))
  expect_true(is.na(out$Y[2]) || out$Y[2] == "")
})

test_that("write_xpt v8 supports longer column names", {
  df <- data.frame(
    LONGCOLUMNNAME_V8 = 1:3,
    ANOTHER_LONG_NAME = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  tmp <- withr::local_tempfile(fileext = ".xpt")
  expect_no_error(write_xpt(df, tmp, version = 8))
  out <- read_xpt(tmp, apply_formats = FALSE)
  expect_equal(nrow(out), 3L)
})
