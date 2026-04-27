test_that("set_var_label / get_var_label roundtrip", {
  df <- data.frame(A = 1:3, B = letters[1:3], stringsAsFactors = FALSE)
  df <- set_var_label(df, c(A = "Column A", B = "Column B"))

  lbls <- get_var_label(df)
  expect_equal(lbls[["A"]], "Column A")
  expect_equal(lbls[["B"]], "Column B")

  # per-column attribute should also be set
  expect_equal(attr(df$A, "label"), "Column A")
})

test_that("set_var_format / get_var_format roundtrip", {
  df <- data.frame(DT = Sys.Date(), AGE = 30L, stringsAsFactors = FALSE)
  df <- set_var_format(df, c(DT = "DATE9.", AGE = "3."))

  fmts <- get_var_format(df)
  expect_equal(fmts[["DT"]],  "DATE9.")
  expect_equal(fmts[["AGE"]], "3.")
})

test_that("set_var_length / get_var_length roundtrip", {
  df <- data.frame(TRT = "Drug A", stringsAsFactors = FALSE)
  df <- set_var_length(df, c(TRT = 200L))

  lens <- get_var_length(df)
  expect_equal(lens[["TRT"]], 200L)
})

test_that("get_metadata returns correct structure", {
  df <- data.frame(USUBJID = "001", AGE = 30L, stringsAsFactors = FALSE)
  df <- set_var_label(df, c(USUBJID = "Subject ID", AGE = "Age"))
  df <- set_var_format(df, c(AGE = "3."))
  df <- set_var_length(df, c(USUBJID = 20L))

  meta <- get_metadata(df, dataset_name = "DM")
  expect_equal(nrow(meta), 2L)
  expect_true("variable" %in% names(meta))
  expect_true("label"    %in% names(meta))
  expect_true("format"   %in% names(meta))
  expect_true("length"   %in% names(meta))
  expect_equal(meta$label[meta$variable == "USUBJID"], "Subject ID")
})

test_that("set_var_label ignores unknown columns", {
  df <- data.frame(A = 1, stringsAsFactors = FALSE)
  expect_no_error(set_var_label(df, c(A = "Good", NOTHERE = "Bad")))
})
