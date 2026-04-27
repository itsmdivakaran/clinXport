test_that("apply_spec sets labels and formats from data.frame spec", {
  df <- data.frame(USUBJID = "001", AGE = 30L, stringsAsFactors = FALSE)
  spec <- data.frame(
    variable = c("USUBJID", "AGE"),
    label    = c("Unique Subject Identifier", "Age at Screening"),
    format   = c("$CHAR20.", "3."),
    type     = c("Char", "Num"),
    length   = c(20L, NA_integer_),
    stringsAsFactors = FALSE
  )

  out <- apply_spec(df, spec, quiet = TRUE)

  lbls <- get_var_label(out)
  fmts <- get_var_format(out)

  expect_equal(lbls[["USUBJID"]], "Unique Subject Identifier")
  expect_equal(lbls[["AGE"]],     "Age at Screening")
  expect_equal(fmts[["USUBJID"]], "$CHAR20.")
  expect_equal(fmts[["AGE"]],     "3.")
})

test_that("apply_spec coerces types", {
  df <- data.frame(AGE = c("30", "42", "55"), stringsAsFactors = FALSE)
  spec <- data.frame(variable = "AGE", type = "Num",
                     stringsAsFactors = FALSE)
  out <- apply_spec(df, spec, coerce_types = TRUE, quiet = TRUE)
  expect_true(is.numeric(out$AGE))
})

test_that("apply_spec reorders columns when reorder = TRUE", {
  df <- data.frame(C = 1, A = 2, B = 3, stringsAsFactors = FALSE)
  spec <- data.frame(variable = c("A", "B", "C"),
                     stringsAsFactors = FALSE)
  out <- apply_spec(df, spec, reorder = TRUE, quiet = TRUE)
  expect_equal(names(out), c("A", "B", "C"))
})

test_that("apply_spec handles spec variables not in df gracefully", {
  df <- data.frame(A = 1:3, stringsAsFactors = FALSE)
  spec <- data.frame(variable = c("A", "NOTHERE"), label = c("A", "Missing"),
                     stringsAsFactors = FALSE)
  expect_no_error(apply_spec(df, spec, quiet = TRUE))
})
