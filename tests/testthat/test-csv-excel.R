test_that("CSV sidecar metadata is restored on read", {
  df <- data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)
  df <- set_var_label(df, c(A = "Analysis value", B = "Category"))
  df <- set_var_format(df, c(A = "3."))
  df <- set_var_length(df, c(B = 12L))

  tmp <- withr::local_tempfile(fileext = ".csv")
  write_pharma_csv(df, tmp, write_spec = TRUE)
  out <- read_pharma_csv(tmp)

  expect_equal(out, df, ignore_attr = TRUE)
  expect_equal(get_var_label(out)[["A"]], "Analysis value")
  expect_equal(get_var_format(out)[["A"]], "3.")
  expect_equal(get_var_length(out)[["B"]], 12L)
})

test_that("Excel label row written by write_pharma_excel is not read as data", {
  df <- data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)
  df <- set_var_label(df, c(A = "Alpha", B = "Beta"))

  tmp <- withr::local_tempfile(fileext = ".xlsx")
  write_pharma_excel(df, tmp, include_labels = TRUE, add_spec_sheet = TRUE)
  out <- read_pharma_excel(tmp, spec_sheet = "SPEC")

  expect_equal(nrow(out), 2L)
  expect_equal(as.numeric(out$A), c(1, 2))
  expect_equal(unname(out$B), c("x", "y"), ignore_attr = TRUE)
  expect_equal(get_var_label(out)[["A"]], "Alpha")
})
