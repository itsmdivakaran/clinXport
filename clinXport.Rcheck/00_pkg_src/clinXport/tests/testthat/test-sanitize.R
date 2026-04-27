test_that("sanitize_col_names uppercases and removes specials", {
  df <- data.frame(`subject id` = 1, `visit.date` = 2, check.names = FALSE)
  out <- sanitize_col_names(df, version = 5, quiet = TRUE)
  expect_true(all(names(out) == toupper(names(out))))
  expect_true(all(!grepl("[^A-Z0-9_]", names(out))))
})

test_that("sanitize_col_names truncates to 8 for v5", {
  df <- data.frame(TOOLONGCOLUMNNAME = 1:3)
  out <- sanitize_col_names(df, version = 5, quiet = TRUE)
  expect_true(all(nchar(names(out)) <= 8))
})

test_that("sanitize_col_names deduplicates", {
  df <- data.frame(TOOLONGNAME1 = 1, TOOLONGNAME2 = 2)
  out <- sanitize_col_names(df, version = 5, quiet = TRUE)
  expect_equal(length(unique(names(out))), 2L)
})

test_that("sanitize_col_names updates var_labels attribute", {
  df <- data.frame(`my var` = 1:3, check.names = FALSE)
  df <- set_var_label(df, c(`my var` = "My Variable"))
  out <- sanitize_col_names(df, version = 5, quiet = TRUE)
  lbls <- attr(out, "var_labels")
  expect_true("MY_VAR" %in% names(lbls))
})
