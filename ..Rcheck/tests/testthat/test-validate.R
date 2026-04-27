test_that("submission_check passes clean CDISC-compliant dataset", {
  dm <- data.frame(
    STUDYID = "STUDY001",
    DOMAIN  = "DM",
    USUBJID = "STUDY001-001",
    DMSEQ   = 1L,
    AGE     = 35L,
    SEX     = "M",
    stringsAsFactors = FALSE
  )
  dm <- set_var_label(dm, c(
    STUDYID = "Study Identifier",
    DOMAIN  = "Domain Abbreviation",
    USUBJID = "Unique Subject Identifier",
    DMSEQ   = "Sequence Number",
    AGE     = "Age",
    SEX     = "Sex"
  ))
  dm <- set_var_format(dm, c(AGE = "3."))

  report <- submission_check(dm, domain = "DM", verbose = FALSE)
  errors <- report[report$level == "ERROR", ]
  expect_equal(nrow(errors), 0L)
})

test_that("submission_check detects missing required CDISC variables", {
  df <- data.frame(A = 1:3, stringsAsFactors = FALSE)
  report <- submission_check(df, domain = "DM", verbose = FALSE)
  expect_true(any(grepl("STUDYID|DOMAIN|USUBJID", report$message)))
  expect_true(any(report$level == "ERROR"))
})

test_that("check_xpt_names flags long and non-uppercase names", {
  df <- data.frame(`subject_id_long` = 1, `lowercase` = 2,
                   check.names = FALSE)
  result <- check_xpt_names(df, version = 5)
  expect_true(nrow(result) > 0)
  expect_true(any(grepl("too long|uppercase", result$issue)))
})

test_that("check_xpt_names returns empty frame for compliant names", {
  df <- data.frame(USUBJID = "x", AGE = 1L, SEX = "M",
                   stringsAsFactors = FALSE)
  result <- check_xpt_names(df, version = 5)
  expect_equal(nrow(result), 0L)
})

test_that("check_var_lengths flags overlength char columns", {
  long_string <- paste(rep("X", 250), collapse = "")
  df <- data.frame(TRT = long_string, stringsAsFactors = FALSE)
  result <- check_var_lengths(df, xpt_version = 5)
  expect_true(any(result$exceeds))
})

test_that("submission_check warns on missing labels", {
  df <- data.frame(STUDYID = "S", DOMAIN = "DM", USUBJID = "U",
                   stringsAsFactors = FALSE)
  report <- submission_check(df, domain = "DM", verbose = FALSE)
  warnings <- report[report$level == "WARNING", ]
  expect_true(any(grepl("label", warnings$check)))
})
