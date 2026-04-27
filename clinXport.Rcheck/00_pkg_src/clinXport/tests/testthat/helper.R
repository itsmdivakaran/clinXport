# testthat helper: shared fixtures

make_dm <- function(n = 5L) {
  data.frame(
    STUDYID = rep("STUDY001", n),
    DOMAIN  = rep("DM", n),
    USUBJID = paste0("STUDY001-", sprintf("%03d", seq_len(n))),
    DMSEQ   = seq_len(n),
    AGE     = sample(18:80, n, replace = TRUE),
    SEX     = sample(c("M", "F"), n, replace = TRUE),
    RACE    = sample(c("WHITE", "BLACK", "ASIAN"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}
