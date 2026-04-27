# clinXport <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/itsmdivakaran/clinXport/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/itsmdivakaran/clinXport/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/clinXport)](https://CRAN.R-project.org/package=clinXport)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**clinXport** is a submission-ready R package for reading and writing clinical
data in SAS XPT v5/v8, SAS7BDAT, Excel, and CSV formats.  
It does **not** depend on `haven` or `xportr`.

---

## Why clinXport?

| Pain point | clinXport solution |
|------------|-------------------|
| `haven` silently truncates labels | Full label roundtrip guaranteed |
| `xportr` rejects valid format strings | Any SAS format accepted (`DATE9.`, `$CHAR200.`, `3.`, …) |
| Column names corrupt on export | Auto-sanitise for XPT v5 (≤ 8 chars) or v8 (≤ 32 chars) |
| No spec-driven workflow | `import_spec()` + `apply_spec()` from Excel / CSV |
| No validation before submission | `submission_check()` with ERROR / WARNING / INFO report |
| Slow CSV reading | `data.table::fread` backend |

---

## Installation

```r
# Install from GitHub (requires devtools or remotes)
# install.packages("devtools")
devtools::install_github("itsmdivakaran/clinXport")
```

> **System requirements:** A C++17 compiler (GCC ≥ 7, Clang ≥ 5, MSVC 2017+).
> This is automatically satisfied on all standard R build environments.

---

## Quick start

### Reading

```r
library(clinXport)

# Read XPT — metadata preserved automatically
dm <- read_xpt("dm.xpt")
attr(dm, "var_labels")   # variable labels
attr(dm, "var_formats")  # SAS format strings
attr(dm, "dataset_name") # SAS dataset name

# Read SAS7BDAT
adsl <- read_sas("adsl.sas7bdat")

# Read Excel (with optional spec sheet)
dm_xl <- read_pharma_excel("dm.xlsx", sheet = "DM", spec_sheet = "SPEC")

# Read CSV
ae <- read_pharma_csv("ae.csv")
```

### Writing

```r
# Attach metadata first
dm <- set_var_label(dm, c(
  USUBJID = "Unique Subject Identifier",
  AGE     = "Age at Screening",
  RFSTDTC = "Date of First Exposure to Treatment"
))
dm <- set_var_format(dm, c(RFSTDTC = "DATE9.", AGE = "3."))
dm <- set_var_length(dm, c(USUBJID = 20L, RACE = 200L))

# Write XPT v5 (FDA submission standard)
write_xpt(dm, "dm.xpt",
           version       = 5,
           dataset_name  = "DM",
           dataset_label = "Demographics")

# Write SAS7BDAT
write_sas(dm, "dm.sas7bdat", dataset_name = "DM")

# Write Excel with clinical styling + spec sheet
write_pharma_excel(dm, "dm.xlsx",
                   header_style   = "clinical",
                   include_labels = TRUE,
                   add_spec_sheet = TRUE)

# Write CSV + sidecar spec
write_pharma_csv(dm, "dm.csv", write_spec = TRUE)
```

### Spec-driven workflow

```r
# Import spec from Excel (one sheet per domain, or filtered by dataset name)
spec <- import_spec("SDTM_spec.xlsx", sheet = "DM", dataset = "DM")

# Apply: labels, formats, lengths, type coercions, column reorder
dm <- apply_spec(dm_raw, spec,
                 coerce_types = TRUE,
                 reorder      = TRUE)
```

### Column name sanitisation

```r
# Check before export
check_xpt_names(dm, version = 5)
#>          variable                                issue suggested_name
#> 1   subject_id_v2     too long (13 > 8); contains '_'       SUBJECTV2

# Auto-fix all columns
dm <- sanitize_col_names(dm, version = 5)
```

### Submission validation

```r
report <- submission_check(dm,
                            domain      = "DM",
                            xpt_version = 5,
                            verbose     = TRUE)

# ── Submission Check Report ──────────────────────────────────
# 0 ERROR(s)  |  2 WARNING(s)  |  1 INFO(s)
# ⚠ [WARNING] RACE: 'RACE' has no variable label.
# ⚠ [WARNING] RFSTDTC: 'RFSTDTC' is a Date but has no DATE* format. Suggest 'DATE9.'.
# ℹ [INFO] DMSEQ: Sequence variable 'DMSEQ' not found.
```

---

## Format support matrix

| Format | Extension | Read | Write | Notes |
|--------|-----------|------|-------|-------|
| SAS Transport v5 | `.xpt` | ✓ | ✓ | FDA eCTD standard; names ≤ 8 chars |
| SAS Transport v8 | `.xpt` | ✓ | ✓ | Names ≤ 32 chars |
| SAS Data | `.sas7bdat` | ✓ | ✓ | Uncompressed write; auto-detects endianness on read |
| Excel | `.xlsx` | ✓ | ✓ | Multi-sheet; clinical header styling; spec sheet |
| CSV | `.csv` | ✓ | ✓ | data.table backend; optional `.spec.csv` sidecar |

---

## Metadata architecture

All metadata is stored in two places simultaneously so it survives subsetting and merging:

1. **Per-column attribute** (`attr(col, "label")`, `attr(col, "format")`, `attr(col, "width")`)
2. **Data-frame attribute** (`attr(df, "var_labels")`, `attr(df, "var_formats")`, `attr(df, "var_lengths")`)

Both are written to XPT and SAS7BDAT files and restored on read.

---

## Function reference

| Category | Functions |
|----------|-----------|
| **Read** | `read_xpt()`, `read_sas()`, `read_pharma_excel()`, `read_pharma_csv()` |
| **Write** | `write_xpt()`, `write_sas()`, `write_pharma_excel()`, `write_pharma_csv()` |
| **Labels** | `set_var_label()`, `get_var_label()` |
| **Formats** | `set_var_format()`, `get_var_format()` |
| **Lengths** | `set_var_length()`, `get_var_length()` |
| **Spec** | `import_spec()`, `apply_spec()`, `get_metadata()` |
| **Validate** | `submission_check()`, `check_xpt_names()`, `check_var_lengths()` |
| **Sanitise** | `sanitize_col_names()` |

---

## Related packages

- [`admiral`](https://github.com/pharmaverse/admiral) — ADaM derivation
- [`metacore`](https://github.com/pharmaverse/metacore) — metadata specification
- [`data.table`](https://github.com/Rdatatable/data.table) — fast CSV I/O backend
- [`openxlsx2`](https://github.com/JanMarvin/openxlsx2) — Excel write backend

---

## Author

**Mahesh Divakaran** · [GitHub](https://github.com/itsmdivakaran) · [ORCID](https://orcid.org/0000-0002-3488-0857)

---

## License

MIT © Mahesh Divakaran
