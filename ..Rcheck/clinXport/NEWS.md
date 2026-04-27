# clinXport 0.1.0

## Initial release

### New features

* **SAS XPT v5/v8 read/write** — custom C++ engine with full IBM float ↔ IEEE 754 conversion; no dependency on `haven` or `xportr`.
* **SAS7BDAT read/write** — C++ reader handles META/DATA/MIX pages, little-endian and big-endian files; writer produces uncompressed SAS 9.4–compatible files.
* **Excel I/O** — `read_pharma_excel()` / `write_pharma_excel()` via `openxlsx2`; supports multi-sheet export, clinical header styling, label rows, and automatic spec sheets.
* **CSV I/O** — `read_pharma_csv()` / `write_pharma_csv()` via `data.table`; optional `.spec.csv` sidecar for metadata roundtrip.
* **Metadata management** — `set_var_label()`, `get_var_label()`, `set_var_format()`, `get_var_format()`, `set_var_length()`, `get_var_length()`, `get_metadata()`.
* **Spec-driven workflow** — `import_spec()` loads variable specifications from Excel or CSV; `apply_spec()` applies labels, formats, lengths, type coercions, and column reordering.
* **Submission validation** — `submission_check()` returns an ERROR / WARNING / INFO report covering XPT naming, label completeness, format syntax, character lengths, CDISC required variables, and infinite value detection.
* **Column sanitisation** — `sanitize_col_names()` auto-corrects names for XPT v5 (≤ 8 chars) and v8 (≤ 32 chars); `check_xpt_names()` reports violations.
