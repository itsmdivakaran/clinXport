// clinXport — SAS7BDAT reader and writer
// Implements a self-contained binary reader for SAS7BDAT files and a writer
// for uncompressed SAS7BDAT files (no RLE/RDC compression).
//
// Format reference:
//   Clint Cummins, "SAS7BDAT Database Binary Format" (2014)
//   Matthew Shotwell, "Reverse Engineering the SAS7BDAT" (2011)
//
// Limitations of the writer:
//   • Only uncompressed data pages (no CHAR/BINARY compression)
//   • Numeric columns stored as 8-byte IEEE doubles
//   • Character columns: blank-padded fixed width
//   • SAS 9.4 compatible page structure

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include "ibm_float.h"

#include <fstream>
#include <cstring>
#include <ctime>
#include <cmath>
#include <vector>
#include <string>
#include <stdexcept>
#include <algorithm>
#include <limits>

using namespace Rcpp;
using namespace clinxport;

// ============================================================
// Constants
// ============================================================
static const uint8_t SAS7BDAT_MAGIC[32] = {
    0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00, 0xc2,0xea,0x81,0x60,
    0xb3,0x14,0x11,0xcf, 0xbd,0x92,0x08,0x00,
    0x09,0xc7,0x31,0x8c, 0x18,0x1f,0x10,0x11
};

static const int SAS_HEADER_SIZE = 1024;   // minimum header size (SAS 9.x)
static const int SAS_PAGE_SIZE   = 4096;   // default page size for new files
// Page types
static const int PAGE_META  = 0;    // metadata
static const int PAGE_DATA  = 256;  // data only
static const int PAGE_MIX   = 512;  // mixed (metadata + data)

// Subheader signatures (32-bit SAS)
static const uint32_t SH_ROW_SIZE  = 0xF7F7F7F7u;
static const uint32_t SH_COL_SIZE  = 0xF6F6F6F6u;
static const uint32_t SH_COL_TEXT  = 0xFFFFFFFDu;
static const uint32_t SH_COL_NAME  = 0xFFFFFFFFu;
static const uint32_t SH_COL_ATTR  = 0xFFFFFFFEu;
static const uint32_t SH_COL_FMT   = 0xFFFBFFF9u;

// ============================================================
// Utility
// ============================================================
static uint16_t read16le(const uint8_t* p) {
    return (uint16_t)(p[0] | ((uint16_t)p[1] << 8));
}
static uint16_t read16be(const uint8_t* p) {
    return (uint16_t)(((uint16_t)p[0] << 8) | p[1]);
}
static uint64_t read64le(const uint8_t* p) {
    uint64_t v = 0;
    for (int i = 7; i >= 0; i--) v = (v << 8) | p[i];
    return v;
}
static uint64_t read64be(const uint8_t* p) {
    uint64_t v = 0;
    for (int i = 0; i < 8; i++) v = (v << 8) | p[i];
    return v;
}
static uint32_t read32le(const uint8_t* p) {
    return (uint32_t)(p[0] | ((uint32_t)p[1]<<8) | ((uint32_t)p[2]<<16) | ((uint32_t)p[3]<<24));
}
static uint32_t read32be(const uint8_t* p) {
    return (uint32_t)(((uint32_t)p[0]<<24) | ((uint32_t)p[1]<<16) | ((uint32_t)p[2]<<8) | p[3]);
}
static std::string trim_right(const char* s, int n) {
    while (n > 0 && (s[n-1] == ' ' || s[n-1] == '\0')) --n;
    return std::string(s, n);
}

static void write16le(uint8_t* p, uint16_t v) {
    p[0] = v & 0xFF; p[1] = (v >> 8) & 0xFF;
}
static void write32le(uint8_t* p, uint32_t v) {
    p[0]=v&0xFF; p[1]=(v>>8)&0xFF; p[2]=(v>>16)&0xFF; p[3]=(v>>24)&0xFF;
}
static void write64le(uint8_t* p, uint64_t v) {
    for(int i=0;i<8;i++) { p[i]=v&0xFF; v>>=8; }
}

// ============================================================
// SAS7BDAT READER
// ============================================================

//' Read a SAS7BDAT file into a data frame
//'
//' @param path Path to the .sas7bdat file.
//' @return A data.frame with metadata attributes (`var_labels`, `var_formats`, `var_lengths`, `dataset_name`).
//' @keywords internal
//' @export
// [[Rcpp::export]]
SEXP cx_read_sas7bdat(std::string path) {
    std::ifstream ifs(path, std::ios::binary);
    if (!ifs.is_open()) stop("Cannot open file: " + path);

    ifs.seekg(0, std::ios::end);
    size_t fsize = (size_t)ifs.tellg();
    ifs.seekg(0, std::ios::beg);

    if (fsize < (size_t)SAS_HEADER_SIZE) stop("File too small to be SAS7BDAT: " + path);

    std::vector<uint8_t> file(fsize);
    ifs.read((char*)file.data(), (std::streamsize)fsize);
    ifs.close();

    // Validate magic
    if (std::memcmp(file.data(), SAS7BDAT_MAGIC, 32) != 0) {
        stop("Not a valid SAS7BDAT file (magic bytes mismatch): " + path);
    }

    // ── Header fields ─────────────────────────────────────────
    // Byte 37: endianness (0x01 = little-endian, 0x00 = big-endian)
    // (Byte 84 is the START of the dataset name, NOT the endianness byte.)
    bool little_endian = (file[37] == 0x01);

    // Helper: endian-aware 16/32 reads
    auto u16 = [&](const uint8_t* p) -> uint16_t {
        return little_endian ? read16le(p) : read16be(p);
    };
    auto u32 = [&](const uint8_t* p) -> uint32_t {
        return little_endian ? read32le(p) : read32be(p);
    };

    // Byte 164 == 0x33 → 64-bit SAS (U64 alignment flag, align2)
    // Byte 20 == 0x33 → align1: adds 4 to the page-field offsets below
    bool sas64   = (file[164] == 0x33);
    int  align1  = (file[20]  == 0x33) ? 4 : 0;  // extra offset shift for page fields

    // Header size at 196+align1 (always uint32_t, both bitnesses)
    uint32_t header_size = u32(file.data() + 196 + align1);
    if (header_size < 1024) header_size = 1024;
    // Fallback: 64-bit SAS uses 8192-byte header
    if (header_size >= 8192) sas64 = true;

    // Page size at offset 200+align1 (uint32_t)
    uint32_t page_size = u32(file.data() + 200 + align1);
    if (page_size < 256) page_size = 4096;

    // Page count at offset 204+align1
    //   32-bit: 4 bytes; 64-bit: 8 bytes
    uint64_t page_count;
    if (sas64) {
        page_count = little_endian ? read64le(file.data() + 204 + align1)
                                   : read64be(file.data() + 204 + align1);
    } else {
        page_count = u32(file.data() + 204 + align1);
    }

    // Dataset name at offset 84, 64 bytes (blank-padded)
    std::string dataset_name = trim_right((char*)file.data() + 84, 64);

    // ── Architecture-dependent sizes and offsets ──────────────
    int int_len     = sas64 ? 8 : 4;   // pointer/int field width
    int ph_size     = sas64 ? 32 : 24; // page header size
    int sh_ptr_size = sas64 ? 24 : 12; // subheader pointer record size
                                        // 32-bit: off(4)+len(4)+type(1)+pad(3)
                                        // 64-bit: off(8)+len(8)+type(1)+pad(7)

    // Page header field offsets (from page start)
    // 32-bit: type@16, block_count@18, sh_count@20, data_row_count@22
    // 64-bit: type@24, block_count@26, sh_count@28, data_row_count@30
    int ph_type_off      = sas64 ? 24 : 16;
    int ph_block_off     = sas64 ? 26 : 18;
    int ph_shcount_off   = sas64 ? 28 : 20;
    int ph_datacount_off = sas64 ? 30 : 22;

    // ── Column metadata from subheaders ──────────────────────
    struct ColInfo {
        std::string name;
        std::string label;
        std::string format;
        int         length = 8;
        int         offset = 0;
        int         type   = 1;   // 1=num, 2=char
    };

    int row_length = 0;
    int total_rows = 0;
    int col_count  = 0;
    std::vector<ColInfo> cols;

    // Text storage for column names/labels/formats
    std::vector<std::vector<uint8_t>> text_blocks;

    // read_int: endian-aware variable-width integer reader
    auto read_int = [&](const uint8_t* p, int bytes) -> uint64_t {
        uint64_t v = 0;
        if (little_endian) {
            for (int i = bytes - 1; i >= 0; i--) v = (v << 8) | p[i];
        } else {
            for (int i = 0; i < bytes; i++) v = (v << 8) | p[i];
        }
        return v;
    };

    // Running column index across multiple SH_COL_NAME subheaders
    int col_name_idx = 0;

    // ── First pass: iterate pages to collect metadata ─────────
    for (uint64_t pg = 0; pg < page_count; pg++) {
        size_t pg_off = (size_t)header_size + pg * (size_t)page_size;
        if (pg_off + page_size > fsize) break;
        const uint8_t* page = file.data() + pg_off;

        uint16_t page_type = u16(page + ph_type_off);
        if (page_type == PAGE_DATA) continue;  // pure data, skip in first pass

        uint16_t sh_count = u16(page + ph_shcount_off);

        for (uint16_t si = 0; si < sh_count; si++) {
            size_t sh_ptr_off = (size_t)ph_size + (size_t)si * sh_ptr_size;
            if (sh_ptr_off + (size_t)sh_ptr_size > (size_t)page_size) break;
            const uint8_t* sh_ptr = page + sh_ptr_off;

            uint32_t sh_offset = (uint32_t)read_int(sh_ptr,          int_len);
            uint32_t sh_length = (uint32_t)read_int(sh_ptr + int_len, int_len);
            // sh_type at 2*int_len+1: if 1 = truncated subheader (no signature), skip
            uint8_t sh_type = sh_ptr[2 * int_len + 1];

            if (sh_length == 0 || sh_offset + sh_length > (uint32_t)page_size) continue;
            if (sh_type == 1) continue;  // truncated/empty subheader

            const uint8_t* sh = page + sh_offset;
            uint32_t sig = little_endian ? read32le(sh) : read32be(sh);

            // ── Row Size ──────────────────────────────────────
            if (sig == SH_ROW_SIZE) {
                // row_length at 5*int_len, total_rows at 6*int_len
                // col_count = p1 (9*int_len) + p2 (10*int_len)
                row_length = (int)read_int(sh + 5 * int_len, int_len);
                total_rows = (int)read_int(sh + 6 * int_len, int_len);
                int cp1    = (int)read_int(sh + 9 * int_len, int_len);
                int cp2    = (int)read_int(sh + 10 * int_len, int_len);
                col_count  = cp1 + cp2;
            }
            // ── Column Text (name/label/format string pool) ───
            else if (sig == SH_COL_TEXT) {
                if ((int)sh_length > 2 * int_len) {
                    // Text starts immediately after the 2*int_len subheader header.
                    // The first 2 bytes of the text data are a length field; actual
                    // strings are offset from the START of the text data (including
                    // that length word), so store from sh + 2*int_len.
                    std::vector<uint8_t> tb(sh + 2 * int_len, sh + sh_length);
                    text_blocks.push_back(std::move(tb));
                }
            }
            // ── Column Names ──────────────────────────────────
            else if (sig == SH_COL_NAME) {
                int entry_stride = sas64 ? 16 : 8;
                int n_entries = ((int)sh_length - 2 * int_len) / entry_stride;
                if (n_entries <= 0) continue;

                for (int ci = 0; ci < n_entries; ci++) {
                    int gcol = col_name_idx + ci;
                    if ((int)cols.size() <= gcol) cols.resize(gcol + 1);
                    const uint8_t* entry = sh + 2 * int_len + ci * entry_stride;
                    // Entry layout (same for 32/64-bit, just different stride):
                    //   +0: text_block_ref (uint16_t)
                    //   +2: name offset in text block (uint16_t)
                    //   +4: name length (uint16_t)
                    uint16_t text_ref = read16le(entry + 0);
                    uint16_t name_off = read16le(entry + 2);
                    uint16_t name_len = read16le(entry + 4);
                    if (text_ref < (uint16_t)text_blocks.size()) {
                        auto& tb = text_blocks[text_ref];
                        if (name_len > 0 && (size_t)(name_off + name_len) <= tb.size())
                            cols[gcol].name = trim_right((char*)tb.data() + name_off, name_len);
                    }
                }
                col_name_idx += n_entries;
            }
            // ── Column Attributes (offset, width, type) ───────
            else if (sig == SH_COL_ATTR) {
                // 32-bit entry: off(4)+len(4)+type(1)+pad(3) = 12 bytes
                // 64-bit entry: off(8)+len(4)+type(1)+pad(3) = 16 bytes
                int entry_size = sas64 ? 16 : 12;
                int n_entries  = ((int)sh_length - 2 * int_len) / entry_size;
                if ((int)cols.size() < n_entries) cols.resize(n_entries);
                for (int ci = 0; ci < n_entries; ci++) {
                    const uint8_t* entry = sh + 2 * int_len + ci * entry_size;
                    cols[ci].offset = (int)read_int(entry,          int_len);
                    cols[ci].length = (int)read_int(entry + int_len, 4);
                    cols[ci].type   = (entry[int_len + 4] == 1) ? 1 : 2;
                }
            }
            // ── Column Formats & Labels ───────────────────────
            else if (sig == SH_COL_FMT) {
                // 32-bit entry (24 bytes):
                //   +0:  fmt text_ref(2), +2: fmt_off(2), +4: fmt_len(2)
                //   +6:  fmt_width(2),    +8: fmt_dec(2), +10: pad(2)
                //   +12: lbl text_ref(2), +14: lbl_off(2), +16: lbl_len(2)
                //   +18..23: padding
                // 64-bit entry (52 bytes):
                //   +0:  fmt_ref(2), +2: fmt_off(2), +4: fmt_len(2)
                //   +6:  fmt_width(2), +8: fmt_dec(2), +10..19: padding
                //   +20: lbl_ref(2), +22: lbl_off(2), +24: lbl_len(2)
                //   +26..51: padding
                int entry_size = sas64 ? 52 : 24;
                int lbl_start  = sas64 ? 20 : 12;
                int n_entries  = ((int)sh_length - 2 * int_len) / entry_size;
                if ((int)cols.size() < n_entries) cols.resize(n_entries);
                for (int ci = 0; ci < n_entries && ci < (int)cols.size(); ci++) {
                    const uint8_t* entry = sh + 2 * int_len + ci * entry_size;
                    uint16_t fmt_ref = read16le(entry + 0);
                    uint16_t fmt_off = read16le(entry + 2);
                    uint16_t fmt_len = read16le(entry + 4);
                    uint16_t lbl_ref = read16le(entry + lbl_start);
                    uint16_t lbl_off = read16le(entry + lbl_start + 2);
                    uint16_t lbl_len = read16le(entry + lbl_start + 4);

                    if (fmt_ref < (uint16_t)text_blocks.size() && fmt_len > 0) {
                        auto& tb = text_blocks[fmt_ref];
                        if ((size_t)(fmt_off + fmt_len) <= tb.size())
                            cols[ci].format = trim_right((char*)tb.data() + fmt_off, fmt_len);
                    }
                    if (lbl_ref < (uint16_t)text_blocks.size() && lbl_len > 0) {
                        auto& tb = text_blocks[lbl_ref];
                        if ((size_t)(lbl_off + lbl_len) <= tb.size())
                            cols[ci].label = trim_right((char*)tb.data() + lbl_off, lbl_len);
                    }
                }
            }
        }
    }

    if (col_count > 0 && (int)cols.size() < col_count) cols.resize(col_count);

    if (cols.empty() || total_rows <= 0 || row_length <= 0) {
        List empty;
        empty.attr("class") = "data.frame";
        empty.attr("row.names") = IntegerVector(0);
        empty.attr("dataset_name") = dataset_name;
        Rcpp::warning(
            "SAS7BDAT: could not parse column metadata "
            "(cols=%d total_rows=%d row_length=%d). "
            "File may use unsupported compression or format.",
            (int)cols.size(), total_rows, row_length);
        return empty;
    }

    int ncol = (int)cols.size();
    int nrow = total_rows;

    // ── Allocate output columns ───────────────────────────────
    std::vector<NumericVector>   num_cols;
    std::vector<CharacterVector> chr_cols;
    std::vector<int> col_vec_idx(ncol, -1);

    for (int i = 0; i < ncol; i++) {
        if (cols[i].type == 2) {
            col_vec_idx[i] = (int)chr_cols.size();
            chr_cols.emplace_back(nrow, NA_STRING);
        } else {
            col_vec_idx[i] = (int)num_cols.size();
            num_cols.emplace_back(nrow, NA_REAL);
        }
    }

    // ── Second pass: read data rows ───────────────────────────
    int row_idx = 0;
    for (uint64_t pg = 0; pg < page_count && row_idx < nrow; pg++) {
        size_t pg_off = (size_t)header_size + pg * (size_t)page_size;
        if (pg_off + page_size > fsize) break;
        const uint8_t* page = file.data() + pg_off;

        uint16_t page_type = u16(page + ph_type_off);
        if (page_type != PAGE_DATA && page_type != PAGE_MIX) continue;

        uint16_t block_count, sh_count_p2;
        size_t data_start;

        if (page_type == PAGE_DATA) {
            // block_count at ph_block_off = number of data rows
            block_count  = u16(page + ph_block_off);
            sh_count_p2  = 0;
            data_start   = (size_t)ph_size;
        } else {
            // MIX page: sh_count at ph_shcount_off, data rows at ph_datacount_off
            sh_count_p2 = u16(page + ph_shcount_off);
            block_count = u16(page + ph_datacount_off);
            // Data rows start after subheader pointer table
            data_start  = (size_t)ph_size + (size_t)sh_count_p2 * sh_ptr_size;
        }

        for (int dr = 0; dr < (int)block_count && row_idx < nrow; dr++) {
            size_t row_off = data_start + (size_t)dr * row_length;
            if (row_off + (size_t)row_length > (size_t)page_size) break;
            const uint8_t* row = page + row_off;

            for (int ci = 0; ci < ncol; ci++) {
                const uint8_t* field = row + cols[ci].offset;
                int vlen = cols[ci].length;

                if (cols[ci].type == 2) {
                    // Character: strip trailing spaces/nulls
                    int end = vlen;
                    while (end > 0 && (field[end-1] == ' ' || field[end-1] == '\0')) --end;
                    chr_cols[col_vec_idx[ci]][row_idx] =
                        end > 0 ? std::string((char*)field, end) : std::string("");
                } else {
                    // Numeric: IEEE double (SAS7BDAT uses IEEE, not IBM float)
                    // Numerics are always 8 bytes; vlen < 8 means truncated (rare)
                    uint8_t tmp[8] = {};
                    int copy = std::min(vlen, 8);
                    if (little_endian) {
                        std::memcpy(tmp, field, copy);
                    } else {
                        // Big-endian: most-significant bytes first
                        for (int k = 0; k < copy; k++)
                            tmp[8 - copy + k] = field[k];
                    }
                    double val;
                    std::memcpy(&val, tmp, 8);
                    // SAS missing values are special NaNs; treat any NaN as NA
                    if (!std::isfinite(val)) {
                        num_cols[col_vec_idx[ci]][row_idx] = NA_REAL;
                    } else {
                        num_cols[col_vec_idx[ci]][row_idx] = val;
                    }
                }
            }
            row_idx++;
        }
    }

    // ── Build result data frame ───────────────────────────────
    List result(ncol);
    CharacterVector rnames(ncol);
    CharacterVector out_labels(ncol);
    CharacterVector out_formats(ncol);
    IntegerVector   out_lengths(ncol);

    for (int i = 0; i < ncol; i++) {
        rnames[i]      = cols[i].name;
        out_labels[i]  = cols[i].label;
        out_formats[i] = cols[i].format;
        out_lengths[i] = cols[i].length;

        SEXP col_sexp;
        if (cols[i].type == 2) {
            CharacterVector cv = chr_cols[col_vec_idx[i]];
            if (!cols[i].label.empty())  cv.attr("label")  = cols[i].label;
            if (!cols[i].format.empty()) cv.attr("format") = cols[i].format;
            col_sexp = cv;
        } else {
            NumericVector nv = num_cols[col_vec_idx[i]];
            if (!cols[i].label.empty())  nv.attr("label")  = cols[i].label;
            if (!cols[i].format.empty()) nv.attr("format") = cols[i].format;
            col_sexp = nv;
        }
        result[i] = col_sexp;
    }

    result.names() = rnames;
    result.attr("class") = "data.frame";
    IntegerVector row_names(nrow);
    for (int i = 0; i < nrow; i++) row_names[i] = i + 1;
    result.attr("row.names") = row_names;

    out_labels.names()  = rnames;
    out_formats.names() = rnames;
    out_lengths.names() = rnames;
    result.attr("var_labels")   = out_labels;
    result.attr("var_formats")  = out_formats;
    result.attr("var_lengths")  = out_lengths;
    result.attr("dataset_name") = dataset_name;

    return result;
}

// ============================================================
// SAS7BDAT WRITER (uncompressed, SAS 9.4 compatible)
// ============================================================

//' Write a data frame to SAS7BDAT format (uncompressed)
//'
//' @param df           Data frame to write.
//' @param path         Output .sas7bdat file path.
//' @param dataset_name SAS dataset name (≤64 chars).
//' @param var_labels   Named character vector of variable labels.
//' @param var_formats  Named character vector of SAS format strings.
//' @param var_lengths  Named integer vector of character variable lengths (numerics always 8).
//' @return Invisibly returns `path`.
//' @keywords internal
//' @export
// [[Rcpp::export]]
SEXP cx_write_sas7bdat(DataFrame df,
                       std::string path,
                       std::string dataset_name = "DATASET",
                       Nullable<CharacterVector> var_labels  = R_NilValue,
                       Nullable<CharacterVector> var_formats = R_NilValue,
                       Nullable<IntegerVector>   var_lengths = R_NilValue)
{
    int ncol = df.size();
    int nrow = df.nrows();
    CharacterVector colnames = df.names();

    // ── Resolve metadata ─────────────────────────────────────
    std::vector<std::string> labels(ncol, "");
    std::vector<std::string> formats(ncol, "");
    std::vector<int>         user_lengths(ncol, 0);

    auto fill_str_map = [&](Nullable<CharacterVector> nv, std::vector<std::string>& vec) {
        if (nv.isNotNull()) {
            CharacterVector cv(nv);
            if (!cv.hasAttribute("names")) return;
            CharacterVector cvnames = cv.names();
            for (int i = 0; i < ncol; i++) {
                std::string cn = as<std::string>(colnames[i]);
                for (int j = 0; j < cvnames.size(); j++) {
                    if (as<std::string>(cvnames[j]) == cn) {
                        if (cv[j] != NA_STRING) vec[i] = as<std::string>(cv[j]);
                        break;
                    }
                }
            }
        }
    };

    fill_str_map(var_labels,  labels);
    fill_str_map(var_formats, formats);

    if (var_lengths.isNotNull()) {
        IntegerVector lv(var_lengths);
        if (lv.hasAttribute("names")) {
            CharacterVector lvnames = lv.names();
            for (int i = 0; i < ncol; i++) {
                std::string cn = as<std::string>(colnames[i]);
                for (int j = 0; j < lvnames.size(); j++) {
                    if (as<std::string>(lvnames[j]) == cn && lv[j] != NA_INTEGER) {
                        user_lengths[i] = lv[j];
                        break;
                    }
                }
            }
        }
    }

    // ── Column layout ─────────────────────────────────────────
    struct ColDef {
        std::string name;
        std::string label;
        std::string format;
        int type;    // 1=num, 2=char
        int length;  // bytes
        int offset;  // in row
    };

    std::vector<ColDef> coldefs(ncol);
    int row_len = 0;

    for (int i = 0; i < ncol; i++) {
        ColDef& c = coldefs[i];
        c.name   = as<std::string>(colnames[i]);
        c.label  = labels[i];
        c.format = formats[i];
        SEXP col  = df[i];
        bool is_char = (TYPEOF(col) == STRSXP) || Rf_isFactor(col);
        c.type = is_char ? 2 : 1;
        if (is_char) {
            int maxw = user_lengths[i] > 0 ? user_lengths[i] : 8;
            if (user_lengths[i] == 0) {
                for (int r = 0; r < nrow; r++) {
                    const char* s = "";
                    if (Rf_isFactor(col)) {
                        IntegerVector fv(col);
                        CharacterVector lvls = fv.attr("levels");
                        int fi = fv[r];
                        if (fi != NA_INTEGER) s = CHAR(STRING_ELT(lvls, fi-1));
                    } else {
                        SEXP el = STRING_ELT(col, r);
                        if (el != NA_STRING) s = CHAR(el);
                    }
                    int sw = (int)strlen(s);
                    if (sw > maxw) maxw = sw;
                }
                maxw = std::max(1, std::min(maxw, 32767));
            }
            c.length = maxw;
        } else {
            c.length = 8;
        }
        c.offset = row_len;
        row_len += c.length;
    }

    // ── Page layout ──────────────────────────────────────────
    const int pg_size       = SAS_PAGE_SIZE;
    const int ph_size       = 24;             // 32-bit page header
    const int sh_ptr_size   = 12;             // subheader pointer (32-bit)

    // Build text blocks for column metadata
    // text_block: single buffer with all names/labels/formats
    std::string text_buf;
    std::vector<uint16_t> name_offs(ncol), name_lens(ncol);
    std::vector<uint16_t> label_offs(ncol), label_lens(ncol);
    std::vector<uint16_t> fmt_offs(ncol), fmt_lens(ncol);

    for (int i = 0; i < ncol; i++) {
        name_offs[i] = (uint16_t)text_buf.size();
        name_lens[i] = (uint16_t)coldefs[i].name.size();
        text_buf += coldefs[i].name;

        label_offs[i] = (uint16_t)text_buf.size();
        label_lens[i] = (uint16_t)coldefs[i].label.size();
        text_buf += coldefs[i].label;

        fmt_offs[i] = (uint16_t)text_buf.size();
        fmt_lens[i] = (uint16_t)coldefs[i].format.size();
        text_buf += coldefs[i].format;
    }

    // ── Compute rows per page ────────────────────────────────
    int rows_per_page = std::max(1, (pg_size - ph_size) / row_len);

    // Build subheaders as byte vectors
    // 1. Row size
    std::vector<uint8_t> sh_rowsize(48, 0);
    write32le(sh_rowsize.data() + 0,  SH_ROW_SIZE);
    write32le(sh_rowsize.data() + 20, (uint32_t)row_len);
    write32le(sh_rowsize.data() + 24, (uint32_t)nrow);
    write32le(sh_rowsize.data() + 36, (uint32_t)ncol);
    write32le(sh_rowsize.data() + 40, 0);

    // 2. Col size
    std::vector<uint8_t> sh_colsize(8, 0);
    write32le(sh_colsize.data() + 0, SH_COL_SIZE);
    write32le(sh_colsize.data() + 4, (uint32_t)ncol);

    // 3. Text block
    std::vector<uint8_t> sh_coltext(8 + text_buf.size(), 0);
    write32le(sh_coltext.data() + 0, SH_COL_TEXT);
    std::memcpy(sh_coltext.data() + 8, text_buf.c_str(), text_buf.size());

    // 4. Column names (8 bytes header + ncol*8 entries)
    std::vector<uint8_t> sh_colname(8 + ncol * 8, 0);
    write32le(sh_colname.data() + 0, SH_COL_NAME);
    for (int i = 0; i < ncol; i++) {
        uint8_t* e = sh_colname.data() + 8 + i * 8;
        write16le(e + 0, 0);              // text block index 0
        write16le(e + 2, name_offs[i]);
        write16le(e + 4, name_lens[i]);
    }

    // 5. Column attributes (8 bytes header + ncol*12 entries)
    std::vector<uint8_t> sh_colattr(8 + ncol * 12, 0);
    write32le(sh_colattr.data() + 0, SH_COL_ATTR);
    for (int i = 0; i < ncol; i++) {
        uint8_t* e = sh_colattr.data() + 8 + i * 12;
        write32le(e + 0, (uint32_t)coldefs[i].offset);
        write32le(e + 4, (uint32_t)coldefs[i].length);
        e[8] = (uint8_t)coldefs[i].type;
        e[9] = 0; e[10] = 0; e[11] = 0;
    }

    // 6. Column format (8 bytes header + ncol*24 entries)
    std::vector<uint8_t> sh_colfmt(8 + ncol * 24, 0);
    write32le(sh_colfmt.data() + 0, SH_COL_FMT);
    for (int i = 0; i < ncol; i++) {
        uint8_t* e = sh_colfmt.data() + 8 + i * 24;
        write16le(e + 0,  0);              // format text block index
        write16le(e + 2,  fmt_offs[i]);
        write16le(e + 4,  fmt_lens[i]);
        write16le(e + 6,  0);              // format width
        write16le(e + 8,  0);              // format decimals
        write16le(e + 12, 0);              // label text block index
        write16le(e + 14, label_offs[i]);
        write16le(e + 16, label_lens[i]);
    }

    // ── Collect all subheaders ────────────────────────────────
    std::vector<std::vector<uint8_t>*> subheaders = {
        &sh_rowsize, &sh_colsize, &sh_coltext,
        &sh_colname, &sh_colattr, &sh_colfmt
    };
    int n_sh = (int)subheaders.size();

    // ── Build META page ───────────────────────────────────────
    std::vector<uint8_t> meta_page(pg_size, 0);

    // Lay subheaders at end of page (growing backwards)
    std::vector<uint16_t> sh_offsets(n_sh), sh_sizes(n_sh);
    int sh_space = pg_size;
    for (int i = n_sh - 1; i >= 0; i--) {
        int sz = (int)subheaders[i]->size();
        // Pad to 4-byte alignment
        int aligned_sz = (sz + 3) & ~3;
        sh_space -= aligned_sz;
        sh_offsets[i] = (uint16_t)sh_space;
        sh_sizes[i]   = (uint16_t)sz;
        std::memcpy(meta_page.data() + sh_space, subheaders[i]->data(), sz);
    }

    // Subheader pointer table starts at ph_size
    for (int i = 0; i < n_sh; i++) {
        uint8_t* ptr = meta_page.data() + ph_size + i * sh_ptr_size;
        write32le(ptr + 0, sh_offsets[i]);
        write32le(ptr + 4, sh_sizes[i]);
        ptr[9] = 0;
    }

    // Page header
    write16le(meta_page.data() + 16, (uint16_t)PAGE_META);
    write16le(meta_page.data() + 18, 0);               // block count (data)
    write16le(meta_page.data() + 20, (uint16_t)n_sh);  // subheader count
    write16le(meta_page.data() + 22, 0);               // data row count

    // ── Build DATA pages ──────────────────────────────────────
    int n_data_pages = (nrow + rows_per_page - 1) / rows_per_page;
    std::vector<std::vector<uint8_t>> data_pages(n_data_pages,
                                                  std::vector<uint8_t>(pg_size, 0x20)); // blank

    int total_pages = 1 + n_data_pages;
    std::vector<uint8_t> row_buf(row_len, 0x20);

    for (int pg = 0; pg < n_data_pages; pg++) {
        std::vector<uint8_t>& dp = data_pages[pg];
        int rows_in_page = std::min(rows_per_page, nrow - pg * rows_per_page);

        // Page header
        write16le(dp.data() + 16, (uint16_t)PAGE_DATA);
        write16le(dp.data() + 18, (uint16_t)rows_in_page);
        write16le(dp.data() + 20, 0);
        write16le(dp.data() + 22, (uint16_t)rows_in_page);

        for (int ri = 0; ri < rows_in_page; ri++) {
            int global_row = pg * rows_per_page + ri;
            std::fill(row_buf.begin(), row_buf.end(), 0x20);

            for (int ci = 0; ci < ncol; ci++) {
                uint8_t* dest = row_buf.data() + coldefs[ci].offset;
                SEXP col = df[ci];

                if (coldefs[ci].type == 2) {
                    const char* s = "";
                    if (Rf_isFactor(col)) {
                        IntegerVector fv(col);
                        CharacterVector lvls = fv.attr("levels");
                        int fi = fv[global_row];
                        if (fi != NA_INTEGER) s = CHAR(STRING_ELT(lvls, fi-1));
                    } else {
                        SEXP el = STRING_ELT(col, global_row);
                        if (el != NA_STRING) s = CHAR(el);
                    }
                    int slen = (int)strlen(s);
                    int copy = std::min(slen, coldefs[ci].length);
                    std::memcpy(dest, s, copy);
                } else {
                    double val = NA_REAL;
                    int rtype = TYPEOF(col);
                    if (rtype == REALSXP)      val = REAL(col)[global_row];
                    else if (rtype == INTSXP) {
                        int iv = INTEGER(col)[global_row];
                        val = (iv == NA_INTEGER) ? NA_REAL : (double)iv;
                    } else if (rtype == LGLSXP) {
                        int iv = LOGICAL(col)[global_row];
                        val = (iv == NA_LOGICAL) ? NA_REAL : (double)iv;
                    }
                    // Write IEEE double little-endian
                    if (ISNAN(val)) {
                        // SAS missing: write IEEE NaN with sign bit pattern
                        uint64_t nan_val = 0x7FF0000000000000ULL | 0x002E000000000000ULL; // '.'
                        write64le(dest, nan_val);
                    } else {
                        write64le(dest, *(uint64_t*)&val);
                    }
                }
            }
            // Write row into page
            int row_off_in_pg = ph_size + ri * row_len;
            std::memcpy(dp.data() + row_off_in_pg, row_buf.data(), row_len);
        }
    }

    // ── Build file header ─────────────────────────────────────
    std::vector<uint8_t> header(SAS_HEADER_SIZE, 0);
    // Magic bytes
    std::memcpy(header.data(), SAS7BDAT_MAGIC, 32);
    // Endianness byte used by the reader and common SAS7BDAT tooling.
    header[37] = 0x01; // little endian
    // Dataset name at 84
    std::string dsname_pad = dataset_name.substr(0, 64);
    std::memcpy(header.data() + 84, dsname_pad.c_str(), dsname_pad.size());
    header[64]  = 0x01; // W = Windows alignment
    // SAS release at 196
    std::string rel = "9.0401M4";
    std::memcpy(header.data() + 216, rel.c_str(), 8);
    // OS name at 172
    std::string osname = "W32_VSPRO";
    std::memcpy(header.data() + 172, osname.c_str(), osname.size());
    // Header size at 196
    write32le(header.data() + 196, (uint32_t)SAS_HEADER_SIZE);
    // Page size at 200
    write32le(header.data() + 200, (uint32_t)pg_size);
    // Page count at 204
    write32le(header.data() + 204, (uint32_t)total_pages);
    // ── Write to file ─────────────────────────────────────────
    std::ofstream ofs(path, std::ios::binary);
    if (!ofs.is_open()) stop("Cannot open file for writing: " + path);

    ofs.write((char*)header.data(),     SAS_HEADER_SIZE);
    ofs.write((char*)meta_page.data(),  pg_size);
    for (auto& dp : data_pages) {
        ofs.write((char*)dp.data(), pg_size);
    }
    ofs.close();

    return wrap(path);
}
