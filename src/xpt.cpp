// clinXport — XPT v5/v8 reader and writer
// SAS Transport File Format (XPORT engine) version 5 and version 8.
// Reference: SAS Institute "SAS Technical Support — TS-140" and
//            "SAS XPORT Transport File Format" specification.
//
// XPT v5: column names ≤8 chars (uppercase), obs records padded to 80 bytes.
// XPT v8: column names ≤32 chars, obs records padded to 80 bytes.

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include "ibm_float.h"

#include <fstream>
#include <sstream>
#include <iomanip>
#include <cstring>
#include <ctime>
#include <stdexcept>
#include <vector>
#include <string>
#include <algorithm>
#include <cctype>
#include <limits>

using namespace Rcpp;
using namespace clinxport;

// ============================================================
// Helpers
// ============================================================
static std::string pad_right(const std::string& s, size_t n, char c = ' ') {
    if (s.size() >= n) return s.substr(0, n);
    return s + std::string(n - s.size(), c);
}

static std::string pad_left(const std::string& s, size_t n, char c = ' ') {
    if (s.size() >= n) return s.substr(0, n);
    return std::string(n - s.size(), c) + s;
}

static std::string xpt_date() {
    time_t t = time(nullptr);
    struct tm* tm_info = localtime(&t);
    char buf[20];
    // SAS XPT date format: DDmmmYY:HH:MM:SS (16 chars)
    strftime(buf, sizeof(buf), "%d%b%y:%H:%M:%S", tm_info);
    // Uppercase month
    for (int i = 2; i < 5; i++) buf[i] = (char)toupper((unsigned char)buf[i]);
    return std::string(buf, 16);
}

// Write exactly `n` bytes from string `s` (blank-padded / truncated)
static void write_str(std::ostream& os, const std::string& s, size_t n) {
    std::string padded = pad_right(s, n);
    os.write(padded.c_str(), (std::streamsize)n);
}

// Write int16_t big-endian
static void write_i16(std::ostream& os, int16_t v) {
    uint8_t b[2] = { (uint8_t)(v >> 8), (uint8_t)(v & 0xFF) };
    os.write((char*)b, 2);
}

// Write int32_t big-endian
static void write_i32(std::ostream& os, int32_t v) {
    uint8_t b[4] = {
        (uint8_t)(v >> 24), (uint8_t)(v >> 16),
        (uint8_t)(v >> 8),  (uint8_t)(v & 0xFF)
    };
    os.write((char*)b, 4);
}

static int16_t read_i16(const uint8_t* p) {
    return (int16_t)(((uint16_t)p[0] << 8) | p[1]);
}
static int32_t read_i32(const uint8_t* p) {
    return (int32_t)(((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
                     ((uint32_t)p[2] << 8)  |  (uint32_t)p[3]);
}

// Pad stream to multiple of 80 bytes with spaces
static void pad_to_80(std::ostream& os) {
    std::streampos pos = os.tellp();
    int rem = (int)(pos % 80);
    if (rem != 0) {
        std::string spaces(80 - rem, ' ');
        os.write(spaces.c_str(), (std::streamsize)(80 - rem));
    }
}

// ============================================================
// NAMESTR structure (XPT v5 = 140 bytes each)
// ============================================================
struct Namestr {
    int16_t  ntype  = 1;      // 1=numeric, 2=char
    int16_t  nhfun  = 0;      // hash (unused, set 0)
    int16_t  nlng   = 8;      // variable length in obs record
    int16_t  nvar0  = 0;      // variable number (0-based)
    char     nname[8]   = {}; // name, blank-padded
    char     nlabel[40] = {}; // label, blank-padded
    char     nform[8]   = {}; // format name, blank-padded
    int16_t  nfl   = 0;       // format field length
    int16_t  nfd   = 0;       // format decimal
    int16_t  nfj   = 0;       // justification (0=left,1=right)
    char     nfill[2]  = {};  // padding
    char     niform[8] = {};  // informat name
    int16_t  nifl  = 0;       // informat length
    int16_t  nifd  = 0;       // informat decimal
    int32_t  npos  = 0;       // byte position in obs record
    char     longname[32] = {}; // v8 long name (bytes 53-84 of 140-byte struct)
    char     rest[24] = {};   // filler to 140 bytes total
};
// Total: 2+2+2+2+8+40+8+2+2+2+2+8+2+2+4+32+24 = 144 bytes
// Actual SAS namestr is 140 bytes; we'll serialise field-by-field

static void write_namestr(std::ostream& os, const Namestr& ns) {
    write_i16(os, ns.ntype);
    write_i16(os, ns.nhfun);
    write_i16(os, ns.nlng);
    write_i16(os, ns.nvar0);
    os.write(ns.nname,  8);
    os.write(ns.nlabel, 40);
    os.write(ns.nform,  8);
    write_i16(os, ns.nfl);
    write_i16(os, ns.nfd);
    write_i16(os, ns.nfj);
    os.write(ns.nfill,  2);
    os.write(ns.niform, 8);
    write_i16(os, ns.nifl);
    write_i16(os, ns.nifd);
    write_i32(os, ns.npos);
    os.write(ns.longname, 32); // v8 extension lives here
    os.write(ns.rest,     20); // pad to 140 bytes total (88+32+20=140)
}

static Namestr read_namestr(const uint8_t* p) {
    Namestr ns;
    ns.ntype  = read_i16(p);      p += 2;
    ns.nhfun  = read_i16(p);      p += 2;
    ns.nlng   = read_i16(p);      p += 2;
    ns.nvar0  = read_i16(p);      p += 2;
    std::memcpy(ns.nname,   p,  8); p += 8;
    std::memcpy(ns.nlabel,  p, 40); p += 40;
    std::memcpy(ns.nform,   p,  8); p += 8;
    ns.nfl    = read_i16(p);      p += 2;
    ns.nfd    = read_i16(p);      p += 2;
    ns.nfj    = read_i16(p);      p += 2;
    std::memcpy(ns.nfill,   p,  2); p += 2;
    std::memcpy(ns.niform,  p,  8); p += 8;
    ns.nifl   = read_i16(p);      p += 2;
    ns.nifd   = read_i16(p);      p += 2;
    ns.npos   = read_i32(p);      p += 4;
    std::memcpy(ns.longname, p, 32); p += 32;
    std::memcpy(ns.rest,     p, 20); // remaining to 140 bytes (88+32+20=140)
    return ns;
}

static std::string rtrim(const char* s, size_t n) {
    size_t end = n;
    while (end > 0 && (s[end - 1] == ' ' || s[end - 1] == '\0')) --end;
    return std::string(s, end);
}

// ============================================================
// WRITE XPT
// ============================================================

//' Write a data frame to SAS XPT format (v5 or v8)
//'
//' @param df          A data.frame to export.
//' @param path        Output file path (character).
//' @param version     XPT version: 5 (default) or 8.
//' @param dataset_name SAS dataset name (≤8 chars for v5, ≤32 for v8).
//' @param dataset_label Dataset label (≤40 chars).
//' @param var_labels  Named character vector of variable labels.
//' @param var_formats Named character vector of SAS format strings (e.g. "DATE9.").
//' @param var_lengths Named integer vector of variable byte lengths.
//' @return Invisibly returns `path`.
//' @keywords internal
//' @export
// [[Rcpp::export]]
SEXP cx_write_xpt(DataFrame df,
                  std::string path,
                  int version = 5,
                  std::string dataset_name  = "DATASET",
                  std::string dataset_label = "",
                  Nullable<CharacterVector> var_labels  = R_NilValue,
                  Nullable<CharacterVector> var_formats = R_NilValue,
                  Nullable<IntegerVector>   var_lengths = R_NilValue)
{
    int ncol = df.size();
    int nrow = df.nrows();
    CharacterVector colnames = df.names();

    // ── Build label / format / length maps ──────────────────
    std::vector<std::string> labels(ncol, "");
    std::vector<std::string> formats(ncol, "");
    std::vector<int>         lengths(ncol, 8);

    CharacterVector dfnames = df.names();

    auto fill_map = [&](Nullable<CharacterVector> nv,
                        std::vector<std::string>& vec) {
        if (nv.isNotNull()) {
            CharacterVector cv(nv);
            CharacterVector cvnames = cv.names();
            for (int i = 0; i < ncol; i++) {
                std::string cn = as<std::string>(colnames[i]);
                for (int j = 0; j < cvnames.size(); j++) {
                    if (as<std::string>(cvnames[j]) == cn) {
                        vec[i] = as<std::string>(cv[j]);
                        break;
                    }
                }
            }
        }
    };

    fill_map(var_labels,  labels);
    fill_map(var_formats, formats);

    if (var_lengths.isNotNull()) {
        IntegerVector lv(var_lengths);
        CharacterVector lvnames = lv.names();
        for (int i = 0; i < ncol; i++) {
            std::string cn = as<std::string>(colnames[i]);
            for (int j = 0; j < lvnames.size(); j++) {
                if (as<std::string>(lvnames[j]) == cn) {
                    lengths[i] = lv[j];
                    break;
                }
            }
        }
    }

    // Determine column types and lengths
    std::vector<int> col_type(ncol);   // 1=numeric, 2=char
    std::vector<int> col_len(ncol);
    int obs_len = 0;
    std::vector<int> col_pos(ncol);

    for (int i = 0; i < ncol; i++) {
        SEXP col = df[i];
        int rtype = TYPEOF(col);
        bool is_char = (rtype == STRSXP);

        // Factor → treated as character
        if (Rf_isFactor(col)) is_char = true;

        col_type[i] = is_char ? 2 : 1;

        if (is_char) {
            // Determine max nchar
            // lengths[i] == 0 means "not specified" → auto-detect from data
            int maxw = lengths[i] > 0 ? lengths[i] : 1;
            if (lengths[i] <= 0) {
                // Auto-detect max width from actual data values
                CharacterVector cv = Rf_isFactor(col)
                    ? as<CharacterVector>(Rcpp::as<CharacterVector>(col))
                    : as<CharacterVector>(col);
                for (int r = 0; r < nrow; r++) {
                    if (cv[r] != NA_STRING) {
                        const char* s = CHAR(STRING_ELT(cv, r));
                        int sw = (int)strlen(s);
                        if (sw > maxw) maxw = sw;
                    }
                }
                // Cap to 200 for v5 or 32767 for v8
                maxw = std::max(1, std::min(maxw, version == 5 ? 200 : 32767));
            }
            col_len[i] = maxw;
        } else {
            col_len[i] = 8; // all numerics are 8 bytes in XPT
        }
        col_pos[i] = obs_len;
        obs_len += col_len[i];
    }

    // ── Sanitise column names ────────────────────────────────
    int name_max = (version == 5) ? 8 : 32;
    std::vector<std::string> names(ncol);
    for (int i = 0; i < ncol; i++) {
        std::string nm = as<std::string>(colnames[i]);
        // Uppercase
        std::transform(nm.begin(), nm.end(), nm.begin(), ::toupper);
        // Remove non-alphanumeric/_
        std::string clean;
        for (char c : nm) {
            if (std::isalnum((unsigned char)c) || c == '_') clean += c;
        }
        if (clean.empty()) clean = "V" + std::to_string(i + 1);
        if (!std::isalpha((unsigned char)clean[0]) && clean[0] != '_')
            clean = "V" + clean;
        if ((int)clean.size() > name_max) clean = clean.substr(0, name_max);
        names[i] = clean;
    }

    // ── Open file ────────────────────────────────────────────
    std::ofstream ofs(path, std::ios::binary);
    if (!ofs.is_open())
        stop("Cannot open file for writing: " + path);

    std::string dt = xpt_date();

    // ── Library header (6 × 80-byte records) ────────────────
    // Record 1
    write_str(ofs, "HEADER RECORD*******LIBRARY HEADER RECORD!!!!!!!000000000000000000000000000000  ", 80);
    // Record 2: libname(8) + blank_label(40) + "SAS     "(8) + os(8) + created(16) = 80 bytes
    write_str(ofs, "LIBRARY ", 8);
    write_str(ofs, "",         40);
    write_str(ofs, "SAS     ", 8);
    write_str(ofs, "SAS     ", 8); // SAS version field (matches TS-140 spec)
    write_str(ofs, dt,         16);
    // Record 3: Member header
    if (version == 8) {
        write_str(ofs, "HEADER RECORD*******MEMBER  HEADER RECORD!!!!!!!000000000000000001600000000320", 80);
    } else {
        write_str(ofs, "HEADER RECORD*******MEMBER  HEADER RECORD!!!!!!!000000000000000001600000000140", 80);
    }
    // Record 4: Descriptor header
    write_str(ofs, "HEADER RECORD*******DSCRPTR HEADER RECORD!!!!!!!000000000000000000000000000000  ", 80);
    // Record 5: Dataset info line 1
    {
        std::string dsname = pad_right(dataset_name, 8);
        if (version == 8 && dataset_name.size() > 8) dsname = pad_right("", 8);
        write_str(ofs, dsname,        8);
        write_str(ofs, dataset_label, 40);
        write_str(ofs, "SASDATA ", 8);
        write_str(ofs, "9.4     ", 8);  // SAS version
        write_str(ofs, "W32_VSPRO", 8); // OS
        write_str(ofs, "        ", 8);  // pad
    }
    // Record 6: dates
    write_str(ofs, dt, 16);
    write_str(ofs, dt, 16);
    write_str(ofs, std::string(48, ' '), 48);

    // ── NAMESTR header ───────────────────────────────────────
    {
        std::ostringstream ns_hdr;
        ns_hdr << "HEADER RECORD*******NAMESTR HEADER RECORD!!!!!!!"
               << std::setw(12) << std::setfill('0') << ncol
               << "00000000000000000000";
        std::string s = ns_hdr.str();
        s = pad_right(s, 80);
        ofs.write(s.c_str(), 80);
    }

    // ── NAMESTR records (140 bytes each, packed, pad to 80) ──
    for (int i = 0; i < ncol; i++) {
        Namestr ns;
        ns.ntype  = (int16_t)col_type[i];
        ns.nlng   = (int16_t)col_len[i];
        ns.nvar0  = (int16_t)i;
        ns.npos   = (int32_t)col_pos[i];
        // short name (≤8, always)
        std::string sname = (names[i].size() > 8) ? names[i].substr(0,8) : names[i];
        std::memcpy(ns.nname, pad_right(sname, 8).c_str(), 8);
        // label
        std::string lbl = pad_right(labels[i], 40);
        std::memcpy(ns.nlabel, lbl.c_str(), 40);
        // format
        std::string fmt = formats[i];
        // Parse e.g. "DATE9." → name="DATE", fl=9, fd=0
        // Simple parse: strip trailing '.', find digits at end
        if (!fmt.empty()) {
            std::string fname; int fl = 0, fd = 0;
            // Remove trailing '.'
            if (fmt.back() == '.') fmt.pop_back();
            // Find decimal
            auto dotpos = fmt.rfind('.');
            if (dotpos != std::string::npos) {
                fd = std::stoi(fmt.substr(dotpos + 1));
                fmt = fmt.substr(0, dotpos);
            }
            // Find digits at end for field length
            size_t pos = fmt.size();
            while (pos > 0 && std::isdigit((unsigned char)fmt[pos-1])) --pos;
            if (pos < fmt.size()) {
                fl = std::stoi(fmt.substr(pos));
                fname = fmt.substr(0, pos);
            } else {
                fname = fmt;
            }
            std::memcpy(ns.nform, pad_right(fname, 8).c_str(), 8);
            ns.nfl = (int16_t)fl;
            ns.nfd = (int16_t)fd;
        }
        ns.nfj = (col_type[i] == 2) ? 0 : 1; // left for char, right for num

        // Long name for v8
        if (version == 8) {
            std::memcpy(ns.longname, pad_right(names[i], 32).c_str(), 32);
        }

        write_namestr(ofs, ns);
    }
    pad_to_80(ofs);

    // ── OBS header ───────────────────────────────────────────
    write_str(ofs, "HEADER RECORD*******OBS     HEADER RECORD!!!!!!!000000000000000000000000000000  ", 80);

    // ── Observation records ──────────────────────────────────
    std::vector<uint8_t> obs_buf(obs_len, ' ');

    for (int r = 0; r < nrow; r++) {
        std::fill(obs_buf.begin(), obs_buf.end(), ' ');

        for (int i = 0; i < ncol; i++) {
            SEXP col = df[i];
            uint8_t* dest = obs_buf.data() + col_pos[i];

            if (col_type[i] == 1) {
                // Numeric
                double val = NA_REAL;
                int rtype = TYPEOF(col);
                if (rtype == REALSXP) {
                    val = REAL(col)[r];
                } else if (rtype == INTSXP) {
                    int iv = INTEGER(col)[r];
                    val = (iv == NA_INTEGER) ? NA_REAL : (double)iv;
                } else if (rtype == LGLSXP) {
                    int iv = LOGICAL(col)[r];
                    val = (iv == NA_LOGICAL) ? NA_REAL : (double)iv;
                }
                if (ISNAN(val)) val = std::numeric_limits<double>::quiet_NaN();
                ieee_to_ibm(val, dest);
            } else {
                // Character / factor
                const char* s = "";
                if (Rf_isFactor(col)) {
                    IntegerVector fv(col);
                    CharacterVector lvls = fv.attr("levels");
                    int fi = fv[r];
                    if (fi != NA_INTEGER) s = CHAR(STRING_ELT(lvls, fi - 1));
                } else {
                    SEXP el = STRING_ELT(col, r);
                    if (el != NA_STRING) s = CHAR(el);
                }
                int slen = (int)strlen(s);
                int maxw = col_len[i];
                int copy_len = std::min(slen, maxw);
                std::memcpy(dest, s, copy_len);
                // Blank-pad the rest (already done by fill)
            }
        }
        ofs.write((char*)obs_buf.data(), obs_len);
    }
    pad_to_80(ofs);
    ofs.close();

    return wrap(path);
}

// ============================================================
// READ XPT
// ============================================================

//' Read a SAS XPT file (v5 or v8) into a data frame
//'
//' @param path Path to the .xpt file.
//' @return A data.frame with attributes: `var_labels`, `var_formats`, `var_lengths`, `dataset_name`, `dataset_label`.
//' @keywords internal
//' @export
// [[Rcpp::export]]
SEXP cx_read_xpt(std::string path) {
    std::ifstream ifs(path, std::ios::binary);
    if (!ifs.is_open()) stop("Cannot open file: " + path);

    // Read entire file into buffer
    ifs.seekg(0, std::ios::end);
    size_t fsize = (size_t)ifs.tellg();
    ifs.seekg(0, std::ios::beg);
    std::vector<uint8_t> buf(fsize);
    ifs.read((char*)buf.data(), (std::streamsize)fsize);
    ifs.close();

    if (fsize < 480) stop("File too small to be a valid XPT file: " + path);

    size_t pos = 0;

    // ── Validate library header ─────────────────────────────
    std::string rec1(buf.begin(), buf.begin() + 80);
    if (rec1.find("LIBRARY HEADER RECORD") == std::string::npos)
        stop("Not a valid SAS XPT file (missing LIBRARY HEADER RECORD): " + path);
    pos = 160; // skip lib name record

    // ── Skip to MEMBER / DSCRPTR headers ────────────────────
    // Find DSCRPTR to get dataset info
    std::string dataset_name = "DATASET";
    std::string dataset_label = "";

    // Scan for member and dscrptr headers
    while (pos + 80 <= fsize) {
        std::string rec((char*)buf.data() + pos, 80);
        if (rec.find("DSCRPTR HEADER RECORD") != std::string::npos) {
            pos += 80;
            // Next 80 bytes = dataset info
            if (pos + 80 <= fsize) {
                dataset_name  = rtrim((char*)buf.data() + pos, 8);
                dataset_label = rtrim((char*)buf.data() + pos + 8, 40);
                pos += 160; // skip dataset info (2 × 80)
            }
            break;
        }
        pos += 80;
    }

    // ── Read NAMESTR header ──────────────────────────────────
    int ncol = 0;
    int namestr_len = 140; // v5 default
    while (pos + 80 <= fsize) {
        std::string rec((char*)buf.data() + pos, 80);
        if (rec.find("NAMESTR HEADER RECORD") != std::string::npos) {
            // The prefix "HEADER RECORD*******NAMESTR HEADER RECORD!!!!!!!" is
            // exactly 48 bytes; the variable count follows as 12 zero-padded digits.
            std::string count_str((char*)buf.data() + pos + 48, 12);
            try { ncol = std::stoi(count_str); } catch (...) {}
            // Detect v8 vs v5 from member header (already passed; default v5)
            pos += 80;
            break;
        }
        if (rec.find("MEMBER  HEADER RECORD") != std::string::npos) {
            // Check if v8 (namestr size = 320 pattern in header)
            std::string mhdr((char*)buf.data() + pos, 80);
            if (mhdr.find("320") != std::string::npos) namestr_len = 140; // v8 still 140
        }
        pos += 80;
    }

    if (ncol <= 0) stop("Could not determine number of variables from XPT file.");

    // ── Read NAMESTR records ─────────────────────────────────
    // Each namestr is 140 bytes, packed sequentially
    std::vector<Namestr> nsvec(ncol);
    for (int i = 0; i < ncol; i++) {
        if (pos + 140 > fsize) stop("Unexpected end of file reading NAMESTR.");
        nsvec[i] = read_namestr(buf.data() + pos);
        pos += 140;
    }
    // Align to 80-byte boundary
    size_t rem = pos % 80;
    if (rem != 0) pos += (80 - rem);

    // ── Skip OBS header ──────────────────────────────────────
    while (pos + 80 <= fsize) {
        std::string rec((char*)buf.data() + pos, 80);
        if (rec.find("OBS     HEADER RECORD") != std::string::npos) {
            pos += 80;
            break;
        }
        pos += 80;
    }

    // ── Compute obs record length ────────────────────────────
    int obs_len = 0;
    for (int i = 0; i < ncol; i++) {
        obs_len += nsvec[i].nlng;
    }
    if (obs_len <= 0) stop("Invalid observation record length.");

    // ── Count rows ───────────────────────────────────────────
    size_t data_bytes = fsize - pos;
    // Data is padded to 80-byte boundary with spaces; compute max possible rows
    int nrow = (int)(data_bytes / obs_len);
    // Trim trailing rows that are all-spaces (XPT 80-byte boundary padding).
    // A real observation cannot be entirely 0x20 bytes: numeric fields use IBM
    // float (0x00 for zero, 0x2E for missing) and char fields may be spaces
    // but a row where EVERY byte is 0x20 is padding, not data.
    while (nrow > 0) {
        const uint8_t* candidate = buf.data() + pos + (size_t)(nrow - 1) * obs_len;
        bool all_space = true;
        for (int b = 0; b < obs_len && all_space; ++b)
            if (candidate[b] != 0x20) all_space = false;
        if (all_space) --nrow; else break;
    }

    // ── Allocate columns ─────────────────────────────────────
    List result(ncol);
    std::vector<std::string> col_names(ncol);
    CharacterVector out_labels(ncol);
    CharacterVector out_formats(ncol);
    IntegerVector   out_lengths(ncol);

    std::vector<std::vector<double>>      num_cols;
    std::vector<std::vector<std::string>> chr_cols;
    std::vector<int> col_idx(ncol, -1); // index into num_cols / chr_cols

    int num_count = 0, chr_count = 0;
    for (int i = 0; i < ncol; i++) {
        // Resolve name: prefer longname if set (v8)
        std::string lname = rtrim(nsvec[i].longname, 32);
        std::string sname = rtrim(nsvec[i].nname, 8);
        col_names[i] = lname.empty() ? sname : lname;
        out_labels[i]  = rtrim(nsvec[i].nlabel, 40);
        // Reconstruct full SAS format string from nform + nfl + nfd fields.
        // Examples: ("DATE", 9, 0) -> "DATE9."  ("", 5, 1) -> "5.1"  ("", 3, 0) -> "3."
        {
            std::string nfm = rtrim(nsvec[i].nform, 8);
            int fl = nsvec[i].nfl, fd = nsvec[i].nfd;
            if (!nfm.empty() || fl > 0) {
                std::string fmt = nfm;
                if (fl > 0) fmt += std::to_string(fl);
                if (fd > 0) fmt += "." + std::to_string(fd);
                else        fmt += ".";   // trailing period: "DATE9." or "3."
                out_formats[i] = fmt;
            } else {
                out_formats[i] = "";
            }
        }
        out_lengths[i] = nsvec[i].nlng;

        if (nsvec[i].ntype == 2) {
            chr_cols.emplace_back(nrow);
            col_idx[i] = chr_count++;
        } else {
            num_cols.emplace_back(nrow);
            col_idx[i] = num_count++;
        }
    }

    // ── Parse observations ────────────────────────────────────
    for (int r = 0; r < nrow; r++) {
        const uint8_t* obs = buf.data() + pos + r * obs_len;
        for (int i = 0; i < ncol; i++) {
            const uint8_t* field = obs + nsvec[i].npos;
            if (nsvec[i].ntype == 2) {
                // Character
                int len = nsvec[i].nlng;
                // rtrim
                int end = len;
                while (end > 0 && (field[end-1] == ' ' || field[end-1] == '\0')) --end;
                chr_cols[col_idx[i]][r] = std::string((char*)field, end);
            } else {
                // Numeric (IBM float)
                num_cols[col_idx[i]][r] = ibm_to_ieee(field);
            }
        }
    }

    // ── Build R list ─────────────────────────────────────────
    for (int i = 0; i < ncol; i++) {
        std::string lbl = as<std::string>(out_labels[i]);
        std::string fmt = as<std::string>(out_formats[i]);
        if (nsvec[i].ntype == 2) {
            CharacterVector cv(nrow);
            for (int r = 0; r < nrow; r++) cv[r] = chr_cols[col_idx[i]][r];
            // Attach per-column attributes (mirrors set_var_label / set_var_format)
            if (!lbl.empty()) cv.attr("label")  = lbl;
            if (!fmt.empty()) cv.attr("format") = fmt;
            result[i] = cv;
        } else {
            NumericVector nv(nrow);
            for (int r = 0; r < nrow; r++) {
                double v = num_cols[col_idx[i]][r];
                nv[r] = std::isnan(v) ? NA_REAL : v;
            }
            // Attach per-column attributes
            if (!lbl.empty()) nv.attr("label")  = lbl;
            if (!fmt.empty()) nv.attr("format") = fmt;
            result[i] = nv;
        }
    }

    result.names() = col_names;

    // Set data.frame class
    result.attr("class") = "data.frame";
    IntegerVector rn(nrow);
    for (int i = 0; i < nrow; i++) rn[i] = i + 1;
    result.attr("row.names") = rn;

    // Attach metadata as attributes
    out_labels.names()  = col_names;
    out_formats.names() = col_names;
    out_lengths.names() = col_names;
    result.attr("var_labels")    = out_labels;
    result.attr("var_formats")   = out_formats;
    result.attr("var_lengths")   = out_lengths;
    result.attr("dataset_name")  = dataset_name;
    result.attr("dataset_label") = dataset_label;

    return result;
}
