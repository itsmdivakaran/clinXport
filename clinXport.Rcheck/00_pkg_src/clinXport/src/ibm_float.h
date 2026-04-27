#pragma once
// IBM System/360 Double-Precision <-> IEEE 754 Double Conversion
// SAS XPT stores all numerics in IBM 360 floating point format (big-endian).
// Reference: SAS XPORT specification, IBM z/Architecture Principles of Operation.

#include <cstdint>
#include <cstring>
#include <cmath>
#include <limits>

namespace clinxport {

// ── IBM → IEEE ──────────────────────────────────────────────────────────────
// IBM double: 1 sign bit | 7-bit exponent (excess-64, base-16) | 56-bit mantissa
// The mantissa is NOT normalised to 1.x; it is 0.hex_fraction.
inline double ibm_to_ieee(const uint8_t* ibm) {
    // SAS missing value: all bytes 0 except first byte being 0x2E ('.')
    // or one of '_A'-'_Z' patterns. We map all to NA.
    if (ibm[0] >= 0x2E && ibm[0] <= 0x5F && ibm[1] == 0 && ibm[2] == 0 &&
        ibm[3] == 0 && ibm[4] == 0 && ibm[5] == 0 && ibm[6] == 0 && ibm[7] == 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }

    // All-zero → 0.0
    uint64_t raw = 0;
    std::memcpy(&raw, ibm, 8);
    if (raw == 0) return 0.0;

    int sign    = (ibm[0] & 0x80) ? -1 : 1;
    int exp16   = (ibm[0] & 0x7F) - 64;          // true exponent (base 16)

    // Build mantissa from 7 bytes (big-endian)
    uint64_t mant = 0;
    for (int i = 1; i < 8; i++) {
        mant = (mant << 8) | ibm[i];
    }
    // mant is a 56-bit integer representing 0.mant * 16^exp16
    // Value = sign * mant * 16^(exp16) / 2^56

    // Convert: value = sign * mant * (2^4)^exp16 / 2^56
    //                = sign * mant * 2^(4*exp16 - 56)
    if (mant == 0) return 0.0;

    double result = (double)mant * std::ldexp(1.0, 4 * exp16 - 56);
    return sign * result;
}

// ── IEEE → IBM ──────────────────────────────────────────────────────────────
inline void ieee_to_ibm(double val, uint8_t* ibm) {
    if (std::isnan(val)) {
        // SAS special missing '.' = 0x2E followed by 7 zero bytes
        ibm[0] = 0x2E;
        for (int i = 1; i < 8; i++) ibm[i] = 0;
        return;
    }
    if (val == 0.0) {
        for (int i = 0; i < 8; i++) ibm[i] = 0;
        return;
    }

    uint8_t sign_bit = 0;
    if (val < 0.0) { sign_bit = 0x80; val = -val; }

    // Express val as mant * 16^exp16 where 1/16 <= mant < 1
    // Use frexp to get binary exponent: val = frac * 2^bexp, 0.5 <= frac < 1
    int bexp = 0;
    double frac = std::frexp(val, &bexp);   // val = frac * 2^bexp

    // Convert binary to hex exponent:
    // val = frac * 2^bexp = frac * 16^(bexp/4)
    // We need bexp divisible by 4 for exact hex exponent.
    // Round bexp up to next multiple of 4:
    int adj = ((bexp % 4) + 4) % 4;    // extra bits to shift frac right
    frac *= std::ldexp(1.0, adj);      // frac now in [0, 16) territory
    bexp -= adj;                        // bexp is now divisible by 4
    int exp16 = bexp / 4 + 64;         // IBM excess-64 exponent

    // Handle exponent overflow / underflow
    if (exp16 > 127) {
        // Overflow → store max representable
        ibm[0] = sign_bit | 0x7F;
        for (int i = 1; i < 8; i++) ibm[i] = 0xFF;
        return;
    }
    if (exp16 < 0) {
        for (int i = 0; i < 8; i++) ibm[i] = 0;
        return;
    }

    // Extract 56-bit mantissa: mant = frac * 2^56 / 16
    // Because frac is in [1/16, 1) after normalisation
    // Actually after adj: frac is in [0.5/16 * 16^adj, ...) — re-normalise to [1/16, 1)
    // Ensure 1/16 <= frac < 1
    while (frac >= 1.0) { frac /= 16.0; exp16++; }
    while (frac < 1.0 / 16.0 && frac > 0.0) { frac *= 16.0; exp16--; }

    ibm[0] = sign_bit | (uint8_t)(exp16 & 0x7F);

    // Extract 7 mantissa bytes
    for (int i = 1; i < 8; i++) {
        frac *= 256.0;
        ibm[i] = (uint8_t)frac;
        frac -= ibm[i];
    }
}

} // namespace clinxport
