#pragma once

#include <QtMath>
#include <QtTypes>
#include <array>

#define SANITY_CHECK
#define DEBUG_SHOW_MAPLEVELS
// #define DEBUG_SHOW_POLY_DATA_SUBDIV
// #define DEBUG_SHOW_POLY_DATA_DECODE
// #define DEBUG_SHOW_POLY_DATA_DECODE_EXT

constexpr int D180 = 180;

#define MAX_FLOAT_PREC qPow(2.0, 24.0)
#define GRMN_UNIT (2 * D180) / MAX_FLOAT_PREC
#define GRMN_DEG(x) ((x) < 0x800000 ? (double)(x) * GRMN_UNIT : (double)((x) - 0x1000000) * GRMN_UNIT)
#define GRMN_RAD(x) ((x) < 0x800000 ? (double)(x) * (2 * M_PI) / MAX_FLOAT_PREC : (double)((x) - 0x1000000) * (2 * M_PI) / MAX_FLOAT_PREC)

#define gar_load(t, x) (t)(x)
#define gar_ptr_load(t, p) __gar_ptr_load_##t((const uint8_t *)(p))
#define __gar_ptr_load_int16_t(p) (*((int16_t *)(p)))
#define __gar_ptr_load_uint16_t(p) (*((uint16_t *)(p)))
#define __gar_ptr_load_int24_t(p) (__gar_ptr_load_int32_t(p) & 0x00FFFFFFu)
#define __gar_ptr_load_uint24_t(p) (__gar_ptr_load_uint32_t(p) & 0x00FFFFFFu)
#define __gar_ptr_load_uint32_t(p) (*((uint32_t *)(p)))
#define __gar_ptr_load_int32_t(p) (*((int32_t *)(p)))

#define PRINT_HDR(field) printData(#field, field)

using quint24 = std::array<quint8, 3>;