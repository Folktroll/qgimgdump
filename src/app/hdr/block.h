#pragma once

#include <QtGlobal>
#include <array>

#include "global.h"

namespace ImgHdr {

#pragma pack(push, 1)

// common header for all subtypes
struct SComm {
  quint16 size;                 // 0000_0001
  std::array<quint8, 10> type;  // 0002_000B
  quint8 x000C;                 // 000C
  quint8 flag;                  // 000D
  qint16 year;                  // 000E_000F
  qint8 month;                  // 0010
  qint8 day;                    // 0011
  qint8 hour;                   // 0012
  qint8 minute;                 // 0013
  qint8 second;                 // 0014
  void printCommon() const;
};

struct SGmp : public SComm {
  std::array<quint8, 4> x0015_0018;  // 0015_0018
  quint32 tre_offset;                // 0019_001C
  quint32 rgn_offset;                // 001D_0020
  quint32 lbl_offset;                // 0021_0024
  quint32 net_offset;                // 0025_0028
  quint32 nod_offset;                // 0029_002E
  quint32 dem_offset;                // 002D_0030
  quint32 mar_offset;                // 0031_0034
  quint32 met_offset;                // 0035_0038
  void print() const;
};

struct STre : public SComm {
  quint24 north_bound = {0};               // 0015_0017 - max lat
  quint24 east_bound = {0};                // 0018_001A - max long
  quint24 south_bound = {0};               // 001B_001D - min lat
  quint24 west_bound = {0};                // 001E_0020 - min long, cant be +180
  quint32 tre1_offset = 0;                 // 0021_0024 - map levels pos
  quint32 tre1_size = 0;                   // 0025_0028
  quint32 tre2_offset = 0;                 // 0029_002C - subdiv pos
  quint32 tre2_size = 0;                   // 002D_0030
  quint32 tre3_offset = 0;                 // 0031_0034 - TRE-body relative offset (new format) or copyright offset in TRE
  quint32 tre3_size = 0;                   // 0035_0038
  quint16 tre3_rec_size = 0;               // 0039_003A
  std::array<quint8, 4> x003B_003E = {0};  // 003B_003E
  quint8 POI_flags = 0;                    // 003F      - poi display flags
  quint24 render_prio = {0x14, 0, 0};      // 0040_0042 - display priority
  std::array<quint8, 7> x0043_0049 = {0};  // 0043_0049
  quint32 tre4_offset = 0;                 // 004A_004D - polyline overview
  quint32 tre4_size = 0;                   // 004E_0051
  quint16 tre4_rec_size = 0;               // 0052_0053
  std::array<quint8, 4> x0054_0057 = {0};  // 0054_0057
  quint32 tre5_offset = 0;                 // 0058_005B - polygon overview
  quint32 tre5_size = 0;                   // 005C_005F
  quint16 tre5_rec_size = 0;               // 0060_0061
  std::array<quint8, 4> x0062_0065 = {0};  // 0062_0065
  quint32 tre6_offset = 0;                 // 0066_0069 - points overview
  quint32 tre6_size = 0;                   // 006A_006D
  quint16 tre6_rec_size = 0;               // 006E_006F
  std::array<quint8, 4> x0070_0073 = {0};  // 0070_0073
  quint32 map_id = 0;                      // 0074_0077
  std::array<quint8, 4> x0078_007B = {0};  // 0078_007B
  quint32 tre7_offset = 0;                 // 007C_007F - extended type offsets
  quint32 tre7_size = 0;                   // 0080_0083
  quint16 tre7_rec_size = 0;               // 0084_0085
  std::array<quint8, 4> x0086_0089 = {0};  // 0086_0089 - 0x01 0x00 0x00 0x00
  quint32 tre8_offset = 0;                 // 008A_008D - extended type overview: ln, pg, po; sorted by type (1 type 1 levels 1 subtype)
  quint32 tre8_size = 0;                   // 008E_0091
  quint16 tre8_rec_size = 0;               // 0092_0093
  quint16 lnex_types_num = 0;              // 0094_0095 - num ext type ln
  quint16 pgex_types_num = 0;              // 0096_0097 - num ext type pg
  quint16 poex_types_num = 0;              // 0098_0099 - num ext type pt
  std::array<quint8, 16> key = {0};        // 009A_00A5 - map values
  std::array<quint8, 4> x00AA_00AD = {0};  // 00AA_00AD
  quint32 tre9_offset;                     // 00AE_00B1
  quint32 tre9_size;                       // 00B2_00B5
  quint16 tre9_rec_size;                   // 00B6_00B7
  std::array<quint8, 4> x00B8_00BB = {0};  // 00B8_00BB
  quint32 tre10_offset;                    // 00BC_00BF
  quint32 tre10_size;                      // 00C0_00C3
  quint16 tre10_rec_size;                  // 00C4_00C5
  std::array<quint8, 9> x00C6_00CE = {0};  // 00C6_00CE
  quint32 map_id2;                         // 00CF_00D2 - map id 2
  void print(quint32 offset) const;
};

struct SRgn : public SComm {
  quint32 rgn1_offset = 0;                  // 0015_0018 - RGN-body relative offset (new format)
  quint32 rgn1_length = 0;                  // 0019_001C
  quint32 pgex_offset = 0;                  // 001D_0020
  quint32 pgex_length = 0;                  // 0021_0024
  std::array<quint8, 20> x0025_0038 = {0};  // 0025_0038
  quint32 lnex_offset = 0;                  // 0039_003C
  quint32 lnex_length = 0;                  // 003D_0040
  std::array<quint8, 20> x0041_0054 = {0};  // 0041_0054
  quint32 ptex_offset = 0;                  // 0055_0058
  quint32 ptex_length = 0;                  // 0059_005C
  std::array<quint8, 20> x005D_0070 = {0};  // 005D_0070
  quint32 rgn2_offset = 0;                  // 0071_0074
  quint32 rgn2_length = 0;                  // 0075_0078
  quint32 unknown = 0;                      // 0079_007C - E3 or E7
  void print(quint32 offset) const;
};

struct SLbl : public SComm {
  quint32 lbl1_offset = 0;                 // 0015_0018 - sort description length
  quint32 lbl1_length = 0;                 // 0019_001C - label size
  quint8 addr_shift = 0;                   // 001D      - offset multiplier
  quint8 coding = 0;                       // 001E      - encoding type
  quint32 lbl2_offset = 0;                 // 001F_0022
  quint32 lbl2_length = 0;                 // 0023_0026
  quint16 lbl2_rec_size = 0;               // 0027_0028
  std::array<quint8, 4> x0029_002C = {0};  // 0029_002C
  quint32 lbl3_offset = 0;                 // 002D_0030
  quint32 lbl3_length = 0;                 // 0031_0034
  quint16 lbl3_rec_size = 0;               // 0035_0036
  std::array<quint8, 4> x0037_003A = {0};  // 0037_003A
  quint32 lbl4_offset = 0;                 // 003B_003E
  quint32 lbl4_length = 0;                 // 003F_0042
  quint16 lbl4_rec_size = 0;               // 0043_0044
  std::array<quint8, 4> x0045_0048 = {0};  // 0045_0048
  quint32 lbl5_offset = 0;                 // 0049_004C
  quint32 lbl5_length = 0;                 // 004D_0050
  quint16 lbl5_rec_size = 0;               // 0051_0052
  std::array<quint8, 4> x0053_0056 = {0};  // 0053_0056
  quint32 lbl6_offset = 0;                 // 0057_005A
  quint32 lbl6_length = 0;                 // 005B_005E
  quint8 lbl6_addr_shift = 0;              // 005F
  quint8 lbl6_glob_mask = 0;               // 0060
  std::array<quint8, 3> x0061_0063 = {0};  // 0061_0063
  quint32 lbl7_offset = 0;                 // 0064_0067
  quint32 lbl7_length = 0;                 // 0068_006B
  quint16 lbl7_rec_size = 0;               // 006C_006D
  std::array<quint8, 4> x006E_0071 = {0};  // 006E_0071
  quint32 lbl8_offset = 0;                 // 0072_0075
  quint32 lbl8_length = 0;                 // 0076_0079
  quint16 lbl8_rec_size = 0;               // 007A_007B
  std::array<quint8, 4> x007C_007F = {0};  // 007C_007F
  quint32 lbl9_offset = 0;                 // 0080_0083
  quint32 lbl9_length = 0;                 // 0084_0087
  quint16 lbl9_rec_size = 0;               // 0088_0089
  std::array<quint8, 4> x008A_008D = {0};  // 008A_008D
  quint32 lbl10_offset = 0;                // 008E_0091
  quint32 lbl10_length = 0;                // 0092_0095
  quint16 lbl10_rec_size = 0;              // 0096_0097
  std::array<quint8, 4> x0098_009B = {0};  // 0098_009B
  quint32 lbl11_offset = 0;                // 009C_009F
  quint32 lbl11_length = 0;                // 00A0_00A3
  quint16 lbl11_rec_size = 0;              // 00A4_00A5
  std::array<quint8, 4> x00A6_00A9 = {0};  // 00A6_00A9
  quint16 codepage = 0;                    // 00AA_00AB - optional check length
  std::array<quint8, 4> x00AC_00AF = {0};  // 00AC_00AF - 0x07 0x00 0x02 0x80 or 0x12 0x00 0x01 0x80
  quint32 lbl12_offset = 0;                // 00B0_00B3 - LBL-body relative offset (new format) or sort descriptor
  quint32 lbl12_length = 0;                // 00B4_00B7
  void print(quint32 offset) const;
};

struct SNet : public SComm {
  quint32 net1_offset;     // 0015_0018 - NET-body relative offset (new format)
  quint32 net1_length;     // 0019_001C
  quint8 net1_addr_shift;  // 001D
  quint32 net2_offset;     // 001E_0021
  quint32 net2_length;     // 0022_0025
  quint8 net2_addr_shift;  // 0026
  quint32 net3_offset;     // 0027_002A
  quint32 net3_length;     // 002B_002E
  void print(quint32 offset) const;
};

struct SNod : public SComm {
  quint32 nod1_offset;  // 0015_0018 - NOD-body relative offset (new format)
  quint32 not1_length;  // 0019_001C
  quint8 not1_flags;    // 001D
  void print(quint32 offset) const;
};

struct SDem : public SComm {
  quint32 flags;                     // 0015_0018 - 0=meter, 1=feet
  quint16 zoom_levels;               // 0019_001A
  std::array<quint8, 4> x001B_001D;  // 001B_001D
  quint16 rec_size;                  // 001F_0020
  quint32 dem1_offset;               // 0021_0024
  std::array<quint8, 4> x0025_0029;  // 0025_0029
  void print(quint32 offset) const;
};

#pragma pack(pop)

};  // namespace ImgHdr