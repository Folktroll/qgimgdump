#pragma once

#include <QRectF>
#include <QtTypes>

#include "misc.h"

#pragma pack(push, 1)
// IMG file header structure, to the start of the FAT blocks

namespace IMG {
struct gmapsupp_imghdr_t {
  quint8 xorByte = 0;              // 0000
  quint8 x0001_0007[7] = {0};      // 0001_0007
  quint16 version = 0;             // 0008
  quint8 upMonth = 0;              // 000A
  quint8 upYear = 0;               // 000B
  quint8 x000C_000D[2] = {0};      // 000C_000D
  quint8 supp = 0;                 // 000E
  quint8 checksum = 0;             // 000F
  char signature[7] = {0};         // 0010_0016
  quint8 x0017 = 0x2;              // 0017
  quint16 sectors1 = 0;            // 0018_0019
  quint16 heads1 = 0;              // 001A_001B
  quint16 cylinders = 0;           // 001C_001D
  quint8 x001E_0038[27] = {0};     // 001E_0038
  qint16 year = 0;                 // 0039_003A
  qint8 month = 0;                 // 003B
  qint8 day = 0;                   // 003C
  qint8 hour = 0;                  // 003D
  qint8 minute = 0;                // 003E
  qint8 second = 0;                // 003F
  qint8 offsetFAT = 0;             // 0040
  char identifier[7];              // 0041_0047
  quint8 x0048;                    // 0048
  char desc1[20];                  // 0049_005C
  quint16 head2 = 0;               // 005D_005E
  quint16 sectors2 = 0;            // 005F_0060
  quint8 e1 = 0;                   // 0061
  quint8 e2 = 0;                   // 0062
  quint16 nBlocks1;                // 0063_0064
  char desc2[30];                  // 0065_0082
  quint8 x0083_01BE[0x13C] = {0};  // 0083_01BE
  quint8 startHead = 0;            // 01BF
  quint8 startSector = 1;          // 01C0
  quint8 startCylinder = 0;        // 01C1
  quint8 systemType = 0;           // 01C2
  quint8 endHead = 0;              // 01C3
  quint8 endSector = 0;            // 01C4
  quint8 endCylinder = 0;          // 01C5
  quint32 relSectors = 0;          // 01C6_01C9
  quint32 nSectors = 0;            // 01CA_01CD
  quint8 x01CE_01FD[0x30] = {0};   // 01CE_01FD
  quint16 terminator = 0xAA55;     // 01FE_01FF
  quint32 blocksize() { return 1 << (e1 + e2); }
  void print();
};

struct FATBlock_t {
  quint8 flag;            // 0000
  char name[8];           // 0001_0008
  char type[3];           // 0009_000B
  quint32 size;           // 000C_000F
  quint16 part;           // 0010_0011
  quint8 x0012_001F[14];  // 0012_001F
  quint16 blocks[240];    // 0020_01FF
};

// common header for all subtypes
struct submap_hdr_t {
  quint16 size;   // 0000_0001
  char type[10];  // 0002_000B
  quint8 x000C;   // 000C
  quint8 flag;    // 000D
  qint16 year;    // 000E_000F
  qint8 month;    // 0010
  qint8 day;      // 0011
  qint8 hour;     // 0012
  qint8 minute;   // 0013
  qint8 second;   // 0014
  void print();
};

struct gmp_hdr_t : public submap_hdr_t {
  quint8 x0015_0018[4];  // 0015_0018
  quint32 tre_offset;    // 0019_001C
  quint32 rgn_offset;    // 001D_0020
  quint32 lbl_offset;    // 0021_0024
  quint32 net_offset;    // 0025_0028
  quint32 nod_offset;    // 0029_002E
  quint32 dem_offset;    // 002D_0030
  quint32 mar_offset;    // 0031_0034
  quint32 met_offset;    // 0035_0038
  void print();
};

struct hdr_tre_t : public submap_hdr_t {
  quint24 northbound = {0};            // 0015_0017 - max lat
  quint24 eastbound = {0};             // 0018_001A - max long
  quint24 southbound = {0};            // 001B_001D - min lat
  quint24 westbound = {0};             // 001E_0020 - min long, cant be +180
  quint32 tre1_offset = 0;             // 0021_0024 - map levels pos
  quint32 tre1_size = 0;               // 0025_0028
  quint32 tre2_offset = 0;             // 0029_002C - subdiv pos
  quint32 tre2_size = 0;               // 002D_0030
  quint32 tre3_offset = 0;             // 0031_0034 - TRE-body relative offset (new format) or copyright offset in TRE
  quint32 tre3_size = 0;               // 0035_0038
  quint16 tre3_rec_size = 0;           // 0039_003A
  quint8 x003B_003E[4] = {0};          // 003B_003E
  quint8 POI_flags = 0;                // 003F      - poi display flags
  quint24 render_prio = {0x14, 0, 0};  // 0040_0042 - display priority
  quint8 x0043_0049[7] = {0};          // 0043_0049
  quint32 tre4_offset = 0;             // 004A_004D - polyline overview
  quint32 tre4_size = 0;               // 004E_0051
  quint16 tre4_rec_size = 0;           // 0052_0053
  quint8 x0054_0057[4] = {0};          // 0054_0057
  quint32 tre5_offset = 0;             // 0058_005B - polygon overview
  quint32 tre5_size = 0;               // 005C_005F
  quint16 tre5_rec_size = 0;           // 0060_0061
  quint8 x0062_0065[4] = {0};          // 0062_0065
  quint32 tre6_offset = 0;             // 0066_0069 - points overview
  quint32 tre6_size = 0;               // 006A_006D
  quint16 tre6_rec_size = 0;           // 006E_006F
  quint8 x0070_0073[4] = {0};          // 0070_0073
  quint32 map_id = 0;                  // 0074_0077
  quint8 x0078_007B[4] = {0};          // 0078_007B
  quint32 tre7_offset = 0;             // 007C_007F - extended type offsets
  quint32 tre7_size = 0;               // 0080_0083
  quint16 tre7_rec_size = 0;           // 0084_0085
  quint8 x0086_0089[4] = {0};          // 0086_0089 - 0x01 0x00 0x00 0x00
  quint32 tre8_offset = 0;             // 008A_008D - extended type overview: ln, pg, po; sorted by type (1 type 1 levels 1 subtype)
  quint32 tre8_size = 0;               // 008E_0091
  quint16 tre8_rec_size = 0;           // 0092_0093
  quint16 polyl2_types_num = 0;        // 0094_0095 - num ext type ln
  quint16 polyg2_types_num = 0;        // 0096_0097 - num ext type pg
  quint16 poi2_types_num = 0;          // 0098_0099 - num ext type pt
  quint8 key[16] = {0};                // 009A_00A5 - map values
  quint8 x00AA_00AD[4] = {0};          // 00AA_00AD
  quint32 tre9_offset;                 // 00AE_00B1
  quint32 tre9_size;                   // 00B2_00B5
  quint16 tre9_rec_size;               // 00B6_00B7
  quint8 x00B8_00BB[4] = {0};          // 00B8_00BB
  quint32 tre10_offset;                // 00BC_00BF
  quint32 tre10_size;                  // 00C0_00C3
  quint16 tre10_rec_size;              // 00C4_00C5
  quint8 x00C6_00CE[9] = {0};          // 00C6_00CE
  quint32 map_id2;                     // 00CF_00D2 - map id 2
  void print(quint32 offset);
};

struct hdr_rgn_t : public submap_hdr_t {
  quint32 rgn1_offset = 0;      // 0015_0018 - RGN-body relative offset (new format)
  quint32 rgn1_length = 0;      // 0019_001C
  quint32 pg2_offset = 0;       // 001D_0020
  quint32 pg2_length = 0;       // 0021_0024
  quint8 x0025_0038[20] = {0};  // 0025_0038
  quint32 ln2_offset = 0;       // 0039_003C
  quint32 ln2_length = 0;       // 003D_0040
  quint8 x0041_0054[20] = {0};  // 0041_0054
  quint32 pt2_offset = 0;       // 0055_0058
  quint32 pt2_length = 0;       // 0059_005C
  quint8 x005D_0070[20] = {0};  // 005D_0070
  quint32 rgn2_offset = 0;      // 0071_0074
  quint32 rgn2_length = 0;      // 0075_0078
  quint32 unknown = 0;          // 0079_007C - E3 or E7
  void print(quint32 offset);
};

struct hdr_lbl_t : public submap_hdr_t {
  quint32 lbl1_offset = 0;     // 0015_0018 - sort description length
  quint32 lbl1_length = 0;     // 0019_001C - label size
  quint8 addr_shift = 0;       // 001D      - offset multiplier
  quint8 coding = 0;           // 001E      - encoding type
  quint32 lbl2_offset = 0;     // 001F_0022
  quint32 lbl2_length = 0;     // 0023_0026
  quint16 lbl2_rec_size = 0;   // 0027_0028
  quint8 x0029_002C[4] = {0};  // 0029_002C
  quint32 lbl3_offset = 0;     // 002D_0030
  quint32 lbl3_length = 0;     // 0031_0034
  quint16 lbl3_rec_size = 0;   // 0035_0036
  quint8 x0037_003A[4] = {0};  // 0037_003A
  quint32 lbl4_offset = 0;     // 003B_003E
  quint32 lbl4_length = 0;     // 003F_0042
  quint16 lbl4_rec_size = 0;   // 0043_0044
  quint8 x0045_0048[4] = {0};  // 0045_0048
  quint32 lbl5_offset = 0;     // 0049_004C
  quint32 lbl5_length = 0;     // 004D_0050
  quint16 lbl5_rec_size = 0;   // 0051_0052
  quint8 x0053_0056[4] = {0};  // 0053_0056
  quint32 lbl6_offset = 0;     // 0057_005A
  quint32 lbl6_length = 0;     // 005B_005E
  quint8 lbl6_addr_shift = 0;  // 005F
  quint8 lbl6_glob_mask = 0;   // 0060
  quint8 x0061_0063[3] = {0};  // 0061_0063
  quint32 lbl7_offset = 0;     // 0064_0067
  quint32 lbl7_length = 0;     // 0068_006B
  quint16 lbl7_rec_size = 0;   // 006C_006D
  quint8 x006E_0071[4] = {0};  // 006E_0071
  quint32 lbl8_offset = 0;     // 0072_0075
  quint32 lbl8_length = 0;     // 0076_0079
  quint16 lbl8_rec_size = 0;   // 007A_007B
  quint8 x007C_007F[4] = {0};  // 007C_007F
  quint32 lbl9_offset = 0;     // 0080_0083
  quint32 lbl9_length = 0;     // 0084_0087
  quint16 lbl9_rec_size = 0;   // 0088_0089
  quint8 x008A_008D[4] = {0};  // 008A_008D
  quint32 lbl10_offset = 0;    // 008E_0091
  quint32 lbl10_length = 0;    // 0092_0095
  quint16 lbl10_rec_size = 0;  // 0096_0097
  quint8 x0098_009B[4] = {0};  // 0098_009B
  quint32 lbl11_offset = 0;    // 009C_009F
  quint32 lbl11_length = 0;    // 00A0_00A3
  quint16 lbl11_rec_size = 0;  // 00A4_00A5
  quint8 x00A6_00A9[4] = {0};  // 00A6_00A9
  quint16 codepage = 0;        // 00AA_00AB - optional check length
  quint8 x00AC_00AF[4] = {0};  // 00AC_00AF - 0x07 0x00 0x02 0x80 or 0x12 0x00 0x01 0x80
  quint32 lbl12_offset = 0;    // 00B0_00B3 - LBL-body relative offset (new format) or sort descriptor
  quint32 lbl12_length = 0;    // 00B4_00B7
  void print(quint32 offset);
};

struct hdr_net_t : public submap_hdr_t {
  quint32 net1_offset;     // 0015_0018 - NET-body relative offset (new format)
  quint32 net1_length;     // 0019_001C
  quint8 net1_addr_shift;  // 001D
  quint32 net2_offset;     // 001E_0021
  quint32 net2_length;     // 0022_0025
  quint8 net2_addr_shift;  // 0026
  quint32 net3_offset;     // 0027_002A
  quint32 net3_length;     // 002B_002E
  void print(quint32 offset);
};

struct hdr_nod_t : public submap_hdr_t {
  quint32 nod1_offset;  // 0015_0018 - NOD-body relative offset (new format)
  quint32 not1_length;  // 0019_001C
  quint8 not1_flags;    // 001D
  void print(quint32 offset);
};

struct hdr_dem_t : public submap_hdr_t {
  quint32 flags;         // 0015_0018 - 0=meter, 1=feet
  quint16 zoom_levels;   // 0019_001A
  quint8 x001B_001D[4];  // 001B_001D
  quint16 rec_size;      // 001F_0020
  quint32 dem1_offset;   // 0021_0024
  quint8 x0025_0029[4];  // 0025_0029
  void print(quint32 offset);
};

// copyright header
struct tre0_t {
  QString descr1;
  QString descr2;
};

// map level definition
struct tre1_t {
  quint8 raw_zoom;  // 7=inherit, 6-0=zoom
  quint8 bits;
  quint16 subdiv;
  tre1_t() : raw_zoom(0), bits(0), subdiv(0) {}
  bool inherit() const { return raw_zoom & 0x80; }
  quint8 zoom() const { return raw_zoom & 0x7F; }
  void print() { printf("zoom: %02X | inherit: %i | bits: %i | subdiv: %i\n", zoom(), inherit(), bits, subdiv); }
};

// map subdivision definition, without pointer to the lower level subparts
struct tre2_t {
  quint24 rgn_offset;  // offset: 27-0 bit, 31-26 elements
  quint8 elements;
  quint24 center_lng;
  quint24 center_lat;
  quint16 width_trm;
#define TRE_SUBDIV_WIDTH(r) (r->width_trm & 0x7FFF)
#define TRE_SUBDIV_TERM(r) ((r->width_trm & 0x8000) != 0)
  quint16 height;
};

// pointer to the lower level subparts
struct tre2_next_t : public tre2_t {
  quint16 next;
};

struct tre7_t {
  quint32 offsetPolygons;
  quint32 offsetPolyline;
  quint32 offsetPoints;
  quint8 btObjects;
};

// subdivision  information
struct subdiv_t {
  quint32 n;
  quint16 next;       // section of next level
  bool terminate;     // end of section group
  quint32 rgn_start;  // offset into the submap's RGN part
  quint32 rgn_end;    // end of section in RGN part (last offset = rgn_end - 1)
  bool hasPoints;     // there are points stored in the RGN subsection
  bool hasPois;       // there are indexed points stored in the RGN subsection
  bool hasPolylines;  // there are polylines stored in the RGN subsection
  bool hasPolygons;   // there are polygons stored in the RGN subsection
  qint32 iCenterLng;  // the center longitude of the area covered by this subdivision
  qint32 iCenterLat;  // the center latitude of the area covered by this subdivision
  double north;       // north boundary of area covered by this subsection []
  double east;        // east boundary of area covered by this subsection []
  double south;       // south boundary of area covered by this subsection []
  double west;        // west boundary of area covered by this subsection []
  QRectF area;        // area in meter coordinates covered by this subdivision []
  quint32 shift;      // number of left shifts for RGN data
  quint32 level;      // map level this subdivision is shown
  quint32 offPolygons2;
  quint32 lenPolygons2;
  quint32 offPoints2;
  quint32 lenPoints2;
  quint32 offPolylines2;
  quint32 lenPolylines2;
  tre1_t *maplevel;
  void print() const;
  void printLite() const;
  // subdiv_t() { memset(this, 0, sizeof(subdiv_t)); }
};
};  // namespace IMG

#pragma pack(pop)