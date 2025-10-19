#pragma once

#include <QtTypes>

#include "print.h"

#pragma pack(push, 1)

namespace ImgHdr {

using App::printData;

struct SSupp {
  quint8 xorByte = 0;                          // 0000
  std::array<quint8, 7> x0001_0007 = {0};      // 0001_0007
  quint16 version = 0;                         // 0008
  quint8 upMonth = 0;                          // 000A
  quint8 upYear = 0;                           // 000B
  std::array<quint8, 2> x000C_000D = {0};      // 000C_000D
  quint8 supp = 0;                             // 000E
  quint8 checksum = 0;                         // 000F
  std::array<char, 7> signature = {0};         // 0010_0016
  quint8 x0017 = 0x2;                          // 0017
  quint16 sectors1 = 0;                        // 0018_0019
  quint16 heads1 = 0;                          // 001A_001B
  quint16 cylinders = 0;                       // 001C_001D
  std::array<quint8, 27> x001E_0038 = {0};     // 001E_0038
  qint16 year = 0;                             // 0039_003A
  qint8 month = 0;                             // 003B
  qint8 day = 0;                               // 003C
  qint8 hour = 0;                              // 003D
  qint8 minute = 0;                            // 003E
  qint8 second = 0;                            // 003F
  qint8 offsetFAT = 0;                         // 0040
  std::array<char, 7> identifier;              // 0041_0047
  quint8 x0048;                                // 0048
  std::array<char, 20> desc1;                  // 0049_005C
  quint16 head2 = 0;                           // 005D_005E
  quint16 sectors2 = 0;                        // 005F_0060
  quint8 e1 = 0;                               // 0061
  quint8 e2 = 0;                               // 0062
  quint16 nBlocks1;                            // 0063_0064
  std::array<char, 30> desc2;                  // 0065_0082
  std::array<quint8, 0x13C> x0083_01BE = {0};  // 0083_01BE
  quint8 startHead = 0;                        // 01BF
  quint8 startSector = 1;                      // 01C0
  quint8 startCylinder = 0;                    // 01C1
  quint8 systemType = 0;                       // 01C2
  quint8 endHead = 0;                          // 01C3
  quint8 endSector = 0;                        // 01C4
  quint8 endCylinder = 0;                      // 01C5
  quint32 relSectors = 0;                      // 01C6_01C9
  quint32 nSectors = 0;                        // 01CA_01CD
  std::array<quint8, 0x30> x01CE_01FD = {0};   // 01CE_01FD
  quint16 terminator = 0xAA55;                 // 01FE_01FF
  quint32 blocksize() const { return 1 << (e1 + e2); }

  void print() const {
    PRINT_HDR(xorByte);
    PRINT_HDR(x0001_0007);
    PRINT_HDR(version);
    PRINT_HDR(upMonth);
    PRINT_HDR(upYear);
    PRINT_HDR(x000C_000D);
    PRINT_HDR(supp);
    PRINT_HDR(checksum);
    PRINT_HDR(signature);
    PRINT_HDR(x0017);
    PRINT_HDR(sectors1);
    PRINT_HDR(heads1);
    PRINT_HDR(cylinders);
    PRINT_HDR(x001E_0038);
    PRINT_HDR(year);
    PRINT_HDR(month);
    PRINT_HDR(day);
    PRINT_HDR(hour);
    PRINT_HDR(minute);
    PRINT_HDR(second);
    PRINT_HDR(offsetFAT);
    PRINT_HDR(identifier);
    PRINT_HDR(x0048);
    PRINT_HDR(desc1);
    PRINT_HDR(head2);
    PRINT_HDR(sectors2);
    PRINT_HDR(e1);
    PRINT_HDR(e2);
    PRINT_HDR(nBlocks1);
    PRINT_HDR(desc2);
    PRINT_HDR(x0083_01BE);
    PRINT_HDR(startHead);
    PRINT_HDR(startSector);
    PRINT_HDR(startCylinder);
    PRINT_HDR(systemType);
    PRINT_HDR(endHead);
    PRINT_HDR(endSector);
    PRINT_HDR(endCylinder);
    PRINT_HDR(relSectors);
    PRINT_HDR(nSectors);
    PRINT_HDR(x01CE_01FD);
    PRINT_HDR(terminator);
  }
};

};  // namespace ImgHdr

#pragma pack(pop)