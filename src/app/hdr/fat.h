#pragma once

#include <QtTypes>

#include "print.h"

namespace ImgHdr {

using App::printData;

#pragma pack(push, 1)
// IMG file header structure, to the start of the FAT blocks

struct SFat {
  quint8 flag;                        // 0000
  std::array<char, 8> name;           // 0001_0008
  std::array<char, 3> type;           // 0009_000B
  quint32 size;                       // 000C_000F
  quint16 part;                       // 0010_0011
  std::array<quint8, 14> x0012_001F;  // 0012_001F
  std::array<quint16, 240> blocks;    // 0020_01FF
  void print() const {
    PRINT_HDR(flag);
    PRINT_HDR(name);
    PRINT_HDR(type);
    PRINT_HDR(size);
    PRINT_HDR(part);
    PRINT_HDR(x0012_001F);
    PRINT_HDR(blocks);
    PRINT_ENDL;
  }
};

#pragma pack(pop)

};  // namespace ImgHdr