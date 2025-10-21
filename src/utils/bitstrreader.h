#pragma once

#include "misc.h"

namespace App {

class BitstrReader {
 public:
  BitstrReader(const quint8 *pData, quint32 index, quint32 bx, quint32 by, bool extraBit, SignInfo_t &si);
  bool get(qint32 &x, qint32 &y);

 private:
  void fill(quint32 bits);
  quint64 reg;          // the register to work on
  const quint8 *pData;  // the data stream to get data from
  quint32 bytes;        // bytes left in stream
  quint32 xMask;        // bitmask x coord.
  quint32 yMask;        // bitmask y coord.
  qint32 xSign;         // sign bit for x value
  qint32 ySign;         // sign bit for y value
  qint32 xSign2;        // sign bit * 2 for x value
  qint32 ySign2;        // sign bit * 2 for y value
  quint8 bits;          // total bits in register
  quint8 bitsPerX;      // bits per x coord
  quint8 bitsOfByte;    // used bits of first byte
  quint8 bitsPerY;      // bits per y coord
  quint8 bitsPerCoord;  // bits per coord.
  SignInfo_t &sInfo;
  bool extraBit;
};

}  // namespace App
