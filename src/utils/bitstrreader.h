#pragma once

#include "misc.h"

namespace App {

class BitstrReader {
 public:
  BitstrReader(const quint8 *pData, quint32 n, quint32 bx, quint32 by, bool extra_bit, SignInfo_t &si);
  bool get(qint32 &x, qint32 &y);

 private:
  void fill(quint32 bits);
  quint64 reg;            // the register to work on
  const quint8 *pData;    // the data stream to get data from
  quint32 bytes;          // bytes left in stream
  quint32 xmask;          // bitmask x coord.
  quint32 ymask;          // bitmask y coord.
  qint32 xsign;           // sign bit for x value
  qint32 ysign;           // sign bit for y value
  qint32 xsign2;          // sign bit * 2 for x value
  qint32 ysign2;          // sign bit * 2 for y value
  quint8 bits;            // total bits in register
  quint8 bits_per_x;      // bits per x coord
  quint8 bits_of_byte;    // used bits of first byte
  quint8 bits_per_y;      // bits per y coord
  quint8 bits_per_coord;  // bits per coord.
  SignInfo_t &sinfo;
  bool extraBit;
};

}  // namespace App
