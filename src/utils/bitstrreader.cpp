#include "bitstrreader.h"

using namespace App;

BitstrReader::BitstrReader(const quint8 *pData, quint32 index, quint32 bx, quint32 by, bool extraBit, SignInfo_t &si)
    : reg(0),
      pData(pData),
      bytes(index),
      xMask(0xFFFFFFFF),
      yMask(0xFFFFFFFF),
      xSign(1),
      ySign(1),
      xSign2(2),
      ySign2(2),
      bits(0),
      bitsPerX(bx),
      bitsPerY(by),
      bitsPerCoord(bx + by + (extraBit ? 1 : 0)),
      sInfo(si),
      extraBit(extraBit) {
  // create bit masks
  xMask = (xMask << (32 - bx)) >> (32 - bx);
  yMask = (yMask << (32 - by)) >> (32 - by);

  xSign <<= (bitsPerX - 1);
  ySign <<= (bitsPerY - 1);
  xSign2 = xSign << 1;
  ySign2 = ySign << 1;

  // add sufficient bytes for the first coord. pair
  fill(bitsPerCoord + si.sign_info_bits);

  // get rid of sign setup bytes
  reg >>= si.sign_info_bits;
  bits -= si.sign_info_bits;
}

bool BitstrReader::get(qint32 &x, qint32 &y) {
  x = y = 0;
  if (bits < (bitsPerCoord)) {
    return false;
  }

  // don't know what to do with it -> skip extra bit
  if (extraBit) {
    reg >>= 1;
    bits -= 1;
  }

  if (sInfo.x_has_sign) {
    qint32 tmp = 0;
    while (true) {
      tmp = reg & xMask;
      if (tmp != xSign) {
        break;
      }
      x += tmp - 1;
      reg >>= bitsPerX;
      bits -= bitsPerX;
      fill(bitsPerY + bitsPerX);
    }
    if (tmp < xSign) {
      x += tmp;
    } else {
      x = tmp - xSign2 - x;
    }
  } else {
    x = reg & xMask;
    if (sInfo.nx) {
      x = -x;
    }
  }
  reg >>= bitsPerX;
  bits -= bitsPerX;

  // take y coord., add sign if neccessary, shift register by bits per y coord.
  if (sInfo.y_has_sign) {
    qint32 tmp = 0;
    while (true) {
      tmp = reg & yMask;
      if (tmp != ySign) {
        break;
      }
      y += tmp - 1;
      reg >>= bitsPerY;
      bits -= bitsPerY;
      fill(bitsPerY);
    }
    if (tmp < ySign) {
      y += tmp;
    } else {
      y = tmp - ySign2 - y;
    }
  } else {
    y = reg & yMask;
    if (sInfo.ny) {
      y = -y;
    }
  }
  reg >>= bitsPerY;
  bits -= bitsPerY;

  // fill register until it has enought bits for one coord. pair again
  fill(bitsPerCoord);
  return true;
}

void BitstrReader::fill(quint32 b) {
  quint64 tmp = 0;
  while ((bits < b) && bytes) {
#if (Q_BYTE_ORDER == Q_LITTLE_ENDIAN)
    (quint8 &)tmp = *pData++;
#else
    tmp = *pData++;
#endif
    --bytes;

    reg |= tmp << bits;
    bits += 8;
  }
}
