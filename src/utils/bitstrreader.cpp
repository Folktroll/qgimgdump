#include "bitstrreader.h"

using namespace App;

BitstrReader::BitstrReader(const quint8 *pData, quint32 n, quint32 bx, quint32 by, bool extra_bit, SignInfo_t &si)
    : reg(0),
      pData(pData),
      bytes(n),
      xmask(0xFFFFFFFF),
      ymask(0xFFFFFFFF),
      xsign(1),
      ysign(1),
      xsign2(2),
      ysign2(2),
      bits(0),
      bits_per_x(bx),
      bits_per_y(by),
      bits_per_coord(bx + by + (extra_bit ? 1 : 0)),
      sinfo(si),
      extraBit(extra_bit) {
  // create bit masks
  xmask = (xmask << (32 - bx)) >> (32 - bx);
  ymask = (ymask << (32 - by)) >> (32 - by);

  xsign <<= (bits_per_x - 1);
  ysign <<= (bits_per_y - 1);
  xsign2 = xsign << 1;
  ysign2 = ysign << 1;

  // add sufficient bytes for the first coord. pair
  fill(bits_per_coord + si.sign_info_bits);

  // get rid of sign setup bytes
  reg >>= si.sign_info_bits;
  bits -= si.sign_info_bits;
}

bool BitstrReader::get(qint32 &x, qint32 &y) {
  x = y = 0;
  if (bits < (bits_per_coord)) {
    return false;
  }

  // don't know what to do with it -> skip extra bit
  if (extraBit) {
    reg >>= 1;
    bits -= 1;
  }

  if (sinfo.x_has_sign) {
    qint32 tmp = 0;
    while (1) {
      tmp = reg & xmask;
      if (tmp != xsign) {
        break;
      }
      x += tmp - 1;
      reg >>= bits_per_x;
      bits -= bits_per_x;
      fill(bits_per_y + bits_per_x);
    }
    if (tmp < xsign) {
      x += tmp;
    } else {
      x = tmp - (xsign2)-x;
    }
  } else {
    x = reg & xmask;
    if (sinfo.nx) {
      x = -x;
    }
  }
  reg >>= bits_per_x;
  bits -= bits_per_x;

  // take y coord., add sign if neccessary, shift register by bits per y coord.
  if (sinfo.y_has_sign) {
    qint32 tmp = 0;
    while (1) {
      tmp = reg & ymask;
      if (tmp != ysign) {
        break;
      }
      y += tmp - 1;
      reg >>= bits_per_y;
      bits -= bits_per_y;
      fill(bits_per_y);
    }
    if (tmp < ysign) {
      y += tmp;
    } else {
      y = tmp - (ysign2)-y;
    }
  } else {
    y = reg & ymask;
    if (sinfo.ny) {
      y = -y;
    }
  }
  reg >>= bits_per_y;
  bits -= bits_per_y;

  // fill register until it has enought bits for one coord. pair again
  fill(bits_per_coord);
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
