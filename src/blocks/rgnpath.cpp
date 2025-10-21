#include "rgnpath.h"

#include "global.h"
#include "inline.h"
#include "misc.h"

using namespace App;

qint32 RgnPath::maxVecSize = 0;

quint32 RgnPath::decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8 *pData, const quint8 *pEnd) {
  quint32 bytes_total = 10;
  bool two_byte_len;   // bitstream has a two byte length
  bool extra_bit;      // coordinates use extra bit - ??? have never seen it
  quint16 bs_len = 0;  // bitstream length
  quint8 bs_info;      // base bit size info for coordinates
  quint32 bx;          // bits per x coord
  quint32 by;          // bits per y coord
  const quint8 *const pStart = pData;

  labels.clear();
  path.resize(0);
  path.reserve(maxVecSize);
  // u.resize(0);
  // v.resize(0);
  // u.reserve(maxVecSize);
  // v.reserve(maxVecSize);

  /* poly_type
      for polylines:
      bit 0..5    type
      bit 6       direction
      for polygons:
      bit 0..6    type
      bit 7       bitstream_len is two bytes (true)
   */
  type = *pData++;

  two_byte_len = type & 0x80;
  if (line) {
    direction = (type & 0x40);
    type &= 0x3F;
  } else {
    type &= 0x7F;
  }

  /* label info
      bit 0..21   off set into LBL block
      bit 22      use extra bit for coordinates
      bit 23      use label data of NET block (net1)
   */
  lblInfo = gar_ptr_load(uint24_t, pData);
  hasNet1Label = lblInfo & 0x800000;
  extra_bit = lblInfo & 0x400000;
  lblInfo = lblInfo & 0x3FFFFF;

  pData += 3;

  // delta longitude and latitude
  dLng = gar_ptr_load(uint16_t, pData);
  pData += 2;
  dLat = gar_ptr_load(uint16_t, pData);
  pData += 2;

  // bitstream length
  if (two_byte_len) {
    bs_len = gar_ptr_load(uint16_t, pData);
    pData += 2;
    bytes_total += bs_len + 1;
  } else {
#if (Q_BYTE_ORDER == Q_LITTLE_ENDIAN)
    (quint8 &)bs_len = *pData++;
#else
    bs_len = *pData++;
#endif
    bytes_total += bs_len;
  }

  if (pEnd && ((pStart + bytes_total) > pEnd)) {
    return bytes_total;
  }

  /* bitstream info
      bit 0..3    base bits longitude
      bit 4..7    base bits latitude
   */
  bs_info = *pData++;

  debugData = debugHex(pData, bs_len);

  // if(extra_bit) qWarning("extrabit");

#ifdef DEBUG_SHOW_POLY_DATA_DECODE
  qDebug() << "type:      " << type << Qt::hex;
  qDebug() << "two byte:  " << two_byte_len;
  qDebug() << "extra bit: " << extra_bit;
  qDebug() << "dLng:      " << dLng;
  qDebug() << "dLat:      " << dLat;
  qDebug() << "len:       " << bs_len;
  qDebug() << "info:      " << Qt::hex << bs_info;
  qDebug() << "1st byte:  " << Qt::hex << *pData;
  qDebug() << "bytes total" << bytes_total;
  // qDebug() << "data1      " << rawData;
#endif

  SignInfo_t signinfo;
  bitsPerCoord(bs_info, *pData, bx, by, signinfo, false);

  BitstrReader sr(pData, bs_len, bx, by, extra_bit, signinfo);
  qint32 x1, y1, x = 0, y = 0;

  bool isNegative = (iCenterLon >= 0x800000);
  // first point
  x1 = ((qint32)dLng << shift) + iCenterLon;
  y1 = ((qint32)dLat << shift) + iCenterLat;

  if (x1 >= 0x800000 && !isNegative) {
    x1 = 0x7fffff;
  }

  // PJ_UV xy;
  // xy.u = GRMN_RAD(x1);
  // xy.v = GRMN_RAD(y1);
  // u << xy.u;
  // v << xy.v;
  // points << QPointF(qRadiansToDegrees(xy.u), qRadiansToDegrees(xy.v));
  path << QPointF(GRMN_RAD(x1), GRMN_RAD(y1));

  // next points
  while (sr.get(x, y)) {
    x1 += (x << shift);
    y1 += (y << shift);

    if (x1 >= 0x800000 && !isNegative) {
      x1 = 0x7fffff;
    }

    // xy.u = GRMN_RAD(x1);
    // xy.v = GRMN_RAD(y1);
    // u << xy.u;
    // v << xy.v;
    // points << QPointF(qRadiansToDegrees(xy.u), qRadiansToDegrees(xy.v));
    path << QPointF(GRMN_RAD(x1), GRMN_RAD(y1));
  }

  // if (maxVecSize < u.size()) {
  // maxVecSize = u.size();
  // }
  // if (u.size() * 1.2 < maxVecSize) {
  // u.squeeze();
  // v.squeeze();
  // }

  if (maxVecSize < path.size()) {
    maxVecSize = path.size();
  }
  if (path.size() * 1.2 < maxVecSize) {
    path.squeeze();
  }

  return bytes_total;
}

quint32 RgnPath::decodeEx(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8 *pData, const quint8 *pEnd) {
  quint32 bytes_total = 6;
  quint16 bs_len = 0;  // bitstream length
  quint32 subtype;     // type and subtype
  quint8 bs_info;      // base bit size info for coordinates
  quint32 bx;          // bits per x coord
  quint32 by;          // bits per y coord

  const quint8 *const pStart = pData;

  labels.clear();
  path.resize(0);
  path.reserve(maxVecSize);
  // u.resize(0);
  // v.resize(0);
  // u.reserve(maxVecSize);
  // v.reserve(maxVecSize);

  type = *pData++;
  subtype = *pData++;

  type = 0x10000 + (quint16(type) << 8) + (subtype & 0x1f);
  hasExLabel = subtype & 0x20;
  // delta longitude and latitude
  dLng = gar_ptr_load(uint16_t, pData);
  pData += 2;
  dLat = gar_ptr_load(uint16_t, pData);
  pData += 2;

  if ((*pData & 0x1) == 0) {
    bs_len = gar_ptr_load(uint16_t, pData);
    bs_len = (bs_len >> 2) - 1;
    pData += 2;
    bytes_total += 2;
  } else {
    bs_len = ((*pData) >> 1) - 1;
    pData += 1;
    bytes_total += 1;
  }

  bs_info = *pData++;
  bytes_total += bs_len + 1;

#ifdef DEBUG_SHOW_POLY_DATA_DECODE_EXT
  qDebug() << "type:      " << type << Qt::hex << type;
  qDebug() << "dLng:      " << dLng;
  qDebug() << "dLat:      " << dLat;
  qDebug() << "len:       " << bs_len;
  qDebug() << "info:      " << Qt::hex << bs_info;
  qDebug() << "1st byte:  " << Qt::hex << *pData;
  qDebug() << "bytes total" << bytes_total;
#endif

  SignInfo_t signinfo;
  bitsPerCoord(bs_info, *pData, bx, by, signinfo, true);

  // qDebug() << ">>" << bs_len << bytes_total << (pEnd - pStart);

  if (((quint32)(pEnd - pStart)) < bytes_total) {
    return pEnd - pStart;
  }

  BitstrReader sr(pData, bs_len, bx, by, false, signinfo);
  qint32 x1, y1, x = 0, y = 0;

  bool isNegative = (iCenterLon >= 0x800000);
  // first point
  x1 = ((qint32)dLng << shift) + iCenterLon;
  y1 = ((qint32)dLat << shift) + iCenterLat;

  if (x1 >= 0x800000 && !isNegative) {
    x1 = 0x7fffff;
  }

  path << QPointF(GRMN_RAD(x1), GRMN_RAD(y1));

  // next points
  while (sr.get(x, y)) {
    x1 += (x << shift);
    y1 += (y << shift);

    if (x1 >= 0x800000 && !isNegative) {
      x1 = 0x7fffff;
    }

    // xy.u = GRMN_RAD(x1);
    // xy.v = GRMN_RAD(y1);
    // u << xy.u;
    // v << xy.v;
    // points << QPointF(qRadiansToDegrees(xy.u), qRadiansToDegrees(xy.v));
    path << QPointF(GRMN_RAD(x1), GRMN_RAD(y1));
  }

  if (hasExLabel) {
    quint32 offset = gar_ptr_load(uint24_t, pData + bs_len);
    bytes_total += 3;
    // @todo: read label information
    lblInfo = offset & 0x3FFFFF;
    // qDebug() << "[INFO] hasExLabel=true" << Qt::hex << offset << lblInfo;
    ++hasExLabelCount;
  } else {
    lblInfo = 0;
  }

  // if (maxVecSize < u.size()) {
  // maxVecSize = u.size();
  // }
  // if (u.size() * 1.2 < maxVecSize) {
  // u.squeeze();
  // v.squeeze();
  // }

  if (maxVecSize < path.size()) {
    maxVecSize = path.size();
  }
  if (path.size() * 1.2 < maxVecSize) {
    path.squeeze();
  }

  return bytes_total;
}

void RgnPath::bitsPerCoord(quint8 base, quint8 bfirst, quint32 &bx, quint32 &by, SignInfo_t &signinfo, bool isVer2) {
  bool x_sign_same;
  bool y_sign_same;

  quint8 mask = 0x1;

  // x_sign_same = bfirst & 0x1;
  x_sign_same = bfirst & mask;
  mask <<= 1;

  if (x_sign_same) {
    signinfo.x_has_sign = false;
    //   signinfo.nx         = bfirst & 0x2;
    signinfo.nx = bfirst & mask;
    mask <<= 1;
    ++signinfo.sign_info_bits;
  } else {
    signinfo.x_has_sign = true;
  }
  bx = bitsPerCoord(base & 0x0F, signinfo.x_has_sign);

  // y_sign_same = x_sign_same ? (bfirst & 0x04) : (bfirst & 0x02);
  y_sign_same = bfirst & mask;
  mask <<= 1;

  if (y_sign_same) {
    signinfo.y_has_sign = false;
    //   signinfo.ny         = x_sign_same ? bfirst & 0x08 : bfirst & 0x04;
    signinfo.ny = bfirst & mask;
    mask <<= 1;
    ++signinfo.sign_info_bits;
  } else {
    signinfo.y_has_sign = true;
  }

  by = bitsPerCoord((base >> 4) & 0x0F, signinfo.y_has_sign);

  // Determine extra bits.
  if (isVer2) {
    ++signinfo.sign_info_bits;
    if (bfirst & mask) {
      ++bx;
      ++by;
    }
  }
}

// extract bits per coordinate
int RgnPath::bitsPerCoord(quint8 base, bool is_signed) {
  int n = 2;

  if (base <= 9) {
    n += base;
  } else {
    n += (2 * base - 9);
  }

  if (is_signed) {
    ++n;
  }
  return n;
}
