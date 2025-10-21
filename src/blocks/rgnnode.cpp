#include "rgnnode.h"

#include "lbl.h"
#include "misc.h"

using namespace App;

quint32 RgnNode::decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8 *pData) {
  type = (quint16)(*pData) << 8;

  ++pData;

  lblPtr = gar_ptr_load(uint24_t, pData);

  hasSubType = lblPtr & 0x00800000;
  isLbl6 = lblPtr & 0x00400000;
  lblPtr = lblPtr & 0x003FFFFF;

  pData += 3;

  qint16 dLng = gar_ptr_load(int16_t, pData);
  pData += 2;
  qint16 dLat = gar_ptr_load(int16_t, pData);
  pData += 2;

  qint32 x1 = ((qint32)dLng << shift) + iCenterLon;
  qint32 y1 = ((qint32)dLat << shift) + iCenterLat;
  node = QPointF(GRMN_RAD(x1), GRMN_RAD(y1));

  if (hasSubType) {
    type |= *pData;
    return 9;
  }

  return 8;
}

quint32 RgnNode::decodeEx(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8 *pData, const quint8 *pEnd) {
  quint32 byte_size = 6;
  quint8 subtype;

  // @todo: fixme: if (lbl3 & 0x80) == 128 -> contains a subtype
  // This is where Mechalas is incorrect: it is not the bit 8 of first byte, but bit 8 of the 4th byte
  type = (quint16)(*pData) << 8;
  ++pData;
  subtype = (quint16)(*pData);
  ++pData;

  type = 0x10000 + type + (subtype & 0x1F);

  /*
  subtype = (quint8)(pData[3]);
  pData += 2;
  */

  if (subtype & 0x80) {
    hasSubType = true;
    byte_size += 1;
  }

  qint16 dLng = gar_ptr_load(int16_t, pData);
  pData += 2;
  qint16 dLat = gar_ptr_load(int16_t, pData);
  pData += 2;

  qint32 x1, y1;

  x1 = ((qint32)dLng << shift) + iCenterLon;
  y1 = ((qint32)dLat << shift) + iCenterLat;
  node = QPointF(GRMN_RAD(x1), GRMN_RAD(y1));

  if (subtype & 0x20) {
    byte_size += 3;
    lblPtr = gar_ptr_load(uint24_t, pData);
    isLbl6 = lblPtr & 0x00400000;
    lblPtr &= 0x003FFFFF;
  }

  return byte_size;
}
