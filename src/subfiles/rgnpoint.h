#pragma once

#include <QPointF>
#include <QStringList>

#include "misc.h"

namespace App {

class RgnPoint {
 public:
  RgnPoint() = default;
  virtual ~RgnPoint() = default;
  quint32 decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8 *pData);
  quint32 decodeExt(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8 *pData, const quint8 *pEnd);
  bool hasLabel() const { return !labels.isEmpty(); }
  quint32 type = 0;
  bool isLbl6 = false;
  bool hasSubType = false;
  QPointF pos;
  QStringList labels;
  quint32 lbl_ptr = 0xFFFFFFFF;
};

using Point_t = QVector<RgnPoint>;

};  // namespace App
