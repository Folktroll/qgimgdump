#pragma once

#include <QPointF>
#include <QStringList>

#include "misc.h"

namespace App {

class RgnNode {
 public:
  RgnNode() = default;
  virtual ~RgnNode() = default;
  quint32 decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8 *pData);
  quint32 decodeEx(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8 *pData, const quint8 *pEnd);
  bool hasLabel() const { return !labels.isEmpty(); }
  quint32 type = 0;
  bool isLbl6 = false;
  bool hasSubType = false;
  QPointF node;
  QPointF nodeRad;
  QStringList labels;
  quint32 lblPtr = 0xFFFFFFFF;
};

using Nodes_t = QVector<RgnNode>;

};  // namespace App
