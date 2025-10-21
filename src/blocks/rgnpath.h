#pragma once

#include <QPolygonF>

#include "bitstrreader.h"
#include "misc.h"

namespace App {

class RgnPath {
 public:
  explicit RgnPath() = default;
  virtual ~RgnPath() = default;

  quint32 decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8 *pData, const quint8 *pEnd);
  quint32 decodeEx(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8 *pData, const quint8 *pEnd);

  bool hasLabel() const { return !labels.isEmpty(); }

  quint32 type = 0;
  bool direction = false;     // direction of line (polyline, only)
  quint32 lblInfo = 0;        // the label offset
  bool hasNet1Label = false;  // true if label offset has to be used in NET block (up to four label pointers), @todo: missing labels
  QPolygonF path;             // the actual polyline points as longitude / latitude [rad]
  QStringList labels;
  quint8 hasExLabelCount = 0;
  bool hasExLabel = false;  // ext label
  qint16 dLng = 0;          // delta longitude from subdivision center
  qint16 dLat = 0;          // delta latitude from subdivision center
  // QVector<double> u;  // the actual polyline points as longitude / latitude [rad]
  // QVector<double> v;  // the actual polyline points as longitude / latitude [rad]
  // inline static qint32 maxVecSize = 0;
  static qint32 maxVecSize;
  QString debugData;

 private:
  void bitsPerCoord(quint8 base, quint8 bFirst, quint32 &bx, quint32 &by, SignInfo_t &signInfo, bool isVer2);
  int bitsPerCoord(quint8 base, bool isSigned);
};

using Paths_t = QVector<RgnPath>;

}  // namespace App
