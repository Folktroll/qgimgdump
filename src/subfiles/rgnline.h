#pragma once

#include <QPolygonF>

#include "bitstrreader.h"
#include "misc.h"

class RgnLine {
 public:
  RgnLine() : type(0), direction(false), lbl_info(0), hasNet1Label(false), hasExtLabel(false), dLng(0), dLat(0) {}
  virtual ~RgnLine() = default;

  quint32 decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8 *pData, const quint8 *pEnd);
  quint32 decodeExt(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8 *pData, const quint8 *pEnd);

  bool hasLabel() const { return !labels.isEmpty(); }

  quint32 type;
  bool direction;     // direction of line (polyline, only)
  quint32 lbl_info;   // the label offset
  bool hasNet1Label;  // true if label offset has to be used in NET subfile (up to four label pointers), @todo: missing labels
  QPolygonF points;   // the actual polyline points as longitude / latitude [rad]
  QStringList labels;
  quint8 hasExtLabelCount = 0;

 private:
  bool hasExtLabel;  // ext label
  qint16 dLng;       // delta longitude from subdivision center
  qint16 dLat;       // delta latitude from subdivision center
  // QVector<double> u;  // the actual polyline points as longitude / latitude [rad]
  // QVector<double> v;  // the actual polyline points as longitude / latitude [rad]
  // inline static qint32 maxVecSize = 0;
  static qint32 maxVecSize;
  QString debugData;
  void bitsPerCoord(quint8 base, quint8 bfirst, quint32 &bx, quint32 &by, sign_info_t &signinfo, bool isVer2);
  int bitsPerCoord(quint8 base, bool is_signed);
};
