#pragma once

#include <QPointF>
#include <QPolygonF>
#include <QtGlobal>
#include <QtMath>

namespace DebugCounters {
static quint8 warnInvalidCoords = 0;
}

inline double normalizeLng(double lngDeg) {
  while (lngDeg > 180.0) lngDeg -= 360.0;
  while (lngDeg < -180.0) lngDeg += 360.0;
  return lngDeg;
}

inline double clampLat(double latDeg) {
  if (latDeg > 90.0) latDeg = 90.0;
  if (latDeg < -90.0) latDeg = -90.0;
  return latDeg;
}

inline QPointF toDegreesSafe(const QPointF &pointRad) {
  double lat = qRadiansToDegrees(pointRad.y());
  double lng = qRadiansToDegrees(pointRad.x());

  lat = clampLat(lat);
  lng = normalizeLng(lng);
  if (qAbs(lat) == 90.0 || qAbs(lng) == 180.0) {
    // qDebug() << "[WARN] Invalid coords:" << lat << lng;
    ++DebugCounters::warnInvalidCoords;
  }
  return QPointF(lng, lat);
}

#ifdef SANITY_CHECK
inline double haversineDistance(double lat1, double lon1, double lat2, double lon2) {
  static const double R = 6371000.0;  // earth radius in meters

  double dLat = qDegreesToRadians(lat2 - lat1);
  double dLon = qDegreesToRadians(lon2 - lon1);

  double a = qSin(dLat / 2) * qSin(dLat / 2) + qCos(qDegreesToRadians(lat1)) * qCos(qDegreesToRadians(lat2)) * qSin(dLon / 2) * qSin(dLon / 2);

  double c = 2 * qAtan2(qSqrt(a), qSqrt(1 - a));
  return R * c;  // in meters
}

inline bool isSuspiciousSegment(const QPointF &p1, const QPointF &p2, double maxDistanceMeters = 50000.0) {
  double lat1 = p1.y();
  double lon1 = p1.x();
  double lat2 = p2.y();
  double lon2 = p2.x();

  double dist = haversineDistance(lat1, lon1, lat2, lon2);
  return dist > maxDistanceMeters;
}
#endif

static inline QString debugHex(const quint8 *pData, int len) {
  if (!pData || pData == nullptr || len <= 0) {
    return QString();
  }

  QByteArray byteArray(reinterpret_cast<const char *>(pData), len);
  return byteArray.toHex(' ');
}

inline static int formatDouble(char *buffer, double value) {
  auto intPart = static_cast<int>(value);
  auto fracPart = static_cast<int>((value - intPart) * 100000 + 0.5);

  if (fracPart < 0) {
    fracPart = -fracPart;
  }

  return sprintf(buffer, "%d.%05d", intPart, fracPart);
}

inline QString roundToDigits(double value, int precision, int cut) {
  QString temp = QString::number(value, 'f', precision);
  int decimalPoint = temp.indexOf('.');
  if (decimalPoint != -1 && temp.length() > decimalPoint + cut + 1) {
    return temp.left(decimalPoint + cut + 1);
  }
  return temp;
}

static inline bool isCompletelyOutside(const QPolygonF &poly, const QRectF &viewport) {
  qreal north = qDegreesToRadians(-90.0);
  qreal south = qDegreesToRadians(90.0);
  qreal west = qDegreesToRadians(180.0);
  qreal east = qDegreesToRadians(-180.0);

  for (const QPointF &pt : poly) {
    if (north < pt.y()) {
      north = pt.y();
    }
    if (south > pt.y()) {
      south = pt.y();
    }
    if (west > pt.x()) {
      west = pt.x();
    }
    if (east < pt.x()) {
      east = pt.x();
    }
  }

  QRectF ref(west, north, east - west, south - north);
  if (ref.width() == 0) {
    ref.setWidth(0.00001);
  }
  if (ref.height() == 0) {
    ref.setHeight(0.00001);
  }

  return !viewport.intersects(ref);
}
