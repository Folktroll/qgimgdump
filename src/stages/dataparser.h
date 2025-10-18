#pragma once

#include <QFile>
#include <QObject>

#include "context.h"
#include "submap.h"

namespace App {

class DataParser {
 public:
  explicit DataParser(Ctx &ctx);
  ~DataParser() = default;

 private:
  Ctx &ctx;

  void readOldFormat(QFile &srcFile, SSubMap &submap) const;
  void readNewFormat(QFile &srcFile, SSubMap &submap) const;

  void readSubmaps(QFile &srcFile);
  void parseMapLevels(QFile &srcFile, SSubMap &submap) const;
  void parseSubdivInfo(QFile &srcFile, SSubMap &submap) const;
  void parseSubdivInfoExt(QFile &srcFile, SSubMap &submap) const;
  void parseStringTable(SSubMap &submap);
  void parseMps(QFile &srcFile, SSubMap &submap);
  void readProductInfo(QDataStream &stream);
  void readMapInfo(QDataStream &stream);
  QString readRawString(QDataStream &stream) const;
  // void readSubmapArea(SSubMap &submap);

  void processObjects(QFile &srcFile, const SSubMap &submap);
  void readObjects(QFile &srcFile);
  void decodeRgn(QFile &srcFile, const ImgHdr::SSubDiv &subdiv, StrTbl *strTbl, const QByteArray &rgndata);

  static void minno(ImgHdr::STre *hdrTre, QByteArray &data);

  double calculatePolygonArea(const QPolygonF &polygon);
  bool isPolygonTinyFast(const QPolygonF &polygon, double minArea = 1e-13);
  bool isPolygonTinyUltraFast(const QPolygonF &polygon, double minDimension = 1e-13) const;

  static inline bool isPolygonTinyOptimized(const QPolygonF &polygon, double minArea = 1e-13);

  quint8 hasExtLabelCount = 0;
  quint8 hasNet1LabelCount = 0;
  QRectF maparea;
};

}  // namespace App
