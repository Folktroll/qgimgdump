#pragma once

#include <QDebug>
#include <QFile>
#include <QObject>

#include "context.h"
#include "submap.h"

namespace App {

class DataParser {
 public:
  explicit DataParser(Ctx &ctx);
  ~DataParser() = default;

  void readSubmaps();
  void readObjects();

 private:
  Ctx &ctx;
  QFile &srcFile;

  void readOldFormat(SSubMap &subMap);
  void readNewFormat(SSubMap &subMap) const;

  void parseMapLevels(SSubMap &subMap) const;
  void parseSubDivInfo(SSubMap &subMap) const;
  void parseSubDivInfoExt(SSubMap &subMap) const;
  void parseStringTable(SSubMap &subMap);
  void parseMps(SSubMap &subMap);
  void readProductInfo(QDataStream &stream) const;
  void readMapInfo(QDataStream &stream) const;
  QString readRawString(QDataStream &stream) const;
  // void readSubmapArea(SSubMap &subMap);

  void processObjects(const SSubMap &subMap);
  void decodeObjects(const ImgHdr::SSubDiv &subdiv, const std::shared_ptr<StrTbl> &strTbl, const QByteArray &rgnData);

  static void minno(ImgHdr::STre *hdrTre, QByteArray &data);

  double calcPgArea(const QPolygonF &polygon) const;
  bool isPgTinyFast(const QPolygonF &polygon, double minArea = 1e-13) const;
  bool isPgTinyUltraFast(const QPolygonF &polygon, double minDimension = 1e-13) const;

  static inline bool isPgTinyOptimized(const QPolygonF &polygon, double minArea = 1e-13);
  bool nodeSanitizer(RgnNode &pt, const QRectF &area);
  bool pathSanitizer(RgnPath &ln, const bool &isLine, const QRectF &area);

  quint8 hasExLabelCount = 0;
  quint8 hasNet1LabelCount = 0;
  QRectF mapArea;

  template <typename T>
  bool readStruct(T &out, qint64 sizeOverride = 0) {
    const qint64 sizeToRead = sizeOverride > 0 ? sizeOverride : sizeof(T);
    std::vector<char> buffer(static_cast<size_t>(sizeToRead));

    if (const qint64 bytesRead = srcFile.read(buffer.data(), sizeToRead); bytesRead != sizeToRead) {
      return false;
    }

    std::memcpy(&out, buffer.data(), std::min<size_t>(sizeof(T), buffer.size()));
    return true;
  }

  template <typename HeaderType, typename SizeType>
  void readHeader(SSubMap &subMap, const QString &name, HeaderType &hdr) {
    if (!subMap.subBlocks.contains(name)) return;

    const auto &subBlock = subMap.subBlocks[name];
    srcFile.seek(subBlock.offset);

    SizeType size;
    if (!readStruct(size)) return;

    if (size > subBlock.size) {
      qDebug() << "[WARN] Header size is bigger than subBlock block size:" << name << Qt::hex << size << subBlock.size;
      size = (SizeType)subBlock.size;
    }

    srcFile.seek(subBlock.offset);
    if (!readStruct(hdr, size)) {
      qWarning() << "[ERROR] Failed to read" << name << "header!";
      return;
    }

    if (ctx.config.debugInfo) {
      misc::Logger::printf("   --- %s header ---\n", name.toLatin1().data());
      hdr.print(subBlock.offset);
    }
  }
};

}  // namespace App
