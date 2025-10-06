#pragma once

#include <QCoreApplication>
#include <QFile>
#include <QMutex>
#include <QPointF>
#include <QPolygonF>
#include <QRectF>
#include <QTextCodec>

#include "lbl.h"
#include "rgnline.h"
#include "rgnpoint.h"
#include "struct.h"

class ImgDump : public QCoreApplication {
 public:
  explicit ImgDump(int &argc, char **argv);
  ~ImgDump() final;

 private:
  using polytype_t = QVector<RgnLine>;
  using pointtype_t = QVector<RgnPoint>;

  // submap subfiles location info
  struct submap_subfile_t {
    quint32 offset = 0;      // file offset of submap part (old format)
    quint32 size = 0;        // size of the submap part (old format)
    quint32 hdrOffset = 0;   // file offset of header part (new format)
    quint32 hdrSize = 0;     // size of the header part (new format)
    quint32 bodyOffset = 0;  // file offset of body part (new format)
    quint32 bodySize = 0;    // size of the body part (new format)
    // submap_subfile_t() : offset(0), size(0), hdrOffset(0), hdrSize(0), bodyOffset(0), bodySize(0) {}
  };

  struct submap_t {
    QString name;                              // the name of the submap
    QMap<QString, submap_subfile_t> subfiles;  // location information of all subfiles
    double north = 0.0;                        // north boundary of area covered by this submap [rad]
    double east = 0.0;                         // east  boundary of area covered by this submap [rad]
    double south = 0.0;                        // south boundary of area covered by this submap [rad]
    double west = 0.0;                         // west  boundary of area covered by this submap [rad]
    QRectF area;                               // area in [] covered by this submap
    bool isTransparent = false;                // bit 1 of POI_flags (TRE header @ 0x3F)
    IMG::hdr_tre_t hdrTRE;
    IMG::hdr_rgn_t hdrRGN;
    IMG::hdr_lbl_t hdrLBL;
    IMG::hdr_net_t hdrNET;
    IMG::hdr_nod_t hdrNOD;
    IMG::hdr_dem_t hdrDEM;
    QVector<IMG::tre1_t> mapLevels;  // used maplevels
    QVector<IMG::subdiv_t> subdivs;  // list of subdivisions
    quint32 nSubdivsNext = 0;
    StrTbl *strtbl;  // object to manage the string table
    bool isPseudoNT = false;
  };

#define TRE_MAP_LEVEL(r) ((r)->zoom & 0x0f)
#define TRE_MAP_INHER(r) (((r)->zoom & 0x80) != 0)

  void print(const char *format, ...);
  void readFat(QFile &srcFile);
  void readOldFormat(QFile &srcFile, submap_t &submasp);
  void readNewFormat(QFile &srcFile, submap_t &submap);
  void readSubmaps(QFile &srcFile);
  // void readSubmapArea(submap_t& submap);
  void parseMapLevels(QFile &srcFile, submap_t &submap);
  IMG::tre0_t readCopyrights(QFile &srcFile, quint32 baseOffset, quint32 limitOffset);
  void parseSubdivInfo(QFile &srcFile, submap_t &submap);
  void parseSubdivInfoExt(QFile &srcFile, submap_t &submap);
  void parseStringTable(QFile &srcFile, submap_t &submap);
  void readProductInfo(QDataStream &stream);
  void readMapInfo(QDataStream &stream);
  QString readRawString(QDataStream &stream);
  void processObjects(QFile &dstFile, QFile &srcFile, const submap_t &submap);
  void readObjects(QFile &srcFile);
  double calculatePolygonArea(const QPolygonF &polygon);
  bool isPolygonTinyFast(const QPolygonF &polygon, double minArea = 1e-13);
  bool isPolygonTinyUltraFast(const QPolygonF &polygon, double minDimension = 1e-13);
  static inline bool isPolygonTinyOptimized(const QPolygonF &polygon, double minArea = 1e-13) {
    const int n = polygon.size();
    if (n < 3) return true;

    // Бърза bounding box проверка
    const QRectF bounds = polygon.boundingRect();
    const double bboxArea = bounds.width() * bounds.height();
    if (bboxArea < minArea * 0.3) return true;

    // Бързо изчисление на площ без излишни операции
    if (n == 3) {
      // Оптимизация за триъгълници
      const QPointF &p1 = polygon[0];
      const QPointF &p2 = polygon[1];
      const QPointF &p3 = polygon[2];
      const double area = std::abs((p2.x() - p1.x()) * (p3.y() - p1.y()) - (p3.x() - p1.x()) * (p2.y() - p1.y())) * 0.5;
      return area < minArea;
    }

    // Общ случай
    double area = 0.0;
    const QPointF *points = polygon.constData();

    for (int i = 0; i < n; ++i) {
      const int j = (i + 1) % n;
      area += points[i].x() * points[j].y() - points[j].x() * points[i].y();
    }

    return std::abs(area) * 0.5 < minArea;
  }
  void decodeRgn(QFile &dstFile, QFile &srcFile, const IMG::subdiv_t &subdiv, StrTbl *strtbl, const QByteArray &rgndata);
  void writeCsv(QFile &dstFile, pointtype_t &points, pointtype_t &pois, polytype_t &polylines, polytype_t &polygons, quint32 level);
  void writeMp(QFile &dstFile, pointtype_t &points, pointtype_t &pois, polytype_t &polylines, polytype_t &polygons, quint32 level);
  QString convPtDegStr(const QPointF &point, bool wkt = false);
  QString convLnDegStr(const QPolygonF &polyline, bool isLine, bool wkt = false);
  void writeHeader(QFile &dstFile, const submap_t &submap);

  // file contains locked/encrypted data
  static void minno(IMG::hdr_tre_t *trehdr, QByteArray &data) {
    if (trehdr->flag & 0x80) {
      quint32 nlevels = trehdr->tre1_size / sizeof(IMG::tre1_t);

      quint8 key[5];
      quint8 *tbl = new quint8[trehdr->tre1_size];
      memcpy(tbl, data.data(), trehdr->tre1_size);

      key[0] = (((tbl[0] >> 4) + 8) & 0x0F);
      key[1] = (((tbl[3] >> 4) + 16) & 0x0F);
      key[2] = (((tbl[3] & 0x0F) + 16) & 0x0F);
      key[3] = ((tbl[4] >> 4) & 0x7);
      if (nlevels > 2) {
        key[3] ^= (((tbl[9] >> 4) + 16 - key[3]) & 0x08);
      }
      key[4] = (((tbl[2] >> 4) + 16 - 0) & 15);

      for (quint32 i = 0; i < nlevels * 4; i++) {
        tbl[i] = (((((tbl[i] >> 4) + 16 - key[(i * 2) % 5]) & 15) << 4) + ((((tbl[i] & 15) + 16 - key[(i * 2 + 1) % 5]) & 15)));
      }

      IMG::tre1_t *ml = (IMG::tre1_t *)tbl;
      for (quint32 i = 0; i < nlevels; i++) {
        ++ml;
      }

      memcpy(data.data(), tbl, trehdr->tre1_size);
      trehdr->flag &= ~0x80;

      delete[] tbl;
    }
  }

  QString inputFile;
  QString outputFile;
  QTextCodec *codec;
  quint8 mask;
  quint32 mask32;
  quint64 mask64;
  QRectF maparea;
  QString codepageStr = "";
  QString codingStr = "";
  QString nameStr;
  QString copyrightsStr = "";
  QMutex mutex;
  QHash<QString, submap_t> submaps;  // hold all submap descriptors or gmapsupp.img files can hold several submaps each with it's own submap parts
  bool splitSubmaps = false;
  bool csvOutput = false;
  bool debugInfo = false;
  float maxFileSize = 1 * 1000 * 1024 * 1024;
  int totalPt = 0;
  int totalPo = 0;
  int totalLn = 0;
  int totalPg = 0;
  int totalPt2 = 0;
  int totalPo2 = 0;
  int totalLn2 = 0;
  int totalPg2 = 0;
  int totalObjectsDecoded = 0;
  int totalPtFailed = 0;
  int totalPoFailed = 0;
  int totalLnFailed = 0;
  int totalPgFailed = 0;
  int totalPt2Failed = 0;
  int totalPo2Failed = 0;
  int totalLn2Failed = 0;
  int totalPg2Failed = 0;
  quint8 hasExtLabelCount = 0;
  quint8 hasNet1LabelCount = 0;
  quint8 infoSkipDupePoint = 0;
  quint8 warnTotals = 0;
  quint8 warnSkipOutside = 0;
  quint8 warnSuspiciousSegment = 0;
  quint8 warnPolyOversize = 0;
  quint8 warnInvalidType = 0;
  QElapsedTimer totalTimer;
  QElapsedTimer timer;
  quint64 method1Time = 0;
  quint64 method2Time = 0;
};
