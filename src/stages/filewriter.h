#pragma once

#include <QFile>

#include "context.h"
#include "rgnline.h"
#include "rgnpoint.h"
#include "structs/subfile_sections.h"
#include "submap.h"

namespace App {

class FileWriter {
 public:
  explicit FileWriter(Ctx &ctx);
  // ~FileWriter() final;

 private:
  Ctx &ctx;

  QString convPtDegStr(const QPointF &point, bool wkt = false) const;
  QString convLnDegStr(const QPolygonF &polyline, bool isLine, bool wkt = false);

  void writeHeader(QFile &dstFile, const SSubMap &submap);
  void writeCsv(QFile &dstFile, quint32 level);
  void writeMp(QFile &dstFile, quint32 level);

  void processObjects(QFile &dstFile, const SSubMap &submap);
  void exportObjects(QFile &srcFile);

  ImgHdr::STre0 readCopyrights(QFile &srcFile, quint32 baseOffset, quint32 limitOffset) const;
};

}  // namespace App
