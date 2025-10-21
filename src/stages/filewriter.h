#pragma once

#include <QFile>

#include "context.h"
#include "hdr/section.h"
#include "rgnnode.h"
#include "rgnpath.h"
#include "submap.h"

namespace App {

class FileWriter {
 public:
  explicit FileWriter(Ctx &ctx);
  // ~FileWriter() final;

  void exportObjects();

 private:
  Ctx &ctx;
  QFile &srcFile;

  QString convPtDegStr(const QPointF &pointF, const bool &wkt) const;
  QString convLnDegStr(const QPolygonF &polygonF, const bool &wkt) const;

  void writeHeader(QFile &dstFile, const SSubMap &subMap) const;
  void writeCsv(QFile &dstFile);
  void writeMp(QFile &dstFile);

  ImgHdr::STre0 readCopyrights(quint32 baseOffset, quint32 limitOffset) const;
};

}  // namespace App
