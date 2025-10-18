#pragma once

#include <QHash>
#include <QMap>
#include <QRectF>
#include <QString>
#include <QVector>

// #include "lbl.h"
#include "structs/img_headers.h"
#include "structs/subdiv.h"
#include "structs/subfile_sections.h"
#include "subfile.h"

namespace App {

class StrTbl;

struct SSubMap {
  QString name;                      // the name of the submap
  QMap<QString, SSubFile> subFiles;  // location information of all subFiles

  double north = 0.0;  // north boundary of area covered by this submap [rad]
  double east = 0.0;   // east  boundary of area covered by this submap [rad]
  double south = 0.0;  // south boundary of area covered by this submap [rad]
  double west = 0.0;   // west  boundary of area covered by this submap [rad]
  QRectF area;         // area in [] covered by this submap

  bool isTransparent = false;  // bit 1 of POI_flags (TRE header @ 0x3F)
  bool isPseudoNt = false;

  ImgHdr::STre hdrTre;
  ImgHdr::SRgn hdrRrn;
  ImgHdr::SLbl hdrLbl;
  ImgHdr::SNet hdrNet;
  ImgHdr::SNod hdrNod;
  ImgHdr::SDem hdrDem;

  QVector<ImgHdr::STre1> mapLevels;  // used maplevels
  QVector<ImgHdr::SSubDiv> subDivs;  // list of subdivisions
  quint32 nSubdivsNext = 0;

  StrTbl *strTbl;  // object to manage the string table
};

using SubMaps_t = QHash<QString, SSubMap>;

}  // namespace App
