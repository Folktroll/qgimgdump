#pragma once

#include <QFile>
#include <QtCore5Compat/QTextCodec>

#include "config.h"
#include "global.h"
#include "rgnline.h"
#include "rgnpoint.h"
#include "submap.h"
#include "total.h"
#include "warnings.h"

namespace App {

struct SIo {
  QFile srcFile;
  // QFile output;
};

struct SMask {
  quint8 x8;
  quint32 x32;
  quint64 x64;
};

struct SObjects {
  Line_t polygons;
  Line_t polylines;
  Point_t points;
  Point_t pois;
};

struct SContext {
  QString nameStr;
  Config config;
  QTextCodec *codec;
  quint16 codepage;
  SIo io;
  Warnings stats;
  Total total;
  SMask mask;
  SubMaps_t submaps;  // hold all submap descriptors or gmapsupp.img files can hold several submaps each with it's own submap parts
  SObjects rgn;
};

// using Ctx = SContext;

}  // namespace App

using Ctx = App::SContext;