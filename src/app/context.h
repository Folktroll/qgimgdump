#pragma once

#include <QFile>
#include <QtCore5Compat/QTextCodec>

#include "config.h"
#include "global.h"
#include "hdr/supp.h"
#include "report.h"
#include "rgnnode.h"
#include "rgnpath.h"
#include "submap.h"
#include "warnings.h"

namespace App {

struct SIo {
  std::shared_ptr<QFile> srcFile;
  // QFile output;
};

struct SMask {
  quint8 x8;
  quint32 x32;
  quint64 x64;
};

struct SObjects {
  Nodes_t ips;
  Nodes_t pts;
  Paths_t lns;
  Paths_t pgs;
};

struct SContext {
  ImgHdr::SSupp hdrSupp;
  Config config;
  QTextCodec *codec;
  quint16 codepage;
  SIo io;
  Warnings stats;
  Report report;
  SMask mask;
  SubMaps_t subMaps;  // hold all submap descriptors or gmapsupp.img files can hold several submaps each with it's own submap parts
  QMap<quint8, SObjects> rgn;
};

// using Ctx = SContext;

}  // namespace App

using Ctx = App::SContext;