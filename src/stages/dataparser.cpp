#include "dataparser.h"

#include <QFileInfo>
#include <QThreadPool>
#include <QtConcurrent/QtConcurrentRun>
// #include <algorithm>  // std::min
// #include <cstddef>    // std::byte
// #include <cstring>    // std::memcpy
// #include <span>
// #include <vector>

#include "context.h"
#include "exception.h"
#include "inline.h"
#include "lbl.h"
#include "rgnline.h"
#include "rgnpoint.h"

using namespace App;

DataParser::DataParser(Ctx &ctx) : ctx(ctx) {}

template <typename T>
bool readStruct(QIODevice &dev, T &out, qint64 sizeOverride = 0) {
  const qint64 sizeToRead = sizeOverride > 0 ? sizeOverride : sizeof(T);
  std::vector<char> buffer(static_cast<size_t>(sizeToRead));

  if (const qint64 bytesRead = dev.read(buffer.data(), sizeToRead); bytesRead != sizeToRead) {
    return false;
  }

  std::memcpy(&out, buffer.data(), std::min<size_t>(sizeof(T), buffer.size()));
  return true;
}

template <typename HeaderType, typename SizeType>
void readHeader(QFile &srcFile, SSubMap &submap, const QString &name, HeaderType &hdr, const SContext &ctx) {
  if (!submap.subFiles.contains(name)) return;

  const auto &subfile = submap.subFiles[name];
  srcFile.seek(subfile.offset);

  SizeType size;
  if (!readStruct(srcFile, size)) return;

  if (size > subfile.size) {
    qDebug() << "[WARN] Header size is bigger than subfile block size:" << name << Qt::hex << size << subfile.size;
    size = (SizeType)subfile.size;
  }

  srcFile.seek(subfile.offset);
  if (!readStruct(srcFile, hdr, size)) {
    qWarning() << "[ERROR] Failed to read" << name << "header!";
    return;
  }

  if (ctx.config.debugInfo) {
    printf("   --- %s header ---\n", name.toLatin1().data());
    hdr.print(subfile.offset);
  }
}

void DataParser::readOldFormat(QFile &srcFile, SSubMap &submap) const {
  readHeader<decltype(submap.hdrTre), quint16>(srcFile, submap, "TRE", submap.hdrTre, ctx);
  readHeader<decltype(submap.hdrRrn), quint16>(srcFile, submap, "RGN", submap.hdrRrn, ctx);
  readHeader<decltype(submap.hdrLbl), quint16>(srcFile, submap, "LBL", submap.hdrLbl, ctx);
  readHeader<decltype(submap.hdrNet), quint16>(srcFile, submap, "NET", submap.hdrNet, ctx);
  readHeader<decltype(submap.hdrNod), quint16>(srcFile, submap, "NOD", submap.hdrNod, ctx);
  readHeader<decltype(submap.hdrDem), quint16>(srcFile, submap, "DEM", submap.hdrDem, ctx);
}

void DataParser::readNewFormat(QFile &srcFile, SSubMap &submap) const {
  const QString gmpSubfileName = "GMP";
  srcFile.seek(submap.subFiles[gmpSubfileName].offset);

  ImgHdr::SGmp hdr;
  srcFile.read((char *)&hdr, sizeof(hdr));
  QString copyright(srcFile.readLine());

  QString subfileName;
  QString prevPartName;

  const quint32 gmpOffset = submap.subFiles[gmpSubfileName].offset;
  if (ctx.config.debugInfo) {
    misc::printf("--- GMP Header %s (%08X)---\n", submap.name.toLatin1().data(), gmpOffset);
    hdr.print();
  }

  if (hdr.tre_offset) {
    subfileName = "TRE";
    srcFile.seek(gmpOffset + hdr.tre_offset);
    srcFile.read((char *)&submap.hdrTre, sizeof(submap.hdrTre));

    if (ctx.config.debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrTre.print(gmpOffset);
    }

    submap.subFiles[subfileName].hdrOffset = gmpOffset + hdr.tre_offset;
    submap.subFiles[subfileName].hdrSize = submap.hdrTre.size;
    submap.subFiles[subfileName].bodyOffset = gmpOffset + submap.hdrTre.tre3_offset;
    // submap.subFiles[prevPartName].bodySize = submap.subFiles[partName].bodyOffset - submap.subFiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.rgn_offset) {
    subfileName = "RGN";
    srcFile.seek(gmpOffset + hdr.rgn_offset);
    srcFile.read((char *)&submap.hdrRrn, sizeof(submap.hdrRrn));

    if (ctx.config.debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrRrn.print(gmpOffset);
    }

    submap.subFiles[subfileName].hdrOffset = gmpOffset + hdr.rgn_offset;
    submap.subFiles[subfileName].hdrSize = submap.hdrRrn.size;
    submap.subFiles[subfileName].bodyOffset = gmpOffset + submap.hdrRrn.rgn1_offset;
    submap.subFiles[prevPartName].bodySize = submap.subFiles[subfileName].bodyOffset - submap.subFiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.lbl_offset) {
    subfileName = "LBL";
    srcFile.seek(gmpOffset + hdr.lbl_offset);
    srcFile.read((char *)&submap.hdrLbl, sizeof(submap.hdrLbl));

    if (ctx.config.debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrLbl.print(gmpOffset);
    }

    submap.subFiles[subfileName].hdrOffset = gmpOffset + hdr.lbl_offset;
    submap.subFiles[subfileName].hdrSize = submap.hdrLbl.size;
    submap.subFiles[subfileName].bodyOffset = gmpOffset + submap.hdrLbl.lbl12_offset;
    submap.subFiles[prevPartName].bodySize = submap.subFiles[subfileName].bodyOffset - submap.subFiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.net_offset) {
    subfileName = "NET";
    srcFile.seek(gmpOffset + hdr.net_offset);
    srcFile.read((char *)&submap.hdrNet, sizeof(submap.hdrNet));

    if (ctx.config.debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrNet.print(gmpOffset);
    }

    submap.subFiles[subfileName].hdrOffset = gmpOffset + hdr.net_offset;
    submap.subFiles[subfileName].hdrSize = submap.hdrNet.size;
    submap.subFiles[subfileName].bodyOffset = gmpOffset + submap.hdrNet.net1_offset;
    submap.subFiles[prevPartName].bodySize = submap.subFiles[subfileName].bodyOffset - submap.subFiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.nod_offset) {
    subfileName = "NOD";
    srcFile.seek(gmpOffset + hdr.nod_offset);
    srcFile.read((char *)&submap.hdrNod, sizeof(submap.hdrNod));

    if (ctx.config.debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrNod.print(gmpOffset);
    }

    submap.subFiles[subfileName].hdrOffset = gmpOffset + hdr.nod_offset;
    submap.subFiles[subfileName].hdrSize = submap.hdrNod.size;
    submap.subFiles[subfileName].bodyOffset = gmpOffset + submap.hdrNod.nod1_offset;
    submap.subFiles[prevPartName].bodySize = submap.subFiles[subfileName].bodyOffset - submap.subFiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.dem_offset) {
    subfileName = "DEM";
    srcFile.seek(gmpOffset + hdr.dem_offset);
    srcFile.read((char *)&submap.hdrDem, sizeof(submap.hdrDem));

    if (ctx.config.debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrDem.print(gmpOffset);
    }

    submap.subFiles[subfileName].hdrOffset = gmpOffset + hdr.dem_offset;
    submap.subFiles[subfileName].hdrSize = submap.hdrDem.size;
    submap.subFiles[subfileName].bodyOffset = gmpOffset + submap.hdrDem.dem1_offset;
    submap.subFiles[prevPartName].bodySize = submap.subFiles[subfileName].bodyOffset - submap.subFiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  submap.subFiles[prevPartName].bodySize = submap.subFiles[gmpSubfileName].offset + submap.subFiles[gmpSubfileName].size - submap.subFiles[prevPartName].bodyOffset;
}

void DataParser::parseMapLevels(QFile &srcFile, SSubMap &submap) const {
  // read map levels from section tre1
  srcFile.seek(submap.subFiles[submap.isPseudoNt ? "GMP" : "TRE"].offset + submap.hdrTre.tre1_offset);
  QByteArray bufMapLevels = srcFile.read(submap.hdrTre.tre1_size);

  if (submap.hdrTre.flag & 0x80) {
    minno(&submap.hdrTre, bufMapLevels);
  }

  quint32 nLevels = submap.hdrTre.tre1_size / sizeof(ImgHdr::STre1);
  quint32 nSubdivs = 0;
  quint32 nSubdivsLast = 0;

  auto pMapLevel = (ImgHdr::STre1 *)bufMapLevels.data();
  for (quint32 i = 0; i < nLevels; i++) {
    nSubdivs += pMapLevel->subdiv;
    nSubdivsLast = pMapLevel->subdiv;

    if (ctx.config.debugInfo) {
      pMapLevel->print();
    }
    submap.mapLevels << *pMapLevel;
    pMapLevel++;
  }

  submap.nSubdivsNext = nSubdivs - nSubdivsLast;
  // resize number of sub-divisions
  submap.subDivs.resize(nSubdivs);
}

/*
void DataParser::readSubmapArea(SSubMap& submap) {
  submap.north = GRMN_RAD(gar_ptr_load(int24_t, submap.hdrTre.northbound));
  submap.east = GRMN_RAD(gar_ptr_load(int24_t, submap.hdrTre.eastbound));
  submap.south = GRMN_RAD(gar_ptr_load(int24_t, submap.hdrTre.southbound));
  submap.west = GRMN_RAD(gar_ptr_load(int24_t, submap.hdrTre.westbound));

  if (submap.east == submap.west) {
    submap.east = -submap.east;
  }

  if (submap.west > 0 && submap.east < 0) {
    submap.east = -submap.east;
  }

  submap.area = QRectF(QPointF(submap.west, submap.north), QPointF(submap.east, submap.south));

  if (maparea.isNull()) {
    maparea = submap.area;
  } else {
    maparea = maparea.united(submap.area);
  }
}
*/

void DataParser::parseSubdivInfo(QFile &srcFile, SSubMap &submap) const {
  srcFile.seek(submap.subFiles[submap.isPseudoNt ? "GMP" : "TRE"].offset + submap.hdrTre.tre2_offset);
  QByteArray tre2 = srcFile.read(submap.hdrTre.tre2_size);

  auto pTre2N = (ImgHdr::STre2Next *)tre2.data();

  auto subdiv = submap.subDivs.begin();
  auto subdiv_prev = submap.subDivs.end();

  int mapLevelIdx = 0;
  if (submap.mapLevels.size() == 0) {
    throw Exception("Missing map levels");
  }

  quint32 nSubdiv = submap.mapLevels[mapLevelIdx].subdiv;
  // parse all 16 byte subdivision entries
  quint32 i;
  quint32 rgnoff = submap.hdrRrn.rgn1_offset;
  for (i = 0; i < submap.nSubdivsNext; ++i, --nSubdiv) {
    qint32 cx;
    qint32 cy;
    qint32 width;
    qint32 height;

    subdiv->maplevel = &submap.mapLevels[mapLevelIdx];
    subdiv->n = i + 1;
    subdiv->next = pTre2N->next;
    subdiv->terminate = TRE_SUBDIV_TERM(pTre2N);
    subdiv->rgn_start = pTre2N->rgn_offset[0] | pTre2N->rgn_offset[1] << 8 | pTre2N->rgn_offset[2] << 16 | (pTre2N->elements & 0x0F) << 24;
    subdiv->rgn_start += rgnoff;
    // skip if this is the first entry
    if (subdiv_prev != submap.subDivs.end()) {
      subdiv_prev->rgn_end = subdiv->rgn_start;
    } else {
      subdiv->rgn_end = 0;
    }
    subdiv_prev = subdiv;

    subdiv->hasPoints = pTre2N->elements & 0x10;
    subdiv->hasPois = pTre2N->elements & 0x20;
    subdiv->hasPolylines = pTre2N->elements & 0x40;
    subdiv->hasPolygons = pTre2N->elements & 0x80;

    // if all subdivisions of this level have been parsed, switch to the next one
    if (nSubdiv == 0) {
      ++mapLevelIdx;
      subdiv->maplevel = &submap.mapLevels[mapLevelIdx];
      nSubdiv = subdiv->maplevel->subdiv;
    }

    subdiv->level = subdiv->maplevel->zoom();
    subdiv->shift = 24 - subdiv->maplevel->bits;

    cx = pTre2N->center_lng[0] | pTre2N->center_lng[1] << 8 | pTre2N->center_lng[2] << 16;
    subdiv->iCenterLng = cx;
    cy = pTre2N->center_lat[0] | pTre2N->center_lat[1] << 8 | pTre2N->center_lat[2] << 16;
    subdiv->iCenterLat = cy;
    width = TRE_SUBDIV_WIDTH(pTre2N) << subdiv->shift;
    height = pTre2N->height << subdiv->shift;

    subdiv->north = GRMN_RAD(cy + height + 1);
    subdiv->south = GRMN_RAD(cy - height);
    subdiv->east = GRMN_RAD(cx + width + 1);
    subdiv->west = GRMN_RAD(cx - width);

    subdiv->area = QRectF(QPointF(subdiv->west, subdiv->north), QPointF(subdiv->east, subdiv->south));

    subdiv_prev = subdiv;
    ++pTre2N;
    ++subdiv;
  }

  // the subdivisions of the last zoom level do not have a `next` field
  const auto nSubdivs = submap.subDivs.size();
  qDebug() << "Total subDivs:" << nSubdivs;
  ++mapLevelIdx;
  // witch pointer to 14 byte subdivision sections
  ImgHdr::STre2 *pTre2L = pTre2N;
  // parse all 14 byte subdivision entries of last map level
  for (; i < nSubdivs; ++i) {
    qint32 cx;
    qint32 cy;
    qint32 width;
    qint32 height;
    subdiv->maplevel = &submap.mapLevels[mapLevelIdx];
    subdiv->n = i + 1;
    subdiv->next = 0;
    subdiv->terminate = TRE_SUBDIV_TERM(pTre2L);
    subdiv->rgn_start = pTre2L->rgn_offset[0] | pTre2L->rgn_offset[1] << 8 | pTre2L->rgn_offset[2] << 16 | (pTre2L->elements & 0x0F) << 24;
    if (subdiv->rgn_start >= submap.hdrRrn.rgn1_length) {
      qDebug() << "[WARN] Block size overflow:" << submap.name << i << nSubdivs << Qt::hex << subdiv->rgn_start << submap.hdrRrn.rgn1_length;
      break;
    }
    subdiv->rgn_start += rgnoff;

    subdiv_prev->rgn_end = subdiv->rgn_start;

    subdiv->hasPoints = pTre2L->elements & 0x10;
    subdiv->hasPois = pTre2L->elements & 0x20;
    subdiv->hasPolylines = pTre2L->elements & 0x40;
    subdiv->hasPolygons = pTre2L->elements & 0x80;

    subdiv->level = subdiv->maplevel->zoom();
    subdiv->shift = 24 - subdiv->maplevel->bits;

    cx = pTre2L->center_lng[0] | pTre2L->center_lng[1] << 8 | pTre2L->center_lng[2] << 16;
    subdiv->iCenterLng = cx;
    cy = pTre2L->center_lat[0] | pTre2L->center_lat[1] << 8 | pTre2L->center_lat[2] << 16;
    subdiv->iCenterLat = cy;
    width = TRE_SUBDIV_WIDTH(pTre2L) << subdiv->shift;
    height = pTre2L->height << subdiv->shift;

    subdiv->north = GRMN_RAD(cy + height + 1);
    subdiv->south = GRMN_RAD(cy - height);
    subdiv->east = GRMN_RAD(cx + width + 1);
    subdiv->west = GRMN_RAD(cx - width);

    subdiv->area = QRectF(QPointF(subdiv->west, subdiv->north), QPointF(subdiv->east, subdiv->south));

    subdiv_prev = subdiv;
    ++pTre2L;
    ++subdiv;
  }

  const quint32 checkSize = pTre2L->rgn_offset[0] | pTre2L->rgn_offset[1] << 8 | pTre2L->rgn_offset[2] << 16 | (pTre2L->elements) << 24;
  if (checkSize != submap.hdrRrn.rgn1_length) {
    qDebug() << "[WARN] The block size check does not match:" << Qt::hex << checkSize << submap.hdrRrn.rgn1_length;
  }
}

// read extended type elements
void DataParser::parseSubdivInfoExt(QFile &srcFile, SSubMap &submap) const {
  const quint16 rec_size = submap.hdrTre.tre7_rec_size;
  const quint32 blockStart = submap.subFiles[submap.isPseudoNt ? "GMP" : "TRE"].offset + submap.hdrTre.tre7_offset;

  quint32 rgnOffPolyg2 = submap.hdrRrn.pg2_offset;
  quint32 rgnLenPolyg2 = submap.hdrRrn.pg2_length;
  quint32 rgnOffPolyl2 = submap.hdrRrn.ln2_offset;
  quint32 rgnLenPolyl2 = submap.hdrRrn.ln2_length;
  quint32 rgnOffPoint2 = submap.hdrRrn.pt2_offset;
  quint32 rgnLenPoint2 = submap.hdrRrn.pt2_length;

  if (submap.isPseudoNt) {
    rgnOffPolyg2 -= submap.hdrRrn.rgn2_offset;
    rgnOffPolyl2 -= submap.hdrRrn.rgn2_offset;
    rgnOffPoint2 -= submap.hdrRrn.rgn2_offset;
  }

  auto subdiv = submap.subDivs.begin();
  auto subdiv_prev = submap.subDivs.end();
  if (submap.hdrTre.size >= 0x9A && submap.hdrTre.tre7_size && rec_size >= sizeof(ImgHdr::STre7)) {
    srcFile.seek(blockStart);
    QByteArray subdiv2 = srcFile.read(submap.hdrTre.tre7_size);
    auto pSubDiv2 = (ImgHdr::STre7 *)subdiv2.data();

    bool skipPois = (rec_size != sizeof(ImgHdr::STre7));

    subdiv = submap.subDivs.begin();
    subdiv_prev = submap.subDivs.begin();
    subdiv->offPolygons2 = pSubDiv2->offsetPolygons + rgnOffPolyg2;
    subdiv->offPolylines2 = pSubDiv2->offsetPolyline + rgnOffPolyl2;
    subdiv->offPoints2 = skipPois ? 0 : pSubDiv2->offsetPoints + rgnOffPoint2;

    ++subdiv;
    pSubDiv2 = reinterpret_cast<ImgHdr::STre7 *>((quint8 *)pSubDiv2 + rec_size);

    while (subdiv != submap.subDivs.end()) {
      subdiv->offPolygons2 = pSubDiv2->offsetPolygons + rgnOffPolyg2;
      subdiv->offPolylines2 = pSubDiv2->offsetPolyline + rgnOffPolyl2;
      subdiv->offPoints2 = skipPois ? 0 : pSubDiv2->offsetPoints + rgnOffPoint2;

      subdiv_prev->lenPolygons2 = subdiv->offPolygons2 - subdiv_prev->offPolygons2;
      subdiv_prev->lenPolylines2 = subdiv->offPolylines2 - subdiv_prev->offPolylines2;
      subdiv_prev->lenPoints2 = skipPois ? 0 : subdiv->offPoints2 - subdiv_prev->offPoints2;

      subdiv_prev = subdiv;

      ++subdiv;
      pSubDiv2 = reinterpret_cast<ImgHdr::STre7 *>((quint8 *)pSubDiv2 + rec_size);
    }

    subdiv_prev->lenPolygons2 = rgnOffPolyg2 + rgnLenPolyg2 - subdiv_prev->offPolygons2;
    subdiv_prev->lenPolylines2 = rgnOffPolyl2 + rgnLenPolyl2 - subdiv_prev->offPolylines2;
    subdiv_prev->lenPoints2 = skipPois ? 0 : rgnOffPoint2 + rgnLenPoint2 - subdiv_prev->offPoints2;
  }
}

void DataParser::parseStringTable(SSubMap &submap) {
  if (!submap.subFiles.keys().contains("LBL")) {
    qDebug().noquote() << "Missing LBL subfile for submap" << submap.name;
    return;
  }

  quint32 offsetLbl1 = submap.subFiles[submap.isPseudoNt ? "GMP" : "LBL"].offset + submap.hdrLbl.lbl1_offset;
  quint32 offsetLbl6 = submap.subFiles[submap.isPseudoNt ? "GMP" : "LBL"].offset + submap.hdrLbl.lbl6_offset;

  quint16 codepage = 0;
  if (submap.hdrLbl.size > 0xAA) {
    codepage = submap.hdrLbl.codepage;
  }
  ctx.codepage = codepage;

  switch (submap.hdrLbl.coding) {
    case 0x06:  // ascii
      submap.strTbl = new StrTbl6(ctx);
      break;

    case 0x09:  // cp0, latin1, cp1251, cp1252
      submap.strTbl = new StrTblUtf8(ctx);
      break;
    case 0x0A:
    case 0x0B:  // cp65001, unicode, cp932, ms932
      qWarning() << "Not implemented LBL coding:" << Qt::hex << submap.hdrLbl.coding;
      break;

    default:
      qWarning() << "Unknown or wrong LBL coding:" << Qt::hex << submap.hdrLbl.coding;
  }

  if (nullptr != submap.strTbl) {
    submap.strTbl->registerLBL1(offsetLbl1, submap.hdrLbl.lbl1_length, submap.hdrLbl.addr_shift);
    submap.strTbl->registerLBL6(offsetLbl6, submap.hdrLbl.lbl6_length);
  }
}

void DataParser::parseMps(QFile &srcFile, SSubMap &submap) {
  if (submap.subFiles.keys().contains("MPS")) {
    MISC_PRINT("--- Map Info ---\n");
    srcFile.seek(submap.subFiles["MPS"].offset);

    QDataStream stream(&srcFile);
    stream.setByteOrder(QDataStream::LittleEndian);

    quint8 type;
    quint16 length;

    stream >> type >> length;
    while (type != 0) {
      switch (type) {
        case 0x46:
          readProductInfo(stream);
          break;

        case 0x4c:
          readMapInfo(stream);
          break;

        default:
          stream.skipRawData(length);
      }

      stream >> type >> length;
    }
  }
}

void DataParser::decodeRgn(QFile &srcFile, const ImgHdr::SSubDiv &subdiv, StrTbl *strTbl, const QByteArray &rgndata) {
  if (subdiv.rgn_start == subdiv.rgn_end && !subdiv.lenPolygons2 && !subdiv.lenPolylines2 && !subdiv.lenPoints2) {
    return;
  }

  if (subdiv.rgn_start > subdiv.rgn_end && subdiv.rgn_end != 0x00) {
    qDebug() << "[ERROR] Bug found: quint24 overflow. Compare rgn_start & rgn_end offsets:" << Qt::hex << subdiv.rgn_start << subdiv.rgn_end;
    return;
  }

  if (subdiv.offPolygons2 > rgndata.size() || subdiv.offPolylines2 > rgndata.size() || subdiv.offPoints2 > rgndata.size()) {
    qDebug() << "[ERROR] Wrong offset:" << Qt::hex << subdiv.offPolygons2 << subdiv.offPolylines2 << subdiv.offPoints2 << "| Max offset:" << rgndata.size();
    return;
  }

  const quint8 *pRawData = (quint8 *)rgndata.data();

  quint32 opnt = 0;
  quint32 opo = 0;
  quint32 oln = 0;
  quint32 opg = 0;
  quint32 objCnt = subdiv.hasPois && subdiv.hasPoints && subdiv.hasPolylines && subdiv.hasPolygons;

  auto pOffset = (quint16 *)(pRawData + subdiv.rgn_start);

  // test for points
  if (subdiv.hasPoints) {
    opnt = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
  }

  // test for pois
  if (subdiv.hasPois) {
    if (opnt) {
      opo = gar_load(uint16_t, *pOffset);
      opo += subdiv.rgn_start;
      ++pOffset;
    } else {
      opo = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
    }
  }

  // test for polylines
  if (subdiv.hasPolylines) {
    if (opnt || opo) {
      oln = gar_load(uint16_t, *pOffset);
      oln += subdiv.rgn_start;
      ++pOffset;
    } else {
      oln = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
    }
  }

  // test for polygons
  if (subdiv.hasPolygons) {
    if (opnt || opo || oln) {
      opg = gar_load(uint16_t, *pOffset);
      opg += subdiv.rgn_start;
      ++pOffset;
    } else {
      opg = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
    }
  }

#ifdef DEBUG_SHOW_POLY_DATA_SUBDIV
  qDebug() << "Subdiv" << subdiv.level << subdiv.n << file.fileName() << "| addr:" << Qt::hex << subdiv.rgn_start << "-" << subdiv.rgn_end << "|" << opnt << opoi << opline
           << opgon;
#endif

  // decode points
  if (subdiv.hasPoints) {
    const quint8 *pData = pRawData + opnt;
    const quint8 *pEnd = pRawData + (opo ? opo : oln ? oln : opg ? opg : subdiv.rgn_end);
    while (pData < pEnd) {
      RgnPoint pt;
      pData += pt.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData);

      if (!subdiv.area.contains(pt.pos)) {
        // qDebug() << "[WARN] Skip points outside subdiv area:" << subdiv.area << pt.pos;
        ++ctx.stats.warnSkipOutside;
        continue;
      }

      if (strTbl) {
        pt.isLbl6 ? strTbl->get(srcFile, pt.lbl_ptr, StrTbl::LabelType::poi, pt.labels) : strTbl->get(srcFile, pt.lbl_ptr, StrTbl::LabelType::lbl, pt.labels);
      }

      ++ctx.total.totalPt;
      ctx.rgn.points.push_back(pt);
    }
  }

  // decode pois
  if (subdiv.hasPois) {
    const quint8 *pData = pRawData + opo;
    const quint8 *pEnd = pRawData + (oln ? oln : opg ? opg : subdiv.rgn_end);
    while (pData < pEnd) {
      RgnPoint po;
      pData += po.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData);

      if (!subdiv.area.contains(po.pos)) {
        // qDebug() << "[WARN] Skip pois outside subdiv area:" << subdiv.area << po.pos;
        ++ctx.stats.warnSkipOutside;
        continue;
      }

      if (strTbl) {
        po.isLbl6 ? strTbl->get(srcFile, po.lbl_ptr, StrTbl::LabelType::poi, po.labels) : strTbl->get(srcFile, po.lbl_ptr, StrTbl::LabelType::lbl, po.labels);
      }

      ++ctx.total.totalPo;
      ctx.rgn.pois.push_back(po);
    }
  }

  // decode polylines
  if (subdiv.hasPolylines) {
    const quint8 *pData = pRawData + oln;
    const quint8 *pEnd = pRawData + (opg ? opg : subdiv.rgn_end);
    while (pData < pEnd) {
      RgnLine ln;
      pData += ln.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, true, pData, pEnd);

      if (isCompletelyOutside(ln.points, subdiv.area)) {
        // qDebug() << "[WARN] Skip polylines outside subdiv area:" << subdiv.area << ln.points.toList().first(10);
        ++ctx.stats.warnSkipOutside;
        continue;
      }

      if (strTbl && ln.lbl_info) {
        if (ln.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[ln] 1 hasNet1Label" << Qt::hex << ln.lbl_info;
          strTbl->get(srcFile, ln.lbl_info, StrTbl::LabelType::net, ln.labels);
        } else {
          strTbl->get(srcFile, ln.lbl_info, StrTbl::LabelType::lbl, ln.labels);
        }
      } else {
        ;
      }
      hasExtLabelCount += ln.hasExtLabelCount;

      ++ctx.total.totalLn;
      ctx.rgn.polylines.push_back(ln);
    }
  }

  // decode polygons
  if (subdiv.hasPolygons) {
    const quint8 *pData = pRawData + opg;
    const quint8 *pEnd = pRawData + subdiv.rgn_end;
    while (pData < pEnd) {
      RgnLine pg;
      pData += pg.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, false, pData, pEnd);

      if (isCompletelyOutside(pg.points, subdiv.area)) {
        // qDebug() << "[WARN] Skip polygons outside subdiv area:" << subdiv.area << pg.points.toList().first(10);
        ++ctx.stats.warnSkipOutside;
        continue;
      }

      if (isPolygonTinyFast(pg.points)) {
        qDebug() << "[WARN] Polygon is too small 2, area:" << pg.points;
        continue;
      }

      if (strTbl && pg.lbl_info) {
        if (pg.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[pg] 1 hasNet1Label" << Qt::hex << pg.lbl_info;
          strTbl->get(srcFile, pg.lbl_info, StrTbl::LabelType::net, pg.labels);
        } else {
          strTbl->get(srcFile, pg.lbl_info, StrTbl::LabelType::lbl, pg.labels);
        }
      } else {
        ;
      }
      hasExtLabelCount += pg.hasExtLabelCount;

      ++ctx.total.totalPg;
      ctx.rgn.polygons.push_back(pg);
    }
  }

  // extended type
  if (subdiv.lenPolygons2) {
    // qDebug() << "Exteneded type: polygon" << Qt::hex << subdiv.offsetPolygons2 << subdiv.lengthPolygons2;
    const quint8 *pData = pRawData + subdiv.offPolygons2;
    const quint8 *pEnd = pData + subdiv.lenPolygons2;
    while (pData < pEnd) {
      RgnLine pg;
      // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
      pData += pg.decodeExt(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, false, pData, pEnd);

      if (isCompletelyOutside(pg.points, subdiv.area)) {
        // qDebug() << "[WARN] Skip polygons outside subdiv area:" << subdiv.area << pg.points.toList().first(10);
        ++ctx.stats.warnSkipOutside;
        continue;
      }

      if (isPolygonTinyFast(pg.points)) {
        qDebug() << "[WARN] Polygon is too small 1, area:" << pg.points;
        continue;
      }

      if (strTbl && pg.lbl_info) {
        if (pg.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[pg] 2 hasNet1Label" << Qt::hex << pg.lbl_info;
          strTbl->get(srcFile, pg.lbl_info, StrTbl::LabelType::net, pg.labels);
        } else {
          strTbl->get(srcFile, pg.lbl_info, StrTbl::LabelType::lbl, pg.labels);
        }
      } else {
        ;
      }
      hasExtLabelCount += pg.hasExtLabelCount;

      ++ctx.total.totalPg2;
      ctx.rgn.polygons.push_back(pg);
    }
  }

  if (subdiv.lenPolylines2) {
    // qDebug() << "Exteneded type: polyline" << Qt::hex << subdiv.offsetPolylines2 << subdiv.lengthPolylines2;
    const quint8 *pData = pRawData + subdiv.offPolylines2;
    const quint8 *pEnd = pData + subdiv.lenPolylines2;
    while (pData < pEnd) {
      RgnLine ln;
      // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
      pData += ln.decodeExt(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, true, pData, pEnd);

      if (isCompletelyOutside(ln.points, subdiv.area)) {
        // qDebug() << "[WARN] Skip polylines outside subdiv area:" << subdiv.area << ln.points.toList().first(10);
        ++ctx.stats.warnSkipOutside;
        continue;
      }

      if (strTbl && ln.lbl_info) {
        if (ln.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[ln] 2 hasNet1Label" << Qt::hex << ln.lbl_info;
          strTbl->get(srcFile, ln.lbl_info, StrTbl::LabelType::net, ln.labels);
        } else {
          strTbl->get(srcFile, ln.lbl_info, StrTbl::LabelType::lbl, ln.labels);
        }
      } else {
        ;
      }
      hasExtLabelCount += ln.hasExtLabelCount;

      ++ctx.total.totalLn2;
      ctx.rgn.polylines.push_back(ln);
    }
  }

  if (subdiv.lenPoints2) {
    // qDebug() << "Exteneded type: point" << Qt::hex << subdiv.offsetPoints2 << subdiv.lengthPoints2;
    const quint8 *pData = pRawData + subdiv.offPoints2;
    const quint8 *pEnd = pData + subdiv.lenPoints2;
    while (pData < pEnd) {
      RgnPoint pt;
      pData += pt.decodeExt(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData, pEnd);

      if (!subdiv.area.contains(pt.pos)) {
        // qDebug() << "[WARN] Skip points outside subdiv area:" << subdiv.area << pt.pos;
        ++ctx.stats.warnSkipOutside;
        continue;
      }

      if (strTbl) {
        pt.isLbl6 ? strTbl->get(srcFile, pt.lbl_ptr, StrTbl::LabelType::poi, pt.labels) : strTbl->get(srcFile, pt.lbl_ptr, StrTbl::LabelType::lbl, pt.labels);
      }

      ++ctx.total.totalPt2;  // or totalPo2?
      ctx.rgn.pois.push_back(pt);
    }
  }
}

double DataParser::calculatePolygonArea(const QPolygonF &polygon) {
  const auto n = polygon.size();
  if (n < 3) return 0.0;

  double area = 0.0;

  // Оптимизация: директен достъп до точки и минимум операции
  const QPointF *points = polygon.constData();
  const QPointF &first = points[0];

  for (int i = 1; i < n - 1; ++i) {
    const QPointF &p1 = points[i];
    const QPointF &p2 = points[i + 1];
    area += (p1.x() - first.x()) * (p2.y() - first.y()) - (p2.x() - first.x()) * (p1.y() - first.y());
  }

  return std::abs(area) * 0.5;
}

bool DataParser::isPolygonTinyFast(const QPolygonF &polygon, double minArea) {
  if (const auto n = polygon.size(); n < 3) return true;

  // Бърза проверка на bounding rect първо (най-евтина)
  const QRectF bounds = polygon.boundingRect();
  const double width = bounds.width();
  const double height = bounds.height();

  // Ако bounding rect е твърде малък, няма нужда да изчисляваме площ
  if (width * height < minArea * 0.5) {  // 0.5 за по-ранно отхвърляне
    return true;
  }

  // Само ако bounding rect е OK, изчисляваме точна площ
  const double area = calculatePolygonArea(polygon);
  return area < minArea;
}

// Алтернатива: още по-бърза версия само с bounding box
bool DataParser::isPolygonTinyUltraFast(const QPolygonF &polygon, double minDimension) const {
  if (polygon.size() < 3) return true;

  const QRectF bounds = polygon.boundingRect();
  return (bounds.width() < minDimension || bounds.height() < minDimension);
}

QString DataParser::readRawString(QDataStream &stream) const {
  QByteArray label;
  quint8 tmp;
  stream >> tmp;
  while (tmp != 0) {
    label.append(tmp);
    stream >> tmp;
  }
  return QString::fromUtf8(label);
}

void DataParser::readProductInfo(QDataStream &stream) {
  quint16 idProd;
  quint16 idFamily;
  stream >> idProd >> idFamily;
  misc::printf("Product Info: %i %i %s\n", idProd, idFamily, readRawString(stream).toUtf8().data());
}

void DataParser::readMapInfo(QDataStream &stream) {
  quint16 idProd;
  quint16 idFamily;
  quint32 mapNumber;

  stream >> idProd >> idFamily >> mapNumber;
  const QString &seriesName = readRawString(stream);
  const QString &mapDesc = readRawString(stream);
  const QString &areaName = readRawString(stream);
  quint32 mapId;
  quint32 dummy;

  stream >> mapId >> dummy;

  misc::printf("Map Info: %i %i %i %s %s %s %08X %i \n", idProd, idFamily, mapNumber, seriesName.toLocal8Bit().data(), mapDesc.toLocal8Bit().data(), areaName.toLocal8Bit().data(),
               mapId, mapId);
}

void DataParser::readSubmaps(QFile &srcFile) {
  maparea = QRectF();
  qDebug() << "Submaps count:" << ctx.submaps.size();

  if (ctx.config.debugInfo) {
    MISC_PRINT("--- Subfiles ---\n");
    for (auto &submap : ctx.submaps) {
      for (const auto &[subfile, part] : submap.subFiles.asKeyValueRange()) {
        misc::printf("%s %s %08X %08X %08X %08X %08X %08X\n", submap.name.toLocal8Bit().data(), subfile.toLocal8Bit().data(), part.offset, part.size, part.hdrOffset, part.hdrSize,
                     part.bodyOffset, part.bodySize);
      }
    }
  }

  for (auto &submap : ctx.submaps) {
    if (submap.isPseudoNt) {
      readNewFormat(srcFile, submap);
    } else {
      readOldFormat(srcFile, submap);
    }
    if (!(submap.subFiles.contains("TRE") && submap.subFiles.contains("RGN"))) {
      // missing mandatory submap subFiles
      continue;
    }
    parseMapLevels(srcFile, submap);
    parseSubdivInfo(srcFile, submap);
    parseSubdivInfoExt(srcFile, submap);
    parseStringTable(submap);
    parseMps(srcFile, submap);
  }

  fflush(stdout);
  fflush(stderr);
}

void DataParser::readObjects(QFile &srcFile) {
  // int numThreads = 4;

  int numThreads = 1;
  if (numThreads > 1) {
    QThreadPool::globalInstance()->setMaxThreadCount(numThreads);
  }

  uint8_t filePart = 0;
  for (const auto &submap : ctx.submaps) {
    qDebug() << "Submap name:" << submap.name << "| Total subDivs:" << submap.subDivs.size();
    if (submap.subDivs.isEmpty()) {
      qDebug() << "Skip RGN decode for this submap (no subDivs)" << submap.name;
      continue;
    }

    QFileInfo ofInfo(ctx.config.outputFile);
    QString dir = ofInfo.path();
    if (numThreads == 1) {
      processObjects(srcFile, submap);
    } else {
      QThreadPool::globalInstance()->start([this, &srcFile, submap]() { processObjects(srcFile, submap); });
    }
#ifdef SANITY_CHECK
    if (ctx.config.debugInfo) {
      if (hasExtLabelCount) {
        // qDebug() << "[INFO] Number of hasExtLabel:" << hasExtLabelCount;
        hasExtLabelCount = 0;
      }
      if (hasNet1LabelCount) {
        // qDebug() << "[INFO] Number of hasNet1Label:" << hasNet1LabelCount;
        hasNet1LabelCount = 0;
      }
      if (ctx.stats.infoSkipDupePoint) {
        qDebug() << "[INFO] Number of skipped duplicate points:" << ctx.stats.infoSkipDupePoint;
        ctx.stats.infoSkipDupePoint = 0;
      }
      if (ctx.stats.warnSkipOutside) {
        qDebug() << "[WARN] Number of objects outside subdiv area:" << ctx.stats.warnSkipOutside;
        ctx.stats.warnSkipOutside = 0;
      }
      if (ctx.stats.warnSuspiciousSegment) {
        qDebug() << "[WARN] Number of suspicious segment between points:" << ctx.stats.warnSuspiciousSegment;
        ctx.stats.warnSuspiciousSegment = 0;
      }
      if (ctx.stats.warnTotals) {
        qDebug() << "[WARN] Number of more then 50 errors in single polyline:" << ctx.stats.warnTotals;
        ctx.stats.warnTotals = 0;
      }
      if (ctx.stats.warnPolyOversize) {
        qDebug() << "[WARN] Number of too long polyline (possibly bitstream error):" << ctx.stats.warnPolyOversize;
        ctx.stats.warnPolyOversize = 0;
      }
      if (ctx.stats.warnInvalidType) {
        qDebug() << "[WARN] Number of invalid objects type:" << ctx.stats.warnInvalidType;
        ctx.stats.warnInvalidType = 0;
      }
      if (ctx.stats.warnInvalidCoords) {
        qDebug() << "[WARN] Number of invalid coordinates:" << ctx.stats.warnInvalidCoords;
        ctx.stats.warnInvalidCoords = 0;
      }
    } else {
      hasExtLabelCount = 0;
      hasNet1LabelCount = 0;
      ctx.stats.reset();
      ctx.stats.warnInvalidCoords = 0;
    }
#endif
  }
  QThreadPool::globalInstance()->waitForDone();
}

void DataParser::processObjects(QFile &srcFile, const SSubMap &submap) {
  try {
    QByteArray rgnData;
    if (submap.isPseudoNt) {
      srcFile.seek(submap.subFiles["RGN"].hdrOffset);
      rgnData = srcFile.read(submap.subFiles["RGN"].hdrSize);
      srcFile.seek(submap.subFiles["RGN"].bodyOffset);
      rgnData += srcFile.read(submap.subFiles["RGN"].bodySize);
      auto totalSize = submap.subFiles["RGN"].hdrSize + submap.subFiles["RGN"].bodySize;
    } else {
      srcFile.seek(submap.subFiles["RGN"].offset);
      rgnData = srcFile.read(submap.subFiles["RGN"].size);
    }

    if (rgnData.isEmpty()) {
      qDebug() << "[WARN] No RGN data";
      return;
    }

    for (const ImgHdr::SSubDiv &subdiv : submap.subDivs) {
      // if (ctx.config.debugInfo) { subdiv.print(); }
      if (ctx.stats.warnInvalidCoords + ctx.stats.warnInvalidType + ctx.stats.warnSuspiciousSegment + ctx.stats.warnSkipOutside + ctx.stats.infoSkipDupePoint +
              ctx.stats.warnTotals + ctx.stats.warnPolyOversize >
          500) {
        qDebug() << "[ERROR] Too many errors: wrong offsets or unknown format with extended headers.";
        break;
      }
      decodeRgn(srcFile, subdiv, submap.strTbl, rgnData);
    }
    ctx.total.objectsDecoded();
    qDebug().noquote() << QString("Total decoded objects: %1 | RGN: %2 %3 %4 %5 %6 %7 %8 %9 | Objects: %10 %11 %12")
                              .arg(ctx.total.totalObjectsDecoded, -8)
                              .arg(ctx.total.totalPt, -8)
                              .arg(ctx.total.totalPo, -8)
                              .arg(ctx.total.totalLn, -8)
                              .arg(ctx.total.totalPg, -8)
                              .arg(ctx.total.totalPt2, -8)
                              .arg(ctx.total.totalPo2, -8)
                              .arg(ctx.total.totalLn2, -8)
                              .arg(ctx.total.totalPg2, -8)
                              .arg(ctx.total.totalPt + ctx.total.totalPo + ctx.total.totalPt2 + ctx.total.totalPo2, -8)
                              .arg(ctx.total.totalLn + ctx.total.totalLn2, -8)
                              .arg(ctx.total.totalPg + ctx.total.totalPg2, -8);
    ctx.total.resetDecoded();
  } catch (const Exception &e) {
    qDebug() << "Fatal error:" << e.msg;
  }
}

// file contains locked/encrypted data
void DataParser::minno(ImgHdr::STre *hdrTre, QByteArray &data) {
  if (hdrTre->flag & 0x80) {
    auto nlevels = hdrTre->tre1_size / sizeof(ImgHdr::STre1);

    unsigned int key[5];
    std::vector<std::byte> tbl(static_cast<size_t>(hdrTre->tre1_size));
    std::memcpy(tbl.data(), data.data(), hdrTre->tre1_size);

    key[0] = ((std::to_integer<unsigned int>(tbl[0]) >> 4) + 8) & 0x0F;
    key[1] = ((std::to_integer<unsigned int>(tbl[3]) >> 4) + 16) & 0x0F;
    key[2] = ((std::to_integer<unsigned int>(tbl[3]) & 0x0F) + 16) & 0x0F;
    key[3] = (std::to_integer<unsigned int>(tbl[4]) >> 4) & 0x7;
    if (nlevels > 2) {
      key[3] ^= (((std::to_integer<unsigned int>(tbl[9]) >> 4) + 16 - key[3]) & 0x08);
    }
    key[4] = ((std::to_integer<unsigned int>(tbl[2]) >> 4) + 16 - 0) & 15;

    for (quint32 i = 0; i < nlevels * 4; i++) {
      unsigned int v = std::to_integer<unsigned int>(tbl[i]);
      unsigned int hi = (((v >> 4) + 16 - key[(i * 2) % 5]) & 15) << 4;
      unsigned int lo = (((v & 15) + 16 - key[(i * 2 + 1) % 5]) & 15);
      tbl[i] = static_cast<std::byte>(hi + lo);
    }

    auto ml = reinterpret_cast<ImgHdr::STre1 *>(tbl.data());
    for (quint32 i = 0; i < nlevels; i++) {
      ++ml;
    }

    std::memcpy(data.data(), tbl.data(), hdrTre->tre1_size);
    hdrTre->flag &= ~0x80;
  }
}

bool DataParser::isPolygonTinyOptimized(const QPolygonF &polygon, double minArea) {
  const auto n = polygon.size();
  if (n < 3) return true;

  // Бърза bounding box проверка
  const QRectF bounds = polygon.boundingRect();
  if (const double bboxArea = bounds.width() * bounds.height(); bboxArea < minArea * 0.3) {
    return true;
  }

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
