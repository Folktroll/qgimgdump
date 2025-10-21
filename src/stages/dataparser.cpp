#include "dataparser.h"

#include <QFileInfo>
#include <QThreadPool>
#include <QtConcurrent/QtConcurrentRun>

#include "context.h"
#include "exception.h"
#include "inline.h"
#include "lbl.h"
#include "rgnnode.h"
#include "rgnpath.h"

using namespace App;

DataParser::DataParser(Ctx &ctx) : ctx(ctx), srcFile(*ctx.io.srcFile) {}

void DataParser::readOldFormat(SSubMap &subMap) {
  readHeader<decltype(subMap.hdrTre), quint16>(subMap, "TRE", subMap.hdrTre);
  readHeader<decltype(subMap.hdrRgn), quint16>(subMap, "RGN", subMap.hdrRgn);
  readHeader<decltype(subMap.hdrLbl), quint16>(subMap, "LBL", subMap.hdrLbl);
  readHeader<decltype(subMap.hdrNet), quint16>(subMap, "NET", subMap.hdrNet);
  readHeader<decltype(subMap.hdrNod), quint16>(subMap, "NOD", subMap.hdrNod);
  readHeader<decltype(subMap.hdrDem), quint16>(subMap, "DEM", subMap.hdrDem);
}

void DataParser::readNewFormat(SSubMap &subMap) const {
  const QString gmpBlockName = "GMP";
  srcFile.seek(subMap.subBlocks[gmpBlockName].offset);

  ImgHdr::SGmp hdr;
  srcFile.read((char *)&hdr, sizeof(hdr));
  QString copyright(srcFile.readLine());

  QString subBlockName;
  QString prevPartName;

  const quint32 gmpOffset = subMap.subBlocks[gmpBlockName].offset;
  if (ctx.config.debugInfo) {
    misc::Logger::printf("--- GMP Header %s (%08X)---\n", subMap.name.toLatin1().data(), gmpOffset);
    hdr.print();
  }

  if (hdr.tre_offset) {
    subBlockName = "TRE";
    srcFile.seek(gmpOffset + hdr.tre_offset);
    srcFile.read((char *)&subMap.hdrTre, sizeof(subMap.hdrTre));

    if (ctx.config.debugInfo) {
      misc::Logger::printf("   --- %s header ---\n", subBlockName.toLatin1().data());
      subMap.hdrTre.print(gmpOffset);
    }

    subMap.subBlocks[subBlockName].hdrOffset = gmpOffset + hdr.tre_offset;
    subMap.subBlocks[subBlockName].hdrSize = subMap.hdrTre.size;
    subMap.subBlocks[subBlockName].bodyOffset = gmpOffset + subMap.hdrTre.tre3_offset;
    // subMap.subBlocks[prevPartName].bodySize = subMap.subBlocks[partName].bodyOffset - subMap.subBlocks[prevPartName].bodyOffset;
    prevPartName = subBlockName;
  }

  if (hdr.rgn_offset) {
    subBlockName = "RGN";
    srcFile.seek(gmpOffset + hdr.rgn_offset);
    srcFile.read((char *)&subMap.hdrRgn, sizeof(subMap.hdrRgn));

    if (ctx.config.debugInfo) {
      misc::Logger::printf("   --- %s header ---\n", subBlockName.toLatin1().data());
      subMap.hdrRgn.print(gmpOffset);
    }

    subMap.subBlocks[subBlockName].hdrOffset = gmpOffset + hdr.rgn_offset;
    subMap.subBlocks[subBlockName].hdrSize = subMap.hdrRgn.size;
    subMap.subBlocks[subBlockName].bodyOffset = gmpOffset + subMap.hdrRgn.rgn1_offset;
    subMap.subBlocks[prevPartName].bodySize = subMap.subBlocks[subBlockName].bodyOffset - subMap.subBlocks[prevPartName].bodyOffset;
    prevPartName = subBlockName;
  }

  if (hdr.lbl_offset) {
    subBlockName = "LBL";
    srcFile.seek(gmpOffset + hdr.lbl_offset);
    srcFile.read((char *)&subMap.hdrLbl, sizeof(subMap.hdrLbl));

    if (ctx.config.debugInfo) {
      misc::Logger::printf("   --- %s header ---\n", subBlockName.toLatin1().data());
      subMap.hdrLbl.print(gmpOffset);
    }

    subMap.subBlocks[subBlockName].hdrOffset = gmpOffset + hdr.lbl_offset;
    subMap.subBlocks[subBlockName].hdrSize = subMap.hdrLbl.size;
    subMap.subBlocks[subBlockName].bodyOffset = gmpOffset + subMap.hdrLbl.lbl12_offset;
    subMap.subBlocks[prevPartName].bodySize = subMap.subBlocks[subBlockName].bodyOffset - subMap.subBlocks[prevPartName].bodyOffset;
    prevPartName = subBlockName;
  }

  if (hdr.net_offset) {
    subBlockName = "NET";
    srcFile.seek(gmpOffset + hdr.net_offset);
    srcFile.read((char *)&subMap.hdrNet, sizeof(subMap.hdrNet));

    if (ctx.config.debugInfo) {
      misc::Logger::printf("   --- %s header ---\n", subBlockName.toLatin1().data());
      subMap.hdrNet.print(gmpOffset);
    }

    subMap.subBlocks[subBlockName].hdrOffset = gmpOffset + hdr.net_offset;
    subMap.subBlocks[subBlockName].hdrSize = subMap.hdrNet.size;
    subMap.subBlocks[subBlockName].bodyOffset = gmpOffset + subMap.hdrNet.net1_offset;
    subMap.subBlocks[prevPartName].bodySize = subMap.subBlocks[subBlockName].bodyOffset - subMap.subBlocks[prevPartName].bodyOffset;
    prevPartName = subBlockName;
  }

  if (hdr.nod_offset) {
    subBlockName = "NOD";
    srcFile.seek(gmpOffset + hdr.nod_offset);
    srcFile.read((char *)&subMap.hdrNod, sizeof(subMap.hdrNod));

    if (ctx.config.debugInfo) {
      misc::Logger::printf("   --- %s header ---\n", subBlockName.toLatin1().data());
      subMap.hdrNod.print(gmpOffset);
    }

    subMap.subBlocks[subBlockName].hdrOffset = gmpOffset + hdr.nod_offset;
    subMap.subBlocks[subBlockName].hdrSize = subMap.hdrNod.size;
    subMap.subBlocks[subBlockName].bodyOffset = gmpOffset + subMap.hdrNod.nod1_offset;
    subMap.subBlocks[prevPartName].bodySize = subMap.subBlocks[subBlockName].bodyOffset - subMap.subBlocks[prevPartName].bodyOffset;
    prevPartName = subBlockName;
  }

  if (hdr.dem_offset) {
    subBlockName = "DEM";
    srcFile.seek(gmpOffset + hdr.dem_offset);
    srcFile.read((char *)&subMap.hdrDem, sizeof(subMap.hdrDem));

    if (ctx.config.debugInfo) {
      misc::Logger::printf("   --- %s header ---\n", subBlockName.toLatin1().data());
      subMap.hdrDem.print(gmpOffset);
    }

    subMap.subBlocks[subBlockName].hdrOffset = gmpOffset + hdr.dem_offset;
    subMap.subBlocks[subBlockName].hdrSize = subMap.hdrDem.size;
    subMap.subBlocks[subBlockName].bodyOffset = gmpOffset + subMap.hdrDem.dem1_offset;
    subMap.subBlocks[prevPartName].bodySize = subMap.subBlocks[subBlockName].bodyOffset - subMap.subBlocks[prevPartName].bodyOffset;
    prevPartName = subBlockName;
  }

  subMap.subBlocks[prevPartName].bodySize = subMap.subBlocks[gmpBlockName].offset + subMap.subBlocks[gmpBlockName].size - subMap.subBlocks[prevPartName].bodyOffset;
}

void DataParser::parseMapLevels(SSubMap &subMap) const {
  // read map levels from section tre1
  srcFile.seek(subMap.subBlocks[subMap.isPseudoNt ? "GMP" : "TRE"].offset + subMap.hdrTre.tre1_offset);
  QByteArray bufMapLevels = srcFile.read(subMap.hdrTre.tre1_size);

  if (subMap.hdrTre.flag & 0x80) {
    minno(&subMap.hdrTre, bufMapLevels);
  }

  quint32 nLevels = subMap.hdrTre.tre1_size / sizeof(ImgHdr::STre1);
  quint32 nSubDivs = 0;
  quint32 nSubDivsLast = 0;

  auto pMapLevel = (ImgHdr::STre1 *)bufMapLevels.data();
  if (ctx.config.debugInfo) {
    misc::Logger::print("---- Levels ----");
  }
  for (quint32 i = 0; i < nLevels; i++) {
    nSubDivs += pMapLevel->subDiv;
    nSubDivsLast = pMapLevel->subDiv;

    if (ctx.config.debugInfo) {
      pMapLevel->print();
    }
    subMap.mapLevels << *pMapLevel;
    pMapLevel++;
  }

  subMap.nSubDivsNext = nSubDivs - nSubDivsLast;
  // resize number of sub-divisions
  subMap.subDivs.resize(nSubDivs);
}

/*
void DataParser::readSubmapArea(SSubMap& subMap) {
  subMap.north = GRMN_RAD(gar_ptr_load(int24_t, subMap.hdrTre.north_bound));
  subMap.east = GRMN_RAD(gar_ptr_load(int24_t, subMap.hdrTre.east_bound));
  subMap.south = GRMN_RAD(gar_ptr_load(int24_t, subMap.hdrTre.south_bound));
  subMap.west = GRMN_RAD(gar_ptr_load(int24_t, subMap.hdrTre.west_bound));

  if (subMap.east == subMap.west) {
    subMap.east = -subMap.east;
  }

  if (subMap.west > 0 && subMap.east < 0) {
    subMap.east = -subMap.east;
  }

  subMap.area = QRectF(QPointF(subMap.west, subMap.north), QPointF(subMap.east, subMap.south));

  if (mapArea.isNull()) {
    mapArea = subMap.area;
  } else {
    mapArea = mapArea.united(subMap.area);
  }
}
*/

void DataParser::parseSubDivInfo(SSubMap &subMap) const {
  srcFile.seek(subMap.subBlocks[subMap.isPseudoNt ? "GMP" : "TRE"].offset + subMap.hdrTre.tre2_offset);
  QByteArray tre2 = srcFile.read(subMap.hdrTre.tre2_size);

  auto pTre2Next = (ImgHdr::STre2Next *)tre2.data();

  auto subDiv = subMap.subDivs.begin();
  auto subDivPrev = subMap.subDivs.end();

  int mapLevelIdx = 0;
  if (subMap.mapLevels.size() == 0) {
    throw Exception("Missing map levels");
  }

  quint32 nSubdiv = subMap.mapLevels[mapLevelIdx].subDiv;
  // parse all 16 byte subdivision entries
  quint32 i;
  quint32 rgnoff = subMap.hdrRgn.rgn1_offset;
  for (i = 0; i < subMap.nSubDivsNext; ++i, --nSubdiv) {
    qint32 cx;
    qint32 cy;
    qint32 width;
    qint32 height;

    subDiv->mapLevel = &subMap.mapLevels[mapLevelIdx];
    subDiv->index = i + 1;
    subDiv->next = pTre2Next->next;
    subDiv->terminate = TRE_SUBDIV_TERM(pTre2Next);
    subDiv->rgnStart = pTre2Next->rgn_offset[0] | pTre2Next->rgn_offset[1] << 8 | pTre2Next->rgn_offset[2] << 16 | (pTre2Next->elements & 0x0F) << 24;
    subDiv->rgnStart += rgnoff;
    // skip if this is the first entry
    if (subDivPrev != subMap.subDivs.end()) {
      subDivPrev->rgnEnd = subDiv->rgnStart;
    } else {
      subDiv->rgnEnd = 0;
    }
    subDivPrev = subDiv;

    subDiv->hasPt = pTre2Next->elements & 0x10;
    subDiv->hasIp = pTre2Next->elements & 0x20;
    subDiv->hasLn = pTre2Next->elements & 0x40;
    subDiv->hasPg = pTre2Next->elements & 0x80;

    // if all subdivisions of this level have been parsed, switch to the next one
    if (nSubdiv == 0) {
      ++mapLevelIdx;
      subDiv->mapLevel = &subMap.mapLevels[mapLevelIdx];
      nSubdiv = subDiv->mapLevel->subDiv;
    }

    subDiv->level = subDiv->mapLevel->zoom();
    subDiv->shift = 24 - subDiv->mapLevel->bits;

    cx = pTre2Next->centerLng[0] | pTre2Next->centerLng[1] << 8 | pTre2Next->centerLng[2] << 16;
    subDiv->iCenterLng = cx;
    cy = pTre2Next->centerLat[0] | pTre2Next->centerLat[1] << 8 | pTre2Next->centerLat[2] << 16;
    subDiv->iCenterLat = cy;
    width = TRE_SUBDIV_WIDTH(pTre2Next) << subDiv->shift;
    height = pTre2Next->height << subDiv->shift;

    subDiv->north = GRMN_RAD(cy + height + 1);
    subDiv->south = GRMN_RAD(cy - height);
    subDiv->east = GRMN_RAD(cx + width + 1);
    subDiv->west = GRMN_RAD(cx - width);

    subDiv->area = QRectF(QPointF(subDiv->west, subDiv->north), QPointF(subDiv->east, subDiv->south));

    subDivPrev = subDiv;
    ++pTre2Next;
    ++subDiv;
  }

  // the subdivisions of the last zoom level do not have a `next` field
  const auto nSubdivs = subMap.subDivs.size();
  ++mapLevelIdx;
  // witch pointer to 14 byte subdivision sections
  ImgHdr::STre2 *pTre2Last = pTre2Next;
  // parse all 14 byte subdivision entries of last map level
  for (; i < nSubdivs; ++i) {
    qint32 cx;
    qint32 cy;
    qint32 width;
    qint32 height;
    subDiv->mapLevel = &subMap.mapLevels[mapLevelIdx];
    subDiv->index = i + 1;
    subDiv->next = 0;
    subDiv->terminate = TRE_SUBDIV_TERM(pTre2Last);
    subDiv->rgnStart = pTre2Last->rgn_offset[0] | pTre2Last->rgn_offset[1] << 8 | pTre2Last->rgn_offset[2] << 16 | (pTre2Last->elements & 0x0F) << 24;
    if (subDiv->rgnStart >= subMap.hdrRgn.rgn1_length) {
      qDebug() << "[WARN] Block size overflow:" << subMap.name << i << nSubdivs << Qt::hex << subDiv->rgnStart << subMap.hdrRgn.rgn1_length;
      break;
    }
    subDiv->rgnStart += rgnoff;

    subDivPrev->rgnEnd = subDiv->rgnStart;

    subDiv->hasPt = pTre2Last->elements & 0x10;
    subDiv->hasIp = pTre2Last->elements & 0x20;
    subDiv->hasLn = pTre2Last->elements & 0x40;
    subDiv->hasPg = pTre2Last->elements & 0x80;

    subDiv->level = subDiv->mapLevel->zoom();
    subDiv->shift = 24 - subDiv->mapLevel->bits;

    cx = pTre2Last->centerLng[0] | pTre2Last->centerLng[1] << 8 | pTre2Last->centerLng[2] << 16;
    subDiv->iCenterLng = cx;
    cy = pTre2Last->centerLat[0] | pTre2Last->centerLat[1] << 8 | pTre2Last->centerLat[2] << 16;
    subDiv->iCenterLat = cy;
    width = TRE_SUBDIV_WIDTH(pTre2Last) << subDiv->shift;
    height = pTre2Last->height << subDiv->shift;

    subDiv->north = GRMN_RAD(cy + height + 1);
    subDiv->south = GRMN_RAD(cy - height);
    subDiv->east = GRMN_RAD(cx + width + 1);
    subDiv->west = GRMN_RAD(cx - width);

    subDiv->area = QRectF(QPointF(subDiv->west, subDiv->north), QPointF(subDiv->east, subDiv->south));

    subDivPrev = subDiv;
    ++pTre2Last;
    ++subDiv;
  }

  const quint32 checkSize = pTre2Last->rgn_offset[0] | pTre2Last->rgn_offset[1] << 8 | pTre2Last->rgn_offset[2] << 16 | (pTre2Last->elements) << 24;
  if (checkSize != subMap.hdrRgn.rgn1_length) {
    qDebug() << "[WARN] The block size check does not match:" << Qt::hex << checkSize << subMap.hdrRgn.rgn1_length;
  }
}

// read extended type elements
void DataParser::parseSubDivInfoExt(SSubMap &subMap) const {
  const quint16 rec_size = subMap.hdrTre.tre7_rec_size;
  const quint32 blockStart = subMap.subBlocks[subMap.isPseudoNt ? "GMP" : "TRE"].offset + subMap.hdrTre.tre7_offset;

  quint32 rgnOffPgEx = subMap.hdrRgn.pgex_offset;
  quint32 rgnLenPgEx = subMap.hdrRgn.pgex_length;
  quint32 rgnOffLnEx = subMap.hdrRgn.lnex_offset;
  quint32 rgnLenLnEx = subMap.hdrRgn.lnex_length;
  quint32 rgnOffPtEx = subMap.hdrRgn.ptex_offset;
  quint32 rgnLenPtEx = subMap.hdrRgn.ptex_length;

  if (subMap.isPseudoNt) {
    rgnOffPgEx -= subMap.hdrRgn.rgn2_offset;
    rgnOffLnEx -= subMap.hdrRgn.rgn2_offset;
    rgnOffPtEx -= subMap.hdrRgn.rgn2_offset;
  }

  auto subDiv = subMap.subDivs.begin();
  auto subDivPrev = subMap.subDivs.end();
  if (subMap.hdrTre.size >= 0x9A && subMap.hdrTre.tre7_size && rec_size >= sizeof(ImgHdr::STre7)) {
    srcFile.seek(blockStart);
    QByteArray subDivEx = srcFile.read(subMap.hdrTre.tre7_size);
    auto pSubDivEx = (ImgHdr::STre7 *)subDivEx.data();

    bool skipPois = (rec_size != sizeof(ImgHdr::STre7));

    subDiv = subMap.subDivs.begin();
    subDivPrev = subMap.subDivs.begin();
    subDiv->offPgEx = pSubDivEx->offsetPg + rgnOffPgEx;
    subDiv->offLnEx = pSubDivEx->offsetLn + rgnOffLnEx;
    subDiv->offPtEx = skipPois ? 0 : pSubDivEx->offsetPt + rgnOffPtEx;

    ++subDiv;
    pSubDivEx = reinterpret_cast<ImgHdr::STre7 *>((quint8 *)pSubDivEx + rec_size);

    while (subDiv != subMap.subDivs.end()) {
      subDiv->offPgEx = pSubDivEx->offsetPg + rgnOffPgEx;
      subDiv->offLnEx = pSubDivEx->offsetLn + rgnOffLnEx;
      subDiv->offPtEx = skipPois ? 0 : pSubDivEx->offsetPt + rgnOffPtEx;

      subDivPrev->lenPgEx = subDiv->offPgEx - subDivPrev->offPgEx;
      subDivPrev->lenLnEx = subDiv->offLnEx - subDivPrev->offLnEx;
      subDivPrev->lenPtEx = skipPois ? 0 : subDiv->offPtEx - subDivPrev->offPtEx;

      subDivPrev = subDiv;

      ++subDiv;
      pSubDivEx = reinterpret_cast<ImgHdr::STre7 *>((quint8 *)pSubDivEx + rec_size);
    }

    subDivPrev->lenPgEx = rgnOffPgEx + rgnLenPgEx - subDivPrev->offPgEx;
    subDivPrev->lenLnEx = rgnOffLnEx + rgnLenLnEx - subDivPrev->offLnEx;
    subDivPrev->lenPtEx = skipPois ? 0 : rgnOffPtEx + rgnLenPtEx - subDivPrev->offPtEx;
  }
}

void DataParser::parseStringTable(SSubMap &subMap) {
  if (!subMap.subBlocks.keys().contains("LBL")) {
    qDebug().noquote() << "Missing LBL block for sub map" << subMap.name;
    return;
  }

  quint32 offsetLbl1 = subMap.subBlocks[subMap.isPseudoNt ? "GMP" : "LBL"].offset + subMap.hdrLbl.lbl1_offset;
  quint32 offsetLbl6 = subMap.subBlocks[subMap.isPseudoNt ? "GMP" : "LBL"].offset + subMap.hdrLbl.lbl6_offset;

  quint16 codepage = 0;
  if (subMap.hdrLbl.size > 0xAA) {
    codepage = subMap.hdrLbl.codepage;
  }
  ctx.codepage = codepage;

  switch (subMap.hdrLbl.coding) {
    case 0x06:  // ascii
      subMap.strTbl = std::make_shared<StrTbl6>(ctx);
      break;

    case 0x09:  // cp0, latin1, cp1251, cp1252
      subMap.strTbl = std::make_shared<StrTbl9>(ctx);
      break;
    case 0x0A:  // cp65001 (utf8), cp932, ms932
      qWarning() << "Not implemented LBL coding:" << Qt::hex << subMap.hdrLbl.coding;
      break;

    default:
      qWarning() << "Unknown or wrong LBL coding:" << Qt::hex << subMap.hdrLbl.coding;
  }

  if (nullptr != subMap.strTbl) {
    subMap.strTbl->registerLbl1(offsetLbl1, subMap.hdrLbl.lbl1_length, subMap.hdrLbl.addr_shift);
    subMap.strTbl->registerLbl6(offsetLbl6, subMap.hdrLbl.lbl6_length);
  }
}

void DataParser::parseMps(SSubMap &subMap) {
  if (subMap.subBlocks.keys().contains("MPS")) {
    misc::Logger::print("--- Map Info ---\n");
    srcFile.seek(subMap.subBlocks["MPS"].offset);

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

void DataParser::decodeObjects(const ImgHdr::SSubDiv &subDiv, const std::shared_ptr<StrTbl> &strTbl, const QByteArray &rgnData) {
  if (subDiv.rgnStart == subDiv.rgnEnd && !subDiv.lenPgEx && !subDiv.lenLnEx && !subDiv.lenPtEx) {
    return;
  }

  if (subDiv.rgnStart > subDiv.rgnEnd && subDiv.rgnEnd != 0x00) {
    qDebug() << "[ERROR] Bug found: quint24 overflow. Compare rgn_start & rgn_end offsets:" << Qt::hex << subDiv.rgnStart << subDiv.rgnEnd;
    return;
  }

  if (subDiv.offPgEx > rgnData.size() || subDiv.offLnEx > rgnData.size() || subDiv.offPtEx > rgnData.size()) {
    qDebug() << "[ERROR] Wrong offset:" << Qt::hex << subDiv.offPgEx << subDiv.offLnEx << subDiv.offPtEx << "| Max offset:" << rgnData.size();
    return;
  }

  const auto level = (quint8)subDiv.level;
  auto &rgn = ctx.rgn[level];
  const quint8 *pRawData = (quint8 *)rgnData.data();

  quint32 offPt = 0;
  quint32 offIp = 0;
  quint32 offLn = 0;
  quint32 offPg = 0;
  quint32 objCnt = subDiv.hasIp && subDiv.hasPt && subDiv.hasLn && subDiv.hasPg;

  auto pOffset = (quint16 *)(pRawData + subDiv.rgnStart);

  // test for points
  if (subDiv.hasPt) {
    offPt = (objCnt - 1) * sizeof(quint16) + subDiv.rgnStart;
  }

  // test for pois
  if (subDiv.hasIp) {
    if (offPt) {
      offIp = gar_load(uint16_t, *pOffset);
      offIp += subDiv.rgnStart;
      ++pOffset;
    } else {
      offIp = (objCnt - 1) * sizeof(quint16) + subDiv.rgnStart;
    }
  }

  // test for polylines
  if (subDiv.hasLn) {
    if (offPt || offIp) {
      offLn = gar_load(uint16_t, *pOffset);
      offLn += subDiv.rgnStart;
      ++pOffset;
    } else {
      offLn = (objCnt - 1) * sizeof(quint16) + subDiv.rgnStart;
    }
  }

  // test for polygons
  if (subDiv.hasPg) {
    if (offPt || offIp || offLn) {
      offPg = gar_load(uint16_t, *pOffset);
      offPg += subDiv.rgnStart;
      ++pOffset;
    } else {
      offPg = (objCnt - 1) * sizeof(quint16) + subDiv.rgnStart;
    }
  }

#ifdef DEBUG_SHOW_POLY_DATA_SUBDIV
  qDebug() << "Subdiv" << subdiv.level << subdiv.n << file.fileName() << "| addr:" << Qt::hex << subdiv.rgn_start << "-" << subdiv.rgn_end << "|" << offPt << opoi << opline
           << opgon;
#endif

  if (subDiv.hasPt) {
    const quint8 *pData = pRawData + offPt;
    const quint8 *pEnd = pRawData + (offIp ? offIp : offLn ? offLn : offPg ? offPg : subDiv.rgnEnd);
    while (pData < pEnd) {
      RgnNode pt;
      pData += pt.decode(subDiv.iCenterLng, subDiv.iCenterLat, subDiv.shift, pData);

      if (strTbl) {
        pt.isLbl6 ? strTbl->get(pt.lblPtr, StrTbl::LabelType::poi, pt.labels) : strTbl->get(pt.lblPtr, StrTbl::LabelType::lbl, pt.labels);
      }

      if (nodeSanitizer(pt, subDiv.area)) {
        ++ctx.report.numPgEx;  // or numPoEx?
        rgn.pts.push_back(pt);
      }
    }
  }

  if (subDiv.hasIp) {
    const quint8 *pData = pRawData + offIp;
    const quint8 *pEnd = pRawData + (offLn ? offLn : offPg ? offPg : subDiv.rgnEnd);
    while (pData < pEnd) {
      RgnNode ip;
      pData += ip.decode(subDiv.iCenterLng, subDiv.iCenterLat, subDiv.shift, pData);

      if (strTbl) {
        ip.isLbl6 ? strTbl->get(ip.lblPtr, StrTbl::LabelType::poi, ip.labels) : strTbl->get(ip.lblPtr, StrTbl::LabelType::lbl, ip.labels);
      }

      if (nodeSanitizer(ip, subDiv.area)) {
        ++ctx.report.numPgEx;
        rgn.pts.push_back(ip);
      }
    }
  }

  if (subDiv.hasLn) {
    const quint8 *pData = pRawData + offLn;
    const quint8 *pEnd = pRawData + (offPg ? offPg : subDiv.rgnEnd);
    while (pData < pEnd) {
      RgnPath ln;
      pData += ln.decode(subDiv.iCenterLng, subDiv.iCenterLat, subDiv.shift, true, pData, pEnd);

      if (strTbl && ln.lblInfo) {
        if (ln.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[ln] 1 hasNet1Label" << Qt::hex << ln.lblInfo;
          strTbl->get(ln.lblInfo, StrTbl::LabelType::net, ln.labels);
        } else {
          strTbl->get(ln.lblInfo, StrTbl::LabelType::lbl, ln.labels);
        }
      } else {
        ;
      }
      hasExLabelCount += ln.hasExLabelCount;

      if (pathSanitizer(ln, true, subDiv.area)) {
        ++ctx.report.numPgEx;
        rgn.pgs.push_back(ln);
      }
    }
  }

  if (subDiv.hasPg) {
    const quint8 *pData = pRawData + offPg;
    const quint8 *pEnd = pRawData + subDiv.rgnEnd;
    while (pData < pEnd) {
      RgnPath pg;
      pData += pg.decode(subDiv.iCenterLng, subDiv.iCenterLat, subDiv.shift, false, pData, pEnd);

      if (strTbl && pg.lblInfo) {
        if (pg.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[pg] 1 hasNet1Label" << Qt::hex << pg.lblInfo;
          strTbl->get(pg.lblInfo, StrTbl::LabelType::net, pg.labels);
        } else {
          strTbl->get(pg.lblInfo, StrTbl::LabelType::lbl, pg.labels);
        }
      } else {
        ;
      }
      hasExLabelCount += pg.hasExLabelCount;

      if (pathSanitizer(pg, false, subDiv.area)) {
        ++ctx.report.numPgEx;
        rgn.pgs.push_back(pg);
      }
    }
  }

  // parsing extended types
  if (subDiv.lenPgEx) {
    // qDebug() << "Exteneded type: pg" << Qt::hex << subdiv.offsetPgEx << subdiv.lengthPgEx;
    const quint8 *pData = pRawData + subDiv.offPgEx;
    const quint8 *pEnd = pData + subDiv.lenPgEx;
    while (pData < pEnd) {
      RgnPath pg;
      // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
      pData += pg.decodeEx(subDiv.iCenterLng, subDiv.iCenterLat, subDiv.shift, false, pData, pEnd);

      if (strTbl && pg.lblInfo) {
        if (pg.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[pg] 2 hasNet1Label" << Qt::hex << pg.lblInfo;
          strTbl->get(pg.lblInfo, StrTbl::LabelType::net, pg.labels);
        } else {
          strTbl->get(pg.lblInfo, StrTbl::LabelType::lbl, pg.labels);
        }
      } else {
        ;
      }
      hasExLabelCount += pg.hasExLabelCount;

      if (pathSanitizer(pg, false, subDiv.area)) {
        ++ctx.report.numPgEx;
        rgn.pgs.push_back(pg);
      }
    }
  }

  if (subDiv.lenLnEx) {
    // qDebug() << "Exteneded type: polyline" << Qt::hex << subdiv.offsetLnEx << subdiv.lengthLnEx;
    const quint8 *pData = pRawData + subDiv.offLnEx;
    const quint8 *pEnd = pData + subDiv.lenLnEx;
    while (pData < pEnd) {
      RgnPath ln;
      // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
      pData += ln.decodeEx(subDiv.iCenterLng, subDiv.iCenterLat, subDiv.shift, true, pData, pEnd);

      if (strTbl && ln.lblInfo) {
        if (ln.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[ln] 2 hasNet1Label" << Qt::hex << ln.lblInfo;
          strTbl->get(ln.lblInfo, StrTbl::LabelType::net, ln.labels);
        } else {
          strTbl->get(ln.lblInfo, StrTbl::LabelType::lbl, ln.labels);
        }
      } else {
        ;
      }
      hasExLabelCount += ln.hasExLabelCount;

      if (pathSanitizer(ln, true, subDiv.area)) {
        ++ctx.report.numPgEx;
        rgn.pgs.push_back(ln);
      }
    }
  }

  if (subDiv.lenPtEx) {
    // qDebug() << "Exteneded type: point" << Qt::hex << subdiv.offsetPtEx << subdiv.lengthPtEx;
    const quint8 *pData = pRawData + subDiv.offPtEx;
    const quint8 *pEnd = pData + subDiv.lenPtEx;
    while (pData < pEnd) {
      RgnNode pt;
      pData += pt.decodeEx(subDiv.iCenterLng, subDiv.iCenterLat, subDiv.shift, pData, pEnd);

      if (strTbl) {
        pt.isLbl6 ? strTbl->get(pt.lblPtr, StrTbl::LabelType::poi, pt.labels) : strTbl->get(pt.lblPtr, StrTbl::LabelType::lbl, pt.labels);
      }

      if (nodeSanitizer(pt, subDiv.area)) {
        ++ctx.report.numPgEx;  // or numPoEx?
        rgn.pts.push_back(pt);
      }
    }
  }
}

double DataParser::calcPgArea(const QPolygonF &pg) const {
  const auto n = pg.size();
  if (n < 3) return 0.0;

  double area = 0.0;

  // Оптимизация: директен достъп до точки и минимум операции
  const QPointF *points = pg.constData();
  const QPointF &first = points[0];

  for (int i = 1; i < n - 1; ++i) {
    const QPointF &p1 = points[i];
    const QPointF &p2 = points[i + 1];
    area += (p1.x() - first.x()) * (p2.y() - first.y()) - (p2.x() - first.x()) * (p1.y() - first.y());
  }

  return std::abs(area) * 0.5;
}

bool DataParser::isPgTinyFast(const QPolygonF &pg, double minArea) const {
  return false;
  if (const auto n = pg.size(); n < 3) return true;

  // Бърза проверка на bounding rect първо (най-евтина)
  const QRectF bounds = pg.boundingRect();
  const double width = bounds.width();
  const double height = bounds.height();

  // Ако bounding rect е твърде малък, няма нужда да изчисляваме площ
  if (width * height < minArea * 0.5) {  // 0.5 за по-ранно отхвърляне
    return true;
  }

  // Само ако bounding rect е OK, изчисляваме точна площ
  const double area = calcPgArea(pg);
  return area < minArea;
}

// Алтернатива: още по-бърза версия само с bounding box
bool DataParser::isPgTinyUltraFast(const QPolygonF &pg, double minDimension) const {
  if (pg.size() < 3) return true;

  const QRectF bounds = pg.boundingRect();
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

void DataParser::readProductInfo(QDataStream &stream) const {
  quint16 idProd;
  quint16 idFamily;
  stream >> idProd >> idFamily;
  misc::Logger::printf("Product Info: %i %i %s\n", idProd, idFamily, readRawString(stream).toUtf8().data());
}

void DataParser::readMapInfo(QDataStream &stream) const {
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

  misc::Logger::printf("Map Info: %i %i %i %s %s %s %08X %i \n", idProd, idFamily, mapNumber, seriesName.toLocal8Bit().data(), mapDesc.toLocal8Bit().data(),
                       areaName.toLocal8Bit().data(), mapId, mapId);
}

void DataParser::readSubmaps() {
  mapArea = QRectF();
  qDebug() << "Submaps count:" << ctx.subMaps.size();

  if (ctx.config.debugInfo) {
    misc::Logger::print("--- Block offsets ---");

    for (auto &subMap : ctx.subMaps) {
      for (const auto &[subBlock, part] : subMap.subBlocks.asKeyValueRange()) {
        misc::Logger::printf("\t%s %s %08X %08X %08X %08X %08X %08X\n", subMap.name.toLocal8Bit().data(), subBlock.toLocal8Bit().data(), part.offset, part.size, part.hdrOffset,
                             part.hdrSize, part.bodyOffset, part.bodySize);
      }
    }
    PRINT_ENDL;
  }

  for (auto &subMap : ctx.subMaps) {
    if (subMap.isPseudoNt) {
      readNewFormat(subMap);
    } else {
      readOldFormat(subMap);
    }
    if (!(subMap.subBlocks.contains("TRE") && subMap.subBlocks.contains("RGN"))) {
      // missing mandatory subMap subBlocks
      continue;
    }
    parseMapLevels(subMap);
    parseSubDivInfo(subMap);
    parseSubDivInfoExt(subMap);
    parseStringTable(subMap);
    parseMps(subMap);
  }

  fflush(stdout);
  fflush(stderr);
}

void DataParser::readObjects() {
  // int numThreads = 4;

  int numThreads = 1;
  if (numThreads > 1) {
    QThreadPool::globalInstance()->setMaxThreadCount(numThreads);
  }

  // uint8_t filePart = 0;
  for (const auto &subMap : ctx.subMaps) {
    qDebug() << "Submap name:" << subMap.name << "| Total sub divisions:" << subMap.subDivs.size();
    if (subMap.subDivs.isEmpty()) {
      qDebug() << "Skip RGN decode for this subMap (no subDivs)" << subMap.name;
      continue;
    }

    QFileInfo ofInfo(ctx.config.outputFile);
    QString dir = ofInfo.path();
    if (numThreads == 1) {
      processObjects(subMap);
    } else {
      QThreadPool::globalInstance()->start([this, subMap]() { processObjects(subMap); });
    }
#ifdef SANITY_CHECK
    if (ctx.config.debugInfo) {
      if (hasExLabelCount) {
        // qDebug() << "[INFO] Number of hasExLabel:" << hasExLabelCount;
        hasExLabelCount = 0;
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
      if (ctx.stats.warnLnOversize) {
        qDebug() << "[WARN] Number of too long polyline (possibly bitstream error):" << ctx.stats.warnLnOversize;
        ctx.stats.warnLnOversize = 0;
      }
      if (ctx.stats.warnInvalidType) {
        qDebug() << "[WARN] Number of invalid objects type:" << ctx.stats.warnInvalidType;
        ctx.stats.warnInvalidType = 0;
      }
      if (ctx.stats.warnInvalidCoord) {
        qDebug() << "[WARN] Number of invalid coordinates:" << ctx.stats.warnInvalidCoord;
        ctx.stats.warnInvalidCoord = 0;
      }
    } else {
      hasExLabelCount = 0;
      hasNet1LabelCount = 0;
      ctx.stats.reset();
      ctx.stats.warnInvalidCoord = 0;
    }
#endif
  }

  if (numThreads > 1) {
    QThreadPool::globalInstance()->waitForDone();
  }
}

void DataParser::processObjects(const SSubMap &subMap) {
  try {
    QByteArray rgnData;
    if (subMap.isPseudoNt) {
      srcFile.seek(subMap.subBlocks["RGN"].hdrOffset);
      rgnData = srcFile.read(subMap.subBlocks["RGN"].hdrSize);
      srcFile.seek(subMap.subBlocks["RGN"].bodyOffset);
      rgnData += srcFile.read(subMap.subBlocks["RGN"].bodySize);
      // auto totalSize = subMap.subBlocks["RGN"].hdrSize + subMap.subBlocks["RGN"].bodySize;
    } else {
      srcFile.seek(subMap.subBlocks["RGN"].offset);
      rgnData = srcFile.read(subMap.subBlocks["RGN"].size);
    }

    if (rgnData.isEmpty()) {
      qDebug() << "[WARN] No RGN data";
      return;
    }

    for (const auto &subDiv : subMap.subDivs) {
      // if (ctx.config.debugInfo) { subdiv.print(); }
      if (ctx.stats.warnInvalidCoord + ctx.stats.warnInvalidType + ctx.stats.warnSuspiciousSegment + ctx.stats.warnSkipOutside + ctx.stats.infoSkipDupePoint +
              ctx.stats.warnTotals + ctx.stats.warnLnOversize >
          500) {
        qDebug() << "[ERROR] Too many errors: wrong offsets or unknown format with extended headers.";
        break;
      }
      decodeObjects(subDiv, subMap.strTbl, rgnData);
      qDebug() << "SubDiv index:" << subDiv.index << subDiv.level;
      if (subDiv.level == 1) {
        break;
        //
      }
    }

    qDebug() << "FINAL???? 1";
    ctx.report.objDecoded();
    qDebug().noquote() << QString("Total decoded objects: %1 | RGN: %2 %3 %4 %5 %6 %7 %8 %9 | Objects: %10 %11 %12")
                              .arg(ctx.report.numObjDecoded, -8)
                              .arg(ctx.report.numPt, -8)
                              .arg(ctx.report.numIp, -8)
                              .arg(ctx.report.numLn, -8)
                              .arg(ctx.report.numPg, -8)
                              .arg(ctx.report.numPtEx, -8)
                              .arg(ctx.report.numPoEx, -8)
                              .arg(ctx.report.numLnEx, -8)
                              .arg(ctx.report.numPgEx, -8)
                              .arg(ctx.report.numPt + ctx.report.numIp + ctx.report.numPtEx + ctx.report.numPoEx, -8)
                              .arg(ctx.report.numLn + ctx.report.numLnEx, -8)
                              .arg(ctx.report.numPg + ctx.report.numPgEx, -8);
    ctx.report.resetDecoded();
  } catch (const Exception &e) {
    qDebug() << "Fatal error:" << e.msg;
  }
}

// data contains locked/encrypted data
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

bool DataParser::isPgTinyOptimized(const QPolygonF &pg, double minArea) {
  const auto n = pg.size();
  if (n < 3) return true;

  // Бърза bounding box проверка
  const QRectF bounds = pg.boundingRect();
  if (const double bboxArea = bounds.width() * bounds.height(); bboxArea < minArea * 0.3) {
    return true;
  }

  // Бързо изчисление на площ без излишни операции
  if (n == 3) {
    // Оптимизация за триъгълници
    const QPointF &p1 = pg[0];
    const QPointF &p2 = pg[1];
    const QPointF &p3 = pg[2];
    const double area = std::abs((p2.x() - p1.x()) * (p3.y() - p1.y()) - (p3.x() - p1.x()) * (p2.y() - p1.y())) * 0.5;
    return area < minArea;
  }

  // Общ случай
  double area = 0.0;
  const QPointF *points = pg.constData();

  for (int i = 0; i < n; ++i) {
    const int j = (i + 1) % n;
    area += points[i].x() * points[j].y() - points[j].x() * points[i].y();
  }

  return std::abs(area) * 0.5 < minArea;
}

bool DataParser::nodeSanitizer(RgnNode &pt, const QRectF &area) {
  if (pt.type <= 0) {
    ++ctx.stats.warnInvalidType;
    return false;
  }

  double lat = qRadiansToDegrees(pt.node.y());
  double lng = qRadiansToDegrees(pt.node.x());

  lat = clampLat(lat);
  lng = normalizeLng(lng);
  if (qAbs(lat) == 90.0 || qAbs(lng) == 180.0) {
    // qDebug() << "[WARN] Invalid coords:" << lat << lng;
    return false;
    ++ctx.stats.warnInvalidCoord;
  }
  pt.nodeRad = QPointF(lng, lat);

  if (!area.contains(pt.node)) {
    // qDebug() << "[WARN] Skip points outside subdiv area:" << subdiv.area << pt.ips;
    ++ctx.stats.warnSkipOutside;
    return false;
  }

  return true;
}

bool DataParser::pathSanitizer(RgnPath &shape, const bool &isLine, const QRectF &area) {
  if (shape.type <= 0) {
    ++ctx.stats.warnInvalidType;
    qWarning() << "[WARN] Invalid line type:" << shape.type;
    return false;
  }

  int minPts = isLine ? 2 : 3;

  if (shape.path.size() > 8000) {
    ++ctx.stats.warnLnOversize;
    qWarning() << "[WARN] Too long polyline:" << shape.path.size();
    return false;
  }

  QVector<QPointF> cleaned;
  cleaned.reserve(shape.path.size());

  QPointF prev;
  QPointF first;
  bool firstSet = false;
  int polyErrors = 0;

  for (const QPointF &pt : shape.path) {
    if (polyErrors > 50) {
      ++ctx.stats.warnTotals;
      break;
    }

    if (!firstSet) {
      first = pt;
      firstSet = true;
    } else if (pt == prev) {
      ++ctx.stats.infoSkipDupePoint;
      continue;  // прескачаме последователни дубликати
    }

    if (firstSet && isSuspiciousSegment(prev, pt)) {
      ++ctx.stats.warnSuspiciousSegment;
      ++polyErrors;
      continue;
    }

    cleaned.push_back(pt);
    prev = pt;
  }

  // Проверка за минимален брой точки
  if (cleaned.size() < minPts) {
    ++ctx.stats.warnTiny;
    qWarning() << "[WARN] Too few points:" << cleaned.size();
    return false;
  }

  // ако последната точка съвпада с първата (затворен полигон), премахваме я
  if (cleaned.size() > minPts && cleaned.first() == cleaned.last()) {
    cleaned.removeLast();
  }

  if (isCompletelyOutside(cleaned, area)) {
    // qDebug() << "[WARN] Skip polygons outside subdiv area:" << subdiv.area << shape.points.toList().first(10);
    ++ctx.stats.warnSkipOutside;
    return false;
  }

  if (isPgTinyFast(cleaned)) {
    // qDebug() << "[WARN] Polygon area is too small:" << shape.points;
    ++ctx.stats.warnSmallArea;
    return false;
  }

  shape.path = std::move(cleaned);

  return true;
}