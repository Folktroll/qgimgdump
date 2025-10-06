#include "imgdump.h"

#include <QCommandLineOption>
#include <QCommandLineParser>
#include <QDebug>
#include <QFileInfo>
#include <QMutexLocker>
#include <QThreadPool>

#include "exception.h"
#include "inline.h"
#include "lbl.h"
#include "struct.h"
#include "task.h"

ImgDump::ImgDump(int &argc, char **argv) : QCoreApplication(argc, argv) {
  QCommandLineParser parser;
  parser.setApplicationDescription("qgimpdec 1.0.0");
  parser.addHelpOption();

  QCommandLineOption inputOption(QStringList() << "i" << "input", ".img input file", "input_file.img");
  QCommandLineOption outputOption(QStringList() << "o" << "output", "base output file", "output_file");
  QCommandLineOption splitOption(QStringList() << "s" << "submap", "Creates a separate file for each submap");
  QCommandLineOption debugOption(QStringList() << "d" << "debug", "Print debug info");
  QCommandLineOption fsizeOption(QStringList() << "maxsize", "File size per chunk in GB (.mp format only)", "maxsize", "1");
  parser.addOption(inputOption);
  parser.addOption(outputOption);
  parser.addOption(splitOption);
  parser.addOption(debugOption);
  parser.addOption(fsizeOption);
  parser.process(ImgDump::arguments());

  if (!parser.isSet(inputOption)) {
    qCritical() << "Option -i is required!";
    parser.showHelp(1);
  } else {
    inputFile = parser.value("input");
    QFileInfo inputFileInfo(inputFile);

    if (!inputFileInfo.exists()) {
      qCritical() << "Error: Input file does not exist:" << inputFile;
      exit(1);
    }

    if (!inputFileInfo.isReadable()) {
      qCritical() << "Error: No permissions to read the file:" << inputFile;
      exit(1);
    }
  }

  if (!parser.isSet(outputOption)) {
    qCritical() << "Option -o is required!";
    parser.showHelp(1);
  } else {
    outputFile = parser.value("output");
    QFileInfo outputFileInfo(outputFile);
    QString suffix = outputFileInfo.suffix();
    if (suffix == "mp") {
      csvOutput = false;
    } else if (suffix == "csv") {
      // simplified output file in csv format for faster processing
      csvOutput = true;
    } else {
      throw Exception("Unsupported output file format. Available formats: .csv, .mp");
    }

    if (outputFileInfo.exists()) {
      QDateTime lastModified = outputFileInfo.lastModified();
      QString timestamp = lastModified.toString("_yyyyMMdd_hhmm");
      QString baseName = outputFileInfo.completeBaseName();
      QString dir = outputFileInfo.path();

      QString renamedFile = dir + "/" + baseName + timestamp + "." + suffix;

      QFile::rename(outputFile, renamedFile);
      qWarning() << "The existing file was renamed to:" << renamedFile;
    }
  }

  if (parser.isSet(splitOption)) {
    splitSubmaps = true;
  }

  if (parser.isSet(debugOption)) {
    debugInfo = true;
  }

  if (parser.isSet(fsizeOption)) {
    maxFileSize = parser.value(fsizeOption.valueName()).toFloat() * 1000 * 1024 * 1024;
  }

  codec = QTextCodec::codecForName("Windows-1251");
  if (!codec) {
    qWarning("CP1251 codec not available");
  }

  QFile srcFile(inputFile);
  try {
    if (!srcFile.open(QIODevice::ReadOnly)) {
      throw Exception("Failed to open file: " + inputFile);
    }

    readFat(srcFile);
    readSubmaps(srcFile);
    readObjects(srcFile);
  } catch (const Exception &e) {
    qDebug() << "Fatal error:" << e.msg;
  }

  srcFile.close();
}

ImgDump::~ImgDump() {
  qDebug() << "✅ Done.";
}

void ImgDump::readFat(QFile &srcFile) {
  submaps.clear();

  IMG::gmapsupp_imghdr_t imghdr;
  srcFile.seek(0);
  srcFile.read((char *)&imghdr, sizeof(IMG::gmapsupp_imghdr_t));
  srcFile.seek(imghdr.offsetFAT * 0x200);

  if (debugInfo) {
    print("----- IMG Header ----- size(%08X)\n", sizeof(IMG::gmapsupp_imghdr_t));
    imghdr.print();
  }

  mask = imghdr.xorByte;

  mask32 = mask;
  mask32 <<= 8;
  mask32 |= mask;
  mask32 <<= 8;
  mask32 |= mask;
  mask32 <<= 8;
  mask32 |= mask;

  mask64 = mask32;
  mask64 <<= 32;
  mask64 |= mask32;

  if (strncmp(imghdr.identifier, "GARMIN", 7) != 0) {
    throw Exception(tr("Bad file format: ") + inputFile);
  }

  nameStr = QByteArray((const char *)imghdr.desc1, 20);
  nameStr += imghdr.desc2;
  nameStr = nameStr.trimmed();

  // if (imghdr.desc1) {
  // nameStr = QString::fromLatin1(imghdr.desc1).trimmed();
  // }

  // imghdr

  size_t blocksize = imghdr.blocksize();

  qDebug() << "---- FAT ----";
  IMG::FATBlock_t FATBlock;
  srcFile.read((char *)&FATBlock, sizeof(IMG::FATBlock_t));
  while (FATBlock.flag == 1) {
    if (srcFile.atEnd()) {
      throw Exception("Premature end of file.");
    }

    if (FATBlock.size != 0) {
      char submapStr[sizeof(FATBlock.name) + 1] = {0};
      memcpy(submapStr, FATBlock.name, sizeof(FATBlock.name));

      char subfileStr[sizeof(FATBlock.type) + 1] = {0};
      memcpy(subfileStr, FATBlock.type, sizeof(FATBlock.type));

      if (submapStr[0] == 0x20) {
        qDebug() << "Skip empty subfile type:" << submapStr << subfileStr;
        // } else if (strcmp(submapStr, "00235022") != 0) {
        //   qDebug() << "Skip subfile by name:" << submapStr;
      } else if (submaps.contains(submapStr) && strcmp(subfileStr, "GMP") != 0 && submaps[submapStr].subfiles.keys().contains(subfileStr)) {
        // or check FATBlock.part > 0x00 ?
        qDebug() << "Skip duplicate subfile type:" << submapStr << subfileStr;
      } else if (strcmp(subfileStr, "SRT") == 0 || strcmp(subfileStr, "MDR") == 0 || strcmp(subfileStr, "MD2") == 0 || strcmp(subfileStr, "TYP") == 0) {
        qDebug() << "Skip useless subfile type:" << submapStr << subfileStr;
        // } else if (strcmp(subfileStr, "NET") == 0 || strcmp(subfileStr, "NOD") == 0 || strcmp(subfileStr, "LBL") == 0 || strcmp(subfileStr, "MPS") == 0) {
        //   qDebug() << "Skip subfile type (for debug purpose):" << submapStr << subfileStr;
      } else {
        submap_t &submap = submaps[submapStr];
        submap.name = submapStr;

        submap_subfile_t &part = submap.subfiles[subfileStr];
        part.size = gar_load(quint32, FATBlock.size);
        part.offset = quint32(gar_load(uint16_t, FATBlock.blocks[0]) * blocksize);

        // @todo: more checks, maybe NT or checl 0x0D flags for bit 0x80
        submap.isPseudoNT = strcmp(subfileStr, "GMP") == 0;
      }
    }

    srcFile.read((char *)&FATBlock, sizeof(IMG::FATBlock_t));
  }
}

void ImgDump::readOldFormat(QFile &srcFile, submap_t &submap) {
  QString subfileName = "TRE";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char *)&size, sizeof(size));

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char *)&submap.hdrTRE, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrTRE.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "RGN";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char *)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char *)&submap.hdrRGN, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrRGN.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "LBL";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char *)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char *)&submap.hdrLBL, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrLBL.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "NET";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char *)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char *)&submap.hdrNET, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrNET.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "NOD";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char *)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char *)&submap.hdrNOD, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrNOD.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "DEM";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char *)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char *)&submap.hdrDEM, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrDEM.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "MPS";
  if (submap.subfiles.keys().contains(subfileName)) {
    print("--- Map Info ---\n");
    srcFile.seek(submap.subfiles[subfileName].offset);

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

void ImgDump::readNewFormat(QFile &srcFile, submap_t &submap) {
  const QString gmpSubfileName = "GMP";
  srcFile.seek(submap.subfiles[gmpSubfileName].offset);

  IMG::gmp_hdr_t hdr;
  srcFile.read((char *)&hdr, sizeof(hdr));
  QString copyright(srcFile.readLine());

  QString subfileName;
  QString prevPartName;

  const quint32 gmpOffset = submap.subfiles[gmpSubfileName].offset;
  if (debugInfo) {
    print("--- GMP Header %s (%08X)---\n", submap.name.toLatin1().data(), gmpOffset);
    hdr.print();
  }

  if (hdr.tre_offset) {
    subfileName = "TRE";
    srcFile.seek(gmpOffset + hdr.tre_offset);
    srcFile.read((char *)&submap.hdrTRE, sizeof(submap.hdrTRE));

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrTRE.print(gmpOffset);
    }

    submap.subfiles[subfileName].hdrOffset = gmpOffset + hdr.tre_offset;
    submap.subfiles[subfileName].hdrSize = submap.hdrTRE.size;
    submap.subfiles[subfileName].bodyOffset = gmpOffset + submap.hdrTRE.tre3_offset;
    // submap.subfiles[prevPartName].bodySize = submap.subfiles[partName].bodyOffset - submap.subfiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.rgn_offset) {
    subfileName = "RGN";
    srcFile.seek(gmpOffset + hdr.rgn_offset);
    srcFile.read((char *)&submap.hdrRGN, sizeof(submap.hdrRGN));

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrRGN.print(gmpOffset);
    }

    submap.subfiles[subfileName].hdrOffset = gmpOffset + hdr.rgn_offset;
    submap.subfiles[subfileName].hdrSize = submap.hdrRGN.size;
    submap.subfiles[subfileName].bodyOffset = gmpOffset + submap.hdrRGN.rgn1_offset;
    submap.subfiles[prevPartName].bodySize = submap.subfiles[subfileName].bodyOffset - submap.subfiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.lbl_offset) {
    subfileName = "LBL";
    srcFile.seek(gmpOffset + hdr.lbl_offset);
    srcFile.read((char *)&submap.hdrLBL, sizeof(submap.hdrLBL));

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrLBL.print(gmpOffset);
    }

    submap.subfiles[subfileName].hdrOffset = gmpOffset + hdr.lbl_offset;
    submap.subfiles[subfileName].hdrSize = submap.hdrLBL.size;
    submap.subfiles[subfileName].bodyOffset = gmpOffset + submap.hdrLBL.lbl12_offset;
    submap.subfiles[prevPartName].bodySize = submap.subfiles[subfileName].bodyOffset - submap.subfiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.net_offset) {
    subfileName = "NET";
    srcFile.seek(gmpOffset + hdr.net_offset);
    srcFile.read((char *)&submap.hdrNET, sizeof(submap.hdrNET));

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrNET.print(gmpOffset);
    }

    submap.subfiles[subfileName].hdrOffset = gmpOffset + hdr.net_offset;
    submap.subfiles[subfileName].hdrSize = submap.hdrNET.size;
    submap.subfiles[subfileName].bodyOffset = gmpOffset + submap.hdrNET.net1_offset;
    submap.subfiles[prevPartName].bodySize = submap.subfiles[subfileName].bodyOffset - submap.subfiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.nod_offset) {
    subfileName = "NOD";
    srcFile.seek(gmpOffset + hdr.nod_offset);
    srcFile.read((char *)&submap.hdrNOD, sizeof(submap.hdrNOD));

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrNOD.print(gmpOffset);
    }

    submap.subfiles[subfileName].hdrOffset = gmpOffset + hdr.nod_offset;
    submap.subfiles[subfileName].hdrSize = submap.hdrNOD.size;
    submap.subfiles[subfileName].bodyOffset = gmpOffset + submap.hdrNOD.nod1_offset;
    submap.subfiles[prevPartName].bodySize = submap.subfiles[subfileName].bodyOffset - submap.subfiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  if (hdr.dem_offset) {
    subfileName = "DEM";
    srcFile.seek(gmpOffset + hdr.dem_offset);
    srcFile.read((char *)&submap.hdrDEM, sizeof(submap.hdrDEM));

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrDEM.print(gmpOffset);
    }

    submap.subfiles[subfileName].hdrOffset = gmpOffset + hdr.dem_offset;
    submap.subfiles[subfileName].hdrSize = submap.hdrDEM.size;
    submap.subfiles[subfileName].bodyOffset = gmpOffset + submap.hdrDEM.dem1_offset;
    submap.subfiles[prevPartName].bodySize = submap.subfiles[subfileName].bodyOffset - submap.subfiles[prevPartName].bodyOffset;
    prevPartName = subfileName;
  }

  submap.subfiles[prevPartName].bodySize = submap.subfiles[gmpSubfileName].offset + submap.subfiles[gmpSubfileName].size - submap.subfiles[prevPartName].bodyOffset;
}

QString ImgDump::readRawString(QDataStream &stream) {
  QByteArray label;
  quint8 tmp;
  stream >> tmp;
  while (tmp != 0) {
    label.append(tmp);
    stream >> tmp;
  }
  return QString::fromUtf8(label);
}

void ImgDump::readProductInfo(QDataStream &stream) {
  quint16 idProd;
  quint16 idFamily;
  stream >> idProd >> idFamily;
  print("Product Info: %i %i %s\n", idProd, idFamily, readRawString(stream).toUtf8().data());
}

void ImgDump::readMapInfo(QDataStream &stream) {
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

  print("Map Info: %i %i %i %s %s %s %08X %i \n", idProd, idFamily, mapNumber, seriesName.toLocal8Bit().data(), mapDesc.toLocal8Bit().data(), areaName.toLocal8Bit().data(), mapId,
        mapId);
}

void ImgDump::readSubmaps(QFile &srcFile) {
  maparea = QRectF();
  qDebug() << "Submaps count:" << submaps.size();

  if (debugInfo) {
    print("--- Subfiles ---\n");
    for (auto &submap : submaps) {
      for (const auto &[subfile, part] : submap.subfiles.asKeyValueRange()) {
        print("%s %s %08X %08X %08X %08X %08X %08X\n", submap.name.toLocal8Bit().data(), subfile.toLocal8Bit().data(), part.offset, part.size, part.hdrOffset, part.hdrSize,
              part.bodyOffset, part.bodySize);
      }
    }
  }

  for (auto &submap : submaps) {
    if (submap.isPseudoNT) {
      readNewFormat(srcFile, submap);
    } else {
      readOldFormat(srcFile, submap);
    }
    if (!(submap.subfiles.contains("TRE") && submap.subfiles.contains("RGN"))) {
      // missing mandatory submap subfiles
      continue;
    }
    parseMapLevels(srcFile, submap);
    parseSubdivInfo(srcFile, submap);
    parseSubdivInfoExt(srcFile, submap);
    parseStringTable(srcFile, submap);
  }

  fflush(stdout);
  fflush(stderr);
}

void ImgDump::parseMapLevels(QFile &srcFile, submap_t &submap) {
  // read map levels from section tre1
  srcFile.seek(submap.subfiles[submap.isPseudoNT ? "GMP" : "TRE"].offset + submap.hdrTRE.tre1_offset);
  QByteArray bufMapLevels = srcFile.read(submap.hdrTRE.tre1_size);

  if (submap.hdrTRE.flag & 0x80) {
    minno(&submap.hdrTRE, bufMapLevels);
  }

  quint32 nLevels = submap.hdrTRE.tre1_size / sizeof(IMG::tre1_t);
  quint32 nSubdivs = 0;
  quint32 nSubdivsLast = 0;

  IMG::tre1_t *pMapLevel = (IMG::tre1_t *)bufMapLevels.data();
  for (quint32 i = 0; i < nLevels; i++) {
    nSubdivs += pMapLevel->subdiv;
    nSubdivsLast = pMapLevel->subdiv;

    if (debugInfo) {
      pMapLevel->print();
    }
    submap.mapLevels << *pMapLevel;
    pMapLevel++;
  }

  submap.nSubdivsNext = nSubdivs - nSubdivsLast;
  // resize number of sub-divisions
  submap.subdivs.resize(nSubdivs);
}

IMG::tre0_t ImgDump::readCopyrights(QFile &srcFile, quint32 baseOffset, quint32 limitOffset) {
  IMG::tre0_t result;

  quint32 size = limitOffset - baseOffset;
  QByteArray buf(size, 0);

  srcFile.seek(baseOffset);
  if (srcFile.read(buf.data(), size) != size) {
    qWarning() << "Failed to read copyright block!";
    return result;
  }

  QList<QByteArray> parts = buf.split('\0');

  if (parts.size() > 0) {
    result.descr1 = QString::fromLatin1(parts[0].constData());
  }

  if (parts.size() > 1) {
    result.descr2 = QString::fromLatin1(parts[1].constData());
  }

  return result;
}

/*
void ImgDump::readSubmapArea(submap_t& submap) {
  // read map boundaries from header
  submap.north = GRMN_RAD(gar_ptr_load(int24_t, submap.hdrTRE.northbound));
  submap.east = GRMN_RAD(gar_ptr_load(int24_t, submap.hdrTRE.eastbound));
  submap.south = GRMN_RAD(gar_ptr_load(int24_t, submap.hdrTRE.southbound));
  submap.west = GRMN_RAD(gar_ptr_load(int24_t, submap.hdrTRE.westbound));

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

void ImgDump::print(const char *format, ...) {
  QMutexLocker lock(&mutex);
  va_list args;
  va_start(args, format);
  vfprintf(stdout, format, args);
  va_end(args);

  fflush(stdout);
}

void ImgDump::parseSubdivInfo(QFile &srcFile, submap_t &submap) {
  quint32 start = submap.subfiles[submap.isPseudoNT ? "GMP" : "TRE"].offset + submap.hdrTRE.size;
  quint32 end = submap.subfiles[submap.isPseudoNT ? "GMP" : "TRE"].offset + submap.hdrTRE.tre2_offset;
  if (end > start) {
    auto crh = readCopyrights(srcFile, start, end);
    copyrightsStr = QString("%1|%2").arg(crh.descr1).arg(crh.descr2);
  }

  srcFile.seek(submap.subfiles[submap.isPseudoNT ? "GMP" : "TRE"].offset + submap.hdrTRE.tre2_offset);
  QByteArray tre2 = srcFile.read(submap.hdrTRE.tre2_size);

  IMG::tre2_next_t *pTre2N = (IMG::tre2_next_t *)tre2.data();

  QVector<IMG::subdiv_t>::iterator subdiv = submap.subdivs.begin();
  QVector<IMG::subdiv_t>::iterator subdiv_prev = submap.subdivs.end();

  int mapLevelIdx = 0;
  if (submap.mapLevels.size() == 0) {
    throw Exception("Missing map levels");
  }

  quint32 nSubdiv = submap.mapLevels[mapLevelIdx].subdiv;
  // parse all 16 byte subdivision entries
  quint32 i;
  quint32 rgnoff = submap.hdrRGN.rgn1_offset;
  for (i = 0; i < submap.nSubdivsNext; ++i, --nSubdiv) {
    qint32 cx, cy;
    qint32 width, height;

    subdiv->maplevel = &submap.mapLevels[mapLevelIdx];
    subdiv->n = i + 1;
    subdiv->next = pTre2N->next;
    subdiv->terminate = TRE_SUBDIV_TERM(pTre2N);
    subdiv->rgn_start = pTre2N->rgn_offset[0] | pTre2N->rgn_offset[1] << 8 | pTre2N->rgn_offset[2] << 16 | (pTre2N->elements & 0x0F) << 24;
    subdiv->rgn_start += rgnoff;
    // skip if this is the first entry
    if (subdiv_prev != submap.subdivs.end()) {
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
  quint32 nSubdivs = submap.subdivs.size();
  qDebug() << "Total subdivs:" << nSubdivs;
  ++mapLevelIdx;
  // witch pointer to 14 byte subdivision sections
  IMG::tre2_t *pTre2L = pTre2N;
  // parse all 14 byte subdivision entries of last map level
  for (; i < nSubdivs; ++i) {
    qint32 cx, cy;
    qint32 width, height;
    subdiv->maplevel = &submap.mapLevels[mapLevelIdx];
    subdiv->n = i + 1;
    subdiv->next = 0;
    subdiv->terminate = TRE_SUBDIV_TERM(pTre2L);
    subdiv->rgn_start = pTre2L->rgn_offset[0] | pTre2L->rgn_offset[1] << 8 | pTre2L->rgn_offset[2] << 16 | (pTre2L->elements & 0x0F) << 24;
    if (subdiv->rgn_start >= submap.hdrRGN.rgn1_length) {
      qDebug() << "[WARN] Block size overflow:" << submap.name << i << nSubdivs << Qt::hex << subdiv->rgn_start << submap.hdrRGN.rgn1_length;
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
  if (checkSize != submap.hdrRGN.rgn1_length) {
    qDebug() << "[WARN] The block size check does not match:" << Qt::hex << checkSize << submap.hdrRGN.rgn1_length;
  }
}

// read extended type elements
void ImgDump::parseSubdivInfoExt(QFile &srcFile, submap_t &submap) {
  const quint16 rec_size = submap.hdrTRE.tre7_rec_size;
  const quint32 blockStart = submap.subfiles[submap.isPseudoNT ? "GMP" : "TRE"].offset + submap.hdrTRE.tre7_offset;

  quint32 rgnOffPolyg2 = submap.hdrRGN.pg2_offset;
  quint32 rgnLenPolyg2 = submap.hdrRGN.pg2_length;
  quint32 rgnOffPolyl2 = submap.hdrRGN.ln2_offset;
  quint32 rgnLenPolyl2 = submap.hdrRGN.ln2_length;
  quint32 rgnOffPoint2 = submap.hdrRGN.pt2_offset;
  quint32 rgnLenPoint2 = submap.hdrRGN.pt2_length;

  if (submap.isPseudoNT) {
    rgnOffPolyg2 -= submap.hdrRGN.rgn2_offset;
    rgnOffPolyl2 -= submap.hdrRGN.rgn2_offset;
    rgnOffPoint2 -= submap.hdrRGN.rgn2_offset;
  }

  auto subdiv = submap.subdivs.begin();
  auto subdiv_prev = submap.subdivs.end();
  if (submap.hdrTRE.size >= 0x9A && submap.hdrTRE.tre7_size && rec_size >= sizeof(IMG::tre7_t)) {
    srcFile.seek(blockStart);
    QByteArray subdiv2 = srcFile.read(submap.hdrTRE.tre7_size);
    auto *pSubDiv2 = (IMG::tre7_t *)subdiv2.data();

    bool skipPois = (rec_size != sizeof(IMG::tre7_t));

    subdiv = submap.subdivs.begin();
    subdiv_prev = submap.subdivs.begin();
    subdiv->offPolygons2 = pSubDiv2->offsetPolygons + rgnOffPolyg2;
    subdiv->offPolylines2 = pSubDiv2->offsetPolyline + rgnOffPolyl2;
    subdiv->offPoints2 = skipPois ? 0 : pSubDiv2->offsetPoints + rgnOffPoint2;

    ++subdiv;
    pSubDiv2 = reinterpret_cast<IMG::tre7_t *>((quint8 *)pSubDiv2 + rec_size);

    while (subdiv != submap.subdivs.end()) {
      subdiv->offPolygons2 = pSubDiv2->offsetPolygons + rgnOffPolyg2;
      subdiv->offPolylines2 = pSubDiv2->offsetPolyline + rgnOffPolyl2;
      subdiv->offPoints2 = skipPois ? 0 : pSubDiv2->offsetPoints + rgnOffPoint2;

      subdiv_prev->lenPolygons2 = subdiv->offPolygons2 - subdiv_prev->offPolygons2;
      subdiv_prev->lenPolylines2 = subdiv->offPolylines2 - subdiv_prev->offPolylines2;
      subdiv_prev->lenPoints2 = skipPois ? 0 : subdiv->offPoints2 - subdiv_prev->offPoints2;

      subdiv_prev = subdiv;

      ++subdiv;
      pSubDiv2 = reinterpret_cast<IMG::tre7_t *>((quint8 *)pSubDiv2 + rec_size);
    }

    subdiv_prev->lenPolygons2 = rgnOffPolyg2 + rgnLenPolyg2 - subdiv_prev->offPolygons2;
    subdiv_prev->lenPolylines2 = rgnOffPolyl2 + rgnLenPolyl2 - subdiv_prev->offPolylines2;
    subdiv_prev->lenPoints2 = skipPois ? 0 : rgnOffPoint2 + rgnLenPoint2 - subdiv_prev->offPoints2;
  }
}

void ImgDump::parseStringTable(QFile &srcFile, submap_t &submap) {
  if (!submap.subfiles.keys().contains("LBL")) {
    qDebug().noquote() << "Missing LBL subfile for submap" << submap.name;
    return;
  }

  quint32 offsetLbl1 = submap.subfiles[submap.isPseudoNT ? "GMP" : "LBL"].offset + submap.hdrLBL.lbl1_offset;
  quint32 offsetLbl6 = submap.subfiles[submap.isPseudoNT ? "GMP" : "LBL"].offset + submap.hdrLBL.lbl6_offset;

  quint16 codepage = 0;
  if (submap.hdrLBL.size > 0xAA) {
    codepage = submap.hdrLBL.codepage;
  }
  codepageStr = QString("%1").arg(codepage);
  codingStr = QString("%1").arg(submap.hdrLBL.coding);

  switch (submap.hdrLBL.coding) {
    case 0x06:  // ascii
      submap.strtbl = new StrTbl6(codepage, mask, this);
      break;

    case 0x09:  // cp0, latin1, cp1251, cp1252
      submap.strtbl = new StrTblUtf8(codepage, mask, this);
      break;
    case 0x0A:
    case 0x0B:  // cp65001, unicode, cp932, ms932
      qWarning() << "Not implemented LBL coding:" << Qt::hex << submap.hdrLBL.coding;
      break;

    default:
      qWarning() << "Unknown or wrong LBL coding:" << Qt::hex << submap.hdrLBL.coding;
  }

  if (nullptr != submap.strtbl) {
    submap.strtbl->registerLBL1(offsetLbl1, submap.hdrLBL.lbl1_length, submap.hdrLBL.addr_shift);
    submap.strtbl->registerLBL6(offsetLbl6, submap.hdrLBL.lbl6_length);
  }
}

void ImgDump::readObjects(QFile &srcFile) {
  // int numThreads = 4;

  int numThreads = 1;
  if (numThreads > 1) {
    QThreadPool::globalInstance()->setMaxThreadCount(numThreads);
  }

  bool isFirst = true;
  QFile dstFile;
  uint8_t filePart = 0;
  for (const submap_t &submap : submaps) {
    qDebug() << "Submap name:" << submap.name << "| Total subdivs:" << submap.subdivs.size();
    if (submap.subdivs.isEmpty()) {
      qDebug() << "Skip RGN decode for this submap (no subdivs)" << submap.name;
      continue;
    }

    QFileInfo ofInfo(outputFile);
    QString dir = ofInfo.path();
    QString fileName = QString("%1\\%2%3.%4").arg(dir).arg(ofInfo.baseName()).arg(splitSubmaps ? "-" + submap.name : "").arg(ofInfo.suffix());
    if (dstFile.isOpen()) {
      QFileInfo fi(dstFile);
      const bool isOversize = fi.size() > maxFileSize;
      if (!csvOutput) {
        if (isOversize) {
          ++filePart;
          fileName = QString("%1\\%2.part%3.%4").arg(dir).arg(ofInfo.baseName()).arg(filePart + 1).arg(ofInfo.suffix());
        }
        if (splitSubmaps || isOversize) {
          dstFile.flush();
          dstFile.close();
          dstFile.setFileName(fileName);
          if (!dstFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
            throw Exception("Error opening output file: " + dstFile.errorString());
          }
          writeHeader(dstFile, submap);

          if (isOversize && filePart == 1) {
            const QString firstPartName = QString("%1\\%2-part%3.%4").arg(dir).arg(ofInfo.baseName()).arg(filePart).arg(ofInfo.suffix());
            if (QFile::exists(firstPartName)) {
              QFile::remove(firstPartName);
            }
            QFile::rename(outputFile, firstPartName);
          }
        }
      }
    } else {
      dstFile.setFileName(fileName);
      if (!dstFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw Exception("Error opening output file: " + dstFile.errorString());
      }
      if (splitSubmaps) {
        writeHeader(dstFile, submap);
      } else if (isFirst) {
        isFirst = false;
        writeHeader(dstFile, submap);
      }
    }

    if (numThreads == 1) {
      processObjects(dstFile, srcFile, submap);
    } else {
      CSubmapTask *task = new CSubmapTask([this, &dstFile, &srcFile, &submap]() { processObjects(dstFile, srcFile, submap); });
      QThreadPool::globalInstance()->start(task);
    }

#ifdef SANITY_CHECK
    if (debugInfo) {
      if (hasExtLabelCount) {
        // qDebug() << "[INFO] Number of hasExtLabel:" << hasExtLabelCount;
        hasExtLabelCount = 0;
      }
      if (hasNet1LabelCount) {
        // qDebug() << "[INFO] Number of hasNet1Label:" << hasNet1LabelCount;
        hasNet1LabelCount = 0;
      }
      if (infoSkipDupePoint) {
        qDebug() << "[INFO] Number of skipped duplicate points:" << infoSkipDupePoint;
        infoSkipDupePoint = 0;
      }
      if (warnSkipOutside) {
        qDebug() << "[WARN] Number of objects outside subdiv area:" << warnSkipOutside;
        warnSkipOutside = 0;
      }
      if (warnSuspiciousSegment) {
        qDebug() << "[WARN] Number of suspicious segment between points:" << warnSuspiciousSegment;
        warnSuspiciousSegment = 0;
      }
      if (warnTotals) {
        qDebug() << "[WARN] Number of more then 50 errors in single polyline:" << warnTotals;
        warnTotals = 0;
      }
      if (warnPolyOversize) {
        qDebug() << "[WARN] Number of too long polyline (possibly bitstream error):" << warnPolyOversize;
        warnPolyOversize = 0;
      }
      if (warnInvalidType) {
        qDebug() << "[WARN] Number of invalid objects type:" << warnInvalidType;
        warnInvalidType = 0;
      }
      if (DebugCounters ::warnInvalidCoords) {
        qDebug() << "[WARN] Number of invalid coordinates:" << DebugCounters ::warnInvalidCoords;
        DebugCounters ::warnInvalidCoords = 0;
      }
    } else {
      hasExtLabelCount = 0;
      hasNet1LabelCount = 0;
      infoSkipDupePoint = 0;
      warnSkipOutside = 0;
      warnSuspiciousSegment = 0;
      warnTotals = 0;
      warnPolyOversize = 0;
      warnInvalidType = 0;
      DebugCounters ::warnInvalidCoords = 0;
    }
#endif
  }
  QThreadPool::globalInstance()->waitForDone();

  if (dstFile.isOpen()) {
    dstFile.flush();
    dstFile.close();
  }
}

void ImgDump::processObjects(QFile &dstFile, QFile &srcFile, const submap_t &submap) {
  try {
    QByteArray rgnData;
    if (submap.isPseudoNT) {
      srcFile.seek(submap.subfiles["RGN"].hdrOffset);
      rgnData = srcFile.read(submap.subfiles["RGN"].hdrSize);
      srcFile.seek(submap.subfiles["RGN"].bodyOffset);
      rgnData += srcFile.read(submap.subfiles["RGN"].bodySize);
      auto totalSize = submap.subfiles["RGN"].hdrSize + submap.subfiles["RGN"].bodySize;
    } else {
      srcFile.seek(submap.subfiles["RGN"].offset);
      rgnData = srcFile.read(submap.subfiles["RGN"].size);
    }

    if (rgnData.isEmpty()) {
      qDebug() << "[WARN] No RGN data";
      return;
    }

    for (const IMG::subdiv_t &subdiv : submap.subdivs) {
      // if (debugInfo) { subdiv.print(); }
      if (DebugCounters::warnInvalidCoords + warnInvalidType + warnSuspiciousSegment + warnSkipOutside + infoSkipDupePoint + warnTotals + warnPolyOversize > 500) {
        qDebug() << "[ERROR] Too many errors: wrong offsets or unknown format with extended headers.";
        break;
      }
      decodeRgn(dstFile, srcFile, subdiv, submap.strtbl, rgnData);
    }
    totalObjectsDecoded = totalPt + totalPo + totalLn + totalPg + totalPt2 + totalPo2 + totalLn2 + totalPg2;
    qDebug().noquote() << QString("Total decoded objects: %1 | RGN: %2 %3 %4 %5 %6 %7 %8 %9 | Objects: %10 %11 %12")
                              .arg(totalObjectsDecoded, -8)
                              .arg(totalPt, -8)
                              .arg(totalPo, -8)
                              .arg(totalLn, -8)
                              .arg(totalPg, -8)
                              .arg(totalPt2, -8)
                              .arg(totalPo2, -8)
                              .arg(totalLn2, -8)
                              .arg(totalPg2, -8)
                              .arg(totalPt + totalPo + totalPt2 + totalPo2, -8)
                              .arg(totalLn + totalLn2, -8)
                              .arg(totalPg + totalPg2, -8);
    totalPt = totalPo = totalLn = totalPg = totalPt2 = totalPo2 = totalLn2 = totalPg2 = 0;
  } catch (const Exception &e) {
    qDebug() << "Fatal error:" << e.msg;
  }
}

double ImgDump::calculatePolygonArea(const QPolygonF &polygon) {
  const int n = polygon.size();
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

bool ImgDump::isPolygonTinyFast(const QPolygonF &polygon, double minArea) {
  const int n = polygon.size();
  if (n < 3) return true;

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
bool ImgDump::isPolygonTinyUltraFast(const QPolygonF &polygon, double minDimension) {
  if (polygon.size() < 3) return true;

  const QRectF bounds = polygon.boundingRect();
  return (bounds.width() < minDimension || bounds.height() < minDimension);
}

void ImgDump::decodeRgn(QFile &dstFile, QFile &srcFile, const IMG::subdiv_t &subdiv, StrTbl *strtbl, const QByteArray &rgndata) {
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

  polytype_t polygons;
  polytype_t polylines;
  pointtype_t points;
  pointtype_t pois;

  const quint8 *pRawData = (quint8 *)rgndata.data();

  quint32 opnt = 0, opoi = 0, opline = 0, opgon = 0;
  quint32 objCnt = subdiv.hasPois + subdiv.hasPoints + subdiv.hasPolylines + subdiv.hasPolygons;

  quint16 *pOffset = (quint16 *)(pRawData + subdiv.rgn_start);

  // test for points
  if (subdiv.hasPoints) {
    opnt = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
  }

  // test for pois
  if (subdiv.hasPois) {
    if (opnt) {
      opoi = gar_load(uint16_t, *pOffset);
      opoi += subdiv.rgn_start;
      ++pOffset;
    } else {
      opoi = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
    }
  }

  // test for polylines
  if (subdiv.hasPolylines) {
    if (opnt || opoi) {
      opline = gar_load(uint16_t, *pOffset);
      opline += subdiv.rgn_start;
      ++pOffset;
    } else {
      opline = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
    }
  }

  // test for polygons
  if (subdiv.hasPolygons) {
    if (opnt || opoi || opline) {
      opgon = gar_load(uint16_t, *pOffset);
      opgon += subdiv.rgn_start;
      ++pOffset;
    } else {
      opgon = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
    }
  }

#ifdef DEBUG_SHOW_POLY_DATA_SUBDIV
  qDebug() << "Subdiv" << subdiv.level << subdiv.n << file.fileName() << "| addr:" << Qt::hex << subdiv.rgn_start << "-" << subdiv.rgn_end << "|" << opnt << opoi << opline
           << opgon;
#endif

  // decode points
  if (subdiv.hasPoints) {
    const quint8 *pData = pRawData + opnt;
    const quint8 *pEnd = pRawData + (opoi ? opoi : opline ? opline : opgon ? opgon : subdiv.rgn_end);
    while (pData < pEnd) {
      RgnPoint pt;
      pData += pt.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData);

      if (!subdiv.area.contains(pt.pos)) {
        // qDebug() << "[WARN] Skip points outside subdiv area:" << subdiv.area << pt.pos;
        ++warnSkipOutside;
        continue;
      }

      if (strtbl) {
        pt.isLbl6 ? strtbl->get(srcFile, pt.lbl_ptr, StrTbl::label_type::poi, pt.labels) : strtbl->get(srcFile, pt.lbl_ptr, StrTbl::label_type::lbl, pt.labels);
      }

      ++totalPt;
      points.push_back(pt);
    }
  }

  // decode pois
  if (subdiv.hasPois) {
    const quint8 *pData = pRawData + opoi;
    const quint8 *pEnd = pRawData + (opline ? opline : opgon ? opgon : subdiv.rgn_end);
    while (pData < pEnd) {
      RgnPoint po;
      pData += po.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData);

      if (!subdiv.area.contains(po.pos)) {
        // qDebug() << "[WARN] Skip pois outside subdiv area:" << subdiv.area << po.pos;
        ++warnSkipOutside;
        continue;
      }

      if (strtbl) {
        po.isLbl6 ? strtbl->get(srcFile, po.lbl_ptr, StrTbl::label_type::poi, po.labels) : strtbl->get(srcFile, po.lbl_ptr, StrTbl::label_type::lbl, po.labels);
      }

      ++totalPo;
      pois.push_back(po);
    }
  }

  // decode polylines
  if (subdiv.hasPolylines) {
    const quint8 *pData = pRawData + opline;
    const quint8 *pEnd = pRawData + (opgon ? opgon : subdiv.rgn_end);
    while (pData < pEnd) {
      RgnLine ln;
      pData += ln.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, true, pData, pEnd);

      if (isCompletelyOutside(ln.points, subdiv.area)) {
        // qDebug() << "[WARN] Skip polylines outside subdiv area:" << subdiv.area << ln.points.toList().first(10);
        ++warnSkipOutside;
        continue;
      }

      if (strtbl && ln.lbl_info) {
        if (ln.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[ln] 1 hasNet1Label" << Qt::hex << ln.lbl_info;
          strtbl->get(srcFile, ln.lbl_info, StrTbl::label_type::net, ln.labels);
        } else {
          strtbl->get(srcFile, ln.lbl_info, StrTbl::label_type::lbl, ln.labels);
        }
      } else {
        ;
      }
      hasExtLabelCount += ln.hasExtLabelCount;

      ++totalLn;
      polylines.push_back(ln);
    }
  }

  // decode polygons
  if (subdiv.hasPolygons) {
    const quint8 *pData = pRawData + opgon;
    const quint8 *pEnd = pRawData + subdiv.rgn_end;
    while (pData < pEnd) {
      RgnLine pg;
      pData += pg.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, false, pData, pEnd);

      if (isCompletelyOutside(pg.points, subdiv.area)) {
        // qDebug() << "[WARN] Skip polygons outside subdiv area:" << subdiv.area << pg.points.toList().first(10);
        ++warnSkipOutside;
        continue;
      }

      if (isPolygonTinyFast(pg.points)) {
        qDebug() << "[WARN] Polygon is too small 2, area:" << pg.points;
        continue;
      }

      if (strtbl && pg.lbl_info) {
        if (pg.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[pg] 1 hasNet1Label" << Qt::hex << pg.lbl_info;
          strtbl->get(srcFile, pg.lbl_info, StrTbl::label_type::net, pg.labels);
        } else {
          strtbl->get(srcFile, pg.lbl_info, StrTbl::label_type::lbl, pg.labels);
        }
      } else {
        ;
      }
      hasExtLabelCount += pg.hasExtLabelCount;

      ++totalPg;
      polygons.push_back(pg);
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
        ++warnSkipOutside;
        continue;
      }

      if (isPolygonTinyFast(pg.points)) {
        qDebug() << "[WARN] Polygon is too small 1, area:" << pg.points;
        continue;
      }

      if (strtbl && pg.lbl_info) {
        if (pg.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[pg] 2 hasNet1Label" << Qt::hex << pg.lbl_info;
          strtbl->get(srcFile, pg.lbl_info, StrTbl::label_type::net, pg.labels);
        } else {
          strtbl->get(srcFile, pg.lbl_info, StrTbl::label_type::lbl, pg.labels);
        }
      } else {
        ;
      }
      hasExtLabelCount += pg.hasExtLabelCount;

      ++totalPg2;
      polygons.push_back(pg);
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
        ++warnSkipOutside;
        continue;
      }

      if (strtbl && ln.lbl_info) {
        if (ln.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[ln] 2 hasNet1Label" << Qt::hex << ln.lbl_info;
          strtbl->get(srcFile, ln.lbl_info, StrTbl::label_type::net, ln.labels);
        } else {
          strtbl->get(srcFile, ln.lbl_info, StrTbl::label_type::lbl, ln.labels);
        }
      } else {
        ;
      }
      hasExtLabelCount += ln.hasExtLabelCount;

      ++totalLn2;
      polylines.push_back(ln);
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
        ++warnSkipOutside;
        continue;
      }

      if (strtbl) {
        pt.isLbl6 ? strtbl->get(srcFile, pt.lbl_ptr, StrTbl::label_type::poi, pt.labels) : strtbl->get(srcFile, pt.lbl_ptr, StrTbl::label_type::lbl, pt.labels);
      }

      ++totalPt2;  // or totalPo2?
      pois.push_back(pt);
    }
  }

  if (csvOutput) {
    writeCsv(dstFile, points, pois, polylines, polygons, subdiv.level);
  } else {
    writeMp(dstFile, points, pois, polylines, polygons, subdiv.level);
  }
}

void ImgDump::writeCsv(QFile &dstFile, pointtype_t &points, pointtype_t &pois, polytype_t &polylines, polytype_t &polygons, quint32 level) {
  int count;
  int ptErrors = 0;
  int poErrors = 0;
  int lnErrors = 0;
  int pgErrors = 0;

  count = 0;

  QString strPoints = "";
  for (const RgnPoint &pt : points) {
    ++count;

    if (pt.type <= 0) {
      qWarning() << "[pt] Invalid type" << Qt::hex << pt.type;
      ++warnInvalidType;
      continue;
    }

    strPoints += QString("pt\t%1\t%2\t%3\tPOINT(%4)\t-1\n").arg(pt.type).arg(pt.hasLabel() ? pt.labels.at(0) : "").arg(level).arg(convPtDegStr(pt.pos, true));
  }
  // dstFile.write(codec->fromUnicode(strPoints));
  dstFile.write(strPoints.toUtf8());
  dstFile.flush();

  count = 0;
  QString strPois = "";
  for (const RgnPoint &po : pois) {
    ++count;

    if (po.type <= 0) {
      qWarning() << "[po] Invalid type" << Qt::hex << po.type;
      ++warnInvalidType;
      continue;
    }

    strPois += QString("pt\t%1\t%2\t%3\tPOINT(%4)\t-1\n").arg(po.type).arg(po.hasLabel() ? po.labels.at(0) : "").arg(level).arg(convPtDegStr(po.pos, true));
  }
  // dstFile.write(codec->fromUnicode(strPois));
  dstFile.write(strPois.toUtf8());
  dstFile.flush();

  count = 0;
  QString strPolylines = "";
  for (const RgnLine &ln : polylines) {
    ++count;

    if (ln.type <= 0) {
      qWarning() << "[ln] Invalid type" << Qt::hex << ln.type;
      ++warnInvalidType;
      continue;
    }

    strPolylines += QString("ln\t%1\t%2\t%3\tLINESTRING(%4)\t-1\n").arg(ln.type).arg(ln.hasLabel() ? ln.labels.at(0) : "").arg(level).arg(convLnDegStr(ln.points, true, true));
  }
  // dstFile.write(codec->fromUnicode(strPolylines));
  dstFile.write(strPolylines.toUtf8());
  dstFile.flush();

  count = 0;
  QString strPolygons = "";
  for (const RgnLine &pg : polygons) {
    ++count;

    if (pg.type <= 0) {
      qWarning() << "[pg] Invalid type" << Qt::hex << pg.type;
      ++warnInvalidType;
      ++pgErrors;
      continue;
    }

    strPolygons += QString("pg\t%1\t%2\t%3\tPOLYGON(%4)\t-1\n").arg(pg.type).arg(pg.hasLabel() ? pg.labels.at(0) : "").arg(level).arg(convLnDegStr(pg.points, false, true));
  }
  // dstFile.write(codec->fromUnicode(strPolygons));
  dstFile.write(strPolygons.toUtf8());
  dstFile.flush();
}

void ImgDump::writeMp(QFile &dstFile, pointtype_t &points, pointtype_t &pois, polytype_t &polylines, polytype_t &polygons, quint32 level) {
  int count = 0;
  int poiErrors = 0;
  int ptErrors = 0;
  int lnErrors = 0;
  int pgErrors = 0;
  for (const RgnPoint &pt : points) {
    QString tmpPoints;
    ++count;

    if (pt.type <= 0) {
      // qWarning() << "[pt] Invalid type" << Qt::hex << pt.type;
      ++warnInvalidType;
      continue;
    }

    tmpPoints += QString("[POI]\nType=0x%1\n").arg(pt.type, 0, 16);

    if (pt.hasLabel()) {
      tmpPoints += QString("Label=%1\n").arg(pt.labels.at(0));
      // for (int i = 2; i < pt.labels.size(); ++i) {
      //   tmpPoints += QString("Label%1=%2\n").arg(i).arg(pt.labels.at(i - 1));
      // }
    }

    if (!pt.pos.isNull()) {
      const QString output = convPtDegStr(pt.pos);
      if (output.startsWith(";")) {
        qDebug() << QString("[W] %1").arg(output);
        tmpPoints += output + "\n";
      } else if (output.length() == 0) {
        continue;
      } else {
        tmpPoints += QString("Data%1=%2\n").arg(level).arg(output);
      }
    }
    tmpPoints += "[END]\n\n";

    dstFile.write(codec->fromUnicode(tmpPoints));
    // dstFile.write(tmpPoints.toUtf8());
    dstFile.flush();
  }

  count = 0;
  for (const RgnPoint &poi : pois) {
    QString tmpPois;
    ++count;

    if (poi.type <= 0) {
      // qWarning() << "[poi] Invalid type" << Qt::hex << poi.type;
      ++warnInvalidType;
      ++poiErrors;
      continue;
    }

    tmpPois += QString("[POI]\nType=0x%1\n").arg(poi.type, 0, 16);

    if (poi.hasLabel()) {
      tmpPois += QString("Label=%1\n").arg(poi.labels.at(0));
      // for (int i = 2; i < poi.labels.size(); ++i) {
      //   tmpPois += QString("Label%1=%2\n").arg(i).arg(poi.labels.at(i - 1));
      // }
    }

    if (!poi.pos.isNull()) {
      const QString output = convPtDegStr(poi.pos);
      if (output.startsWith(";")) {
        qDebug() << QString("[W] %1").arg(output);
        tmpPois += output;
      } else if (output.length() == 0) {
        continue;
      } else {
        tmpPois += QString("Data%1=%2\n").arg(level).arg(output);
      }
    }
    tmpPois += "[END]\n\n";

    dstFile.write(codec->fromUnicode(tmpPois));
    // dstFile.write(tmpPois.toUtf8());
    dstFile.flush();
  }

  count = 0;
  // qDebug() << "total polylines:" << polylines.length();
  for (const RgnLine &ln : polylines) {
    QString tmpPolylines;
    ++count;

    if (ln.type <= 0) {
      // qWarning() << "[ln] Invalid type" << Qt::hex << ln.type;
      ++warnInvalidType;
      continue;
    }

    tmpPolylines += QString("[POLYLINE]\nType=0x%1\n").arg(ln.type, 0, 16);

    if (ln.hasLabel()) {
      tmpPolylines += QString("Label=%1\n").arg(ln.labels.at(0));
      for (int i = 2; i < ln.labels.size(); ++i) {
        tmpPolylines += QString("Label%1=%2\n").arg(i).arg(ln.labels.at(i - 1));
      }
    }

    // useless for me
    // if (ln.direction) {
    // tmpPolylines += "DirIndicator=1\n";
    // }

    const QString output = convLnDegStr(ln.points, true);

    if (output.length() == 0) {
      continue;
    }

    tmpPolylines += QString("Data%1=%2\n").arg(level).arg(output);
    tmpPolylines += "[END]\n\n";

    dstFile.write(codec->fromUnicode(tmpPolylines));
    // dstFile.write(tmpPolylines.toUtf8());
    dstFile.flush();
  }

  count = 0;
  for (const RgnLine &pg : polygons) {
    QString tmpPolygons;
    ++count;

    if (pg.type <= 0) {
      // qWarning() << "[pg] Invalid type" << Qt::hex << pg.type;
      ++warnInvalidType;
      ++pgErrors;
      continue;
    }

    tmpPolygons += QString("[POLYGON]\nType=0x%1\n").arg(pg.type, 0, 16);

    if (pg.hasLabel()) {
      tmpPolygons += QString("Label=%1\n").arg(pg.labels.at(0));
      // for (int i = 2; i < pg.labels.size(); ++i) {
      //   tmpPolygons += QString("Label%1=%2\n").arg(i).arg(pg.labels.at(i - 1));
      // }
    }

    const QString output = convLnDegStr(pg.points, false);
    if (output.length() == 0) {
      continue;
    }

    tmpPolygons += QString("Data%1=%2\n").arg(level).arg(output);
    tmpPolygons += "[END]\n\n";

    dstFile.write(codec->fromUnicode(tmpPolygons));
    // dstFile.write(tmpPolygons.toUtf8());
    dstFile.flush();
  }

  if (poiErrors) {
    // qWarning() << "[poi] Invalid type:" << poiErrors;
    ++warnInvalidType;
  }
}

QString ImgDump::convPtDegStr(const QPointF &pointRad, bool wkt) {
  QPointF pointDeg = toDegreesSafe(pointRad);
  const double lat = pointDeg.y();
  const double lng = pointDeg.x();
  thread_local char buffer[64];

  if (wkt) {
    snprintf(buffer, sizeof(buffer), "%.5f %.5f", lng, lat);
  } else {
    snprintf(buffer, sizeof(buffer), "(%.5f,%.5f)", lat, lng);
  }

  return QString::fromLatin1(buffer);
}

QString ImgDump::convLnDegStr(const QPolygonF &polyline, bool isLine, bool wkt) {
  QString result;

  // probably too high a value? (256?)
  if (polyline.size() > 8000) {
    // qDebug() << "[WARN] Too long polyline?" << polyline.size();
    ++warnPolyOversize;
    return "";
  }
  result.reserve(polyline.size() * 20);

  QPointF pointPrev;
  QPointF firstPoint;
  quint8 pointCount = 0;
  quint8 polyErrors = 0;

  for (const QPointF &point : polyline) {
    if (polyErrors > 50) {
      ++warnTotals;
      // qDebug() << "[WARN] More then 50 errors:" << totalErrors;
      return "";
    }

    if (pointCount == 0) {
      firstPoint = point;
    } else if (pointPrev == point) {
      // qDebug() << "[INFO] Skipping next duplicate point\n";
      ++infoSkipDupePoint;
      continue;
    }

    // @investigate: cut the polygon if it forms a figure eight?
    // @investigate: removes the last point if it matches the first one (closed polygon)
    if (isLine == false && pointCount > 2 && point == firstPoint) {
      break;
    }

#ifdef SANITY_CHECK
    if (pointCount && isSuspiciousSegment(pointPrev, point)) {
      // qDebug() << "[WARN] Suspicious segment between points:" << pointPrev << point << pointCount << "/" << polyline.size();
      ++warnSuspiciousSegment;
      ++polyErrors;
      break;
    }
#endif

    if (pointCount > 0) {
      result += wkt ? ", " : ",";
    }

    pointPrev = point;
    ++pointCount;

    result += convPtDegStr(point, wkt);
  }

  // @investigate: a polygon with 3 points sounds dubious
  if ((isLine && pointCount < 2) || (!isLine && pointCount < 3)) {
    // qDebug() << "[WARN] Does not make much sense: insufficient points";
    return "";
  }

  // if (wkt && !isLine) {
  //   result += ", " + convPtDegStr(firstPoint, wkt);
  // }

  return result;
}

void ImgDump::writeHeader(QFile &dstFile, const submap_t &submap) {
  if (csvOutput) {
    const QString headerStr = "Feature\tType\tLabel\tLevel\tWKT\tRoadID\n";
    // dstFile.write(codec->fromUnicode(headerStr));
    dstFile.write(headerStr.toUtf8());
    dstFile.flush();
    return;
  }

  QString idStr = "";
  QMap<QString, QString> levelZoom;
  QString zoomsStr = "";
  QString levelsStr = "";

  for (const auto &ml : submap.mapLevels) {
    levelZoom.insert(QVariant(ml.zoom()).toString(), QVariant(ml.bits).toString());
  }

  idStr = submap.name;

  int index = 0;
  for (auto [key, value] : levelZoom.asKeyValueRange()) {
    levelsStr += QString("Level%1=%2\n").arg(index).arg(value);
    zoomsStr += QString("Zoom%1=%2\n").arg(index).arg(key);
    ++index;
  }

  const auto headerStr = QString(
                             "; Generated by qgimgdec 1.0.0\n\n"
                             "[IMG ID]\n"
                             "CodePage=%1\n"
                             "LblCoding=%2\n"
                             "ID=%3\n"
                             "Name=%4\n"
                             "Preprocess=G\n"
                             "TreSize=8096\n"
                             "TreMargin=0.00000\n"
                             "RgnLimit=1024\n"
                             "POIIndex=N\n"
                             "POINumberFirst=N\n"
                             "POIZipFirst=N\n"
                             "MG=N\n"
                             "Routing=N\n"
                             "Copyright=%5\n"
                             "Levels=%6\n%7%8"
                             "[END-IMG ID]\n\n")
                             .arg(codepageStr)
                             .arg(codingStr)
                             .arg(idStr)
                             .arg(nameStr)
                             .arg(copyrightsStr)
                             .arg(levelZoom.count())
                             .arg(levelsStr)
                             .arg(zoomsStr);

  dstFile.write(codec->fromUnicode(headerStr));
  // dstFile.write(headerStr.toUtf8());
  dstFile.flush();
}
