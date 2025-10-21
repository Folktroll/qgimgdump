#include "fileloader.h"

#include <QDebug>
#include <algorithm>
#include <ranges>

#include "context.h"
#include "exception.h"
#include "hdr/block.h"
#include "hdr/fat.h"
#include "hdr/supp.h"
#include "misc.h"

using namespace App;

FileLoader::FileLoader(Ctx &ctx) : ctx(ctx), srcFile(*ctx.io.srcFile) {
  // @todo: as arg: config.codec
  ctx.codec = QTextCodec::codecForName("Windows-1251");
  if (!ctx.codec) {
    qWarning("CP1251 codec not available");
  }
}

void FileLoader::readFat() const {
  ctx.subMaps.clear();

  ImgHdr::SSupp hdrSupp;
  srcFile.seek(0);
  srcFile.read((char *)&hdrSupp, sizeof(ImgHdr::SSupp));
  srcFile.seek(hdrSupp.offsetFAT * 0x200);

  ctx.hdrSupp = hdrSupp;

  if (ctx.config.debugInfo) {
    misc::Logger::printf("----- IMG Header ----- size(%08X)\n", sizeof(ImgHdr::SSupp));
    hdrSupp.print();
    misc::Logger::print("");
  }

  ctx.mask.x8 = hdrSupp.xorByte;

  ctx.mask.x32 = ctx.mask.x8;
  ctx.mask.x32 <<= 8;
  ctx.mask.x32 |= ctx.mask.x8;
  ctx.mask.x32 <<= 8;
  ctx.mask.x32 |= ctx.mask.x8;
  ctx.mask.x32 <<= 8;
  ctx.mask.x32 |= ctx.mask.x8;

  ctx.mask.x64 = ctx.mask.x32;
  ctx.mask.x64 <<= 32;
  ctx.mask.x64 |= ctx.mask.x32;

  if (hdrSupp.identifier.data() == "GARMIN") {
    throw Exception("Bad file format");
  }

  size_t blockSize = hdrSupp.blockSize();

  ImgHdr::SFat hdrFat;
  srcFile.read((char *)&hdrFat, sizeof(ImgHdr::SFat));
  while (hdrFat.flag == 1) {
    if (srcFile.atEnd()) {
      throw Exception("Premature end of file.");
    }

    if (hdrFat.size != 0) {
      misc::Logger::print("---- FAT ----");
      hdrFat.print();

      const auto &subMapStr = misc::arrayToQString(hdrFat.name);
      const auto &subBlockStr = misc::arrayToQString(hdrFat.type);

      if (subMapStr.at(0) == " ") {
        // qDebug() << "Skip empty block type:" << subMapStr << subBlockStr;
        // } else if (strcmp(subMapStr, "00235022") != 0) {
        //   qDebug() << "Skip block by name:" << subMapStr;
      } else if (ctx.subMaps.contains(subMapStr) && subBlockStr != "GMP" && ctx.subMaps[subMapStr].subBlocks.keys().contains(subBlockStr)) {
        // or check FATBlock.part > 0x00 ?
        qDebug() << "Skip duplicate block type:" << subMapStr << subBlockStr;
      } else if (subBlockStr == "SRT" || subBlockStr == "MDR" || subBlockStr == "MD2" || subBlockStr == "TYP") {
        qDebug() << "Skip useless block type:" << subMapStr << subBlockStr;
      } else if (subBlockStr == "NET" || subBlockStr == "NOD" || subBlockStr == "LBL" || subBlockStr == "MPS") {
        qDebug() << "Skip block type (for debug purpose):" << subMapStr << subBlockStr;
      } else {
        SSubMap &subMap = ctx.subMaps[subMapStr];
        subMap.name = subMapStr;

        SSubBlock &part = subMap.subBlocks[subBlockStr];
        part.size = gar_load(quint32, hdrFat.size);
        part.offset = (quint32)(gar_load(uint16_t, hdrFat.blocks[0]) * blockSize);

        // @todo: more checks, maybe NT or check 0x0D flags for bit 0x80
        subMap.isPseudoNt = subBlockStr == "GMP";
      }
    }

    srcFile.read((char *)&hdrFat, sizeof(ImgHdr::SFat));
  }
}

FileLoader::~FileLoader() = default;
