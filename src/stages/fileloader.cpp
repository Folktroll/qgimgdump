#include "fileloader.h"

#include <QDebug>

#include "context.h"
#include "exception.h"
#include "misc.h"
#include "structs/base_header.h"
#include "structs/fat_header.h"
#include "structs/img_headers.h"

using namespace App;

FileLoader::FileLoader(Ctx &ctx) : ctx(ctx) {
  // @todo: as arg: config.codec
  ctx.codec = QTextCodec::codecForName("Windows-1251");
  if (!ctx.codec) {
    qWarning("CP1251 codec not available");
  }
}

void FileLoader::readFat() const {
  // QFile &srcFile
  ctx.submaps.clear();

  ImgHdr::SSupp hdrSupp;
  ctx.io.srcFile.seek(0);
  ctx.io.srcFile.read((char *)&hdrSupp, sizeof(ImgHdr::SSupp));
  ctx.io.srcFile.seek(hdrSupp.offsetFAT * 0x200);

  if (ctx.config.debugInfo) {
    misc::printf("----- IMG Header ----- size(%08X)\n", sizeof(ImgHdr::SSupp));
    hdrSupp.print();
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

  ctx.nameStr = hdrSupp.desc1.data();

  size_t blocksize = hdrSupp.blocksize();

  qDebug() << "---- FAT ----";
  ImgHdr::SFat FATBlock;
  ctx.io.srcFile.read((char *)&FATBlock, sizeof(ImgHdr::SFat));
  while (FATBlock.flag == 1) {
    if (ctx.io.srcFile.atEnd()) {
      throw Exception("Premature end of file.");
    }

    if (FATBlock.size != 0) {
      const auto &submapStr = FATBlock.name.data();
      // char submapStr[sizeof(FATBlock.name) + 1] = {0};
      // memcpy(submapStr, FATBlock.name, sizeof(FATBlock.name));

      const auto &subfileStr = FATBlock.type.data();
      // char subfileStr[sizeof(FATBlock.type) + 1] = {0};
      // memcpy(subfileStr, FATBlock.type, sizeof(FATBlock.type));

      if (submapStr[0] == 0x20) {
        qDebug() << "Skip empty subfile type:" << submapStr << subfileStr;
        // } else if (strcmp(submapStr, "00235022") != 0) {
        //   qDebug() << "Skip subfile by name:" << submapStr;
      } else if (ctx.submaps.contains(submapStr) && subfileStr != "GMP" && ctx.submaps[submapStr].subFiles.keys().contains(subfileStr)) {
        // or check FATBlock.part > 0x00 ?
        qDebug() << "Skip duplicate subfile type:" << submapStr << subfileStr;
      } else if (subfileStr == "SRT" || subfileStr == "MDR" || subfileStr == "MD2" || subfileStr == "TYP") {
        qDebug() << "Skip useless subfile type:" << submapStr << subfileStr;
        // } else if (subfileStr == "NET" || subfileStr == "NOD" || subfileStr == "LBL" || subfileStr == "MPS") {
        //   qDebug() << "Skip subfile type (for debug purpose):" << submapStr << subfileStr;
      } else {
        SSubMap &submap = ctx.submaps[submapStr];
        submap.name = submapStr;

        SSubFile &part = submap.subFiles[subfileStr];
        part.size = gar_load(quint32, FATBlock.size);
        part.offset = (quint32)(gar_load(uint16_t, FATBlock.blocks[0]) * blocksize);

        // @todo: more checks, maybe NT or checl 0x0D flags for bit 0x80
        submap.isPseudoNt = subfileStr == "GMP";
      }
    }

    ctx.io.srcFile.read((char *)&FATBlock, sizeof(ImgHdr::SFat));
  }
}

FileLoader::~FileLoader() = default;
