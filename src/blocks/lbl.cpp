#include "lbl.h"

#include <QDebug>
#include <QFile>
#include <QObject>
#include <QtCore5Compat/QTextCodec>

#include "misc.h"

using namespace App;

StrTbl::StrTbl(Ctx &ctx) : ctx(ctx), srcFile(*ctx.io.srcFile) {
  if (ctx.codepage != 0) {
    std::string strcp;
    if (1250 <= ctx.codepage && ctx.codepage <= 1258) {
      strcp = "Windows-" + std::to_string(ctx.codepage);
    } else if (ctx.codepage == 950) {
      strcp = "Big5";
    } else if (ctx.codepage == 850) {
      strcp = "IBM 850";
    } else if (ctx.codepage == 65001) {
      strcp = "UTF-8";
    } else {
      qDebug() << "Unknown codepage:" << ctx.codepage << "0x" << Qt::hex << ctx.codepage;
      strcp = "Latin1";
    }
    ctx.codec = QTextCodec::codecForName(strcp.c_str());
  }

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
}

StrTbl::~StrTbl() = default;

void StrTbl::registerLbl1(const quint32 offset, const quint32 size, const quint8 shift) {
  offsetLbl1 = offset;
  sizeLbl1 = size;
  addrShift1 = shift;
}
void StrTbl::registerLbl6(const quint32 offset, const quint32 size) {
  offsetLbl6 = offset;
  sizeLbl6 = size;
}
void StrTbl::registerNet1(const quint32 offset, const quint32 size, const quint8 shift) {
  offsetNet1 = offset;
  sizeNet1 = size;
  addrShift2 = shift;
}

void StrTbl::readFile(quint32 offset, quint32 size, QByteArray &data) const {
  if (offset + size > static_cast<quint32>(srcFile.size())) {
    data.clear();
    qWarning() << "[WARN] Offset and size exceed file size. Aborting read operation.";
    return;
  }

  if (!srcFile.seek(offset)) {
    data.clear();
    qWarning() << "[WARN] Failed to seek to offset" << offset << ". Aborting read operation.";
    return;
  }

  data = srcFile.read(size);
  if (data.size() != static_cast<qint64>(size)) {
    return;
  }

  // no xor is necessary
  if (ctx.mask.x8 == 0) {
    return;
  }

  // quint64 *p64 = (quint64 *)data.data();
  // for (quint32 i = 0; i < size / 8; ++i)
  // {
  // *p64++ ^= mask64;
  // }
  // quint32 rest = size % 8;
  // quint8 *p = (quint8 *)p64;

  // for (quint32 i = 0; i < rest; ++i)
  // {
  // *p++ ^= mask;
  // }

  char *rawData = data.data();
  const quint64 m64 = quint64(ctx.mask.x8) * 0x0101010101010101ULL;

  quint32 processed = 0;
  while (processed + 8 <= size) {
    quint64 value;
    std::memcpy(&value, rawData + processed, 8);
    value ^= m64;
    std::memcpy(rawData + processed, &value, 8);
    processed += 8;
  }

  while (processed < size) {
    rawData[processed] ^= ctx.mask.x8;
    ++processed;
  }
}

quint32 StrTbl::getNewOffset(const quint32 offset, LabelType t) const {
  quint32 newOffset = offset;

  if (t == LabelType::poi) {
    QByteArray buf;
    readFile(offsetLbl6 + offset, sizeof(quint32), buf);
    newOffset = gar_ptr_load(uint32_t, buf.data());
    newOffset = (newOffset & 0x003FFFFF);
  } else if (t == LabelType::net) {
    if (offsetNet1 == 0) {
      return 0xFFFFFFFF;
    }

    QByteArray data;
    readFile(offsetNet1 + (offset << addrShift2), sizeof(quint32), data);
    newOffset = gar_ptr_load(uint32_t, data.data());
    if (newOffset & 0x00400000) {
      return 0xFFFFFFFF;
    }
    newOffset = (newOffset & 0x003FFFFF);
  }

  newOffset <<= addrShift1;
  return newOffset;
}

// QString StrTbl::processLabel(std::string buf, unsigned lastSeperator) const {
QString StrTbl::processLabel(const char *buf, unsigned lastSeperator) const {
  QString label;
  if (ctx.codepage != 0) {
    // label = ctx.codec->toUnicode(buf.data());
    label = ctx.codec->toUnicode(buf);
  } else {
    // label = buf.data();
    label = buf;
  }

  if (lastSeperator == 0x1F) {
    bool ok = false;
    // double ele = label.toDouble(&ok);
    if (ok) {
      QString val;
      QString unit;
      label = val + " " + unit;
    }
  }
  return label;
}

StrTbl6::StrTbl6(Ctx &ctx) : StrTbl(ctx) {};

StrTbl6::~StrTbl6() = default;

void StrTbl6::get(quint32 offset, LabelType t, QStringList &labels) {
  labels.clear();

  offset = getNewOffset(offset, t);

  if (offset == 0xFFFFFFFF) {
    return;
  }

  if (offset > sizeLbl1) {
    return;
  }

  // local buf to avoid relying on external/global buf type
  // std::string buf;
  char buf[256];
  quint8 c1 = 0;
  quint8 c2 = 0;
  quint32 idx = 0;
  reg = 0;
  bits = 0;

  QByteArray data;
  quint32 size = (sizeLbl1 - offset) < 200 ? (sizeLbl1 - offset) : 200;

  readFile(offsetLbl1 + offset, size, data);

  p = (quint8 *)data.data();

  fill();

  unsigned lastSeperator = 0;
  buf[0] = 0;
  while (idx < (sizeof(buf) - 1)) {
    c1 = reg >> 26;
    reg <<= 6;
    bits -= 6;
    fill();
    // terminator
    if (c1 > 0x2F) {
      break;
    }

    c2 = str6tbl1[c1];
    if (c2 == 0) {
      if (c1 == 0x1C) {
        c1 = reg >> 26;
        reg <<= 6;
        bits -= 6;
        fill();
        buf[idx] = str6tbl2[c1];
        idx++;
      } else if (c1 == 0x1B) {
        c1 = reg >> 26;
        reg <<= 6;
        bits -= 6;
        fill();
        buf[idx] = str6tbl0[c1];
        idx++;
      } else if (c1 > 0x1C && c1 < 0x20) {
        lastSeperator = c1;
        buf[idx] = 0;
        // if (buf.length()) {
        if (strlen(buf)) {
          labels << processLabel(buf, lastSeperator);
        }
        idx = 0;
        buf[0] = 0;
      }
    } else {
      buf[idx] = str6tbl1[c1];
      idx++;
    }
  }

  buf[idx] = 0;
  // if (buf.length()) {
  if (strlen(buf)) {
    labels << processLabel(buf, lastSeperator);
  }
}

void StrTbl6::fill() {
  quint32 tmp;
  if (bits < 6) {
    tmp = *p++;
    reg |= tmp << (24 - bits);
    bits += 8;
  }
}

StrTbl9::StrTbl9(Ctx &ctx) : StrTbl(ctx) {};

StrTbl9::~StrTbl9() = default;

void StrTbl9::get(quint32 offset, LabelType t, QStringList &labels) {
  labels.clear();
  offset = getNewOffset(offset, t);

  if (offset == 0xFFFFFFFF) {
    return;
  }

  // qDebug() << "get()" << Qt::hex << offset;

  if (offset > sizeLbl1) {
    return;
  }

  QByteArray data;
  quint32 size = (sizeLbl1 - offset) < 200 ? (sizeLbl1 - offset) : 200;
  readFile(offsetLbl1 + offset, size, data);
  // const char *lbl = data.data();
  const char *lbl = data.data();

  // char *pBuffer = buf.data();
  // std::string buf;
  char buf[256];
  char *pBuffer = buf;
  *pBuffer = 0;

  unsigned lastSeperator = 0;
  while (*lbl != 0) {
    if ((unsigned)*lbl >= 0x1B && (unsigned)*lbl <= 0x1F) {
      lastSeperator = *lbl;
      *pBuffer = 0;
      // if (buf.length()) {
      if (strlen(buf)) {
        labels << processLabel(buf, lastSeperator);
        // pBuffer = buf.data();
        // reset local buf pointer
        pBuffer = buf;
        *pBuffer = 0;
      }
      ++lbl;
      continue;
    } else if ((unsigned)*lbl < 0x07) {
      ++lbl;
      continue;
    } else {
      *pBuffer++ = *lbl++;
    }
  }

  *pBuffer = 0;
  // if (buf.length()) {
  if (strlen(buf)) {
    // qDebug() << "buf.length()" << buf.length();
    // qDebug() << "buf.length()" << strlen(buf);
    labels << processLabel(buf, lastSeperator);
  }
}
