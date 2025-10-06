#include "lbl.h"

#include <QDebug>
#include <QFile>
#include <QObject>
#include <QTextCodec>

#include "misc.h"

StrTbl::StrTbl(const quint16 codepage, const quint8 mask, QObject *parent) : QObject(parent), codepage(codepage), mask(mask) {
  if (codepage != 0) {
    if (1250 <= codepage && codepage <= 1258) {
      char strcp[64];
      sprintf(strcp, "Windows-%i", codepage);
      codec = QTextCodec::codecForName(strcp);
    } else if (codepage == 950) {
      codec = QTextCodec::codecForName("Big5");
    } else if (codepage == 850) {
      codec = QTextCodec::codecForName("IBM 850");
    } else if (codepage == 65001) {
      codec = QTextCodec::codecForName("UTF-8");
    } else {
      qDebug() << "Unknown codepage:" << codepage << "0x" << Qt::hex << codepage;
      codec = QTextCodec::codecForName("Latin1");
    }
  }

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
}

StrTbl::~StrTbl() = default;

void StrTbl::registerLBL1(const quint32 offset, const quint32 size, const quint8 shift) {
  offsetLBL1 = offset;
  sizeLBL1 = size;
  addrshift1 = shift;
}
void StrTbl::registerLBL6(const quint32 offset, const quint32 size) {
  offsetLBL6 = offset;
  sizeLBL6 = size;
}
void StrTbl::registerNET1(const quint32 offset, const quint32 size, const quint8 shift) {
  offsetNET1 = offset;
  sizeNET1 = size;
  addrshift2 = shift;
}

void StrTbl::readFile(QFile &srcFile, quint32 offset, quint32 size, QByteArray &data) {
  if (offset + size > static_cast<quint32>(srcFile.size())) {
    data.clear();
    return;
  }

  if (!srcFile.seek(offset)) {
    data.clear();
    return;
  }

  data = srcFile.read(size);
  if (data.size() != static_cast<qint64>(size)) {
    return;
  }

  // if mask == 0, no xor is necessary
  if (mask == 0) {
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
  const quint64 mask64 = quint64(mask) * 0x0101010101010101ULL;

  quint32 processed = 0;
  while (processed + 8 <= size) {
    quint64 value;
    std::memcpy(&value, rawData + processed, 8);
    value ^= mask64;
    std::memcpy(rawData + processed, &value, 8);
    processed += 8;
  }

  while (processed < size) {
    rawData[processed] ^= mask;
    ++processed;
  }
}

quint32 StrTbl::getNewOffset(QFile &srcFile, const quint32 offset, label_type t) {
  quint32 newOffset = offset;

  if (t == label_type::poi) {
    QByteArray buffer;
    readFile(srcFile, offsetLBL6 + offset, sizeof(quint32), buffer);
    newOffset = gar_ptr_load(uint32_t, buffer.data());
    newOffset = (newOffset & 0x003FFFFF);
  } else if (t == label_type::net) {
    if (offsetNET1 == 0) {
      return 0xFFFFFFFF;
    }

    QByteArray data;
    readFile(srcFile, offsetNET1 + (offset << addrshift2), sizeof(quint32), data);
    newOffset = gar_ptr_load(uint32_t, data.data());
    if (newOffset & 0x00400000) {
      return 0xFFFFFFFF;
    }
    newOffset = (newOffset & 0x003FFFFF);
  }

  newOffset <<= addrshift1;
  return newOffset;
}

QString StrTbl::processLabel(const char *buffer, unsigned lastSeperator) const {
  QString label;
  if (codepage != 0) {
    label = codec->toUnicode(buffer);
  } else {
    label = buffer;
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

StrTbl6::StrTbl6(const quint16 codepage, const quint8 mask, QObject *parent) : StrTbl(codepage, mask, parent) {};

StrTbl6::~StrTbl6() = default;

void StrTbl6::get(QFile &srcFile, quint32 offset, label_type t, QStringList &labels) {
  labels.clear();

  offset = getNewOffset(srcFile, offset, t);

  if (offset == 0xFFFFFFFF) {
    return;
  }

  if (offset > sizeLBL1) {
    return;
  }

  quint8 c1 = 0;
  quint8 c2 = 0;
  quint32 idx = 0;
  reg = 0;
  bits = 0;

  QByteArray data;
  quint32 size = (sizeLBL1 - offset) < 200 ? (sizeLBL1 - offset) : 200;

  readFile(srcFile, offsetLBL1 + offset, size, data);

  p = (quint8 *)data.data();

  fill();

  unsigned lastSeperator = 0;
  while (idx < (sizeof(buffer) - 1)) {
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
        buffer[idx++] = str6tbl2[c1];
      } else if (c1 == 0x1B) {
        c1 = reg >> 26;
        reg <<= 6;
        bits -= 6;
        fill();
        buffer[idx++] = str6tbl3[c1];
      } else if (c1 > 0x1C && c1 < 0x20) {
        lastSeperator = c1;
        buffer[idx] = 0;
        if (strlen(buffer)) {
          labels << processLabel(buffer, lastSeperator);
        }
        idx = 0;
        buffer[0] = 0;
      }
    } else {
      buffer[idx++] = str6tbl1[c1];
    }
  }

  buffer[idx] = 0;
  if (strlen(buffer)) {
    labels << processLabel(buffer, lastSeperator);
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

StrTblUtf8::StrTblUtf8(const quint16 codepage, const quint8 mask, QObject *parent) : StrTbl(codepage, mask, parent) {};

StrTblUtf8::~StrTblUtf8() = default;

void StrTblUtf8::get(QFile &srcFile, quint32 offset, label_type t, QStringList &labels) {
  labels.clear();
  offset = getNewOffset(srcFile, offset, t);

  if (offset == 0xFFFFFFFF) {
    return;
  }

  if (offset > sizeLBL1) {
    return;
  }

  QByteArray data;
  quint32 size = (sizeLBL1 - offset) < 200 ? (sizeLBL1 - offset) : 200;
  readFile(srcFile, offsetLBL1 + offset, size, data);
  char *lbl = data.data();

  char *pBuffer = buffer;
  *pBuffer = 0;

  unsigned lastSeperator = 0;
  while (*lbl != 0) {
    if ((unsigned)*lbl >= 0x1B && (unsigned)*lbl <= 0x1F) {
      lastSeperator = *lbl;
      *pBuffer = 0;
      if (strlen(buffer)) {
        labels << processLabel(buffer, lastSeperator);
        pBuffer = buffer;
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
  if (strlen(buffer)) {
    labels << processLabel(buffer, lastSeperator);
  }
}
