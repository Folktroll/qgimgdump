// Copyright (C) 2006-2025 Original authors & contributors
// SPDX-License-Identifier: GPL-3.0-or-later

#include <QCommandLineOption>
#include <QCommandLineParser>
#include <QCoreApplication>
#include <QDebug>
#include <QElapsedTimer>
#include <QException>
#include <QFile>
#include <QFileInfo>
#include <QMutex>
#include <QMutexLocker>
#include <QPen>
#include <QPolygonF>
#include <QRegularExpression>
#include <QRunnable>
#include <QStack>
#include <QThreadPool>
#include <QtCore5Compat/QTextCodec>
#include <iostream>

#define SANITY_CHECK
#define DEBUG_SHOW_MAPLEVELS
// #define DEBUG_SHOW_POLY_DATA_SUBDIV
// #define DEBUG_SHOW_POLY_DATA_DECODE
// #define DEBUG_SHOW_POLY_DATA_DECODE_EXT

// 1 garmin unit = 360 / MAX_FLOAT_PREC

#define D180 180
#define MAX_FLOAT_PREC qPow(2.0, 24.0)
#define GARMIN_UNIT (2 * D180) / MAX_FLOAT_PREC
#define GARMIN_DEG(x) ((x) < 0x800000 ? (double)(x) * GARMIN_UNIT : (double)((x) - 0x1000000) * GARMIN_UNIT)
#define GARMIN_RAD(x) ((x) < 0x800000 ? (double)(x) * (2 * M_PI) / MAX_FLOAT_PREC : (double)((x) - 0x1000000) * (2 * M_PI) / MAX_FLOAT_PREC)

#define gar_load(t, x) (t)(x)
#define gar_ptr_load(t, p) __gar_ptr_load_##t((const uint8_t*)(p))
#define __gar_ptr_load_int16_t(p) (*((int16_t*)(p)))
#define __gar_ptr_load_uint16_t(p) (*((uint16_t*)(p)))
#define __gar_ptr_load_int24_t(p) (__gar_ptr_load_int32_t(p) & 0x00FFFFFFu)
#define __gar_ptr_load_uint24_t(p) (__gar_ptr_load_uint32_t(p) & 0x00FFFFFFu)
#define __gar_ptr_load_uint32_t(p) (*((uint32_t*)(p)))
#define __gar_ptr_load_int32_t(p) (*((int32_t*)(p)))

typedef quint8 quint24[3];

// PJ_UV.u: lng/E-W/x
// PJ_UV.v: lat/N-S/y
// typedef struct {
// double u, v;
// } PJ_UV;

struct sign_info_t {
  quint32 sign_info_bits;
  bool x_has_sign;
  bool nx;
  bool y_has_sign;
  bool ny;
  sign_info_t() : sign_info_bits(2), x_has_sign(true), nx(false), y_has_sign(true), ny(false) {}
};

void printInt8(const char* label, qint8 val) {
  printf("%30s %i\n", label, val);
}
void printUInt8(const char* label, quint8 val) {
  printf("%30s %02X\n", label, val);
}
void printInt16(const char* label, qint16 val) {
  printf("%30s %i\n", label, val);
}
void printUInt16(const char* label, quint16 val) {
  printf("%30s %04X\n", label, val);
}
void printInt32(const char* label, qint32 val) {
  printf("%30s %i\n", label, val);
}
void printUInt32(const char* label, quint32 val) {
  printf("%30s %08X\n", label, val);
}

void printUInt24(const char* label, quint24 val) {
  qint32 tmp = val[0] | val[1] << 8 | val[2] << 16;
  printf("%30s %f (0x%06X, %i)\n", label, GARMIN_DEG(tmp), tmp, tmp);
}

void printArrayUInt8(const char* label, quint8* array, int size) {
  printf("%30s ", label);
  for (int i = 0; i < size; i++) {
    printf("%02X ", array[i]);
  }
  printf("\n");
}

void printArrayInt8(const char* label, char* array, int size) {
  printf("%30s ", label);
  for (int i = 0; i < size; i++) {
    printf("%02X ", array[i]);
  }
  printf("| %s\n", QString(QByteArray(array, size)).toUtf8().data());
}

void printArrayInt8(const char* label, qint8* array, int size) {
  printf("%30s ", label);
  for (int i = 0; i < size; i++) {
    printf("%02X ", array[i]);
  }
  printf("\n");
}

using fSubmapTask = std::function<void()>;

static inline QString debugHex(const quint8* pData, int len) {
  if (!pData || pData == nullptr || len <= 0) {
    return QString();
  }

  QByteArray byteArray(reinterpret_cast<const char*>(pData), len);
  return byteArray.toHex(' ');
}

inline static int formatDouble(char* buffer, double value) {
  int intPart = static_cast<int>(value);
  int fracPart = static_cast<int>((value - intPart) * 100000 + 0.5);

  if (fracPart < 0) fracPart = -fracPart;

  return sprintf(buffer, "%d.%05d", intPart, fracPart);
}

inline QString roundToDigits(double value, int precision, int cut) {
  QString temp = QString::number(value, 'f', precision);
  int decimalPoint = temp.indexOf('.');
  if (decimalPoint != -1 && temp.length() > decimalPoint + cut + 1) {
    return temp.left(decimalPoint + cut + 1);
  }
  return temp;
}

class Exception : public QException {
 public:
  Exception(const QString& msg) : msg(msg) {}

  operator const QString&() const { return msg; }
  QString msg;
};

class CSubmapTask : public QRunnable {
 public:
  CSubmapTask(fSubmapTask task) : task(task) {}
  ~CSubmapTask() = default;

  void run() { task(); }

 private:
  fSubmapTask task;
};

class StrTbl : public QObject {
 public:
  StrTbl(const quint16 codepage, const quint8 mask, QObject* parent) : QObject(parent), codepage(codepage), mask(mask) {
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
  virtual ~StrTbl() {};

  enum label_type { lbl, poi, net };

  void registerLBL1(const quint32 offset, const quint32 size, const quint8 shift) {
    offsetLBL1 = offset;
    sizeLBL1 = size;
    addrshift1 = shift;
  }
  void registerLBL6(const quint32 offset, const quint32 size) {
    offsetLBL6 = offset;
    sizeLBL6 = size;
  }
  void registerNET1(const quint32 offset, const quint32 size, const quint8 shift) {
    offsetNET1 = offset;
    sizeNET1 = size;
    addrshift2 = shift;
  }

  virtual void get(QFile& srcFile, quint32 offset, label_type t, QStringList& info) = 0;

 protected:
  void readFile(QFile& srcFile, quint32 offset, quint32 size, QByteArray& data) {
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

    char* rawData = data.data();
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

  quint32 getNewOffset(QFile& srcFile, const quint32 offset, label_type t) {
    quint32 newOffset = offset;

    if (t == poi) {
      QByteArray buffer;
      readFile(srcFile, offsetLBL6 + offset, sizeof(quint32), buffer);
      newOffset = gar_ptr_load(uint32_t, buffer.data());
      newOffset = (newOffset & 0x003FFFFF);
    } else if (t == net) {
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

  QString processLabel(const char* buffer, unsigned lastSeperator) {
    QString label;
    if (codepage != 0) {
      label = codec->toUnicode(buffer);
    } else {
      label = buffer;
    }

    if (lastSeperator == 0x1F) {
      bool ok = false;
      double ele = label.toDouble(&ok);
      if (ok) {
        QString val, unit;
        label = val + " " + unit;
      }
    }
    return label;
  }

  quint32 offsetLBL1 = 0;
  quint32 sizeLBL1 = 0;
  quint32 offsetLBL6 = 0;
  quint32 sizeLBL6 = 0;
  quint32 offsetNET1 = 0;
  quint32 sizeNET1 = 0;
  quint8 addrshift1 = 0;
  quint8 addrshift2 = 0;
  quint16 codepage;
  QTextCodec* codec = nullptr;
  const quint8 mask;
  quint32 mask32;
  quint64 mask64;
  char buffer[1025];
};

class StrTbl6 : public StrTbl {
 public:
  StrTbl6(const quint16 codepage, const quint8 mask, QObject* parent) : StrTbl(codepage, mask, parent) {};
  virtual ~StrTbl6() {};

  void get(QFile& srcFile, quint32 offset, label_type t, QStringList& labels) override {
    labels.clear();

    offset = getNewOffset(srcFile, offset, t);

    if (offset == 0xFFFFFFFF) {
      return;
    }

    if (offset > (quint32)sizeLBL1) {
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

    p = (quint8*)data.data();

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

 private:
  inline static const char str6tbl1[] = {' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
                                         'X', 'Y', 'Z', 0,   0,   0,   0,   0,   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 0,   0,   0,   0,   0,   0};
  inline static const char str6tbl2[] = {'@', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', 0, 0, 0, 0,   0,    0,   0,   0,
                                         0,   0,   ':', ';', '<', '=', '>', '?',  0,   0,   0,   0,   0,   0,   0,   0,   0, 0, 0, '[', '\\', ']', '^', '_'};
  inline static const char str6tbl3[] = {'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};

  void fill() {
    quint32 tmp;
    if (bits < 6) {
      tmp = *p++;
      reg |= tmp << (24 - bits);
      bits += 8;
    }
  }

  quint32 reg = 0;            // temp shift reg buffer
  quint32 bits = 0;           // bits in buffer
  const quint8* p = nullptr;  // pointer to current data;
};

class StrTblUtf8 : public StrTbl {
 public:
  StrTblUtf8(const quint16 codepage, const quint8 mask, QObject* parent) : StrTbl(codepage, mask, parent) {};
  virtual ~StrTblUtf8() {};

  void get(QFile& srcFile, quint32 offset, label_type t, QStringList& labels) override {
    labels.clear();
    offset = getNewOffset(srcFile, offset, t);

    if (offset == 0xFFFFFFFF) {
      return;
    }

    if (offset > (quint32)sizeLBL1) {
      return;
    }

    QByteArray data;
    quint32 size = (sizeLBL1 - offset) < 200 ? (sizeLBL1 - offset) : 200;
    readFile(srcFile, offsetLBL1 + offset, size, data);
    char* lbl = data.data();

    char* pBuffer = buffer;
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
};

class RgnPoint {
 public:
  RgnPoint() = default;
  virtual ~RgnPoint() = default;
  quint32 decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8* pData);
  quint32 decodeExt(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8* pData, const quint8* pEnd);
  bool hasLabel() const { return !labels.isEmpty(); }
  quint32 type = 0;
  bool isLbl6 = false;
  bool hasSubType = false;
  QPointF pos;
  QStringList labels;
  quint32 lbl_ptr = 0xFFFFFFFF;
};

quint32 RgnPoint::decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8* pData) {
  type = (quint16)(*pData) << 8;

  ++pData;

  lbl_ptr = gar_ptr_load(uint24_t, pData);

  hasSubType = lbl_ptr & 0x00800000;
  isLbl6 = lbl_ptr & 0x00400000;
  lbl_ptr = lbl_ptr & 0x003FFFFF;

  pData += 3;

  qint16 dLng = gar_ptr_load(int16_t, pData);
  pData += 2;
  qint16 dLat = gar_ptr_load(int16_t, pData);
  pData += 2;

  qint32 x1 = ((qint32)dLng << shift) + iCenterLon;
  qint32 y1 = ((qint32)dLat << shift) + iCenterLat;
  pos = QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));

  if (hasSubType) {
    type |= *pData;
    return 9;
  }

  return 8;
}

quint32 RgnPoint::decodeExt(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8* pData, const quint8* pEnd) {
  quint32 byte_size = 6;
  quint8 subtype;

  // @todo: fixme: if (lbl3 & 0x80) == 128 -> contains a subtype
  // This is where Mechalas is incorrect: it is not the bit 8 of first byte, but bit 8 of the 4th byte
  type = (quint16)(*pData) << 8;
  ++pData;
  subtype = (quint16)(*pData);
  ++pData;

  type = 0x10000 + type + (subtype & 0x1F);

  /*
  subtype = (quint8)(pData[3]);
  pData += 2;
  */

  if (subtype & 0x80) {
    hasSubType = true;
    byte_size += 1;
  }

  qint16 dLng = gar_ptr_load(int16_t, pData);
  pData += 2;
  qint16 dLat = gar_ptr_load(int16_t, pData);
  pData += 2;

  qint32 x1, y1;

  x1 = ((qint32)dLng << shift) + iCenterLon;
  y1 = ((qint32)dLat << shift) + iCenterLat;
  pos = QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));

  if (subtype & 0x20) {
    byte_size += 3;
    lbl_ptr = gar_ptr_load(uint24_t, pData);
    isLbl6 = lbl_ptr & 0x00400000;
    lbl_ptr &= 0x003FFFFF;
  }

  return byte_size;
}
class BitstrReader {
 public:
  BitstrReader(const quint8* pData, quint32 n, quint32 bx, quint32 by, bool extra_bit, sign_info_t& si);
  bool get(qint32& x, qint32& y);

 private:
  void fill(quint32 bits);
  quint64 reg;            // the register to work on
  const quint8* pData;    // the data stream to get data from
  quint32 bytes;          // bytes left in stream
  quint32 xmask;          // bitmask x coord.
  quint32 ymask;          // bitmask y coord.
  qint32 xsign;           // sign bit for x value
  qint32 ysign;           // sign bit for y value
  qint32 xsign2;          // sign bit * 2 for x value
  qint32 ysign2;          // sign bit * 2 for y value
  quint8 bits;            // total bits in register
  quint8 bits_per_x;      // bits per x coord
  quint8 bits_of_byte;    // used bits of first byte
  quint8 bits_per_y;      // bits per y coord
  quint8 bits_per_coord;  // bits per coord.
  sign_info_t& sinfo;
  bool extraBit;
};

BitstrReader::BitstrReader(const quint8* pData, quint32 n, quint32 bx, quint32 by, bool extra_bit, sign_info_t& si)
    : reg(0),
      pData(pData),
      bytes(n),
      xmask(0xFFFFFFFF),
      ymask(0xFFFFFFFF),
      xsign(1),
      ysign(1),
      xsign2(2),
      ysign2(2),
      bits(0),
      bits_per_x(bx),
      bits_per_y(by),
      bits_per_coord(bx + by + (extra_bit ? 1 : 0)),
      sinfo(si),
      extraBit(extra_bit) {
  // create bit masks
  xmask = (xmask << (32 - bx)) >> (32 - bx);
  ymask = (ymask << (32 - by)) >> (32 - by);

  xsign <<= (bits_per_x - 1);
  ysign <<= (bits_per_y - 1);
  xsign2 = xsign << 1;
  ysign2 = ysign << 1;

  // add sufficient bytes for the first coord. pair
  fill(bits_per_coord + si.sign_info_bits);

  // get rid of sign setup bytes
  reg >>= si.sign_info_bits;
  bits -= si.sign_info_bits;
}

bool BitstrReader::get(qint32& x, qint32& y) {
  x = y = 0;
  if (bits < (bits_per_coord)) {
    return false;
  }

  // don't know what to do with it -> skip extra bit
  if (extraBit) {
    reg >>= 1;
    bits -= 1;
  }

  if (sinfo.x_has_sign) {
    qint32 tmp = 0;
    while (1) {
      tmp = reg & xmask;
      if (tmp != xsign) {
        break;
      }
      x += tmp - 1;
      reg >>= bits_per_x;
      bits -= bits_per_x;
      fill(bits_per_y + bits_per_x);
    }
    if (tmp < xsign) {
      x += tmp;
    } else {
      x = tmp - (xsign2)-x;
    }
  } else {
    x = reg & xmask;
    if (sinfo.nx) {
      x = -x;
    }
  }
  reg >>= bits_per_x;
  bits -= bits_per_x;

  // take y coord., add sign if neccessary, shift register by bits per y coord.
  if (sinfo.y_has_sign) {
    qint32 tmp = 0;
    while (1) {
      tmp = reg & ymask;
      if (tmp != ysign) {
        break;
      }
      y += tmp - 1;
      reg >>= bits_per_y;
      bits -= bits_per_y;
      fill(bits_per_y);
    }
    if (tmp < ysign) {
      y += tmp;
    } else {
      y = tmp - (ysign2)-y;
    }
  } else {
    y = reg & ymask;
    if (sinfo.ny) {
      y = -y;
    }
  }
  reg >>= bits_per_y;
  bits -= bits_per_y;

  // fill register until it has enought bits for one coord. pair again
  fill(bits_per_coord);
  return true;
}

void BitstrReader::fill(quint32 b) {
  quint64 tmp = 0;
  while ((bits < b) && bytes) {
#if (Q_BYTE_ORDER == Q_LITTLE_ENDIAN)
    (quint8&)tmp = *pData++;
#else
    tmp = *pData++;
#endif
    --bytes;

    reg |= tmp << bits;
    bits += 8;
  }
}

class RgnLine {
 public:
  RgnLine() : type(0), direction(false), lbl_info(0), hasNet1Label(false), hasExtLabel(false), dLng(0), dLat(0) {}
  virtual ~RgnLine() = default;

  quint32 decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8* pData, const quint8* pEnd);
  quint32 decodeExt(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8* pData, const quint8* pEnd);

  bool hasLabel() const { return !labels.isEmpty(); }

  quint32 type;
  bool direction;     // direction of line (polyline, only)
  quint32 lbl_info;   // the label offset
  bool hasNet1Label;  // true if label offset has to be used in NET subfile (up to four label pointers), @todo: missing labels
  QPolygonF points;   // the actual polyline points as longitude / latitude [rad]
  QStringList labels;
  quint8 hasExtLabelCount = 0;

 private:
  bool hasExtLabel;  // ext label
  qint16 dLng;       // delta longitude from subdivision center
  qint16 dLat;       // delta latitude from subdivision center
  // QVector<double> u;  // the actual polyline points as longitude / latitude [rad]
  // QVector<double> v;  // the actual polyline points as longitude / latitude [rad]
  // inline static qint32 maxVecSize = 0;
  static qint32 maxVecSize;
  QString debugData;
  void bitsPerCoord(quint8 base, quint8 bfirst, quint32& bx, quint32& by, sign_info_t& signinfo, bool isVer2);
  int bitsPerCoord(quint8 base, bool is_signed);
};

qint32 RgnLine::maxVecSize = 0;

quint32 RgnLine::decode(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8* pData, const quint8* pEnd) {
  quint32 bytes_total = 10;
  bool two_byte_len;   // bitstream has a two byte length
  bool extra_bit;      // coordinates use extra bit - ??? have never seen it
  quint16 bs_len = 0;  // bitstream length
  quint8 bs_info;      // base bit size info for coordinates
  quint32 bx;          // bits per x coord
  quint32 by;          // bits per y coord
  const quint8* const pStart = pData;

  labels.clear();
  points.resize(0);
  points.reserve(maxVecSize);
  // u.resize(0);
  // v.resize(0);
  // u.reserve(maxVecSize);
  // v.reserve(maxVecSize);

  /* poly_type
      for polylines:
      bit 0..5    type
      bit 6       direction
      for polygons:
      bit 0..6    type
      bit 7       bitstream_len is two bytes (true)
   */
  type = *pData++;

  two_byte_len = type & 0x80;
  if (line) {
    direction = (type & 0x40);
    type &= 0x3F;
  } else {
    type &= 0x7F;
  }

  /* label info
      bit 0..21   off set into LBL subfile
      bit 22      use extra bit for coordinates
      bit 23      use label data of NET subfile (net1)
   */
  lbl_info = gar_ptr_load(uint24_t, pData);
  hasNet1Label = lbl_info & 0x800000;
  extra_bit = lbl_info & 0x400000;
  lbl_info = lbl_info & 0x3FFFFF;

  pData += 3;

  // delta longitude and latitude
  dLng = gar_ptr_load(uint16_t, pData);
  pData += 2;
  dLat = gar_ptr_load(uint16_t, pData);
  pData += 2;

  // bitstream length
  if (two_byte_len) {
    bs_len = gar_ptr_load(uint16_t, pData);
    pData += 2;
    bytes_total += bs_len + 1;
  } else {
#if (Q_BYTE_ORDER == Q_LITTLE_ENDIAN)
    (quint8&)bs_len = *pData++;
#else
    bs_len = *pData++;
#endif
    bytes_total += bs_len;
  }

  if (pEnd && ((pStart + bytes_total) > pEnd)) {
    return bytes_total;
  }

  /* bitstream info
      bit 0..3    base bits longitude
      bit 4..7    base bits latitude
   */
  bs_info = *pData++;

  debugData = debugHex(pData, bs_len);

  // if(extra_bit) qWarning("extrabit");

#ifdef DEBUG_SHOW_POLY_DATA_DECODE
  qDebug() << "type:      " << type << Qt::hex;
  qDebug() << "two byte:  " << two_byte_len;
  qDebug() << "extra bit: " << extra_bit;
  qDebug() << "dLng:      " << dLng;
  qDebug() << "dLat:      " << dLat;
  qDebug() << "len:       " << bs_len;
  qDebug() << "info:      " << Qt::hex << bs_info;
  qDebug() << "1st byte:  " << Qt::hex << *pData;
  qDebug() << "bytes total" << bytes_total;
  // qDebug() << "data1      " << rawData;
#endif

  sign_info_t signinfo;
  bitsPerCoord(bs_info, *pData, bx, by, signinfo, false);

  BitstrReader sr(pData, bs_len, bx, by, extra_bit, signinfo);
  qint32 x1, y1, x = 0, y = 0;

  bool isNegative = (iCenterLon >= 0x800000);
  // first point
  x1 = ((qint32)dLng << shift) + iCenterLon;
  y1 = ((qint32)dLat << shift) + iCenterLat;

  if (x1 >= 0x800000 && !isNegative) {
    x1 = 0x7fffff;
  }

  // PJ_UV xy;
  // xy.u = GARMIN_RAD(x1);
  // xy.v = GARMIN_RAD(y1);
  // u << xy.u;
  // v << xy.v;
  // points << QPointF(qRadiansToDegrees(xy.u), qRadiansToDegrees(xy.v));
  points << QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));

  // next points
  while (sr.get(x, y)) {
    x1 += (x << shift);
    y1 += (y << shift);

    if (x1 >= 0x800000 && !isNegative) {
      x1 = 0x7fffff;
    }

    // xy.u = GARMIN_RAD(x1);
    // xy.v = GARMIN_RAD(y1);
    // u << xy.u;
    // v << xy.v;
    // points << QPointF(qRadiansToDegrees(xy.u), qRadiansToDegrees(xy.v));
    points << QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));
  }

  // if (maxVecSize < u.size()) {
  // maxVecSize = u.size();
  // }
  // if (u.size() * 1.2 < maxVecSize) {
  // u.squeeze();
  // v.squeeze();
  // }

  if (maxVecSize < points.size()) {
    maxVecSize = points.size();
  }
  if (points.size() * 1.2 < maxVecSize) {
    points.squeeze();
  }

  return bytes_total;
}

quint32 RgnLine::decodeExt(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8* pData, const quint8* pEnd) {
  quint32 bytes_total = 6;
  quint16 bs_len = 0;  // bitstream length
  quint32 subtype;     // type and subtype
  quint8 bs_info;      // base bit size info for coordinates
  quint32 bx;          // bits per x coord
  quint32 by;          // bits per y coord

  const quint8* const pStart = pData;

  labels.clear();
  points.resize(0);
  points.reserve(maxVecSize);
  // u.resize(0);
  // v.resize(0);
  // u.reserve(maxVecSize);
  // v.reserve(maxVecSize);

  type = *pData++;
  subtype = *pData++;

  type = 0x10000 + (quint16(type) << 8) + (subtype & 0x1f);
  hasExtLabel = subtype & 0x20;
  // delta longitude and latitude
  dLng = gar_ptr_load(uint16_t, pData);
  pData += 2;
  dLat = gar_ptr_load(uint16_t, pData);
  pData += 2;

  if ((*pData & 0x1) == 0) {
    bs_len = gar_ptr_load(uint16_t, pData);
    bs_len = (bs_len >> 2) - 1;
    pData += 2;
    bytes_total += 2;
  } else {
    bs_len = ((*pData) >> 1) - 1;
    pData += 1;
    bytes_total += 1;
  }

  bs_info = *pData++;
  bytes_total += bs_len + 1;

#ifdef DEBUG_SHOW_POLY_DATA_DECODE_EXT
  qDebug() << "type:      " << type << Qt::hex << type;
  qDebug() << "dLng:      " << dLng;
  qDebug() << "dLat:      " << dLat;
  qDebug() << "len:       " << bs_len;
  qDebug() << "info:      " << Qt::hex << bs_info;
  qDebug() << "1st byte:  " << Qt::hex << *pData;
  qDebug() << "bytes total" << bytes_total;
#endif

  sign_info_t signinfo;
  bitsPerCoord(bs_info, *pData, bx, by, signinfo, true);

  // qDebug() << ">>" << bs_len << bytes_total << (pEnd - pStart);

  if (((quint32)(pEnd - pStart)) < bytes_total) {
    return pEnd - pStart;
  }

  BitstrReader sr(pData, bs_len, bx, by, false, signinfo);
  qint32 x1, y1, x = 0, y = 0;

  bool isNegative = (iCenterLon >= 0x800000);
  // first point
  x1 = ((qint32)dLng << shift) + iCenterLon;
  y1 = ((qint32)dLat << shift) + iCenterLat;

  if (x1 >= 0x800000 && !isNegative) {
    x1 = 0x7fffff;
  }

  points << QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));

  // next points
  while (sr.get(x, y)) {
    x1 += (x << shift);
    y1 += (y << shift);

    if (x1 >= 0x800000 && !isNegative) {
      x1 = 0x7fffff;
    }

    // xy.u = GARMIN_RAD(x1);
    // xy.v = GARMIN_RAD(y1);
    // u << xy.u;
    // v << xy.v;
    // points << QPointF(qRadiansToDegrees(xy.u), qRadiansToDegrees(xy.v));
    points << QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));
  }

  if (hasExtLabel) {
    quint32 offset = gar_ptr_load(uint24_t, pData + bs_len);
    bytes_total += 3;
    // @todo: read label information
    lbl_info = offset & 0x3FFFFF;
    // qDebug() << "[INFO] hasExtLabel=true" << Qt::hex << offset << lbl_info;
    ++hasExtLabelCount;
  } else {
    lbl_info = 0;
  }

  // if (maxVecSize < u.size()) {
  // maxVecSize = u.size();
  // }
  // if (u.size() * 1.2 < maxVecSize) {
  // u.squeeze();
  // v.squeeze();
  // }

  if (maxVecSize < points.size()) {
    maxVecSize = points.size();
  }
  if (points.size() * 1.2 < maxVecSize) {
    points.squeeze();
  }

  return bytes_total;
}

void RgnLine::bitsPerCoord(quint8 base, quint8 bfirst, quint32& bx, quint32& by, sign_info_t& signinfo, bool isVer2) {
  bool x_sign_same, y_sign_same;

  quint8 mask = 0x1;

  // x_sign_same = bfirst & 0x1;
  x_sign_same = bfirst & mask;
  mask <<= 1;

  if (x_sign_same) {
    signinfo.x_has_sign = false;
    //   signinfo.nx         = bfirst & 0x2;
    signinfo.nx = bfirst & mask;
    mask <<= 1;
    ++signinfo.sign_info_bits;
  } else {
    signinfo.x_has_sign = true;
  }
  bx = bitsPerCoord(base & 0x0F, signinfo.x_has_sign);

  // y_sign_same = x_sign_same ? (bfirst & 0x04) : (bfirst & 0x02);
  y_sign_same = bfirst & mask;
  mask <<= 1;

  if (y_sign_same) {
    signinfo.y_has_sign = false;
    //   signinfo.ny         = x_sign_same ? bfirst & 0x08 : bfirst & 0x04;
    signinfo.ny = bfirst & mask;
    mask <<= 1;
    ++signinfo.sign_info_bits;
  } else {
    signinfo.y_has_sign = true;
  }

  by = bitsPerCoord((base >> 4) & 0x0F, signinfo.y_has_sign);

  // Determine extra bits.
  if (isVer2) {
    ++signinfo.sign_info_bits;
    if (bfirst & mask) {
      ++bx;
      ++by;
    }
  }
}

// extract bits per coordinate
int RgnLine::bitsPerCoord(quint8 base, bool is_signed) {
  int n = 2;

  if (base <= 9) {
    n += base;
  } else {
    n += (2 * base - 9);
  }

  if (is_signed) {
    ++n;
  }
  return n;
}

class ImgDump : public QCoreApplication {
 public:
  explicit ImgDump(int& argc, char** argv);
  virtual ~ImgDump();

 private:
  typedef QVector<RgnLine> polytype_t;
  typedef QVector<RgnPoint> pointtype_t;

#pragma pack(1)
  // Garmin IMG file header structure, to the start of the FAT blocks
  struct gmapsupp_imghdr_t {
    quint8 xorByte = 0;              // 0000
    quint8 x0001_0007[7] = {0};      // 0001..0007
    quint16 version = 0;             // 0008
    quint8 upMonth = 0;              // 000A
    quint8 upYear = 0;               // 000B
    quint8 x000C_000D[2] = {0};      // 000C..000D
    quint8 supp = 0;                 // 000E
    quint8 checksum = 0;             // 000F
    char signature[7] = {0};         // 0010..0016
    quint8 x0017 = 0x2;              // 0017
    quint16 sectors1 = 0;            // 0018..0019
    quint16 heads1 = 0;              // 001A..001B
    quint16 cylinders = 0;           // 001C..001D
    quint8 x001E_0038[27] = {0};     // 001E..0038
    qint16 year = 0;                 // 0039..003A
    qint8 month = 0;                 // 003B
    qint8 day = 0;                   // 003C
    qint8 hour = 0;                  // 003D
    qint8 minute = 0;                // 003E
    qint8 second = 0;                // 003F
    qint8 offsetFAT = 0;             // 0040
    char identifier[7];              // 0041..0047
    quint8 x0048;                    // 0048
    char desc1[20];                  // 0049..005C
    quint16 head2 = 0;               // 005D..005E
    quint16 sectors2 = 0;            // 005F..0060
    quint8 e1 = 0;                   // 0061
    quint8 e2 = 0;                   // 0062
    quint16 nBlocks1;                // 0063..0064
    char desc2[30];                  // 0065..0082
    quint8 x0083_01BE[0x13C] = {0};  // 0083..01BE
    quint8 startHead = 0;            // 01BF
    quint8 startSector = 1;          // 01C0
    quint8 startCylinder = 0;        // 01C1
    quint8 systemType = 0;           // 01C2
    quint8 endHead = 0;              // 01C3
    quint8 endSector = 0;            // 01C4
    quint8 endCylinder = 0;          // 01C5
    quint32 relSectors = 0;          // 01C6..01C9
    quint32 nSectors = 0;            // 01CA..01CD
    quint8 x01CE_01FD[0x30] = {0};   // 01CE..01FD
    quint16 terminator = 0xAA55;     // 01FE..01FF
    quint32 blocksize() { return 1 << (e1 + e2); }
    void print();
  };

  struct FATBlock_t {
    quint8 flag;            // 0000
    char name[8];           // 0001..0008
    char type[3];           // 0009..000B
    quint32 size;           // 000C..000F
    quint16 part;           // 0010..0011
    quint8 x0012_001F[14];  // 0012..001F
    quint16 blocks[240];    // 0020..01FF
  };

  // common header for all subtypes
  struct submap_hdr_t {
    quint16 size;   // 0000..0001
    char type[10];  // 0002..000B
    quint8 x000C;   // 000C
    quint8 flag;    // 000D
    qint16 year;    // 000E..000F
    qint8 month;    // 0010
    qint8 day;      // 0011
    qint8 hour;     // 0012
    qint8 minute;   // 0013
    qint8 second;   // 0014
    void print();
  };

  struct gmp_hdr_t : public submap_hdr_t {
    quint8 x0015_0018[4];  // 0015..0018
    quint32 tre_offset;    // 0019..001C
    quint32 rgn_offset;    // 001D..0020
    quint32 lbl_offset;    // 0021..0024
    quint32 net_offset;    // 0025..0028
    quint32 nod_offset;    // 0029..002E
    quint32 dem_offset;    // 002D..0030
    quint32 mar_offset;    // 0031..0034
    quint32 met_offset;    // 0035..0038
    void print();
  };

  struct hdr_tre_t : public submap_hdr_t {
    quint24 northbound = {0};            // 0015..0017 - max lat
    quint24 eastbound = {0};             // 0018..001A - max long
    quint24 southbound = {0};            // 001B..001D - min lat
    quint24 westbound = {0};             // 001E..0020 - min long, cant be +180
    quint32 tre1_offset = 0;             // 0021..0024 - map levels pos
    quint32 tre1_size = 0;               // 0025..0028
    quint32 tre2_offset = 0;             // 0029..002C - subdiv pos
    quint32 tre2_size = 0;               // 002D..0030
    quint32 tre3_offset = 0;             // 0031..0034 - TRE-body relative offset (pseudo-NT) or copyright offset in TRE
    quint32 tre3_size = 0;               // 0035..0038
    quint16 tre3_rec_size = 0;           // 0039..003A
    quint8 x003B_003E[4] = {0};          // 003B..003E
    quint8 POI_flags = 0;                // 003F       - poi display flags
    quint24 render_prio = {0x14, 0, 0};  // 0040..0042 - display priority
    quint8 x0043_0049[7] = {0};          // 0043..0049
    quint32 tre4_offset = 0;             // 004A..004D - polyline overview
    quint32 tre4_size = 0;               // 004E..0051
    quint16 tre4_rec_size = 0;           // 0052..0053
    quint8 x0054_0057[4] = {0};          // 0054..0057
    quint32 tre5_offset = 0;             // 0058..005B - polygon overview
    quint32 tre5_size = 0;               // 005C..005F
    quint16 tre5_rec_size = 0;           // 0060..0061
    quint8 x0062_0065[4] = {0};          // 0062..0065
    quint32 tre6_offset = 0;             // 0066..0069 - points overview
    quint32 tre6_size = 0;               // 006A..006D
    quint16 tre6_rec_size = 0;           // 006E..006F
    quint8 x0070_0073[4] = {0};          // 0070..0073
    quint32 map_id = 0;                  // 0074..0077
    quint8 x0078_007B[4] = {0};          // 0078..007B
    quint32 tre7_offset = 0;             // 007C..007F - extended type offsets
    quint32 tre7_size = 0;               // 0080..0083
    quint16 tre7_rec_size = 0;           // 0084..0085
    quint8 x0086_0089[4] = {0};          // 0086..0089 - 0x01 0x00 0x00 0x00
    quint32 tre8_offset = 0;             // 008A..008D - extended type overview: ln, pg, po; sorted by type (1 type 1 levels 1 subtype)
    quint32 tre8_size = 0;               // 008E..0091
    quint16 tre8_rec_size = 0;           // 0092..0093
    quint16 polyl2_types_num = 0;        // 0094..0095 - num ext type ln
    quint16 polyg2_types_num = 0;        // 0096..0097 - num ext type pg
    quint16 poi2_types_num = 0;          // 0098..0099 - num ext type pt
    quint8 key[16] = {0};                // 009A..00A5 - map values
    quint8 x00AA_00AD[4] = {0};          // 00AA..00AD
    quint32 tre9_offset;                 // 00AE..00B1
    quint32 tre9_size;                   // 00B2..00B5
    quint16 tre9_rec_size;               // 00B6..00B7
    quint8 x00B8_00BB[4] = {0};          // 00B8..00BB
    quint32 tre10_offset;                // 00BC..00BF
    quint32 tre10_size;                  // 00C0..00C3
    quint16 tre10_rec_size;              // 00C4..00C5
    quint8 x00C6_00CE[9] = {0};          // 00C6..00CE
    quint32 map_id2;                     // 00CF..00D2 - map id 2
    void print(quint32 offset);
  };

  struct hdr_rgn_t : public submap_hdr_t {
    quint32 rgn1_offset = 0;      // 0015..0018 - RGN-body relative offset (pseudo-NT)
    quint32 rgn1_length = 0;      // 0019..001C
    quint32 pg2_offset = 0;       // 001D..0020
    quint32 pg2_length = 0;       // 0021..0024
    quint8 x0025_0038[20] = {0};  // 0025..0038
    quint32 ln2_offset = 0;       // 0039..003C
    quint32 ln2_length = 0;       // 003D..0040
    quint8 x0041_0054[20] = {0};  // 0041..0054
    quint32 pt2_offset = 0;       // 0055..0058
    quint32 pt2_length = 0;       // 0059..005C
    quint8 x005D_0070[20] = {0};  // 005D..0070
    quint32 rgn2_offset = 0;      // 0071..0074
    quint32 rgn2_length = 0;      // 0075..0078
    quint32 unknown = 0;          // 0079..007C - E3 or E7
    void print(quint32 offset);
  };

  struct hdr_lbl_t : public submap_hdr_t {
    quint32 lbl1_offset = 0;     // 0015..0018 - sort description length
    quint32 lbl1_length = 0;     // 0019..001C - label size
    quint8 addr_shift = 0;       // 001D       - offset multiplier
    quint8 coding = 0;           // 001E       - encoding type
    quint32 lbl2_offset = 0;     // 001F..0022
    quint32 lbl2_length = 0;     // 0023..0026
    quint16 lbl2_rec_size = 0;   // 0027..0028
    quint8 x0029_002C[4] = {0};  // 0029..002C
    quint32 lbl3_offset = 0;     // 002D..0030
    quint32 lbl3_length = 0;     // 0031..0034
    quint16 lbl3_rec_size = 0;   // 0035..0036
    quint8 x0037_003A[4] = {0};  // 0037..003A
    quint32 lbl4_offset = 0;     // 003B..003E
    quint32 lbl4_length = 0;     // 003F..0042
    quint16 lbl4_rec_size = 0;   // 0043..0044
    quint8 x0045_0048[4] = {0};  // 0045..0048
    quint32 lbl5_offset = 0;     // 0049..004C
    quint32 lbl5_length = 0;     // 004D..0050
    quint16 lbl5_rec_size = 0;   // 0051..0052
    quint8 x0053_0056[4] = {0};  // 0053..0056
    quint32 lbl6_offset = 0;     // 0057..005A
    quint32 lbl6_length = 0;     // 005B..005E
    quint8 lbl6_addr_shift = 0;  // 005F
    quint8 lbl6_glob_mask = 0;   // 0060
    quint8 x0061_0063[3] = {0};  // 0061..0063
    quint32 lbl7_offset = 0;     // 0064..0067
    quint32 lbl7_length = 0;     // 0068..006B
    quint16 lbl7_rec_size = 0;   // 006C..006D
    quint8 x006E_0071[4] = {0};  // 006E..0071
    quint32 lbl8_offset = 0;     // 0072..0075
    quint32 lbl8_length = 0;     // 0076..0079
    quint16 lbl8_rec_size = 0;   // 007A..007B
    quint8 x007C_007F[4] = {0};  // 007C..007F
    quint32 lbl9_offset = 0;     // 0080..0083
    quint32 lbl9_length = 0;     // 0084..0087
    quint16 lbl9_rec_size = 0;   // 0088..0089
    quint8 x008A_008D[4] = {0};  // 008A..008D
    quint32 lbl10_offset = 0;    // 008E..0091
    quint32 lbl10_length = 0;    // 0092..0095
    quint16 lbl10_rec_size = 0;  // 0096..0097
    quint8 x0098_009B[4] = {0};  // 0098..009B
    quint32 lbl11_offset = 0;    // 009C..009F
    quint32 lbl11_length = 0;    // 00A0..00A3
    quint16 lbl11_rec_size = 0;  // 00A4..00A5
    quint8 x00A6_00A9[4] = {0};  // 00A6..00A9
    quint16 codepage = 0;        // 00AA..00AB - optional check length
    quint8 x00AC_00AF[4] = {0};  // 00AC..00AF - 0x07 0x00 0x02 0x80 or 0x12 0x00 0x01 0x80
    quint32 lbl12_offset = 0;    // 00B0..00B3 - LBL-body relative offset (pseudo-NT) or sort descriptor
    quint32 lbl12_length = 0;    // 00B4..00B7
    void print(quint32 offset);
  };

  struct hdr_net_t : public submap_hdr_t {
    quint32 net1_offset;     // 0015..0018 - NET-body relative offset (pseudo-NT)
    quint32 net1_length;     // 0019..001C
    quint8 net1_addr_shift;  // 001D
    quint32 net2_offset;     // 001E..0021
    quint32 net2_length;     // 0022..0025
    quint8 net2_addr_shift;  // 0026
    quint32 net3_offset;     // 0027..002A
    quint32 net3_length;     // 002B..002E
    void print(quint32 offset);
  };

  struct hdr_nod_t : public submap_hdr_t {
    quint32 nod1_offset;  // 0015..0018 - NOD-body relative offset (pseudo-NT)
    quint32 not1_length;  // 0019..001C
    quint8 not1_flags;    // 001D
    void print(quint32 offset);
  };

  struct hdr_dem_t : public submap_hdr_t {
    quint32 flags;         // 0015..0018 - 0=meter, 1=feet
    quint16 zoom_levels;   // 0019..001A
    quint8 x001B_001D[4];  // 001B..001D
    quint16 rec_size;      // 001F..0020
    quint32 dem1_offset;   // 0021..0024
    quint8 x0025_0029[4];  // 0025..0029
    void print(quint32 offset);
  };

  // copyright header
  struct tre0_t {
    QString descr1;
    QString descr2;
  };

  // map level definition
  struct tre1_t {
    quint8 raw_zoom;  // 7=inherit, 6-0=zoom
    quint8 bits;
    quint16 subdiv;
    tre1_t() : raw_zoom(0), bits(0), subdiv(0) {}
    bool inherit() const { return raw_zoom & 0x80; }
    quint8 zoom() const { return raw_zoom & 0x7F; }
    void print() { printf("zoom: %02X | inherit: %i | bits: %i | subdiv: %i\n", zoom(), inherit(), bits, subdiv); }
  };

  // map subdivision definition, without pointer to the lower level subparts
  struct tre2_t {
    quint24 rgn_offset;  // offset: 27-0 bit, 31-26 elements
    quint8 elements;
    quint24 center_lng;
    quint24 center_lat;
    quint16 width_trm;
#define TRE_SUBDIV_WIDTH(r) (r->width_trm & 0x7FFF)
#define TRE_SUBDIV_TERM(r) ((r->width_trm & 0x8000) != 0)
    quint16 height;
  };

  // pointer to the lower level subparts
  struct tre2_next_t : public tre2_t {
    quint16 next;
  };

  struct tre7_t {
    quint32 offsetPolygons;
    quint32 offsetPolyline;
    quint32 offsetPoints;
    quint8 btObjects;
  };

  // subdivision  information
  struct subdiv_t {
    quint32 n;
    quint16 next;       // section of next level
    bool terminate;     // end of section group
    quint32 rgn_start;  // offset into the submap's RGN part
    quint32 rgn_end;    // end of section in RGN part (last offset = rgn_end - 1)
    bool hasPoints;     // there are points stored in the RGN subsection
    bool hasPois;       // there are indexed points stored in the RGN subsection
    bool hasPolylines;  // there are polylines stored in the RGN subsection
    bool hasPolygons;   // there are polygons stored in the RGN subsection
    qint32 iCenterLng;  // the center longitude of the area covered by this subdivision
    qint32 iCenterLat;  // the center latitude of the area covered by this subdivision
    double north;       // north boundary of area covered by this subsection []
    double east;        // east boundary of area covered by this subsection []
    double south;       // south boundary of area covered by this subsection []
    double west;        // west boundary of area covered by this subsection []
    QRectF area;        // area in meter coordinates covered by this subdivision []
    quint32 shift;      // number of left shifts for RGN data
    quint32 level;      // map level this subdivision is shown
    quint32 offPolygons2;
    quint32 lenPolygons2;
    quint32 offPoints2;
    quint32 lenPoints2;
    quint32 offPolylines2;
    quint32 lenPolylines2;
    tre1_t* maplevel;
    void print() const;
    void printLite() const;
    // subdiv_t() { memset(this, 0, sizeof(subdiv_t)); }
  };

#pragma pack()
  // submap subfiles location info
  struct submap_subfile_t {
    quint32 offset = 0;      // file offset of submap part (non-NT)
    quint32 size = 0;        // size of the submap part (non-NT)
    quint32 hdrOffset = 0;   // file offset of header part (pseudo-NT)
    quint32 hdrSize = 0;     // size of the header part (pseudo-NT)
    quint32 bodyOffset = 0;  // file offset of body part (pseudo-NT)
    quint32 bodySize = 0;    // size of the body part (pseudo-NT)
    submap_subfile_t() : offset(0), size(0), hdrOffset(0), hdrSize(0), bodyOffset(0), bodySize(0) {}
  };

  struct submap_t {
    QString name;                              // the name of the submap
    QMap<QString, submap_subfile_t> subfiles;  // location information of all subfiles
    double north = 0.0;                        // north boundary of area covered by this submap [rad]
    double east = 0.0;                         // east  boundary of area covered by this submap [rad]
    double south = 0.0;                        // south boundary of area covered by this submap [rad]
    double west = 0.0;                         // west  boundary of area covered by this submap [rad]
    QRectF area;                               // area in [] covered by this submap
    bool isTransparent = false;                // bit 1 of POI_flags (TRE header @ 0x3F)
    hdr_tre_t hdrTRE;
    hdr_rgn_t hdrRGN;
    hdr_lbl_t hdrLBL;
    hdr_nod_t hdrNOD;
    hdr_net_t hdrNET;
    hdr_dem_t hdrDEM;
    QVector<tre1_t> mapLevels;  // used maplevels
    QVector<subdiv_t> subdivs;  // list of subdivisions
    quint32 nSubdivsNext = 0;
    StrTbl* strtbl;  // object to manage the string tables
    bool isPseudoNT = false;
  };

#define TRE_MAP_LEVEL(r) ((r)->zoom & 0x0f)
#define TRE_MAP_INHER(r) (((r)->zoom & 0x80) != 0)

  void print(const char* format, ...);
  void readFat(QFile& srcFile);
  void readImg(QFile& srcFile, submap_t& submap);
  void readGmp(QFile& srcFile, submap_t& submap);
  void readSubmaps(QFile& srcFile);
  void readSubmapArea(submap_t& submap);
  void parseMapLevels(QFile& srcFile, submap_t& submap);
  tre0_t readCopyrights(QFile& srcFile, quint32 baseOffset, quint32 limitOffset);
  void parseSubdivInfo(QFile& srcFile, submap_t& submap);
  void parseSubdivInfoExt(QFile& srcFile, submap_t& submap);
  void parseStringTable(QFile& srcFile, submap_t& submap);
  void readProductInfo(QDataStream& stream);
  void readMapInfo(QDataStream& stream);
  QString readRawString(QDataStream& stream);
  void processShapes(QFile& dstFile, QFile& srcFile, const submap_t& submap);
  void readShapes(QFile& srcFile);
  void decodeRgn(QFile& dstFile, QFile& srcFile, const subdiv_t& subdiv, StrTbl* strtbl, const QByteArray& rgndata);
  void writeCsv(QFile& dstFile, pointtype_t& points, pointtype_t& pois, polytype_t& polylines, polytype_t& polygons, quint32 level);
  void writeMp(QFile& dstFile, pointtype_t& points, pointtype_t& pois, polytype_t& polylines, polytype_t& polygons, quint32 level);
  QString convPtDegStr(const QPointF& point, bool wkt = false);
  QString convLnDegStr(const QPolygonF& polyline, bool isLine, bool wkt = false);
  void writeHeader(QFile& dstFile, const submap_t& submap);

  // file contains locked/encrypted data
  static void minno(hdr_tre_t* trehdr, QByteArray& data) {
    if (trehdr->flag & 0x80) {
      quint32 nlevels = trehdr->tre1_size / sizeof(tre1_t);

      quint8 key[5];
      quint8* tbl = new quint8[trehdr->tre1_size];
      memcpy(tbl, data.data(), trehdr->tre1_size);

      key[0] = (((tbl[0] >> 4) + 8) & 0x0F);
      key[1] = (((tbl[3] >> 4) + 16) & 0x0F);
      key[2] = (((tbl[3] & 0x0F) + 16) & 0x0F);
      key[3] = ((tbl[4] >> 4) & 0x7);
      if (nlevels > 2) {
        key[3] ^= (((tbl[9] >> 4) + 16 - key[3]) & 0x08);
      }
      key[4] = (((tbl[2] >> 4) + 16 - 0) & 15);

      for (quint32 i = 0; i < nlevels * 4; i++) {
        tbl[i] = (((((tbl[i] >> 4) + 16 - key[(i * 2) % 5]) & 15) << 4) + ((((tbl[i] & 15) + 16 - key[(i * 2 + 1) % 5]) & 15)));
      }

      tre1_t* ml = (tre1_t*)tbl;
      for (quint32 i = 0; i < nlevels; i++) {
        ++ml;
      }

      memcpy(data.data(), tbl, trehdr->tre1_size);
      trehdr->flag &= ~0x80;

      delete[] tbl;
    }
  }

  QString inputFile;
  QString outputFile;
  QTextCodec* codec;
  quint8 mask;
  quint32 mask32;
  quint64 mask64;
  QRectF maparea;
  QString codepageStr = "";
  QString codingStr = "";
  QString nameStr;
  QString copyrightsStr = "";
  QMutex mutex;
  QHash<QString, submap_t> submaps;  // hold all submap descriptors or gmapsupp.img files can hold several submaps each with it's own submap parts
  bool splitSubmaps = false;
  bool csvOutput = false;
  bool debugInfo = false;
  float maxFileSize = 1 * 1000 * 1024 * 1024;
  int totalPt = 0;
  int totalPo = 0;
  int totalLn = 0;
  int totalPg = 0;
  int totalPt2 = 0;
  int totalPo2 = 0;
  int totalLn2 = 0;
  int totalPg2 = 0;
  int totalShapesDecoded = 0;
  int totalPtFailed = 0;
  int totalPoFailed = 0;
  int totalLnFailed = 0;
  int totalPgFailed = 0;
  int totalPt2Failed = 0;
  int totalPo2Failed = 0;
  int totalLn2Failed = 0;
  int totalPg2Failed = 0;
  quint8 hasExtLabelCount = 0;
  quint8 hasNet1LabelCount = 0;
  quint8 infoSkipDupePoint = 0;
  quint8 warnTotals = 0;
  quint8 warnSkipOutside = 0;
  quint8 warnSuspiciousSegment = 0;
  quint8 warnPolyOversize = 0;
  quint8 warnInvalidType = 0;
  QElapsedTimer totalTimer;
  QElapsedTimer timer;
  quint64 method1Time = 0;
  quint64 method2Time = 0;
};

inline quint8 warnInvalidCoords = 0;

ImgDump::ImgDump(int& argc, char** argv) : QCoreApplication(argc, argv) {
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
  parser.process(this->arguments());

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
    readShapes(srcFile);
  } catch (const Exception& e) {
    qDebug() << "Fatal error:" << e.msg;
  }

  srcFile.close();
}

ImgDump::~ImgDump() {
  qDebug() << "✅ Done.";
}

void ImgDump::gmapsupp_imghdr_t::print() {
  printUInt8("xorByte", xorByte);
  printArrayUInt8("x0001_0007", x0001_0007, sizeof(x0001_0007));
  printUInt16("version", version);
  printInt8("upMonth", upMonth);
  printInt16("upYear", upYear + 1900);
  printArrayUInt8("x000C_000D", x000C_000D, sizeof(x000C_000D));
  printUInt8("supp", supp);
  printUInt8("checksum", checksum);
  printArrayInt8("signature", signature, sizeof(signature));
  printUInt8("x0017", x0017);
  printUInt16("sectors1", sectors1);
  printUInt16("heads1", heads1);
  printUInt16("cylinders", cylinders);
  printArrayUInt8("x001E_00038", x001E_0038, sizeof(x001E_0038));
  printInt16("year", year);
  printInt8("month", month);
  printInt8("day", day);
  printInt8("hour", hour);
  printInt8("minute", minute);
  printInt8("second", second);
  printInt8("offsetFAT", offsetFAT);
  printArrayInt8("identifier", identifier, sizeof(identifier));
  printUInt8("x0048", x0048);
  printArrayInt8("desc1", desc1, sizeof(desc1));
  printUInt16("head2", head2);
  printUInt16("sectors2", sectors2);
  printUInt8("e1", e1);
  printUInt8("e2", e2);
  printUInt16("nBlocks1", nBlocks1);
  printArrayInt8("desc2", desc2, sizeof(desc2));
  printArrayUInt8("x0083_01BE", x0083_01BE, sizeof(x0083_01BE));
  printUInt8("startHead", startHead);
  printUInt8("startSector", startSector);
  printUInt8("startCylinder", startCylinder);
  printUInt8("systemType", systemType);
  printUInt8("endHead", endHead);
  printUInt8("endSector", endSector);
  printUInt8("endCylinder", endCylinder);
  printUInt32("relSectors", relSectors);
  printUInt32("nSectors", nSectors);
  printArrayUInt8("x01CE_01FD", x01CE_01FD, sizeof(x01CE_01FD));
  printUInt16("terminator", terminator);
}

void ImgDump::submap_hdr_t::print() {
  printUInt16("size", size);
  printArrayInt8("type", type, sizeof(type));
  printUInt8("x000C", x000C);
  printUInt8("flag", flag);
  printInt16("year", year);
  printInt8("month", month);
  printInt8("day", day);
  printInt8("hour", hour);
  printInt8("minute", minute);
  printInt8("second", second);
}

void ImgDump::gmp_hdr_t::print() {
  submap_hdr_t::print();

  printArrayUInt8("x0015_0018", x0015_0018, sizeof(x0015_0018));
  printUInt32("tre_offset", tre_offset);
  printUInt32("rgn_offset", rgn_offset);
  printUInt32("lbl_offset", lbl_offset);
  printUInt32("net_offset", net_offset);
  printUInt32("nod_offset", nod_offset);
  printUInt32("dem_offset", dem_offset);
  printUInt32("mar_offset", mar_offset);
  printUInt32("met_offset", met_offset);
}

void ImgDump::hdr_tre_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt24("northbound", northbound);
  printUInt24("eastbound", eastbound);
  printUInt24("southbound", southbound);
  printUInt24("westbound", westbound);
  printUInt32("tre1_offset", tre1_offset + offset);
  printUInt32("tre1_size", tre1_size);
  printUInt32("tre2_offset", tre2_offset + offset);
  printUInt32("tre2_size", tre2_size);
  printUInt32("tre3_offset", tre3_offset + offset);
  printUInt32("tre3_size", tre3_size);
  printUInt16("tre3_rec_size", tre3_rec_size);
  printArrayUInt8("x003B_003E", x003B_003E, sizeof(x003B_003E));
  printUInt8("POI_flags", POI_flags);
  printArrayUInt8("render_prio", render_prio, sizeof(render_prio));
  printArrayUInt8("x0043_0049", x0043_0049, sizeof(x0043_0049));
  printUInt32("tre4_offset", tre4_offset + offset);
  printUInt32("tre4_size", tre4_size);
  printUInt16("tre4_rec_size", tre4_rec_size);
  printArrayUInt8("x0054_0057", x0054_0057, sizeof(x0054_0057));
  printUInt32("tre5_offset", tre5_offset + offset);
  printUInt32("tre5_size", tre5_size);
  printUInt16("tre5_rec_size", tre5_rec_size);
  printArrayUInt8("x0062_0065", x0062_0065, sizeof(x0062_0065));
  printUInt32("tre6_offset", tre6_offset + offset);
  printUInt32("tre6_size", tre6_size);
  printUInt16("tre6_rec_size", tre6_rec_size);
  printArrayUInt8("x0070_0073", x0070_0073, sizeof(x0070_0073));
  printUInt32("map_id", map_id);
  printArrayUInt8("x0078_007B", x0078_007B, sizeof(x0078_007B));
  printUInt32("tre7_offset", tre7_offset + offset);
  printUInt32("tre7_size", tre7_size);
  printUInt16("tre7_rec_size", tre7_rec_size);
  printArrayUInt8("x0086_0089", x0086_0089, sizeof(x0086_0089));
  printUInt32("tre8_offset", tre8_offset + offset);
  printUInt32("tre8_size", tre8_size);
  printUInt16("tre8_rec_size", tre8_rec_size);
  printUInt16("polyl2_types_num", polyl2_types_num);
  printUInt16("polyg2_types_num", polyg2_types_num);
  printUInt16("poi2_types_num", poi2_types_num);
  printArrayUInt8("key", key, sizeof(key));
  printArrayUInt8("x00AA_00AD", x00AA_00AD, sizeof(x00AA_00AD));
  printUInt32("tre9_offset", tre9_offset + offset);
  printUInt32("tre9_size", tre9_size);
  printUInt16("tre9_rec_size", tre9_rec_size);
  printArrayUInt8("x00B8_00BB", x00B8_00BB, sizeof(x00B8_00BB));
  printUInt32("tre10_offset", tre9_offset + offset);
  printUInt32("tre10_size", tre9_size);
  printUInt16("tre10_rec_size", tre9_rec_size);
  printArrayUInt8("x00C6_00CE", x00C6_00CE, sizeof(x00C6_00CE));
  printUInt32("map_id2", map_id2);
}

void ImgDump::hdr_rgn_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("rgn1_offset", rgn1_offset + offset);
  printUInt32("rgn1_length", rgn1_length);
  printUInt32("pg2_offset", pg2_offset + offset);
  printUInt32("pg2_length", pg2_length);
  printArrayUInt8("x0025_0038", x0025_0038, sizeof(x0025_0038));
  printUInt32("ln2_offset", ln2_offset + offset);
  printUInt32("ln2_length", ln2_length);
  printArrayUInt8("x0041_0054", x0041_0054, sizeof(x0041_0054));
  printUInt32("pt2_offset", pt2_offset + offset);
  printUInt32("pt2_length", pt2_length);
  printArrayUInt8("x005D_0070", x005D_0070, sizeof(x005D_0070));
  printUInt32("rgn2_offset", rgn2_offset + offset);
  printUInt32("rgn2_length", rgn2_length);
  printUInt32("unknown", unknown);
}

void ImgDump::hdr_lbl_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("lbl1_offset", lbl1_offset + offset);
  printUInt32("lbl1_length", lbl1_length);
  printUInt8("addr_shift", addr_shift);
  printUInt8("coding", coding);

  printUInt32("lbl2_offset", lbl2_offset + offset);
  printUInt32("lbl2_length", lbl2_length);
  printUInt16("lbl2_rec_size", lbl2_rec_size);
  printArrayUInt8("x0029_002C", x0029_002C, sizeof(x0029_002C));

  printUInt32("lbl3_offset", lbl3_offset + offset);
  printUInt32("lbl3_length", lbl3_length);
  printUInt16("lbl3_rec_size", lbl3_rec_size);
  printArrayUInt8("x0037_003A", x0037_003A, sizeof(x0037_003A));

  printUInt32("lbl4_offset", lbl4_offset + offset);
  printUInt32("lbl4_length", lbl4_length);
  printUInt16("lbl4_rec_size", lbl4_rec_size);
  printArrayUInt8("x0045_0048", x0045_0048, sizeof(x0045_0048));

  printUInt32("lbl5_offset", lbl5_offset + offset);
  printUInt32("lbl5_length", lbl5_length);
  printUInt16("lbl5_rec_size", lbl5_rec_size);
  printArrayUInt8("x0053_0056", x0053_0056, sizeof(x0053_0056));

  printUInt32("lbl6_offset", lbl6_offset + offset);
  printUInt32("lbl6_length", lbl6_length);
  printUInt8("lbl6_addr_shift", lbl6_addr_shift);
  printUInt8("lbl6_glob_mask", lbl6_glob_mask);
  printArrayUInt8("x0061_0063", x0061_0063, sizeof(x0061_0063));

  printUInt32("lbl7_offset", lbl7_offset + offset);
  printUInt32("lbl7_length", lbl7_length);
  printUInt16("lbl7_rec_size", lbl7_rec_size);
  printArrayUInt8("x006E_0071", x006E_0071, sizeof(x006E_0071));

  printUInt32("lbl8_offset", lbl8_offset + offset);
  printUInt32("lbl8_length", lbl8_length);
  printUInt16("lbl8_rec_size", lbl8_rec_size);
  printArrayUInt8("x007C_007F", x007C_007F, sizeof(x007C_007F));

  printUInt32("lbl9_offset", lbl9_offset + offset);
  printUInt32("lbl9_length", lbl9_length);
  printUInt16("lbl9_rec_size", lbl9_rec_size);
  printArrayUInt8("x008A_008D", x008A_008D, sizeof(x008A_008D));

  printUInt32("lbl10_offset", lbl10_offset + offset);
  printUInt32("lbl10_length", lbl10_length);
  printUInt16("lbl10_rec_size", lbl10_rec_size);
  printArrayUInt8("x0098_009B", x0098_009B, sizeof(x0098_009B));

  printUInt32("lbl11_offset", lbl11_offset + offset);
  printUInt32("lbl11_length", lbl11_length);
  printUInt16("lbl11_rec_size", lbl11_rec_size);
  printArrayUInt8("x00A6_00A9", x00A6_00A9, sizeof(x00A6_00A9));

  printUInt16("codepage", codepage);
  printArrayUInt8("x00AC_00AF", x00AC_00AF, sizeof(x00AC_00AF));

  printUInt32("lbl12_offset", lbl12_offset + offset);
  printUInt32("lbl12_length", lbl12_length);
}

void ImgDump::hdr_net_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("net1_offset", net1_offset + offset);
  printUInt32("net1_length", net1_length);
  printUInt8("net1_addr_shift", net1_addr_shift);
  printUInt32("net2_offset", net2_offset + offset);
  printUInt32("net2_length", net2_length);
  printUInt8("net2_addr_shift", net2_addr_shift);
  printUInt32("net3_offset", net3_offset + offset);
  printUInt32("net3_length", net3_length);
}

void ImgDump::hdr_nod_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("nod1_offset", nod1_offset + offset);
}

void ImgDump::hdr_dem_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("flags", flags);
  printUInt16("zoom_levels", zoom_levels);
  printArrayUInt8("x001B_001D", x001B_001D, sizeof(x001B_001D));
  printUInt16("rec_size", rec_size);
  printUInt32("dem1_offset", dem1_offset + offset);
  printArrayUInt8("x0025_0029", x0025_0029, sizeof(x0025_0029));
}

void ImgDump::subdiv_t::print() const {
  if (next) {
    printf("--- subdiv #%i next #%i---\n", n, next);
  } else {
    printf("--- subdiv #%i ---\n", n);
  }
  printf("north %f east  %f south %f west  %f\n", qRadiansToDegrees(north), qRadiansToDegrees(east), qRadiansToDegrees(south), qRadiansToDegrees(west));
  printf("shift %i level %i\n", shift, level);
  printf("rgn_start %08X rgn_end %08X\n", rgn_start, rgn_end);
  printf("Terminate %i hasPoints %i hasPois %i hasPolylines %i hasPolygons %i\n", terminate, hasPoints, hasPois, hasPolylines, hasPolygons);
  printf("offsetPolygons2:  %08X  lengthPolygons2:  %08X\n", offPolygons2, lenPolygons2);
  printf("offsetPolylines2: %08X  lengthPolylines2: %08X\n", offPolylines2, lenPolylines2);
  printf("offsetPoints2:    %08X  lengthPoints2:    %08X\n", offPoints2, lenPoints2);
  printf("iCenterLng %f iCenterLat %f\n", GARMIN_DEG(iCenterLng), GARMIN_DEG(iCenterLat));
}

void ImgDump::subdiv_t::printLite() const {
  printf("subdiv: %i | zoom: %i | pt: %i | poi: %i | ln: %i | pg: %i\n", n, level, hasPoints, hasPois, hasPolylines, hasPolygons);
}

void ImgDump::print(const char* format, ...) {
  QMutexLocker lock(&mutex);
  va_list args;
  va_start(args, format);
  vfprintf(stdout, format, args);
  va_end(args);

  fflush(stdout);
}

void ImgDump::readFat(QFile& srcFile) {
  submaps.clear();

  gmapsupp_imghdr_t imghdr;
  srcFile.seek(0);
  srcFile.read((char*)&imghdr, sizeof(gmapsupp_imghdr_t));
  srcFile.seek(imghdr.offsetFAT * 0x200);

  if (debugInfo) {
    print("----- IMG Header ----- size(%08X)\n", sizeof(gmapsupp_imghdr_t));
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

  if (strncmp(imghdr.signature, "DSKIMG", 7) != 0) {
    throw Exception(tr("Bad file format: ") + inputFile);
  }
  if (strncmp(imghdr.identifier, "GARMIN", 7) != 0) {
    throw Exception(tr("Bad file format: ") + inputFile);
  }

  nameStr = QByteArray((const char*)imghdr.desc1, 20);
  nameStr += imghdr.desc2;
  nameStr = nameStr.trimmed();

  // if (imghdr.desc1) {
  // nameStr = QString::fromLatin1(imghdr.desc1).trimmed();
  // }

  size_t blocksize = imghdr.blocksize();

  qDebug() << "---- FAT ----";
  FATBlock_t FATBlock;
  srcFile.read((char*)&FATBlock, sizeof(FATBlock_t));
  while (FATBlock.flag == 1) {
    if (srcFile.atEnd()) {
      throw Exception("Premature end of file.");
    }

    if (FATBlock.size != 0) {
      char nameStr[sizeof(FATBlock.name) + 1] = {0};
      memcpy(nameStr, FATBlock.name, sizeof(FATBlock.name));

      char typeStr[sizeof(FATBlock.type) + 1] = {0};
      memcpy(typeStr, FATBlock.type, sizeof(FATBlock.type));

      if (nameStr[0] == 0x20) {
        qDebug() << "Skip empty subfile type:" << nameStr << typeStr;
      } else if (submaps.contains(nameStr) && strcmp(typeStr, "GMP") != 0 && submaps[nameStr].subfiles.keys().contains(typeStr)) {
        // or check FATBlock.part > 0x00 ?
        qDebug() << "Skip duplicate subfile type:" << nameStr << typeStr;
      } else if (strcmp(typeStr, "SRT") == 0 || strcmp(typeStr, "MDR") == 0 || strcmp(typeStr, "MD2") == 0 || strcmp(typeStr, "TYP") == 0) {
        qDebug() << "Skip useless subfile type:" << nameStr << typeStr;

        // } else if (strcmp(typeStr, "NET") == 0) {  //|| strcmp(typeStr, "NOD") == 0 || strcmp(typeStr, "LBL") == 0 || strcmp(typeStr, "MPS") == 0) {
        // NET subfile: throwing an exception
        // qDebug() << "Skip subfile type (for debug purpose):" << nameStr << typeStr;
      } else {
        submap_t& submap = submaps[nameStr];
        submap.name = nameStr;

        submap_subfile_t& part = submap.subfiles[typeStr];
        part.size = gar_load(quint32, FATBlock.size);
        part.offset = quint32(gar_load(uint16_t, FATBlock.blocks[0]) * blocksize);

        // @todo: more checks, maybe NT or checl 0x0D flags for bit 0x80
        submap.isPseudoNT = strcmp(typeStr, "GMP") == 0;
      }
    }

    srcFile.read((char*)&FATBlock, sizeof(FATBlock_t));
  }
}

void ImgDump::readImg(QFile& srcFile, submap_t& submap) {
  QString subfileName = "TRE";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char*)&size, sizeof(size));

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char*)&submap.hdrTRE, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrTRE.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "RGN";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char*)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char*)&submap.hdrRGN, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrRGN.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "LBL";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char*)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char*)&submap.hdrLBL, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrLBL.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "NET";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char*)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char*)&submap.hdrNET, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrNET.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "NOD";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char*)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char*)&submap.hdrNOD, size);

    if (debugInfo) {
      printf("   --- %s header ---\n", subfileName.toLatin1().data());
      submap.hdrNOD.print(submap.subfiles[subfileName].offset);
    }
  }

  subfileName = "DEM";
  if (submap.subfiles.keys().contains(subfileName)) {
    srcFile.seek(submap.subfiles[subfileName].offset);
    quint16 size;
    srcFile.read((char*)&size, sizeof(size));

    if (size > submap.subfiles[subfileName].size) {
      qDebug() << "[WARN] Header size is bigger then subfile block size:" << subfileName << Qt::hex << size << submap.subfiles[subfileName].size;
      size = submap.subfiles[subfileName].size;
    }

    srcFile.seek(submap.subfiles[subfileName].offset);
    srcFile.read((char*)&submap.hdrDEM, size);

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

void ImgDump::readGmp(QFile& srcFile, submap_t& submap) {
  const QString gmpSubfileName = "GMP";
  srcFile.seek(submap.subfiles[gmpSubfileName].offset);

  gmp_hdr_t hdr;
  srcFile.read((char*)&hdr, sizeof(hdr));
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
    srcFile.read((char*)&submap.hdrTRE, sizeof(submap.hdrTRE));

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
    srcFile.read((char*)&submap.hdrRGN, sizeof(submap.hdrRGN));

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
    srcFile.read((char*)&submap.hdrLBL, sizeof(submap.hdrLBL));

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
    srcFile.read((char*)&submap.hdrNET, sizeof(submap.hdrNET));

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
    srcFile.read((char*)&submap.hdrNOD, sizeof(submap.hdrNOD));

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
    srcFile.read((char*)&submap.hdrDEM, sizeof(submap.hdrDEM));

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

QString ImgDump::readRawString(QDataStream& stream) {
  QByteArray label;
  quint8 tmp;
  stream >> tmp;
  while (tmp != 0) {
    label.append(tmp);
    stream >> tmp;
  }
  return QString::fromUtf8(label);
}

void ImgDump::readProductInfo(QDataStream& stream) {
  quint16 idProd;
  quint16 idFamily;
  stream >> idProd >> idFamily;
  print("Product Info: %i %i %s\n", idProd, idFamily, readRawString(stream).toUtf8().data());
}

void ImgDump::readMapInfo(QDataStream& stream) {
  quint16 idProd;
  quint16 idFamily;
  quint32 mapNumber;

  stream >> idProd >> idFamily >> mapNumber;
  const QString& seriesName = readRawString(stream);
  const QString& mapDesc = readRawString(stream);
  const QString& areaName = readRawString(stream);
  quint32 mapId;
  quint32 dummy;

  stream >> mapId >> dummy;

  print("Map Info: %i %i %i %s %s %s %08X %i \n", idProd, idFamily, mapNumber, seriesName.toLocal8Bit().data(), mapDesc.toLocal8Bit().data(), areaName.toLocal8Bit().data(), mapId,
        mapId);
}

void ImgDump::readSubmaps(QFile& srcFile) {
  maparea = QRectF();
  qDebug() << "Submaps count:" << submaps.size();

  for (auto& submap : submaps) {
    if (submap.isPseudoNT) {
      readGmp(srcFile, submap);

    } else {
      if (!(submap.subfiles.contains("TRE") && submap.subfiles.contains("RGN"))) {
        // throw CException(QString("Missing mandatory submap subfiles: %1 %2 %3").arg(submap.name).arg(submap.subfiles.contains("TRE")).arg(submap.subfiles.contains("RGN")));
        // skip system subfiles with no road data
        continue;
      }
      readImg(srcFile, submap);
    }
    parseMapLevels(srcFile, submap);
    parseSubdivInfo(srcFile, submap);
    parseSubdivInfoExt(srcFile, submap);
    parseStringTable(srcFile, submap);
  }

  if (debugInfo) {
    print("--- Subfiles ---\n");
    for (auto& submap : submaps) {
      for (const auto& [subfile, part] : submap.subfiles.asKeyValueRange()) {
        print("%s %s %08X %08X %08X %08X %08X %08X\n", submap.name.toLocal8Bit().data(), subfile.toLocal8Bit().data(), part.offset, part.size, part.hdrOffset, part.hdrSize,
              part.bodyOffset, part.bodySize);
      }
    }
  }

  fflush(stdout);
  fflush(stderr);
}

void ImgDump::parseMapLevels(QFile& srcFile, submap_t& submap) {
  // read map levels from section tre1
  srcFile.seek(submap.subfiles[submap.isPseudoNT ? "GMP" : "TRE"].offset + submap.hdrTRE.tre1_offset);
  QByteArray bufMapLevels = srcFile.read(submap.hdrTRE.tre1_size);

  if (submap.hdrTRE.flag & 0x80) {
    minno(&submap.hdrTRE, bufMapLevels);
  }

  quint32 nLevels = submap.hdrTRE.tre1_size / sizeof(tre1_t);
  quint32 nSubdivs = 0;
  quint32 nSubdivsLast = 0;

  tre1_t* pMapLevel = (tre1_t*)bufMapLevels.data();
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

ImgDump::tre0_t ImgDump::readCopyrights(QFile& srcFile, quint32 baseOffset, quint32 limitOffset) {
  tre0_t result;

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

void ImgDump::readSubmapArea(submap_t& submap) {
  // read map boundaries from header
  submap.north = GARMIN_RAD(gar_ptr_load(int24_t, submap.hdrTRE.northbound));
  submap.east = GARMIN_RAD(gar_ptr_load(int24_t, submap.hdrTRE.eastbound));
  submap.south = GARMIN_RAD(gar_ptr_load(int24_t, submap.hdrTRE.southbound));
  submap.west = GARMIN_RAD(gar_ptr_load(int24_t, submap.hdrTRE.westbound));

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

void ImgDump::parseSubdivInfo(QFile& srcFile, submap_t& submap) {
  quint32 start = submap.subfiles[submap.isPseudoNT ? "GMP" : "TRE"].offset + submap.hdrTRE.size;
  quint32 end = submap.subfiles[submap.isPseudoNT ? "GMP" : "TRE"].offset + submap.hdrTRE.tre2_offset;
  if (end > start) {
    auto crh = readCopyrights(srcFile, start, end);
    copyrightsStr = QString("%1|%2").arg(crh.descr1).arg(crh.descr2);
  }

  srcFile.seek(submap.subfiles[submap.isPseudoNT ? "GMP" : "TRE"].offset + submap.hdrTRE.tre2_offset);
  QByteArray tre2 = srcFile.read(submap.hdrTRE.tre2_size);

  tre2_next_t* pTre2N = (tre2_next_t*)tre2.data();

  QVector<subdiv_t>::iterator subdiv = submap.subdivs.begin();
  QVector<subdiv_t>::iterator subdiv_prev = submap.subdivs.end();

  int mapLevelIdx = 0;
  if (submap.mapLevels.size() == 0) {
    throw Exception("Missing map levels");
  }

  quint32 nSubdiv = submap.mapLevels[mapLevelIdx].subdiv;
  // parse all 16 byte subdivision entries
  quint32 i;
  quint32 rgnoff = submap.hdrRGN.size;
  for (i = 0; i < submap.nSubdivsNext; ++i, --nSubdiv) {
    qint32 cx, cy;
    qint32 width, height;

    subdiv->maplevel = &submap.mapLevels[mapLevelIdx];
    subdiv->n = i;
    subdiv->next = pTre2N->next;
    subdiv->terminate = TRE_SUBDIV_TERM(pTre2N);
    subdiv->rgn_start = pTre2N->rgn_offset[0] | pTre2N->rgn_offset[1] << 8 | pTre2N->rgn_offset[2] << 16 | (pTre2N->elements & 0x0F) << 24;
    subdiv->rgn_start += rgnoff;
    // skip if this is the first entry
    if (subdiv_prev != submap.subdivs.end()) {
      subdiv_prev->rgn_end = subdiv->rgn_start;
    } else {
      subdiv_prev->rgn_end = 0;
    }

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

    subdiv->north = GARMIN_RAD(cy + height + 1);
    subdiv->south = GARMIN_RAD(cy - height);
    subdiv->east = GARMIN_RAD(cx + width + 1);
    subdiv->west = GARMIN_RAD(cx - width);

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
  tre2_t* pTre2L = pTre2N;
  // parse all 14 byte subdivision entries of last map level
  for (; i < nSubdivs; ++i) {
    qint32 cx, cy;
    qint32 width, height;
    subdiv->maplevel = &submap.mapLevels[mapLevelIdx];
    subdiv->n = i;
    subdiv->next = 0;
    subdiv->terminate = TRE_SUBDIV_TERM(pTre2L);
    subdiv->rgn_start = pTre2L->rgn_offset[0] | pTre2L->rgn_offset[1] << 8 | pTre2L->rgn_offset[2] << 16 | (pTre2L->elements & 0x0F) << 24;
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

    subdiv->north = GARMIN_RAD(cy + height + 1);
    subdiv->south = GARMIN_RAD(cy - height);
    subdiv->east = GARMIN_RAD(cx + width + 1);
    subdiv->west = GARMIN_RAD(cx - width);

    subdiv->area = QRectF(QPointF(subdiv->west, subdiv->north), QPointF(subdiv->east, subdiv->south));

    subdiv_prev = subdiv;
    ++pTre2L;
    ++subdiv;
  }
}

// read extended type elements
void ImgDump::parseSubdivInfoExt(QFile& srcFile, submap_t& submap) {
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

  QVector<subdiv_t>::iterator subdiv = submap.subdivs.begin();
  QVector<subdiv_t>::iterator subdiv_prev = submap.subdivs.end();
  if (submap.hdrTRE.size >= 0x9A && submap.hdrTRE.tre7_size && rec_size >= sizeof(tre7_t)) {
    srcFile.seek(blockStart);
    QByteArray subdiv2 = srcFile.read(submap.hdrTRE.tre7_size);
    tre7_t* pSubDiv2 = (tre7_t*)subdiv2.data();

    bool skipPois = (rec_size != sizeof(tre7_t));

    subdiv = submap.subdivs.begin();
    subdiv_prev = submap.subdivs.begin();
    subdiv->offPolygons2 = pSubDiv2->offsetPolygons + rgnOffPolyg2;
    subdiv->offPolylines2 = pSubDiv2->offsetPolyline + rgnOffPolyl2;
    subdiv->offPoints2 = skipPois ? 0 : pSubDiv2->offsetPoints + rgnOffPoint2;

    ++subdiv;
    pSubDiv2 = reinterpret_cast<tre7_t*>((quint8*)pSubDiv2 + rec_size);

    while (subdiv != submap.subdivs.end()) {
      subdiv->offPolygons2 = pSubDiv2->offsetPolygons + rgnOffPolyg2;
      subdiv->offPolylines2 = pSubDiv2->offsetPolyline + rgnOffPolyl2;
      subdiv->offPoints2 = skipPois ? 0 : pSubDiv2->offsetPoints + rgnOffPoint2;

      subdiv_prev->lenPolygons2 = subdiv->offPolygons2 - subdiv_prev->offPolygons2;
      subdiv_prev->lenPolylines2 = subdiv->offPolylines2 - subdiv_prev->offPolylines2;
      subdiv_prev->lenPoints2 = skipPois ? 0 : subdiv->offPoints2 - subdiv_prev->offPoints2;

      subdiv_prev = subdiv;

      ++subdiv;
      pSubDiv2 = reinterpret_cast<tre7_t*>((quint8*)pSubDiv2 + rec_size);
    }

    subdiv_prev->lenPolygons2 = rgnOffPolyg2 + rgnLenPolyg2 - subdiv_prev->offPolygons2;
    subdiv_prev->lenPolylines2 = rgnOffPolyl2 + rgnLenPolyl2 - subdiv_prev->offPolylines2;
    subdiv_prev->lenPoints2 = skipPois ? 0 : rgnOffPoint2 + rgnLenPoint2 - subdiv_prev->offPoints2;
  }
}

void ImgDump::parseStringTable(QFile& srcFile, submap_t& submap) {
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

void ImgDump::readShapes(QFile& srcFile) {
  // int numThreads = 4;

  int numThreads = 1;
  if (numThreads > 1) {
    QThreadPool::globalInstance()->setMaxThreadCount(numThreads);
  }
  qDebug() << "Submaps count:" << submaps.size();

  bool isFirst = true;
  QFile dstFile;
  uint8_t filePart = 0;
  for (const submap_t& submap : submaps) {
    qDebug() << "Submap name:" << submap.name << "| Total subdivs:" << submap.subdivs.size();
    if (submap.subdivs.isEmpty()) {
      qDebug() << "Skip RGN decode for this submap (no subdivs)" << submap.name;
      continue;
    }

    QFileInfo ofInfo(outputFile);
    QString fileName = QString("%1%2.%3").arg(ofInfo.baseName()).arg(splitSubmaps ? "-" + submap.name : "").arg(ofInfo.suffix());
    if (dstFile.isOpen()) {
      QFileInfo fi(dstFile);
      const bool isOversize = fi.size() > maxFileSize;
      if (!csvOutput) {
        if (isOversize) {
          ++filePart;
          fileName = QString("%1.part%2.%3").arg(ofInfo.baseName()).arg(filePart + 1).arg(ofInfo.suffix());
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
            const QString firstPartName = QString("%1-part%2.%3").arg(ofInfo.baseName()).arg(filePart).arg(ofInfo.suffix());
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
      processShapes(dstFile, srcFile, submap);
    } else {
      CSubmapTask* task = new CSubmapTask([this, &dstFile, &srcFile, &submap]() { processShapes(dstFile, srcFile, submap); });
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
        qDebug() << "[WARN] Number of shapes outside subdiv area:" << warnSkipOutside;
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
        qDebug() << "[WARN] Number of invalid shapes type:" << warnInvalidType;
        warnInvalidType = 0;
      }
      if (warnInvalidCoords) {
        qDebug() << "[WARN] Number of invalid coordinates:" << warnInvalidCoords;
        warnInvalidCoords = 0;
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
      warnInvalidCoords = 0;
    }
#endif
  }
  QThreadPool::globalInstance()->waitForDone();

  if (dstFile.isOpen()) {
    dstFile.flush();
    dstFile.close();
  }
}

void ImgDump::processShapes(QFile& dstFile, QFile& srcFile, const submap_t& submap) {
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

    if (rgnData.size() == 0) {
      qDebug() << "[WARN] No RGN data";
      return;
    }

    for (const subdiv_t& subdiv : submap.subdivs) {
      // if (debugInfo) { subdiv.print(); }
      if (warnInvalidCoords + warnInvalidType + warnSuspiciousSegment + warnSkipOutside + infoSkipDupePoint + warnTotals + warnPolyOversize > 500) {
        qDebug() << "[ERROR] Too many errors: wrong offsets or unknown format with extended headers.";
        break;
      }
      decodeRgn(dstFile, srcFile, subdiv, submap.strtbl, rgnData);
    }
    totalShapesDecoded = totalPt + totalPo + totalLn + totalPg + totalPt2 + totalPo2 + totalLn2 + totalPg2;
    qDebug().noquote() << QString("Total decoded shapes: %1 | RGN: %2 %3 %4 %5 %6 %7 %8 %9 | Shapes: %10 %11 %12")
                              .arg(totalShapesDecoded, -8)
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
  } catch (const Exception& e) {
    qDebug() << "Fatal error:" << e.msg;
  }
}

static inline bool isCompletelyOutside(const QPolygonF& poly, const QRectF& viewport) {
  qreal north = qDegreesToRadians(-90.0);
  qreal south = qDegreesToRadians(90.0);
  qreal west = qDegreesToRadians(180.0);
  qreal east = qDegreesToRadians(-180.0);

  for (const QPointF& pt : poly) {
    if (north < pt.y()) {
      north = pt.y();
    }
    if (south > pt.y()) {
      south = pt.y();
    }
    if (west > pt.x()) {
      west = pt.x();
    }
    if (east < pt.x()) {
      east = pt.x();
    }
  }

  QRectF ref(west, north, east - west, south - north);
  if (ref.width() == 0) {
    ref.setWidth(0.00001);
  }
  if (ref.height() == 0) {
    ref.setHeight(0.00001);
  }

  return !viewport.intersects(ref);
}

void ImgDump::decodeRgn(QFile& dstFile, QFile& srcFile, const subdiv_t& subdiv, StrTbl* strtbl, const QByteArray& rgndata) {
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

  const quint8* pRawData = (quint8*)rgndata.data();

  quint32 opnt = 0, opoi = 0, opline = 0, opgon = 0;
  quint32 objCnt = subdiv.hasPois + subdiv.hasPoints + subdiv.hasPolylines + subdiv.hasPolygons;

  quint16* pOffset = (quint16*)(pRawData + subdiv.rgn_start);

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
    const quint8* pData = pRawData + opnt;
    const quint8* pEnd = pRawData + (opoi ? opoi : opline ? opline : opgon ? opgon : subdiv.rgn_end);
    while (pData < pEnd) {
      RgnPoint pt;
      pData += pt.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData);

      if (!subdiv.area.contains(pt.pos)) {
        // qDebug() << "[WARN] Skip points outside subdiv area:" << subdiv.area << pt.pos;
        ++warnSkipOutside;
        continue;
      }

      if (strtbl) {
        pt.isLbl6 ? strtbl->get(srcFile, pt.lbl_ptr, StrTbl::poi, pt.labels) : strtbl->get(srcFile, pt.lbl_ptr, StrTbl::lbl, pt.labels);
      }

      ++totalPt;
      points.push_back(pt);
    }
  }

  // decode pois
  if (subdiv.hasPois) {
    const quint8* pData = pRawData + opoi;
    const quint8* pEnd = pRawData + (opline ? opline : opgon ? opgon : subdiv.rgn_end);
    while (pData < pEnd) {
      RgnPoint po;
      pData += po.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData);

      if (!subdiv.area.contains(po.pos)) {
        // qDebug() << "[WARN] Skip pois outside subdiv area:" << subdiv.area << po.pos;
        ++warnSkipOutside;
        continue;
      }

      if (strtbl) {
        po.isLbl6 ? strtbl->get(srcFile, po.lbl_ptr, StrTbl::poi, po.labels) : strtbl->get(srcFile, po.lbl_ptr, StrTbl::lbl, po.labels);
      }

      ++totalPo;
      pois.push_back(po);
    }
  }

  // decode polylines
  if (subdiv.hasPolylines) {
    const quint8* pData = pRawData + opline;
    const quint8* pEnd = pRawData + (opgon ? opgon : subdiv.rgn_end);
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
          strtbl->get(srcFile, ln.lbl_info, StrTbl::net, ln.labels);
        } else {
          strtbl->get(srcFile, ln.lbl_info, StrTbl::lbl, ln.labels);
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
    const quint8* pData = pRawData + opgon;
    const quint8* pEnd = pRawData + subdiv.rgn_end;
    while (pData < pEnd) {
      RgnLine pg;
      pData += pg.decode(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, false, pData, pEnd);

      if (isCompletelyOutside(pg.points, subdiv.area)) {
        // qDebug() << "[WARN] Skip polygons outside subdiv area:" << subdiv.area << pg.points.toList().first(10);
        ++warnSkipOutside;
        continue;
      }

      if (strtbl && pg.lbl_info) {
        if (pg.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[pg] 1 hasNet1Label" << Qt::hex << pg.lbl_info;
          strtbl->get(srcFile, pg.lbl_info, StrTbl::net, pg.labels);
        } else {
          strtbl->get(srcFile, pg.lbl_info, StrTbl::lbl, pg.labels);
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
    const quint8* pData = pRawData + subdiv.offPolygons2;
    const quint8* pEnd = pData + subdiv.lenPolygons2;
    while (pData < pEnd) {
      RgnLine pg;
      // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
      pData += pg.decodeExt(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, false, pData, pEnd);

      if (isCompletelyOutside(pg.points, subdiv.area)) {
        // qDebug() << "[WARN] Skip polygons outside subdiv area:" << subdiv.area << pg.points.toList().first(10);
        ++warnSkipOutside;
        continue;
      }

      if (strtbl && pg.lbl_info) {
        if (pg.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[pg] 2 hasNet1Label" << Qt::hex << pg.lbl_info;
          strtbl->get(srcFile, pg.lbl_info, StrTbl::net, pg.labels);
        } else {
          strtbl->get(srcFile, pg.lbl_info, StrTbl::lbl, pg.labels);
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
    const quint8* pData = pRawData + subdiv.offPolylines2;
    const quint8* pEnd = pData + subdiv.lenPolylines2;
    while (pData < pEnd) {
      RgnLine ln;
      // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
      pData += ln.decodeExt(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, true, pData, pEnd);

      if (isCompletelyOutside(ln.points, subdiv.area)) {
        // qDebug() << "[WARN] Skip polygons outside subdiv area:" << subdiv.area << ln.points.toList().first(10);
        ++warnSkipOutside;
        continue;
      }

      if (strtbl && ln.lbl_info) {
        if (ln.hasNet1Label) {
          ++hasNet1LabelCount;
          // qDebug() << "[ln] 2 hasNet1Label" << Qt::hex << ln.lbl_info;
          strtbl->get(srcFile, ln.lbl_info, StrTbl::net, ln.labels);
        } else {
          strtbl->get(srcFile, ln.lbl_info, StrTbl::lbl, ln.labels);
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
    const quint8* pData = pRawData + subdiv.offPoints2;
    const quint8* pEnd = pData + subdiv.lenPoints2;
    while (pData < pEnd) {
      RgnPoint pt;
      pData += pt.decodeExt(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData, pEnd);

      if (!subdiv.area.contains(pt.pos)) {
        // qDebug() << "[WARN] Skip polygons outside subdiv area:" << subdiv.area << pt.pos;
        ++warnSkipOutside;
        continue;
      }

      if (strtbl) {
        pt.isLbl6 ? strtbl->get(srcFile, pt.lbl_ptr, StrTbl::poi, pt.labels) : strtbl->get(srcFile, pt.lbl_ptr, StrTbl::lbl, pt.labels);
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

void ImgDump::writeCsv(QFile& dstFile, pointtype_t& points, pointtype_t& pois, polytype_t& polylines, polytype_t& polygons, quint32 level) {
  int count;
  int ptErrors = 0;
  int poErrors = 0;
  int lnErrors = 0;
  int pgErrors = 0;

  count = 0;

  QString strPoints = "";
  for (const RgnPoint& pt : points) {
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
  for (const RgnPoint& po : pois) {
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
  for (const RgnLine& ln : polylines) {
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
  for (const RgnLine& pg : polygons) {
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

void ImgDump::writeMp(QFile& dstFile, pointtype_t& points, pointtype_t& pois, polytype_t& polylines, polytype_t& polygons, quint32 level) {
  int count = 0;
  int poiErrors = 0;
  int ptErrors = 0;
  int lnErrors = 0;
  int pgErrors = 0;
  for (const RgnPoint& pt : points) {
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

    // dstFile.write(codec->fromUnicode(tmpPoints));
    dstFile.write(tmpPoints.toUtf8());
    dstFile.flush();
  }

  count = 0;
  for (const RgnPoint& poi : pois) {
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

    // dstFile.write(codec->fromUnicode(tmpPois));
    dstFile.write(tmpPois.toUtf8());
    dstFile.flush();
  }

  count = 0;
  // qDebug() << "total polylines:" << polylines.length();
  for (const RgnLine& ln : polylines) {
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

    // dstFile.write(codec->fromUnicode(tmpPolylines));
    dstFile.write(tmpPolylines.toUtf8());
    dstFile.flush();
  }

  count = 0;
  for (const RgnLine& pg : polygons) {
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

    // dstFile.write(codec->fromUnicode(tmpPolygons));
    dstFile.write(tmpPolygons.toUtf8());
    dstFile.flush();
  }

  if (poiErrors) {
    // qWarning() << "[poi] Invalid type:" << poiErrors;
    ++warnInvalidType;
  }
}

inline double normalizeLng(double lngDeg) {
  while (lngDeg > 180.0) lngDeg -= 360.0;
  while (lngDeg < -180.0) lngDeg += 360.0;
  return lngDeg;
}

inline double clampLat(double latDeg) {
  if (latDeg > 90.0) latDeg = 90.0;
  if (latDeg < -90.0) latDeg = -90.0;
  return latDeg;
}

inline QPointF toDegreesSafe(const QPointF& pointRad) {
  double lat = qRadiansToDegrees(pointRad.y());
  double lng = qRadiansToDegrees(pointRad.x());

  lat = clampLat(lat);
  lng = normalizeLng(lng);
  if (qAbs(lat) == 90.0 || qAbs(lng) == 180.0) {
    // qDebug() << "[WARN] Invalid coords:" << lat << lng;
    ++warnInvalidCoords;
  }
  return QPointF(lng, lat);
}

#ifdef SANITY_CHECK
inline double haversineDistance(double lat1, double lon1, double lat2, double lon2) {
  static const double R = 6371000.0;  // earth radius in meters

  double dLat = qDegreesToRadians(lat2 - lat1);
  double dLon = qDegreesToRadians(lon2 - lon1);

  double a = qSin(dLat / 2) * qSin(dLat / 2) + qCos(qDegreesToRadians(lat1)) * qCos(qDegreesToRadians(lat2)) * qSin(dLon / 2) * qSin(dLon / 2);

  double c = 2 * qAtan2(qSqrt(a), qSqrt(1 - a));
  return R * c;  // in meters
}

inline bool isSuspiciousSegment(const QPointF& p1, const QPointF& p2, double maxDistanceMeters = 50000.0) {
  double lat1 = p1.y();
  double lon1 = p1.x();
  double lat2 = p2.y();
  double lon2 = p2.x();

  double dist = haversineDistance(lat1, lon1, lat2, lon2);
  return dist > maxDistanceMeters;
}
#endif

QString ImgDump::convPtDegStr(const QPointF& pointRad, bool wkt) {
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

QString ImgDump::convLnDegStr(const QPolygonF& polyline, bool isLine, bool wkt) {
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

  for (const QPointF& point : polyline) {
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

void ImgDump::writeHeader(QFile& dstFile, const submap_t& submap) {
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

  for (const auto& ml : submap.mapLevels) {
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

  // dstFile.write(codec->fromUnicode(headerStr));
  dstFile.write(headerStr.toUtf8());
  dstFile.flush();
}

int main(int argc, char* argv[]) {
  int result = 0;
  try {
    QElapsedTimer timerMain;
    timerMain.start();
    ImgDump app(argc, argv);
    qDebug() << "Run time:" << QString::number(timerMain.elapsed() / 1000) << "sec.";
  } catch (const Exception& e) {
    fflush(stdout);
    fflush(stderr);
    std::cerr << std::endl << "Kaboom!" << std::endl << QString(e).toUtf8().constData();
    result = -1;
  }
  return result;
}
