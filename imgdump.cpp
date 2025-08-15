#include <QStack>
#include <QRegularExpression>
#include <QPen>
#include <QtCore5Compat/QTextCodec>
#include <QFileInfo>
#include <QDebug>
#include <QCoreApplication>

#define DBG

#define HOST_IS_64_BIT

#define DEBUG_SHOW_SECT_DESC
#define DEBUG_SHOW_TRE_DATA
#define DEBUG_SHOW_MAPLEVEL_DATA
#define DEBUG_SHOW_SUBDIV_DATA
#define DEBUG_SHOW_MAPLEVELS
// #define DEBUG_SHOW_POINTS
#define DEBUG_SHOW_POLY_DATA_SUBDIV
#define DEBUG_SHOW_POLY_DATA_DECODE1
// #define DEBUG_SHOW_POLY_DATA_DECODE2
#define DEBUG_SHOW_POLY_PTS

#define NOFLOAT 1000000000000.0
#define NOINT 0x7FFFFFFF
#define NOTIME 0xFFFFFFFF
#define NOIDX (-1)

#define PI M_PI
#define TWOPI (2 * PI)

#define RAD_TO_DEG 57.295779513082321
#define DEG_TO_RAD .017453292519943296

#define GARMIN_DEG(x) ((x) < 0x800000 ? (qreal)(x) * 360.0 / 16777216.0 : (qreal)((x) - 0x1000000) * 360.0 / 16777216.0)
#define GARMIN_RAD(x) \
  ((x) < 0x800000 ? (qreal)(x) * (2 * M_PI) / 16777216.0 : (qreal)((x) - 0x1000000) * (2 * M_PI) / 16777216.0)
typedef quint8 quint24[3];

// little endian platform: just return the argument
#define gar_endian(t, x) (t)(x)

// macros to deal with pointers or unaligned arguments
// load argument of type t from pointer p
#define gar_ptr_load(t, p) __gar_ptr_load_##t((const uint8_t *)(p))

// store argument src of type t in in the location to which the pointer p points
#define gar_ptr_store(t, p, src) __gar_ptr_store_##t((uint8_t *)(p), (src))

// load argument x of type t - noop with proper cast
#define gar_load(t, x) (t)(x)

// store argument src of type t in the variable dst of type t - just assign
#define gar_store(t, dst, src) (dst) = (src)

// load from pointer - simply map memory
#define __gar_ptr_load_int16_t(p) (*((int16_t *)(p)))
#define __gar_ptr_load_int32_t(p) (*((int32_t *)(p)))
#define __gar_ptr_load_int64_t(p) (*((int64_t *)(p)))
#define __gar_ptr_load_uint16_t(p) (*((uint16_t *)(p)))
#define __gar_ptr_load_quint32(p) (*((quint32 *)(p)))
#define __gar_ptr_load_uint64_t(p) (*((uint64_t *)(p)))
#define __gar_ptr_load_float(p) (*((float *)(p)))
#define __gar_ptr_load_double(p) (*((double *)(p)))
#define __gar_ptr_load_uint24_t(p) (__gar_ptr_load_quint32(p) & 0x00FFFFFFu)
#define __gar_ptr_load_int24_t(p) (__gar_ptr_load_int32_t(p) & 0x00FFFFFFu)
#define __gar_ptr_store_int16_t(p, src) (*((int16_t *)(p))) = (src)
#define __gar_ptr_store_int32_t(p, src) (*((int32_t *)(p))) = (src)
#define __gar_ptr_store_int64_t(p, src) (*((int64_t *)(p))) = (src)
#define __gar_ptr_store_uint16_t(p, src) (*((uint16_t *)(p))) = (src)
#define __gar_ptr_store_quint32(p, src) (*((quint32 *)(p))) = (src)
#define __gar_ptr_store_uint64_t(p, src) (*((uint64_t *)(p))) = (src)
#define __gar_ptr_store_float(p, src) (*((float *)(p))) = (src)
#define __gar_ptr_store_double(p, src) (*((double *)(p))) = (src)

typedef struct
{
  double u, v;
} PJ_UV;

struct subdiv_desc_t;
struct sign_info_t
{
  quint32 sign_info_bits = 2;
  bool x_has_sign = true;
  bool nx = false;
  bool y_has_sign = true;
  bool ny = false;
};

static inline void __gar_ptr_store_int24_t(uint8_t *p, int32_t src)
{
  __gar_ptr_store_uint16_t(p, src & 0xffffu);
  p[2] = src >> 16;
}

static inline void __gar_ptr_store_uint24_t(uint8_t *p, quint32 src)
{
  __gar_ptr_store_uint16_t(p, src & 0xffffu);
  p[2] = src >> 16;
}

inline void GPS_Math_DegMin_To_Deg(bool sign, const qint32 d, const qreal m, qreal &deg)
{
  deg = qAbs(d) + m / 60.0;
  if (sign)
  {
    deg = -deg;
  }
}

inline void GPS_Math_DegMinSec_To_Deg(bool sign, const qint32 d, const qint32 m, const qreal s, qreal &deg)
{
  deg = qAbs(d) + qreal(m) / 60.0 + s / 3600;
  if (sign)
  {
    deg = -deg;
  }
}

inline bool GPS_Math_Deg_To_DegMin(qreal v, qint32 *deg, qreal *min)
{
  *deg = qAbs(v);
  *min = (qAbs(v) - *deg) * 60.0;

  return v < 0;
}

class CFileExt : public QFile
{
public:
  CFileExt(const QString &filename) : QFile(filename), mapped(nullptr) { cnt++; }
  ~CFileExt() { cnt--; }

  // data access function
  const char *data(qint64 offset, qint64 s)
  {
    uchar *p = map(offset, s);
    return (const char *)p;
  }

private:
  static int cnt;

  uchar *mapped;
  QSet<uchar *> mappedSections;
};

class IGarminStrTbl : public QObject
{
public:
  IGarminStrTbl(const quint16 codepage, const quint8 mask, QObject *parent) : QObject(parent), codepage(codepage), mask(mask)
  {
    if (codepage != 0)
    {
      if (1250 <= codepage && codepage <= 1258)
      {
        char strcp[64];
        sprintf(strcp, "Windows-%i", codepage);
        codec = QTextCodec::codecForName(strcp);
      }
      else if (codepage == 950)
      {
        codec = QTextCodec::codecForName("Big5");
      }
      else if (codepage == 850)
      {
        codec = QTextCodec::codecForName("IBM 850");
      }
      else if (codepage == 65001)
      {
        codec = QTextCodec::codecForName("UTF-8");
      }
      else
      {
        qDebug() << "unknown codepage:" << codepage << "0x" << Qt::hex << codepage;
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

  virtual ~IGarminStrTbl() {};

  enum type_e
  {
    norm,
    poi,
    net
  };

  void registerLBL1(const quint32 offset, const quint32 size, const quint8 shift)
  {
    offsetLBL1 = offset;
    sizeLBL1 = size;
    addrshift1 = shift;
  }
  void registerLBL6(const quint32 offset, const quint32 size)
  {
    offsetLBL6 = offset;
    sizeLBL6 = size;
  }
  void registerNET1(const quint32 offset, const quint32 size, const quint8 shift)
  {
    offsetNET1 = offset;
    sizeNET1 = size;
    addrshift2 = shift;
  }

  virtual void get(CFileExt &file, quint32 offset, type_e t, QStringList &info) = 0;

protected:
  void readFile(CFileExt &file, quint32 offset, quint32 size, QByteArray &data)
  {
    if (offset + size > file.size())
    {
      //         throw exce_t(eErrOpen, tr("Failed to read: ") + file.filename());
      return;
    }

    data = QByteArray::fromRawData(file.data(offset, size), size);
    // wenn mask == 0 ist kein xor noetig
    if (mask == 0)
    {
      return;
    }

#ifdef HOST_IS_64_BIT
    quint64 *p64 = (quint64 *)data.data();
    for (quint32 i = 0; i < size / 8; ++i)
    {
      *p64++ ^= mask64;
    }
    quint32 rest = size % 8;
    quint8 *p = (quint8 *)p64;
#else
    quint32 *p32 = (quint32 *)data.data();
    for (quint32 i = 0; i < size / 4; ++i)
    {
      *p32++ ^= mask32;
    }
    quint32 rest = size % 4;
    quint8 *p = (quint8 *)p32;
#endif

    for (quint32 i = 0; i < rest; ++i)
    {
      *p++ ^= mask;
    }
  }

  quint32 calcOffset(CFileExt &file, const quint32 offset, type_e t)
  {
    quint32 newOffset = offset;

    if (t == poi)
    {
      QByteArray buffer;
      readFile(file, offsetLBL6 + offset, sizeof(quint32), buffer);
      newOffset = gar_ptr_load(quint32, buffer.data());
      newOffset = (newOffset & 0x003FFFFF);
    }
    else if (t == net)
    {
      if (offsetNET1 == 0)
      {
        return 0xFFFFFFFF;
      }

      QByteArray data;
      readFile(file, offsetNET1 + (offset << addrshift2), sizeof(quint32), data);
      newOffset = gar_ptr_load(quint32, data.data());
      if (newOffset & 0x00400000)
      {
        return 0xFFFFFFFF;
      }
      newOffset = (newOffset & 0x003FFFFF);
    }

    newOffset <<= addrshift1;
    //     qDebug() << Qt::hex << newOffset;
    return newOffset;
  }

  QString processLabel(const char *buffer, unsigned lastSeperator)
  {
    QString label;
    if (codepage != 0)
    {
      label = codec->toUnicode(buffer);
    }
    else
    {
      label = buffer;
    }

    if (lastSeperator == 0x1F)
    {
      bool ok = false;
      qreal ele = label.toDouble(&ok);
      if (ok)
      {
        QString val, unit;
        // IUnit::self().feet2elevation(ele, val, unit);
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

  // conversion of strings
  quint16 codepage;
  QTextCodec *codec = nullptr;
  const quint8 mask;
  quint32 mask32;
  quint64 mask64;

  char buffer[1025];
};

class CGarminStrTbl6 : public IGarminStrTbl
{
public:
  CGarminStrTbl6(const quint16 codepage, const quint8 mask, QObject *parent) : IGarminStrTbl(codepage, mask, parent) {};
  virtual ~CGarminStrTbl6() {};

  void get(CFileExt &file, quint32 offset, type_e t, QStringList &labels) override
  {
    labels.clear();

    offset = calcOffset(file, offset, t);

    if (offset == 0xFFFFFFFF)
    {
      return;
    }

    if (offset > (quint32)sizeLBL1)
    {
      //         qWarning() << "Index into string table to large" << Qt::hex << offset << dataLBL.size() <<
      //         hdrLbl->addr_shift << hdrNet->net1_addr_shift;
      return;
    }

    quint8 c1 = 0;
    quint8 c2 = 0;
    quint32 idx = 0;
    reg = 0;
    bits = 0;

    QByteArray data;
    quint32 size = (sizeLBL1 - offset) < 200 ? (sizeLBL1 - offset) : 200;

    readFile(file, offsetLBL1 + offset, size, data);

    p = (quint8 *)data.data();

    fill();

    unsigned lastSeperator = 0;
    while (idx < (sizeof(buffer) - 1))
    {
      c1 = reg >> 26;
      reg <<= 6;
      bits -= 6;
      fill();
      // terminator
      if (c1 > 0x2F)
      {
        break;
      }

      c2 = str6tbl1[c1];
      if (c2 == 0)
      {
        if (c1 == 0x1C)
        {
          c1 = reg >> 26;
          reg <<= 6;
          bits -= 6;
          fill();
          buffer[idx++] = str6tbl2[c1];
        }
        else if (c1 == 0x1B)
        {
          c1 = reg >> 26;
          reg <<= 6;
          bits -= 6;
          fill();
          buffer[idx++] = str6tbl3[c1];
        }
        else if (c1 > 0x1C && c1 < 0x20)
        {
          lastSeperator = c1;
          buffer[idx] = 0;
          if (strlen(buffer))
          {
            labels << processLabel(buffer, lastSeperator);
          }
          idx = 0;
          buffer[0] = 0;
        }
      }
      else
      {
        buffer[idx++] = str6tbl1[c1];
      }
    }

    buffer[idx] = 0;
    if (strlen(buffer))
    {
      labels << processLabel(buffer, lastSeperator);
    }
  }

private:
  inline static const char str6tbl1[] = {' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 0, 0, 0, 0, 0, '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 0, 0, 0, 0, 0, 0};
  inline static const char str6tbl2[] = {'@', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ':', ';', '<', '=', '>', '?', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '[', '\\', ']', '^', '_'};
  inline static const char str6tbl3[] = {'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};

  void fill()
  {
    quint32 tmp;
    if (bits < 6)
    {
      tmp = *p++;
      reg |= tmp << (24 - bits);
      bits += 8;
    }
  }

  /// temp shift reg buffer
  quint32 reg = 0;
  /// bits in buffer
  quint32 bits = 0;
  /// pointer to current data;
  const quint8 *p = nullptr;
};

class CGarminStrTblUtf8 : public IGarminStrTbl
{
public:
  CGarminStrTblUtf8(const quint16 codepage, const quint8 mask, QObject *parent) : IGarminStrTbl(codepage, mask, parent) {};
  virtual ~CGarminStrTblUtf8() {};

  void get(CFileExt &file, quint32 offset, type_e t, QStringList &labels) override
  {
    labels.clear();
    offset = calcOffset(file, offset, t);

    if (offset == 0xFFFFFFFF)
    {
      return;
    }

    if (offset > (quint32)sizeLBL1)
    {
      // qWarning() << "Index into string table to large" << Qt::hex << offset << dataLBL.size() << hdrLbl->addr_shift <<
      // hdrNet->net1_addr_shift;
      return;
    }

    QByteArray data;
    quint32 size = (sizeLBL1 - offset) < 200 ? (sizeLBL1 - offset) : 200;
    readFile(file, offsetLBL1 + offset, size, data);
    char *lbl = data.data();

    char *pBuffer = buffer;
    *pBuffer = 0;

    unsigned lastSeperator = 0;
    while (*lbl != 0)
    {
      if ((unsigned)*lbl >= 0x1B && (unsigned)*lbl <= 0x1F)
      {
        lastSeperator = *lbl;
        *pBuffer = 0;
        if (strlen(buffer))
        {
          labels << processLabel(buffer, lastSeperator);
          pBuffer = buffer;
          *pBuffer = 0;
        }
        ++lbl;
        continue;
      }
      else if ((unsigned)*lbl < 0x07)
      {
        ++lbl;
        continue;
      }
      else
      {
        *pBuffer++ = *lbl++;
      }
    }

    *pBuffer = 0;
    if (strlen(buffer))
    {
      labels << processLabel(buffer, lastSeperator);
    }
  }
};

class CGarminPoint
{
public:
  CGarminPoint() = default;
  virtual ~CGarminPoint() = default;

  quint32 decode1(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8 *pData)
  {
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

#ifdef DEBUG_SHOW_POINTS
    qDebug() << x1 << y1 << point.u << point.v;
#endif

    if (hasSubType)
    {
      type |= *pData;
      return 9;
    }

    return 8;
  }

  quint32 decode2(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, const quint8 *pData, const quint8 *pEnd)
  {
    quint32 byte_size = 6;
    quint8 subtype;

    type = (quint16)(*pData) << 8;
    ++pData;
    subtype = (quint16)(*pData);
    ++pData;

    type = 0x10000 + type + (subtype & 0x1F);

    if (subtype & 0x80)
    {
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

    if (subtype & 0x20)
    {
      byte_size += 3;
      lbl_ptr = gar_ptr_load(uint24_t, pData);
      isLbl6 = lbl_ptr & 0x00400000;
      lbl_ptr &= 0x003FFFFF;
    }

    return byte_size;
  }

  QString getLabelText() const
  {
    QString str;
    if (!labels.isEmpty())
    {
      if ((type == 0x6200) || (type == 0x6300))
      {
        qDebug() << "1" << labels;
        QString unit;
        QString val = labels[0];
        // IUnit::self().meter2elevation(val.toFloat() / 3.28084f, val, unit);
        str = QString("%1 %2").arg(val, unit);
      }
      //        else if(type == 0x6616) //669 DAV
      //        {
      //            qDebug() << "2" << labels;
      //            if(labels.size() > 1)
      //            {
      //                QString unit;
      //                QString val = labels[1];
      //                IUnit::self().meter2elevation(val.toFloat() / 3.28084f, val, unit);
      //                str = QString("%1 %2 %3").arg(labels[0]).arg(val, unit);
      //            }
      //            else
      //            {
      //                str = labels[0];
      //            }
      //        }
      else
      {
        str = labels.join(" ");
      }
    }
    return str;
  }

  bool hasLabel() const { return !labels.isEmpty(); }

  quint32 type = 0;
  bool isLbl6 = false;
  bool hasSubType = false;

  QPointF pos;

  QStringList labels;

  quint32 lbl_ptr = 0xFFFFFFFF;
};

class CShiftReg
{
public:
  CShiftReg(const quint8 *pData, quint32 n, quint32 bx, quint32 by, bool extra_bit, sign_info_t &si)
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
        extraBit(extra_bit)
  {
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

  bool get(qint32 &x, qint32 &y)
  {
    x = y = 0;
    if (bits < (bits_per_coord))
    {
      return false;
    }

    // don't know what to do with it -> skip extra bit
    if (extraBit)
    {
      reg >>= 1;
      bits -= 1;
    }

    if (sinfo.x_has_sign)
    {
      qint32 tmp = 0;
      while (1)
      {
        tmp = reg & xmask;
        if (tmp != xsign)
        {
          break;
        }
        x += tmp - 1;
        reg >>= bits_per_x;
        bits -= bits_per_x;
        fill(bits_per_y + bits_per_x);
      }
      if (tmp < xsign)
      {
        x += tmp;
      }
      else
      {
        x = tmp - (xsign2)-x;
      }
    }
    else
    {
      x = reg & xmask;
      if (sinfo.nx)
      {
        x = -x;
      }
    }
    reg >>= bits_per_x;
    bits -= bits_per_x;

    // take y coord., add sign if necessary, shift register by bits per y coord.
    if (sinfo.y_has_sign)
    {
      qint32 tmp = 0;
      while (1)
      {
        tmp = reg & ymask;
        if (tmp != ysign)
        {
          break;
        }
        y += tmp - 1;
        reg >>= bits_per_y;
        bits -= bits_per_y;
        fill(bits_per_y);
      }
      if (tmp < ysign)
      {
        y += tmp;
      }
      else
      {
        y = tmp - (ysign2)-y;
      }
    }
    else
    {
      y = reg & ymask;
      if (sinfo.ny)
      {
        y = -y;
      }
    }
    reg >>= bits_per_y;
    bits -= bits_per_y;

    // fill register until it has enough bits for one coord. pair again
    fill(bits_per_coord);
    return true;
  }

private:
  void fill(quint32 b)
  {
    quint64 tmp = 0;
    while ((bits < b) && bytes)
    {
#if (Q_BYTE_ORDER == Q_LITTLE_ENDIAN)
      (quint8 &)tmp = *pData++;
#else
      tmp = *pData++;
#endif
      --bytes;

      reg |= tmp << bits;
      bits += 8;
    }
  }

  /// the register to work on
  quint64 reg;
  /// the data stream to get data from
  const quint8 *pData;

  quint32 bytes;         //< bytes left in stream
  quint32 xmask;         //< bitmask x coord.
  quint32 ymask;         //< bitmask y coord.
  qint32 xsign;          //< sign bit for x value
  qint32 ysign;          //< sign bit for y value
  qint32 xsign2;         //< sign bit * 2 for x value
  qint32 ysign2;         //< sign bit * 2 for y value
  quint8 bits;           //< total bits in register
  quint8 bits_per_x;     //< bits per x coord.
  quint8 bits_per_y;     //< bits per y coord.
  quint8 bits_per_coord; //< bits per coord.

  sign_info_t &sinfo;

  bool extraBit;
};

class CGarminPolygon
{
private:
  QString convertPolygonToDegreesString(const QPolygonF &polygon)
  {
    QString result;
    const double radToDeg = 180.0 / M_PI;

    for (const QPointF &point : polygon)
    {
      // Конвертиране от радиани в градуси
      double lon = point.x() * radToDeg;
      double lat = point.y() * radToDeg;

      // Форматиране с 5 знака след десетичната запетая
      QString pointStr = QString("(%1,%2)").arg(lat, 0, 'f', 5).arg(lon, 0, 'f', 5);

      // Добавяне към резултата (с запетая между точките)
      if (!result.isEmpty())
      {
        result += ",";
      }
      result += pointStr;
    }

    return "Data0=" + result;
  }

public:
  CGarminPolygon() = default;
  virtual ~CGarminPolygon() = default;

  QString debugData(const quint8 *pData, int len)
  {
    QByteArray byteArray(reinterpret_cast<const char *>(pData), len);
    QString str = byteArray.toHex(' '); // с интервал между байтовете
    return str;
  }

  quint32 decode1(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8 *pData, const quint8 *pEnd)
  {
    QString rawData = debugData(pData, sizeof(pData));
    QString rawData2 = debugData(pEnd, sizeof(pEnd));
    quint32 bytes_total = 10;
    // bitstream has a two byte length
    bool two_byte_len;
    // coordinates use extra bit - ??? have never seen it
    bool extra_bit;
    // bitstream length
    quint16 bs_len = 0;
    // base bit size info for coordinates
    quint8 bs_info;
    // bits per x coord.
    quint32 bx;
    // bits per y coord.
    quint32 by;
    const quint8 *const pStart = pData;

    labels.clear();
    coords.resize(0);
    coords.reserve(maxVecSize);

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
    if (line)
    {
      direction = (type & 0x40);
      type &= 0x3F;
    }
    else
    {
      type &= 0x7F;
    }

    /* label info
        bit 0..21   off set into LBL section
        bit 22      use extra bit for coordinates
        bit 23      use label data of NET section
     */
    lbl_info = gar_ptr_load(uint24_t, pData);
    lbl_in_NET = lbl_info & 0x800000;
    extra_bit = lbl_info & 0x400000;
    lbl_info = lbl_info & 0x3FFFFF;

    pData += 3;

    //     qDebug() << Qt::hex << lbl_in_NET << extra_bit << lbl_info;

    // delta longitude and latitude
    dLng = gar_ptr_load(uint16_t, pData);
    pData += 2;
    dLat = gar_ptr_load(uint16_t, pData);
    pData += 2;

    // bitstream length
    if (two_byte_len)
    {
      bs_len = gar_ptr_load(uint16_t, pData);
      pData += 2;
      bytes_total += bs_len + 1;
    }
    else
    {
#if (Q_BYTE_ORDER == Q_LITTLE_ENDIAN)
      (quint8 &)bs_len = *pData++;
#else
      bs_len = *pData++;
#endif
      bytes_total += bs_len;
    }

    if (pEnd && ((pStart + bytes_total) > pEnd))
    {
      return bytes_total;
    }

    /* bitstream info
        bit 0..3    base bits longitude
        bit 4..7    base bits latitude
     */
    bs_info = *pData++;

    // if(extra_bit) qWarning("extrabit");

#ifdef DEBUG_SHOW_POLY_DATA_DECODE1
    qDebug()
        << "type:      " << type << Qt::hex;
    qDebug() << "two byte:  " << two_byte_len;
    qDebug() << "extra bit: " << extra_bit;
    qDebug() << "dLng:      " << dLng;
    qDebug() << "dLat:      " << dLat;
    qDebug() << "len:       " << bs_len;
    qDebug() << "info:      " << Qt::hex << bs_info;
    qDebug() << "1st byte:  " << Qt::hex << *pData;
    qDebug() << "bytes total" << bytes_total;
    qDebug() << "data1      " << rawData;
    qDebug() << "data2      " << rawData2;
#endif

    sign_info_t signinfo;
    bits_per_coord(bs_info, *pData, bx, by, signinfo, false);

    CShiftReg sr(pData, bs_len, bx, by, extra_bit, signinfo);
    qint32 x1, y1, x = 0, y = 0;

    bool isNegative = (iCenterLon >= 0x800000);
    // first point
    x1 = ((qint32)dLng << shift) + iCenterLon;
    y1 = ((qint32)dLat << shift) + iCenterLat;

    if (x1 >= 0x800000 && !isNegative)
    {
      x1 = 0x7fffff;
    }

    coords << QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));

    // next points
    while (sr.get(x, y))
    {
      x1 += (x << shift);
      y1 += (y << shift);

      if (x1 >= 0x800000 && !isNegative)
      {
        x1 = 0x7fffff;
      }

      coords << QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));
    }

    id = cnt++;
    //     qDebug() << "<<<" << id;

    if (maxVecSize < coords.size())
    {
      maxVecSize = coords.size();
    }
    if (coords.size() * 1.2 < maxVecSize)
    {
      coords.squeeze();
    }

    pixel = coords;

    qDebug() << "decode1() coords:" << type << Qt::hex << type << Qt::hex << bs_info << coords << convertPolygonToDegreesString(coords) << id;

    return bytes_total;
  }

  // subtype?
  quint32 decode2(qint32 iCenterLon, qint32 iCenterLat, quint32 shift, bool line, const quint8 *pData, const quint8 *pEnd)
  {
    quint32 bytes_total = 6;
    // bitstream length
    quint16 bs_len = 0;
    // type and subtype
    quint32 subtype;
    // base bit size info for coordinates
    quint8 bs_info;
    // bits per x coord.
    quint32 bx;
    // bits per y coord.
    quint32 by;

    const quint8 *const pStart = pData;

    labels.clear();
    coords.resize(0);
    coords.reserve(maxVecSize);

    type = *pData++;
    subtype = *pData++;

    type = 0x10000 + (quint16(type) << 8) + (subtype & 0x1f);
    hasV2Label = subtype & 0x20;
    // delta longitude and latitude
    dLng = gar_ptr_load(uint16_t, pData);
    pData += 2;
    dLat = gar_ptr_load(uint16_t, pData);
    pData += 2;

    if ((*pData & 0x1) == 0)
    {
      bs_len = gar_ptr_load(uint16_t, pData);
      bs_len = (bs_len >> 2) - 1;
      pData += 2;
      bytes_total += 2;
    }
    else
    {
      bs_len = ((*pData) >> 1) - 1;
      pData += 1;
      bytes_total += 1;
    }

    bs_info = *pData++;
    bytes_total += bs_len + 1;

#ifdef DEBUG_SHOW_POLY_DATA_DECODE2
    qDebug() << "type:      " << type << Qt::hex << type;
    qDebug() << "dLng:      " << dLng;
    qDebug() << "dLat:      " << dLat;
    qDebug() << "len:       " << bs_len;
    qDebug() << "info:      " << Qt::hex << bs_info;
    qDebug() << "1st byte:  " << Qt::hex << *pData;
    qDebug() << "bytes total" << bytes_total;
#endif

    sign_info_t signinfo;
    bits_per_coord(bs_info, *pData, bx, by, signinfo, true);

    //     qDebug() << ">>" << bs_len << bytes_total << (pEnd - pStart);

    //     assert((pEnd - pStart) >= bytes_total);
    if (((quint32)(pEnd - pStart)) < bytes_total)
    {
      return pEnd - pStart;
    }

    CShiftReg sr(pData, bs_len, bx, by, false, signinfo);
    qint32 x1, y1, x = 0, y = 0;

    bool isNegative = (iCenterLon >= 0x800000);
    // first point
    x1 = ((qint32)dLng << shift) + iCenterLon;
    y1 = ((qint32)dLat << shift) + iCenterLat;

    if (x1 >= 0x800000 && !isNegative)
    {
      x1 = 0x7fffff;
    }

    coords << QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));

    // next points
    while (sr.get(x, y))
    {
      x1 += (x << shift);
      y1 += (y << shift);

      if (x1 >= 0x800000 && !isNegative)
      {
        x1 = 0x7fffff;
      }

      //        xy.u = GARMIN_RAD(x1);
      //        xy.v = GARMIN_RAD(y1);

      //        if(qAbs(xy.v) > 2*M_PI || qAbs(xy.u) > 2*M_PI)
      //        {
      //            qDebug() << "bam";
      //            qDebug() << xy.u << xy.v << pStart << pEnd << (pEnd - pStart) << (cnt + 1) << line;
      //            //assert(0);
      //        }
      // #ifdef DEBUG_SHOW_POLY_PTS
      //        qDebug() << xy.u << xy.v << (RAD_TO_DEG * xy.u) << (RAD_TO_DEG * xy.v);
      // #endif
      coords << QPointF(GARMIN_RAD(x1), GARMIN_RAD(y1));
    }

    if (hasV2Label)
    {
      quint32 offset = gar_ptr_load(uint24_t, pData + bs_len);
      bytes_total += 3;
      lbl_info = offset & 0x3FFFFF;
    }
    else
    {
      lbl_info = 0;
    }

    id = cnt++;
    //     qDebug() << "<<<" << id;

    if (maxVecSize < coords.size())
    {
      maxVecSize = coords.size();
    }
    if (coords.size() * 1.2 < maxVecSize)
    {
      coords.squeeze();
    }

    pixel = coords;

    // qDebug() << "decode2() coords:" << type << Qt::hex << type << bs_info << coords;
    qDebug() << "decode2() coords:" << type << Qt::hex << type << Qt::hex << subtype << Qt::hex << bs_info << coords << convertPolygonToDegreesString(coords);

    return bytes_total;
  }

  QString getLabelText() const
  {
    QString str;

    switch (type)
    {
    case 0x23: //< "Minor depth contour"
    case 0x20: //< "Minor land contour"
    case 0x24: //< "Intermediate depth contour"
    case 0x21: //< "Intermediate land contour"
    case 0x25: //< "Major depth contour"
    case 0x22: //< "Major land contour"
    {
      QString unit;
      QString val = labels[0];
      // IUnit::self().meter2elevation(val.toFloat() / 3.28084f, val, unit);
      str = QString("%1 %2").arg(val, unit);
    }
    break;

    default:
      str = labels.join(" ").simplified();
    }

    return str;
  }

  bool hasLabel() const { return !labels.isEmpty(); }

  quint32 type = 0;
  /// direction of line (polyline, only)
  bool direction = false;
  /// the label offset
  quint32 lbl_info = 0;
  /// true if label offset has to be used in NET section
  bool lbl_in_NET = false;
  ///
  bool hasV2Label = false;
  /// delta longitude from subdivision center
  qint16 dLng = 0;
  /// delta latitude from subdivision center
  qint16 dLat = 0;
  /** @brief the actual polyline points as [pixel]
  @note After decode1() or decode2() the content will be the same as coords.
  It is up to the render object to convert it into pixel coordinates
  */
  QPolygonF pixel;
  /// the actual polyline points as longitude / latitude [rad]
  QPolygonF coords;

  quint32 id = 0;

  QStringList labels;

  inline static quint32 cnt = 0;
  inline static qint32 maxVecSize = 0;

private:
  void bits_per_coord(quint8 base, quint8 bfirst, quint32 &bx, quint32 &by, sign_info_t &signinfo, bool isVer2)
  {
    bool x_sign_same, y_sign_same;

    quint8 mask = 0x1;

    //     x_sign_same = bfirst & 0x1;
    x_sign_same = bfirst & mask;
    mask <<= 1;

    if (x_sign_same)
    {
      signinfo.x_has_sign = false;
      //         signinfo.nx         = bfirst & 0x2;
      signinfo.nx = bfirst & mask;
      mask <<= 1;
      ++signinfo.sign_info_bits;
    }
    else
    {
      signinfo.x_has_sign = true;
    }
    bx = bits_per_coord(base & 0x0F, signinfo.x_has_sign);

    //     y_sign_same = x_sign_same ? (bfirst & 0x04) : (bfirst & 0x02);
    y_sign_same = bfirst & mask;
    mask <<= 1;

    if (y_sign_same)
    {
      signinfo.y_has_sign = false;
      //         signinfo.ny         = x_sign_same ? bfirst & 0x08 : bfirst & 0x04;
      signinfo.ny = bfirst & mask;
      mask <<= 1;
      ++signinfo.sign_info_bits;
    }
    else
    {
      signinfo.y_has_sign = true;
    }

    by = bits_per_coord((base >> 4) & 0x0F, signinfo.y_has_sign);

    // Determine extra bits.
    if (isVer2)
    {
      ++signinfo.sign_info_bits;
      if (bfirst & mask)
      {
        //             qDebug() << "V2";
        ++bx;
        ++by;
      }
    }
  }

  int bits_per_coord(quint8 base, bool is_signed)
  {
    int n = 2;

    if (base <= 9)
    {
      n += base;
    }
    else
    {
      n += (2 * base - 9);
    }

    if (is_signed)
    {
      ++n;
    }
    return n;
  }
};

class CGarminTyp
{
public:
  CGarminTyp() = default;
  virtual ~CGarminTyp() = default;

  enum label_type_e
  {
    eStandard = 0,
    eNone = 1,
    eSmall = 2,
    eNormal = 3,
    eLarge = 4
  };

  struct polyline_property
  {
    polyline_property()
        : type(0),
          penLineDay(Qt::magenta, 3),
          penLineNight(Qt::magenta, 3),
          hasBorder(false),
          penBorderDay(Qt::NoPen),
          penBorderNight(Qt::NoPen),
          hasPixmap(false),
          labelType(eStandard),
          colorLabelDay(Qt::black),
          colorLabelNight(Qt::black),
          known(false)

    {
    }

    polyline_property(quint16 type, const QPen &penLineDay, const QPen &penLineNight, const QPen &penBorderDay,
                      const QPen &penBorderNight)
        : type(type),
          penLineDay(penLineDay),
          penLineNight(penLineNight),
          hasBorder(true),
          penBorderDay(penBorderDay),
          penBorderNight(penBorderNight),
          hasPixmap(false),
          labelType(eStandard),
          colorLabelDay(Qt::black),
          colorLabelNight(Qt::black),
          known(true) {}

    polyline_property(quint16 type, const QColor &color, int width, Qt::PenStyle style)
        : type(type),
          penLineDay(QPen(color, width, style)),
          penLineNight(penLineDay),
          hasBorder(false),
          penBorderDay(Qt::NoPen),
          penBorderNight(Qt::NoPen),
          hasPixmap(false),
          labelType(eStandard),
          colorLabelDay(Qt::black),
          colorLabelNight(Qt::black),
          known(true) {}

    quint16 type;

    QPen penLineDay;
    QPen penLineNight;

    bool hasBorder;
    QPen penBorderDay;
    QPen penBorderNight;

    bool hasPixmap;
    QImage imgDay;
    QImage imgNight;

    QMap<int, QString> strings;
    label_type_e labelType;
    QColor colorLabelDay;
    QColor colorLabelNight;

    bool known;
  };

  struct polygon_property
  {
    polygon_property()
        : type(0),
          pen(Qt::magenta),
          brushDay(Qt::magenta, Qt::BDiagPattern),
          brushNight(Qt::magenta, Qt::BDiagPattern),
          labelType(eStandard),
          colorLabelDay(Qt::black),
          colorLabelNight(Qt::black),
          known(false) {}

    polygon_property(quint16 type, const Qt::PenStyle pensty, const QColor &brushColor, Qt::BrushStyle pattern)
        : type(type),
          pen(pensty),
          brushDay(brushColor, pattern),
          brushNight(brushColor.darker(150), pattern),
          labelType(eStandard),
          colorLabelDay(Qt::black),
          colorLabelNight(Qt::black),
          known(true)
    {
      pen.setWidth(1);
    }

    polygon_property(quint16 type, const QColor &penColor, const QColor &brushColor, Qt::BrushStyle pattern)
        : type(type),
          pen(penColor, 1),
          brushDay(brushColor, pattern),
          brushNight(brushColor.darker(150), pattern),
          labelType(eStandard),
          colorLabelDay(Qt::black),
          colorLabelNight(Qt::black),
          known(true) {}

    quint16 type;
    QPen pen;
    QBrush brushDay;
    QBrush brushNight;

    QMap<int, QString> strings;
    label_type_e labelType;
    QColor colorLabelDay;
    QColor colorLabelNight;
    bool known;
  };

  struct point_property
  {
    point_property() : labelType(eStandard) {}
    QImage imgDay;
    QImage imgNight;

    QMap<int, QString> strings;
    label_type_e labelType;
    QColor colorLabelDay;
    QColor colorLabelNight;
  };

  /// decode typ file
  /**
      This pure virtual function has to be implemented in every subclass. It should
      be the only public function needed. The typ file is read and it's content is
      stored in the passed map/list objects.

      @param in input data stream
      @param polygons reference to polygon properties map
      @param polylines reference to polyline properties map
      @param drawOrder reference to list of polygon draw orders
      @param points reference to point properties map

   */
  bool decode(const QByteArray &array, QMap<quint32, polygon_property> &polygons,
              QMap<quint32, polyline_property> &polylines, QList<quint32> &drawOrder,
              QMap<quint32, point_property> &points)
  {
    QDataStream in(array);
    in.setVersion(QDataStream::Qt_4_5);
    in.setByteOrder(QDataStream::LittleEndian);

    /* Read typ file descriptor */
    quint16 descriptor;
    in >> descriptor;

    qDebug() << "descriptor" << Qt::hex << descriptor;

    if (!parseHeader(in))
    {
      return false;
    }

    if (!parseDrawOrder(in, drawOrder))
    {
      return false;
    }

    if (!parsePolygon(in, polygons))
    {
      return false;
    }

    if (!parsePolyline(in, polylines))
    {
      return false;
    }

    if (!parsePoint(in, points))
    {
      return false;
    }

    return true;
  }

  QSet<quint8> getLanguages() { return languages; }

  quint16 getFid() { return fid; }

  quint16 getPid() { return pid; }

protected:
  virtual bool parseHeader(QDataStream &in)
  {
    int i;
    QString garmintyp;
    quint8 byte;

    for (i = 0; i < 10; ++i)
    {
      in >> byte;
      garmintyp.append(QChar(byte));
    }
    garmintyp.append(0);
    if (garmintyp != "GARMIN TYP")
    {
      qDebug() << "CMapTDB::readTYP() not a known typ file";
      return false;
    }

    /* reading typ creation date string */

    in.device()->seek(0x0c);
    in >> version >> year >> month >> day >> hour >> minutes >> seconds >> codepage;
    month -= 1; /* Month are like Microsoft starting 0 ? */
    year += 1900;

    /* Reading points / lines / polygons struct */
    in >> sectPoints.dataOffset >> sectPoints.dataLength;
    in >> sectPolylines.dataOffset >> sectPolylines.dataLength;
    in >> sectPolygons.dataOffset >> sectPolygons.dataLength;

    in >> pid >> fid;

    /* Read Array datas */
    in >> sectPoints.arrayOffset >> sectPoints.arrayModulo >> sectPoints.arraySize;
    in >> sectPolylines.arrayOffset >> sectPolylines.arrayModulo >> sectPolylines.arraySize;
    in >> sectPolygons.arrayOffset >> sectPolygons.arrayModulo >> sectPolygons.arraySize;
    in >> sectOrder.arrayOffset >> sectOrder.arrayModulo >> sectOrder.arraySize;

#ifdef DBG
    qDebug() << "Version:" << version << "Codepage:" << codepage;
    qDebug() << "PID" << Qt::hex << pid << "FID" << Qt::hex << fid;
    qDebug() << "Points     doff/dlen/aoff/amod/asize:" << Qt::hex << "\t" << sectPoints.dataOffset << "\t"
             << sectPoints.dataLength << "\t" << sectPoints.arrayOffset << "\t" << sectPoints.arrayModulo << "\t"
             << sectPoints.arrayOffset;
    qDebug() << "Polylines  doff/dlen/aoff/amod/asize:" << Qt::hex << "\t" << sectPolylines.dataOffset << "\t"
             << sectPolylines.dataLength << "\t" << sectPolylines.arrayOffset << "\t" << sectPolylines.arrayModulo << "\t"
             << sectPolylines.arrayOffset;
    qDebug() << "Polygons   doff/dlen/aoff/amod/asize:" << Qt::hex << "\t" << sectPolygons.dataOffset << "\t"
             << sectPolygons.dataLength << "\t" << sectPolygons.arrayOffset << "\t" << sectPolygons.arrayModulo << "\t"
             << sectPolygons.arrayOffset;
    qDebug() << "Order      doff/dlen/aoff/amod/asize:" << Qt::hex << "\t" << sectOrder.dataOffset << "\t"
             << sectOrder.dataLength << "\t" << sectOrder.arrayOffset << "\t" << sectOrder.arrayModulo << "\t"
             << sectOrder.arrayOffset;
#endif

    return true;
  }

  virtual bool parseDrawOrder(QDataStream &in, QList<quint32> &drawOrder)
  {
    if (sectOrder.arraySize == 0)
    {
      return true;
    }

    if (sectOrder.arrayModulo != 5)
    {
      return false;
    }

    if ((sectOrder.arraySize % sectOrder.arrayModulo) != 0)
    {
      return true;
    }

    in.device()->seek(sectOrder.arrayOffset);

    int i, n;
    quint8 typ;
    quint32 subtyp;

    const int N = sectOrder.arraySize / sectOrder.arrayModulo;

    for (i = 0; i < N; i++)
    {
      in >> typ >> subtyp;
      //         qDebug() << Qt::hex << typ << subtyp;
      if (typ == 0)
      {
      }
      else if (subtyp == 0)
      {
#ifdef DBG
        // qDebug() << QString("Type 0x%1 is priority %2").arg(typ, 0, 16).arg(count);
#endif
        int idx = drawOrder.indexOf(typ);
        if (idx != NOIDX)
        {
          drawOrder.move(idx, 0);
        }
      }
      else
      {
        quint32 exttyp = 0x010000 | (typ << 8);
        quint32 mask = 0x1;

        for (n = 0; n < 0x20; ++n)
        {
          if (subtyp & mask)
          {
            drawOrder.push_front(exttyp | n);
#ifdef DBG
            // qDebug() << QString("Type 0x%1 is priority %2").arg(exttyp | n, 0, 16).arg(count);
#endif
          }
          mask = mask << 1;
        }
      }
    }

#ifdef DBG
    for (unsigned i = 0; i < drawOrder.size(); ++i)
    {
      if (i && i % 16 == 0)
      {
        printf(" \n");
      }
      printf("%06X ", drawOrder[i]);
    }

    printf(" \n");
#endif

    return true;
  }
  virtual bool parsePolygon(QDataStream &in, QMap<quint32, polygon_property> &polygons)
  {
    bool tainted = false;

    if (sectPolygons.arraySize == 0)
    {
      return true;
    }

    if (!sectPolygons.arrayModulo || ((sectPolygons.arraySize % sectPolygons.arrayModulo) != 0))
    {
      return true;
    }

    QTextCodec *codec = getCodec(codepage);

    const int N = sectPolygons.arraySize / sectPolygons.arrayModulo;
    for (int element = 0; element < N; element++)
    {
      quint16 t16_1 = 0, t16_2, subtyp;
      quint8 t8;
      quint32 typ, offset = 0;
      bool hasLocalization = false;
      bool hasTextColor = false;
      quint8 ctyp;
      QImage xpmDay(32, 32, QImage::Format_Indexed8);
      QImage xpmNight(32, 32, QImage::Format_Indexed8);
      quint8 r, g, b;
      quint8 langcode;

      in.device()->seek(sectPolygons.arrayOffset + (sectPolygons.arrayModulo * element));

      if (sectPolygons.arrayModulo == 5)
      {
        in >> t16_1 >> t16_2 >> t8;
        offset = t16_2 | (t8 << 16);
      }
      else if (sectPolygons.arrayModulo == 4)
      {
        in >> t16_1 >> t16_2;
        offset = t16_2;
      }
      else if (sectPolygons.arrayModulo == 3)
      {
        in >> t16_1 >> t8;
        offset = t8;
      }

      t16_2 = (t16_1 >> 5) | ((t16_1 & 0x1f) << 11);
      typ = t16_2 & 0x7F;
      subtyp = t16_1 & 0x1F;

      if (t16_1 & 0x2000)
      {
        typ = 0x10000 | (typ << 8) | subtyp;
      }

      in.device()->seek(sectPolygons.dataOffset + offset);
      in >> t8;
      hasLocalization = t8 & 0x10;
      hasTextColor = t8 & 0x20;
      ctyp = t8 & 0x0F;

#ifdef DBG
      qDebug() << "Polygon typ:" << Qt::hex << typ << "ctype:" << ctyp << "offset:" << (sectPolygons.dataOffset + offset) << "orig data:" << t16_1;
#endif

      polygon_property &property = polygons[typ];

      switch (ctyp)
      {
      case 0x01:
      {
        // day & night single color
        in >> b >> g >> r;
        property.brushDay = QBrush(qRgb(r, g, b));
        in >> b >> g >> r;
        property.brushNight = QBrush(qRgb(r, g, b));

        // night and day color for line?
        in >> b >> g >> r;
        property.pen = QPen(QBrush(qRgb(r, g, b)), 2);
        in >> b >> g >> r;
        property.known = true;

        break;
      }

      case 0x06:
      {
        // day & night single color
        in >> b >> g >> r;
        property.brushDay = QBrush(qRgb(r, g, b));
        property.brushNight = QBrush(qRgb(r, g, b));
        property.pen = Qt::NoPen;
        property.known = true;

        break;
      }

      case 0x07:
      {
        // day single color & night single color
        in >> b >> g >> r;
        property.brushDay = QBrush(qRgb(r, g, b));
        in >> b >> g >> r;
        property.brushNight = QBrush(qRgb(r, g, b));
        property.pen = Qt::NoPen;
        property.known = true;

        break;
      }

      case 0x08:
      {
        // day & night two color
        xpmDay.setColorCount(2);

        in >> b >> g >> r;
        xpmDay.setColor(1, qRgb(r, g, b));
        in >> b >> g >> r;
        xpmDay.setColor(0, qRgb(r, g, b));

        decodeBitmap(in, xpmDay, 32, 32, 1);
        property.brushDay.setTextureImage(xpmDay);
        property.brushNight.setTextureImage(xpmDay);
        property.pen = Qt::NoPen;
        property.known = true;
        break;
      }

      case 0x09:
      {
        // day two color & night two color
        xpmDay.setColorCount(2);
        xpmNight.setColorCount(2);
        in >> b >> g >> r;
        xpmDay.setColor(1, qRgb(r, g, b));
        in >> b >> g >> r;
        xpmDay.setColor(0, qRgb(r, g, b));
        in >> b >> g >> r;
        xpmNight.setColor(1, qRgb(r, g, b));
        in >> b >> g >> r;
        xpmNight.setColor(0, qRgb(r, g, b));

        decodeBitmap(in, xpmDay, 32, 32, 1);
        memcpy(xpmNight.bits(), xpmDay.bits(), (32 * 32));
        property.brushDay.setTextureImage(xpmDay);
        property.brushNight.setTextureImage(xpmNight);
        property.pen = Qt::NoPen;
        property.known = true;

        break;
      }

      case 0x0B:
      {
        // day one color, transparent & night two color
        xpmDay.setColorCount(2);
        xpmNight.setColorCount(2);
        in >> b >> g >> r;
        xpmDay.setColor(1, qRgb(r, g, b));
        xpmDay.setColor(0, qRgba(255, 255, 255, 0));

        in >> b >> g >> r;
        xpmNight.setColor(1, qRgb(r, g, b));
        in >> b >> g >> r;
        xpmNight.setColor(0, qRgb(r, g, b));

        decodeBitmap(in, xpmDay, 32, 32, 1);
        memcpy(xpmNight.bits(), xpmDay.bits(), (32 * 32));
        property.brushDay.setTextureImage(xpmDay);
        property.brushNight.setTextureImage(xpmNight);
        property.pen = Qt::NoPen;
        property.known = true;
        break;
      }

      case 0x0D:
      {
        // day two color & night one color, transparent

        xpmDay.setColorCount(2);
        xpmNight.setColorCount(2);
        in >> b >> g >> r;
        xpmDay.setColor(1, qRgb(r, g, b));
        in >> b >> g >> r;
        xpmDay.setColor(0, qRgb(r, g, b));

        in >> b >> g >> r;
        xpmNight.setColor(1, qRgb(r, g, b));
        xpmNight.setColor(0, qRgba(255, 255, 255, 0));

        decodeBitmap(in, xpmDay, 32, 32, 1);
        memcpy(xpmNight.bits(), xpmDay.bits(), (32 * 32));
        property.brushDay.setTextureImage(xpmDay);
        property.brushNight.setTextureImage(xpmNight);
        property.pen = Qt::NoPen;
        property.known = true;

        break;
      }

      case 0x0E:
      {
        // day & night one color, transparent
        xpmDay.setColorCount(2);
        in >> b >> g >> r;
        xpmDay.setColor(1, qRgb(r, g, b));
        xpmDay.setColor(0, qRgba(255, 255, 255, 0));

        decodeBitmap(in, xpmDay, 32, 32, 1);
        property.brushDay.setTextureImage(xpmDay);
        property.brushNight.setTextureImage(xpmDay);
        property.pen = Qt::NoPen;
        property.known = true;

        break;
      }

      case 0x0F:
      {
        // day one color, transparent & night one color, transparent
        xpmDay.setColorCount(2);
        xpmNight.setColorCount(2);
        in >> b >> g >> r;
        xpmDay.setColor(1, qRgb(r, g, b));
        xpmDay.setColor(0, qRgba(255, 255, 255, 0));

        in >> b >> g >> r;
        xpmNight.setColor(1, qRgb(r, g, b));
        xpmNight.setColor(0, qRgba(255, 255, 255, 0));

        decodeBitmap(in, xpmDay, 32, 32, 1);
        memcpy(xpmNight.bits(), xpmDay.bits(), (32 * 32));
        property.brushDay.setTextureImage(xpmDay);
        property.brushNight.setTextureImage(xpmNight);
        property.pen = Qt::NoPen;
        property.known = true;

        break;
      }

      default:
        if (!tainted)
        {
          // QMessageBox::warning(CMainWindow::getBestWidgetForParent(), tr("Warning..."),
          //                      tr("This is a typ file with unknown polygon encoding. Please report!"),
          //                      QMessageBox::Abort, QMessageBox::Abort);
          tainted = true;
        }
        qDebug() << "Failed polygon:" << typ << subtyp << Qt::hex << typ << subtyp << ctyp;
      }

      if (hasLocalization)
      {
        qint16 len;
        quint8 n = 1;

        in >> t8;
        len = t8;

        if (!(t8 & 0x01))
        {
          n = 2;
          in >> t8;
          len |= t8 << 8;
        }

        len -= n;
        while (len > 0)
        {
          QByteArray str;
          in >> langcode;
          languages << langcode;
          len -= 2 * n;
          while (len > 0)
          {
            in >> t8;
            len -= 2 * n;

            if (t8 == 0)
            {
              break;
            }

            str += t8;
          }
          if (codec != nullptr)
          {
            property.strings[langcode] = codec->toUnicode(str);
          }
#ifdef DBG
          qDebug() << len << langcode << property.strings[langcode];
#endif
        }
      }

      if (hasTextColor)
      {
        in >> t8;
        property.labelType = (label_type_e)(t8 & 0x07);

        if (t8 & 0x08)
        {
          in >> b >> g >> r;
          property.colorLabelDay = qRgb(r, g, b);
        }

        if (t8 & 0x10)
        {
          in >> b >> g >> r;
          property.colorLabelNight = qRgb(r, g, b);
        }
#ifdef DBG
        qDebug() << "ext. label: type" << property.labelType << "day" << property.colorLabelDay << "night"
                 << property.colorLabelNight;
#endif
      }
    }

    return true;
  }

  virtual bool parsePolyline(QDataStream &in, QMap<quint32, polyline_property> &polylines)
  {
    bool tainted = false;

    if (sectPolylines.arraySize == 0)
    {
      return true;
    }

    if (!sectPolylines.arrayModulo || ((sectPolylines.arraySize % sectPolylines.arrayModulo) != 0))
    {
      return true;
    }

    QTextCodec *codec = getCodec(codepage);

    const int N = sectPolylines.arraySize / sectPolylines.arrayModulo;
    for (int element = 0; element < N; element++)
    {
      quint16 t16_1 = 0, t16_2, subtyp;
      quint8 t8_1, t8_2;
      quint32 typ, offset = 0;
      bool hasLocalization = false;
      bool hasTextColor = false;
      // bool renderMode = false;
      quint8 ctyp, rows;
      quint8 r, g, b;
      quint8 langcode;

      in.device()->seek(sectPolylines.arrayOffset + (sectPolylines.arrayModulo * element));

      if (sectPolylines.arrayModulo == 5)
      {
        in >> t16_1 >> t16_2 >> t8_1;
        offset = t16_2 | (t8_1 << 16);
      }
      else if (sectPolylines.arrayModulo == 4)
      {
        in >> t16_1 >> t16_2;
        offset = t16_2;
      }
      else if (sectPolylines.arrayModulo == 3)
      {
        in >> t16_1 >> t8_1;
        offset = t8_1;
      }

      t16_2 = (t16_1 >> 5) | ((t16_1 & 0x1f) << 11);
      typ = t16_2 & 0x7F;
      subtyp = t16_1 & 0x1F;

      if (t16_1 & 0x2000)
      {
        typ = 0x10000 | (typ << 8) | subtyp;
      }

      in.device()->seek(sectPolylines.dataOffset + offset);
      in >> t8_1 >> t8_2;
      ctyp = t8_1 & 0x07;
      rows = t8_1 >> 3;

      hasLocalization = t8_2 & 0x01;
      // renderMode      = t8_2 & 0x02;
      hasTextColor = t8_2 & 0x04;

#ifdef DBG
      qDebug() << "Polyline typ:" << Qt::hex << typ << "ctyp:" << ctyp << "offset:" << (sectPolylines.dataOffset + offset)
               << "orig data:" << t16_1;
#endif

      polyline_property &property = polylines[typ];
#ifdef DBG
      qDebug() << "rows" << rows << "t8_2" << Qt::hex << t8_2;
#endif

      switch (ctyp)
      {
      case 0x00:
      {
        if (rows)
        {
          QImage xpm(32, rows, QImage::Format_Indexed8);
          in >> b >> g >> r;
          xpm.setColor(1, qRgb(r, g, b));
          in >> b >> g >> r;
          xpm.setColor(0, qRgb(r, g, b));
          decodeBitmap(in, xpm, 32, rows, 1);
          property.imgDay = xpm;
          property.imgNight = xpm;
          property.hasPixmap = true;
          property.known = true;
        }
        else
        {
          quint8 w1, w2;
          in >> b >> g >> r;
          property.penLineDay = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          property.penLineNight = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> b >> g >> r;
          property.penBorderDay = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          property.penBorderNight = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> w1 >> w2;
          property.penLineDay.setWidth(w1);
          property.penLineNight.setWidth(w1);
          property.penBorderDay.setWidth(w2);
          property.penBorderNight.setWidth(w2);
          property.hasBorder = w2 > w1;
          property.hasPixmap = false;
          property.known = true;
        }

        break;
      }

      case 0x01:
      {
        if (rows)
        {
          QImage xpm1(32, rows, QImage::Format_Indexed8);
          QImage xpm2(32, rows, QImage::Format_Indexed8);
          in >> b >> g >> r;
          xpm1.setColor(1, qRgb(r, g, b));
          in >> b >> g >> r;
          xpm1.setColor(0, qRgb(r, g, b));
          in >> b >> g >> r;
          xpm2.setColor(1, qRgb(r, g, b));
          in >> b >> g >> r;
          xpm2.setColor(0, qRgb(r, g, b));
          decodeBitmap(in, xpm1, 32, rows, 1);
          memcpy(xpm2.bits(), xpm1.bits(), (32 * rows));
          property.imgDay = xpm1;
          property.imgNight = xpm2;
          property.hasPixmap = true;
          property.known = true;
        }
        else
        {
          quint8 w1, w2;
          in >> b >> g >> r;
          property.penLineDay = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> b >> g >> r;
          property.penBorderDay = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> b >> g >> r;
          property.penLineNight = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> b >> g >> r;
          property.penBorderNight = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> w1 >> w2;
          property.penLineDay.setWidth(w1);
          property.penLineNight.setWidth(w1);
          property.penBorderDay.setWidth(w2);
          property.penBorderNight.setWidth(w2);
          property.hasBorder = w2 > w1;
          property.hasPixmap = false;
          property.known = true;
        }
        break;
      }

      case 0x03:
      {
        if (rows)
        {
          QImage xpm1(32, rows, QImage::Format_Indexed8);
          QImage xpm2(32, rows, QImage::Format_Indexed8);
          in >> b >> g >> r;
          xpm1.setColor(1, qRgb(r, g, b));
          xpm1.setColor(0, qRgba(255, 255, 255, 0));
          in >> b >> g >> r;
          xpm2.setColor(1, qRgb(r, g, b));
          in >> b >> g >> r;
          xpm2.setColor(0, qRgb(r, g, b));
          decodeBitmap(in, xpm1, 32, rows, 1);
          memcpy(xpm2.bits(), xpm1.bits(), (32 * rows));
          property.imgDay = xpm1;
          property.imgNight = xpm2;
          property.hasPixmap = true;
          property.known = true;
        }
        else
        {
          quint8 w1, w2;
          in >> b >> g >> r;
          property.penLineDay = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          property.penBorderDay = QPen(Qt::NoPen);
          in >> b >> g >> r;
          property.penLineNight = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> b >> g >> r;
          property.penBorderNight = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> w1 >> w2;
          property.penLineDay.setWidth(w1);
          property.penLineNight.setWidth(w1);
          property.penBorderDay.setWidth(w2);
          property.penBorderNight.setWidth(w2);
          property.hasBorder = w2 > w1;
          property.hasPixmap = false;
          property.known = true;
        }

        break;
      }

      case 0x05:
      {
        if (rows)
        {
          QImage xpm1(32, rows, QImage::Format_Indexed8);
          QImage xpm2(32, rows, QImage::Format_Indexed8);
          in >> b >> g >> r;
          xpm1.setColor(1, qRgb(r, g, b));
          in >> b >> g >> r;
          xpm1.setColor(0, qRgb(r, g, b));
          in >> b >> g >> r;
          xpm2.setColor(1, qRgb(r, g, b));
          xpm2.setColor(0, qRgba(255, 255, 255, 0));
          decodeBitmap(in, xpm1, 32, rows, 1);
          memcpy(xpm2.bits(), xpm1.bits(), (32 * rows));
          property.imgDay = xpm1;
          property.imgNight = xpm2;
          property.hasPixmap = true;
          property.known = true;
        }
        else
        {
          quint8 w1;
          in >> b >> g >> r;
          property.penLineDay = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> b >> g >> r;
          property.penBorderDay = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          in >> b >> g >> r;
          property.penLineNight = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          property.penBorderNight = QPen(Qt::NoPen);
          in >> w1;
          property.penLineDay.setWidth(w1);
          property.penLineNight.setWidth(w1);
          property.hasBorder = false;
          property.hasPixmap = false;
          property.known = true;
        }
        break;
      }

      case 0x06:
      {
        if (rows)
        {
          QImage xpm(32, rows, QImage::Format_Indexed8);
          in >> b >> g >> r;
          xpm.setColor(1, qRgb(r, g, b));
          xpm.setColor(0, qRgba(255, 255, 255, 0));
          decodeBitmap(in, xpm, 32, rows, 1);
          property.imgDay = xpm;
          property.imgNight = xpm;
          property.hasPixmap = true;
          property.known = true;
        }
        else
        {
          quint8 w1;
          in >> b >> g >> r;
          property.penLineDay = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          property.penBorderDay = QPen(Qt::NoPen);
          property.penLineNight = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          property.penBorderNight = QPen(Qt::NoPen);
          in >> w1;
          property.penLineDay.setWidth(w1);
          property.penLineNight.setWidth(w1);
          property.hasBorder = false;
          property.hasPixmap = false;
          property.known = true;
        }
        break;
      }

      case 0x07:
      {
        if (rows)
        {
          QImage xpm1(32, rows, QImage::Format_Indexed8);
          QImage xpm2(32, rows, QImage::Format_Indexed8);
          in >> b >> g >> r;
          xpm1.setColor(1, qRgb(r, g, b));
          xpm1.setColor(0, qRgba(255, 255, 255, 0));
          in >> b >> g >> r;
          xpm2.setColor(1, qRgb(r, g, b));
          xpm2.setColor(0, qRgba(255, 255, 255, 0));
          decodeBitmap(in, xpm1, 32, rows, 1);
          memcpy(xpm2.bits(), xpm1.bits(), (32 * rows));
          property.imgDay = xpm1;
          property.imgNight = xpm2;
          property.hasPixmap = true;
          property.known = true;
        }
        else
        {
          quint8 w1;
          in >> b >> g >> r;
          property.penLineDay = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          property.penBorderDay = QPen(Qt::NoPen);
          in >> b >> g >> r;
          property.penLineNight = QPen(QBrush(qRgb(r, g, b)), 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
          property.penBorderNight = QPen(Qt::NoPen);
          in >> w1;
          property.penLineDay.setWidth(w1);
          property.penLineNight.setWidth(w1);
          property.hasBorder = false;
          property.hasPixmap = false;
          property.known = true;
        }
        break;
      }

      default:
        if (!tainted)
        {
          // QMessageBox::warning(CMainWindow::getBestWidgetForParent(), tr("Warning..."),
          //                      tr("This is a typ file with unknown polyline encoding. Please report!"),
          //                      QMessageBox::Abort, QMessageBox::Abort);
          tainted = true;
        }

        qDebug() << "Failed polyline" << Qt::hex << ":" << typ << ctyp << rows;
        continue;
      }

      property.imgDay = property.imgDay.convertToFormat(QImage::Format_ARGB32_Premultiplied);
      property.imgNight = property.imgNight.convertToFormat(QImage::Format_ARGB32_Premultiplied);
      if (hasLocalization)
      {
        qint16 len;
        quint8 n = 1;

        in >> t8_1;
        len = t8_1;

        if (!(t8_1 & 0x01))
        {
          n = 2;
          in >> t8_1;
          len |= t8_1 << 8;
        }

        len -= n;
        while (len > 0)
        {
          QByteArray str;
          in >> langcode;
          languages << langcode;
          len -= 2 * n;
          while (len > 0)
          {
            in >> t8_1;
            len -= 2 * n;

            if (t8_1 == 0)
            {
              break;
            }

            str += t8_1;
          }
          if (codec != nullptr)
          {
            property.strings[langcode] = codec->toUnicode(str);
          }
#ifdef DBG
          qDebug() << len << langcode << property.strings[langcode];
#endif
        }
      }

      if (hasTextColor)
      {
        in >> t8_1;
        property.labelType = (label_type_e)(t8_1 & 0x07);

        if (t8_1 & 0x08)
        {
          in >> b >> g >> r;
          property.colorLabelDay = qRgb(r, g, b);
        }

        if (t8_1 & 0x10)
        {
          in >> b >> g >> r;
          property.colorLabelNight = qRgb(r, g, b);
        }
#ifdef DBG
        qDebug() << "ext. label: type" << property.labelType << "day" << property.colorLabelDay << "night"
                 << property.colorLabelNight;
#endif
      }

      if (property.hasPixmap)
      {
        property.imgDay = property.imgDay.mirrored(false, true);
        property.imgNight = property.imgNight.mirrored(false, true);
      }
    }
    return true;
  }
  virtual bool parsePoint(QDataStream &in, QMap<quint32, point_property> &points)
  {
    //    bool tainted = false;

    if (!sectPoints.arrayModulo || ((sectPoints.arraySize % sectPoints.arrayModulo) != 0))
    {
      return true;
    }

    QTextCodec *codec = getCodec(codepage);

    const int N = sectPoints.arraySize / sectPoints.arrayModulo;
    for (int element = 0; element < N; element++)
    {
      quint16 t16_1 = 0, t16_2, subtyp;
      quint8 t8_1;
      quint32 typ, offset = 0;
      bool hasLocalization = false;
      bool hasTextColor = false;
      quint8 langcode;
      quint8 r, g, b;

      in.device()->seek(sectPoints.arrayOffset + (sectPoints.arrayModulo * element));

      if (sectPoints.arrayModulo == 5)
      {
        in >> t16_1 >> t16_2 >> t8_1;
        offset = t16_2 | (t8_1 << 16);
      }
      else if (sectPoints.arrayModulo == 4)
      {
        in >> t16_1 >> t16_2;
        offset = t16_2;
      }
      else if (sectPoints.arrayModulo == 3)
      {
        in >> t16_1 >> t8_1;
        offset = t8_1;
      }

      t16_2 = (t16_1 >> 5) | ((t16_1 & 0x1f) << 11);
      typ = t16_2 & 0x7FF;
      subtyp = t16_1 & 0x01F;

      if (t16_1 & 0x2000)
      {
        typ = 0x10000 | (typ << 8) | subtyp;
      }
      else
      {
        typ = (typ << 8) + subtyp;
      }

      in.device()->seek(sectPoints.dataOffset + offset);

      int bpp = 0, wbytes = 0;
      quint8 w, h, ncolors, ctyp;
      in >> t8_1 >> w >> h >> ncolors >> ctyp;

      hasLocalization = t8_1 & 0x04;
      hasTextColor = t8_1 & 0x08;
      t8_1 = t8_1 & 0x03;
#ifdef DBG
      qDebug() << "Point typ:" << Qt::hex << typ << "ctyp:" << ctyp << "offset:" << (sectPoints.dataOffset + offset)
               << "orig data:" << t16_1;
#endif

      if (!decodeBppAndBytes(ncolors, w, ctyp, bpp, wbytes))
      {
        continue;
      }

#ifdef DBG
      qDebug() << "          " << "w" << w << "h" << h << "ncolors" << ncolors << "bpp" << bpp << "wbytes"
               << wbytes;
#endif

      if (ctyp == 0x20 || ctyp == 0x00)
      {
        if ((ncolors == 0) && (bpp >= 16))
        {
          ncolors = w * h;
        }
      }

      point_property &property = points[typ];
      QImage imgDay(w, h, QImage::Format_Indexed8);
      QImage imgNight(w, h, QImage::Format_Indexed8);

      if (!decodeColorTable(in, imgDay, ncolors, 1 << bpp, ctyp == 0x20))
      {
        continue;
      }

      if (bpp >= 16)
      {
        continue;
      }
      else
      {
        decodeBitmap(in, imgDay, w, h, bpp);
        property.imgDay = imgDay;
      }

      if (t8_1 == 0x03)
      {
        in >> ncolors >> ctyp;
        if (!decodeBppAndBytes(ncolors, w, ctyp, bpp, wbytes))
        {
          continue;
        }
        if (!decodeColorTable(in, imgNight, ncolors, 1 << bpp, ctyp == 0x20))
        {
          continue;
        }
        decodeBitmap(in, imgNight, w, h, bpp);
        points[typ].imgNight = imgNight;
      }
      else if (t8_1 == 0x02)
      {
        in >> ncolors >> ctyp;
        if (!decodeBppAndBytes(ncolors, w, ctyp, bpp, wbytes))
        {
          continue;
        }
        if (!decodeColorTable(in, imgDay, ncolors, 1 << bpp, ctyp == 0x20))
        {
          continue;
        }
        property.imgNight = imgDay;
      }
      else
      {
        property.imgNight = imgDay;
      }

      if (hasLocalization)
      {
        qint16 len;
        quint8 n = 1;

        in >> t8_1;
        len = t8_1;

        if (!(t8_1 & 0x01))
        {
          n = 2;
          in >> t8_1;
          len |= t8_1 << 8;
        }

        len -= n;
        while (len > 0)
        {
          QByteArray str;
          in >> langcode;
          languages << langcode;
          len -= 2 * n;
          while (len > 0)
          {
            in >> t8_1;
            len -= 2 * n;

            if (t8_1 == 0)
            {
              break;
            }

            str += t8_1;
          }
          if (codec != nullptr)
          {
            property.strings[langcode] = codec->toUnicode(str);
          }
#ifdef DBG
          qDebug() << len << langcode << property.strings[langcode];
#endif
        }
      }

      if (hasTextColor)
      {
        in >> t8_1;
        property.labelType = (label_type_e)(t8_1 & 0x07);

        if (t8_1 & 0x08)
        {
          in >> b >> g >> r;
          property.colorLabelDay = qRgb(r, g, b);
        }

        if (t8_1 & 0x10)
        {
          in >> b >> g >> r;
          property.colorLabelNight = qRgb(r, g, b);
        }
#ifdef DBG
        qDebug() << "ext. label: type" << property.labelType << "day" << property.colorLabelDay << "night"
                 << property.colorLabelNight;
#endif
      }
    }

    return true;
  }

  QTextCodec *getCodec(quint16 codepage)
  {
    QTextCodec *codec = QTextCodec::codecForName(QString("CP%1").arg(codepage).toLatin1());
    if (codepage == 65001)
    {
      codec = QTextCodec::codecForName("UTF-8");
    }

    return codec;
  }

  void decodeBitmap(QDataStream &in, QImage &img, int w, int h, int bpp)
  {
    int x = 0;
    quint8 color;

    if (bpp == 0)
    {
      return;
    }

    for (int y = 0; y < h; y++)
    {
      while (x < w)
      {
        in >> color;

        for (int i = 0; (i < (8 / bpp)) && (x < w); i++)
        {
          int value;
          if (i > 0)
          {
            value = (color >>= bpp);
          }
          else
          {
            value = color;
          }
          if (bpp == 4)
          {
            value = value & 0xf;
          }
          if (bpp == 2)
          {
            value = value & 0x3;
          }
          if (bpp == 1)
          {
            value = value & 0x1;
          }
          img.setPixel(x, y, value);
          //                 qDebug() << QString("value(%4) pixel at (%1,%2) is 0x%3 j is
          //                 %5").arg(x).arg(y).arg(value,0,16).arg(color).arg(j);
          x += 1;
        }
      }
      x = 0;
    }
  }
  bool decodeBppAndBytes(int ncolors, int w, int flags, int &bpp, int &bytes)
  {
    switch (flags)
    {
    case 0x00:
    {
      if (ncolors < 3)
      {
        bpp = ncolors;
      }
      else if (ncolors == 3)
      {
        bpp = 2;
      }
      else if (ncolors < 16)
      {
        bpp = 4;
      }
      else if (ncolors < 256)
      {
        bpp = 8;
      }
      else
      {
        return false;
      }
      break;
    }

    case 0x10:
    {
      if (ncolors == 0)
      {
        bpp = 1;
      }
      else if (ncolors < 3)
      {
        bpp = 2;
      }
      else if (ncolors < 15)
      {
        bpp = 4;
      }
      else if (ncolors < 256)
      {
        bpp = 8;
      }
      else
      {
        return false;
      }
      break;
    }

    case 0x20:
    {
      if (ncolors == 0)
      {
        bpp = 16;
      }
      else if (ncolors < 3)
      {
        bpp = ncolors;
      }
      else if (ncolors < 4)
      {
        bpp = 2;
      }
      else if (ncolors < 16)
      {
        bpp = 4;
      }
      else if (ncolors < 256)
      {
        bpp = 8;
      }
      else
      {
        return false;
      }
      break;
    }

    default:
      return false;
    }

    bytes = (w * bpp) / 8;
    if ((w * bpp) & 0x07)
    {
      ++bytes;
    }

    return true;
  }
  bool decodeColorTable(QDataStream &in, QImage &img, int ncolors, int maxcolor, bool hasAlpha)
  {
    img.setColorCount(ncolors);

    if (hasAlpha)
    {
      int i;
      quint8 byte;
      quint32 bits = 0;
      quint32 reg = 0;
      quint32 mask = 0x000000FF;

      for (i = 0; i < ncolors; i++)
      {
        while (bits < 28)
        {
          in >> byte;
          mask = 0x000000FF << bits;
          reg = reg & (~mask);
          reg = reg | (byte << bits);
          bits += 8;
        }

        img.setColor(i, qRgba((reg >> 16) & 0x0FF, (reg >> 8) & 0x0FF, reg & 0x0FF, ~((reg >> 24) & 0x0F) << 4));

        reg = reg >> 28;
        bits -= 28;
      }
      for (; i < maxcolor; ++i)
      {
        img.setColor(i, qRgba(0, 0, 0, 0));
      }
    }
    else
    {
      int i;
      quint8 r, g, b;
      for (i = 0; i < ncolors; ++i)
      {
        in >> b >> g >> r;
        img.setColor(i, qRgb(r, g, b));
      }
      for (; i < maxcolor; ++i)
      {
        img.setColor(i, qRgba(0, 0, 0, 0));
      }
    }
    return true;
  }

  struct typ_section_t
  {
    typ_section_t() : dataOffset(0), dataLength(0), arrayOffset(0), arrayModulo(0), arraySize(0) {}
    quint32 dataOffset;
    quint32 dataLength;
    quint32 arrayOffset;
    quint16 arrayModulo;
    quint32 arraySize;
  };

  quint16 version = 0;
  quint16 codepage = 0;
  quint16 year = 0;
  quint8 month = 0;
  quint8 day = 0;
  quint8 hour = 0;
  quint8 minutes = 0;
  quint8 seconds = 0;

  quint16 fid = 0;
  quint16 pid = 0;

  typ_section_t sectPoints;
  typ_section_t sectPolylines;
  typ_section_t sectPolygons;
  typ_section_t sectOrder;

  QSet<quint8> languages;
};

typedef QVector<CGarminPolygon> polytype_t;
typedef QVector<CGarminPoint> pointtype_t;

inline int CFileExt::cnt = 0;

class CMap : public QCoreApplication
{
public:
  explicit CMap(int &argc, char **argv) : QCoreApplication(argc, argv)
  {
    // eFeatVisibility | eFeatVectorItems | eFeatTypFile filename = "";
    qDebug() << "IMG: try to open:" << filename;

    try
    {
      readBasics();
      processPrimaryMapData();

      // quint8 bits = scale2bits(bufferScale);
      quint8 bits = 23;

      QVector<map_level_t>::const_iterator maplevel = maplevels.constEnd();
      do
      {
        --maplevel;
        if (bits >= maplevel->bits)
        {
          break;
        }
      } while (maplevel != maplevels.constBegin());

      loadVisibleData(false, polygons, polylines, points, pois, maplevel->level);
    }
    catch (const exce_t &e)
    {
      qDebug() << "Fatal error:" << e.msg;
    }
  }

  virtual ~CMap()
  {
    qDebug() << "✅ Done.";
  }

  struct maplevel_t
  {
    bool inherited;
    quint8 level;
    quint8 bits;
  };
  enum features_e
  {
    eFeatVisibility = 0x00000001,
    eFeatVectorItems = 0x00000002,
    eFeatTileCache = 0x00000004,
    eFeatLayers = 0x00000008,
    eFeatTypFile = 0x00000010
  };

  /// subfile part (TRE, RGN, ...) location information
  struct subfile_part_t
  {
    quint32 offset = 0; //< file offset of subfile part
    quint32 size = 0;   //< size of the subfile part
  };

  /// subdivision  information
  struct subdiv_desc_t
  {
    quint32 n;

    quint16 next;      //< section of next level
    bool terminate;    //< end of section group
    quint32 rgn_start; //< offset into the subfile's RGN part
    quint32 rgn_end;   //< end of section in RGN part (last offset = rgn_end - 1)

    bool hasPoints;    //< there are points stored in the RGN subsection
    bool hasIdxPoints; //< there are index points stored in the RGN subsection
    bool hasPolylines; //< there are polylines stored in the RGN subsection
    bool hasPolygons;  //< there are polygons stored in the RGN subsection

    qint32 iCenterLng; //< the center longitude of the area covered by this subdivision
    qint32 iCenterLat; //< the center latitude  of the area covered by this subdivision

    qreal north; //< north boundary of area covered by this subsection []
    qreal east;  //< east  boundary of area covered by this subsection []
    qreal south; //< south boundary of area covered by this subsection []
    qreal west;  //< west  boundary of area covered by this subsection []

    /// area in meter coordinates covered by this subdivision []
    QRectF area;

    /// number of left shifts for RGN data
    quint32 shift;
    /// map level this subdivision is shown
    quint32 level;

    quint32 offsetPoints2;
    qint32 lengthPoints2;
    quint32 offsetPolylines2;
    qint32 lengthPolylines2;
    quint32 offsetPolygons2;
    qint32 lengthPolygons2;
  };

  struct subfile_desc_t
  {
    /// the name of the subfile (not really needed)
    QString name;
    /// location information of all parts
    QMap<QString, subfile_part_t> parts;

    qreal north = 0.0; //< north boundary of area covered by this subfile [rad]
    qreal east = 0.0;  //< east  boundary of area covered by this subfile [rad]
    qreal south = 0.0; //< south boundary of area covered by this subfile [rad]
    qreal west = 0.0;  //< west  boundary of area covered by this subfile [rad]

    /// area in [] covered by this subfile
    QRectF area;

    /// list of subdivisions
    QVector<subdiv_desc_t> subdivs;
    /// used maplevels
    QVector<maplevel_t> maplevels;
    /// bit 1 of POI_flags (TRE header @ 0x3F)
    bool isTransparent = false;
    /// object to manage the string tables
    IGarminStrTbl *strtbl = nullptr;
  };

#pragma pack(1)
  // Garmin IMG file header structure, to the start of the FAT blocks
  struct hdr_img_t
  {
    quint8 xorByte; ///< 0x00000000
    quint8 byte0x00000001_0x0000000F[15];
    char signature[7]; ///< 0x00000010 .. 0x00000016
    quint8 byte0x00000017_0x00000040[42];
    ///< 0x00000041 .. 0x00000047
    char identifier[7];
    quint8 byte0x00000048;
    char desc1[20]; ///< 0x00000049 .. 0x0000005C
    quint8 byte0x0000005D_0x00000060[4];
    quint8 e1; ///< 0x00000061
    quint8 e2; ///< 0x00000062
    quint8 byte0x00000063_0x00000064[2];
    char desc2[31]; ///< 0x00000065 .. 0x00000083
    quint8 byte0x00000084_0x0000040B[904];
    quint32 dataoffset; ///< 0x0000040C .. 0x0000040F
    quint8 byte0x00000410_0x0000041F[16];
    quint16 blocks[240]; ///< 0x00000420 .. 0x000005FF

    quint32 blocksize() { return 1 << (e1 + e2); }
  };
  struct FATblock_t
  {
    quint8 flag;  ///< 0x00000000
    char name[8]; ///< 0x00000001 .. 0x00000008
    char type[3]; ///< 0x00000009 .. 0x0000000B
    quint32 size; ///< 0x0000000C .. 0x0000000F
    quint16 part; ///< 0x00000010 .. 0x00000011
    quint8 byte0x00000012_0x0000001F[14];
    quint16 blocks[240]; ///< 0x00000020 .. 0x000001FF
  };

  // common header of the RGN, TRE, LBL, NET, ... parts of the IMG file
  struct hdr_subfile_part_t
  {
    quint16 length; ///< 0x00000000 .. 0x00000001
    char type[10];  ///< 0x00000002 .. 0x0000000B
    quint8 byte0x0000000C;
    quint8 flag; ///< 0x0000000D
    quint8 byte0x0000000E_0x00000014[7];
  };

  // TRE part header, to 0xB7
  struct hdr_tre_t : public hdr_subfile_part_t
  {
    quint24 northbound;  ///< 0x00000015 .. 0x00000017
    quint24 eastbound;   ///< 0x00000018 .. 0x0000001A
    quint24 southbound;  ///< 0x0000001B .. 0x0000001D
    quint24 westbound;   ///< 0x0000001E .. 0x00000020
    quint32 tre1_offset; ///< 0x00000021 .. 0x00000024
    quint32 tre1_size;   ///< 0x00000025 .. 0x00000028
    quint32 tre2_offset; ///< 0x00000029 .. 0x0000002C
    quint32 tre2_size;   ///< 0x0000002D .. 0x00000030
    quint32 tre3_offset; ///< 0x00000031 .. 0x00000034
    quint32 tre3_size;   ///< 0x00000035 .. 0x00000038
    ///< 0x00000039 .. 0x0000003A
    quint16 tre3_rec_size;
    quint8 byte0x0000003B_0x0000003E[4];
    quint8 POI_flags; ///< 0x0000003F
    quint8 byte0x00000040_0x00000049[10];
    quint32 tre4_offset; ///< 0x0000004A .. 0x0000004D
    quint32 tre4_size;   ///< 0x0000004E .. 0x00000051
    ///< 0x00000052 .. 0x00000053
    quint16 tre4_rec_size;
    quint8 byte0x00000054_0x00000057[4];
    quint32 tre5_offset; ///< 0x00000058 .. 0x0000005B
    quint32 tre5_size;   ///< 0x0000005C .. 0x0000005F
    ///< 0x00000060 .. 0x00000061
    quint16 tre5_rec_size;
    quint8 byte0x00000062_0x00000065[4];
    quint32 tre6_offset; ///< 0x00000066 .. 0x00000069
    quint32 tre6_size;   ///< 0x0000006A .. 0x0000006D
    ///< 0x0000006E .. 0x0000006F
    quint16 tre6_rec_size;
    quint8 byte0x00000070_0x00000073[4];
    /*-----------------------------------------------------*/
    quint8 byte0x00000074_0x0000007B[8];
    // Object groups V2 (CTreGroup2).
    quint32 tre7_offset; ///< 0x0000007C .. 0x0000007F //Groups2Offset
    quint32 tre7_size;   ///< 0x00000080 .. 0x00000083  //dwGroups2Length
    ///< 0x00000084 .. 0x00000085 //wGroup2RecSize
    quint16 tre7_rec_size;
    quint8 byte0x00000086_0x00000089[4];
    // Order: polyline, polygon, POI; each sorted by type (1 type 1 levels 1 subtype)
    quint32 tre8_offset; ///< 0x0000008A .. 0x0000008D
    quint32 tre8_size;   ///< 0x0000008E .. 0x00000091
    ///< 0x00000092 .. 0x00000093
    quint16 tre8_rec_size;

    ///< 0x00000094 .. 0x00000095
    quint16 polyl2_types_num;
    ///< 0x00000096 .. 0x00000097
    quint16 polyg2_types_num;
    ///< 0x00000098 .. 0x00000099
    quint16 poi2_types_num;

    /*-----------------------------------------------------*/
    quint8 key[20];      ///< 0x0000009A .. 0x000000AD
    quint32 tre9_offset; ///< 0x000000AE .. 0x000000B1
    quint32 tre9_size;   ///< 0x000000B2 .. 0x000000B5
    ///< 0x000000B6 .. 0x000000B7
    quint16 tre9_rec_size;
  };

  // RGN part header
  struct hdr_rgn_t : public hdr_subfile_part_t
  {
    quint32 offset; ///< 0x00000015 .. 0x00000018
    quint32 length; ///< 0x00000019 .. 0x0000001C
    ///< 0x0000001D .. 0x00000020
    quint32 offset_polyg2;
    ///< 0x00000021 .. 0x00000024
    quint32 length_polyg2;
    quint8 byte0x00000025_0x00000038[20];
    ///< 0x00000039 .. 0x0000003C
    quint32 offset_polyl2;
    ///< 0x0000003D .. 0x00000040
    quint32 length_polyl2;
    quint8 byte0x00000041_0x00000054[20];
    ///< 0x00000055 .. 0x00000058
    quint32 offset_point2;
    ///< 0x00000059 .. 0x0000005C
    quint32 length_point2;
  };

  // LBL part header
  struct hdr_lbl_t : public hdr_subfile_part_t
  {
    quint32 lbl1_offset; ///< 0x00000015 .. 0x00000018
    quint32 lbl1_length; ///< 0x00000019 .. 0x0000001C
    quint8 addr_shift;   ///< 0x0000001D
    quint8 coding;       ///< 0x0000001E
    quint32 lbl2_offset; ///< 0x0000001F .. 0x00000022
    quint32 lbl2_length; ///< 0x00000023 .. 0x00000026
    ///< 0x00000027 .. 0x00000028
    quint16 lbl2_rec_size;
    quint8 byte0x00000029_0x0000002C[4];
    quint32 lbl3_offset; ///< 0x0000002D .. 0x00000030
    quint32 lbl3_length; ///< 0x00000031 .. 0x00000034
    ///< 0x00000035 .. 0x00000036
    quint16 lbl3_rec_size;
    quint8 byte0x00000037_0x0000003A[4];
    quint32 lbl4_offset; ///< 0x0000003B .. 0x0000003E
    quint32 lbl4_length; ///< 0x0000003F .. 0x00000042
    ///< 0x00000043 .. 0x00000044
    quint16 lbl4_rec_size;
    quint8 byte0x00000045_0x00000048[4];
    quint32 lbl5_offset; ///< 0x00000049 .. 0x0000004C
    quint32 lbl5_length; ///< 0x0000004D .. 0x00000050
    ///< 0x00000051 .. 0x00000052
    quint16 lbl5_rec_size;
    quint8 byte0x00000053_0x00000056[4];
    quint32 lbl6_offset; ///< 0x00000057 .. 0x0000005A
    quint32 lbl6_length; ///< 0x0000005B .. 0x0000005E
    ///< 0x0000005F
    quint8 lbl6_addr_shift;
    ///< 0x00000060
    quint8 lbl6_glob_mask;
    quint8 byte0x00000061_0x00000063[3];
    quint32 lbl7_offset; ///< 0x00000064 .. 0x00000067
    quint32 lbl7_length; ///< 0x00000068 .. 0x0000006B
    ///< 0x0000006C .. 0x0000006D
    quint16 lbl7_rec_size;
    quint8 byte0x0000006E_0x00000071[4];
    quint32 lbl8_offset; ///< 0x00000072 .. 0x00000075
    quint32 lbl8_length; ///< 0x00000076 .. 0x00000079
    ///< 0x0000007A .. 0x0000007B
    quint16 lbl8_rec_size;
    quint8 byte0x0000007C_0x0000007F[4];
    quint32 lbl9_offset; ///< 0x00000080 .. 0x00000083
    quint32 lbl9_length; ///< 0x00000084 .. 0x00000087
    ///< 0x00000088 .. 0x00000089
    quint16 lbl9_rec_size;
    quint8 byte0x0000008A_0x0000008D[4];
    quint32 lbl10_offset; ///< 0x0000008E .. 0x00000091
    quint32 lbl10_length; ///< 0x00000092 .. 0x00000095
    ///< 0x00000096 .. 0x00000097
    quint16 lbl10_rec_size;
    quint8 byte0x00000098_0x0000009B[4];
    quint32 lbl11_offset; ///< 0x0000009C .. 0x0000009F
    quint32 lbl11_length; ///< 0x000000A0 .. 0x000000A3
    ///< 0x000000A4 .. 0x000000A5
    quint16 lbl11_rec_size;
    quint8 byte0x000000A6_0x000000A9[4];
    quint16 codepage; ///< 0x000000AA .. 0x000000AB  optional check length
  };

  // NET part header
  struct hdr_net_t : public hdr_subfile_part_t
  {
    quint32 net1_offset; ///< 0x00000015 .. 0x00000018
    quint32 net1_length; ///< 0x00000019 .. 0x0000001C
    ///< 0x0000001D
    quint8 net1_addr_shift;
    quint32 net2_offset; ///< 0x0000001E .. 0x00000021
    quint32 net2_length; ///< 0x00000022 .. 0x00000025
    ///< 0x00000026
    quint8 net2_addr_shift;
    quint32 net3_offset; ///< 0x00000027 .. 0x0000002A
    quint32 net3_length; ///< 0x0000002B .. 0x0000002E
  };

#define TRE_MAP_LEVEL(r) ((r)->zoom & 0x0f)
#define TRE_MAP_INHER(r) (((r)->zoom & 0x80) != 0)

  // map level definition
  struct tre_map_level_t
  {
    quint8 zoom;
    quint8 bits;
    quint16 nsubdiv;
  };

  // map subdivision definition, without pointer to the lower level subparts
  struct tre_subdiv_t
  {
    quint24 rgn_offset;
    quint8 elements;
    quint24 center_lng;
    quint24 center_lat;
    quint16 width_trm;
#define TRE_SUBDIV_WIDTH(r) (gar_load(uint16_t, (r)->width_trm) & 0x7FFF)
#define TRE_SUBDIV_TERM(r) ((gar_load(uint16_t, (r)->width_trm) & 0x8000) != 0)
    quint16 height;
  };

  // pointer to the lower level subparts
  struct tre_subdiv_next_t : public tre_subdiv_t
  {
    quint16 next;
  };

  struct tre_subdiv2_t
  {
    quint32 offsetPolygons;
    quint32 offsetPolyline;
    quint32 offsetPoints;
    quint8 btObjects;
  };

#pragma pack()
  struct map_level_t
  {
    quint8 bits;
    quint8 level;
    bool useBaseMap;

    bool operator==(const map_level_t &ml) const
    {
      if (ml.bits != bits || ml.level != level || ml.useBaseMap != useBaseMap)
      {
        return false;
      }
      else
      {
        return true;
      }
    }

    static bool GreaterThan(const map_level_t &ml1, const map_level_t &ml2) { return ml1.bits < ml2.bits; }
  };

private:
  void addLabel(const CGarminPoint &pt, const QRect &rect, const CGarminTyp::point_property &property, bool isNight)
  {
    QString str;
    if (pt.hasLabel())
    {
      str = pt.getLabelText();
    }

    labels.push_back(strlbl_t());
    strlbl_t &strlbl = labels.last();
    strlbl.pt = pt.pos.toPoint();
    strlbl.str = str;
    strlbl.rect = rect;
    strlbl.property = property;
    strlbl.isNight = isNight;
  }

  void collectText(const CGarminPolygon &item, const QPolygonF &line, const QFont &font, qint32 lineWidth, const QColor &color)
  {
    QString str;
    if (item.hasLabel())
    {
      str = item.getLabelText();
    }

    if (str.isEmpty())
    {
      return;
    }

    textpath_t tp;
    tp.polyline = line;
    // tp.font = font;
    tp.text = str;
    tp.lineWidth = lineWidth;
    tp.color = color;

    const int size = line.size();
    for (int i = 1; i < size; ++i)
    {
      const QPointF &p1 = line[i - 1];
      const QPointF &p2 = line[i];
      qreal dx = p2.x() - p1.x();
      qreal dy = p2.y() - p1.y();
      tp.lengths << qSqrt(dx * dx + dy * dy);
    }

    textpaths << tp;
  }

  void getInfoPoints(const pointtype_t &points, const QPoint &pt, QMultiMap<QString, QString> &dict) const
  {
    for (const CGarminPoint &point : points)
    {
      QPoint x = pt - QPoint(point.pos.x(), point.pos.y());
      if (x.manhattanLength() < 10)
      {
        if (point.hasLabel())
        {
          dict.insert(tr("Point of Interest"), point.getLabelText());
        }
        else
        {
          if (pointProperties.contains(point.type))
          {
            dict.insert(tr("Point of Interest"),
                        pointProperties[point.type].strings[selectedLanguage != NOIDX ? selectedLanguage : 0]);
          }
          else
          {
            dict.insert(tr("Point of Interest"), QString(" (%1)").arg(point.type, 2, 16, QChar('0')));
          }
        }
      }
    }
  }

  void getInfoPolylines(const QPoint &pt, QMultiMap<QString, QString> &dict) const
  {
    PJ_UV p1, p2;        // the two points of the polyline close to pt
    qreal u;             // ratio u the tangent point will divide d_p1_p2
    qreal shortest = 20; // shortest distance so far

    QPointF resPt = pt;
    QString key;
    QStringList value;
    quint32 type = 0;

    bool found = false;

    for (const CGarminPolygon &line : polylines)
    {
      int len = line.pixel.size();
      // need at least 2 points
      if (len < 2)
      {
        continue;
      }

      // see http://local.wasp.uwa.edu.au/~pbourke/geometry/pointline/
      for (int i = 1; i < len; ++i)
      {
        p1.u = line.pixel[i - 1].x();
        p1.v = line.pixel[i - 1].y();
        p2.u = line.pixel[i].x();
        p2.v = line.pixel[i].y();

        qreal dx = p2.u - p1.u;
        qreal dy = p2.v - p1.v;

        // distance between p1 and p2
        qreal d_p1_p2 = qSqrt(dx * dx + dy * dy);

        u = ((pt.x() - p1.u) * dx + (pt.y() - p1.v) * dy) / (d_p1_p2 * d_p1_p2);

        if (u < 0.0 || u > 1.0)
        {
          continue;
        }

        // coord. (x,y) of the point on line defined by [p1,p2] close to pt
        qreal x = p1.u + u * dx;
        qreal y = p1.v + u * dy;

        qreal distance = qSqrt((x - pt.x()) * (x - pt.x()) + (y - pt.y()) * (y - pt.y()));

        if (distance < shortest)
        {
          type = line.type;
          value.clear();
          value << (line.hasLabel() ? line.getLabelText() : "-");

          resPt.setX(x);
          resPt.setY(y);
          shortest = distance;
          found = true;
        }
        else if (distance == shortest)
        {
          if (line.hasLabel())
          {
            value << line.getLabelText();
          }
        }
      }
    }

    value.removeDuplicates();

    if (!found)
    {
      return;
    }

    if (selectedLanguage != NOIDX)
    {
      key = polylineProperties[type].strings[selectedLanguage];
    }

    if (!key.isEmpty())
    {
      dict.insert(key + QString("(%1)").arg(type, 2, 16, QChar('0')), value.join("\n"));
    }
    else
    {
      if (polylineProperties[type].strings.isEmpty())
      {
        dict.insert(tr("Unknown") + QString("(%1)").arg(type, 2, 16, QChar('0')), value.join("\n"));
      }
      else
      {
        dict.insert(polylineProperties[type].strings[0] + QString("(%1)").arg(type, 2, 16, QChar('0')), value.join("\n"));
      }
    }

    //    pt = resPt.toPoint();
  }

  void getInfoPolygons(const QPoint &pt, QMultiMap<QString, QString> &dict) const
  {
    PJ_UV p1, p2; // the two points of the polyline close to pt
    const qreal x = pt.x();
    const qreal y = pt.y();

    for (const CGarminPolygon &line : polygons)
    {
      int npol = line.pixel.size();
      if (npol > 2)
      {
        bool c = false;
        // see http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/
        for (int i = 0, j = npol - 1; i < npol; j = i++)
        {
          p1.u = line.pixel[j].x();
          p1.v = line.pixel[j].y();
          p2.u = line.pixel[i].x();
          p2.v = line.pixel[i].y();

          if ((((p2.v <= y) && (y < p1.v)) || ((p1.v <= y) && (y < p2.v))) &&
              (x < (p1.u - p2.u) * (y - p2.v) / (p1.v - p2.v) + p2.u))
          {
            c = !c;
          }
        }

        if (c)
        {
          if (line.labels.size())
          {
            dict.insert(tr("Area"), line.labels.join(" ").simplified());
          }
          else
          {
            if (selectedLanguage != NOIDX)
            {
              if (polygonProperties[line.type].strings[selectedLanguage].size())
              {
                dict.insert(tr("Area"), polygonProperties[line.type].strings[selectedLanguage]);
              }
            }
            else
            {
              if (polygonProperties[line.type].strings[0].size())
              {
                dict.insert(tr("Area"), polygonProperties[line.type].strings[0]);
              }
            }
          }
        }
      }
    }
  }

  void drawPolylines(QPainter &p, polytype_t &lines)
  {
    textpaths.clear();

    QVector<qreal> lengths;
    lengths.reserve(100);

    QHash<quint32, QList<quint32>> dict;
    for (int i = 0; i < lines.count(); ++i)
    {
      dict[lines[i].type].push_back(i);
    }

    QMap<quint32, CGarminTyp::polyline_property>::iterator props = polylineProperties.begin();
    QMap<quint32, CGarminTyp::polyline_property>::iterator end = polylineProperties.end();
    for (; props != end; ++props)
    {
      const quint32 &type = props.key();
      const CGarminTyp::polyline_property &property = props.value();

      if (dict[type].isEmpty())
      {
        continue;
      }

      if (property.hasPixmap)
      {
        qDebug() << "";

        QList<quint32>::const_iterator it = dict[type].constBegin();
        for (; it != dict[type].constEnd(); ++it)
        {
          CGarminPolygon &item = lines[*it];
          {
            // pixmapCount++;

            QPolygonF &poly = item.pixel;
            int size = poly.size();

            if (size < 2)
            {
              continue;
            }

            // map->convertRad2Px(poly);

            lengths.resize(0);

            lengths.reserve(size);

            // QPainterPath path;
            qreal totalLength = 0;

            qreal u1 = poly[0].x();
            qreal v1 = poly[0].y();

            for (int i = 1; i < size; ++i)
            {
              qreal u2 = poly[i].x();
              qreal v2 = poly[i].y();

              qreal segLength = qSqrt((u2 - u1) * (u2 - u1) + (v2 - v1) * (v2 - v1));
              totalLength += segLength;
              lengths << segLength;

              u1 = u2;
              v1 = v2;
            }

            if (property.labelType != CGarminTyp::eNone)
            {
              // collectText(item, poly, f, h,
              //             CMainWindow::self().isNight() ? property.colorLabelNight : property.colorLabelDay);
            }

            // path.addPolygon(poly);
            const int nLength = lengths.count();

            qreal curLength = 0;
            // QPointF p2 = path.pointAtPercent(curLength / totalLength);
            for (int i = 0; i < nLength; ++i)
            {
              qreal segLength = lengths.at(i);

              //                         qDebug() << curLength << totalLength << curLength / totalLength;

              // QPointF p1 = p2;
              // p2 = path.pointAtPercent((curLength + segLength) / totalLength);
              // qreal angle = qAtan((p2.y() - p1.y()) / (p2.x() - p1.x())) * 180 / M_PI;

              // if (p2.x() - p1.x() < 0)
              // {
              //   angle += 180;
              // }

              curLength += segLength;
            }
          }
        }
      }
      else
      {
        if (property.hasBorder)
        {
          // draw background line 1st
          // p.setPen(CMainWindow::self().isNight() ? property.penBorderNight : property.penBorderDay);

          QList<quint32>::const_iterator it = dict[type].constBegin();
          for (; it != dict[type].constEnd(); ++it)
          {
            // drawLine(p, lines[*it], property);
          }
          // draw foreground line in a second run for nicer borders
        }
        else
        {
          // p.setPen(CMainWindow::self().isNight() ? property.penLineNight : property.penLineDay);

          QList<quint32>::const_iterator it = dict[type].constBegin();
          for (; it != dict[type].constEnd(); ++it)
          {
            // drawLine(p, lines[*it], property);
          }
        }
      }
    }

    // 2nd run to draw foreground lines.
    props = polylineProperties.begin();
    for (; props != end; ++props)
    {
      const quint32 &type = props.key();
      const CGarminTyp::polyline_property &property = props.value();

      if (dict[type].isEmpty())
      {
        continue;
      }

      if (property.hasBorder && !property.hasPixmap)
      {
        QList<quint32>::const_iterator it = dict[type].constBegin();
        for (; it != dict[type].constEnd(); ++it)
        {
          // qDebug() << p << lines[*it];
        }
      }
    }
  }
  void loadVisibleData(bool fast, polytype_t &polygons, polytype_t &polylines, pointtype_t &points, pointtype_t &pois, unsigned level)
  {
    // qDebug() << "level:" << level;
    // if (level != 0)
    // {
    //   qDebug() << "skip level:" << level;
    // }
    for (const subfile_desc_t &subfile : std::as_const(subfiles))
    {
      qDebug() << "-------";
      qDebug() << (subfile.area.topLeft() * RAD_TO_DEG) << (subfile.area.bottomRight() * RAD_TO_DEG);

      CFileExt file(filename);
      if (!file.open(QIODevice::ReadOnly))
      {
        return;
      }

      QByteArray rgndata;
      readFile(file, subfile.parts["RGN"].offset, subfile.parts["RGN"].size, rgndata);

      qDebug() << "rgn range" << Qt::hex << subfile.parts["RGN"].offset << (subfile.parts["RGN"].offset + subfile.parts["RGN"].size);

      const QVector<subdiv_desc_t> &subdivs = subfile.subdivs;
      for (const subdiv_desc_t &subdiv : subdivs)
      {
        loadSubDiv(file, subdiv, subfile.strtbl, rgndata, fast, polylines, polygons, points, pois);
      }

      file.close();
    }
  }

  void loadSubDiv(CFileExt &file, const subdiv_desc_t &subdiv, IGarminStrTbl *strtbl, const QByteArray &rgndata, bool fast, polytype_t &polylines, polytype_t &polygons, pointtype_t &points, pointtype_t &pois)
  {
    if (subdiv.rgn_start == subdiv.rgn_end && !subdiv.lengthPolygons2 && !subdiv.lengthPolylines2 &&
        !subdiv.lengthPoints2)
    {
      return;
    }
    fprintf(stderr, "loadSubDiv\n");
    qDebug() << "---------" << file.fileName() << "---------";

    const quint8 *pRawData = (quint8 *)rgndata.data();

    quint32 opnt = 0, oidx = 0, opline = 0, opgon = 0;
    quint32 objCnt = subdiv.hasIdxPoints + subdiv.hasPoints + subdiv.hasPolylines + subdiv.hasPolygons;

    quint16 *pOffset = (quint16 *)(pRawData + subdiv.rgn_start);

    // test for points
    if (subdiv.hasPoints)
    {
      opnt = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
    }

    // test for indexed points
    if (subdiv.hasIdxPoints)
    {
      if (opnt)
      {
        oidx = gar_load(uint16_t, *pOffset);
        oidx += subdiv.rgn_start;
        ++pOffset;
      }
      else
      {
        oidx = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
      }
    }

    // test for polylines
    if (subdiv.hasPolylines)
    {
      if (opnt || oidx)
      {
        opline = gar_load(uint16_t, *pOffset);
        opline += subdiv.rgn_start;
        ++pOffset;
      }
      else
      {
        opline = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
      }
    }

#ifdef DEBUG_SHOW_POLY_DATA_SUBDIV
    qDebug() << "--- Subdivision" << subdiv.n << "---";
    qDebug() << "address:" << Qt::hex << subdiv.rgn_start << "- " << subdiv.rgn_end;
    qDebug() << "points:            " << Qt::hex << opnt;
    qDebug() << "indexed points:    " << Qt::hex << oidx;
    qDebug() << "polylines:         " << Qt::hex << opline;
    qDebug() << "polygons:          " << Qt::hex << opgon;
#endif

    CGarminPolygon p;

    // decode polylines
    if (subdiv.hasPolylines)
    {
      CGarminPolygon::cnt = 0;
      const quint8 *pData = pRawData + opline;
      const quint8 *pEnd = pRawData + (opgon ? opgon : subdiv.rgn_end);
      while (pData < pEnd)
      {
        pData += p.decode1(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, true, pData, pEnd);
        // pData += p.decode2(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, true, pData, pEnd); // da testvam da vidim kakvo shte vyrne

        if (strtbl && !p.lbl_in_NET && p.lbl_info)
        {
          strtbl->get(file, p.lbl_info, IGarminStrTbl::norm, p.labels);
        }
        else if (strtbl && p.lbl_in_NET && p.lbl_info)
        {
          strtbl->get(file, p.lbl_info, IGarminStrTbl::net, p.labels);
        }

        polylines.push_back(p);
      }
    }

    qDebug() << "--- Subdivision" << subdiv.n << "---";
    qDebug() << "adress:" << Qt::hex << subdiv.rgn_start << "- " << subdiv.rgn_end;
    // qDebug() << "polyg off: " << Qt::hex << subdiv.offsetPolygons2;
    // qDebug() << "polyg len: " << Qt::hex << subdiv.lengthPolygons2 << subdiv.lengthPolygons2;
    // qDebug() << "polyg end: " << Qt::hex << subdiv.lengthPolygons2 + subdiv.offsetPolygons2;
    qDebug() << "polyl off: " << Qt::hex << subdiv.offsetPolylines2;
    qDebug() << "polyl len: " << Qt::hex << subdiv.lengthPolylines2 << subdiv.lengthPolylines2;
    qDebug() << "polyl end: " << Qt::hex << subdiv.lengthPolylines2 + subdiv.offsetPolylines2;
    // qDebug() << "point off: " << Qt::hex << subdiv.offsetPoints2;
    // qDebug() << "point len: " << Qt::hex << subdiv.lengthPoints2 << subdiv.lengthPoints2;
    // qDebug() << "point end: " << Qt::hex << subdiv.lengthPoints2 + subdiv.offsetPoints2;

    if (subdiv.lengthPolylines2)
    {
      const quint8 *pData = pRawData + subdiv.offsetPolylines2;
      const quint8 *pEnd = pData + subdiv.lengthPolylines2;
      while (pData < pEnd)
      {
        // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
        pData += p.decode2(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, true, pData, pEnd);

        if (strtbl && !p.lbl_in_NET && p.lbl_info)
        {
          strtbl->get(file, p.lbl_info, IGarminStrTbl::norm, p.labels);
        }

        qDebug() << pData;
        polylines.push_back(p);
      }
    }
  }

  QString copyright; //< a copyright string to be displayed as tool tip
  enum exce_e
  {
    eErrOpen,
    eErrAccess,
    errFormat,
    errLock,
    errAbort
  };
  struct exce_t
  {
    exce_t(exce_e err, const QString &msg) : err(err), msg(msg) {}
    exce_e err;
    QString msg;
  };
  struct strlbl_t
  {
    QPoint pt;
    QRect rect;
    QString str;
    CGarminTyp::point_property property;
    bool isNight = false;
  };

  void readBasics()
  {
    char tmpstr[64];
    qint64 fsize = QFileInfo(filename).size();

    CFileExt file(filename);
    if (!file.open(QIODevice::ReadOnly))
    {
      throw exce_t(eErrOpen, tr("Failed to open: ") + filename);
    }

    mask = (quint8)*file.data(0, 1);

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

    // read hdr_img_t
    QByteArray imghdr;
    readFile(file, 0, sizeof(hdr_img_t), imghdr);
    hdr_img_t *pImgHdr = (hdr_img_t *)imghdr.data();

    if (strncmp(pImgHdr->signature, "DSKIMG", 7) != 0)
    {
      throw exce_t(errFormat, tr("Bad file format: ") + filename);
    }
    if (strncmp(pImgHdr->identifier, "GARMIN", 7) != 0)
    {
      throw exce_t(errFormat, tr("Bad file format: ") + filename);
    }

    mapdesc = QByteArray((const char *)pImgHdr->desc1, 20);
    mapdesc += pImgHdr->desc2;
    qDebug() << mapdesc;

    size_t blocksize = pImgHdr->blocksize();

    // 1st read FAT
    QByteArray FATblock;
    readFile(file, sizeof(hdr_img_t), sizeof(FATblock_t), FATblock);
    const FATblock_t *pFATBlock = (const FATblock_t *)FATblock.data();

    size_t dataoffset = sizeof(hdr_img_t);

    // skip dummy blocks at the beginning
    while (dataoffset < (size_t)fsize)
    {
      if (pFATBlock->flag != 0x00)
      {
        break;
      }
      dataoffset += sizeof(FATblock_t);
      readFile(file, quint32(dataoffset), quint32(sizeof(FATblock_t)), FATblock);
      pFATBlock = (const FATblock_t *)FATblock.data();
    }

    QSet<QString> subfileNames;
    while (dataoffset < (size_t)fsize)
    {
      if (pFATBlock->flag != 0x01)
      {
        break;
      }

      memcpy(tmpstr, pFATBlock->name, sizeof(pFATBlock->name) + sizeof(pFATBlock->type));
      tmpstr[sizeof(pFATBlock->name) + sizeof(pFATBlock->type)] = 0;

      if (gar_load(quint32, pFATBlock->size) != 0 && !subfileNames.contains(tmpstr) && tmpstr[0] != 0x20)
      {
        subfileNames << tmpstr;

        memcpy(tmpstr, pFATBlock->name, sizeof(pFATBlock->name));
        tmpstr[sizeof(pFATBlock->name)] = 0;

        // skip MAPSORC.MPS section
        if (strcmp(tmpstr, "MAPSOURC") && strcmp(tmpstr, "SENDMAP2"))
        {
          subfile_desc_t &subfile = subfiles[tmpstr];
          subfile.name = tmpstr;

          memcpy(tmpstr, pFATBlock->type, sizeof(pFATBlock->type));
          tmpstr[sizeof(pFATBlock->type)] = 0;

          subfile_part_t &part = subfile.parts[tmpstr];
          part.size = gar_load(quint32, pFATBlock->size);
          part.offset = quint32(gar_load(uint16_t, pFATBlock->blocks[0]) * blocksize);
        }
      }

      dataoffset += sizeof(FATblock_t);
      readFile(file, quint32(dataoffset), quint32(sizeof(FATblock_t)), FATblock);
      pFATBlock = (const FATblock_t *)FATblock.data();
    }

    if ((dataoffset == sizeof(hdr_img_t)) || (dataoffset >= (size_t)fsize))
    {
      throw exce_t(errFormat, tr("Failed to read file structure: ") + filename);
    }

    // gmapsupp.img files do not have a data offset field
    if (gar_load(quint32, pImgHdr->dataoffset) == 0)
    {
      pImgHdr->dataoffset = gar_load(quint32, dataoffset);
    }

    // sometimes there are dummy blocks at the end of the FAT
    if (gar_load(quint32, pImgHdr->dataoffset) != dataoffset)
    {
      dataoffset = gar_load(quint32, pImgHdr->dataoffset);
    }

#ifdef DEBUG_SHOW_SECT_DESC
    {
      QMap<QString, subfile_desc_t>::const_iterator subfile = subfiles.begin();
      while (subfile != subfiles.end())
      {
        qDebug() << "--- subfile" << subfile->name << "---";
        QMap<QString, subfile_part_t>::const_iterator part = subfile->parts.begin();
        while (part != subfile->parts.end())
        {
          qDebug() << part.key() << Qt::hex << part->offset << part->size;
          ++part;
        }
        ++subfile;
      }
    }
#endif

    int cnt = 1;
    int tot = subfiles.count();

    maparea = QRectF();
    QMap<QString, subfile_desc_t>::iterator subfile = subfiles.begin();
    while (subfile != subfiles.end())
    {
      if ((*subfile).parts.contains("GMP"))
      {
        throw exce_t(errFormat, tr("File is NT format. qgimg is unable to read map files with NT format: ") + filename);
      }

      readSubfileBasics(*subfile, file);

      ++subfile;
    }

    // combine copyright sections
    copyright.clear();
    for (const QString &str : std::as_const(copyrights))
    {
      if (!copyright.isEmpty())
      {
        copyright += "\n";
      }
      copyright += str;
    }

    qDebug() << "dimensions:\t"
             << "N" << (maparea.bottom() * RAD_TO_DEG) << "E" << (maparea.right() * RAD_TO_DEG) << "S"
             << (maparea.top() * RAD_TO_DEG) << "W" << (maparea.left() * RAD_TO_DEG);
  }

  void readSubfileBasics(subfile_desc_t &subfile, CFileExt &file)
  {
    // test for mandatory subfile parts
    if (!(subfile.parts.contains("TRE") && subfile.parts.contains("RGN")))
    {
      return;
    }

    QByteArray trehdr;
    readFile(file, subfile.parts["TRE"].offset, sizeof(hdr_tre_t), trehdr);
    hdr_tre_t *pTreHdr = (hdr_tre_t *)trehdr.data();

    subfile.isTransparent = pTreHdr->POI_flags & 0x02;
    transparent = subfile.isTransparent ? true : transparent;

#ifdef DEBUG_SHOW_TRE_DATA
    qDebug() << "+++" << subfile.name << "+++";
    qDebug() << "TRE header length  :" << gar_load(uint16_t, pTreHdr->length);
    qDebug() << "TRE1 offset        :" << Qt::hex << gar_load(quint32, pTreHdr->tre1_offset);
    qDebug() << "TRE1 size          :" << Qt::hex << gar_load(quint32, pTreHdr->tre1_size);
    qDebug() << "TRE2 offset        :" << Qt::hex << gar_load(quint32, pTreHdr->tre2_offset);
    qDebug() << "TRE2 size          :" << Qt::hex << gar_load(quint32, pTreHdr->tre2_size);
#endif

    copyrights << QString(file.data(subfile.parts["TRE"].offset + gar_load(uint16_t, pTreHdr->length), 0x7FFF));

    // read map boundaries from header
    qint32 i32;
    i32 = gar_ptr_load(int24_t, &pTreHdr->northbound);
    subfile.north = GARMIN_RAD(i32);
    i32 = gar_ptr_load(int24_t, &pTreHdr->eastbound);
    subfile.east = GARMIN_RAD(i32);
    i32 = gar_ptr_load(int24_t, &pTreHdr->southbound);
    subfile.south = GARMIN_RAD(i32);
    i32 = gar_ptr_load(int24_t, &pTreHdr->westbound);
    subfile.west = GARMIN_RAD(i32);

    if (subfile.east == subfile.west)
    {
      subfile.east = -subfile.east;
    }

    if (subfile.west > 0 && subfile.east < 0)
    {
      subfile.east = -subfile.east;
    }

    subfile.area = QRectF(QPointF(subfile.west, subfile.north), QPointF(subfile.east, subfile.south));

    if (maparea.isNull())
    {
      maparea = subfile.area;
    }
    else
    {
      maparea = maparea.united(subfile.area);
    }

    qDebug() << "bounding area (\260)" << (subfile.north * RAD_TO_DEG) << (subfile.east * RAD_TO_DEG)
             << (subfile.south * RAD_TO_DEG) << (subfile.west * RAD_TO_DEG);
    qDebug() << "bounding area (rad)" << subfile.area;

    QByteArray maplevel;
    readFile(file, subfile.parts["TRE"].offset + gar_load(quint32, pTreHdr->tre1_offset),
             gar_load(quint32, pTreHdr->tre1_size), maplevel);
    const tre_map_level_t *pMapLevel = (const tre_map_level_t *)maplevel.data();

    if (pTreHdr->flag & 0x80)
    {
      throw exce_t(errLock, tr("File contains locked / encrypted data. Garmin does no want you to use this file with any other software than the one supplied by Garmin."));
    }

    quint32 nlevels = gar_load(quint32, pTreHdr->tre1_size) / sizeof(tre_map_level_t);
    quint32 nsubdivs = 0;
    quint32 nsubdivs_last = 0;

    // count subsections
    for (quint32 i = 0; i < nlevels; ++i)
    {
      maplevel_t ml;
      ml.inherited = TRE_MAP_INHER(pMapLevel);
      ml.level = TRE_MAP_LEVEL(pMapLevel);
      ml.bits = pMapLevel->bits;
      subfile.maplevels << ml;
      nsubdivs += gar_load(uint16_t, pMapLevel->nsubdiv);
      nsubdivs_last = gar_load(uint16_t, pMapLevel->nsubdiv);
#ifdef DEBUG_SHOW_MAPLEVEL_DATA
      qDebug() << "level:" << TRE_MAP_LEVEL(pMapLevel) << "| inherited:" << TRE_MAP_INHER(pMapLevel) << "| bits:"
               << pMapLevel->bits << "| #subdivs:" << gar_load(uint16_t, pMapLevel->nsubdiv);
#endif // DEBUG_SHOW_MAPLEVEL_DATA
      ++pMapLevel;
    }

    quint32 nsubdivs_next = nsubdivs - nsubdivs_last;

    //////////////////////////////////
    // read subdivision information
    //////////////////////////////////
    // point to first map level definition
    pMapLevel = (const tre_map_level_t *)maplevel.data();
    // number of subdivisions per map level
    quint32 nsubdiv = gar_load(uint16_t, pMapLevel->nsubdiv);

    // point to first 16 byte subdivision definition entry
    QByteArray subdiv_n;
    readFile(file, subfile.parts["TRE"].offset + gar_load(quint32, pTreHdr->tre2_offset), gar_load(quint32, pTreHdr->tre2_size), subdiv_n);
    tre_subdiv_next_t *pSubDivN = (tre_subdiv_next_t *)subdiv_n.data();

    QVector<subdiv_desc_t> subdivs;
    subdivs.resize(nsubdivs);
    QVector<subdiv_desc_t>::iterator subdiv = subdivs.begin();
    QVector<subdiv_desc_t>::iterator subdiv_prev = subdivs.end();

    // absolute offset of RGN data
    QByteArray rgnhdr;
    readFile(file, subfile.parts["RGN"].offset, sizeof(hdr_rgn_t), rgnhdr);
    hdr_rgn_t *pRgnHdr = (hdr_rgn_t *)rgnhdr.data();
    quint32 rgnoff = /*subfile.parts["RGN"].offset +*/ gar_load(quint32, pRgnHdr->offset);

    quint32 rgnOffPolyg2 = /*subfile.parts["RGN"].offset +*/ gar_load(quint32, pRgnHdr->offset_polyg2);
    quint32 rgnOffPolyl2 = /*subfile.parts["RGN"].offset +*/ gar_load(quint32, pRgnHdr->offset_polyl2);
    quint32 rgnOffPoint2 = /*subfile.parts["RGN"].offset +*/ gar_load(quint32, pRgnHdr->offset_point2);

    quint32 rgnLenPolyg2 = /*subfile.parts["RGN"].offset +*/ gar_load(quint32, pRgnHdr->length_polyg2);
    quint32 rgnLenPolyl2 = /*subfile.parts["RGN"].offset +*/ gar_load(quint32, pRgnHdr->length_polyl2);
    quint32 rgnLenPoint2 = /*subfile.parts["RGN"].offset +*/ gar_load(quint32, pRgnHdr->length_point2);

    qDebug() << "***" << Qt::hex << subfile.parts["RGN"].offset << (subfile.parts["RGN"].offset + subfile.parts["RGN"].size);
    qDebug() << "+++" << Qt::hex << rgnOffPolyg2 << (rgnOffPolyg2 + rgnLenPolyg2);
    qDebug() << "+++" << Qt::hex << rgnOffPolyl2 << (rgnOffPolyl2 + rgnLenPolyl2);
    qDebug() << "+++" << Qt::hex << rgnOffPoint2 << (rgnOffPoint2 + rgnLenPoint2);

    // parse all 16 byte subdivision entries
    quint32 i;
    for (i = 0; i < nsubdivs_next; ++i, --nsubdiv)
    {
      subdiv->n = i;
      subdiv->next = gar_load(uint16_t, pSubDivN->next);
      subdiv->terminate = TRE_SUBDIV_TERM(pSubDivN);
      subdiv->rgn_start = gar_ptr_load(uint24_t, &pSubDivN->rgn_offset);
      subdiv->rgn_start += rgnoff;
      // skip if this is the first entry
      if (subdiv_prev != subdivs.end())
      {
        subdiv_prev->rgn_end = subdiv->rgn_start;
      }

      subdiv->hasPoints = pSubDivN->elements & 0x10;
      subdiv->hasIdxPoints = pSubDivN->elements & 0x20;
      subdiv->hasPolylines = pSubDivN->elements & 0x40;
      subdiv->hasPolygons = pSubDivN->elements & 0x80;

      // if all subdivisions of this level have been parsed, switch to the next one
      if (nsubdiv == 0)
      {
        ++pMapLevel;
        nsubdiv = gar_load(uint16_t, pMapLevel->nsubdiv);
      }

      subdiv->level = TRE_MAP_LEVEL(pMapLevel);
      subdiv->shift = 24 - pMapLevel->bits;

      qint32 cx = gar_ptr_load(uint24_t, &pSubDivN->center_lng);
      subdiv->iCenterLng = cx;
      qint32 cy = gar_ptr_load(uint24_t, &pSubDivN->center_lat);
      subdiv->iCenterLat = cy;
      qint32 width = TRE_SUBDIV_WIDTH(pSubDivN) << subdiv->shift;
      qint32 height = gar_load(uint16_t, pSubDivN->height) << subdiv->shift;

      subdiv->north = GARMIN_RAD(cy + height + 1);
      subdiv->south = GARMIN_RAD(cy - height);
      subdiv->east = GARMIN_RAD(cx + width + 1);
      subdiv->west = GARMIN_RAD(cx - width);

      subdiv->area = QRectF(QPointF(subdiv->west, subdiv->north), QPointF(subdiv->east, subdiv->south));

      subdiv->offsetPoints2 = 0;
      subdiv->lengthPoints2 = 0;
      subdiv->offsetPolylines2 = 0;
      subdiv->lengthPolylines2 = 0;
      subdiv->offsetPolygons2 = 0;
      subdiv->lengthPolygons2 = 0;

      subdiv_prev = subdiv;
      ++pSubDivN;
      ++subdiv;
    }

    // switch to last map level
    ++pMapLevel;
    // witch pointer to 14 byte subdivision sections
    tre_subdiv_t *pSubDivL = pSubDivN;
    // parse all 14 byte subdivision entries of last map level
    for (; i < nsubdivs; ++i)
    {
      subdiv->n = i;
      subdiv->next = 0;
      subdiv->terminate = TRE_SUBDIV_TERM(pSubDivL);
      subdiv->rgn_start = gar_ptr_load(uint24_t, &pSubDivL->rgn_offset);
      subdiv->rgn_start += rgnoff;
      subdiv_prev->rgn_end = subdiv->rgn_start;
      subdiv->hasPoints = pSubDivL->elements & 0x10;
      subdiv->hasIdxPoints = pSubDivL->elements & 0x20;
      subdiv->hasPolylines = pSubDivL->elements & 0x40;
      subdiv->hasPolygons = pSubDivL->elements & 0x80;

      subdiv->level = TRE_MAP_LEVEL(pMapLevel);
      subdiv->shift = 24 - pMapLevel->bits;

      qint32 cx = gar_ptr_load(uint24_t, &pSubDivL->center_lng);
      subdiv->iCenterLng = cx;
      qint32 cy = gar_ptr_load(uint24_t, &pSubDivL->center_lat);
      subdiv->iCenterLat = cy;
      qint32 width = TRE_SUBDIV_WIDTH(pSubDivL) << subdiv->shift;
      qint32 height = gar_load(uint16_t, pSubDivL->height) << subdiv->shift;

      subdiv->north = GARMIN_RAD(cy + height + 1);
      subdiv->south = GARMIN_RAD(cy - height);
      subdiv->east = GARMIN_RAD(cx + width + 1);
      subdiv->west = GARMIN_RAD(cx - width);

      subdiv->area = QRectF(QPointF(subdiv->west, subdiv->north), QPointF(subdiv->east, subdiv->south));

      subdiv->offsetPoints2 = 0;
      subdiv->lengthPoints2 = 0;
      subdiv->offsetPolylines2 = 0;
      subdiv->lengthPolylines2 = 0;
      subdiv->offsetPolygons2 = 0;
      subdiv->lengthPolygons2 = 0;

      subdiv_prev = subdiv;
      ++pSubDivL;
      ++subdiv;
    }
    subdivs.last().rgn_end =
        gar_load(quint32, pRgnHdr->hdr_rgn_t::offset) + gar_load(quint32, pRgnHdr->hdr_rgn_t::length);

    // read extended NT elements
    if ((gar_load(uint16_t, pTreHdr->hdr_subfile_part_t::length) >= 0x9A) && pTreHdr->tre7_size &&
        (gar_load(uint16_t, pTreHdr->tre7_rec_size) >= sizeof(tre_subdiv2_t)))
    {
      if (subdiv->level > 0)
      {
        qDebug() << "Skiping level:" << subdiv->level;
        // continue;
      }
      rgnoff = subfile.parts["RGN"].offset;
      qDebug() << subdivs.count() << (pTreHdr->tre7_size / pTreHdr->tre7_rec_size) << pTreHdr->tre7_rec_size;
      QByteArray subdiv2;
      readFile(file, subfile.parts["TRE"].offset + gar_load(quint32, pTreHdr->tre7_offset), gar_load(quint32, pTreHdr->tre7_size), subdiv2);
      tre_subdiv2_t *pSubDiv2 = (tre_subdiv2_t *)subdiv2.data();

      const quint32 entries1 = gar_load(quint32, pTreHdr->tre7_size) / gar_load(quint32, pTreHdr->tre7_rec_size);
      const quint32 entries2 = subdivs.size();

      bool skipPois = (gar_load(uint16_t, pTreHdr->tre7_rec_size) != sizeof(tre_subdiv2_t));

      for (int i = 0; i < pTreHdr->tre7_rec_size; ++i)
      {
        if (i % 4 == 0)
          fprintf(stderr, "\n");
        fprintf(stderr, "%02X ", ((quint8 *)pSubDiv2)[i]);
      }
      fprintf(stderr, "\n");

      subdiv = subdivs.begin();
      subdiv_prev = subdivs.begin();
      subdiv->offsetPolygons2 = gar_load(quint32, pSubDiv2->offsetPolygons) + rgnOffPolyg2;
      subdiv->offsetPolylines2 = gar_load(quint32, pSubDiv2->offsetPolyline) + rgnOffPolyl2;
      subdiv->offsetPoints2 = skipPois ? 0 : gar_load(quint32, pSubDiv2->offsetPoints) + rgnOffPoint2;

      ++subdiv;
      pSubDiv2 = reinterpret_cast<tre_subdiv2_t *>((quint8 *)pSubDiv2 + gar_endian(uint16_t, pTreHdr->tre7_rec_size));

      while (subdiv != subdivs.end())
      {
        for (int i = 0; i < pTreHdr->tre7_rec_size; ++i)
        {
          if (i % 4 == 0)
            fprintf(stderr, "\n");
          fprintf(stderr, "%02X ", ((quint8 *)pSubDiv2)[i]);
        }
        fprintf(stderr, "\n");

        subdiv->offsetPolygons2 = gar_load(quint32, pSubDiv2->offsetPolygons) + rgnOffPolyg2;
        subdiv->offsetPolylines2 = gar_load(quint32, pSubDiv2->offsetPolyline) + rgnOffPolyl2;
        subdiv->offsetPoints2 = skipPois ? 0 : gar_load(quint32, pSubDiv2->offsetPoints) + rgnOffPoint2;

        subdiv_prev->lengthPolygons2 = subdiv->offsetPolygons2 - subdiv_prev->offsetPolygons2;
        subdiv_prev->lengthPolylines2 = subdiv->offsetPolylines2 - subdiv_prev->offsetPolylines2;
        subdiv_prev->lengthPoints2 = skipPois ? 0 : subdiv->offsetPoints2 - subdiv_prev->offsetPoints2;

        subdiv_prev = subdiv;

        ++subdiv;
        pSubDiv2 = reinterpret_cast<tre_subdiv2_t *>((quint8 *)pSubDiv2 + gar_endian(uint16_t, pTreHdr->tre7_rec_size));
      }

      subdiv_prev->lengthPolygons2 = rgnOffPolyg2 + rgnLenPolyg2 - subdiv_prev->offsetPolygons2;
      subdiv_prev->lengthPolylines2 = rgnOffPolyl2 + rgnLenPolyl2 - subdiv_prev->offsetPolylines2;
      subdiv_prev->lengthPoints2 = skipPois ? 0 : rgnOffPoint2 + rgnLenPoint2 - subdiv_prev->offsetPoints2;
    }

    subfile.subdivs = subdivs;

#ifdef DEBUG_SHOW_SUBDIV_DATA
    {
      QVector<subdiv_desc_t>::iterator subdiv = subfile.subdivs.begin();
      while (subdiv != subfile.subdivs.end())
      {
        qDebug() << "--- subdiv" << subdiv->n << "---";
        qDebug() << "RGN start          " << Qt::hex << subdiv->rgn_start;
        qDebug() << "RGN end            " << Qt::hex << subdiv->rgn_end;
        qDebug() << "center lng         " << GARMIN_DEG(subdiv->iCenterLng);
        qDebug() << "center lat         " << GARMIN_DEG(subdiv->iCenterLat);
        qDebug() << "has points         " << subdiv->hasPoints;
        qDebug() << "has indexed points " << subdiv->hasIdxPoints;
        qDebug() << "has polylines      " << subdiv->hasPolylines;
        qDebug() << "has polygons       " << subdiv->hasPolygons;
        qDebug() << "bounding area (m)  " << subdiv->area.topLeft() << subdiv->area.bottomRight();
        qDebug() << "map level          " << subdiv->level;
        qDebug() << "left shifts        " << subdiv->shift;
        qDebug() << "polyg off.         " << Qt::hex << subdiv->offsetPolygons2;
        qDebug() << "polyg len.         " << Qt::hex << subdiv->lengthPolygons2;
        qDebug() << "polyl off.         " << Qt::hex << subdiv->offsetPolylines2;
        qDebug() << "polyl len.         " << Qt::hex << subdiv->lengthPolylines2;
        qDebug() << "point off.         " << Qt::hex << subdiv->offsetPoints2;
        qDebug() << "point len.         " << Qt::hex << subdiv->lengthPoints2;
        ++subdiv;
      }
    }
#endif

    qDebug() << "***" << Qt::hex << subfile.parts["RGN"].offset << (subfile.parts["RGN"].offset + subfile.parts["RGN"].size);
    qDebug() << "+++" << Qt::hex << rgnOffPolyg2 << (rgnOffPolyg2 + pRgnHdr->length_polyg2);
    qDebug() << "+++" << Qt::hex << rgnOffPolyl2 << (rgnOffPolyl2 + pRgnHdr->length_polyl2);
    qDebug() << "+++" << Qt::hex << rgnOffPoint2 << (rgnOffPoint2 + pRgnHdr->length_point2);

    if (subfile.parts.contains("LBL"))
    {
      QByteArray lblhdr;
      readFile(file, subfile.parts["LBL"].offset, sizeof(hdr_lbl_t), lblhdr);
      hdr_lbl_t *pLblHdr = (hdr_lbl_t *)lblhdr.data();

      quint32 offsetLbl1 = subfile.parts["LBL"].offset + gar_load(quint32, pLblHdr->lbl1_offset);
      quint32 offsetLbl6 = subfile.parts["LBL"].offset + gar_load(quint32, pLblHdr->lbl6_offset);

      QByteArray nethdr;
      quint32 offsetNet1 = 0;
      hdr_net_t *pNetHdr = nullptr;
      if (subfile.parts.contains("NET"))
      {
        readFile(file, subfile.parts["NET"].offset, sizeof(hdr_net_t), nethdr);
        pNetHdr = (hdr_net_t *)nethdr.data();
        offsetNet1 = subfile.parts["NET"].offset + gar_load(quint32, pNetHdr->net1_offset);
      }

      quint16 codepage = 0;
      if (gar_load(uint16_t, pLblHdr->length) > 0xAA)
      {
        codepage = gar_load(uint16_t, pLblHdr->codepage);
      }

      qDebug() << file.fileName() << Qt::hex << offsetLbl1 << offsetLbl6 << offsetNet1;

      QObject *obj = qobject_cast<QObject *>(this);
      switch (pLblHdr->coding)
      {
      case 0x06:
        subfile.strtbl = new CGarminStrTbl6(codepage, mask, obj);
        break;

      case 0x09:
      case 0x0A:
        subfile.strtbl = new CGarminStrTblUtf8(codepage, mask, obj);
        break;

      default:
        qWarning() << "Unknown label coding" << Qt::hex << pLblHdr->coding;
      }

      if (nullptr != subfile.strtbl)
      {
        subfile.strtbl->registerLBL1(offsetLbl1, gar_load(quint32, pLblHdr->lbl1_length), pLblHdr->addr_shift);
        subfile.strtbl->registerLBL6(offsetLbl6, gar_load(quint32, pLblHdr->lbl6_length));
        if (nullptr != pNetHdr)
        {
          subfile.strtbl->registerNET1(offsetNet1, gar_load(quint32, pNetHdr->net1_length), pNetHdr->net1_addr_shift);
        }
      }
    }
  }

  void processPrimaryMapData()
  {
    /*
     * Query all subfiles for possible maplevels.
     * Exclude basemap to avoid pollution.
     */
    for (const subfile_desc_t &subfile : std::as_const(subfiles))
    {
      for (const maplevel_t &maplevel : subfile.maplevels)
      {
        if (!maplevel.inherited)
        {
          map_level_t ml;
          ml.bits = maplevel.bits;
          ml.level = maplevel.level;
          ml.useBaseMap = false;
          maplevels << ml;
        }
      }
    }

    /* Sort all entries, note that stable sort should insure that basemap is preferred when available. */
    std::stable_sort(maplevels.begin(), maplevels.end(), map_level_t::GreaterThan);
    /* Delete any duplicates for obvious performance reasons. */
    auto where = std::unique(maplevels.begin(), maplevels.end());
    maplevels.erase(where, maplevels.end());

#ifdef DEBUG_SHOW_MAPLEVELS
    for (int i = 0; i < maplevels.count(); ++i)
    {
      map_level_t &ml = maplevels[i];
      qDebug() << ml.bits << ml.level << ml.useBaseMap;
    }
#endif
  }

  void readFile(CFileExt &file, quint32 offset, quint32 size, QByteArray &data)
  {
    if (offset + size > file.size())
    {
      throw exce_t(eErrOpen, tr("Failed to read: ") + filename);
    }

    data = QByteArray::fromRawData(file.data(offset, size), size);
    // if mask == 0, no xor is necessary
    if (mask == 0)
    {
      return;
    }

#ifdef HOST_IS_64_BIT
    quint64 *p64 = (quint64 *)data.data();
    for (quint32 i = 0; i < size / 8; ++i)
    {
      *p64++ ^= mask64;
    }
    quint32 rest = size % 8;
    quint8 *p = (quint8 *)p64;
#else
    quint32 *p32 = (quint32 *)data.data();
    for (quint32 i = 0; i < size / 4; ++i)
    {
      *p32++ ^= mask32;
    }
    quint32 rest = size % 4;
    quint8 *p = (quint8 *)p32;
#endif

    for (quint32 i = 0; i < rest; ++i)
    {
      *p++ ^= mask;
    }
  }

  QString filename = "02235001-trim.img";
  quint8 mask;
  quint32 mask32;
  quint64 mask64;
  QString mapdesc;
  /// hold all subfile descriptors
  /**
      In a normal *.img file there is only one subfile. However
      gmapsupp.img files can hold several subfiles each with it's
      own subfile parts.
   */
  QMap<QString, subfile_desc_t> subfiles;
  /// relay the transparent flags from the subfiles
  bool transparent = false;

  QRectF maparea;

  // QFontMetrics fm;

  /// combined maplevels of all tiles
  QVector<map_level_t> maplevels;

  QMap<quint32, CGarminTyp::polyline_property> polylineProperties;
  QMap<quint32, CGarminTyp::polygon_property> polygonProperties;
  QList<quint32> polygonDrawOrder;
  QMap<quint32, CGarminTyp::point_property> pointProperties;
  QMap<quint8, QString> languages;

  polytype_t polygons;
  polytype_t polylines;
  pointtype_t points;
  pointtype_t pois;

  QVector<strlbl_t> labels;

  struct textpath_t
  {
    // QPainterPath path;
    QPolygonF polyline;
    QString text;
    // QFont font;
    QVector<qreal> lengths;
    qint32 lineWidth;
    QColor color;
  };

  QVector<textpath_t> textpaths;
  qint8 selectedLanguage;
  QSet<QString> copyrights;
};

int main(int argc, char *argv[])
{
  CMap cmap(argc, argv);
  return 0;
}
