#include <QStack>
#include <QRegularExpression>
#include <QPen>
#include <QtCore5Compat/QTextCodec>
#include <QFileInfo>
#include <QDebug>
#include <QCoreApplication>
#include <QCommandLineParser>
#include <QCommandLineOption>

#define DEBUG_SHOW_SECT_DESC
#define DEBUG_SHOW_TRE_DATA
#define DEBUG_SHOW_MAPLEVEL_DATA
#define DEBUG_SHOW_SUBDIV_DATA
#define DEBUG_SHOW_MAPLEVELS
// #define DEBUG_SHOW_POINTS
#define DEBUG_SHOW_POLY_DATA_SUBDIV
// #define DEBUG_SHOW_POLY_DATA_DECODE1
// #define DEBUG_SHOW_POLY_DATA_DECODE2
#define DEBUG_SHOW_POLY_PTS

#define MAX_FLOAT_PREC 16777216.0 // 2^24: precision limit of float
#define RAD_TO_DEG 180 / M_PI
#define DEG_TO_RAD M_PI / 180
#define GARMIN_DEG(x) ((x) < 0x800000 ? (qreal)(x) * 360.0 / MAX_FLOAT_PREC : (qreal)((x) - 0x1000000) * 360.0 / MAX_FLOAT_PREC)
#define GARMIN_RAD(x) ((x) < 0x800000 ? (qreal)(x) * (2 * M_PI) / MAX_FLOAT_PREC : (qreal)((x) - 0x1000000) * (2 * M_PI) / MAX_FLOAT_PREC)

// load argument of type t from pointer p
#define gar_ptr_load(t, p) __gar_ptr_load_##t((const uint8_t *)(p))
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

// store argument src of type t in in the location to which the pointer p points
#define gar_ptr_store(t, p, src) __gar_ptr_store_##t((uint8_t *)(p), (src))
#define __gar_ptr_store_int16_t(p, src) (*((int16_t *)(p))) = (src)
#define __gar_ptr_store_int32_t(p, src) (*((int32_t *)(p))) = (src)
#define __gar_ptr_store_int64_t(p, src) (*((int64_t *)(p))) = (src)
#define __gar_ptr_store_uint16_t(p, src) (*((uint16_t *)(p))) = (src)
#define __gar_ptr_store_quint32(p, src) (*((quint32 *)(p))) = (src)
#define __gar_ptr_store_uint64_t(p, src) (*((uint64_t *)(p))) = (src)
#define __gar_ptr_store_float(p, src) (*((float *)(p))) = (src)
#define __gar_ptr_store_double(p, src) (*((double *)(p))) = (src)

#define gar_endian(t, x) (t)(x) // little endian platform: just return the argument
#define gar_load(t, x) (t)(x)   // load argument x of type t - noop with proper cast

typedef quint8 quint24[3];

struct sign_info_t
{
  quint32 sign_info_bits = 2;
  bool x_has_sign = true;
  bool nx = false;
  bool y_has_sign = true;
  bool ny = false;
};

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
  inline static int cnt = 0;

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
      // throw exce_t(eErrOpen, tr("Failed to read: ") + file.filename());
      return;
    }

    data = QByteArray::fromRawData(file.data(offset, size), size);
    // wenn mask == 0 ist kein xor noetig
    if (mask == 0)
    {
      return;
    }

    quint64 *p64 = (quint64 *)data.data();
    for (quint32 i = 0; i < size / 8; ++i)
    {
      *p64++ ^= mask64;
    }
    quint32 rest = size % 8;
    quint8 *p = (quint8 *)p64;

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
    // qDebug() << Qt::hex << newOffset;
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
      // qWarning() << "Index into string table to large" << Qt::hex << offset << dataLBL.size() <<
      // hdrLbl->addr_shift << hdrNet->net1_addr_shift;
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
      // qWarning() << "Index into string table to large" << Qt::hex << offset << dataLBL.size() << hdrLbl->addr_shift << hdrNet->net1_addr_shift;
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

    // qDebug() << Qt::hex << lbl_in_NET << extra_bit << lbl_info;

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

    if (maxVecSize < coords.size())
    {
      maxVecSize = coords.size();
    }
    if (coords.size() * 1.2 < maxVecSize)
    {
      coords.squeeze();
    }

    pixel = coords;

    // qDebug() << "decode1() coords:" << type << Qt::hex << type << Qt::hex << bs_info << coords;

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

    // qDebug() << ">>" << bs_len << bytes_total << (pEnd - pStart);

    // assert((pEnd - pStart) >= bytes_total);
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

      // xy.u = GARMIN_RAD(x1);
      // xy.v = GARMIN_RAD(y1);

      // if(qAbs(xy.v) > 2*M_PI || qAbs(xy.u) > 2*M_PI)
      // {
      //    qDebug() << "bam";
      //    qDebug() << xy.u << xy.v << pStart << pEnd << (pEnd - pStart) << (cnt + 1) << line;
      //    //assert(0);
      // }
      // #ifdef DEBUG_SHOW_POLY_PTS
      // qDebug() << xy.u << xy.v << (RAD_TO_DEG * xy.u) << (RAD_TO_DEG * xy.v);
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

    return bytes_total;
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

  QStringList labels;

  inline static quint32 cnt = 0;
  inline static qint32 maxVecSize = 0;

private:
  void bits_per_coord(quint8 base, quint8 bfirst, quint32 &bx, quint32 &by, sign_info_t &signinfo, bool isVer2)
  {
    bool x_sign_same, y_sign_same;

    quint8 mask = 0x1;

    // x_sign_same = bfirst & 0x1;
    x_sign_same = bfirst & mask;
    mask <<= 1;

    if (x_sign_same)
    {
      signinfo.x_has_sign = false;
      // signinfo.nx         = bfirst & 0x2;
      signinfo.nx = bfirst & mask;
      mask <<= 1;
      ++signinfo.sign_info_bits;
    }
    else
    {
      signinfo.x_has_sign = true;
    }
    bx = bits_per_coord(base & 0x0F, signinfo.x_has_sign);

    // y_sign_same = x_sign_same ? (bfirst & 0x04) : (bfirst & 0x02);
    y_sign_same = bfirst & mask;
    mask <<= 1;

    if (y_sign_same)
    {
      signinfo.y_has_sign = false;
      // signinfo.ny         = x_sign_same ? bfirst & 0x08 : bfirst & 0x04;
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
        //     qDebug() << "V2";
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

class CMap : public QCoreApplication
{
public:
  explicit CMap(int &argc, char **argv) : QCoreApplication(argc, argv)
  {
    QCommandLineParser parser;
    parser.setApplicationDescription("qgimp 1.0");
    parser.addHelpOption();

    // Добавяне на опции
    QCommandLineOption inputOption(QStringList() << "i" << "input", ".img input file", "input_file.img");
    QCommandLineOption outputOption(QStringList() << "o" << "output", ".mp output file", "output_file.mp");
    parser.addOption(inputOption);
    parser.addOption(outputOption);
    parser.process(this->arguments());

    if (!parser.isSet(inputOption))
    {
      qCritical() << "Option -i is required!";
      parser.showHelp(1);
    }
    else
    {
      inputFile = parser.value("input");
      QFileInfo inputFileInfo(inputFile);

      if (!inputFileInfo.exists())
      {
        qCritical() << "Грешка: Входният файл не съществува:" << inputFile;
        exit(1);
      }

      if (!inputFileInfo.isReadable())
      {
        qCritical() << "Грешка: Няма права за четене на файла:" << inputFile;
        exit(1);
      }
    }

    if (!parser.isSet(outputOption))
    {
      qCritical() << "Option -o is required!";
      parser.showHelp(1);
    }
    else
    {
      outputFile = parser.value("output");
      QFileInfo outputFileInfo(outputFile);

      if (outputFileInfo.exists())
      {
        // Генериране на timestamp
        QDateTime lastModified = outputFileInfo.lastModified();
        QString timestamp = lastModified.toString("_yyyyMMdd_hhmmss");
        QString baseName = outputFileInfo.completeBaseName();
        QString suffix = outputFileInfo.suffix();
        QString dir = outputFileInfo.path();

        QString renamedFile = dir + "/" + baseName + timestamp + "." + suffix;

        // Преименуване на съществуващия файл
        QFile::rename(outputFile, renamedFile);
        qWarning() << "Съществуващият файл беше преименуван на:" << renamedFile;
      }
    }

    of.setFileName(outputFile);
    if (!of.open(QIODevice::WriteOnly | QIODevice::Text))
    {
      qCritical() << "Грешка при отваряне на изходен файл:" << of.errorString();
      exit(1);
    }

    codec = QTextCodec::codecForName("Windows-1251");
    if (!codec)
    {
      qWarning("CP1251 codec not available");
    }
    // of.setBufferSize(16 * 1024);

    try
    {
      readBasics();
      processPrimaryMapData();
      printHeader();

      QVector<map_level_t>::const_iterator maplevel = maplevels.constBegin();
      while (maplevel)
      // QMapIterator<QString, int> i(maplevels);
      // while (i.hasNext())
      {
        // const auto maplevel = i.value()->level;
        // loadData(polygons, polylines, points, pois, maplevel);
        ++maplevel;
        loadData(polygons, polylines, points, pois, maplevel->level);

        if (maplevels.constEnd() == maplevel)
        {
          break;
        }
      }
    }
    catch (const exce_t &e)
    {
      qDebug() << "Fatal error:" << e.msg;
    }
  }

  virtual ~CMap()
  {
    if (of.isOpen())
    {
      of.flush();
      of.close();
    }
    qDebug() << "✅ Done.";
  }

private:
  typedef QVector<CGarminPolygon> polytype_t;
  typedef QVector<CGarminPoint> pointtype_t;

  struct maplevel_t
  {
    bool inherited;
    quint8 level;
    quint8 bits;
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

  QString copyright; //< a copyright string to be displayed as tool tip
  QString inputFile = "";
  QString outputFile = "";
  QFile of;
  QTextCodec *codec = nullptr;
  quint8 mask;
  quint32 mask32;
  quint64 mask64;
  QString mapdesc;
  QMap<QString, subfile_desc_t> subfiles; // hold all subfile descriptors: In a normal *.img file there is only one subfile. However gmapsupp.img files can hold several subfiles each with it's own subfile parts.
  QRectF maparea;
  QVector<map_level_t> maplevels; // combined maplevels of all tiles
  polytype_t polygons;
  polytype_t polylines;
  pointtype_t points;
  pointtype_t pois;
  QSet<QString> copyrights;
  QString mCodePage = "";
  QString mCoding = "";

  QString convPtDegStr(const QPointF &point)
  {
    return QString("(%1,%2)").arg(point.y() * RAD_TO_DEG, 0, 'f', 5).arg(point.x() * RAD_TO_DEG, 0, 'f', 5);
  }

  QString convLnDegStr(const QPolygonF &polygon)
  {
    QStringList resultSl;
    for (const QPointF &point : polygon)
    {
      resultSl << convPtDegStr(point);
    }
    return resultSl.join(',');
  }

  static inline bool isCompletelyOutside(const QPolygonF &poly, const QRectF &viewport)
  {
    // qDebug() << "isCompletelyOutside()" << poly << viewport << viewport.topLeft() << viewport.bottomRight();
    qreal north = -90.0 * DEG_TO_RAD;
    qreal south = 90.0 * DEG_TO_RAD;
    qreal west = 180.0 * DEG_TO_RAD;
    qreal east = -180.0 * DEG_TO_RAD;

    for (const QPointF &pt : poly)
    {
      if (north < pt.y())
      {
        north = pt.y();
      }
      if (south > pt.y())
      {
        south = pt.y();
      }
      if (west > pt.x())
      {
        west = pt.x();
      }
      if (east < pt.x())
      {
        east = pt.x();
      }
    }

    QRectF ref(west, north, east - west, south - north);

    if (ref.width() == 0)
    {
      ref.setWidth(0.00001);
    }
    if (ref.height() == 0)
    {
      ref.setHeight(0.00001);
    }

    return !viewport.intersects(ref);
  }

  void loadData(polytype_t &polygons, polytype_t &polylines, pointtype_t &points, pointtype_t &pois, unsigned level)
  {
    qDebug() << "ML:111 level" << level;

    for (const subfile_desc_t &subfile : std::as_const(subfiles))
    {
      qDebug() << "-------";
      qDebug() << (subfile.area.topLeft() * RAD_TO_DEG) << (subfile.area.bottomRight() * RAD_TO_DEG);

      CFileExt file(inputFile);
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
        if (subdiv.level != level)
        {
          qDebug() << "Level does not exists (skip):" << level << subdiv.level;
          continue;
        }
        loadSubDiv(file, subdiv, subfile.strtbl, rgndata, polylines, polygons, points, pois);
      }

      file.close();
    }
  }

  void loadSubDiv(CFileExt &file, const subdiv_desc_t &subdiv, IGarminStrTbl *strtbl, const QByteArray &rgndata, polytype_t &polylines, polytype_t &polygons, pointtype_t &points, pointtype_t &pois)
  {
    if (subdiv.rgn_start == subdiv.rgn_end && !subdiv.lengthPolygons2 && !subdiv.lengthPolylines2 &&
        !subdiv.lengthPoints2)
    {
      return;
    }

    points.clear();
    pois.clear();
    polylines.clear();
    polygons.clear();

    qDebug() << "\n--------- loadSubDiv()" << file.fileName() << "---------";
    qDebug() << " Level:" << subdiv.level << " Area:" << subdiv.area;

    const quint8 *pRawData = (quint8 *)rgndata.data();

    quint32 opnt = 0, oidx = 0, opline = 0, opgon = 0;
    quint32 objCnt = subdiv.hasIdxPoints + subdiv.hasPoints + subdiv.hasPolylines + subdiv.hasPolygons;

    quint16 *pOffset = (quint16 *)(pRawData + subdiv.rgn_start);

    // test for points
    if (subdiv.hasPoints)
    {
      opnt = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
    }

    // test for pois
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

    // test for polygons
    if (subdiv.hasPolygons)
    {
      if (opnt || oidx || opline)
      {
        opgon = gar_load(uint16_t, *pOffset);
        opgon += subdiv.rgn_start;
        ++pOffset;
      }
      else
      {
        opgon = (objCnt - 1) * sizeof(quint16) + subdiv.rgn_start;
      }
    }

#ifdef DEBUG_SHOW_POLY_DATA_SUBDIV
    qDebug() << "--- Subdivision" << subdiv.n << subdiv.area.topLeft() << subdiv.area.bottomRight() << "---";
    qDebug() << "addr:" << Qt::hex << subdiv.rgn_start << "- " << subdiv.rgn_end;
    qDebug() << "addr points:            " << Qt::hex << opnt;
    qDebug() << "addr pois:    " << Qt::hex << oidx;
    qDebug() << "addr polylines:         " << Qt::hex << opline;
    qDebug() << "addr polygons:          " << Qt::hex << opgon;
#endif

    CGarminPolygon p;

    // decode points
    if (subdiv.hasPoints)
    {
      const quint8 *pData = pRawData + opnt;
      const quint8 *pEnd = pRawData + (oidx ? oidx : opline ? opline
                                                 : opgon    ? opgon
                                                            : subdiv.rgn_end);
      while (pData < pEnd)
      {
        CGarminPoint p;
        pData += p.decode1(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData);

        // skip points outside our current viewport
        if (!subdiv.area.contains(p.pos))
        {
          qDebug() << "skip points outside our current viewport:" << subdiv.area.topLeft() << subdiv.area.bottomRight() << p.pos;
          continue;
        }

        if (strtbl)
        {
          p.isLbl6 ? strtbl->get(file, p.lbl_ptr, IGarminStrTbl::poi, p.labels) : strtbl->get(file, p.lbl_ptr, IGarminStrTbl::norm, p.labels);
        }

        points.push_back(p);
      }
    }

    // decode pois
    if (subdiv.hasIdxPoints)
    {
      const quint8 *pData = pRawData + oidx;
      const quint8 *pEnd = pRawData + (opline ? opline : opgon ? opgon
                                                               : subdiv.rgn_end);
      while (pData < pEnd)
      {
        CGarminPoint p;
        pData += p.decode1(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData);

        // skip points outside our current viewport
        if (!subdiv.area.contains(p.pos))
        {
          continue;
        }

        if (strtbl)
        {
          p.isLbl6 ? strtbl->get(file, p.lbl_ptr, IGarminStrTbl::poi, p.labels) : strtbl->get(file, p.lbl_ptr, IGarminStrTbl::norm, p.labels);
        }

        pois.push_back(p);
      }
    }

    // decode polylines
    if (subdiv.hasPolylines)
    {
      CGarminPolygon::cnt = 0;
      const quint8 *pData = pRawData + opline;
      const quint8 *pEnd = pRawData + (opgon ? opgon : subdiv.rgn_end);
      while (pData < pEnd)
      {
        pData += p.decode1(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, true, pData, pEnd);

        // skip points outside our current viewport
        if (isCompletelyOutside(p.pixel, subdiv.area))
        {
          continue;
        }

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

    // decode polygons
    if (subdiv.hasPolygons)
    {
      CGarminPolygon::cnt = 0;
      const quint8 *pData = pRawData + opgon;
      const quint8 *pEnd = pRawData + subdiv.rgn_end;

      while (pData < pEnd)
      {
        pData += p.decode1(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, false, pData, pEnd);

        // skip points outside our current viewport
        if (isCompletelyOutside(p.pixel, subdiv.area))
        {
          continue;
        }

        if (strtbl && !p.lbl_in_NET && p.lbl_info)
        {
          strtbl->get(file, p.lbl_info, IGarminStrTbl::norm, p.labels);
        }
        else if (strtbl && p.lbl_in_NET && p.lbl_info)
        {
          strtbl->get(file, p.lbl_info, IGarminStrTbl::net, p.labels);
        }

        polygons.push_back(p);
      }
    }

    // qDebug() << "polyg off: " << Qt::hex << subdiv.offsetPolygons2;
    // qDebug() << "polyg len: " << Qt::hex << subdiv.lengthPolygons2 << subdiv.lengthPolygons2;
    // qDebug() << "polyg end: " << Qt::hex << subdiv.lengthPolygons2 + subdiv.offsetPolygons2;
    // qDebug() << "polyl off: " << Qt::hex << subdiv.offsetPolylines2;
    // qDebug() << "polyl len: " << Qt::hex << subdiv.lengthPolylines2 << subdiv.lengthPolylines2;
    // qDebug() << "polyl end: " << Qt::hex << subdiv.lengthPolylines2 + subdiv.offsetPolylines2;
    // qDebug() << "point off: " << Qt::hex << subdiv.offsetPoints2;
    // qDebug() << "point len: " << Qt::hex << subdiv.lengthPoints2 << subdiv.lengthPoints2;
    // qDebug() << "point end: " << Qt::hex << subdiv.lengthPoints2 + subdiv.offsetPoints2;

    if (subdiv.lengthPolygons2)
    {
      const quint8 *pData = pRawData + subdiv.offsetPolygons2;
      const quint8 *pEnd = pData + subdiv.lengthPolygons2;
      while (pData < pEnd)
      {
        // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
        pData += p.decode2(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, false, pData, pEnd);

        // skip points outside our current viewport
        if (isCompletelyOutside(p.pixel, subdiv.area))
        {
          continue;
        }

        if (strtbl && !p.lbl_in_NET && p.lbl_info)
        {
          strtbl->get(file, p.lbl_info, IGarminStrTbl::norm, p.labels);
        }

        polygons.push_back(p);
      }
    }

    if (subdiv.lengthPolylines2)
    {
      const quint8 *pData = pRawData + subdiv.offsetPolylines2;
      const quint8 *pEnd = pData + subdiv.lengthPolylines2;
      while (pData < pEnd)
      {
        // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
        pData += p.decode2(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, true, pData, pEnd);

        // skip points outside our current viewport
        if (isCompletelyOutside(p.pixel, subdiv.area))
        {
          continue;
        }

        if (strtbl && !p.lbl_in_NET && p.lbl_info)
        {
          strtbl->get(file, p.lbl_info, IGarminStrTbl::norm, p.labels);
        }

        polylines.push_back(p);
      }
    }

    if (subdiv.lengthPoints2)
    {
      const quint8 *pData = pRawData + subdiv.offsetPoints2;
      const quint8 *pEnd = pData + subdiv.lengthPoints2;
      while (pData < pEnd)
      {
        CGarminPoint p;
        // qDebug() << "rgn offset:" << Qt::hex << (rgnoff + (pData - pRawData));
        pData += p.decode2(subdiv.iCenterLng, subdiv.iCenterLat, subdiv.shift, pData, pEnd);

        // skip points outside our current viewport
        if (!subdiv.area.contains(p.pos))
        {
          continue;
        }

        if (strtbl)
        {
          p.isLbl6 ? strtbl->get(file, p.lbl_ptr, IGarminStrTbl::poi, p.labels) : strtbl->get(file, p.lbl_ptr, IGarminStrTbl::norm, p.labels);
        }

        pois.push_back(p);
      }
    }

    // po dobre tuka da se pishe vyv faila...
    QStringList sl = QStringList();

    qDebug() << "total points:" << points.length();

    int count = 0;
    for (const CGarminPoint &pt : points)
    {
      ++count;
      // sl << "[POINT]" << QString("; subdiv=%1 | level=%2 | count=%3 | hasSubType=%4").arg(subdiv.n).arg(subdiv.level).arg(count).arg(pt.hasSubType) << "Type=0x" + QString::number(pt.type, 16);
      sl << "[POI]" << QString("; subdiv=%1 | level=%2 | count=%3 | hasSubType=%4").arg(subdiv.n).arg(subdiv.level).arg(count).arg(pt.hasSubType) << "Type=0x" + QString::number(pt.type, 16);

      int i = 0;
      if (pt.hasLabel())
      {
        if (pt.labels.length() == 1)
        {
          sl << QString("Label=%1").arg(pt.labels.at(0));
        }
        else
        {
          for (const QString &l : pt.labels)
          {
            sl << QString("Label%1=%2").arg(i).arg(l);
            ++i;
          }
        }
      }

      if (!pt.pos.isNull())
      {
        sl << QString("Data%1=%2").arg(subdiv.level).arg(convPtDegStr(pt.pos));
      }
      sl << "[END]" << "\n";
    }

    count = 0;
    qDebug() << "total pois : " << pois.length();
    for (const CGarminPoint &poi : pois)
    {
      ++count;
      sl << "[POI]" << QString("; subdiv=%1 | level=%2 | count=%3").arg(subdiv.n).arg(subdiv.level).arg(count) << "Type=0x" + QString::number(poi.type, 16);

      int i = 0;
      if (poi.hasLabel())
      {
        if (poi.labels.length() == 1)
        {
          sl << QString("Label=%1").arg(poi.labels.at(0));
        }
        else
        {
          for (const QString &l : poi.labels)
          {
            sl << QString("Label%1=%2").arg(i).arg(l);
            ++i;
          }
        }
      }

      if (!poi.pos.isNull())
      {
        sl << QString("Data%1=%2").arg(subdiv.level).arg(convPtDegStr(poi.pos));
      }
      sl << "[END]" << "\n";
    }

    count = 0;
    qDebug() << "total polylines:" << polylines.length();
    for (const CGarminPolygon &ln : polylines)
    {
      ++count;
      sl << "[POLYLINE]" << QString("; subdiv=%1 | level=%2 | count=%3 | hasSubType=?").arg(subdiv.n).arg(subdiv.level).arg(count) << "Type = 0x" + QString::number(ln.type, 16);

      int i = 0;
      if (ln.hasLabel())
      {
        if (ln.labels.length() == 1)
        {
          sl << QString("Label=%1").arg(ln.labels.at(0));
        }
        else
        {
          for (const QString &l : ln.labels)
          {
            sl << QString("Label%1=%2").arg(i).arg(l);
            ++i;
          }
        }
      }

      if (ln.coords.size() > 0)
      {
        sl << QString("Data%1=%2").arg(subdiv.level).arg(convLnDegStr(ln.coords));
      }
      sl << "[END]" << "\n";
    }

    count = 0;
    qDebug() << "total polygons:" << polygons.length();
    for (const CGarminPolygon &pg : polygons)
    {
      ++count;
      sl << "[POLYGON]" << QString("; subdiv=%1 | level=%2 | count=%3 | %4 = %5").arg(subdiv.n).arg(subdiv.level).arg(count).arg(RAD_TO_DEG).arg(180 / M_PI) << "Type=0x" + QString::number(pg.type, 16);

      int i = 0;
      if (pg.hasLabel())
      {
        if (pg.labels.length() == 1)
        {
          sl << QString("Label=%1").arg(pg.labels.at(0));
        }
        else
        {
          for (const QString &l : pg.labels)
          {
            sl << QString("Label%1=%2").arg(i).arg(l);
            ++i;
          }
        }
      }

      if (pg.coords.size() > 0)
      {
        sl << QString("Data%1=%2").arg(subdiv.level).arg(convLnDegStr(pg.coords));
      }
      sl << "[END]" << "\n";
    }

    of.write(codec->fromUnicode(sl.join("\n")));
    of.flush();
  }

  void readBasics()
  {
    char tmpstr[64];
    qint64 fsize = QFileInfo(inputFile).size();

    CFileExt file(inputFile);
    if (!file.open(QIODevice::ReadOnly))
    {
      throw exce_t(eErrOpen, tr("Failed to open: ") + inputFile);
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
      throw exce_t(errFormat, tr("Bad file format: ") + inputFile);
    }
    if (strncmp(pImgHdr->identifier, "GARMIN", 7) != 0)
    {
      throw exce_t(errFormat, tr("Bad file format: ") + inputFile);
    }

    mapdesc = QByteArray((const char *)pImgHdr->desc1, 20);
    mapdesc += pImgHdr->desc2;
    qDebug() << "MAP Description:" << mapdesc;

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
      throw exce_t(errFormat, tr("Failed to read file structure: ") + inputFile);
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
        throw exce_t(errFormat, tr("File is NT format. qgimg is unable to read map files with NT format: ") + inputFile);
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
    // qDebug() << "copyright:" << copyright;

    qDebug()
        << "dimensions:\t"
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
    readFile(file, subfile.parts["TRE"].offset + gar_load(quint32, pTreHdr->tre1_offset), gar_load(quint32, pTreHdr->tre1_size), maplevel);
    const tre_map_level_t *pMapLevel = (const tre_map_level_t *)maplevel.data();

    if (pTreHdr->flag & 0x80)
    {
      throw exce_t(errLock, tr("File contains locked / encrypted data. Garmin does no want you to use this file with any other software than the one supplied by Garmin."));
    }

    quint32 nlevels = gar_load(quint32, pTreHdr->tre1_size) / sizeof(tre_map_level_t);
    quint32 nsubdivs = 0;
    quint32 nsubdivs_last = 0;

    // QMap<QString, QString> levelZoom;
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
      // levelZoom.insert(QVariant(ml.level).toString(), QVariant(ml.bits).toString());
#ifdef DEBUG_SHOW_MAPLEVEL_DATA
      qDebug()
          << "level:" << TRE_MAP_LEVEL(pMapLevel) << "| inherited:" << TRE_MAP_INHER(pMapLevel) << "| bits:" << pMapLevel->bits << "| #subdivs:" << gar_load(uint16_t, pMapLevel->nsubdiv);
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
    // subfile.parts["RGN"].offset +
    quint32 rgnoff = gar_load(quint32, pRgnHdr->offset);
    quint32 rgnOffPolyg2 = gar_load(quint32, pRgnHdr->offset_polyg2);
    quint32 rgnOffPolyl2 = gar_load(quint32, pRgnHdr->offset_polyl2);
    quint32 rgnOffPoint2 = gar_load(quint32, pRgnHdr->offset_point2);
    quint32 rgnLenPolyg2 = gar_load(quint32, pRgnHdr->length_polyg2);
    quint32 rgnLenPolyl2 = gar_load(quint32, pRgnHdr->length_polyl2);
    quint32 rgnLenPoint2 = gar_load(quint32, pRgnHdr->length_point2);

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
        qDebug() << "has pois " << subdiv->hasIdxPoints;
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

      if (codepage > 0)
      {
        mCodePage = QString("%1").arg(codepage);
      }

      qDebug() << file.fileName() << Qt::hex << offsetLbl1 << offsetLbl6 << offsetNet1;

      QObject *obj = qobject_cast<QObject *>(this);
      switch (pLblHdr->coding)
      {
      case 0x06:
        subfile.strtbl = new CGarminStrTbl6(codepage, mask, obj);
        mCoding = QString("%1").arg(pLblHdr->coding);
        break;

      case 0x09: // utf-8
      case 0x0A: // utf-10?
        subfile.strtbl = new CGarminStrTblUtf8(codepage, mask, obj);
        mCoding = QString("%1").arg(pLblHdr->coding);
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
    // Query all subfiles for possible maplevels. Exclude basemap to avoid pollution
    for (const subfile_desc_t &subfile : std::as_const(subfiles))
    {
      for (const maplevel_t &maplevel : subfile.maplevels)
      {
        // if (!maplevel.inherited) // премахва първият слой, който по-принцип не ни трябва, но изглежда по-близо до резултата от MapEdit
        {
          map_level_t ml;
          ml.bits = maplevel.bits;
          ml.level = maplevel.level;
          ml.useBaseMap = false;
          maplevels << ml;
        }
      }
    }

    // Sort all entries, note that stable sort should insure that basemap is preferred when available
    std::stable_sort(maplevels.begin(), maplevels.end(), map_level_t::GreaterThan);

    // Delete any duplicates for obvious performance reasons
    auto where = std::unique(maplevels.begin(), maplevels.end());
    maplevels.erase(where, maplevels.end());

#ifdef DEBUG_SHOW_MAPLEVELS
    qDebug() << "========= Map levels info: =========";
    for (int i = 0; i < maplevels.count(); ++i)
    {
      map_level_t &ml = maplevels[i];
      qDebug() << ml.bits << ml.level << ml.useBaseMap;
    }
#endif
  }

  void printHeader()
  {
    qDebug() << "ALL MAP LEVEL:" << subfiles.size() << subfiles.count();
    bool isFirst = true;

    QStringList sl;
    QString name = "";
    QMap<QString, QString> levelZoom;
    QStringList zooms;
    QStringList levels;

    for (const subfile_desc_t &subfile : std::as_const(subfiles))
    {
      if (isFirst)
      {
        isFirst = false;

        for (int i = 0; i < maplevels.count(); ++i)
        {
          map_level_t &ml = maplevels[i];
          levelZoom.insert(QVariant(ml.level).toString(), QVariant(ml.bits).toString());
        }

        name = subfile.name;

        int index = 0;
        for (auto [key, value] : levelZoom.asKeyValueRange())
        {
          levels << QString("Level%1=%2").arg(index).arg(value);
          zooms << QString("Zoom%1=%2").arg(index).arg(key);
          ++index;
        }
      }
    }

    sl << "; Generated by qgimg 1.0\n"
       << "[IMG ID]" << QString("CodePage=%1").arg(mCodePage) << QString("LblCoding=%1").arg(mCoding)
       << QString("ID=%1").arg(name) << QString("Name=%1").arg(mapdesc.trimmed()) << QString("Levels=%1").arg(levelZoom.count()) << levels << zooms << "[END-IMG ID]\n\n";

    of.write(codec->fromUnicode(sl.join("\n")));
    of.flush();
  }

  void readFile(CFileExt &file, quint32 offset, quint32 size, QByteArray &data)
  {
    if (offset + size > file.size())
    {
      throw exce_t(eErrOpen, tr("Failed to read: ") + inputFile);
    }

    data = QByteArray::fromRawData(file.data(offset, size), size);
    // if mask == 0, no xor is necessary
    if (mask == 0)
    {
      return;
    }

    quint64 *p64 = (quint64 *)data.data();
    for (quint32 i = 0; i < size / 8; ++i)
    {
      *p64++ ^= mask64;
    }
    quint32 rest = size % 8;
    quint8 *p = (quint8 *)p64;

    for (quint32 i = 0; i < rest; ++i)
    {
      *p++ ^= mask;
    }
  }
};

int main(int argc, char *argv[])
{
  CMap cmap(argc, argv);
  return 0;
}

// samo za spravka
/*
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
    }
  }
}
*/
