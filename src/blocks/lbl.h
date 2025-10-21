#pragma once

#include <QDebug>
#include <QFile>
#include <QObject>
#include <QtCore5Compat/QTextCodec>
#include <string>

#include "context.h"

namespace App {

class StrTbl {
 public:
  explicit StrTbl(Ctx &ctx);
  virtual ~StrTbl();

  enum class LabelType { lbl, poi, net };

  void registerLbl1(const quint32 offset, const quint32 size, const quint8 shift);
  void registerLbl6(const quint32 offset, const quint32 size);
  void registerNet1(const quint32 offset, const quint32 size, const quint8 shift);
  virtual void get(quint32 offset, LabelType t, QStringList &info) = 0;
  quint32 sizeLbl1 = 0;
  quint32 offsetLbl1 = 0;

  quint32 offsetLbl6 = 0;
  quint32 sizeLbl6 = 0;
  quint32 offsetNet1 = 0;
  quint32 sizeNet1 = 0;
  quint8 addrShift1 = 0;
  quint8 addrShift2 = 0;

  quint16 codepage;
  Ctx &ctx;
  QFile &srcFile;

 protected:
  void readFile(quint32 offset, quint32 size, QByteArray &data) const;
  quint32 getNewOffset(const quint32 offset, LabelType t) const;
  QString processLabel(const char *buf, unsigned lastSeperator) const;
};

class StrTbl6 : public StrTbl {
 public:
  explicit StrTbl6(Ctx &ctx);
  ~StrTbl6() final;

  void get(quint32 offset, LabelType t, QStringList &labels) override;

 private:
  inline static const char str6tbl0[] = {'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};
  inline static const char str6tbl1[] = {' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
                                         'X', 'Y', 'Z', 0,   0,   0,   0,   0,   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 0,   0,   0,   0,   0,   0};
  inline static const char str6tbl2[] = {'@', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', 0, 0, 0, 0,   0,    0,   0,   0,
                                         0,   0,   ':', ';', '<', '=', '>', '?',  0,   0,   0,   0,   0,   0,   0,   0,   0, 0, 0, '[', '\\', ']', '^', '_'};

  void fill();

  quint32 reg = 0;            // temp shift reg buffer
  quint32 bits = 0;           // bits in buffer
  const quint8 *p = nullptr;  // pointer to current data;
};

class StrTbl9 : public StrTbl {
 public:
  explicit StrTbl9(Ctx &ctx);
  ~StrTbl9() final;

  void get(quint32 offset, LabelType t, QStringList &labels) override;
};

}  // namespace App
