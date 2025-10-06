#pragma once

#include <QDebug>
#include <QFile>
#include <QObject>
#include <QTextCodec>

#include "misc.h"

class StrTbl : public QObject {
 public:
  StrTbl(const quint16 codepage, const quint8 mask, QObject *parent);
  ~StrTbl() override;

  quint32 sizeLBL1 = 0;
  quint32 offsetLBL1 = 0;
  char buffer[1025];
  enum class label_type { lbl, poi, net };

  void registerLBL1(const quint32 offset, const quint32 size, const quint8 shift);
  void registerLBL6(const quint32 offset, const quint32 size);
  void registerNET1(const quint32 offset, const quint32 size, const quint8 shift);
  virtual void get(QFile &srcFile, quint32 offset, label_type t, QStringList &info) = 0;

 private:
  quint32 offsetLBL6 = 0;
  quint32 sizeLBL6 = 0;
  quint32 offsetNET1 = 0;
  quint32 sizeNET1 = 0;
  quint8 addrshift1 = 0;
  quint8 addrshift2 = 0;
  quint16 codepage;
  QTextCodec *codec = nullptr;
  const quint8 mask;
  quint32 mask32;
  quint64 mask64;

 protected:
  void readFile(QFile &srcFile, quint32 offset, quint32 size, QByteArray &data);
  quint32 getNewOffset(QFile &srcFile, const quint32 offset, label_type t);
  QString processLabel(const char *buffer, unsigned lastSeperator) const;
};

class StrTbl6 : public StrTbl {
 public:
  StrTbl6(const quint16 codepage, const quint8 mask, QObject *parent);
  ~StrTbl6() override;

  void get(QFile &srcFile, quint32 offset, label_type t, QStringList &labels) override;

 private:
  inline static const char str6tbl1[] = {' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
                                         'X', 'Y', 'Z', 0,   0,   0,   0,   0,   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 0,   0,   0,   0,   0,   0};
  inline static const char str6tbl2[] = {'@', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', 0, 0, 0, 0,   0,    0,   0,   0,
                                         0,   0,   ':', ';', '<', '=', '>', '?',  0,   0,   0,   0,   0,   0,   0,   0,   0, 0, 0, '[', '\\', ']', '^', '_'};
  inline static const char str6tbl3[] = {'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};

  void fill();

  quint32 reg = 0;            // temp shift reg buffer
  quint32 bits = 0;           // bits in buffer
  const quint8 *p = nullptr;  // pointer to current data;
};

class StrTblUtf8 : public StrTbl {
 public:
  StrTblUtf8(const quint16 codepage, const quint8 mask, QObject *parent);
  ~StrTblUtf8() override;

  void get(QFile &srcFile, quint32 offset, label_type t, QStringList &labels) override;
};
