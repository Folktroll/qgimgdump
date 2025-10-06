#pragma once

#include <QException>

class Exception : public QException {
 public:
  Exception(const QString &msg) : msg(msg) {}

  operator const QString &() const { return msg; }
  QString msg;
};
