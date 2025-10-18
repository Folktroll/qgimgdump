#pragma once

#include <QException>

namespace App {

class Exception : public QException {
 public:
  explicit Exception(const QString &msg) : msg(msg) {}

  explicit operator const QString &() const { return msg; }
  QString msg;
};

}  // namespace App
