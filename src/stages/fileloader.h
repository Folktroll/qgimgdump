#pragma once

#include <QFile>
#include <QtCore5Compat/QTextCodec>

#include "context.h"

namespace App {

class FileLoader {
 public:
  explicit FileLoader(Ctx &ctx);
  ~FileLoader();

  void readFat() const;

 private:
  Ctx &ctx;
  QFile &srcFile;
};

}  // namespace App
