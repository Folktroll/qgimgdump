#pragma once

#include <QString>

namespace App {

struct Config {
  QString inputFile = "";
  QString outputFile = "";
  bool splitSubmaps = false;
  bool csvOutput = false;
  bool debugInfo = false;
  float maxFileSize = 1 * 1000 * 1024 * 1024;

  bool shouldSplit() const { return splitSubmaps; }
  bool isDebugEnabled() const { return debugInfo; }
  QString getOutputFormat() const { return csvOutput ? "csv" : "mp"; }
};

}  // namespace App