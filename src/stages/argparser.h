#pragma once

#include <QCommandLineParser>
#include <QString>

#include "context.h"

namespace App {

class ArgParser {
 public:
  explicit ArgParser(Ctx &ctx);
  ~ArgParser() = default;

  void parseArguments(int &argc, char **argv);

 private:
  Ctx &ctx;
  QCommandLineParser parser;
  QCommandLineOption inputOption;
  QCommandLineOption outputOption;
  QCommandLineOption splitOption;
  QCommandLineOption debugOption;
  QCommandLineOption fsizeOption;
};

}  // namespace App
