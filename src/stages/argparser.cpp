#include "argparser.h"

#include <QDateTime>
#include <QDebug>
#include <QFile>
#include <QFileInfo>

#include "config.h"

// #include "exception.h"

using namespace App;

ArgParser::ArgParser(Ctx &ctx)
    : ctx(ctx),
      inputOption(QStringList() << "i" << "input", ".img input file", "input_file.img"),
      outputOption(QStringList() << "o" << "output", "base output file", "output_file"),
      splitOption(QStringList() << "s" << "submap", "Creates a separate file for each submap"),
      debugOption(QStringList() << "d" << "debug", "Print debug info"),
      fsizeOption(QStringList() << "maxsize", "File size per chunk in GB (.mp format only)", "maxsize", "1") {
  parser.setApplicationDescription("qgimpdec 1.0.0");
  parser.addHelpOption();
  parser.addVersionOption();

  parser.addOption(inputOption);
  parser.addOption(outputOption);
  parser.addOption(splitOption);
  parser.addOption(debugOption);
  parser.addOption(fsizeOption);
}

void ArgParser::parseArguments(int &argc, char **argv) {
  QCoreApplication app(argc, argv);
  parser.process(app);

  // Проверка за задължителни опции
  if (!parser.isSet(inputOption)) {
    qCritical() << "Option -i is required!";
    parser.showHelp(1);
  } else {
    ctx.config.inputFile = parser.value(inputOption);
    QFileInfo inputFileInfo(ctx.config.inputFile);

    if (!inputFileInfo.exists()) {
      qCritical() << "Error: Input file does not exist:" << ctx.config.inputFile;
      exit(1);
    }

    if (!inputFileInfo.isReadable()) {
      qCritical() << "Error: No permissions to read the file:" << ctx.config.inputFile;
      exit(1);
    }
  }

  if (!parser.isSet(outputOption)) {
    qCritical() << "Option -o is required!";
    parser.showHelp(1);
  } else {
    ctx.config.outputFile = parser.value(outputOption);
    QFileInfo outputFileInfo(ctx.config.outputFile);
    QString suffix = outputFileInfo.suffix();

    if (suffix == "mp") {
      ctx.config.csvOutput = false;
    } else if (suffix == "csv") {
      // simplified output file in csv format for faster processing
      ctx.config.csvOutput = true;
    } else {
      // Временно решение докато нямаш Exception клас
      qCritical() << "Unsupported output file format. Available formats: .csv, .mp";
      exit(1);
      // throw Exception("Unsupported output file format. Available formats: .csv, .mp");
    }

    if (outputFileInfo.exists()) {
      QDateTime lastModified = outputFileInfo.lastModified();
      QString timestamp = lastModified.toString("_yyyyMMdd_hhmm");
      QString baseName = outputFileInfo.completeBaseName();
      QString dir = outputFileInfo.path();

      QString renamedFile = dir + "/" + baseName + timestamp + "." + suffix;

      if (QFile::rename(ctx.config.outputFile, renamedFile)) {
        qWarning() << "The existing file was renamed to:" << renamedFile;
      } else {
        qWarning() << "Failed to rename existing file:" << ctx.config.outputFile;
      }
    }
  }

  if (parser.isSet(splitOption)) {
    ctx.config.splitSubmaps = true;
  }

  if (parser.isSet(debugOption)) {
    ctx.config.debugInfo = true;
  }

  if (parser.isSet(fsizeOption)) {
    ctx.config.maxFileSize = parser.value(fsizeOption).toFloat() * 1000 * 1024 * 1024;
  }
}