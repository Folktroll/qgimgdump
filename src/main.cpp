// Copyright (C) 2006-2025 Original authors & contributors
// SPDX-License-Identifier: GPL-3.0-or-later

#include <QCoreApplication>
#include <QDebug>
#include <QElapsedTimer>
#include <QFile>
#include <iostream>

#include "argparser.h"
#include "context.h"
#include "dataparser.h"
#include "exception.h"
#include "fileloader.h"
#include "filewriter.h"

#ifdef _DEBUG
#include <crtdbg.h>
#endif

using namespace App;

int main(int argc, char *argv[]) {
#ifdef _DEBUG
  _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);  // check for memory leaks & heap corruption
  //_CrtSetDbgFlag(_CRTDBG_CHECK_ALWAYS_DF);
#endif

  int result = 0;
  try {
    QElapsedTimer timerMain;
    timerMain.start();

    Ctx ctx;

    ArgParser argParser(ctx);
    argParser.parseArguments(argc, argv);

    ctx.io.srcFile = std::make_shared<QFile>(ctx.config.inputFile);

    if (!ctx.io.srcFile->open(QIODevice::ReadOnly)) {
      throw Exception("Failed to open file: " + ctx.config.inputFile);
    }

    FileLoader fileLoader(ctx);
    fileLoader.readFat();

    DataParser dataParser(ctx);
    dataParser.readSubmaps();
    dataParser.readObjects();

    FileWriter fileWriter(ctx);
    fileWriter.exportObjects();

    if (ctx.io.srcFile->isOpen()) {
      ctx.io.srcFile->close();
    }

    qDebug() << "✅ Done.";
    qDebug() << "Run time:" << QString::number(timerMain.elapsed() / 1000) << "sec.";
  } catch (const Exception &e) {
    fflush(stdout);
    fflush(stderr);
    std::cerr << std::endl << "Kaboom!" << std::endl << QString(e).toUtf8().constData();
    result = -1;
  }
  return result;
}
