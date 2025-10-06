// Copyright (C) 2006-2025 Original authors & contributors
// SPDX-License-Identifier: GPL-3.0-or-later

#include <QCommandLineOption>
#include <QCommandLineParser>
#include <QCoreApplication>
#include <QDebug>
#include <QElapsedTimer>
#include <QException>
#include <QFile>
#include <QFileInfo>
#include <QMutex>
#include <QMutexLocker>
#include <QPen>
#include <QPolygonF>
#include <QRegularExpression>
#include <QRunnable>
#include <QStack>
#include <QThreadPool>
#include <QtCore5Compat/QTextCodec>
#include <iostream>

#ifdef _DEBUG
#include <crtdbg.h>
#endif

#include <cmath>

#include "exception.h"
#include "imgdump.h"
#include "misc.h"

int main(int argc, char *argv[]) {
#ifdef _DEBUG
  _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);  // check for memory leaks & heap corruption
  //_CrtSetDbgFlag(_CRTDBG_CHECK_ALWAYS_DF);
#endif

  int result = 0;
  try {
    QElapsedTimer timerMain;
    timerMain.start();
    ImgDump app(argc, argv);
    qDebug() << "Run time:" << QString::number(timerMain.elapsed() / 1000) << "sec.";
  } catch (const Exception &e) {
    fflush(stdout);
    fflush(stderr);
    std::cerr << std::endl << "Kaboom!" << std::endl << QString(e).toUtf8().constData();
    result = -1;
  }
  return result;
}
