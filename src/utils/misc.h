#pragma once

#include <QString>
#include <QtGlobal>
#include <QtMath>
#include <array>
#include <iostream>

// PJ_UV.u: lng/E-W/x
// PJ_UV.v: lat/N-S/y
// typedef struct {
// double u, v;
// } PJ_UV;

struct SignInfo_t {
  quint32 sign_info_bits{2};
  bool x_has_sign{true};
  bool nx{false};
  bool y_has_sign{true};
  bool ny{false};
};

enum class OutputType { empty, mp, csv };

/*
void print(const QString &message) {
  QMutexLocker lock(&mutex);
  fprintf(stdout, "%s", message.toUtf8().constData());
  fflush(stdout);
}
*/

namespace misc {
// base
inline void print(const QString &message) {
  std::cout << message.toStdString() << std::endl;
}

// overloads
inline void print(const std::string &message) {
  std::cout << message << std::endl;
}

inline void print(const char *message) {
  std::cout << message << std::endl;
}

// helpers
inline void debug(const QString &message) {
  print("[DEBUG] " + message);
}

inline void info(const QString &message) {
  print("[INFO] " + message);
}

inline void error(const QString &message) {
  print("[ERROR] " + message);
}

// format
inline void printf(const QString &format) {
  print(format);
}

template <typename T, typename... Args>
inline void printf(const QString &format, T &&first, Args &&...args) {
  QString s = format.arg(std::forward<T>(first));
  printf(s, std::forward<Args>(args)...);
}
}  // namespace misc

#define MISC_PRINT(msg) misc::print(msg)
#define MISC_DEBUG(msg) misc::debug(msg)
#define MISC_ERROR(msg) misc::error(msg)