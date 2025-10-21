#pragma once

#include <QString>
#include <QtGlobal>
#include <QtMath>
#include <array>
#include <iomanip>
#include <iostream>
#include <type_traits>

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

struct Logger {
  // --- base print ---
  static void print(const QString &msg) { std::cout << msg.toStdString() << std::endl; }

  static void print(const std::string &msg) { std::cout << msg << std::endl; }

  static void print(const char *msg) { std::cout << msg << std::endl; }

  // --- debug ---
  static void debug(const QString &msg) { print("[DEBUG] " + msg); }

  static void info(const QString &msg) { print("[INFO] " + msg); }

  static void error(const QString &msg) { print("[ERROR] " + msg); }

  // --- universal printf-like helper ---
  template <typename... Args>
  static void printf(const char *format, Args &&...args) {
    // using standard C-style printf
    std::printf(format, std::forward<Args>(args)...);
  }
};

template <size_t N>
std::string arrayToString(const std::array<char, N> &arr, bool stopAtNull = true) {
  if (stopAtNull) {
    auto it = std::ranges::find(arr, '\0');
    return std::string(arr.begin(), it);
  } else {
    return std::string(arr.begin(), arr.end());
  }
}

template <size_t N>
QString arrayToQString(const std::array<char, N> &arr, bool stopAtNull = true) {
  return QString::fromLatin1(arrayToString(arr, stopAtNull));
}

}  // namespace misc
