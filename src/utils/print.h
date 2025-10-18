#pragma once

#include <QString>
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <type_traits>

namespace App {

struct PrintOptions {
  bool showHex = true;
  bool showAscii = true;
  bool showSize = false;
  int maxElements = 0;
  bool compactHex = false;
};

template <typename Container>
void printAsciiData(const Container &data, size_t elementsToShow) {
  size_t count = 0;
  for (const auto &element : data) {
    if (count++ >= elementsToShow) break;
    auto c = static_cast<char>(element);
    std::cout << ((c >= 32 && c <= 126) ? c : '.');
  }
}

template <typename Container>
void printHexData(const Container &data, size_t elementsToShow, bool compact) {
  size_t count = 0;
  for (const auto &element : data) {
    if (count++ >= elementsToShow) break;
    std::cout << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(static_cast<quint8>(element));
    if (!compact && count < elementsToShow) std::cout << " ";
  }
  if (!compact && count > 0) std::cout << " ";
}

template <typename T, typename = void>
struct is_container : std::false_type {};

template <typename T>
struct is_container<T, std::void_t<decltype(std::begin(std::declval<T>())), decltype(std::end(std::declval<T>()))>> : std::true_type {};

template <typename T>
constexpr bool is_container_v = is_container<T>::value;

template <typename Container>
void printContainer(const Container &data, const PrintOptions &options) {
  using std::size;

  const size_t containerSize = size(data);
  const size_t elementsToShow = options.maxElements > 0 ? std::min(static_cast<size_t>(options.maxElements), containerSize) : containerSize;

  if (options.showSize) {
    std::cout << "[" << containerSize << "] ";
  }

  if (options.showHex) {
    printHexData(data, elementsToShow, options.compactHex);
  }

  if (options.showAscii) {
    if (options.showHex) std::cout << "| ";
    printAsciiData(data, elementsToShow);
  }

  if (elementsToShow < containerSize) {
    std::cout << "...";
  }

  std::cout << std::endl;
}

template <typename T>
void printSingleValue(const T &data, const PrintOptions &options) {
  if constexpr (std::is_same_v<T, std::array<quint8, 3>>) {
    quint32 tmp = static_cast<quint32>(data[0]) | (static_cast<quint32>(data[1]) << 8) | (static_cast<quint32>(data[2]) << 16);
    std::printf("%f (0x%06X, %i)\n", GRMN_DEG(tmp), tmp, tmp);
  } else if constexpr (std::is_unsigned_v<T>) {
    if constexpr (std::is_same_v<T, quint8>)
      std::printf("%02X\n", data);
    else if constexpr (std::is_same_v<T, quint16>)
      std::printf("%04X\n", data);
    else if constexpr (std::is_same_v<T, quint32>)
      std::printf("%08X\n", data);
    else
      std::printf("%X\n", data);
  } else {
    std::printf("%i\n", static_cast<int>(data));
  }
}

// core
template <typename T>
void printDataImpl(const std::string &label, const T &data, PrintOptions options = {}) {
  std::cout << std::setw(30) << std::left << label;

  if constexpr (is_container_v<T>) {
    printContainer(data, options);
  } else {
    printSingleValue(data, options);
  }
}

// helpers
template <typename T>
void printData(const std::string &label, const T &data) {
  PrintOptions options;
  options.showHex = is_container_v<T> || std::is_unsigned_v<T>;
  options.showAscii = is_container_v<T>;
  printDataImpl(label, data, options);
}

template <typename Container>
void printCompact(const std::string &label, const Container &data, int maxElements = 0) {
  printData(label, data, PrintOptions{true, false, false, maxElements, true});
}

}  // namespace App
