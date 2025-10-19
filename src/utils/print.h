#pragma once

#include <QString>
#include <cctype>
#include <format>
#include <iomanip>
#include <iostream>
#include <string>
#include <type_traits>
#include <vector>

namespace App {

// --- Options for printing ---
struct PrintOptions {
  bool showHex = true;      // show hex values
  bool showAscii = true;    // show ASCII chars
  bool showSize = false;    // show container size
  int maxElements = 0;      // limit number of elements to show
  bool compactHex = false;  // 0x00 vs (0x) 00 11 22

  bool shouldShowHex(const std::string_view &label, bool isUnsigned) const {
    if (showHex) return true;
    static const std::vector<std::string> hexKeywords = {"offset", "size", "flag", "type", "mask", "addr", "crc"};
    for (const auto &kw : hexKeywords)
      if (label.find(kw) != std::string::npos) return true;
    return isUnsigned;
  }
};

// --- Check if type is container ---
template <typename T, typename = void>
struct is_container : std::false_type {};

template <typename T>
struct is_container<T, std::void_t<decltype(std::begin(std::declval<T>())), decltype(std::end(std::declval<T>()))>> : std::true_type {};

template <typename T>
constexpr bool is_container_v = is_container<T>::value;

// --- Print label ---
inline void printLabel(const std::string &label) {
  std::cout << std::format("{:>20}: ", label);  // right-align
}

// --- Print single value ---
template <typename T>
void printSingleValue(const std::string &label, const T &data, const PrintOptions &options) {
  bool isUnsigned = std::is_unsigned_v<T>;
  bool asHex = options.shouldShowHex(label, isUnsigned);

  printLabel(label);

  if (asHex) {
    std::cout << std::format("0x{:0{}X}", static_cast<uint64_t>(data), sizeof(T) * 2);
  } else {
    std::cout << data;
  }

  std::cout << std::endl;
}

template <typename Container>
void printAsciiData(const Container &data, size_t elementsToShow) {
  size_t count = 0;
  std::ios oldState(nullptr);
  oldState.copyfmt(std::cout);
  for (const auto &element : data) {
    if (count++ >= elementsToShow) break;
    auto c = static_cast<char>(element);
    std::cout << ((c >= 32 && c <= 126) ? c : '.');
  }
  std::cout.copyfmt(oldState);
}

// --- Print container ---
template <typename Container>
void printContainer(const std::string &label, const Container &data, const PrintOptions &options) {
  using Value = typename Container::value_type;
  const size_t containerSize = data.size();
  const size_t elementsToShow = options.maxElements > 0 ? std::min(static_cast<size_t>(options.maxElements), containerSize) : containerSize;

  printLabel(label);

  if (options.showSize) {
    std::cout << std::format("[{}] ", containerSize);
  }

  // Decide if we print hex
  bool shouldHex = options.showHex || std::is_integral_v<Value> || std::is_enum_v<Value>;

  if (shouldHex) {
    if (options.compactHex) {
      std::cout << "0x";
    } else {
      std::cout << "(0x) ";
    }

    size_t count = 0;
    for (const auto &element : data) {
      if (count++ >= elementsToShow) break;
      std::cout << std::format("{:02X}", static_cast<unsigned char>(element));
      if (!options.compactHex && count < elementsToShow) std::cout << " ";
    }
  }

  // Print ASCII if requested
  if (options.showAscii) {
    if (shouldHex) std::cout << " -> '";
    size_t count = 0;
    for (const auto &element : data) {
      if (count++ >= elementsToShow) break;
      auto c = static_cast<unsigned char>(element);
      std::cout << (std::isprint(c) ? static_cast<char>(c) : '.');
    }
  }

  if (elementsToShow < containerSize) {
    std::cout << "...";
  }

  if (options.showAscii && shouldHex) std::cout << "'";
  std::cout << std::endl;
}

// --- Core printing implementation ---
template <typename T>
void printDataImpl(const std::string &label, const T &data, const PrintOptions &options = {}) {
  if constexpr (is_container_v<T>) {
    printContainer(label, data, options);
  } else {
    printSingleValue(label, data, options);
  }
}

// --- Helper ---
constexpr bool isXLabel(const char *label, size_t len) {
  if (len == 0 || label[0] != 'x') return false;

  if (len == 5 || len == 9) return true;
  if (len == 10) return label[5] == '_';

  return false;
}

// convenience overload за std::string_view
constexpr bool isXLabel(std::string_view label) {
  return isXLabel(label.data(), label.size());
}

template <typename T>
void printData(const std::string &label, const T &data) {
  PrintOptions options;
  options.showHex = is_container_v<T> || std::is_unsigned_v<T>;

  if (isXLabel(label)) {
    options.compactHex = true;
    options.showAscii = false;
  } else if constexpr (is_container_v<T>) {
    using ValueType = typename T::value_type;
    options.showAscii = std::is_same_v<ValueType, quint8> || std::is_same_v<ValueType, char>;
  } else {
    options.showAscii = false;
  }

  printDataImpl(label, data, options);
}

template <typename Container>
void printCompact(const std::string &label, const Container &data, int maxElements = 0) {
  printDataImpl(label, data, PrintOptions{true, false, false, maxElements, true});
}

}  // namespace App
