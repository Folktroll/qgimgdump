#pragma once

#include <stdint.h>

#include <QByteArray>
#include <QString>
#include <QtGlobal>

#include "misc.h"

void printInt8(const char *label, qint8 val) {
  printf("%30s %i\n", label, val);
}

void printUInt8(const char *label, quint8 val) {
  printf("%30s %02X\n", label, val);
}

void printInt16(const char *label, qint16 val) {
  printf("%30s %i\n", label, val);
}

void printUInt16(const char *label, quint16 val) {
  printf("%30s %04X\n", label, val);
}

void printInt32(const char *label, qint32 val) {
  printf("%30s %i\n", label, val);
}

void printUInt32(const char *label, quint32 val) {
  printf("%30s %08X\n", label, val);
}

void printUInt24(const char *label, quint24 val) {
  qint32 tmp = val[0] | val[1] << 8 | val[2] << 16;
  printf("%30s %f (0x%06X, %i)\n", label, GRMN_DEG(tmp), tmp, tmp);
}

void printArrayUInt8(const char *label, quint8 *array, int size) {
  printf("%30s ", label);
  for (int i = 0; i < size; i++) {
    printf("%02X ", array[i]);
  }
  printf("\n");
}

void printArrayInt8(const char *label, char *array, int size) {
  printf("%30s ", label);
  for (int i = 0; i < size; i++) {
    printf("%02X ", array[i]);
  }
  printf("| %s\n", QString(QByteArray(array, size)).toUtf8().data());
}

void printArrayInt8(const char *label, qint8 *array, int size) {
  printf("%30s ", label);
  for (int i = 0; i < size; i++) {
    printf("%02X ", array[i]);
  }
  printf("\n");
}
