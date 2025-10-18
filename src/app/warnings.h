
#pragma once

#include <QtTypes>

struct Warnings {
  quint32 infoSkipDupePoint = 0;
  quint32 warnTotals = 0;
  quint32 warnSkipOutside = 0;
  quint32 warnSuspiciousSegment = 0;
  quint32 warnPolyOversize = 0;
  quint32 warnInvalidType = 0;
  quint32 warnInvalidCoords = 0;

  void report() const {
    printf("=== Warnings Report ===\n");
    printf("Info skip dupe point: %u\n", infoSkipDupePoint);
    printf("Warn totals: %u\n", warnTotals);
    printf("Warn skip outside: %u\n", warnSkipOutside);
    printf("Warn suspicious segment: %u\n", warnSuspiciousSegment);
    printf("Warn poly oversize: %u\n", warnPolyOversize);
    printf("Warn invalid type: %u\n", warnInvalidType);
  }

  void reset() {
    infoSkipDupePoint = 0;
    warnTotals = 0;
    warnSkipOutside = 0;
    warnSuspiciousSegment = 0;
    warnPolyOversize = 0;
    warnInvalidType = 0;
    warnInvalidCoords = 0;
  }
};