
#pragma once

#include <QtTypes>

struct Warnings {
  quint32 infoSkipDupePoint = 0;
  quint32 warnTotals = 0;
  quint32 warnSkipOutside = 0;
  quint32 warnSuspiciousSegment = 0;
  quint32 warnLnOversize = 0;
  quint32 warnInvalidType = 0;
  quint32 warnInvalidCoord = 0;
  quint32 warnTiny = 0;
  quint32 warnSmallArea = 0;

  void report() const {
    misc::Logger::printf("=== Warnings Report ===\n");
    misc::Logger::printf("Info skip dupe point: %u\n", infoSkipDupePoint);
    misc::Logger::printf("Warn totals: %u\n", warnTotals);
    misc::Logger::printf("Warn skip outside: %u\n", warnSkipOutside);
    misc::Logger::printf("Warn suspicious segment: %u\n", warnSuspiciousSegment);
    misc::Logger::printf("Warn poly oversize: %u\n", warnLnOversize);
    misc::Logger::printf("Warn invalid type: %u\n", warnInvalidType);
    misc::Logger::printf("Warn invalid coord: %u\n", warnInvalidCoord);
    misc::Logger::printf("Warn tiny: %u\n", warnTiny);
    misc::Logger::printf("Warn small area: %u\n", warnSmallArea);
  }

  void reset() {
    infoSkipDupePoint = 0;
    warnTotals = 0;
    warnSkipOutside = 0;
    warnSuspiciousSegment = 0;
    warnLnOversize = 0;
    warnInvalidType = 0;
    warnInvalidCoord = 0;
    warnTiny = 0;
    warnSmallArea = 0;
  }
};