#pragma once

#include <QRectF>
#include <QtTypes>

#include "hdr/section.h"
#include "misc.h"

namespace ImgHdr {

#pragma pack(push, 1)

// subdivision  information
struct SSubDiv {
  quint32 index;
  quint16 next;       // section of next level
  bool terminate;     // end of section group
  quint32 rgnStart;   // offset into the submap's RGN part
  quint32 rgnEnd;     // end of section in RGN part (lastOffset = rgnEnd - 1)
  bool hasPt;         // there are points stored in the RGN subsection
  bool hasIp;         // there are indexed points stored in the RGN subsection
  bool hasLn;         // there are polylines stored in the RGN subsection
  bool hasPg;         // there are polygons stored in the RGN subsection
  qint32 iCenterLng;  // the center longitude of the area covered by this subdivision
  qint32 iCenterLat;  // the center latitude of the area covered by this subdivision
  double north;       // north boundary of area covered by this subsection []
  double east;        // east boundary of area covered by this subsection []
  double south;       // south boundary of area covered by this subsection []
  double west;        // west boundary of area covered by this subsection []
  QRectF area;        // area in meter coordinates covered by this subdivision []
  quint32 shift;      // number of left shifts for RGN data
  quint32 level;      // map level this subdivision is shown
  quint32 offPgEx;
  quint32 lenPgEx;
  quint32 offPtEx;
  quint32 lenPtEx;
  quint32 offLnEx;
  quint32 lenLnEx;
  STre1 *mapLevel;

  void print() const {
    if (next) {
      misc::Logger::printf("--- subdiv #%i next #%i---\n", index, next);
    } else {
      misc::Logger::printf("--- subdiv #%i ---\n", index);
    }
    misc::Logger::printf("north %f east  %f south %f west  %f\n", qRadiansToDegrees(north), qRadiansToDegrees(east), qRadiansToDegrees(south), qRadiansToDegrees(west));
    misc::Logger::printf("shift %i level %i\n", shift, level);
    misc::Logger::printf("rgn_start %08X rgn_end %08X\n", rgnStart, rgnEnd);
    misc::Logger::printf("Terminate %i hasPt %i hasIp %i hasLn %i hasPg %i\n", terminate, hasPt, hasIp, hasLn, hasPg);
    misc::Logger::printf("offsetPgEx:  %08X  lengthPgEx:  %08X\n", offPgEx, lenPgEx);
    misc::Logger::printf("offsetLnEx: %08X  lengthLnEx: %08X\n", offLnEx, lenLnEx);
    misc::Logger::printf("offsetPtEx:    %08X  lengthPtEx:    %08X\n", offPtEx, lenPtEx);
    misc::Logger::printf("iCenterLng %f iCenterLat %f\n", GRMN_DEG(iCenterLng), GRMN_DEG(iCenterLat));
  }

  void printLite() const { misc::Logger::printf("subdiv: %i | zoom: %i | pt: %i | poi: %i | ln: %i | pg: %i\n", index, level, hasPt, hasIp, hasLn, hasPg); }
};

#pragma pack(pop)

};  // namespace ImgHdr