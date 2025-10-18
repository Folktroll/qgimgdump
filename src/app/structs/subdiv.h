#pragma once

#include <QRectF>
#include <QtTypes>

#include "misc.h"
#include "subfile_sections.h"

#pragma pack(push, 1)
// IMG file header structure, to the start of the FAT blocks

namespace ImgHdr {

// subdivision  information
struct SSubDiv {
  quint32 n;
  quint16 next;       // section of next level
  bool terminate;     // end of section group
  quint32 rgn_start;  // offset into the submap's RGN part
  quint32 rgn_end;    // end of section in RGN part (last offset = rgn_end - 1)
  bool hasPoints;     // there are points stored in the RGN subsection
  bool hasPois;       // there are indexed points stored in the RGN subsection
  bool hasPolylines;  // there are polylines stored in the RGN subsection
  bool hasPolygons;   // there are polygons stored in the RGN subsection
  qint32 iCenterLng;  // the center longitude of the area covered by this subdivision
  qint32 iCenterLat;  // the center latitude of the area covered by this subdivision
  double north;       // north boundary of area covered by this subsection []
  double east;        // east boundary of area covered by this subsection []
  double south;       // south boundary of area covered by this subsection []
  double west;        // west boundary of area covered by this subsection []
  QRectF area;        // area in meter coordinates covered by this subdivision []
  quint32 shift;      // number of left shifts for RGN data
  quint32 level;      // map level this subdivision is shown
  quint32 offPolygons2;
  quint32 lenPolygons2;
  quint32 offPoints2;
  quint32 lenPoints2;
  quint32 offPolylines2;
  quint32 lenPolylines2;
  STre1 *maplevel;

  void print() const {
    if (next) {
      printf("--- subdiv #%i next #%i---\n", n, next);
    } else {
      printf("--- subdiv #%i ---\n", n);
    }
    printf("north %f east  %f south %f west  %f\n", qRadiansToDegrees(north), qRadiansToDegrees(east), qRadiansToDegrees(south), qRadiansToDegrees(west));
    printf("shift %i level %i\n", shift, level);
    printf("rgn_start %08X rgn_end %08X\n", rgn_start, rgn_end);
    printf("Terminate %i hasPoints %i hasPois %i hasPolylines %i hasPolygons %i\n", terminate, hasPoints, hasPois, hasPolylines, hasPolygons);
    printf("offsetPolygons2:  %08X  lengthPolygons2:  %08X\n", offPolygons2, lenPolygons2);
    printf("offsetPolylines2: %08X  lengthPolylines2: %08X\n", offPolylines2, lenPolylines2);
    printf("offsetPoints2:    %08X  lengthPoints2:    %08X\n", offPoints2, lenPoints2);
    printf("iCenterLng %f iCenterLat %f\n", GRMN_DEG(iCenterLng), GRMN_DEG(iCenterLat));
  }

  void printLite() const { printf("subdiv: %i | zoom: %i | pt: %i | poi: %i | ln: %i | pg: %i\n", n, level, hasPoints, hasPois, hasPolylines, hasPolygons); }
};

};  // namespace ImgHdr

#pragma pack(pop)