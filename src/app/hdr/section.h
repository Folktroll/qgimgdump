#pragma once

#include <QRectF>
#include <QtTypes>

#include "global.h"

namespace ImgHdr {

#pragma pack(push, 1)

// copyright header
struct STre0 {
  QString descr1;
  QString descr2;
};

// map level definition
struct STre1 {
  quint8 rawZoom = 0;  // 7=inherit, 6-0=zoom
  quint8 bits = 0;
  quint16 subDiv = 0;
  bool inherit() const { return rawZoom & 0x80; }
  quint8 zoom() const { return rawZoom & 0x7F; }
  void print() { misc::Logger::printf("\tzoom: %02X | inherit: %i | bits: %i | subdiv: %i\n", zoom(), inherit(), bits, subDiv); }
};

// map subdivision definition, without pointer to the lower level subparts
struct STre2 {
  quint24 rgn_offset;  // offset: 27-0 bit, 31-26 elements
  quint8 elements;
  quint24 centerLng;
  quint24 centerLat;
  quint16 widthTrm;
#define TRE_SUBDIV_WIDTH(r) (r->widthTrm & 0x7FFF)
#define TRE_SUBDIV_TERM(r) ((r->widthTrm & 0x8000) != 0)
  quint16 height;
};

// pointer to the lower level subparts
struct STre2Next : public STre2 {
  quint16 next;
};

struct STre7 {
  quint32 offsetPg;
  quint32 offsetLn;
  quint32 offsetPt;
  quint8 btObjects;
};

#pragma pack(pop)

};  // namespace ImgHdr