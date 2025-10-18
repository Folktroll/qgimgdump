#pragma once

#include <QRectF>
#include <QtTypes>

#include "global.h"

#pragma pack(push, 1)
// IMG file header structure, to the start of the FAT blocks

namespace ImgHdr {

// copyright header
struct STre0 {
  QString descr1;
  QString descr2;
};

// map level definition
struct STre1 {
  quint8 raw_zoom;  // 7=inherit, 6-0=zoom
  quint8 bits;
  quint16 subdiv;
  STre1() : raw_zoom{0}, bits{0}, subdiv{0} {}
  bool inherit() const { return raw_zoom & 0x80; }
  quint8 zoom() const { return raw_zoom & 0x7F; }
  void print() { printf("zoom: %02X | inherit: %i | bits: %i | subdiv: %i\n", zoom(), inherit(), bits, subdiv); }
};

// map subdivision definition, without pointer to the lower level subparts
struct STre2 {
  quint24 rgn_offset;  // offset: 27-0 bit, 31-26 elements
  quint8 elements;
  quint24 center_lng;
  quint24 center_lat;
  quint16 width_trm;
#define TRE_SUBDIV_WIDTH(r) (r->width_trm & 0x7FFF)
#define TRE_SUBDIV_TERM(r) ((r->width_trm & 0x8000) != 0)
  quint16 height;
};

// pointer to the lower level subparts
struct STre2Next : public STre2 {
  quint16 next;
};

struct STre7 {
  quint32 offsetPolygons;
  quint32 offsetPolyline;
  quint32 offsetPoints;
  quint8 btObjects;
};

};  // namespace ImgHdr

#pragma pack(pop)