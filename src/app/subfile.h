
#pragma once
#include <QtGlobal>

namespace App {

// submap subFiles location info
struct SSubFile {
  quint32 offset = 0;      // file offset of submap part (old format)
  quint32 size = 0;        // size of the submap part (old format)
  quint32 hdrOffset = 0;   // file offset of header part (new format)
  quint32 hdrSize = 0;     // size of the header part (new format)
  quint32 bodyOffset = 0;  // file offset of body part (new format)
  quint32 bodySize = 0;    // size of the body part (new format)
  // SSubFile() : offset(0), size(0), hdrOffset(0), hdrSize(0), bodyOffset(0), bodySize(0) {}
};

}  // namespace App
