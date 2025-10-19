#include "img_headers.h"

#include "print.h"

using namespace ImgHdr;
using App::printData;

void SComm::printCommon() const {
  PRINT_HDR(size);
  PRINT_HDR(type);
  PRINT_HDR(x000C);
  PRINT_HDR(flag);
  PRINT_HDR(year);
  PRINT_HDR(month);
  PRINT_HDR(day);
  PRINT_HDR(hour);
  PRINT_HDR(minute);
  PRINT_HDR(second);
}

void SGmp::print() const {
  SComm::printCommon();

  PRINT_HDR(x0015_0018);
  PRINT_HDR(tre_offset);
  PRINT_HDR(rgn_offset);
  PRINT_HDR(lbl_offset);
  PRINT_HDR(net_offset);
  PRINT_HDR(nod_offset);
  PRINT_HDR(dem_offset);
  PRINT_HDR(mar_offset);
  PRINT_HDR(met_offset);
}

void STre::print(quint32 offset) const {
  SComm::printCommon();

  PRINT_HDR(northbound);
  PRINT_HDR(eastbound);
  PRINT_HDR(southbound);
  PRINT_HDR(westbound);
  PRINT_HDR(tre1_offset);
  PRINT_HDR(tre1_size);
  PRINT_HDR(tre2_offset);
  PRINT_HDR(tre2_size);
  PRINT_HDR(tre3_offset);
  PRINT_HDR(tre3_size);
  PRINT_HDR(tre3_rec_size);
  PRINT_HDR(x003B_003E);
  PRINT_HDR(POI_flags);
  PRINT_HDR(render_prio);
  PRINT_HDR(x0043_0049);
  PRINT_HDR(tre4_offset);
  PRINT_HDR(tre4_size);
  PRINT_HDR(tre4_rec_size);
  PRINT_HDR(x0054_0057);
  PRINT_HDR(tre5_offset);
  PRINT_HDR(tre5_size);
  PRINT_HDR(tre5_rec_size);
  PRINT_HDR(x0062_0065);
  PRINT_HDR(tre6_offset);
  PRINT_HDR(tre6_size);
  PRINT_HDR(tre6_rec_size);
  PRINT_HDR(x0070_0073);
  PRINT_HDR(map_id);
  PRINT_HDR(x0078_007B);
  PRINT_HDR(tre7_offset);
  PRINT_HDR(tre7_size);
  PRINT_HDR(tre7_rec_size);
  PRINT_HDR(x0086_0089);
  PRINT_HDR(tre8_offset);
  PRINT_HDR(tre8_size);
  PRINT_HDR(tre8_rec_size);
  PRINT_HDR(polyl2_types_num);
  PRINT_HDR(polyg2_types_num);
  PRINT_HDR(poi2_types_num);
  PRINT_HDR(key);
  PRINT_HDR(x00AA_00AD);
  PRINT_HDR(tre9_offset);
  PRINT_HDR(tre9_size);
  PRINT_HDR(tre9_rec_size);
  PRINT_HDR(x00B8_00BB);
  PRINT_HDR(tre10_offset);
  PRINT_HDR(tre10_size);
  PRINT_HDR(tre10_rec_size);
  PRINT_HDR(x00C6_00CE);
  PRINT_HDR(map_id2);
}

void SRgn::print(quint32 offset) const {
  SComm::printCommon();

  PRINT_HDR(rgn1_offset);
  PRINT_HDR(rgn1_length);
  PRINT_HDR(pg2_offset);
  PRINT_HDR(pg2_length);
  PRINT_HDR(x0025_0038);
  PRINT_HDR(ln2_offset);
  PRINT_HDR(ln2_length);
  PRINT_HDR(x0041_0054);
  PRINT_HDR(pt2_offset);
  PRINT_HDR(pt2_length);
  PRINT_HDR(x005D_0070);
  PRINT_HDR(rgn2_offset);
  PRINT_HDR(rgn2_length);
  PRINT_HDR(unknown);
}

void SLbl::print(quint32 offset) const {
  SComm::printCommon();

  PRINT_HDR(lbl1_offset);
  PRINT_HDR(lbl1_length);
  PRINT_HDR(addr_shift);
  PRINT_HDR(coding);

  PRINT_HDR(lbl2_offset);
  PRINT_HDR(lbl2_length);
  PRINT_HDR(lbl2_rec_size);
  PRINT_HDR(x0029_002C);

  PRINT_HDR(lbl3_offset);
  PRINT_HDR(lbl3_length);
  PRINT_HDR(lbl3_rec_size);
  PRINT_HDR(x0037_003A);

  PRINT_HDR(lbl4_offset);
  PRINT_HDR(lbl4_length);
  PRINT_HDR(lbl4_rec_size);
  PRINT_HDR(x0045_0048);

  PRINT_HDR(lbl5_offset);
  PRINT_HDR(lbl5_length);
  PRINT_HDR(lbl5_rec_size);
  PRINT_HDR(x0053_0056);

  PRINT_HDR(lbl6_offset);
  PRINT_HDR(lbl6_length);
  PRINT_HDR(lbl6_addr_shift);
  PRINT_HDR(lbl6_glob_mask);
  PRINT_HDR(x0061_0063);

  PRINT_HDR(lbl7_offset);
  PRINT_HDR(lbl7_length);
  PRINT_HDR(lbl7_rec_size);
  PRINT_HDR(x006E_0071);

  PRINT_HDR(lbl8_offset);
  PRINT_HDR(lbl8_length);
  PRINT_HDR(lbl8_rec_size);
  PRINT_HDR(x007C_007F);

  PRINT_HDR(lbl9_offset);
  PRINT_HDR(lbl9_length);
  PRINT_HDR(lbl9_rec_size);
  PRINT_HDR(x008A_008D);

  PRINT_HDR(lbl10_offset);
  PRINT_HDR(lbl10_length);
  PRINT_HDR(lbl10_rec_size);
  PRINT_HDR(x0098_009B);

  PRINT_HDR(lbl11_offset);
  PRINT_HDR(lbl11_length);
  PRINT_HDR(lbl11_rec_size);
  PRINT_HDR(x00A6_00A9);

  PRINT_HDR(codepage);
  PRINT_HDR(x00AC_00AF);

  PRINT_HDR(lbl12_offset);
  PRINT_HDR(lbl12_length);
}

void SNet::print(quint32 offset) const {
  SComm::printCommon();

  PRINT_HDR(net1_offset);
  PRINT_HDR(net1_length);
  PRINT_HDR(net1_addr_shift);
  PRINT_HDR(net2_offset);
  PRINT_HDR(net2_length);
  PRINT_HDR(net2_addr_shift);
  PRINT_HDR(net3_offset);
  PRINT_HDR(net3_length);
}

void SNod::print(quint32 offset) const {
  SComm::printCommon();

  PRINT_HDR(nod1_offset);
}

void SDem::print(quint32 offset) const {
  SComm::printCommon();

  PRINT_HDR(flags);
  PRINT_HDR(zoom_levels);
  PRINT_HDR(x001B_001D);
  PRINT_HDR(rec_size);
  PRINT_HDR(dem1_offset);
  PRINT_HDR(x0025_0029);
}
