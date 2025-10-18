#include "img_headers.h"

#include "print.h"

using namespace ImgHdr;
using App::printData;

void SComm::printCommon() const {
  printData("size", size);
  printData("type", type);
  printData("x000C", x000C);
  printData("flag", flag);
  printData("year", year);
  printData("month", month);
  printData("day", day);
  printData("hour", hour);
  printData("minute", minute);
  printData("second", second);
}

void SGmp::print() const {
  SComm::printCommon();

  printData("x0015_0018", x0015_0018);
  printData("tre_offset", tre_offset);
  printData("rgn_offset", rgn_offset);
  printData("lbl_offset", lbl_offset);
  printData("net_offset", net_offset);
  printData("nod_offset", nod_offset);
  printData("dem_offset", dem_offset);
  printData("mar_offset", mar_offset);
  printData("met_offset", met_offset);
}

void STre::print(quint32 offset) const {
  SComm::printCommon();

  printData("northbound", northbound);
  printData("eastbound", eastbound);
  printData("southbound", southbound);
  printData("westbound", westbound);
  printData("tre1_offset", tre1_offset + offset);
  printData("tre1_size", tre1_size);
  printData("tre2_offset", tre2_offset + offset);
  printData("tre2_size", tre2_size);
  printData("tre3_offset", tre3_offset + offset);
  printData("tre3_size", tre3_size);
  printData("tre3_rec_size", tre3_rec_size);
  printData("x003B_003E", x003B_003E);
  printData("POI_flags", POI_flags);
  printData("render_prio", render_prio);
  printData("x0043_0049", x0043_0049);
  printData("tre4_offset", tre4_offset + offset);
  printData("tre4_size", tre4_size);
  printData("tre4_rec_size", tre4_rec_size);
  printData("x0054_0057", x0054_0057);
  printData("tre5_offset", tre5_offset + offset);
  printData("tre5_size", tre5_size);
  printData("tre5_rec_size", tre5_rec_size);
  printData("x0062_0065", x0062_0065);
  printData("tre6_offset", tre6_offset + offset);
  printData("tre6_size", tre6_size);
  printData("tre6_rec_size", tre6_rec_size);
  printData("x0070_0073", x0070_0073);
  printData("map_id", map_id);
  printData("x0078_007B", x0078_007B);
  printData("tre7_offset", tre7_offset + offset);
  printData("tre7_size", tre7_size);
  printData("tre7_rec_size", tre7_rec_size);
  printData("x0086_0089", x0086_0089);
  printData("tre8_offset", tre8_offset + offset);
  printData("tre8_size", tre8_size);
  printData("tre8_rec_size", tre8_rec_size);
  printData("polyl2_types_num", polyl2_types_num);
  printData("polyg2_types_num", polyg2_types_num);
  printData("poi2_types_num", poi2_types_num);
  printData("key", key);
  printData("x00AA_00AD", x00AA_00AD);
  printData("tre9_offset", tre9_offset + offset);
  printData("tre9_size", tre9_size);
  printData("tre9_rec_size", tre9_rec_size);
  printData("x00B8_00BB", x00B8_00BB);
  printData("tre10_offset", tre9_offset + offset);
  printData("tre10_size", tre9_size);
  printData("tre10_rec_size", tre9_rec_size);
  printData("x00C6_00CE", x00C6_00CE);
  printData("map_id2", map_id2);
}

void SRgn::print(quint32 offset) const {
  SComm::printCommon();

  printData("rgn1_offset", rgn1_length ? rgn1_offset + offset : 0);
  printData("rgn1_length", rgn1_length);
  printData("pg2_offset", pg2_offset + offset);
  printData("pg2_length", pg2_length);
  printData("x0025_0038", x0025_0038);
  printData("ln2_offset", ln2_offset + offset);
  printData("ln2_length", ln2_length);
  printData("x0041_0054", x0041_0054);
  printData("pt2_offset", pt2_offset + offset);
  printData("pt2_length", pt2_length);
  printData("x005D_0070", x005D_0070);
  printData("rgn2_offset", rgn2_offset + offset);
  printData("rgn2_length", rgn2_length);
  printData("unknown", unknown);
}

void SLbl::print(quint32 offset) const {
  SComm::printCommon();

  printData("lbl1_offset", lbl1_offset + offset);
  printData("lbl1_length", lbl1_length);
  printData("addr_shift", addr_shift);
  printData("coding", coding);

  printData("lbl2_offset", lbl2_offset + offset);
  printData("lbl2_length", lbl2_length);
  printData("lbl2_rec_size", lbl2_rec_size);
  printData("x0029_002C", x0029_002C);

  printData("lbl3_offset", lbl3_offset + offset);
  printData("lbl3_length", lbl3_length);
  printData("lbl3_rec_size", lbl3_rec_size);
  printData("x0037_003A", x0037_003A);

  printData("lbl4_offset", lbl4_offset + offset);
  printData("lbl4_length", lbl4_length);
  printData("lbl4_rec_size", lbl4_rec_size);
  printData("x0045_0048", x0045_0048);

  printData("lbl5_offset", lbl5_offset + offset);
  printData("lbl5_length", lbl5_length);
  printData("lbl5_rec_size", lbl5_rec_size);
  printData("x0053_0056", x0053_0056);

  printData("lbl6_offset", lbl6_offset + offset);
  printData("lbl6_length", lbl6_length);
  printData("lbl6_addr_shift", lbl6_addr_shift);
  printData("lbl6_glob_mask", lbl6_glob_mask);
  printData("x0061_0063", x0061_0063);

  printData("lbl7_offset", lbl7_offset + offset);
  printData("lbl7_length", lbl7_length);
  printData("lbl7_rec_size", lbl7_rec_size);
  printData("x006E_0071", x006E_0071);

  printData("lbl8_offset", lbl8_offset + offset);
  printData("lbl8_length", lbl8_length);
  printData("lbl8_rec_size", lbl8_rec_size);
  printData("x007C_007F", x007C_007F);

  printData("lbl9_offset", lbl9_offset + offset);
  printData("lbl9_length", lbl9_length);
  printData("lbl9_rec_size", lbl9_rec_size);
  printData("x008A_008D", x008A_008D);

  printData("lbl10_offset", lbl10_offset + offset);
  printData("lbl10_length", lbl10_length);
  printData("lbl10_rec_size", lbl10_rec_size);
  printData("x0098_009B", x0098_009B);

  printData("lbl11_offset", lbl11_offset + offset);
  printData("lbl11_length", lbl11_length);
  printData("lbl11_rec_size", lbl11_rec_size);
  printData("x00A6_00A9", x00A6_00A9);

  printData("codepage", codepage);
  printData("x00AC_00AF", x00AC_00AF);

  printData("lbl12_offset", lbl12_offset + offset);
  printData("lbl12_length", lbl12_length);
}

void SNet::print(quint32 offset) const {
  SComm::printCommon();

  printData("net1_offset", net1_offset + offset);
  printData("net1_length", net1_length);
  printData("net1_addr_shift", net1_addr_shift);
  printData("net2_offset", net2_offset + offset);
  printData("net2_length", net2_length);
  printData("net2_addr_shift", net2_addr_shift);
  printData("net3_offset", net3_offset + offset);
  printData("net3_length", net3_length);
}

void SNod::print(quint32 offset) const {
  SComm::printCommon();

  printData("nod1_offset", nod1_offset + offset);
}

void SDem::print(quint32 offset) const {
  SComm::printCommon();

  printData("flags", flags);
  printData("zoom_levels", zoom_levels);
  printData("x001B_001D", x001B_001D);
  printData("rec_size", rec_size);
  printData("dem1_offset", dem1_offset + offset);
  printData("x0025_0029", x0025_0029);
}
