#include "imgdump.h"
#include "print.h"

void ImgDump::gmapsupp_imghdr_t::print() {
  printUInt8("xorByte", xorByte);
  printArrayUInt8("x0001_0007", x0001_0007, sizeof(x0001_0007));
  printUInt16("version", version);
  printInt8("upMonth", upMonth);
  printInt16("upYear", upYear + 1900);
  printArrayUInt8("x000C_000D", x000C_000D, sizeof(x000C_000D));
  printUInt8("supp", supp);
  printUInt8("checksum", checksum);
  printArrayInt8("signature", signature, sizeof(signature));
  printUInt8("x0017", x0017);
  printUInt16("sectors1", sectors1);
  printUInt16("heads1", heads1);
  printUInt16("cylinders", cylinders);
  printArrayUInt8("x001E_00038", x001E_0038, sizeof(x001E_0038));
  printInt16("year", year);
  printInt8("month", month);
  printInt8("day", day);
  printInt8("hour", hour);
  printInt8("minute", minute);
  printInt8("second", second);
  printInt8("offsetFAT", offsetFAT);
  printArrayInt8("identifier", identifier, sizeof(identifier));
  printUInt8("x0048", x0048);
  printArrayInt8("desc1", desc1, sizeof(desc1));
  printUInt16("head2", head2);
  printUInt16("sectors2", sectors2);
  printUInt8("e1", e1);
  printUInt8("e2", e2);
  printUInt16("nBlocks1", nBlocks1);
  printArrayInt8("desc2", desc2, sizeof(desc2));
  printArrayUInt8("x0083_01BE", x0083_01BE, sizeof(x0083_01BE));
  printUInt8("startHead", startHead);
  printUInt8("startSector", startSector);
  printUInt8("startCylinder", startCylinder);
  printUInt8("systemType", systemType);
  printUInt8("endHead", endHead);
  printUInt8("endSector", endSector);
  printUInt8("endCylinder", endCylinder);
  printUInt32("relSectors", relSectors);
  printUInt32("nSectors", nSectors);
  printArrayUInt8("x01CE_01FD", x01CE_01FD, sizeof(x01CE_01FD));
  printUInt16("terminator", terminator);
}

void ImgDump::submap_hdr_t::print() {
  printUInt16("size", size);
  printArrayInt8("type", type, sizeof(type));
  printUInt8("x000C", x000C);
  printUInt8("flag", flag);
  printInt16("year", year);
  printInt8("month", month);
  printInt8("day", day);
  printInt8("hour", hour);
  printInt8("minute", minute);
  printInt8("second", second);
}

void ImgDump::gmp_hdr_t::print() {
  submap_hdr_t::print();

  printArrayUInt8("x0015_0018", x0015_0018, sizeof(x0015_0018));
  printUInt32("tre_offset", tre_offset);
  printUInt32("rgn_offset", rgn_offset);
  printUInt32("lbl_offset", lbl_offset);
  printUInt32("net_offset", net_offset);
  printUInt32("nod_offset", nod_offset);
  printUInt32("dem_offset", dem_offset);
  printUInt32("mar_offset", mar_offset);
  printUInt32("met_offset", met_offset);
}

void ImgDump::hdr_tre_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt24("northbound", northbound);
  printUInt24("eastbound", eastbound);
  printUInt24("southbound", southbound);
  printUInt24("westbound", westbound);
  printUInt32("tre1_offset", tre1_offset + offset);
  printUInt32("tre1_size", tre1_size);
  printUInt32("tre2_offset", tre2_offset + offset);
  printUInt32("tre2_size", tre2_size);
  printUInt32("tre3_offset", tre3_offset + offset);
  printUInt32("tre3_size", tre3_size);
  printUInt16("tre3_rec_size", tre3_rec_size);
  printArrayUInt8("x003B_003E", x003B_003E, sizeof(x003B_003E));
  printUInt8("POI_flags", POI_flags);
  printArrayUInt8("render_prio", render_prio, sizeof(render_prio));
  printArrayUInt8("x0043_0049", x0043_0049, sizeof(x0043_0049));
  printUInt32("tre4_offset", tre4_offset + offset);
  printUInt32("tre4_size", tre4_size);
  printUInt16("tre4_rec_size", tre4_rec_size);
  printArrayUInt8("x0054_0057", x0054_0057, sizeof(x0054_0057));
  printUInt32("tre5_offset", tre5_offset + offset);
  printUInt32("tre5_size", tre5_size);
  printUInt16("tre5_rec_size", tre5_rec_size);
  printArrayUInt8("x0062_0065", x0062_0065, sizeof(x0062_0065));
  printUInt32("tre6_offset", tre6_offset + offset);
  printUInt32("tre6_size", tre6_size);
  printUInt16("tre6_rec_size", tre6_rec_size);
  printArrayUInt8("x0070_0073", x0070_0073, sizeof(x0070_0073));
  printUInt32("map_id", map_id);
  printArrayUInt8("x0078_007B", x0078_007B, sizeof(x0078_007B));
  printUInt32("tre7_offset", tre7_offset + offset);
  printUInt32("tre7_size", tre7_size);
  printUInt16("tre7_rec_size", tre7_rec_size);
  printArrayUInt8("x0086_0089", x0086_0089, sizeof(x0086_0089));
  printUInt32("tre8_offset", tre8_offset + offset);
  printUInt32("tre8_size", tre8_size);
  printUInt16("tre8_rec_size", tre8_rec_size);
  printUInt16("polyl2_types_num", polyl2_types_num);
  printUInt16("polyg2_types_num", polyg2_types_num);
  printUInt16("poi2_types_num", poi2_types_num);
  printArrayUInt8("key", key, sizeof(key));
  printArrayUInt8("x00AA_00AD", x00AA_00AD, sizeof(x00AA_00AD));
  printUInt32("tre9_offset", tre9_offset + offset);
  printUInt32("tre9_size", tre9_size);
  printUInt16("tre9_rec_size", tre9_rec_size);
  printArrayUInt8("x00B8_00BB", x00B8_00BB, sizeof(x00B8_00BB));
  printUInt32("tre10_offset", tre9_offset + offset);
  printUInt32("tre10_size", tre9_size);
  printUInt16("tre10_rec_size", tre9_rec_size);
  printArrayUInt8("x00C6_00CE", x00C6_00CE, sizeof(x00C6_00CE));
  printUInt32("map_id2", map_id2);
}

void ImgDump::hdr_rgn_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("rgn1_offset", rgn1_length ? rgn1_offset + offset : 0);
  printUInt32("rgn1_length", rgn1_length);
  printUInt32("pg2_offset", pg2_offset + offset);
  printUInt32("pg2_length", pg2_length);
  printArrayUInt8("x0025_0038", x0025_0038, sizeof(x0025_0038));
  printUInt32("ln2_offset", ln2_offset + offset);
  printUInt32("ln2_length", ln2_length);
  printArrayUInt8("x0041_0054", x0041_0054, sizeof(x0041_0054));
  printUInt32("pt2_offset", pt2_offset + offset);
  printUInt32("pt2_length", pt2_length);
  printArrayUInt8("x005D_0070", x005D_0070, sizeof(x005D_0070));
  printUInt32("rgn2_offset", rgn2_offset + offset);
  printUInt32("rgn2_length", rgn2_length);
  printUInt32("unknown", unknown);
}

void ImgDump::hdr_lbl_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("lbl1_offset", lbl1_offset + offset);
  printUInt32("lbl1_length", lbl1_length);
  printUInt8("addr_shift", addr_shift);
  printUInt8("coding", coding);

  printUInt32("lbl2_offset", lbl2_offset + offset);
  printUInt32("lbl2_length", lbl2_length);
  printUInt16("lbl2_rec_size", lbl2_rec_size);
  printArrayUInt8("x0029_002C", x0029_002C, sizeof(x0029_002C));

  printUInt32("lbl3_offset", lbl3_offset + offset);
  printUInt32("lbl3_length", lbl3_length);
  printUInt16("lbl3_rec_size", lbl3_rec_size);
  printArrayUInt8("x0037_003A", x0037_003A, sizeof(x0037_003A));

  printUInt32("lbl4_offset", lbl4_offset + offset);
  printUInt32("lbl4_length", lbl4_length);
  printUInt16("lbl4_rec_size", lbl4_rec_size);
  printArrayUInt8("x0045_0048", x0045_0048, sizeof(x0045_0048));

  printUInt32("lbl5_offset", lbl5_offset + offset);
  printUInt32("lbl5_length", lbl5_length);
  printUInt16("lbl5_rec_size", lbl5_rec_size);
  printArrayUInt8("x0053_0056", x0053_0056, sizeof(x0053_0056));

  printUInt32("lbl6_offset", lbl6_offset + offset);
  printUInt32("lbl6_length", lbl6_length);
  printUInt8("lbl6_addr_shift", lbl6_addr_shift);
  printUInt8("lbl6_glob_mask", lbl6_glob_mask);
  printArrayUInt8("x0061_0063", x0061_0063, sizeof(x0061_0063));

  printUInt32("lbl7_offset", lbl7_offset + offset);
  printUInt32("lbl7_length", lbl7_length);
  printUInt16("lbl7_rec_size", lbl7_rec_size);
  printArrayUInt8("x006E_0071", x006E_0071, sizeof(x006E_0071));

  printUInt32("lbl8_offset", lbl8_offset + offset);
  printUInt32("lbl8_length", lbl8_length);
  printUInt16("lbl8_rec_size", lbl8_rec_size);
  printArrayUInt8("x007C_007F", x007C_007F, sizeof(x007C_007F));

  printUInt32("lbl9_offset", lbl9_offset + offset);
  printUInt32("lbl9_length", lbl9_length);
  printUInt16("lbl9_rec_size", lbl9_rec_size);
  printArrayUInt8("x008A_008D", x008A_008D, sizeof(x008A_008D));

  printUInt32("lbl10_offset", lbl10_offset + offset);
  printUInt32("lbl10_length", lbl10_length);
  printUInt16("lbl10_rec_size", lbl10_rec_size);
  printArrayUInt8("x0098_009B", x0098_009B, sizeof(x0098_009B));

  printUInt32("lbl11_offset", lbl11_offset + offset);
  printUInt32("lbl11_length", lbl11_length);
  printUInt16("lbl11_rec_size", lbl11_rec_size);
  printArrayUInt8("x00A6_00A9", x00A6_00A9, sizeof(x00A6_00A9));

  printUInt16("codepage", codepage);
  printArrayUInt8("x00AC_00AF", x00AC_00AF, sizeof(x00AC_00AF));

  printUInt32("lbl12_offset", lbl12_offset + offset);
  printUInt32("lbl12_length", lbl12_length);
}

void ImgDump::hdr_net_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("net1_offset", net1_offset + offset);
  printUInt32("net1_length", net1_length);
  printUInt8("net1_addr_shift", net1_addr_shift);
  printUInt32("net2_offset", net2_offset + offset);
  printUInt32("net2_length", net2_length);
  printUInt8("net2_addr_shift", net2_addr_shift);
  printUInt32("net3_offset", net3_offset + offset);
  printUInt32("net3_length", net3_length);
}

void ImgDump::hdr_nod_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("nod1_offset", nod1_offset + offset);
}

void ImgDump::hdr_dem_t::print(quint32 offset) {
  submap_hdr_t::print();

  printUInt32("flags", flags);
  printUInt16("zoom_levels", zoom_levels);
  printArrayUInt8("x001B_001D", x001B_001D, sizeof(x001B_001D));
  printUInt16("rec_size", rec_size);
  printUInt32("dem1_offset", dem1_offset + offset);
  printArrayUInt8("x0025_0029", x0025_0029, sizeof(x0025_0029));
}

void ImgDump::subdiv_t::print() const {
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

void ImgDump::subdiv_t::printLite() const {
  printf("subdiv: %i | zoom: %i | pt: %i | poi: %i | ln: %i | pg: %i\n", n, level, hasPoints, hasPois, hasPolylines, hasPolygons);
}

void ImgDump::print(const char *format, ...) {
  QMutexLocker lock(&mutex);
  va_list args;
  va_start(args, format);
  vfprintf(stdout, format, args);
  va_end(args);

  fflush(stdout);
}
