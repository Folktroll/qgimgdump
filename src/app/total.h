#pragma once

struct Total {
  int totalPt = 0;
  int totalPo = 0;
  int totalLn = 0;
  int totalPg = 0;
  int totalPt2 = 0;
  int totalPo2 = 0;
  int totalLn2 = 0;
  int totalPg2 = 0;
  int totalObjectsDecoded = 0;
  int totalPtFailed = 0;
  int totalPoFailed = 0;
  int totalLnFailed = 0;
  int totalPgFailed = 0;
  int totalPt2Failed = 0;
  int totalPo2Failed = 0;
  int totalLn2Failed = 0;
  int totalPg2Failed = 0;

  void resetDecoded() {
    totalPt = 0;
    totalPo = 0;
    totalLn = 0;
    totalPg = 0;
    totalPt2 = 0;
    totalPo2 = 0;
    totalLn2 = 0;
    totalPg2 = 0;
    // totalObjectsDecoded = 0;
    // totalPtFailed = 0;
    // totalPoFailed = 0;
    // totalLnFailed = 0;
    // totalPgFailed = 0;
    // totalPt2Failed = 0;
    // totalPo2Failed = 0;
    // totalLn2Failed = 0;
    // totalPg2Failed = 0;
  }
  void objectsDecoded() { totalObjectsDecoded = totalPt + totalPo + totalLn + totalPg + totalPt2 + totalPo2 + totalLn2 + totalPg2; }
};
