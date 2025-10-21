#pragma once

struct Report {
  int numPt = 0;
  int numIp = 0;
  int numLn = 0;
  int numPg = 0;
  int numPtEx = 0;
  int numPoEx = 0;
  int numLnEx = 0;
  int numPgEx = 0;
  int numObjDecoded = 0;
  int numPtFailed = 0;
  int numPoFailed = 0;
  int numLnFailed = 0;
  int numPgFailed = 0;
  int numPtExFailed = 0;
  int numPoExFailed = 0;
  int numLnExFailed = 0;
  int numPgExFailed = 0;

  void printDecoded() const {
    //
  }
  void resetDecoded() {
    numPt = 0;
    numIp = 0;
    numLn = 0;
    numPg = 0;
    numPtEx = 0;
    numPoEx = 0;
    numLnEx = 0;
    numPgEx = 0;
  }

  void printFailed() const {
    //
  }
  void resetFailed() {
    numPtFailed = 0;
    numPoFailed = 0;
    numLnFailed = 0;
    numPgFailed = 0;
    numPtExFailed = 0;
    numPoExFailed = 0;
    numLnExFailed = 0;
    numPgExFailed = 0;
  }

  void objDecoded() { numObjDecoded = numPt + numIp + numLn + numPg + numPtEx + numPoEx + numLnEx + numPgEx; }
};
