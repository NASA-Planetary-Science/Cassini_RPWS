
#ifndef CasCmdParse_h
#define CasCmdParse_h

#ifdef __cplusplus
extern "C"
{
#endif



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
  typedef struct das_time_tag
  {
    double dSecond, dDays;
    int nYear, nMonth, nDayOfMonth, nDayOfYear, nHour, nMinute;
    Ulong nSclk, nFine, nDays, nMsec;
    char sScet[32], sSclk[32];
  } DasTime;



  void CasCmd_ParseTime (char *sTime, DasTime * p);
  Ulong CasCmd_RcvStrToMode (const char *sRcv, char *sArg);

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/



#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasCmdParse_h */
