#ifndef rajTime_h
#define rajTime_h

#ifdef __cplusplus
extern "C" {
#endif


#include <stdint.h>

typedef struct raj_time_tag{
  char sTime[128];
  int32_t nYear,nDoy,nMonth,nDom,nDow,nHour,nMinute,nSecond,nMsec;
  double dYear,dDoy,dMonth,dDom,dHour,dMinute,dSecond;
}rajTime;



int fxParseTime(char *string,rajTime *t);
void fxNormalize(rajTime *t);



int rajParseTime(char *string,rajTime *t);
int rajParseIsoTFormat(char *s,rajTime *t);
void rajNormalize(rajTime *t);



#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses the file. */
#endif

#endif  /* rajTime_h */
