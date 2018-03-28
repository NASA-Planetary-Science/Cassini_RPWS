#ifndef CasHist_h
#define CasHist_h

#ifdef __cplusplus
extern "C" {
#endif

#include "pdsrdr.h"


#ifndef _Uchar 
#define _Uchar 
typedef unsigned char Uchar;
#endif

#ifndef _Ulong 
#define _Ulong 
typedef uint32_t Ulong;
#endif


#define CAS_HIST_MAX_BINS 2001  /* Maximum number bins in the histogram */



typedef struct cas_histogram_tag{
  uint32_t nItems,nNumRec;
  uint32_t (*arHist)[CAS_HIST_MAX_BINS];
  float *arBgnd;
}CasHistogram;


/* Okay Robert, why are you using globals for no good reason??? */
extern Ulong nBgdScetDaysBeg,nBgdScetMsecBeg;
extern Ulong nBgdScetDaysEnd,nBgdScetMsecEnd;
extern CasHistogram *pHstEu,*pHstEv,*pHstEx,*pHstEz;
extern CasHistogram *pHstBx,*pHstBy,*pHstBz;
extern CasHistogram *pHstLs,*pHstHf;



CasHistogram* CasPds_LRfull_LogBinHist(CasPdsFileList *pList,Ulong nIdx,int nPercent);
CasHistogram* CasPds_HfrJup_Hist(CasPdsFileList *pList,Ulong nIdxBeg,int nNoiseProfile);

void CasPds_Dump_LogBinHist(CasHistogram *pHgram);

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* CasHist_h */
