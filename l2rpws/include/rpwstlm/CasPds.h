
#ifndef CasPds_h
#define CasPds_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <Cext.h>

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */


#define CAS_PDS_MAX_FILE_SIZE (64*1024*1024)    /* bytes */
#define CAS_PDS_MAX_RECORD_ITEMS (64*1024)


  int CasPds_LRfull_GetFiles (char *sFiles[], char *sBeg, char *sEnd,
                              char *sDir, Ulong nLfdr, Ulong nMfdr,
                              Ulong nMfr, Ulong nHfr, Ulong nMsc);
  Uchar *CasPds_LRfull_ReadArchive (char *sFilename, Uchar * pRec);



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */



#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasPds_h */
