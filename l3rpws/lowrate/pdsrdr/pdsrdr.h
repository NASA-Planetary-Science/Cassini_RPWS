#ifndef PdsRdr_h
#define PdsRdr_h

#ifdef __cplusplus
extern "C" {
#endif


#include <Cext.h>



#define DAS_B0_PACKET_MAX_SIZE    0x00010000  /* 64KB buffer */


/* assumed a pointer to an unsigned char */
#define CasPds_nRecLen(x)     ( *((Ulong*)(pByte+ 8)) )
#define CasPds_nNumRec(x)     ( *((Ulong*)(pByte+12)) )
#define CasPds_nType(x)       ( *((Ulong*)(pByte+72)) )
#define CasPds_nMode(x)       ( *((Ulong*)(pByte+76)) )

extern Uchar arPdsFileBuf[]; /* CAS_PDS_MAX_FILE_SIZE=64MB buffer */
extern bool bVerbose;

#define PDS_FILE_LIST_MAX  16384 
typedef struct caspdsfilelist_tag{
  char *sList[PDS_FILE_LIST_MAX];
  Uchar nMpHdr[PDS_FILE_LIST_MAX][32];
  Ulong nType[PDS_FILE_LIST_MAX],nRecLen[PDS_FILE_LIST_MAX];
  Ulong nId[PDS_FILE_LIST_MAX],nMax;
}CasPdsFileList;



#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* PdsRdr_h */
