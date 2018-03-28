/*	#include "Cext.h"		*/
/*	#include "CasTlmFrontEnd.h"	*/

#ifndef CasDas_h
#define CasDas_h

#ifdef __cplusplus
extern "C" {
#endif


#define CAS_DAS_MAX_PKT_SIZE   32768  /* 32K Bytes */ 

#define CasDasIsB0(p)  ( p->pData[0]==':' && p->pData[1]=='b' &&  \
                         p->pData[2]=='0' && p->pData[3]==':' ) 
#define CasDasIsH0(p)  ( p->pData[0]==':' && p->pData[1]=='h' &&  \
                         p->pData[2]=='0' && p->pData[3]==':' ) 

typedef union cas_das_float2char_tag{
char arChar[4];
float fFloat;   /* Four Bytes as Well */
}CasDasFloat2Char;



typedef struct cas_das_packet_tag{
  ULONG nLength;  
  UCHAR *pData;
  int nFileDes;
  size_t nRdWrSize;
}CasDasPacket;

/*
void CasDasPacket_Dump(CasDasPacket *pObj,FILE *hHandle);

Bool CasDasPacket_Read(CasDasPacket *pObj, FILE *hHandle);
Bool CasDasPacket_Write(CasDasPacket *pObj);

Bool CasDasPacket_FormatB0(CasDasPacket *pObj,float *pX,float *pY,float *pZ,ULONG nLength);
Bool CasDasPacket_FormatH0(CasDasPacket *pObj,CasAnySci *pSci);


Bool CasDasPacket_FormatOldPacket(CasDasPacket *pObj,float fX,float *pZ,ULONG nLength);
*/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* CasDas_h */
