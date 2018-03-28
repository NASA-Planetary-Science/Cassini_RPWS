
#ifndef CasMiniPacket_h
#define CasMiniPacket_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <Cext.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasType.h>


#define GetEventTime(s,r)     ( (Ulong)( (((s)-(((r)>>3)&0x1FFF))&0xFFFFE000) | (((r)>>3)&0x1FFF) ) )


  Ulong CasMp_nHeaderLength (CasRecord * pRec);
  Ulong CasMp_nMode (CasRecord * pRec);
  char *CasMp_sReceiver (CasRecord * pRec);     /* simple string < 16 (8) char */
  char *CasMp_sStdHdr (CasRecord * pRec);

  void CasMp_Dump (CasRecord * pRec, FILE * h);
  Ulong CasMp_ParseDataBase (char *sDataBase, char *tBeg, char *tEnd,
                             char *sFiles[]);
  char *CasMp_DumpHeader (CasRecord * pRec, FILE * h);

#define CasMp_nId(x)   ( ((x[0]&0xF0)==0x00)?true:false )
#define CasMp_nRti(x)  ( (((Ulong)x[3])<<8)|(x[2]) )

#define CasMp_bStimPacket(x)  ( ((x[0]&0xF0)==0x00)?true:false )
#define CasMp_bMfrPacket(x)   ( ((x[0]&0xF0)==0x10)?true:false )
#define CasMp_bHfrPacket(x)   ( ((x[0]&0xF0)==0x20)?true:false )
#define CasMp_bLpPacket(x)    ( ((x[0]&0xF0)==0x40)?true:false )
#define CasMp_bWfdrPacket(x)  ( ((x[0]&0xF0)==0x70)?true:false )
#define CasMp_bWfrPacket(x)   ( ((x[0]&0xF0)==0x80)?true:false )
#define CasMp_bDustPacket(x)  ( ((x[0]&0xF0)==0xB0)?true:false )
#define CasMp_bMroPacket(x)   ( ((x[0]&0xF0)==0xD0)?true:false )
#define CasMp_bWbrPacket(x)   ( ((x[0]&0xF0)==0xE0)?true:false )
#define CasMp_bFillPacket(x)  ( ((x[0]&0xF0)==0xF0)?true:false )


#define CasWfr_bMoreShitFollows(x)  (x[6]&0x08?true:false)
#define CasWbr_bMoreShitFollows(x)  (x[6]&0x08?true:false)


#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasMiniPacket_h */
