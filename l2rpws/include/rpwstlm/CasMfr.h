
#ifndef CasMfr_h
#define CasMfr_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <Cext.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasType.h>

/* mfr packet header was redefined with the flight software upload, ver2.4
     on June 1, 2000: 2000-153T08:06:32 (0x4FC87A44). */

#define CasMfr_bMfrPacket(x)      ( (x[0]&0xF0)==0x10?true:false )
#define CasMfr_nLength(x)        ( ((x[0]&0x00F)<<8)|(x[1]&0x0FF) )
#define CasMfr_bCompressed(x)     ( (x[4]&0x01)?true:false )
#define CasMfr_nAntenna(x)        ( (x[4]>>1)&0x03 )
#define CasMfr_bNormal(x)         ( (x[4]&0x08)?false:true )
#define CasMfr_bFastToggle(x)     ( (x[4]&0x08)?true:false )
#define CasMfr_n1stAnt(x)         ( (x[4]>>4)&0x03 )
#define CasMfr_n2ndAnt(x)         ( (x[4]>>6)&0x03 )



  void CasMfr_Init (void);
  int CasMfr_GetPhysicalUnits (CasRecord * pRec, unsigned long nMode,
                               float *pTime, float *pFreq, float *pAmpl);
  Ulong CasMfr_nMode (CasRecord * pRec);
  char *CasMfr_sMiniPacketHeader (CasRecord * pRec);



#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasMfr_h */
