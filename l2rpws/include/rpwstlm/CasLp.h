
#ifndef CasLp_h
#define CasLp_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <stdio.h>
#include <stdlib.h>

#include <Cext.h>
#include <rpwstlm/CasRecord.h>



#define CasLp_BadPacket       0x00800000
#define CasLp_ModeMask        0xFF000000
#define CasLp_AnyMode         0x4F000000
#define CasLp_RawSweep        0x41000000
#define CasLp_RawDensity      0x42000000
#define CasLp_AnalyzedSweep   0x44000000
#define CasLp_TbdMode         0x48000000


#define CasLp_bLpPacket(x)          ( (x[0]&0xF0)==0x40?true:false )
#define CasLp_bRawSweep(x)          ( (x[4]&0x03)==0x00?true:false )
#define CasLp_bRawDensity(x)        ( (x[4]&0x03)==0x01?true:false )
#define CasLp_bAnalyzedSweep(x)     ( (x[4]&0x03)==0x02?true:false )
#define CasLp_bTbdMode(x)           ( (x[4]&0x03)==0x03?true:false )
#define CasLp_DataCompression(x)    ( (x[4]>>2)&0x03 )
#define CasLp_bCompressed(x)        ( (x[4]&0x0C)?true:false )
#define CasLp_bRawUnpacked(x)       ( (x[4]&0x0C)==0x00?true:false )
#define CasLp_bRawPacked(x)         ( (x[4]&0x0C)==0x04?true:false )
#define CasLp_bTbdCompression(x)    ( (x[4]&0x0C)==0x08?true:false )
#define CasLp_bDcpCompressed(x)     ( (x[4]&0x0C)==0x0C?true:false )
#define CasLp_nSegment(x)           ( (x[4]>>4)&0x03 )
#define CasLp_nSize(x)              ( (x[4]>>6)&0x03 )
#define CasLp_ClockPeriod(x)        ( (((Ulong)x[9])<<8)|((Ulong)x[8]) )
#define CasLp_bRtiClockPeriod(x)    ( (x[9]&0xC0)==0xC0?true:false )


#define CasLp_nHeaderLength 10

  Ulong CasLp_nMode (CasRecord * pRec);
  float CasLp_fDuration (CasRecord * pRec, Ulong nMode);



#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasLp_h */
