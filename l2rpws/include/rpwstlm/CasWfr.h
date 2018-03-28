
#ifndef CasWfr_h
#define CasWfr_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <Cext.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasType.h>


/* Generic for all packets */
#define CasWfr_nSize(x)        ( (x[4]>>4)&0x0F )
#define CasWfr_nSeg(x)         (  x[4]&0x0F )
#define CasWfr_GainCh0(x)      (  x[5]&0x03 )
#define CasWfr_nGainCh0(x)    (  (x[5]&0x03)*10 )
#define CasWfr_GainCh1(x)      ( (x[5]>>2)&0x03 )
#define CasWfr_nGainCh1(x)    ( ((x[5]>>2)&0x03)*10 )
#define CasWfr_GainCh234(x)    ( (x[5]>>4)&0x03 )
#define CasWfr_nGainCh234(x)  ( ((x[5]>>4)&0x03)*10 )
#define CasWfr_bLangmuirProbeMode(x)   ( x[5]&0x40?true:false )
#define CasWfr_bLowBand(x)         ( x[5]&0x80?false:true )
#define CasWfr_bHighBand(x)        ( x[5]&0x80?true:false )

#define CasWfr_Antenna(x)            ( x[6]&0x07 )
#define CasWfr_bMsf(x)             (  (x[6]&0x08)?true:false )
#define CasWfr_Compression(x)      (  (x[6]>>4)&0x0F )
#define CasWfr_bDccCompressed(x)   (  (x[6]&0x20)?true:false )
#define CasWfr_bDccFixedIn(x)      ( ((x[6]&0xF0)==0x20)?true:false )
#define CasWfr_bDccFixedOut(x)     ( ((x[6]&0xF0)==0x30)?true:false )
#define CasWfr_bDcpCompressed(x)   (  (x[6]&0x40)?true:false )
#define CasWfr_bDccJunkByte(x)     (  (x[6]&0x80)?true:false )

#define CasWfr_CaptureMode(x)      (  x[7]&0x07 )
#define CasWfr_Channel(x)          ( (x[7]>>3)&0x07 )
#define CasWfr_bInterleaved(x)     (((x[7]&0x38)==0x38)?true:false )

/* x[6] anntenna,compression */

/* x[7] mode, channel , walsh dig gain factor */

/* x[8] LP_DAC0 */

/* x[9] LP_DAC1 */


  Ulong CasWfr_nMode (CasRecord * pRec);


#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasWfr_h */
