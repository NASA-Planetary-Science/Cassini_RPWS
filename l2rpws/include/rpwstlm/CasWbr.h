
#ifndef CasWbr_h
#define CasWbr_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <Cext.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasType.h>


/* Generic for all packets */
#define CasWbr_nSize(x)             ( (x[4]>>4)&0x0F )
#define CasWbr_nSeg(x)              (  x[4]&0x0F )
#define CasWbr_Gain(x)              (  x[5]&0x03 )
#define CasWbr_nGain(x)            (  (x[5]&0x03)*10 )
#define CasWbr_Antenna(x)            ( x[6]&0x07 )
#define CasWbr_bMsf(x)             (  (x[6]&0x08)?true:false )
#define CasWbr_Compression(x)      (  (x[6]>>4)&0x0F )
#define CasWbr_bDccCompressed(x)   (  (x[6]&0x20)?true:false )
#define CasWbr_bDccFixedIn(x)      ( ((x[6]&0xF0)==0x20)?true:false )
#define CasWbr_bDccFixedOut(x)     ( ((x[6]&0xF0)==0x30)?true:false )
#define CasWbr_bDcpCompressed(x)   (  (x[6]&0x40)?true:false )
#define CasWbr_bDccJunkSample(x)   (  (x[6]&0x80)?false:true )


#define CasWbr_bLowBand(x)         ( (x[5]&0x80)?false:true )
#define CasWbr_bHighBand(x)       (  (x[5]&0x80)?true:false )

/* x[6] */

/* x[7] */

/* x[8] */

/* x[9] */


  Ulong CasWbr_nMode (CasRecord * pRec);


#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasWbr_h */
