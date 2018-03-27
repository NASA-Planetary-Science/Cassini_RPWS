
#ifndef CasWfdr_h
#define CasWfdr_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <Cext.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasType.h>

/*
  This receiver has been renamed a few time while in route to Saturn.  This is
basically the WFR receiver FFTed and then channelized.  There are two bands:
25Hz and 2.5Khz, each 32 channels wide.  
  In the beginning, the Low Frequency Digital Receiver (LFDR) had two bands:
25Hz and 2.5KHz.  The 25Hz band became the LFR to match the three letter 
acroynames for the MFR and HFR.  Suddenly, the 2.5KHz appeared and was named 
the Mediam Frequency Digital Receiver (MFDR).

  LFDR 25.0Hz mode is LFR.   (today)
  LFDR 2.5KHz mode is MFDR.  (today)

  For the purpose of the code, LFDR will be Waveform Digital Reciever (WFDR)
with two bands: Low Frequency Digital Reciever (LFDR) and Mediam Frequency 
Digital Reciever (MFDR)
*/

/* Generic for all packets */
#define CasWfdr_Channel(x)      ( (x[4]>>5)&0x07 )
#define CasWfdr_nSize(x)        ( 256<<((x[4]>>3)&0x03) )
#define CasWfdr_bFake(x)        ( (x[4]&0x04)?true:false )
#define CasWfdr_ANT(x)          ( (x[4]>>1)&0x01 )
#define CasWfdr_bLog(x)         ( (x[4]&0x01)?false:true )
#define CasWfdr_bLinear(x)      ( (x[4]&0x01)?true:false )
#define CasWfdr_bLfdr(x)        ( (x[5]&0x80)?false:true )
#define CasWfdr_bMfdr(x)        ( (x[5]&0x80)?true:false )
#define CasWfdr_bLpMode(x)      ( (x[5]&0x40)?true:false )
#define CasWfdr_Gain(x)         ( (x[5]>>4)&0x03 )
#define CasWfdr_nGain(x)        ( ((x[5]>>4)&0x03)*10 )
#define CasWfdr_PreScale(x)     (    (x[5]&0x0F) )
#define CasWfdr_nPreScale(x)    ( 1<<(x[5]&0x0F) )



  Ulong CasWfdr_nMode (CasRecord * pRec);
  float CasWfdr_fDuration (CasRecord * pRec, Ulong nMode);

  void CasWfdr_Init (void);
  int CasWfdr_Calibrate (CasRecord * pRec);
  int CasWfdr_GetPhysicalUnits (CasRecord * pRec, Ulong nMode,
                                float *pTime, float *pFreq, float *pAuto);

  extern bool bWfdr_BaseCapacitance;

/*
int (*CasLfdr_GetPhysicalUnits)()=CasWfdr_GetPhysicalUnits;
int (*CasMfdr_GetPhysicalUnits)()=CasWfdr_GetPhysicalUnits;
*/

#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasWfdr_h */
