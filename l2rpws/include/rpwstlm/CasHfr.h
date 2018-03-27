
#ifndef CasHfr_h
#define CasHfr_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <Cext.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasType.h>


#define CasHfr_bHfrPacket(x)		( (x[0]&0xF0)==0x20?true:false )
#define CasHfr_RTI(x)		        ( ((x[3]&0x0FF)<<8)|(x[2]) )

#define CasHfr_bEOF(x)		        ( (x[4]&0x80)?true:false )

/* C quirk: x[5] should be promoted to an int allowing the 16 bit number */
#define CasHfr_nPacketSize(x)		( (((Ulong)x[5]&0x0FF)<<8) | (x[6]) )
#define CasHfr_bMemoryOverrun(x)	( (x[7]&0x80)?true:false )
#define CasHfr_bErrorFlag(x)	        ( (x[7]&0x80)?true:false )
#define CasHfr_bAnalysis(x)		( ((x[7]&0x60)==0x00)?true:false )
#define CasHfr_bSounder(x)		( ((x[7]&0x60)==0x20)?true:false )
#define CasHfr_bCalibration(x)		( ((x[7]&0x60)==0x40)?true:false )
#define CasHfr_bMillisecond(x)		( ((x[7]&0x60)==0x60)?true:false )

#define CasHFR_bSndRlyOn(x)	        ( (x[7]&0x10)?true:false )
#define CasHFR_bClmpRlyClamped(x)	( (x[7]&0x08)?false:true )

/*
#define CasHFR_MFRExSwitch(x)		( ((x)->Header.all.bytes[2]>>1)&0x03 )
#define CasHFR_MFREzSwitch(x)		(  (x)->Header.all.bytes[2]&0x01 )
#define CasHFR_MFREwSwitch(x)		(  (x)->Header.all.bytes[2]&0x01 )
*/

#define CasHfr_bDumpMemory(x)		( (x[8]&0x80)?true:false )
#define CasHfr_bCompressed(x)		( (x[8]&0x70)?true:false )
#define CasHfr_bMeanderCompressed(x)    ( (x[8]&0x70)==0x10?true:false )
#define CasHfr_nHeaderVersion(x)        ( x[8]&0x0F )



/* Analysis Packets */
#define CasHfrAnl_HeaderLength 25

#define CasHfr_bAttnOn(x)            ( (x[9]&0x80)?true:false )
#define CasHfr_bBandASelected(x)     ( (x[9]&0x01)?true:false )
#define CasHfr_bBandBSelected(x)     ( (x[9]&0x02)?true:false )
#define CasHfr_bBandCSelected(x)     ( (x[9]&0x04)?true:false )
#define CasHfr_bBandHF1Selected(x)   ( (x[9]&0x08)?true:false )
#define CasHfr_bBandHF2Selected(x)   ( (x[9]&0x10)?true:false )

#define CasHfr_nRepeatCountABC(x)	( x[10] )
#define CasHfr_nRepeatCountHF1(x)	( x[11] )
#define CasHfr_nRepeatCountHF2(x)	( x[12] )
#define CasHfr_nRepeatCountAll(x)	( x[13] )

#define CasHfr_IntegrationTimeABC(x)    ( (x[14]>>6)&0x03 )
#define CasHfr_fIntegrationTimeABC(x)   ( (1<< ((x[14]>>6)&0x03)) * 0.125 )
#define CasHfr_bDirectionFindingABC(x)  ( (x[14]&0x20)?true:false )
#define CasHfr_bEzOnABC(x)              ( (x[14]&0x10)?true:false )
#define CasHfr_bEwOnABC(x)              ( (x[14]&0x10)?true:false )
#define CasHfr_bEuOnABC(x)              ( (x[14]&0x0C)==0x04?true:false )
#define CasHfr_bEvOnABC(x)              ( (x[14]&0x0C)==0x08?true:false )
#define CasHfr_bExOnABC(x)              ( (x[14]&0x0C)==0x0C?true:false )
#define CasHfr_FiltersABC(x)            ( (x[14]&0x03)==3?2:x[14]&0x03 )
#define CasHfr_nFiltersABC(x)           ( (x[14]&0x03)<3?(8<<(x[14]&0x03)):32 )

#define CasHfr_IntegrationTimeHF1(x)    ( (x[15]>>6)&0x03 )
#define CasHfr_fIntegrationTimeHF1(x)   ( (1<< ((x[15]>>6)&0x03)) * 0.020 )
#define CasHfr_bDirectionFindingHF1(x)  ( (x[15]&0x20)?true:false )
#define CasHfr_bEzOnHF1(x)              ( (x[15]&0x10)?true:false )
#define CasHfr_bEwOnHF1(x)              ( (x[15]&0x10)?true:false )
#define CasHfr_bEuOnHF1(x)              ( (x[15]&0x0C)==0x04?true:false )
#define CasHfr_bEvOnHF1(x)              ( (x[15]&0x0C)==0x08?true:false )
#define CasHfr_bExOnHF1(x)              ( (x[15]&0x0C)==0x0C?true:false )
#define CasHfr_FiltersHF1(x)            ( (x[15]&0x03) )
#define CasHfr_nFiltersHF1(x)           ( 1 << (x[15]&0x03) )

#define CasHfr_IntegrationTimeHF2(x)    ( (x[16]>>6)&0x03 )
#define CasHfr_fIntegrationTimeHF2(x)   ( (1<< ((x[16]>>6)&0x03)) * 0.010 )
#define CasHfr_bDirectionFindingHF2(x)  ( (x[16]&0x20)?true:false )
#define CasHfr_bEzOnHF2(x)              ( (x[16]&0x10)?true:false )
#define CasHfr_bEwOnHF2(x)              ( (x[16]&0x10)?true:false )
#define CasHfr_bEuOnHF2(x)              ( (x[16]&0x0C)==0x04?true:false )
#define CasHfr_bEvOnHF2(x)              ( (x[16]&0x0C)==0x08?true:false )
#define CasHfr_bExOnHF2(x)              ( (x[16]&0x0C)==0x0C?true:false )
#define CasHfr_FiltersHF2(x)            ( (x[16]&0x03) )
#define CasHfr_nFiltersHF2(x)           ( 1 << (x[16]&0x03) )

#define CasHfr_nStartFrequencyHF1(x)    ( x[17] )
#define CasHfr_nNumberStepsHF1(x)       ( x[18] )
#define CasHfr_nFrequencyStepHF1(x)     ( x[19] )

/* C quirk: x[23] should be promoted to an int allowing the 16 bit number */
#define CasHfr_nStartFrequencyHF2(x)    ( ((x[23]&0x080)<<1) | (x[20]) )
#define CasHfr_nNumberStepsHF2(x)       ( ((x[23]&0x040)<<2) | (x[21]) )
#define CasHfr_nFrequencyStepHF2(x)     ( x[22] )

#define CasHfr_bCrossABC(x)             ( (x[23]&0x20)?true:false )
#define CasHfr_bCrossHF1(x)             ( (x[23]&0x10)?true:false )
#define CasHfr_bCrossHF2(x)             ( (x[23]&0x08)?true:false )

#define CasHfr_bAutoABC(x)             ( (x[23]&0x04)?true:false )
#define CasHfr_bAutoHF1(x)             ( (x[23]&0x02)?true:false )
#define CasHfr_bAutoHF2(x)             ( (x[23]&0x01)?true:false )

#define CasHfr_bSpareValid(x)          ( (x[24]==0xAA)?true:false )
#define CasHfr_bHeaderTerminated(x)    ( (x[24]==0xAA)?true:false )
#define CasHfr_HeaderTermination(x)    ( (x[24]) )




/* Calibration Mode*/
#define CasHfrCal_HeaderLength 12

#define CasHfrCal_bASelected(x)	        ( (x[9]&0x01)?true:false )
#define CasHfrCal_bBSelected(x)	        ( (x[9]&0x02)?true:false )
#define CasHfrCal_bCSelected(x)	        ( (x[9]&0x04)?true:false )
#define CasHfrCal_bHF1Selected(x)       ( (x[9]&0x08)?true:false )
#define CasHfrCal_bHF2Selected(x)       ( (x[9]&0x10)?true:false )

#define CasHfrCal_bMFRAntennaEu(x)      ( (x[10]&0x03)==0x01?true:false )
#define CasHfrCal_bMFRAntennaEv(x)      ( (x[10]&0x03)==0x02?true:false )
#define CasHfrCal_bMFRAntennaEx(x)      ( (x[10]&0x03)==0x03?true:false )
#define CasHfrCal_bMFRAntennaEz(x)      ( (x[10]&0x04)==0x04?true:false )
#define CasHfrCal_bMFRAntennaEw(x)      ( (x[10]&0x04)==0x04?true:false )

#define CasHFRCal_bIowaCalOn(x)	( ((x)->Header.ms.bytes[5])&0x04 )

#define CasHfrCal_HeaderTermination(x)    ( (x[11]) )


/* Sounder Mode */
#define CasHfrSnd_HeaderLength 21

#define CasHfrSnd_nCycles(x)            ( x[9] )
#define CasHfrSnd_nPassive(x)           ( x[10] )
#define CasHfrSnd_nActive(x)            ( x[11] )
#define CasHfrSnd_nStartFrequency(x)    ( x[12] )
#define CasHfrSnd_nStopFrequency(x)     ( x[13] )
#define CasHfrSnd_nT1(x)                ( x[14] )
#define CasHfrSnd_nT2(x)                ( x[15] )
#define CasHfrSnd_nT3(x)                ( x[16] )
#define CasHfrSnd_bRcvAntEz(x)         ( (x[17]&0x80)?false:true )
#define CasHfrSnd_bRcvAntEx(x)         ( (x[17]&0x80)?true:false )
#define CasHfrSnd_bMode_RI_AAA(x)      ( (x[17]&0x10)?true:false )
#define CasHfrSnd_bMode_RI_PAA(x)      ( (x[17]&0x10)?false:true )

#define CasHfrSnd_nAutoOutA(x)  (((x[17]<<8)|x[18])&0x01FF)
#define CasHfrSnd_nAutoOutB(x)  ((((x[17]&0x06)<<6)|(x[19]&0x7F))&0x03FF)

#define CasHfrSnd_bHeaderTerminated(x)    ( (x[20]==0xBB)?true:false )
#define CasHfrSnd_HeaderTermination(x)    ( (x[20]) )






/* Millisecond Mode */
#define CasHfrMsc_HeaderLength 12

#define CasHfrMsc_Antenna(x)            ( (x[9]&0xC0)>>6 )
#define CasHfrMsc_bEzOn(x)              ( (x[9]&0xC0)==0x00?true:false )
#define CasHfrMsc_bEwOn(x)              ( (x[9]&0xC0)==0x00?true:false )
#define CasHfrMsc_bEuOn(x)              ( (x[9]&0xC0)==0x40?true:false )
#define CasHfrMsc_bEvOn(x)              ( (x[9]&0xC0)==0x80?true:false )
#define CasHfrMsc_bExOn(x)              ( (x[9]&0xC0)==0xC0?true:false )

#define CasHfrMsc_SampleRate(x)	       ( (x[9]>>3)&0x07 )
#define CasHfrMsc_fSampleRate(x)       ( (1 << ((x[9]>>3)&0x07)) * 0.0005 )
#define CasHfrMsc_NumberOfSamples(x)   ( (x[9]&0x07) )
#define CasHfrMsc_nNumberOfSamples(x)  ( (1 << (x[9]&0x07)) * 256 )
#define CasHfrMsc_Frequency(x)	       ( x[10] )
#define CasHfrMsc_fFrequency(x)        ( x[11]&0x01?(x[10]*100+25)*1.0E3:(x[10]*25)*1.0E3 )

#define CasHfrMsc_bHF1(x)              ( x[11]&0x01?false:true )
#define CasHfrMsc_bHF2(x)              ( x[11]&0x01?true:false )



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/

/* -=-=-=-=-=-=-=-=-=-=-=-= Meander Decompression =-=-=-=-=-=-=-=-=-=-=-=-=-=*/

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/
#ifndef CasHfrHeander_C
  extern FILE *hCasHfrMeanderErr;
#endif

  extern bool bMeanderDebug, bMeanderHeader;
  extern Ulong nMeanderInCnt, nMeanderOutCnt, nMeanderErrCnt;

  extern bool BALESE;                   /* Calibration Output Units: 
                                         * true  dBv/rtHz 
                                         * false V^2/m^2/Hz  (default) */




  void CasHfr_Init (void);
  Ulong CasHfr_nMode (CasRecord * pRec);

  bool CasHfr_GetMpAnalysis (CasRecord * pRec);
  void CasHfr_DumpAgc (void);
  void CasHfr_DumpAuto (void);
  void CasHfr_DumpAutoMag (void);

  void CasHfr_DumpCalConstants (FILE * h);

  void CasHfr_DumpRecord (CasRecord * pRec);    /* doesn't belong here */

  Ulong CasHfr_GetAgcByteCount (CasRecord * pRec);
  bool CasHfr_CalibrateAnalysis (CasRecord * pRec);

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/

  Ulong CasHfr_Meander (CasRecord * pInRec, CasRecord * pOutRec);
  Ulong CasHfrMeander_nValidPacket (CasRecord * pInRec);
  char *CasHfrMeander_DecodeErrorStatus (CasRecord * pRec);


  Ulong CasHfr_RecordToKronos (CasRecord * pIn, Uchar * pOut, bool bSwap);
  Ulong CasHfr_ByteCount (CasRecord * pRec, Ulong nType);

  Ulong CasHfrAnalysis_AgcByteCount (CasRecord * pRec, Ulong nType);
  Ulong CasHfrAnalysis_AutoByteCount (CasRecord * pRec, Ulong nType);
  Ulong CasHfrAnalysis_CrossByteCount (CasRecord * pRec, Ulong nType);
  Ulong CasHfrAnalysis_ByteCount (CasRecord * pRec, Ulong nType);
  char *CasHfrAnalysis_sByteCount (CasRecord * pRec);

  Ulong CasHfrAnalysis_GetAgcByteCount (CasRecord * pRec);
  Ulong CasHfrAnalysis_GetAutoByteCount (CasRecord * pRec);
  Ulong CasHfrAnalysis_XtractAgc (CasRecord * pRec);
  Ulong CasHfrAnalysis_XtractAuto (CasRecord * pRec);
  Ulong CasHfrAnalysis_XtractCross (CasRecord * pRec);
  Ulong CasHfrAnalysis_CalibrateAuto (CasRecord * pRec);
  void CasHfrAnalysis_InstrumentTiming (CasRecord * pRec);

  char *CasHfrAnalysis_sValidPacket (CasRecord * pRec);
  int CasHfrAnalysis_GetPhysicalUnits (CasRecord * pRec,
                                       Ulong nBndAnt, float *pTime,
                                       float *pFreq, float *pAuto);

  float CasHfrAnalysis_fDuration (CasRecord * pRec, Ulong nType);
  float CasHfrAnalysis_fSampleWidth (CasRecord * pRec, Ulong nType);



  /* Looks to be an initialization function */
  void CasHfrCalibration_dBMagnitudePhase (void);
  
  bool CasHfrCalibration_bValidPacket (FILE * h, CasRecord * pRec);

  Ulong CasHfrCalibration_XtractRaw (CasRecord * pRec);

  /* Data to write are dug out of module global arrays, don't know what
     sets these arrays yet */
  void CasHfrCalibration_WritePdsFormatABC (FILE * h);
  void CasHfrCalibration_WritePdsFormatHF1 (FILE * h);
  void CasHfrCalibration_WritePdsFormatHF2 (FILE * h);

  void CasHfrCalibration_DumpRaw (FILE * h);
  void CasHfrCalibration_DumpRawABC (FILE * h);
  void CasHfrCalibration_DumpRawHF1 (FILE * h);
  void CasHfrCalibration_DumpRawHF2 (FILE * h);
  Ulong CasHfrCalibration_ByteCount (CasRecord * pRec, Ulong nType);


  Ulong CasHfrSounder_ByteCount (CasRecord * pRec, Ulong nType);
  /*
   * returns the sounder active time in seconds, nType is unused 
   */
  float CasHfrSounder_fDuration (CasRecord * pRec, Ulong nType);
  char *CasHfrSounder_sValidPacket (CasRecord * pRec);



  Ulong CasHfrMillisecond_ByteCount (CasRecord * pRec, Ulong nType);
  int CasHfrMillisecond_GetPhysicalUnits (CasRecord * pRec,
                                          Ulong nBndAnt, float *pTime,
                                          float *pFreq, float *pAuto);
  float CasHfrMillisecond_fDuration (CasRecord * pRec, Ulong nType);
  char *CasHfrMillisecond_sValidPacket (CasRecord * pRec);


#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasHfr_h */
