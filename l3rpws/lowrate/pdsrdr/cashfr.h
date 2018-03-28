#ifndef cashfr_h
#define cashfr_h

#ifdef __cplusplus
extern "C" {
#endif





#ifndef _Bool 
#define _Bool 
typedef enum {False,True} Bool;
#endif


#define CasHfr_nPacketSize(x)     ( ((x[0]&0x00FF)<<4) | x[1] )
#define CasHfr_RTI(x)             ( ((x[3]&0x00FF)<<8) | x[2] )

#define CasHfr_nBytesInPacket(x)     ( ((x[5]&0x00FF)<<8) | x[6] )

#define CasHfr_bErrorFlag(x)        (  x[7]&0x80?True:False )
#define CasHfr_PacketType(x)        ( (x[7]>>5)&0x03 )
#define CasHfr_bAnalysis(x)          ( (x[7]&0x60)==0x00?True:False )
#define CasHfr_bSounder(x)          ( (x[7]&0x60)==0x01?True:False )
#define CasHfr_bCalibration(x)      ( (x[7]&0x60)==0x02?True:False )
#define CasHfr_bMsMode(x)           ( (x[7]&0x60)==0x03?True:False )

#define CasHfr_bSounderRelayOn(x)   (  x[7]&0x10?True:False )
#define CasHfr_bClampRelayClamped(x) ( x[7]&0x08?False:True )
#define CasHfr_bMfrExOff(x)          ( x[7]&0x06?False:True )
#define CasHfr_bMfrExp(x)           ( (x[7]&0x06)==0x02?True:False )
#define CasHfr_bMfrExm(x)           ( (x[7]&0x06)==0x04?True:False )
#define CasHfr_bMfrEx(x)            ( (x[7]&0x06)==0x06?True:False )
#define CasHfr_bMfrEzOff(x)         (  x[7]&0x01?False:True )
#define CasHfr_bMfrEz(x)            (  x[7]&0x01?True:False )


#define CasHfr_bDumpMemMode(x)      ( x[8]&0x80?True:False )
#define CasHfr_bCompression(x )     ( x[8]&0x70?True:False )
#define CasHfr_bMeander(x)          ( x[8]&0x10?True:False )
#define CasHfr_HeaderVersion(x)     ( x[8]&0x0F )


/* Analysis Packets */
#define CasHfr_bBandsABC(x)  ( x[9]&0x07?True:False )
#define CasHfr_bBandA(x)     ( x[9]&0x01?True:False )
#define CasHfr_bBandB(x)     ( x[9]&0x02?True:False )
#define CasHfr_bBandC(x)     ( x[9]&0x04?True:False )
#define CasHfr_bBandHF1(x)   ( x[9]&0x08?True:False )
#define CasHfr_bBandHF2(x)   ( x[9]&0x10?True:False )

#define CasHfr_nRepeatCountABC(x)	( x[10] )
#define CasHfr_nRepeatCountHF1(x)	( x[11] )
#define CasHfr_nRepeatCountHF2(x)	( x[12] )
#define CasHfr_nRepeatCountAll(x)	( x[13] )

#define CasHfr_fIntegrationTimeABC(x)   ( 0.125*(1<<((x[14]>>6)&0x03)) )
#define CasHfr_bDirectionFindingABC(x)  ( x[14]&0x20?True:False )
#define CasHfr_bEzABC(x)                ( x[14]&0x10?True:False )
#define CasHfr_ExAntSwitchABC(x)        ( (x[14]>>2)&0x03 )
#define CasHfr_bExAnyABC(x)             ( x[14]&0x0C?True:False )
#define CasHfr_bExpABC(x)               ( (x[14]&0x0C)==0x08?True:False )
#define CasHfr_bExmABC(x)               ( (x[14]&0x0C)==0x04?True:False )
#define CasHfr_bExABC(x)                ( (x[14]&0x0C)==0x0C?True:False )
#define CasHfr_nFiltersABC(x)           ( 0x08<<(x[14]&0x03) )

#define CasHfr_fIntegrationTimeHF1(x)     ( 0.020*(1<<((x[15]>>6)&0x03)) )
#define CasHfr_bDirectionFindingHF1(x)  ( x[15]&0x20?True:False )
#define CasHfr_bEzHF1(x)                ( x[15]&0x10?True:False )
#define CasHfr_ExAntSwitchHF1(x)        ( (x[15]>>2)&0x03 )
#define CasHfr_bExAnyHF1(x)             ( x[15]&0x0C?True:False )
#define CasHfr_bExpHF1(x)               ( (x[15]&0x0C)==0x08?True:False )
#define CasHfr_bExmHF1(x)               ( (x[15]&0x0C)==0x04?True:False )
#define CasHfr_bExHF1(x)                ( (x[15]&0x0C)==0x0C?True:False )
#define CasHfr_nFiltersHF1(x)           ( 0x01<<(x[15]&0x03) )

#define CasHfr_fIntegrationTimeHF2(x)     ( 0.010*(1<<((x[16]>>6)&0x03)) )
#define CasHfr_bDirectionFindingHF2(x)  ( x[16]&0x20?True:False )
#define CasHfr_bEzHF2(x)                ( x[16]&0x10?True:False )
#define CasHfr_ExAntSwitchHF2(x)        ( (x[16]>>2)&0x03 )
#define CasHfr_bExAnyHF2(x)             ( x[16]&0x0C?True:False )
#define CasHfr_bExpHF2(x)               ( (x[16]&0x0C)==0x08?True:False )
#define CasHfr_bExmHF2(x)               ( (x[16]&0x0C)==0x04?True:False )
#define CasHfr_bExHF2(x)                ( (x[16]&0x0C)==0x0C?True:False )
#define CasHfr_nFiltersHF2(x)           ( 0x01<<(x[16]&0x03) )

#define CasHfr_StartFrequencyHF1(x)       ( x[17] )
#define CasHfr_NumberStepsHF1(x)          ( x[18] )
#define CasHfr_FrequencyStepHF1(x)        ( x[19] )

#define CasHfr_StartFrequencyHF2(x)      ( x[20] | ((x[23]&0x0080)<<1) ) 
#define CasHfr_NumberStepsHF2(x)         ( x[21] | ((x[23]&0x0040)<<2) ) 
#define CasHfr_FrequencyStepHF2(x)       ( x[22] )

#define CasHfr_bCrossABC(x)             ( x[23]&0x20?True:False )
#define CasHfr_bCrossHF1(x)             ( x[23]&0x10?True:False )
#define CasHfr_bCrossHF2(x)             ( x[23]&0x08?True:False )
#define CasHfr_bAutoABC(x)              ( x[23]&0x04?True:False )
#define CasHfr_bAutoHF1(x)              ( x[23]&0x02?True:False )
#define CasHfr_bAutoHF2(x)              ( x[23]&0x01?True:False )



/* Calibration Mode*/
#define CasHfr_bCalBandA(x)            ( x[9]&0x01?True:False )
#define CasHfr_bCalBandB(x)            ( x[9]&0x02?True:False )
#define CasHfr_bCalBandC(x)            ( x[9]&0x04?True:False )
#define CasHfr_bCalBandHF1(x)          ( x[9]&0x08?True:False )
#define CasHfr_bCalBandHF2(x)          ( x[9]&0x10?True:False )

#define CasHfr_bCalIowaOn(x)        ( x[10]&0x08?True:False )
#define CasHfr_bCalMfrEz(x)         ( x[10]&0x04?True:False )
#define CasHfr_bCasMfrExOn(x)       ( x[10]?True:False )
#define CasHfr_bCasMfrExp(x)        ( x[10]==0x01?True:False )
#define CasHfr_bCasMfrExm(x)        ( x[10]==0x02?True:False )
#define CasHfr_bCasMfrEx(x)         ( x[10]==0x03?True:False )



/* Millisecond Mode */
#define CasHfr_MsAntenna(x)		( (x[4]>>6)&0x03 )
#define CasHfr_MsSampleRate(x)	        ( (x[4]>>3)&0x07 )
#define CasHfr_MsNumberOfSamples(x)	( x[4]&0x07 )
#define CasHfr_MsFrequency(x)		( x[5] )
#define CasHfr_MsBand(x)		( x[6]&0x01 )



/* Sounder Mode */
#define CasHfr_nCycles(x)          ( x[9] )
#define CasHfr_nPassive(x)         ( x[10] )
#define CasHfr_nActive(x)          ( x[11] ) 
#define CasHfr_StartFreq(x)        ( x[12] )
#define CasHfr_StopFreq(x)         ( x[13] )
#define CasHfr_SoundT1(x)          ( x[14] )
#define CasHfr_SoundT2(x)          ( x[15] )
#define CasHfr_SoundT3(x)          ( x[16] )

#define CasHfr_RcvAntEx(x)         ( x[17]&0x80?True:False )
#define CasHfr_RcvAntEz(x)         ( x[17]&0x80?False:True )
#define CasHfr_SndModeRI_AAA(x)    ( x[17]&0x10?True:False )
#define CasHfr_SndModeRI_PAA(x)    ( x[17]&0x10?False:True )

#define CasHfr_SndAutoA(x)         ( ((x[17]&0x0001)<<8) | x[18] )
#define CasHfr_SndAutoB(x)         ( ((x[17]&0x0006)<<6) | x[19]&0x70 )



char* CasHfr_sFormatMiniPacketHeader(unsigned char *p);





#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* cashfr_h */
