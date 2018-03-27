
#ifndef _INSTRUMENT_H
#define _INSTRUMENT_H

#include <telemetry.h>

#define LP_SAMPLING_PERIOD			(10.0E-6)       /* 12.345mS??? */
#define MFR_CYCLE_PERIOD			(32.0)  /* 32.0Sec */
#define WBR_LOW_BAND_SAMPLING_PERIOD		(36.0E-6)       /* 36uS */
#define WBR_HIGH_BAND_SAMPLING_PERIOD		(4.5E-6)        /* 4.5uS */
#define WFR_LOW_BAND_SAMPLING_PERIOD		(10.0E-3)       /* 10mS */
#define WFR_HIGH_BAND_SAMPLING_PERIOD		(140.0E-6)      /* 140uS */

#define Instrument_Mask		0xFFFFFE00
#define Instrument_Any		0xFFFFFE00

#define Instrument_DUST		0x00002000

#define Instrument_LFDR		0x0000C000
#define Instrument_LFDRlowBand	0x00004000
#define Instrument_LFDRhighBand	0x00008000

#define Instrument_MFR		0x00070000
#define Instrument_MFRband1	0x00010000
#define Instrument_MFRband2	0x00020000
#define Instrument_MFRband3	0x00040000

#define Instrument_HFR		0x00F80000
#define Instrument_HFRa 	0x00080000
#define Instrument_HFRb		0x00100000
#define Instrument_HFRc		0x00200000
#define Instrument_HFRhf1	0x00400000
#define Instrument_HFRhf2	0x00800000

#define Instrument_LP		0x0F000000
#define Instrument_LPanalyzed	0x01000000
#define Instrument_LPdensity	0x02000000
#define Instrument_LPsweep	0x04000000
#define Instrument_LPtbd	0x08000000
#define Instrument_WBR		0x30000000
#define Instrument_WBRlowBand	0x10000000
#define Instrument_WBRhighBand	0x20000000
#define Instrument_WFR		0xC0000000
#define Instrument_WFRlowBand	0x40000000
#define Instrument_WFRhighBand	0x80000000

#define Antenna_Mask		0x000001FF
#define Antenna_Any		0x000001FF
#define Antenna_None		0x00000000
#define Antenna_ExLo		0x00000001
#define Antenna_EzLo		0x00000002
#define Antenna_Bx		0x00000004
#define Antenna_By		0x00000008
#define Antenna_Bz		0x00000010
#define Antenna_LMRp		0x00000020
#define Antenna_LMRm		0x00000040
#define Antenna_LP		0x00000080
#define Antenna_HF		0x00000100

#define Valid_LFDR_Antenna	(Antenna_Any&~(Antenna_HF))
#define Valid_LP_Antenna	(Antenna_LMRp|Antenna_LMRm|Antenna_LP)
#define Valid_MFR_Antenna	(Antenna_ExLo|Antenna_EzLo|Antenna_Bx|Antenna_Bz)
#define Valid_WBR_Antenna	(Antenna_Any&~(Antenna_By|Antenna_Bz|Antenna_LMRp|Antenna_LMRm))
#define Valid_WFR_Antenna	(Antenna_Any&~(Antenna_HF))

typedef struct instrument
{
  int32_t data_type;                       /* Receiver/Antenna */
  int32_t band, gain, agc, dgf, mode;
  int32_t capture_length, packet_length;
  float capture_duration;
  RTI rti;
  CDSCLK cdsclk;
  EVTCLK evtclk;
  SCLK sclk;
  SCET scet;
  struct
  {
    int32_t length;
    float *data;
  } raw;
  struct
  {
    int32_t length;
    float *real, *imag;
  } fft;

/*
  LINE_OBJECT raw,fft;
  LINE_OBJECT time,mag,phs,avg;
*/
  struct instrument *next, *previous;
} INSTRUMENT;

char *ExtractMiniPacketStatus (TELEMETRY * tlm, INSTRUMENT * inst);
int32_t ExtractMiniPacketData (TELEMETRY * tlm, float *data);
char *get_receiver_string (int32_t x);
char *get_antenna_string (int32_t x);

INSTRUMENT *CreateNewInstrument (void);
#endif
