
/*
  HFR Calibration Packets Subroutines

Thursday August 26, 2004 
  Begin Subroutine Coding

*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>  /* C99 */
#include <stdint.h>   /* C99 */

#include <Cext.h>
#include <rpwstlm/CasHfr.h>
#include "cal/CasHfrCal.h"

/*
 =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
*/

#define HFRCAL_RAW_LENGTH 256
#define HFRCAL_CAL_LENGTH 256


typedef struct chfrcal_ant
{
  Uchar Eu[HFRCAL_RAW_LENGTH];
  Uchar Ev[HFRCAL_RAW_LENGTH];
  Uchar Ew[HFRCAL_RAW_LENGTH];
  Uchar Ez[HFRCAL_RAW_LENGTH];
} cHfrCalAnt;                           /* 1K */

typedef struct chfrcal_df
{
  Uchar EuEw[HFRCAL_RAW_LENGTH];
  Uchar EvEz[HFRCAL_RAW_LENGTH];
} cHfrCalDf;                            /* 1K */

typedef struct schfrcal_df
{
  int8_t EuEw[HFRCAL_RAW_LENGTH];
  int8_t EvEz[HFRCAL_RAW_LENGTH];
} scHfrCalDf;                           /* 1K */


typedef struct fhfrcal_ant
{
  float Eu[HFRCAL_RAW_LENGTH];
  float Ev[HFRCAL_RAW_LENGTH];
  float Ew[HFRCAL_RAW_LENGTH];
  float Ez[HFRCAL_RAW_LENGTH];
} fHfrCalAnt;                           /* 4K */

typedef struct fhfrcal_df
{
  float EuEw[HFRCAL_RAW_LENGTH];
  float EvEz[HFRCAL_RAW_LENGTH];
} fHfrCalDf;                            /* 1K */

typedef struct hfrcal_mode
{
  cHfrCalAnt agc, acor;
  cHfrCalDf rmag, imag;
  scHfrCalDf rsign, isign;
  fHfrCalAnt AutoMag;
  fHfrCalDf CrossMag, CrossPhs;
} HfrCalMode;

static HfrCalMode A, B, C, HF1, HF2;


int CasHfrCalibration_XtractCrossSign (CasRecord * pRec);


void CasHfrCalibration_Phase (void);
int CasHfrCalibration_calibrate (CasRecord * pRec, float *pAutoMag,
                                 float *pCrossMag, float *pCrossPhs);


float CasHfrCalibration_AutoMag (Uchar acor);
float CasHfrCalibration_CrossMag (Uchar creal, Uchar cimag);
float CasHfrCalibration_CrossPhs (Uchar creal, Uchar cimag,
                                   char sreal, char simag);
int8_t cross_sign_bit (Uchar * p);

/*
void CasHfrCalibration_WritePdsFormatABC(FILE *h,CasRecord *pRec);
void CasHfrCalibration_WritePdsFormatHF1(FILE *h,CasRecord *pRec);
void CasHfrCalibration_WritePdsFormatHF2(FILE *h,CasRecord *pRec);
*/

void silence_compiler_warnings (void);

/* ************************************************************************* */

void silence_compiler_warnings (void)
{

/* 
cal/CasHfrA1HF1.h:7: warning: `A1HF1' defined but not used 
cal/CasHfrA1HF1.h:9: warning: `A1hf1tmp' defined but not used
cal/CasHfrA1HF2.h:8: warning: `A1HF2' defined but not used
cal/CasHfrA1HF2.h:10: warning: `A1hf2tmp' defined but not used
cal/CasHfrCal.h:34: warning: `A123' defined but not used
cal/CasHfrCal.h:48: warning: `dBcalABC_08' defined but not used
cal/CasHfrCal.h:49: warning: `dbcal_08' defined but not used
cal/CasHfrCal.h:65: warning: `dBcalABC_16' defined but not used
cal/CasHfrCal.h:66: warning: `dbcal_16' defined but not used
cal/CasHfrCal.h:90: warning: `dBcalABC_32' defined but not used
cal/CasHfrCal.h:91: warning: `dbcal_32' defined but not used
cal/CasHfrCal.h:131: warning: `dBcalHF' defined but not used
cal/CasHfrCal.h:132: warning: `dbcalhf' defined but not used
cal/CasHfrCal.h:152: warning: `ABC_CycleTime' defined but not used
cal/CasHfrCal.h:163: warning: `HF1_InitStepTime' defined but not used
cal/CasHfrCal.h:174: warning: `HF1_PrecStepTime' defined but not used
cal/CasHfrCal.h:185: warning: `HF2_InitStepTime' defined but not used
cal/CasHfrCal.h:196: warning: `HF2_PrecStepTime' defined but not used
cal/CasHfrCal.h:206: warning: `CasHfr_BandA_Freq' defined but not used
cal/CasHfrCal.h:207: warning: `CasHfr_BandB_Freq' defined but not used
cal/CasHfrCal.h:208: warning: `CasHfr_BandC_Freq' defined but not used
*/

  A1HF1[0][0] += 0;
  A1hf1tmp[0][0] += 0;
  A1HF2[0][0] += 0;
  A1hf2tmp[0][0] += 0;
  A123[0][0] += 0;
  dBcalABC_08[0][0] += 0;
  dBcalABC_16[0][0] += 0;
  dBcalABC_32[0][0] += 0;
  dbcal_08[0][0] += 0;
  dbcal_16[0][0] += 0;
  dbcal_32[0][0] += 0;
  dBcalHF[0][0] += 0;
  dbcalhf[0][0] += 0;
  ABC_CycleTime[0][0][0] += 0;
  HF1_InitStepTime[0][0] += 0;
  HF1_PrecStepTime[0][0] += 0;
  HF2_InitStepTime[0][0] += 0;
  HF2_PrecStepTime[0][0] += 0;
  CasHfr_BandA_Freq[0][0] += 0;
  CasHfr_BandB_Freq[0][0] += 0;
  CasHfr_BandC_Freq[0][0] += 0;

  return;
}

/* ************************************************************************* */

bool CasHfrCalibration_bValidPacket (FILE * h, CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  bool bStatus = true;
  Ulong nRecLen, nMpLen, nHfrLen;

  nRecLen = pRec->status.packet_length;
  nMpLen = pMp[0] & 0x0F;
  nMpLen << 8;              /* Statement with no effect in original as well, odd. -cwp */
  nMpLen |= pMp[1];
  nHfrLen = CasHfr_nPacketSize (pMp);
  if (CasHfr_bHfrPacket (pMp) != true) {
    bStatus = false;
    if (h != NULL)
      fprintf (h, "not a hfr packet\n");
  } else if (CasHfr_bCalibration (pMp) != true) {
    bStatus = false;
    if (h != NULL)
      fprintf (h, "not a calibration packet\n");
  } else if (CasHfr_bErrorFlag (pMp) == true) {
    bStatus = false;
    if (h != NULL)
      fprintf (h, "memory overrun/error flag set\n");
  } else if (CasHfr_nHeaderVersion (pMp) != 0x05) {
    bStatus = false;
    if (h != NULL)
      fprintf (h, "incorrect header version %d\n",
               CasHfr_nHeaderVersion (pMp));
  } else if (CasHfrCal_HeaderTermination (pMp) != 0xCC) {
    bStatus = false;
    if (h != NULL)
      fprintf (h, "header not terminated %02X\n",
               CasHfrCal_HeaderTermination (pMp));
  } else if (pMp[nRecLen + 2] != 0x5A) {
    bStatus = false;
    if (h != NULL)
      fprintf (h, "data not terminated %02X\n", pMp[nRecLen + 2]);
  } else if ((nRecLen + 3) != (nHfrLen + 7)) {
    bStatus = false;
    if (h != NULL)
      fprintf (h, "length mismatch, mp len + 3 = %d, hfr len + 7 = %d\n",
               nRecLen, nHfrLen);
  }
  /*
   * check for correct byte count: agc+auto+cross+sign 
   */

  return bStatus;
}

/* ************************************************************************* */

Ulong CasHfrCalibration_XtractRaw (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Uchar *pData;
  Ulong i, j, nLength = 0;


/*
typedef struct hfrcal_raw_mode{
  cHfrCalAnt agc,acor;
  cHfrCalDf rmag,rsign,imag,isign;
  fHfrCalAnt AutoMag;
  fHfrCalDf  CrossMag,CrossPhs;
}HfrCalMode;                              
*/
  pData = pRec->data + CasHfrCal_HeaderLength;

  /*
   * Extract the AGC data 
   */
  if (CasHfrCal_bASelected (pMp) == true) {
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      A.agc.Eu[i] = *pData++;
      A.agc.Ev[i] = *pData++;
    }
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      A.agc.Ew[i] = *pData++;
      A.agc.Ez[i] = *pData++;
    }
  }
  if (CasHfrCal_bBSelected (pMp) == true) {
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      B.agc.Eu[i] = *pData++;
      B.agc.Ev[i] = *pData++;
    }
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      B.agc.Ew[i] = *pData++;
      B.agc.Ez[i] = *pData++;
    }
  }
  if (CasHfrCal_bCSelected (pMp) == true) {
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      C.agc.Eu[i] = *pData++;
      C.agc.Ev[i] = *pData++;
    }
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      C.agc.Ew[i] = *pData++;
      C.agc.Ez[i] = *pData++;
    }
  }
  if (CasHfrCal_bHF1Selected (pMp) == true) {
    for (i = 0; i < 36; i++) {          /* 7_on F4 to F19 */
      HF1.agc.Eu[i] = *pData++;         /* 5_off F4 to F19 */
      HF1.agc.Ev[i] = *pData++;         /* F19 filters 1,2,4,8 */
    }
    for (i = 0; i < 36; i++) {          /* 7_on F4 to F19 */
      HF1.agc.Ew[i] = *pData++;         /* 5_off F4 to F19 */
      HF1.agc.Ez[i] = *pData++;         /* F19 filters 1,2,4,8 */
    }
  }
  if (CasHfrCal_bHF2Selected (pMp) == true) {
    for (i = 0; i < 140; i++) {         /* 7_on F5,F10,...F320 */
      HF2.agc.Eu[i] = *pData++;         /* 5_off F5,F10,...F320 */
      HF2.agc.Ev[i] = *pData++;         /* F19 filters 1,2,4,8 */
    }
    for (i = 0; i < 140; i++) {         /* 7_on F4 to F19 */
      HF2.agc.Ew[i] = *pData++;         /* 5_off F4 to F19 */
      HF2.agc.Ez[i] = *pData++;         /* F15 5_on to 0_on & 7_off to 2_off */
    }
  }
  nLength = (pData - (pRec->data + CasHfrCal_HeaderLength));
  /*
   * end of AGC data 
   */



  /*
   * Extract the Auto data 
   */
  if (CasHfrCal_bASelected (pMp) == true) {
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        A.acor.Eu[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        A.acor.Ev[j] = *pData++;
    }
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        A.acor.Ew[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        A.acor.Ez[j] = *pData++;
    }
  }
  if (CasHfrCal_bBSelected (pMp) == true) {
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        B.acor.Eu[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        B.acor.Ev[j] = *pData++;
    }
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        B.acor.Ew[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        B.acor.Ez[j] = *pData++;
    }
  }
  if (CasHfrCal_bCSelected (pMp) == true) {
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        C.acor.Eu[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        C.acor.Ev[j] = *pData++;
    }
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        C.acor.Ew[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        C.acor.Ez[j] = *pData++;
    }
  }

  if (CasHfrCal_bHF1Selected (pMp) == true) {
    for (i = 0; i < 32; i++) {          /* 7_on F4 to F19 */
      HF1.acor.Eu[i] = *pData++;        /* 5_off F4 to F19 */
      HF1.acor.Ev[i] = *pData++;
    }
    HF1.acor.Eu[i] = *pData++;          /* F19 filter 1 */
    HF1.acor.Ev[i] = *pData++;          /* F19 filter 1 */
    for (i = 33; i < 35; i++)
      HF1.acor.Eu[i] = *pData++;        /* F19 filter 2 */
    for (i = 33; i < 35; i++)
      HF1.acor.Ev[i] = *pData++;        /* F19 filter 2 */
    for (i = 35; i < 39; i++)
      HF1.acor.Eu[i] = *pData++;        /* F19 filter 4 */
    for (i = 35; i < 39; i++)
      HF1.acor.Ev[i] = *pData++;        /* F19 filter 4 */
    for (i = 39; i < 47; i++)
      HF1.acor.Eu[i] = *pData++;        /* F19 filter 8 */
    for (i = 39; i < 47; i++)
      HF1.acor.Ev[i] = *pData++;        /* F19 filter 8 */

    for (i = 0; i < 32; i++) {          /* 7_on F4 to F19 */
      HF1.acor.Ew[i] = *pData++;        /* 5_off F4 to F19 */
      HF1.acor.Ez[i] = *pData++;
    }
    HF1.acor.Ew[i] = *pData++;          /* F19 filter 1 */
    HF1.acor.Ez[i] = *pData++;          /* F19 filter 1 */
    for (i = 33; i < 35; i++)
      HF1.acor.Ew[i] = *pData++;        /* F19 filter 2 */
    for (i = 33; i < 35; i++)
      HF1.acor.Ez[i] = *pData++;        /* F19 filter 2 */
    for (i = 35; i < 39; i++)
      HF1.acor.Ew[i] = *pData++;        /* F19 filter 4 */
    for (i = 35; i < 39; i++)
      HF1.acor.Ez[i] = *pData++;        /* F19 filter 4 */
    for (i = 39; i < 47; i++)
      HF1.acor.Ew[i] = *pData++;        /* F19 filter 8 */
    for (i = 39; i < 47; i++)
      HF1.acor.Ez[i] = *pData++;        /* F19 filter 8 */

  }
  if (CasHfrCal_bHF2Selected (pMp) == true) {
    for (i = 0; i < 140; i++) {         /* 7_on F5,F10,...F320 */
      HF2.acor.Eu[i] = *pData++;        /* 5_off F5,F10,...F320 */
      HF2.acor.Ev[i] = *pData++;        /* F15 5_on to 0_on & 7_off to 2_off */
    }
    for (i = 0; i < 140; i++) {         /* 7_on F5 to F320 */
      HF2.acor.Ew[i] = *pData++;        /* 5_off F5 to F320 */
      HF2.acor.Ez[i] = *pData++;        /* F15 5_on to 0_on & 7_off to 2_off */
    }
  }



  /*
   * Extract the Cross data 
   */
  if (CasHfrCal_bASelected (pMp) == true) {
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        A.rmag.EuEw[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        A.rmag.EvEz[j] = *pData++;
    }
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        A.imag.EuEw[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        A.imag.EvEz[j] = *pData++;
    }
  }
  if (CasHfrCal_bBSelected (pMp) == true) {
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        B.rmag.EuEw[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        B.rmag.EvEz[j] = *pData++;
    }
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        B.imag.EuEw[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        B.imag.EvEz[j] = *pData++;
    }
  }
  if (CasHfrCal_bCSelected (pMp) == true) {
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        C.rmag.EuEw[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        C.rmag.EvEz[j] = *pData++;
    }
    for (i = 0; i < 12; i++) {          /* 5_on to 0_on & 7_off to 2_off */
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        C.imag.EuEw[j] = *pData++;
      for (j = i * 16; j < i * 16 + 16; j++)    /* filter 0 to 15 */
        C.imag.EvEz[j] = *pData++;
    }
  }

  if (CasHfrCal_bHF1Selected (pMp) == true) {
    for (i = 0; i < 32; i++) {          /* 7_on F4 to F19 */
      HF1.rmag.EuEw[i] = *pData++;      /* 5_off F4 to F19 */
      HF1.rmag.EvEz[i] = *pData++;
    }
    HF1.rmag.EuEw[i] = *pData++;        /* F19 filter 1 */
    HF1.rmag.EvEz[i] = *pData++;        /* F19 filter 1 */
    for (i = 33; i < 35; i++)
      HF1.rmag.EuEw[i] = *pData++;      /* F19 filter 2 */
    for (i = 33; i < 35; i++)
      HF1.rmag.EvEz[i] = *pData++;      /* F19 filter 2 */
    for (i = 35; i < 39; i++)
      HF1.rmag.EuEw[i] = *pData++;      /* F19 filter 4 */
    for (i = 35; i < 39; i++)
      HF1.rmag.EvEz[i] = *pData++;      /* F19 filter 4 */
    for (i = 39; i < 47; i++)
      HF1.rmag.EuEw[i] = *pData++;      /* F19 filter 8 */
    for (i = 39; i < 47; i++)
      HF1.rmag.EvEz[i] = *pData++;      /* F19 filter 8 */

    for (i = 0; i < 32; i++) {          /* 7_on F4 to F19 */
      HF1.imag.EuEw[i] = *pData++;      /* 5_off F4 to F19 */
      HF1.imag.EvEz[i] = *pData++;
    }
    HF1.imag.EuEw[i] = *pData++;        /* F19 filter 1 */
    HF1.imag.EvEz[i] = *pData++;        /* F19 filter 1 */
    for (i = 33; i < 35; i++)
      HF1.imag.EuEw[i] = *pData++;      /* F19 filter 2 */
    for (i = 33; i < 35; i++)
      HF1.imag.EvEz[i] = *pData++;      /* F19 filter 2 */
    for (i = 35; i < 39; i++)
      HF1.imag.EuEw[i] = *pData++;      /* F19 filter 4 */
    for (i = 35; i < 39; i++)
      HF1.imag.EvEz[i] = *pData++;      /* F19 filter 4 */
    for (i = 39; i < 47; i++)
      HF1.imag.EuEw[i] = *pData++;      /* F19 filter 8 */
    for (i = 39; i < 47; i++)
      HF1.imag.EvEz[i] = *pData++;      /* F19 filter 8 */
  }
  if (CasHfrCal_bHF2Selected (pMp) == true) {
    for (i = 0; i < 140; i++) {         /* 7_on F5,F10,...F320 */
      HF2.rmag.EuEw[i] = *pData++;      /* 5_off F5,F10,...F320 */
      HF2.rmag.EvEz[i] = *pData++;      /* F15 5_on to 0_on & 7_off to 2_off */
    }
    for (i = 0; i < 140; i++) {         /* 7_on F5 to F320 */
      HF2.imag.EuEw[i] = *pData++;      /* 5_off F5 to F320 */
      HF2.imag.EvEz[i] = *pData++;      /* F15 5_on to 0_on & 7_off to 2_off */
    }
  }


  CasHfrCalibration_XtractCrossSign (pRec);

  nLength = (pData - (pRec->data + CasHfrCal_HeaderLength));

  return nLength;
}

/* ************************************************************************* */

int CasHfrCalibration_XtractCrossSign (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Uchar *pData;
  Ulong i, j; /* , nLength = 0; */
  Ulong nIdx; /* , nBit; */


  pData = pMp + 12;                     /* header terminated with 0xCC */
  pData += 0x350;                       /* agc byte count */
  pData += 0xBEC;                       /* auto byte count */
  pData += 0xBEC;                       /* cross byte count */

  /*
   * Imaginary Sign Bits 
   */
  if (CasHfrCal_bASelected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 12; i++) {
      for (j = 0; j < 16; j++)
        A.isign.EuEw[nIdx + j] = cross_sign_bit (NULL);
      for (j = 0; j < 16; j++)
        A.isign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      nIdx += 16;
    }
    pData += 48;
  }
  /*
   * Band A 
   */
  if (CasHfrCal_bBSelected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 12; i++) {
      for (j = 0; j < 16; j++)
        B.isign.EuEw[nIdx + j] = cross_sign_bit (NULL);
      for (j = 0; j < 16; j++)
        B.isign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      nIdx += 16;
    }
    pData += 48;
  }
  /*
   * Band B 
   */
  if (CasHfrCal_bCSelected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 12; i++) {
      for (j = 0; j < 16; j++)
        C.isign.EuEw[nIdx + j] = cross_sign_bit (NULL);
      for (j = 0; j < 16; j++)
        C.isign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      nIdx += 16;
    }
    pData += 48;
  }
  /*
   * Band C 
   */
  if (CasHfrCal_bHF1Selected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 2; i++) {
      for (j = 0; j < 16; j++) {
        HF1.isign.EuEw[nIdx + j] = cross_sign_bit (NULL);
        HF1.isign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      }
      nIdx += 16;
    }

    HF1.isign.EuEw[nIdx] = cross_sign_bit (NULL);
    HF1.isign.EvEz[nIdx] = cross_sign_bit (NULL);
    ++nIdx;

    for (i = 0; i < 2; i++)
      HF1.isign.EuEw[nIdx + i] = cross_sign_bit (NULL);
    for (i = 0; i < 2; i++)
      HF1.isign.EvEz[nIdx + i] = cross_sign_bit (NULL);
    nIdx += i;

    for (i = 0; i < 4; i++)
      HF1.isign.EuEw[nIdx + i] = cross_sign_bit (NULL);
    for (i = 0; i < 4; i++)
      HF1.isign.EvEz[nIdx + i] = cross_sign_bit (NULL);
    nIdx += i;

    for (i = 0; i < 8; i++)
      HF1.isign.EuEw[nIdx + i] = cross_sign_bit (NULL);
    for (i = 0; i < 8; i++)
      HF1.isign.EvEz[nIdx + i] = cross_sign_bit (NULL);
    nIdx += i;

    pData += 12;
  }
  /*
   * Band HF1 
   */
  if (CasHfrCal_bHF2Selected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 2; i++) {
      for (j = 0; j < 64; j++) {
        HF2.isign.EuEw[nIdx + j] = cross_sign_bit (NULL);
        HF2.isign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      }
      nIdx += j;
    }
    for (j = 0; j < 12; j++) {
      HF2.isign.EuEw[nIdx + j] = cross_sign_bit (NULL);
      HF2.isign.EvEz[nIdx + j] = cross_sign_bit (NULL);
    }

    pData += 35;
  }


  /*
   * Band H2 
   */
  pData = pMp + 12;                     /* header terminated with 0xCC */
  pData += 0x350;                       /* agc byte count */
  pData += 0xBEC;                       /* auto byte count */
  pData += 0xBEC;                       /* cross byte count */
  pData += 0xBF;                        /* imag sign byte count */

  /*
   * Real Sign Bits 
   */
  if (CasHfrCal_bASelected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 12; i++) {
      for (j = 0; j < 16; j++)
        A.rsign.EuEw[nIdx + j] = cross_sign_bit (NULL);
      for (j = 0; j < 16; j++)
        A.rsign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      nIdx += 16;
    }
    pData += 48;
  }
  /*
   * Band A 
   */
  if (CasHfrCal_bBSelected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 12; i++) {
      for (j = 0; j < 16; j++)
        B.rsign.EuEw[nIdx + j] = cross_sign_bit (NULL);
      for (j = 0; j < 16; j++)
        B.rsign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      nIdx += 16;
    }
    pData += 48;
  }
  /*
   * Band B 
   */
  if (CasHfrCal_bCSelected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 12; i++) {
      for (j = 0; j < 16; j++)
        C.rsign.EuEw[nIdx + j] = cross_sign_bit (NULL);
      for (j = 0; j < 16; j++)
        C.rsign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      nIdx += 16;
    }
    pData += 48;
  }
  /*
   * Band C 
   */
  if (CasHfrCal_bHF1Selected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 2; i++) {
      for (j = 0; j < 16; j++) {
        HF1.rsign.EuEw[nIdx + j] = cross_sign_bit (NULL);
        HF1.rsign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      }
      nIdx += 16;
    }

    HF1.rsign.EuEw[nIdx] = cross_sign_bit (NULL);
    HF1.rsign.EvEz[nIdx] = cross_sign_bit (NULL);
    ++nIdx;

    for (i = 0; i < 2; i++)
      HF1.rsign.EuEw[nIdx + i] = cross_sign_bit (NULL);
    for (i = 0; i < 2; i++)
      HF1.rsign.EvEz[nIdx + i] = cross_sign_bit (NULL);
    nIdx += i;

    for (i = 0; i < 4; i++)
      HF1.rsign.EuEw[nIdx + i] = cross_sign_bit (NULL);
    for (i = 0; i < 4; i++)
      HF1.rsign.EvEz[nIdx + i] = cross_sign_bit (NULL);
    nIdx += i;

    for (i = 0; i < 8; i++)
      HF1.rsign.EuEw[nIdx + i] = cross_sign_bit (NULL);
    for (i = 0; i < 8; i++)
      HF1.rsign.EvEz[nIdx + i] = cross_sign_bit (NULL);
    nIdx += i;

    pData += 12;
  }
  /*
   * Band HF1 
   */
  if (CasHfrCal_bHF2Selected (pMp) == true) {
    nIdx = 0;
    cross_sign_bit (pData);
    for (i = 0; i < 2; i++) {
      for (j = 0; j < 64; j++) {
        HF2.rsign.EuEw[nIdx + j] = cross_sign_bit (NULL);
        HF2.rsign.EvEz[nIdx + j] = cross_sign_bit (NULL);
      }
      nIdx += j;
    }
    for (j = 0; j < 12; j++) {
      HF2.rsign.EuEw[nIdx + j] = cross_sign_bit (NULL);
      HF2.rsign.EvEz[nIdx + j] = cross_sign_bit (NULL);
    }

    pData += 35;
  }
  /*
   * Band H2 
   */
  return 0;
}



/* ************************************************************************* */
/* Returns +1 or -1 */
int8_t cross_sign_bit (Uchar * p)
{
  int8_t nSign;
  static Uchar *pByte;
  static Ulong nBit;

  if (p) {                              /* reset conditions */
    pByte = p;
    nBit = 1;
    return 0;
  }
  nSign = (*pByte & nBit) ? -1 : +1;
  nBit <<= 1;
  if (nBit > 129) {
    ++pByte;
    nBit = 1;
  }

  return nSign;
}

/* ************************************************************************* */

void CasHfrCalibration_dBMagnitudePhase (void)
{
  int i, bands, nEnd;
  HfrCalMode *p;

  for (bands = 0; bands < 5; bands++) {
    switch (bands) {
     case 0:
       p = &A;
       nEnd = 12 * 16;
       break;
     case 1:
       p = &B;
       nEnd = 12 * 16;
       break;
     case 2:
       p = &C;
       nEnd = 12 * 16;
       break;
     case 3:
       p = &HF1;
       nEnd = 2 * 16 + 15;
       break;
     default:
       p = &HF2;
       nEnd = 2 * 64 + 12;
       break;
    }

    for (i = 0; i < nEnd; i++) {
      /*
       * gcc 2.95.2 erronous warnings, foo(Uchar) proto and argument 
       */
      p->AutoMag.Eu[i] = CasHfrCalibration_AutoMag (p->acor.Eu[i]);
      p->AutoMag.Ew[i] = CasHfrCalibration_AutoMag (p->acor.Ew[i]);
      p->AutoMag.Ev[i] = CasHfrCalibration_AutoMag (p->acor.Ev[i]);
      p->AutoMag.Ez[i] = CasHfrCalibration_AutoMag (p->acor.Ez[i]);

      p->CrossMag.EuEw[i] = CasHfrCalibration_CrossMag (p->rmag.EuEw[i],
                                                        p->imag.EuEw[i]);
      p->CrossPhs.EuEw[i] =
        CasHfrCalibration_CrossPhs (p->rmag.EuEw[i], p->imag.EuEw[i],
                                    p->rsign.EuEw[i], p->isign.EuEw[i]);
      p->CrossMag.EvEz[i] =
        CasHfrCalibration_CrossMag (p->rmag.EvEz[i], p->imag.EvEz[i]);
      p->CrossPhs.EvEz[i] =
        CasHfrCalibration_CrossPhs (p->rmag.EvEz[i], p->imag.EvEz[i],
                                    p->rsign.EvEz[i], p->isign.EvEz[i]);
    }
  }


  /*
   * Calibrate Bands ABC 
   */

  return;
}



float CasHfrCalibration_AutoMag (Uchar acor)
{
 /* Uchar m, e;  */
 /* double dMag; */

  return (float) acor;
  /*
   * e=m=acor;
   * e>>=3;  m&=0x07;
   * dMag=((Ulong)1<<e)*(m+8);
   * dMag=10*log10(dMag); 
   * 
   * return dMag;
   */
}



float CasHfrCalibration_CrossMag (Uchar creal, Uchar cimag)
{
  Uchar m, e;
  double nr, ni;
  double dMag;

  Uchar mag;
  int i;


  e = m = creal;
  e >>= 3;
  m &= 0x07;
  nr = ((Ulong) 1 << e) * ((Ulong)(m + 8));
  /*
   * nr*=sreal;
   */
  e = m = cimag;
  e >>= 3;
  m &= 0x07;
  ni = ((Ulong) 1 << e) * ((Ulong)(m + 8));
  /*
   * ni*=simag;
   */
  dMag = sqrt (nr * nr + ni * ni);

/*  dMag=10*log10(dMag);*/

/* 
  solving for (2^e)*(m+8) 
*/
  frexp (dMag, &i);
  if(i<4){ fprintf(stderr, "CasHfrCalibration_CrossMag, exponent underflow, %d\n", i); exit(15);}
  e = (Uchar)(i - 4);
  dMag = dMag / ((Ulong) 1 << e);
  dMag = dMag - 8.0;
  m = (Uchar) dMag;
  mag = (Uchar) ((e << 3) | (m & 0x07));
  dMag = mag;


  return (float)dMag;
}



float CasHfrCalibration_CrossPhs (Uchar creal, Uchar cimag, char sreal,
                                   char simag)
{
  Uchar m, e;
  double nr, ni;
  double dPhs;

  e = m = creal;
  e >>= 3;
  m &= 0x07;
  nr = ((Ulong)1 << e) * (m + 8);
  nr *= sreal;
  e = m = cimag;
  e >>= 3;
  m &= 0x07;
  ni = ((Ulong)1 << e) * (m + 8);
  ni *= simag;
  dPhs = atan2 (ni, nr) * 180 / 3.141593;

  return (float)dPhs;
}



void CasHfrCalibration_DumpRaw (FILE * h)
{

/*
typedef struct hfrcal_raw_mode{
  cHfrCalAnt agc,acor;
  cHfrCalDf rmag,rsign,imag,isign;
  fHfrCalAnt AutoMag;
  fHfrCalDf  CrossMag,CrossPhs;
}HfrCalMode;                              
*/

/*
*/
  CasHfrCalibration_DumpRawABC (h);
  CasHfrCalibration_DumpRawHF1 (h);
  CasHfrCalibration_DumpRawHF2 (h);

  return;
}

void CasHfrCalibration_DumpRawABC (FILE * h)
{
  const char *sStimSig[12] = { "5_on", "4_on", "3_on", "2_on", "1_on", "0_on",
    "7_off", "6_off", "5_off", "4_off", "3_off", "2_off"
  };

/* const char *sAnt[4]={"Eu","Ev","Ew","Ez"};*/
  int i, j, nBnd;
  cHfrCalAnt *pAgc, *pAuto;
  cHfrCalDf *pRmag, *pImag;
  scHfrCalDf *pRsign, *pIsign;


  for (nBnd = 0; nBnd < 3; nBnd++) {
    switch (nBnd) {
     case 0:
       fprintf (h, "Band A:\n");
       pAgc = &A.agc;
       pAuto = &A.acor;
       pRmag = &A.rmag;
       pImag = &A.imag;
       pRsign = &A.rsign;
       pIsign = &A.isign;
       break;
     case 1:
       fprintf (h, "Band B:\n");
       pAgc = &B.agc;
       pAuto = &B.acor;
       pRmag = &B.rmag;
       pImag = &B.imag;
       pRsign = &B.rsign;
       pIsign = &B.isign;
       break;
     case 2:
       fprintf (h, "Band C:\n");
       pAgc = &C.agc;
       pAuto = &C.acor;
       pRmag = &C.rmag;
       pImag = &C.imag;
       pRsign = &C.rsign;
       pIsign = &C.isign;
       break;
    }

    fprintf (h, "AGCs  5_on  4_on  3_on  2_on  1_on  0_on  ");
    fprintf (h, "7_off 6_off 5_off 4_off 3_off 2_off\n");
    fprintf (h, "      ");
    for (i = 0; i < 6; i++)
      fprintf (h, "----  ");
    for (i = 6; i < 12; i++)
      fprintf (h, "----- ");
    fprintf (h, "\n");
    fprintf (h, "  Eu   ");
    for (i = 0; i < 6; i++)
      fprintf (h, "%02X    ", pAgc->Eu[i]);
    for (i = 6; i < 12; i++)
      fprintf (h, "%02X    ", pAgc->Eu[i]);
    fprintf (h, "\n  Ev   ");
    for (i = 0; i < 6; i++)
      fprintf (h, "%02X    ", pAgc->Ev[i]);
    for (i = 6; i < 12; i++)
      fprintf (h, "%02X    ", pAgc->Ev[i]);
    fprintf (h, "\n  Ew   ");
    for (i = 0; i < 6; i++)
      fprintf (h, "%02X    ", pAgc->Ew[i]);
    for (i = 6; i < 12; i++)
      fprintf (h, "%02X    ", pAgc->Ew[i]);
    fprintf (h, "\n  Ez   ");
    for (i = 0; i < 6; i++)
      fprintf (h, "%02X    ", pAgc->Ez[i]);
    for (i = 6; i < 12; i++)
      fprintf (h, "%02X    ", pAgc->Ez[i]);

    fprintf (h, "\n\n");



    /*
     * Autos 
     */
    fprintf (h, "AUTOs Eu    ");
    for (i = 0; i < 16; i++)
      fprintf (h, "%2d  ", i);
    fprintf (h, "\n%12s", " ");
    for (i = 0; i < 16; i++)
      fprintf (h, "--  ");
    fprintf (h, "\n");
    for (i = 0; i < 12; i++) {
      fprintf (h, "  %-10s", sStimSig[i]);
      for (j = i * 16; j < i * 16 + 16; j++)
        fprintf (h, "%02X  ", pAuto->Eu[j]);
      fprintf (h, "\n");
    }
    fprintf (h, "\n");

    fprintf (h, "AUTOs Ev    ");
    for (i = 0; i < 16; i++)
      fprintf (h, "%2d  ", i);
    fprintf (h, "\n%12s", " ");
    for (i = 0; i < 16; i++)
      fprintf (h, "--  ");
    fprintf (h, "\n");
    for (i = 0; i < 12; i++) {
      fprintf (h, "  %-10s", sStimSig[i]);
      for (j = i * 16; j < i * 16 + 16; j++)
        fprintf (h, "%02X  ", pAuto->Ev[j]);
      fprintf (h, "\n");
    }
    fprintf (h, "\n");

    fprintf (h, "AUTOs Ew    ");
    for (i = 0; i < 16; i++)
      fprintf (h, "%2d  ", i);
    fprintf (h, "\n%12s", " ");
    for (i = 0; i < 16; i++)
      fprintf (h, "--  ");
    fprintf (h, "\n");
    for (i = 0; i < 12; i++) {
      fprintf (h, "  %-10s", sStimSig[i]);
      for (j = i * 16; j < i * 16 + 16; j++)
        fprintf (h, "%02X  ", pAuto->Ew[j]);
      fprintf (h, "\n");
    }
    fprintf (h, "\n");

    fprintf (h, "AUTOs Ez    ");
    for (i = 0; i < 16; i++)
      fprintf (h, "%2d  ", i);
    fprintf (h, "\n%12s", " ");
    for (i = 0; i < 16; i++)
      fprintf (h, "--  ");
    fprintf (h, "\n");
    for (i = 0; i < 12; i++) {
      fprintf (h, "  %-10s", sStimSig[i]);
      for (j = i * 16; j < i * 16 + 16; j++)
        fprintf (h, "%02X  ", pAuto->Ez[j]);
      fprintf (h, "\n");
    }
    fprintf (h, "\n");


    /*
     * Cross Magnitude 
     */
    fprintf (h, "Rcross EuEw ");
    for (i = 0; i < 16; i++)
      fprintf (h, "%2d  ", i);
    fprintf (h, "\n%12s", " ");
    for (i = 0; i < 16; i++)
      fprintf (h, "--  ");
    fprintf (h, "\n");
    for (i = 0; i < 12; i++) {
      fprintf (h, "  %-10s", sStimSig[i]);
      for (j = i * 16; j < i * 16 + 16; j++)
        fprintf (h, "%02X  ", pRmag->EuEw[j]);
      fprintf (h, "\n");
    }
    fprintf (h, "\n");

    fprintf (h, "Rcross EvEz ");
    for (i = 0; i < 16; i++)
      fprintf (h, "%2d  ", i);
    fprintf (h, "\n%12s", " ");
    for (i = 0; i < 16; i++)
      fprintf (h, "--  ");
    fprintf (h, "\n");
    for (i = 0; i < 12; i++) {
      fprintf (h, "  %-10s", sStimSig[i]);
      for (j = i * 16; j < i * 16 + 16; j++)
        fprintf (h, "%02X  ", pRmag->EvEz[j]);
      fprintf (h, "\n");
    }
    fprintf (h, "\n");

    fprintf (h, "Icross EuEw ");
    for (i = 0; i < 16; i++)
      fprintf (h, "%2d  ", i);
    fprintf (h, "\n%12s", " ");
    for (i = 0; i < 16; i++)
      fprintf (h, "--  ");
    fprintf (h, "\n");
    for (i = 0; i < 12; i++) {
      fprintf (h, "  %-10s", sStimSig[i]);
      for (j = i * 16; j < i * 16 + 16; j++)
        fprintf (h, "%02X  ", pImag->EuEw[j]);
      fprintf (h, "\n");
    }
    fprintf (h, "\n");

    fprintf (h, "Icross EvEz ");
    for (i = 0; i < 16; i++)
      fprintf (h, "%2d  ", i);
    fprintf (h, "\n%12s", " ");
    for (i = 0; i < 16; i++)
      fprintf (h, "--  ");
    fprintf (h, "\n");
    for (i = 0; i < 12; i++) {
      fprintf (h, "  %-10s", sStimSig[i]);
      for (j = i * 16; j < i * 16 + 16; j++)
        fprintf (h, "%02X  ", pImag->EvEz[j]);
      fprintf (h, "\n");
    }
    fprintf (h, "\n");










  }                                     /* for band A,B,C */

  return;
}

void CasHfrCalibration_DumpRawHF1 (FILE * h)
{
  int i;
  cHfrCalAnt *pAgc, *pAuto;
  cHfrCalDf *pRmag, *pImag;
  scHfrCalDf *pRsign, *pIsign;

  pAgc = &HF1.agc;
  pAuto = &HF1.acor;
  pRmag = &HF1.rmag;
  pImag = &HF1.imag;
  pRsign = &HF1.rsign;
  pIsign = &HF1.isign;

  fprintf (h, "Band HF1: attenuator = ON and CAL level = 7\n");
  fprintf (h, "AGCs      ");
  for (i = 4; i < 20; i++)
    fprintf (h, "%2d  ", i);
  fprintf (h, "\n");
  fprintf (h, "          ");
  for (i = 4; i < 20; i++)
    fprintf (h, "--  ");
  fprintf (h, "\n");
  fprintf (h, "  Eu      ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pAgc->Eu[i]);
  fprintf (h, "\n  Ev      ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pAgc->Ev[i]);
  fprintf (h, "\n  Ew      ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pAgc->Ew[i]);
  fprintf (h, "\n  Ez      ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pAgc->Ez[i]);
  fprintf (h, "\n\n");

  fprintf (h, "AUTOs     ");
  for (i = 4; i < 20; i++)
    fprintf (h, "%2d  ", i);
  fprintf (h, "\n");
  fprintf (h, "          ");
  for (i = 4; i < 20; i++)
    fprintf (h, "--  ");
  fprintf (h, "\n");
  fprintf (h, "  Eu      ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pAuto->Eu[i]);
  fprintf (h, "\n  Ev      ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pAuto->Ev[i]);
  fprintf (h, "\n  Ew      ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pAuto->Ew[i]);
  fprintf (h, "\n  Ez      ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pAuto->Ez[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Cross EuEw");
  for (i = 4; i < 20; i++)
    fprintf (h, "%2d  ", i);
  fprintf (h, "\n");
  fprintf (h, "          ");
  for (i = 4; i < 20; i++)
    fprintf (h, "--  ");
  fprintf (h, "\n");
  fprintf (h, "  Rmag    ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pRmag->EuEw[i]);
  fprintf (h, "\n  Rsign   ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%+d  ", pRsign->EuEw[i]);
  fprintf (h, "\n  Imag    ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pImag->EuEw[i]);
  fprintf (h, "\n  Isign   ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%+d  ", pIsign->EuEw[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Cross EvEz");
  for (i = 4; i < 20; i++)
    fprintf (h, "%2d  ", i);
  fprintf (h, "\n");
  fprintf (h, "          ");
  for (i = 4; i < 20; i++)
    fprintf (h, "--  ");
  fprintf (h, "\n");
  fprintf (h, "  Rmag    ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pRmag->EvEz[i]);
  fprintf (h, "\n  Rsign   ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%+d  ", pRsign->EvEz[i]);
  fprintf (h, "\n  Imag    ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%02X  ", pImag->EvEz[i]);
  fprintf (h, "\n  Isign   ");
  for (i = 0; i < 16; i++)
    fprintf (h, "%+d  ", pIsign->EvEz[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Band HF1: attenuator = OFF and CAL level = 5\n");
  fprintf (h, "AGCs      ");
  for (i = 4; i < 20; i++)
    fprintf (h, "%2d  ", i);
  fprintf (h, "\n");
  fprintf (h, "          ");
  for (i = 4; i < 20; i++)
    fprintf (h, "--  ");
  fprintf (h, "\n");
  fprintf (h, "  Eu      ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pAgc->Eu[i]);
  fprintf (h, "\n  Ev      ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pAgc->Ev[i]);
  fprintf (h, "\n  Ew      ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pAgc->Ew[i]);
  fprintf (h, "\n  Ez      ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pAgc->Ez[i]);
  fprintf (h, "\n\n");

  fprintf (h, "AUTOs     ");
  for (i = 4; i < 20; i++)
    fprintf (h, "%2d  ", i);
  fprintf (h, "\n");
  fprintf (h, "          ");
  for (i = 4; i < 20; i++)
    fprintf (h, "--  ");
  fprintf (h, "\n");
  fprintf (h, "  Eu      ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pAuto->Eu[i]);
  fprintf (h, "\n  Ev      ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pAuto->Ev[i]);
  fprintf (h, "\n  Ew      ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pAuto->Ew[i]);
  fprintf (h, "\n  Ez      ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pAuto->Ez[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Cross EuEw");
  for (i = 4; i < 20; i++)
    fprintf (h, "%2d  ", i);
  fprintf (h, "\n");
  fprintf (h, "          ");
  for (i = 4; i < 20; i++)
    fprintf (h, "--  ");
  fprintf (h, "\n");
  fprintf (h, "  Rmag    ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pRmag->EuEw[i]);
  fprintf (h, "\n  Rsign   ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%+d  ", pRsign->EuEw[i]);
  fprintf (h, "\n  Imag    ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pImag->EuEw[i]);
  fprintf (h, "\n  Isign   ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%+d  ", pIsign->EuEw[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Cross EvEz");
  for (i = 4; i < 20; i++)
    fprintf (h, "%2d  ", i);
  fprintf (h, "\n");
  fprintf (h, "          ");
  for (i = 4; i < 20; i++)
    fprintf (h, "--  ");
  fprintf (h, "\n");
  fprintf (h, "  Rmag    ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pRmag->EvEz[i]);
  fprintf (h, "\n  Rsign   ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%+d  ", pRsign->EvEz[i]);
  fprintf (h, "\n  Imag    ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%02X  ", pImag->EvEz[i]);
  fprintf (h, "\n  Isign   ");
  for (i = 16; i < 32; i++)
    fprintf (h, "%+d  ", pIsign->EvEz[i]);
  fprintf (h, "\n\n");



  fprintf (h, "Band HF1: frequency = 475 kHz (F19)\n");
  fprintf (h, "Filters   ");
  fprintf (h, "%2d  ", 1);
  for (i = 0; i < 1; i++)
    fprintf (h, "  ");
  fprintf (h, "%2d  ", 2);
  for (i = 0; i < 4; i++)
    fprintf (h, "  ");
  fprintf (h, "%2d  ", 4);
  for (i = 0; i < 10; i++)
    fprintf (h, "  ");
  fprintf (h, "%2d  ", 8);
  fprintf (h, "\n");
  fprintf (h, "AGCs      ");
  fprintf (h, "--  ");
  for (i = 0; i < 1; i++)
    fprintf (h, "  ");
  fprintf (h, "--  ");
  for (i = 0; i < 4; i++)
    fprintf (h, "  ");
  fprintf (h, "--  ");
  for (i = 0; i < 10; i++)
    fprintf (h, "  ");
  fprintf (h, "--  ");
  fprintf (h, "\n");
  fprintf (h, "  Eu      ");
  fprintf (h, "%02X  ", pAgc->Eu[32]);
  for (i = 0; i < 1; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Eu[33]);
  for (i = 0; i < 4; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Eu[34]);
  for (i = 0; i < 10; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Eu[35]);
  fprintf (h, "\n");
  fprintf (h, "  Ev      ");
  fprintf (h, "%02X  ", pAgc->Ev[32]);
  for (i = 0; i < 1; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Ev[33]);
  for (i = 0; i < 4; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Ev[34]);
  for (i = 0; i < 10; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Ev[35]);
  fprintf (h, "\n");
  fprintf (h, "  Ew      ");
  fprintf (h, "%02X  ", pAgc->Ew[32]);
  for (i = 0; i < 1; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Ew[33]);
  for (i = 0; i < 4; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Ew[34]);
  for (i = 0; i < 10; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Ew[35]);
  fprintf (h, "\n");
  fprintf (h, "  Ez      ");
  fprintf (h, "%02X  ", pAgc->Ez[32]);
  for (i = 0; i < 1; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Ez[33]);
  for (i = 0; i < 4; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Ez[34]);
  for (i = 0; i < 10; i++)
    fprintf (h, "  ");
  fprintf (h, "%02X  ", pAgc->Ez[35]);
  fprintf (h, "\n");
  fprintf (h, "\n");

  fprintf (h, "AUTOs     ");
  fprintf (h, "--  ");
  for (i = 0; i < 3; i++)
    fprintf (h, "--");
  fprintf (h, "  ");
  for (i = 0; i < 7; i++)
    fprintf (h, "--");
  fprintf (h, "  ");
  for (i = 0; i < 15; i++)
    fprintf (h, "--");
  fprintf (h, "  ");
  fprintf (h, "\n");
  fprintf (h, "  Eu      ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%02X  ", pAuto->Eu[i]);
  fprintf (h, "\n  Ev      ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%02X  ", pAuto->Ev[i]);
  fprintf (h, "\n  Ew      ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%02X  ", pAuto->Ew[i]);
  fprintf (h, "\n  Ez      ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%02X  ", pAuto->Ez[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Cross EuEz");
  fprintf (h, "--  ");
  for (i = 0; i < 3; i++)
    fprintf (h, "--");
  fprintf (h, "  ");
  for (i = 0; i < 7; i++)
    fprintf (h, "--");
  fprintf (h, "  ");
  for (i = 0; i < 15; i++)
    fprintf (h, "--");
  fprintf (h, "  ");
  fprintf (h, "\n");
  fprintf (h, "  Rmag    ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%02X  ", pRmag->EuEw[i]);
  fprintf (h, "\n  Rsign   ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%+d  ", pRsign->EuEw[i]);
  fprintf (h, "\n  Imag    ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%02X  ", pImag->EuEw[i]);
  fprintf (h, "\n  Isign   ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%+d  ", pIsign->EuEw[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Cross EvEz");
  fprintf (h, "--  ");
  for (i = 0; i < 3; i++)
    fprintf (h, "--");
  fprintf (h, "  ");
  for (i = 0; i < 7; i++)
    fprintf (h, "--");
  fprintf (h, "  ");
  for (i = 0; i < 15; i++)
    fprintf (h, "--");
  fprintf (h, "  ");
  fprintf (h, "\n");
  fprintf (h, "  Rmag    ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%02X  ", pRmag->EvEz[i]);
  fprintf (h, "\n  Rsign   ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%+d  ", pRsign->EvEz[i]);
  fprintf (h, "\n  Imag    ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%02X  ", pImag->EvEz[i]);
  fprintf (h, "\n  Isign   ");
  for (i = 32; i < 47; i++)
    fprintf (h, "%+d  ", pIsign->EvEz[i]);
  fprintf (h, "\n\n");



  return;
}

void CasHfrCalibration_DumpRawHF2 (FILE * h)
{
  int i, j;
  cHfrCalAnt *pAgc, *pAuto;
  cHfrCalDf *pRmag, *pImag;
  scHfrCalDf *pRsign, *pIsign;

  pAgc = &HF2.agc;
  pAuto = &HF2.acor;
  pRmag = &HF2.rmag;
  pImag = &HF2.imag;
  pRsign = &HF2.rsign;
  pIsign = &HF2.isign;


  fprintf (h, "Band HF2: attenuator = ON  and CAL level = 7\n");
  for (j = 0; j < 4; j++) {
    fprintf (h, "          ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%3d ", i * 5 + 5);
    fprintf (h, "\n");
    fprintf (h, "AGCs      ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "--- ");
    fprintf (h, "\n");
    fprintf (h, "  Eu       ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pAgc->Eu[i]);
    fprintf (h, "\n  Ev       ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pAgc->Ev[i]);
    fprintf (h, "\n  Ew       ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pAgc->Ew[i]);
    fprintf (h, "\n  Ez       ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pAgc->Ez[i]);
    fprintf (h, "\n\n");
  }

  for (j = 0; j < 4; j++) {
    fprintf (h, "          ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%3d ", i * 5 + 5);
    fprintf (h, "\n");
    fprintf (h, "AUTOs     ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "--- ");
    fprintf (h, "\n");
    fprintf (h, "  Eu       ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pAuto->Eu[i]);
    fprintf (h, "\n  Ev       ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pAuto->Ev[i]);
    fprintf (h, "\n  Ew       ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pAuto->Ew[i]);
    fprintf (h, "\n  Ez       ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pAuto->Ez[i]);
    fprintf (h, "\n\n");
  }

  for (j = 0; j < 4; j++) {
    fprintf (h, "          ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%3d ", i * 5 + 5);
    fprintf (h, "\n");
    fprintf (h, "Cross EuEw");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "--- ");
    fprintf (h, "\n");
    fprintf (h, "  Rmag     ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pRmag->EuEw[i]);
    fprintf (h, "\n  Rsign    ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%+d  ", pRsign->EuEw[i]);
    fprintf (h, "\n  Imag     ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pImag->EuEw[i]);
    fprintf (h, "\n  Isign    ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%+d  ", pIsign->EuEw[i]);
    fprintf (h, "\n\n");
  }

  for (j = 0; j < 4; j++) {
    fprintf (h, "          ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%3d ", i * 5 + 5);
    fprintf (h, "\n");
    fprintf (h, "Cross EvEz");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "--- ");
    fprintf (h, "\n");
    fprintf (h, "  Rmag     ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pRmag->EvEz[i]);
    fprintf (h, "\n  Rsign    ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%+d  ", pRsign->EvEz[i]);
    fprintf (h, "\n  Imag     ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%02X  ", pImag->EvEz[i]);
    fprintf (h, "\n  Isign    ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%+d  ", pIsign->EvEz[i]);
    fprintf (h, "\n\n");
  }

  fprintf (h, "Band HF2: attenuator = OFF  and CAL level = 5\n");
  for (j = 0; j < 4; j++) {
    fprintf (h, "          ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%3d ", i * 5 + 5);
    fprintf (h, "\n");
    fprintf (h, "AGCs      ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "--- ");
    fprintf (h, "\n");
    fprintf (h, "  Eu       ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pAgc->Eu[i]);
    fprintf (h, "\n  Ev       ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pAgc->Ev[i]);
    fprintf (h, "\n  Ew       ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pAgc->Ew[i]);
    fprintf (h, "\n  Ez       ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pAgc->Ez[i]);
    fprintf (h, "\n\n");
  }

  for (j = 0; j < 4; j++) {
    fprintf (h, "          ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%3d ", i * 5 + 5);
    fprintf (h, "\n");
    fprintf (h, "AUTOs     ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "--- ");
    fprintf (h, "\n");
    fprintf (h, "  Eu       ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pAuto->Eu[i]);
    fprintf (h, "\n  Ev       ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pAuto->Ev[i]);
    fprintf (h, "\n  Ew       ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pAuto->Ew[i]);
    fprintf (h, "\n  Ez       ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pAuto->Ez[i]);
    fprintf (h, "\n\n");
  }

  for (j = 0; j < 4; j++) {
    fprintf (h, "          ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%3d ", i * 5 + 5);
    fprintf (h, "\n");
    fprintf (h, "Cross EuEw");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "--- ");
    fprintf (h, "\n");
    fprintf (h, "  Rmag     ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pRmag->EuEw[i]);
    fprintf (h, "\n  Rsign    ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%+d  ", pRsign->EuEw[i]);
    fprintf (h, "\n  Imag     ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pImag->EuEw[i]);
    fprintf (h, "\n  Isign    ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%+d  ", pIsign->EuEw[i]);
    fprintf (h, "\n\n");
  }

  for (j = 0; j < 4; j++) {
    fprintf (h, "          ");
    for (i = j * 16; i < j * 16 + 16; i++)
      fprintf (h, "%3d ", i * 5 + 5);
    fprintf (h, "\n");
    fprintf (h, "Cross EvEz");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "--- ");
    fprintf (h, "\n");
    fprintf (h, "  Rmag     ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pRmag->EvEz[i]);
    fprintf (h, "\n  Rsign    ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%+d  ", pRsign->EvEz[i]);
    fprintf (h, "\n  Imag     ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%02X  ", pImag->EvEz[i]);
    fprintf (h, "\n  Isign    ");
    for (i = j * 16 + 64; i < j * 16 + 64 + 16; i++)
      fprintf (h, "%+d  ", pIsign->EvEz[i]);
    fprintf (h, "\n\n");
  }



  fprintf (h, "Band HF2: frequency = 775 kHz (F15)\n");
  fprintf (h, "AGCs  5_on  4_on  3_on  2_on  1_on  0_on  ");
  fprintf (h, "7_off 6_off 5_off 4_off 3_off 2_off\n");
  fprintf (h, "      ");
  for (i = 128; i < 134; i++)
    fprintf (h, "----  ");
  for (i = 134; i < 140; i++)
    fprintf (h, "----- ");
  fprintf (h, "\n");
  fprintf (h, "  Eu   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pAgc->Eu[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pAgc->Eu[i]);
  fprintf (h, "\n  Ev   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pAgc->Ev[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pAgc->Ev[i]);
  fprintf (h, "\n  Ew   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pAgc->Ew[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pAgc->Ew[i]);
  fprintf (h, "\n  Ez   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pAgc->Ez[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pAgc->Ez[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Band HF2: frequency = 775 kHz (F15)\n");
  fprintf (h, "AUTOs 5_on  4_on  3_on  2_on  1_on  0_on  ");
  fprintf (h, "7_off 6_off 5_off 4_off 3_off 2_off\n");
  fprintf (h, "      ");
  for (i = 128; i < 134; i++)
    fprintf (h, "----  ");
  for (i = 134; i < 140; i++)
    fprintf (h, "----- ");
  fprintf (h, "\n");
  fprintf (h, "  Eu   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pAuto->Eu[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pAuto->Eu[i]);
  fprintf (h, "\n  Ev   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pAuto->Ev[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pAuto->Ev[i]);
  fprintf (h, "\n  Ew   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pAuto->Ew[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pAuto->Ew[i]);
  fprintf (h, "\n  Ez   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pAuto->Ez[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pAuto->Ez[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Band HF2 Cross EuEw: frequency = 775 kHz (F15)\n");
  fprintf (h, "       5_on  4_on  3_on  2_on  1_on  0_on  ");
  fprintf (h, "7_off 6_off 5_off 4_off 3_off 2_off\n");
  fprintf (h, "       ");
  for (i = 128; i < 134; i++)
    fprintf (h, "----  ");
  for (i = 134; i < 140; i++)
    fprintf (h, "----- ");
  fprintf (h, "\n Rmag   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pRmag->EuEw[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pRmag->EuEw[i]);
  fprintf (h, "\n Rsign  ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%+d    ", pRsign->EuEw[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%+d    ", pRsign->EuEw[i]);
  fprintf (h, "\n Imag   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pImag->EuEw[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pImag->EuEw[i]);
  fprintf (h, "\n Isign  ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%+d    ", pIsign->EuEw[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%+d    ", pIsign->EuEw[i]);
  fprintf (h, "\n\n");

  fprintf (h, "Band HF2 Cross EvEz: frequency = 775 kHz (F15)\n");
  fprintf (h, "       5_on  4_on  3_on  2_on  1_on  0_on  ");
  fprintf (h, "7_off 6_off 5_off 4_off 3_off 2_off\n");
  fprintf (h, "       ");
  for (i = 128; i < 134; i++)
    fprintf (h, "----  ");
  for (i = 134; i < 140; i++)
    fprintf (h, "----- ");
  fprintf (h, "\n Rmag   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pRmag->EvEz[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pRmag->EvEz[i]);
  fprintf (h, "\n Rsign  ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%+d    ", pRsign->EvEz[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%+d    ", pRsign->EvEz[i]);
  fprintf (h, "\n Imag   ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%02X    ", pImag->EvEz[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%02X    ", pImag->EvEz[i]);
  fprintf (h, "\n Isign  ");
  for (i = 128; i < 134; i++)
    fprintf (h, "%+d    ", pIsign->EvEz[i]);
  for (i = 134; i < 140; i++)
    fprintf (h, "%+d    ", pIsign->EvEz[i]);
  fprintf (h, "\n\n");

  return;
}


void CasHfrCalibration_Phase (void)
{
  Uchar m, e;
  int i;
  double nr, ni;

/*
typedef struct hfrcal_mode{
  cHfrCalAnt agc,acor;
  cHfrCalDf rmag,imag;
  scHfrCalDf rsign,isign;
  fHfrCalAnt AutoMag;
  fHfrCalDf  CrossMag,CrossPhs;
}HfrCalMode;                              
*/
  for (i = 0; i < 16 * 12; i++) {
    e = m = A.rmag.EuEw[i];
    e >>= 3;
    m &= 0x07;
    nr = ((Ulong) 1 << e) * (m + 8);
    nr *= A.rsign.EuEw[i];
    e = m = A.imag.EuEw[i];
    e >>= 3;
    m &= 0x07;
    ni = ((Ulong) 1 << e) * (m + 8);
    ni *= A.isign.EuEw[i];
    A.CrossPhs.EuEw[i] = (float) (atan2 (ni, nr) * 180 / 3.141593);

    e = m = A.rmag.EvEz[i];
    e >>= 3;
    m &= 0x07;
    nr = ((Ulong) 1 << e) * (m + 8);
    nr *= A.rsign.EvEz[i];
    e = m = A.imag.EvEz[i];
    e >>= 3;
    m &= 0x07;
    ni = ((Ulong) 1 << e) * (m + 8);
    ni *= A.isign.EvEz[i];
    A.CrossPhs.EvEz[i] = (float)(atan2 (ni, nr) * 180 / 3.141593);
    fprintf (stderr, "%3d = %7.1f   %7.1f\n", i, A.CrossPhs.EuEw[i],
             A.CrossPhs.EvEz[i]);
  }



  return;
}



void CasHfrCalibration_WritePdsFormatABC (FILE * h)
{
  char sBnd[16];
  int nBand, nAntenna, nFilter, nCalLvl, nIndex;

  const char *sStimSig[12] = { "5_on", "4_on", "3_on", "2_on", "1_on", "0_on",
    "7_off", "6_off", "5_off", "4_off", "3_off", "2_off"
  };
  float *pFlt;
  HfrCalMode *pBnd;

  for (nBand = 0; nBand < 3; nBand++) {
    if (nBand == 0) {       sprintf (sBnd, "Band A"); pBnd = &A; }
	 else if (nBand == 1) {  sprintf (sBnd, "Band B"); pBnd = &B; } 
	 else {                  sprintf (sBnd, "Band C"); pBnd = &C; }

    /*
     * agc values 
     */
    fprintf (h, "%s AGC\n", sBnd);
    fprintf (h, "%7s", " ");
    for (nCalLvl = 0; nCalLvl < 12; nCalLvl++)
      fprintf (h, "%-5s   ", sStimSig[nCalLvl]);
    fprintf (h, "\n");

    fprintf (h, "%6s", " ");
    for (nCalLvl = 0; nCalLvl < 12; nCalLvl++)
      fprintf (h, "------- ");
    fprintf (h, "\n");

    fprintf (h, "%-5s ", "Eu");
    for (nCalLvl = 0; nCalLvl < 12; nCalLvl++)
      fprintf (h, "%7.2f ", (double) pBnd->agc.Eu[nCalLvl]);
    fprintf (h, "\n");
    fprintf (h, "%-5s ", "Ev");
    for (nCalLvl = 0; nCalLvl < 12; nCalLvl++)
      fprintf (h, "%7.2f ", (double) pBnd->agc.Ev[nCalLvl]);
    fprintf (h, "\n");
    fprintf (h, "%-5s ", "Ew");
    for (nCalLvl = 0; nCalLvl < 12; nCalLvl++)
      fprintf (h, "%7.2f ", (double) pBnd->agc.Ew[nCalLvl]);
    fprintf (h, "\n");
    fprintf (h, "%-5s ", "Ez");
    for (nCalLvl = 0; nCalLvl < 12; nCalLvl++)
      fprintf (h, "%7.2f ", (double) pBnd->agc.Ez[nCalLvl]);
    fprintf (h, "\n\n");




    for (nAntenna = 0; nAntenna < 8; nAntenna++) {
      if (nAntenna == 0) {
        fprintf (h, "%s Eu Auto Magnitude\n", sBnd);
        pFlt = pBnd->AutoMag.Eu;
      } else if (nAntenna == 1) {
        fprintf (h, "%s Ew Auto Magnitude\n", sBnd);
        pFlt = pBnd->AutoMag.Ew;
      } else if (nAntenna == 2) {
        fprintf (h, "%s Ev Auto Magnitude\n", sBnd);
        pFlt = pBnd->AutoMag.Ev;
      } else if (nAntenna == 3) {
        fprintf (h, "%s Ez Auto Magnitude\n", sBnd);
        pFlt = pBnd->AutoMag.Ez;
      } else if (nAntenna == 4) {
        fprintf (h, "%s EuEw Cross Magnitude\n", sBnd);
        pFlt = pBnd->CrossMag.EuEw;
      } else if (nAntenna == 5) {
        fprintf (h, "%s EuEw Cross Phase\n", sBnd);
        pFlt = pBnd->CrossPhs.EuEw;
      } else if (nAntenna == 6) {
        fprintf (h, "%s EvEz Cross Magnitude\n", sBnd);
        pFlt = pBnd->CrossMag.EvEz;
      } else {
        fprintf (h, "%s EvEz Cross Phase\n", sBnd);
        pFlt = pBnd->CrossPhs.EvEz;
      }

      nIndex = 0;

      fprintf (h, "%6s", " ");
      for (nFilter = 0; nFilter < 16; nFilter++)
        fprintf (h, "%5d   ", nFilter);
      fprintf (h, "\n");

      fprintf (h, "%6s", " ");
      for (nFilter = 0; nFilter < 16; nFilter++)
        fprintf (h, "------- ");
      fprintf (h, "\n");

      for (nCalLvl = 0; nCalLvl < 12; nCalLvl++) {
        fprintf (h, "%-5s ", sStimSig[nCalLvl]);

        for (nFilter = 0; nFilter < 16; nFilter++)
          fprintf (h, "%7.2f ", pFlt[nIndex++]);
        fprintf (h, "\n");

      }                                 /* cal level */
      fprintf (h, "\n");

    }                                   /* antenna */
  }                                     /* bands abc */


  return;
}



void CasHfrCalibration_WritePdsFormatHF1 (FILE * h)
{
  Uchar *pUch;
  int nProduct, nFilter, nCalLvl, nIndex, nPad;
  float *pFlt;

  nIndex = 0;
  for (nCalLvl = 0; nCalLvl < 2; nCalLvl++) {
    if (nCalLvl == 0)
      fprintf (h, "Band HF1 Cal Level = 7 and Attenuator On\n");
    else
      fprintf (h, "Band HF1 Cal Level = 5 and Attenuator Off\n");

    fprintf (h, "%8s ", " ");
    for (nFilter = 4; nFilter < 20; nFilter++) fprintf (h, "%5d   ", nFilter);
    fprintf (h, "\n");

    fprintf (h, "%8s ", " ");
    for (nFilter = 4; nFilter < 20; nFilter++) fprintf (h, "------- ");
    fprintf (h, "\n");

    for (nProduct = 0; nProduct < 4; nProduct++) {
      switch (nProduct) {
       case 0:   fprintf (h, "AgcEu    ");  pUch = HF1.agc.Eu;  break;
       case 1:   fprintf (h, "AgcEw    ");  pUch = HF1.agc.Ew;  break;
       case 2:   fprintf (h, "AgcEv    ");  pUch = HF1.agc.Ev;  break;
       default:  fprintf (h, "AgcEz    ");  pUch = HF1.agc.Ez;  break;
      }
      for (nFilter = 0; nFilter < 16; nFilter++)
        fprintf (h, "%7.2f ", (float) pUch[nIndex + nFilter]);
      fprintf (h, "\n");
    }                                   /* product */
    for (nProduct = 0; nProduct < 8; nProduct++) {
      switch (nProduct) {
       case 0:   fprintf (h, "AutoEu   ");  pFlt = HF1.AutoMag.Eu;    break;
       case 1:   fprintf (h, "AutoEw   ");  pFlt = HF1.AutoMag.Ew;    break;
       case 2:   fprintf (h, "AutoEv   ");  pFlt = HF1.AutoMag.Ev;    break;
       case 3:   fprintf (h, "AutoEz   ");  pFlt = HF1.AutoMag.Ez;    break;
       case 4:   fprintf (h, "CmagEuEw ");  pFlt = HF1.CrossMag.EuEw; break;
       case 5:   fprintf (h, "CmagEvEz ");  pFlt = HF1.CrossMag.EvEz; break;
       case 6:   fprintf (h, "CphsEuEw ");  pFlt = HF1.CrossPhs.EuEw; break;
       default:  fprintf (h, "CphsEvEz ");  pFlt = HF1.CrossPhs.EvEz; break;
      }
      for (nFilter = 0; nFilter < 16; nFilter++)
        fprintf (h, "%7.2f ", pFlt[nIndex + nFilter]);
      fprintf (h, "\n");
    }                                   /* product */
    fprintf (h, "\n");
    nIndex += nFilter;
  }                                     /* calibration level */

  fprintf (h, "Band HF1 475 KHz (F 19)\n");

  /*
   * columns 13,28,55,106 
   */
  fprintf (h, "%*s1%*s2%*s4%*s8\n", 12, " ", 14, " ", 26, " ", 50, " ");

  /*
   * agc values 
   */
  fprintf (h, "%*s------%*s------%*s------%*s------\n", 10, " ", 10, " ", 20,
           " ", 45, " ");
  for (nProduct = 0; nProduct < 4; nProduct++) {
    switch (nProduct) {
     case 0:  fprintf (h, "AgcEu    "); pUch = HF1.agc.Eu;  break;
     case 1:  fprintf (h, "AgcEw    "); pUch = HF1.agc.Ew;  break;
     case 2:  fprintf (h, "AgcEv    "); pUch = HF1.agc.Ev;  break;
     default: fprintf (h, "AgcEz    "); pUch = HF1.agc.Ez;  break;
    }
    fprintf (h, "%7.2f", (float) pUch[nIndex + 0]);
    fprintf (h, "%9s%7.2f", " ", (float) pUch[nIndex + 1]);
    fprintf (h, "%19s%7.2f", " ", (float) pUch[nIndex + 2]);
    fprintf (h, "%44s%7.2f", " ", (float) pUch[nIndex + 3]);
    fprintf (h, "\n");
  }                                     /* agc */

  nPad = 4;
  fprintf (h, "%8s ", " ");
  for (nFilter = 0; nFilter < 8 - 1; nFilter++)
    fprintf (h, "-");
  fprintf (h, "%*s", nPad, " ");
  for (nFilter = 0; nFilter < 8 * 2 - 1; nFilter++)
    fprintf (h, "-");
  fprintf (h, "%*s", nPad, " ");
  for (nFilter = 0; nFilter < 8 * 4 - 1; nFilter++)
    fprintf (h, "-");
  fprintf (h, "%*s", nPad, " ");
  for (nFilter = 0; nFilter < 8 * 8 - 1; nFilter++)
    fprintf (h, "-");
  fprintf (h, "\n");

  for (nProduct = 0; nProduct < 8; nProduct++) {
    switch (nProduct) {
     case 0:   fprintf (h, "AutoEu   "); pFlt = HF1.AutoMag.Eu;     break;
     case 1:  fprintf (h, "AutoEw   ");  pFlt = HF1.AutoMag.Ew;     break;
     case 2:  fprintf (h, "AutoEv   ");  pFlt = HF1.AutoMag.Ev;     break;
     case 3:  fprintf (h, "AutoEz   ");  pFlt = HF1.AutoMag.Ez;     break;
     case 4:  fprintf (h, "CmagEuEw ");  pFlt = HF1.CrossMag.EuEw;  break;
     case 5:  fprintf (h, "CmagEvEz ");  pFlt = HF1.CrossMag.EvEz;  break;
     case 6:  fprintf (h, "CphsEuEw ");  pFlt = HF1.CrossPhs.EuEw;  break;
     default: fprintf (h, "CphsEvEz ");  pFlt = HF1.CrossPhs.EvEz;  break;
    }

    for (nFilter = 0; nFilter < 1; nFilter++)
      fprintf (h, "%7.2f ", pFlt[nIndex + nFilter]);
    fprintf (h, "%*s", nPad - 1, " ");

    for (nFilter = 1; nFilter < 3; nFilter++)
      fprintf (h, "%7.2f ", pFlt[nIndex + nFilter]);
    fprintf (h, "%*s", nPad - 1, " ");

    for (nFilter = 3; nFilter < 7; nFilter++)
      fprintf (h, "%7.2f ", pFlt[nIndex + nFilter]);
    fprintf (h, "%*s", nPad - 1, " ");

    for (nFilter = 7; nFilter < 15; nFilter++)
      fprintf (h, "%7.2f ", pFlt[nIndex + nFilter]);
    fprintf (h, "\n");

  }                                     /* product */
  fprintf (h, "\n");


  return;
}



void CasHfrCalibration_WritePdsFormatHF2 (FILE * h)
{
  Uchar *pUch;
  int nProduct, nFilter, nCalLvl, nIndex, nLoop;
  float *pFlt;

  nIndex = 0;
  for (nCalLvl = 0; nCalLvl < 2; nCalLvl++) {
    if (nCalLvl == 0)
      fprintf (h, "Band HF2 Cal Level = 7 and Attenuator On\n");
    else
      fprintf (h, "Band HF2 Cal Level = 5 and Attenuator Off\n");

    for (nLoop = 0; nLoop < 4; nLoop++) {
      nFilter = nLoop * 5 * 16 + 5;
      fprintf (h, "%8s ", " ");
      for (; nFilter < (nLoop + 1) * 5 * 16 + 5; nFilter += 5)
        fprintf (h, "%5d   ", nFilter);
      fprintf (h, "\n");

      fprintf (h, "%8s ", " ");
      for (nFilter = 0; nFilter < 16; nFilter++)
        fprintf (h, "------- ");
      fprintf (h, "\n");

      for (nProduct = 0; nProduct < 4; nProduct++) {
        switch (nProduct) {
         case 0:
           fprintf (h, "AgcEu    ");
           pUch = HF2.agc.Eu;
           break;
         case 1:
           fprintf (h, "AgcEw    ");
           pUch = HF2.agc.Ew;
           break;
         case 2:
           fprintf (h, "AgcEv    ");
           pUch = HF2.agc.Ev;
           break;
         default:
           fprintf (h, "AgcEz    ");
           pUch = HF2.agc.Ez;
           break;
        }
        for (nFilter = 0; nFilter < 16; nFilter++)
          fprintf (h, "%7.2f ", (float) pUch[nIndex + nFilter]);
        fprintf (h, "\n");
      }                                 /* product */

      for (nProduct = 0; nProduct < 8; nProduct++) {
        switch (nProduct) {
         case 0:
           fprintf (h, "AutoEu   ");
           pFlt = HF2.AutoMag.Eu;
           break;
         case 1:
           fprintf (h, "AutoEw   ");
           pFlt = HF2.AutoMag.Ew;
           break;
         case 2:
           fprintf (h, "AutoEv   ");
           pFlt = HF2.AutoMag.Ev;
           break;
         case 3:
           fprintf (h, "AutoEz   ");
           pFlt = HF2.AutoMag.Ez;
           break;
         case 4:
           fprintf (h, "CmagEuEw ");
           pFlt = HF2.CrossMag.EuEw;
           break;
         case 5:
           fprintf (h, "CmagEvEz ");
           pFlt = HF2.CrossMag.EvEz;
           break;
         case 6:
           fprintf (h, "CphsEuEw ");
           pFlt = HF2.CrossPhs.EuEw;
           break;
         default:
           fprintf (h, "CphsEvEz ");
           pFlt = HF2.CrossPhs.EvEz;
           break;
        }
        for (nFilter = 0; nFilter < 16; nFilter++)
          fprintf (h, "%7.2f ", pFlt[nIndex + nFilter]);
        fprintf (h, "\n");
      }                                 /* product */
      fprintf (h, "\n");
      nIndex += nFilter;
    }                                   /* split hfr steps into 4 groups of 16 */
  }                                     /* calibration level */




  fprintf (h, "Band HF2 775 KHz (F 15)\n");
  /*
   * columns 13, 
   */
  fprintf (h, "%10s", " ");
  fprintf (h, "%-8s%-8s%-8s%-8s%-8s%-8s",
           "5_on", "4_on", "3_on", "2_on", "1_on", "0_on");
  fprintf (h, "%4s", " ");
  fprintf (h, "%-8s%-8s%-8s%-8s%-8s%-8s\n",
           "7_off", "6_off", "5_off", "4_off", "3_off", "2_off");
  fprintf (h, "%9s", " ");
  for (nFilter = 0; nFilter < 6; nFilter++)
    fprintf (h, "------- ");
  fprintf (h, "    ");
  for (nFilter = 0; nFilter < 6; nFilter++)
    fprintf (h, "------- ");
  fprintf (h, "\n");

  for (nProduct = 0; nProduct < 4; nProduct++) {
    switch (nProduct) {
     case 0:
       fprintf (h, "AgcEu    ");
       pUch = HF2.agc.Eu;
       break;
     case 1:
       fprintf (h, "AgcEw    ");
       pUch = HF2.agc.Ew;
       break;
     case 2:
       fprintf (h, "AgcEv    ");
       pUch = HF2.agc.Ev;
       break;
     default:
       fprintf (h, "AgcEz    ");
       pUch = HF2.agc.Ez;
       break;
    }

    for (nFilter = 0; nFilter < 6; nFilter++)
      fprintf (h, "%7.2f ", (float) pUch[nIndex + nFilter]);
    fprintf (h, "    ");
    for (nFilter = 6; nFilter < 12; nFilter++)
      fprintf (h, "%7.2f ", (float) pUch[nIndex + nFilter]);
    fprintf (h, "\n");
  }

  for (nProduct = 0; nProduct < 8; nProduct++) {
    switch (nProduct) {
     case 0:
       fprintf (h, "AutoEu   ");
       pFlt = HF2.AutoMag.Eu;
       break;
     case 1:
       fprintf (h, "AutoEw   ");
       pFlt = HF2.AutoMag.Ew;
       break;
     case 2:
       fprintf (h, "AutoEv   ");
       pFlt = HF2.AutoMag.Ev;
       break;
     case 3:
       fprintf (h, "AutoEz   ");
       pFlt = HF2.AutoMag.Ez;
       break;
     case 4:
       fprintf (h, "CmagEuEw ");
       pFlt = HF2.CrossMag.EuEw;
       break;
     case 5:
       fprintf (h, "CmagEvEz ");
       pFlt = HF2.CrossMag.EvEz;
       break;
     case 6:
       fprintf (h, "CphsEuEw ");
       pFlt = HF2.CrossPhs.EuEw;
       break;
     default:
       fprintf (h, "CphsEvEz ");
       pFlt = HF2.CrossPhs.EvEz;
       break;
    }

    for (nFilter = 0; nFilter < 6; nFilter++)
      fprintf (h, "%7.2f ", pFlt[nIndex + nFilter]);
    fprintf (h, "    ");
    for (nFilter = 6; nFilter < 12; nFilter++)
      fprintf (h, "%7.2f ", pFlt[nIndex + nFilter]);
    fprintf (h, "\n");

  }                                     /* product */
  fprintf (h, "\n");


  return;
}
