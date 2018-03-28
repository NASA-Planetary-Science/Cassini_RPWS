#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasMfr.h>

#include "cal/CasMfrCal.h"              /* private access, calibration constant files */



typedef struct mfr_raw_data
{
  Uchar data[32];
  Ulong length;
} MfrRawData;

MfrRawData MfrRawBnd1[2], MfrRawBnd2[2], MfrRawBnd3[4];


typedef struct mfr_cal_data
{
  float data[32];
  float time[32];
  float freq[32];
  Ulong length;
} MfrCalData;

MfrCalData MfrCalBnd1[2], MfrCalBnd2[2], MfrCalBnd3[4];






/*
static double CasMfr_Frq[3][32]={
static double CasMfr_Bandwidth[3][32]={
static double CasMfr_Bx_Vrms2nT[3][32]={
static double CasMfr_Bz_Vrms2nT[3][32]={
static double CasMfr_B1S01_Dn2Vrms[256]=

void CasMfr_Init(void);
int CasMfr_XtractData(CasRecord *pRec);
int CasMfr_Calibrate(CasRecord *pRec);
int CasMfr_GetPhysicalUnits(CasRecord *pRec,unsigned long nMode,
                            float *pTime,float *pFreq,float *pAmpl);
char *CasMfr_sMiniPacketHeader(CasRecord *pRec);
*/

static float CasMfr_Dn2Vrms[3][32][256];

static float CasMfrEu_Vrms_to_EleFld[3][32];
static float CasMfrEv_Vrms_to_EleFld[3][32];
static float CasMfrEx_Vrms_to_EleFld[3][32];
static float CasMfrEz_Vrms_to_EleFld[3][32];
static float CasMfrBx_Vrms_to_nTelsa[3][32];
static float CasMfrBz_Vrms_to_nTelsa[3][32];



int CasMfr_GetPhysicalUnits (CasRecord * pRec, unsigned long nMode,
                             float *pTime, float *pFreq, float *pAmpl)
{
  bool bNewData = false;                /* assume the data is calibrated */
  bool bFastToggle;
  Uchar *pMp = pRec->data;
  int nIdx, nLength = 0;
  int nBand, nStep, nSweep;
  Ulong nAnt;
  float (*pXfact1)[32], (*pXfact2)[32]; /* Vrms->nT or Vrms->V/m */
  static Uchar arMp[32];
  static Ulong n1stAnt, n2ndAnt;


  if (CasMfr_bMfrPacket (pMp) == false)
    return nLength;

  for (nIdx = 0; nIdx < 5; nIdx++) {
    if (arMp[nIdx] != pMp[nIdx])
      bNewData = true;
    arMp[nIdx] = pMp[nIdx];
  }


  /*
   * mfr packet header was redefined with the flight software upload, ver2.4
   * on June 1, 2000: 2000-153T08:06:32 (0x4FC87A44).
   */
  if (bNewData == true) {
    if (pRec->status.cds_time < CasMfr_MagicDate)
      bFastToggle = false;
    else
      bFastToggle = CasMfr_bFastToggle (pMp);

    if (bFastToggle == true) {
      switch (CasMfr_n1stAnt (pMp)) {
       case 0x00:
         n1stAnt = CasAntEx;
         pXfact1 = CasMfrEx_Vrms_to_EleFld;
         break;
       case 0x01:
         n1stAnt = CasAntEz;
         pXfact1 = CasMfrEz_Vrms_to_EleFld;
         break;
       case 0x02:
         n1stAnt = CasAntBx;
         pXfact1 = CasMfrBx_Vrms_to_nTelsa;
         break;
       case 0x03:
         n1stAnt = CasAntBz;
         pXfact1 = CasMfrBz_Vrms_to_nTelsa;
         break;
       default:
         assert (0);
      }
      switch (CasMfr_n2ndAnt (pMp)) {
       case 0x00:
         n2ndAnt = CasAntEx;
         pXfact2 = CasMfrEx_Vrms_to_EleFld;
         break;
       case 0x01:
         n2ndAnt = CasAntEz;
         pXfact2 = CasMfrEz_Vrms_to_EleFld;
         break;
       case 0x02:
         n2ndAnt = CasAntBx;
         pXfact2 = CasMfrBx_Vrms_to_nTelsa;
         break;
       case 0x03:
         n2ndAnt = CasAntBz;
         pXfact2 = CasMfrBz_Vrms_to_nTelsa;
         break;
       default:
         assert (0);
      }
    } else {                            /* normal mfr sweep */
      switch (CasMfr_nAntenna (pMp)) {
       case 0x00:
         n1stAnt = CasAntEx;
         pXfact1 = CasMfrEx_Vrms_to_EleFld;
         break;
       case 0x01:
         n1stAnt = CasAntEz;
         pXfact1 = CasMfrEz_Vrms_to_EleFld;
         break;
       case 0x02:
         n1stAnt = CasAntBx;
         pXfact1 = CasMfrBx_Vrms_to_nTelsa;
         break;
       case 0x03:
         n1stAnt = CasAntBz;
         pXfact1 = CasMfrBz_Vrms_to_nTelsa;
         break;
       default:
         assert (0);
      }
      n2ndAnt = n1stAnt;
      pXfact2 = pXfact1;
    }

    /*
     * Extract the data, by making some fatal assumptions:
     * 1. band 1 has 2 sweeps 16 bytes each
     * 2. band 2 has 2 sweeps 32 bytes each
     * 3. band 3 has 4 sweeps 32 bytes each 
     * Convert from data numbers to Vrms
     */
    pMp += 5;                           /* mp header ends at byte 5 */
    for (nStep = 0; nStep < 16; nStep++) {
      MfrRawBnd1[0].data[nStep] = *pMp;
      MfrCalBnd1[0].data[nStep] = CasMfr_Dn2Vrms[0][nStep][*pMp++];
    }
    for (nStep = 0; nStep < 16; nStep++) {
      MfrRawBnd1[1].data[nStep] = *pMp;
      MfrCalBnd1[1].data[nStep] = CasMfr_Dn2Vrms[0][nStep][*pMp++];
    }

    for (nStep = 0; nStep < 32; nStep++) {
      MfrRawBnd2[0].data[nStep] = *pMp;
      MfrCalBnd2[0].data[nStep] = CasMfr_Dn2Vrms[1][nStep][*pMp++];
    }
    for (nStep = 0; nStep < 32; nStep++) {
      MfrRawBnd2[1].data[nStep] = *pMp;
      MfrCalBnd2[1].data[nStep] = CasMfr_Dn2Vrms[1][nStep][*pMp++];
    }

    for (nStep = 0; nStep < 32; nStep++) {
      MfrRawBnd3[0].data[nStep] = *pMp;
      MfrCalBnd3[0].data[nStep] = CasMfr_Dn2Vrms[2][nStep][*pMp++];
    }
    for (nStep = 0; nStep < 32; nStep++) {
      MfrRawBnd3[1].data[nStep] = *pMp;
      MfrCalBnd3[1].data[nStep] = CasMfr_Dn2Vrms[2][nStep][*pMp++];
    }
    for (nStep = 0; nStep < 32; nStep++) {
      MfrRawBnd3[2].data[nStep] = *pMp;
      MfrCalBnd3[2].data[nStep] = CasMfr_Dn2Vrms[2][nStep][*pMp++];
    }
    for (nStep = 0; nStep < 32; nStep++) {
      MfrRawBnd3[3].data[nStep] = *pMp;
      MfrCalBnd3[3].data[nStep] = CasMfr_Dn2Vrms[2][nStep][*pMp++];
    }

    /*
     * MfrCalBndx should in units of Vrms: length, time, and freq are assigned
     * in the init() routine.  
     * 1.  Convert from Vrms to either nTelsa or V/m.  
     * 2.  Square either Vrms or nTesla => Vrms^2/m^2 or nT^2
     * 3.  Divide by the bandwidth, Vrms^2/m^2/Hz or nT^2/Hz 
     */

    nBand = 0;
    nSweep = 0;
    for (nStep = 0; nStep < 16; nStep++) {
      MfrCalBnd1[nSweep].data[nStep] *= pXfact1[nBand][nStep];
      MfrCalBnd1[nSweep].data[nStep] *= MfrCalBnd1[nSweep].data[nStep];
      MfrCalBnd1[nSweep].data[nStep] /= CasMfr_Bandwidth[nBand][nStep];
    }
    nBand = 0;
    nSweep = 1;
    for (nStep = 0; nStep < 16; nStep++) {
      MfrCalBnd1[nSweep].data[nStep] *= pXfact2[nBand][nStep];
      MfrCalBnd1[nSweep].data[nStep] *= MfrCalBnd1[nSweep].data[nStep];
      MfrCalBnd1[nSweep].data[nStep] /= CasMfr_Bandwidth[nBand][nStep];
    }

    nBand = 1;
    nSweep = 0;
    for (nStep = 0; nStep < 32; nStep++) {
      MfrCalBnd2[nSweep].data[nStep] *= pXfact1[nBand][nStep];
      MfrCalBnd2[nSweep].data[nStep] *= MfrCalBnd2[nSweep].data[nStep];
      MfrCalBnd2[nSweep].data[nStep] /= CasMfr_Bandwidth[nBand][nStep];
    }
    nBand = 1;
    nSweep = 1;
    for (nStep = 0; nStep < 32; nStep++) {
      MfrCalBnd2[nSweep].data[nStep] *= pXfact2[nBand][nStep];
      MfrCalBnd2[nSweep].data[nStep] *= MfrCalBnd2[nSweep].data[nStep];
      MfrCalBnd2[nSweep].data[nStep] /= CasMfr_Bandwidth[nBand][nStep];
    }

    nBand = 2;
    nSweep = 0;
    for (nStep = 0; nStep < 32; nStep++) {
      MfrCalBnd3[nSweep].data[nStep] *= pXfact1[nBand][nStep];
      MfrCalBnd3[nSweep].data[nStep] *= MfrCalBnd3[nSweep].data[nStep];
      MfrCalBnd3[nSweep].data[nStep] /= CasMfr_Bandwidth[nBand][nStep];
    }
    nBand = 2;
    nSweep = 1;
    for (nStep = 0; nStep < 32; nStep++) {
      MfrCalBnd3[nSweep].data[nStep] *= pXfact1[nBand][nStep];
      MfrCalBnd3[nSweep].data[nStep] *= MfrCalBnd3[nSweep].data[nStep];
      MfrCalBnd3[nSweep].data[nStep] /= CasMfr_Bandwidth[nBand][nStep];
    }
    nBand = 2;
    nSweep = 2;
    for (nStep = 0; nStep < 32; nStep++) {
      MfrCalBnd3[nSweep].data[nStep] *= pXfact2[nBand][nStep];
      MfrCalBnd3[nSweep].data[nStep] *= MfrCalBnd3[nSweep].data[nStep];
      MfrCalBnd3[nSweep].data[nStep] /= CasMfr_Bandwidth[nBand][nStep];
    }
    nBand = 2;
    nSweep = 3;
    for (nStep = 0; nStep < 32; nStep++) {
      MfrCalBnd3[nSweep].data[nStep] *= pXfact2[nBand][nStep];
      MfrCalBnd3[nSweep].data[nStep] *= MfrCalBnd3[nSweep].data[nStep];
      MfrCalBnd3[nSweep].data[nStep] /= CasMfr_Bandwidth[nBand][nStep];
    }

  }


  /*
   * xtract and calibrate the new data 
   */
  /*
   * just ftech the calibrated data 
   */
  /*
   * make a union with the data we have and the data we want 
   */
  nLength = 0;
  nMode &= (CasMfr_Band123 | n1stAnt | n2ndAnt);
  while (nMode & CasAntMask) {

    if (nMode & n1stAnt)
      nAnt = n1stAnt;
    else if (nMode & n2ndAnt)
      nAnt = n2ndAnt;
    else
      assert (0);

    if ((nMode & CasMfr_Band1Sweep1) && (nAnt & n1stAnt)) {
      for (nStep = 0; nStep < 16; nStep++, nLength++) {
        if (pTime != NULL)
          pTime[nLength] = MfrCalBnd1[0].time[nStep];
        if (pFreq != NULL)
          pFreq[nLength] = MfrCalBnd1[0].freq[nStep];
        if (pAmpl != NULL)
          pAmpl[nLength] = MfrCalBnd1[0].data[nStep];
      }
    }
    if ((nMode & CasMfr_Band1Sweep2) && (nAnt & n2ndAnt)) {
      for (nStep = 0; nStep < 16; nStep++, nLength++) {
        if (pTime != NULL)
          pTime[nLength] = MfrCalBnd1[1].time[nStep];
        if (pFreq != NULL)
          pFreq[nLength] = MfrCalBnd1[1].freq[nStep];
        if (pAmpl != NULL)
          pAmpl[nLength] = MfrCalBnd1[1].data[nStep];
      }
    }

    if ((nMode & CasMfr_Band2Sweep1) && (nAnt & n1stAnt)) {
      for (nStep = 0; nStep < 32; nStep++, nLength++) {
        if (pTime != NULL)
          pTime[nLength] = MfrCalBnd2[0].time[nStep];
        if (pFreq != NULL)
          pFreq[nLength] = MfrCalBnd2[0].freq[nStep];
        if (pAmpl != NULL)
          pAmpl[nLength] = MfrCalBnd2[0].data[nStep];
      }
    }
    if ((nMode & CasMfr_Band2Sweep2) && (nAnt & n2ndAnt)) {
      for (nStep = 0; nStep < 32; nStep++, nLength++) {
        if (pTime != NULL)
          pTime[nLength] = MfrCalBnd2[1].time[nStep];
        if (pFreq != NULL)
          pFreq[nLength] = MfrCalBnd2[1].freq[nStep];
        if (pAmpl != NULL)
          pAmpl[nLength] = MfrCalBnd2[1].data[nStep];
      }
    }

    if ((nMode & CasMfr_Band3Sweep1) && (nAnt & n1stAnt)) {
      for (nStep = 0; nStep < 32; nStep++, nLength++) {
        if (pTime != NULL)
          pTime[nLength] = MfrCalBnd3[0].time[nStep];
        if (pFreq != NULL)
          pFreq[nLength] = MfrCalBnd3[0].freq[nStep];
        if (pAmpl != NULL)
          pAmpl[nLength] = MfrCalBnd3[0].data[nStep];
      }
    }
    if ((nMode & CasMfr_Band3Sweep2) && (nAnt & n1stAnt)) {
      for (nStep = 0; nStep < 32; nStep++, nLength++) {
        if (pTime != NULL)
          pTime[nLength] = MfrCalBnd3[1].time[nStep];
        if (pFreq != NULL)
          pFreq[nLength] = MfrCalBnd3[1].freq[nStep];
        if (pAmpl != NULL)
          pAmpl[nLength] = MfrCalBnd3[1].data[nStep];
      }
    }
    if ((nMode & CasMfr_Band3Sweep3) && (nAnt & n2ndAnt)) {
      for (nStep = 0; nStep < 32; nStep++, nLength++) {
        if (pTime != NULL)
          pTime[nLength] = MfrCalBnd3[2].time[nStep];
        if (pFreq != NULL)
          pFreq[nLength] = MfrCalBnd3[2].freq[nStep];
        if (pAmpl != NULL)
          pAmpl[nLength] = MfrCalBnd3[2].data[nStep];
      }
    }
    if ((nMode & CasMfr_Band3Sweep4) && (nAnt & n2ndAnt)) {
      for (nStep = 0; nStep < 32; nStep++, nLength++) {
        if (pTime != NULL)
          pTime[nLength] = MfrCalBnd3[3].time[nStep];
        if (pFreq != NULL)
          pFreq[nLength] = MfrCalBnd3[3].freq[nStep];
        if (pAmpl != NULL)
          pAmpl[nLength] = MfrCalBnd3[3].data[nStep];
      }
    }

    nMode &= ~nAnt;                     /* subtract antenna from list */
  }                                     /* each antenna */

  return nLength;
}



Ulong CasMfr_nMode (CasRecord * pRec)
{
  bool bFastToggle;
  Uchar *pMp = pRec->data;
  Ulong nMode;

  if (pRec->status.cds_time < 0x4FC87A44)
    bFastToggle = false;
  else
    bFastToggle = CasMfr_bFastToggle (pMp);

  if (bFastToggle == true) {
    nMode = CasMfr_FastToggle;
    switch (CasMfr_n1stAnt (pMp)) {
     case 0x00:
       nMode |= CasAntEx;
       break;
     case 0x01:
       nMode |= CasAntEz;
       break;
     case 0x02:
       nMode |= CasAntBx;
       break;
     case 0x03:
       nMode |= CasAntBz;
       break;
     default:
       assert (0);
    }
    switch (CasMfr_n2ndAnt (pMp)) {
     case 0x00:
       nMode |= CasAntEx;
       break;
     case 0x01:
       nMode |= CasAntEz;
       break;
     case 0x02:
       nMode |= CasAntBx;
       break;
     case 0x03:
       nMode |= CasAntBz;
       break;
     default:
       assert (0);
    }
  } else {                              /* normal mfr sweep */
    nMode = CasMfr_Normal;
    switch (CasMfr_nAntenna (pMp)) {
     case 0x00:
       nMode |= CasAntEx;
       break;
     case 0x01:
       nMode |= CasAntEz;
       break;
     case 0x02:
       nMode |= CasAntBx;
       break;
     case 0x03:
       nMode |= CasAntBz;
       break;
     default:
       assert (0);
    }
  }

  nMode |= CasMfr_Band123;              /* mfr always has three bands */

  return nMode;
}



void CasMfr_Init (void)
{
  int nBand, nStep, i;
  float fTime;


  /*
   * bandwidths in hertz 
   */
  for (nStep = 0; nStep < 32; nStep++)
    CasMfr_Bandwidth[0][nStep] = 5.6f;
  for (nStep = 0; nStep < 32; nStep++)
    CasMfr_Bandwidth[1][nStep] = 19.4f;
  for (nStep = 0; nStep < 32; nStep++)
    CasMfr_Bandwidth[2][nStep] = 139.0f;


  /*
   * conversion from Vrms to Electric Field V/m, taking into account the 
   * base capacitance.  
   * Monopole Length=5.00m, Monopole Base Capacitance=8.01dB (2.51478x)
   * Dipole Length=  9.26m, Dipole Base Capacitance=  7.94dB (2.49459x)
   */
  for (nBand = 0; nBand < 3; nBand++) {
    for (nStep = 0; nStep < 32; nStep++) {
      CasMfrEu_Vrms_to_EleFld[nBand][nStep] = (1.0f / 5.00f) * 2.51478f;
      CasMfrEv_Vrms_to_EleFld[nBand][nStep] = (1.0f / 5.00f) * 2.51478f;
      CasMfrEx_Vrms_to_EleFld[nBand][nStep] = (1.0f / 9.26f) * 2.49459f;
      CasMfrEz_Vrms_to_EleFld[nBand][nStep] = (1.0f / 5.00f) * 2.51478f;
    }
  }


  /*
   * conversion factors for the magnetic search coils, Vrms -> nT 
   */
  /*
   * invert for a multiply 
   */
  for (nBand = 0; nBand < 3; nBand++) {
    for (nStep = 0; nStep < 32; nStep++) {
      CasMfr_MagPreAmpGain[nBand][nStep] = 24.0f;
      CasMfrBx_Vrms_to_nTelsa[nBand][nStep] =
        CasMfr_MagPreAmpGain[nBand][nStep];
      CasMfrBx_Vrms_to_nTelsa[nBand][nStep] /=
        CasMfr_Bx_Vrms2nT[nBand][nStep];
      CasMfrBz_Vrms_to_nTelsa[nBand][nStep] =
        CasMfr_MagPreAmpGain[nBand][nStep];
      CasMfrBz_Vrms_to_nTelsa[nBand][nStep] /=
        CasMfr_Bz_Vrms2nT[nBand][nStep];
    }
  }


  /*
   * conversion from data numbers to Vrms 
   */
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][0][i] = CasMfr_B1S01_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][1][i] = CasMfr_B1S02_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][2][i] = CasMfr_B1S03_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][3][i] = CasMfr_B1S04_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][4][i] = CasMfr_B1S05_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][5][i] = CasMfr_B1S06_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][6][i] = CasMfr_B1S07_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][7][i] = CasMfr_B1S08_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][8][i] = CasMfr_B1S09_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][9][i] = CasMfr_B1S10_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][10][i] = CasMfr_B1S11_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][11][i] = CasMfr_B1S12_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][12][i] = CasMfr_B1S13_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][13][i] = CasMfr_B1S14_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][14][i] = CasMfr_B1S15_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[0][15][i] = CasMfr_B1S16_Dn2Vrms[i];

  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][0][i] = CasMfr_B2S01_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][1][i] = CasMfr_B2S02_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][2][i] = CasMfr_B2S03_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][3][i] = CasMfr_B2S04_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][4][i] = CasMfr_B2S05_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][5][i] = CasMfr_B2S06_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][6][i] = CasMfr_B2S07_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][7][i] = CasMfr_B2S08_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][8][i] = CasMfr_B2S09_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][9][i] = CasMfr_B2S10_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][10][i] = CasMfr_B2S11_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][11][i] = CasMfr_B2S12_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][12][i] = CasMfr_B2S13_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][13][i] = CasMfr_B2S14_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][14][i] = CasMfr_B2S15_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][15][i] = CasMfr_B2S16_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][16][i] = CasMfr_B2S17_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][17][i] = CasMfr_B2S18_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][18][i] = CasMfr_B2S19_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][19][i] = CasMfr_B2S20_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][20][i] = CasMfr_B2S21_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][21][i] = CasMfr_B2S22_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][22][i] = CasMfr_B2S23_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][23][i] = CasMfr_B2S24_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][24][i] = CasMfr_B2S25_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][25][i] = CasMfr_B2S26_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][26][i] = CasMfr_B2S27_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][27][i] = CasMfr_B2S28_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][28][i] = CasMfr_B2S29_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][29][i] = CasMfr_B2S30_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][30][i] = CasMfr_B2S31_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[1][31][i] = CasMfr_B2S32_Dn2Vrms[i];

  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][0][i] = CasMfr_B3S01_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][1][i] = CasMfr_B3S02_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][2][i] = CasMfr_B3S03_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][3][i] = CasMfr_B3S04_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][4][i] = CasMfr_B3S05_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][5][i] = CasMfr_B3S06_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][6][i] = CasMfr_B3S07_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][7][i] = CasMfr_B3S08_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][8][i] = CasMfr_B3S09_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][9][i] = CasMfr_B3S10_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][10][i] = CasMfr_B3S11_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][11][i] = CasMfr_B3S12_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][12][i] = CasMfr_B3S13_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][13][i] = CasMfr_B3S14_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][14][i] = CasMfr_B3S15_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][15][i] = CasMfr_B3S16_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][16][i] = CasMfr_B3S17_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][17][i] = CasMfr_B3S18_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][18][i] = CasMfr_B3S19_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][19][i] = CasMfr_B3S20_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][20][i] = CasMfr_B3S21_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][21][i] = CasMfr_B3S22_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][22][i] = CasMfr_B3S23_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][23][i] = CasMfr_B3S24_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][24][i] = CasMfr_B3S25_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][25][i] = CasMfr_B3S26_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][26][i] = CasMfr_B3S27_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][27][i] = CasMfr_B3S28_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][28][i] = CasMfr_B3S29_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][29][i] = CasMfr_B3S30_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][30][i] = CasMfr_B3S31_Dn2Vrms[i];
  for (i = 0; i < 256; i++)
    CasMfr_Dn2Vrms[2][31][i] = CasMfr_B3S32_Dn2Vrms[i];



  /*
   * init the scratch space for the mfr calibrations 
   */
  MfrRawBnd1[0].length = 16;
  MfrCalBnd1[0].length = 16;
  MfrRawBnd1[1].length = 16;
  MfrCalBnd1[1].length = 16;
  for (i = 0, fTime = 0.0f; i < 16; i++, fTime += 1.0f) {
    MfrCalBnd1[0].time[i] = fTime;      /* one second per sweep */
    MfrCalBnd1[0].freq[i] = CasMfr_Frq[0][i];
    MfrCalBnd1[1].time[i] = fTime + 16.0f;       /* one second per sweep */
    MfrCalBnd1[1].freq[i] = CasMfr_Frq[0][i];
  }

  MfrRawBnd2[0].length = 32;
  MfrCalBnd2[0].length = 32;
  MfrRawBnd2[1].length = 32;
  MfrCalBnd2[1].length = 32;
  for (i = 0, fTime = 0.0f; i < 32; i++, fTime += 0.5f) {
    MfrCalBnd2[0].time[i] = fTime;      /* one half second per sweep */
    MfrCalBnd2[0].freq[i] = CasMfr_Frq[1][i];
    MfrCalBnd2[1].time[i] = fTime + 16.0f;       /* one half second per sweep */
    MfrCalBnd2[1].freq[i] = CasMfr_Frq[1][i];
  }

  MfrRawBnd3[0].length = 32;
  MfrCalBnd3[0].length = 32;
  MfrRawBnd3[1].length = 32;
  MfrCalBnd3[1].length = 32;
  MfrRawBnd3[2].length = 32;
  MfrCalBnd3[2].length = 32;
  MfrRawBnd3[3].length = 32;
  MfrCalBnd3[3].length = 32;
  for (i = 0, fTime = 0.0f; i < 32; i++, fTime += 0.25f) {
    MfrCalBnd3[0].time[i] = fTime;      /* one quarter second per sweep */
    MfrCalBnd3[0].freq[i] = CasMfr_Frq[2][i];
    MfrCalBnd3[1].time[i] = fTime + 8.0f;        /* one quarter second per sweep */
    MfrCalBnd3[1].freq[i] = CasMfr_Frq[2][i];
    MfrCalBnd3[2].time[i] = fTime + 16.0f;       /* one quarter second per sweep */
    MfrCalBnd3[2].freq[i] = CasMfr_Frq[2][i];
    MfrCalBnd3[3].time[i] = fTime + 24.0f;       /* one quarter second per sweep */
    MfrCalBnd3[3].freq[i] = CasMfr_Frq[2][i];
  }



  return;
}
