#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasWfdr.h>

#include "cal/CasWfdrCal.h"

bool bWfdr_BaseCapacitance = true;
static float CasWfdr_Ampl[32];



Ulong CasWfdr_nMode (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nMode, nSize;

  if (CasWfdr_bLfdr (pMp) == true)
    nMode = CasWfdr_Lfdr;
  else if (CasWfdr_bMfdr (pMp) == true)
    nMode = CasWfdr_Mfdr;
  else
    assert (0);

  if (CasWfdr_bFake (pMp) == true)
    nMode |= CasWfdr_Fake;

  if (CasWfdr_bLinear (pMp) == true)
    nMode |= CasWfdr_Lin;
  else
    nMode |= CasWfdr_Log;

  if (CasWfdr_PreScale (pMp) == 0x00)
    nMode |= CasWfdr_DPFzero;

  nSize = (Ulong)CasWfdr_nSize (pMp);
  switch (CasWfdr_nSize (pMp)) {
   case 256:
     nMode |= CasWfdr_256;
     break;
   case 512:
     nMode |= CasWfdr_512;
     break;
   case 1024:
     nMode |= CasWfdr_1024;
     break;
   case 2048:
     nMode |= CasWfdr_2048;
     break;
   default:
     fprintf (stderr, "CasWfdr_nMode(), nSize=%d\n", CasWfdr_nSize (pMp));
     assert (0);
     break;
  }

  /*
   * Channel + Antenna bit determine sensor 
   */
  switch (CasWfdr_Channel (pMp)) {
   case 0x00:
     if (CasWfdr_ANT (pMp) == 0x01)
       nMode |= CasAntEx;
     else
       nMode |= CasAntLMRp;
     break;
   case 0x01:
     if (CasWfdr_ANT (pMp) == 0x01)
       nMode |= CasAntEw;               /* formally Ez */
     else
       nMode |= CasAntLMRm;
     break;
   case 0x02:
     if (CasWfdr_ANT (pMp) == 0x01)
       nMode |= CasAntLPs;
     else
       nMode |= CasAntBx;
     break;
   case 0x03:
     nMode |= CasAntBy;
     break;
   case 0x04:
     nMode |= CasAntBz;
     break;
   default:                            /*assert(0); */
     break;
  }

  return nMode;
}



float CasWfdr_fDuration (CasRecord * pRec, Ulong nMode)
{
  Uchar *pMp = pRec->data;
  float fDuration;

  if (nMode == 0x00)
    nMode = CasWfdr_Lfdr | CasWfdr_Mfdr | CasAntAll;

  assert (CasMp_bWfdrPacket (pMp) == true);

  if (CasWfdr_bLfdr (pMp) == true)
    fDuration = 10.0E-3f;                /* sampling rate */
  else if (CasWfdr_bMfdr (pMp) == true)
    fDuration = 140.0E-6f;               /* sampling rate */
  else
    assert (0);

  fDuration *= (float)CasWfdr_nSize (pMp);

  return fDuration;
}


void CasWfdr_Init (void)
{
  Uchar mantissa, exponent;
  int base[8] = { 0, 32, 96, 224, 480, 992, 2016, 4064 };
  int i;

  /*
   * preload an array to convert the 8 bit float to a 4 byte float 
   */
  for (i = 0; i < 256; i++) {
    mantissa = i & 0x1F;
    exponent = (i >> 5) & 0x07;
	 
	 /* Impossible to overflow float with this array but compiler doesn't
	    know what so add a cast to make it happy */
    CasWfdr_Counts[i] = (float) (base[exponent] + mantissa * (1 << exponent));
  }

  return;
}


int CasWfdr_Calibrate (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  int i, nLength = 32;                  /* should always be true */
  Ulong nGain;
  float fDgf, fGain, fAntLen, fBaseCap;
  float *pVrms2nT, *pBandWidth, *pAmpl;


  fBaseCap = 0.0;                       /* satisify compiler warnings */
  fDgf = (float) CasWfdr_nPreScale (pMp);       /* digital gain factor */
  nGain = CasWfdr_Gain (pMp);           /* channel gain */

  pAmpl = CasWfdr_Ampl;

  for (i = 0; i < nLength; i++) {
    pAmpl[i] = CasWfdr_Counts[pMp[6 + i]];      /* 8bit float to 4byte float */
    pAmpl[i] /= fDgf;                   /* divide by digital gain factor */
    if (pAmpl[i] == 0.0)
      pAmpl[i] = 0.5;                   /* some non-zero value */
  }

  pVrms2nT = NULL;
  fAntLen = 0.0;
  if (CasWfdr_bLfdr (pMp) == true) {
    for (i = 0; i < nLength; i++)       /* divide by channel gain and  */
      pAmpl[i] /= CasLfdr_Gain[i][nGain];       /* convert to Vrms */

    switch (CasWfdr_Channel (pMp)) {
     case 0:                           /* ExLo or Lmr+ */
       if (CasWfdr_ANT (pMp) == 0) {
         CasMp_Dump (pRec, stderr);
         assert (0);
       } /* Lmr+ */
       else
         fAntLen = 9.26f;                /* ExLo 9.26 meters */
       fBaseCap = 2.49459f;              /* 9.26m */
       break;
     case 1:                           /* EzLo or Lmr- */
       if (CasWfdr_ANT (pMp) == 0) {
         CasMp_Dump (pRec, stderr);
         assert (0);
       } /* Lmr- */
       else
         fAntLen = 5.00f;                /* EzLo 5.00 meters */
       fBaseCap = 2.51478f;              /* 5.00m */
       break;
     case 2:
       if (CasWfdr_ANT (pMp) == 1)
         assert (0);                    /* Lp */
       else
         pVrms2nT = CasLfdr_Bx_Vrms2nT; /* Bx Vrms->nT */
       break;
     case 3:
       pVrms2nT = CasLfdr_By_Vrms2nT;   /* By Vrms->nT */
       break;
     case 4:
       pVrms2nT = CasLfdr_Bz_Vrms2nT;   /* Bz Vrms->nT */
       break;
     default:
       assert (0);
    }

    pBandWidth = CasLfdr_Bandwidth;
  } else if (CasWfdr_bMfdr (pMp) == true) {
    fGain = 9.45f + 10 * (float) nGain;
    fGain = (float) pow (10.0, fGain / 20.0);
    fGain *= 2047.5f;
    for (i = 0; i < nLength; i++)       /* divide by channel gain */
      pAmpl[i] /= fGain;                /* convert to Vrms */

    switch (CasWfdr_Channel (pMp)) {
     case 0:                           /* ExLo or Lmr+ */
       if (CasWfdr_ANT (pMp) == 0)
         assert (0);                    /* Lmr+ */
       else
         fAntLen = 9.26f;                /* ExLo 9.26 meters */
       fBaseCap = 2.49459f;              /* 9.26m */
       break;
     case 1:                           /* EzLo or Lmr- */
       if (CasWfdr_ANT (pMp) == 0)
         assert (0);                    /* Lmr- */
       else
         fAntLen = 5.00f;                /* EzLo 5.00 meters */
       fBaseCap = 2.51478f;              /* 5.00m */
       break;
     case 2:
       if (CasWfdr_ANT (pMp) == 1)
         assert (0);                    /* Lp */
       else
         pVrms2nT = CasMfdr_Bx_Vrms2nT; /* Bx Vrms->nT */
       break;
     case 3:
       pVrms2nT = CasMfdr_By_Vrms2nT;   /* By Vrms->nT */
       break;
     case 4:
       pVrms2nT = CasMfdr_Bz_Vrms2nT;   /* Bz Vrms->nT */
       break;
     default:
       assert (0);
    }

    pBandWidth = CasMfdr_Bandwidth;
  } else
    assert (0);

  /*
   * conversion from Vrms to Electric Power Spectral Density, by taking 
   * into account the base capacitance.  
   * Monopole Length=5.00m, Monopole Base Capacitance=8.01dB (2.51478x)
   * Dipole Length=  9.26m, Dipole Base Capacitance=  7.94dB (2.49459x)
   */
  if (bWfdr_BaseCapacitance == false)
    fBaseCap = 1.0;

  if (pVrms2nT == NULL) {               /* Ex,Ez Calibration */
    for (i = 0; i < 32; i++) {
      pAmpl[i] *= fBaseCap;             /* Base Capacitance correction */
      pAmpl[i] /= fAntLen;              /* Vrms -> Vrms/m */
      pAmpl[i] *= pAmpl[i];             /* V^2/m^2        */
      pAmpl[i] /= pBandWidth[i];        /* V^2/m^2/Hz     */
    }
  } else {                              /* Bx,By,Bz calibration */
    for (i = 0; i < 32; i++) {
      pAmpl[i] *= 24.0f;                 /* Search Coil Preamp Gain = 24 */
      pAmpl[i] /= pVrms2nT[i];          /* Vrms -> nT  */
      pAmpl[i] *= pAmpl[i];             /* nT^2        */
      pAmpl[i] /= pBandWidth[i];        /* nT^2/Hz     */
    }
  }


  return nLength;
}



int CasWfdr_GetPhysicalUnits (CasRecord * pRec, Ulong nWrtMode,
                              float *pTime, float *pFreq, float *pAmpl)
{
  bool bNewData = false;                /* assume the data is calibrated */
  Uchar *pMp = pRec->data;
  Ulong nMode;
  int i, nLength = 32;                  /* should always be true */
  static Uchar arMp[32];

  assert (CasMp_bWfdrPacket (pMp) == true);
  assert (pRec->status.packet_length == 35);
  /*
   * Hdr=6bytes Data=32bytes=38bytes. length is bytes to follow - 1 (35) 
   */

  /*
   * check for new data 
   */
  bNewData = false;
  for (i = 0; i < 6; i++) {
    if (arMp[i] != pMp[i])
      bNewData = true;
    arMp[i] = pMp[i];
  }
  if (bNewData == true)
    CasWfdr_Calibrate (pRec);

  nMode = CasWfdr_nMode (pRec);
  nMode &= nWrtMode;
  if (((nMode & CasLfdr_Normal) != CasLfdr_Normal) &&
      ((nMode & CasMfdr_Normal) != CasMfdr_Normal))
    return 0;
  if ((nMode & CasAntMask) == 0)
    return 0;

  if (pTime != NULL) {
    for (i = 0; i < 32; i++)
      pTime[i] = 0.0;
  }
  if (pFreq != NULL) {
    float *p;

    if (CasWfdr_bLfdr (pMp) == true)
      p = CasLfdr_Frq;
    else if (CasWfdr_bMfdr (pMp) == true)
      p = CasMfdr_Frq;
    else
      assert (0);
    for (i = 0; i < 32; i++)
      pFreq[i] = p[i];
  }
  if (pAmpl != NULL) {
    for (i = 0; i < 32; i++)
      pAmpl[i] = CasWfdr_Ampl[i];
  }


  return nLength;
}
