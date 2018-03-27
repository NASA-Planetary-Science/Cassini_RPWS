#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <math.h>

#include <casephem/CasSpice.h>

#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasLp.h>


Ulong CasLp_nMode (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nMode;

  if (CasLp_bRawSweep (pMp) == true)
    nMode = CasLp_RawSweep;
  else if (CasLp_bRawDensity (pMp) == true)
    nMode = CasLp_RawDensity;
  else if (CasLp_bAnalyzedSweep (pMp) == true)
    nMode = CasLp_AnalyzedSweep;
  else if (CasLp_bTbdMode (pMp) == true)
    nMode = CasLp_TbdMode;
  else
    assert (0);

  return nMode;
}



float CasLp_fDuration (CasRecord * pRec, Ulong nMode)
{
  Uchar *pMp = pRec->data;
  double dClk;
  Ulong nLength;

  if ((CasLp_bRawSweep (pMp) == false) || (CasLp_bCompressed (pMp) == true)) {
    Ulong nSec, nRti, nCds, nFine;

    nCds = pRec->status.cds_time;
    nRti = pRec->data[3];
    nRti <<= 8;
    nRti |= pRec->data[2];
    nSec = GetEventTime (nCds, nRti);
    nFine = (nRti << 5) & 0x0FF;
    fprintf (stderr, "CasLp_fDuration() error - ");
    if (CasLp_bRawSweep (pMp) == false)
      fprintf (stderr, "not raw sweep packet\n");
    if (CasLp_bCompressed (pMp) == true)
      fprintf (stderr, "packet is compressed\n");
    fprintf (stderr, "  cds time %08X      %s\n", nCds,
             CasSpice_nSclk_to_sScet (nCds, 0x00, NULL));
    fprintf (stderr, "  mp  time %08X %04X %s\n", nSec, nRti,
             CasSpice_nSclk_to_sScet (nSec, nFine, NULL));
    return 0.0;
  }

  /*
   * check upper two bits to see if software controlled 
   */
  if (CasLp_bRtiClockPeriod (pMp) == true)
    dClk = (1.0 / 8.0) * CasLp_ClockPeriod (pMp);       /* 8Hz (rti) counter */
  else
    dClk = (1.0 / 125E3) * CasLp_ClockPeriod (pMp);

  nLength = pRec->status.packet_length + 3;
  nLength -= CasLp_nHeaderLength;
  nLength /= 2;                         /* 12 bit samples stuffed into 16 bits */

  dClk *= nLength;                      /* Note: One second before a Lp Raw Sweep, a warning is    */
  /*
   * sent to the S/C.  Imediately followin the warning the   
   */
  /*
   * sphere bias is changed from zero to -45V causing a      
   */
  /*
   * glitch to proceed the capture by approximately 1 second 
   */
  /*
   * (7 to 8 RTIs).                                          
   */

	if(fabs(dClk) > 0x80000000){
	   fprintf(stderr, "CasLp_fDuration() clock overflow, %f > 0x80000000\n", dClk);
		exit(30);
	}

  return (float)dClk;
}
