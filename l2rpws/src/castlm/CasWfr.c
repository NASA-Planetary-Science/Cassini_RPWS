#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasWfr.h>




Ulong CasWfr_nMode (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nMode;

  if (CasWfr_bLowBand (pMp) == true)
    nMode = CasWfr_Lband;
  else if (CasWfr_bHighBand (pMp) == true)
    nMode = CasWfr_Hband;
  else
    assert (0);

  switch (CasWfr_CaptureMode (pMp)) {
   case 0:
     nMode |= CasWfr_Ch0;
     if (CasWfr_Antenna (pMp) & 0x01)
       nMode |= CasAntEx;
     else
       nMode |= CasAntLMRp;
     break;
   case 1:
     nMode |= CasWfr_Ch1;
     if (CasWfr_Antenna (pMp) & 0x02)
       nMode |= CasAntEz;
     else
       nMode |= CasAntLMRm;
     break;
   case 2:
     nMode |= CasWfr_Ch2;
     nMode |= CasAntBx;
     break;
   case 3:
     nMode |= CasWfr_Ch3;
     nMode |= CasAntBy;
     break;
   case 4:
     nMode |= CasWfr_Ch4;
     nMode |= CasAntBz;
     break;
   case 5:
     nMode |= CasWfr_Ch01;
     if (CasWfr_Antenna (pMp) & 0x01)
       nMode |= CasAntEx;
     else
       nMode |= CasAntLMRp;
     if (CasWfr_Antenna (pMp) & 0x02)
       nMode |= CasAntEz;
     else
       nMode |= CasAntLMRm;
     break;
   case 6:
     if (CasWfr_bLangmuirProbeMode (pMp) == true) {     /* should be LMRp,LMRm,LPs */
       nMode |= CasWfr_Ch012;
       if (CasWfr_Antenna (pMp) & 0x01)
         nMode |= CasAntEx;
       else
         nMode |= CasAntLMRp;
       if (CasWfr_Antenna (pMp) & 0x02)
         nMode |= CasAntEz;
       else
         nMode |= CasAntLMRm;
       if (CasWfr_Antenna (pMp) & 0x04)
         nMode |= CasAntLPs;
       else
         nMode |= CasAntBx;
     } else {                           /* should be Bx,By,Bz */
       nMode |= CasWfr_Ch234;
       if (CasWfr_Antenna (pMp) & 0x04)
         nMode |= CasAntLPs;
       else
         nMode |= CasAntBx;
       nMode |= CasAntBy;
       nMode |= CasAntBz;
     }
     break;
   case 7:
     nMode |= CasWfr_Ch01234;
     if (CasWfr_Antenna (pMp) & 0x01)
       nMode |= CasAntEx;
     else
       nMode |= CasAntLMRp;
     if (CasWfr_Antenna (pMp) & 0x02)
       nMode |= CasAntEz;
     else
       nMode |= CasAntLMRm;
     if (CasWfr_Antenna (pMp) & 0x04)
       nMode |= CasAntLPs;
     else
       nMode |= CasAntBx;
     nMode |= CasAntBy;
     nMode |= CasAntBz;
     break;
   default:
     fprintf (stderr, "CasWfr_nMode(), Mode=%d\n", CasWfr_CaptureMode (pMp));
     break;
  }

  return nMode;
}
