#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasWbr.h>




Ulong CasWbr_nMode (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nMode;

  if (CasWbr_bLowBand (pMp) == true)
    nMode = CasWbr_Lband;
  else if (CasWbr_bHighBand (pMp) == true)
    nMode = CasWbr_Hband;
  else
    assert (0);

  switch (CasWbr_Antenna (pMp)) {
   case 0x00:
     nMode |= CasAntEx;
     break;
   case 0x01:
     nMode |= CasAntBx;
     break;
   case 0x02:
     nMode |= CasAntEz;
     break;
   case 0x03:
     nMode |= CasAntHF;
     break;
   case 0x04:
     nMode |= CasAntLPs;
     break;                             /* documented as Lmr ?  */
   default:                            /* and the other bits ? */
     fprintf (stderr, "CasWbr_nMode(), Antenna=%d\n", CasWbr_Antenna (pMp));
     break;
  }

  return nMode;
}
