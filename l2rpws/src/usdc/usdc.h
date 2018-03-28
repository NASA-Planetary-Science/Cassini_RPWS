
#include <rpwstlm/CasRecord.h>

Ulong CasUsdc_DccDecompress (CasRecord * pRec);
Ulong CasUsdc_ZeroCheck (CasRecord * pRec);
char *CasUsdc_DecodeGndStatus (Ulong nStatus);

Ulong CasWfr_Unleave (CasRecord * pIn, CasRecord ** pOut);
