#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include <rpwstlm/CasRecord.h>


CasRecord *CasRecord_Constructor (CasRecord * pSrc)
{
  CasRecord *pObj;

  if ((pObj = (CasRecord *) calloc (1, sizeof (CasRecord))) == NULL)
    fprintf (stderr, "calloc() failed in CasRecord_Constructor()\n");
  else if (CasRecord_Initialize (pObj, pSrc) == false) {
    CasRecord_Destructor (pObj);
    pObj = NULL;
  }

  return pObj;
}


void CasRecord_Destructor (CasRecord * pObj)
{
  free (pObj);

  return;
}



bool CasRecord_Initialize (CasRecord * pObj, CasRecord * pSrc)
{
  bool bStatus = true;

  if (pSrc != NULL) {
    bStatus = CasRecord_Copy (pObj, pSrc);
  } else {                              /* calloc() in constructor should zero out structure */
    pObj->forward_length = 0;
    pObj->status.packet_length = 0;
    pObj->data[0] = pObj->data[1] = 0;  /* Mini Packet ID and Length */
  }

  return bStatus;
}



bool CasRecord_Copy (CasRecord * pObj, CasRecord * pSrc)
{
  memcpy (pObj, pSrc, sizeof (CasRecord));

  return true;
}
