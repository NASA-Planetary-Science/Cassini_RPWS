
#ifndef CasSpice_h
#define CasSpice_h

#ifdef __cplusplus
extern "C"
{
#endif

/*
#include <stdio.h> 
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include "SpiceUsr.h"
*/
#include <Cext.h>


  extern char *CAS_NAIF_METAKERNEL;     /* metafiles for kernels to load */



  /*********************************************************
   **** If you're familiar with SPICE S- and P-kernels,	**** 
   **** you know that NAIF codes for spacecraft are 	****
   **** negative integers: -31 for Voyager 1, -32 for	****
   **** Voyager 2, -94 for Mars Observer, and so on. We	****
   **** borrow from this convention in defining		****
   **** instrument codes.				****
   ****							****
   **** Well, who'd-a thunk-it... negative numbers...	****
   **** sheesh, I guess that means Cassini is -82, then	****
   **** isn't it ?!?   Doooh				****
   ****                                                 ****
   **** Hmm, some good ideas for job security.          ****
   ****                                                 ****
   **** Not to be negative about this but, the labels   ****
   **** in various files should be corrected to reflect ****
   **** the 'true' NAIF code...The sclk/scet file labels****
   **** SCLK_DATA_TYPE_82 should become                 ****
   **** SCLK_DATA_TYPE_-82, but alas, it would probably ****
   **** negate the simple minded parsing software       ****
   **** shared with pds.                                ****
   ****                                                 ****
   *********************************************************/
	
	/* Comment on above comment.  There is a required readme for
	   spice kernels, distributed with the toolkit it explains 
		the code scheme. */

#define CAS_NAIF_ID_CODE -82


  bool CasSpice_Init (char *sMetaFile); /* standard setup */
  char *CasSpice_sError (void);

  char *CasSpice_nSclk_to_sSclk (Ulong nSclk, Ulong nFine, char *pSin);
  char *CasSpice_nSclk_to_sScet (Ulong nSclk, Ulong nFine, char *pSin);
  Ulong CasSpice_nSclk_to_nScet (Ulong nSclk, Ulong nFine, Ulong * pDays,
                                 Ulong * pMil);
  int CasSpice_sScet_to_nSclk (char *sScet, Ulong * pSclk, Ulong * pFine);
  Ulong CasSpice_sScet_to_nScet (char *sScet, Ulong * pDays, Ulong * pMils);
  char *CasSpice_nScet_to_sScet (Ulong nDays, Ulong nMils, char *pSin);

  char *CasSpice_nSclk_to_GOOFY_sSclk (Ulong nSclk, Ulong nFine, char *pSin);
  char *CasSpice_nScet_to_GOOFY_sScet (Ulong nDays, Ulong nMils, char *pSin);

  char *CasSpice_GMT (char *pSin);
  char *CasSpice_GMT_yyyymmdd (char *pSin);

#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasSpice_h */
