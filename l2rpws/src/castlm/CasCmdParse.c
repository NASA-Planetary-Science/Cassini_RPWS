#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

#include <das2/das1.h>

#include <casephem/CasSpice.h>

#include <rpwstlm/CasType.h>
#include <rpwstlm/CasCmdParse.h>


/* void CasCmd_ParseTime (char *sTime, DasTime * p) */
void CasCmd_ParseTime (char *sTime, DasTime* p)
{
  double dStatus;

  dStatus = parsetime(sTime, &p->nYear, &p->nMonth, &p->nDayOfMonth,
                           &p->nDayOfYear, &p->nHour, &p->nMinute,
                           &p->dSecond);

  if (dStatus < 0) {
    fprintf (stderr, "%f=parsetime(%s) choked \n", dStatus, sTime);
    exit (0);
  }

  sprintf (p->sScet, "%d-%03dT%02d:%02d:%02d", p->nYear, p->nDayOfYear,
           p->nHour, p->nMinute, (int) p->dSecond);
  CasSpice_sScet_to_nSclk (p->sScet, &p->nSclk, &p->nFine);
  CasSpice_sScet_to_nScet (p->sScet, &p->nDays, &p->nMsec);
  p->dDays = p->nDays + p->nMsec / (24 * 60 * 60 * 1.0E3);
  sprintf (p->sSclk, "%d.%03d", p->nSclk, p->nFine);

  return;
}



Ulong CasCmd_RcvStrToMode (const char *sRcv, char *sArg)
{
  char *p;
  Ulong nType = 0;

  p = (char *) sArg;
  assert (p != NULL);

  if (!strcmp ("lfr", sRcv) || !strcmp ("lfdr", sRcv)) {
    nType = CasLfdr_Normal;
    while (isalnum ((int) (*p))) {
      switch (*p) {
       case 'E':
         ++p;
         if (*p == 'u')
           nType |= CasAntEu;
         else if (*p == 'v')
           nType |= CasAntEv;
         else if (*p == 'x')
           nType |= CasAntEx;
         else if (*p == 'w')
           nType |= CasAntEw;
         else
           fprintf (stderr, "invalid lfdr antenna E%c\n", *p);
         break;
       case 'B':
         ++p;
         if (*p == 'x')
           nType |= CasAntBx;
         else if (*p == 'y')
           nType |= CasAntBy;
         else if (*p == 'z')
           nType |= CasAntBz;
         else
           fprintf (stderr, "invalid lfdr antenna B%c\n", *p);
         break;
       default:
         fprintf (stderr, "invalid lfdr option %c\n", *p);
         break;
      }
      ++p;
    }                                   /* elihw */
  } else if (!strcmp ("mfdr", sRcv)) {
    nType = CasMfdr_Normal;
    while (isalnum ((int) (*p))) {
      switch (*p) {
       case 'E':
         ++p;
         if (*p == 'u')
           nType |= CasAntEu;
         else if (*p == 'v')
           nType |= CasAntEv;
         else if (*p == 'x')
           nType |= CasAntEx;
         else if (*p == 'w')
           nType |= CasAntEw;
         else
           fprintf (stderr, "invalid mfdr antenna E%c\n", *p);
         break;
       case 'B':
         ++p;
         if (*p == 'x')
           nType |= CasAntBx;
         else if (*p == 'y')
           nType |= CasAntBy;
         else if (*p == 'z')
           nType |= CasAntBz;
         else
           fprintf (stderr, "invalid mfdr antenna B%c\n", *p);
         break;
       default:
         fprintf (stderr, "invalid mfdr option %c\n", *p);
         break;
      }
      ++p;
    }                                   /* elihw */
  } else if (!strcmp ("mfr", sRcv)) {
    nType = CasMfr_AnyMode;             /* Normal and Fast Toggle */
    nType = CasMfr_Normal | CasMfr_FastToggle;  /* Normal and Fast Toggle */
    while (isalnum ((int) (*p))) {
      switch (*p) {
       case '1':
         nType |= CasMfr_Band1;
         break;
       case '2':
         nType |= CasMfr_Band2;
         break;
       case '3':
         nType |= CasMfr_Band3;
         break;
       case 'E':
         ++p;
         if (*p == 'u')
           nType |= CasAntEu;
         else if (*p == 'v')
           nType |= CasAntEv;
         else if (*p == 'x')
           nType |= CasAntEx;
         else if (*p == 'w')
           nType |= CasAntEw;
         else
           fprintf (stderr, "invalid mfr antenna E%c\n", *p);
         break;
       case 'B':
         ++p;
         if (*p == 'x')
           nType |= CasAntBx;
         else if (*p == 'z')
           nType |= CasAntBz;
         else
           fprintf (stderr, "invalid mfr antenna B%c\n", *p);
         break;
       default:
         fprintf (stderr, "invalid mfr option %c\n", *p);
         break;
      }
      ++p;
    }                                   /* elihw */
  } else if (!strcmp ("hfr", sRcv)) {   /* hfr analysis mode */
    nType = CasHfr_Analysis;
    while (isalnum ((int) (*p))) {
      switch (*p) {
       case 'A':
         nType |= CasHfr_BandA;
         break;
       case 'B':
         nType |= CasHfr_BandB;
         break;
       case 'C':
         nType |= CasHfr_BandC;
         break;
       case '1':
         nType |= CasHfr_BandHF1;
         break;
       case '2':
         nType |= CasHfr_BandHF2;
         break;
       case 'E':
         ++p;
         if (*p == 'u')
           nType |= CasAntEu;
         else if (*p == 'v')
           nType |= CasAntEv;
         else if (*p == 'x')
           nType |= CasAntEx;
         else if (*p == 'w')
           nType |= CasAntEw;
         else
           fprintf (stderr, "invalid hfr antenna E%c\n", *p);
         break;
       default:
         fprintf (stderr, "invalid hfr option %c\n", *p);
         break;
      }
      ++p;
    }                                   /* elihw */
  } else if (!strcmp ("msc", sRcv)) {   /* hfr millisecond mode */
    nType = CasHfr_Millisecond;
    while (isalnum ((int) (*p))) {
      switch (*p) {
       case '1':
         nType |= CasHfr_BandHF1;
         break;
       case '2':
         nType |= CasHfr_BandHF2;
         break;
       case 'E':
         ++p;
         if (*p == 'u')
           nType |= CasAntEu;
         else if (*p == 'v')
           nType |= CasAntEv;
         else if (*p == 'x')
           nType |= CasAntEx;
         else if (*p == 'w')
           nType |= CasAntEw;
         else
           fprintf (stderr, "invalid msc antenna E%c\n", *p);
         break;
       default:
         fprintf (stderr, "invalid msc option %c\n", *p);
         break;
      }
      ++p;
    }                                   /* elihw */
  } else {
    assert (0);
    exit (1);
  }

  return nType;
}

