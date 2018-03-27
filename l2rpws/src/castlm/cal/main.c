#include <stdio.h>
#include <stdlib.h>

#include "CasHfrCal.h"

/*
#include "CasLfdrCal.h"
#include "CasMfrCal.h"   
#include "CasMfrDn2Vrms.h"  include in CasMfrCal.h 
*/
void make_mfr_dn2vrms (void);           /* takes *.cal files and generates header files */
void make_mfr_code (void);              /* generates calibration initialization code */


int main (int argc, char *argv[])
{
  int i, j;
  float *p;

  for (i = 0; i < 8; i++) {
    for (j = 0; j < 8; j++)
      fprintf (stdout, " %7.2f", A1hf1tmp[i][j]);
    fprintf (stdout, "\n");
  }
  fprintf (stdout, "\n");

  p = A1hf1tmp[0];
  for (j = 0; j < 8; j++)
    fprintf (stdout, " %7.2f", p[j]);



/*
  for(i=0;i<32;i++)
    fprintf(stdout,"%2d %7.2f %7.2f\n",i,CasLfdr_Frq[i],CasMfdr_Frq[i]);
  fprintf(stdout,"\n");

  for(i=0;i<32;i++)
    fprintf(stdout,"%2d %7.4f %7.4f\n",i,CasLfdr_Bandwidth[i],
            CasMfdr_Bandwidth[i]);
  fprintf(stdout,"\n");
*/





/* mfr calibration check 

  fprintf(stdout,"MFR Bx Cal Vrms to nT\n");
  fprintf(stdout,"Step   Band 1   Band 2   Band 3\n");
  for(i=0;i<32;i++){
    fprintf(stdout,"%3d    %6.4f   %6.4f   %6.4f\n",i+1,
            CasMfr_Bx_Vrms2nT[0][i],
            CasMfr_Bx_Vrms2nT[1][i],
            CasMfr_Bx_Vrms2nT[2][i]);
  }
  fprintf(stdout,"\n");

  fprintf(stdout,"MFR Bz Cal Vrms to nT\n");
  fprintf(stdout,"Step   Band 1   Band 2   Band 3\n");
  for(i=0;i<32;i++){
    fprintf(stdout,"%3d    %6.4f   %6.4f   %6.4f\n",i+1,
            CasMfr_Bz_Vrms2nT[0][i],
            CasMfr_Bz_Vrms2nT[1][i],
            CasMfr_Bz_Vrms2nT[2][i]);
  }
  fprintf(stdout,"\n");

  fprintf(stdout,"MFR Frequencies\n");
  fprintf(stdout,"Step     Band 1     Band 2    Band 3\n");
  for(i=0;i<32;i++){
    fprintf(stdout,"%3d    %8.2f   %8.2f   %8.2f\n",i+1,
            CasMfr_Frq[0][i],
            CasMfr_Frq[1][i],
            CasMfr_Frq[2][i]);
  }
  fprintf(stdout,"\n");

mfr calibration check */



  return 0;
}



void make_mfr_dn2vrms (void)
{
  char sOld[64], sNew[64];
  char *p, arInLine[1024], *pEnd;
  int nBand, nStep, i;
  double dVrms, dDn, arDnVrms[256];
  FILE *hOld, *hNew;


  for (nBand = 0; nBand < 3; nBand++) {
    for (nStep = 0; nStep < 32; nStep++) {

      sprintf (sOld, "oldcal/b%ds%02d.cal", nBand + 1, nStep + 1);
      sprintf (sNew, "newcal/CasMfrB%dS%02d.h", nBand + 1, nStep + 1);
      if ((hOld = fopen (sOld, "rt")) == NULL) {
        fprintf (stderr, "Unable to read %s\n", sOld);
        continue;
      }
      if ((hNew = fopen (sNew, "wt")) == NULL) {
        fprintf (stderr, "Unable to write %s\n", sNew);
        continue;
      }

      /*
       * zero out data array 
       */
      for (i = 0; i < 256; i++)
        arDnVrms[i] = 0.0;

      /*
       * Parse two column file 
       */
      while ((p = fgets (arInLine, 1024, hOld)) != NULL) {
        while ((*p != '\0') && (*p != '#'))
          ++p;
        *p = '\0';                      /* discard comments */

        /*
         * assume 2 floating point numbers per line 
         */
        p = arInLine;
        dVrms = strtod (p, &pEnd);
        if (dVrms == 0.0)
          continue;                     /* blank line ? */
        dDn = strtod (pEnd, NULL);

        if ((dDn < 1.0) || (dDn > 255.0)) {
          fprintf (stderr, "range error dDn=%g, dVrms=%g in %s\n",
                   dDn, dVrms, sOld);
          exit (127);
        }

        arDnVrms[(int) dDn] = dVrms;
      }                                 /* while parsing file */

      /*
       * check to see that all the data numbers got filled in 
       */
      for (i = 1; i < 256; i++) {
        if (arDnVrms[i] == 0.0) {
          fprintf (stderr, "Missing array value %d in %s\n", i, sOld);
          exit (127);
        }
      }

      /*
       * write header file 
       */
      fprintf (hNew, "static double CasMfr_B%dS%02d_Dn2Vrms[256]={\n",
               nBand + 1, nStep + 1);
      for (i = 0; i < 255; i++)
        fprintf (hNew, "/* %3d */ %10.4E,\n", i, arDnVrms[i]);
      fprintf (hNew, "/* %3d */ %10.4E };\n", i, arDnVrms[i]);

      fclose (hOld);
      fclose (hNew);

    }                                   /* Step */
  }                                     /* Band */


  return;
}



void make_mfr_code (void)
{

/* char sVar[32]="CasMfr_B%dS%02d_Dn2Vrms[256]"; */
  int nBand, nStep;


  for (nBand = 0; nBand < 3; nBand++) {
    for (nStep = 0; nStep < 32; nStep++) {
      fprintf (stdout, "for(i=0;i<256;i++)\n");
      fprintf (stdout,
               "  CAS_MFR_CAL_TABLE[%d][%d][i]=CasMfr_B%dS%02d_Dn2Vrms[i];\n",
               nBand, nStep, nBand + 1, nStep + 1);
    }                                   /* step */
  }                                     /* band */



  return;
}
