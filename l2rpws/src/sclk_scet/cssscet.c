/*----------------------------------------------------------------------

  cssscet.c written by L. Granroth  2003-11-15
  to take either a command line argument or multiple text lines from
  stdin, parse the input as Cassini spacecraft clock and write a normalized
  time string with Cassini spacecraft clock (SCLK) to stdout.

  Output is in the following form:

  1/1447622002.177   2003-319 // 20:51:16.000

  Under Sun/Solaris, compile with:

  cc -O -c cssscet.c
  f77 -O -o cssscet cssscet.o -L/local/lib -lspicelib

  Under Linux, compile with:

  gcc -O -c cssscet.c
  gfortran -O -o cssscet cssscet.o -lspicelib

  ----------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>

#define LEAPSECKERNEL "/opt/project/cassini/spice/kernels/lsk/leapseconds.tls"
#define SCLKKERNEL "/opt/project/cassini/spice/kernels/sclk/cassini.tsc"

char *myname;

/* fortran spicelib routines */

void clpool_ (void);
void ldpool_ (char *name, int namelen);
void scs2e_  (int *sc, char *sclk, double *et, int sclklen);
void sce2s_  (int *sc, double *et, char *sclk, int sclklen);
void et2utc_
  (double *et, char *form, int *prec, char *utc, int formlen, int utclen);

int
main (int argc, char *argv[])		/* cssscet - parse SCLK return SCET */
{

  int i, len;
  char clock[80] = {""};
  char scet[25] = {""};
  char sclk[20] = {""};
  int sc = -82, prec = 3;
  char text[81];
  double et;

/*----------------------------- cssscet --------------------------------*/

  clpool_ ();
  ldpool_ (LEAPSECKERNEL, sizeof(LEAPSECKERNEL));
  ldpool_ (SCLKKERNEL, sizeof(SCLKKERNEL));


  len = 0;
  for (i = 1; i < argc; i++) {
    strncat (clock, argv[i], 79 - len);
    strcat (clock, " ");
    len = strlen (clock);
  }

  while (1) {

    if (len > 0) {

      scs2e_  (&sc, clock, &et, len);
      sce2s_  (&sc, &et, sclk, sizeof(sclk) - 1);
      et2utc_ (&et, "D", &prec, scet, 1, sizeof(scet) - 1);

      fprintf (stdout, "%s%s\n", sclk, scet);
      fflush (stdout);

    }

    if (argc > 1) break;

    if (!fgets (clock, sizeof(clock), stdin)) break;
    len = strlen (clock) - 1;

  }

  return 0;

} /* cssscet - parse SCLK return SCET */
