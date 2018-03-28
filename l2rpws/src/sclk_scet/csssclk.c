/*----------------------------------------------------------------------

  csssclk.c written by L. Granroth  2003-11-15
  to take either a command line argument or multiple text lines from
  stdin, parse the input as time (Cassini SCET) and write a normalized
  time string with Cassini spacecraft clock (SCLK) to stdout.

  Output is in the following form:

  1/1447622002.177   2003-319 // 20:51:16.000

  Under Sun/Solaris, compile with:

  cc -O -c -I/local/include csssclk.c
  f77 -O -o csssclk csssclk.o -L/local/lib -ldas -lspicelib

  Under Linux, compile with (isn't there a more generic place for daslib?):

  gcc -O -c -I/opt/project/cassini/Linux.x86_64/include csssclk.c
  gfortran -O -o csssclk csssclk.o -L/opt/project/cassini/Linux.x86_64/lib64 \
           -ldas -lspicelib

  ----------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>

#include <SpiceUsr.h>

#include <das.h>

#define LEAPSECKERNEL "/opt/project/cassini/spice/kernels/lsk/leapseconds.tls"
#define SCLKKERNEL "/opt/project/cassini/spice/kernels/sclk/cassini.tsc"

/* fortran spicelib routines */

/* 
void clpool_ (void);
void ldpool_ (char *name, int namelen);
void utc2et_ (char *utc, double *et, int utclen);
void sce2s_  (int *sc, double *et, char *sclk, int sclklen);
void et2utc_
  (double *et, char *form, int *prec, char *utc, int formlen, int utclen);
*/
		
char *myname;

int
main (int argc, char *argv[])		/* csssclk - parse SCET return SCLK */
{

  int i, len;
  char time[80] = {""};
  char scet[25] = {""};
  char sclk[20] = {""};
  int sc = -82, prec = 3;
  char text[81];
  double et;

  int year, month, day, doy, hour, minute;
  double second;

/*----------------------------- csssclk --------------------------------*/

  myname = argv[0];

  clpool_c ();
  ldpool_c (LEAPSECKERNEL);
  ldpool_c (SCLKKERNEL, sizeof(SCLKKERNEL));


  len = 0;
  for (i = 1; i < argc; i++) {
    strncat (time, argv[i], 79 - len);
    strcat (time, " ");
    len = strlen (time);
  }

  while (1) {

    if (len > 0) {

      if (parsetime (time, &year, &month, &day, &doy,
                           &hour, &minute, &second)) {
	fprintf (stderr, "%s: error parsing time string %s\n", myname, time);
	return -1;
      }
      sprintf (time, "%04d-%03d // %02d:%02d:%06.3f",
                     year, doy, hour, minute, second);
      utc2et_c (time, &et, 24);
      sce2s_c  (&sc, &et, sclk);
      et2utc_c (&et, "D", &prec, scet);

      fprintf (stdout, "%s%s\n", sclk, scet);
      fflush (stdout);

    }

    if (argc > 1) break;

    if (!fgets (time, sizeof(time), stdin)) break;
    len = strlen (time) - 1;

  }

  return 0;

} /* csssclk - parse SCET return SCLK */
