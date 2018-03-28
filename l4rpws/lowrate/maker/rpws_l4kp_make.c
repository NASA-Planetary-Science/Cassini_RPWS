/* --------------------------------------------------------------------------

  rpws_key.c written by T. Averkamp  2003-06-04
  to read input from Bob Johnson's das reader (ordered triples) and
  write PDS Archive KEY data set. Uses median data values.
  Modifications:
 		29-Dec-00 TFA. Eliminate H2 data at 16.1 MHz
 		or higher, because reader has a problem.

		20-Feb-01 TFA. Attempt to eliminate ratty data.

		13-Mar-03 TFA. Take median measurement in each bin.

		04-Jun-03 TFA. Freqs changed to be exactly 10/decade.

		05-Jun-03 TFA. Processing of 42 magnetic frequencies added.

		12-Jun-03 TFA. Add Label File output.

		17-Oct-03 TFA. Provided storage for begstring, endstring.

		27-Oct-03 TFA. Eliminate quotes & Z from time field.

		02-Feb-04 TFA. Eliminate MFR3 data which overlaps HFR-A.

		13-Apr-04 TFA. Allow MFDR data from 181 Hz to 1492 Hz.
		               Eliminate MFR3 data which overlaps HFR-A,
			       even when MFR2 has been excluded from data.
					 
		2012-11-05 CWP. Added help text, removed unused variables, made sure
		          error exit values are non-zero.
				
  Command line arguments are begin and end time.

  -------------------------------------------------------------------------- */

#define NI              1440
#define NJ              73
#define NK              42
#define TBINMIN         60.0
#define FMIN            1.0
#define FMAX            16.1e6
#define BUFSIZE         65280
#define MAXINTERP       30
#define FILL            0.00

#define _POSIX_C_SOURCE 200112L

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <das2/core.h>

const char* myname = "rpws_kp_make";
FILE	*fpin;
int	first=1;
int	file_records=0;
char	start_time[128];
char	stop_time[128];
char	sclk_start[128];
char	sclk_stop[128];

void	sort (float *x, int index);
void	stuffit_lo (float value, float array[], int i, int j, int k);
void	stuffit_hi (float value, float array[], int i, int j, int k);
void	get_median_lo (float array[], int i, int j, int count, float *median);
void	get_median_hi (float array[], int i, int j, int count, float *median);

int main (int argc, char *argv[])
{
  float		e_array[NI][NJ] = {{0}};
  float		b_array[NI][NK] = {{0}};
  float		*array_lo, *array_hi;
  int		Isize1, Isize2;
  int   	e_count[NI][NJ] = {{0}};
  int   	b_count[NI][NK] = {{0}};
  int		data_ok, ni;
  int		year, month, day_month, day_year, hour, minute;
  int		Year, Month, Day, Doy, Hr, Mn;
  double	Sec;
  double	second;
  char		*Begstring = "1999-230T00:00:00";
  char		*Endstring = "1999-231T00:00:00";
  char		*begstring, *endstring;
  double	tbeg, tend, trange, tbinsize;
  float		toff;
  double	pkt_sec;
  short		YEAR, DOY;
  double	fmin, fmax, fscale;
  int		i, j, n, j0;
  int		nbytes, ntriples;
  char		*ph = ":  :0000";
  char		*E_options = "-lfdr LExEz -mfr 123ExEz -hfr ABC12ExpExmExEz -filter lfdr mfr hfr -car -ndp";
  char		*B_options = "-lfdr LBxByBz -mfr 123BxBz -filter lfdr mfr -ndp";
  float		data[BUFSIZE/4];
  float		*xyz;
  char      sCwd[128] = {'\0'};
  char		command[1024];

  /* ---------------------- rpwsrebin -------------------------------------- */

 
  if (argc < 3) {
    fprintf (stderr, "usage: %s begin-time end-time E_options B_options\n", myname);
    return -1;
  }
  begstring = argv[1];
  endstring = argv[2];
  E_options = argv[3];
  B_options = argv[4];
/*
  fprintf (stderr, "%s\n", begstring);
  fprintf (stderr, "%s\n", endstring);
  fprintf (stderr, "%s\n", E_options);
  fprintf (stderr, "%s\n", B_options);
*/
  /* parse begin time string */
  if (parsetime (begstring, &year, &month, &day_month, &day_year,
    &hour, &minute, &second)) {
    fprintf (stderr, "%s: error parsing begin time %s\n", myname, begstring);
    exit (-1);
  }
  sprintf (Begstring, "%4.4d-%3.3dT%2.2d:%2.2d:%2.2d", year, day_year, hour, minute, (int)second);
  begstring = Begstring;
  /* convert begin time to double precision seconds since epoch */
  tbeg =
    ttime (&year, &month, &day_month, &day_year, &hour, &minute, &second);

  /* parse end time string */
  if (parsetime (endstring, &year, &month, &day_month, &day_year,
    &hour, &minute, &second)) {
    fprintf (stderr, "%s: error parsing end time %s\n", myname, endstring);
    exit (-1);
  }
  sprintf (Endstring, "%4.4d-%3.3dT%2.2d:%2.2d:%2.2d", year, day_year, hour, minute, (int)second);
  endstring = Endstring;
  /* convert end time to double precision seconds since epoch */
  tend =
    ttime (&year, &month, &day_month, &day_year, &hour, &minute, &second);

  /* check for valid time interval */
  if (tbeg >= tend) {
    fprintf (stderr, "%s: error: begin time \"%s\" is less than or equal to "
      "end time \"%s\"\n", myname, begstring, endstring);
    exit (-1);
  }

  /* time scaling (tbeg to tend, exclusive) */
  trange = tend - tbeg;
  tbinsize = trange / (double)NI;
  if (tbinsize < TBINMIN) tbinsize = TBINMIN;
  ni = (int)(trange / tbinsize);

  /* log frequency scaling (fmin to fmax, inclusive) */
/*
  fmin = log10(FMIN);
  fmax = log10(FMAX);
*/
  fmin = -0.05;
  fmax =  7.25;
  fscale = (double)NJ / (fmax - fmin);

  Isize1 = NI*37*128*4;
  array_lo = (float *) malloc (Isize1);
  if ( array_lo == NULL)
  {
    printf ("I have a problem allocating the first array.\n");
    return -1;
  }
  Isize2 = NI*(NJ-37)*2200*4;
  array_hi = (float *) malloc (Isize2);
  if ( array_hi == NULL)
  {
    printf ("I have a problem allocating the second array.\n");
    return -1;
  }
/*
	Open Bob's das reader, querying for Electric data.
*/
  sprintf (command, "rpws_lr_cal -s");
  sprintf (command, "%s -tStart %s -tStop %s ", command, begstring, endstring);
  sprintf (command, "%s %s 2>/dev/null", command, argv[3]);
  
  fprintf(stderr, "rpws_kp_make cwd: %s\n", getcwd(sCwd, 127));
  fprintf(stderr, "rpws_kp_make exec: %s\n", command);
  if ( (fpin = popen (command, "r") ) == NULL)
  { fprintf (stderr, "Error opening Reader Pipe\n");
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: Error reading data\n", myname);
    exit (1);
  }

  /* loop while input from DasCasSpec on stdin */
  while( (nbytes = fgetpkt (fpin, ph, data, BUFSIZE)) > 0 ){

    /* process only :b0: packets */
    if (!strncmp (ph, ":b0:", 4)) {

      ntriples = nbytes / sizeof(float) / 3;
      if (ntriples * 12 != nbytes) fail ("YIKES! ntriples * 12 != nbytes");
/*
	Look for suspect data records.
*/
      data_ok = 1;
      xyz = data;
      xyz -= 3;
      for (n = 0; n < ntriples; n++)
      {
        xyz += 3;
        if (xyz[0] < 0.0) continue;
        if (xyz[1] >= 16.1e6 ) continue;
        if (xyz[1] < 0.2) continue;
        if (xyz[2] < 1.e-20) data_ok = 0;
      }
      if (!data_ok)
      {
        pkt_sec = tbeg + (double)data[0];
        emitt (pkt_sec, &Year, &Month, &Day, &Doy, &Hr, &Mn, &Sec);
        fprintf (stderr, "Problem with record: %4d-%3.3dT%2.2d:%2.2d:%6.3f %10.4E %10.4E %10.4E \n",
		     Year, Doy, Hr, Mn, Sec,
                     data[0],data[1],data[2]);
      }

      if (data_ok)
      {

        /* accumulate the data in the correct bins */
        xyz = data;
        xyz -= 3;
        for (n = 0; n < ntriples; n++)
	{
          xyz += 3;
          if (xyz[0] < 0.0) continue;
          if (xyz[1] >= 16.1e6 ) continue;	/* problem with reader here */
          if (xyz[1] < 0.2) continue;

/* Want to eliminate the first MFR2 sample, because it is noisy */

	  if ( (ntriples == 224) && ( n == 32 ) && ( (int)xyz[1] ==  192) ) continue;

/* Want to eliminate MFR-3 electric data which overlaps HFR-A data.
   Do this for packet size 224 and 160, where MFR2 has been excluded. */
   
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  3659) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  3992) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  4279) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  4651) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  4813) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  5086) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  5419) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  5758) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  6162) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  6575) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  7027) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  7490) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  7997) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  8518) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  9120) ) continue;
 	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] ==  9705) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] == 10165) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] == 11078) ) continue;
	  if ( ( (ntriples == 224) || (ntriples == 160) ) && ( (int)xyz[1] == 11799) ) continue;

/* If we have MFDR data, want to eliminate frequencies outside MFR2 range */

	  if ( (ntriples == 32) && ( n ==  0 ) && ( (int)xyz[1] ==    13) ) continue;
	  if ( (ntriples == 32) && ( n ==  1 ) && ( (int)xyz[1] ==    27) ) continue;
	  if ( (ntriples == 32) && ( n ==  2 ) && ( (int)xyz[1] ==    41) ) continue;
	  if ( (ntriples == 32) && ( n ==  3 ) && ( (int)xyz[1] ==    55) ) continue;
	  if ( (ntriples == 32) && ( n ==  4 ) && ( (int)xyz[1] ==    69) ) continue;
	  if ( (ntriples == 32) && ( n ==  5 ) && ( (int)xyz[1] ==    83) ) continue;
	  if ( (ntriples == 32) && ( n ==  6 ) && ( (int)xyz[1] ==    97) ) continue;
	  if ( (ntriples == 32) && ( n ==  7 ) && ( (int)xyz[1] ==   111) ) continue;
	  if ( (ntriples == 32) && ( n ==  8 ) && ( (int)xyz[1] ==   125) ) continue;
	  if ( (ntriples == 32) && ( n ==  9 ) && ( (int)xyz[1] ==   139) ) continue;
	  if ( (ntriples == 32) && ( n == 10 ) && ( (int)xyz[1] ==   153) ) continue;
	  if ( (ntriples == 32) && ( n == 11 ) && ( (int)xyz[1] ==   167) ) continue;
	  if ( (ntriples == 32) && ( n == 31 ) && ( (int)xyz[1] ==  1736) ) continue;

/* Have problem with Freeze mode, e.g., Trigger 92 */      
	  if ( (ntriples == 2326) && (xyz[1] == 314062.5))  continue;
	  if ( (ntriples == 2326) && (xyz[1] == 317187.5) ) continue;
	  if ( (ntriples == 2326) && (xyz[1] == 320312.5) ) continue;
	  if ( (ntriples == 2326) && (xyz[1] == 323437.5) ) continue;
	  if ( (ntriples == 2326) && (xyz[1] == 326562.5) ) continue;
	  if ( (ntriples == 2326) && (xyz[1] == 329687.5) ) continue;
	  if ( (ntriples == 2326) && (xyz[1] == 332812.5) ) continue;
	  if ( (ntriples == 2326) && (xyz[1] == 335937.5) ) continue;
/* Have problem with Freeze mode, e.g., Trigger 92 */
	  if ( (ntriples == 800) && (xyz[1] == 314062.5) ) continue;
	  if ( (ntriples == 800) && (xyz[1] == 317187.5) ) continue;
	  if ( (ntriples == 800) && (xyz[1] == 320312.5) ) continue;
	  if ( (ntriples == 800) && (xyz[1] == 323437.5) ) continue;
	  if ( (ntriples == 800) && (xyz[1] == 326562.5) ) continue;
	  if ( (ntriples == 800) && (xyz[1] == 329687.5) ) continue;
	  if ( (ntriples == 800) && (xyz[1] == 332812.5) ) continue;
	  if ( (ntriples == 800) && (xyz[1] == 335937.5) ) continue;
/* So eliminate all that Freeze mode data around 325 KHz */	  
          i = xyz[0] / tbinsize;		/* already offset time */
          if (i >= ni) continue;
          if (log10(xyz[1]) < fmin) continue;
          j = (log10(xyz[1]) - fmin) * fscale;
          if (j >= NJ) continue;
	  if (j < 37)
	    stuffit_lo (xyz[2] , array_lo, i, j, e_count[i][j]);
	  else
	    stuffit_hi (xyz[2] , array_hi, i, j-37, e_count[i][j]);
          e_count[i][j]++;
        }

      } /* if data_ok */

    } /* if :b0: packet */

  } /* while input available */

  /* now get medians . . . */
  for (i = 0; i < ni; i++)
  {
    j0 = 0;
    /* do a column (spectrum) at a time */
    for (j = 0; j < NJ; j++)
    { if (e_count[i][j])
      {
	j0++;
	if (j<37)
	  get_median_lo (array_lo, i, j, e_count[i][j], &e_array[i][j]);
	else
	  get_median_hi (array_hi, i, j-37, e_count[i][j], &e_array[i][j]);
      }
      else
        e_array[i][j] = FILL;
    } /* for this column (spectrum) */
  } /* for all values of i */

/*
	Now Open Bob's das reader, querying for Magnetic data.
*/
  sprintf (command, "rpws_lr_cal");
  sprintf (command, "%s -tStart %s -tStop %s ", command, argv[1], argv[2]);
  sprintf (command, "%s %s", command, argv[4]);
  fprintf(stderr, "rpws_kp_make cwd: %s\n", getcwd(sCwd, 127));
  fprintf(stderr, "rpws_kp_make exec: %s\n", command);
  if ( (fpin = popen (command, "r") ) == NULL)
  { fprintf (stderr, "Error opening Reader Pipe\n");
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: Error reading data\n", myname);
    return 1;
  }

  /* loop while input from DasCasSpec on stdin */
  while( (nbytes = fgetpkt (fpin, ph, data, BUFSIZE)) > 0){

    /* process only :b0: packets */
    if (!strncmp (ph, ":b0:", 4)) {

      ntriples = nbytes / sizeof(float) / 3;
      if (ntriples * 12 != nbytes) fail ("YIKES! ntriples * 12 != nbytes");
/*
	Look for suspect data records.
*/
      data_ok = 1;
      xyz = data;
      xyz -= 3;
      for (n = 0; n < ntriples; n++)
      {
        xyz += 3;
        if (xyz[0] < 0.0) continue;
        if (xyz[1] >= 16.1e6 ) continue;
        if (xyz[1] < 0.2) continue;
        if (xyz[2] < 1.e-20) data_ok = 0;
      }
      if (!data_ok)
      {
        pkt_sec = tbeg + (double)data[0];
        emitt (pkt_sec, &Year, &Month, &Day, &Doy, &Hr, &Mn, &Sec);
        fprintf (stderr, "Problem with record: %4d-%3.3dT%2.2d:%2.2d:%6.3f %10.4E %10.4E %10.4E \n",
		     Year, Doy, Hr, Mn, Sec,
                     data[0],data[1],data[2]);
      }

      if (data_ok)
      {

        /* accumulate the data in the correct bins */
        xyz = data;
        xyz -= 3;
        for (n = 0; n < ntriples; n++)
	{
          xyz += 3;
          if (xyz[0] < 0.0) continue;
          if (xyz[1] >= 16.1e6 ) continue;	/* problem with reader here */
          if (xyz[1] < 0.2) continue;

/* Want to eliminate the first MFR2 sample, because it is noisy */

	  if ( (ntriples == 224) && ( n == 32 ) && ( (int)xyz[1] ==  192) ) continue;
	  
/* If we have MFDR data, want to eliminate frequencies outside MFR2 range */

	  if ( (ntriples == 32) && ( n ==  0 ) && ( (int)xyz[1] ==    13) ) continue;
	  if ( (ntriples == 32) && ( n ==  1 ) && ( (int)xyz[1] ==    27) ) continue;
	  if ( (ntriples == 32) && ( n ==  2 ) && ( (int)xyz[1] ==    41) ) continue;
	  if ( (ntriples == 32) && ( n ==  3 ) && ( (int)xyz[1] ==    55) ) continue;
	  if ( (ntriples == 32) && ( n ==  4 ) && ( (int)xyz[1] ==    69) ) continue;
	  if ( (ntriples == 32) && ( n ==  5 ) && ( (int)xyz[1] ==    83) ) continue;
	  if ( (ntriples == 32) && ( n ==  6 ) && ( (int)xyz[1] ==    97) ) continue;
	  if ( (ntriples == 32) && ( n ==  7 ) && ( (int)xyz[1] ==   111) ) continue;
	  if ( (ntriples == 32) && ( n ==  8 ) && ( (int)xyz[1] ==   125) ) continue;
	  if ( (ntriples == 32) && ( n ==  9 ) && ( (int)xyz[1] ==   139) ) continue;
	  if ( (ntriples == 32) && ( n == 10 ) && ( (int)xyz[1] ==   153) ) continue;
	  if ( (ntriples == 32) && ( n == 11 ) && ( (int)xyz[1] ==   167) ) continue;
	  if ( (ntriples == 32) && ( n == 31 ) && ( (int)xyz[1] ==  1736) ) continue;

          i = xyz[0] / tbinsize;		/* already offset time */
          if (i >= ni) continue;
          if (log10(xyz[1]) < fmin) continue;
          j = (log10(xyz[1]) - fmin) * fscale;
          if (j >= NK) continue;
	  if (j < 37)
	    stuffit_lo (xyz[2] , array_lo, i, j, b_count[i][j]);
	  else
	    stuffit_hi (xyz[2] , array_hi, i, j-37, b_count[i][j]);
          b_count[i][j]++;
        }

      } /* if data_ok */

    } /* if :b0: packet */

  } /* while input available */

  /* now get medians . . . */
  for (i = 0; i < ni; i++)
  {
    j0 = 0;
    /* do a column (spectrum) at a time */
    for (j = 0; j < NK; j++)
    { if (b_count[i][j])
      {
	j0++;
	if (j<37)
	  get_median_lo (array_lo, i, j, b_count[i][j], &b_array[i][j]);
	else
	  get_median_hi (array_hi, i, j-37, b_count[i][j], &b_array[i][j]);
      }
      else
        b_array[i][j] = FILL;
    } /* for this column (spectrum) */
  } /* for all values of i */

  /* now output the data */
  for (i = 0; i < ni; i++)
  {
    j0 = 0;
    for (j = 0; j < NJ; j++)
    { if (e_count[i][j])
	j0++;
    }
    for (j = 0; j < NK; j++)
    { if (b_count[i][j])
	j0++;
    }
    /* if there were any data, generate a time tag and write the spectrum */
    if (j0)
    { toff = i * tbinsize;
      pkt_sec = tbeg + (double)toff;
      emitt (pkt_sec, &Year, &Month, &Day, &Doy, &Hr, &Mn, &Sec);
      YEAR = Year;
      DOY  = Doy;
      if (first)
      { first = 0;
        printf ("%4.4d-%3.3dT00:00:00.000", Year, Doy);
        printf (" 0");
        for (j=0;j<NJ;j++)
          printf (" %9.3e", pow(10.0, (0.+((float)j)/10)));
        for (j=0;j<NK;j++)
          printf (" %9.3e", pow(10.0, (0.+((float)j)/10)));
        printf ("\r\n");

/*	Keep track of how many records we wrote */

        file_records++;
      }
      
/*	For PDS need "yyyy-doyThh:mm:30.000Z" for time	*/
/*	Change time format to  yyyy-doyThh:mm:30.000 for time	*/

      printf ("%4.4d-%3.3dT%2.2d:%2.2d:30.000", Year, Doy, Hr, Mn);

/*	Include a one-digit data quality flag		*/

      printf (" 0");
      
      for (j=0;j<NJ;j++)
        printf (" %9.3e", e_array[i][j]);
      for (j=0;j<NK;j++)
        printf (" %9.3e", b_array[i][j]);

      printf ("\r\n");

/*	Keep track of how many records we wrote */

      file_records++;

/*
      if (!fwrite (&YEAR, sizeof(short), 1, stdout)) fail ("write error");
      if (!fwrite (&DOY,  sizeof(short), 1, stdout)) fail ("write error");
      if (!fwrite (&toff, sizeof(float), 1, stdout)) fail ("write error");
      if (!fwrite (e_array[i], sizeof(float)*NJ, 1, stdout)) fail ("write error");
      if (!fwrite (b_array[i], sizeof(float)*NK, 1, stdout)) fail ("write error");
*/

    } /* if any data */

  } /* for all rebinned spectra */

  return 0;
}

void	sort (float x[], int n)
{ float tempx;
  int i, j, k, l, m;
  
/* shell sort FORTRAN-II style */

  m = n;
L20:
  m = m/2;
  if (m == 0)
    return ;		/* done */
  k = n - m;
  j = 1;
L41:
  i = j;
L49:
  l = i + m;
  if (x[i-1] > x[l-1])
  { tempx = x[i-1];
    x[i-1] = x[l-1];
    x[l-1] = tempx;
    i = i - m;
    if ( (i-1) >= 0) goto L49;
  }
    j++;
    if (j > k) goto L20;
    goto L41;
}

void	stuffit_lo (float value, float array[], int i, int j, int k)
{
/*
  float		array_lo[NI][37][128];
*/
	    array[128*37*i+128*j+k] = value;
}
void	stuffit_hi (float value, float array[], int i, int j, int k)
{
/*
  float		array_lo[NI][NJ-37][2200];
*/
	    array[2200*(NJ-37)*i+2200*j+k] = value;
}
void	get_median_lo (float array[], int i, int j, int count, float *median)
{
/*
  float		array_lo[NI][37][128];
*/
	  sort (&array[128*37*i+128*j+0], count);
	  if (count & 1)
	    *median = array[128*37*i+128*j+(count/2)];
	  else
	    *median = (array[128*37*i+128*j+(count/2)]+array[128*37*i+128*j+((count/2)-1)])/2.0;
}
void	get_median_hi (float array[], int i, int j, int count, float *median)
{
/*
  float		array_lo[NI][NJ-37][2200];
*/

	  sort (&array[2200*(NJ-37)*i+2200*j+0], count);
	  if (count & 1)
	    *median = array[2200*(NJ-37)*i+2200*j+(count/2)];
	  else
	    *median = (array[2200*(NJ-37)*i+2200*j+(count/2)] +
	               array[2200*(NJ-37)*i+2200*j+((count/2)-1)])/2.0;
}
