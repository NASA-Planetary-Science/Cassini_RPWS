/*
	hfrselect.c	written 20-Feb-2001 by TFA to output HFR data
			for a selected frequency and a specified time
			interval for a web request form.
	Modifications:
			28-Dec-00 TFA. For records containing multiple
			measurements at one frequency, get an average
			value for each frequency. This takes care of
			H1 and H2 when num_filters > 1.

			28-Dec-00 TFA. Eliminate H2 data at 16.1 MHz
			or higher, because reader has a problem.
*/
#include <stdio.h> 
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include <das.h>
#include "Cext.h"
#include "CasDas.h"

#define  TRUE   1 
#define  FALSE  0 
#define  MAXENTRIES     100

typedef struct
{
   char *name;
   char *val;
}
entry;

static	FILE	*fperror;
static	char	*progname;
static	char	MIME_Header[40];
static	char	gif_file[8];
static	int	calibrated;

void    unescape_url (char *url);
void    plustospace (char *str);
char    *makeword (char *line, char stop);
char    *fmakeword (FILE *f, char stop, int *len);

int main(int argc,char *argv[])
{
  int		num_rec=-1;
  int		nbytes;
  FILE		*fpin;
  Bool		bStatus;
  UCHAR		arBuf[16];
  int		i, ii, index, first=TRUE;
  int		Zero=0, One=1, Axis=30;
  float		Fzero=0.0, Fone=1.0;
  ULONG		nBufLen;
  float		x[65536],y[65536];
  float		pfTrip[256000];
  float		Tlast=-9999.0;
  float		tsave, fsave, amp;
  float		freq_select=20000.0;		/* sometime make variable */
  ULONG		newLength, n_avg;
  ULONG		nLength;
  CasDasPacket	DasPkt;
  float		xmin=log10(2000.), xmax=log10(2.e7), ymin=-18., ymax=-10.;
  int		year, month, mday, yday, hour, minute;
  double	second;
  double	base, tt, ttstop;
  float		*buf;
  char		ylabel[200];
  char		*tstring = "1999-01-01 (001) 00:00:00.000";
  char		*tstart = "1999-001T00:00:00.000";
  char		*tstop  = "1999-001T00:00:00.000";
  char		*toplbl="Cassini HFR/MFR Data 1999-01-01 (001) 00:00:00.000";
  int		num_files, cl;
  entry		entries [MAXENTRIES];
  char  	command[1024];
  int nCnt,nRead,nLen,nSize;
  int nTotal=0;
  float last_time, dt, fr, sp;
  long  kindex;
        
   progname = argv [0];
   if (!(fperror = fopen ("/var/tmp/tfa-cgi.log", "w")))
   {
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("%s:  error opening /var/tmp/tfa-cgi.log\n", progname);
      exit (-1);
   }
   fprintf (fperror, "Opened tfa-cgi.log file\n");
   fflush (fperror);

   if (!getenv ("REQUEST_METHOD"))
   {
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("This script should be referenced with a METHOD of POST.\n");
      printf ("If you don't understand this, see this ");
      printf ("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/overview.html\">forms overview</A>.%c",10);
      exit (1);
   }
   else if (strcmp (getenv ("REQUEST_METHOD"), "POST"))
   {
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("This script should be referenced with a METHOD of POST.\n");
      printf ("If you don't understand this, see this ");
      printf ("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/overview.html\">forms overview</A>.%c",10);
      exit (1);
   }

   if (!getenv ("CONTENT_TYPE"))
   {
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("This script can only be used to decode form results. \n");
      exit (1);
   }
   if (strcmp (getenv ("CONTENT_TYPE"), "application/x-www-form-urlencoded"))
   {
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("This script can only be used to decode form results. \n");
      exit (1);
   }

   fprintf (fperror, "Testing %s\n", progname);
   cl = atoi (getenv ("CONTENT_LENGTH"));
   for (i = 0; cl && (!feof (stdin)); i++)
   {
      entries [i].val = fmakeword (stdin, '&', &cl);
      plustospace (entries [i].val);
      unescape_url (entries [i].val);
      entries [i].name = makeword (entries [i].val,'=');
      fprintf (fperror, " %s %s \n", entries [i].name, entries [i].val );
   }

 if (parsetime (entries[0].val, &year, &month, &mday, &yday,
 		 &hour, &minute, &second))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, entries[0].val);
    exit (1);
 }
 fprintf (fperror, " %d %d %d %d %d %d %lf \n",
 		 year, month, mday, yday, hour, minute, second);

 base = ttime (&year, &month, &mday, &yday, &hour, &minute, &second);
/* base = base + 1200.0; */		/* Bob goes 20 minutes earlier */
 emitt (base, &year, &month, &mday, &yday, &hour, &minute, &second);   

 sprintf (tstart, "%04d-%03dT%02d:%02d:%06.3f",
 	      year, yday, hour, minute, second);

 if (parsetime (entries[1].val, &year, &month, &mday, &yday,
 		 &hour, &minute, &second))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, entries[0].val);
    exit (1);
 }
 fprintf (fperror, " %d %d %d %d %d %d %lf \n",
 		 year, month, mday, yday, hour, minute, second);

 tt = ttime (&year, &month, &mday, &yday, &hour, &minute, &second);
 ttstop = tt;
 tt = tt + 1200.0;		/* Bob's time FU by 20 minutes */
 emitt (tt, &year, &month, &mday, &yday, &hour, &minute, &second);   

 sprintf (tstop, "%04d-%03dT%02d:%02d:%06.3f",
 	      year, yday, hour, minute, second);

 sscanf (entries[2].val, "%f", &freq_select);


  if ( !strcmp (entries[3].val, "TEXT") )	/* Text file ? */
  {
    fprintf (fperror, "entries[3].val is equal to TEXT\n");
    sprintf (MIME_Header, "Content-type: text/plain");
    sprintf (gif_file, "TEXT");
    if ( !strcmp (entries[4].name, "text_mime_header") )
      sprintf (MIME_Header, "Content-type: %s", entries[4].val);
  }

  fprintf (fperror, "MIME Header is %s\n", MIME_Header);

/*	  sprintf (command, "/home/raj/das/cassini/dasCasSpec");
  sprintf (command, "/opt/project/cassini/SunOS.sparc/bin/rpws_lr_pdsrdr");
  sprintf (command, "%s -hfr ABC12EuEx -n mfr3_hfra -n hf1_hfrc -n hf1pwr -n bad_data -n bascap",
  	   command);
*/

  sprintf (command, "/home/tfa/cassini/hfr/web/dasrdb0/pdsrdr_hfr.sh");
  
/*
  sprintf (command, "%s -hfr ABC12ExpEx -filter hfr  -Nix3  +NixC  -PitchHF150KHz ",
           command);
*/
/*
  sprintf (command, "%s -tStart %s -tStop %s",
  	   command, tstart, tstop);
*/
  sprintf (command, "%s %s %s",
        command, tstart, tstop);
               	   
  sprintf (command, "%s 2>>/var/tmp/tfa-cgi.log", command);

  fprintf (fperror, "reader string: %s\n", command);
  
  if ( (fpin = popen (command, "r") ) == NULL)
  { fprintf (fperror, "Error opening Reader Pipe\n");
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: Error reading data\n", progname);
    exit (1);
  }



/* Construct Program Context */  
  DasPkt.nLength=0;
  DasPkt.pData=arBuf;
  DasPkt.nFileDes=0;	/* stdin */
  DasPkt.nRdWrSize=-1;  /* All at once, use packet size as write size */

  DasPkt.nLength=0;
  DasPkt.pData=arBuf;
  Tlast = -9999.0;
  last_time = -9999.0;
  kindex=0;
  
  while(1){

there:

    if ( fscanf(fpin, "%f %f %f", &dt, &fr, &sp) == EOF )
    { pclose (fpin);
      if (first)
      {
        fprintf (stdout, "Content-type: text/plain\n\n");
        printf ("No Data for requested time interval\n");
      }
      exit (0);
    }
that:
    if ( (dt == last_time) ||  (last_time == -9999.0) )
    { 
      pfTrip[kindex*3+0] = dt;
      pfTrip[kindex*3+1] = fr;
      pfTrip[kindex*3+2] = sp;
      last_time = dt;
      if (kindex < 65536)
      	kindex++;
      goto there;
    }

/* reached end of record */


    nLength=kindex*3;  /* kindex is number of triplets in record */
    kindex = 0;
    last_time = dt;
      for(i=0;i<nLength;i+=3)  
        if (pfTrip[i+0] >= 0.0)
	{
	  if (Tlast == -9999.0)
	  {
	    Tlast = pfTrip[i+0];
	    index = 0;
	  }
	  if ( 1 )
	  {
	    if ( pfTrip[i+1] < 16.1e6 )
	    { x[index] = pfTrip[i+1];		/* freq */
	      y[index] = pfTrip[i+2];		/* amp  */
	      index++;
	    }
	  }
	}
	if (index > 0)
	{
	  {
	   num_rec++;
	   {tt = base + (double)Tlast;
	    if (tt > ttstop)
	    {
              if (first)
              {
                fprintf (stdout, "Content-type: text/plain\n\n");
                printf ("No Data for requested time interval\n");
              }
              exit(127);
            }
	    if (first)
	    {  
	       first = FALSE;
	       fprintf (stdout, "%s\n\n", MIME_Header);
	       fflush (stdout);
	    }
            emitt (tt, &year, &month, &mday, &yday, &hour, &minute, &second);
	    
	    {
	      for (ii=1; ii<index; ii++)
	      { if ( (x[ii-1] < freq_select) &&
	             (x[ii] >= freq_select) )
		{
	          fprintf (stdout, "%04d-%03dT%02d:%02d:%06.3f",
		           year, yday,hour, minute, second);
		  fprintf (stdout, "%12.1f %10.3e\n", x[ii], y[ii]);
		  fflush (stdout);
		}
	      }
            }
	   }
	   Tlast = last_time;
	   index = 0;
	  }
        }
        goto that;
  }

return 1;
}
