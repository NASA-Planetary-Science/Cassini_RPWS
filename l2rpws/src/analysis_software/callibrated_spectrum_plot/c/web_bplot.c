/*
        web_bplot.c     written 10/25/01 by TFA to process
                        Cassini RPWS browse data files,
			
        Modifications:
			02/14/01. read modified browse file format,
			          including year and day-of-year. TFA
			02/21/01. web_avpow version for web requests. TFA
			02/26/01. plotting option added. TFA
			07/18/01. for frequency range .5-5.6 MHz, set
			          int.pow to 1.5e-12 if below that. TFA
			07/18/01. Antenna Base Capacitance correction
				  applied: increase voltage by 7.9 db. TFA
*/
#include <stdio.h> 
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <string.h>
#include <libgen.h>
#include <time.h>
#include <math.h>
#include <das.h>
#include "Cext.h"
#include "CasDas.h"
#include <rtiu.h>
#include <util.h>
#include <utilt.h>

#define  MAXFILES 1024
#define  MAXCHARS 200
#define  MAXENTRIES     100
#define  True   1
#define  False  0

#define NJ 73
#define FMIN 1.0
#define FMAX 16.1e6
#define FILL 1.0e-32

struct file_structure
{
   char line [MAXCHARS];
   int year;
   int day;
   int hr;
   int mn;
   int num_recs;
   float sec;
   double ms;
   double st_sec;
   double sp_sec;
};

typedef struct
{
   char *name;
   char *val;
}
entry;

static	FILE	*fperror;
static	char	MIME_Header[40];
static	char	gif_file[8];
static	char	*device_name;

char	*progname;
char	start_string[80], stop_string[80];
char	command[1024];
double	avg_deltat, slide_deltat;
double	flow, fhigh, ybottom, ytop;
double  bandwidth;
double  ant_corr;
float	*av_array;
int	nitems, nslide, ilo, ihi;
int	remove_spikes=False;
int	magnetics=True;
int	fake_mfr2=False;
int	browse=True;
double	spike_level;
int	antenna_adj=False;
int	plot_option;
void	sort_xy (float *x, float *y, int index);
void    unescape_url (char *url);
void    plustospace (char *str);
char    *makeword (char *line, char stop);
char    *fmakeword (FILE *f, char stop, int *len);

main(int argc,char **argv)
{
int     i, j, ii, cl, index, num_entries, num_files;
int     year, month, day, doy, hr, mn;
int	first = True;
double	percent;
double  sec, start_sec, stop_sec;
double	fmin, fmax, fscale, f1, f2, fcenter;
struct  file_structure files [MAXFILES];       
entry	entries [MAXENTRIES];
char	*tstart, *tstop, *query_options, *qopt, *dataset;
float	tsave, fsave, amp, parm, Fzero=0.0, Fone=1.0;
  int           Zero=0, One=1, Two=2, Three=3, Axis=30, Six=6;
  ULONG         newLength, n_avg;
  FILE *fpin;
  Bool		bStatus;
  UCHAR		arBuf[165536];
  float		x[500000], y[500000];
  float		ypeak[5000], yavg[5000];
  int		num_avg;
  float		*pfTrip;
  ULONG		nLength;
  CasDasPacket	DasPkt;
  char          ylabel[200];
  char          *tstring = "1999-01-01 (001) 00:00:00.000";
  char          *toplbl="Cassini HFR/MFR Data 1999-01-01 (001) 00:00:00.000";
  float         xmin=log10(2000.), xmax=log10(2.e7), ymin=-18., ymax=-10.;
  
/***************************************************************/

   progname = argv [0];

   if (!(fperror = fopen ("/var/tmp/tfa-cgi.log", "w")))
   {
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("%s:  error opening /var/tmp/tfa-cgi.log\n", progname);
      exit (-1);
   }
   fprintf (fperror, "Opened tfa-cgi.log file\n");

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
/*
    fprintf (stdout, "Content-type: text/plain\n\n");
*/
   for (i = 0; cl && (!feof (stdin)); i++)
   {
      entries [i].val = fmakeword (stdin, '&', &cl);
      plustospace (entries [i].val);
      unescape_url (entries [i].val);
      entries [i].name = makeword (entries [i].val,'=');
      fprintf (fperror, " %d %s %s \n", i, entries [i].name, entries [i].val );
/*
      fprintf (stdout, " %d %s %s \n", i, entries [i].name, entries [i].val );
*/
   }
   num_entries = i;
   tstart = "";
   tstop  = "";
   query_options  = "";
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "query_options") )
     {  query_options = entries[i].val;
        break;
     }
   }
   fprintf (fperror, "query_options = %s \n", query_options);   
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "dataset") )
     {  dataset = entries[i].val;
        break;
     }
   }
   fprintf (fperror, "dataset = %s \n", dataset);   
/* See if browse dataset is desired. */
   if ( (qopt = strstr(dataset,"browse")) )
     browse = True;
   else
     browse = False;
/* See if fake_mfr2 dataset is desired. */
   if ( (qopt = strstr(dataset,"MFDR")) )
     fake_mfr2 = True;
   else
     fake_mfr2 = False;

/* Search for magnetic options. */
   if ( (qopt = strstr(query_options,"Bx")) ||
        (qopt = strstr(query_options,"By")) ||
	(qopt = strstr(query_options,"Bz")) )	/* any magnetics ? */
   {						/* if so, electrics too ? */
     if ( (qopt = strstr(query_options,"Ex")) ||
          (qopt = strstr(query_options,"Ez")) )
     {	  
       fprintf (stdout, "Content-type: text/plain\n\n");
       printf ("Can not process both electric and magnetic data.\n");
       printf ("Please select one or the other.\n");
       exit (-1);
     }
     else
       magnetics = True;
    }
    else
      magnetics = False;
   fprintf (fperror, "magnetics option = %d\n", magnetics);
   if (magnetics && browse)
   {	  
     fprintf (stdout, "Content-type: text/plain\n\n");
     printf ("Browse dataset does not contain magnetic sensor data.\n");
     exit (-1);
   }

   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "tstart") )
     {  tstart = entries[i].val;
        break;
     }
   }
   fprintf (fperror, "tstart = %s \n", tstart);
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "tstop") )
     {  tstop = entries[i].val;
        break;
     }
   }
   fprintf (fperror, "tstop = %s \n", tstop);
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "flow") )
     { sscanf (entries[i].val, "%lf", &flow);		/* lower freq, Hz  */ 
        break;
     }
   }
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "fhigh") )
     { sscanf (entries[i].val, "%lf", &fhigh);		/* upper freq, Hz  */
       break;
     }
   }
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "ybottom") )
     { sscanf (entries[i].val, "%lf", &ybottom);	/* minimum amp  */ 
        break;
     }
   }
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "ytop") )
     { sscanf (entries[i].val, "%lf", &ytop);		/* maximum amp  */ 
        break;
     }
   }
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "title") )
     { toplbl = entries[i].val;			/* title  */ 
        break;
     }
   }
   fprintf (fperror, "title:  %s\n",
            toplbl);
   fprintf (fperror, "flow,fhigh,ybottom,ytop:  %lf %lf %e %e \n",
            flow,fhigh,ybottom,ytop);
/*
	Find out which type of output is desired.
*/
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "device_name") )
     {  device_name = entries[i].val;
        break;
     }
   }
   if ( !strcmp(device_name, "Z") )		/* GIF ? */
   { sprintf (gif_file, "-/gif");
     sprintf (MIME_Header, "Content-type: image/gif");
     for (i=0; i<num_entries; i++)
     {
       if ( !strcmp(entries[i].name, "gif_mime_header") )
       {  sprintf (MIME_Header, "Content-type: %s", entries[i].val);
          break;
       }
     }
   }
   else    if ( !strcmp(device_name, "PS") )		/* PS ? */
   { sprintf (gif_file, "-/ps");
     sprintf (MIME_Header, "Content-type: application/postscript");
     for (i=0; i<num_entries; i++)
     {
       if ( !strcmp(entries[i].name, "ps_mime_header") )
       {  sprintf (MIME_Header, "Content-type: %s", entries[i].val);
          break;
       }
     }
   }
   else if ( !strcmp(device_name, "TEXT") )		/* TEXT ? */
   { sprintf (gif_file, "TEXT");
     sprintf (MIME_Header, "Content-type: text/plain");
     for (i=0; i<num_entries; i++)
     {
       if ( !strcmp(entries[i].name, "text_mime_header") )
       {  sprintf (MIME_Header, "Content-type: %s", entries[i].val);
          break;
       }
     }
   }
   fprintf (fperror, "device_name, mime_header:  %s %s \n",
            device_name, MIME_Header);
/*
	Find out whether antenna resonance adjustment is desired.
*/
   antenna_adj = False;
   for (i=0; i<num_entries; i++)
   {
     if ( !strcmp(entries[i].name, "ant_res") &&
          !strcmp(entries[i].val, "adjust") )
     {  antenna_adj = True;
        break;
     }
   }
 if (antenna_adj)
   fprintf (fperror, "Will adjust for antenna resonance.\n");
 else
   fprintf (fperror, "Will NOT adjust for antenna resonance.\n");

 if (parsetime (tstart, &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, tstart);
    exit (1);
 }
 sprintf (start_string, "%04d-%02d-%02d (%03d) %02d:%02d:%02d\n",
       		 year, month, day, doy, hr, mn, (int)sec);
 start_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );
 sprintf (tstart, "%04d-%03dT%02d:%02d:%06.3f",
              year, doy, hr, mn, sec);

 if (parsetime (tstop, &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, tstop);
    exit (1);
 }
 sprintf (stop_string, "%04d-%02d-%02d (%03d) %02d:%02d:%02d\n",
       		 year, month, day, doy, hr, mn, (int)sec);
 stop_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );
 emitt (stop_sec+1200.0, &year, &month, &day, &doy, &hr, &mn, &sec);
 sprintf (tstop, "%04d-%03dT%02d:%02d:%06.3f",
              year, doy, hr, mn, sec);

  if (!browse && !fake_mfr2)
  {
    sprintf (command, "/opt/project/cassini/SunOS.sparc/bin/rpws_lr_pdsrdr");
    sprintf (command, "%s -tStart %s -tStop %s", command, tstart, tstop);
    sprintf (command, "%s %s", command, query_options);
/*    sprintf (command, "%s 2>>/var/tmp/tfa-cgi.log", command);	*/
    if ( (fpin = popen (command, "r") ) == NULL)
    { fprintf (fperror, "Error opening Reader Pipe\n");
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("%s: Error reading data\n", progname);
      exit (1);
    }
  }
  else if (!browse && fake_mfr2)
  {
    sprintf (command, "/home/raj/das/cassini/dasCasSpec");
    sprintf (command, "%s -tStart %s -tStop %s", command, tstart, tstop);
    sprintf (command, "%s %s", command, query_options);
    sprintf (command, "%s %s", command,
    		 " | /home/tfa/cassini/mfr/mfdr/sond_del");
    sprintf (command, "%s %s %s", command, tstart, tstop);
    sprintf (command, "%s %s", command, query_options);
    sprintf (command, "%s %s", command,
    		" | /home/tfa/cassini/mfr/mfdr/mfr2_switch");
    sprintf (command, "%s %s %s", command, tstart, tstop);
    sprintf (command, "%s %s", command, query_options);
    sprintf (command, "%s 2>>/var/tmp/tfa-cgi.log", command);
    if ( (fpin = popen (command, "r") ) == NULL)
    { fprintf (fperror, "Error opening Reader Pipe\n");
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("%s: Error reading data\n", progname);
      exit (1);
    }
  }
  else
  {
    sprintf (command, "/home/tfa/cassini/browse/web/browser.exe");
    sprintf (command, "%s %s %s", command, tstart, tstop);
    sprintf (command, "%s 2>>/var/tmp/tfa-cgi.log", command);
    if ( (fpin = popen (command, "r") ) == NULL)
    { fprintf (fperror, "Error opening Reader Pipe\n");
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("%s: Error reading data\n", progname);
      exit (1);
    }
  }

   fprintf (fperror, "%s \n", command);   
   fflush (fperror);

  if (!magnetics)
    sprintf (ylabel, "E\\u2\\d/\\(0530)f, Spectral Density (V\\u2\\dm\\u-2\\dHz\\u-1\\d)"); 
  else
    sprintf (ylabel, "B\\u2\\d/\\(0530)f, Spectral Density (nT\\u2\\d\\dHz\\u-1\\d)"); 



/* Construct Program Context */  
  DasPkt.nLength=0;
  DasPkt.pData=arBuf;
  DasPkt.nFileDes=0;	/* stdin */
  DasPkt.nRdWrSize=-1;  /* All at once, use packet size as write size */

  DasPkt.nLength=0;
  DasPkt.pData=arBuf;
  index = 0;
  while(1){

read_data:

    DasPkt.nLength=0;
    bStatus=CasDasPacket_Read(&DasPkt, fpin);
    if(bStatus==False){
      if (first)
      {
        fprintf (stdout, "Content-type: text/plain\n\n");
        printf ("No Data for requested time interval\n");
        exit(127);
      }
      goto plot_data;
    }

    pfTrip=(float*)(DasPkt.pData+8);
    nLength=DasPkt.nLength-8;  /* buffer len - header bytes = data len */
    nLength/=4;  /* 4 bytes per float */

/*	Search for case of H1 or H2 num_filters > 1	*/

    if (pfTrip[1] == pfTrip[4])		/* identical frequencies? */
    {
      tsave = pfTrip[0];
      fsave = pfTrip[1];
      amp   = pfTrip[2];
      newLength = 0;
      n_avg = 1;
      for (i=3;i<nLength;i+=3)  
      { if (pfTrip[i+0] != tsave) goto read_data;	/* sanity check */
        if (pfTrip[i+1] == fsave)		/* same frequency ? */
	{ amp += pfTrip[i+2];
	  n_avg++;
	}
	else
	{ pfTrip[newLength+1] = fsave;
	  pfTrip[newLength+2] = amp/(float)n_avg;
	  newLength += 3;
      	  fsave = pfTrip[i+1];
      	  amp   = pfTrip[i+2];
          n_avg = 1;
	}
      }
      pfTrip[newLength+1] = fsave;
      pfTrip[newLength+2] = amp/(float)n_avg;
      newLength += 3;
      nLength = newLength;
    }
/*
	Process, store data
*/
      for(i=0;i<nLength;i+=3)  
      { if ( (pfTrip[i+0] >= 0.0) &&
	     (pfTrip[i+0] <  (stop_sec-start_sec)) &&
	     (pfTrip[i+1] >= flow) &&
	     (pfTrip[i+1] <  16.1e6 ) &&
	     (pfTrip[i+1] <  fhigh ) )
	{
	   first = False;
	   x[index] = pfTrip[i+1];		/* freq */
	   y[index] = pfTrip[i+2];		/* amp  */
	   if (index < 500000) index++;
	   else
	   {
	     fprintf (fperror, "Ran out of space\n");
             fprintf (stdout, "Content-type: text/plain\n\n");
             printf ("The interval you specified is too long.\n");
	     printf ("I do not have enough storage allocated.\n");
	     printf ("Please shorten the interval and try again.\n");
             exit (-1);
	   }
	}
      }
  }
plot_data:
  if (first)
  {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("No Data for requested time interval\n");
    exit(127);
  }
      sort_xy (x, y, index);
/* data is sorted, now average matching frequencies */
      j=0;
      yavg[j] = y[j];
      ypeak[j] = y[j];
      num_avg=1;
      for (i=1; i<index; i++)
      {
	if ( x[i] == x[j] )
	{ yavg[j] += y[i];
	  num_avg++;
	  if ( y[i] > ypeak[j] ) ypeak[j] = y[i];	/* peak */
	}
	else
	{ yavg[j] /= num_avg;
	  num_avg=1;
	  j++;
	  x[j] = x[i];
	  ypeak[j] = y[i];
	  yavg[j] = y[i];
	}
      }
      yavg[j] /= num_avg;
      index = j+1;
      if (antenna_adj && !magnetics)
      {
        for (i=0; i<index; i++)
	{
	  fcenter = x[i];
/*
	Calculate Antenna Resonance Correction Factor. TFA 10-Sep-01
*/
/* dipole */
          ant_corr = 1.0-fcenter*fcenter/((8.75e6)*(8.75e6));
          ant_corr = (1.0+0.58/27.49)/(ant_corr*ant_corr + (0.58/27.49) );

/*	monopole	*/
/*
   ant_corr = 1.0-fcenter*fcenter/((9.36e6)*(9.36e6));
   ant_corr = (1.0+(0.1255*0.1255))/(ant_corr*ant_corr + (0.1255*0.1255) );
*/

          fprintf (fperror,"%10d %10.3e %10.3e \n", i, fcenter, ant_corr);
          yavg[i] /= ant_corr;
	  ypeak[i] /= ant_corr;
	}
      }
      fprintf (stdout, "%s\n\n", MIME_Header);
      fflush (stdout);
      if ( strcmp (gif_file, "TEXT") )
      {
        pgbegin_ (&Zero, gif_file, &One, &One, strlen(gif_file));
        pgscr_ (&Zero, &Fone, &Fone, &Fone); 	/* invert black & white */
        pgscr_ (&One, &Fzero, &Fzero, &Fzero);  /* invert black & white */
      }

   if ( strcmp (gif_file, "TEXT") )
   { pgpage_ ();
     pgslw_ (&Three);
     xmin=log10(flow);
     xmax=log10(fhigh);
     ymin=log10(ybottom);
     ymax=log10(ytop);

fprintf (fperror, "xmin,xmax,ymin,ymax: %f %f %f %f \n", xmin,xmax,ymin,ymax);

     pgenv_ ( &xmin, &xmax, &ymin, &ymax, &Zero, &Axis);
     pglabel_ ("Frequency (Hz)",
			ylabel,
			toplbl, strlen("Frequency (Hz)"),
  			strlen(ylabel),
			strlen(toplbl));

     parm=1.035;
     emitt (start_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
     sprintf (tstring, "%04d-%02d-%02d (%03d) %02d:%02d:%06.3f", year, month, day,
  			doy, hr, mn, sec);
     pgmtext_ ("LV", &Fzero, &parm, &Fzero, tstring, 2L, strlen(tstring));
     emitt (stop_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
     sprintf (tstring, "%04d-%02d-%02d (%03d) %02d:%02d:%06.3f", year, month, day,
  			doy, hr, mn, sec);
     pgmtext_ ("RV", &Fzero, &parm, &Fone, tstring, 2L, strlen(tstring));

     pgslw_ (&Six);

      for (ii=0; ii<index; ii++)
      { 
         x[ii] = log10(x[ii]);
         ypeak[ii] = log10(ypeak[ii]);
         pgmove_ (&x[ii], &ypeak[ii]);
         pgdraw_ (&x[ii], &ypeak[ii]);
      }
      pgslw_ (&Three);
      yavg[0] = log10(yavg[0]);
      pgmove_ (&x[0], &yavg[0]);
      for (ii=1; ii<index; ii++)
      { 
         yavg[ii] = log10(yavg[ii]);
         pgdraw_ (&x[ii], &yavg[ii]);
      }
      pgend_ ();
      fflush (stdout);
   }
   else
   {
      printf ("%s \n", toplbl);
      if (!magnetics)
        printf ("E-Field Spectral Density,(V/m)**2/Hz\n");
      else
        printf ("B-Field Spectral Density,nT**2/Hz\n");
      emitt (start_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
      sprintf (tstring, "Start: %04d-%02d-%02d (%03d) %02d:%02d:%06.3f", year, month, day,
  			doy, hr, mn, sec);
      printf ("%s ", tstring);
      emitt (stop_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
      sprintf (tstring, "  Stop: %04d-%02d-%02d (%03d) %02d:%02d:%06.3f", year, month, day,
  			doy, hr, mn, sec);
      printf ("%s\n", tstring);
      printf ("Frequency,Hz                            Avg                           Peak \n");
/*
                       123456789012345678901
*/
      for (ii=0; ii<index; ii++)
        printf ("%12.3e                     %10.3e                     %10.3e\n",
			x[ii], yavg[ii], ypeak[ii] );
   }

return 0;
}
void	sort_xy (float x[], float y[], int n)
{ float tempx, tempy;
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
L50:
  { tempx = x[i-1];
    tempy = y[i-1];
    x[i-1] = x[l-1];
    y[i-1] = y[l-1];
    x[l-1] = tempx;
    y[l-1] = tempy;
    i = i - m;
    if ( (i-1) >= 0) goto L49;
  }
    j++;
    if (j > k) goto L20;
    goto L41;
}
