/*
        web_avpow_pds.c         written 2/12/07 by TFA to process
                        	Cassini RPWS PDS Key Parameter data files, integrating
				power over a selected frequency range, and doing
				a sliding time average over a selected interval
			
        Modifications:
*/
#include <stdio.h> 
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <rtiu.h>
#include <util.h>
#include <utilt.h>
#include <time.h>
#include <curses.h>
#include <math.h>
#include <das.h>
#include <dirent.h>

#define  MAXFILES 2048
#define  MAXCHARS 200
#define  MAXENTRIES     100
#define  True   1
#define  False  0

#define NJ 73
#define NJMAG 42
#define FILL 1.0e-32

#define RVenus    6051.8
#define REarth    6378.14
#define RJupiter 71492.0
#define RSaturn  60268.0
#define RAU  149600000.0

#define PI	3.141592654
#define DPR	57.295779506

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
static	FILE	*fdebug;
static	char	MIME_Header[40];
static	char	gif_file[8];

char	*progname;
char	start_string[80], stop_string[80];
double	avg_deltat, slide_deltat;
double	flow, fhigh;
double  bandwidth[NJ];
double  ant_corr[NJ];
float	*av_array;
int	nitems, nslide, ilo, ihi;
int	r_adjust=False;
double	rscale, rnormalize;
int	remove_spikes=False;
double	spike_level;
int	antenna_adj=False;
int	plot_option;
double	FMIN=1.0;
double	FMAX=16.1e6;
int	E_or_B=0;

void make_dbase (double start, double stop, struct file_structure *dbase, int *num_files);

int process_data (double start, double stop, struct file_structure *dbase, int n_files);

void    unescape_url (char *url);
void    plustospace (char *str);
char    *makeword (char *line, char stop);
char    *fmakeword (FILE *f, char stop, int *len);

void ktotal_ (char *name, int *count, int namelen);
void furnsh_ (char *filename, int namelen);
void spkez_ (int *target, double *et, char *frame, char *aberr, int *obs, double state [], double *lt, int framelen, int aberrlen);
void utc2et_ (char *fstring, double *et, int len);
void radjust (int yr, int dy, int hr, int mn, int ss, int planet, float *factor);
void vminus_ (double vin [], double vout []);
void mxv_ (double tipm [][], double vin [], double vout []);
void bodmat_ (int *target, double *et, double tipm [][]);
void reclat_ (double vin [], double *rad, double *lon, double *lat);

main(int argc,char **argv)
{
int     i, cl, num_files;
int     year, month, day, doy, hr, mn;
int	Sun = 10, Venus = 299, Earth = 399, Jupiter = 599, Saturn = 699;
double	percent;
char	ch;
double  sec, start_sec, stop_sec;
double	fmin, fmax, fscale, f1, f2, fcenter;
struct  file_structure files [MAXFILES];       
entry	entries [MAXENTRIES];

/***************************************************************/

   progname = argv [0];

   if (!(fperror = fopen ("/var/tmp/tfa-cgi.log", "w")))
   {
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("%s:  error opening /var/tmp/tfa-cgi.log\n", progname);
      exit (-1);
   }
   fprintf (fperror, "Opened tfa-cgi.log file\n");

/*
   if (!(fdebug = fopen ("/home/tfa/cassini/browse/power/test.txt", "r")))
   {
      fprintf (stdout, "Content-type: text/plain\n\n");
      printf ("%s:  error opening test.txt\n", progname);
      exit (-1);
   }
*/

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
      fprintf (fperror, " %d %s %s \n", i, entries [i].name, entries [i].val );
   }
   fflush (fperror);

 if (parsetime (entries[0].val, &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, entries[0].val);
    exit (1);
 }
 sprintf (start_string, "%04d-%02d-%02d (%03d) %02d:%02d:%02d\n",
       		 year, month, day, doy, hr, mn, (int)sec);
 start_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 if (parsetime (entries[1].val, &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, entries[1].val);
    exit (1);
 }
 sprintf (stop_string, "%04d-%02d-%02d (%03d) %02d:%02d:%02d\n",
       		 year, month, day, doy, hr, mn, (int)sec);
 stop_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 sscanf (entries[2].val, "%lf", &avg_deltat);	/* interval, minutes */
 sscanf (entries[3].val, "%lf", &slide_deltat);	/* interval, minutes */
 sscanf (entries[4].val, "%lf", &flow);		/* lower freq, Hz  */ 
 sscanf (entries[5].val, "%lf", &fhigh);	/* upper freq, Hz  */ 

  if ( !strcmp (entries[6].name, "r_adjust") )	/* adjust for R ? */
  {
    fprintf (fperror, "entries[6].name is equal to %s\n", entries[6].name);
    fprintf (fperror, "entries[6].val is equal to %s\n", entries[6].val);
    if ( !strcmp(entries[6].val, "NO") )
    {
      r_adjust = False;
      rnormalize = 0.0;
    }
    else if ( !strcmp(entries[6].val, "RE") )
    {
      r_adjust = Earth;
      rnormalize = REarth;
    }
    else if ( !strcmp(entries[6].val, "RJ") )
    {
      r_adjust = Jupiter;
      rnormalize = RJupiter;
    }
    else if ( !strcmp(entries[6].val, "RS") )
    {
      r_adjust = Saturn;
      rnormalize = RSaturn;
    }
    fprintf (fperror, "rad= %d rnorm= %lf\n", r_adjust, rnormalize);
  }
  if ( !strcmp (entries[7].name, "radii") )	/* adjust for R ? */
  {
    fprintf (fperror, "entries[7].name is equal to %s\n", entries[7].name);
    sscanf (entries[7].val, "%lf", &rscale);	/* normalization radii */ 
    fprintf (fperror, "entries[7].val is equal to %s\n", entries[7].val);
  }

  if ( !strcmp (entries[8].val, "TEXT") )	/* Text file ? */
  {
    fprintf (fperror, "entries[8].val is equal to TEXT\n");
    sprintf (MIME_Header, "Content-type: text/plain");
    plot_option = False;
    sprintf (gif_file, "TEXT");
    if ( !strcmp (entries[9].name, "text_mime_header") )
      sprintf (MIME_Header, "Content-type: %s", entries[9].val);
  }

  if (strcmp (entries[8].name, "device_name") )
  {
    fprintf (fperror, "entries[8].name is not equal to device_name\n");
    sprintf (MIME_Header, "Content-type: image/gif");
    plot_option = True;
    sprintf (gif_file, "-/gif");
  }
  else   if ( !strcmp (entries[8].val, "Z") )   /* GIF file ? */
  {
    fprintf (fperror, "entries[8].val is equal to Z\n");
    sprintf (MIME_Header, "Content-type: image/gif");
    plot_option = True;
    sprintf (gif_file, "-/gif");
    if ( !strcmp (entries[9].name, "gif_mime_header") )
    sprintf (MIME_Header, "Content-type: %s", entries[9].val);
  }

  if (strcmp (entries[9].name, "device_name") )
  {
    fprintf (fperror, "entries[9].name is not equal to device_name\n");
  }
  else   if ( !strcmp (entries[9].val, "PS") )  /* PS file ? */
  {
    fprintf (fperror, "entries[9].val is equal to PS\n");
    sprintf (MIME_Header, "Content-type: application/postscript");
    plot_option = True;
    sprintf (gif_file, "-/ps");
    if ( !strcmp (entries[10].name, "ps_mime_header") )
    sprintf (MIME_Header, "Content-type: %s", entries[10].val);
  }

  if (strcmp (entries[10].name, "device_name") )
  {
    fprintf (fperror, "entries[10].name is not equal to device_name\n");
  }
  else   if ( !strcmp (entries[10].val, "TEXT") )        /* Text file ? */
  {
    fprintf (fperror, "entries[10].val is equal to TEXT\n");
    sprintf (MIME_Header, "Content-type: text/plain");
    plot_option = False;
    sprintf (gif_file, "TEXT");
    if ( !strcmp (entries[11].name, "text_mime_header") )
    sprintf (MIME_Header, "Content-type: %s", entries[11].val);
  }

  fprintf (fperror, "MIME Header is %s\n", MIME_Header);

  if ( !strcmp (entries[12].name, "E_or_B") )	/* data set selection */
  {
    fprintf (fperror, "entries[12].name is equal to %s\n", entries[12].name);
    fprintf (fperror, "entries[12].val is equal to %s\n", entries[12].val);
    if ( !strcmp(entries[12].val, "Electric") )
    {
      E_or_B = 0;
    }
    else if ( !strcmp(entries[12].val, "Magnetic") )
    {
      E_or_B = 1;
    }
  }  

  fprintf (fperror, "E_or_B selection is %d\n", E_or_B);

  if ( !strcmp (entries[13].name, "remove") )	/* remove spikes? */
  {
    remove_spikes = True;
    sscanf (entries[14].val, "%lf", &spike_level); /* spike level */ 
    fprintf (fperror, "Will remove spikes larger than %e\n", spike_level);
  }
    fflush (fperror);
    
/* log frequency scaling (fmin to fmax, inclusive) */
 fmin = -0.05;
 fmax = 7.25;
 FMIN = pow(10.,fmin);
 FMAX = pow(10.,fmax);
 fscale = (double)NJ / (fmax - fmin);

 ilo =  (log10(flow) - fmin) * fscale;
 ihi =  (log10(fhigh) - fmin) * fscale;

 fprintf (fperror, "%f %f %d %d\n", flow, fhigh, ilo, ihi);

 if ((ilo < 0) || (flow < FMIN) )
 {
    fprintf (stdout, "%s\n\n", "Content-type: text/plain");
    fprintf (stdout, "%s: Low Frequency %s, must be 1 Hz or greater\n",
     	     progname, entries[4].val);
    fflush (stdout);
    exit (1);
 }
 if ( (E_or_B == 0) && (ilo >= NJ ) )
 {
    fprintf (stdout, "%s\n\n", "Content-type: text/plain");
    fprintf (stdout, "%s: Low Frequency %s, must be less than 16100000 Hz\n",
     	     progname, entries[4].val);
    fflush (stdout);
    exit (1);
 }
 else if ( (E_or_B == 1) && (ilo >= NJMAG ) )
 {
    fprintf (stdout, "%s\n\n", "Content-type: text/plain");
    fprintf (stdout, "%s: Low Frequency %s, must be less than 14200 Hz\n",
     	     progname, entries[4].val);
    fflush (stdout);
    exit (1);
 }
 if ((ihi < 0 ) || (fhigh < FMIN))
 {
    fprintf (stdout, "%s\n\n", "Content-type: text/plain");
    fprintf (stdout, "%s: High Frequency %s, must be 1 Hz or greater\n",
     	     progname, entries[5].val);
    fflush (stdout);
    exit (1);
 }
 if ( (E_or_B == 0) && (ihi >= NJ ) )
 {
    fprintf (stdout, "%s\n\n", "Content-type: text/plain");
    fprintf (stdout, "%s: High Frequency %s, must be less than 16100000 Hz\n",
     	     progname, entries[5].val);
    fflush (stdout);
    exit (1);
 }
 else if ( (E_or_B == 1) && (ihi >= NJMAG ) )
 {
    fprintf (stdout, "%s\n\n", "Content-type: text/plain");
    fprintf (stdout, "%s: High Frequency %s, must be less than 14200 Hz\n",
     	     progname, entries[5].val);
    fflush (stdout);
    exit (1);
 }
 if (flow >= fhigh )
 {
    fprintf (stdout, "%s\n\n", "Content-type: text/plain");
    fprintf (stdout, "%s: High Frequency %s <= Low Frequency %s\n",
     	     progname, entries[5].val, entries[4].val);
    fflush (stdout);
    exit (1);
 }

 f1 = FMIN;
 for (i=0; i<NJ; i++)
 { f2 = pow (10.0, (double)(i+1)/fscale);
   bandwidth[i] = f2 - f1;
   fprintf (fperror, "Bandwidth[%d] = %e %e %e\n", i, bandwidth[i], f1, f2);
   percent = bandwidth[i] / sqrt(f1*f2);
   ant_corr[i] = 1.0;
   f1 = f2;
 }

 nitems = avg_deltat;				/* save for sliding avg. */
 nslide = slide_deltat;
 if (nslide > nitems)
 { nslide = nitems;		/* can't slide more than avg */
   slide_deltat = nslide;
 }
 if (nslide <= 0)
 { nslide = 1;			/* must slide by at least 1 minute */
   slide_deltat = nslide;
 }
 av_array = malloc (4*nitems);
 for (i=0; i<nitems; i++)
   av_array[i] = 0.0;				/* initalize array */

 avg_deltat = 60.0*avg_deltat;			/* convert to seconds */
 slide_deltat = 60.0*slide_deltat;		/* convert to seconds */
 start_sec = start_sec - avg_deltat/2.0;
 stop_sec = stop_sec + avg_deltat/2.0;

 make_dbase (start_sec, stop_sec, files, &num_files);
 if (num_files == 0)
 {
   fprintf (stdout, "%s\n\n", "Content-type: text/plain");
   fprintf (stdout, "No data coverage of the requested time interval.\n");
   fflush (stdout);
   exit (1);
 }

 if (process_data (start_sec, stop_sec, files, num_files) )
 {
   fprintf (stdout, "%s\n\n", "Content-type: text/plain");
   fprintf (stdout, "No data coverage of the requested time interval.\n");
   fflush (stdout);
   exit (1);
 }
 return (0);
}

int process_data (double START_SEC, double STOP_SEC, struct file_structure *dbase, int n_files)
{
 int    Zero=0, One=1, Three=3;
 float  parm, parm2, Fzero=0.0, Fone=1.0;
 float  pgxmin, pgxmax, pgymin, pgymax, ymin, ymax, radial_distance;
 char   top[200];
 char   top_label[200];
 float  xbase;
 double pkt_sec=0, start_sec, stop_sec;
 int    year, month, day, doy, hr, mn, mon, mday;
 int	first=TRUE, file_records;
 double sec;
 double Sec;
 double avg_power, decimal_days;
 double n_avg;
 time_t pkt_sclk, pkt_epoc, start_sclk, stop_sclk;
 struct tm *pkt_event;
 struct event_time *evt_tim;
 struct event_clock evt_clk;
 int	st_inx, no_data;
 int	i, index;
 float  x[65536], y[65536];
 char	inputline[1178];
 char	time_string[21];
 float	efreq[73], bfreq[42];
 int	plot_index=0;
 float  adjustment;
 FILE	*finput;
 struct new_array
 {
   short	year;
   short	day;
   float	array[74];
 };
 struct		new_array	data;       

   no_data = True;
   start_sec = START_SEC;
   stop_sec = STOP_SEC;
   ymin = 1.0;
   ymax = 0.0;
   if (stop_sec < dbase->st_sec)
      return (1);

   if (start_sec > (dbase + (n_files - 1))->sp_sec)
      return (1);

   for (index = 0; index < n_files; index++)
      if (start_sec < (dbase + index)->sp_sec)
         break;

   if (index == n_files)
      return (1);

   if (stop_sec < (dbase + index)->st_sec)
      return (1);

   st_inx = index;

   if (!(finput = fopen ((dbase + st_inx)->line, "r")))
   {
      fprintf (fperror, "%s:  error opening %s\n",
               progname, (dbase + st_inx)->line);
      exit (-1);
   }

 file_records = 0;
 pkt_sec = 0;
 avg_power = 0.0;
 n_avg = 0.0;
 do
 {
   if ( fread (inputline, 1, 1175, finput) == 0)
   {
     fclose (finput);           /* done with this file, time for next */
     st_inx++;
     if (st_inx < n_files)
     {
       if (!(finput = fopen ((dbase + st_inx)->line, "r")))
       {
         fprintf (fperror, "%s:  error opening %s\n",
               progname, (dbase + st_inx)->line);
         exit (-1);
       }
       file_records = 0;
       if ( fread (inputline, 1, 1175, finput) == 0 )
       {
         fclose (finput);
         fprintf (fperror, "%s:  error opening %s\n",
               progname, (dbase + st_inx)->line);
         exit (-1);
       }
     }
     else
       break;                 /* ran out of files, exit do loop */
   }
/*
	If first record, then get frequencies.
*/
      if (file_records == 0)
      {
        pkt_sec = 0;
        for (i=0; i<73; i++)
	 sscanf ( &inputline[10*i+23], "%e", &efreq[i] );
	for (i=0; i<42; i++)
	 sscanf ( &inputline[10*i+23+730], "%e", &bfreq[i] );
      }
      else
      {
        memcpy (time_string, inputline, 20);
	time_string[20] = NULL;
        if (parsetime (time_string, &year, &month, &day, &doy, &hr, &mn, &sec))
        {
          fprintf (stderr, "%s: error parsing %s\n", progname, time_string);
          exit (1);
        }
        pkt_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );
        if (E_or_B == 0)
        {
          for (i=0; i<73; i++)
            sscanf ( &inputline[10*i+23], "%e", &data.array[i+1] );
        }
        else if (E_or_B == 1)
        {
          for (i=0; i<42; i++)
            sscanf ( &inputline[10*i+23+730], "%e", &data.array[i+1] );
        }
      }
      file_records++;

there:

   if ( ((pkt_sec >= start_sec) && (pkt_sec < (start_sec+avg_deltat))) )
   {
     avg_power = 0.0;
     for (i=ilo; i<=ihi; i++)
     {  if (data.array[i+1] > FILL)
          avg_power = avg_power + data.array[i+1]*bandwidth[i];	/* integrate over frequency */
     }

     if (E_or_B == 0)
       avg_power = avg_power / 377.0;		/* if electric, convert to Watts/meters^2 */

     if ( (!remove_spikes) || (avg_power < spike_level) )
     {
       i = (pkt_sec-start_sec)/60.0;
       if (i < nitems)
         av_array[i] = avg_power;
       else
         fprintf (fperror, "%s: Exceeded # of values to be averaged\n", progname);
     }

     no_data = False;
   }  /* if ( ((pkt_sec >= start_sec) && (pkt_sec < (start_sec+avg_deltat))) ) */
   else if ( pkt_sec >= (start_sec+avg_deltat) )
   {
     avg_power = 0.0;
     n_avg = 0.0;
     for (i=0; i<nitems; i++)
     { if (av_array[i] > FILL)
       { avg_power = avg_power + av_array[i];
         n_avg++;
       }
     }
     if (n_avg > 0.0)
     { avg_power = avg_power / n_avg;
       emitt ((start_sec+avg_deltat/2.0),
     	    &year, &month, &day, &doy, &hr, &mn, &sec);
       if (r_adjust)
       {
         radjust (year, doy, hr, mn, (int)sec, r_adjust, &adjustment);
	 avg_power *= adjustment;	/* multiply by r^2/radjust^2 */
	 radial_distance = sqrt(adjustment*rscale*rscale);
       }

       decimal_days = (double)doy + 
                      (double)hr/24.0 +
		      (double)mn/1440.0 +
		      sec/86400.0;
       if (first)
       {  
	 first = FALSE;
	 fprintf (stdout, "%s\n\n", MIME_Header);
	 fflush (stdout);
       }
       {
         if (!plot_option)
	 {
           fprintf (stdout, "%4.4d-%3.3dT%2.2d:%2.2d  %15.3e\n", year, doy, hr, mn, avg_power);
         }
	 else
         {
           x[plot_index] = start_sec - START_SEC;;
	   y[plot_index] = log10(avg_power);
	   if (avg_power > ymax) ymax = avg_power;
	   if (avg_power < ymin) ymin = avg_power;
	   if (plot_index < 65535) plot_index++;
         }
       }
     }
     start_sec = start_sec + slide_deltat;
     if (nslide < nitems)
     { for (i=nslide; i<nitems; i++)
         av_array[i-nslide] = av_array[i];
       for (i=(nitems-nslide); i<nitems; i++)
         av_array[i] = 0.0;
     }
     else
     { for (i=0; i<nitems; i++)
         av_array[i] = 0.0;
     }
     goto there;			/* go back and process this record */
   }
 }
 while (pkt_sec < stop_sec);
 fclose (finput);
 if (!no_data && plot_option)
 {
   pgbegin_ (&Zero, gif_file, &One, &One, strlen(gif_file));
   pgscr_ (&Zero, &Fone, &Fone, &Fone);		/* invert Black and White */
   pgscr_ (&One, &Fzero, &Fzero, &Fzero);	/* invert Black and White */
   pgslw_ (&Three);
   pgpage_ ();
   pgxmin=0.09; pgxmax=0.90; pgymin=0.09; pgymax=0.90;
   pgvport_ (&pgxmin, &pgxmax, &pgymin, &pgymax);
   parm = fmod (START_SEC+avg_deltat/2.0, 86400.);
   xbase = parm;
   parm2 = (STOP_SEC - START_SEC - avg_deltat) + parm;
   ymin = (int)(log10 (ymin)) - 1;
   ymax = (int)(log10 (ymax));
   pgwindow_ (&parm, &parm2, &ymin, &ymax);
   pgtbox_ ("bcitsnvyz",&Fzero,&Zero,"bcilmstv",&Fzero,&Zero,9L,8L);

   if (E_or_B == 0)
     sprintf (top, "E-Field Power Flux (W m\\u-2\\d)");
   else if (E_or_B == 1)
     sprintf (top, "B-Field Power (nT\\u2\\d)");

   sprintf (top_label, "Cassini RPWS Integrated Power");
   if ( flow >= 1000.)
     sprintf (top_label, "%s (%d KHz - %d KHz)", 
   	   	top_label, (int)(flow/1000.), (int)(fhigh/1000.) );
   else
     sprintf (top_label, "%s (%d Hz - %d Hz)", 
   	   	top_label, (int)flow, (int)fhigh );
   
   if ( (STOP_SEC - START_SEC - avg_deltat) <= 18000.)
     pglabel_ ("TIME (HH MM)", &top,
             top_label, 12L, strlen(top) , 
	     strlen (top_label) );
   else
     pglabel_ ("TIME (HOUR)", &top,
             top_label, 11L, strlen(top) , 
	     strlen (top_label) );
   x[0] += xbase;     
   pgmove_ ( &x[0], &y[0] );
   for (i=1; i<plot_index; i++)
   { x[i] += xbase;     
     pgdraw_ ( &x[i], &y[i] );
   }

   pgvport_ (&Fzero, &Fone, &Fzero, &Fone);
   pgwindow_ (&Fzero, &Fone, &Fzero, &Fone);
   x[0] = pgxmin;
   y[0] = 0.01;
   pgtext_ (&x, &y, &start_string, 25L);
   x[0] = pgxmax;
   pgptxt_ (&x, &y, &Fzero, &Fone, &stop_string, 25L);
   pgend_ ();
   fflush (stdout); 
 }
 return (no_data);
}


void make_dbase (double start_sec, double stop_sec, struct file_structure *dbase, int *num_files)
{
   int          index, year, month, day, doy, hr, mn, nQtr;
   int          begin_sclk, end_sclk, databits;
   double       sec, total_secs, file_start, file_stop;

   int          file_okay;
   FILE         *finput;

   double	Start_sec, Stop_sec;
   char         inputline [1178], sPath[2048], fname[2048], file_name[2048];
   char         start [80], stop [80];
   struct	dirent *dirp;
   DIR		*dp;

   emitt (start_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
   hr = 0; mn = 0; sec = 0.0;
   Start_sec = ttime (&year, &month, &day, &doy, &hr, &mn, &sec);
   emitt (stop_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
   if ( (hr == 0) && (mn == 0) && (sec == 0.0) )
     emitt (stop_sec-1.0, &year, &month, &day, &doy, &hr, &mn, &sec);
   hr = 0; mn = 0; sec = 0.0;
   Stop_sec = ttime (&year, &month, &day, &doy, &hr, &mn, &sec);

   *num_files = 0;
   while (Start_sec <= Stop_sec)
   {
      emitt (Start_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
      nQtr = doy/100;		/* directory for each 100 days of data */
      sprintf (sPath, "/opt/project/cassini/pds/DATA/RPWS_KEY_PARAMETERS/T%4d%1dXX/", year, nQtr);
      sprintf (fname, "RPWS_KEY__%4d%03d_", year, doy);

    fprintf (fperror, "Directory Path %s\n", sPath);
    fprintf (fperror, "file name %s\n", fname);

      if ((dp=opendir(sPath)) != NULL)	/* directory exists */
      { while ((dirp=readdir(dp)) != NULL)  /* directory is not empty */
        { if ((strcmp (dirp->d_name, ".")==0) || (strcmp(dirp->d_name,"..")==0))
            continue;	/* skip those . and .. entries */
	  if ( strstr(dirp->d_name, ".TAB") && strstr(dirp->d_name, fname) )
          {

            file_okay = True;
	    sprintf (file_name, "%s%s", sPath, dirp->d_name);
            if (!(finput = fopen (file_name, "r")))
            {
              file_okay = False;
            }
/* File exists, so see if it has any data. */
            else if (fread (inputline, 1, 1175, finput) == 0)
            {
              fclose (finput);
              file_okay = False;
            }

            if (file_okay)	/* file exists, and is not empty */
            {  fclose (finput);
               index = *num_files;
	       sprintf ((dbase+index)->line, "%s%s", sPath, dirp->d_name);
	       fprintf (stderr, "%s\n", (dbase+index)->line);
	    
/* get file start time and insert into data base */

               (dbase + index)->year = year;
               (dbase + index)->day = doy;
               (dbase + index)->hr = hr;
               (dbase + index)->mn = mn;
               (dbase + index)->sec = sec;
               (dbase + index)->ms  = 1000.*((double)(hr*3600+mn*60)+(double)sec);
               total_secs = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );
               (dbase + index)->st_sec = total_secs;
               file_start = total_secs;

/* get file stop time and insert into data base */

               emitt (Start_sec+86400.0, &year, &month, &day, &doy, &hr, &mn, &sec);
   
               total_secs = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );
               (dbase + index)->sp_sec = total_secs;
               file_stop = total_secs;
         
               if ( (file_start < stop_sec) && (file_stop >= start_sec))
                 (*num_files)++;

	    } /* fi */
          } /* fi */
        } /* elihw */
	closedir (dp);		/* done with directory, close it */
      } /* fi */
     Start_sec += 86400.0;
   }

}


void radjust (int yr, int dy, int hr, int mn, int ss, int planet, float *factor)
{
   static first=True;
   char scstr [80];
   int Sun = 10, Venus = 299, Earth = 399, Jupiter = 599, Saturn = 699;
   int Cassini = -82;
   int index=0;
   float temp;
   double sc_pos [6];
   double pos [3];
   double temp2 [3], position [3], tipm [3] [3];
   double radius, longitude, latitude;
   double lt, et=0.0, corrected_et;
   char *frame = "J2000";
   char *aberr = "LT+S";

   if (first)
   {
      furnsh_ ("/home/jbg/data1/spice/cassini/ephemeris/CassiniKernels.list",
      strlen ("/home/jbg/data1/spice/cassini/ephemeris/CassiniKernels.list") );
      first = False;
      ktotal_ ("SPK", &index, strlen("SPK"));
      fprintf (fperror, "There are %d kernels loaded\n", index);
      fflush (fperror);
   }

      sprintf (scstr, "%04d %03d // %02d:%02d:%02d.0", yr, dy, hr, mn, ss);

      utc2et_ (scstr, &et, strlen(scstr));

      spkez_ (&planet, &et, frame, aberr, &Cassini, sc_pos, &lt, 
      					strlen("J2000"), strlen("NONE"));

      for (index = 0; index < 3; index++)
         pos [index] = sc_pos [index];

      temp = (pos[0]*pos[0]+pos[1]*pos[1]+pos[2]*pos[2]) /
      		(rscale*rscale*rnormalize*rnormalize);

      corrected_et = et - lt;
      vminus_ (pos, temp2);
      bodmat_ (&planet, &corrected_et, tipm);
      mxv_ (tipm, temp2, position);
      reclat_ (position, &radius, &longitude, &latitude);
      if (longitude < 0.0)
         longitude += 2.0*PI;
      radius = (radius/rnormalize);
      latitude = (latitude*DPR);
      longitude = (longitude*DPR);
      longitude = (360.0 - longitude);

      fprintf (fperror, "%s  %10.3lf  %10.3lf  %10.3lf\n", scstr,
      			radius, longitude, latitude);

      *factor = temp;

}
