/* --------------------------------------------------------------------------

  key_reader.c written by T. Averkamp  2004-01-27
  to read rpws key files & output data to das.
  Modifications:
		9-Apr-04	using PDS directory structure


  -------------------------------------------------------------------------- */

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

#define  MAXFILES 1024
#define  MAXCHARS 200
#define  True   1
#define  False  0
#define	 NJ 73
#define	 FMIN 1.0
#define	 FMAX 16.1e6

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

static char *progname;
static char *options;
static int  sensor_is_electric;
double  ant_corr[NJ];

void make_dbase (double start, double stop, struct file_structure *dbase, int *num_files);

int process_data (double start, double stop, struct file_structure *dbase, int n_files);

main(int argc,char **argv)
{
int     num_files;
int     year, month, day, doy, hr, mn;
double  sec, start_sec, stop_sec;
struct  file_structure files [MAXFILES];       

/***************************************************************/

 if (argc <= 2)
 { printf ("Usage is: %s start stop options\n", argv[0]);
   exit(0);
 }
 progname = argv [0];
 if (parsetime (argv[1], &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stderr, "%s: error parsing %s\n", argv[0], argv[1]);
    exit (1);
 }
 start_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 if (parsetime (argv[2], &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stderr, "%s: error parsing %s\n", argv[0], argv[2]);
    exit (1);
 }
 stop_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 if (argc >= 4)
 {  fprintf (stderr, "%s:  %s\n", argv[0], argv[3]);
    if (strstr (argv[3], "Bx") )
      sensor_is_electric = False;
    else if (strstr (argv[3], "By") )
      sensor_is_electric = False;
    else if (strstr (argv[3], "Bz") )
      sensor_is_electric = False;
    else
      sensor_is_electric = True;
 }
 else
   sensor_is_electric = True;
   
 if (sensor_is_electric)
   fprintf (stderr, "%s:  using Electric data\n", argv[0]);
 else
   fprintf (stderr, "%s:  using Magnetic data\n", argv[0]);

 make_dbase (start_sec, stop_sec, files, &num_files);
 if (num_files == 0)
 {
   fprintf (stderr, "No data coverage of the requested time interval.\n");
   exit (1);
 }
 if (process_data (start_sec, stop_sec, files, num_files) )
 {
   fprintf (stderr, "No data coverage of the requested time interval.\n");
   exit (1);
 }
 return (0);
}

int process_data (double start_sec, double stop_sec, struct file_structure *dbase, int n_files)
{
 int    i, year, month, day, doy, hr, mn, mon, mday;
 char	inputline[1178];
 char	time_string[21];
 int	file_records;
 float  efreq[73], bfreq[42];
 double pkt_sec=0;
 double sec;
 double Sec;
 double	fmin, fmax, fscale, f1, f2, fcenter;
 time_t pkt_sclk, pkt_epoc, start_sclk, stop_sclk;
 struct tm *pkt_event;
 struct event_time *evt_tim;
 struct event_clock evt_clk;
 int	st_inx, no_data;
 int	index;
 char   header[8] = { ":b0:300C" };
 float  x_y_z[3];
 FILE	*finput;
 struct new_array
 {
   short	year;
   short	day;
   float	array[74];
 };
 struct		new_array	data;       

   no_data = True;
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
      fprintf (stderr, "%s:  error opening %s\n",
               progname, (dbase + st_inx)->line);
      exit (-1);
   }

  file_records = 0;

 pkt_sec = 0;
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
         fprintf (stderr, "%s:  error opening %s\n",
               progname, (dbase + st_inx)->line);
         exit (-1);
       }
       file_records = 0;
       if ( fread (inputline, 1, 1175, finput) == 0 )
       {
         fclose (finput);
         fprintf (stderr, "%s:  error opening %s\n",
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
      }
      file_records++;

   if ( ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) )
   {
/*
     emitt (pkt_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
     printf ("%4.4d %3.3d %2.2d:%2.2d:%06.3f\n", year,doy,hr,mn,sec);
*/
     data.array[0] = (float)(pkt_sec - start_sec);	/* seconds offset */

     if (sensor_is_electric)
     {
       for (i=0; i<73; i++)
         sscanf ( &inputline[10*i+23], "%e", &data.array[i+1] );
	 
	 sprintf (header, ":b0:036C");
	 if ( !fwrite (header, 8, 1, stdout) )
         {
	   fprintf (stderr, "%s:  error writing output\n", progname);
	   exit (-1);
	 }
	 x_y_z[0] = data.array[0];
         for (i=0; i<73; i++)
	 { 
	   x_y_z[2] = data.array[i+1];
	   if (data.array[i+1] == 0.0)
	     x_y_z[1] = 0.0;
	   else
	     x_y_z[1] = efreq[i];
	   if ( !fwrite (x_y_z, sizeof(x_y_z), 1, stdout) )
           {
	     fprintf (stderr, "%s:  error writing output\n", progname);
	     exit (-1);
	   }
	 }

     }
     else
     {
       for (i=73; i<115; i++)
         sscanf ( &inputline[10*i+23], "%e", &data.array[i+1-73] );
	 
	 sprintf (header, ":b0:01F8");
	 if ( !fwrite (header, 8, 1, stdout) )
         {
	   fprintf (stderr, "%s:  error writing output\n", progname);
	    exit (-1);
	 }
	 x_y_z[0] = data.array[0];
         for (i=0; i<42; i++)
	 { 
	   x_y_z[2] = data.array[i+1];
	   if (data.array[i+1] == 0.0)
	     x_y_z[1] = 0.0;
	   else
	     x_y_z[1] = bfreq[i];
	   if ( !fwrite (x_y_z, sizeof(x_y_z), 1, stdout) )
           {
	     fprintf (stderr, "%s:  error writing output\n", progname);
	     exit (-1);
	   }
	 }

     }


     no_data = False;
   } /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
 }
 while (pkt_sec < stop_sec);
 fclose (finput);
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
   char         inputline [1178], sPath[1024], fname[1024], file_name[1024];
   char         start [80], stop [80];
   struct	dirent *dirp;
   DIR		*dp;

/* Assume we are positioned in the key parameter top level directory,
   e.g., /opt/project/cassini/data/key_parameter
*/

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
