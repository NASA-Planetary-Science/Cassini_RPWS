#include <stdio.h> 
/* #include <stdlib.h> */
/* #include <stdarg.h> */
#include <string.h>
/* #include <rtiu.h> */
/* #include <util.h> */
/* #include <utilt.h> */
/* #include <time.h> */
/* #include <curses.h> */
/* #include <math.h> */
#include <das2/das1.h>
#include <dirent.h>

#include "dbase.h"

static  FILE    *fperror;

void make_dbase (double start_sec, double stop_sec, struct file_structure *dbase, int *num_files)
{
   int          index, year, month, day, doy, hr, mn, nQtr;
   double       sec, total_secs, file_start, file_stop;

   int          file_okay;
   FILE         *finput;

   double       Start_sec, Stop_sec;
   char         in_line [1178], sPath[2048], fname[2048], file_name[2048];
   struct       dirent *dirp;
   DIR          *dp;

   emitt (start_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
   hr = 0; mn = 0; sec = 0.0;
   Start_sec = ttime (&year, &month, &day, &doy, &hr, &mn, &sec);
   emitt (stop_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
   if ( (hr == 0) && (mn == 0) && (sec == 0.0) )
     emitt (stop_sec-1.0, &year, &month, &day, &doy, &hr, &mn, &sec);
   hr = 0; mn = 0; sec = 0.0;
   Stop_sec = ttime (&year, &month, &day, &doy, &hr, &mn, &sec);

   /* fprintf( stderr, "enter dbase\n" ); */
   fperror= stderr;
   
   *num_files = 0;
   while (Start_sec <= Stop_sec)
   {
      emitt (Start_sec, &year, &month, &day, &doy, &hr, &mn, &sec);
      nQtr = doy/100;           /* directory for each 100 days of data */
      sprintf (sPath, "/opt/project/cassini/pds/DATA/RPWS_KEY_PARAMETERS/T%4d%1dXX/", year, nQtr);
      sprintf (fname, "RPWS_KEY__%4d%03d_", year, doy);

    /* fprintf (fperror, "Directory Path %s\n", sPath); */
    /* fprintf (fperror, "file name %s\n", fname); */

    if ((dp=opendir(sPath)) != NULL)  /* directory exists */
      { while ((dirp=readdir(dp)) != NULL)  /* directory is not empty */
        { if ((strcmp (dirp->d_name, ".")==0) || (strcmp(dirp->d_name,"..")==0))
            continue;   /* skip those . and .. entries */
          if ( strstr(dirp->d_name, ".TAB") && strstr(dirp->d_name, fname) )
          {

            file_okay = True;
            
            /* fprintf( stderr, "%s %s \n", sPath, dirp->d_name ); */
            
            sprintf (file_name, "%s%s", sPath, dirp->d_name);
            if (!(finput = fopen (file_name, "r")))
            {
              file_okay = False;
            }
/* File exists, so see if it has any data. */
            else if (fread (in_line, 1, 1175, finput) == 0)
            {
              fclose (finput);
              file_okay = False;
            }

            if (file_okay)      /* file exists, and is not empty */
            {  fclose (finput);
               index = *num_files;
               sprintf ((dbase+index)->line, "%s%s", sPath, dirp->d_name);
               /* fprintf (stderr, "%s\n", (dbase+index)->line); */
            
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
        closedir (dp);          /* done with directory, close it */
      } /* fi */
     Start_sec += 86400.0;
   }

}
