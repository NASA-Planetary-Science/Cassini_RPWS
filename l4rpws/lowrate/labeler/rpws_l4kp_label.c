/* --------------------------------------------------------------------------

  read_key.c written by T. Averkamp  2003-06-13
  to read rpws key files and write PDS Archive KEY LBL files.
  Modifications:

	19-Oct-2005	Version Numbers referenced by pointers
			^LRKEY_FREQUENCY_TABLE and
			^LRKEY_SPECTRAL_DENSITY_TABLE fixed ---
			previously they were all version 0, now the
			version number is correct. TFA
	
	2012-11-06 CWP: Willy's rpws_target_name function has been changed, 
	      switched to using Robert's get_target_name instead
	
	2016-03-24 CWP: Updated to build on Linux and be C99 compliant
	                (a sub-set of not BSD/GNU compliant)

  -------------------------------------------------------------------------- */

/* Which POSIX standard version do we match */
#define _POSIX_C_SOURCE 200112L


#define MAXCHARS	200

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <sys/types.h>
#include <stdbool.h>

#include <SpiceUsr.h>

#include <casephem/CasTables.h>
#include <das2/das1.h>

#ifndef INST_ETC
#error Need INST_ETC, compiled in settings directory not defined
#endif

char	*myname;
FILE	*fpin;
FILE	*label_in;
FILE	*label_out;
bool	first=true;
int	file_records=0;
char	start_time[128];
char	stop_time[128];
char	sclk_start[80];
char	sclk_stop[80];

static int     convert_underscore_to_space (char *buf);
static int decompose_string_to_file(FILE *outfile, char *buf, int num_spaces);


int main (int argc, char *argv[])
{
  int		year, month;
  int		mon, mday, doy, hr, mn, sec;
  double	Sec, start_sec, stop_sec;
  char		yyyy[5]= {0};
  char		ddd[4]={0};
  char		inputline[1178];
  char		mpn[1178];
  int		i;
  char		scet [80];
  char		sStart_time[32];
  char		sStop_time[32];
  char		*temp, *target, targets[80];
  int		Cassini = -82;
  double	et, tol;
  char		directory[253] = {'\0'};
  char		file_name[80] = "RPWS_KEY__1999001_0";
  char		*str, *s1, *s2;
  time_t  	time_t_uttime;
  struct  	tm *tm_uttime;

  /* ---------------------- read_key -------------------------------------- */

  myname = argv[0];
  
  
  if ( !(label_in = fopen( INST_ETC "/pds/KEY_MASTER.LBL","r")) )
  {
    fprintf (stderr, "%s:  error opening " INST_ETC "/pds/KEY_MASTER\n",  myname) ;
    return -1;
  }
  if ( argc < 2 )
  {
    fprintf (stderr, "Syntax is %s data_file_name \n",  myname) ;
    return -1;
  }
  else if ( !(fpin = fopen (argv[1],"r")) )
  {
    fprintf (stderr, "%s: error opening %s \n",  myname, argv[1]) ;
    return -1;
  }
  s2 = strstr(argv[1], "RPWS_KEY__");
  strcpy (file_name, s2);
  s1 = strstr (file_name, ".");
  *s1 = '\0';
  strcpy (directory, argv[1]);
  s2 = strstr(directory, "RPWS_KEY__");
  *s2 = '\0';
/*
  fprintf (stderr, "File Name is %s \n", file_name);
  fprintf (stderr, "Directory is %s \n", directory);
*/
  while (fread (inputline, 1, 1175, fpin) != 0)
  {
     
/*	For PDS need yyyy-doyThh:mm:30.000 for time	*/

      if (file_records == 0)
      { for (i=1; i<23; i++)
        { start_time[i-1] = inputline[i-1];
          if ( (i>0) && (i<5) ) yyyy[i-1]=inputline[i-1];
	  if ( (i>5) && (i<9) ) ddd[i-6]=inputline[i-1];
	}
	start_time[22]='\0';
      }
      if (file_records >= 1)
      { for (i=1; i<23; i++)
          stop_time[i-1] = inputline[i-1];
	stop_time[22]='\0';
      }
       sprintf (sStart_time, "%s", start_time); 

/*	Keep track of how many records we wrote */

      file_records++;

  } /* while input is available */

  if (file_records == 0) return 0;

  /* Load the cassini clock kernels */
  if( getenv("CAS_TIME_KERNELS") == NULL){
    fprintf(stderr, "Environmet Variable CAS_TIME_KERNELS is not defined\n");
	 return 13;
  }
  
  furnsh_c(getenv("CAS_TIME_KERNELS"));

  sctiks_c(Cassini, "1000:000", &tol);

/*	Format is "yyyy-ddd // hh:mm:00.000" */
   
   for (i=0; i<8; i++)
     scet[i] = start_time[i];
   scet[8] = 0x20;
   scet[9] = 0x2F;
   scet[10] = 0x2F;
   scet[11] = 0x20;
   for (i=12; i<24; i++)
     scet[i] = start_time[i-3];
   scet[24] = '\0';

   utc2et_c(scet, &et);
   sce2s_c(Cassini, et, 80, sclk_start);

/* calculate next day's time, using das utilities */

   if (parsetime (start_time, &year, &month, &mday, &doy, &hr, &mn, &Sec))
   { fprintf (stderr, "%s: error parsing %s\n", myname, start_time);
     exit(1);
   }
   start_sec = ttime ( &year, &month, &mday, &doy, &hr, &mn, &Sec );
   stop_sec = start_sec + 86400.0;
   emitt ( stop_sec, &year, &month, &mday, &doy, &hr, &mn, &Sec );
   sprintf (stop_time, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f",
            year, doy, hr, mn, Sec );

   for (i=0; i<8; i++)
     scet[i] = stop_time[i];
/*
   for (i=9; i<21; i++)
     stop_time[i] = end_of_day[i-9];
*/
   scet[8] = 0x20;
   scet[9] = 0x2F;
   scet[10] = 0x2F;
   scet[11] = 0x20;
   for (i=12; i<24; i++)
/*
     scet[i] = end_of_day[i-12];
*/
     scet[i] = stop_time[i-3];
   scet[24] = '\0';
   utc2et_c(scet, &et);
   sce2s_c(Cassini, et, 80, sclk_stop);

   /*str = strdup (sclk_start); Not available in C99 */
	
	str = (char*) calloc(strlen(sclk_start)+1, sizeof(char));
	strcpy(str, sclk_start);
	
   s1 = strstr (str, ".");
   sclk_start[s1-str] = 0x3A;			/* replace with : */
   sclk_start[s1-str+4]='\0';
   /* str = strdup (sclk_stop); Not in C99 */
	str = (char*) calloc(strlen(sclk_stop)+1, sizeof(char));
	strcpy(str, sclk_stop);
	
   s1 = strstr (str, ".");
   sclk_stop[s1-str] = 0x3A;			/* replace with : */
   sclk_stop[s1-str+4]='\0';

   sprintf (sStart_time, "%s", start_time); 
   sprintf (sStop_time, "%s", stop_time); 
   sStart_time[21] = '\0';
   sStop_time[21] = '\0';
/*
	Get Target list
*/
   target = get_target_name(sStart_time, sStop_time);
   convert_underscore_to_space (target);
   strcpy (targets, target);
   for (i=0; i<strlen(targets); i++)
   {  if (targets[i] == 0x22)
       targets[i] = 0x20;		/* change quotes to blanks */
   }
   
   while ( (str=fgets(inputline, MAXCHARS, label_in)) != NULL )
   {
     if ( first &&  strstr(str,"DESCRIPTION") )
     { first = false;
       s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s\"%s contains Cassini Radio\r\n", inputline, file_name);
       printf ("                                 ");
       printf ("and Plasma (RPWS) key parameter data for the\r\n");
       printf ("                                 ");
       printf ("time period between %s and\r\n", sStart_time);
       printf ("                                 ");
       printf ("%s and includes the\r\n", sStop_time);
       printf ("                                 ");
       printf ("following targets:\r\n");
       printf ("                                ");
       printf ("%s.\"\r\n", targets);
     }
     else if (strstr(str,"FILE_RECORDS"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s%d\r\n", inputline, file_records); 
     }
     else if (strstr(str,"^LRKEY_FREQUENCY_TABLE"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
/*
       printf ("%s(\"RPWS_KEY__%s%s_0.TAB\",1)\r\n", inputline, yyyy,ddd); 
*/
       printf ("%s(\"%s.TAB\",1)\r\n", inputline, file_name); 
     }
     else if (strstr(str,"^LRKEY_SPECTRAL_DENSITY_TABLE"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
/*
       printf ("%s(\"RPWS_KEY__%s%s_0.TAB\",2)\r\n", inputline, yyyy,ddd); 
*/
       printf ("%s(\"%s.TAB\",2)\r\n", inputline, file_name); 
     }
     else if ( strstr(str,"PRODUCT_ID") &&
              !strstr(str,"STANDARD_DATA_PRODUCT_ID" ) )
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s\"%s_V1\"\r\n", inputline, file_name);
     }
     else if (strstr(str,"MISSION_PHASE_NAME"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s{", inputline); 
       temp = get_mission_phase_name(sStart_time, sStop_time);
       strcpy (mpn, temp);
       convert_underscore_to_space (mpn);
       decompose_string_to_file(stdout, mpn, (s1-str+3));
     }
     else if (strstr(str,"TARGET_NAME"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s%s\r\n", inputline, target); 
     }
     else if (strstr(str,"START_TIME"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s%s\r\n", inputline, start_time); 
     }
     else if (strstr(str,"STOP_TIME"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s%s\r\n", inputline, stop_time); 
     }
     else if (strstr(str,"SPACECRAFT_CLOCK_START_COUNT"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s\"%s\"\r\n", inputline, sclk_start); 
     }
     else if (strstr(str,"SPACECRAFT_CLOCK_STOP_COUNT"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s\"%s\"\r\n", inputline, sclk_stop); 
     }
     else if (strstr(str,"PRODUCT_CREATION_TIME"))
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       time_t_uttime = time(NULL);
       tm_uttime = gmtime(&time_t_uttime);
       tm_uttime->tm_yday++;         /* gmtime returns days after Jan. 1 */
       tm_uttime->tm_mon++;                 /* months since Jan */
       year = tm_uttime->tm_year + 1900;
       doy  = tm_uttime->tm_yday;
       mon  = tm_uttime->tm_mon;            /* month, 1...12 */
       mday = tm_uttime->tm_mday;           /* day of month */
       hr   = tm_uttime->tm_hour;
       mn   = tm_uttime->tm_min;
       sec  = tm_uttime->tm_sec;
       printf ("%s%4.4d-%2.2d-%2.2d\r\n", inputline, year, mon, mday); 
     }
     else if ( strstr(str,"ROWS") && !strstr(str,"= 1") )
     { s1 = strstr(str,"= ");
       inputline[s1-str+2] = '\0';
       printf ("%s%d\r\n", inputline, file_records-1); 
     }
     else
     { inputline[strlen(inputline)-1] = '\0';
       printf ("%s\r\n", inputline); 
     }

   }
return(0);
}
 static int convert_underscore_to_space(char *buf)
   {
     int i;
     for(i=0; i<strlen(buf); i++)
       {
         if(buf[i] == '_')
             buf[i] = ' ';
        }
     return 0;     
    }
 static int decompose_string_to_file(FILE *outfile, char *buf, int num_spaces)
   {
     char *token[256];
     int i = 0, j;
     
     if(buf)
       {
         if(buf[0])
           {
             token[0] = strtok(buf, ",");
             token[1] = NULL;
             while(token[i])
               {
                 token[i+1] = strtok(NULL, ",");
                 token[i+2] = NULL;
                 i++;
                 if(i>254)
                     break;
                }
            }
        }

     i=0;
     while(token[i])
       {
	 if (i==0)
           fprintf(outfile,"%s", token[i]);
	 else
	 {
	   fprintf(outfile,",\r\n");
	   for (j=0; j<num_spaces; j++)
	     fprintf(outfile, " ");
           fprintf(outfile,"%s", token[i]);
	 }
         i++;
        }
    fprintf(outfile,"}\r\n");
    return 0;
    }
