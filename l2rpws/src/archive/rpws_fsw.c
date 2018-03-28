#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <SpiceUsr.h>

/* Cas stuff */
#include <mdb_time.h>
#include <archive.h>

/* Local Stuff */
#include "rpws_label.h"
#include "rpws_fsw_ver.h"

/****************************************************************************/


static int wbr_flag = RPWS_LABEL_WFR;
static int Delta_T = 3599;
static char *Version = { "V2.2" };

extern char *optarg;
extern int optind, opterr, optopt;

static int main1 (FILE * outfile, char *tag, char *names)
{
  char space[] = { "                               " };
  char *token[256];
  int icnt = 0;
  int i;

  for (i = 0; i < 256; i++)
    token[i] = NULL;
  if (names) {
    if (names[0]) {
      token[0] = strtok (names, ",");
      token[1] = NULL;
      while (token[icnt]) {
        token[icnt + 1] = strtok (NULL, ",");
        token[icnt + 2] = NULL;
        icnt++;
        if (icnt > 254)
          break;
      }
      icnt--;
    }
  } else
    icnt = -1;

  switch (icnt) {
   default:
     fprintf (outfile, "%s = {%s,\n", tag, token[0]);
     space[strlen (tag) + 4] = 0;
     for (i = 1; i < icnt; i++)
       fprintf (outfile, "%s%s,\n", space, token[i]);
     fprintf (outfile, "%s%s}\n", space, token[icnt]);
     break;
   case 0:
     fprintf (outfile, "%s = {", tag);
     if (token[0])
       fprintf (outfile, "%s", token[0]);
     fprintf (outfile, "}\n");
     break;
   case -1:
     fprintf (outfile, "%s NOT FOUND \n", tag);
     break;
  }
  return 1;
}
static int decompose_string_to_file (FILE * outfile, char *buf, char *mne)
{
  char *token[256];
  int i = 0;

  if (buf) {
    if (buf[0]) {
      token[0] = strtok (buf, ",");
      token[1] = NULL;
      while (token[i]) {
        token[i + 1] = strtok (NULL, ",");
        token[i + 2] = NULL;
        i++;
        if (i > 254)
          break;
      }
    }
  }

  fprintf (outfile, "%d  %s\n", i, mne);
  i = 0;
  while (token[i]) {
    fprintf (outfile, "%s\n", token[i]);
    i++;
  }

}
static int convert_underscore_to_space (char *buf)
{
  int i;

  for (i = 0; i < strlen (buf); i++) {
    if (buf[i] == '_')
      buf[i] = ' ';
  }
  return 0;
}
int main (int argc, char *argv[])
{
  FILE *outfile = stdout;
  char c;
  int iStart_time;
  int iStop_time;
  int i;
  int puke_flag = 1;
  char sStart_time[32];
  char sStop_time[32];
  char *temp;
  char *empty = { "" };
  char* sKernelFile = NULL;

  if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }
  sKernelFile = getenv("CAS_TIME_KERNELS");
  
  furnsh_c(sKernelFile);

     /**********************************************/
  /*
   * 1st. time argument is, of course, required 
   */
  /*
   * start by assuming no stop time...          
   */

     /**********************************************/
  iStart_time = MDB_time_verify (argv[argc - 1]);
  iStop_time = iStart_time + Delta_T;

     /**********************************************/
  /*
   * Now, see if there are 2 time arguments     
   */
  /*
   * and if there are, move time arund...       
   */

     /**********************************************/
  i = MDB_time_verify (argv[argc - 2]);
  if (i) {
    iStop_time = iStart_time;
    iStart_time = i;
  }


  sprintf (sStart_time, "%08X", iStart_time);
  sprintf (sStop_time, "%08X", iStop_time);
  iStart_time = MDB_time (sStart_time);
  iStop_time = MDB_time (sStop_time);

  /*
   *         c       Coordinate System Name          time
   *         t       Target Name                     start stop
   *         p       Mission Phase Name              start stop
   *         C       Coordinate System Name          start stop
   *         T       Target Name (lines)             start stop
   *         P       Mission Phase Name(lines)       start stop
   *         f       Flight Software Version         time
   *         i       IEB load                        time
   *         d       Dump all                        start stop
   *         D       Dump all (unquoted)             start stop
   *         x       dump name table
   *         v       Version Dump
   *         l       load database from file
   *         h       Help
   */
  while ((c = getopt (argc, argv, "C:T:P:F:I:S:O:l:1ctpfisodDvhxX")) != EOF) {
    puke_flag = 0;
    switch (c) {
     case 'h':
       fprintf (outfile, "%s  %s\n", argv[0], Version);
       fprintf (outfile, "            \n");
       fprintf (outfile, "    usage %s [-1] [-l file] -flag start [stop]\n",
                argv[0]);
       fprintf (outfile, "            \n");
       fprintf (outfile, "        [-1] is optional time field selection\n");
       fprintf (outfile, "              -1     use 1-hour boundaries\n");
       fprintf (outfile,
                "                       default is to use 24-hour boundaries\n");
       fprintf (outfile,
                "                       must be 1st. flag to be effective\n");
       fprintf (outfile, "        [-l] is optional setup file selection\n");
       fprintf (outfile, "              -l name  \n");
       fprintf (outfile,
                "                       loads the databse from the selected\n");
       fprintf (outfile,
                "                       file (-time argument to rpws_archive)\n");
       fprintf (outfile, "        -flag is any of these:\n");
       fprintf (outfile, "          (results to <stdout>)\n");
       fprintf (outfile, "              -c     COORDINATE_NAME string\n");
       fprintf (outfile, "              -t     TARGET_NAME string\n");
       fprintf (outfile, "              -p     MISSION_PHASE_NAME string\n");
       fprintf (outfile, "              -s     Mission Sequence Number\n");
       fprintf (outfile, "              -o     Orbit Number\n");
       fprintf (outfile,
                "              -f     Flight Software Version string\n");
       fprintf (outfile, "              -i     IEB load name string\n");
       fprintf (outfile, "          (results to file <fn>)\n");
       fprintf (outfile, "              -C fn  COORDINATE_NAME list\n");
       fprintf (outfile, "              -T fn  TARGET_NAME list\n");
       fprintf (outfile, "              -P fn  MISSION_PHASE_NAME list\n");
       fprintf (outfile, "              -S fn  Mission Sequence list\n");
       fprintf (outfile, "              -O fn  Orbit Number list\n");
       fprintf (outfile,
                "              -F fn  Flight Software Version string\n");
       fprintf (outfile, "              -I fn  IEB load name string\n");
       fprintf (outfile,
                "               {might find this works a little better\n");
       fprintf (outfile,
                "                if you have a space between the flag\n");
       fprintf (outfile,
                "                and the filename for TARGET_NAME, otherwise\n");
       fprintf (outfile,
                "                it tries to interpret the -T as a time (with\n");
       fprintf (outfile, "                little success)}\n");
       fprintf (outfile, "           debugging\n");
       fprintf (outfile, "              -d     dump all of the above\n");
       fprintf (outfile,
                "              -D     dump all of the above (unquoted)\n");
       fprintf (outfile, "              -x     dump names table\n");
       fprintf (outfile,
                "              -X     dump names table (2 time columns)\n");
       fprintf (outfile, "              -v     dump version information\n");
       fprintf (outfile, "              -h     help (this file)\n");
       fprintf (outfile, "            \n");
       fprintf (outfile, "        start [stop]\n");
       fprintf (outfile, "            start and stop times.  May\n");
       fprintf (outfile, "            be SCLK or SCET.  SCLK may be\n");
       fprintf (outfile, "            hexadecimal (8 digits) or Spice.\n");
       fprintf (outfile,
                "              SCET must be Spice and contain the \"T\"\n");
       fprintf (outfile, "            character.  Default stop time\n");
       fprintf (outfile, "            is %d seconds after start time\n",
                Delta_T);
       fprintf (outfile,
                "              SCET must be expressed in a format that\n");
       fprintf (outfile,
                "            SPICE will handle.  It appears that SPICE\n");
       fprintf (outfile,
                "            is flexible about times, i.e. day-of-year\n");
       fprintf (outfile,
                "            may be grater than 366, hours may be larger\n");
       fprintf (outfile,
                "            than 23, minutes and seconds may be larger\n");
       fprintf (outfile,
                "            than 59 (it appears that days must be 3\n");
       fprintf (outfile,
                "            digits amd hr/min/sec must be 23 digits)\n");
       fprintf (outfile, "            \n");
       fprintf (outfile, "        V2.0\n");
       fprintf (outfile,
                "            -l names.tab now required to load (no auto-load)\n");
       fprintf (outfile, "            New names.tab format support\n");
       fprintf (outfile, "            allows 1-hour or 24-hour boundary\n");
       fprintf (outfile, "            \n");
       exit (0);
     case '1':
       wbr_flag = RPWS_LABEL_WBR;
       break;
     case 'l':
       rpws_ver_load (optarg);
       break;
     case 'v':
       fprintf (outfile, "%s\n", argv[0]);
       fprintf (outfile, "    \"%s %s\" = Version\n", __FILE__, Version);
       fprintf (outfile, "    \"%s\" = MDB_time_ver(void);\n",
                MDB_time_ver ());
       fprintf (outfile, "    \"%s\" = rpws_ver_ver(void);\n",
                rpws_ver_ver ());
       temp = empty;
       exit (0);

       /*
        *      First Example: Obtaining TARGET_NAME string
        *
        *
        *
        */
     case 't':
       temp = rpws_target_name (sStart_time, sStop_time, 1, wbr_flag);
       convert_underscore_to_space (temp);
       break;
     case 'T':
       outfile = fopen (optarg, "w");
       temp = rpws_target_name (sStart_time, sStop_time, 1, wbr_flag);
       convert_underscore_to_space (temp);
       decompose_string_to_file (outfile, temp, "TGT");
       temp = empty;
       break;

     case 'o':
       temp = rpws_orbit_number (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       break;
     case 'O':
       outfile = fopen (optarg, "w");
       temp = rpws_orbit_number (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       decompose_string_to_file (outfile, temp, "COR");
       temp = empty;
       break;

     case 'c':
       temp = rpws_coordinate_name (sStart_time, sStop_time, 1, wbr_flag);
       convert_underscore_to_space (temp);
       break;
     case 'C':
       outfile = fopen (optarg, "w");
       temp = rpws_coordinate_name (sStart_time, sStop_time, 1, wbr_flag);
       convert_underscore_to_space (temp);
       decompose_string_to_file (outfile, temp, "COR");
       temp = empty;
       break;

     case 'p':
       temp = rpws_mission_phase_name (sStart_time, sStop_time, 1, wbr_flag);
       convert_underscore_to_space (temp);
       break;
     case 'P':
       outfile = fopen (optarg, "w");
       temp = rpws_mission_phase_name (sStart_time, sStop_time, 1, wbr_flag);
       convert_underscore_to_space (temp);
       decompose_string_to_file (outfile, temp, "MPN");
       temp = empty;
       break;

     case 's':
       temp = rpws_sequence_name (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       break;
     case 'S':
       outfile = fopen (optarg, "w");
       temp = rpws_sequence_name (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       decompose_string_to_file (outfile, temp, "SEQ");
       temp = empty;
       break;

     case 'f':
       temp = rpws_software_version_name (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       break;
     case 'F':
       outfile = fopen (optarg, "w");
       temp = rpws_software_version_name (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       decompose_string_to_file (outfile, temp, "FSW");
       temp = empty;
       break;

     case 'i':
       temp = rpws_ieb_load_name (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       break;
     case 'I':
       outfile = fopen (optarg, "w");
       temp = rpws_ieb_load_name (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       decompose_string_to_file (outfile, temp, "IEB");
       temp = empty;
       break;

     case 'd':
       temp = rpws_software_version_name (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       main1 (outfile, "FLIGHT_SOFTWARE_VERSION", temp);

       temp = rpws_ieb_load_name (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       main1 (outfile, "IEB_LOAD", temp);

       temp = rpws_sequence_name (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       main1 (outfile, "MISSION_SEQUENCE", temp);

       temp = rpws_mission_phase_name (sStart_time, sStop_time, 1, wbr_flag);
       convert_underscore_to_space (temp);
       main1 (outfile, "MISSION_PHASE_NAME", temp);

       temp = rpws_target_name (sStart_time, sStop_time, 1, wbr_flag);
       convert_underscore_to_space (temp);
       main1 (outfile, "TARGET_NAME", temp);

       temp = rpws_coordinate_name (sStart_time, sStop_time, 1, wbr_flag);
       convert_underscore_to_space (temp);
       main1 (outfile, "COORDINATE_NAME", temp);

       temp = rpws_orbit_number (sStart_time, sStop_time, 1);
       convert_underscore_to_space (temp);
       main1 (outfile, "ORBIT_NAME", temp);

       temp = empty;
       break;

     case 'D':

       temp = rpws_software_version_name (sStart_time, sStop_time, 0);
       fprintf (outfile, "FLIGHT_SOFTWARE_VERSION(%s)\n", temp);

       temp = rpws_ieb_load_name (sStart_time, sStop_time, 0);
       fprintf (outfile, "IEB_LOAD(%s)\n", temp);

       temp = rpws_sequence_name (sStart_time, sStop_time, 0);
       fprintf (outfile, "MISSION_SEQUENCE(%s)\n", temp);

       temp = rpws_mission_phase_name (sStart_time, sStop_time, 0, wbr_flag);
       fprintf (outfile, "MISSION_PHASE_NAME(%s)\n", temp);

       temp = rpws_target_name (sStart_time, sStop_time, 0, wbr_flag);
       fprintf (outfile, "TARGET_NAME(%s)\n", temp);

       temp = rpws_coordinate_name (sStart_time, sStop_time, 0, wbr_flag);
       fprintf (outfile, "COORDINATE_NAME(%s)\n", temp);

       temp = rpws_orbit_number (sStart_time, sStop_time, 0);
       fprintf (outfile, "ORBIT_NAME(%s)\n", temp);

       temp = empty;
       break;
     case 'x':
       rpws_ver_dump (stdout, RPWS_LABEL_WFR);
       temp = empty;
       break;
     case 'X':
       rpws_ver_dump (stdout, RPWS_LABEL_WBR);
       temp = empty;
       break;
    }
  }
  if (temp)
    fprintf (outfile, "%s\n", temp);
}
