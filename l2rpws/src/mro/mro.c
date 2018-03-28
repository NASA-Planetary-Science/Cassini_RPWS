
/*
  * rpws_archive.c   
  */
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <math.h>
#include <ulimit.h>
#include <sys/types.h>
#include <sys/stat.h>

static int hsk_packet_size = 192;
static int hsk_packet_cnt = 5;
static int delta_t = 10;
static char *t_flag = NULL;
static char *sasf_flag = NULL;
FILE *mro_file = stdout;

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****							****
   *********************************************************/
int time_command (int start, int stop, int flag)
{
  static int time = 0;
  static int sequence = 1;
  int hr, mn, sc;

  if (sasf_flag) {
    time = delta_t;
    sc = time % 60;
    mn = (time / 60) % 60;
    hr = time / 3600;
    fprintf (mro_file, "command(%d,\r\n", sequence++);
    fprintf (mro_file,
             "        SCHEDULED_TIME,\\%02d:%02d:%02d\\,FROM_PREVIOUS_START,\r\n",
             hr, mn, sc);
    switch (flag) {
     case 1:
       fprintf (mro_file,
                "        73MEM_TWEAK(\"LRP\",\"BYTE\",0x%04X,0x%04X,\"TWEK\")\r\n",
                start, stop);
       break;
     case 2:
       fprintf (mro_file, "        73MRO(\"LRP\",\"TLM\",0x%04X,0x%04X)\r\n",
                start, stop);
       break;
    }
    fprintf (mro_file, "),\r\n");
  } else {
    time += delta_t;
    sc = time % 60;
    mn = (time / 60) % 60;
    hr = time / 3600;
    switch (flag) {
     case 1:
       fprintf (mro_file,
                "%02d:%02d:%02d 73MEM_TWEAK, LRP, BYTE, 0x%02X, 0x%02X, TWEK\n",
                hr, mn, sc, start, stop);
       break;
     case 2:
       fprintf (mro_file, "%02d:%02d:%02d 73MRO, LRP, TLM, 0x%04X, 0x%04X\n",
                hr, mn, sc, start, stop);
       break;
    }
  }
  return 1;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****							****
   *********************************************************/
int main (int argc, char *argv[])
{
  int count = 0;
  int start;
  int stop;
  int i;
  char c;
  char *argument;
  char t_file[128];

  while ((c = getopt (argc, argv, "hs:c:d:t:f:")) != EOF) {
    switch (c) {
     case 's':
       hsk_packet_size = atoi (optarg);
       break;
     case 'c':
       hsk_packet_cnt = atoi (optarg);
       break;
     case 'd':
       delta_t = atoi (optarg);
       break;
     case 't':
       if (sasf_flag)
         fprintf (stderr, "SASF already in use\n");
       else
         t_flag = optarg;
       break;
     case 'f':
       if (t_flag)
         fprintf (stderr, "ppccrtiu already in use\n");
       else
         sasf_flag = optarg;
       break;
     case '?':
       fprintf (stdout, "Invalid Command Line Arguments\n");
     case 'h':
       fprintf (stdout, "\n");
       fprintf (stdout, "  %s \n", argv[0]);
       fprintf (stdout, "    \n");
       fprintf (stdout, "    -s size     (%d)\n", hsk_packet_size);
       fprintf (stdout, "                   Housekeeping packet size\n");
       fprintf (stdout, "                   (used to calcualte start of\n");
       fprintf (stdout, "                    next MRO command).\n");
       fprintf (stdout, "    \n");
       fprintf (stdout, "    -c count    (%d)\n", hsk_packet_cnt);
       fprintf (stdout, "                   Packet count, number\n");
       fprintf (stdout, "                   of MRO to generate with\n");
       fprintf (stdout,
                "                   each command (used to calculate\n");
       fprintf (stdout, "                   end address in MRO comamnd).\n");
       fprintf (stdout, "    \n");
       fprintf (stdout, "    -d delta    (%d)\n", delta_t);
       fprintf (stdout, "                   Time between each MRO packet.\n");
       fprintf (stdout,
                "                   Allow enough time for real-time\n");
       fprintf (stdout, "                   commanding, if ground based.\n");
       fprintf (stdout, "    \n");
       fprintf (stdout, "    -t file        PPCRTIU Timed sequence\n");
       fprintf (stdout, "    -f file        SASF Timed sequence\n");
       fprintf (stdout, "    \n");
       fprintf (stdout, "    \n");
       fflush (stdout);
       exit (0);
    }
  }
  if (sasf_flag) {
    sprintf (t_file, "%s.sasf", sasf_flag);
    mro_file = fopen (t_file, "w");
  }
  if (t_flag) {
    sprintf (t_file, "%s.cmd", t_flag);
    mro_file = fopen (t_file, "w");
    fprintf (mro_file, "-cdsseq_file %s\n", t_flag);
  }
  time_command (0x64, 0x01, 1);
  time_command (0x66, hsk_packet_size, 1);
  time_command (0x14, 0x40, 1);
  for (i = 0; i < 0x10000; i += hsk_packet_size * hsk_packet_cnt) {
    start = i;
    stop = i + hsk_packet_size * hsk_packet_cnt - 16;
    if (stop > 0x10000)
      stop = 0xFFF0;
    time_command (start, stop, 2);
  }
  time_command (0x14, 0xC0, 1);
  for (i = 0x8000; i < 0xC000; i += hsk_packet_size * hsk_packet_cnt) {
    start = i;
    stop = i + hsk_packet_size * hsk_packet_cnt - 16;
    if (stop > 0xC000)
      stop = 0xBFF0;
    time_command (start, stop, 2);
  }
  time_command (0x14, 0xC0, 1);
  time_command (0x64, 0x09, 1);
  time_command (0x66, 192, 1);
  if (t_flag) {
    fprintf (mro_file, "-cdsseq_end\n");
  }
}
