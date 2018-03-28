
/*
 * rpws_archive_dumpo.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <SpiceUsr.h>

#include <fg.h>

#include <das2/das1.h>

/* Cas Stuff */
#include <rtiu.h>
#include <archive.h>
#include <UTIL_status.h>


#define WIDE8  6
#define WIDE12 8
#undef  WIDE8
#undef  WIDE12
#define WIDE8  32
#define WIDE12 40


static char *title = { " CASSINI/RPWS Archive Dump" };
static char *Ver = { "0.0" };
int SpaceCraft_ID = -82;

#define BUFFER_SIZE 65536
static struct ARCHIVE_TIME_SERIES *archive;

char *Frequency_Band[] = { " 26 Hz",
  "2.5Khz",
  " 10Khz",
  " 75Khz"
};
char *Antenna[] = { "Ex ",
  "Eu (Ex+)",
  "Ev (Ex-)",
  "Ew (Ez) ",
  "Bx ",
  "By ",
  "Bz ",
  "   ",
  "HF",
  "   ",
  "   ",
  "LP ",
  "   ",
  "   ",
  "   ",
  "XX "
};

void hexdump (char *buf, int start, int stop)
{
  int i;

  for (i = start; i < stop; i++)
    fprintf (stdout, " %02X", buf[i] & 0xFF);
}
void emit_time (int seconds, int fine, int partition)
{
  static int first = 1;
  char sclk_temp[64];
  char *temp;
  int part = 1;
  int year;
  int month;
  int mday;
  int yday;
  int hour;
  int min;
  double dsec;
  double et;
  char format[] = { "D" };
  int prec = 3;

  char* metafile = getenv("CAS_TIME_KERNELS");
  
  if(metafile == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		exit(13);
  }
  
  if (first) {
    first = 0;
    furnsh_c(metafile);
  }

  if (partition)
    part = partition;
  fprintf (stdout, "SPICE Time ");
  sprintf (sclk_temp, "%d/%d:%03d", part, seconds, fine);
  scs2e_c(SpaceCraft_ID, sclk_temp, &et);
  et2utc_c(et, format, prec, 32, sclk_temp);

  year = strtol (sclk_temp, &temp, 10);
  yday = strtol (temp + 1, &temp, 10);
  mday = yday;
  month = 1;
  hour = strtol (temp + 3, &temp, 10);
  min = strtol (temp + 1, &temp, 10);
  dsec = strtod (temp + 1, &temp);
  tnorm (&year, &month, &mday, &yday, &hour, &min, &dsec);
  fprintf (stdout, "%04d-%03dT%02d:%02d:%06.3f ",
           year, yday, hour, min, dsec);
  fprintf (stdout, "\n");
  fflush (stdout);
}

  /**********************************************************************
   *	All we're up to is reading the Archive file and dumping status	*
   *	This provides a quick&dirty verificatiuon that the archive has	*
   *	been successfully written.					*
   *									*
   *	NOTE that the various fields are built in such a way that we	*
   *	do not need to do anything unique for either WFR or WBR,	*
   *	simply decode all status.  Inappropriate fields are coded	*
   *	so they can be easily ignored when appropriate			*
   **********************************************************************/
int main (int argc, char *argv[])
{

  FILE *input = stdin;
  int status;
  int i;
  int seconds;
  int hour, min, sec, msec;
  static int count = 1;
  int prec = 3;
  char *format[2] = { "D", "J" };
  char utcout[64];

  fg_flags (argc, argv);
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stdout, "\n");
    fprintf (stdout, "%s %s   HELP SCREEN\n", title, Ver);
    fprintf (stdout, "  This utility dumps archive data\n");
    fprintf (stdout, "                \n");
    fprintf (stdout, "  Sample output\n");
    fprintf (stdout, "  WFR cnt:1234  1/12345678.9.0   ");
    fprintf (stdout, "12345-12:34:56.789 ");
    fprintf (stdout, "rec byt:4128 ");
    fprintf (stdout, "samples:2048   ");
    fprintf (stdout, "26 Hz ");
    fprintf (stdout, "30dB ");
    fprintf (stdout, "Ew (Ez)\n");
    fprintf (stdout, "  ^^^--- WFR or WBR indication\n");
    fprintf (stdout, "         blank would indicate neither bit is set\n");
    fprintf (stdout, "         (this woould be an error indication).\n");
    fprintf (stdout, "      ^^^--- packet count\n");
    fprintf (stdout,
             "             A simple counter, should en up matching\n");
    fprintf (stdout, "             the reord count that appears in the \n");
    fprintf (stdout, "             label file.\n");
    fprintf (stdout, "                ^^^--- SCLK.RTI.fine.TQF\n");
    fprintf (stdout,
             "                       Spacecraft Clock: 32 bits of seconds, 3 bits of RTI,\n");
    fprintf (stdout,
             "                       4 bits of RPWS packet sequence (makes all 40 bit SCLKs\n");
    fprintf (stdout,
             "                       unique), and 1 bit of 'time quality' set to indicate\n");
    fprintf (stdout,
             "                       CDS did not provide time (always set in HRS data from HRP)\n");
    fprintf (stdout,
             "                               ^^^--- SCET, decoded (mSec resolution)\n");
    fprintf (stdout,
             "                                      This is the Spacecraft Event Time that\n");
    fprintf (stdout,
             "                                      was derived from the SCLK value.\n");
    fprintf (stdout, "                                      \n");
    fprintf (stdout,
             "                                                  ^^^--- Record length\n");
    fprintf (stdout,
             "                                                         Indication of the overall length\n");
    fprintf (stdout,
             "                                                         of this record.  All records should match\n");
    fprintf (stdout,
             "                                                         in any given file (and match the label file).\n");
    fprintf (stdout,
             "                                                               ^^^--- samples in the record\n");
    fprintf (stdout,
             "                                                                      Number of data samples in the time series.\n");
    fprintf (stdout,
             "                                                                      Note that WBR and WFR use different sample\n");
    fprintf (stdout,
             "                                                                      sizes (this is NOT a byte counter).\n");
    fprintf (stdout,
             "                                                                              ^^^--- band\n");
    fprintf (stdout,
             "                                                                                     Sampling rate, filter selection.\n");
    fprintf (stdout,
             "                                                                                     WBR uses two values while WFR uses 2\n");
    fprintf (stdout,
             "                                                                                     other values.\n");
    fprintf (stdout,
             "                                                                                        ^^^--- gain setting\n");
    fprintf (stdout,
             "                                                                                               Gain setting, expredded in dB.  \n");
    fprintf (stdout,
             "                                                                                               Also the WALSH digital gain factor.\n");
    fprintf (stdout,
             "                                                                                               WBR and WFR have different ranges, \n");
    fprintf (stdout,
             "                                                                                               but they are all expressed in a\n");
    fprintf (stdout,
             "                                                                                               common system.  WFR is 00dB to 30dB \n");
    fprintf (stdout,
             "                                                                                               while WBR is 00dB to 70dB.\n");
    fprintf (stdout,
             "                                                                                             ^^^--- antenna selection\n");
    fprintf (stdout,
             "                                                                                                    Antenna selection.  As with \n");
    fprintf (stdout,
             "                                                                                                    all the other status fields, \n");
    fprintf (stdout,
             "                                                                                                    this uses a common set of \n");
    fprintf (stdout,
             "                                                                                                    values with WBR using 5 of \n");
    fprintf (stdout,
             "                                                                                                    the set and WFR using about \n");
    fprintf (stdout,
             "                                                                                                    6 of them.\n");
    fprintf (stdout, "\n");
    fprintf (stdout,
             "  hdr 4E 71 0A A6 00 60 3B 80 04 4F 80 08 10 20 08 00 55 33 20 00 00 03 00 00 00 00 00 00 00 00 00 00:  850 80E 7DA 7E3\n");
    fprintf (stdout,
             "      HEX dump of the packet header (32 bytes) followed by a data dump\n");
    fprintf (stdout,
             "      This is a WFR record (as indicated in the previous line and by the 12 bit sample)\n");
    fprintf (stdout, "      The WBR would display 8 bit samples\n");
    fprintf (stdout, "      \n");
    return 0;
  }

#ifdef ARCHIVE_TIME_V2
  /* ldpool _ (leapfile, strlen (leapfile)); */
  /* ldpool _ (sclkfile, strlen (sclkfile)); */
#endif

#ifdef ARCHIVE_TIME_V4
  /* ldpool _ (leapfile, strlen (leapfile)); */
  /* ldpool _ (sclkfile, strlen (sclkfile)); */
#endif

  archive = malloc (BUFFER_SIZE);
  status = fread (archive, 32, 1, input);
  while (status) {
    if (archive->sclk.seconds) {
      emit_time (archive->sclk.seconds, archive->sclk.fine,
                 archive->sclk.partition);

#ifdef ARCHIVE_TIME_V1
      seconds = archive->scet.msec_of_day / 1000;
      hour = seconds / 3600;
      min = seconds / 60 - hour * 60;
      sec = seconds % 60;
      msec = archive->scet.msec_of_day % 1000;
#endif

#ifdef ARCHIVE_TIME_V2
      et2utc_ (&archive->et, format[0], &prec, utcout, strlen (format[0]),
               32);
      *strchr (&utcout[0], ' ') = 0;
      *strchr (&utcout[12], ' ') = 0;
#endif

#ifdef ARCHIVE_TIME_V4
      seconds = archive->scet.msec_of_day / 1000;
      hour = seconds / 3600;
      min = seconds / 60 - hour * 60;
      sec = seconds % 60;
      msec = archive->scet.msec_of_day % 1000;
      et2utc_ (&archive->et, format[0], &prec, utcout, strlen (format[0]),
               32);
      *strchr (&utcout[0], ' ') = 0;
      *strchr (&utcout[12], ' ') = 0;
#endif

      fprintf (stdout, "%s%s",
               get_status (&archive->validity_flag,
                           ARCH_VALIDITY_FLAG_INST_ID_BIT_WBR, 0) ?
               "WBR" : "",
               get_status (&archive->validity_flag,
                           ARCH_VALIDITY_FLAG_INST_ID_BIT_WFR, 0) ?
               "WFR" : "");
      fprintf (stdout, "%s", archive->status_flag & 0x20 ? "TMO " : " ");
      fprintf (stdout, "cnt:%4d  ", count++);
      fprintf (stdout, "%d/", archive->sclk.partition);
      fprintf (stdout, "%08X", archive->sclk.seconds);
      fprintf (stdout, ".%d", (archive->sclk.fine >> 5) & 0x03);
      fprintf (stdout, ".%d", (archive->sclk.fine >> 1) & 0x0F);
      fprintf (stdout, "%s", ((archive->sclk.fine >> 0) & 0x01) ? "*" : " ");
      fprintf (stdout, "%s", archive->status_flag & 0x40 ? "Q" : " ");
      fprintf (stdout, " ");

#ifdef ARCHIVE_TIME_V1
      fprintf (stdout, "%5d-", archive->scet.days);
      fprintf (stdout, "%02d:%02d:%02d.%3d ", hour, min, sec, msec);
#endif

#ifdef ARCHIVE_TIME_V2
      fprintf (stdout, "%sT%s ", &utcout[0], &utcout[12]);
#endif

#ifdef ARCHIVE_TIME_V3
      fprintf (stdout, "%s ", archive->scet);
#endif

#ifdef ARCHIVE_TIME_V4
      fprintf (stdout, "%sT%s ", &utcout[0], &utcout[12]);
      fprintf (stdout, "%5d-", archive->scet.days);
      fprintf (stdout, "%02d:%02d:%02d.%3d ", hour, min, sec, msec);
#endif

      fprintf (stdout, "rti:%04X ", archive->data_rti);
      fprintf (stdout, "rec byt:%d ", archive->record_bytes);
      fprintf (stdout, "samples:%d ", archive->samples);
      fprintf (stdout, "%s ", Frequency_Band[archive->frequency_band & 0x03]);
      fprintf (stdout, "%d0dB ", archive->gain);
      fprintf (stdout, "%s", archive->status_flag & 0x80 ? "AGC " : "");
      fprintf (stdout, "%s", Antenna[archive->antenna & 0x0F]);
      fprintf (stdout, "%s", get_status (&archive->status_flag,
                                         ARCH_STATUS_FLAG_HFR_H2,
                                         0) ? "/H2 " : "");
      fprintf (stdout, "%s",
               get_status (&archive->status_flag, ARCH_STATUS_FLAG_HFR_H1,
                           0) ? "/H1 " : "");

      if (get_status (&archive->validity_flag, ARCH_VALIDITY_FLAG_SUB_RTI, 0)) {
        fprintf (stdout, "%3dmS ", archive->sub_rti);
        if (!get_status
            (&archive->validity_flag, ARCH_STATUS_FLAG_FINE_TIME_QUALITY, 0))
          fprintf (stdout, "+10/-0 mS ");
      }

      if (get_status
          (&archive->validity_flag, ARCH_VALIDITY_FLAG_HFR_XLATE, 0)) {
        if (get_status (&archive->status_flag, ARCH_STATUS_FLAG_HFR_H1, 0))
          fprintf (stdout, "%6.3fMhz ", (float) archive->hfr_xlate * 0.025);
        if (get_status (&archive->status_flag, ARCH_STATUS_FLAG_HFR_H2, 0))
          fprintf (stdout, "%6.3fMhz ",
                   4.025 + (float) archive->hfr_xlate * 0.050);
      }

      if (get_status
          (&archive->validity_flag, ARCH_VALIDITY_FLAG_LP_DAC_0, 0))
        fprintf (stdout, "DAC0:%03d ", archive->lp_dac_0);

      if (get_status
          (&archive->validity_flag, ARCH_VALIDITY_FLAG_LP_DAC_1, 0))
        fprintf (stdout, "%DAC1:%03d ", archive->lp_dac_1);

      if (archive->fsw_ver)
        fprintf (stdout, "FSW V%d.%d ", archive->fsw_ver / 100,
                 archive->fsw_ver % 100);

      fprintf (stdout, "\n");
      memset (archive->time_series.byte_sample, 0xFF, 1024);
      status =
        fread (archive->time_series.byte_sample, archive->record_bytes - 32,
               1, input);
      if (!status)
        return 0;
      fprintf (stdout, "hdr");
      hexdump ((char *) archive, 0, 32);
      fprintf (stdout, ": ");
      if (WIDE8 > 10)
        fprintf (stdout, "\n");
      switch (get_status
              (&archive->validity_flag, ARCH_VALIDITY_FLAG_INST_ID, 0)) {
       case ARCH_VALIDITY_FLAG_INST_ID_WBR:
         for (i = 0; i < WIDE8; i++)
           fprintf (stdout, " %02X", archive->time_series.wbr_sample[i]);
         break;
       case ARCH_VALIDITY_FLAG_INST_ID_WFR:
         for (i = 0; i < WIDE12; i++)
           fprintf (stdout, " %03X", archive->time_series.wfr_sample[i]);
         break;
      }
    } else {
      fprintf (stdout, "BAD");
      hexdump ((char *) archive, 0, 32);
      fprintf (stdout, ": ");
      hexdump ((char *) archive, 32, 48);
    }
    fprintf (stdout, "\n");
    fprintf (stdout, "\n");
    status = fread (archive, 32, 1, input);
  }

  return 0;
}
