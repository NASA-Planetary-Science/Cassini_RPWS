
/*
 * file.c
 */
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <stdbool.h>

#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilo.h"
#include "utilt.h"

#include <Cext.h> /**/
#include <rpwstlm/CasType.h>
#include <rpwstlm/CasRecord.h> /**/
#include <rpwstlm/CasHfr.h>
#define SCIOP_TIMEOUT 10
#define BUFFER_SIZE 65536
#define CASSINI_STARTING_SCLK 0x4AE30000
#define DEBUG_PROFILE	0x0001
#define DEBUG_BIT_1	0x0002
#define DEBUG_BIT_2	0x0004
#define DEBUG_NAMES	0x0008
#define DEBUG_STATUS	0x0010
#define DEBUG_BIT_5	0x0020
#define DEBUG_BIT_6	0x0040
#define DEBUG_BIT_7	0x0080
#define DEBUG_DATABASE	0x0100
#define DEBUG_MEANDER	0x0200
#define DEBUG_LINE	0x0400
#define DEBUG_NEW_NAME	0x0800
#define DEBUG_NW_NAME	0x1000
#define DEBUG_BIT_13	0x2000
#define DEBUG_RAJ	0x4000
#define DEBUG_F_LENGTH	0x8000

/* extern FILE *hCasHfrErr;	/* meander decompress error message file */

/* extern FILE *hCasHfrMeanderErr;	/* meander decompress error message file */
extern int nMeanderDebug;
extern char *sCasHfrMeanderVersion;

static char *title = { " CASSINI rpws_file (file_58)" };
static char *Ver = { "5.6.3" };

/* int MiniPacketToKronos(struct RPWS_buffer *, unsigned char *, int); /**/
static int K60 = 60;
static int Kstyle = 0;
static int Kstdout = 0;
static int addstdout = 0;
static int fix_time_flag = 1;
static int Kfile_list = 1;
static int debug_flag = 0;
static int Split_flag = 0;
static int Pad_flag = 1;
static int stpk1_flag = 0;
static int meander_flag = 1;
static int hfrcal_flag = 0;
static int rti_flag = 0;
static int chdo_flag = 0;
static int sciop_flag = 0;
static int filerc_flag = 0;
static int nodupe_flag = 0;
static int regression_flag = 0;
static int main_file_list_flag = 0;
static int message_status_flag = 1;

struct RPWS_buffer *buffer;
struct RPWS_buffer *bufferd;
struct RPWS_buffer *buffer1;
static unsigned char kbuffer[128000];
static char Raw_Data = 'r';
static char Segmented_Data = 'm';
static char Unsegmented_Data = 'u';
static char Langmuir_Probe_Data = 'l';
static char HFR_Data = 'h';
static char Stim_Data = 's';
static char path[256] = { "" };

static int strip_flag = 0;

  /*
   *    This data structure holds the earliest and latest
   *    time-tag for each file created.  
   */
struct Status_Entry
{
  struct Status_Entry *link;
  int count;
  int flags;
  struct event_time scet_start;
  struct event_time scet_stop;
  struct event_clock sclk_start;
  struct event_clock sclk_stop;
  struct event_time chdo_scet_start;
  struct event_time chdo_scet_stop;
  struct event_clock chdo_sclk_start;
  struct event_clock chdo_sclk_stop;
  char filename[32];
} *status_entry = NULL;
FILE *mssg;
FILE *debug_file;

int epoch_flag = 0;
int bench_flag = 0;

 /****************************************************************/
int a_dump (FILE * out, char *label, unsigned char *buffer)
{
  int i;

  fprintf (out, "\n");

  fprintf (out, "%s", CasTlm_Version ());

  fprintf (out, "\n");
  fprintf (out, "%s: ", label);
  for (i = 0; i < 12; i++) {
    fprintf (out, "%02X", buffer[i]);
    if (i % 4 == 3)
      fprintf (out, "  ");
  }
  fprintf (out, "\n");
  fflush (out);
  for (i = 0; i < 256; i++) {
    if (i % 32 == 0)
      fprintf (out, "%04X: ", i);
    fprintf (out, "%02X", buffer[i + 12]);
    if (i % 4 == 3)
      fprintf (out, "  ");
    if (i % 32 == 31)
      fprintf (out, "\n");
  }

  for (i = 0; i < 64; i++) {
    if (i % 32 == 0)
      fprintf (out, "%04X: ", i);
    fprintf (out, " %02X", buffer[i + 12 + 256]);
    if (i % 32 == 31)
      fprintf (out, "\n");
  }
  return 0;
}
int b_dump (FILE * out,
            char *label, unsigned char *buffer, unsigned char *buffer1)
{
  int i;
  int m;

  fprintf (out, "\n");
  fprintf (out, "%s: ", label);
  m = 0;
  for (i = 0; i < 12; i++) {
    fprintf (out, "%02X", buffer[i]);
    if (buffer[i] != buffer1[i])
      m++;
    if (i % 4 == 3) {
      if (m) {
        fprintf (out, "* ");
        m = 0;
      } else
        fprintf (out, "  ");
    }
  }
  fprintf (out, "\n");
  fflush (out);
  m = 0;
  for (i = 0; i < 256; i++) {
    if (i % 32 == 0)
      fprintf (out, "%04X: ", i);
    fprintf (out, "%02X", buffer[i + 12]);
    if (buffer[i + 12] != buffer1[i + 12])
      m++;
    if (i % 4 == 3) {
      if (m) {
        fprintf (out, "* ");
        m = 0;
      } else
        fprintf (out, "  ");
    }
    if (i % 32 == 31)
      fprintf (out, "\n");
  }

  for (i = 0; i < 64; i++) {
    if (i % 32 == 0)
      fprintf (out, "%04X: ", i);
    fprintf (out, "%02X", buffer[i + 12 + 256]);
    if (buffer[i + 12 + 256] == buffer1[i + 12 + 256])
      fprintf (out, " ");
    else
      fprintf (out, "*");
    if (i % 32 == 31)
      fprintf (out, "\n");
  }
  return 0;
}

 /****************************************************************/
static int status_up (struct RPWS_buffer *buffer,
                      struct Status_Entry *se,
                      struct event_clock _clock, struct event_time *_time)
{
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_up\n");
  se->count += 1;
  if (_clock.seconds < se->sclk_start.seconds) {
    se->sclk_start.seconds = _clock.seconds;
    se->sclk_start.fine = _clock.fine;
    se->scet_start.days = _time->days;
    se->scet_start.milliseconds = _time->milliseconds;
  }
  if (_clock.seconds > se->sclk_stop.seconds) {
    se->sclk_stop.seconds = _clock.seconds;
    se->sclk_stop.fine = _clock.fine;
    se->scet_stop.days = _time->days;
    se->scet_stop.milliseconds = _time->milliseconds;
  }
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_up ---\n");
}

 /****************************************************************/
static int status_out_line (FILE * outf, struct Status_Entry *se)
{
  struct tm *tm_start, *tm_stop;
  struct tm tm_temp;
  char start[32], stop[32];
  char text[1024];
  char tfmt[] = { "%04d-%03dT%02d:%02d:%02d.%03d" };

  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_out_line\n");

  if (sciop_flag)
    return 0;

  if (se->sclk_start.seconds < CASSINI_STARTING_SCLK) { /* before deploy */
    memcpy (&tm_temp,
            gmtime ((const long *) &se->sclk_start.seconds),
            sizeof (const struct tm));
    tm_start = &tm_temp;
    tm_stop = gmtime ((const long *) &se->sclk_stop.seconds);
  } else {
    tm_start = UTIL_event_scet_tm (se->scet_start, 0);
    tm_stop = UTIL_event_scet_tm (se->scet_stop, 0);
  }

  sprintf (start, tfmt, tm_start->tm_year + 1900,
           tm_start->tm_yday + 1,
           tm_start->tm_hour,
           tm_start->tm_min,
           tm_start->tm_sec, se->scet_start.milliseconds % 1000);
  sprintf (stop, tfmt, tm_stop->tm_year + 1900,
           tm_stop->tm_yday + 1,
           tm_stop->tm_hour,
           tm_stop->tm_min,
           tm_stop->tm_sec, se->scet_stop.milliseconds % 1000);

  if (rti_flag)
    sprintf (text, "%s %s  %08X.%X %08X.%X  %s%s  %08X",
             start,
             stop,
             se->sclk_start.seconds,
             se->sclk_start.fine >> 5,
             se->sclk_stop.seconds,
             se->sclk_stop.fine >> 5, path, se->filename, se->flags);
  else
    sprintf (text, "%s %s  %08X %08X  %s%s  %08X",
             start,
             stop,
             se->sclk_start.seconds,
             se->sclk_stop.seconds, path, se->filename, se->flags);
  if (chdo_flag)
    fprintf (outf, "%s %d.%d  %X.%X\n", text,
             se->chdo_scet_start.days,
             se->chdo_scet_start.milliseconds,
             se->chdo_sclk_start.seconds, se->chdo_sclk_start.fine >> 5);
  else
    fprintf (outf, "%s\n", text);
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_out_line ---\n");
}

  /****************************************************************/
static int status_out (FILE * outf)
{
  struct Status_Entry *se;

  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_out\n");

  se = status_entry;
  while (se) {
    status_out_line (outf, se);
    se = se->link;
  }
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_out ---\n");
  return 0;
}

 /****************************************************************/
static struct Status_Entry *status_search (char *fn)
{
  struct Status_Entry *se;

  if (debug_flag & DEBUG_STATUS) {
    fprintf (debug_file, "status_search\n");
    fflush (debug_file);
  }

  se = status_entry;
  while (se) {
    if (!strcmp (fn, se->filename)) {
      if (debug_flag & DEBUG_STATUS) {
        fprintf (debug_file, "status_search ---\n");
        fflush (debug_file);
      }
      return se;
    }
    se = se->link;
  }
  if (debug_flag & DEBUG_STATUS) {
    fprintf (debug_file, "status_search --- NULL\n");
    fflush (debug_file);
  }
  return NULL;
}

 /****************************************************************/
static int status_ (char *fn,
                    struct RPWS_buffer *buffer,
                    struct Status_Entry *se,
                    struct event_clock _clock, struct event_time *_time)
{
  if (debug_flag & DEBUG_STATUS) {
    fprintf (debug_file, "x line %d %s %p\n", __LINE__, fn, _time);
    fflush (debug_file);
  }

  if (strlen (fn) < 32)
    strcpy (se->filename, fn);
  se->link = NULL;
  se->count = 1;
  se->scet_start.days = _time->days;
  se->scet_start.milliseconds = _time->milliseconds;
  se->scet_stop.days = _time->days;
  se->scet_stop.milliseconds = _time->milliseconds;
  se->sclk_start.seconds = _clock.seconds;
  se->sclk_start.fine = _clock.fine;
  se->sclk_stop.seconds = _clock.seconds;
  se->sclk_stop.fine = _clock.fine;
  se->chdo_scet_start.days = buffer->packet.chdo_tag.scet.days;
  se->chdo_scet_start.milliseconds =
    buffer->packet.chdo_tag.scet.milliseconds;
  se->chdo_scet_stop.days = buffer->packet.chdo_tag.scet.days;
  se->chdo_scet_stop.milliseconds = buffer->packet.chdo_tag.scet.milliseconds;
  se->chdo_sclk_start.seconds = buffer->packet.chdo_tag.sclk.seconds;
  se->chdo_sclk_start.fine = buffer->packet.chdo_tag.sclk.fine;
  se->chdo_sclk_stop.seconds = buffer->packet.chdo_tag.sclk.seconds;
  se->chdo_sclk_stop.fine = buffer->packet.chdo_tag.sclk.fine;
  if (debug_flag & DEBUG_STATUS) {
    fprintf (debug_file, "line %d\n", __LINE__);
    fflush (debug_file);
  }
  status_up (buffer, se, _clock, _time);
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_ ---\n");
  if (debug_flag & DEBUG_STATUS)
    fprintf (debug_file, "status_ %s\n", fn);
}

 /****************************************************************/
static int status_chain (FILE * file)
{
  struct Status_Entry *se;

  se = status_entry;
  fprintf (file, "  Chain %p=>%p", &se, se);
  while (se) {
    fprintf (file, "  %p->%p", se, se->link);
    se = se->link;
  }
  fprintf (file, "  %p\n", se);
}

 /****************************************************************/
static int status_insert (struct Status_Entry *element)
{
  int type;
  int i = 0;
  struct Status_Entry *se;

  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_insert\n");

  do {
    /*
     *     empty list
     */
    if (status_entry == NULL) {
      status_entry = element;
      element->link = NULL;
      break;
    }

    /*
     *     Does it belong at head ???
     */
    if (element->sclk_start.seconds < status_entry->sclk_start.seconds) {
      element->link = status_entry;
      status_entry = element;
      break;
    }

    /*
     *    insert in list (somewhere)
     */
    se = status_entry;
    while (se->link) {
      if (element->sclk_start.seconds < se->link->sclk_start.seconds) {
        element->link = se->link;
        se->link = element;
        break;
      }
      se = se->link;
    }

    /*
     *    insert at tail end of list...
     *            get here when link is NULL
     */
    se->link = element;
    element->link = NULL;
    break;
  } while (i);
  return 0;
}

 /****************************************************************/
static struct Status_Entry *allocate_slot (void)
{
  struct Status_Entry *se;

  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_ allocate_slot\n");
  se = malloc (sizeof (struct Status_Entry));
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_ allocate_slot ---\n");
  if (debug_flag & DEBUG_STATUS)
    fprintf (debug_file, "status_ allocate_slot(%p)\n", se);
  return se;
}

 /****************************************************************/
static struct Status_Entry *status_flags (struct RPWS_buffer *buffer,
                                          struct Status_Entry *se)
{
  switch (buffer->record_type & 0xFF) {
   case PACKET_TYPE_stim:
     se->flags |= CasSciStim;
     /**/ break;
   case PACKET_TYPE_mfr:
     se->flags |= CasSciMFR;
     /**/ break;
   case PACKET_TYPE_hfr:
     se->flags |= CasSciHFR;
     /**/ break;
   case PACKET_TYPE_lp:
     se->flags |= CasSciLP;
     /**/ break;
   case PACKET_TYPE_lfdr:
     se->flags |= CasSciLFDR;
     /**/ break;
   case PACKET_TYPE_wfr:
     se->flags |= CasSciWFR;
     /**/ break;
   case PACKET_TYPE_dust:
     se->flags |= CasSciDust;
     /**/ break;
   case PACKET_TYPE_wbr:
     se->flags |= CasSciWBR;
     /**/ break;
   case PACKET_TYPE_mro:
     se->flags |= CasSciMRO;
     /**/ break;
  }
}

  /****************************************************************/
static struct Status_Entry *status_report (char *fn,
                                           struct RPWS_buffer *buffer,
                                           struct event_clock _clock,
                                           struct event_time *_time)
{
  struct Status_Entry *se = NULL;

  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_report\n");
  if (debug_flag & DEBUG_STATUS)
    fprintf (debug_file, "status_report  --------------------------\n"
             "    %s\n", fn);

  if (debug_flag & DEBUG_STATUS) {
    fprintf (debug_file, "line %d\n", __LINE__);
    fflush (debug_file);
  }
  if (status_entry) {                   /* list exists, sort required */
    if (debug_flag & DEBUG_STATUS) {
      fprintf (debug_file, "line %d\n", __LINE__);
      fflush (debug_file);
    }
    if (se = status_search (fn)) {
      status_up (buffer, se, _clock, _time);
      if (debug_flag & DEBUG_STATUS) {
        fprintf (debug_file, "line %d\n", __LINE__);
        fflush (debug_file);
      }
    } else {
      if (debug_flag & DEBUG_STATUS) {
        fprintf (debug_file, "line %d\n", __LINE__);
        fflush (debug_file);
      }
      se = allocate_slot ();
      if (debug_flag & DEBUG_STATUS) {
        fprintf (debug_file, "line %d\n", __LINE__);
        fflush (debug_file);
      }
      status_ (fn, buffer, se, _clock, _time);
      if (debug_flag & DEBUG_STATUS) {
        fprintf (debug_file, "line %d\n", __LINE__);
        fflush (debug_file);
      }
      status_insert (se);
      if (debug_flag & DEBUG_STATUS) {
        fprintf (debug_file, "line %d\n", __LINE__);
        fflush (debug_file);
      }
    }
  } else {                              /* no list, so allocate 1st. element */

    if (debug_flag & DEBUG_STATUS) {
      fprintf (debug_file, "status_report  FIRST TIME\n");
      fflush (debug_file);
    }
    se = allocate_slot ();

    if (debug_flag & DEBUG_STATUS) {
      fprintf (debug_file, "a line %d %p\n", __LINE__, se);
      fflush (debug_file);
    }

    status_ (fn, buffer, se, _clock, _time);

    if (debug_flag & DEBUG_STATUS) {
      fprintf (debug_file, "b line %d\n", __LINE__);
      fflush (debug_file);
    }
    status_entry = se;
    if (debug_flag & DEBUG_STATUS) {
      fprintf (debug_file, "c line %d\n", __LINE__);
      fflush (debug_file);
    }
  }
  status_flags (buffer, se);
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "status_report ---\n");
  if (debug_flag & DEBUG_STATUS)
    status_chain (debug_file);
  return se;
}

 /****************************************************************/
static void time_dump (struct tm *pkt_tm,
                       struct event_clock _clock,
                       struct event_time *_time, char *text)
{
  fprintf (debug_file, "    ");
  fflush (debug_file);
  fprintf (debug_file, "%s ", text);
  fflush (debug_file);
  fprintf (debug_file,
           "    Time clk:%08X.%X ", _clock.seconds, _clock.fine >> 5);
  fflush (debug_file);
  fprintf (debug_file, "tim:%04X,%08X ", _time->days, _time->milliseconds);
  fflush (debug_file);
  fprintf (debug_file,
           "- %4d %03d %2.2d:%2.2d:%2.2d\n",
           pkt_tm->tm_year + 1900,
           pkt_tm->tm_yday + 1,
           pkt_tm->tm_hour, pkt_tm->tm_min, pkt_tm->tm_sec);
  fflush (debug_file);
  return;
}


 /***************************=*************************************/
static void fix_time (struct RPWS_buffer *buffer, unsigned char *kbuffer)
{

  struct tm *pkt_tm;
  struct event_time *_time;
  struct event_clock evt_clock;
  int epoch;
  time_t evt_time;
  int year;

  epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
    (buffer->packet.cds_tag.epoch[1] << 16) |
    (buffer->packet.cds_tag.epoch[2] << 8) |
    (buffer->packet.cds_tag.epoch[3] << 0);

  evt_clock.seconds = UTIL_event_time ((struct MP_buffer *) buffer, 0);
  evt_clock.fine = UTIL_extract_MP_RTI ((struct MP_buffer *) buffer) << 5;
  evt_time = UTIL_event_time ((struct MP_buffer *) buffer, epoch);

  if (epoch) {
    _time = UTIL_event_scet ((struct MP_buffer *) buffer, evt_clock);
    pkt_tm = UTIL_event_scet_tm (*_time, 0);
  } else {
    pkt_tm = gmtime (&evt_time);
  }

  year = pkt_tm->tm_year + 1900;
  kbuffer[5] = pkt_tm->tm_min;          /* minutes */
  kbuffer[6] = pkt_tm->tm_hour;         /* hours  */
  kbuffer[7] = (_time->milliseconds % 1000) / 10;       /* 10mSec */
  kbuffer[8] = pkt_tm->tm_sec;          /* seconds */
  kbuffer[9] = (year >> 0) & 0xFF;      /* year LSB */
  kbuffer[10] = (year >> 8) & 0xFF;     /* year MSB */
  kbuffer[11] = pkt_tm->tm_mday;        /* day */
  kbuffer[12] = pkt_tm->tm_mon + 1;     /* month */

  return;
}

  /*
   * for the bench model, figure out something about time 
   */
time_t *bench_fixup (struct RPWS_buffer * buffer, struct event_time * _time)
{
  static time_t temp;

  _time->days = 0;
  _time->milliseconds = 0;
  temp = UTIL_event_time ((struct MP_buffer *) buffer, 0);
  if (debug_flag & DEBUG_STATUS) {
    fprintf (debug_file, " %X  beg:%02X%02X%02X%02X end:%02X%02X%02X%02X\n",
             temp,
             buffer->packet.cds_tag.begin[0],
             buffer->packet.cds_tag.begin[1],
             buffer->packet.cds_tag.begin[2],
             buffer->packet.cds_tag.begin[3],
             buffer->packet.cds_tag.end[0],
             buffer->packet.cds_tag.end[1],
             buffer->packet.cds_tag.end[2], buffer->packet.cds_tag.end[3]
      );

    fflush (debug_file);
  }
  return &temp;
}

static char *new_file_name (struct RPWS_buffer *buffer,
                            char flag, char HFR_Flag)
{
  struct event_clock hdr_clock;
  struct event_clock cds_clock;
  struct event_clock mp_clock;
  struct event_clock evt_clock;
  struct event_clock _clock;
  struct event_time *_time;
  struct event_time bench_time = { 0, 0 };
  struct tm *pkt_tm;
  struct Status_Entry *se;
  time_t *em_time = NULL;
  time_t evt_time;

  int record_type;
  int minute_of_day;
  int second_of_day;
  int epoch;
  int epOch = 0;
  char *text = { " CDS Header" };
  char *cds_text = { " CDS Record" };
  char *mp_text = { "  MP Record" };
  char *mpu_text = { "MPUS Record" };
  char type = 'a';
  static char filename[8][256];
  static int filename_index = 0;

  filename_index += 1;
  filename_index &= 7;

  epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
    (buffer->packet.cds_tag.epoch[1] << 16) |
    (buffer->packet.cds_tag.epoch[2] << 8) |
    (buffer->packet.cds_tag.epoch[3] << 0);
  if (debug_flag & DEBUG_NEW_NAME) {
    fprintf (debug_file, "new file name(%d)  epoch %X  buffer %p  ", __LINE__,
             epoch, buffer);
    fprintf (debug_file, "************************\n");
    fflush (debug_file);
  }

  if (debug_flag & (DEBUG_NEW_NAME | DEBUG_PROFILE)) {
    fprintf (debug_file,
             "new_file_name(%d) %02X%02X%02X%02X  %02X%02X%02X%02X  %d\n",
             __LINE__, buffer->packet.cds_tag.begin[0],
             buffer->packet.cds_tag.begin[1], buffer->packet.cds_tag.begin[2],
             buffer->packet.cds_tag.begin[3], buffer->packet.cds_tag.end[0],
             buffer->packet.cds_tag.end[1], buffer->packet.cds_tag.end[2],
             buffer->packet.cds_tag.end[3], 0);
    fflush (debug_file);
  }

  hdr_clock.seconds = (buffer->packet.cds_tag.begin[0] << 24) |
    (buffer->packet.cds_tag.begin[1] << 16) |
    (buffer->packet.cds_tag.begin[2] << 8) |
    (buffer->packet.cds_tag.begin[3] << 0);
  hdr_clock.fine = 0;

  cds_clock.seconds = UTIL_extract_TIME ((struct CDS_buffer *) buffer);
  cds_clock.fine = UTIL_extract_RTI ((struct CDS_buffer *) buffer) << 5;

  mp_clock.seconds = UTIL_extract_PKT_TIME ((struct MP_buffer *) buffer);
  mp_clock.fine = UTIL_extract_MP_RTI ((struct MP_buffer *) buffer) << 5;

  evt_clock.seconds = UTIL_event_time ((struct MP_buffer *) buffer, 0);
  evt_clock.fine = UTIL_extract_MP_RTI ((struct MP_buffer *) buffer) << 5;

  evt_time = UTIL_event_time ((struct MP_buffer *) buffer, epoch);

  if (debug_flag & DEBUG_NEW_NAME) {
    fprintf (debug_file, "    hdr_clock.seconds %X\n", __LINE__,
             hdr_clock.seconds);
    fprintf (debug_file, "    cds_clock.seconds %X\n", __LINE__,
             cds_clock.seconds);
    fprintf (debug_file, "    evt_clock.seconds %X\n", __LINE__,
             evt_clock.seconds);
    fprintf (debug_file, "    mp_clock.seconds  %X\n", __LINE__,
             mp_clock.seconds);
    fprintf (debug_file, "    evt_time          %X\n", __LINE__, evt_time);
  }

  record_type = buffer->record_type;
  if (debug_flag & DEBUG_NEW_NAME) {
    fprintf (debug_file, "    record type       %X\n", __LINE__,
             buffer->record_type);
  }
  switch (record_type & 0xF00) {
   case DATA_telemetry:
     if (debug_flag & (DEBUG_NEW_NAME | DEBUG_PROFILE)) {
       fprintf (debug_file, "    case DATA_telemetry\n");
       fflush (debug_file);
     }
     _clock.seconds = cds_clock.seconds;
     _clock.fine = cds_clock.fine;
     text = cds_text;
     type = Raw_Data;
     em_time = (long *) &cds_clock.seconds;
     break;
   case DATA_MP_packet:
   case DATA_MP_segment:
   case DATA_MP_large_segment:
     if (debug_flag & (DEBUG_NEW_NAME | DEBUG_PROFILE)) {
       fprintf (debug_file, "    case MP_\n");
       fflush (debug_file);
     }
     _clock.seconds = evt_clock.seconds;
     _clock.fine = evt_clock.fine;
     text = mp_text;
     type = Segmented_Data;
     em_time = &evt_time;
     break;
   case DATA_MP_complete_segment:
     if (debug_flag & (DEBUG_NEW_NAME | DEBUG_PROFILE)) {
       fprintf (debug_file, "    case MP_complete_segment\n");
       fflush (debug_file);
     }
     _clock.seconds = evt_clock.seconds;
     _clock.fine = evt_clock.fine;
     text = mpu_text;
     type = Unsegmented_Data;
     em_time = &evt_time;
     break;
  }
  if (flag)
    type = flag;
  /*
   *     Decisions about how to do time...
   *       We found some ATLO files that were goofey, so
   *     "epoch_flag" may be used to invert the decision...
   */
  if (epoch != 0)
    epOch = 1;
  if (epoch_flag)
    epOch += 1;
  epOch &= 1;

  if (debug_flag & (DEBUG_LINE | DEBUG_NEW_NAME)) {
    fprintf (debug_file, "line %d epOch(%d)\n", __LINE__, epOch);
    fflush (debug_file);
  }
  if (epOch) {                          /* This happens for S/C data */
    _time = UTIL_event_scet ((struct MP_buffer *) buffer, _clock);
    pkt_tm = UTIL_event_scet_tm (*_time, 0);
    second_of_day = _time->milliseconds / 1000;
    minute_of_day = second_of_day / 60;
    minute_of_day = minute_of_day / K60;
    minute_of_day = minute_of_day * K60;
    if (debug_flag & (DEBUG_LINE | DEBUG_NEW_NAME)) {
      fprintf (debug_file, "line %d\n", __LINE__);
      fprintf (debug_file, "    em_time %X (%p)\n", *em_time, em_time);
      fprintf (debug_file, "    pkt_tm  %X (%p)\n", *pkt_tm, pkt_tm);
      fprintf (debug_file, "    _time   %X.%X\n", _time->days,
               _time->milliseconds);
      fflush (debug_file);
    }
  } else {                              /* This happens for Bench Data */

    _time = &bench_time;
    if (bench_flag)
      em_time = bench_fixup (buffer, _time);
    pkt_tm = gmtime (em_time);
    if (debug_flag & (DEBUG_LINE | DEBUG_NEW_NAME)) {
      fprintf (debug_file, "line %d\n", __LINE__);
      fprintf (debug_file, "    em_time %X (%p)\n", *em_time, em_time);
      fprintf (debug_file, "    pkt_tm  %X (%p)\n", *pkt_tm, pkt_tm);
      fprintf (debug_file, "    _time   %X.%X\n", _time->days,
               _time->milliseconds);
      fflush (debug_file);
    }
    minute_of_day = pkt_tm->tm_hour * 60 + pkt_tm->tm_min;
    minute_of_day = minute_of_day / K60;
    minute_of_day = minute_of_day * K60;
    if (debug_flag & (DEBUG_LINE | DEBUG_NEW_NAME)) {
      fprintf (debug_file, "line %d\n", __LINE__);
      fprintf (debug_file, "    em_time %X (%p)\n", *em_time, em_time);
      fprintf (debug_file, "    pkt_tm  %X (%p)\n", *pkt_tm, pkt_tm);
      fprintf (debug_file, "    _time   %X.%X\n", _time->days,
               _time->milliseconds);
      fflush (debug_file);
    }
  }

  switch (Kstyle) {
   case 0:
     sprintf (filename[filename_index], "t%04d%03d%02d%02d.%c00",
              pkt_tm->tm_year + 1900,
              pkt_tm->tm_yday + 1,
              minute_of_day / 60, minute_of_day % 60, type);
     break;
   case 1:
     sprintf (filename[filename_index], "t%04d%03d%04d.%c00",
              pkt_tm->tm_year + 1900,
              pkt_tm->tm_yday + 1, minute_of_day, type);
     break;
  }
  if (debug_flag & (DEBUG_LINE | DEBUG_NEW_NAME)) {
    fprintf (debug_file, "line %d\n", __LINE__);
    fprintf (debug_file, "   %d %s\n", filename_index,
             filename[filename_index]);
    fflush (debug_file);
  }
  if (HFR_Flag) {
    sprintf (filename[filename_index], "K%04d%03d.%02d",
             pkt_tm->tm_year + 1900, pkt_tm->tm_yday + 1, minute_of_day / 60);
  }

  if (debug_flag & (DEBUG_LINE | DEBUG_NEW_NAME)) {
    fprintf (debug_file, "line %d\n", __LINE__);
    fprintf (debug_file, "   %d %s\n", filename_index,
             filename[filename_index]);
    fprintf (debug_file, "   %X flag\n", flag);
    fflush (debug_file);
  }
  if (debug_flag & (DEBUG_BIT_1 | DEBUG_BIT_2 | DEBUG_NAMES | DEBUG_NEW_NAME)) {
    if (!flag) {
      fprintf (debug_file, "new_file_name --------------------->%d\n",
               __LINE__);
      if (debug_flag & (DEBUG_BIT_1 | DEBUG_NEW_NAME))
        fprintf (debug_file, "    %s hdr %08X  "
                 "cds %08X.%X  "
                 "mp  %08X.%X  "
                 "evt %08X.%X  "
                 "delta %4X \n",
                 UTIL_extract_MP_packet_type ((struct MP_buffer *) buffer),
                 hdr_clock.seconds,
                 cds_clock.seconds,
                 cds_clock.fine >> 5,
                 mp_clock.seconds,
                 mp_clock.fine >> 5,
                 evt_clock.seconds,
                 evt_clock.fine >> 5, mp_clock.seconds - evt_clock.seconds);
      if (debug_flag & (DEBUG_BIT_2 | DEBUG_NEW_NAME)) {
        fprintf (debug_file, " time dump %d\n", __LINE__);
        time_dump (pkt_tm, _clock, _time, filename[filename_index]);
      /**/}
      if (debug_flag & (DEBUG_NAMES | DEBUG_NEW_NAME)) {
        fprintf (debug_file, "    copied: sclk %08X.%X  scet %04d,%08d\n",
                 buffer->packet.chdo_tag.sclk.seconds,
                 buffer->packet.chdo_tag.sclk.fine >> 5,
                 buffer->packet.chdo_tag.scet.days,
                 buffer->packet.chdo_tag.scet.milliseconds);
        fflush (debug_file);
      }
      fprintf (debug_file,
               "new_file_name ---------------------<<<<<<<<<<<\n");
      fflush (debug_file);
    }
  }
  if (debug_flag & (DEBUG_LINE | DEBUG_NEW_NAME)) {
    fprintf (debug_file, "line %d\n", __LINE__);
    fflush (debug_file);
  }
  if (!flag)                            /* when flag=0 the primary output (HRP/LP otherwise) */
    se = status_report (filename[filename_index], buffer, _clock, _time);
  if (debug_flag & (DEBUG_LINE | DEBUG_NEW_NAME)) {
    fprintf (debug_file, "line %d\n", __LINE__);
    fflush (debug_file);
  }
  if (debug_flag & (DEBUG_NEW_NAME | DEBUG_DATABASE)) {
    status_out_line (debug_file, se);
    fflush (debug_file);
  }
  if (debug_flag & (DEBUG_NEW_NAME | DEBUG_PROFILE)) {
    fprintf (debug_file, "new_file_name --- %d\n", __LINE__);
    fflush (debug_file);
  }
  return filename[filename_index];
}

unsigned char extract_char (char *buf, int index)
{
  return buf[index];
}

int util_getbuffer_RPWS (struct RPWS_buffer *buffer, FILE * input,
                         int eof_flag)
{
  int ilen;
  int bad_record = 1;
  int i;
  int mp_len;
  static int last_time = 0;
  int current_time;
  int local_eof_flag = eof_flag;

  if (sciop_flag) {
    local_eof_flag = UTIL_GET_RELEASE;  /* let go of CPU for just a bit */
    bad_record = SCIOP_TIMEOUT;         /* about a 30 second timeout */
  }
  while (bad_record) {
    ilen = UTIL_getbuffer_RPWS (buffer, input, local_eof_flag);
    /*
     * fprintf(stderr,"%d=ilen %d=bad_record\n", ilen, bad_record); /*
     */
    if (sciop_flag)                     /* data from sciop */
      bad_record -= 1;                  /* then do timeout */
    if (ilen > 0) {                     /* data to read ??? */
      mp_len = UTIL_MP_length ((struct MP_buffer *) buffer);
      if (mp_len == 0) {                /* funny record encountered */
        bad_record = strip_flag;        /* dump it ? */
      } else {
        bad_record = 0;
        current_time = (buffer->packet.cds.header[6] << 24) |
          (buffer->packet.cds.header[9] << 16) |
          (buffer->packet.cds.header[8] << 8) |
          (buffer->packet.cds.header[11] << 0);
        if (nodupe_flag) {
          if (!memcmp (bufferd->packet.cds.header,
                       buffer->packet.cds.header, 12)) {
            bad_record = 1;
            /*
             * fprintf(stderr,"nodupe  %08X\n", current_time); /*
             */
          }
        }                               /* nodupe flag */
        if (regression_flag) {
          if (last_time) {
            if ((last_time - current_time) > regression_flag) {
              bad_record = 1;
              fprintf (stderr, "regress %08X = %08X - %08X > %08X \n",
                       last_time - current_time,
                       last_time, current_time, regression_flag);

            }                           /* time regression */
          }                             /* last time > 0 */
        }                               /* regression flag */
      }                                 /* mp_len > 0 */
    } /* if (ilen) */
    else {                              /* else if (ilen) */

      if (main_file_list_flag)
        bad_record = 0;
    }                                   /* else  if (ilen) */
    memcpy (bufferd, buffer, 1024);     /* only need begining of the record */
  }                                     /* bad record */
  last_time = (buffer->packet.cds.header[6] << 24) |
    (buffer->packet.cds.header[9] << 16) |
    (buffer->packet.cds.header[8] << 8) |
    (buffer->packet.cds.header[11] << 0);
  /*
   * fprintf(stderr,"timetag %08X\n", last_time); /*
   */
  return ilen;
}

 /*
  *     Fix buffer length
  */
int fix_len (unsigned char *buf, int len)
{
  int i;
  int result;
  int length = len;

  if ((length < 0) || (length > 32768)) {
    if (DEBUG_F_LENGTH) {
      fprintf (debug_file, "fix_len(%d)", len);

      for (i = 0; i < 12; i++) {
        if (i % 32 == 0)
          fprintf (debug_file, "\n%04X", i);
        fprintf (debug_file, " %02X", buf[i]);
      }

      for (i = 12; i < 256 + 12; i++) {
        if (i % 32 == 12)
          fprintf (debug_file, "\n%04X", i);
        fprintf (debug_file, " %02X", buf[i]);
      }

      for (i = 256 + 12; i < 256 + 12 + 32; i++) {
        if (i % 32 == 12)
          fprintf (debug_file, "\n%04X", i);
        fprintf (debug_file, " %02X", buf[i]);
      }

      fprintf (debug_file, "\n");
    }
    length = 12 +                       /* f_length, record_type, status */
      256 +                             /* ancillary data (chdo)         */
      12 +                              /* mp status */
      4096;                             /* typical WBR record */
    return 0;
  }

  memset (&buf[length], 0, 8);
  result = length + 3;                  /* correct buffer lengths */
  result &= 0x0FFFC;                    /* ALWAYS leave multiple of 32 bits */
  buf[result + 0] = (result >> 24) & 0xFF;
  buf[result + 1] = (result >> 16) & 0xFF;
  buf[result + 2] = (result >> 8) & 0xFF;
  buf[result + 3] = (result >> 0) & 0xFF;
  return result;
}

char *make_path (char *buffer, char type)
{
  static char buf[1024];
  char *temp;

  strcpy (buf, buffer);
  temp = strchr (buf, '.');
  if (temp) {
    temp[1] = type;
  }
  return buf;
}

 /****************************************************************/
  /*
   *    Process 1 input file
   */
int main_file (FILE * input, int write_flag)
{
  FILE *output = stdout;

  /**/ FILE * output_lp = NULL;
  /**/ FILE * output_hfr = NULL;
  /**/ FILE * output_stpk1 = NULL;
  /**/ FILE * output_stim = NULL;
  /**/ FILE * output_stdout = stdout;
  /**/ FILE * filerc = NULL;
  int ilen;
  int klen;
  bool HFR_Valid;
  int icnt = 0, jcnt = 0;
  int eof_flag = UTIL_GET_NON_BLOCKING;
  int epoch;
  char old_name[256] = { "" };
  char *new_name = { "" };
  char *new_name_lp = { "" };
  char *new_name_hfr = { "" };
  char *new_name_stpk1 = { "" };
  char *new_name_stim = { "" };
  char *filerc_name = { "filerc" };
  union
  {
    char *char_temp;
    struct RPWS_buffer *RPWS_temp;
  } tmp;

  mssg = stderr;
  debug_file = stdout;

  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "main_file\n");

  ilen = 0;
  if (sciop_flag) {
    while (!ilen) {
      ilen = util_getbuffer_RPWS (buffer, input, eof_flag);
    }
  } else {
    ilen = util_getbuffer_RPWS (buffer, input, eof_flag);
  }

  buffer->f_length = fix_len ((unsigned char *) buffer, buffer->f_length);

  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "  main_file: %d = UTIL_getbuffer_RPWS()\n", ilen);

  while (ilen > 0) {
    epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
      (buffer->packet.cds_tag.epoch[1] << 16) |
      (buffer->packet.cds_tag.epoch[2] << 8) |
      (buffer->packet.cds_tag.epoch[3] << 0);
    if (1) {
      icnt += 1;
      jcnt += 1;
      if (debug_flag & DEBUG_NW_NAME) {
        fprintf (debug_file, "call new_file_name(%d) "
                 "epoch:%02X%02X%02X%02X  "
                 "begin:%02X%02X%02X%02X  "
                 "end:%02X%02X%02X%02X  "
                 "%02X %02X %02X %02X %02X %02X %02X %02X  "
                 "%d\n",
                 __LINE__,
                 buffer->packet.cds_tag.epoch[0],
                 buffer->packet.cds_tag.epoch[1],
                 buffer->packet.cds_tag.epoch[2],
                 buffer->packet.cds_tag.epoch[3],
                 buffer->packet.cds_tag.begin[0],
                 buffer->packet.cds_tag.begin[1],
                 buffer->packet.cds_tag.begin[2],
                 buffer->packet.cds_tag.begin[3],
                 buffer->packet.cds_tag.end[0],
                 buffer->packet.cds_tag.end[1],
                 buffer->packet.cds_tag.end[2],
                 buffer->packet.cds_tag.end[3],
                 buffer->packet.mpp.mini_packet[0],
                 buffer->packet.mpp.mini_packet[1],
                 buffer->packet.mpp.mini_packet[2],
                 buffer->packet.mpp.mini_packet[3],
                 buffer->packet.mpp.mini_packet[4],
                 buffer->packet.mpp.mini_packet[5],
                 buffer->packet.mpp.mini_packet[6],
                 buffer->packet.mpp.mini_packet[7], 0);
        fflush (debug_file);
      }
      new_name = new_file_name (buffer, 0, 0);
      new_name_lp = new_file_name (buffer, Langmuir_Probe_Data, 0);
      new_name_hfr = new_file_name (buffer, HFR_Data, 0);
      new_name_stpk1 = new_file_name (buffer, HFR_Data, 1);
      new_name_stim = new_file_name (buffer, Stim_Data, 0);
      if (new_name[0])
        if (strcmp (old_name, new_name)) {
          strcpy (old_name, new_name);  /* mark file as "active" */
          if (!Kstdout)                 /* Do close ONLY if */
            if (output != stdout) {     /* needed.  NOT needed *//* when using stdout */
              fclose (output);          /*  */
              output = NULL;            /*  */
              if (output_lp) {
                fclose (output_lp);     /*  */
                output_lp = NULL;       /*  */
              }
              if (output_hfr) {
                fclose (output_hfr);    /*  */
                output_hfr = NULL;      /*  */
              }
              if (output_stpk1) {
                fclose (output_stpk1);  /*  */
                output_stpk1 = NULL;    /*  */
              }
              if (output_stim) {
                fclose (output_stim);   /*  */
                output_stim = NULL;     /*  */
              }
            }
          /*
           */
          if (Kfile_list)
            if (message_status_flag)
              fprintf (mssg, "  %d records \n", jcnt);
          /**/ if (write_flag)
            if (!Kstdout) {
              output = fopen (new_name, "ab");
              /*
               * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", new_name, "ab"); /*
               */
              if (filerc_flag) {
                filerc = fopen (filerc_name, "w");
                /*
                 * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", filerc_name, "w"); /*
                 */
              }
              if (filerc) {
                fprintf (filerc, "PATH=%s\n", getcwd (NULL, 256));
                fprintf (filerc, "RAW=%s\n", make_path (new_name, 'r'));
                fprintf (filerc, "MPUS=%s\n", make_path (new_name, 'u'));
              }
              if (Split_flag) {
                output_lp = fopen (new_name_lp, "ab");
                output_hfr = fopen (new_name_hfr, "ab");
                output_stim = fopen (new_name_stim, "ab");
                /*
                 * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", new_name_lp, "ab"); /*
                 */
                /*
                 * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", new_name_hfr, "ab"); /*
                 */
                /*
                 * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", new_name_stim, "ab"); /*
                 */
                if (stpk1_flag) {
                  output_stpk1 = fopen (new_name_stpk1, "ab");
                  /*
                   * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", new_name_stpk1, "ab"); /*
                   */
                }
              }
              if (filerc) {
                fclose (filerc);
                filerc = NULL;
              }
            }
          if (Kfile_list)
            if (message_status_flag)
              fprintf (mssg, "        file:%s ", new_name);
          /**/ jcnt = 0;
        }
      /*
       *    PAD the record out to keep f_length/r_length at the start/end
       *    of a 16 byte line.  Makes reading dumps a little easier as the
       *    f_length always appears at the begining of a line...
       */
      if (Pad_flag) {
        tmp.RPWS_temp = buffer;
        tmp.char_temp[buffer->f_length + 0] = 0;        /* remove the */
        tmp.char_temp[buffer->f_length + 1] = 0;        /* old length */
        tmp.char_temp[buffer->f_length + 2] = 0;        /* indicator  */
        tmp.char_temp[buffer->f_length + 3] = 0;        /* (r_length) */

        buffer->f_length += 15;         /* 15 rounds to the end of line */
        buffer->f_length += 4;          /* add 4 for the r_length      */
        buffer->f_length &= 0x3FFF0;    /* round down to full line     */
        buffer->f_length -= 4;          /* remove 4 for r_length at end */
      }
      /*
       */
      if (write_flag) {
        /*
         * fprintf(stderr,"write record\n"); /*
         */
        if (addstdout) {                /* output to stdout */
          UTIL_putbuffr2_RPWS (buffer, output_stdout, buffer->f_length);
        }

/****************Langmuir Probe ***********************/

        if (output_lp) {
          if ((buffer->record_type & 0x0FF) == PACKET_TYPE_lp) {
            UTIL_putbuffr2_RPWS (buffer, output_lp, buffer->f_length);
          /**/}
          if ((buffer->record_type & 0x0FF) == PACKET_TYPE_stim)
            if (UTIL_MP_length ((struct MP_buffer *) buffer))
              UTIL_putbuffr2_RPWS (buffer, output_lp, buffer->f_length);
        /**/}

/*************** 1st. produce the "H" file (NO Decompress applied) *****************/
        if (output_hfr) {
          if ((buffer->record_type & 0x0FF) == PACKET_TYPE_hfr) {
            UTIL_putbuffr2_RPWS (buffer, output_hfr, buffer->f_length); /* COMPRESSED HFR Record */
          }
          if ((buffer->record_type & 0x0FF) == PACKET_TYPE_stim)
            if (UTIL_MP_length ((struct MP_buffer *) buffer))
              UTIL_putbuffr2_RPWS (buffer, output_hfr, buffer->f_length);       /* STIM record */
        }


/*************** 1st. clone & decompress the HFR buffer *****************/

        memcpy (buffer1, buffer, buffer->f_length + 4); /* buf1 <- buf */
        if ((buffer->record_type & 0x0FF) == PACKET_TYPE_hfr) {
          if (debug_flag & DEBUG_RAJ) {

                         /*************************************************************************/
            /*
             * shit1 = CasHfr_bHfrPacket(buffer1->packet.mpp.mini_packet);           
             */
            /*
             * shit2 = CasHfr_bMeanderCompressed(buffer1->packet.mpp.mini_packet);   
             */
            /*
             * shit3 = CasHfr_bValidPacket(buffer1);                                 
             */
            /*
             * fprintf(stdout, "We see HFR Packet %d %d %d\n", shit1, shit2, shit3); 
             */
            /*
             * fflush(stdout);                                                       
             */

                         /*************************************************************************/
          }
          HFR_Valid = CasHfr_bAnalysis (buffer1->packet.mpp.mini_packet);

                     /***********************************************************************************/
          /*
           * if(     (CasHfr_bHfrPacket(buffer1->packet.mpp.mini_packet) == true) &&         
           */
          /*
           * (CasHfr_bMeanderCompressed(buffer1->packet.mpp.mini_packet) == true)  &&
           */
          /*
           * (shit3 == true) )                                                       
           */

                     /***********************************************************************************/
          if (1) {
            if (debug_flag & DEBUG_RAJ) {
              a_dump (stdout, "buffer before", (unsigned char *) buffer);
              fflush (stdout);
            }
            CasHfr_Meander ((CasRecord *) buffer1, (CasRecord *) buffer);       /* buf is now decompressed */

            buffer->f_length =
              fix_len ((unsigned char *) buffer, buffer->f_length);
            if (debug_flag & DEBUG_RAJ) {
              b_dump (stdout, "buffer after ", (unsigned char *) buffer,
                      (unsigned char *) buffer1);
              fprintf (stdout,
                       "==========================================================================\n");
              fflush (stdout);
            }
          }
        }

/*************** Special French Format for HFR data ********************/

/* Hey Terry, here's where "K" files are written,
   but I suspect that CasHfr_RecordToKronos isn't
   returning a non-zero length for millisecond mode
   data, and that is required for it to be written  */

        if (output_stpk1)
          /**/ {
          if (((buffer->record_type & 0x0FF) == PACKET_TYPE_hfr)
              && hfrcal_flag) {
            if ((buffer1->packet.mpp.mini_packet[7] & 0x60) == 0x40)    /* snoop for Calibration packet */
              HFR_Valid = true;
          }

/* TFA */
          if ((buffer->record_type & 0x0FF) == PACKET_TYPE_hfr) {
            if ((buffer1->packet.mpp.mini_packet[7] & 0x60) == 0x60)    /* snoop for MS mode packet */
              HFR_Valid = true;
          }

/* TFA */
          if (((buffer->record_type & 0x0FF) == PACKET_TYPE_hfr) &&
              (HFR_Valid == true)
            ) {
            klen = CasHfr_RecordToKronos ((CasRecord *) buffer,
                                          kbuffer, false);
            if (fix_time_flag)
              fix_time (buffer1, kbuffer);      /* look at time in untouched buffer */
            if (klen)
              fwrite (kbuffer, klen, 1, output_stpk1);  /* BUT!!!  write decompressed data */
          }
          }


/**************** STIM file ****************************/

        if (output_stim) {
          if ((buffer->record_type & 0x0FF) == PACKET_TYPE_stim)
            if (UTIL_MP_length ((struct MP_buffer *) buffer))
              UTIL_putbuffr2_RPWS (buffer, output_stim, buffer->f_length);
        /**/}

/*************** HFR is uncompressed by now ********************/

        UTIL_putbuffr2_RPWS (buffer, output, buffer->f_length);
        /**/ if (debug_flag & DEBUG_PROFILE)
          fprintf (debug_file, "    UTIL_putbuffr2_RPWS\n");
      }
    }

/************  Read next record & do timeout (for areal-time ops) *****/
    /*
     * fprintf(stderr,"next record\n"); /*
     */
    ilen = 0;
    if (sciop_flag)                     /* real-time feed, no data arriving */
      while (!ilen) {                   /*   so we'll close output for now *//*   more data will reopen it */
        ilen = util_getbuffer_RPWS (buffer, input, eof_flag);
        if (!ilen) {                    /* TIMEOUT ****** TIMEOUT ****** TIMEOUT */
          if (output) {
            fclose (output);            /*  */
            output = NULL;              /*  */
          }
          if (output_lp) {
            fclose (output_lp);         /*  */
            output_lp = NULL;           /*  */
          }
          if (output_hfr) {
            fclose (output_hfr);        /*  */
            output_hfr = NULL;          /*  */
          }
          if (output_stpk1) {
            fclose (output_stpk1);      /*  */
            output_stpk1 = NULL;        /*  */
          }
          if (output_stim) {
            fclose (output_stim);       /*  */
            output_stim = NULL;         /*  */
          }
        }                               /* if !ilen */
      } /* while !ilen */
    else                                /* if sciop_flag */
      ilen = util_getbuffer_RPWS (buffer, input, eof_flag);
    buffer->f_length = fix_len ((unsigned char *) buffer, buffer->f_length);

  }                     /** did this get lost 6/21/00   **/
  if (Kfile_list)
    if (message_status_flag)
      fprintf (mssg, "  %d records , %d total\n", jcnt, icnt);
  /**/ if (!Kstdout)
    if (output) {
      fclose (output);
      output = NULL;
      old_name[0] = 0;
      if (output_lp) {
        fclose (output_lp);
        output_lp = NULL;
      }
      if (output_hfr) {
        fclose (output_hfr);
        output_hfr = NULL;
      }
      if (output_stpk1) {
        fclose (output_stpk1);
        output_stpk1 = NULL;
      }
      if (output_stim) {
        fclose (output_stim);
        output_stim = NULL;
      }
    }
  return (0);
}
char *cleanup (char *buf)
{
  int len = strlen (buf);
  int i;

  for (i = len - 1; i > 0; i--) {
    if (buf[i] > 0x20 && buf[i] < 0x7F)
      break;
    buf[i] = 0x00;
  }
  /*
   * fprintf(stderr, "Cleanup %s\n", buf); /*
   */
  return buf;
}

 /****************************************************************/
  /*
   *    Process list of files
   */
#define FNL 128
int main_file_list (int write_flag, char *list_file)
{
  FILE *list = NULL;
  FILE *input = NULL;
  int icnt;
  char filename[128];
  char *fp;

  main_file_list_flag = 1;
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "main_file_list\n");

  list = fopen (list_file, "r");
  /*
   * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", list_file, "r"); /*
   */
  if (!list)
    return 0;
  while (1) {
    fp = fgets (filename, FNL - 1, list);
    cleanup (filename);
    if (debug_flag & DEBUG_NAMES)
      fprintf (debug_file, "     from file list (%s)\n", filename);

    if (!fp)
      break;
    if (message_status_flag)
      fprintf (mssg, "Processing [%s]   ", filename);
    if (!Kfile_list)
      if (message_status_flag)
        fprintf (mssg, "\n");
    input = fopen (filename, "rb");
    /*
     * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", filename, "rb"); /*
     */
    if (input) {
      main_file (input, write_flag);
      icnt += 1;
      fclose (input);
    } else {
      if (message_status_flag)
        fprintf (mssg, "UNSUCCESSFUL because (%s)\n", strerror (errno));
    }
  }
  return icnt;
}

 /****************************************************************/
int main (int argc, char *argv[])
{
  FILE *input = stdin;
  FILE *stsfile = stderr;
  int write_flag = 1;
  char fname[128];
  int icnt;
  int buffer_size = BUFFER_SIZE;

  fg_flags (argc, argv);
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stdout, "\n");
    fprintf (stdout, "%s %s   HELP SCREEN\n", title, Ver);
    fprintf (stdout, "                   %s\n", sCasHfrMeanderVersion);
    fprintf (stdout, "  This utility requires CHDO records !!!!!\n");
    fprintf (stdout,
             "  This utility accepts RAW data, UNsegmented mini-packets\n");
    fprintf (stdout,
             "    or segmented mini-packets and produces output files\n");
    fprintf (stdout,
             "    based on the time of the data.  This is intended to\n");
    fprintf (stdout,
             "    allow the data query to be performed on a long time\n");
    fprintf (stdout,
             "    period while allowing data to be stored in manageable\n");
    fprintf (stdout, "    files.\n");
    fprintf (stdout, "  In the case of mini-packet data, there will be no\n");
    fprintf (stdout, "    packet loss at the begining of each file.  In\n");
    fprintf (stdout,
             "    addition, each minipacket file will be time-aligned\n");
    fprintf (stdout,
             "    (i.e. the data in the file is placed in the file\n");
    fprintf (stdout,
             "    based on the time in the minipacket, not the CDS \n");
    fprintf (stdout,
             "    timestamp which is normally after the data timestamp\n");
    fprintf (stdout,
             "  Note that the first housekeeping record followinga reset\n");
    fprintf (stdout,
             "    or software load usually has zero in the SCLK field.\n");
    fprintf (stdout, "    this record will be placed into a filed called \n");
    fprintf (stdout,
             "    't19580000.r00'.  The record contains all zeros.\n");
    fprintf (stdout,
             "  It is possible to process multiple files with this program.\n");
    fprintf (stdout,
             "    the RAW data files tend to be scattered and better coverage\n");
    fprintf (stdout, "    is achieved by gathering all files.\n");
    fprintf (stdout, "    \n");
    fprintf (stdout,
             "  For example, to process a number of RAW files (from CD):\n");
    fprintf (stdout, "    cd /opt/project/cassini/atlo\n");
    fprintf (stdout, "    ls -1 /mnt/drwr? > drwr?\n");
    fprintf (stdout, "    rpws_file -files drwr?\n");
    fprintf (stdout,
             "    rpws_file -files drwr? -stdout | mpii | decomp | mpus | usdc | rpws_file\n");
    fprintf (stdout, "\n");
    fprintf (stdout,
             "  The '-strip' flag allows zero length records to be eliminated\n");
    fprintf (stdout,
             "    from the output files.  This is intended for use when producing\n");
    fprintf (stdout, "    mini-packet files (i.e. u00/h00/l00 files)\n");
    fprintf (stdout, "    \n");
    fprintf (stdout, "\n");
    fprintf (stdout,
             "  The '-sciop' flag is used for background operation on raw (.r00) data\n");
    fprintf (stdout,
             "      when data does not arrive in a timely manner, the output\n");
    fprintf (stdout,
             "      file is closed after about %d seconds of inactivity\n",
             SCIOP_TIMEOUT);
    fprintf (stdout, "          -size 1440 may be used for 1 day files\n");
    fprintf (stdout, "    \n");
    fprintf (stdout, "\n");
    fprintf (stdout, "    Default buffer size %d\n", buffer_size);
    fprintf (stdout, "\n");
    fprintf (stdout,
             "    -nopad    supress padding to next 16 byte boundary\n");
    fprintf (stdout, "    -size mm  file size in minutes\n");
    fprintf (stdout, "                  default is %d\n", K60);
    fprintf (stdout, "    -style n  file name style\n");
    fprintf (stdout, "                  0 tYYYYDDDHHMM\n");
    fprintf (stdout, "                  1 tYYYYDDDMMMM\n");
    fprintf (stdout, "    -files xx file containing a list of\n");
    fprintf (stdout, "                files to process.\n");
    fprintf (stdout, "    -nodupe   supress duplicate CDS records.\n");
    fprintf (stdout, "                (seems to happen occasionally)\n");
    fprintf (stdout, "    -reg xx   supress time regressions greater than\n");
    fprintf (stdout, "                (use to fix bad append)\n");
    fprintf (stdout, "    -nolist   supress listing filenames as they\n");
    fprintf (stdout, "                are created.\n");
    fprintf (stdout, "    -stdout   route directly to stdout.\n");
    fprintf (stdout, "                (use this to gather RAW files\n");
    fprintf (stdout, "                   or decomp|mpus|usdc)\n");
    fprintf (stdout, "    -addstdout\n");
    fprintf (stdout,
             "              route to stdout (in addition to making files)\n");
    fprintf (stdout,
             "    -sciop    background connection from sciop_ at JPL\n");
    fprintf (stdout, "    -nomsg    Suppress status messages\n");
    fprintf (stdout,
             "    -strip    strip zero-length records (for u files)\n");
    fprintf (stdout, "    -test     test run, generate filenames only\n");
    fprintf (stdout,
             "    +chdo     include mode dubug garbage in dbase file\n");
    fprintf (stdout, "    +rti      include RTI number in dbase file\n");
    fprintf (stdout, "    -rti      exclude RTI number in dbase file\n");
    fprintf (stdout, "    -epoch_flag\n");
    fprintf (stdout, "              change time processing \n");
    fprintf (stdout, "    -bench_flag\n");
    fprintf (stdout,
             "              change time processing for bench operations\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "    -buffer_size nn\n");
    fprintf (stdout, "              alter the packet buffer \n");
    fprintf (stdout, "                memory allocation\n");
    fprintf (stdout, "    -dbase xx file containing a list of\n");
    fprintf (stdout, "                files processed \n");
    fprintf (stdout, "    -fred xx  same as -dbase\n");
    fprintf (stdout, "    -path xx  hack in the directory path into the\n");
    fprintf (stdout, "                database file\n");
    fprintf (stdout, "    -debug n  debugging flags\n");
    fprintf (stdout, "                Bit 0  profiling\n");
    fprintf (stdout, "                Bit 3  -file names\n");
    fprintf (stdout, "                Bit 8  database file internals\n");
    fprintf (stdout, "                Bit 9  Meander messaging\n");
    fprintf (stdout, "                Bit 15 goofy packet f_length dump\n");
    fprintf (stdout, "    -split\n");
    fprintf (stdout, "                Split HFR and LP data into\n");
    fprintf (stdout, "                separate files.  This will create\n");
    fprintf (stdout,
             "                empty files when used with raw data.\n");
    fprintf (stdout,
             "                STIM packets are included in these files.\n");
    fprintf (stdout, "                V2.3 splits STIM packets.\n");
    fprintf (stdout, "    -filerc\n");
    fprintf (stdout, "                Create filerc\n");
    fprintf (stdout, "  These two options rely on mp/mpii to filter\n");
    fprintf (stdout, "            LP or HFR packets.\n");
    fprintf (stdout, "    -lp\n");
    fprintf (stdout, "                Use this flag to generate\n");
    fprintf (stdout, "                Langmuir Probe files\n");
    fprintf (stdout, "                \n");
    fprintf (stdout, "    -hfr\n");
    fprintf (stdout, "                Use this flag to generate\n");
    fprintf (stdout, "                HFR files\n");
    fprintf (stdout, "    -stpk1\n");
    fprintf (stdout, "                HFR files in the special format\n");
    fprintf (stdout, "    -meander\n");
    fprintf (stdout,
             "                Supress meander decompress for HFR files\n");
    fprintf (stdout, "                \n");
    fprintf (stdout, "    +hfrcal\n");
    fprintf (stdout,
             "                Force HFR Calibration packets into K file\n");
    fprintf (stdout, "                \n");
    fprintf (stdout, "    -fix\n");
    fprintf (stdout, "                Supress K file time fix\n");
    fprintf (stdout, "    -stim\n");
    fprintf (stdout, "                Use this flag to generate\n");
    fprintf (stdout, "                Stim packet files\n");
    fprintf (stdout, "                \n");
    fprintf (stdout, "                \n");
    exit (0);
  }

  debug_flag = fg_int ("debug", 0);

	if (debug_flag & DEBUG_MEANDER) {
		/*
		 * bMeanderDebug = 1;  /*
		 */
		/*
		 * bMeanderHeader = 1; /*
		*/

		/* hCasHfrErr = stderr;	/**/
		if (debug_flag & DEBUG_RAJ){
			nMeanderDebug = 2;                
			hCasHfrMeanderErr = stdout; 
		}
		else{
			nMeanderDebug = 1;                
			hCasHfrMeanderErr = stderr;
		}
	} 
	else {
		nMeanderDebug = 0;
    /*
     * bMeanderDebug = 0;  /*
     */
    /*
     * bMeanderHeader = 0; /*
     */

/*        hCasHfrErr = NULL; */
		hCasHfrMeanderErr = NULL;
  }

  K60 = fg_int ("size", K60);
  regression_flag = fg_int ("reg", regression_flag);
  Kstyle = fg_int ("style", Kstyle);
  if (fg_flag ("path") == '-') {
    strcpy (path, fg_flagc ("path"));
  }
  if (fg_flag ("filerc") == '-') {
    filerc_flag = 1;
  }
  if (fg_flag ("nopad") == '-') {
    Pad_flag = 0;
  }
  if (fg_flag ("nomsg") == '-') {
    message_status_flag = 0;
  }
  if (fg_flag ("split") == '-') {
    Split_flag = 1;
  }
  if (fg_flag ("stpk1") == '-') {
    stpk1_flag = 1;
  }
  if (fg_flag ("sciop") == '-') {
    sciop_flag = 1;
    Kfile_list = 0;
    fprintf (stderr, "%s %s\n", title, Ver);
  }
  if (fg_flag ("meander") == '-') {
    meander_flag = 0;
  }
  if (fg_flag ("hfrcal") == '+') {
    hfrcal_flag = 1;
  }
  if (fg_flag ("fix") == '-') {
    fix_time_flag = 0;
  }
  if (fg_flag ("rti") == '-') {
    rti_flag = 0;
  }
  if (fg_flag ("epoch_flag") == '-') {
    epoch_flag = 1;
  }
  if (fg_flag ("bench_flag") == '-') {
    bench_flag = 1;
  }
  if (fg_flag ("strip_flag") == '-') {
    strip_flag = 0;
  }
  if (fg_flag ("rti") == '+') {
    rti_flag = 1;
  }
  if (fg_flag ("chdo") == '+') {
    chdo_flag = 1;
  }
  if (fg_flag ("lp") == '-') {
    Unsegmented_Data = Langmuir_Probe_Data;
    Segmented_Data = Langmuir_Probe_Data;
  }
  if (fg_flag ("hfr") == '-') {
    Unsegmented_Data = HFR_Data;
    Segmented_Data = HFR_Data;
  }
  if (fg_flag ("stim") == '-') {
    Unsegmented_Data = Stim_Data;
    Segmented_Data = Stim_Data;
  }
  if (fg_flag ("test")) {
    write_flag = 0;
    mssg = stdout;
  }
  if (fg_flag ("stdout")) {
    Kstdout = 1;
    mssg = stderr;
    debug_file = stderr;
  }
  if (fg_flag ("addstdout")) {
    addstdout = 1;
    mssg = stderr;
    debug_file = stderr;
  }
  if (fg_flag ("nodupe")) {
    nodupe_flag = 1;
  }
  if (fg_flag ("nolist")) {
    Kfile_list = 0;
  }
  buffer_size = fg_int ("buffer_size", buffer_size);
  /**/ buffer = malloc (buffer_size);
  /**/ bufferd = malloc (buffer_size);
  /**/ buffer1 = malloc (buffer_size);
  /**/ if (fg_flag ("getmp") == '+') {
    strcpy (fname, UTIL_filename (FILE_MP, FILE_LOCATE_DEFAULT));
    fprintf (mssg, "file source: %s\n", fname);
    if (!fname)
      exit (0);
    input = fopen (fname, "rb");
    /*
     * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", fname, "rb"); /*
     */
  }
  if (fg_flag ("getcds") == '+') {
    strcpy (fname, UTIL_filename (FILE_RAW, FILE_LOCATE_DEFAULT));
    fprintf (mssg, "file source: %s\n", fname);
    if (!fname)
      exit (0);
    input = fopen (fname, "rb");
    /*
     * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", fname, "rb"); /*
     */
  }
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "main\n");

  if (fg_flag ("files") == '-') {
    main_file_list (write_flag, fg_flagc ("files"));
  } else {
    icnt = main_file (input, write_flag);
    fclose (input);
  }

  if (fg_flag ("dbase") == '-') {
    stsfile = fopen (fg_flagc ("dbase"), "w");
    /*
     * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", fg_flagc("dbase"), "w"); /*
     */
    status_out (stsfile);
    fclose (stsfile);
  } else if (fg_flag ("fred") == '-') {
    stsfile = fopen (fg_flagc ("fred"), "w");
    /*
     * fprintf(stderr, "OPEN output = fopen(\"%s\",\"%s\");\n", fg_flagc("dbase"), "w"); /*
     */
    status_out (stsfile);
    fclose (stsfile);
  }
  status_out (stsfile);
  if (debug_flag & DEBUG_PROFILE)
    fprintf (debug_file, "main ---\n");
  if (!sciop_flag)
    fprintf (mssg, "END OF PROCESSING %d files\n", icnt);

  return 0;
}
