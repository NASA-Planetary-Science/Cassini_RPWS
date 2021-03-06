
/*
 * hkrom.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <term.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <strings.h>
#include <string.h>

/* Cassini Stuff */
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <fg.h>

#ifdef _RPWS_
#include "rpws_au.h"
#endif

int eng_3 (struct CDS_buffer *buffer, int v_current);

struct LIMIT
{
  float red_lower;
  float red_upper;
  float yellow_lower;
  float yellow_upper;
};

struct CDS_buffer *buffer = NULL;
struct CDS_buffer mro_buffer;
struct Ancillary_packet ancil;
struct BIU_status_packet biust;

static char *Version = { "CASSINI RPWS housekeeping dump (HKROM14) V14.1" };
int bell_flag = 0;
int format_flag = 1;
int display_flag = 1;
int upkt_flag = 0;
int stdout_flag = 0;
int recent_flag = 0;
int virtual_channel_flag = 0;
int flag_bench = 0;
int size = 0;
static int epoch;

int ancil_count = 0;
int biust_count = 0;
FILE *msgs;
int recentF;
int recentF0;
int recentF1;
int logno = 0;
static int cnv = 0;

static mode_t mode = S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR | S_IWGRP;

#define HSK_p_l 1
struct p_l
{
  int length;
  int flag;
} packet_length[] = {
  -1, 0,                                /* dummy (not used) */
    96, 0,                              /* housekeeping */
    476, 1,                             /* new ISS value */
    545, 1,                             /* old ISS vbalue */
0, 0, 0, 0};                            /* terminating value */
static char temp[1024];

enum
{ FLAG_NORMAL = 0,
  FLAG_GREEN = 1,
  FLAG_YELLOW = 2,
  FLAG_RED = 3
};

int mv_printw (int vert, int hor, char *stg, int color)
{
  static int vertical = 99;
  static int horizontal = 0;
  static struct ATTR_PAIR
  {
    int color;
    int attribute;
  } attribute_pair[] = {
  0, 0,
      COLOR_GREEN, A_UNDERLINE,
      COLOR_YELLOW, A_BOLD,
      COLOR_RED, A_REVERSE, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  int spaces;
  int i;

  if (!display_flag)
    return vert;
  if (format_flag) {
    if (attribute_pair[color & 7].color) {
      if (!flag_bench)
        fprintf (stderr, "\a");
      attron (attribute_pair[color & 7].attribute);
      attron (COLOR_PAIR (attribute_pair[color & 7].color));
      /*
       * attrset(COLOR_PAIR(attribute_pair[color&7].color) | attribute_pair[color&7].attribute); /*
       */
    }
    mvprintw (vert, hor, "%s", stg);
    if (attribute_pair[color & 7].color) {
      attroff (COLOR_PAIR (attribute_pair[color & 7].color));
      attroff (attribute_pair[color & 7].attribute);
    }

    if (vert != vertical)
      clrtoeol ();
  } else {
    if (vert != vertical) {
      fputs ("\n", stdout);
      horizontal = 0;
    }
    spaces = hor - horizontal;
    horizontal = hor + strlen (stg);
    if (spaces > 0) {
      for (i = 0; i < spaces; i++)
        fputc (' ', stdout);
    }
    fputs (stg, stdout);
  }
  vertical = vert;
  return vert + 1;
}
char *reason (char i)
{
  static char *rs;
  static char rsn[128];

  sprintf (rsn, "0x%2.2X unknown failure  ", i);
  rs = rsn;
  switch (i) {
   case 'B':
     rs = "B attempting to boot  ";
     break;
   case 'C':
     rs = "C word count (not 22) ";
     break;
   case 'L':
     rs = "L word count (> 256)  ";
     break;
   case 's':
     rs = "s sequence word       ";
     break;
   case 'c':
     rs = "c checksum            ";
     break;
   case 'i':
     rs = "i ALF id (not 2000)   ";
     break;
   case 'p':
     rs = "p ALF_EOF packet count";
     break;
   case '1':
     rs = "1 boot checksum       ";
     break;
   case '2':
     rs = "2 boot id             ";
     break;
   case 0:
     rs = "0x00 nominal          ";
     break;
   default:
     break;
  }
  return rs;
}
int play (char *text)
{
  int istat = 0;

#ifdef _RPWS_
  static char *path = { "/usr/cassini/audio/" };
  static char *type = { ".wav" };
  static char filename[128];

  if (bell_flag) {
    strcpy (filename, path);
    strcat (filename, text);
    strcat (filename, type);
    istat = uplay_file (filename);
  }
#endif

  return istat;
}
int biust_disp (int v_current)
{
  int vert;

  vert = v_current;
  if (!biust_count)
    return vert;
  sprintf (temp, " -------------- BIU discrete/Status --------------- %d ",
           biust_count);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp, " %2.2X%2.2X %2.2X%2.2X   %2.2X%2.2X   %2.2X%2.2X  ",
           biust.data[1], biust.data[0], biust.data[3], biust.data[2],
           biust.data[5], biust.data[4], biust.data[7], biust.data[6]
    );
  vert = mv_printw (vert, 0, temp, 0);
  return vert;
}
int ancil_disp (int v_current)
{
  int vert;

  vert = v_current;
  if (!ancil_count)
    return vert;
  sprintf (temp, " ---------------- Ancillary Data ------------------ %d ",
           ancil_count);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp, " %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X   [%4.4X] ",
           ancil.data[1], ancil.data[0], ancil.data[3], ancil.data[2],
           ancil.data[5], ancil.data[4], ancil.data[7], ancil.data[6],
           (unsigned int) (ancil.data[3] * 256 + ancil.data[2]) >> 3);
  vert = mv_printw (vert, 0, temp, 0);
  return vert;
}
int command_disp (struct CDS_buffer *buffer, int v_current)
{
  char *mask_id = "Step";
  char *checksum = "OK ";
  char *Load_sts = "No IEB Load      ";
  char *Load_sts2[] = { "IEB Mem Empty    ",
    "IEB Mem Load Fail",
    "IEB Mem Loaded   ",
    "IEB Mem Loaded(F)"
  };
  char *record = "";
  static int invalid = -1;
  static int loop = -1;
  int vert;
  int i;
  int flag = FLAG_NORMAL;

  vert = v_current;
  sprintf (temp, " ---------------- Command Monitors ------------------   ");
  vert = mv_printw (vert, 0, temp, FLAG_NORMAL);
  sprintf (temp, " Cmd Count [%d]  Valid [%d]  Invalid[%d] Loop[%d]     ",
           UTIL_short_to_MSB (buffer->packet.rom_housekeeping.
                              Command_Byte_Count),
           buffer->packet.rom_housekeeping.Valid_Command_Count[0],
           buffer->packet.rom_housekeeping.Invalid_Command_Count[0],
           buffer->packet.rom_housekeeping.Command_Loop_Count[0]);
  if (buffer->packet.rom_housekeeping.Invalid_Command_Count[0])
    flag = FLAG_YELLOW;
  vert = mv_printw (vert, 0, temp, flag);
  if (loop == -1)
    loop = buffer->packet.rom_housekeeping.Command_Loop_Count[0];
  if (loop != buffer->packet.rom_housekeeping.Command_Loop_Count[0]) {
    loop = buffer->packet.rom_housekeeping.Command_Loop_Count[0];
    play ("looper");
  }
  if (invalid == -1)
    invalid = buffer->packet.rom_housekeeping.Invalid_Command_Count[0];
  if (invalid != buffer->packet.rom_housekeeping.Invalid_Command_Count[0]) {
    invalid = buffer->packet.rom_housekeeping.Invalid_Command_Count[0];
    play ("invalid");
  }

  flag = FLAG_NORMAL;
  if (0x00 == buffer->packet.rom_housekeeping.Time_Tag[1])
    mask_id = "ID  ";
  if (0xFF == buffer->packet.rom_housekeeping.Time_Tag[1])
    mask_id = "MASK";
  if (0) {                              /* oroginal IEB status */
    if (buffer->packet.rom_housekeeping.LRP_analog[5])
      record = "BAD IEB RECORDS";
    if (buffer->packet.rom_housekeeping.LRP_analog[5] == 1)
      record = "BAD IEB RECORD";
    if (0x80 & buffer->packet.rom_housekeeping.LRP_analog[6])
      checksum = "BAD";
    if (0x7F & buffer->packet.rom_housekeeping.LRP_analog[6])
      Load_sts = "IEB Loading?  ";
    if (0x80 & buffer->packet.rom_housekeeping.LRP_analog[6]) {
      Load_sts = "IEB Load FAIL ";
      flag = FLAG_YELLOW;
    }
    sprintf (temp,
             " IEB %s [%02X %02X] %s [%02X %02X] CKS-%s %s                ",
             mask_id, buffer->packet.rom_housekeeping.Time_Tag[0],
             buffer->packet.rom_housekeeping.Time_Tag[1], Load_sts,
             buffer->packet.rom_housekeeping.LRP_analog[5],
             buffer->packet.rom_housekeeping.LRP_analog[6], checksum, record);
  } else {                              /* [5] has BAD shit [6] has GOOD shit */

    i = (buffer->packet.rom_housekeeping.LRP_analog[5] >> 7) & 0x01;
    i |= (buffer->packet.rom_housekeeping.LRP_analog[6] >> 6) & 0x02;
    sprintf (temp, " IEB %s [%02X %02X] Bad-%d[%d] Good-%d[%d] %s        ",
             mask_id,
             buffer->packet.rom_housekeeping.Time_Tag[0],
             buffer->packet.rom_housekeeping.Time_Tag[1],
             (buffer->packet.rom_housekeeping.LRP_analog[5] >> 7) & 0x01,
             buffer->packet.rom_housekeeping.LRP_analog[5] & 0x7F,
             (buffer->packet.rom_housekeeping.LRP_analog[6] >> 7) & 0x01,
             buffer->packet.rom_housekeeping.LRP_analog[6] & 0x7F,
             Load_sts2[i]);
  }
  if (0x15 == UTIL_extract_CDS_type (buffer))
    vert = mv_printw (vert, 0, temp, flag);
  return vert;
}
char *power_disp (int flags)
{
  static char result[48];

  result[0] = 0;

  if (flags & 0xF8)
    strcat (result, " HFR");
  if (flags & 0xE0)
    strcat (result, "/CMD");
  if (flags & 0x18)
    strcat (result, "/RST");
  if (flags & 0x04)
    strcat (result, " LP");
  if (flags & 0x02)
    strcat (result, " ME2");
  if (flags & 0x01)
    strcat (result, " HFR");
  strcat (result, "              ");
  result[26] = 0;
  return result;
}
char *p8155_disp (int flags)
{
  static char result[32];

  result[0] = 0;

  if (flags & 0x04)
    strcat (result, " DAC");
  else
    strcat (result, "    ");
  if (flags & 0x02)
    strcat (result, " ADC");
  else
    strcat (result, "    ");
  if (flags & 0x01)
    strcat (result, " PWR");
  strcat (result, "              ");
  result[14] = 0;
  return result;
}
char *sts_disp (int flags)
{
  static char result[64];

  result[0] = 0;

  if (flags & 0x80)
    strcat (result, " AFLT");
  if (flags & 0x40)
    strcat (result, " MAIN");
  if (flags & 0x20)
    strcat (result, " ANOR");
  if (flags & 0x10)
    strcat (result, " AMOT");
  switch (flags & 0x0C) {
   case 0x08:
     strcat (result, "  L-SLP");
     break;
   case 0x04:
     strcat (result, " HL-SLP");
     break;
   case 0x0C:
     strcat (result, "  H-SLP");
     break;
   case 0x00:
     strcat (result, " ACTIVE");
     break;
  }
  if (flags & 0x02)
    strcat (result, " HRST");
  if (flags & 0x01)
    strcat (result, " DRST");
  return result;
}
char *misc_disp (int flags)
{
  static char result[128];

  result[0] = 0;

  if (flags & 0x80)
    strcat (result, " WDT1");
  if (flags & 0x40)
    strcat (result, " WDT0");
  if (flags & 0x20)
    strcat (result, " RAME");
  if (flags & 0x10)
    strcat (result, " BIUL");
  if (flags & 0x08)
    strcat (result, " DTIM");
  if (flags & 0x04)
    strcat (result, " BCRTM");
  if (flags & 0x02)
    strcat (result, " WDTE");
  if (flags & 0x01)
    strcat (result, " WPV");
  strcat (result, "               ");
  strcat (result, "               ");
  strcat (result, "               ");
  result[64] = 0;
  return result;
}
char *antl_disp (int flags)
{
  static char result[32];

  result[0] = 0;

  if (!(flags & 0x20))
    strcat (result, " -ZR");
  if (!(flags & 0x10))
    strcat (result, " -ZE");
  if (!(flags & 0x08))
    strcat (result, " -XR");
  if (!(flags & 0x04))
    strcat (result, " -XE");
  if (!(flags & 0x02))
    strcat (result, " +XR");
  if (!(flags & 0x01))
    strcat (result, " +XE");
  return result;
}
int digital_disp (struct CDS_buffer *buffer, int v_current)
{
  int vert;
  int flag;

  vert = v_current;
  sprintf (temp, " ---------------- Digital Monitors ------------------   ");
  vert = mv_printw (vert, 0, temp, FLAG_NORMAL);
  sprintf (temp, " Ant sts (in C0) [%2.2X]%s  Pwr_Sts (out E8) [%2.2X]%s",
           buffer->packet.rom_housekeeping.Antenna_Status[0],
           antl_disp (buffer->packet.rom_housekeeping.Antenna_Status[0]),
           buffer->packet.rom_housekeeping.Power_Status[0],
           power_disp (buffer->packet.rom_housekeeping.Power_Status[0]));
  flag = FLAG_NORMAL;
  if (flag_bench) {
    if (buffer->packet.rom_housekeeping.Antenna_Status[0] != 0x3F)
      flag = FLAG_RED;
  } else {
    if (buffer->packet.rom_housekeeping.Antenna_Status[0] != 0x2A)
      flag = FLAG_RED;
  }
  if (buffer->packet.rom_housekeeping.Power_Status[0] & 0xF8)
    flag = FLAG_RED;
  vert = mv_printw (vert, 0, temp, flag);

  sprintf (temp,
           " BIU_Disc_Cmmd (in E0) [%2.2X]   BIU_rst %d BIU_RTI %d BIU_DT %d    ",
           buffer->packet.rom_housekeeping.BIU_Discrete_Command[0],
           buffer->packet.rom_housekeeping.BIU_Reset_Count[0],
           buffer->packet.rom_housekeeping.BIU_RTI_Status[0],
           buffer->packet.rom_housekeeping.LRP_analog[10]);
  flag = FLAG_NORMAL;
  if (buffer->packet.rom_housekeeping.BIU_Reset_Count[0])
    flag = FLAG_RED;
  if (buffer->packet.rom_housekeeping.BIU_RTI_Status[0])
    flag = FLAG_RED;
  if (buffer->packet.rom_housekeeping.LRP_analog[10])
    flag = FLAG_RED;
  vert = mv_printw (vert, 0, temp, flag);

  sprintf (temp, " Sts (out F0) [%2.2X]%s  Misc (in D8) [%2.2X]%s",
           buffer->packet.rom_housekeeping.BIU_Discrete_Status[0],
           sts_disp (buffer->packet.rom_housekeeping.BIU_Discrete_Status[0]),
           buffer->packet.rom_housekeeping.BIU_Discrete_misc[0],
           misc_disp (buffer->packet.rom_housekeeping.BIU_Discrete_misc[0]));
  flag = FLAG_NORMAL;
  if (buffer->packet.rom_housekeeping.BIU_Discrete_misc[0] != 0x30)
    flag = FLAG_YELLOW;
  vert = mv_printw (vert, 0, temp, flag);
  return vert;
}

int analog_1 (struct CDS_buffer *buffer, int v_current)
{
  int vert;

  vert = v_current;
  sprintf (temp,
           " Current Monitor HFR [%d]  ME2a [%d]  LPa [%d]  ME1d [%d]    ",
           buffer->packet.rom_housekeeping.HFR_analog[0],
           buffer->packet.rom_housekeeping.HFR_analog[1],
           buffer->packet.rom_housekeeping.HFR_analog[2],
           buffer->packet.rom_housekeeping.HFR_analog[3]);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp,
           " Voltage Monitor HFR +5 [%d]  HFR +6 [%d]  MFR +12 [%d]  MFR +6 [%d]    ",
           buffer->packet.rom_housekeeping.HFR_analog[4],
           buffer->packet.rom_housekeeping.HFR_analog[5],
           buffer->packet.rom_housekeeping.HFR_analog[6],
           buffer->packet.rom_housekeeping.HFR_analog[7]);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp,
           " Voltage Monitor MFR +5 [%d]  LP +45 [%d]  LRP +12 [%d]  LRP +5 [%d]    ",
           buffer->packet.rom_housekeeping.HFR_analog[8],
           buffer->packet.rom_housekeeping.HFR_analog[9],
           buffer->packet.rom_housekeeping.HFR_analog[10],
           buffer->packet.rom_housekeeping.HFR_analog[11]);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp,
           " Voltage Monitor HFR -6 [%d]  MFR -12 [%d]  MFR -6 [%d]  LP -45 [%d]    ",
           buffer->packet.rom_housekeeping.HFR_analog[12],
           buffer->packet.rom_housekeeping.HFR_analog[13],
           buffer->packet.rom_housekeeping.HFR_analog[14],
           buffer->packet.rom_housekeeping.HFR_analog[15]);
  vert = mv_printw (vert, 0, temp, 0);
  return vert;
}
int analog_2 (struct CDS_buffer *buffer, int v_current)
{
  int vert;

  vert = v_current;
  sprintf (temp, "  HFR  I[%3d] +5[%3d]  +6[%3d]  -6[%3d]  ",
           buffer->packet.rom_housekeeping.HFR_analog[0],
           buffer->packet.rom_housekeeping.HFR_analog[4],
           buffer->packet.rom_housekeeping.HFR_analog[5],
           buffer->packet.rom_housekeeping.HFR_analog[12]);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp, "  LRP  I[%3d] +5[%3d] +12[%3d]  ",
           buffer->packet.rom_housekeeping.HFR_analog[3],
           buffer->packet.rom_housekeeping.HFR_analog[11],
           buffer->packet.rom_housekeeping.HFR_analog[10]);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp,
           "  MFR  I[%3d] +5[%3d]  +6[%3d] +12[%3d] -12[%3d]  -6[%3d]  ",
           buffer->packet.rom_housekeeping.HFR_analog[1],
           buffer->packet.rom_housekeeping.HFR_analog[8],
           buffer->packet.rom_housekeeping.HFR_analog[7],
           buffer->packet.rom_housekeeping.HFR_analog[6],
           buffer->packet.rom_housekeeping.HFR_analog[13],
           buffer->packet.rom_housekeeping.HFR_analog[14]);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp, "  LP   I[%3d]         +45[%3d] -45[%3d]  ",
           buffer->packet.rom_housekeeping.HFR_analog[2],
           buffer->packet.rom_housekeeping.HFR_analog[9],
           buffer->packet.rom_housekeeping.HFR_analog[15]);
  vert = mv_printw (vert, 0, temp, 0);
  return vert;
}
int analog_disp (struct CDS_buffer *buffer, int v_current, int sort)
{
  int vert;
  int i;
  int itemp;
  float hlt;

  vert = v_current;
  sprintf (temp,
           " -------------------------- Analog Monitors COUNTS ------------------  ");
  vert = mv_printw (vert, 0, temp, FLAG_NORMAL);
  i = 0;
  sprintf (temp, " MFR 1 [%d]  MFR 2 [%d]  MFR 3 [%d]    ",
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++]);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp, " HFR T [%d] Ant mtr I [%d]  HFR Ana 0 [%d]    ",
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++]);
  vert = mv_printw (vert, 0, temp, 0);
  i += 1;
  itemp = buffer->packet.rom_housekeeping.LRP_analog[i++];
  sprintf (temp, " Motor Temp +X [%d]  -X [%d]  +Z [%d]    ",
           itemp,
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++]);
  vert = mv_printw (vert, 0, temp, 0);
  i += 1;
  itemp = buffer->packet.rom_housekeeping.LRP_analog[i++];
  hlt = buffer->packet.rom_housekeeping.LRP_analog[15];
  sprintf (temp, " Search Coil Temp [%d]  HALT%3.0f%%    ",
           itemp, 100. - (hlt * 100.) / 255.);
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp, " Motor Position +X [%d]  -X [%d]  +Z [%d]    ",
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++]);
  vert = mv_printw (vert, 0, temp, 0);
  if (cnv)
    vert = eng_3 (buffer, vert);
  else {
    if (sort)
      vert = analog_2 (buffer, vert);
    else
      vert = analog_1 (buffer, vert);
  }

  return vert;
}

#define VGUESS
float eng_conv (int count)
{
  float k = 5.00 / 255;
  float c;

  c = count;
  return c * k;
}
int gse_conv (float vcomp)
{
  float temp;
  int res;

  switch (3) {
   case 1:
     temp = vcomp - 0.14;
     temp = temp / 0.916;
     temp = temp * 100.0;
     break;
   case 2:
     temp = vcomp * 0.916;
     temp = temp + 0.14;
     temp = temp * 100.0;
     break;
   case 3:
     temp = vcomp / 0.916;
     temp = temp - 0.14;
     temp = temp * 100.0;
     temp = temp - 8;
     break;
  }
  res = temp + 0.4999;
  return res;
}
float eng3_conv (unsigned char *count, int index, int *flag)
{
  float k = 5.00 / 255;
  float vlrp, result;
  int flag_temp;
  static int icnt = 0;

  /*
   * lower RED upper   lower YEL upper                        
   */
  struct LIMIT limit_[2][16] = { 175.00, 220.00, 180.00, 215.00,        /* HFR  I   */
    90.00, 110.00, 94.00, 106.00,       /* ME02 I   */
    38.00, 47.00, 40.00, 45.00,         /* LP   I   */
    180.00, 350.00, 200.00, 333.00,     /* ME01 I   */
    4.90, 5.60, 5.05, 5.40,             /* HFR  +5  */
    5.60, 6.10, 5.70, 6.00,             /* HFR  +6  */
    11.20, 12.60, 11.40, 12.40,         /* ME02 +12 */
    5.65, 6.45, 5.80, 6.30,             /* ME02 +6  */
    4.95, 5.65, 5.10, 5.50,             /* ME02 +5  */
    45.00, 54.00, 47.00, 52.00,         /* LP   +45 */
    11.30, 12.50, 11.50, 12.30,         /* ME01 +12 */
    4.80, 5.40, 4.90, 5.20,             /* ME01 +5  */
    -6.25, -5.60, -6.15, -5.70,         /* HFR  -6  */
    -12.20, -11.30, -12.00, -11.50,     /* ME02 -12 */
    -6.50, -5.60, -6.30, -5.80,         /* ME02 -6  */
    -51.80, -41.80, -49.80, -43.80,     /* LP   -45 */

    150.00, 215.00, 160.00, 205.00,     /* HFR  I   */
    108.00, 148.00, 118.00, 138.00,     /* ME02 I   */
    38.00, 47.00, 40.00, 45.00,         /* LP   I   */
    180.00, 350.00, 200.00, 333.00,     /* ME01 I   */
    4.90, 5.60, 5.05, 5.40,             /* HFR  +5  */
    5.60, 6.10, 5.70, 6.00,             /* HFR  +6  */
    11.20, 12.60, 11.40, 12.40,         /* ME02 +12 */
    5.65, 6.45, 5.80, 6.30,             /* ME02 +6  */
    4.95, 5.65, 5.10, 5.50,             /* ME02 +5  */
    47.50, 52.50, 45.50, 54.50,         /* LP   +45 */
    10.26, 13.26, 10.76, 12.76,         /* ME01 +12 */
    4.80, 5.40, 4.90, 5.20,             /* ME01 +5  */
    -6.25, -5.10, -6.15, -5.20,         /* HFR  -6  */
    -13.00, -11.00, -12.50, -11.50,     /* ME02 -12 */
    -6.50, -5.60, -6.30, -5.80,         /* ME02 -6  */
    -56.00, -45.00, -53.00, -48.00
  };                                    /* LP   -45 */

  flag_temp = FLAG_NORMAL;
  vlrp = (float) count[index] * k;
  switch (index) {
   default:
     result = vlrp;
     break;
   case 0:                             /*  HFR Current  */
     result = vlrp / 18.6 * 1000.;
     break;
   case 1:                             /*  ME02 Current  */
     result = vlrp / 30.4 * 1000.;
     break;
   case 2:                             /*  LP Current  */
     result = vlrp / 67. * 1000.;
     break;
   case 3:                             /*  ME01 Current  */
     result = vlrp / 9.75 * 1000.;
     break;
   case 4:                             /*  HFR +5  */
     result = vlrp * 1.22;
     break;
   case 5:                             /*  HFR +6  */
     result = vlrp * 1.48;
     break;
   case 6:                             /*  ME02 +12  */
     result = vlrp * 2.85;
     break;
   case 7:                             /*  ME02 +6  */
     result = vlrp * 1.48;
     break;
   case 8:                             /*  ME02 +5  */
     result = vlrp * 1.23;
     break;
   case 9:                             /*  LP +45  */
     result = vlrp * 11.1;
     break;
   case 10:                            /*  ME01 +12  */
     result = vlrp * 3.21;
     break;
   case 11:                            /*  ME01 +5  */
     result = vlrp * 1.22;
     break;
   case 12:                            /*  HFR -6  */
     result = (float) count[5] * k;
     result = vlrp * 3.2 - result * 3.245;
     break;
   case 13:                            /*  ME02 -12  */
     result = (float) count[6] * k;
     result = vlrp * 2.46 - result * 4.14;
     break;
   case 14:                            /*  ME02 -6  */
     result = (float) count[7] * k;
     result = vlrp * 3.2 - result * 3.25;
     break;
   case 15:                            /*  LP -45  */
     result = (float) count[9] * k;
     result = result * -13.14 + vlrp * 2.1;
     break;
   case 20:
   case 21:
     result = -1;
  }
  switch (index) {
   case 0:
   case 1:
   case 2:
   case 3:
   case 4:
   case 5:
   case 6:
   case 7:
   case 8:
   case 9:
   case 10:
   case 11:
   case 12:
   case 13:
   case 14:
   case 15:
     if (result >= limit_[flag_bench][index].yellow_upper)
       flag_temp = FLAG_YELLOW;
     if (result <= limit_[flag_bench][index].yellow_lower)
       flag_temp = FLAG_YELLOW;
     if (result >= limit_[flag_bench][index].red_upper)
       flag_temp = FLAG_RED;
     if (result <= limit_[flag_bench][index].red_lower)
       flag_temp = FLAG_RED;
     break;
   default:
     flag_temp = FLAG_NORMAL;
     break;
  }
  if (flag_temp > *flag)
    *flag = flag_temp;
  return result;
}
float eng3a_conv (unsigned char *count, int index, int *flag)
{
  float result;
  float val;
  int flag_temp = FLAG_NORMAL;

  /*
   * lower RED upper   lower YEL upper                 
   */
  struct LIMIT limit_[2][16] = { 0.0, 0.0, 0.0, 0.0,    /*  0 MFR A  */
    0.0, 0.0, 0.0, 0.0,                 /*  1 MFR B  */
    0.0, 0.0, 0.0, 0.0,                 /*  2 MFR C  */
    10.0, 40.0, 20.0, 30.0,             /*  3 P/S T  */
    0.0, 0.0, 0.0, 0.0,                 /*  4 MTR I  */
    0.0, 0.0, 0.0, 0.0,                 /*  5 IEB Error  */
    0.0, 0.0, 0.0, 0.0,                 /*  6 IEB Valid  */
    -20.0, 40.0, 0.0, 30.0,             /*  7 MTR T  */
    -20.0, 40.0, 0.0, 30.0,             /*  8 MTR T  */
    -20.0, 40.0, 0.0, 30.0,             /*  9 MTR T  */
    0.0, 0.0, 0.0, 0.0,                 /* 10 0      */
    -70.0, 50.0, -65.0, 40.0,           /* 11 S/C T  */
    0.0, 0.0, 0.0, 0.0,                 /* 12 POS X  */
    0.0, 0.0, 0.0, 0.0,                 /* 13 POS Y  */
    0.0, 0.0, 0.0, 0.0,                 /* 14 POS Z  */
    0.0, 0.0, 0.0, 0.0,                 /* 15 HALT  */
    0.0, 0.0, 0.0, 0.0,                 /*  0 MFR A  */
    0.0, 0.0, 0.0, 0.0,                 /*  1 MFR B  */
    0.0, 0.0, 0.0, 0.0,                 /*  2 MFR C  */
    10.0, 40.0, 20.0, 30.0,             /*  3 P/S T  */
    0.0, 0.0, 0.0, 0.0,                 /*  4 MTR I  */
    0.0, 0.0, 0.0, 0.0,                 /*  5 IEB Error  */
    0.0, 0.0, 0.0, 0.0,                 /*  6 IEB Valid  */
    -60.0, -20.0, -59.0, -30.0,         /*  7 MTR T  */
    -60.0, -20.0, -59.0, -30.0,         /*  8 MTR T  */
    -60.0, -20.0, -59.0, -30.0,         /*  9 MTR T  */
    0.0, 0.0, 0.0, 0.0,                 /* 10 0      */
    -110.0, -90.0, -105.0, -95.0,       /* 11 S/C T  */
    0.0, 0.0, 0.0, 0.0,                 /* 12 POS X  */
    0.0, 0.0, 0.0, 0.0,                 /* 13 POS Y  */
    0.0, 0.0, 0.0, 0.0,                 /* 14 POS Z  */
    0.0, 0.0, 0.0, 0.0
  };                                    /* 15 HALT  */

  val = (float) count[index];
  switch (index) {
   default:
     result = -999;
     break;
   case 3:                             /* HFR Temp */
     /*
      * val = val * .01960784;/*
      */
     result = (119.3227) +
       (-1.5300595 * val) +
       (.009225 * val * val) + (-.00002561 * val * val * val);
     /**/ break;
   case 7:                             /* Antenna Deploy Motors */
   case 8:
   case 9:
     result = (128.556358) +
       (-1.76813034 * val) +
       (.0107905618 * val * val) + (-.0000261868956 * val * val * val);
     /**/ break;
   case 11:                            /* Search Coil */
     result = .780 * val - 98.5;
     break;
  }
  switch (index) {
   default:
     break;
   case 3:
   case 7:
   case 8:
   case 9:
   case 11:
     if (result >= limit_[flag_bench][index].yellow_upper)
       flag_temp = FLAG_YELLOW;
     if (result <= limit_[flag_bench][index].yellow_lower)
       flag_temp = FLAG_YELLOW;
     if (result >= limit_[flag_bench][index].red_upper)
       flag_temp = FLAG_RED;
     if (result <= limit_[flag_bench][index].red_lower)
       flag_temp = FLAG_RED;
     break;
  }
  return result;
}












int eng_3 (struct CDS_buffer *buffer, int v_current)
{
  int vert;
  int flag;

  vert = v_current;
  flag = FLAG_NORMAL;
  sprintf (temp, "  HFR %5.1fmA  +5:%2.3fV  +6:%2.3fV  -6:%2.3fV    ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 0, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 4, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 5, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 12, &flag));
  vert = mv_printw (vert, 0, temp, flag);
  flag = FLAG_NORMAL;
  sprintf (temp, "  LRP %5.1fmA  +5:%2.3fV +12:%2.3fV    ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 3, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 11, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 10, &flag));
  vert = mv_printw (vert, 0, temp, flag);
  flag = FLAG_NORMAL;
  sprintf (temp,
           "  MFR %5.1fmA  +5:%2.3fV  +6:%2.3fV +12:%2.3fV -12:%2.3fV  -6:%2.3fV    ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 1, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 8, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 7, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 6, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 13, &flag),
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 14, &flag));
  vert = mv_printw (vert, 0, temp, flag);
  if (format_flag) {
    flag = FLAG_NORMAL;
    sprintf (temp, "  LP  %5.1fmA            ",
             eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 2,
                        &flag));
    mv_printw (vert, 0, temp, flag);
    flag = FLAG_NORMAL;
    sprintf (temp, "+45:%2.3fV ",
             eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 9,
                        &flag));
    mv_printw (vert, 24, temp, flag);
    flag = FLAG_NORMAL;
    sprintf (temp, "-45:%2.3fV    ",
             eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 15,
                        &flag));
    vert = mv_printw (vert, 36, temp, flag);
  } else {
    sprintf (temp, "  LP  %5.1fmA            +45:%2.3fV -45:%2.3fV    ",
             eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 2, &flag),
             eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 9, &flag),
             eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 15,
                        &flag));
    vert = mv_printw (vert, 0, temp, flag);
  }
  return vert;
}
int eng_2 (struct CDS_buffer *buffer, int v_current)
{
  int vert;

  vert = v_current;
  sprintf (temp, "  HFR  I[%2.3f] +5[%2.3f]  +6[%2.3f]  -6[%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[0]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[4]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[5]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[12]));
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp, "  LRP  I[%2.3f] +5[%2.3f] +12[%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[3]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[11]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[10]));
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp,
           "  MFR  I[%2.3f] +5[%2.3f]  +6[%2.3f] +12[%2.3f] -12[%2.3f]  -6[%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[1]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[8]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[7]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[6]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[13]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[14]));
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp, "  LP   I[%2.3f]           +45[%2.3f] -45[%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[2]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[9]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[15]));
  vert = mv_printw (vert, 0, temp, 0);
  return vert;
}
int eng_1 (struct CDS_buffer *buffer, int v_current)
{
  int vert;
  int i = 0;
  int flag = FLAG_NORMAL;

  vert = v_current;
  sprintf (temp,
           " Current Monitor HFR [%2.3f]  ME2a [%2.3f]  LPa [%2.3f]  ME1d [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]));
  vert = mv_printw (vert, 0, temp, flag);
  sprintf (temp,
           " Voltage Monitor HFR +5 [%2.3f]  HFR +6 [%2.3f]  MFR +12 [%2.3f]  MFR +6 [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]));
  vert = mv_printw (vert, 0, temp, flag);
  sprintf (temp,
           " Voltage Monitor MFR +5 [%2.3f]  LP +45 [%2.3f]  LRP +12 [%2.3f]  LRP +5 [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]));
  vert = mv_printw (vert, 0, temp, flag);
  sprintf (temp,
           " Voltage Monitor HFR -6 [%2.3f]  MFR -12 [%2.3f]  MFR -6 [%2.3f]  LP -45 [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]));
  vert = mv_printw (vert, 0, temp, flag);
  return vert;
}
int eng_disp (struct CDS_buffer *buffer, int v_current, int sort)
{
  int vert;
  int i;
  int flag = FLAG_NORMAL;
  float hlt;

  vert = v_current;
  sprintf (temp,
           " -------------------------- Analog Monitors VOLTS -------------------  ");
  vert = mv_printw (vert, 0, temp, flag);
  i = 0;
  flag = FLAG_NORMAL;
  sprintf (temp, " MFR 1 [%2.3f]  MFR 2 [%2.3f]  MFR 3 [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]));
  vert = mv_printw (vert, 0, temp, flag);
  flag = FLAG_NORMAL;
  if (cnv)
    sprintf (temp, " HFR T %2.1fC Ant mtr I [%2.3f]  HFR Ana 0 [%2.3f]    ",
             eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, i++,
                         &flag),
             eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
             eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]));
  else
    sprintf (temp, " HFR T [%2.3f] Ant mtr I [%2.3f]  HFR Ana 0 [%2.3f]    ",
             eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
             eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
             eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]));
  vert = mv_printw (vert, 0, temp, flag);
  i += 1;
  flag = FLAG_NORMAL;
  if (cnv)
    sprintf (temp, " Motor Temp +X %2.1fC  -X %2.1fC  +Z %2.1fC    ",
             eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, i++,
                         &flag),
             eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, i++,
                         &flag),
             eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, i++,
                         &flag));
  else
    sprintf (temp, " Motor Temp +X [%2.3f]  -X [%2.3f]  +Z [%2.3f]    ",
             eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
             eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
             eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]));
  vert = mv_printw (vert, 0, temp, flag);
  i += 1;
  hlt = buffer->packet.rom_housekeeping.LRP_analog[15];
  flag = FLAG_NORMAL;
  if (cnv)
    sprintf (temp, " Search Coil Temp %2.1fC  HALT%3.0f%%    ",
             eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, i++,
                         &flag), 100. - (hlt * 100.) / 255.);
  else
    sprintf (temp, " Search Coil Temp [%2.3f]  HALT%3.0f%%    ",
             eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
             100. - (hlt * 100.) / 255.);
  vert = mv_printw (vert, 0, temp, flag);
  switch (cnv) {
   case 0:
   case 1:
     flag = FLAG_NORMAL;
     sprintf (temp, " Motor Position +X [%2.3f]  -X [%2.3f]  +Z [%2.3f]    ",
              eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i + 0]),
              eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i + 1]),
              eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i + 2]));
     vert = mv_printw (vert, 0, temp, flag);
     break;
   case 2:
     flag = FLAG_NORMAL;
     sprintf (temp, " Motor Pos(GSE) +X  %2.3d    -X %2.3d    +Z %2.3d   ",
              gse_conv (eng_conv
                        (buffer->packet.rom_housekeeping.LRP_analog[i + 0])),
              gse_conv (eng_conv
                        (buffer->packet.rom_housekeeping.LRP_analog[i + 1])),
              gse_conv (eng_conv
                        (buffer->packet.rom_housekeeping.LRP_analog[i + 2])));
     vert = mv_printw (vert, 0, temp, flag);
     break;
   case 3:
     flag = FLAG_NORMAL;
     sprintf (temp,
              " Motor Pos +X  [%2.3f] %2.3d -X [%2.3f] %2.3d +Z [%2.3f] %2.3d   ",
              eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i + 0]),
              gse_conv (eng_conv
                        (buffer->packet.rom_housekeeping.LRP_analog[i + 0])),
              eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i + 1]),
              gse_conv (eng_conv
                        (buffer->packet.rom_housekeeping.LRP_analog[i + 1])),
              eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i + 2]),
              gse_conv (eng_conv
                        (buffer->packet.rom_housekeeping.LRP_analog[i + 2])));
     vert = mv_printw (vert, 0, temp, flag);
     break;
  }

  i = 0;
  if (cnv)
    vert = eng_3 (buffer, vert);
  else {
    if (sort)
      vert = eng_2 (buffer, vert);
    else
      vert = eng_1 (buffer, vert);
  }
  return vert;
}
int alf_memory_delta (struct CDS_buffer *buffer)
{
  static int previous = -1;
  int current;
  int delta;

  current =
    UTIL_short_to_MSB (buffer->packet.rom_housekeeping.Memory_Address);
  if (previous < 0)
    previous = current - 64;
  delta = current - previous;
  previous = current;
  return delta;
}
int alf_active (struct CDS_buffer *buffer)
{
  static int active = 1;
  int delta;

  delta = alf_memory_delta (buffer);
  if (delta > 32)
    active = 0;
  if (delta == 32)
    active = 1;
  return active;
}
int alf_disp (struct CDS_buffer *buffer, int v_current, int active)
{
  static int old_reason = -1;
  int new_reason;
  int vert;
  static char *format = { "%s: %5d %5.4Xx %4d %5d %5d  %3d  %s    " };
  vert = v_current;
  if (active) {
    new_reason = UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[10]);
    if (old_reason == 0)
      if (new_reason != 00) {
        char *text = NULL;

        if (buffer->packet.rom_housekeeping.hrp[11])
          play ("alffailint");
        else
          play ("alffailext");
        switch (buffer->packet.rom_housekeeping.lrp[10]) {
         case 'B':
           text = "alf_B";
           break;
         case 'C':
           text = "alf_C";
           break;
         case 'L':
           text = "alf_L";
           break;
         case 's':
           text = "alf_s";
           break;
         case 'c':
           text = "alf_c";
           break;
         case 'i':
           text = "alf_i";
           break;
         case 'p':
           text = "alf_p";
           break;
         case '1':
           text = "alf_1";
           break;
         case '2':
           text = "alf_2";
           break;
         default:
           text = "alf_";
           break;
           break;
        }
        if (text)
          play (text);
      }
    old_reason = new_reason;
    sprintf (temp, " ----------------------- ALF  -----------------------");
    vert = mv_printw (vert, 0, temp, FLAG_NORMAL);
    sprintf (temp, " ALF   seq  addr  skip  load packet cnt reason  ");
    vert = mv_printw (vert, 0, temp, 0);
    sprintf (temp, format, "LRP",
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[4]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[6]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[8]),
             buffer->packet.rom_housekeeping.lrp[11],
             reason (buffer->packet.rom_housekeeping.lrp[10]));
    vert = mv_printw (vert, 0, temp, 0);
    sprintf (temp, format, "HRP",
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[4]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[6]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[8]),
             buffer->packet.rom_housekeeping.hrp[11],
             reason (buffer->packet.rom_housekeeping.hrp[10]));
    vert = mv_printw (vert, 0, temp, 0);
    sprintf (temp, format, "DCP",
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[4]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[6]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[8]),
             buffer->packet.rom_housekeeping.dcp[11],
             reason (buffer->packet.rom_housekeeping.dcp[10]));
    vert = mv_printw (vert, 0, temp, 0);
    sprintf (temp, " IPC ->HRP ->DCP HRP:BulkRD BulkWR  LRP:SeqRcv SeqExp  ");
    vert = mv_printw (vert, 0, temp, 0);
    sprintf (temp, "     %5d %5d       %4.4X   %4.4X        %4d   %4d  ",
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp_ipc[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp_ipc[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp_bulk[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp_bulk[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.alf_stat[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.
                                alf_stat[2]));
    vert = mv_printw (vert, 0, temp, 0);
  } else {
    sprintf (temp, " -----INVALID----------- ALF  -----------INVALID-----");
    vert = mv_printw (vert, 0, temp, FLAG_NORMAL);
  }
  return vert;
}
int maint_disp (struct CDS_buffer *buffer, int v_current, int cds)
{
  int flag;
  int vert;
  char *maint[] = { "Sleep- Mode ---------",
    "Normal Mode ---------",
    "Maint LP PWR OFF ----",
    "Maint LP PWR OFF dly-",
    "Maint LP PWR on -----",
    "Maint LP PWR delay --",
    "Maint LP RELAY/MUX --",
    "Maint LP RELAY dly --",
    "Maint BIU talkback --",
    "Maint BIU tb. dly ---",
    "Maintenance Mode ----",
    "?? 0B ?? ------------",
    "?? 0C ?? ------------",
    "?? 0D ?? ------------",
    "?? 0E ?? ------------",
    "?? 0F ?? ------------"
  };
  vert = v_current;
  if (cds == 0x0A90)
    sprintf (temp, " ---- HRP mode -------- [%2.2X] %s--- ",
             buffer->packet.rom_housekeeping.hrp_mode[0],
             maint[buffer->packet.rom_housekeeping.hrp_mode[0] & 0x0F]);
  else
    sprintf (temp, " ---- HRP status -------------------- ");
  vert = mv_printw (vert, 0, temp, 0);
  sprintf (temp, "  LP P8155 port C [%2.2X]  %s",
           buffer->packet.rom_housekeeping.LP_P8155[0],
           p8155_disp (buffer->packet.rom_housekeeping.LP_P8155[0]));
  flag = FLAG_NORMAL;
  switch (buffer->packet.rom_housekeeping.LP_P8155[0]) {
   case 0xC1:
   case 0xC3:
     break;
   default:
     flag = FLAG_YELLOW;
     break;
  }
  vert = mv_printw (vert, 0, temp, flag);
  sprintf (temp, "  LP  MUX [%2.2X]  DAC-0 [%2.2X]  DAC-1 [%2.2X] ",
           buffer->packet.rom_housekeeping.LP_MUX_0[0],
           buffer->packet.rom_housekeeping.LP_Probe_Bias[0],
           buffer->packet.rom_housekeeping.LP_DAC1_Bias[0]);

  vert = mv_printw (vert, 0, temp, 0);
  return vert;
}
int upkt_disp (struct CDS_buffer *buffer, int vert_in)
{
  struct upkt_struct
  {
    unsigned int rti;
    unsigned int adr;
    unsigned char *buf;
  };
  struct upkt_struct upkt[8];
  struct upkt_struct upkt_sorted[8];
  char ch;
  int vert;
  int i, j, k, rti;

  vert = vert_in;

  for (i = 0; i < 8; i++) {
    upkt[i].rti = buffer->packet.housekeeping.header[0x43 + i * 16] << 8;
    upkt[i].rti = upkt[i].rti |
      buffer->packet.housekeeping.header[0x42 + i * 16];
    upkt[i].adr = 0x40 + i * 16;
    upkt[i].buf = &buffer->packet.housekeeping.header[0x40 + i * 16];
  }
  for (i = 0; i < 8; i++) {
    k = 0;
    rti = 0x10000;
    for (j = 0; j < 8; j++) {
      if (upkt[j].rti < rti) {
        rti = upkt[j].rti;
        k = j;
      }
    }
    upkt_sorted[i].rti = upkt[k].rti;
    upkt_sorted[i].adr = upkt[k].adr;
    upkt_sorted[i].buf = upkt[k].buf;
    upkt[k].rti = 0x10000;
  }

  for (i = 0; i < 8; i++) {
    ch = ' ';
    if (upkt_sorted[i].adr == 0x40)
      ch = '*';
    sprintf (temp, "%4.4X:%c", upkt_sorted[i].adr, ch);
    mv_printw (vert, 0, temp, 0);
    for (j = 0; j < 16; j++) {
      sprintf (temp, "%2.2X ", upkt_sorted[i].buf[j]);
      k = j * 3 + 6;
      mv_printw (vert, k, temp, 0);
    }
    for (j = 0; j < 16; j++) {
      ch = upkt_sorted[i].buf[j];
      if (ch < 0x20)
        ch = '.';
      if (ch >= 0x7f)
        ch = '.';
      sprintf (temp, "%c", ch);
      mv_printw (vert, j + 4 + k, temp, 0);
    }
    vert += 1;
  }
  return vert;
}

int tlm_disp (struct CDS_buffer *buffer, int vert_in, int width, int upkt_flg)
{
  int ilen, i, j, k, vert;
  char ch;

  ilen = UTIL_extract_CDS_length (buffer) + 8;
  ilen = ilen - 32;
  ilen = ilen + width - 1;
  ilen = ilen / width;
  vert = vert_in;
  if (upkt_flg)
    if (ilen > 3)
      ilen = 3;
  if (!fg_flag ("nohex"))
    for (i = 0; i <= ilen; i++) {
      sprintf (temp, "%4.4X: ", i * width);
      mv_printw (vert, 0, temp, 0);
      for (j = 0; j < width; j++) {
        if (i * width + j >= UTIL_extract_CDS_length (buffer) + 7)
          break;
        k = j * 3 + 6;
        sprintf (temp, "%2.2X ",
                 buffer->packet.housekeeping.header[i * width + j]);
        mv_printw (vert, k, temp, 0);
      }
      for (j = 0; j < width; j++) {
        if (i * width + j >= UTIL_extract_CDS_length (buffer) + 7)
          break;
        ch = buffer->packet.housekeeping.header[i * width + j];
        if (ch < 0x20)
          ch = '.';
        if (ch >= 0x7f)
          ch = '.';
        sprintf (temp, "%c", ch);
        mv_printw (vert, j + 4 + k, temp, 0);

      }
      vert = vert + 1;
    }
  if (upkt_flg)
    vert = upkt_disp (buffer, vert);
  return vert;
}

int rom_disp (struct CDS_buffer *buffer, int width, int dump_flag)
{
  int ilen, i, j, k;
  int ccsds_pattern;
  char ch;
  static int address;
  static int old_address = -1;

  ilen = UTIL_extract_CDS_length (buffer) + 8 - 64;
  ilen = ilen - 32;
  ilen = ilen + width - 1;
  ilen = ilen / width;
  ccsds_pattern = (buffer->packet.housekeeping.header[0] << 0) |
    (buffer->packet.housekeeping.header[1] << 8);
  if (ccsds_pattern != 0x0A90)
    return 0;

  address = (buffer->packet.housekeeping.header[62] << 0) |
    (buffer->packet.housekeeping.header[63] << 8);
  if (address == 0)
    old_address = -1;
  if (dump_flag == 2)
    old_address = -1;
  if (old_address != address) {
    old_address = address;
    for (i = 0; i <= ilen; i++) {
      fprintf (stdout, "%4.4X: ", address);
      for (j = 0; j < width; j++) {
        if (i * width + j >= UTIL_extract_CDS_length (buffer) + 7)
          break;
        k = j * 3 + 6;
        fprintf (stdout, "%2.2X ",
                 buffer->packet.housekeeping.header[i * width + j + 64]);
      }
      for (j = 0; j < width; j++) {
        if (i * width + j >= UTIL_extract_CDS_length (buffer) + 7)
          break;
        ch = buffer->packet.housekeeping.header[i * width + j + 64];
        if (ch < 0x20)
          ch = '.';
        if (ch >= 0x7f)
          ch = '.';
        fprintf (stdout, "%c", ch);
      }
      fprintf (stdout, "\n");
      address += width;
    }
  }
  fflush (stdout);
  return 1;
}
int antenna_disp (struct CDS_buffer *buffer, int vert_in)
{
  int ilen, i, j, k, vert;
  char ch;

  ilen = UTIL_extract_CDS_length (buffer);
  if (ilen != 185)
    return 0;
  vert = vert_in;

  sprintf (temp,
           "--------------- ANTENNA DEPLOY BUFFERS ---- %d - %d -------",
           buffer->packet.housekeeping.header[0xBE],
           buffer->packet.housekeeping.header[0xBF]);
  mv_printw (vert, 1, temp, 0);
  vert += 1;
  sprintf (temp, "%4.4X: ", 0x40);
  mv_printw (vert, 0, temp, 0);
  for (i = 0; i < 16; i++) {
    sprintf (temp, "%2.2X ", buffer->packet.housekeeping.header[i + 0x40]);
    mv_printw (vert, i * 3 + 6, temp, 0);
  }
  for (i = 0; i < 16; i++) {
    ch = buffer->packet.housekeeping.header[i + 0x40];
    if (ch < 0x20)
      ch = '.';
    if (ch >= 0x7f)
      ch = '.';
    sprintf (temp, "%c", ch);
    mv_printw (vert, i + 55, temp, 0);
  }
  mv_printw (vert, 75, "        ", 0);
  vert += 1;
  for (j = 0; j < 5; j++) {
    k = 0x50 + j * 22;
    sprintf (temp, "%4.4X: ", k);
    mv_printw (vert, 0, temp, 0);
    for (i = 0; i < 22; i++) {
      k = j * 22 + i + 0x50;
      sprintf (temp, "%2.2X ", buffer->packet.housekeeping.header[k]);
      mv_printw (vert, i * 3 + 6, temp, 0);
    }
    /*
     * for (i=0; i<22; i++)
     * {
     * k = j*22 + i + 0x50;
     * ch = buffer->packet.housekeeping.header[k];
     * if (ch < 0x20) ch = '.';
     * if (ch>= 0x7f) ch = '.';
     * sprintf(temp, "%c", ch);
     * mv_printw(vert, i + 73, temp, 0);
     * }  
     */
    vert += 1;
  }
  mv_printw (vert, 1, "---------------                        -------------",
             0);
  vert += 1;
  return vert;
}
int upk_int (unsigned char *buf)
{
  int i;

  i = buf[1] << 8;
  i &= 0x00FF00;
  i |= buf[0] & 0x00FF;
  return i;
}
int upk_int8 (unsigned char *buf)
{
  return (buf[0]) & 0x00FF;
}
int upk_1170_disp (unsigned char *buffer, int vert)
{
  char temp[128];

  sprintf (temp, "IPC RTI:%02X%02X "
           "TX L:%d "
           "TX R:%d "
           "TX A:%d "
           "RX L:%d "
           "RX F5:%d "
           "            ",
           buffer[3], buffer[2],
           upk_int (&buffer[6]),
           upk_int (&buffer[8]),
           upk_int (&buffer[10]), upk_int (&buffer[12]), upk_int (&buffer[14])
    );
  mv_printw (vert, 0, temp, 0);
  return vert + 1;
}
int upk_25B0_disp (unsigned char *buffer, int vert)
{
  char temp[128];

  sprintf (temp, "MMISR:%02X%02X     ", buffer[3], buffer[2]);
  switch (buffer[5]) {
   case 0xB0:
     sprintf (&temp[11], "Ks:%02X "
              "%02X "
              "%02X "
              "%02X "
              "int:%5d "
              "tr:%5d "
              "mv:%5d ",
              upk_int8 (&buffer[6]),
              upk_int8 (&buffer[7]),
              upk_int8 (&buffer[8]),
              upk_int8 (&buffer[9]),
              upk_int (&buffer[10]),
              upk_int (&buffer[12]), upk_int (&buffer[14]));
     break;
   case 0xBA:
     sprintf (&temp[11], "LowK:%5d "
              "port:%02X-%02X-%02X "
              "%02X "
              "WBR: %5d "
              "main:%5d ",
              upk_int (&buffer[6]),
              upk_int8 (&buffer[8]),
              upk_int8 (&buffer[9]),
              upk_int8 (&buffer[10]),
              upk_int8 (&buffer[11]),
              upk_int (&buffer[12]), upk_int (&buffer[14]));
     break;
  }
  mv_printw (vert, 0, temp, 0);
  return vert + 1;
}
char *lbl_mro_disp (unsigned char *buffer, char *temp)
{
  char ctemp[128];
  int address;

  address = (buffer[5] << 0) | (buffer[4] << 8);
  switch (address) {
   case 0x2210:
     switch (buffer[8]) {
      case 0:
        strcat (temp, "  W08I OK");
        break;
      case 0x80:
        strcat (temp, "  W08I Ex");
        break;
      case 0x81:
        strcat (temp, "  W08I Bx");
        break;
      case 0x82:
        strcat (temp, "  W08I Ez");
        break;
      case 0x83:
        strcat (temp, "  W08I HF");
        break;
      case 0x84:
        strcat (temp, "  W08I LP");
        break;
      default:
        /*
         * WTF ?? 
         */
        /*
         * strcat(temp, "  W08I %02X", buffer[8]); 
         */
        strcat (temp, "  W08I %02X");
        break;
     }
     break;
   case 0x2520:
     switch (buffer[12]) {
      case 0x00:
        strcat (temp, "  WBRC stop");
        break;
      case 0x01:
        strcat (temp, "  WBRC idle");
        break;
      case 0x02:
        strcat (temp, "  WBRC run");
        break;
      case 0x03:
        strcat (temp, "  WBRC once");
        break;
      default:
        /*
         * WTF?? 
         */
        /*
         * strcat(temp, "  WBRC %02X", buffer[12]); 
         */
        strcat (temp, "  WBRC %02X");
        break;
     }
     break;
   case 0x1B30:
     if (buffer[8])
       strcat (temp, "  BIUH HRS");
     else
       strcat (temp, "  BIUH");
     break;
   case 0x1B31:
     if (buffer[7])
       strcat (temp, "  BIUH HRS");
     else
       strcat (temp, "  BIUH");
     break;
   case 0x1B32:
     if (buffer[6])
       strcat (temp, "  BIUH HRS");
     else
       strcat (temp, "  BIUH");
     break;
   case 0x10C0:
     address = (buffer[13] << 8) | (buffer[12] << 0);
     sprintf (ctemp, "  BIU %d", address);
     strcat (temp, ctemp);
     break;
   case 0x10C1:
   case 0x10C2:
   case 0x10C3:
   case 0x10C4:
   case 0x10C5:
   case 0x10C6:
     strcat (temp, "  BIU Direct");
     break;
   case 0x104A:
   case 0x104B:
   case 0x104C:
   case 0x104D:
   case 0x104E:
   case 0x104F:
     strcat (temp, "  IPC Direct");
     break;
   default:
     break;
  }
  return temp;
}

int upk_mro_disp (unsigned char *buffer, int vert)
{
  int i;
  int slen;
  char *temp = { "                                "
      "                                "
      "                                " "                                "
  };

  unsigned char temp2;

  sprintf (temp, "MRO RTI:%02X%02X "
           "Addr:%02X%02X  "
           "%02X %02X %02X %02X %02X "
           "%02X %02X %02X %02X %02X"
           "   ",
           buffer[3], buffer[2],
           buffer[4], buffer[5],
           buffer[6], buffer[7],
           buffer[8], buffer[9],
           buffer[10], buffer[11],
           buffer[12], buffer[13], buffer[14], buffer[15]);
  slen = strlen (temp);
  for (i = 6; i < 16; i++) {
    temp2 = buffer[i];
    if (temp2 < 0x20)
      temp2 = '.';
    if (temp2 > 0x7E)
      temp2 = '.';
    temp[slen++] = temp2;
    temp[slen] = 0;
  }
  temp = lbl_mro_disp (buffer, temp);
  mv_printw (vert, 0, temp, 0);
  return vert + 1;
}

int upk_BFD1_disp (unsigned char *buffer, int vert)
{
  char temp[128];

  sprintf (temp, "BFD RTI:%02X%02X "
           "Cmd:%d "
           "Msg:%d "
           "Bad:%d "
           "Cmd:%d "
           "WDT:%d "
           "Pkt:%d "
           "            ",
           buffer[3], buffer[2],
           upk_int (&buffer[4]),
           upk_int (&buffer[6]),
           upk_int (&buffer[8]),
           upk_int (&buffer[10]), upk_int (&buffer[12]), upk_int (&buffer[14])
    );
  mv_printw (vert, 0, temp, 0);
  return vert + 1;
}
int upk_27E0_disp (unsigned char *buffer, int vert)
{
  char temp[128];

  sprintf (temp, "DST RTI:%02X%02X "
           "Pkt:%5d "
           "0:%d "
           "10:%d "
           "20:%d "
           "30:%d "
           "40:%d "
           "50:%d "
           "60:%d "
           "70:%d "
           "            ",
           buffer[3], buffer[2],
           upk_int (&buffer[6]),
           upk_int8 (&buffer[8]),
           upk_int8 (&buffer[9]),
           upk_int8 (&buffer[10]),
           upk_int8 (&buffer[11]),
           upk_int8 (&buffer[12]),
           upk_int8 (&buffer[13]),
           upk_int8 (&buffer[14]), upk_int8 (&buffer[15]));
  mv_printw (vert, 0, temp, 0);
  return vert + 1;
}
int upk_1170_test (unsigned char *buffer)
{
  if (buffer[0] != 0xD0)
    return 0;
  if (buffer[1] != 0x0D)
    return 0;
  if (buffer[4] != 0x11)
    return 0;
  if (buffer[5] != 0x70)
    return 0;
  return 1;
}
int upk_25B0_test (unsigned char *buffer)
{
  if (buffer[0] != 0xD0)
    return 0;
  if (buffer[1] != 0x0D)
    return 0;
  if (buffer[4] != 0x25)
    return 0;
  if (buffer[5] == 0xB0)
    return 1;
  if (buffer[5] == 0xBA)
    return 1;
  return 0;
}
int upk_mro_test (unsigned char *buffer)
{
  if (buffer[0] != 0xD0)
    return 0;
  if (buffer[1] != 0x0D)
    return 0;

 /***************************** ignore IPC MRO ********************/
  if (buffer[4] == 0x11)
    return 0;
  if (buffer[5] == 0x70)
    return 0;

 /***************************** ignore MMISR debug ********************/
  if (buffer[4] == 0x25) {
    if (buffer[5] == 0xB0)
      return 0;
    if (buffer[5] == 0xBA)
      return 0;
  }

 /***************************** ignore DUST MRO ********************/
  if (buffer[4] == 0x27) {
    if (buffer[5] == 0xD0)
      return 0;
    if (buffer[5] == 0xE0)
      return 0;
  }
  return 1;
}
int upk_BFD1_test (unsigned char *buffer)
{
  if (buffer[0] != 0xC1)
    return 0;
  if (buffer[1] != 0x0D)
    return 0;
  return 1;
}
int upk_27E0_test (unsigned char *buffer)
{
  if (buffer[0] != 0xD0)
    return 0;
  if (buffer[1] != 0x0D)
    return 0;
  if (buffer[4] != 0x27)
    return 0;
  if (buffer[5] == 0xD0)
    return 1;
  if (buffer[5] == 0xE0)
    return 1;
  return 0;
}
int upkt_1170 (struct CDS_buffer *buffer, int vert_in)
{
  int i;
  int vert = vert_in;

  for (i = 0; i < 8; i++) {
    if (upk_1170_test (buffer->packet.housekeeping.micro_packet[i]))
      vert =
        upk_1170_disp (buffer->packet.housekeeping.micro_packet[i], vert);
    else if (upk_mro_test (buffer->packet.housekeeping.micro_packet[i]))
      vert = upk_mro_disp (buffer->packet.housekeeping.micro_packet[i], vert);
  }
  return vert;
}
int upkt_25B0 (struct CDS_buffer *buffer, int vert_in)
{
  int i;
  int vert = vert_in;

  for (i = 0; i < 8; i++) {
    if (upk_25B0_test (buffer->packet.housekeeping.micro_packet[i]))
      vert =
        upk_25B0_disp (buffer->packet.housekeeping.micro_packet[i], vert);
  }
  return vert;
}
int upkt_27E0 (struct CDS_buffer *buffer, int vert_in)
{
  int i;
  int vert = vert_in;

  for (i = 0; i < 8; i++) {
    if (upk_27E0_test (buffer->packet.housekeeping.micro_packet[i]))
      vert =
        upk_27E0_disp (buffer->packet.housekeeping.micro_packet[i], vert);
  }
  return vert;
}
int upkt_BFD1 (struct CDS_buffer *buffer, int vert_in)
{
  int i;
  int vert = vert_in;

  for (i = 0; i < 8; i++) {
    if (upk_BFD1_test (buffer->packet.housekeeping.micro_packet[i]))
      vert =
        upk_BFD1_disp (buffer->packet.housekeeping.micro_packet[i], vert);
  }
  return vert;
}

int raw_disp (struct CDS_buffer *buffer, int width)
{
  int ilen, i, j, vert;

  ilen = UTIL_extract_CDS_length (buffer) + 7;
  ilen = ilen + width - 1;
  ilen = ilen / width;
  vert = 6;
  for (i = 0; i < ilen; i++) {
    sprintf (temp, "%4.4X: ", i * width);
    mv_printw (vert, 0, temp, 0);
    for (j = 0; j < width; j++) {
      if (i * width + j >= UTIL_extract_CDS_length (buffer) + 7)
        break;
      sprintf (temp, "%2.2X ", buffer->packet.rtiu.header[i * width + j]);
      mv_printw (vert, j * 3 + 6, temp, 0);
    }
    vert = vert + 1;
  }
  return vert;
}

int format_mro (struct CDS_buffer *c_buffer)
{
  int status = 0;
  int addr;
  int len;

  /*
   *  First step is to look at the address in the MRO packet
   *    and ignore inappropriate MRO's
   *  Don't mask anything off so we can see the LRP select bit...
   */
  addr = c_buffer->packet.mpp.mini_packet[4] << 8;
  addr = addr + c_buffer->packet.mpp.mini_packet[5];

  /*
   *  Also, we'll take a look at the record length to
   *  accomodate the V2.4 changes to TWEAK process.
   *  It does 192 byte dumps...
   */
  len = c_buffer->packet.mpp.mini_packet[0] & 0x0F << 8;
  len = len + c_buffer->packet.mpp.mini_packet[1];

  /*
   *  Second step is to update the header area 
   *   and copy the housekeeping block
   */
  switch (addr) {
   case 0xCC42:
   case 0xCC62:
   case 0xCC82:
   case 0xCCA2:
   case 0xCCC2:
   case 0xCCE2:
     memcpy (&mro_buffer,
             c_buffer, sizeof (struct HEADER_area) + 3 * sizeof (long));
   default:
     break;
  }
  /*
   *  Last step is to  copy the housekeeping block
   */
  switch (addr) {
   case 0xCC42:
     if (len > 128)
       len = 192;
     else
       len = 128;
     memcpy (&mro_buffer.packet.mpp.mini_packet[0],
             &c_buffer->packet.mpp.mini_packet[6], len);
     status = 0;
     break;
   case 0xCC62:
     memcpy (&mro_buffer.packet.mpp.mini_packet[32],
             &c_buffer->packet.mpp.mini_packet[6], 128);
     status = 0;
     break;
   case 0xCC82:
     memcpy (&mro_buffer.packet.mpp.mini_packet[64],
             &c_buffer->packet.mpp.mini_packet[6], 128);
     status = 0;
     break;
   case 0xCCA2:
     memcpy (&mro_buffer.packet.mpp.mini_packet[96],
             &c_buffer->packet.mpp.mini_packet[6], 96);
     status = 0;
     break;
   case 0xCCC2:
     memcpy (&mro_buffer.packet.mpp.mini_packet[128],
             &c_buffer->packet.mpp.mini_packet[6], 64);
     status = 0;
     break;
   case 0xCCE2:
     memcpy (&mro_buffer.packet.mpp.mini_packet[160],
             &c_buffer->packet.mpp.mini_packet[6], 32);
     status = 0;
     break;
   default:
     status = 1;
     break;
  }
  if (status == 0)
    memcpy (c_buffer, &mro_buffer, sizeof (struct CDS_buffer));
  return status;
}
int sequential_getbuffer_CDS (struct CDS_buffer *c_buffer, FILE * file,
                              int flag)
{
  int i;
  int ilen;
  int active = 1;
  long cds_header[6];
  long type;

  while (active) {
    active = 0;
    ilen = UTIL_getbuffer_CDS (c_buffer, file, flag);
    if (ilen > 0)
      active = 1;
    if (msgs) {
      fprintf (msgs, "%4d: r_length:%5.4X r_type:%5.4X",
               logno++, c_buffer->f_length, c_buffer->record_type);
      fflush (msgs);
    }
    switch (c_buffer->record_type) {
     case DATA_RTIU_biust:
       for (i = 0; i < 16; i++)
         biust.data[i] = c_buffer->packet.biust.data[i];
       biust_count += 1;
       active = 1;
       break;
     case DATA_RTIU_ancil:
       for (i = 0; i < 16; i++)
         ancil.data[i] = c_buffer->packet.ancil.data[i];
       ancil_count += 1;
       active = 1;
       break;
     case DATA_RTIU_telem:
       if (c_buffer->packet.cds.header[1] == 0x0A)
         active = 0;
       break;
     case DATA_RTIU_nomcmd:
     case DATA_RTIU_crtcmd:
     case DATA_RTIU_fpcmd:
       active = 1;
       break;
     case DATA_MPP_MRO:
     case DATA_MPS_MRO:
     case DATA_MPL_MRO:
     case DATA_MPC_MRO:
       active = format_mro (c_buffer);
       break;
     default:
       active = 0;
       break;
    }
    if (active)
      if (msgs) {
        fprintf (msgs, "\n");
        fflush (msgs);
      }

  }
  if (msgs) {
    for (i = 0; i < 16; i++)
      fprintf (msgs, "%3.2X", c_buffer->packet.cds.header[i]);
    fprintf (msgs, "done \n");
    fflush (msgs);
  }

  return ilen;
}
volatile int recent_getbuffer_CDS (volatile char *buffer, int recent_file,
                                   int flag)
{
  static int sleep_period = 1;
  int count = 0;
  volatile int ilen;
  static volatile char previous[1024] = { 1024 * 0x00 };

  memset ((void *) buffer, 0x00, 1024);

  /*
   * Willy, you need to correct warnings, or you get really screwball things
   * like specifing the wrong number of arguments and not noticing.   I hope
   * 1024 is the proper value to use, who knows. 
   */

  /*
   * while(!memcmp((void*)buffer, (void*)previous)) 
   */
  while (!memcmp ((void *) buffer, (void *) previous, 1024)) {
    count++;
    sleep (sleep_period);
    lseek (recent_file, 0, SEEK_SET);
    ilen = read (recent_file, (char *) buffer, 1024);
  }
  if (count < 2)
    sleep_period -= 1;
  else if (count == 2)
    sleep_period + -1;
  else if (count == 3)
    sleep_period *= 2;
  else
    sleep_period *= 2;
  if (sleep_period < 1)
    sleep_period = 1;
  if (sleep_period > 32)
    sleep_period = 32;
  memcpy ((void *) previous, (void *) buffer, 1024);
  return ilen;
}

int getbuffer_CDS (struct CDS_buffer *c_buffer, FILE * file, int flag)
{
  int ilen;

  if (recent_flag == 2)
    ilen = recent_getbuffer_CDS ((char *) c_buffer, recentF, flag);
  else
    ilen = sequential_getbuffer_CDS (c_buffer, file, flag);
  return ilen;
}

int main (int argc, char *argv[])
{
  static int width = 16;
  static int delay = 0;
  int flag, second;
  int jcnt = 0;
  int fcnt = 0;
  int bcnt = 0;
  int icnt = 0;
  int comp_flag = 0;
  int dcnt[16] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  int ocnt[16] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  int ilen;
  int templ;
  long cds[6];
  int blocking_flag = UTIL_GET_BLOCKING;
  int i;
  int sclk;
  int flag_yday = 1;
  int raw = 0;
  int sort_flag = 0;
  int dump_flag = 0;
  int local_flag = 0;
  int mixed_flag = 0;
  int upkt_flg;
  int hsk_flag = 0;
  int sequence;
  int house_flag[2] = { 0, 0 };
  int cdsold = 0;
  char string[128];
  int outt = 0;
  int cds_length_valid;
  char in[256] = { "" };
  char out[256] = { "" };
  char binfile[256] = { "" };
  char hskfile[256] = { "" };
  FILE *inF = stdin;
  FILE *outF = stdout;
  FILE *binF;
  FILE *hskF;
  char type[] = { "wb" };
  time_t cds_time, pkt_epoc;
  struct tm *pkt_tm;
  struct event_time *evt_tim;
  struct event_clock evt_clk;
  mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH;

  fg_flags (argc, argv);
  /*
   * epoch = utilo_epoch(NULL); /*
   */
  if (fg_flag ("help") || fg_flag ("h")) {
    printf ("hkrom parameters:  %s\n", Version);
    printf ("\n");
    printf ("    This version accepts either CDS records or \n");
    printf ("      MP/MPUS records.  The MP/MPUS stream may \n");
    printf ("      contain MRO packets.  Each occurence of an\n");
    printf ("      MRO will emit a housekeeping update (i.e. you\n");
    printf ("      will see duplicate housekeeping records)\n");
    printf ("    TIME\n");
    printf ("      When SCLK/SCET information is available it will\n");
    printf ("      be used to display the time of the housekeeping\n");
    printf ("      record.  The difference between CDS-SCLK adn CHDO-SCLK\n");
    printf
      ("      is applied to the CHDO-SCET and then used to display the\n");
    printf ("      time field.  When this 'more or less' accurate time is\n");
    printf
      ("      avaliable, the time field displays as yyyy-dddThh:mm:ss.mmm\n");
    printf ("\n");
    printf ("       +pipe        pipe cds to %s\n", UTIL_filepipe (FILE_RAW));
    printf ("       +putcds      create new RAW file (NEW)\n");
    printf ("       -putcds                    (overwrite)\n");
    printf ("       -getcds      read from current file\n");
    printf
      ("       -stdout      send output to stdout and supress screen displays\n");
    printf ("       +eof         normal end of file processing\n");
    printf ("\n");
    printf
      ("     The recent flag is used to grab data from the real-time data feed\n");
    printf
      ("        This is accomplished by writing the housekeeping buffer to the\n");
    printf
      ("        begining of the specified file.  Another copy of this program\n");
    printf
      ("        can then repeatedly read from the file to display the\n");
    printf ("        housekeeping data without the need to change files\n");
    printf
      ("       -recent fn   write most recently received data to file\n");
    printf
      ("       +recent fn   read most recently received data from file\n");
    printf ("       -vc          separate VC0/VC1 data (separate files)\n");
    printf ("\n");
    printf ("       -raw          (analog raw counts)\n");
    printf ("       -cnv          (analog actual)\n");
    printf ("       -acnv         (analog actual) antenna position == GSE\n");
    printf ("       -bcnv         (analog actual) both antenna \n");
    printf ("       -sort         (analog mux reordererd)\n");
    printf ("       -upkt         (uPacket reordererd)\n");
    printf ("       +house        (include housekeeping to file)\n");
    printf ("       -house        (supress housekeeping to file)\n");
    printf
      ("       +comp         compress output file (eliminate trailing fill)\n");
    printf ("       -bench        alarm limits setup for Flight Spare\n");
    printf ("       -size nn      Break files into 'nn' minute files\n");
    printf ("\n");
    printf ("       -display      supress screen display\n");
    printf ("       +dump         ROM dump analysis display\n");
    printf ("                       hexdump of last 128 bytes of frame\n");
    printf ("       -dump         ROM dump analysis display\n");
    printf ("                       supress duplicates\n");
    printf ("       -format       supress screen formatting\n");
    printf ("       -nohex        supress hex dump\n");
    printf ("       -delay nn     delay nn seconds between records\n");
    printf ("       -localtime    display local time (otherwise GMT)\n");
    printf ("       -mixed        Process mixed real-time/playback\n");
    printf ("                       display real-time only\n");
    printf ("\n");
    printf ("       +binfile      BIN file for JPL (NEW)\n");
    printf ("       -binfile                 (overwrite)\n");
    printf ("       +hskfile      HSK file for JPL (NEW)\n");
    printf ("       -hskfile                 (overwrite)\n");
    printf ("\n");
    printf ("       -exit         filename scan\n");
    printf ("       -yday         revert to old gmtime(yday) handling\n");
    printf ("\n");
    printf ("       -hsk_all      all micro packet\n");
    printf ("       -hsk_ipc      IPC micro packet\n");
    printf ("       -hsk_bfdl     WFR/WBR micro packet\n");
    printf ("       -hsk_dust     DUST micro packet\n");
    printf ("\n");
    return 0;
  }

  if (fg_flag ("size") == '-') {
    size = fg_int ("size", size);
    fprintf (stderr, "File size %d minutes <<<<<<<<<<<<<<<<<<<<<\n", size);
  }
  if (fg_flag ("vc") == '-')
    virtual_channel_flag = 1;
  if (fg_flag ("recent") == '-') {
    char temp[256];
    char temp0[256];
    char temp1[256];

    recent_flag = 1;

    strcpy (temp, fg_flagc ("recent"));
    strcpy (temp0, temp);
    strcpy (temp1, temp);
    strcat (temp0, ".vc0");
    strcat (temp1, ".vc1");

    recentF = open (temp, O_WRONLY | O_CREAT, mode);
    if (virtual_channel_flag) {
      recentF0 = open (temp0, O_WRONLY | O_CREAT, mode);
      recentF1 = open (temp1, O_WRONLY | O_CREAT, mode);
    }

  }
  if (fg_flag ("recent") == '+') {
    recent_flag = 2;
    recentF = open (fg_flagc ("recent"), O_RDONLY);
  }
  if (fg_flag ("bench") == '-')
    flag_bench = 1;
  if (fg_flag ("yday") == '-')
    flag_yday = 0;
  if (fg_flag ("eof") == '+')
    blocking_flag = UTIL_GET_NON_BLOCKING;
  if (fg_flag ("hsk_all") == '-') {
    hsk_flag = -1;
  }
  if (fg_flag ("hsk_ipc") == '-') {
    hsk_flag |= 1;
  }
  if (fg_flag ("hsk_bfdl") == '-') {
    hsk_flag |= 2;
  }
  if (fg_flag ("hsk_dust") == '-') {
    hsk_flag |= 4;
  }
  if (fg_flag ("delay") == '-')
    delay = fg_flagi ("delay");
  if (fg_flag ("bell") == '+')
    bell_flag = 1;
  if (fg_flag ("localtime") == '-')
    local_flag = 1;
  if (fg_flag ("format") == '-') {
    format_flag = 0;
    bell_flag = 0;
  }
  if (fg_flag ("dump")) {
    dump_flag = 1;
    display_flag = 0;
    format_flag = 0;
    bell_flag = 0;
  }
  if (fg_flag ("dump") == '+') {
    dump_flag = 2;
  }

  if (fg_flag ("display") == '-') {
    display_flag = 0;
  }
  if (fg_flag ("house") == '-')
    house_flag[0] = 0;
  msgs = NULL;
  if (fg_flag ("cnv") == '-')
    cnv = 1;
  if (fg_flag ("acnv") == '-')
    cnv = 2;
  if (fg_flag ("bcnv") == '-')
    cnv = 3;
  if (fg_flag ("debug"))
    msgs = fopen ("hkrom2.log", "wa");
  if (fg_flag ("sort") == '-')
    sort_flag = 1;
  if (fg_flag ("upkt") == '-')
    upkt_flag = 1;
  if (fg_flag ("house") == '+')
    house_flag[0] = 1;
  if (fg_flag ("comp") == '+')
    comp_flag = 1;
  if (fg_flag ("mixed") == '-')
    mixed_flag = 1;
  if (!house_flag)
    fprintf (stderr, "-house (housekeeping supressed)\n");

  if (fg_flag ("binfile") == '+') {
    strcpy (binfile, UTIL_filenew (FILE_BIN, 0));
    fprintf (stderr, "new binfile: %s\n", binfile);
  }
  if (fg_flag ("binfile") == '-') {
    strcpy (binfile, UTIL_filename (FILE_BIN, 0));
    fprintf (stderr, "old binfile: %s\n", binfile);
  }
  if (fg_flag ("hskfile") == '+') {
    strcpy (hskfile, UTIL_filenew (FILE_HSK, 0));
    fprintf (stderr, "new hskfile: %s\n", hskfile);
  }
  if (fg_flag ("hskfile") == '-') {
    strcpy (hskfile, UTIL_filename (FILE_HSK, 0));
    fprintf (stderr, "old hskfile: %s\n", hskfile);
  }

  if (fg_flag ("pipe") == '+') {
    strcpy (out, UTIL_filepipe (FILE_RAW));
    fprintf (stderr, "pipe to: %s\n", out);
  }
  if (fg_flag ("putcds") == '+') {
    strcpy (out, UTIL_filenew (FILE_RAW, 0));
    fprintf (stderr, "new file: %s\n", out);
  }
  if (fg_flag ("putcds") == '-') {
    strcpy (out, UTIL_filename (FILE_RAW, 0));
    fprintf (stderr, "old file: %s\n", out);
  }
  if ((fg_flag ("getcds") == '-') | (fg_flag ("getcds") == '+')) {
    strcpy (in, UTIL_filename (FILE_RAW, 0));
    fprintf (stderr, "input file: %s\n", in);
  }
  if (fg_flag ("stdout") == '-') {
    stdout_flag = 1;
    display_flag = 0;
    format_flag = 0;
    house_flag[0] = 1;
  }

  if (!stdout_flag) {
    if (out) {
      outF = fopen (out, type);
      chmod (out, mode);
    }
    if (outF)
      fprintf (stderr, "     outfile (\"%s\",\"%s\")\n", out, type);
  }
  /*
   * if(in)
   * inF = fopen(in,type);
   * if(inF)
   * fprintf(stderr,"     infile  (\"%s\",\"%s\")\n", in, type);
   */
  if (binfile) {
    binF = fopen (binfile, type);
    chmod (binfile, mode);
  }
  if (hskfile) {
    hskF = fopen (hskfile, type);
    chmod (hskfile, mode);
  }
  if (binF) {
    fprintf (stderr, "fopen(\"%s\",\"%s\")\n", binfile, "wb");
    blocking_flag = UTIL_GET_NON_BLOCKING;
  }
  if (hskF) {
    fprintf (stderr, "fopen(\"%s\",\"%s\")\n", hskfile, "wb");
  }
  raw = fg_flag ("raw");
  if (fg_flag ("exit"))
    return 0;
  sleep (1);
  if (format_flag) {
    initscr ();
    nonl ();
    clear ();
    refresh ();
  } else
    fflush (stdout);
  mv_printw (1, 2, Version, 0);
  if (argc > 1) {
    width = strtol (argv[1], (char **) NULL, 0);
    if (!width)
      width = 16;
  }

  buffer = malloc (65536);
  if (!buffer) {
    fprintf (stderr, "hkrom7: unable to allocate buffer\n");
    return 1;
  }
  ilen = getbuffer_CDS (buffer, inF, blocking_flag);
  icnt = 0;
  while (ilen > 0) {

/********************************************************
 *	move the sequence field to header to allow	*
 *	the cds sequence to be passed through to	*
 *	other data products				*
 ********************************************************/

    sequence = UTIL_extract_CDS_sequence (buffer);
    buffer->packet.cds_tag.sequence[0] = (sequence >> 24) & 0xFF;
    buffer->packet.cds_tag.sequence[1] = (sequence >> 16) & 0xFF;
    buffer->packet.cds_tag.sequence[2] = (sequence >> 8) & 0xFF;
    buffer->packet.cds_tag.sequence[3] = (sequence >> 0) & 0xFF;

/********************************************************
 *	Display HSK record from CDS			*
 *	Ancillary data is really short, so ignore it	*
 *	1553 mode & BIU discrete really short too	*
 ********************************************************/

    cds_length_valid = 0;
    if (UTIL_extract_CDS_length (buffer) > 16)
      if (UTIL_extract_CDS_length (buffer) < 200)
        cds_length_valid = 1;

    if (cds_length_valid && mixed_flag) {
      sclk = UTIL_extract_TIME (buffer);
      if (sclk >= mixed_flag) {
        mixed_flag = sclk;
      } else
        cds_length_valid = 0;
    }
    if (cds_length_valid) {
      epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
        (buffer->packet.cds_tag.epoch[1] << 16) |
        (buffer->packet.cds_tag.epoch[2] << 8) |
        (buffer->packet.cds_tag.epoch[3] << 0);

      mv_printw (0, 6, binfile, 0);
      mv_printw (0, 26, out, 0);
      sprintf (temp, "%s  VC%d  In: %3d ",
               Version,
               buffer->packet.chdo_ancillary.type_92.virtual_channel_id,
               icnt);
      mv_printw (1, 1, temp, 0);
      if (bcnt) {
        sprintf (temp, " Bin:%3d ", bcnt);
        mv_printw (1, 60, temp, 0);
      }
      if (fcnt) {
        sprintf (temp, " Out:%3d ", fcnt);
        mv_printw (1, 67, temp, 0);
      }
      UTIL_extract_CDS (buffer, cds);
      sprintf (temp,
               "%2.2X %s (.rec_typ)%4d %04X   ",
               UTIL_extract_CDS_type (buffer),
               UTIL_extract_packet_type (buffer),
               buffer->record_type,
               ((UTIL_extract_TIME (buffer) & 0x001FFF) << 3) |
               UTIL_extract_RTI (buffer)
        );
      if (UTIL_extract_CDS_type (buffer) == 0x15)
        upkt_flg = upkt_flag;
      else
        upkt_flg = 0;

      mv_printw (2, 1, temp, 0);
      sprintf (temp, "RTIU length %d*   ", UTIL_extract_RTIU_length (buffer));
      mv_printw (2, 50, temp, 0);

      sprintf (temp,
               "Seq%6.2d Len%4.2d epoch:%8X Time:%8X.%d    Dsp: %3d %3d %3d ",
               UTIL_extract_CDS_sequence (buffer),
               UTIL_extract_CDS_length (buffer),
               epoch,
               UTIL_extract_TIME (buffer),
               UTIL_extract_RTI (buffer), dcnt[1], dcnt[2], dcnt[3]);
      mv_printw (3, 1, temp, 0);

      cds_time = (time_t) UTIL_extract_TIME (buffer);
      pkt_epoc = cds_time + epoch;
      if (epoch) {
        evt_clk.seconds = cds_time;
        evt_clk.fine = UTIL_extract_RTI (buffer) << 5;

        evt_tim = UTIL_event_scet ((struct MP_buffer *) buffer, evt_clk);
        pkt_tm = UTIL_event_scet_tm (*evt_tim, 0);

        sprintf (temp,
                 "%4.4X %4.4X %4.4X %4.4X %4.4X %4.4X %04d-%03dT%02d:%02d:%02d.%03d Out: %3d %3d %3d ",
                 cds[0], cds[1], cds[2], cds[3], cds[4], cds[5],
                 pkt_tm->tm_year + 1900, pkt_tm->tm_yday + flag_yday,
                 pkt_tm->tm_hour, pkt_tm->tm_min, pkt_tm->tm_sec,
                 evt_tim->milliseconds % 1000, ocnt[1], ocnt[2], ocnt[3]);

      } else {
        if (local_flag)
          pkt_tm = localtime (&pkt_epoc);
        else
          pkt_tm = gmtime (&pkt_epoc);

        sprintf (temp,
                 "%4.4X %4.4X %4.4X %4.4X %4.4X %4.4X %4d %3d %2.2d:%2.2d:%2.2d     Out: %3d %3d %3d ",
                 cds[0], cds[1], cds[2], cds[3], cds[4], cds[5],
                 pkt_tm->tm_year + 1900, pkt_tm->tm_yday + flag_yday,
                 pkt_tm->tm_hour, pkt_tm->tm_min, pkt_tm->tm_sec, ocnt[1],
                 ocnt[2], ocnt[3]);
      }
      mv_printw (4, 1, temp, 0);

      if (size) {
        static int old_minute_of_day = -1;
        static int new_minute_of_day;

        new_minute_of_day = pkt_tm->tm_hour * 60 + pkt_tm->tm_min;

        if (old_minute_of_day > 0) {
          if (old_minute_of_day / size < new_minute_of_day / size) {
            fclose (outF);
            strcpy (out, UTIL_filenew (FILE_RAW, 0));
            outF = fopen (out, type);
            chmod (out, mode);
          }
        }
        old_minute_of_day = new_minute_of_day;
      }

      i = 6;
      i = tlm_disp (buffer, i, width, upkt_flg);
      i = ancil_disp (i);
      i = biust_disp (i);
      if (raw)
        i = analog_disp (buffer, i, sort_flag);
      else
        i = eng_disp (buffer, i, sort_flag);
      i = digital_disp (buffer, i);
      i = command_disp (buffer, i);
      i = maint_disp (buffer, i, cds[0]);
      if (cds[0] == 0x0A90) {
        i = alf_disp (buffer, i, alf_active (buffer));
      }
      if (cds[0] == 0x0A93)
        if ((buffer->packet.housekeeping.Memory_Address[0] == 0) &&
            (buffer->packet.housekeeping.Memory_Address[1] == 0))
          i = antenna_disp (buffer, i);
      if (cds[0] == 0x0A95) {
        mv_printw (i++, 1, "----------- micro packet dump ----------",
                   FLAG_NORMAL);
        if (hsk_flag & 0x01)
          i = upkt_1170 (buffer, i);
        if (hsk_flag & 0x02)
          i = upkt_BFD1 (buffer, i);
        if (hsk_flag & 0x04)
          i = upkt_27E0 (buffer, i);
        if (hsk_flag & 0x10)
          i = upkt_25B0 (buffer, i);
      }
      if (dump_flag) {
        i = rom_disp (buffer, width, dump_flag);
      }
      if (format_flag)
        refresh ();
      else {
        if (display_flag)
          fputs ("\n", stdout);
      }
      if (cdsold != cds[0]) {
        if (cdsold) {
          char fname[128];

          cdsold = cds[0];
          sprintf (fname, "hsk%4.4X", cdsold);
          play (fname);
          if (cdsold == 0x0A90)
            play ("crash");
        }
        cdsold = cds[0];
      }
      if (delay)
        sleep (delay);
    }


/********************************************************
 *	Validate length of record from CDS		*
 ********************************************************/

    templ = (UTIL_extract_CDS_length (buffer) + 7) / 2;
    house_flag[1] = house_flag[0];
    i = 1;
    while (i) {
      if (packet_length[i].length) {
        if (packet_length[i].length == templ) {
          if (packet_length[i].flag)
            house_flag[1] = 1;
          dcnt[i] += 1;
          break;
        }
        i += 1;
      } else
        i = 0;
    }

/********************************************************
 *	Write to file if selection criteria met		*
 ********************************************************/
    if (i) {                            /* length valid                   */
      if (house_flag[1]) {              /* science (or hsk if selected)   */
        ocnt[i] += 1;
        if (outF) {
          fcnt += 1;
          if (comp_flag) {
            static int f_length;

            f_length = 3 + 12 +         /* length words */
              256 +                     /* spare at begining */
              UTIL_extract_CDS_length (buffer) + 7;
            f_length = f_length & 0x7FFFFFFC;
            UTIL_putbuffr2_CDS (buffer, outF, f_length);
          } else
            UTIL_putbuffer_CDS (buffer, outF);
        }
        if (binF) {
          bcnt += 1;
          fwrite (&buffer->packet.cds.header,
                  UTIL_extract_CDS_length (buffer) + 7, 1, binF);
        }
      }
    }

    if (hskF) {
      if ((UTIL_extract_CDS_length (buffer) + 7) ==
          (packet_length[HSK_p_l].length * 2)) {
        fwrite (&buffer->packet.cds.header,
                packet_length[HSK_p_l].length * 2, 1, hskF);
      }
    }

    if (recent_flag == 1) {
      int flag = 1;

      if ((UTIL_extract_CDS_length (buffer) + 7) ==
          (packet_length[HSK_p_l].length * 2)) {
        if (recentF) {
          lseek (recentF, 0, SEEK_SET);
          write (recentF, buffer, buffer->f_length);
        }
        if (buffer->packet.chdo_ancillary.type_92.virtual_channel_id) {
          if (recentF1) {
            lseek (recentF1, 0, SEEK_SET);
            write (recentF1, buffer, buffer->f_length);
          }
        } else {
          if (recentF0) {
            lseek (recentF0, 0, SEEK_SET);
            write (recentF0, buffer, buffer->f_length);
          }
        }
      }
    }

/********************************************************
 *	get next record from CDS			*
 ********************************************************/
    ilen = getbuffer_CDS (buffer, inF, blocking_flag);
    icnt += 1;
  }
  if (binF)
    fclose (binF);
  return 0;
}
