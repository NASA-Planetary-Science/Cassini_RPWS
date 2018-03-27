
/*
 * hkrom.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <term.h>
#include <time.h>
#include <sys/stat.h>
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilo.h"
#include "fg.h"

#ifdef _RPWS_
#include "rpws_au.h"
#endif
struct CDS_buffer buffer;
struct Ancillary_packet ancil;
struct BIU_status_packet biust;

static char *Version = { "V2.2" };
int bell_flag = 0;
int format_flag = 1;
int upkt_flag = 0;
static int epoch;

int ancil_count = 0;
int biust_count = 0;
FILE *msgs;
int logno = 0;

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

int mv_printw (int vert, int hor, char *stg)
{
  static int vertical = 99;
  static int horizontal = 0;
  int spaces;
  int i;

  if (format_flag) {
    mvprintw (vert, hor, "%s", stg);
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
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, " %2.2X%2.2X %2.2X%2.2X   %2.2X%2.2X   %2.2X%2.2X  ",
           biust.data[1], biust.data[0], biust.data[3], biust.data[2],
           biust.data[5], biust.data[4], biust.data[7], biust.data[6]
    );
  vert = mv_printw (vert, 0, temp);
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
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, " %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X   [%4.4X] ",
           ancil.data[1], ancil.data[0], ancil.data[3], ancil.data[2],
           ancil.data[5], ancil.data[4], ancil.data[7], ancil.data[6],
           (ancil.data[3] * 256 + ancil.data[2]) >> 3);
  vert = mv_printw (vert, 0, temp);
  return vert;
}
int command_disp (struct CDS_buffer *buffer, int v_current)
{
  static int invalid = -1;
  static int loop = -1;
  int vert;

  vert = v_current;
  sprintf (temp, " ---------------- Command Monitors ------------------   ");
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, " Cmd Count [%d]  Valid [%d]  Invalid[%d] Loop[%d]     ",
           UTIL_short_to_MSB (buffer->packet.rom_housekeeping.
                              Command_Byte_Count),
           buffer->packet.rom_housekeeping.Valid_Command_Count[0],
           buffer->packet.rom_housekeeping.Invalid_Command_Count[0],
           buffer->packet.rom_housekeeping.Command_Loop_Count[0]);
  vert = mv_printw (vert, 0, temp);
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
  return vert;
}
int digital_disp (struct CDS_buffer *buffer, int v_current)
{
  int vert;

  vert = v_current;
  sprintf (temp, " ---------------- Digital Monitors ------------------   ");
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, " Ant sts (in C0) [%2.2X]  Pwr_Sts (out E8) [%2.2X]    ",
           buffer->packet.rom_housekeeping.Antenna_Status[0],
           buffer->packet.rom_housekeeping.Power_Status[0]);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, " BIU_Disc_Cmmd (in E0) [%2.2X]   BIU_rst %d BIU_RTI %d    ",
           buffer->packet.rom_housekeeping.BIU_Discrete_Command[0],
           buffer->packet.rom_housekeeping.BIU_Reset_Count[0],
           buffer->packet.rom_housekeeping.BIU_RTI_Status[0]);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, "  Status (out F0) [%2.2X]  Misc (in D8) [%2.2X]    ",
           buffer->packet.rom_housekeeping.BIU_Discrete_Status[0],
           buffer->packet.rom_housekeeping.BIU_Discrete_misc[0]);
  vert = mv_printw (vert, 0, temp);
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
  vert = mv_printw (vert, 0, temp);
  sprintf (temp,
           " Voltage Monitor HFR +5 [%d]  HFR +6 [%d]  MFR +12 [%d]  MFR +6 [%d]    ",
           buffer->packet.rom_housekeeping.HFR_analog[4],
           buffer->packet.rom_housekeeping.HFR_analog[5],
           buffer->packet.rom_housekeeping.HFR_analog[6],
           buffer->packet.rom_housekeeping.HFR_analog[7]);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp,
           " Voltage Monitor MFR +5 [%d]  LP +45 [%d]  LRP +12 [%d]  LRP +5 [%d]    ",
           buffer->packet.rom_housekeeping.HFR_analog[8],
           buffer->packet.rom_housekeeping.HFR_analog[9],
           buffer->packet.rom_housekeeping.HFR_analog[10],
           buffer->packet.rom_housekeeping.HFR_analog[11]);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp,
           " Voltage Monitor HFR -6 [%d]  MFR -12 [%d]  MFR -6 [%d]  LP -45 [%d]    ",
           buffer->packet.rom_housekeeping.HFR_analog[12],
           buffer->packet.rom_housekeeping.HFR_analog[13],
           buffer->packet.rom_housekeeping.HFR_analog[14],
           buffer->packet.rom_housekeeping.HFR_analog[15]);
  vert = mv_printw (vert, 0, temp);
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
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, "  LRP  I[%3d] +5[%3d] +12[%3d]  ",
           buffer->packet.rom_housekeeping.HFR_analog[3],
           buffer->packet.rom_housekeeping.HFR_analog[11],
           buffer->packet.rom_housekeeping.HFR_analog[10]);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp,
           "  MFR  I[%3d] +5[%3d]  +6[%3d] +12[%3d] -12[%3d]  -6[%3d]  ",
           buffer->packet.rom_housekeeping.HFR_analog[1],
           buffer->packet.rom_housekeeping.HFR_analog[8],
           buffer->packet.rom_housekeeping.HFR_analog[7],
           buffer->packet.rom_housekeeping.HFR_analog[6],
           buffer->packet.rom_housekeeping.HFR_analog[13],
           buffer->packet.rom_housekeeping.HFR_analog[14]);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, "  LP   I[%3d]         +45[%3d] -45[%3d]  ",
           buffer->packet.rom_housekeeping.HFR_analog[2],
           buffer->packet.rom_housekeeping.HFR_analog[9],
           buffer->packet.rom_housekeeping.HFR_analog[15]);
  vert = mv_printw (vert, 0, temp);
  return vert;
}
int analog_disp (struct CDS_buffer *buffer, int v_current, int sort)
{
  int vert;
  int i;

  vert = v_current;
  sprintf (temp,
           " -------------------------- Analog Monitors COUNTS ------------------  ");
  vert = mv_printw (vert, 0, temp);
  i = 0;
  sprintf (temp, " MFR 1 [%d]  MFR 2 [%d]  MFR 2 [%d]    ",
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++]);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, " HFR T [%d] Ant mtr I [%d]  HFR Ana 0 [%d]    ",
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++]);
  vert = mv_printw (vert, 0, temp);
  i += 1;
  sprintf (temp, " Moter Temp +X [%d]  -X [%d]  +Z [%d]    ",
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++]);
  vert = mv_printw (vert, 0, temp);
  i += 1;
  sprintf (temp, " Search Coil Temp [%d]  HALT %d%%    ",
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           100 -
           (buffer->packet.rom_housekeeping.LRP_analog[15] * 100) / 255);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, " Moter Position +X [%d]  -X [%d]  +Z [%d]    ",
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++],
           buffer->packet.rom_housekeeping.LRP_analog[i++]);
  vert = mv_printw (vert, 0, temp);
  if (sort)
    vert = analog_2 (buffer, vert);
  else
    vert = analog_1 (buffer, vert);

  return vert;
}
float eng_conv (int count)
{
  float k = 5.00 / 255;
  float c;

  c = count;
  return c * k;
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
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, "  LRP  I[%2.3f] +5[%2.3f] +12[%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[3]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[11]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[10]));
  vert = mv_printw (vert, 0, temp);
  sprintf (temp,
           "  MFR  I[%2.3f] +5[%2.3f]  +6[%2.3f] +12[%2.3f] -12[%2.3f]  -6[%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[1]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[8]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[7]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[6]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[13]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[14]));
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, "  LP   I[%2.3f]           +45[%2.3f] -45[%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[2]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[9]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[15]));
  vert = mv_printw (vert, 0, temp);
  return vert;
}
int eng_1 (struct CDS_buffer *buffer, int v_current)
{
  int vert;
  int i = 0;

  vert = v_current;
  sprintf (temp,
           " Current Monitor HFR [%2.3f]  ME2a [%2.3f]  LPa [%2.3f]  ME1d [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]));
  vert = mv_printw (vert, 0, temp);
  sprintf (temp,
           " Voltage Monitor HFR +5 [%2.3f]  HFR +6 [%2.3f]  MFR +12 [%2.3f]  MFR +6 [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]));
  vert = mv_printw (vert, 0, temp);
  sprintf (temp,
           " Voltage Monitor MFR +5 [%2.3f]  LP +45 [%2.3f]  LRP +12 [%2.3f]  LRP +5 [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]));
  vert = mv_printw (vert, 0, temp);
  sprintf (temp,
           " Voltage Monitor HFR -6 [%2.3f]  MFR -12 [%2.3f]  MFR -6 [%2.3f]  LP -45 [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.HFR_analog[i++]));
  vert = mv_printw (vert, 0, temp);
  return vert;
}
int eng_disp (struct CDS_buffer *buffer, int v_current, int sort)
{
  int vert;
  int i;

  vert = v_current;
  sprintf (temp,
           " -------------------------- Analog Monitors VOLTS -------------------  ");
  vert = mv_printw (vert, 0, temp);
  i = 0;
  sprintf (temp, " MFR 1 [%2.3f]  MFR 2 [%2.3f]  MFR 2 [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]));
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, " HFR T [%2.3f] Ant mtr I [%2.3f]  HFR Ana 0 [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]));
  vert = mv_printw (vert, 0, temp);
  i += 1;
  sprintf (temp, " Moter Temp +X [%2.3f]  -X [%2.3f]  +Z [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]));
  vert = mv_printw (vert, 0, temp);
  i += 1;
  sprintf (temp, " Search Coil Temp [%2.3f]  HALT %d%%    ",
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           100 -
           (buffer->packet.rom_housekeeping.LRP_analog[15] * 100) / 255);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, " Moter Position +X [%2.3f]  -X [%2.3f]  +Z [%2.3f]    ",
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]),
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[i++]));
  vert = mv_printw (vert, 0, temp);
  i = 0;
  if (sort)
    vert = eng_2 (buffer, vert);
  else
    vert = eng_1 (buffer, vert);
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
    vert = mv_printw (vert, 0, temp);
    sprintf (temp, " ALF   seq  addr  skip  load packet cnt reason  ");
    vert = mv_printw (vert, 0, temp);
    sprintf (temp, format, "LRP",
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[4]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[6]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp[8]),
             buffer->packet.rom_housekeeping.lrp[11],
             reason (buffer->packet.rom_housekeeping.lrp[10]));
    vert = mv_printw (vert, 0, temp);
    sprintf (temp, format, "HRP",
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[4]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[6]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp[8]),
             buffer->packet.rom_housekeeping.hrp[11],
             reason (buffer->packet.rom_housekeeping.hrp[10]));
    vert = mv_printw (vert, 0, temp);
    sprintf (temp, format, "DCP",
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[4]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[6]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.dcp[8]),
             buffer->packet.rom_housekeeping.dcp[11],
             reason (buffer->packet.rom_housekeeping.dcp[10]));
    vert = mv_printw (vert, 0, temp);
    sprintf (temp, " IPC ->HRP ->DCP HRP:BulkRD BulkWR  LRP:SeqRcv SeqExp  ");
    vert = mv_printw (vert, 0, temp);
    sprintf (temp, "     %5d %5d       %4.4X   %4.4X        %4d   %4d  ",
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp_ipc[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.lrp_ipc[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp_bulk[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.hrp_bulk[2]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.alf_stat[0]),
             UTIL_short_to_MSB (&buffer->packet.rom_housekeeping.
                                alf_stat[2]));
    vert = mv_printw (vert, 0, temp);
  } else {
    sprintf (temp, " -----INVALID----------- ALF  -----------INVALID-----");
    vert = mv_printw (vert, 0, temp);
  }
  return vert;
}
int maint_disp (struct CDS_buffer *buffer, int v_current, int cds)
{
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
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, "  LP P8155 port C [%2.2X]",
           buffer->packet.rom_housekeeping.LP_P8155[0]);
  vert = mv_printw (vert, 0, temp);
  sprintf (temp, "  LP  MUX [%2.2X]  DAC-0 [%2.2X]  DAC-1 [%2.2X] ",
           buffer->packet.rom_housekeeping.LP_MUX_0[0],
           buffer->packet.rom_housekeeping.LP_Probe_Bias[0],
           buffer->packet.rom_housekeeping.LP_DAC1_Bias[0]);

  vert = mv_printw (vert, 0, temp);
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
    mv_printw (vert, 0, temp);
    for (j = 0; j < 16; j++) {
      sprintf (temp, "%2.2X ", upkt_sorted[i].buf[j]);
      k = j * 3 + 6;
      mv_printw (vert, k, temp);
    }
    for (j = 0; j < 16; j++) {
      ch = upkt_sorted[i].buf[j];
      if (ch < 0x20)
        ch = '.';
      if (ch >= 0x7f)
        ch = '.';
      sprintf (temp, "%c", ch);
      mv_printw (vert, j + 4 + k, temp);
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
      mv_printw (vert, 0, temp);
      for (j = 0; j < width; j++) {
        if (i * width + j >= UTIL_extract_CDS_length (buffer) + 7)
          break;
        k = j * 3 + 6;
        sprintf (temp, "%2.2X ",
                 buffer->packet.housekeeping.header[i * width + j]);
        mv_printw (vert, k, temp);
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
        mv_printw (vert, j + 4 + k, temp);

      }
      vert = vert + 1;
    }
  if (upkt_flg)
    vert = upkt_disp (buffer, vert);
  return vert;
}

int antenna_disp (struct CDS_buffer *buffer, int vert_in)
{
  int ilen, i, j, k, vert;
  char ch;

  ilen = UTIL_extract_CDS_length (buffer);
  if (ilen != 185)
    return;
  vert = vert_in;

  sprintf (temp,
           "--------------- ANTENNA DEPLOY BUFFERS ---- %d - %d -------",
           buffer->packet.housekeeping.header[0xBE],
           buffer->packet.housekeeping.header[0xBF]);
  mv_printw (vert, 1, temp);
  vert += 1;
  sprintf (temp, "%4.4X: ", 0x40);
  mv_printw (vert, 0, temp);
  for (i = 0; i < 16; i++) {
    sprintf (temp, "%2.2X ", buffer->packet.housekeeping.header[i + 0x40]);
    mv_printw (vert, i * 3 + 6, temp);
  }
  for (i = 0; i < 16; i++) {
    ch = buffer->packet.housekeeping.header[i + 0x40];
    if (ch < 0x20)
      ch = '.';
    if (ch >= 0x7f)
      ch = '.';
    sprintf (temp, "%c", ch);
    mv_printw (vert, i + 55, temp);
  }
  mv_printw (vert, 75, "        ");
  vert += 1;
  for (j = 0; j < 5; j++) {
    k = 0x50 + j * 22;
    sprintf (temp, "%4.4X: ", k);
    mv_printw (vert, 0, temp);
    for (i = 0; i < 22; i++) {
      k = j * 22 + i + 0x50;
      sprintf (temp, "%2.2X ", buffer->packet.housekeeping.header[k]);
      mv_printw (vert, i * 3 + 6, temp);
    }
    /*
     * for (i=0; i<22; i++)
     * {
     * k = j*22 + i + 0x50;
     * ch = buffer->packet.housekeeping.header[k];
     * if (ch < 0x20) ch = '.';
     * if (ch>= 0x7f) ch = '.';
     * sprintf(temp, "%c", ch);
     * mv_printw(vert, i + 73, temp);
     * }  
     */
    vert += 1;
  }
  mv_printw (vert, 1, "---------------                        -------------");
  vert += 1;
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
    mv_printw (vert, 0, temp);
    for (j = 0; j < width; j++) {
      if (i * width + j >= UTIL_extract_CDS_length (buffer) + 7)
        break;
      sprintf (temp, "%2.2X ", buffer->packet.rtiu.header[i * width + j]);
      mv_printw (vert, j * 3 + 6, temp);
    }
    vert = vert + 1;
  }
  return vert;
}

int getbuffer_CDS (struct CDS_buffer *c_buffer, FILE * file, int flag)
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

main (int argc, char *argv[])
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
  int raw = 0;
  int sort_flag = 0;
  int upkt_flg;
  int sequence;
  int house_flag[2] = { 0, 0 };
  int cdsold = 0;
  char string[128];
  int outt = 0;
  char in[256] = { "" };
  char out[256] = { "" };
  char binfile[256] = { "" };
  char hskfile[256] = { "" };
  FILE *inF = stdin;
  FILE *outF;
  FILE *binF;
  FILE *hskF;
  char type[] = { "wb" };
  time_t cds_time, pkt_epoc;
  struct tm *pkt_tm;

  fg_flags (argc, argv);
  epoch = utilo_epoch (NULL);
  if (fg_flag ("help") || fg_flag ("h")) {
    printf ("hkrom parameters:  %s\n", Version);
    printf ("\n");
    printf ("       +pipe        pipe cds to %s\n", UTIL_filepipe (FILE_RAW));
    printf ("       +putcds      create new RAW file (NEW)\n");
    printf ("       -putcds                    (overwrite)\n");
    printf ("       -getcds      read from current file\n");
    printf ("\n");
    printf ("       -raw          (analog raw counts)\n");
    printf ("       -sort         (analog mux reordererd)\n");
    printf ("       -upkt         (uPacket reordererd)\n");
    printf ("       +house        (include housekeeping to file)\n");
    printf ("       -house        (supress housekeeping to file)\n");
    printf
      ("       +comp         compress output file (eliminate trailing fill)\n");
    printf ("\n");
    printf ("       -format       supress screen formatting\n");
    printf ("       -nohex        supress hex dump\n");
    printf ("       -delay nn     delay nn seconds between records\n");
    printf ("\n");
    printf ("       +binfile      BIN file for JPL (NEW)\n");
    printf ("       -binfile                 (overwrite)\n");
    printf ("       +hskfile      HSK file for JPL (NEW)\n");
    printf ("       -hskfile                 (overwrite)\n");
    printf ("\n");
    printf ("       -exit          filename scan\n");
    printf ("\n");
    exit (0);
  }
  if (fg_flag ("delay") == '-')
    delay = fg_flagi ("delay");
  if (fg_flag ("bell") == '+')
    bell_flag = 1;
  if (fg_flag ("format") == '-') {
    format_flag = 0;
    bell_flag = 0;
  }
  if (fg_flag ("house") == '-')
    house_flag[0] = 0;
  msgs = NULL;
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

  if (out) {
    outF = fopen (out, type);
    chmod (out, mode);
  }
  if (outF)
    fprintf (stderr, "     outfile (\"%s\",\"%s\")\n", out, type);
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
    exit (0);
  sleep (1);
  if (format_flag) {
    initscr ();
    nonl ();
    clear ();
    refresh ();
  } else
    fflush (stdout);
  mv_printw (1, 2, "CASSINI RPWS ROM housekeeping dump (HKROM2)");
  if (argc > 1) {
    width = strtol (argv[1], (char **) NULL, 0);
    if (!width)
      width = 16;
  }

  ilen = getbuffer_CDS (&buffer, inF, blocking_flag);
  icnt = 0;
  while (ilen > 0) {

/********************************************************
 *	move the sequence field to header to allow	*
 *	the cds sequence to be passed through to	*
 *	other data products				*
 ********************************************************/

    sequence = UTIL_extract_CDS_sequence (&buffer);
    buffer.packet.cds_tag.sequence[0] = (sequence >> 24) & 0xFF;
    buffer.packet.cds_tag.sequence[1] = (sequence >> 16) & 0xFF;
    buffer.packet.cds_tag.sequence[2] = (sequence >> 8) & 0xFF;
    buffer.packet.cds_tag.sequence[3] = (sequence >> 0) & 0xFF;

/********************************************************
 *	Display HSK record from CDS			*
 *	Ancillary data is really short, so ignore it	*
 *	1553 mode & BIU discrete really short too	*
 ********************************************************/
    if (UTIL_extract_CDS_length (&buffer) > 16)
      if (UTIL_extract_CDS_length (&buffer) < 200) {
        epoch = (buffer.packet.cds_tag.epoch[0] << 24) |
          (buffer.packet.cds_tag.epoch[1] << 16) |
          (buffer.packet.cds_tag.epoch[2] << 8) |
          (buffer.packet.cds_tag.epoch[3] << 0);

        mv_printw (0, 6, binfile);
        mv_printw (0, 26, out);
        mv_printw (1, 2, "CASSINI RPWS ROM housekeeping dump (HKROM2)");
        sprintf (temp, "In: %d ", icnt);
        mv_printw (1, 50, temp);
        if (bcnt) {
          sprintf (temp, "Bin:%d ", bcnt);
          mv_printw (1, 57, temp);
        }
        if (fcnt) {
          sprintf (temp, "Out:%d ", fcnt);
          mv_printw (1, 64, temp);
        }
        UTIL_extract_CDS (&buffer, cds);
        sprintf (temp,
                 "%2.2X %s  (.record_type)%4d    ",
                 UTIL_extract_CDS_type (&buffer),
                 UTIL_extract_packet_type (&buffer), buffer.record_type);
        if (UTIL_extract_CDS_type (&buffer) == 0x15)
          upkt_flg = upkt_flag;
        else
          upkt_flg = 0;

        mv_printw (2, 1, temp);
        sprintf (temp, "RTIU length %d   ",
                 UTIL_extract_RTIU_length (&buffer));
        mv_printw (2, 50, temp);
        sprintf (temp, "Seq%6.2d ", UTIL_extract_CDS_sequence (&buffer));
        mv_printw (3, 1, temp);
        sprintf (temp, "Len%6.2d ", UTIL_extract_CDS_length (&buffer));
        mv_printw (3, 15, temp);
        sprintf (temp, " Time %8X.%d ",
                 UTIL_extract_TIME (&buffer), UTIL_extract_RTI (&buffer)
          );
        mv_printw (3, 30, temp);
        sprintf (temp, "Dsp: %d %d %d ", dcnt[1], dcnt[2], dcnt[3]);
        mv_printw (3, 50, temp);
        sprintf (temp, "%4.4X %4.4X %4.4X %4.4X %4.4X %4.4X",
                 cds[0], cds[1], cds[2], cds[3], cds[4], cds[5]);
        mv_printw (4, 1, temp);
        cds_time = (time_t) UTIL_extract_TIME (&buffer);
        pkt_epoc = cds_time + epoch;
        pkt_tm = localtime (&pkt_epoc);

        sprintf (string, "%4d %3d %2.2d:%2.2d:%2.2d",
                 pkt_tm->tm_year + 1900,
                 pkt_tm->tm_yday,
                 pkt_tm->tm_hour, pkt_tm->tm_min, pkt_tm->tm_sec);
        mv_printw (4, 31, string);
        sprintf (temp, "Out: %d %d %d ", ocnt[1], ocnt[2], ocnt[3]);
        mv_printw (4, 50, temp);
        i = 6;
        i = tlm_disp (&buffer, i, width, upkt_flg);
        i = ancil_disp (i);
        i = biust_disp (i);
        if (raw)
          i = analog_disp (&buffer, i, sort_flag);
        else
          i = eng_disp (&buffer, i, sort_flag);
        i = digital_disp (&buffer, i);
        i = command_disp (&buffer, i);
        i = maint_disp (&buffer, i, cds[0]);
        if (cds[0] == 0x0A90) {
          i = alf_disp (&buffer, i, alf_active (&buffer));
        }
        if (cds[0] == 0x0A93)
          i = antenna_disp (&buffer, i);
        if (format_flag)
          refresh ();
        else
          fputs ("\n", stdout);
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

    templ = (UTIL_extract_CDS_length (&buffer) + 7) / 2;
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
              UTIL_extract_CDS_length (&buffer) + 7;
            f_length = f_length & 0x7FFFFFFC;
            UTIL_putbuffr2_CDS (&buffer, outF, f_length);
          } else
            UTIL_putbuffer_CDS (&buffer, outF);
        }
        if (binF) {
          bcnt += 1;
          fwrite (&buffer.packet.cds.header,
                  UTIL_extract_CDS_length (&buffer) + 7, 1, binF);
        }
      }
    }

    if (hskF) {
      if ((UTIL_extract_CDS_length (&buffer) + 7) ==
          (packet_length[HSK_p_l].length * 2)) {
        fwrite (&buffer.packet.cds.header,
                packet_length[HSK_p_l].length * 2, 1, hskF);
      }
    }

/********************************************************
 *	get next record from CDS			*
 ********************************************************/
    ilen = getbuffer_CDS (&buffer, inF, blocking_flag);
    icnt += 1;
  }
  if (binF)
    fclose (binF);
  return (0);
}
