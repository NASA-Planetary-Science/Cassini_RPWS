#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rtiu.h"

extern int dsp9b_alarm (char *, char *, int);
char *dsp9c_title = { "(dsp9c) 1.2" };


char *dsp9c_color_key_table[] = {
  " <TABLE border=\"1\"",
  "    summary=\"Color key table\">",
  "    <CAPTION><EM>Color Key Table</EM></CAPTION>",
  "    <TR><TH>",
  "        <TH>Black",
  "        <TH>Red",
  "        <TH>Yellow",
  "        <TH>Brown",
  "        <TH>Green",
  "        <TH>Purple",
  "        <TH>Olive",
  "    <TR><TH>Micro Packet",
  "        <TH>",
  "        <TH> WBR/WFR <br> error",
  "        <TH> IPC MRO <br> error count",
  "        <TH>",
  "        <TH> Dust <br> micropacket",
  "        <TH> IPC MRO <br> micropacket",
  "        <TH> WBR/WFR <br> command status",
  "    <TR><TH>Time <br> UT, ERT, <BR> RCT, SCET",
  "        <TH>Data <BR>Real Time",
  "        <TH>Data more than <BR> 1 Minute old",
  "        <TH>Data more than <BR> 1 Hour old",
  "        <TH>Data more than <BR> 1 Day old",
  "        <TH>",
  "    <TR><TH>Processor Health <br> Software load",
  "        <TH>Nominal",
  "        <TH>FAULT <br> Instrument Problem",
  "        <TH>Running from <BR> ROM",
  "    <TR><TH>Processor Health <br> Halt",
  "        <TH>Nominal<br> load low",
  "        <TH>LRP CPU <br> load high",
  "        <TH>LRP CPU <br> load meduim",
  "    <TR><TH>Deploy",
  "        <TH>Nominal",
  "        <TH>Limit Switch <br> Position pot",
  "        <TH>Position pot",
  "    <TR><TH>Temperature",
  "        <TH>Nominal",
  "        <TH>Temperature <BR> extreme",
  "        <TH>Temperature <BR> high/low",
  "    <TR><TH>Power Supply",
  "        <TH>Nominal",
  "        <TH>Supply <br> overlimit",
  "        <TH>Supply <br> approaching <br> limit",
  "    <TR><TH>Command",
  "        <TH>Nominal",
  "        <TH>Invalid Commands",
  "    <TR><TH>LRP Digital",
  "        <TH>Nominal",
  "        <TH>HFR cmd err<br>BIU problem",
  "        <TH>BIU discreet<BR>Analog not powered",
  "    <TR><TH>HRP Digital",
  "        <TH>Nominal",
  "        <TH>",
  "        <TH>L/P not powered",
  "    <TR><TH>",
  "        <TH>Black",
  "        <TH>Red",
  "        <TH>Yellow",
  "        <TH>Brown",
  "        <TH>Green",
  "        <TH>Purple",
  "        <TH>Olive",
  "    </TABLE>",
  NULL
};

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

  /********** TEST **********/
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
int upk_117A_test (unsigned char *buffer)
{
  if (buffer[0] != 0xD0)
    return 0;
  if (buffer[1] != 0x0D)
    return 0;
  if (buffer[4] != 0x11)
    return 0;
  if (buffer[5] != 0x7A)
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
int upk_BFD1_test (unsigned char *buffer)
{
  if (buffer[0] != 0xC1)
    return 0;
  if (buffer[1] != 0x0D)
    return 0;
  return 1;
}
int upk_mro_test (unsigned char *buffer)
{
  if (buffer[0] != 0xD0)
    return 0;

 /***************************** ignore IPC MRO ********************/
  if (buffer[1] == 0x0D) {

    if (buffer[4] == 0x11) {
      if (buffer[5] == 0x70)
        return 0;
      if (buffer[5] == 0x7A)
        return 0;
    }

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
  }
  return 1;
}

  /********** DISP **********/
int upk_1170_disp (unsigned char *buffer)
{
  char temp[128];
  char *color;

  color = "purple";
  sprintf (temp, "IPC RTI:%02X%02X ", buffer[3], buffer[2]);
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "TX L:%3d ", upk_int (&buffer[6]));
  if (upk_int (&buffer[6]))
    color = "yellow";
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "TX R:%3d ", upk_int (&buffer[8]));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "TX A:%3d ", upk_int (&buffer[10]));
  if (upk_int (&buffer[10]))
    color = "yellow";
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "RX L:%3d ", upk_int (&buffer[12]));
  if (upk_int (&buffer[10]))
    color = "yellow";
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "RX F5:%3d ", upk_int (&buffer[14]));
  dsp9b_alarm (temp, color, 1);
}
int upk_117A_disp (unsigned char *buffer)
{
  char temp[128];
  char *color;

  color = "purple";
  sprintf (temp, "IPC RTI:%02X%02X ", buffer[3], buffer[2]);    /* RTI                  */
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "Trig:%3d          ", upk_int (&buffer[10]));  /*  Trigger             */
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "RX A:%3d ", upk_int (&buffer[6]));    /*  RX Abort            */
  if (upk_int (&buffer[6]))
    color = "yellow";
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "TX R:%3d Cnt        ", upk_int (&buffer[8])); /*  TX Retry Count      */
  dsp9b_alarm (temp, color, 0);
  sprintf (temp,
           "\"%c%c%c%c\"  ", buffer[12], buffer[13], buffer[14], buffer[15]);
  dsp9b_alarm (temp, color, 1);
}

int upk_25B0_disp (unsigned char *buffer)
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
  dsp9b_alarm (temp, "maroon", 1);
}

int upk_27E0_disp (unsigned char *buffer)
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
           "70:%d ",
           buffer[3], buffer[2],
           upk_int (&buffer[6]),
           upk_int8 (&buffer[8]),
           upk_int8 (&buffer[9]),
           upk_int8 (&buffer[10]),
           upk_int8 (&buffer[11]),
           upk_int8 (&buffer[12]),
           upk_int8 (&buffer[13]),
           upk_int8 (&buffer[14]), upk_int8 (&buffer[15]));
  dsp9b_alarm (temp, "green", 1);
}
int upk_BFD1_disp (unsigned char *buffer)
{
  char temp[128];
  char *color;

  color = "olive";
  sprintf (temp, "BFD RTI:%02X%02X ", buffer[3], buffer[2]);
  dsp9b_alarm (temp, color, 0);
  color = "olive";
  sprintf (temp, "Cmd:%d ", upk_int (&buffer[4]));
  dsp9b_alarm (temp, color, 0);
  color = "olive";
  sprintf (temp, "Msg:%d ", upk_int (&buffer[6]));
  dsp9b_alarm (temp, color, 0);
  color = "olive";
  sprintf (temp, "Bad:%d ", upk_int (&buffer[8]));
  if (upk_int (&buffer[8]))
    color = "red";
  dsp9b_alarm (temp, color, 0);
  color = "olive";
  sprintf (temp, "Cmd:%d ", upk_int (&buffer[10]));
  if (upk_int (&buffer[8]))
    color = "red";
  dsp9b_alarm (temp, color, 0);
  color = "olive";
  sprintf (temp, "WDT:%d ", upk_int (&buffer[12]));
  if (upk_int (&buffer[12]))
    color = "yellow";
  dsp9b_alarm (temp, color, 0);
  color = "olive";
  sprintf (temp, "Pkt:%d ", upk_int (&buffer[14]));
  dsp9b_alarm (temp, color, 1);
}
int upk_mro_disp (unsigned char *buffer)
{
  int i;
  int slen;
  char temp[128];
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
  dsp9b_alarm (temp, "teal", 1);
}

  /********** UPKT **********/
int upkt_1170 (struct CDS_buffer *buffer)
{
  int i;

  for (i = 0; i < 8; i++) {
    if (upk_1170_test (buffer->packet.housekeeping.micro_packet[i])) {
      upk_1170_disp (buffer->packet.housekeeping.micro_packet[i]);
    } else if (upk_117A_test (buffer->packet.housekeeping.micro_packet[i])) {
      upk_117A_disp (buffer->packet.housekeeping.micro_packet[i]);
    } else if (upk_mro_test (buffer->packet.housekeeping.micro_packet[i])) {
      upk_mro_disp (buffer->packet.housekeeping.micro_packet[i]);
    }
  }
  return 1;
}
int upkt_25B0 (struct CDS_buffer *buffer)
{
  int i;

  for (i = 0; i < 8; i++) {
    if (upk_25B0_test (buffer->packet.housekeeping.micro_packet[i]))
      upk_25B0_disp (buffer->packet.housekeeping.micro_packet[i]);
  }
  return 1;
}
int upkt_27E0 (struct CDS_buffer *buffer)
{
  int i;

  for (i = 0; i < 8; i++) {
    if (upk_27E0_test (buffer->packet.housekeeping.micro_packet[i]))
      upk_27E0_disp (buffer->packet.housekeeping.micro_packet[i]);
  }
  return 1;
}
int upkt_BFD1 (struct CDS_buffer *buffer)
{
  int i;

  for (i = 0; i < 8; i++) {
    if (upk_BFD1_test (buffer->packet.housekeeping.micro_packet[i]))
      upk_BFD1_disp (buffer->packet.housekeeping.micro_packet[i]);
  }
  return 1;
}



int dsp9c_micro (struct CDS_buffer *buffer)
{
  /*
   * "--------                            --------" 
   */
  dsp9b_alarm ("<u>            </u>Micro Packet Monitor<u>            </u>",
               "blue", 1);

            /*********************************/
  upkt_1170 (buffer);
  upkt_BFD1 (buffer);
  upkt_27E0 (buffer);
  upkt_25B0 (buffer);
  return 1;
}

int dsp9c_color_key (void)
{
  int index = 0;

  while (dsp9c_color_key_table[index]) {
    printf ("%s\n", dsp9c_color_key_table[index]);
    index++;
  }
  return index;
}
