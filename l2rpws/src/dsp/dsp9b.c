#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>

/* Cassini Stuff */
#include <rtiu.h>
#include <utilt.h>
#include <util.h>

/* Local Stuff: */
int dsp9c_micro (struct CDS_buffer *buffer);    /* From dsp9c */

struct LIMIT
{
  float red_lower;
  float red_upper;
  float yellow_lower;
  float yellow_upper;
};
struct HRMNSC
{
  int hours;
  int minutes;
  int seconds;
  int milliseconds;
};

char *dsp9b_title = { "(dsp9b) 1.6" };

static int red_alarm_flag = 0;
static int yellow_alarm_flag = 0;

struct LIMIT limit_lrp[] = {            /* Flight Model */
  0.0, 0.0, 0.0, 0.0,                   /*  0 MFR A  */
  0.0, 0.0, 0.0, 0.0,                   /*  1 MFR B  */
  0.0, 0.0, 0.0, 0.0,                   /*  2 MFR C  */
  10.0, 40.0, 20.0, 30.0,               /*  3 P/S T  */
  0.0, 0.0, 0.0, 0.0,                   /*  4 MTR I  */
  0.0, 0.0, 0.0, 0.0,                   /*  5 IEB Error  */
  0.0, 0.0, 0.0, 0.0,                   /*  6 IEB Valid  */
  -20.0, 40.0, 0.0, 30.0,               /*  7 MTR T  */
  -20.0, 40.0, 0.0, 30.0,               /*  8 MTR T  */
  -20.0, 40.0, 0.0, 30.0,               /*  9 MTR T  */
  0.0, 0.0, 0.0, 0.0,                   /* 10 0      */
  -70.0, 50.0, -65.0, 40.0,             /* 11 S/C T  */
  0.0, 0.0, 0.0, 0.0,                   /* 12 POS X  */
  0.0, 0.0, 0.0, 0.0,                   /* 13 POS Y  */
  0.0, 0.0, 0.0, 0.0,                   /* 14 POS Z  */
  50.0, 99.0, 60.0, 90.0,               /* 15 HALT  */
  /*
   * Flight Spare 
   */
  0.0, 0.0, 0.0, 0.0,                   /*  0 MFR A  */
  0.0, 0.0, 0.0, 0.0,                   /*  1 MFR B  */
  0.0, 0.0, 0.0, 0.0,                   /*  2 MFR C  */
  10.0, 40.0, 20.0, 30.0,               /*  3 P/S T  */
  0.0, 0.0, 0.0, 0.0,                   /*  4 MTR I  */
  0.0, 0.0, 0.0, 0.0,                   /*  5 IEB Error  */
  0.0, 0.0, 0.0, 0.0,                   /*  6 IEB Valid  */
  -60.0, -20.0, -59.0, -30.0,           /*  7 MTR T  */
  -60.0, -20.0, -59.0, -30.0,           /*  8 MTR T  */
  -60.0, -20.0, -59.0, -30.0,           /*  9 MTR T  */
  0.0, 0.0, 0.0, 0.0,                   /* 10 0      */
  90.0, 110.0, 95.0, 105.0,             /* 11 S/C T  */
  0.0, 0.0, 0.0, 0.0,                   /* 12 POS X  */
  0.0, 0.0, 0.0, 0.0,                   /* 13 POS Y  */
  0.0, 0.0, 0.0, 0.0,                   /* 14 POS Z  */
  50.0, 99.0, 60.0, 90.0
};                                      /* 15 HALT  */

struct LIMIT limit_hfr[] = {            /* Flight Model */
  175.00, 220.00, 180.00, 215.00,       /* HFR  I   */
  90.00, 110.00, 94.00, 106.00,         /* ME02 I   */
  38.00, 47.00, 40.00, 45.00,           /* LP   I   */
  180.00, 350.00, 200.00, 333.00,       /* ME01 I   */
  4.90, 5.60, 5.05, 5.40,               /* HFR  +5  */
  5.60, 6.10, 5.70, 6.00,               /* HFR  +6  */
  11.20, 12.60, 11.40, 12.40,           /* ME02 +12 */
  5.65, 6.45, 5.80, 6.30,               /* ME02 +6  */
  4.95, 5.65, 5.10, 5.50,               /* ME02 +5  */
  45.00, 54.00, 47.00, 52.00,           /* LP   +45 */
  11.30, 12.50, 11.50, 12.30,           /* ME01 +12 */
  4.80, 5.40, 4.90, 5.20,               /* ME01 +5  */
  -6.25, -5.60, -6.15, -5.70,           /* HFR  -6  */
  -12.20, -11.30, -12.00, -11.50,       /* ME02 -12 */
  -6.50, -5.60, -6.30, -5.80,           /* ME02 -6  */
  -51.80, -41.80, -50.00, -44.00,       /* LP   -45 */
  /*
   * Flight Spare 
   */
  150.00, 215.00, 160.00, 205.00,       /* HFR  I   */
  108.00, 148.00, 118.00, 138.00,       /* ME02 I   */
  38.00, 47.00, 40.00, 45.00,           /* LP   I   */
  180.00, 350.00, 200.00, 333.00,       /* ME01 I   */
  4.90, 5.60, 5.05, 5.40,               /* HFR  +5  */
  5.60, 6.10, 5.70, 6.00,               /* HFR  +6  */
  11.20, 12.60, 11.40, 12.40,           /* ME02 +12 */
  5.65, 6.45, 5.80, 6.30,               /* ME02 +6  */
  4.95, 5.65, 5.10, 5.50,               /* ME02 +5  */
  47.50, 52.50, 45.50, 54.50,           /* LP   +45 */
  10.26, 13.26, 10.76, 12.76,           /* ME01 +12 */
  4.80, 5.40, 4.90, 5.20,               /* ME01 +5  */
  -6.25, -5.10, -6.15, -5.20,           /* HFR  -6  */
  -13.00, -11.00, -12.50, -11.50,       /* ME02 -12 */
  -6.50, -5.60, -6.30, -5.80,           /* ME02 -6  */
  -56.25, -45.00, -53.25, -49.00
};                                      /* LP   -45 */

float chdo_float (unsigned char *si)
{
  union
  {
    unsigned char s[4];
    float f;
  } t;

  t.s[0] = si[0];
  t.s[1] = si[1];
  t.s[2] = si[2];
  t.s[3] = si[3];
  return t.f;
}
char *chdo_dump_antenna (unsigned char ant)
{
  int i;
  unsigned char antt;
  static char *antenna[] = { "26M ",
    "34M STD",
    "34M hef",
    "Un ",
    "70M ",
    "34M BWG",
    "34M HSB",
    "Un "
  };
  static char temp[65];

  antt = ant;
  temp[0] = 0;
  for (i = 0; i < 8; i++) {
    if (antt & 0x80) {
      strcat (temp, antenna[i]);
      strcat (temp, ",");
    }
    antt = antt << 1;
  }
  temp[strlen (temp) - 1] = 0;
  return temp;                          /* removed % */
}
char *chdo_dump_decode (struct CHDO_type_92 *buffer)
{
  static char temp[128];
  static char *decode_status[] = { "undecoded",
    "N/A      ",
    "invalid  ",
    "valid    ",
    "corrected",
    "error    ",
    "error    ",
    "error    "
  };
  static char *decode_method[] = { "off",
    "R/S"
  };

  sprintf (temp, "%s  %s",
           decode_method[buffer->decode_method & 0x01],
           decode_status[buffer->decode_status & 0x07]
    );
  return temp;
}
int dsp9b_alarm (char *buffer, char *color, int flag)
{

  if (color)
    if (color[0]) {
      if (strcmp (color, "yellow") == 0)
        yellow_alarm_flag = 1;
      if (strcmp (color, "red") == 0)
        red_alarm_flag = 1;
      printf ("<font color=%s>", color);
    }

  printf (buffer);

  if (color)
    if (color[0]) {
      printf ("</font>");
    }

  if (flag)
    printf ("\n");

}

char *antl_disp (int flags)
{
  static char result[32];

  memset (result, 0, 48);
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
char *power_disp (int flags)
{
  static char result[48];

  memset (result, 0, 48);
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
  return result;
}
char *disc_disp (int flags)
{
  static char result[48];

  memset (result, 0, 48);
  if (flags & 0x80)
    strcat (result, "WPD ");
  if (flags & 0x40)
    strcat (result, "MAINT ");
  if (flags & 0x20)
    strcat (result, "EX+ ");
  if (flags & 0x10)
    strcat (result, "EZ ");
  if (flags & 0x08)
    strcat (result, "EX- ");
  if ((flags & 0x04))
    strcat (result, "RUN ");
  else
    strcat (result, "SLEEP ");
  if (flags & 0x02)
    strcat (result, "WDT ");
  if (flags & 0x01)
    strcat (result, "RESET ");
  return result;
}
char *misc_disp (int flags)
{
  static char result[128];

  memset (result, 0, 48);
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
  return result;
}
char *sts_disp (int flags)
{
  static char result[64];

  memset (result, 0, 48);
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
char *p8155_disp (int flags)
{
  static char result[32];

  memset (result, 0, 48);
  if (flags & 0x04)
    strcat (result, " DAC");
  if (flags & 0x02)
    strcat (result, " ADC");
  if (flags & 0x01)
    strcat (result, " PWR");
  return result;
}

float eng_conv (int count)
{
  float k = 5.00 / 255;
  float c;

  c = count;
  return c * k;
}
float eng3_conv (unsigned char *count, int index, char **flag, int epoch)
{
  float k = 5.00 / 255;
  float vlrp, result;
  static int icnt = 0;
  int offset = 16;

  /*
   * lower RED upper   lower YEL upper                        
   */

  if (epoch)
    offset = 0;
  *flag = "";
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
     if (result >= limit_hfr[index + offset].yellow_upper)
       *flag = "yellow";
     if (result <= limit_hfr[index + offset].yellow_lower)
       *flag = "yellow";
     if (result >= limit_hfr[index + offset].red_upper)
       *flag = "red";
     if (result <= limit_hfr[index + offset].red_lower)
       *flag = "red";
     break;
   default:
     *flag = "";
     break;
  }
  return result;
}

float eng3a_conv (unsigned char *count, int index, char **flag, int epoch)
{
  float result;
  float val;
  int offset = 16;

  /*
   * lower RED upper   lower YEL upper                 
   */
  if (epoch)
    offset = 0;
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
     if (result >= limit_lrp[index + offset].yellow_upper)
       *flag = "yellow";
     if (result <= limit_lrp[index + offset].yellow_lower)
       *flag = "yellow";
     if (result >= limit_lrp[index + offset].red_upper)
       *flag = "red";
     if (result <= limit_lrp[index + offset].red_lower)
       *flag = "red";
     break;
  }
  return result;
}



int dsp9b_digital_HRP (struct CDS_buffer *buffer, int epoch)
{
  char temp[256];
  char *color;

  /*
   * "--------                            --------" 
   */
  dsp9b_alarm ("<u>         </u>HRP Digital Status Monitor<u>         </u>",
               "blue", 1);

            /*********************************/
  color = "";
  sprintf (temp, " P8155 [%2.2X] %s",
           buffer->packet.rom_housekeeping.LP_P8155[0],
           p8155_disp (buffer->packet.rom_housekeeping.LP_P8155[0]));
  switch (buffer->packet.rom_housekeeping.LP_P8155[0]) {
   case 0xC1:
   case 0xC3:
     break;
   default:
     color = "yellow";
     break;
  }
  dsp9b_alarm (temp, color, 1);

            /*********************************/
  color = "";

  sprintf (temp, " LP MUX [%2.2X] ",
           buffer->packet.rom_housekeeping.LP_MUX_0[0]);
  dsp9b_alarm (temp, color, 0);

  sprintf (temp, " DAC-0 [%2.2X] ",
           buffer->packet.rom_housekeeping.LP_Probe_Bias[0]);
  dsp9b_alarm (temp, color, 0);

  sprintf (temp, " DAC-1 [%2.2X] ",
           buffer->packet.rom_housekeeping.LP_DAC1_Bias[0]);
  dsp9b_alarm (temp, color, 1);

}

int dsp9b_digital_LRP (struct CDS_buffer *buffer, int epoch)
{
  char temp[256];
  char *color;

  /*
   * "--------                            --------" 
   */
  dsp9b_alarm ("<u>         </u>LRP Digital Status Monitor<u>         </u>",
               "blue", 1);

            /*********************************/

  color = "";
  sprintf (temp, " BIU disc [%2.2X] %s ",
           buffer->packet.rom_housekeeping.BIU_Discrete_Command[0],
           disc_disp (buffer->packet.rom_housekeeping.
                      BIU_Discrete_Command[0]));
  if (buffer->packet.rom_housekeeping.BIU_Discrete_Command[0] & 0xFB)
    color = "yellow";
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, " Pwr Cntl [%2.2X]%s ",
           buffer->packet.rom_housekeeping.Power_Status[0],
           power_disp (buffer->packet.rom_housekeeping.Power_Status[0]));
  if (buffer->packet.rom_housekeeping.Power_Status[0] & 0xF8)
    color = "red";
  if ((buffer->packet.rom_housekeeping.Power_Status[0] & 0x7) != 0x07)
    color = "yellow";
  dsp9b_alarm (temp, color, 1);


  color = "";
  sprintf (temp, " BIU_DT %d ",
           buffer->packet.rom_housekeeping.LRP_analog[10]);
  if (buffer->packet.rom_housekeeping.LRP_analog[10])
    color = "red";
  /*
   * this doesn't seem right... 
   */

            /*********************************/

  color = "";
  sprintf (temp, " \" Status [%2.2X] %s  ",
           buffer->packet.rom_housekeeping.BIU_Discrete_Status[0],
           sts_disp (buffer->packet.rom_housekeeping.BIU_Discrete_Status[0]));
  if (buffer->packet.rom_housekeeping.BIU_Discrete_Status[0])
    color = "yellow";
  dsp9b_alarm (temp, color, 0);

  sprintf (temp, " Misc [%2.2X] %s",
           buffer->packet.rom_housekeeping.BIU_Discrete_misc[0],
           misc_disp (buffer->packet.rom_housekeeping.BIU_Discrete_misc[0]));
  if (buffer->packet.rom_housekeeping.BIU_Discrete_misc[0] & 0xC9)
    color = "yellow";
  dsp9b_alarm (temp, color, 1);

  return 1;
}
int dsp9b_analog_ANT (struct CDS_buffer *buffer, int epoch)
{
  char temp[256];
  char *color;

  /*
   * "--------                            --------" 
   */
  dsp9b_alarm ("<u>          </u>Deploy Mechanism Monitor<u>          </u>",
               "blue", 1);

            /*********************************/
  color = "";
  sprintf (temp, " Antenna ");
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, "I %.3fV  ",
           eng_conv (buffer->packet.rom_housekeeping.LRP_analog[4]));
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, "Limit [%2.2X] %s ",
           buffer->packet.rom_housekeeping.Antenna_Status[0],
           antl_disp (buffer->packet.rom_housekeeping.Antenna_Status[0]));
  if (epoch) {
    if (buffer->packet.rom_housekeeping.Antenna_Status[0] != 0x2A)
      color = "red";
  } else {
    if (buffer->packet.rom_housekeeping.Antenna_Status[0] != 0x3F)
      color = "red";
  }
  dsp9b_alarm (temp, color, 1);

            /*********************************/

  color = "";
  sprintf (temp, " Antenna Position ");
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, "+X[%d] ", buffer->packet.rom_housekeeping.LRP_analog[12]);
  if (epoch) {
    if (buffer->packet.rom_housekeeping.LRP_analog[12] < 187)
      color = "yellow";
    if (buffer->packet.rom_housekeeping.LRP_analog[12] > 193)
      color = "yellow";
    if (buffer->packet.rom_housekeeping.LRP_analog[12] < 184)
      color = "red";
    if (buffer->packet.rom_housekeeping.LRP_analog[12] > 196)
      color = "red";
  }
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, "-X[%d] ", buffer->packet.rom_housekeeping.LRP_analog[13]);
  if (epoch) {
    if (buffer->packet.rom_housekeeping.LRP_analog[13] < 190)
      color = "yellow";
    if (buffer->packet.rom_housekeeping.LRP_analog[13] > 196)
      color = "yellow";
    if (buffer->packet.rom_housekeeping.LRP_analog[13] < 187)
      color = "red";
    if (buffer->packet.rom_housekeeping.LRP_analog[13] > 199)
      color = "red";
  }
  dsp9b_alarm (temp, color, 0);
  color = "";
  sprintf (temp, "+Z[%d] ", buffer->packet.rom_housekeeping.LRP_analog[14]);
  if (epoch) {
    if (buffer->packet.rom_housekeeping.LRP_analog[14] < 182)
      color = "yellow";
    if (buffer->packet.rom_housekeeping.LRP_analog[14] > 187)
      color = "yellow";
    if (buffer->packet.rom_housekeeping.LRP_analog[14] < 179)
      color = "red";
    if (buffer->packet.rom_housekeeping.LRP_analog[14] > 191)
      color = "red";
  }
  dsp9b_alarm (temp, color, 1);

}

int dsp9b_analog_LRP (struct CDS_buffer *buffer, int epoch)
{
  char temp[256];
  char *color;

  /*
   * "--------                            --------" 
   */
  dsp9b_alarm ("<u>            </u>Temperature Monitor<u>             </u>",
               "blue", 1);

        /*********************************/

  color = "";
  sprintf (temp, " P/S %2.1f ",
           eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, 3, &color,
                       epoch));
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, " S/C %2.1f ",
           eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, 11, &color,
                       epoch));
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, " Motors");
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, " %2.1f",
           eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, 7, &color,
                       epoch));
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, " %2.1f",
           eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, 8, &color,
                       epoch));
  dsp9b_alarm (temp, color, 0);
  color = "";
  sprintf (temp, " %2.1f",
           eng3a_conv (buffer->packet.rom_housekeeping.LRP_analog, 8, &color,
                       epoch));
  dsp9b_alarm (temp, color, 1);


}

int dsp9b_analog_HFR (struct CDS_buffer *buffer, int epoch)
{
  char temp[256];
  char *color;

  /*
   * "--------                            --------" 
   */
  dsp9b_alarm ("<u>            </u>Power Supply Monitor<u>            </u>",
               "blue", 1);

        /*********************************/
  sprintf (temp, " HFR %5.1fmA ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 0, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, " +5:%2.2fV ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 4, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, " +6:% 2.2fV ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 5, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, " -6:%2.2fV ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 12, &color,
                      epoch));

  dsp9b_alarm (temp, color, 1);

        /*********************************/
  sprintf (temp, " LRP %5.1fmA ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 3, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, " +5:%2.2fV ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 11, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "+12:%2.1f V",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 10, &color,
                      epoch));
  dsp9b_alarm (temp, color, 1);

        /*********************************/
  sprintf (temp, " MFR %5.1fmA ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 1, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, " +5:%2.2fV ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 8, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, " +6: %2.2fV ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 7, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "+12: %2.1fV ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 6, &color,
                      epoch));
  dsp9b_alarm (temp, color, 1);

  sprintf (temp, "                        ");
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "-6:%2.2fV ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 14, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "-12:%2.1fV ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 13, &color,
                      epoch));
  dsp9b_alarm (temp, color, 1);

        /*********************************/
  sprintf (temp, " LP  %5.1fmA           ",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 2, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, "+45:%2.1f V",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 9, &color,
                      epoch));
  dsp9b_alarm (temp, color, 0);
  sprintf (temp, " -45:%2.1fV",
           eng3_conv (buffer->packet.rom_housekeeping.HFR_analog, 15, &color,
                      epoch));
  dsp9b_alarm (temp, color, 1);


}

int dsp9b_command (struct CDS_buffer *buffer, int epoch)
{
  int i;
  char temp[256];
  char *color;
  char *mask_id = { "" };

  char *Load_sts2[] = { "IEB Mem Empty    ",
    "IEB Mem Load Fail",
    "IEB Mem Loaded   ",
    "IEB Mem Loaded(F)"
  };

  /*
   * "--------                            --------" 
   */
  dsp9b_alarm ("<u>              </u>Command Counters<u>              </u>",
               "blue", 1);

        /*********************************/
  color = "";
  sprintf (temp, " Cmd Count [%d] ",
           UTIL_short_to_MSB (buffer->packet.rom_housekeeping.
                              Command_Byte_Count));
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, " Valid [%d] ",
           buffer->packet.rom_housekeeping.Valid_Command_Count[0]);
  dsp9b_alarm (temp, color, 0);

  sprintf (temp, " Invalid[%d] ",
           buffer->packet.rom_housekeeping.Invalid_Command_Count[0]);
  color = "";
  if (buffer->packet.rom_housekeeping.Invalid_Command_Count[0])
    color = "red";
  dsp9b_alarm (temp, color, 1);

        /*********************************/
  if (0x00 == buffer->packet.rom_housekeeping.Time_Tag[1])
    mask_id = "ID  ";
  if (0xFF == buffer->packet.rom_housekeeping.Time_Tag[1])
    mask_id = "MASK";

  i = (buffer->packet.rom_housekeeping.LRP_analog[5] >> 7) & 0x01;
  i |= (buffer->packet.rom_housekeeping.LRP_analog[6] >> 6) & 0x02;

  color = "";
  sprintf (temp, " IEB %s [%02X %02X]",
           mask_id,
           buffer->packet.rom_housekeeping.Time_Tag[0],
           buffer->packet.rom_housekeeping.Time_Tag[1]);
  dsp9b_alarm (temp, color, 0);

  sprintf (temp, " Bad-%d[%d] ",
           (buffer->packet.rom_housekeeping.LRP_analog[5] >> 7) & 0x01,
           buffer->packet.rom_housekeeping.LRP_analog[5] & 0x7F);
  color = "";
  if ((buffer->packet.rom_housekeeping.LRP_analog[5] >> 7) & 0x01)
    color = "red";
  dsp9b_alarm (temp, color, 0);

  sprintf (temp, "Good-%d[%d] %s        ",
           (buffer->packet.rom_housekeeping.LRP_analog[6] >> 7) & 0x01,
           buffer->packet.rom_housekeeping.LRP_analog[6] & 0x7F,
           Load_sts2[i]);
  color = "";
  if (i == 1)
    color = "red";
  dsp9b_alarm (temp, color, 1);


        /*********************************/
  return 1;

}

int dsp9b_processor (struct CDS_buffer *buffer, int epoch)
{
  long int cds[6];
  int index;
  int ihalt;
  float fhalt;
  char temp[256];
  char *color;
  int offset = 16;

  if (epoch)
    offset = 0;

  /*
   * Don't do this! cds is an integer, but the routine expects a long
   * you can nuke memory: -cwp 
   */
  UTIL_extract_CDS (buffer, cds);

  /*
   * "--------                            --------" 
   */
  dsp9b_alarm ("<u>          </u>Processor Health Monitor<u>          </u>",
               "blue", 1);

        /*********************************/
  color = "";
  switch (cds[0]) {
   case 0x0A90:
     sprintf (temp, " ROM Software      ");
     color = "yellow";
     break;
   case 0x0A30:
     sprintf (temp, " Deploy Software   ");
     color = "red";
     break;
   case 0x0A95:
     sprintf (temp, " Science Software  ");
     break;
   default:
     sprintf (temp, " Housekeeping FAULT");
     color = "red";
     break;
  }
  dsp9b_alarm (temp, color, 1);

        /*********************************/
  ihalt = buffer->packet.rom_housekeeping.LRP_analog[15];
  fhalt = 100. - (ihalt * 100.) / 255.;
  sprintf (temp, " HALT%3.0f%%%%  ", fhalt);
  index = 15;
  color = "";
  if (fhalt >= limit_lrp[index + offset].yellow_upper)
    color = "yellow";
  if (fhalt <= limit_lrp[index + offset].yellow_lower)
    color = "yellow";
  if (fhalt >= limit_lrp[index + offset].red_upper)
    color = "red";
  if (fhalt <= limit_lrp[index + offset].red_lower)
    color = "red";
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, "Loop[%03d]  ",
           buffer->packet.rom_housekeeping.Command_Loop_Count[0]);
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, " BIU_rst %d ",
           buffer->packet.rom_housekeeping.BIU_Reset_Count[0]);
  if (buffer->packet.rom_housekeeping.BIU_Reset_Count[0])
    color = "red";
  dsp9b_alarm (temp, color, 0);

  color = "";
  sprintf (temp, " BIU_RTI %d ",
           buffer->packet.rom_housekeeping.BIU_RTI_Status[0]);
  if (buffer->packet.rom_housekeeping.BIU_RTI_Status[0])
    color = "red";
  dsp9b_alarm (temp, color, 1);

}
int dsp9b_rti (int clock, int rti)
{
  int i;

  return ((clock & 0x1FFF) << 3) | (rti & 0x03);
}
int dsp9b_clock (struct CDS_buffer *buffer, int refresh_period)
{
  char temp[256];
  long int cds[6];
  time_t cds_time, ut_time;
  struct event_clock evt_clk;
  struct tm *pkt_scet;
  struct tm *pkt_ert;
  struct tm *pkt_rct;
  struct tm pkt_ut;
  struct tm pkt_lcl;
  struct tm pkt_transit;
  struct tm pkt_process;
  struct event_time *evt_tim;
  struct event_time ert_tim;
  struct event_time rct_tim;
  struct event_time owlt_tim;
  struct HRMNSC owlt = { 0, 0, 0, 0 };
  int epoch;
  char *color = NULL;
  int flag_yday = 1;

  ut_time = time (NULL);

  memcpy (&pkt_ut, gmtime (&ut_time), sizeof (struct tm));

  memcpy (&pkt_lcl, localtime (&ut_time), sizeof (struct tm));

  epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
    (buffer->packet.cds_tag.epoch[1] << 16) |
    (buffer->packet.cds_tag.epoch[2] << 8) |
    (buffer->packet.cds_tag.epoch[3] << 0);

  /*
   * Don't do this! cds is an integer, but the routine expects a long
   * you can nuke memory: -cwp 
   */
  UTIL_extract_CDS (buffer, cds);
  cds_time = (time_t) UTIL_extract_TIME (buffer);

  evt_clk.seconds = cds_time;
  evt_clk.fine = UTIL_extract_RTI (buffer) << 5;
  evt_tim = UTIL_event_scet ((struct MP_buffer *) buffer, evt_clk);

  pkt_scet = UTIL_event_scet_tm (*evt_tim, 0);

  ert_tim.days = buffer->packet.chdo_tag.ert.days;
  ert_tim.milliseconds = buffer->packet.chdo_tag.ert.milliseconds;
  pkt_ert = UTIL_event_scet_tm (ert_tim, 0);

  rct_tim.days = buffer->packet.chdo_tag.rct.days;
  rct_tim.milliseconds = buffer->packet.chdo_tag.rct.milliseconds;
  pkt_rct = UTIL_event_scet_tm (rct_tim, 0);

  owlt_tim.milliseconds = ert_tim.milliseconds - evt_tim->milliseconds;
  owlt_tim.days = ert_tim.days - evt_tim->days;
  if (owlt_tim.milliseconds < 0) {
    owlt_tim.days -= 1;
    owlt_tim.milliseconds += 86400000;
  }
  if (!owlt_tim.days) {
    owlt.milliseconds = owlt_tim.milliseconds % 1000;
    owlt.seconds = owlt_tim.milliseconds / 1000;
    owlt.minutes = owlt.seconds / 60;
    owlt.minutes = owlt.minutes % 60;
    owlt.hours = owlt.seconds / 3600;
    owlt.seconds = owlt.seconds % 60;
  }

/*
	local - ert
 */
  pkt_transit.tm_sec = pkt_ut.tm_sec - pkt_ert->tm_sec;
  pkt_transit.tm_min = pkt_ut.tm_min - pkt_ert->tm_min;
  pkt_transit.tm_hour = pkt_ut.tm_hour - pkt_ert->tm_hour;
  pkt_transit.tm_yday = pkt_ut.tm_yday - pkt_ert->tm_yday;

  if (pkt_transit.tm_sec < 0) {
    pkt_transit.tm_sec += 60;
    pkt_transit.tm_min -= 1;
  }
  if (pkt_transit.tm_sec > 60) {
    pkt_transit.tm_sec -= 60;
    pkt_transit.tm_min += 1;
  }

  if (pkt_transit.tm_min < 0) {
    pkt_transit.tm_min += 60;
    pkt_transit.tm_hour -= 1;
  }
  if (pkt_transit.tm_min > 60) {
    pkt_transit.tm_min -= 60;
    pkt_transit.tm_hour += 1;
  }
  pkt_transit.tm_hour += pkt_transit.tm_yday * 24;

  pkt_process.tm_sec = pkt_rct->tm_sec - pkt_ert->tm_sec;
  pkt_process.tm_min = pkt_rct->tm_min - pkt_ert->tm_min;
  pkt_process.tm_hour = pkt_rct->tm_hour - pkt_ert->tm_hour;
  pkt_process.tm_yday = pkt_rct->tm_yday - pkt_ert->tm_yday;

  if (pkt_process.tm_sec < 0) {
    pkt_process.tm_sec += 60;
    pkt_process.tm_min -= 1;
  }
  if (pkt_process.tm_sec > 60) {
    pkt_process.tm_sec -= 60;
    pkt_process.tm_min += 1;
  }

  if (pkt_process.tm_min < 0) {
    pkt_process.tm_min += 60;
    pkt_process.tm_hour -= 1;
  }
  if (pkt_process.tm_min > 60) {
    pkt_process.tm_min -= 60;
    pkt_process.tm_hour += 1;
  }
  pkt_process.tm_hour += pkt_process.tm_yday * 24;

  if (pkt_transit.tm_min)
    color = "red";
  if (pkt_transit.tm_hour)
    color = "yellow";
  if (pkt_transit.tm_yday)
    color = "brown";
  if (epoch)
    if (color)
      printf ("<font color=%s>", color);

  if (epoch)
    printf ("  UT:    %04d-%03dT%2.2d:%2.2d:%2.2d ",
            pkt_ut.tm_year + 1900,
            pkt_ut.tm_yday + flag_yday,
            pkt_ut.tm_hour, pkt_ut.tm_min, pkt_ut.tm_sec);
  else
    printf ("LOCAL:   %04d-%03dT%2.2d:%2.2d:%2.2d ",
            pkt_lcl.tm_year + 1900,
            pkt_lcl.tm_yday + flag_yday,
            pkt_lcl.tm_hour, pkt_lcl.tm_min, pkt_lcl.tm_sec);
  if (pkt_transit.tm_hour < 24)
    printf ("(-%02d:%02d:%02d) ",
            pkt_transit.tm_hour, pkt_transit.tm_min, pkt_transit.tm_sec);
  else
    printf ("   ");
  if (refresh_period > 0)
    printf ("  Ref:%2d\n", refresh_period);
  else
    printf ("NO REFRESH\n");
  if (pkt_ert->tm_year > 100) {
    printf (" ERT:    %04d-%03dT%2.2d:%2.2d:%2.2d.%3.3d\n",
            pkt_ert->tm_year + 1900,
            pkt_ert->tm_yday + flag_yday,
            pkt_ert->tm_hour,
            pkt_ert->tm_min, pkt_ert->tm_sec, ert_tim.milliseconds % 1000);

    if (pkt_process.tm_yday)
      color = "yellow";
    if (pkt_process.tm_yday > 2)
      color = "red";
    if (epoch)
      if (color)
        printf ("<font color=%s>", color);
    printf (" RCT:    %04d-%03dT%2.2d:%2.2d:%2.2d.%3.3d\n",
            pkt_rct->tm_year + 1900,
            pkt_rct->tm_yday + flag_yday,
            pkt_rct->tm_hour,
            pkt_rct->tm_min, pkt_rct->tm_sec, rct_tim.milliseconds % 1000);
    if (epoch)
      if (color)
        printf ("</font>");
  }
  if (epoch)
    printf ("SCET:    %04d-%03dT%2.2d:%2.2d:%2.2d.%3.3d FM\n",
            pkt_scet->tm_year + 1900,
            pkt_scet->tm_yday + flag_yday,
            pkt_scet->tm_hour,
            pkt_scet->tm_min, pkt_scet->tm_sec, evt_tim->milliseconds % 1000);
  else {
    cftime (temp, "%Y-%jT%T FS", &cds_time);
    printf ("SCET:    %s\n", temp);
  }
  if (epoch)
    if (color)
      printf ("</font>");
  printf ("SCLK:    %8.8X.%X  RTI:%4.4X  ",
          cds_time,
          UTIL_extract_RTI (buffer),
          dsp9b_rti (cds_time, UTIL_extract_RTI (buffer))
    );
  if (!owlt_tim.days)
    if (owlt.hours < 3)
      printf ("OWLT:%02d:%02d:%02d", owlt.hours, owlt.minutes, owlt.seconds);
  printf ("\n");
  printf ("CCSDS:   %4.4X %4.4X %4.4X %4.4X %4.4X %4.4X\n",
          cds[0], cds[1], cds[2], cds[3], cds[4], cds[5]);
  printf ("Packet Sequence: %5d    VC %d",
          UTIL_extract_CDS_sequence (buffer),
          buffer->packet.chdo_ancillary.type_92.virtual_channel_id);
  printf ("\n");
  return epoch;
}

int dsp9b_chdo_92 (struct CHDO_type_92 *type_92)
{
  /*
   * "--------                            --------" 
   */
  dsp9b_alarm ("<u>            </u>CHDO Type 92 Monitor<u>            </u>",
               "blue", 1);

  printf ("Receiving Antenna %s",
          chdo_dump_antenna (type_92->master_antenna));
  printf ("\n");
  printf ("Error Decode      %s", chdo_dump_decode (type_92));
  printf ("\n");
  printf ("Bit Rate          %.0f b/s", chdo_float (type_92->bit_rate));
  printf ("\n");
  printf ("Signal Level      %.0f dBm", chdo_float (type_92->signal_level));
  printf ("\n");
}
int dsp9b (char *filename, int refresh_period)
{
  struct CDS_buffer *buffer;
  int recentF;
  int ilen;
  int epoch;

  buffer = (struct CDS_buffer *) malloc (16384);

  if (!buffer) {
    printf ("<br>Ack, Ack, P-too'ey, says Bill-the-Cat\n");
    return 0;
  }
  recentF = open (filename, O_RDONLY);
  if (recentF) {
    ilen = read (recentF, buffer, 4);
    if (ilen < 1) {
      printf
        ("<br>No recent housekeeping data available at this time <br>\n");
      printf ("(We may not be collecting VC0/VC1 separatly) <p>\n");
      return 0;
    }
    ilen = -1;
    if (buffer->f_length < 16380)
      ilen = read (recentF, &buffer->record_type, buffer->f_length);
    if (ilen < 1) {
      printf
        ("<br>No recent housekeeping data available at this time (B)<br>\n");
      return 0;
    }
    close (recentF);
    red_alarm_flag = 0;
    yellow_alarm_flag = 0;
    printf ("<pre>\n");
    epoch = dsp9b_clock (buffer, refresh_period);
    dsp9b_chdo_92 (&buffer->packet.chdo_ancillary.type_92);
    dsp9b_processor (buffer, epoch);
    dsp9b_analog_ANT (buffer, epoch);
    dsp9b_analog_LRP (buffer, epoch);
    dsp9b_analog_HFR (buffer, epoch);
    dsp9b_command (buffer, epoch);
    dsp9b_digital_LRP (buffer, epoch);
    dsp9b_digital_HRP (buffer, epoch);
    dsp9c_micro (buffer /* , epoch */ );
    printf ("</pre>\n");

/*
         printf("UT line also shows UT-ERT (i.e. data transit time from JPL to Iowa ");
         printf("as well as the web-page pull cycle period.");
*/

/*
	/plasma-wave/cassini/wtr
 */
    printf ("<br>\n");
    if (1) {
      printf ("\n");
    } else if (red_alarm_flag) {
      printf ("\n");
      printf ("<IMG SRC=\"/plasma-wave/cassini/wtr/red_alarm.gif\" \n");
      printf (" ONLOAD=\"PlaySound()\" /IMG> \n");
      printf ("<br>\n");
    } else if (yellow_alarm_flag) {
      printf ("\n");
      printf ("<IMG SRC=\"/plasma-wave/cassini/wtr/yellow_alarm.gif\" \n");
      printf (" ONLOAD=\"PlaySound()\" /IMG> \n");
      printf ("<br>\n");
    } else {
      if (0) {
        printf ("\n");
        printf ("<IMG SRC=\"/plasma-wave/cassini/wtr/phone.gif\" \n");
        printf (" ONLOAD=\"PlaySound(phone)\" /IMG> \n");
        printf ("<br>\n");
      }
    }
  } else {
    printf
      ("<br>No recent housekeeping file available at this time (C)<br>\n");
  }
  return 1;
}
