 /*
  * rpws_alarm.c   
  */
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>



/****************************************************************************/
/* Compiled in config file directory locations */


#ifndef CFG
#error Compiled in default configuration directory is missing.
#endif


/****************************************************************************/
/* Tables */

static char *alarmfile = { CFG "/alarm_limits.tab" };
  
static int first_flag = 1;
static char *text[] = { "", "YEL_LO", "YEL_HI", "RED_LO", "RED_HI" };
struct ALARM_LIMITS
{
  char channel[16];
  char mnemonic[16];
  float red_low;
  float red_high;
  float yellow_low;
  float yellow_high;
} alarm_table[] = {
  {
  "I-HFR", "S1421", 175.00, 220.00, 180.00, 215.00,}, {
  "I-ME02", "S1420", 90.00, 110.00, 94.00, 106.00,}, {
  "I-LP", "S1423", 38.00, 47.00, 40.00, 45.00,}, {
  "I-ME01", "S1422", 180.00, 350.00, 200.00, 333.00,}, {
  "V-HFR_+5", "S1425", 4.90, 5.60, 5.05, 5.4,}, {
  "V-HFR_+6", "S1424", 5.60, 6.10, 5.70, 6.0,}, {
  "V-ME02_+12", "S1427", 11.20, 12.60, 11.40, 12.40,}, {
  "V-ME02_+6", "S1426", 5.65, 6.45, 5.80, 6.30,}, {
  "V-ME02_+5", "S1429", 4.95, 5.65, 5.10, 5.50,}, {
  "V-LP _+45", "S1428", 45.00, 54.00, 47.00, 52.00,}, {
  "V-ME01_+12", "S1431", 11.30, 12.50, 11.50, 12.30,}, {
  "V-ME01_+5", "S1430", 4.80, 5.40, 4.90, 5.20,}, {
  "V-HFR_-6", "S1433", -6.25, -5.60, -6.15, -5.70,}, {
  "V-ME02_-12", "S1432", -12.20, -11.30, -12.00, -11.50,}, {
  "V-ME02_-6", "S1435", -6.50, -5.60, -6.30, -5.80,}, {
  "V-LP_-45", "S1434", -51.80, -41.80, -50.00, -44.00,}, {
  "T_HFR", "", 10.0, 40.0, 20.0, 30.0,}, {
  "T_AN1", "", -20.0, 40.0, 0.0, 30.0,}, {
  "T_AN2", "", -20.0, 40.0, 0.0, 30.0,}, {
  "T_AN3", "", -20.0, 40.0, 0.0, 30.0,}, {
  "T_SC", "", -65.0, 50.0, -63.0, 40.0,}, {
  "HALT", "", 50.0, 99.0, 60.0, 90.0,}, {
  "", "", 0.0, 0.0, 0.0, 0.0}
};


void rpws_alarm_table (FILE * fd)
{
  char buf[128];

  while (fgets (buf, 127, fd)) {
  }
}

char *rpws_alarm (char *buf)
{
  FILE *fd;
  char *temp;
  int i;
  int alarm_flag;
  int len;
  float reading[22];
  float limit;
  static char result_buffer[128];
  char temp_buffer[64];

  result_buffer[0] = 0;
  if (first_flag) {
    fd = fopen (alarmfile, "r");
    if (fd) {
      rpws_alarm_table (fd);
      fclose (fd);
    }
    first_flag = 0;
  }

  temp = buf;
  for (i = 0; i < 3; i++)
    temp = strchr (temp, ',') + 1;

  for (i = 0; i < 22; i++) {
    reading[i] = strtod (temp, &temp);
    temp++;
  }

  for (i = 0; i < 22; i++) {
    alarm_flag = 0;
    if (reading[i] <= alarm_table[i].yellow_low) {
      alarm_flag = 1;
    }
    if (reading[i] >= alarm_table[i].yellow_high) {
      alarm_flag = 2;
    }
    if (reading[i] <= alarm_table[i].red_low) {
      alarm_flag = 3;
    }
    if (reading[i] >= alarm_table[i].red_high) {
      alarm_flag = 4;
    }
    if (alarm_flag) {
      sprintf (temp_buffer, "%s %s(%s)%.3f, ",
               text[alarm_flag],
               alarm_table[i].channel, alarm_table[i].mnemonic, reading[i]);
      strcat (result_buffer, temp_buffer);
    }
  }
  len = strlen (result_buffer);
  if (len) {
    result_buffer[len - 2] = 0;
    return result_buffer;
  } else
    return NULL;
}
