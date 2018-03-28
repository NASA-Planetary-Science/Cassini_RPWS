#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include "ugpib.h"

#define IbaPAD 0x0001
#define IbaTMO 0x0003

void mon_help (void)
{
  int i = 0;
  char buf[64];
  char *newline = "\n";
  char *blanks = "                ";

  fprintf (stderr, "    mcmd_gpib help table (cmd.mne)\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "       macro$     device, type, params\n");
  fprintf (stderr, "                  device from ibconf table\n");
  fprintf (stderr, "                  type indicates type of command\n");
  fprintf (stderr, "                  params sent to device\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "              type table:\n");
  fprintf (stderr, "              * special parameter substitute rules\n");
  fprintf (stderr, "\n");
}
void mon_ibsta (int ib_sta)
{
  static char *mne[] = { "ERR  ",
    "TIMO ",
    "END  ",
    "SRQI ",
    "RQS  ",
    "CMPL ",
    "LOK  ",
    "REM  ",
    "CIC  ",
    "ATN  ",
    "TACS ",
    "LACS ",
    "DTAS ",
    "DCAS "
  };
  int i;
  int temp;

  temp = ib_sta;
  for (i = 0; i < 16; i++) {
    if (temp & 0x8000)
      fputs (mne[i], stderr);
    temp = temp << 1;
  }
  return;
}
void mon_iberr (int ib_err)
{
  static char *mne[] = { "EDVR ",       /*  0 */
    "ECIC ",                            /*  1 */
    "ENOL ",                            /*  2 */
    "EADR ",                            /*  3 */
    "EARG ",                            /*  4 */
    "ESAC ",                            /*  5 */
    "EABO ",                            /*  6 */
    "ENEB ",                            /*  7 */
    "EDMA ",                            /*  8 */
    "EBTO ",                            /*  9 */
    "",                                 /* 10 */
    "ECAP ",                            /* 11 */
    "EFSO ",                            /* 12 */
    "",                                 /* 13 */
    "EBUS ",                            /* 14 */
    "ESTB ",                            /* 15 */
    "ESRQ ",                            /* 16 */
    "",                                 /* 17 */
    "",                                 /* 18 */
    "",                                 /* 19 */
    "ETAB ",                            /* 20 */
    "",                                 /* 21 */
    "",                                 /* 22 */
    "",                                 /* 23 */
    "",                                 /* 24 */
    "",                                 /* 25 */
    "",                                 /* 26 */
    "",                                 /* 27 */
    "",                                 /* 28 */
    "",                                 /* 29 */
    "",                                 /* 30 */
    ""
  };                                    /* 31 */
  fputs (mne[ib_err & 31], stderr);
}


void mon_cmd (char *device_name, int device_handle, char *type)
{
  int devsts;
  char string[128];
  char crlf[3] = { 0x0D, 0x0A, 0x00 };

  strcpy (string, type);
  strcat (string, crlf);

  devsts = ibwrt (device_handle, string, strlen (string));
  if (devsts & ERR) {
    mon_iberr (iberr);
    mon_ibsta (devsts);
    fprintf (stderr, "0x%X=ibwrt(%s(0x%X),\"%s\",%d)\n",
             devsts, device_name, device_handle, type, strlen (type));
    exit (0);
  }
}

char *mon_read (char *device_name, int device_handle)
{
  static char *msg = { "no reading" };
  int devsts;
  static char fill[8];
  static char string[128];

  memset (string, 0, 128);
  devsts = ibrd (device_handle, string, 16);
  if (devsts & ERR) {
    mon_iberr (iberr);
    mon_ibsta (devsts);
    fprintf (stderr, "0x%X=ibrd(%s(0x%X),\"%s\",%d)\n",
             devsts, device_name, device_handle, string, strlen (string));
    exit (0);
  }

  if (strlen (string) == 0)
    return msg;
  string[strlen (string) - 2] = 0;
  return string;
}
void main (int argc, char *argv[])
{
  int device_handle;
  char *device_name = { "HP3478" };
  char I_[32], V_[32], ACI_[32];

  device_handle = ibfind (device_name);
  if (!device_handle || device_handle < 0) {
    fprintf (stderr,
             "0x%X=ibfind(\"%s\") ARE you running on the correct machine?\n",
             device_handle, device_name);
    exit (0);
  }

  while (1) {
    mon_cmd (device_name, device_handle, "F5");
    strcpy (I_, mon_read (device_name, device_handle));
    mon_cmd (device_name, device_handle, "F1");
    strcpy (V_, mon_read (device_name, device_handle));
    mon_cmd (device_name, device_handle, "F6");
    strcpy (ACI_, mon_read (device_name, device_handle));
    fprintf (stdout, "Current:%s  %s  Voltage:%s\n", I_, ACI_, V_);
  }
}
