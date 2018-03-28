#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include "ugpib.h"

#define TOKEN_MAX 16
#define IbaPAD 0x0001
#define IbaTMO 0x0003
void mcmd_delay (int);

enum
{
  TYPE_INVALID,
  TYPE_TEXT,
  TYPE_TEXTCR,
  TYPE_TERM,
  TYPE_CSI,
  TYPE_CNTL_RESET,
  TYPE_HEX,
  TYPE_ITEXT,
  TYPE_LOCK,
  TYPE_UNLOCK,
  TYPE_FILE,
  TYPE_HEXFILE,
  TYPE_CURVE16,
  TYPE_CURVE16L,
  TYPE_CURVE
};
static struct cmd_type
{
  int type;
  char key[16];
  char *help;
} cmd_type_table[] = {
TYPE_TEXT, "TEXT", "Text line with <newline>",
    TYPE_TEXTCR, "TEXTCR", "Text line with <CR>",
    TYPE_TERM, "TERM", "Terminal text (GPIB-232CT) *",
    TYPE_CSI, "CSI", "ANSI Control Sequence (Esc-[)",
    TYPE_ITEXT, "INIT", "GPIB device initilization",
    TYPE_HEX, "HEX", "Binary device control string",
    TYPE_CNTL_RESET, "RESET", "BGPIB bus reset",
    TYPE_LOCK, "LOCK", "GPIB-ENET device lock",
    TYPE_UNLOCK, "UNLOCK", "GPIB-ENET device unlock",
    TYPE_FILE, "FILE", "Data file (waveform)",
    TYPE_HEXFILE, "HEXFILE", "Hex data file (waveform)",
    TYPE_CURVE, "CURVE", "AFG2020 Curve file (8 bit)",
    TYPE_CURVE16L, "CURVE16L", "AFG2020 Curve file (16 bit LSB)",
    TYPE_CURVE16, "CURVE16", "AFG2020 Curve file (16 bit MSB)",
    0, "", "", -1, "", ""};
static struct device
{
  struct device *link;
  char device_name[16];
  int device_handle;
  int lock;
  int ibapad;
  int ibatmo;
}
 *gpib_table = NULL;
static struct device *gp;

void mcmd_gpib_help (void)
{
  int i = 0;
  char buf[64];
  char *newline = "\n";
  char *blanks = "                ";

  fprintf (stderr, "    mcmd_gpib help table (stim.mne)\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "       macro$     device, type, params\n");
  fprintf (stderr, "                  device from ibconf table\n");
  fprintf (stderr, "                  type indicates type of command\n");
  fprintf (stderr, "                  params sent to device\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "              type table:\n");
  while (cmd_type_table[i].type) {
    strcpy (buf, blanks);
    strcat (buf, cmd_type_table[i].key);
    strcat (buf, blanks);
    buf[30] = 0;
    strcat (buf, cmd_type_table[i].help);
    strcat (buf, newline);
    fputs (buf, stdout);
    i = i + 1;
  }
  fprintf (stderr, "              * special parameter substitute rules\n");
  fprintf (stderr, "\n");
}
char *mcmd_long (int *ival, int cnt)
{
  static char string[256];
  int i;

  for (i = 0; i < cnt; i++) {
    string[i + 0] = (ival[i] >> 24) & 0xFF;
    string[i + 1] = (ival[i] >> 16) & 0xFF;
    string[i + 2] = (ival[i] >> 8) & 0xFF;
    string[i + 3] = (ival[i] >> 0) & 0xFF;
  }
  return string;
}
int mcmd_type (char *tok)
{
  int i = 0;

  while (1) {
    if (!strcasecmp (cmd_type_table[i].key, tok))
      return cmd_type_table[i].type;
    if (!cmd_type_table[i].key[0])
      break;
    i = i + 1;
  }
  return -1;
}
void mcmd_ibsta (int ib_sta)
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
void mcmd_iberr (int ib_err)
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
int mcmd_iblock (int state)
{
  int old_state;

  old_state = gp->lock;
  gp->lock = state;
  return old_state;;
}
int mcmd_ibfind (char *name)
{
  int device_open = 0;
  struct device *old_gp;

  gp = gpib_table;
  if (gp) {
    while (gp) {
      if (!strcmp (gp->device_name, name)) {
        return gp->device_handle;
      } else {
        old_gp = gp;
        gp = gp->link;
      }
    }
    gp = malloc (sizeof (struct device));
    old_gp->link = gp;
    gp->link = NULL;
    gp->device_handle = 0;
    gp->lock = 0;
    strcpy (gp->device_name, name);
  } else {
    gp = malloc (sizeof (struct device));
    gp->link = NULL;
    gp->device_handle = 0;
    gp->lock = 0;
    gpib_table = gp;
    strcpy (gp->device_name, name);
  }
  gp->device_handle = ibfind (gp->device_name);
  if (!gp->device_handle || gp->device_handle < 0) {
    fprintf (stderr,
             "0x%X=mcmd_ibfind(\"%s\") ARE you running on the correct machine?\n",
             gp->device_handle, gp->device_name);
  } else {
    /*
     * ibask(gp->device_handle, IbaPAD, &gp->ibapad); /*
     */
    /*
     * ibask(gp->device_handle, IbaTMO, &gp->ibatmo); /*
     */
  }
  return gp->device_handle;
}
void mcmd_fdump (char *curve, int length)
{
  int i;

  printf ("--------------xfile-----------------\n");
  printf ("length %d\n", length);
  for (i = 0; i < length; i++) {
    printf (" %2.2X", (unsigned char) curve[i] & 0xFF);
    if ((i % 16) == 15)
      printf ("\n");
  }
  printf ("\n");
}
int mcmd_gfile (int devid, char *name)
{
  FILE *curve_file = NULL;
  static char *curve_buffer;
  int length, nlen;
  int devsts;

  curve_file = fopen (name, "rb");
  if (!curve_file)
    return 1;
  fseek (curve_file, 0, SEEK_END);
  length = ftell (curve_file);
  if (!length)
    return 2;
  fseek (curve_file, 0, SEEK_SET);
  curve_buffer = malloc (length);
  if (!curve_buffer)
    return 3;
  if (length != fread (curve_buffer, 1, length, curve_file))
    return 4;

  /*
   * mcmd_fdump(curve_buffer,length); /* 
   */
  devsts = ibwrt (devid, curve_buffer, length);
  free (curve_buffer);
  if (devsts & ERR) {
    mcmd_iberr (iberr);
    mcmd_ibsta (devsts);
    fprintf (stderr, "0x%X=mcmd_ibwrt(0x%X,buffer,%d)\n",
             devsts, devid, length);
  }
  return 0;
}
int mcmd_xfile (int devid, char *filename)
{
  FILE *curve_file = NULL;
  char curve[16384];
  char line[1024];
  int length;
  int next;
  int devsts;
  int i;

  curve_file = fopen (filename, "r");
  if (!curve_file)
    return 1;

  length = 0;
  while (EOF != fscanf (curve_file, "%i", &next)) {
    curve[length] = next & 0xFF;
    length += 1;
    if (length > 16383)
      break;
  }
  fclose (curve_file);

  /*
   * mcmd_fdump(curve,length); /* 
   */
  devsts = ibwrt (devid, curve, length);
  if (devsts & ERR) {
    mcmd_iberr (iberr);
    mcmd_ibsta (devsts);
    fprintf (stderr, "0x%X=mcmd_ibwrt(0x%X,buffer,%d)\n",
             devsts, devid, length);
  }
  return 0;
}
int mcmd_cfile (int devid, char *filename, int wordsize)
{
  FILE *curve_file = NULL;
  char curve[16500];
  char line[1024];
  int length;
  int next;
  int devsts;
  int i;

  curve_file = fopen (filename, "r");
  if (!curve_file)
    return 1;

  fgets (curve, 128, curve_file);
  length = strlen (curve);
  if (curve[length - 1] == 0x0A) {
    length -= 1;
    curve[length] = 0;
  }
  while (EOF != fscanf (curve_file, "%i", &next)) {
    switch (wordsize) {
     case TYPE_CURVE:
       curve[length++] = next & 0xFF;
       break;
     case TYPE_CURVE16:
       curve[length++] = (next >> 8) & 0xFF;
       curve[length++] = (next >> 0) & 0xFF;
       break;
     case TYPE_CURVE16L:
       curve[length++] = (next >> 0) & 0xFF;
       curve[length++] = (next >> 8) & 0xFF;
       break;
    }
    if (length > 16384)
      break;
  }
  fclose (curve_file);
  /*
   * mcmd_fdump(curve,length); /* 
   */
  devsts = ibwrt (devid, curve, length);
  if (devsts & ERR) {
    mcmd_iberr (iberr);
    mcmd_ibsta (devsts);
    fprintf (stderr, "0x%X=mcmd_ibwrt(0x%X,buffer,%d)\n",
             devsts, devid, length);
  }
  return 0;
}
int mcmd_gpib (const char *tr, const char *buf)
{
  static char trans[256];
  static char trans1[256];
  static char delim[] = { " ," };
  char *tok[TOKEN_MAX];
  char *bs, *es;
  char *string;
  char *curve;
  int curve_length;
  int i, j, k, narg;
  int hex_words[TOKEN_MAX];
  int cmd_type;
  int devid;
  int devsts, spr;

  strcpy (trans, tr);
  narg = strlen (trans);
  for (i = 0; i < narg; i++) {
    if (trans[i] == 0x0D)
      trans[i] = 0x20;
    if (trans[i] == 0x0A)
      trans[i] = 0x20;
    if (trans[i] == 0x09)
      trans[i] = 0x20;
  }
  tok[0] = strtok (trans, delim);
  for (i = 1; i < TOKEN_MAX; i++) {
    tok[i] = strtok (NULL, delim);
    if (!tok[i])
      break;
  }
  narg = i;
  devid = mcmd_ibfind (tok[0]);
  cmd_type = mcmd_type (tok[1]);
  mcmd_delay (250);
  switch (cmd_type) {
   case TYPE_CURVE:
   case TYPE_CURVE16:
   case TYPE_CURVE16L:
     if (i = mcmd_cfile (devid, tok[2], cmd_type))
       fprintf (stderr, "Curve file: %s not found (%d)\n", tok[2], i);
     break;
   case TYPE_FILE:
     if (i = mcmd_gfile (devid, tok[2]))
       fprintf (stderr, "Curve file: %s not found (%d)\n", tok[2], i);
     break;
   case TYPE_HEXFILE:
     if (i = mcmd_xfile (devid, tok[2]))
       fprintf (stderr, "Curve file: %s not found (%d)\n", tok[2], i);
     break;
   case TYPE_HEX:                      /* parameter is a HEX string */
     for (i = 0; i < TOKEN_MAX; i++)    /*   decode and send as binary... */
       if (tok[i + 2][0]) {
         hex_words[i] = 0;
         hex_words[i] = strtol (tok[i + 2], NULL, 0);
       } else
         break;
     devsts = ibwrt (devid, mcmd_long (hex_words, i), i * 4);
     if (devsts & ERR) {
       mcmd_iberr (iberr);
       mcmd_ibsta (devsts);
       fprintf (stderr, "0x%X=mcmd_ibwrt(%s(0x%X),buffer,%d)\n",
                devsts, tok[0], devid, i * 4);
     }
     break;
   case TYPE_ITEXT:                    /* text parameter */
   case TYPE_TEXT:                     /*   send it to specified device */
   case TYPE_TEXTCR:
     memset (trans1, 0, strlen (tr));
     k = 0;
     for (i = 0; i < strlen (tr); i++)
       if (tr[i] == '"')
         for (j = i + 1; j < strlen (tr); j++)
           if (tr[j] != '"')
             trans1[k++] = tr[j];
           else {
             if (cmd_type == TYPE_TEXTCR)
               trans1[k++] = '\r';
             else
               trans1[k++] = '\n';
             trans1[k++] = 0;
           }
     devsts = ibwrt (devid, trans1, strlen (trans1));
     if (devsts & ERR) {
       mcmd_iberr (iberr);
       mcmd_ibsta (devsts);
       trans1[strlen (trans1) - 1] = 0;
       fprintf (stderr, "0x%X=mcmd_ibwrt(%s(0x%X),\"%s\",%d)\n",
                devsts, tok[0], devid, trans1, strlen (trans1));
     }
     if (cmd_type != TYPE_ITEXT)        /* interlocked text */
       break;                           /* wait for serial poll before */
     /*
      * TYPE_ITEXT    
      *//*
      * continue 
      */
     mcmd_delay (100);
     spr = 0;
     for (i = 0; i < 300; i++) {
       mcmd_delay (100);
       devsts = ibrsp (devid, &spr);
       if (spr)
         break;
     }
     if (devsts & ERR) {
       mcmd_iberr (iberr);
       fprintf (stderr, "0x%X=mcmd_ibrsp(%s(0x%X),0x%X)\n",
                devsts, tok[0], devid, spr);
     }
     break;
   case TYPE_CNTL_RESET:               /* reset specified device */
     devsts = ibclr (devid);
     if (devsts & ERR) {
       mcmd_iberr (iberr);
       fprintf (stderr, "0x%X=mcmd_ibclr(%s(0x%X))\n", devsts, tok[0], devid);
     }
     break;
   case TYPE_LOCK:
     if (!mcmd_iblock (1)) {
       devsts = iblock (devid);
       if (devsts & ERR)
         /**/ {
         mcmd_ibsta (devsts);
         fprintf (stderr, "0x%X=mcmd_unlock(%s(0x%X)) [[0x%X]]\n",
                  devsts, tok[0], devid, ibcnt);
         }
     } else
       fprintf (stderr, "mcmd_iblock device already locked\n");
     break;
   case TYPE_UNLOCK:
     if (mcmd_iblock (0)) {
       devsts = ibunlock (devid);
       if (devsts & ERR) {
         mcmd_ibsta (devsts);
         fprintf (stderr, "0x%X=mcmd_ibunlock(%s(0x%X)) [[0x%X]]\n",
                  devsts, tok[0], devid, ibcnt);
       }
     } else
       fprintf (stderr, "mcmd_iblock device already locked\n");
     break;
   case TYPE_TERM:                     /*   send it to specified device */
     sprintf (trans1, "\033[%d;%dH",
              strtol (tok[2], NULL, 0), strtol (tok[3], NULL, 0));
     devsts = ibwrt (devid, trans1, strlen (trans1));
     memset (trans1, 0, strlen (tr));
     k = 0;
     for (i = 0; i < strlen (tr); i++)
       if (tr[i] == '"')
         for (j = i + 1; j < strlen (tr); j++)
           if (tr[j] != '"')
             trans1[k++] = tr[j];
           else {
             trans1[k++] = 0;
           }
     for (i = 0; i < strlen (buf); i++)
       if (buf[i] == '"') {
         k = k - 1;
         for (j = i + 1; j < strlen (buf); j++)
           if (buf[j] != '"')
             trans1[k++] = buf[j];
           else {
             trans1[k++] = 0x0D;
             trans1[k + 1] = 0;
           }
       }
     devsts = ibwrt (devid, trans1, strlen (trans1));
     if (devsts & ERR) {
       mcmd_iberr (iberr);
       mcmd_ibsta (devsts);
       trans1[strlen (trans1) - 1] = 0;
       fprintf (stderr, "0x%X=mcmd_ibwrt(%s(0x%X),\"%s\",%d)\n",
                devsts, tok[0], devid, trans1, strlen (trans1));
     }
     break;                             /* wait for serial poll before */
   case TYPE_CSI:                      /*   send it to specified device */
     memset (trans1, 0, strlen (tr));
     k = 0;
     trans1[k++] = 0x1B;
     trans1[k++] = '[';
     for (i = 0; i < strlen (tr); i++)
       if (tr[i] == '"')
         for (j = i + 1; j < strlen (tr); j++)
           if (tr[j] != '"')
             trans1[k++] = tr[j];
           else
             trans1[k++] = 0;
     devsts = ibwrt (devid, trans1, strlen (trans1));
     if (devsts & ERR) {
       mcmd_iberr (iberr);
       mcmd_ibsta (devsts);
       trans1[strlen (trans1) - 1] = 0;
       fprintf (stderr, "0x%X=mcmd_ibwrt(%s(0x%X),\"%s\",%d)\n",
                devsts, tok[0], devid, trans1, strlen (trans1));
     }
     break;                             /* wait for serial poll before */
  }
  return 0;
}
