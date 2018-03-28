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
    TYPE_CURVE, "CURVE", "GPIB_ENET AFG2020 load", 0, "", "", -1, "", ""};
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

  fprintf (stderr, "    mcmd_gpib help table (cmd.mne)\n");
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

 /*
  *     Load a wavefile into the AFG 2020
  *       8) 8 bit samples per line
  *
  *     DATA xxxx
  *     MISC xxxx
  *     CURV npts  
  *                nnn nnn nnn nnn nnn nnn nnn nnn
  *                nnn nnn nnn nnn nnn nnn nnn nnn
  *                ...
  *                nnn nnn nnn nnn nnn nnn nnn nnn
  *     MISC xxxx
  */
int mcmd_waveform (int devid, FILE * curve_file)
{
  char *tok;
  char s[128];
  char delim[] = { " \t,;:" };
  int iberr, devsts;
  int npts, i, j;
  unsigned char buf[2500];
  unsigned short wav[1024];

  while (fgets (s, 127, curve_file)) {
    tok = strtok (s, delim);
    if (strcmp (tok, "CURV")) {
      devsts = ibwrt (devid, s, strlen (s));
      if (devsts & ERR) {
        mcmd_iberr (iberr);
        mcmd_ibsta (devsts);
        s[strlen (s) - 1] = 0;
        fprintf (stderr, "0x%X=mcmd_ibwrt(0x%X,\"%s\",%d)\n",
                 devsts, devid, s, strlen (s));
      }
    } else {
      tok = strtok (NULL, delim);       /* 2nd. token (i.e. length) */
      npts = strtol (tok, NULL, 0);
      for (i = 0; i < npts; i = i + 8);
      {
        fgets (s, 127, curve_file);
        tok = strtok (s, delim);
        wav[i] = strtol (tok, NULL, 0);
        for (j = 1; j < 8; j++) {
          tok = strtok (NULL, delim);
          wav[i + j] = strtol (tok, NULL, 0);
        }
      }
      sprintf (s, "CURV #3%d ", npts);
      memcpy (&buf[strlen (s)], wav, 2048);
      devsts = ibwrt (devid, buf, strlen (s) + 2048);
      if (devsts & ERR) {
        mcmd_iberr (iberr);
        mcmd_ibsta (devsts);
        s[strlen (s) - 1] = 0;
        fprintf (stderr, "0x%X=mcmd_ibwrt(0x%X,\"%s\",%d)\n",
                 devsts, devid, s, strlen (s));
      }
    }
  }
}

 /*
  *
  */
int mcmd_gpib (const char *tr, const char *buf)
{
  static char trans[256];
  static char trans1[256];
  static char delim[] = { " ," };
  char *tok[TOKEN_MAX];
  char *bs, *es;
  char *string;
  int i, j, k, narg;
  int hex_words[TOKEN_MAX];
  int cmd_type;
  int devid;
  int devsts, spr;
  FILE *curve_file;

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
   case TYPE_CURVE:                    /* Load AFG2020 waveform memory */
     memset (trans1, 0, strlen (tr));
     k = 0;
     for (i = 0; i < strlen (tr); i++)
       if (tr[i] == '"')
         for (j = i + 1; j < strlen (tr); j++)
           if (tr[j] != '"')
             trans1[k++] = tr[j];
     if (k) {
       curve_file = NULL;
       curve_file = fopen (trans1, "r");
       if (!curve_file) {
         fprintf (stderr, "mcmd_gpib(device, CURVE, %s) - file not found\n",
                  trans1);
         break;
       }
       mcmd_waveform (devid, curve_file);
       fclose (curve_file);
     } else
       fprintf (stderr,
                "mcmd_gpib(device, CURVE, filename) - filename missing\n");
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
     break;
  }
  return 0;
}
