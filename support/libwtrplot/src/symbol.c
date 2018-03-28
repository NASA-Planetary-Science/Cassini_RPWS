
/*********************************************/

/* Copyright 1993-2003, William T Robison    */

/*                      952 N. St. SW        */

/*                      Cedar Rapids, Iowa   */

/*                          52404-2710       */

/*                      U.S.A.               */

/*********************************************/

/*       See COPYING for terms of use        */

/* This software is covered under terms of   */

/* the GNU General Public License as         */

/* published by the Free Software Foundation */

/*********************************************/

/* You should have received this software at */

/* no charge.  If you have been charged for  */

/* the privelege of obtaining a copy please  */

/* contact the author at                     */

/*                william-robison@uiowa.edu  */

/*********************************************/

/********************* SYMBOL.C ********************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _SYMBOL

#ifdef __MSDOS__
#include <alloc.h>
#include "source\symbol.h"
#include "source\set.h"
#include "source\util.h"
#else
#include "symbol.h"
#include "set.h"
#include "util.h"
#endif

static struct sym_tab_entry *vectors;
static struct sym_tab_entry *symbols[128];
static struct sym_tab_entry *ste;
static int chindex = 1;

void symbol_err (const char *routine, const char *text, const char *name,
                 const int severity)
{
  char tmp[256];

  if (name) {
    strcpy (tmp, text);
    strcat (tmp, ": ");
    strcat (tmp, name);
    set_error ("SYMBOL", routine, tmp);
  } else if (severity)
    set_error ("SYMBOL", routine, text);
  else
    set_warn ("MACRO", routine, text);
}

/*0         1         2         3*/

/*0123456789012345678901234567890*/

/*  1 BCD 01   1.0000 1.0000 2   */

/*  0 1   2    3      4      5   */

/* static struct sym_tab_entry *vectors;
 static struct sym_tab_entry *ste;
 static struct sym_tab_entry *symbols[128];*/

int symbol_read (void)
{
  int istat = 1;
  int lines = 0;
  int i;
  char text[128];
  char *filename;
  char *txttok[util_tok_items];
  FILE *scmd;

  for (i = 0; i < 128; i++)
    symbols[i] = NULL;

  filename = util_fname ("scmd");
  scmd = fopen (filename, "r");
  if (scmd) {
    while (fgets (text, 127, scmd)) {
      text[strlen (text) - 1] = 0;      /* newline */
      util_ctlz (text);
      util_tok (txttok, text, 's');
      if (!strcmp (txttok[1], "BCD"))
        lines = lines + 1;
    }
  } else {
    symbol_err ("read", "no scmd file", filename, 1);
    istat = 0;
  }
  fclose (scmd);
  if (istat) {
    i = sizeof (struct sym_tab_entry);
    i = i * lines;
    vectors = malloc (i);

#ifdef debug
    printf ("%p malloc(%i)  %ix%i\n",
            vectors, i, lines, sizeof (struct sym_tab_entry));
#endif

    for (i = 0; i < lines; i++) {
      vectors[i].x = 0.0;
      vectors[i].y = 0.0;
      vectors[i].index = 0;
    }
    scmd = fopen (filename, "r");
    lines = 0;
    if (scmd) {
      int sym;

      while (fgets (text, 127, scmd)) {
        i = strlen (text);
        if (i > 10) {
          text[i - 1] = 0;              /* newline */
          util_ctlz (text);
          util_tok (txttok, text, 's');
          i = atoi (txttok[0]);
          sym = strtol (txttok[2], NULL, 16);
          if (i == 1)
            if ((sym >= 0) && (sym < 128))
              symbols[sym] = &vectors[lines];
          vectors[lines].x = 1000 * atof (txttok[3]);
          vectors[lines].y = 1000 * atof (txttok[4]);
          switch (atoi (txttok[5])) {
           case 1:
             vectors[lines].index = i;
             break;
           case 2:
             vectors[lines].index = -i;
             break;
          }
          lines = lines + 1;
        }
      }
    }
  }
  if (istat)
    return lines;
  else
    return istat;
}
struct sym_tab_entry *symbol_entry (int i)
{
  chindex = 1;
  ste = symbols[i];                     /* ste is 1st. char */
  return symbols[i];
}
int symbol_value (float *x, float *y, char *ipen, float h)
{
  static float oldx, oldy;
  int sts;

  if (util_iabs (ste->index) != chindex) {
    *x = oldx;
    *y = oldy;
    *ipen = 0;
    return 0;
  }
  *x = ((float) (ste->x - 1000) / 1000.) * h;
  *y = ((float) (ste->y - 1000) / 1000.) * h;
  oldx = *x;
  oldy = *y;
  if (ste->index > 0)
    *ipen = 'D';
  else
    *ipen = 'U';
  sts = ste->index;
  chindex = chindex + 1;
  ste += 1;
  return sts;
}
