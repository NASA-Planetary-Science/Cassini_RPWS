
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

/************************** UTILITY.C *******************/
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define UTIL
#include "util.h"

 /***********************************************************
  *
  *  Misc routines (i.e. Fortran emulations)
  *
  ***********************************************************/

char *UTIL_filetype = { ".dat" };
float Max (float v1, float v2)
{
  if (v1 > v2)
    return v1;
  else
    return v2;
}
int iMax (int v1, int v2)
{
  if (v1 > v2)
    return v1;
  else
    return v2;
}
float Min (float v1, float v2)
{
  if (v1 < v2)
    return v1;
  else
    return v2;
}
int iMin (int v1, int v2)
{
  if (v1 < v2)
    return v1;
  else
    return v2;
}
float util_fabs (float v)
{
  if (v < 0.0)
    return 0.0 - v;
  else
    return v;
}
int util_iabs (int v)
{
  if (v < 0)
    return 0 - v;
  else
    return v;
}

    /******************************
    *   SET6 UTILITY ROUTINES     *
    *	Argument extraction       *
    *                             *
    *******************************/
int util_strncmpi (char *a1, char *a2, int n)
{
  int i, j;

  for (i = 0; i < n; i++) {
    j = toupper (a1[i]) - toupper (a2[i]);
    if (j)
      return j;
  }
  return 0;
}
int util_strcmpi (char *a1, char *a2)
{
  int i, j, i1, i2, imax;

  i1 = strlen (a1);
  i2 = strlen (a2);
  imax = iMin (i1, i2);
  for (i = 0; i < imax; i++) {
    j = toupper (a1[i]) - toupper (a2[i]);
    if (j)
      return j;
  }
  return i1 - i2;
}
void util_strupr (char *s)
{
  int i;

  for (i = 0; i < strlen (s); i++)
    s[i] = toupper (s[i]);
}
char *util_strupr1 (char *s)
{
  int i;
  static char t[128];

  for (i = 0; i < strlen (s); i++) {
    t[i] = toupper (s[i]);
    if (!t[i])
      break;
    if (i >= 127) {
      t[127] = 0;
      break;
    }
  }
  t[i] = 0;
  return t;
}
int util_tok (char *item[],             /* pointers to the tokens       */
              const char *txt,          /* input text stream            */
              const char flag)

{                                       /* flag g = grace notes parse   */
  /*
   * m = macro parse         
   */
  /*
   * s = source parse       
   */
  static char text[128];
  static int i, index;

  strcpy (&text[1], txt);
  util_strupr (&text[1]);

  index = 0;
  text[0] = 0x00;
  for (i = 0; i < 8; i++)
    item[i] = NULL;

  for (i = 1; i < 128; i++)
    switch (text[i]) {
     case '\n':
       text[i] = 0;
       return i;
     case 0:
     case '!':
     case '#':
       return i;
     case '/':
       if (tolower (flag & 0x7F) == 'p')
         return i;
     case 0x09:
     case 0x20:
     case '=':
     case ',':
       text[i] = 0;
       break;
     case ';':
       if (tolower (flag & 0x7F) == 'g')
         return i;
       if (tolower (flag & 0x7F) == 'p')
         return i;
     default:
       if (!text[i - 1]) {
         item[index] = &text[i];
         index = index + 1;
         if (index >= util_tok_items)
           index = util_tok_items - 1;
         break;
       }
    }
  return i;
}
int util_txt (char *item[],             /* pointers to the tokens       */
              const char text[])
{                                       /* input text stream              */
  int i, index;
  char flag;

  for (i = 0; i < util_tok_items; i++)
    item[i] = NULL;
  flag = 1;
  index = 0;
  for (i = 0; i < 128; i++)
    switch (text[i]) {
     case '\n':
     case 0:
       return i;
     case 0x09:
     case 0x20:
     case '=':
     case ',':
       flag = 1;
       break;
     case ';':
     default:
       if (flag) {
         flag = 0;
         item[index] = (char *) &text[i];
         index = index + 1;
         if (index >= util_tok_items)
           index = util_tok_items - 1;
         break;
       }
    }
  return i;
}

    /******************************************
     * translate a logical name (VMS-ese)
     *   *tran = util_env("NAME");
     *  tran == NULL if none...
     ******************************************/
char *util_env (const char *symbol)
{
  char sym[128];
  char *s;

  /*
   * strcpy(sym, "SET_"); 
   */
  strcpy (sym, symbol);
  util_strupr (sym);
  s = getenv (util_strupr1 (sym));
  return s;
}
char *util_envd (const char *symbol, char *def)
{
  char *t;

  t = util_env (symbol);
  if (t == NULL)
    return def;
  return t;
}

  /*
   *    build filename:
   *            use environment variable
   *            if NO 'dot', add ".DAT"
   */
char *util_fpath (char *path)
{
  static char slash = '/';
  static char fdir[128] = "";
  int i;

  if (path == NULL)
    return fdir;
  strcpy (fdir, path);
  for (i = strlen (path); i > 0; i--) {
    if (fdir[i - 1] == slash) {
      fdir[i] = 0;
      return fdir;
    }
  }
  return fdir;
}
char *util_fname (const char *name)
{
  static char sym[128];
  char *s;

  sym[0] = 0;
  s = util_env (name);                  /*  translate name      */
  if (s == NULL) {                      /*   no translation     *//*     so simply        */
    sym[0] = 0;                         /*  CLEAR the file name */
    strcat (sym, "/usr/local/set6/");   /*                      */
    strcat (sym, name);                 /*     patch in         */
    strcat (sym, UTIL_filetype);        /*     .DAT             */
    return sym;                         /*                      */
  }
  strcpy (sym, s);
  if (!strchr (sym, '.')) {             /*  was a translation     *//*    and it MUST have  */
    if (util_strcmpi ((char *) name, "FOR010")) /*    only the path     */
      strcat (sym, name);               /* UNLESS it's source   */
    else                                /*  when .DAT is ALWAYS     */
      util_fpath (s);                   /*  SAVE path for INCLUDE */
    strcat (sym, UTIL_filetype);        /*    else patch in     */
  }                                     /*    .DAT              */
  return sym;
}
char *util_sname (const char *name)
{
  static char sym[128];
  char *s;
  int index;

  sym[0] = 0;
  s = util_env ("FOR010");              /*  translate path name    */
  if (s)                                /*   start with it if      */
    strcat (sym, s);                    /*   it's there...         */
  if (name)                             /*  copy in name specified */
    strcat (sym, name);                 /*     by the caller       */
  for (index = 1; index < strlen (sym); index++) {
    if (sym[index] == '.')
      return sym;
  }
  strcat (sym, UTIL_filetype);          /*  default to .DAT        */
  return sym;                           /*                      */
}

int util_numeric (const char *string)
{
  int in, cmp, in_l, cmp_l;
  int match = 0;
  static char numbers[] = ",0123456789.";

  in_l = strlen (string);
  cmp_l = strlen (numbers);
  if (in_l == 0) {
    return 0;                           /* well, that wasn't a number */
  }
  for (in = 0; in < in_l; in++) {
    for (cmp = 1; cmp < cmp_l; cmp++) {
      if (string[in] == numbers[cmp]) {
        match = match + 1;
        break;
      }
    }
  }
  if (match == in_l)
    return 1;

  return 0;
}

 /*
  *     util_ctlz strips the damn ctrl-z stuff wordstar leaves
  *     behind (afects UNIX systems)
  */
int util_ctlz (char *t)
{
  int i;
  int j = 0;

  i = strlen (t);
  if (i)
    for (j = 0; j < i; j++) {
      switch (t[j]) {
       case 0x1A:
         t[j] = 0;
         break;
       case 0x0D:
         t[j] = t[j + 1];
         t[j + 1] = 0;
         break;
      }
    }
  return j;
}
int util_octal (char *in, int dig)
{
  char temp[16];
  int i;

  for (i = 0; i < dig; i++)
    temp[i] = in[i];
  temp[dig] = 0;
  return strtol (temp, NULL, 8);
}
char *util_stg (char *in)
{
  static char res[256];
  int i;
  int o = 0;
  int esc = 0;

  for (i = 0; i < strlen (in); i++) {
    if (in[i] == '\\')
      esc = 4;
    if (esc)
      switch (esc) {
       case 4:
         res[o++] = (char) util_octal (&in[i + 1], 3);
       case 3:
       case 2:
       case 1:
         esc = esc - 1;
         break;
       default:
         esc = 0;
         break;
    } else
      res[o++] = in[i];
  }
  res[o++] = '\n';
  res[o] = 0;
  return res;
}
