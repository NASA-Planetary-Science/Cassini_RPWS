
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

 /**************
  *
  *	error reporting
  *
  *	lib-s-rout, text
  *
  *
  ***************/
void set_msg (const char *lib, const char *rout, const char *text,
              const char *severity)
{
  char error_string[128];

  strcpy (error_string, lib);
  strcat (error_string, severity);
  strcat (error_string, rout);
  strcat (error_string, ", ");
  strcat (error_string, text);
  strcat (error_string, "\n");
  fputs (error_string, stderr);
  return;
}
void set_error (const char *lib, const char *rout, const char *text)
{
  set_msg (lib, rout, text, "-E-");
  return;
}
void set_warn (const char *lib, const char *rout, const char *text)
{
  set_msg (lib, rout, text, "-W-");
  return;
}
void set_information (const char *lib, const char *rout, const char *text)
{
  set_msg (lib, rout, text, "-I-");
  return;
}
