
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

/*************** PS_PLOT.C ****************/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"
#include "plotinit.h"
#include "set.h"
#include "wtrplot.h"

#define POINTS  72.00
#define FUDGE    1.50
#define new_line_max 15
#define COPIES
#define CMYK

enum
{
  PS_IDLE,
  PS_SETPLT,
  PS_PLOT,
  PS_ZINC,
  PS_SYMBOL,
  PS_FRAME,
  PS_FACTOR,
  PS_WRAPUP
};
extern FILE *plotout;

static char *cmd_filename = { "plot" };
static char Version[][16] = { "version 1 ", "version 2" };
/* static char *dt_current = { "DD-MMM-YYYY" }; */
static int last_call = PS_IDLE;
static char fill_string[32] = { "" };
extern int page_count;
extern float set_date_size[3];
int orient[8] = { 1, -794, 0, 0, 1, -794, 0, 0 };       /* -744 0 then -779 19 */
static float ps_set_date_size[2][3] = { 
  {8.00, 21.00, 0.25},
  {10.00, 15.00, 0.25}
};

char ps_default_font[2][64];

                             /*
                              * 1         2      2  3      
                              */
                             /*
                              * 123456789012345678901234567890123456
                              */
static char driver_name[] = { "PS_PLOT, PostScript driver,         " };

#define DRIVER_NAME_COMMA 27
static float offset_x = 0.0;
static float offset_y = 0.0;
static float current_x = 0.0;
static float current_y = 0.0;
static float current_s = 1.0;
static float old_x = -9999;
static float old_y = +9999;
static int old_pen = 99;
static int min_x = 1000;
static int min_y = 1000;
static int max_x = 0;
static int max_y = 0;
static int plotting = 0;
static int newline = 0;
static int page_number = 0;
static int icnt = 0;

int psi_macro (void)
{
  FILE *macro_file;
  char *filename;
  char line[257];

  filename = util_fname ("pscmd");
  macro_file = fopen (filename, "r");
  if (!macro_file)
    return 1;
  while (fgets (line, 256, macro_file)) {
    if (util_ctlz (line))
      fprintf (plotout, "%s", line);
  }
  fclose (macro_file);
  return 1;
}
void psi_page_begin (void)
{
  page_number = page_number + 1;
  fprintf (plotout, "%%%%Page: %d %d\n", page_number, page_number);
  fprintf (plotout, "%%%%BeginPageSetup\n");
  fprintf (plotout, "stup\n");
  fprintf (plotout, "%.2f setlinewidth\n", (POINTS * current_s) / 75.0);
  fprintf (plotout, "%%%%EndPageSetup\n");
  return;
}
int psi_newline (int icnt, int increment)
{
  int itmp;

  itmp = icnt + increment;
  if (itmp < new_line_max)
    return itmp;
  fputs ("\n", plotout);
  newline = 1;
  return 0;
}
void psi_page_end (void)
{
  if (!newline)
    fputc ('\n', plotout);
  fprintf (plotout, "stroke\n");

#ifdef COPIES
  if (page_count > 1)
    fprintf (plotout, "/#copies %d def\n", page_count);
#else
  if (page_count > 1)
    for (i = 0; i < page_count - 1; i++) {
      fprintf (plotout, "copypage ");
      icnt = psi_newline (icnt, 3);
    }
#endif

  fprintf (plotout, "showpage erasepage\n");
  fprintf (plotout, "%%%%PageTrailer\n");
  return;
}
int ps_factor (float val)
{
  last_call = PS_FACTOR;
  current_s = val;
  if (plotting)
    fprintf (plotout, "%.2f setlinewidth\n", (POINTS * current_s) / 53.0);
  return plotting;
}
float pf_bound_x (float x)
{
  int ix;

  ix = x;
  min_x = iMin (ix, min_x);
  ix++;
  max_x = iMax (ix, max_x);
  return x;
}
float pf_bound_y (float y, float h)
{
  int iy;

  iy = (y + h);
  min_y = iMin (iy, min_y);
  iy++;
  max_y = iMax (iy, max_y);
  return y;
}
float psf_scale (float val, int loc)
{
  float q;

  q = val * current_s;

  switch (loc) {
   case 1:
     current_x = q;
     return q + offset_x;
   case 2:
     current_y = q;
     return q + offset_y;
   default:
     return q;
  }
}
float psi_scale (float val, int loc)
{
  return psf_scale (val, loc) * POINTS;
}
static char *init_font;
char *ps_setplt (char *ver,
                 char *date, char *fname, char *font, int orient_flag)
{
  int i;

  last_call = PS_SETPLT;
  if (plotting == 2)
    fclose (plotout);

  if (orient_flag & 4)
    strcpy (fill_string, " ");

  if (orient_flag & 1) {
    for (i = 0; i < 3; i++)
      orient[i] = 0;
  } else {
    for (i = 0; i < 3; i++)
      orient[i] = orient[i + 4];
  }


  for (i = 0; i < 3; i++)
    set_date_size[i] = ps_set_date_size[orient[0] & 1][i];

  if (fname == NULL) {
    plotout = stdout;
    plotting = 1;
  } else {
    plotout = plot_fopen (fname, TEXT);
    if (plotout != NULL)
      plotting = 2;
  }
  if (plotting) {
    plot_init (plotout, 0);
    fprintf (plotout, "%%!%s\n", util_envd ("PLT_PSVER", "PS-Adobe-3.0"));
    if (cmd_filename)
      fprintf (plotout, "%%%%Title: %s\n", cmd_filename);
    fprintf (plotout, "%%%%Creator: %s\n", Version[0]);
    if (date)
      if (date[0])
        fprintf (plotout, "%%%%CreationDate: %s\n", date);
    fprintf (plotout, "%%%%BoundingBox: (atend)\n");
    fprintf (plotout, "%%%%Pages: (atend)\n");
    fprintf (plotout, "%%%%%s 0\n", Version[1]);
    plot_init (plotout, 50);
    fprintf (plotout, "%%%%EndComments\n");
    fprintf (plotout, "%%%%BeginSetup\n");
    fprintf (plotout, "/pu {stroke moveto} bind def\n");
    fprintf (plotout, "/pd {lineto} bind def\n");
    fprintf (plotout, "/dot {2 copy moveto lineto} bind def\n");
    fprintf (plotout, "/scl {findfont exch scalefont setfont} bind def\n");
    fprintf (plotout, "/just1 {show} bind def\n");
    fprintf (plotout,
             "/just4 {dup stringwidth pop 2 div neg 0 rmoveto show} bind def\n");
    fprintf (plotout,
             "/just7 {dup stringwidth pop neg 0 rmoveto show} bind def\n");
    fprintf (plotout, "/mark {pop} bind def\n");
    if (orient[0] & 1) {
      fprintf (plotout,
               "/stup {-90 rotate %d %d translate %d setlinecap %d setlinejoin} bind def\n",
               orient[1], orient[2], 1, 1);
    } else {
      fprintf (plotout,
               "/stup {0 rotate %d %d translate %d setlinecap %d setlinejoin} bind def\n",
               orient[1], orient[2], 1, 1);
    }
    if (orient_flag & 2)
      psi_macro ();                     /* insert note macros */
    strcpy (ps_default_font[0], "Times-Roman");
    strcpy (ps_default_font[1], "Times-Bold");
    if (font)
      if (font[0]) {
        strcpy (ps_default_font[0], font);
        strcpy (ps_default_font[1], font);
        strcat (ps_default_font[1], "-Bold");
      }
    init_font = util_env ("PLT_FONT");
    if (init_font) {
      strcpy (ps_default_font[0], init_font);
      strcpy (ps_default_font[1], init_font);
      strcat (ps_default_font[1], "-Bold");
      init_font = util_env ("PLT_BOLD");
      if (init_font)
        strcpy (ps_default_font[1], init_font);
    }
    plot_init (plotout, 70);
    fprintf (plotout, "%%%%EndSetup\n");
    psi_page_begin ();
  }
  old_pen = PEN_DOWN;

  if (page_count)
    sprintf (&driver_name[DRIVER_NAME_COMMA + 2], "%i", page_count);
  else
    driver_name[DRIVER_NAME_COMMA] = 0;

  return driver_name;
}
int ps_wrapup (void)
{
  int border_min_x, border_max_x;
  int border_min_y, border_max_y;

  border_min_x = 0;
  border_min_y = 0;
  border_max_x = 0;
  border_max_y = 0;
  border_min_x = (max_x - min_x) / 25;
  border_min_y = (max_y - min_y) / 25;
  border_max_x = (max_x - min_x) / 25;
  border_max_y = (max_y - min_y) / 25;
  if (plotting) {
    if (last_call != PS_FRAME)
      psi_page_end ();
    fprintf (plotout, "%%%%Trailer\n");

/*	fprintf(plotout,"%%%%BoundingBox: %d %d %d %d\n",
			min_y + border_min_y, 
			min_x + border_min_x, 
			max_y + border_max_y, 
			max_x + border_max_x); */
    if (orient[0])
      fprintf (plotout, "%%%%BoundingBox: %d %d %d %d\n",
               min_y - border_min_y,
               min_x - border_min_x,
               max_y + border_max_y, max_x + border_max_x);
    else
      fprintf (plotout, "%%%%BoundingBox: %d %d %d %d\n",
               min_x - border_min_x,
               min_y - border_min_y,
               max_x + border_max_x, max_y + border_max_y);
    fprintf (plotout, "%%%%Pages: %d\n", page_number);
    fprintf (plotout, "%%%%Pages: %d\n", page_number);
    fprintf (plotout, "%%%%EOF\n");
    plot_clear (plotout, 0);
    newline = 1;
  }
  if (plotting == 2)
    fclose (plotout);
  plotting = 0;
  plotout = NULL;
  old_pen = PEN_UP;
  return plotting;
}
int ps_frame (void)
{
  last_call = PS_FRAME;
  if (plotting) {
    psi_page_end ();
    plot_page (plotout, 0);
    psi_page_begin ();
    newline = 1;
  }
  return plotting;
}

struct valid_macros
{
  char name[16];
  char mnemonic[4];
} v_m[] = {
	{"OPEN", "NO"},                         /* open note   */
	{"CLOSED", "NC"},                     /* closed note */
	{"GRACE", "NG"},                      /* grace note  */
	{"GCLEF", "GC"},                      /* G clef      */
{	"", ""}
	};
int ps_macro (float x, float y, float h, char *name)
{
  int index = 0;

  while (1) {
    if (!v_m[index].name)               /* no matchin macro implemented */
      return 0;                         /*                              */
    if (!strcmp (name, v_m[index].name))
      break;
    index = index + 1;
  }
  fprintf (plotout, " %.2f %.2f %.2f %s",
           psi_scale (h, 0),
           psi_scale (x, 1), psi_scale (y, 2), v_m[index].mnemonic);
  icnt = psi_newline (icnt, new_line_max);
  return 1;
}
float ps_color (int inten, int Color)
{
  static float                          /*     RED    GREEN  BLUE   YELLOW MAGEN  CYAN   BLACK                 */
    Cindex[][8] = { {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 1.000},     /* 0 Black */
    /*
     */
  {0.000, 1.000, 0.000, 0.000, 0.000, 1.000, 1.000, 0.000},     /* 1 Red */
  {0.000, 0.000, 1.000, 0.000, 1.000, 0.000, 1.000, 0.000},     /* 2 Green */
  {0.000, 0.000, 0.000, 1.000, 1.000, 1.000, 0.000, 0.000},     /* 3 Blue */
    /*
     */
  {0.000, 1.000, 1.000, 0.000, 1.000, 0.000, 0.000, 0.000},     /* 4 Yellow */
  {0.000, 1.000, 0.000, 1.000, 0.000, 1.000, 0.000, 0.000},     /* 5 Magenta */
  {0.000, 0.000, 1.000, 1.000, 0.000, 0.000, 1.000, 0.000},     /* 6 Cyan */
    /*
     */
  {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 1.000},     /* 7 Black */

  {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.666},     /* 8 Black */
    /*
     */
  {0.000, 0.666, 0.000, 0.000, 0.000, 0.666, 0.666, 0.000},     /* 9 Red */
  {0.000, 0.000, 0.666, 0.000, 0.666, 0.000, 0.666, 0.000},     /* 10 Green */
  {0.000, 0.000, 0.000, 0.666, 0.666, 0.666, 0.000, 0.000},     /* 11 Blue */
    /*
     */
  {0.000, 0.666, 0.666, 0.000, 0.666, 0.000, 0.000, 0.000},     /* 12 Yellow */
  {0.000, 0.666, 0.000, 0.666, 0.000, 0.666, 0.000, 0.000},     /* 13 Magenta */
  {0.000, 0.000, 0.666, 0.666, 0.000, 0.000, 0.666, 0.000},     /* 14 Cyan */
    /*
     */
  {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.333},     /* 15 Black */
  {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.200},     /* 16 Black */

  {0.000, 0.333, 0.000, 0.000, 0.000, 0.333, 0.333, 0.000},     /* 17 Red */
  {0.000, 0.000, 0.333, 0.000, 0.333, 0.000, 0.333, 0.000},     /* 18 Green */
  {0.000, 0.000, 0.000, 0.333, 0.333, 0.333, 0.000, 0.000},     /* 19 Blue */
    /*
     */
  {0.000, 0.333, 0.333, 0.000, 0.333, 0.000, 0.000, 0.000},     /* 20 Yellow */
  {0.000, 0.333, 0.000, 0.333, 0.000, 0.333, 0.000, 0.000},     /* 21 Magenta */
  {0.000, 0.000, 0.333, 0.333, 0.000, 0.000, 0.333, 0.000},     /* 22 Cyan */

  {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.100},     /* 23 Black */
    /*
     */

  {0.000, 0.000, 0.000, 0.000, 0.000, 0.300, 0.900, 0.000},     /* 24 Orange */
  {0.000, 0.666, 0.333, 0.000, 0.300, 0.900, 0.000, 0.000},     /* 25 Purple */
  {0.000, 0.000, 0.000, 0.000, 0.250, 0.000, 0.750, 0.000},     /* 26 Lt Green */
  {0.000, 0.000, 0.000, 0.000, 0.750, 0.250, 0.000, 0.000},     /* 27 */
  {0.000, 0.000, 0.000, 0.000, 0.000, 0.250, 0.750, 0.000},     /* 28 m Orange */
  {0.000, 0.000, 0.000, 0.000, 0.000, 0.150, 0.450, 0.000},     /* 29 w Orange */
  {0.000, 0.000, 0.000, 0.000, 0.000, 0.750, 0.250, 0.000},     /* 30 */
  {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.100},     /* 31 */
  {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000}
  };                                    /* 32  */
  /*
   * RED    GREEN  BLUE   YELLOW MAGEN  CYAN   BLACK                     
   */
  int i;

  i = inten;
  if (inten > 32)
    i = 0;
  return Cindex[i][Color];
}

int ps_zinc (int inten, int color)
{
  last_call = PS_ZINC;
  if (plotting) {
    if (!newline) {
      fputc ('\n', plotout);
      newline = 1;
    }
    if (color)
      fprintf (plotout, "stroke\n");

#ifdef CMYK
    fprintf (plotout, "%5.3f %5.3f %5.3f %5.3f setcmykcolor\n",
             ps_color (color, p_CYAN),
             ps_color (color, p_MAGENTA),
             ps_color (color, p_YELLOW), ps_color (color, p_BLACK));
#else
    fprintf (plotout, "%5.3f %5.3f %5.3f setrgbcolor\n",
             ps_color (color, p_RED),
             ps_color (color, p_GREEN), ps_color (color, p_BLUE));
#endif

    fprintf (plotout, "%.2f setlinewidth\n",
             (inten * POINTS * current_s) / 5300.0);
    newline = 1;
  }
  return plotting;
}
int ps_zmark (char *text)
{
  if (plotting) {
    if (!newline) {
      fputc ('\n', plotout);
    }
    fprintf (plotout, "(%s) mark\n", text);
    newline = 1;
  }
  return plotting;
}
int ps_zcolor (int inten, float *color, int cmy_flag)
{
  last_call = PS_ZINC;
  if (plotting) {
    if (!newline) {
      fputc ('\n', plotout);
      newline = 1;
    }
    if (color)
      fprintf (plotout, "stroke\n");
    if (cmy_flag)
      fprintf (plotout, "%5.3f %5.3f %5.3f %5.3f setcmykcolor\n",
               color[0], color[1], color[2], color[3]);
    else
      fprintf (plotout, "%5.3f %5.3f %5.3f setrgbcolor\n",
               color[0], color[1], color[2]);

    fprintf (plotout, "%.2f setlinewidth\n",
             (inten * POINTS * current_s) / 5300.0);
    newline = 1;
  }
  return plotting;
}
int ps_plot (float x, float y, int ipen)
{
  float new_x, new_y;

  last_call = PS_PLOT;
  if (newline)
    icnt = 0;
  newline = 0;

  new_x = psi_scale (x, 1);
  new_y = psi_scale (y, 2);

  if (plotting) {
    old_x = new_x;
    old_y = new_y;
    fprintf (plotout, " %.2f %.2f",
             pf_bound_x (new_x), pf_bound_y (new_y, 0));
    if (labs (ipen) == PEN_UP) {
      fputs (" pu", plotout);
      old_pen = PEN_UP;
    }
    if (labs (ipen) == PEN_DOWN) {
      fputs (" pd", plotout);
      old_pen = PEN_DOWN;
    }
    if (labs (ipen) == PEN_DOT) {
      fputs (" dot", plotout);
      old_pen = PEN_DOWN;
    }

    icnt = psi_newline (icnt, 4);
  }

  if (ipen < 0) {
    offset_x = psf_scale (x, 1);
    offset_y = psf_scale (y, 1);
  }
  return plotting;
}

#define HW_ratio .7
#define CM_inch 2.54
int ps_symbol (float x,
               float y, float h, char *c, float theta, int just, char *font)
{
  static float old_h = -999.0;
  static int old_just = 999;
  static int justification;

  last_call = PS_SYMBOL;
  if (plotting) {
    if (!newline) {
      fputc ('\n', plotout);
      newline = 1;
    }
    if ((old_h != h) || font) {
      int isize = 0;

      if (h > 0.25)
        isize = 1;
      if (abs (just) & 0x02)
        isize = 1;
      if (font){
        if (font[0])
          fprintf (plotout, " %.2f /%s scl\n",
                   psf_scale (h, 0) * POINTS * FUDGE, font);
        else
          fprintf (plotout, " %.2f /%s scl\n",
                   psf_scale (h, 0) * POINTS * FUDGE, ps_default_font[isize]);
	  }
      old_h = h;
    }
    if (just != -99) {
      if (old_just != just) {
        justification = 4;
        if (just >= 1)
          justification = 7;
        if (just <= -1)
          justification = 1;
        old_just = just;
      }

      fprintf (plotout, " %.2f %.2f pu",
               pf_bound_x (psi_scale (x, 1)),
               pf_bound_y (psi_scale (y, 2), h * POINTS));
    }
    fprintf (plotout, " (%s) just%d\n", c, justification);
    old_y = -9999;
    old_x = -9999;
    old_pen = PEN_UP;
    newline = 1;
  }
  return plotting;
}
int ps_comment (char *c)
{
  if (plotout)
    fprintf (plotout, "\n%%DEBUG %s\n", c);
  return 0;
}
