
/********************************************************************
 ****		rpws_label.graphics.c				*****
 ****	files.  						*****
 ********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <wtrplot.h>

#define _rpws_label_graphics_
#include "rpws_label_graphics.h"
#include "rpws_image.h"

static char *Version = { "V1.0" };

#define PI 3.141592654
#define XPOS   4.225
#define YUPPER 7.975
#define YLOWER 2.935
#define ROUTER 2.40
#define RINNER 0.77
int gr_circle (float x, float y, float r)
{
  float angle, a;
  float xoff, yoff;
  int ipen = PEN_UP;

  for (angle = 0.0; angle <= 360.0; angle += 5.0) {
    a = angle / 180.0 * PI;
    xoff = sin (a) * r;
    yoff = cos (a) * r;
    p_plot (x + xoff, y + yoff, ipen);
    ipen = PEN_DOWN;
  }
}

int gr_main (int func, struct GRAPHICS_TEXT *text, char directory[])
{
  char filename[128];
  static int label_index = 1;
  struct TABLE
  {
    float xoffset;
    float yoffset;
    float height;
    int justification;
    char *font;
  };
  struct TABLE upper_table[] = {
    0.0, 1.05, .250, JUST_CENTER, "Times-Bold", /*  0  */
    0.0, 1.40, .125, JUST_CENTER, "Times-Roman",        /*  1  */
    0.0, 1.58, .125, JUST_CENTER, "Times-Roman",        /*  2  */
    0.0, 1.76, .125, JUST_CENTER, "Times-Roman",        /*  3  */
    0.0, 2.00, .090, JUST_CENTER, "Times-Roman",        /*  4  */
    0.0, 2.16, .090, JUST_CENTER, "Times-Roman",        /*  5  */
  };

  struct TABLE lower_table[] = {
    0.0, -1.15, .250, JUST_CENTER, "Times-Roman",       /*  0  */
    0.0, -1.50, .125, JUST_CENTER, "Times-Roman",       /*  1  */
    0.0, -1.70, .100, JUST_CENTER, "Times-Bold",        /*  2  */
    0.0, -1.80, .100, JUST_CENTER, "Times-Bold",        /*  3  */
    0.0, -1.98, .100, JUST_CENTER, "Times-Roman",       /*  4  */
    0.0, -2.15, .085, JUST_CENTER, "Times-Roman",       /*  5  */
  };

  struct TABLE left_table[] = {
    -0.60, 0.83, .100, JUST_RIGHT, "Helvetica-Bold",    /*  0  */
    -0.65, 0.65, .100, JUST_RIGHT, "Helvetica", /*  1  */
    -0.70, 0.49, .100, JUST_RIGHT, "Helvetica", /*  2  */
    -0.76, 0.32, .100, JUST_RIGHT, "Helvetica", /*  3  */
    -0.83, 0.15, .100, JUST_RIGHT, "Helvetica", /*  4  */
    -0.85, -0.02, .100, JUST_RIGHT, "Helvetica",        /*  5  */
    -0.83, -0.19, .100, JUST_RIGHT, "Helvetica",        /*  6  */
    -0.76, -0.38, .100, JUST_RIGHT, "Helvetica",        /*  7  */
    -0.70, -0.55, .100, JUST_RIGHT, "Helvetica",        /*  8  */
    -0.65, -0.72, .100, JUST_RIGHT, "Helvetica",        /*  9  */
    -0.60, -0.89, .100, JUST_RIGHT, "Helvetica",        /*  10  */
    -0.55, -1.06, .100, JUST_RIGHT, "Helvetica",        /*  11  */
  };

  struct TABLE right_table[] = {
    0.60, 0.83, .100, JUST_LEFT, "Helvetica-Bold",      /*  0  */
    0.65, 0.65, .100, JUST_LEFT, "Helvetica",   /*  1  */
    0.70, 0.49, .100, JUST_LEFT, "Helvetica",   /*  2  */
    0.76, 0.32, .100, JUST_LEFT, "Helvetica",   /*  3  */
    0.83, 0.15, .100, JUST_LEFT, "Helvetica",   /*  4  */
    0.85, -0.02, .100, JUST_LEFT, "Helvetica",  /*  5  */
    0.83, -0.19, .100, JUST_LEFT, "Helvetica",  /*  6  */
    0.76, -0.38, .100, JUST_LEFT, "Helvetica",  /*  7  */
    0.70, -0.55, .100, JUST_LEFT, "Helvetica",  /*  8  */
    0.65, -0.72, .100, JUST_LEFT, "Helvetica",  /*  9  */
    0.60, -0.89, .100, JUST_LEFT, "Helvetica",  /*  10  */
    0.55, -1.06, .100, JUST_RIGHT, "Helvetica", /*  11  */
  };
  int i;
  float xpos, ypos;
  static int flag = 0;
  char string[128];

  switch (func) {
   case 0:
     sprintf (filename, "%s/MEDIA_LABEL_HR_%d.PS", directory, label_index++);
     p_setplt ("PS", "", filename, "", 1);
     break;
   case 2:
     p_wrapup ();
     flag = 0;
     break;
   case 1:
     switch (flag) {
      case 1:
        p_frame ();
      case 0:
        p_zinc (5, p_BLACK);
        xpos = XPOS;
        ypos = YUPPER;
        flag = 2;
        break;
      case 2:
        rpws_image (2);
        p_zinc (5, p_BLACK);
        xpos = XPOS;
        ypos = YLOWER;
        sprintf (string, "Printer model");
        p_symbol (.25, YUPPER + 1.350, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "HP Laser Jet 4M Plus");
        p_symbol (.25, YUPPER + 1.200, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "Positioned for");
        p_symbol (.25, YUPPER + 1.050, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "Fellowes");
        p_symbol (.25, YUPPER + 0.900, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "Photo Quality");
        p_symbol (.25, YUPPER + 0.750, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "Matte CD Labels");
        p_symbol (.25, YUPPER + 0.600, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "Neato-68863");
        p_symbol (.25, YUPPER + 0.450, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "USCD2lbl.NTT");
        p_symbol (.25, YUPPER + 0.300, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "XPOS = %.2f", XPOS);
        p_symbol (.25, YLOWER + 0.150, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "YUPPER = %.2f", YUPPER);
        p_symbol (.25, YLOWER + 0.000, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "YLOWER = %.2f", YLOWER);
        p_symbol (.25, YLOWER - 0.150, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "ROUTER = %.2f", ROUTER);
        p_symbol (.25, YLOWER - 0.300, .1, string, 90.0, JUST_LEFT, "");

        sprintf (string, "RINNER = %.2f", RINNER);
        p_symbol (.25, YLOWER - 0.450, .1, string, 90.0, JUST_LEFT, "");

        flag = 1;
        break;
     }
     gr_circle (xpos, ypos, ROUTER);
     gr_circle (xpos, ypos, RINNER);
     for (i = 0; i < 6; i++) {
       if (!text->upper_text[i])
         break;
       if (text->upper_text[i][0]) {
         p_symbol (xpos + upper_table[i].xoffset,
                   ypos + upper_table[i].yoffset,
                   upper_table[i].height,
                   text->upper_text[i],
                   0.0, upper_table[i].justification, upper_table[i].font);
       }
     }
     for (i = 0; i < 6; i++) {
       if (!text->lower_text[i])
         break;
       if (text->lower_text[i][0]) {
         p_symbol (xpos + lower_table[i].xoffset,
                   ypos + lower_table[i].yoffset,
                   lower_table[i].height,
                   text->lower_text[i],
                   0.0, lower_table[i].justification, lower_table[i].font);
       }
     }
     for (i = 0; i < 11; i++) {
       if (!text->left_text[i])
         break;
       if (text->left_text[i][0]) {
         p_symbol (xpos + left_table[i].xoffset,
                   ypos + left_table[i].yoffset,
                   left_table[i].height,
                   text->left_text[i],
                   0.0, left_table[i].justification, left_table[i].font);
       }
     }
     for (i = 0; i < 11; i++) {
       if (!text->right_text[i])
         break;
       if (text->right_text[i][0]) {
         p_symbol (xpos + right_table[i].xoffset,
                   ypos + right_table[i].yoffset,
                   right_table[i].height,
                   text->right_text[i],
                   0.0, right_table[i].justification, right_table[i].font);
       }
     }
  }
  return 0;
}
