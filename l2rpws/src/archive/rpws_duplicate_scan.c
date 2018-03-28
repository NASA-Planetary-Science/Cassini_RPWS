
/********************************************************************
 ****		rpws_duplicate_scan.c				*****
 ********************************************************************/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "rpws_label.h"
 
#define DEBUG

static char *Version = { "V2.0" };

int rpws_duplicate_scan (struct RPWS_LABEL *rpws_label_head)
{
  struct RPWS_LABEL *label_temp;
  struct RPWS_LABEL *label_loop;
  int count_temp = 0;
  int count_loop = 0;

  label_temp = rpws_label_head;
  count_temp = 0;

#ifdef DEBUG
  fprintf (stdout, "%s %d\n", __FILE__, __LINE__);
  /**/
#endif

    while (label_temp) {

#ifdef DEBUG
    fprintf (stdout, "%4d  %s\n", count_temp, label_temp->thumbname);
    /**/
#endif

      label_loop = rpws_label_head;
    count_loop = 0;
    while (label_loop) {
      if (label_loop != label_temp) {
        if (!strcmp (label_loop->thumbname, label_temp->thumbname) &&
            !strstr (label_loop->filename, "10KHZD") &&
            !strstr (label_temp->filename, "10KHZD") &&
            !(label_loop->instrument & RPWS_LABEL_DUPLICATE) &&
            !(label_temp->instrument & RPWS_LABEL_DUPLICATE)) {

#ifdef DEBUG
          fprintf (stdout, "MATCH %d:%s %d:%s\n",
                   count_loop, label_loop->thumbname,
                   count_temp, label_temp->thumbname);
          /**/
#endif

            label_loop->instrument |= RPWS_LABEL_DUPLICATE;

          if (strcmp (label_loop->sclk_start, label_temp->sclk_start) < 0) {
            label_temp->sclk_start = label_loop->sclk_start;
            label_temp->scet_start = label_loop->scet_start;
            label_temp->scet_start_2 = label_loop->scet_start_2;
            label_temp->ephem_start = label_loop->ephem_start;
            label_temp->plot_start = label_loop->plot_start;
            label_temp->scet_start_3 = label_loop->scet_start_3;
          }

          if (strcmp (label_loop->sclk_stop, label_temp->sclk_stop) > 0) {
            label_temp->sclk_stop = label_loop->sclk_stop;
            label_temp->scet_stop = label_loop->scet_stop;
            label_temp->ephem_stop = label_loop->ephem_stop;
            label_temp->plot_stop = label_loop->plot_stop;
          }

        }
      }
      label_loop = label_loop->link;
      count_loop++;
    }
    label_temp = label_temp->link;
    count_temp++;
  }

#ifdef DEBUG
  label_loop = rpws_label_head;
  while (label_loop) {
    fprintf (stdout, "ds %8X %s %s\n", label_loop->instrument,
             label_loop->filename, label_loop->thumbname);
    label_loop = label_loop->link;
  }
#endif
}
