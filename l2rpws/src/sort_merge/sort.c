
/*
 * sort.c 
 */

#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>

/* Other Cas Module includes */
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <UTIL_status.h>

/* #include "cds_status.h"
#include "stim_status.h"*/

#include "rpws_sclk.h"

#define _sort_
#include "sort.h"

char *SORT_Version = { "V0.0" };
static FILE *error_file = NULL;

static int cds_time_bits[4] = { CDS_Time_Bits_07_00,
  CDS_Time_Bits_15_08,
  CDS_Time_Bits_23_16,
  CDS_Time_Bits_31_24
};

struct SORT_LIST *SORT_open (FILE * erfile)
{
  struct SORT_LIST *list_head;
  list_head = malloc (sizeof (struct SORT_LIST));
  if (erfile)
    error_file = erfile;
  list_head->link = NULL;

#ifdef DEBUG_001
  fprintf (stdout, "OPEN %p\n", list_head);
#endif

  return list_head;
}

int SORT_insert (struct SORT_LIST *list_head, struct CDS_buffer *buffer)
{
  struct SORT_LIST *temp[3] = { NULL, NULL, NULL };
  struct SORT_LIST *newb;
  int new_ID;

   /***************************************************************************/

   /*******	Scarf off some space					*******/

   /*******		and copy buffer, while we're at it		*******/

   /***************************************************************************/
  newb = malloc (sizeof (struct SORT_LIST));
  newb->link = NULL;
  if (!newb) {
    if (error_file)
      fprintf (error_file, "%s/%d Malloc failed\n", __FILE__, __LINE__);
    exit (0);
  }
  newb->buffer = malloc (sizeof (struct CDS_buffer));
  memcpy (newb->buffer, buffer, buffer->f_length + 4);

#ifdef DEBUG_001
  fprintf (stdout, "NEWB %p BUFFER %p\n", newb, newb->buffer);
#endif

   /***************************************************************************/

   /*******	Build clock						*******/

   /*******	  LRS and HRS are unique, hack HSK to make it unique	*******/

   /***************************************************************************/
  newb->sclk = (double) get_status_32 (newb->buffer->packet.cds.header,
                                       cds_time_bits, __LINE__);
  newb->sclk += (double) get_status (newb->buffer->packet.cds.header,
                                     CDS_Time_Bits_SUB, 0) / 256.0;

  new_ID = get_status (newb->buffer->packet.cds.header, CDS_Packet_ID, 0);
  switch (new_ID) {
   case CDS_Packet_ID_Housekeeping_ROM:
   case CDS_Packet_ID_Housekeeping_Deploy:
   case CDS_Packet_ID_Housekeeping_Science:
     newb->sclk += .102;                /* make housekeeping sort at the end */
     break;
  }

   /***************************************************************************/

   /*******	No SORT LSIT ARGUMENT supplied				*******/

   /***************************************************************************/
  if (!list_head) {
    if (error_file)
      fprintf (error_file, "%s/%d SORT_LIST argument NULL\n", __FILE__,
               __LINE__);
    exit (0);
  }

   /***************************************************************************/

   /*******	First Element						*******/

   /***************************************************************************/
  temp[0] = list_head->link;
  if (temp[0])
    temp[1] = temp[0]->link;
  else {
    list_head->link = newb;

#ifdef DEBUG_001
    fprintf (stdout, "FIRST\n");
#endif

    return 1;
  }

#ifdef DEBUG_001
  fprintf (stdout, "   %p %p\n", temp[0], temp[1]);
#endif

   /***************************************************************************/

   /*******	Belongs at begining					*******/

   /***************************************************************************/
  if (newb->sclk < temp[0]->sclk) {
    newb->link = list_head->link;
    list_head->link = newb;

#ifdef DEBUG_001
    fprintf (stdout, "BEGIN\n");
#endif

    return 1;
  }

   /***************************************************************************/

   /*******	Belongs at end of short list ???			*******/

   /***************************************************************************/
  if (!temp[1]) {
    if (newb->sclk == temp[0]->sclk) {
      free (newb->buffer);
      free (newb);
      return 0;
    }
    temp[0]->link = newb;

#ifdef DEBUG_001
    fprintf (stdout, "END\n");
#endif

    return 1;
  }

   /***************************************************************************/

   /*******	Belongs in middle ???					*******/

   /***************************************************************************/

#ifdef DEBUG_001
  fprintf (stdout, "MIDDLE\n");
#endif

  while (temp[1]) {

#ifdef DEBUG_001
    fprintf (stdout, "  %.3f  %.3f\n", newb->sclk, temp[1]->sclk);
#endif

    if (newb->sclk == temp[1]->sclk) {
      free (newb->buffer);
      free (newb);
      return 0;
    }
    if (newb->sclk < temp[1]->sclk) {
      newb->link = temp[0]->link;
      temp[0]->link = newb;
      return 1;
    }
    temp[0] = temp[0]->link;
    temp[1] = temp[0]->link;
  }
  temp[0]->link = newb;
  return 1;
}
