
/*
 * write_raw.c
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

#include <das2/das1.h>

#include <fg.h>

#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <UTIL_status.h>
#include <rpws_sclk.h>

#include "sort_merge.h"
#include "spice.h"

#define SOURCE_DATABASE 4
#define SOURCE_USER 0
#define BUFFER_SIZE 1276

char *write_raw_Version = { "V0.0" };

static int cds_time_bits[4] = { CDS_Time_Bits_07_00,
  CDS_Time_Bits_15_08,
  CDS_Time_Bits_23_16,
  CDS_Time_Bits_31_24
};

static FILE *file_handle;
static int file_count = 0;

static int write_record (struct CDS_buffer *buffer, FILE * status,
                         FILE * file_list)
{
  int sclk, fine;
  static char *old_name = { "" };
  static char *new_name = { "" };

  sclk = get_status_32 (buffer->packet.cds.header, cds_time_bits, __LINE__);
  fine = get_status (buffer->packet.cds.header, CDS_Time_Bits_SUB_Second, 0);

  new_name = sclk_2_filename (sclk, fine);

  if (strcmp (new_name, old_name)) {
    if (file_handle) {
      fclose (file_handle);
    /**/}
    unlink (new_name);                  /* we're in order, so kill old stuff */
    file_handle = fopen (new_name, "w");
    /**/ if (status) {
      /*
       * 000000000011111111112222222222333       
       */
      /*
       * 012345678901234567890123456789012       
       */
      fprintf (status, "KEY8 %32s %4d Filename\n", new_name, ++file_count);
      fprintf (file_list, "%s\n", new_name);
    }
  }
  UTIL_putbuffr2_CDS (buffer, file_handle, buffer->f_length);
  /**/ old_name = new_name;
  return 0;
}
int write_raw (struct SORT_ELEMENT *sort_element_head, FILE * status,
               FILE * file_list)
{
  int count = 0;
  struct SORT_ELEMENT *temp = sort_element_head;

  file_count = 0;
  while (temp) {
    count++;
    write_record (temp->buffer, status, file_list);
    temp = temp->link;
  }
  if (file_handle)
    fclose (file_handle);
  return file_count;
}
