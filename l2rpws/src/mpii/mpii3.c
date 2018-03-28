#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include <rtiu.h>
#include <util.h>
#include <utilf.h>

#define __mpii_
#include "mpii3.h"

FILE *que_diag_file = NULL;
FILE *diag_file = NULL;
FILE *debug_file = NULL;
FILE *dirt_file = NULL;

extern unsigned int sclk_stop;
extern unsigned int sclk_span;

#define POOL_MINIMUM 8
unsigned mpii_rti_offset = 0;
static int Pool_Size = 128;
static int Pool_Minimum = 8;

static int hrs_count = 0;
static int lrs_count = 0;
static int mro_count = 0;
static int hsk_count = 0;

struct DATA_RECORD Head[] = { {"FREE", NULL, NULL, NULL, 1, 0, 0, 0, TYPE_FREE},        /*0 */
{"LRS", NULL, NULL, NULL, 1, 0, 0, 0, LOW_RATE},        /*1 */
{"HRS", NULL, NULL, NULL, 1, 0, 0, 0, HIGH_RATE},       /*2 */
{"MRO", NULL, NULL, NULL, 1, 0, 0, 0, TYPE_MRO},        /*3 */
{"HSK", NULL, NULL, NULL, 1, 0, 0, 0, TYPE_HSK},        /*4 */
{"BAD", NULL, NULL, NULL, 1, 0, 0, 0, TYPE_FREE},       /*5 */
{"", NULL, NULL, NULL, 1, 0, 0, 0, 0,}
};                                      /*6 */


 /*
  * 
  */

 /*********************************************************
  *
  *	Find the appropriate queue
  *		We keep a separate list for each data channel
  *
  *********************************************************/
struct DATA_RECORD *mpii_find_head (char *name)
{
  int index = 0;

  while (Head[index].queue_name[0]) {
    if (!strcmp (name, Head[index].queue_name)) {
      return &Head[index];
    }
    index += 1;
  }
  return NULL;
}

 /*
  * 
  */

 /*********************************************************
  *
  *	SOME DIAGNOSTIC ROUTINES TO DUMP THE QUEUES	
  *
  *	mpii_dump		- dumps a single queue element
  *	mpii_dump_queue		- dumps a queue (calling mpii_dump)
  *	mpii_dump_queue_all	- dumps all queues (calling mpii_dump_queue)
  *
  *********************************************************/
void mpii_dump (FILE * diag, struct DATA_RECORD *element, int index)
{
  static char *format[] = {
    " %3d %4s  %8p: %8p %8p %8p %8.8X.%d    %2.2X\n",
    "HEAD %4s  %8p: --NULL--\n",
    "---- %4s  --head--  -forward -reverse --link-- ------------ TYPE ********\n",
    "     %4s  -element  -forward -reverse --link-- --sclk-- rti TYPE\n",
    "     %4s  --NULL--\n",
    "HEAD %4s  %8p: %8p %8p %8p               %2.2X\n",
    "---- %4s  --------  -------- -------- -------- ------------\n",
  };
  if ((index > 0) && (!element))
    index = -4;
  switch (index) {
   case -6:
   case -2:
   case -3:
   case -4:
     if (diag)
       fprintf (diag, format[0 - index], element->queue_name);
     break;
   case -1:
     if (diag)
       fprintf (diag, format[1], element->queue_name, element);
     break;
   case 0:
     if (diag)
       fprintf (diag, format[5],
                element->queue_name,
                element,
                element->forward,
                element->reverse, element->link, element->type);
     break;
   default:
     if (diag)
       fprintf (diag, format[0],
                index++,
                element->queue_name,
                element,
                element->forward,
                element->reverse,
                element->link,
                element->sclk, element->rti >> 5, element->type);
     break;
  }

  if (diag)
    fflush (diag);
  return;
}
int mpii_dump_queue (FILE * diag, char *queue_name)
{
  int index = 1;
  struct DATA_RECORD *temp;
  struct DATA_RECORD *head;

  head = mpii_find_head (queue_name);
  temp = head;
  mpii_dump (diag, head, -2);
  mpii_dump (diag, head, 0);
  mpii_dump (diag, head, -3);
  while (temp->forward) {
    temp = temp->forward;
    mpii_dump (diag, temp, index++);
  }
  mpii_dump (diag, head, -6);
  if (diag)
    fprintf (diag, "\n");
  if (diag)
    fflush (diag);
}

int mpii_dump_queue_all (FILE * diag)
{
  struct DATA_RECORD *temp;
  int index = 1;

  temp = mpii_find_head ("FREE");
  mpii_dump (diag, mpii_find_head ("FREE"), 0);
  mpii_dump (diag, mpii_find_head (" LRS"), 0);
  mpii_dump (diag, mpii_find_head (" HRS"), 0);
  mpii_dump (diag, mpii_find_head (" MRO"), 0);
  mpii_dump (diag, mpii_find_head (" HSK"), 0);
  mpii_dump (diag, NULL, -3);
  while (temp->link) {
    temp = temp->link;
    mpii_dump (diag, temp, index++);
  }
  if (diag)
    fflush (diag);
}

 /*
  * 
  */

 /*********************************************************
  *
  *	build the data pool
  *
  *********************************************************/
int mpii_allocate_memory (int new_pool_size)
{
  int i;
  struct DATA_RECORD *temp;
  struct DATA_RECORD *head;

  if (new_pool_size) {
    Pool_Size = new_pool_size;
    if (Pool_Size < POOL_MINIMUM)
      Pool_Size = POOL_MINIMUM;
  }
  Pool_Minimum = Pool_Size / 4;
  head = mpii_find_head ("FREE");
  head->link = (struct DATA_RECORD *) malloc (sizeof (struct DATA_RECORD));
  memset (head->link, 0, sizeof (struct DATA_RECORD));
  temp = head->link;
  head->forward = head->link;
  head->reverse = head->link;
  temp->forward = NULL;
  temp->reverse = NULL;
  temp->link = NULL;
  strcpy (temp->queue_name, "NEW");

  for (i = 0; i < Pool_Size - 1; i++) {
    temp->link = (struct DATA_RECORD *) malloc (sizeof (struct DATA_RECORD));
    if (!temp->link) {
      fprintf (stderr, "mpii-error-malloc fail, not enough free memory\n");
      exit (0);
    }
    memset (temp->link, 0, sizeof (struct DATA_RECORD));
    temp->forward = temp->link;
    temp = temp->link;                  /* follow link */
    temp->reverse = head->reverse;
    head->reverse = temp;
    strcpy (temp->queue_name, "NEW");
  }
  return 1;
}

 /*
  * 
  */

 /*********************************************************
  *
  *	Insert an element into the queue at 
  *	either head or tail.  If the HEAD/TAIL
  *	flag is set wrong, stick it on the free queue
  *
  *	Note the case of "SORT" that is used to sort
  *	  the queue according to time.
  *
  *********************************************************/
int mpii_fenqueue (struct DATA_RECORD *element)
{
  int status = 0;
  struct DATA_RECORD *head;

  head = mpii_find_head ("FREE");
  if (element->head_flag) {
    /*
     * fprintf(stderr," mpii_fenqueue (element->head_flag) is set\n"); /*
     */
    return 0;
  } else
    strcpy (element->queue_name, head->queue_name);

  if (head->forward) {
    element->forward = head->forward;
    element->reverse = NULL;
    head->forward = element;
    element->forward->reverse = element;
    head->element_count += 1;
    status = head->element_count;
  } else {
    element->forward = NULL;
    element->reverse = NULL;
    head->forward = element;
    head->reverse = element;
    head->element_count = 1;
    status = head->element_count;
  }
}

int mpii_enqueue (char *queue_name, struct DATA_RECORD *element, int where)
{
  int status = 0;
  struct DATA_RECORD *temp = NULL;
  struct DATA_RECORD *head = NULL;

  head = mpii_find_head (queue_name);
  if (!head) {
    fprintf (stderr, "mpii_enqueue: queue head! (%s)\n", queue_name);
    return 0;
  }
  strcpy (element->queue_name, head->queue_name);
  switch (where) {
   case SORT:
   case SORT_NO_DUPLICATE:
   case SORT_BY_RTI:
     if (head->forward) {
       temp = head;
       status = 1;
       while (status == 1) {
         if (where == SORT_NO_DUPLICATE)
           if (temp->sclk == element->sclk)
             if (temp->rti == element->rti)
               status = -1;
         if (where == SORT_BY_RTI) {
           unsigned temp_rti, element_rti;

           temp_rti =
             temp->cds_record.packet.mpp.mini_packet[0] |
             (temp->cds_record.packet.mpp.mini_packet[1] << 8);
           temp_rti += mpii_rti_offset;
           temp_rti &= 0xFFFF;

           element_rti =
             element->cds_record.packet.mpp.mini_packet[0] |
             (element->cds_record.packet.mpp.mini_packet[1] << 8);

           element_rti += mpii_rti_offset;
           element_rti &= 0xFFFF;

           if (temp_rti > element_rti)
             status = 0;
         } else if (temp->sclk >= element->sclk)
           if (temp->rti > element->rti)
             status = 0;
         if (!temp->forward)
           break;
         if (status == 1)
           temp = temp->forward;
       }
       switch (status) {
        case 1:                        /* after temp */
          element->reverse = head->reverse;
          element->forward = NULL;
          head->reverse = element;
          element->reverse->forward = element;
          head->element_count += 1;
          status = head->element_count;
          break;
        case 0:                        /* before temp */
          if (que_diag_file) {
            fprintf (que_diag_file, "mpii_enqueue 1\n");
            mpii_dump (que_diag_file, temp->reverse, 20);
            mpii_dump (que_diag_file, element, 21);
            mpii_dump (que_diag_file, temp, 22);
          }
          element->forward = temp;
          element->reverse = temp->reverse;
          temp->reverse = element;
          if (!element->reverse)
            head->forward = element;
          else {
            element->reverse->forward = element;
          }
          head->element_count += 1;
          status = head->element_count; /* ?? */
          break;
        case -1:                       /* swap out, use newer if same length */
          if (temp->cds_record.packet.chdo_ancillary.type_94.
              non_fill_length <=
              element->cds_record.packet.chdo_ancillary.type_94.
              non_fill_length) {
            element->forward = temp->forward;
            element->reverse = temp->reverse;
            if (element->reverse)
              element->reverse->forward = element;
            else
              head->forward = element;
            if (element->forward)
              element->forward->reverse = element;
            else
              head->reverse = element;
            temp->forward = NULL;
            temp->reverse = NULL;
            mpii_fenqueue (temp);
          } else {
            mpii_fenqueue (element);
          }
          if (que_diag_file) {
            fprintf (que_diag_file, "mpii_enqueue 2\n");
            mpii_dump (que_diag_file, element, 10);
            mpii_dump (que_diag_file, temp, 11);
            mpii_dump (que_diag_file, element->reverse, 12);
          }
          status = head->element_count;
          break;
        default:
          status = 0;
          break;
       }
     } else {
       element->forward = NULL;
       element->reverse = NULL;
       head->forward = element;
       head->reverse = element;
       head->element_count = 1;
       status = head->element_count;
     }
     break;

   case HEAD:
     if (head->forward) {
       element->forward = head->forward;
       element->reverse = NULL;
       head->forward = element;
       element->forward->reverse = element;
       head->element_count += 1;
       status = head->element_count;
     } else {
       element->forward = NULL;
       element->reverse = NULL;
       head->forward = element;
       head->reverse = element;
       head->element_count = 1;
       status = head->element_count;
     }
     break;

   case TAIL:
     if (head->reverse) {
       element->reverse = head->reverse;
       element->forward = NULL;
       head->reverse = element;
       element->reverse->forward = element;
       head->element_count += 1;
       status = head->element_count;
     } else {
       element->forward = NULL;
       element->reverse = NULL;
       head->forward = element;
       head->reverse = element;
       head->element_count = 1;
       status = head->element_count;
     }
     break;

   default:
     mpii_fenqueue (element);
     break;
  }
  if (que_diag_file) {
    fprintf (que_diag_file, "mpii_enqueue 3\n");
    mpii_dump (que_diag_file, element, 2);
    fprintf (que_diag_file, "..............\n");
  }
  return status;
}

 /*
  * 
  */

 /*********************************************************
  *
  *	Extract from Head, Tail
  *
  *********************************************************/
struct DATA_RECORD *mpii_dequeue (char *queue_name,
                                  struct DATA_RECORD *element, int where)
{
  int status = 0;
  int iwhere;
  int index = 0;
  struct DATA_RECORD *temp;
  struct DATA_RECORD *head;

  head = mpii_find_head (queue_name);
  switch (where) {
   default:
     temp = NULL;
     break;

   case HEAD:
     if (head->forward) {               /* empty queue ? */
       temp = head->forward;            /* *temp-> tail element */
       head->forward = head->forward->forward;
       /*
        * move last r-link to head 
        */

       if (temp->forward)
         temp->forward->reverse = NULL;
       else
         head->reverse = NULL;

       head->element_count -= 1;
       temp->forward = NULL;
       temp->reverse = NULL;
       temp->element_count = head->element_count;
     }
     break;

   case TAIL:
     temp = NULL;
     if (head->reverse) {               /* empty queue ? */
       temp = head->reverse;            /* *temp-> tail element */
       head->reverse = head->reverse->reverse;
       /*
        * move last r-link to head 
        */

       if (temp->reverse)
         temp->reverse->forward = NULL;
       else
         head->forward = NULL;

       head->element_count -= 1;
       temp->forward = NULL;
       temp->reverse = NULL;
       temp->element_count = head->element_count;
     }
     break;
  }

  if (temp)
    temp->queue_name[0] = 0;
  return temp;
}

 /*
  * 
  */

 /*********************************************************
  *
  *	This is kind-of obvious, but here to complete
  *	the complement of functions...  Simply chains to
  *	the next element in the list
  *
  *********************************************************/
struct DATA_RECORD *mpii_scan (struct DATA_RECORD *element, int where)
{
  switch (where) {
   default:
   case HEAD:
     return element->forward;
   case TAIL:
     return element->reverse;
  }
}

/*  **********************************************************************
  **********************************************************************
  *	  Implement some crude time filters here...
  **********************************************************************/

int _UTIL_getbuffer_CDS (struct CDS_buffer *cds_record,
                         FILE * input, int eof_flag)
{
  static int first_time = 1;
  int length;
  unsigned int pkt_time;

  length = UTIL_getbuffer_CDS (cds_record, input, eof_flag);
  pkt_time = UTIL_extract_TIME (cds_record);
  /*
   * fprintf(stderr, "pkt:%8X stop:%8X length:%d\n", pkt_time, sclk_stop, length); /*
   */
  if (first_time) {
    if (sclk_span)
      sclk_stop = pkt_time + sclk_span;
    fprintf (stderr, "start SCLK %8X  stop SCLK %8X\n", pkt_time, sclk_stop);
    first_time = 0;
  }
  if (length > 0)
    if (pkt_time > sclk_stop)
      length = -1;
  /*
   * fprintf(stderr, "pkt:%8X stop:%8X  RETURN\n", pkt_time, sclk_stop);/*
   */
  return length;
}

 /*
  *   **********************************************************************
  * **********************************************************************
  * *   main_fill_free_queue reads data (from stdin) and places
  * *     the data into the appropriate queue until the available
  * *     free space is all used up
  * ********************
  * *   We are depending on the fact that RPWS always produces unique
  * *     SCLK values to perform a micro sort.  Duplicates SCLK matches
  * *     are discarded by keeping the record with the largest 
  * *     non_fill_length then by keeping the most recently arrived 
  * *     record.
  * *********************************************************************
  */
int main_fill_free_queue (FILE * input, int eof_flag, int count)
{
  static int iLRS = 0;
  static int iHRS = 0;
  static int iMRO = 0;
  static int iHSK = 0;
  static int iBAD = 0;
  int icnt;
  struct DATA_RECORD *element;
  int i;
  int j;
  int jmax;
  int length;

  for (i = 0; i < count; i++) {
    element = mpii_dequeue ("FREE", NULL, TAIL);        /* was TAIL */
    if (element) {
      length = _UTIL_getbuffer_CDS (&element->cds_record, input, eof_flag);
      if (que_diag_file)
        fprintf (que_diag_file,
                 "main_fill_free_queue %d-getbuffer_CDS(%2.2X%2.2X)\n",
                 length,
                 element->cds_record.packet.cds.header[1] & 0x00FF,
                 element->cds_record.packet.cds.header[0] & 0x00FF);

      if (length < 0) {
        Pool_Minimum = 1;
        return 0;
      }

      element->sclk = element->cds_record.packet.cds.header[6] << 24 |
        element->cds_record.packet.cds.header[9] << 16 |
        element->cds_record.packet.cds.header[8] << 8 |
        element->cds_record.packet.cds.header[11] << 0;
      element->rti = element->cds_record.packet.cds.header[10] << 0;

      if (UTIL_extract_HRS (&element->cds_record)) {
        icnt = ++iHRS;
        element->type = HIGH_RATE;
        mpii_enqueue ("HRS", element, SORT_NO_DUPLICATE);
        hrs_count += 1;
      } else if (UTIL_extract_LRS (&element->cds_record)) {
        icnt = ++iLRS;
        element->type = LOW_RATE;
        mpii_enqueue ("LRS", element, SORT_NO_DUPLICATE);
        lrs_count += 1;
      } else if (UTIL_extract_MRO (&element->cds_record)) {
        icnt = ++iMRO;
        element->type = TYPE_MRO;
        mpii_enqueue ("MRO", element, SORT_NO_DUPLICATE);
        mro_count += 1;
      } else if (UTIL_extract_HSK (&element->cds_record)) {
        icnt = ++iHSK;
        element->type = TYPE_HSK;
        mpii_enqueue ("HSK", element, SORT_NO_DUPLICATE);
        hsk_count += 1;
      } else {
        icnt = ++iBAD;
        element->type = TYPE_BAD;
        mpii_enqueue ("FREE", element, HEAD);
      }
      if (dirt_file) {
        fprintf (dirt_file, "%s**(%4d) ",
                 Head[element->type].queue_name, icnt);
        fprintf (dirt_file, "%2d=getCDS(%P, stdin, %d) ",
                 length, &element->cds_record, UTIL_GET_BLOCKING);
        fprintf (dirt_file, "%02X%02X %02X%02X %02X%02X %X.%d.%d ",
                 element->cds_record.packet.cds.header[1],
                 element->cds_record.packet.cds.header[0],
                 element->cds_record.packet.cds.header[3] & 0x3F,
                 element->cds_record.packet.cds.header[2],
                 element->cds_record.packet.cds.header[5],
                 element->cds_record.packet.cds.header[4],
                 element->sclk, element->rti >> 5, element->rti >> 1 & 0x0F);
        fprintf (dirt_file, " [mp] %02X %02X %02X %02X",
                 element->cds_record.packet.cds.header[12],
                 element->cds_record.packet.cds.header[13],
                 element->cds_record.packet.cds.header[14],
                 element->cds_record.packet.cds.header[15]
          );
        jmax = 96;
        if (strcmp (Head[element->type].queue_name, "HSK"))
          jmax = 952;
        for (j = 0; j < jmax; j++) {
          if (!(j & 31))
            fprintf (dirt_file, "\n          %5d  %4X: ", j, j);
          fprintf (dirt_file, "%02X ",
                   element->cds_record.packet.cds.header[j]);
        }
        fprintf (dirt_file, "\n");
        fflush (dirt_file);
      }

    }

  }

  return -1;
}

 /*
  * 
  */

 /*********************************************************
  *
  *	diagnostic dump...
  *
  *********************************************************/
void main_dump (FILE * diag)
{
  if (diag)
    fprintf (diag, "%c", 0x0C);
  mpii_dump_queue (diag, "FREE");
  mpii_dump_queue (diag, "LRS");
  mpii_dump_queue (diag, "HRS");
  mpii_dump_queue (diag, "MRO");
  mpii_dump_queue (diag, "HSK");
  if (diag)
    fprintf (diag, "LRS=%d HRS=%d MRO=%d HSK=%d\n",
             lrs_count, hrs_count, mro_count, hsk_count);
  if (diag)
    fflush (diag);
  return;
}

 /*
  * 
  */

 /*********************************************************
  *
  *	purge queue.  Move all queue elements to the
  *	  free queue.  Housekeeping, for example,
  *	  is not present in the mini-packet stream
  *	  and may be safely discarded...
  *
  *********************************************************/
int mp_purge_queue (int count, char *head)
{
  int i;
  struct DATA_RECORD *element;

  if (count)
    for (i = 0; i < count; i++) {
      element = mpii_dequeue (head, NULL, TAIL);
      if (!element)
        break;
      mpii_enqueue ("FREE", element, HEAD);
    }
  return i;
}

 /*
  * 
  */

 /*********************************************************
  *	Generate m/p where needed
  *	discard others
  *********************************************************/
int main_generate_minipackets (FILE * output,
                               int count, int minimum, int item_mask)
{
  int packets = 0;

  packets += MP2_generate_mp (output,
                              count,
                              minimum,
                              item_mask,
                              "LRS", "FREE", debug_file, diag_file);
  packets += MP2_generate_mp (output,
                              count,
                              minimum,
                              item_mask,
                              "HRS", "FREE", debug_file, diag_file);
  packets += mp_purge_queue (count, "MRO");
  packets += mp_purge_queue (count, "HSK");
  return packets;
}

 /*
  * 
  */

 /*********************************************************
  *	This is easy, read data until no more free space
  *	  then purge the queues until less than "8"
  *	  elements are left, writing mini-packets as
  *	  needed
  **********************************************************/
void mpii (FILE * infile,               /* input file  */
           FILE * outfile,              /* output file  */
           unsigned int item_mask,      /* -filter mask  */
           int new_pool_size,           /* microsort pool  */
           int eof_flag,                /* real-time or NERT  */
           int debug_flag)
{                                       /* debugging shit  */
  int statusin = 1;
  int statusout = 1;
  int loop_count = 0;
  int fill_pool;
  int minimum_pool;

  if (debug_flag)
    fprintf (stderr, "mpii debug %X\n", debug_flag);
  if (debug_flag & 1)
    diag_file = fopen ("ddebug.mp2", "w");
  if (debug_flag & 2)
    debug_file = fopen ("mdebug.mp2", "w");
  if (debug_flag & 4)
    dirt_file = fopen ("cdebug.mp2", "w");      /* dump cds records */
  if (debug_flag & 8)
    que_diag_file = fopen ("qdebug.mp2", "w");  /* dump cds records */

  mpii_allocate_memory (new_pool_size);
  fill_pool = Pool_Size;
  minimum_pool = Pool_Minimum;

  if (fill_pool < 20) {                 /*  Check to see if we are          *//*  supressing micro-sort         */
    fill_pool = 1;                      /*  and adjust pool parameters    */
    minimum_pool = 0;                   /*  to cause soft size to be      */
  }
  /*
   * only 1 CDS record                  
   */
  while (statusin) {
    loop_count += 1;

    if (que_diag_file)
      fprintf (que_diag_file,
               "\n******************************** %d\n", loop_count);
    mpii_dump_queue (que_diag_file, "FREE");

    statusin = main_fill_free_queue (infile, eof_flag, fill_pool);
    if (Pool_Minimum == 1)
      minimum_pool = 1;

    mpii_dump_queue (que_diag_file, "LRS");
    mpii_dump_queue (que_diag_file, "HRS");
    mpii_dump_queue (que_diag_file, "HSK");
    mpii_dump_queue (que_diag_file, "MRO");
    mpii_dump_queue (que_diag_file, "BAD");

    statusout = main_generate_minipackets (outfile,
                                           Pool_Size,
                                           minimum_pool, item_mask);
  }

  exit (0);
}
