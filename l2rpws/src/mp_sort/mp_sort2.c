
/*
 * mp_sort2.c
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"

#define	MAX_BUFFERS 8192

#define FREE_QUEUE	0
#define SORT_QUEUE	1
#define HEAD  1
#define SORT  2
#define TAIL -1

struct MP_buffer in_buffer;
struct RPWS_buffer *out_buffer;
FILE *input = NULL;
FILE *output = NULL;
static char *version = { "mp_sort V2.0" };

struct DATA_RECORD
{
  struct DATA_RECORD *forward;          /* link to next element */
  struct DATA_RECORD *reverse;          /* link to previous element */
  struct DATA_RECORD *link;             /* link this element */
  int head_flag;
  int element_count;
  double evt_clk;
  struct MP_buffer mp_record;           /* data record */
};

struct DATA_RECORD Head[] = { {NULL, NULL, NULL, 1, 0, 0.0},    /* Free Space */
{NULL, NULL, NULL, 1, 0, 0.0}
};                                      /* Sort buffer */

int mpii_allocate_memory (int new_pool_size);
int mpii_dump (FILE * diag, struct DATA_RECORD *element, int index);
int mpii_dump_queue (FILE * diag, int queue_index);
int mpii_dump_queue_all (FILE * diag);
int mpii_enqueue (int queue_number, struct DATA_RECORD *element, int where);
struct DATA_RECORD *mpii_dequeue (int queue_number, int where);

main (int argc, char *argv[])
{
  int ilen, i, j;
  int debug_flag = 0;
  int sort_all = 0;
  char *in_name = { "stdin" };
  char *out_name = { "stdout" };
  int eof_flag = UTIL_GET_BLOCKING;

  static int epoch;
  time_t pkt_time, pkt_etime;
  struct event_clock evt_clk;
  int flag_yday = 1;
  int num_buffers;
  int index = 0;
  double min_clk, next_clk;
  struct DATA_RECORD *free;
  struct DATA_RECORD *sorted;

  fg_flags (argc, argv);
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stderr, "%s   HELP SCREEN\n", version);
    fprintf (stderr, "\n");
    fprintf (stderr, "     +eof        pass eof from input file\n");
    fprintf (stderr, "     +debug      debug information\n");
    fprintf (stderr, "     +sort       sort all data\n");
    fprintf (stderr, "     -buffers nn allocate nn buffers (default 8192)\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "\n");
    exit (0);
  }


  if (fg_flag ("eof"))
    eof_flag = UTIL_GET_NON_BLOCKING;

  if (fg_flag ("debug"))
    debug_flag = 1;

  if (fg_flag ("sort"))
    sort_all = 1;

  num_buffers = fg_int ("buffers", MAX_BUFFERS);
  if ((num_buffers <= 0) || (num_buffers > MAX_BUFFERS))
    num_buffers = MAX_BUFFERS;

  mpii_allocate_memory (num_buffers);

  input = stdin;
  output = stdout;
  fprintf (stderr, "%s: input file: %s\n", version, in_name);
  fprintf (stderr, "%s: output file: %s\n", version, out_name);
  fprintf (stderr, "%s: number of buffers: %d\n", version, num_buffers);

  if (debug_flag)
    mpii_dump_queue_all (stderr);

  sleep (3);
  ilen = UTIL_getbuffer_MP (&in_buffer, input, eof_flag);
  while (ilen > 0) {
    pkt_etime = UTIL_event_time (&in_buffer, 0);
    evt_clk.seconds = pkt_etime;
    evt_clk.fine = UTIL_extract_MP_RTI (&in_buffer) << 5;

    out_buffer = NULL;

/*
	if ( sort_all && ((UTIL_extract_MP_type(&in_buffer) & 0x0f)!=0x0E) )
*/

    if (sort_all && (in_buffer.packet.index.data_start))
    {
      free = mpii_dequeue (FREE_QUEUE, TAIL);   /* get free space buffer */
      if (free) {

/* Free Space buffer was available. Move input record to free space buffer */

        memcpy (&free->mp_record, &in_buffer, sizeof (in_buffer));
        free->evt_clk = (double) evt_clk.seconds +
          ((double) evt_clk.fine) / 256.00;

/* Insert record into sort queue, sorted by event clock. */

        mpii_enqueue (SORT_QUEUE, free, SORT);

        if (debug_flag)
          mpii_dump_queue (stderr, SORT_QUEUE);

      } else {

/* Buffers are full. Flush out the mini-packets with the earliest time. */

        sorted = mpii_dequeue (SORT_QUEUE, HEAD);
        min_clk = sorted->evt_clk;
        do {
          out_buffer = (struct RPWS_buffer *) &(sorted->mp_record);
          UTIL_putbuffr2_RPWS (out_buffer, output, out_buffer->f_length);
          mpii_enqueue (FREE_QUEUE, sorted, HEAD);
          sorted = mpii_dequeue (SORT_QUEUE, HEAD);
        } while (sorted->evt_clk == min_clk);
        if (sorted)
          mpii_enqueue (SORT_QUEUE, sorted, HEAD);      /* put back */
        free = mpii_dequeue (FREE_QUEUE, TAIL); /* get free space buffer */
        if (free) {

/* Free Space buffer was available. Move input record to free space buffer */

          memcpy (&free->mp_record, &in_buffer, sizeof (in_buffer));
          free->evt_clk = (double) evt_clk.seconds +
            ((double) evt_clk.fine) / 256.00;

/* Insert record into sort queue, sorted by event clock. */

          mpii_enqueue (SORT_QUEUE, free, SORT);

          if (debug_flag)
            mpii_dump_queue (stderr, SORT_QUEUE);

        } else
          exit (-1);
      }
    } else if (in_buffer.packet.index.data_start) {
      out_buffer = (struct RPWS_buffer *) &in_buffer;
      UTIL_putbuffr2_RPWS (out_buffer, output, out_buffer->f_length);
    }

    ilen = UTIL_getbuffer_MP (&in_buffer, input, eof_flag);
  }
  fclose (input);

/* See if data is still sitting in buffers. Flush it. */
  if (sorted = mpii_dequeue (SORT_QUEUE, HEAD)) {
    do {
      out_buffer = (struct RPWS_buffer *) &(sorted->mp_record);
      UTIL_putbuffr2_RPWS (out_buffer, output, out_buffer->f_length);
      sorted = mpii_dequeue (SORT_QUEUE, HEAD);
    } while (sorted);
  }
  fclose (output);
}

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

  if (new_pool_size <= 0)
    exit (-1);

  head = &Head[0];                      /* free space queue */
  head->link = (struct DATA_RECORD *) malloc (sizeof (struct DATA_RECORD));
  memset (head->link, 0, sizeof (struct DATA_RECORD));
  temp = head->link;
  head->forward = head->link;
  head->reverse = head->link;
  head->element_count = 1;
  temp->forward = NULL;
  temp->reverse = NULL;
  temp->link = NULL;

  for (i = 0; i < new_pool_size - 1; i++) {
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
    head->element_count += 1;
  }
  return 1;
}

 /*********************************************************
  *
  *	SOME DIAGNOSTIC ROUTINES TO DUMP THE QUEUES	
  *
  *	mpii_dump		- dumps a single queue element
  *	mpii_dump_queue		- dumps a queue (calling mpii_dump)
  *	mpii_dump_queue_all	- dumps all queues (calling mpii_dump_queue)
  *
  *********************************************************/
int mpii_dump (FILE * diag, struct DATA_RECORD *element, int index)
{
  static char *format[] = {
    " %3d  %8p: %8p %8p %8p %15.3f\n",
    "HEAD  %8p: --NULL--\n",
    "----  --head--  -forward -reverse --link--\n",
    "      -element  -forward -reverse --link-- --evt_clk---\n",
    "      --NULL--\n",
    "HEAD  %8p: %8p %8p %8p               \n",
    "----  --------  -------- -------- -------- ------------\n",
  };
  if ((index > 0) && (!element))
    index = -4;
  switch (index) {
   case -6:
   case -2:
   case -3:
   case -4:
   case -1:
     if (diag)
       fprintf (diag, format[1], element);
     break;
   case 0:
     if (diag)
       fprintf (diag, format[5],
                element, element->forward, element->reverse, element->link);
     break;
   default:
     if (diag)
       fprintf (diag, format[0],
                index++,
                element,
                element->forward,
                element->reverse, element->link, element->evt_clk);
     break;
  }

  if (diag)
    fflush (diag);
  return;
}
int mpii_dump_queue (FILE * diag, int queue_index)
{
  int index = 1;
  struct DATA_RECORD *temp;
  struct DATA_RECORD *head;

  if (queue_index == 0)
    fprintf (diag, "Free Space\n");
  else
    fprintf (diag, "Sort Space\n");
  head = &Head[queue_index];
  temp = head;

/*      mpii_dump(diag, head, -2);	*/
  mpii_dump (diag, head, 0);

/*      mpii_dump(diag, head, -3);	*/
  while (temp->forward) {
    temp = temp->forward;
    mpii_dump (diag, temp, index++);
  }

/*      mpii_dump(diag, head, -6);	*/
  if (diag)
    fprintf (diag, "\n");
  if (diag)
    fflush (diag);
}

int mpii_dump_queue_all (FILE * diag)
{
  struct DATA_RECORD *temp;
  int index = 1;

  temp = &Head[0];
  mpii_dump (diag, temp, 0);
  mpii_dump (diag, &Head[1], 0);
  mpii_dump (diag, NULL, -3);
  while (temp->link) {
    temp = temp->link;
    mpii_dump (diag, temp, index++);
  }
  if (diag)
    fflush (diag);
}

 /*********************************************************
  *
  *	Extract from Head or Tail
  *
  *********************************************************/
struct DATA_RECORD *mpii_dequeue (int queue_number, int where)
{
  struct DATA_RECORD *temp;
  struct DATA_RECORD *head;

  head = &Head[queue_number];
  switch (where) {
   default:
     temp = NULL;
     break;

   case HEAD:
     temp = NULL;
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

  return temp;
}

int mpii_enqueue (int queue_number, struct DATA_RECORD *element, int where)
{
  int status = 0;
  struct DATA_RECORD *temp = NULL;
  struct DATA_RECORD *head = NULL;

  head = &Head[queue_number];
  if (!head) {
    fprintf (stderr, "mpii_enqueue: queue head! (%d)\n", queue_number);
    return 0;
  }
  switch (where) {
   case SORT:
     if (head->forward) {
       temp = head;
       status = 1;
       while (status == 1) {
         if (temp->evt_clk > element->evt_clk)
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
     break;
  }
  return status;
}
