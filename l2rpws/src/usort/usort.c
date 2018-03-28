#include <stdlib.h>
#include <stdio.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"



struct element
{
  struct element *forward;
  struct element *reverse;
  long sequence;
  struct CDS_buffer buffer;
};
struct control
{
  struct element *head;
  struct element *tail;
  long sequence;
  int count;
};


static struct control lrs_head = { NULL, NULL, 0, 0 };
static struct control hrs_head = { NULL, NULL, 0, 0 };
static struct control hsk_head = { NULL, NULL, 0, 0 };
static struct control free_head = { NULL, NULL, 0, 0 };
static struct element *current = NULL;

 /*
  *     Build a linked list.  Look in the list-head to
  *     find the number of elementes to build...
  */
int usort_build_buffer (struct control *list_head)
{
  int i;
  struct element *temp;

  fprintf (stderr, "head %d\n", list_head->count);
  list_head->head = malloc (sizeof (struct element));
  list_head->tail = list_head->head;
  if (!list_head->head) {
    exit (0);
  }
  fprintf (stderr, "     %p\n", list_head->head);
  for (i = 1; i < list_head->count; i++) {
    temp = malloc (sizeof (struct element));    /* allocate element */
    if (!temp) {
      exit (0);
    }
    fprintf (stderr, "     %p\n", list_head->head);
    list_head->head->reverse = temp;    /* reset reverse link */
    temp->forward = list_head->head;    /* move forward link */
    list_head->head = temp;             /* add element */
  }
  return 1;
}

 /*
  *     element mainpulation routines 
  */
int usort_insert_head (struct control *list_head,
                       struct element *list_element)
{
  list_element->forward = list_head->head;
  list_head->head->reverse = list_element;
  list_head->head = list_element;
  list_head->count += 1;
  return list_head->count;
}
int usort_insert_tail (struct control *list_head,
                       struct element *list_element)
{
  list_element->reverse = list_head->tail;
  list_head->tail->forward = list_element;
  list_head->tail = list_element;
  list_head->count += 1;
  return list_head->count;
}
struct element *usort_examine_tail (struct control *list_head)
{
  return list_head->tail;
}
struct element *usort_examine_head (struct control *list_head)
{
  return list_head->head;
}
struct element *usort_remove_tail (struct control *list_head)
{
  struct element *temp;

  temp = list_head->tail;
  if (temp) {
    list_head->tail->reverse->forward = NULL;
    list_head->tail = list_head->tail->reverse;
    temp->forward = NULL;
    temp->reverse = NULL;
    list_head->count -= 1;
  }
  return temp;
}
struct element *usort_remove_head (struct control *list_head)
{
  struct element *temp;

  temp = list_head->head;
  if (temp) {
    list_head->head->forward->reverse = NULL;
    list_head->head = list_head->head->forward;
    temp->forward = NULL;
    temp->reverse = NULL;
    list_head->count -= 1;
  }
  return temp;
}
int usort_insert_ (struct control *list_head, struct element *list_element)
{
  return usort_insert_tail (list_head, list_element);
}
struct element *usort_remove_ (struct control *list_head)
{
  return usort_remove_head (list_head);
}
struct element *usort_remove_oldest (struct control *list_head)
{
  return usort_remove_oldest (list_head);
}
struct element *usort_examine_oldest (struct control *list_head)
{
  return usort_examine_oldest (list_head);
}

int usort_insert_SEQ (struct control *list_head, struct element *list_element)
{
  struct element *temp;
  int i;

  if (list_element->sequence < list_head->tail->sequence) {
    usort_insert_tail (list_head, list_element);
    list_head->count += 1;
    return list_head->count;
  }

  if (list_element->sequence > list_head->head->sequence) {
    usort_insert_head (list_head, list_element);
    list_head->count += 1;
    return list_head->count;
    return 1;
  }
  temp = list_head->head;
  for (i = 1; i < list_head->count; i++) {
    if (list_element->sequence > temp->sequence) {
      list_element->forward = temp;
      list_element->reverse = temp->reverse;
      temp->reverse->forward = list_element;
      temp->reverse = list_element;
      list_head->count += 1;
      return list_head->count;
    }
    temp = temp->forward;
  }
  return 0;
}

 /*
  *     find the packet sequence number and place it into
  *     the element header (so it's easy to get at)
  */
int usort_extract_SEQ (struct element *list_element)
{
  list_element->sequence = UTIL_extract_CDS_sequence (&list_element->buffer);
  return list_element->sequence;
}

 /*
  *     This is the micro-sort routine:
  *
  *     1. if sequence as expected, go ahead and write,
  *             release and return
  *
  *     2. insert the new buffer into the queue in the
  *             appropriate position. (i.e. sort it
  *             into the list)
  *
  *     3. if list is maximum length, extract the oldest
  *             element.
  *
  *     4. just for fun, see if we can release another element
  *
  */
int usort_ (struct control *list_head, struct element *list_element)
{
  struct element *temp;
  int cnt;

  if (list_head->sequence == list_element->sequence) {
    list_head->sequence += 1;
    UTIL_putbuffer_CDS (&list_element->buffer, stdout);
    usort_insert_ (&free_head, list_element);

/*	  usort_remove_all(list_head); /**/
    return 1;
  }

  cnt = usort_insert_SEQ (list_head, list_element);

  if (cnt >= list_head->count) {
    temp = usort_remove_oldest (list_head);
    list_head->sequence = temp->sequence + 1;
    UTIL_putbuffer_CDS (&temp->buffer, stdout);
    usort_insert_ (&free_head, temp);
  }

  temp = usort_examine_oldest (list_head);
  {
    if (list_head->sequence == temp->sequence) {
      list_head->sequence += 1;
      UTIL_putbuffer_CDS (&temp->buffer, stdout);
      usort_insert_ (&free_head, temp);
      return 1;
    }
  }

  return 1;
}
void main (int argc, char *argv[])
{


  fg_flags (argc, argv);
  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" \n");
    printf (" \n");
    printf (" uSort 1.0 microsort\n");
    printf (" \n");
    printf ("      -hrs nn   number of HRS packets to sort\n");
    printf ("      -lrs nn   number of LRS packets to sort\n");
    printf ("      -hsk nn   number of HSK packets to sort\n");
    printf (" \n");
    exit (0);
  }

  free_head.count = fg_int ("hrs", 16) +
    fg_int ("lrs", 8) + fg_int ("hsk", 2) + 9;
  hrs_head.count = 0;
  lrs_head.count = 0;

  usort_build_buffer (&free_head);

  while (1) {
    current = usort_remove_ (&free_head);
    UTIL_getbuffer_CDS (&current->buffer, stdin, UTIL_GET_BLOCKING);
    switch (UTIL_extract_packet_sindex (&current->buffer)) {
     default:
       UTIL_putbuffer_CDS (&current->buffer, stdout);
       usort_insert_ (&free_head, current);
       break;
     case 0:
       usort_ (&hsk_head, current);
       break;
     case 1:
       usort_ (&lrs_head, current);
       break;
     case 2:
       usort_ (&hrs_head, current);
       break;
    }
  }
}
