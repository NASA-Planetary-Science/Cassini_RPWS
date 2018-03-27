#define HEAD  1
#define SORT 2
#define SORT_NO_DUPLICATE 3
#define SORT_BY_RTI 4
#define TAIL -1
#define ITEM 0

/*
 *	When defined, this will put the actual byte count
 *	 into the header...
 */
#define DATA_LENGTH
#undef  DATA_LENGTH

struct DATA_RECORD
{
  char queue_name[8];
  struct DATA_RECORD *forward;          /* link to next element */
  struct DATA_RECORD *reverse;          /* link to previous element */
  struct DATA_RECORD *link;             /* link through all allocated elements */
  int head_flag;
  int element_count;
  unsigned int sclk;
  unsigned short rti;
  unsigned short type;
  struct CDS_buffer cds_record;         /* data record */
 };

#define  TYPE_FREE		0
#define  SEG_BIT		0x20

#define  LOW_RATE		1
#define  UNSEG_LOW_RATE		0x01
#define  SEG_LOW_RATE		0x20

#define  HIGH_RATE		2
#define  UNSEG_HIGH_RATE	0x02
#define  SEG_HIGH_RATE		0x23

#define  TYPE_MRO		3
#define  UNSEG_MRO		0xFF
#define  SEG_MRO		0xFF

#define  TYPE_HSK		4
#define  ROM_HSK		0x10
#define  MAINT_HSK		0x13
#define  SCIENCE_HSK		0X15

#define  TYPE_BAD		5

/* the table of all the valid data source id's */

/* when anything new is added, add it to the source id switch further on */

#define  STIM		  0x0
#define  MFR		  0x1
#define  HFR              0x2
#define  Langmuir_Probe   0x4
#define  LFDR 		  0x7
#define  WFR              0x8
#define  DUST 		  0xB
#define  MRO 		  0xD
#define  WBR              0xE
#define  FILL             0xF

#ifndef __mp2_
#define __mp2_
int MP2_generate_mp (FILE *,            /* output file */
                     int,               /* free pool size */
                     int,               /* minimum list size (to accomplish uSort) */
                     int,               /* data item write mask */
                     char *,            /* queue to process */
                     char *,            /* FREE queue */
                     FILE *,            /* debug file */
                     FILE *);           /* discard file */
#endif

#ifndef __mpii_
#define __mpii_
int mpii (FILE *, FILE *, int, int, int, int);
int mpii_enqueue (char *, struct DATA_RECORD *, int);

struct DATA_RECORD *mpii_dequeue (char *, struct DATA_RECORD *, int);
struct DATA_RECORD *mpii_find_head (char *);
#endif
