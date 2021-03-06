#include <stdlib.h>
#include <stdio.h>
#include "rtiu.h"
#include "utilo.h"
#pragma HP_ALIGN NOPADDING

/*      CHDO.C  CHDO.C  CHDO.C  CHDO.C
 *	read chdo style data and convert to
 *	RPWS internal.
 *
 *	This routine knows how to pipe from "ctot" and knows how
 *	to ignore the extra text "ctot" spews occasionally (i.e.
 *	some of the status messages are routed to stdout...
 */

static char htxt[] = { "NJPL2I00C" };
struct sfdu_header
{
  char authority[4];
  short version_class;
  short spare;
  short ddp_id[2];
  long length_msw;
  long length;
};
struct chdo_header
{
  short type;
  short length;
};
struct chdo_data
{
  short type;
  short length;
  char data[2];
};
struct chdo_aggregation
{
  struct chdo_header agg;
  struct chdo_header hdr;
};
struct chdo_primary
{
  struct chdo_header header;
  short fill1[2];
};
struct chdo_secondary
{
  short fill1[5];
  short ert[3];
  short fill2[24];
  short rct[3];
  short fill3[8];
};
struct chdo_tertiary
{
  short fill1[7];
  short sclk[3];
  short scet[3];
  short fill2[2];
};

struct chdo_
{
  union
  {
    struct chdo_aggregation aggregation;
    struct chdo_header header;
    struct chdo_data data;
    struct chdo_primary primary;
    struct chdo_secondary secondary;
    struct chdo_tertiary tertiary;
  } rec;
};
static int dbg = 1;

 /*
  *     calculate the number that represents
  *     the epoch for SCLK expressed in UNIX
  *     time (i.e. add this number to SCLK to
  *     get a UNIX style time (seconds since
  *     1 JAN 1970)
  */
long chdo_sclkscet (struct CDS_buffer *buffer)
{
  static long scet_epoch = 378691200;
  static long scet_delta = -1;
  static int first = 1;
  long scet_seconds;

  if (first) {
    first = 0;
    scet_delta = utilo_int ("SCET_DELTA");
  }

  scet_seconds = buffer->packet.chdo_tag.scet.days * 86400;
  scet_seconds = scet_seconds +
    (buffer->packet.chdo_tag.scet.milliseconds / 1000);

  return scet_seconds
    - buffer->packet.chdo_tag.sclk.seconds - scet_epoch + scet_delta;
}

 /*
  *     looks like debugging stuff, eh?
  */
void dump (char *buf, int offset)
{
  int i;

  for (i = 0; i < 24; i++)
    if (dbg)
      fprintf (stderr, " %2.2X", (unsigned) buf[i + offset + 8] & 0x00FF);
  if (dbg)
    fprintf (stderr, "\n");
}

 /*
  *     THis little section does ant required byte-flipping.
  *     our little 8085 is a little-endian beast, and we
  *     tend to treat data like we see it within the 8085
  *     even though that requires munching up 16 bit words
  *     to use on HP/SUN systems...
  */
int format (struct CDS_buffer *cds_record, char *inbuf, int offset)
{
  int i;
  int len;

  if (0) {
    len = (unsigned) inbuf[offset + 12] & 0x3F;
    len = (len << 8) + ((unsigned) inbuf[offset + 13] & 0xFF);
    len += 7;
  } else {
    len = ((unsigned) inbuf[offset + 6] & 0x3F) << 8;
    len += ((unsigned) inbuf[offset + 7] & 0xFF);
  }

  for (i = 0; i < len; i++)
    cds_record->packet.cds.header[i ^ 1] = inbuf[i + offset + 8];

  return len;
}

 /*
  *     Look for the "htxt" string at the begining
  *       of a CHDO record.  Seems like cTOT occasionally
  *       spews chunks (i.e. status messages) between
  *       data records...
  */
int chdo_MATCH (char *hstg, int index_max)
{
  int index;

  if (index_max >= strlen (htxt))
    return 0;
  for (index = 0; index < index_max; index++) {
    if (hstg[index] != htxt[index])
      return 1;
  }
  return 2;
}

 /*
  *     Beginig of check for CHDO record alignment....
  */
int chdo_SYNC (char *primary_header, FILE * file)
{
  int index = 0;
  char hstg[21];

  memset (hstg, 0, 21);
  while (index < 20) {
    hstg[index++] = fgetc (file);
    if (feof (file))
      return 0;
    switch (chdo_MATCH (hstg, index)) {
     case 0:                           /* GOOD !!! */
     case 2:                           /* so far, so good */
       break;
     case 1:                           /* mis-match */
       hstg[0] = hstg[index - 1];
       memset (&hstg[1], 0, 20);
       index = 1;
       break;
    }
  }
  memcpy (primary_header, hstg, 20);
  return 1;
}
struct chdo_ *chdo_ancillary_hdr (struct CDS_buffer *cds, struct chdo_ *chdo)
{
  switch (chdo->rec.header.type) {
   default:
     break;
   case 92:                            /* 0x5C */
     memcpy (&cds->packet.chdo_tag.ert.days, chdo->rec.secondary.ert, 6);
     memcpy (&cds->packet.chdo_tag.rct.days, chdo->rec.secondary.rct, 6);
     break;
   case 94:                            /* 0x5E */
     memcpy (&cds->packet.chdo_tag.sclk.seconds, chdo->rec.tertiary.sclk, 6);
     memcpy (&cds->packet.chdo_tag.scet.days, chdo->rec.tertiary.scet, 6);
     break;
   case 10:                            /* data */
     break;
  }
  return &chdo->rec.data.data[chdo->rec.data.length];
}
int chdo_ancillary (struct CDS_buffer *cds, struct chdo_aggregation *aggr)
{
  struct chdo_ *chdo;
  int length;

  if (aggr->agg.type != 1)              /* see if chdo aggregation present */
    return 0;
  chdo = &aggr->hdr;
  length = aggr->agg.length;
  if (aggr->hdr.length) {               /* see if chdo headers present */
    while (length > 0) {
      length -= chdo->rec.data.length;
      length -= 4;
      chdo = chdo_ancillary_hdr (cds, chdo);
    /**/}
  /**/}
  return 0;
}
int chdo_getCDS (struct CDS_buffer *cds)
{
  static struct sfdu_header *primary_header = NULL;
  static struct chdo_aggregation *data_area = NULL;
  int ilen, jlen;
  int res;

  if (!primary_header)
    primary_header = malloc (16384);
  if (!data_area)
    data_area = malloc (16384);

  res = 0;
  ilen = 0;
  jlen = 0;
  ilen = chdo_SYNC ((char *) primary_header, stdin);
  if (ilen) {
    jlen = fread (data_area, primary_header->length, 1, stdin);
    res = format (cds, data_area, data_area->agg.length);
    chdo_ancillary (cds, data_area);
  /**/}
  return res;
}
