#include <stdio.h>

/*      CHDO.C  CHDO.C  CHDO.C  CHDO.C
 *	read chdo style data and convert to
 *	RPWS internal.
 *
 *	This routine knows how to pipe from "ctot" and knows how
 *	to ignore the extra text "ctot" spews occasionally (i.e.
 *	some of the status messages are routed to stdout...
 */

static char htxt[] = { "NJPL2I00C" };
struct chdop
{
  char header[12];
  int length_msw;
  int length;
} primary_header;
struct chdoh
{
  short type;
  short length;
};
struct chdoa
{
  short type;
  short length;
  char buffer[32768];
} aggregation;
static int dbg = 1;

 /*
  *     more debugging stuff...
  */
void dump_h (struct chdop buf)
{
  int i;

  if (dbg)
    fprintf (stderr, "CHDO ");
  for (i = 0; i < 12; i++)
    if (dbg)
      fprintf (stderr, "%c", buf.header[i]);
  if (dbg)
    fprintf (stderr, " : %d\n", buf.length);
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
int format (char *outbuf, char *inbuf, int offset)
{
  int i;
  int len;

  len = (unsigned) inbuf[4 + offset + 8] & 0x3F;
  len = (len << 8) + ((unsigned) inbuf[5 + offset + 8] & 0xFF);
  for (i = 0; i < len + 7; i++)
    outbuf[i ^ 1] = inbuf[i + offset + 8];
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
int chdo_getCDS (char *cds)
{
  int ilen, jlen;
  int res;

  res = 0;
  ilen = 0;
  jlen = 0;
  /*
   * ilen = fread(&primary_header, 20, 1, stdin);
   */
  ilen = chdo_SYNC (&primary_header, stdin);
  if (ilen) {
    jlen = fread (&aggregation, primary_header.length, 1, stdin);
    res = format (cds, &aggregation, aggregation.length);
  }
  return res;
}
