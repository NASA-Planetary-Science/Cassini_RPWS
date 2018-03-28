#include <stdio.h>
#include <string.h>

static char *title = { " CASSINI/RPWS HFR dump (stpk1) 0.1" };

int dump_hfr_packet (unsigned char *buf, int index)
{
  int i;
  int j;
  int width = 16;

  for (i = 0; i < (index / width); i++) {
    fprintf (stdout, "%4.4X:", i * width);
    for (j = 0; j < width; j++) {
      fprintf (stdout, " %2.2X", buf[i * width + j]);
    }
    fprintf (stdout, "\n");
  }
  if (index % width) {
    fprintf (stdout, "%4.4X:", i * width);
    for (j = 0; j < (index % width); j++) {
      fprintf (stdout, " %2.2X", buf[i * width + j]);
    }
    fprintf (stdout, "\n");
  }
}
int dump_header (unsigned char *buf, int index, int flag)
{
  int i;

  fprintf (stdout, "        ------------------\n");
  fprintf (stdout, "HFR Header %c%c%c%c%c  (%X)\n",
           buf[0], buf[1], buf[2], buf[3], buf[4], index);
  fprintf (stdout, " Time");
  if (flag) {
    for (i = 5; i < 13; i++)
      fprintf (stdout, " %02X", buf[i]);
    fprintf (stdout, " %04d-%02d-%02dT%02d:%02d:%02d.%03d",
             ((buf[10] << 8) & 0x00FF00) | ((buf[9] << 0) & 0x0000FF),
             buf[12], buf[11], buf[6], buf[5], buf[8], buf[7] * 10);
  } else
    fprintf (stdout, " supressed");
  fprintf (stdout, "\n");
  return 1;
}
int sync_buffer (unsigned char *buf)
{
  int i;
  int sts;
  unsigned char tag[] = { "StPk1" };

  sts = fread (buf, 5, 1, stdin);       /* prime the compare area */
  if (sts != 1)
    return 0;                           /* assume EOF on error */
  buf[5] = 0;                           /* delimit string */

  while (strcmp ((char *) buf, (char *) tag)) {
    for (i = 0; i < 5; i++)             /* move string up */
      buf[i] = buf[i + 1];
    sts = fread (&buf[4], 1, 1, stdin); /* next char */
    if (sts != 1)
      return 0;                         /* assume EOF on error */
    buf[5] = 0;                         /* re-delimit */
    if (1) {
      fprintf (stdout, "sync_buffer: ");
      for (i = 0; i < 5; i++)           /* move string up */
        fprintf (stdout, " %02X", buf[i]);
      fprintf (stdout, "\n");
    }
  }
  return 1;
}
int get_buffer (unsigned char *buf)
{
  int len;
  int sts;

  if (!sync_buffer (buf))               /* EOF ? */
    return 0;                           /* yes, then apss it on */
  sts = fread (&buf[5], 10, 1, stdin);  /* time & length */
  if (sts != 1)
    return 0;                           /* assume EOF on error */
  len = (buf[14] << 8) & 0x00FF00;
  len |= (buf[13] << 0) & 0x0000FF;
  sts = fread (&buf[15], len, 1, stdin);        /* time & length */
  if (sts != 1)
    return 0;                           /* assume EOF on error */
  return len;
}
void main (int argc, char *argv[])
{
  int len;
  int time_flag = 1;
  unsigned char *buffer;

  buffer = (unsigned char *) malloc (1024 * 128);

  fg_flags (argc, argv);
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stdout, "\n");
    fprintf (stdout, "%s   HELP SCREEN\n", title);
    fprintf (stdout, "  This utility requires StPk1 records\n");
    fprintf (stdout, "                \n");
    fprintf (stdout, "                \n");
    exit (0);
  }

  if (fg_flag ("time"))
    time_flag = 0;
  len = get_buffer (buffer);
  while (len) {
    if (1)
      dump_header (&buffer[0], len, time_flag);
    if (1)
      dump_hfr_packet (&buffer[13], len + 2);

    len = get_buffer (buffer);
  }

}
