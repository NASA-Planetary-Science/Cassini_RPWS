
/*
 * rpws_archive.c   
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
#include <ulimit.h>
#include <sys/types.h>
#include <sys/stat.h>

char *Title = { "LBL_VER" };
char *Version = { "V1.0" };

extern char *optarg;
extern int optind, opterr, optopt;

static char *getline (FILE * file, int *len)
{
  char *temp;
  char term;
  static char buffer[1024];

  len[0] = 256;
  temp = fgets (buffer, len[0], file);
  if (!temp)
    return NULL;
  len[0] = strlen (buffer);
  return buffer;
}
static int lbl_quote (char *line)
{
  int i;
  int len;
  int count = 0;

  len = strlen (line);
  for (i = 0; i < len; i++)
    if (line[i] == '"')
      count++;
  return count;
}
static int lbl_parand (char *line)
{
  int i;
  int len;
  int count = 0;

  len = strlen (line);
  for (i = 0; i < len; i++) {
    if (line[i] == '(')
      count++;
    if (line[i] == ')')
      count--;
  }
  return count;
}
static int lbl_brace (char *line)
{
  int i;
  int len;
  int count = 0;

  len = strlen (line);
  for (i = 0; i < len; i++) {
    if (line[i] == '{')
      count++;
    if (line[i] == '}')
      count--;
  }
  return count;
}
static int lbl_ver2 (char *filename, FILE * file)
{
  int line_number = 1;
  int result[2] = { 0, 0 };
  int len;
  char *line;
  char cr, lf;
  int quote_count = 0;
  int parand_count = 0;
  int brace_count = 0;

  while (line = getline (file, &len)) {
    result[0] = 0;
    if (line[len - 2] != 0x0D)
      result[0] |= 0x10;
    if (line[len - 1] != 0x0A)
      result[0] |= 0x20;
    if (len != 80)
      result[0] |= 0x08;
    quote_count += lbl_quote (line);
    parand_count += lbl_parand (line);
    brace_count += lbl_brace (line);
    if (result[0]) {
      result[1] |= result[0];
    }

#ifdef DEBUG1
    if (result[0] & 0x30)
      fprintf (stdout, "line: %3d len:%2d %02X/%02X\n",
               line_number, len, line[len - 2], line[len - 1]);
#endif

    line_number++;
  }
  if (quote_count & 1)
    result[1] |= 0x100;
  if (parand_count)
    result[1] |= 0x200;
  if (brace_count)
    result[1] |= 0x400;
  return result[1];
}
static int lbl_ver (char *filename)
{
  int i;
  FILE *lbl_file;

  lbl_file = fopen (filename, "r");
  if (lbl_file) {
    i = lbl_ver2 (filename, lbl_file);
    if (i & 0xFFF0) {
      fprintf (stdout, "Error: ");
      fprintf (stdout, "%s", (i & 0x010) ? "CR " : "");
      fprintf (stdout, "%s", (i & 0x020) ? "LF " : "");
      fprintf (stdout, "%s", (i & 0x008) ? "length " : "");
      fprintf (stdout, "%s", (i & 0x100) ? "quotes " : "");
      fprintf (stdout, "%s", (i & 0x200) ? "parand " : "");
      fprintf (stdout, "%s", (i & 0x400) ? "brace " : "");
      fprintf (stdout, "%s", filename);
      fprintf (stdout, "\n");
    } else if (i & 0x08)
      fprintf (stdout, "LEN %s\n", filename);
    else
      fprintf (stdout, "OK  %s\n", filename);
    fclose (lbl_file);
  } else {
    fprintf (stdout, "File not found %s\n", filename);
    return 0;
  }
  return 1;
}
int lbl_ver_help (char *argv, FILE * out)
{
  fprintf (out, "  %s %s\n", Title, Version);
  fprintf (out, "    Usage %s file file file ...\n");
  fprintf (out, "  \n");
  fprintf (out, "  \n");
  fprintf (out, "    Scan the label files for simple errors\n");
  fprintf (out, "  1. <cr><lf> as a line terminator.\n");
  fprintf (out, "  2. balanced quotes (i.e. even number\n");
  fprintf (out, "       of them occur in the file).\n");
  fprintf (out, "  3. balanced paranthesis.\n");
  fprintf (out, "  4. balanced braces.\n");
  fprintf (out, "  \n");
  fprintf (out, "  5. line length of 80 characters.\n");
  fprintf (out, "  \n");
  fprintf (out, "  \n");
  return 0;
}
int main (int argc, char *argv[])
{
  char c;
  int i;

  if (argc < 2) {
    lbl_ver_help (argv[0], stdout);
    exit (0);
  }

  while ((c = getopt (argc, argv, "?h")) != EOF) {
    switch (c) {
     case '?':
       fprintf (stdout, "Invalid Command Line Arguments\n");
     case 'h':
       lbl_ver_help (argv[0], stdout);
       exit (0);
    }
  }
  for (i = optind; i < argc; i++) {
    lbl_ver (argv[i]);
  }
}
