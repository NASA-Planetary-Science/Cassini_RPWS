#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#define BUFFER_SIZE 1024

static char *Version = { "V5.1" };

static int flag_lf = 0x0A;
static int flag_cr = 0x0D;
static int flag_pad = 0;
static int flag_tab = 8;
static int flag_max = 78;
static int flag_strip = 0;
static int flag_spice = 0;
static char flag_html[64] = { 64 * 0 };
static char *flag_date = NULL;
static char *flag_delete = NULL;
static int flag_delete_len = 0;
static char flag_nest = 0;
static char include[1024] = { 1024 * 0 };
static char delete[1024] = { 1024 * 0 };
static char pad_chars[256] = { 256 * 0 };

static int pad_rule = 0;

FILE *outfile = stdout;
FILE *infile = stdin;
FILE *html_file = NULL;
extern char *optarg;
extern int optind;

static int line_count = 0;
static int include_count = 0;
static int delete_count = 0;
static int help (int argc, char *argv[])
{
  fprintf (stdout, "  Utility to reformat text files %s\n", Version);
  fprintf (stdout, "  into correct PDS_ese (length/tabs, etc.)\n");
  fprintf (stdout, "  \n");
  fprintf (stdout, "usage: %s < source_file > destination_file\n", argv[0]);
  fprintf (stdout, "      -a       input file (defauilts to stdin)\n");
  fprintf (stdout, "      -b       output file (defauilts to stdout)\n");
  fprintf (stdout,
           "      -g path  Alter reference in HTML file from gif to PNG (change case of filename too)\n");
  fprintf (stdout, "      -G file  List files that have been changed\n");
  fprintf (stdout, "      -l       suppress line feed (defualt %s)\n",
           flag_lf ? "append <lf>" : "suppress <lf>");
  fprintf (stdout, "      -c       suppress carriage return (defualt %s)\n",
           flag_cr ? "append <cr>" : "suppress <cr>");
  fprintf (stdout, "      -s       strips trailing blanks (default %s)\n",
           flag_strip ? "strip blanks" : "leave blanks");
  fprintf (stdout, "      -tnn     expand tabs (default %d)\n", flag_tab);
  fprintf (stdout, "      -mnn     max length (default %d)\n", flag_max);
  fprintf (stdout, "                 ZERO suppresses this function\n");
  fprintf (stdout, "      -pnn     pad (default %d)\n", flag_pad);
  fprintf (stdout,
           "                 this number does NOT include <cr> or <lf>\n");

  fprintf (stdout, "      -P\"xyz\"     pad (default %d) and add \"xyz\"\n",
           flag_pad);
  fprintf (stdout,
           "                 this number does NOT include <cr> or <lf>\n");
  fprintf (stdout,
           "                 this is for assembler code, it will not strip\n");
  fprintf (stdout, "                 code before the \"x\" delimiter\n");

  fprintf (stdout,
           "      -i/path  expand include files (#include \"xxx.h\")\n");
  fprintf (stdout, "      -i/path,path,path (multiple paths, if needed)\n");
  fprintf (stdout, " \n");
  fprintf (stdout, "      -d/path  discard include files with this string\n");
  fprintf (stdout, "      -x       comment out #define SPICE\n");
  fprintf (stdout, "      -y xxx   replace \"xxx\" with the current data\n");
  fprintf (stdout,
           "                  Simply a method to insert a date into\n");
  fprintf (stdout, "                  sample code for the PDS archive\n");
  fprintf (stdout, "      -z \"xxx\"   delete the inidcated text string\n");
  fprintf (stdout, " \n");
  fprintf (stdout, "  Example uses:\n");
  fprintf (stdout,
           "      %s -m0 -t0 -s < file.htm > FILE.HTM    # line end <cr><lf>\n",
           argv[0]);
  fprintf (stdout,
           "      %s -p78 < file.lbl > FILE.LBL          # pad to 80 characters\n",
           argv[0]);
  fprintf (stdout,
           "      %s -s < file.lbl > FILE.LBL            # line end <cr><lf> limit to 80 char\n",
           argv[0]);
  fprintf (stdout,
           "      %s -s -i/path,path < file.c > FILE.C   # line end <cr><lf> limit to 80 char\n",
           argv[0]);
  fprintf (stdout,
           "      %s -t8 -c -m128 -p65 -P\";|\" < in > out # eZ80 assembler tidy\n",
           argv[0]);

  return 0;
}
static int main_line (FILE * outfile, char *inbuf)
{
  int i;
  int in;
  int len;
  int out = 0;
  static char outbuf[BUFFER_SIZE * 2];

  memset (outbuf, ' ', BUFFER_SIZE);    /* fill with spaces */
  memset (outbuf + BUFFER_SIZE - 1, 0, BUFFER_SIZE);    /* fill with zero */
  out = 0;                              /* size of buffer */
  len = strlen (inbuf);
  if (len > BUFFER_SIZE) {
    fprintf (stderr, "Input line too long %d\n", len);
    exit (0);
  }
  for (in = 0; in < len; in++) {        /* copy buffer */
    if ((inbuf[in] == 0x09) && flag_tab) {      /* expand tabs */
      out++;
      out += (flag_tab - 1);
      out &= ~(flag_tab - 1);
      if (out > BUFFER_SIZE) {
        fprintf (stderr, "%d %d SHIT out(%d) too big\n", __LINE__, line_count,
                 out);
        exit (0);
      }
    } else {
      switch (inbuf[in] & 0x7F) {
       default:
         outbuf[out++] = inbuf[in];
         break;
       case 0x0D:                      /* strip existing cr/lf */
       case 0x0A:
       case 0x00:
         break;
      }
      if (out > BUFFER_SIZE) {
        fprintf (stderr, "%d %d SHIT out(%d) too big\n", __LINE__, line_count,
                 out);
        exit (0);
      }
    }
  }
  outbuf[out] = 0;


  pad_rule = 0;
  if (flag_pad) {
    pad_rule = 1;
    if (strlen (pad_chars))
      pad_rule = 2;
  }
  if (outbuf[out - 1] == '\\')
    pad_rule = 0;
  switch (pad_rule) {
   case 1:
     {
       outbuf[out] = ' ';
       outbuf[flag_pad] = 0;
       out = flag_pad;
       break;
     }
   case 2:
     {
       int len;                         /* overall length */
       int sub_len;                     /* important length */
       char delim;

       len = strlen (outbuf);
       delim = pad_chars[0];

       for (sub_len = 0; sub_len < len; sub_len++) {
         if (outbuf[sub_len] == delim)
           break;
       }
       /*
        *     OK, by now
        *             len is buffer length
        *             sub_len is the length of the buffer before the delimiter
        */
       if (sub_len < flag_pad) {        /* OK to pad out ??? */
         outbuf[out] = ' ';
         outbuf[flag_pad] = 0;
         strcat (outbuf, pad_chars);
       }
       out = strlen (outbuf);
       break;
     }
  }

  if (flag_strip) {                     /* strip trailing blanks */
    out = strlen (outbuf) - 1;
    while (out) {
      if (outbuf[out] == ' ')
        outbuf[out] = 0;
      else
        break;
      out--;
    }
  }


  out = strlen (outbuf);                /* new line length */

  if (out > BUFFER_SIZE) {
    fprintf (stderr, "%d %d SHIT out(%d) too big\n", __LINE__, line_count,
             out);
    exit (0);
  }

  if (flag_max) {                       /* limiting length ??? */
    if (out > flag_max) {
      if (1) {
        if (include_count)
          fprintf (stderr,
                   "******** Truncate include line %d from %d to %d ********\n",
                   include_count, out, flag_max);
        /**/
        else
          fprintf (stderr,
                   "******** Truncate line %d from %d to %d ********\n",
                   line_count, out, flag_max);
      /**/}
      out = flag_max;
    }
  }

  if (flag_cr)                          /* append cr/lf as required */
    outbuf[out++] = flag_cr;

  if (flag_lf)
    outbuf[out++] = flag_lf;

  outbuf[out] = 0;                      /* terminate end of buffer */
  if (out > BUFFER_SIZE) {
    fprintf (stderr, "%d %d SHIT out(%d) too big\n", __LINE__, line_count,
             out);
    exit (0);
  }

  if (flag_nest) {
    for (i = 0; i < out; i++) {
      if (outbuf[i] == '{')
        flag_nest++;
      if (outbuf[i] == '}')
        flag_nest--;
    }
    if (flag_nest - 100)
      fprintf (outfile, "%3d %s", flag_nest - 100, outbuf);
    else
      fprintf (outfile, "    %s", outbuf);
  } else
    fprintf (outfile, "%s", outbuf);
  fflush (outfile);
}

FILE *include_fopen (char *incl,        /* string from -i, (comma separated) */
                     char *inbuf,       /* #include line from file */
                     char *stg)
{                                       /* place results here */
  FILE *file;
  char in[256];
  char *token;
  char include[512];

  strcpy (include, incl);
  strcpy (in, strchr (inbuf, '"') + 1);
  *strchr (in, '"') = 0;

  token = strtok (include, ",");
  while (token) {
    sprintf (stg, "%s/%s", token, in);
    file = fopen (stg, "r");
    if (file)
      return file;
    token = strtok (NULL, ",");
  }
  strcpy (stg, inbuf);
  return NULL;
}

 /*
  *     Fix HTML references to gif images
  */
static char img_src[] = { "<IMG SRC=\"" };
int fix_html_1 (char *gif)
{
  char *temp;
  char before[256];
  char after[256];
  char *b = before;
  char *a = after;

  memset (before, 0, 256);
  memset (after, 0, 256);
  temp = strchr (gif, '"');
  if (temp) {
    temp++;
    while (temp[0] != '.') {
      b[0] = temp[0];
      b++;
      temp[0] = toupper (temp[0]);
      a[0] = temp[0];
      a++;
      temp++;
    }
    if ((temp[1] == 'g') && (temp[2] == 'i') && (temp[3] == 'f')) {
      temp[1] = 'P';
      temp[2] = 'N';
      temp[3] = 'G';
    }
    if (html_file) {
      fprintf (html_file, "%s.gif %s%s.PNG \n", before, flag_html, after);
    }
  }
}
int fix_html_2 (char *s)
{
  static char buf[64 + BUFFER_SIZE] = { 64 + BUFFER_SIZE * 0 };
  int i = 64;
  int j = 0;
  int k;

  strcpy (&buf[64], s);
  while (buf[i]) {
    s[j++] = buf[i];
    if (buf[i] == '"') {
      if (!strncmp
          (&buf[i - strlen (img_src) + 1], img_src, strlen (img_src))) {
        for (k = 0; k < strlen (flag_html); k++)
          s[j++] = flag_html[k];
      }
    }
    i++;
  }
}
char *fix_html (char *s)
{
  char *temp;

  if (!flag_html[0])
    return s;

  temp = s;
  while (temp) {
    temp = strstr (temp, img_src);
    if (temp) {
      fix_html_1 (temp);
      temp++;
    }
  }
  fix_html_2 (s);
  return s;
}

 /*
  *     Get an input line that is terminted with <cr>, <lf>, <lf><cr>, or <cr><lf>
  */
char internal_getc (FILE * stream)
{
  static char c[2] = { 0, 0 };
  c[0] = c[1];
  c[1] = getc (stream);
  switch (c[1]) {
   case 0x0D:
     if (c[0] == 0x0A)
       c[1] = getc (stream);
   case 0x0A:
     if (c[0] == 0x0D)
       c[1] = getc (stream);
  }
  return c[1];
}
char *internal_fgets (char *s, int n, FILE * stream)
{
  static int previous = 0;
  int i;

  memset (s, 0, n);
  for (i = 0; i < n; i++) {
    s[i] = internal_getc (stream);
    switch (s[i]) {
     case 0x0A:
     case 0x0D:
       return fix_html (s);
     case 0x00:
     case EOF:
       if (i)
         return fix_html (s);
       else
         return NULL;
    }
  }
  return fix_html (s);
}
int main (int argc, char *argv[])
{
  char date[256];
  FILE *include_file = NULL;
  int sts;
  char c;
  char inbuf[BUFFER_SIZE * 2];
  time_t time_t_uttime;
  struct tm *tm_uttime;


  time_t_uttime = time (NULL);
  tm_uttime = gmtime (&time_t_uttime);
  sprintf (date, "%04d-%02d-%02d",
           tm_uttime->tm_year + 1900,
           tm_uttime->tm_mon + 1, tm_uttime->tm_mday);
  if (argc < 1) {
    help (argc, argv);
    exit (0);
  }

  while ((c = getopt (argc, argv, "?ha:b:G:g:clsnxy:t:p:P:m:i:d:")) != EOF) {

#ifdef DEBUG
    fprintf (stderr, "%s ", __FILE__);
    fprintf (stderr, "[%c] ", c);
    if (optarg)
      fprintf (stderr, "%s ", optarg);
    ~fprintf (stderr, "\n");
#endif

    switch (c) {
     case 'a':
       infile = fopen (optarg, "r");
       fprintf (stderr, "INput File %s\n", infile);
       break;
     case 'b':
       outfile = fopen (optarg, "w");
       fprintf (stderr, "OUTput File %s\n", outfile);
       break;
     case 'g':
       strcpy (flag_html, optarg);
       break;
     case 'G':
       html_file = fopen (optarg, "w");
       break;
     case 'n':
       flag_nest = 100;
       break;
     case 'l':
       flag_lf = 0;
       break;
     case 'c':
       flag_cr = 0;
       fprintf (stderr, "Removing Carriage Returns\n");
       break;
     case 's':
       flag_strip = 1;
       break;
     case 't':
       flag_tab = atol (optarg);
       fprintf (stderr, "Replacing tabs with %d columns\n", flag_tab);
       break;
     case 'p':
       flag_pad = atol (optarg);
       fprintf (stderr, "Padding to %d columns\n", flag_pad);
       break;
     case 'P':
       strcpy (pad_chars, optarg);
       fprintf (stderr, "Padding with \"%s\"\n", pad_chars);
       break;
     case 'm':
       flag_max = atol (optarg);
       fprintf (stderr, "Maximum Length %d characters\n", flag_max);
       break;
     case 'i':
       strcpy (include, optarg);
       break;
     case 'd':
       strcpy (delete, optarg);
       break;
     case 'x':
       flag_spice = 1;
       break;
     case 'y':
       flag_date = malloc (strlen (optarg) + 1);
       strcpy (flag_date, optarg);
       break;
     case 'z':
       flag_delete = malloc (strlen (optarg) + 1);
       strcpy (flag_delete, optarg);
       flag_delete_len = strlen (flag_delete);
       break;
     default:
       help (argc, argv);
       exit (0);
       break;

    }
  }
  if (flag_strip && flag_pad) {
    fprintf (stderr,
             "strip trailing blanks and pad (AT THE SAME TIME?!?).  You'll only get stripped lines.\n");
  }

  while (internal_fgets (inbuf, BUFFER_SIZE - 1, infile)) {
    line_count++;
    sts = 1;

    if (flag_date) {
      char *btemp;

      if (btemp = strstr (inbuf, flag_date)) {
        char temp[256];
        char *ctemp;

        strcpy (temp, inbuf);
        ctemp = strstr (temp, flag_date);
        ctemp[0] = 0;
        strcat (temp, date);
        strcat (temp, btemp + strlen (flag_date));
        strcpy (inbuf, temp);
      }
    }
    if (flag_delete_len) {
      char *btemp;

      if (btemp = strstr (inbuf, flag_delete)) {
        btemp[0] = 0;
        if (btemp[flag_delete_len])
          strcat (inbuf, &btemp[flag_delete_len]);
      }
    }
    if (flag_spice &&
        (strstr (inbuf, "#define") == inbuf) && strstr (inbuf, "SPICE")) {
      char temp[BUFFER_SIZE * 2];

      strcpy (temp, "/*");
      strcat (temp, inbuf);
      strcpy (inbuf, temp);
    } else if (delete[0] &&
               (strstr (inbuf, "#include") == inbuf) &&
               strchr (inbuf, '"') && strstr (inbuf, delete)
      ) {
      sts = 0;
      delete_count++;
    } else if (include[0] &&
               (strstr (inbuf, "#include") == inbuf) && strchr (inbuf, '"')) {
      char *tmp;
      char stg[256];
      char fnm[256];
      char cmt[256];

      include_file = include_fopen (include, inbuf, stg);
      include_count = 1;
      if (include_file) {
        static char spaces[] = { "          "
            "          "
            "          "
            "          " "          " "          " "          " "          "
        };
        char asterisk[] = { "/*********"
            "**********"
            "**********"
            "**********" "**********" "**********" "**********" "*******"
        };

        sprintf (fnm, "/* \"%s\" */\n", stg);
        sts = strlen (fnm);
        if (sts < 78)
          asterisk[sts - 2] = 0;
        strcat (asterisk, "/");

        main_line (outfile, asterisk);
        sprintf (cmt, "/* The following #include file has  %s", spaces);
        cmt[77] = 0;
        if (sts < 77)
          cmt[sts - 3] = 0;
        strcat (cmt, "*/");
        main_line (outfile, cmt);

        sprintf (cmt, "/* been inserted into this file %s", spaces);
        cmt[77] = 0;
        if (sts < 77)
          cmt[sts - 3] = 0;
        strcat (cmt, "*/");
        main_line (outfile, cmt);
        main_line (outfile, fnm);

        inbuf[strlen (inbuf) - 1] = 0;
        sprintf (cmt, "/* %s %s", inbuf, spaces);
        cmt[77] = 0;
        if (sts < 77)
          cmt[sts - 3] = 0;
        strcat (cmt, "*/");
        main_line (outfile, cmt);

        main_line (outfile, asterisk);
        while (internal_fgets (inbuf, BUFFER_SIZE - 1, include_file)) {
          main_line (outfile, inbuf);
          include_count++;
        }
        main_line (outfile, asterisk);
        main_line (outfile, fnm);

        sprintf (cmt, "/* included line count %d %s", include_count, spaces);
        cmt[77] = 0;
        if (sts < 77)
          cmt[sts - 3] = 0;
        strcat (cmt, "*/");
        main_line (outfile, cmt);

        main_line (outfile, asterisk);
        fclose (include_file);
        include_count = 0;
        sts = 0;
      }
    }
    if (sts)
      main_line (outfile, inbuf);
  }
  if (html_file)
    fclose (html_file);
  if (infile)
    fclose (infile);
  if (outfile)
    fclose (outfile);
  exit (0);
}
