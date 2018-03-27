
/****************************************************************************/

/*                                                                          */

/*      Author of this file : William Robison                               */

/*                                                                          */

/****************************************************************************/

#include <stdio.h>
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "mpus.h"                       /* uses definitions in util.h */
#include "fg.h"

int DCCDecompress = 1;                  /* decompress DCC compressed data */

static char Mpus_Version[] = { "Version 2.0" };

int main (int argc, char *argv[])
{
  int c;
  int ilen;
  int eof_flag = UTIL_GET_BLOCKING;
  int sts = 1;
  long status;
  int local_flag = FILE_LOCATE_DEFAULT;
  struct MP_buffer a;
  struct RPWS_buffer b;
  FILE *input = stdin;
  FILE *output = stdout;
  char fname[256];

  /*
   *      MPUS    MPUS    MPUS    MPUS    MPUS
   *      ------------------------------------
   *     Sample filter file naming services.
   *
   *     1. initilize flag parser.
   *             CALL "fg_flags" with argc/argv
   *     2. establish input stream
   *             Use UTIL_FILEname to establish the input stream.
   *               "stdin" unless user supplies +getXXX to use current
   *               file (this is why fg_flags is required).
   *               XXX will match the /usr/cassini/data/.filerc entry
   *               +getcds +getmp +getwfr +getmpus, etc.
   *             Use UTIL_fname(1) to obtain input filename if required.
   *     3. establish output stream.
   *             Use UTIL_FILEnew to establish the output stream.
   *               "stdout" unless user supplies +putXXX to make a
   *               new output file (again, this is why fg_flags is
   *               needed).   XXX just like UTIL_FILEname.
   *
   *      The argument to UTIL_FILE* is an integer whose value is
   *        enumerated in "utilf.h" header.  This determines the
   *        get/put flag name expected on the command line.
   *
   *      These utility routines enforce filenaming conventions as
   *        the user is not involved in generating filenames.  The
   *        /usr/cassini/data/.filerc control file has an additional
   *        detail line that specifies the path to the data files.
   *        This results in ALL data files residing in the same 
   *        directory, although not necessarily on the same device that
   *        /usr/cassini lives on...
   */

  /*
   *  fg_flags - initialize the parser for the file naming routines
   *  fg_flag  - look for the presence of an item on the command line
   *                  non-zero indiactes the parameter is present
   *  fg_flagx - add a command line aprameter to the parser
   */
  fg_flags (argc, argv);
  if (fg_flag ("get"))                  /* alias for getmp is "get" */
    fg_flagx ("+getmp", "");
  if (fg_flag ("put"))                  /* alias for putmp is "put" */
    fg_flagx ("+putmpus", "");
  /*
   *          connect to files, if return is zero, this indicates
   *          the file wasn't selected and probably needs to come
   *          from stdin...
   */

  if (fg_flag ("find")) {
    input = UTIL_find_open (fg_flagc ("find"), "rb");
    if (input)
      fprintf (stderr, "mpus find: %s\n", UTIL_find_name ());
    else {
      fprintf (stderr, "mpus find: file not found: %s\n", fg_flagc ("find"));
      exit (0);
    }
  } else {
    input = UTIL_FILEname (FILE_MP, local_flag);
    if (input)
      fprintf (stderr, "mpus source: %s\n", UTIL_fname (FILE_OLD));
    else
      input = stdin;
    fprintf (stderr, "mpus source: stdin\n");
  }

  output = UTIL_FILEnew (FILE_MPUS, local_flag);
  if (!output)
    output = stdout;
  fprintf (stderr, "mpus destination:%s\n", UTIL_fname (FILE_NEW));

  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" mpus - mini packet de-segmentation\n");
    printf ("         %s, %s\n", Mpus_Version, sMpusVersion);
    printf (" \n");
    printf ("      +find fn   find archive file\n");
    printf ("      +getmp     get current file\n");
    printf ("                  else stdin\n");
    printf ("      +putmpus   put new file\n");
    printf ("                  else stdout\n");
    printf ("      +eof       end at eof\n");
    printf ("      help       (help menu)\n");
    exit (0);
    printf (" \n");
  }

  if (fg_flag ("eof"))
    eof_flag = UTIL_GET_NON_BLOCKING;

  while (--argc) {
    ++argv;
    if (!strcmp (*argv, "-nodcc"))
      DCCDecompress = 0;
  }



  sts = UTIL_getbuffer_MP (&a, input, eof_flag);        /* get mini-packet. Returns ??? */
  while (sts > 0) {
    while ((MiniPacketUnsegmentation (&a, &b)) == True)
      UTIL_putbuffr2_RPWS (&b, output, b.f_length);
    sts = UTIL_getbuffer_MP (&a, input, eof_flag);      /* get mini-packet. Returns ??? */
  }

  return 1;
}
