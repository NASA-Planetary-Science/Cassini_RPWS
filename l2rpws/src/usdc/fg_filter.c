#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include <fg.h>

#include <rtiu.h>
#include <util.h>
#include <utilf.h>



/* return a file name */

extern char IN_FILE_NAME[512];
extern char OUT_FILE_NAME[512];

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
int fg_filter (int argc, char *argv[])
{                                       /* Start William Robison's Stuff */

  int local_flag = FILE_LOCATE_DEFAULT;
  int eof_flag;

/*
  FILE *input = stdin;
  FILE *output = stdout;
*/

  fg_flags (argc, argv);
  if (fg_flag ("get"))                  /* alias for getmp is "get" */
    fg_flagx ("+getmpus", "");
  if (fg_flag ("put"))                  /* alias for putmp is "put" */
    fg_flagx ("+putmpus", "");

  if (fg_flag ("find")) {
    if (UTIL_find_open (fg_flagc ("find"), "rb")) {
      fprintf (stderr, "mpus find: %s\n", UTIL_find_name ());
      strcpy (IN_FILE_NAME, UTIL_find_name ()); /* raj mod. 2/11/98 */
    } else {
      fprintf (stderr, "mpus find: file not found: %s\n", fg_flagc ("find"));
      exit (0);
    }
  } else {
    if (UTIL_FILEname (FILE_MPUS, local_flag)) {
      fprintf (stderr, "usdc source: %s\n", UTIL_fname (FILE_OLD));
      strcpy (IN_FILE_NAME, UTIL_fname (FILE_OLD));
    } else {
      fprintf (stderr, "usdc source: stdin\n");
      strcpy (IN_FILE_NAME, "stdin");
    }
  }

  if (!1) {
    strcpy (OUT_FILE_NAME, "stdout");
  } else {
    UTIL_FILEnew (FILE_MPUS, local_flag);
    fprintf (stderr, "usdc destination:%s\n", UTIL_fname (FILE_NEW));
    strcpy (OUT_FILE_NAME, UTIL_fname (FILE_NEW));
  }


  if (fg_flag ("eof"))
    eof_flag = 1;

  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" usdc - mini packet Data Compressin Chip/Rice decompression\n");
    printf (" \n");
    printf ("      +find fn   find archive file\n");
    printf ("      +getmpus   get current file\n");
    printf ("                  else stdin\n");
    printf ("      +putmpus   put new file\n");
    printf ("                  else stdout\n");
    printf ("      +eof       non real-time\n");
    printf ("      help       (help menu)\n");
    exit (0);
    printf (" \n");
  }

  return 0;
}                                       /* End William Robison's Stuff */

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
