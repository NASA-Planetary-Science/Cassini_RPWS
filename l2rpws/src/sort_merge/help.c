#include <stdlib.h>
#include <stdio.h>
#include "debug.h"

extern int starting_point;
extern int ending_point;
extern int dataset_size;

extern char *read_raw_Version;
extern char *write_raw_Version;

int help_ver (FILE * outfile, char *argv[], char *Version)
{
  fprintf (outfile, "Cassini/RPWS-Sort/Merge (%s)\n", argv[0]);
  fprintf (outfile, "      sort_merge version %s\n", Version);
  fprintf (outfile, "      write_raw  version %s\n", write_raw_Version);
  fprintf (outfile, "       read_raw  version %s\n", read_raw_Version);
  return 0;
}
int help (FILE * outfile, char *argv[], char *Version)
{
  fprintf (outfile, "    \n");
  fprintf (outfile, "    usage: %s <-flags> <list-file>\n", argv[0]);
  fprintf (outfile, "    \n");
  fprintf (outfile,
           "    <list-file> is a file containing a list of recently\n");
  fprintf (outfile, "      acquired data to merge into the database.\n");
  fprintf (outfile,
           "    <list-file> may be specified as \"-\" to indicate data\n");
  fprintf (outfile,
           "      is to be taken from <stdin>.  This is to allow use of\n");
  fprintf (outfile,
           "      mdb (useful when reprocessing data without doing a requery).\n");
  fprintf (outfile, "    \n");
  fprintf (outfile, "    <-flags> control flags\n");
  fprintf (outfile,
           "                Normally we would drop the first and last\n");
  fprintf (outfile,
           "                files as they would be partials.  These flags\n");
  fprintf (outfile,
           "                will, when set to zero, will casue the starting\n");
  fprintf (outfile,
           "                and ending files (respectively) to be retained.\n");
  fprintf (outfile, "        -starting_point n  Default: %d\n",
           starting_point);
  fprintf (outfile, "        -ending_point n    Default: %d\n", ending_point);
  fprintf (outfile,
           "        -start_all         All files at the start of the period\n");
  fprintf (outfile,
           "        -end_all           All files at the end of the period\n");
  fprintf (outfile, "        -path xx           Database path\n");
  fprintf (outfile, "        -name xx           Database name\n");
  fprintf (outfile, "        \n");
  fprintf (outfile, "      Size of data file (hours)\n");
  fprintf (outfile, "        \n");
  fprintf (outfile, "        -dataset_size n    Default: %d\n", dataset_size);
  fprintf (outfile, "        \n");
  fprintf (outfile,
           "        -debug             controls debug output (stdout)\n");
  fprintf (outfile, "                  %02X         record selection\n",
           DEBUG_SELECT);
  fprintf (outfile, "                  %02X         sorting internals\n",
           DEBUG_INSERT);
  fprintf (outfile,
           "                  %02X         sclk that is being sorted on\n",
           DEBUG_SCLK);
  fprintf (outfile, "                  %02X         record read counters\n",
           DEBUG_COUNT);
  fprintf (outfile, "        \n");
  fprintf (outfile,
           "    --------------------------------------------------------\n");
  fprintf (outfile, "      This routine will merge the files given\n");
  fprintf (outfile, "    as an argument to this routine with the\n");
  fprintf (outfile, "    database files.  The file given to this routine\n");
  fprintf (outfile, "    is simply a list of files to process.\n");
  fprintf (outfile, "    \n");
  fprintf (outfile,
           "      We begin by scanning the given list of files into\n");
  fprintf (outfile,
           "    memory, sorting and eliminating duplicates.  Start and\n");
  fprintf (outfile,
           "    stop times are detemined by this phase of the operation.\n");
  fprintf (outfile,
           "    We then move an hour before and after the indicated\n");
  fprintf (outfile,
           "    time and scan the database, adding these records to the\n");
  fprintf (outfile,
           "    list, again eliminating duplicates and keeping things\n");
  fprintf (outfile, "    in order.\n");
  fprintf (outfile, "    \n");
  fprintf (outfile,
           "      Once all is in memory, we can then proceed to write the\n");
  fprintf (outfile,
           "    merged (raw) files back out to a new set of files.  These\n");
  fprintf (outfile,
           "    files are named on 1 hour boundaries.  We keep track of the\n");
  fprintf (outfile,
           "    filenames and spit them out to a file, thinking this might\n");
  fprintf (outfile,
           "    be convenient later on.  We also reformat the list, dropping\n");
  fprintf (outfile,
           "    the first and last files from the list, and changing the\n");
  fprintf (outfile,
           "    filetype to that of the u-files (again, thinking this might\n");
  fprintf (outfile,
           "    be useful a bit later).  Now, to get really sophisticated,\n");
  fprintf (outfile,
           "    we can build something to move all of the derivative files\n");
  fprintf (outfile,
           "    to the target directory (Kronos format files end up in a slightly\n");
  fprintf (outfile,
           "    different directory, and we try to take that into account\n");
  fprintf (outfile, "    as well).\n");
  fprintf (outfile, "    \n");
  fprintf (outfile,
           "      You should be able to process the r-files, using the list\n");
  fprintf (outfile,
           "    of r-files, and then move the u-files using the other lists\n");
  fprintf (outfile,
           "    eliminating the fragments at the begining and end.\n");
  fprintf (outfile, "    \n");
  fprintf (outfile, "  REPROCESSING\n");
  fprintf (outfile,
           "      You may need to make use of sort_merge to reporcess data that\n");
  fprintf (outfile,
           "    is already resident at Iowa.  This can be accomplished using the\n");
  fprintf (outfile,
           "    MDB routine (-s R_FILE) to select records between the appropriate\n");
  fprintf (outfile,
           "    times.  This will simply sort/merge the selected period twice, a\n");
  fprintf (outfile,
           "    little extraq work, but operationally no different that processing the\n");
  fprintf (outfile,
           "    results of a query.  As a side effect, the r data is time-ordered and\n");
  fprintf (outfile, "    duplicate records are deleted.\n");
  fprintf (outfile, "    \n");
  fprintf (outfile, "  Selection Criteria:\n");
  fprintf (outfile,
           "    1. Records from the raw file are only accepted if the CDS packet\n");
  fprintf (outfile,
           "       length field in non-zero and the packet ID matches one of the\n");
  fprintf (outfile, "       7 valid patterns for the RPWS instrument.\n");
  fprintf (outfile,
           "    2. CHDO fill length is used to select the \"better\" record.  If the\n");
  fprintf (outfile,
           "       non-fill length is the same, the newly acquired record is used.\n");
  fprintf (outfile, "       \n");
  fprintf (outfile, "    \n");
  fprintf (outfile, "    Result:\n");
  fprintf (outfile,
           "        We should end up with all records that have shown up at JPL.\n");
  fprintf (outfile,
           "        If the database has records that don't show up in a later query,\n");
  fprintf (outfile, "        they are retained.\n");
  fprintf (outfile, "        \n");
  fprintf (outfile, "    \n");
  fprintf (outfile, "  Version Information\n");
  fprintf (outfile, "    \n");
  fprintf (outfile, "    read_raw V2.1\n");
  fprintf (outfile, "      Hope we got <stdin> hacked correctly...\n");
  fprintf (outfile, "    V2.2\n");
  fprintf (outfile, "      Added <stdin> for reprocessing.\n");
  fprintf (outfile, "    V1.9\n");
  fprintf (outfile,
           "      Having trouble with packets getting lost (i.e. discarded).\n");
  fprintf (outfile,
           "      Looks like LRS and HSK with identical SCLK tags get identified as\n");
  fprintf (outfile,
           "      duplicates, with one of the getting tossed.  We'll solve this\n");
  fprintf (outfile,
           "      by assigning HSK packkets a time tag that is 102mS after the start\n");
  fprintf (outfile,
           "      of the RTI.  This will NOT match any HRS data (maximum of 12 packets\n");
  fprintf (outfile, "      in any given RTI period).\n");
  fprintf (outfile, "    V1.5\n");
  fprintf (outfile, "      Hmmm...\n");
  fprintf (outfile, "    V1.4\n");
  fprintf (outfile,
           "      Database path/name selection.  This makes some debugging tasks\n");
  fprintf (outfile, "      a little easier to manage\n");
  fprintf (outfile, "    V1.1\n");
  fprintf (outfile,
           "      Added delete to script files (but commented out)\n");
  fprintf (outfile, "    \n");
  fprintf (outfile, "    V1.0\n");
  fprintf (outfile, "      Initial Release\n");
  fprintf (outfile, "    \n");
  return 0;
}
