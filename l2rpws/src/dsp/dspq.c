#include <SpiceUsr.h>

/*
 * dspq.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <term.h>
#include <strings.h>

/* Cas Stuff */
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <cds_status.h>
#include <mdb.h>


/*****************************************************************************/

extern char *optarg;
extern int optind, optopt;

static char *Version = { "V1.0" };
static char *Title = { "CASSINI CDS data recovery analysis  (DSPQ)" };
static char *database_path = NULL;
static char *database_file = NULL;
static int file_type = MDB_R_FILE;
struct RPWS_buffer *buffer;

#define DROP_FLAG 16
static int drop_flag[DROP_FLAG] = { DROP_FLAG * 0 };
static char *pkt_type[] = { "HSK",
  "LRS",
  "HRS",
  "",
  ""
};
static int drop_counts[4][256];
void bin (int index, int delta_in)
{
  int delta = 255;

  if (delta_in < 255)
    delta = delta_in;
  drop_counts[index][delta] += delta_in;
  return;
}

int expected (int count)
{
  int total = count;
  int index = 2;
  int i;

  for (i = 0; i < DROP_FLAG; i++) {
    if (drop_flag[i])
      total -= drop_counts[index][drop_flag[i]];
  }
  return total;
}
int main (int argc, char *argv[])
{
  float percentage;
  struct MDB *mdb_file;
  int total_packet_count = 0;
  int total_calculated_count = 0;
  int sequence[2];
  int expected_seq[2];
  int old_sequence[4] = { -1, -1, -1, -1 };
  int first_sequence[4] = { -1, -1, -1, -1 };
  int packet_count[4] = { 0, 0, 0, 0 };
  int non_fill_count[4] = { 0, 0, 0, 0 };
  int calculated_count;
  int roll_over[4] = { 0, 0, 0, 0 };
  int type;
  int i;
  int j;
  int index = 0;
  int delta;
  int non_fill;
  char c;
  
  if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }
  
  furnsh_c(getenv("CAS_TIME_KERNELS"));

  fprintf (stdout, "\n");
  if (strcmp (MDB_VERSION, MDB_Version)) {
    fprintf (stdout, "%s %s\n", MDB_VERSION, MDB_Version);
    fprintf (stdout, "There is a problem with MDB that MUST be resolved\n");
    exit (0);
  }
  while ((c = getopt (argc, argv, "d:h")) != EOF) {
    switch (c) {
     case 'd':
       drop_flag[index] = atoi (optarg);
       index++;
       break;
     case 'h':
     default:
       fprintf (stdout, "Display data recovery quality statistics\n");
       fprintf (stdout, "     %s %s\n", Title, Version);
       fprintf (stdout, "     MDB library %s\n", MDB_Version);
       fprintf (stdout, "     \n");
       fprintf (stdout, " %s -flags <start_time> <stop_time>\n", argv[0]);
       fprintf (stdout, "     \n");
       fprintf (stdout, " -flags\n");
       fprintf (stdout, "     \n");
       fprintf (stdout,
                "     -d nn     don't count gaps <nn> records long\n");
       fprintf (stdout, "             that occur in HRS.\n");
       fprintf (stdout,
                "               They will still show up on the report\n");
       fprintf (stdout, "             but they are flagged \"<drop>\".\n");
       fprintf (stdout, "               MULTIPLE -d are allowed.  You can\n");
       fprintf (stdout, "             specify multiple gap sizes.\n");
       fprintf (stdout, "               \n");
       fprintf (stdout, " R_FILE must be in SCLK order to count correctly\n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "   Fields that show up on the report:\n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     pkts:\n");
       fprintf (stdout, "         Received and expected packet counts.\n");
       fprintf (stdout, "       received/expected percent-coverage\n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     zero fill pkt:\n");
       fprintf (stdout, "     \n");
       fprintf (stdout,
                "         We count zero-filled packets as well and\n");
       fprintf (stdout, "       these counts show up here\n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     bin:\n");
       fprintf (stdout, "         The gaps are binned into groups to\n");
       fprintf (stdout,
                "       indicate the number of consecutive packets\n");
       fprintf (stdout, "       in each gap.  Gaps of 255 or more packets\n");
       fprintf (stdout,
                "       are accumulated in bin 255 and are reported\n");
       fprintf (stdout, "       together.\n");
       fprintf (stdout, "       \n");
       fprintf (stdout, "     packet count:\n");
       fprintf (stdout,
                "         Count of the number of gar groups and the\n");
       fprintf (stdout,
                "       number of lost packets.  Lost packet counts\n");
       fprintf (stdout, "       are listed in parands.\n");
       fprintf (stdout, "       \n");
       fprintf (stdout,
                "     Note that the 255 bin (the accumulation of all\n");
       fprintf (stdout,
                "       gaps that are 255 or more packets in length)\n");
       fprintf (stdout,
                "       only lists the total packets lost and not the\n");
       fprintf (stdout, "       number of gaps.\n");
       fprintf (stdout, "       \n");
       fprintf (stdout, "     TOTAL line:\n");
       fprintf (stdout, "       \n");
       fprintf (stdout,
                "         Like the 1st. detail line of each group, the total\n");
       fprintf (stdout,
                "       line lists received / expected packets and the percent\n");
       fprintf (stdout,
                "       of packets we have received.  NOTE that these counts\n");
       fprintf (stdout, "       do NOT include Housekeeping.\n");
       fprintf (stdout, "       \n");
       exit (0);
    }
  }
  if (argc < 3) {
    fprintf (stdout, " %s -h\n", argv[0]);
    exit (0);
  }
  mdb_file = MDB_open (argv[argc - 2],
                       argv[argc - 1],
                       database_path, database_file, file_type);
  /**/ while (buffer = MDB_read (mdb_file)) {
    sequence[1] = UTIL_extract_CDS_sequence ((struct CDS_buffer *) buffer);
    type = UTIL_extract_CDS_type ((struct CDS_buffer *) buffer);
    non_fill = buffer->packet.chdo_ancillary.type_94.non_fill_length;
    switch (type) {
     case CDS_Packet_ID_UnSegmented_HRS_I:
     case CDS_Packet_ID_Segmented_HRS_I:
       index = 2;
       if (non_fill == 952)
         packet_count[index]++;
       else
         non_fill_count[index]++;
       break;
     case CDS_Packet_ID_UnSegmented_LRS_I:
     case CDS_Packet_ID_Segmented_LRS_I:
       index = 1;
       if (non_fill == 952)
         packet_count[index]++;
       else
         non_fill_count[index]++;
       break;
     case CDS_Packet_ID_Housekeeping_Science:
     case CDS_Packet_ID_Housekeeping_ROM:
       index = 0;
       if (non_fill == 192)
         packet_count[index]++;
       else
         non_fill_count[index]++;
       break;
     default:
       index = 3;
       break;
    }
    sequence[0] = old_sequence[index];
    if (sequence[0] < 0) {              /* FIRST  TIME */
      first_sequence[index] = sequence[1];
    } else {                            /* can determine lost pkts now */
      expected_seq[0] = sequence[0];
      expected_seq[0] &= 0x3FFF;
      expected_seq[1] = sequence[1];
      expected_seq[1] &= 0x3FFF;
      if (expected_seq[0] > expected_seq[1])
        roll_over[index] += 16384;
      expected_seq[0]++;
      expected_seq[0] &= 0x3FFF;
      if (expected_seq[0] != expected_seq[1]) {
        delta = expected_seq[1] - expected_seq[0];
        if (delta < 0)
          delta += 16384;
        if (delta >= 255)
          printf ("%08X\n", UTIL_extract_TIME ((struct CDS_buffer *) buffer));
        bin (index, delta);
      /**/}
    }                                   /* FIRST  TIME */

    old_sequence[index] = sequence[1];
  }

  MDB_close (mdb_file);

  /*
   * dump results 
   */

  for (i = 0; i < argc; i++)
    printf ("%s ", argv[i]);
  printf ("\n");
  for (index = 0; index < 3; index++) {
    printf ("\n");
    calculated_count = old_sequence[index] -
      first_sequence[index] + roll_over[index];

    percentage = packet_count[index];
    percentage /= calculated_count;
    percentage *= 100.;

    printf (" %s    ", pkt_type[index]);
    printf ("pkts: ");
    printf ("%d/%d  %.1f%%  ",
            packet_count[index], calculated_count, percentage);
    if (index == 2) {
      percentage = packet_count[index];
      percentage /= expected (calculated_count);
      percentage *= 100.;
      printf ("(%d/%d  %.1f%%)  ",
              packet_count[index], expected (calculated_count), percentage);
    }
    printf ("zero fill pkt: %3d  ", non_fill_count[index]);
    printf ("\n");
    for (i = 0; i < 255; i++) {
      if (drop_counts[index][i]) {
        printf (" %s bin:%3d    packet count: %5d (%5d) ",
                pkt_type[index],
                i, drop_counts[index][i] / i, drop_counts[index][i]);
        for (j = 0; j < DROP_FLAG; j++) {
          if ((index == 2) && (i == drop_flag[j]))
            printf ("<drop>");
        }
        printf ("\n");
      }
    }
    i = 255;
    {
      if (drop_counts[index][i]) {
        printf (" %s bin:>=%3d  packet count:       (%5d)\n",
                pkt_type[index], i, drop_counts[index][i]);
      }
    }
  }


  for (index = 1; index < 3; index++) {
    total_calculated_count += old_sequence[index] -
      first_sequence[index] + roll_over[index];
    total_packet_count += packet_count[index];
  }
  printf ("\n");
  printf (" %s  ", "TOTAL");
  printf ("pkts: ");
  percentage = total_packet_count;
  percentage /= total_calculated_count;
  percentage *= 100.;
  printf ("%d/%d  %.1f%%  ",
          total_packet_count, total_calculated_count, percentage);
  percentage = total_packet_count;
  percentage /= expected (total_calculated_count);
  percentage *= 100.;
  printf ("(%d/%d  %.1f%%)",
          total_packet_count, expected (total_calculated_count), percentage);
  printf ("\n");
  return 0;
}
