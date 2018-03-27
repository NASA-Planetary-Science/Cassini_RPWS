
/*
 * sort_merge.c 
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
#include <sys/types.h>
#include <sys/stat.h>

#include <das2/das1.h>

/* Cas stuff */
#include <fg.h>
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <UTIL_status.h>


#include <rpws_sclk.h>
/* #include <rpws_status.h> */
#include <mdb_time.h>


/*#include "CasTlmDef.h" /**/

/*#include "Cext.h" /**/

/*#include "CasRecord.h" /**/

/*#include "CasHFR.h" /**/

/* Local Stuff */
#include "read_raw.h"
#include "write_raw.h"
#include "sort_merge.h"
#include "spice.h"
#include "help.h"
#include "debug.h"

/****************************************************************************/
/* Compiled in binary file directory location */

#ifndef INST_BIN
#error Compiled in default binary directory is missing
#endif
/****************************************************************************/


char *Version = { "V2.2a" };
int starting_point = 1;
int ending_point = 1;
int dataset_size = 6;

char database[256];

struct DATABASE_TIME_TAG database_start_time =
  { 1997, 10, 24, 297, 22, 27, 02.0 };
int database_sclk_start_i = CASSINI_STARTING_SCLK;
struct DATABASE_TIME_TAG database_stop_time = { 2035, 1, 1, 1, 0, 0, 0.0 };
int database_sclk_stop_i = 0x7FFFFFFF;


static int cds_time_bits[4] = { CDS_Time_Bits_07_00,
  CDS_Time_Bits_15_08,
  CDS_Time_Bits_23_16,
  CDS_Time_Bits_31_24
};
int debug_flag = 0;

static int raw_read_stat_read1 = 0;
static int raw_read_stat_dupe1 = 0;
static int raw_read_stat_head1 = 0;
static int raw_read_stat_tail1 = 0;
static int raw_read_stat_inst1 = 0;
static int raw_read_stat_null1 = 0;

char *reformat1 (char *file, int offset)
{
  static char result[] = { "0000-000" };
  int year;
  int day;

  result[0] = file[1];                  /* y 1000  */
  result[1] = file[2];                  /* y  100  */
  result[2] = file[3];                  /* y   10  */
  result[3] = file[4];                  /* y    1  */
  result[5] = file[5];                  /* d  100  */
  result[6] = file[6];                  /* d   10  */
  result[7] = file[7];                  /* d    1  */
  result[8] = 0;
  year = atoi (&result[0]);
  day = atoi (&result[5]);
  sprintf (result, "%04d-%03d", year, day + offset);
  return result;
}
int reformat (int flag,
              FILE * file_list,
              FILE * u_file_list, FILE * rm_file_list, int count, char *type)
{
  char *temp;
  int index;
  int flag_1 = 0;
  int flag_2 = 0;
  char line[129] = { "" };              /*01234567890 */
  char old_line[129] = { "" };          /*01234567890 */
  char kronos[] = { "Kyyyyddd.hh" };
  int new_count = 0;
  static int first_flag = 1;
  char burp = 1;
  char old_burp = 1;

  if (first_flag && rm_file_list) {
    first_flag = 0;
    fprintf (rm_file_list, "#!/bin/ksh\n");
    fprintf (rm_file_list, "#\n");
    fprintf (rm_file_list, "#  Version %s\n", Version);
    fprintf (rm_file_list, "#\n");
    fprintf (rm_file_list, "#  Sort/Merge file remove script\n");
    fprintf (rm_file_list, "#      These files are outside the\n");
    fprintf (rm_file_list, "#      period of interest and will\n");
    fprintf (rm_file_list, "#      be removed prior to creating\n");
    fprintf (rm_file_list, "#      the \"fred\" file.\n");
    fprintf (rm_file_list, "#\n");
    fprintf (rm_file_list, "#    %3d starting_point\n", starting_point);
    fprintf (rm_file_list, "#    %3d ending_point\n", ending_point);
    fprintf (rm_file_list, "#    %3d count\n", count);
    fprintf (rm_file_list, "#\n");
  }
  switch (flag) {
   case 0:
     fprintf (u_file_list, "#!/bin/ksh\n");
     fprintf (u_file_list, "#\n");
     fprintf (u_file_list, "#  Version %s\n", Version);
     fprintf (u_file_list, "#\n");
     fprintf (u_file_list, "#  Sort/Merge file move script\n");
     fprintf (u_file_list,
              "#      give it the file path of the destination\n");
     fprintf (u_file_list, "#      as the first parameter\n");
     fprintf (u_file_list, "#\n");
     fprintf (u_file_list, "#    %3d starting_point\n", starting_point);
     fprintf (u_file_list, "#    %3d ending_point\n", ending_point);
     fprintf (u_file_list, "#    %3d count\n", count);
     fprintf (u_file_list, "#\n");
     break;
   case 3:
     fprintf (u_file_list, "#!/bin/ksh\n");
     fprintf (u_file_list, "#\n");
     fprintf (u_file_list, "#  Version %s\n", Version);
     fprintf (u_file_list, "#\n");
     fprintf (u_file_list,
              "#  Call this routine to run Bob's PDS processing\n");
     fprintf (u_file_list, "#\n");
     fprintf (u_file_list, "#    $1 is the path to the executable\n");
     fprintf (u_file_list, "#        " INST_BIN "\n");
     fprintf (u_file_list, "#\n");
     fprintf (u_file_list, "#    $2 is the script/executable to run\n");
     fprintf (u_file_list, "#        lrs2dat\n");
     fprintf (u_file_list, "#\n");
     fprintf (u_file_list, "umask 002\n");
     fprintf (u_file_list, "#\n");
     break;
  }

  for (index = 0; index < count; index++) {
    old_burp = burp;
    burp = 0;
    if (index < starting_point)
      burp = 1;
    if ((count - ending_point) <= index)
      burp = 2;
    strcpy (old_line, line);
    fgets (line, 128, file_list);
    temp = strchr (line, '.');
    if (temp) {
      *temp = 0;
      if (!index) {
        if (!flag) {
          fprintf (u_file_list, "original_path_name=\"%s\"\n",
                   read_raw_find_path (line, type));
          fprintf (u_file_list, "#\n");
        }
      }
      if (type) {
        strcat (line, type);
      } else {
        memcpy (&kronos[1], &line[1], 7);
        memcpy (&kronos[9], &line[8], 2);
        strcpy (line, kronos);
      }
      if (!flag) {
        fprintf (u_file_list, "#\n");
        fprintf (u_file_list, "#  index %d %s\n", index, line);
        if (burp) {
          if (rm_file_list) {
            fprintf (rm_file_list, "#\n");
            fprintf (rm_file_list, "#  index %d %s\n", index, line);
          }
          switch (burp) {
           case 1:
             if (rm_file_list)
               fprintf (rm_file_list,
                        "#  This file if before the period of interest\n");
             break;
           case 2:
             if (rm_file_list)
               fprintf (rm_file_list,
                        "#  This file if after the period of interest\n");
             break;
          }
          if (rm_file_list) {
            fprintf (rm_file_list, "#\n");
            fprintf (rm_file_list, "   rm %s;\n", line);
          }
        } else {
          fprintf (u_file_list, "#\n");
          fprintf (u_file_list, "if [[ -s %s ]] ; then\n", line);
          fprintf (u_file_list, "   echo \"move   %s\";\n", line);
          fprintf (u_file_list, "   mv %s $1/%s ;\n", line, line);
          fprintf (u_file_list, "else\n");
          fprintf (u_file_list, "   echo \"remove %s\";\n", line);
          fprintf (u_file_list, "   rm %s;\n", line);
          fprintf (u_file_list, "fi\n");
          fprintf (u_file_list, "\n");
        }
      } else {
        switch (flag) {
         case 1:
           fprintf (u_file_list, "%s\n", line);
           break;
         case 2:
           if (!burp)
             fprintf (u_file_list, "%s\n", line);
           break;
         case 3:
           switch (burp) {
            case 0:
              if (flag_1)
                break;
              if (old_burp == 1) {
                fprintf (u_file_list, "$1/$2 %s ", reformat1 (line, 0));
                flag_1 = 1;
              }
              break;
            case 2:
              if (flag_2)
                break;
              if (old_burp == 0) {
                fprintf (u_file_list, "%s\n", reformat1 (old_line, 1));
                flag_2 = 1;
              }
              break;

           }
           break;
        }
      }
      new_count++;
    }
  }

  switch (flag) {
   case 3:
     if (!flag_2) {
       fprintf (u_file_list, "%s\n", reformat1 (old_line, 1));
     }
     fprintf (u_file_list, "#\n");
     fprintf (u_file_list, "cat $2.err >> /var/tmp/$2.err\n");
     /**/ fprintf (u_file_list, "#\n");
     fprintf (u_file_list, "#\n");
  }

  return new_count;
}

char *sort_merge_status (char *type)
{
  static char filename[64];

  sprintf (filename, "z_sort_merge_status.%s", type);
  return filename;
}

static int dump_record (struct CDS_buffer *buffer, FILE * sou,
                        int record_count)
{
  char stemp[64];

  sprintf (stemp, "1/%d.%03d",
           get_status_32 (buffer->packet.cds.header, cds_time_bits, __LINE__),
           get_status (buffer->packet.cds.header, CDS_Time_Bits_SUB_Second, 0)
    );
  fprintf (sou, "SCLK(%6d) %s  %08X.%02X %s = %s ",
           record_count,
           UTIL_extract_packet_brief (buffer),
           get_status_32 (buffer->packet.cds.header, cds_time_bits, __LINE__),
           get_status (buffer->packet.cds.header, CDS_Time_Bits_SUB_Second,
                       0), stemp, MDB_time_SCET (stemp));
  fprintf (sou, "data set build time: %02X%02X %X%02X %02X:%02X ",
           buffer->packet.ws_tag.A.year[0], buffer->packet.ws_tag.A.year[1],
           buffer->packet.ws_tag.A.doy[0], buffer->packet.ws_tag.A.doy[1],
           buffer->packet.ws_tag.A.hour, buffer->packet.ws_tag.A.minute);

  fprintf (sou, "\n");
  fflush (sou);
  return 0;
}

int main (int argc, char *argv[])
{
  struct SORT_ELEMENT *list;
  struct SORT_ELEMENT *temp[2];
  static int sclk_early = 0x7FFFFFFF;
  static int sclk_late = 0x00000000;
  static int _sclk;
  static char *early[3];
  static char *late[3];
  int record_count = 0;
  int errors = 0;
  int file_count;
  int i;
  double new_sclk = 0.0;
  double old_sclk = 0.0;
  int new_ID = 0;
  int old_ID = 0;
  char *launch = { "1997-288T00:00:00.000" };
  char *end_of_mission = { "2038-001T00:00:00.000" };
  FILE *status;
  FILE *file_list;
  FILE *u_file_list;
  FILE *rm_file_list;
  
  char *database_name = NULL;
  char *database_path = NULL;


  fg_flags (argc, argv);
  debug_flag = fg_int ("debug", debug_flag);
  dataset_size = fg_int ("dataset_size", dataset_size);
  if (fg_flag ("start_all"))
    starting_point = 0;
  if (fg_flag ("end_all"))
    ending_point = 0;

  if(fg_flag("path") && fg_flag("name")){
    database_path = fg_flagc ("path");
    database_name = fg_flagc ("name");

	  strcpy (database, database_path);
	  strcat (database, "/");
	  strcat (database, database_name);
  }
  else{
		if(getenv("RPWS_MPDB")){  
	  		strcpy(database, getenv("RPWS_MPDB"));
		}
		else{
			fprintf(stderr, "-path and -name are both needed as the env var "
			        "RPWS_MPDB is not defined.");
		}
  }

  starting_point = fg_int ("starting_point", starting_point);
  ending_point = fg_int ("ending_point", ending_point);

  help_ver (stdout, argv, Version);
  fprintf (stdout, "Setup\n");
  fprintf (stdout, "    %3d starting_point\n", starting_point);
  fprintf (stdout, "    %3d ending_point\n", ending_point);
  fprintf (stdout, "    %3d dataset_size\n", dataset_size);
  fprintf (stdout, "    database %s\n", database);
  if (debug_flag & DEBUG_SELECT)
    fprintf (stdout, "    basic debug\n");
  if (debug_flag & DEBUG_INSERT)
    fprintf (stdout, "    insert debug\n");
  if (debug_flag & DEBUG_SCLK)
    fprintf (stdout, "    sclk debug\n");

  fprintf (stdout, "    \n");

  if ((argc < 2) || fg_flag ("h") || fg_flag ("help")) {
    help (stdout, argv, Version);
    exit (0);
  }
  list = read_raw (argv[argc - 1], launch, end_of_mission);
  raw_read_stat_read1 = raw_read_stat_read;
  raw_read_stat_dupe1 = raw_read_stat_dupe;
  raw_read_stat_head1 = raw_read_stat_head;
  raw_read_stat_tail1 = raw_read_stat_tail;
  raw_read_stat_inst1 = raw_read_stat_inst;
  raw_read_stat_null1 = raw_read_stat_null;

     /*****************************************************************/

  fprintf (stdout, "1st. Scan for time regressions/duplications\n");
  temp[1] = list;                       /* list should have list head */
  temp[0] = list;                       /* list should have list head */
  old_sclk = 0.0;
  record_count = 0;
  while (temp[1]) {
    new_sclk = (double) get_status_32 (temp[1]->buffer->packet.cds.header,
                                       cds_time_bits, __LINE__);
    new_sclk += (double) get_status (temp[1]->buffer->packet.cds.header,
                                     CDS_Time_Bits_SUB, 0) / 256.0;

    new_ID = get_status (temp[1]->buffer->packet.cds.header,
                         CDS_Packet_ID, 0);
    switch (new_ID) {
     case CDS_Packet_ID_Housekeeping_ROM:
     case CDS_Packet_ID_Housekeeping_Deploy:
     case CDS_Packet_ID_Housekeeping_Science:
       new_sclk += .102;                /* make housekeeping sort at the end */
       break;
    }
    if (new_sclk < old_sclk) {          /* new clock must NEVER regress */
      fprintf (stdout, "Regression ");
      dump_record (temp[1]->buffer, stdout, record_count);
      errors++;
    } else if (new_sclk == old_sclk) {  /* duplicate */
      if (new_ID == old_ID) {
        fprintf (stdout, "Duplicate  ");
        dump_record (temp[1]->buffer, stdout, record_count);
        errors++;
      }
    }
    record_count++;
    old_sclk = new_sclk;
    old_ID - new_ID;
    temp[0] = temp[1];
    temp[1] = temp[1]->link;
  }
  fprintf (stdout, "Scanned %d records ", record_count);
  if (errors)
    fprintf (stdout, "found %d errors", errors);
  fprintf (stdout, "\n");
  if (errors) {
    if (errors)
      fprintf (stdout, "Abort\n");
    exit (0);
  }

     /*****************************************************************/


  temp[0] = list;
  temp[1] = list;
  while (temp[1]) {
    _sclk = get_status_32 (temp[1]->buffer->packet.cds.header,
                           cds_time_bits, __LINE__);
    if (_sclk < sclk_early)
      sclk_early = _sclk;
    if (_sclk > sclk_late)
      sclk_late = _sclk;
    temp[1] = temp[1]->link;
  }

  if (argc > 1) {
    early[0] = sclk_2_scet (sclk_early, 0);
    late[0] = sclk_2_scet (sclk_late, 255);
    early[1] = scet_border (early[0], dataset_size, 1);
    late[1] = scet_border (late[0], -dataset_size, 1);
    early[2] = scet_border (early[0], dataset_size, 0);
    late[2] = scet_border (late[0], -dataset_size, 0);
    /*
     * fprintf(stdout, 
     * "Start/Stop times[1]( mdb -sR_FILE %s %s )\n",
     * early[1],
     * late[1]); /*
     */
    fprintf (stdout,
             "Start/Stop times[2]( mdb -sR_FILE %s %s )\n",
             early[2], late[2]);
    /**/ debug_flag |= DEBUG_TAIL;
    list = read_mdb_file (early[2], late[2]);
  }

     /*****************************************************************/

  fprintf (stdout, "2nd. Scan for time regressions/duplications\n");
  temp[0] = list;                       /* list should have list head */
  temp[1] = list;                       /* list should have list head */
  old_sclk = 0.0;
  record_count = 0;
  while (temp[1]) {
    new_sclk = (double) get_status_32 (temp[1]->buffer->packet.cds.header,
                                       cds_time_bits, __LINE__);
    new_sclk += (double) get_status (temp[1]->buffer->packet.cds.header,
                                     CDS_Time_Bits_SUB, 0) / 256.0;

    new_ID = get_status (temp[1]->buffer->packet.cds.header,
                         CDS_Packet_ID, 0);
    switch (new_ID) {
     case CDS_Packet_ID_Housekeeping_ROM:
     case CDS_Packet_ID_Housekeeping_Deploy:
     case CDS_Packet_ID_Housekeeping_Science:
       new_sclk += .102;                /* make housekeeping sort at the */
       break;
    }
    if (new_sclk < old_sclk) {          /* new clock must NEVER regress */
      fprintf (stdout, "Regression ");
      dump_record (temp[0]->buffer, stdout, record_count);
      fprintf (stdout, "  (Delete) ");
      dump_record (temp[1]->buffer, stdout, record_count);
      fprintf (stdout, "           ");
      dump_record (temp[1]->link->buffer, stdout, record_count);

      temp[0]->link = temp[1]->link;

      errors++;
    } else if (new_sclk == old_sclk) {  /* duplicate */
      if (new_ID == old_ID) {
        fprintf (stdout, "Duplicate  ");
        dump_record (temp[0]->buffer, stdout, record_count);
        fprintf (stdout, "  (Delete) ");
        dump_record (temp[1]->buffer, stdout, record_count);
        fprintf (stdout, "           ");
        dump_record (temp[1]->link->buffer, stdout, record_count);
        temp[0]->link = temp[1]->link;
        errors++;
      }
    }

    record_count++;
    old_sclk = new_sclk;
    old_ID - new_ID;
    temp[0] = temp[1];
    temp[1] = temp[1]->link;
  }
  fprintf (stdout, "Scanned %d records ", record_count);
  if (errors)
    fprintf (stdout, "found %d errors", errors);
  fprintf (stdout, "\n");

     /*****************************************************************/

  status = fopen (sort_merge_status ("txt"), "w");
  file_list = fopen (sort_merge_status ("dat"), "w");
  rm_file_list = fopen (sort_merge_status ("remove"), "w");
  if (status) {
    /*
     * 0000000000111111111122222222223333  
     */
    /*
     * 0123456789012345678901234567890123  
     */
    fprintf (status, "KEY0 %32s    0 Version\n", Version);
    fprintf (status, "KEY1 %32s    0 Start time\n", early[2]);
    fprintf (status, "KEY2 %32s    0 Stop time\n", late[2]);
    fprintf (status, "KEY3 %32X    0 SCLK early\n", sclk_early);
    fprintf (status, "KEY4 %32X    0 SCLK late\n", sclk_late);
    fprintf (status, "KEY5 %32d    0 Scan count\n", record_count);
    fprintf (status, "KEY6 %32d    0 Error count\n", errors);
    fprintf (status, "\n");
    fprintf (status, "KEY7a %31d    0 read_raw util_getbuf new\n",
             raw_read_stat_read1);
    fprintf (status, "KEY7b %31d    0 read_raw duplicates  new\n",
             raw_read_stat_dupe1);
    fprintf (status, "KEY7c %31d    0 read_raw queue head  new\n",
             raw_read_stat_head1);
    fprintf (status, "KEY7d %31d    0 read_raw queue mid   new\n",
             raw_read_stat_inst1);
    fprintf (status, "KEY7e %31d    0 read_raw queue tail  new\n",
             raw_read_stat_tail1);
    fprintf (status, "KEY7e %31d    0 read_raw queue null  new\n",
             raw_read_stat_null1);
    fprintf (status, "KEY7f %31d    0 read_raw util_getbuf database\n",
             raw_read_stat_read - raw_read_stat_read1);
    fprintf (status, "KEY7g %31d    0 read_raw duplicates  database\n",
             raw_read_stat_dupe - raw_read_stat_dupe1);
    fprintf (status, "KEY7h %31d    0 read_raw queue head  database\n",
             raw_read_stat_head - raw_read_stat_head1);
    fprintf (status, "KEY7i %31d    0 read_raw queue mid   database\n",
             raw_read_stat_inst - raw_read_stat_inst1);
    fprintf (status, "KEY7j %31d    0 read_raw queue tail  database\n",
             raw_read_stat_tail - raw_read_stat_tail1);
    fprintf (status, "KEY7j %31d    0 read_raw queue null  database\n",
             raw_read_stat_null - raw_read_stat_null1);
    fprintf (status, "KEY7k %31d    0 read_raw util_getbuf both\n",
             raw_read_stat_read);
    fprintf (status, "KEY7l %31d    0 read_raw duplicates  both\n",
             raw_read_stat_dupe);
    fprintf (status, "KEY7m %31d    0 read_raw queue head  both\n",
             raw_read_stat_head);
    fprintf (status, "KEY7n %31d    0 read_raw queue mid   both\n",
             raw_read_stat_inst);
    fprintf (status, "KEY7o %31d    0 read_raw queue tail  both\n",
             raw_read_stat_tail);
    fprintf (status, "KEY7o %31d    0 read_raw queue null  both\n",
             raw_read_stat_null);
    fprintf (status, "\n");
  }
  file_count = write_raw (list, status, file_list);
  if (status) {
    fprintf (status, "KEY9 %32d    0 File count\n", file_count);
    fclose (status);
  }
  if (file_list) {
    fclose (file_list);

    file_list = fopen (sort_merge_status ("dat"), "r");
    u_file_list = fopen (sort_merge_status ("r_file"), "w");
    if (u_file_list) {
      reformat (0, file_list, u_file_list, rm_file_list, file_count, ".r00");
      fclose (u_file_list);
    }

    rewind (file_list);
    u_file_list = fopen (sort_merge_status ("u_list"), "w");
    if (u_file_list) {
      reformat (1, file_list, u_file_list, NULL, file_count, ".u00");
      fclose (u_file_list);
    }

    rewind (file_list);
    u_file_list = fopen (sort_merge_status ("fred"), "w");
    if (u_file_list) {
      reformat (2, file_list, u_file_list, NULL, file_count, ".u00");
      fclose (u_file_list);
    }

    rewind (file_list);
    u_file_list = fopen (sort_merge_status ("ksh"), "w");
    if (u_file_list) {
      reformat (3, file_list, u_file_list, NULL, file_count, ".u00");
      fclose (u_file_list);
      chmod (sort_merge_status ("ksh"), S_IRWXU | S_IRWXG | S_IROTH);
    }

    rewind (file_list);
    u_file_list = fopen (sort_merge_status ("u_file"), "w");
    if (u_file_list) {
      reformat (0, file_list, u_file_list, rm_file_list, file_count, ".u00");
      fclose (u_file_list);
    }

    rewind (file_list);
    u_file_list = fopen (sort_merge_status ("h_file"), "w");
    if (u_file_list) {
      reformat (0, file_list, u_file_list, rm_file_list, file_count, ".h00");
      fclose (u_file_list);
    }

    rewind (file_list);
    u_file_list = fopen (sort_merge_status ("s_file"), "w");
    if (u_file_list) {
      reformat (0, file_list, u_file_list, rm_file_list, file_count, ".s00");
      fclose (u_file_list);
    }

    rewind (file_list);
    u_file_list = fopen (sort_merge_status ("l_file"), "w");
    if (u_file_list) {
      reformat (0, file_list, u_file_list, rm_file_list, file_count, ".l00");
      fclose (u_file_list);
    }

    rewind (file_list);
    u_file_list = fopen (sort_merge_status ("kronos"), "w");
    if (u_file_list) {
      reformat (0, file_list, u_file_list, rm_file_list, file_count, NULL);
      fclose (u_file_list);
    }
    fclose (file_list);
    fclose (rm_file_list);
  }

  exit (0);
}
