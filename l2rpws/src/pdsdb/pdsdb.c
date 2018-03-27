#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "rtiu.h"
#include "util.h"
#define __pdsdb__
#include "pdsdb.h"

#define DEBUG_OPEN 0

 /*******************************************************
  *     Version Information                             *
  *******************************************************/

const char *PDSDB_Version = { PDSDB_VERSION };
static const char *PDSDB_database_pathname = { "/opt/project/cassini/pds" };
static const char *PDSDB_database_filename = { "INDEX/INDEX.LBL" };
static int pdsdb_stat_status = 0;

 /*
  * 
  */

 /*******************************************************
  *******************************************************/

struct LABEL_FIELDS
{
  int result;
  char key[32];
  char c_value[64];
  int i_value;
};
struct OBJECT_FIELDS
{
  int result;
  char key[32];
  char name[64];
  char data_type[64];
  int start_byte;
  int bytes;
  int start_sclk;
  int stop_sclk;
  char description[1024];
};
static struct LABEL_FIELDS label_[] = {
  0, "RECORD_TYPE", "", 0,
  0, "RECORD_BYTES", "", 0,
  0, "FILE_RECORDS", "", 0,
  0, "VOLUME_ID", "", 0,
  1, "OBJECT", "", 0,                   /* name of index file */
  -1, "", "", 0
};
static struct LABEL_FIELDS object_1[] = {
  1, "INTERCHANGE_FORMAT", "", 0,
  1, "ROWS", "", 0,
  1, "ROW_BYTES", "", 0,
  1, "COLUMNS", "", 0,
  1, "INDEX_TYPE", "", 0,
  2, "OBJECT", "", 0,
  1, "END", "", 0,
  -1, "", "", 0
};
static struct OBJECT_FIELDS *object_;
static struct OBJECT_FIELDS object_2[] = {
  3, "VOLUME_ID", "", "", 0, 0, 0, 0, "",
  3, "STANDARD_DATA_PRODUCT_ID", "", "", 0, 0, 0, 0, "",
  3, "DATA_SET_ID", "", "", 0, 0, 0, 0, "",
  3, "PRODUCT_ID", "", "", 0, 0, 0, 0, "",
  3, "START_TIME", "", "", 0, 0, 0, 0, "",
  3, "STOP_TIME", "", "", 0, 0, 0, 0, "",
  3, "SPACECRAFT_CLOCK_START_COUNT", "", "", 0, 0, 0, 0, "",
  3, "FILE_SPECIFICATION_NAME", "", "", 0, 0, 0, 0, "",
  3, "PRODUCT_CREATION_TIME", "", "", 0, 0, 0, 0, "",
  2, "END_OBJECT", "", "", 0, 0, 0, 0, "",
  2, "END", "", "", 0, 0, 0, 0, "",
  -1, "", "", "", 0, 0, 0, 0, ""
};

#define CASE_0 0
#define CASE_1 1
#define CASE_2 2
#define CASE_3 3
#define CASE_4 4

 /*
  * 
  */

 /*******************************************************
  *	returns a line, stripping comments		*
  *******************************************************/
static char *pdsdb_gets_0 (char *string, int count, FILE * stream)
{
  static int comment = 0;
  int i;
  char *temp1;
  char *temp2;

  temp1 = fgets (string, count, stream);
  if (!temp1)
    return NULL;
  for (i = strlen (string) - 1; i > 0; i--) {
    if (string[i] > 0x20)
      break;
    string[i] = 0;
  }
  if (temp1 = strstr (string, "/*"))
    comment |= 1;
  if (temp2 = strstr (string, "*/"))
    comment |= 2;
  switch (comment) {
   case 0:
     break;
   case 1:
     if (!temp1)
       temp1 = string;
     temp1[0] = 0;                      /* remove comment */
     break;
   case 2:
     temp2[1] = 0;                      /* end of comment */
     strcpy (string, temp2 + 3);
     comment = 0;
     break;
   case 3:
     if (!temp1)
       temp1 = string;
     strcpy (temp1, temp2 + 3);
     comment = 0;
     break;
  }
  return string;
}

  /**************************************
   *	Returns lines with some length	*
   **************************************/
static char *pdsdb_gets (char *buffer, int count, FILE * stream)
{
  while (pdsdb_gets_0 (buffer, 80, stream)) {
    if (strlen (buffer))
      return buffer;
  }
  return NULL;
}
static char *nb_scan (char *buf)
{
  static char *null = { "" };
  int i;

  if (!buf)
    return NULL;
  for (i = 1; i < 80; i++) {
    if (!buf[i])
      return null;
    if (buf[i] != ' ')
      return &buf[i];
  }
  return null;
}
static char *pk_scan (char *buffer)
{
  char *temp = buffer;

  while (temp[0] == ' ')
    temp++;
  strcpy (buffer, temp);
  return buffer;
}

 /*
  * 
  */

 /*******************************************************
  *     Decode the index label                          *
  *******************************************************/
static int decode_label_0 (char *buffer, struct LABEL_FIELDS *field_,
                           int obj_flg)
{
  int i = 0;
  int len;
  char *temp;

  len = strlen (buffer);
  if (len < 3)
    return obj_flg;
  pk_scan (buffer);
  while (field_[i].key[0]) {
    temp = strstr (buffer, field_[i].key);
    if (temp) {
      len = strlen (field_[i].key);
      if (!strncmp (field_[i].key, buffer, len)) {
        temp = nb_scan (strchr (buffer + len, '='));
        strcpy (field_[i].c_value, temp);
        field_[i].i_value = atoi (temp);
        return field_[i].result;
      }
    }
    i++;
  }
  return obj_flg;
}
static int decode_label_2 (char *buffer, struct OBJECT_FIELDS *field_,
                           int obj_flg)
{
  char *temp;
  int i = 0;

  object_ = NULL;
  pk_scan (buffer);
  if (!strncmp (buffer, "NAME ", 5)) {
    temp = nb_scan (strchr (buffer + 5, '='));
    while (object_2[i].result > -1) {
      if (!strcmp (object_2[i].key, temp)) {
        object_ = &object_2[i];
        strcpy (object_->name, temp);
        return 3;
      }
      i++;
    }
  }
  return obj_flg;
}
static int decode_label_3 (char *buffer, struct OBJECT_FIELDS *field_,
                           int obj_flg)
{
  char *temp;
  int i = 0;
  int obj_flag = obj_flg;

  pk_scan (buffer);

  if (!field_)
    return obj_flg;

  if (temp = strstr (buffer, "DATA_TYPE ")) {
    temp = nb_scan (strchr (buffer + 10, '='));
    strcpy (field_->data_type, temp);
    obj_flag = 3;
  }


  if (temp = strstr (buffer, "START_BYTE ")) {
    temp = nb_scan (strchr (buffer + 10, '='));
    field_->start_byte = atoi (temp);
    obj_flag = 3;
  }


  if (temp = strstr (buffer, "BYTES ")) {
    temp = nb_scan (strchr (buffer + 6, '='));
    field_->bytes = atoi (temp);
    obj_flag = 3;
  }


  if (temp = strstr (buffer, "END_OBJECT ")) {
    temp = nb_scan (strchr (buffer + 11, '='));
    obj_flag = 2;
  }

  return obj_flag;
}
static int pds_index_label (struct PDSDB *pdsdb_element)
{
  char buffer[128];
  int object_flag = 0;

  while (pdsdb_gets (buffer, 80, pdsdb_element->database_file)) {
    switch (object_flag) {
     case 0:
       object_flag = decode_label_0 (buffer, label_, object_flag);
       break;
     case 1:
       object_flag = decode_label_0 (buffer, object_1, object_flag);
       break;
     case 2:
       object_flag = decode_label_2 (buffer, object_2, object_flag);
       break;
     case 3:
       object_flag = decode_label_3 (buffer, object_, object_flag);
       break;
    }
  }
}

 /*
  * 
  */

 /*******************************************************
  *     Find the index file                             *
  *******************************************************/
static int pds_index_name (struct PDSDB *pdsdb_element, char *keyword)
{
  char *temp;
  char *temp1;
  char *temp2;
  char buffer[128];
  char key[64] = { "^" };
  if (keyword)
    strcat (key, keyword);
  while (pdsdb_gets (buffer, 80, pdsdb_element->database_file)) {
    if (temp = strstr (buffer, key)) {
      temp1 = strchr (temp, '"') + 1;
      temp2 = strchr (temp1, '"');
      temp2[0] = 0;
      temp2 += 2;                       /* skip ", */
      strcpy (pdsdb_element->database_name, temp1);
      pdsdb_element->database_seek = atoi (temp2);
      if (pdsdb_element->database_seek)
        pdsdb_element->database_seek--;
      return 1;
    }
  }
  return 0;
}
static int pds_index_bytes (struct PDSDB *pdsdb_element)
{
  int i = 0;

  while (label_[i].result > -1) {
    if (!strcmp (label_[i].key, "RECORD_BYTES")) {
      return label_[i].i_value;
    }
    i++;
  }
  return 0;
}
static int pds_index_records (struct PDSDB *pdsdb_element)
{
  int i = 0;

  while (label_[i].result > -1) {
    if (!strcmp (label_[i].key, "FILE_RECORDS")) {
      return label_[i].i_value;
    }
    i++;
  }
  return 0;
}
static int pds_index_offset (struct PDSDB *pdsdb_element)
{
  int offset;

  offset = pdsdb_element->database_seek;
  offset *= pds_index_bytes (pdsdb_element);
  return offset;
}
static int pds_index_seek (struct PDSDB *pdb)
{
  int i = 0;

  while (i < PDSDB_TOKENS) {
    if (!pdb->datalabel_record_name[i])
      break;
    if (!strcmp (pdb->pdsdb_open_record, pdb->datalabel_record_name[i] + 1))
      return pdb->datalabel_seek[i];
    i++;
  }
}
static char *pds_index_file (struct LABEL_FIELDS *label_)
{
  int i = 0;

  while (label_[i].result > -1) {
    if (!strcmp (label_[i].key, "OBJECT")) {
      return label_[i].c_value;
    }
    i++;
  }
  return NULL;
}

 /*
  *     Path name hacking,,,
  *     we find the data file from the label, and use the
  *      path (up to the last "/") from the label fiel to access
  *      the data file...
  */
static char *pds_path (char *path)
{
  char *temp;
  static char buf[256];

  strcpy (buf, path);
  temp = strrchr (buf, '/');
  temp[1] = 0;
  return buf;
}
static char *pds_filename (struct PDSDB *pdsdb_element,
                           char *path, char *name, int line)
{
  int i;
  static int index = 0;
  static char buf[8][256];


  index += 1;
  index &= 0x07;
  strcpy (buf[index], pdsdb_element->database_path);
  strcat (buf[index], "/");
  if (path)
    strcat (buf[index], pds_path (path));
  strcat (buf[index], name);
  i = strlen (buf[index]) - 1;
  while (buf[index][i] == ' ') {
    buf[index][i] = 0;
    i--;
  }
  return buf[index];
}

 /*
  * 
  */

/************************************************/

static int pds_index_item (char *key)
{
  int i = 0;

  while (object_2[i].result > -1) {
    if (!strcmp (object_2[i].key, key))
      return i;
    i++;
  }
  return 0;
}
static int pds_index_extract (struct PDSDB *pdb)
{
  int i;

  for (i = 0; i < PDSDB_TOKENS; i++) {
    pdb->database_fields[i] = NULL;
    if (object_2[i].result < 0)
      break;
    pdb->database_fields[i] =
      &pdb->database_detail_line[object_2[i].start_byte - 1];
    pdb->database_fields[i][object_2[i].bytes] = 0;
  }
  return i;
}
static int pds_index_next (struct PDSDB *pdb, int line)
{
  int status;
  int index;

  while (status = fread (pdb->database_detail_line,
                         pdb->database_bytes, 1, pdb->database_file)) {
    pdb->database_rec_num++;
    pds_index_extract (pdb);
    index = pds_index_item ("STANDARD_DATA_PRODUCT_ID");
    if (strlen (pdb->pdsdb_open_type) &&
        strstr (pdb->database_fields[index], pdb->pdsdb_open_type)) {
      strcpy (pdb->datalabel_name,
              pdb->
              database_fields[pds_index_item ("FILE_SPECIFICATION_NAME")]);
      strcpy (pdb->datalabel_volume,
              pdb->database_fields[pds_index_item ("VOLUME_ID")]);
      if (!pdb->pdsdb_open_subtype)
        break;
      if (!strlen (pdb->pdsdb_open_subtype))
        break;
      if (strstr (pdb->datalabel_name, pdb->pdsdb_open_subtype))
        break;
    }
  }
  if (0)
    fprintf (stderr, "NEXT %d/%d %s\n", __LINE__, line, pdb->datalabel_name);
  return status;
}

 /*
  * 
  */

 /*******************************************************
  *     Extract some data file label information	*
  *******************************************************/
static int pds_datafile_label (struct PDSDB *pds)
{
  char buffer[128];
  int findex = 0;
  char *temp;
  char *tempz;
  int status;

  status = fseek (pds->datalabel_file, 0, SEEK_SET);
  while (pdsdb_gets (buffer, 80, pds->datalabel_file)) {
    pk_scan (buffer);
    if (!strncmp (buffer, "RECORD_BYTES ", 13)) {
      temp = nb_scan (strchr (buffer + 13, '='));
      pds->datalabel_bytes = atoi (temp);
    }
    if (!strncmp (buffer, "FILE_RECORDS ", 13)) {
      temp = nb_scan (strchr (buffer + 13, '='));
      pds->datalabel_records = atoi (temp);
    }

    if (!strncmp (buffer, "SPACECRAFT_CLOCK_START_COUNT", 28)) {
      temp = nb_scan (strchr (buffer + 28, '='));
      temp = nb_scan (strchr (temp + 1, '/'));
      pds->datalabel_sclk_cnt_start = atoi (temp);
    }
    if (!strncmp (buffer, "SPACECRAFT_CLOCK_STOP_COUNT", 27)) {
      temp = nb_scan (strchr (buffer + 27, '='));
      temp = nb_scan (strchr (temp + 1, '/'));
      pds->datalabel_sclk_cnt_stop = atoi (temp);
    }

    if (!pds->datalabel_sclk_cnt_start)
      if (!strncmp (buffer, "START_TIME ", 11)) {
        temp = nb_scan (strchr (buffer + 11, '='));
        tempz = strchr (temp, 'Z');
        if (tempz)
          tempz[0] = 0;
        pds->datalabel_sclk_cnt_start = MDB_time (temp);
      }

    if (!pds->datalabel_sclk_cnt_stop)
      if (!strncmp (buffer, "STOP_TIME ", 10)) {
        temp = nb_scan (strchr (buffer + 10, '='));
        tempz = strchr (temp, 'Z');
        if (tempz)
          tempz[0] = 0;
        pds->datalabel_sclk_cnt_stop = MDB_time (temp);
      }

    if (!strncmp (buffer, "OBJECT ", 7)) {
      temp = nb_scan (strchr (buffer + 7, '='));
      if (strcmp (temp, "COLUMN")) {
        pds->datalabel_record_name[findex][0] = '^';
        pds->datalabel_record_name[findex][1] = 0;
        strcat (pds->datalabel_record_name[findex], temp);
        findex++;
      }
    }
    if (!strncmp (buffer, "END_OBJECT ", 11)) {
    }
  }

  status = fseek (pds->datalabel_file, 0, SEEK_SET);
  while (pdsdb_gets (buffer, 80, pds->datalabel_file)) {
    int i;

    pk_scan (buffer);
    i = 0;
    for (i = 0; i < findex; i++) {
      if (strstr (buffer, pds->datalabel_record_name[i])) {
        temp = nb_scan (strchr (buffer + 10, '='));
        temp = strchr (temp, '"');
        strcpy (pds->datafile_name, temp + 1);
        temp = strchr (pds->datafile_name, '"');
        temp[0] = 0;
        temp += 2;                      /* skip quote & comma */
        pds->datalabel_seek[i] = atoi (temp);
        if (pds->datalabel_seek[i])
          pds->datalabel_seek[i]--;
        pds->datalabel_seek[i] *= pds->datalabel_bytes;
      }
    }
  }
}

 /*
  * 
  */

 /*******************************************************
  *     Debugging dumps	                                *
  *******************************************************/
static char *dump_strip (char *inbuf)
{
  static int index = 0;
  static char buf[8][256];
  int i = strlen (inbuf);

  i--;
  index++;
  index &= 7;
  strcpy (buf[index], inbuf);
  while (buf[index][i] == ' ') {
    buf[index][i] = 0;
    i--;
  }
  return buf[index];
}
static int pds_index_dump (FILE * out)
{
  int i;

  fprintf (out, "pds_index_dump\n");
  i = 0;
  while (label_[i].result > -1) {
    fprintf (out, "    %16s = %6d %s\n",
             label_[i].key,
             label_[i].i_value, dump_strip (label_[i].c_value));
    i++;
  }
  i = 0;
  while (object_1[i].result > -1) {
    fprintf (out, "    %20s = %6d %s\n",
             object_1[i].key,
             object_1[i].i_value, dump_strip (object_1[i].c_value));
    i++;
  }
  i = 0;
  while (object_2[i].result > -1) {
    fprintf (out, "    %32s %16s %3d %3d %08X %s\n",
             object_2[i].name,
             object_2[i].data_type,
             object_2[i].start_byte,
             object_2[i].bytes,
             object_2[i].start_sclk, dump_strip (object_2[i].description));
    i++;
  }
}
char name[64];
char data_type[64];
int start_byte;
int bytes;
char description[1024];
static int pdsdb_dump (FILE * out, struct PDSDB *pdsdb)
{
  int i;

  fprintf (out, "DB  database_path         \"%s\"\n",
           dump_strip (pdsdb->database_path));
  fprintf (out, "DB  database_name         \"%s\"\n",
           dump_strip (pdsdb->database_name));
  fprintf (out, "DB  database_file          %p %s\n", pdsdb->database_file,
           pdsdb->database_file ? "OPEN" : "");
  fprintf (out, "DB  database_rec_num       %d\n", pdsdb->database_rec_num);
  fprintf (out, "DB  database_detail_line  \"%s\"\n",
           dump_strip (pdsdb->database_detail_line));
  fprintf (out, "DB  database_seek          %d\n", pdsdb->database_seek);
  fprintf (out, "DB  database_records       %d\n", pdsdb->database_records);
  fprintf (out, "DB  database_bytes         %d\n", pdsdb->database_bytes);
  for (i = 0; i < PDSDB_TOKENS; i++) {
    if (pdsdb->database_fields[i])
      fprintf (out,
               "DB  database_fields[%02d]   \"%s\"\n", i,
               dump_strip (pdsdb->database_fields[i]));
    else
      break;
  }
  fprintf (out, "DO  pdsdb_open_type       \"%s\"\n",
           dump_strip (pdsdb->pdsdb_open_type));
  fprintf (out, "DO  pdsdb_open_subtype    \"%s\"\n",
           dump_strip (pdsdb->pdsdb_open_subtype));
  fprintf (out, "DO  pdsdb_open_record     \"%s\"\n",
           dump_strip (pdsdb->pdsdb_open_record));

  fprintf (out, "DB  datalabel_volume      \"%s\"\n",
           dump_strip (pdsdb->datalabel_volume));
  fprintf (out, "DL  datalabel_name        \"%s\"\n",
           dump_strip (pdsdb->datalabel_name));
  fprintf (out, "DL  datalabel_file         %p %s\n", pdsdb->datalabel_file,
           pdsdb->datalabel_file ? "OPEN" : "");
  fprintf (out, "DL        _sclk_cnt_start  %08X\n",
           pdsdb->datalabel_sclk_cnt_start);
  fprintf (out, "DL        _sclk_cnt_stop   %08X\n",
           pdsdb->datalabel_sclk_cnt_stop);
  fprintf (out, "DL  datalabel_records      %d\n", pdsdb->datalabel_records);
  fprintf (out, "DL  datalabel_bytes        %d\n", pdsdb->datalabel_bytes);
  fprintf (out, "DL  datalabel_seek         %d\n", pdsdb->datalabel_seek);
  for (i = 0; i < PDSDB_TOKENS; i++) {
    if (strlen (pdsdb->datalabel_record_name[i]))
      fprintf (out,
               "DB  datalabel_record_name[%02d] \"%d=%s\"\n", i,
               pdsdb->datalabel_seek[i],
               dump_strip (pdsdb->datalabel_record_name[i]));
    else
      break;
  }

  fprintf (out, "DF  datafile_name         \"%s\"\n",
           dump_strip (pdsdb->datafile_name));
  fprintf (out, "DF  datafile_file          %p %s\n", pdsdb->datafile_file,
           pdsdb->datafile_file ? "OPEN" : "");
  fprintf (out, "DF  datafile_rec_num       %d\n", pdsdb->datafile_rec_num);
  fprintf (out, "DF  datafile_stream        %d\n", pdsdb->datafile_stream);

  fprintf (out, "TM  requested_start_sclk   %08X\n",
           pdsdb->requested_start_sclk);
  fprintf (out, "TM  requested_stop_sclk    %08X\n",
           pdsdb->requested_stop_sclk);

  fprintf (out, "DT  buffer                 %p %s\n", pdsdb->buffer,
           pdsdb->buffer ? "ALLOC" : "");
  /**/ fflush (out);
  return 0;
}
static int pds_time_dump (FILE * out, struct PDSDB *pdb)
{
  int time = 0;
  static char string[32] = { "UNKNOWN TIME" };
  switch (pdb->datafile_stream) {
   case PDSDB_STREAM_RPWS_KEY_PARAMETERS:
   case PDSDB_STREAM_RPWS_KEY_HEADER:
     memcpy (string, &pdb->buffer[0], 17);
     break;
   case PDSDB_STREAM_RPWS_LOW_RATE_FULL:
   case PDSDB_STREAM_RPWS_LOW_RATE_LRFULL:
   case PDSDB_STREAM_RPWS_LOW_RATE_TIME:
   case PDSDB_STREAM_RPWS_LOW_RATE_FREQUENCY:
     time = (pdb->buffer[16] & 0xFF) << 24;
     time |= (pdb->buffer[17] & 0xFF) << 16;
     time |= (pdb->buffer[18] & 0xFF) << 8;
     time |= (pdb->buffer[19] & 0xFF) << 0;
     break;
   case PDSDB_STREAM_RPWS_WAVEFORM_FULL:
   case PDSDB_STREAM_RPWS_WIDEBAND_FULL:
     time = (pdb->buffer[0] & 0xFF) << 24;
     time |= (pdb->buffer[1] & 0xFF) << 16;
     time |= (pdb->buffer[2] & 0xFF) << 8;
     time |= (pdb->buffer[3] & 0xFF) << 0;
     break;
   case PDSDB_STREAM_RPWS_RAW_COMPLETE:
     fprintf (stderr, "STREAM %d Can't read RAW with this call\n", __LINE__);
  }
  if (time)
    sprintf (string, "1/%d:000", time);
  fprintf (out, "TIME %D %s\n", __LINE__, MDB_time_SCET (string));
  return 0;
}
int PDSDB_dump (FILE * out, struct PDSDB *pdsdb, int flag)
{
  if (flag & PDSDB_DUMP_PDSDB)
    pdsdb_dump (out, pdsdb);
  if (flag & PDSDB_DUMP_INDEX)
    pds_index_dump (out);
  if (flag & PDSDB_DUMP_TIME)
    pds_time_dump (out, pdsdb);
  return 0;
}

 /*
  * 
  */

 /*******************************************************
  *     Open PDS database                               *
  *******************************************************/

                                                /********************************/
struct PDSDB *PDSDB_open (char *start_time,     /*       desired stat time      */
                          char *stop_time,      /* (opt) desired stop time      */
                          char *path,   /* (opt) path to index/cumindex */
                          char *name,   /* (opt) name of index LABEL    */
                          char *type,   /* STANDARD_DATA_PRODUCT_ID     */
                          char *subtype,        /* FILE_SPECIFICATION_NAME      */
                          char *record)

{                                               /* Record Offset          *//********************************/
  int i;
  int status;
  struct PDSDB *pdsdb_element;
  char *temp;
  char index_path[256];

     /****************************************
      *	Allocate memory for data structure,  *
      *   filename buffers, and data buffer  *
      ****************************************/
  pdsdb_element = malloc (sizeof (struct PDSDB));
  memset (pdsdb_element, 0, sizeof (struct PDSDB));
  if (!pdsdb_element) {
    fprintf (stderr, "%s/%d malloc(struct PDSDB) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }
  pdsdb_element->database_path = malloc (PDSDB_NAME_BUFFER + 1);
  if (!pdsdb_element->database_path) {
    fprintf (stderr, "%s/%d malloc(database_path) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }
  pdsdb_element->database_name = malloc (PDSDB_NAME_BUFFER + 1);
  if (!pdsdb_element->database_name) {
    fprintf (stderr, "%s/%d malloc(database_name) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }
  pdsdb_element->pdsdb_open_type = malloc (PDSDB_NAME_BUFFER + 1);
  if (!pdsdb_element->pdsdb_open_type) {
    fprintf (stderr, "%s/%d malloc(pdsdb_open_type) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }
  pdsdb_element->pdsdb_open_subtype = malloc (PDSDB_NAME_BUFFER + 1);
  if (!pdsdb_element->pdsdb_open_subtype) {
    fprintf (stderr, "%s/%d malloc(pdsdb_open_subtype) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }
  pdsdb_element->buffer = malloc (PDSDB_DATA_BUFFER + 1);
  if (!pdsdb_element->buffer) {
    fprintf (stderr, "%s/%d malloc(record buffer) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }

  pdsdb_element->datalabel_volume = malloc (PDSDB_NAME_BUFFER + 1);
  if (!pdsdb_element->datalabel_volume) {
    fprintf (stderr, "%s/%d malloc(datalabel_volume) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }

  pdsdb_element->datalabel_name = malloc (PDSDB_NAME_BUFFER + 1);
  if (!pdsdb_element->datalabel_name) {
    fprintf (stderr, "%s/%d malloc(datalabel_name) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }
  pdsdb_element->pdsdb_open_record = malloc (PDSDB_NAME_BUFFER + 1);
  if (!pdsdb_element->pdsdb_open_record) {
    fprintf (stderr, "%s/%d malloc(pdsdb_open_record) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }
  pdsdb_element->datafile_name = malloc (PDSDB_NAME_BUFFER + 1);
  if (!pdsdb_element->datafile_name) {
    fprintf (stderr, "%s/%d malloc(datafile_name) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }
  for (i = 0; i < PDSDB_TOKENS; i++) {
    pdsdb_element->datalabel_record_name[i] = malloc (PDSDB_NAME_BUFFER + 1);
    if (!pdsdb_element->datalabel_record_name[i]) {
      fprintf (stderr, "%s/%d malloc(datalabel_record_name[%d] failed\n",
               __FILE__, __LINE__, i);
      exit (0);
    }
  }

     /*******************************
      *	Initialize data structure,  *
      *  loading start & stop times *
      *******************************/
  pdsdb_element->link = NULL;           /* reserved for user */

  pdsdb_element->requested_start_sclk = MDB_time (start_time);
  if (stop_time)
    pdsdb_element->requested_stop_sclk = MDB_time (stop_time);
  if (type)
    strcpy (pdsdb_element->pdsdb_open_type, type);
  if (subtype)
    strcpy (pdsdb_element->pdsdb_open_subtype, subtype);
  if (record)
    strcpy (pdsdb_element->pdsdb_open_record, record);
  pdsdb_stat_status = 1;

  if (stop_time)
    pdsdb_element->requested_stop_sclk = MDB_time (stop_time);
  else
    pdsdb_element->requested_stop_sclk = 0x7FFFFFFE;

    /****************************************************
     * Open the LABEL file and find out about INDEX.TAB	*
     ****************************************************/
  if (name)
    strcpy (pdsdb_element->database_name, name);
  else
    strcpy (pdsdb_element->database_name, PDSDB_database_filename);

  if (path)
    strcpy (pdsdb_element->database_path, path);
  else
    strcpy (pdsdb_element->database_path, PDSDB_database_pathname);

  if (DEBUG_OPEN)
    fprintf (stdout, "%4d OPEN base  %s\n", __LINE__,
             pdsdb_element->database_name);
  pdsdb_element->database_file =
    fopen (pds_filename
           (pdsdb_element, NULL, pdsdb_element->database_name, __LINE__),
           "r");
  if (!pdsdb_element->database_file) {
    fprintf (stderr, "FAIL %d fopen(\"%s\")\n", __LINE__,
             pds_filename (pdsdb_element, NULL, pdsdb_element->database_name,
                           __LINE__));
    return NULL;
  }
  pds_index_label (pdsdb_element);

    /****************************************************
     * go back and find the name of the INDEX FILE 	*
     ****************************************************/
  strcpy (index_path, pdsdb_element->database_name);
  fseek (pdsdb_element->database_file, 0, SEEK_SET);
  pds_index_name (pdsdb_element, pds_index_file (label_));
  fclose (pdsdb_element->database_file);
  pdsdb_element->database_rec_num = 0;
  pdsdb_element->database_file = NULL;

    /****************************************************
     * Open the INDEX file and find the data LABELS	*
     ****************************************************/
  if (DEBUG_OPEN)
    fprintf (stdout, "%4d OPEN base  %s\n", __LINE__,
             pdsdb_element->database_name);
  pdsdb_element->database_file =
    fopen (pds_filename
           (pdsdb_element, index_path, pdsdb_element->database_name,
            __LINE__), "r");
  if (!pdsdb_element->database_file) {
    fprintf (stderr, "FAIL %d fopen(\"%s\")\n", __LINE__,
             pds_filename (pdsdb_element,
                           NULL, pdsdb_element->database_name, __LINE__));
    exit (0);
  }
  pdsdb_element->database_bytes = pds_index_bytes (pdsdb_element);
  pdsdb_element->database_records = pds_index_records (pdsdb_element);
  pdsdb_element->database_detail_line =
    malloc (pdsdb_element->database_bytes + 15);
  if (!pdsdb_element->database_detail_line) {
    fprintf (stderr, "%s/%d malloc(database_detail_line) failed\n", __FILE__,
             __LINE__);
    exit (0);
  }
  memset (pdsdb_element->database_detail_line, 0,
          pdsdb_element->database_bytes + 15);
  for (i = 0; i < pdsdb_element->database_seek; i++) {
    fread (pdsdb_element->database_detail_line,
           pdsdb_element->database_bytes, 1, pdsdb_element->database_file);
    pdsdb_element->database_rec_num++;
  }
  pdsdb_element->database_seek = ftell (pdsdb_element->database_file);
  status = pds_index_next (pdsdb_element, __LINE__);
  if (DEBUG_OPEN)
    fprintf (stdout, "%4d OPEN label %s\n", __LINE__,
             pdsdb_element->datalabel_name);
  pdsdb_element->datalabel_file =
    fopen (pds_filename
           (pdsdb_element, NULL, pdsdb_element->datalabel_name, __LINE__),
           "r");
  if (!pdsdb_element->datalabel_file) {
    fprintf (stderr, "FAIL%d %s %X=fopen(\"%s\")\n", __LINE__,
             strerror (errno),
             pdsdb_element->datalabel_file,
             pds_filename (pdsdb_element,
                           NULL, pdsdb_element->datalabel_name, __LINE__));
    return NULL;
  }
  pds_datafile_label (pdsdb_element);
  fclose (pdsdb_element->datalabel_file);
  pdsdb_element->datalabel_file = NULL;
  return pdsdb_element;
}

 /*******************************************************
  *	Close, releasing all resources			*
  *		deallocate all buffers			*
  *******************************************************/
int PDSDB_close (struct PDSDB *pdb)
{
  int i;
  int n = 28;

  if (pdb->datafile_file) {
    fclose (pdb->datalabel_file);
    n--;
  }
  if (pdb->datalabel_file) {
    fclose (pdb->datalabel_file);
    n--;
  }
  if (pdb->database_file) {
    fclose (pdb->database_file);
    n--;
  }

  if (pdb->database_detail_line) {
    free (pdb->database_detail_line);
    n--;
  }
  for (i = PDSDB_TOKENS; i > 0; i--) {
    if (pdb->datalabel_record_name[i - 1]) {
      free (pdb->datalabel_record_name[i - 1]);
      n--;
    }
  }
  if (pdb->datafile_name) {
    free (pdb->datafile_name);
    n--;
  }
  if (pdb->pdsdb_open_record) {
    free (pdb->pdsdb_open_record);
    n--;
  }
  if (pdb->datalabel_name) {
    free (pdb->datalabel_name);
    n--;
  }
  if (pdb->datalabel_volume) {
    free (pdb->datalabel_volume);
    n--;
  }
  if (pdb->buffer) {
    free (pdb->buffer);
    n--;
  }
  if (pdb->pdsdb_open_subtype) {
    free (pdb->pdsdb_open_subtype);
    n--;
  }
  if (pdb->pdsdb_open_type) {
    free (pdb->pdsdb_open_type);
    n--;
  }
  if (pdb->database_name) {
    free (pdb->database_name);
    n--;
  }
  if (pdb->database_path) {
    free (pdb->database_path);
    n--;
  }
  if (pdb) {
    free (pdb);
    n--;
  }
  return n;
}

/**/

 /*******************************************************
  *     Read PDS datafile				*
  *******************************************************/
 /*
  *     Read data file (->datafile_)
  *
  *     1. need to open a file
  *             datafile_file == NULL
  *             datafile_rec_num == 0
  *         OPEN next file and pass back data
  *         MIGHT pass back NULL if select didn't
  *             find any data.
  *     2. reading some records (im middle)
  *             datafile_file != NULL
  *             datafile_rec_num < datalabel_records
  *         READ should return data that we pass back.
  *         MIGHT have a problem, pass back NULL
  *         CLOSE datafile after last record read.
  *     3. time to open next file
  *             datafile_file == NULL
  *             datafile_rec_num == datalabel_records
  *         CLOSE current file (reset stuff too)
  *         MIGHT have problem in database (NULL)
  *         MIGHT have problem with open (NULL)
  *         MIGHT not see data (NULL)
  *     4. All done
  *             database_rec_num == database_records
  */
int pdsdb_read (struct PDSDB *pdb)
{
  int status;
  int flag = CASE_0;
  char *temp;

  if ((pdb->datafile_file == NULL) && (pdb->datafile_rec_num == 0))
    flag = CASE_1;

  if ((pdb->datafile_file != NULL) &&
      (pdb->datafile_rec_num < pdb->datalabel_records))
    flag = CASE_2;

  if ((pdb->datafile_file == NULL) &&
      (pdb->datafile_rec_num >= pdb->datalabel_records))
    flag = CASE_3;

  if (pdb->database_rec_num >= pdb->database_records)
    flag = CASE_4;

  switch (flag) {
   case 0:
     return 1;
   case 3:
     status = pds_index_next (pdb, __LINE__);
     if (DEBUG_OPEN)
       fprintf (stdout, "%4d OPEN label %s\n", __LINE__, pdb->datalabel_name);
     pdb->datalabel_file = fopen (pds_filename (pdb,
                                                NULL,
                                                pdb->datalabel_name,
                                                __LINE__), "r");
     if (!pdb->datalabel_file) {
       fprintf (stderr, "LOST %d %s %X=fopen(\"%s\")\n", __LINE__,
                strerror (errno),
                pdb->datalabel_file,
                pds_filename (pdb, NULL, pdb->datalabel_name, __LINE__));
       return 1;
     }
     pds_datafile_label (pdb);
     fclose (pdb->datalabel_file);
     pdb->datalabel_file = NULL;
     pdb->datafile_rec_num = 0;
     return 13;
   case 1:
     temp = pds_filename (pdb,
                          pdb->datalabel_name, pdb->datafile_name, __LINE__);
     if (DEBUG_OPEN)
       fprintf (stdout, "%4d OPEN file  %s\n", __LINE__, temp);
     pdb->datafile_file = fopen (temp, "r");
     /**/ pdb->datafile_rec_num = 0;
     if (!pdb->datafile_file)
       return 1;
     fseek (pdb->datafile_file, pds_index_seek (pdb), SEEK_SET);
     return 11;
   case 2:
     break;
   case 4:
     return 1;
  }
  if (!pdb->datafile_file)
    return 1;
  status = fread (pdb->buffer, pdb->datalabel_bytes, 1, pdb->datafile_file);
  pdb->datafile_rec_num++;
  if (pdb->datafile_rec_num >= pdb->datalabel_records) {
    fclose (pdb->datafile_file);
    pdb->datafile_file = NULL;
  }
  return 0;
}
unsigned char *PDSDB_read (struct PDSDB *pdb)
{                                       /*       read next record      */
  int status;

  while (status = pdsdb_read (pdb)) {
    switch (status) {
     case 1:
       return NULL;
     case 13:
     case 11:
       if ((pdb->datalabel_sclk_cnt_stop < pdb->requested_start_sclk) ||
           (pdb->requested_stop_sclk < pdb->datalabel_sclk_cnt_start)) {
         if (pdb->datafile_file) {
           fclose (pdb->datafile_file);
           pdb->datafile_file = NULL;
         }
         pdb->datafile_rec_num = pdb->datalabel_records + 1;
       }
       break;
    }
  }
  if (0)
    fprintf (stderr, "PDSDB_read %d\n", __LINE__);
  return pdb->buffer;
}
unsigned char *PDSDB_read_stream (struct PDSDB *pdb, int stream)
{                                       /*       read next record      */
  int time;
  char time_temp[32] = { 32 * 0 };
  unsigned char *buffer;

  pdb->datafile_stream = stream;
  while (buffer = PDSDB_read (pdb)) {
    switch (pdb->datafile_stream) {
     case PDSDB_STREAM_RPWS_KEY_PARAMETERS:
     case PDSDB_STREAM_RPWS_KEY_HEADER:
       memcpy (time_temp, &buffer[0], 17);
       time = MDB_time (time_temp);
       break;
     case PDSDB_STREAM_RPWS_LOW_RATE_FULL:
     case PDSDB_STREAM_RPWS_LOW_RATE_LRFULL:
     case PDSDB_STREAM_RPWS_LOW_RATE_TIME:
     case PDSDB_STREAM_RPWS_LOW_RATE_FREQUENCY:
       time = (buffer[16] & 0xFF) << 24;
       time |= (buffer[17] & 0xFF) << 16;
       time |= (buffer[18] & 0xFF) << 8;
       time |= (buffer[19] & 0xFF) << 0;
       break;
     case PDSDB_STREAM_RPWS_WAVEFORM_FULL:
     case PDSDB_STREAM_RPWS_WIDEBAND_FULL:
       time = (buffer[0] & 0xFF) << 24;
       time |= (buffer[1] & 0xFF) << 16;
       time |= (buffer[2] & 0xFF) << 8;
       time |= (buffer[3] & 0xFF) << 0;
       break;
     case PDSDB_STREAM_RPWS_RAW_COMPLETE:
       fprintf (stderr, "STREAM %d Can't read RAW with this call\n",
                __LINE__);
    }
    if ((time >= pdb->requested_start_sclk) &&
        ((time + 1) <= pdb->requested_stop_sclk))
      break;
    if (0)
      fprintf (stderr,
               "%08X  %08X  %08X\n",
               pdb->requested_start_sclk, time, pdb->requested_stop_sclk);
  }
  if (0)
    fprintf (stderr, "RETURN\n");
  return buffer;
}
