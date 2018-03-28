#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <rtiu.h>
#include <util.h>

#include "mpii3.h"

#define MP_THREADS 4

                        /************************************************
			 *						*
			 ************************************************/

char mp2_gen_Version[] = { "mp3_gen V3.8" };

enum
{ STEP_,
  STEP_Unsegmented,
  STEP_Segmented,
  STEP_Segmented_missing,
  STEP_Continue,
  STEP_Continue_Again
};
static char *step_[] = { "STEP_",
  "STEP_Unsegmente",
  "STEP_Segmented",
  "STEP_Segmented_missing",
  "STEP_Continue",
  "STEP_Continue_Again" "STEP_Error" "STEP_Error"
};

enum
{ COUNT_MOVE,
  COUNT_MOVE_XX,
  COUNT_MP,
  COUNT_CDS,
  COUNT_FILL,
  COUNT_REMAIN
};

static FILE *debug = NULL;
static FILE *diag = NULL;

static int item_mask = -1;

                        /************************************************
			 *	STREAM DATA STRUCTURE			*
			 ************************************************/

#define STIM_MASK	(1<<PACKET_TYPE_stim)
#define MFR_MASK	(1<<PACKET_TYPE_mfr)
#define HFR_MASK	(1<<PACKET_TYPE_hfr)
#define LP_MASK		(1<<PACKET_TYPE_lp)
#define LFDR_MASK	(1<<PACKET_TYPE_lfdr)
#define WFR_MASK	(1<<PACKET_TYPE_wfr)
#define DUST_MASK	(1<<PACKET_TYPE_dust)
#define MRO_MASK	(1<<PACKET_TYPE_mro)
#define WBR_MASK	(1<<PACKET_TYPE_wbr)
#define FILL_MASK	(1<<PACKET_TYPE_fill)

#define LRS_MASK	STIM_MASK|MFR_MASK|HFR_MASK|LP_MASK|LFDR_MASK|WFR_MASK|DUST_MASK|MRO_MASK|WBR_MASK
#define HRS_MASK	WBR_MASK|WFR_MASK

struct MP
{
  int sequence;                         /* ccsds sequence  */
  int mindex;                           /* index into mp   */
  unsigned char flag_1;                 /* STEP            */
  unsigned char flag_2;                 /* 3rd packet flag */
  unsigned char flag_3;                 /*                 */
  unsigned char flag_4;                 /*                 */
  signed int valid_mask;                /* valid types for */
  /*
   * this CDS stream 
   */
  struct DATA_RECORD *data_buffer;      /* cds data struct */
  struct MP_buffer *mp_buffer;          /* mp dta struct   */
};
static struct MP mp[MP_THREADS + 1] = { 0, 0, 0, 0, 0, 0, -1, NULL, NULL,       /* Free queue           */
  0, 0, 0, 0, 0, 0, LRS_MASK, NULL, NULL,       /* Low Rate Science     */
  0, 0, 0, 0, 0, 0, HRS_MASK, NULL, NULL,       /* High Rate Science    */
  0, 0, 0, 0, 0, 0, -1, NULL, NULL,     /* MRO records          */
  0, 0, 0, 0, 0, 0, -1, NULL, NULL
};                                      /* spare                */
static int first_time = 1;


extern int Minimum_Buffer_Size;
extern int Minimum_Buffer_Pad;

 /*
  * 
  */

 /*******************************************************
  *	Diagnostic DUMP
  *		note 3rd. argument controls 
  *		what junk is dumped...
  *
  *	1st. arg is the stream data structure (HRS/LRS/MRO)
  *	2nd. argument is the file handle (FILE)
  *	3rd. argument is the information flag word
  *		0x01  sequence field
  *		0x02  mp sclk
  *		0x04  cds data
  *		0x08  minipakcet data
  *		0x10  (supress) entire minipacket data area
  *		0x20  mp/cds time tag
  *		0x40  cds data
  *		0x80
  *		0x100 "Not Written"
  *	4th. argument is the offset into the data array
  *******************************************************/
void MP2_MP_dump (struct MP *mp, FILE * fout, int flag, int index)
{
  static int icnt[17] = { 17 * 0 };
  int i;
  int imax;
  int type;
  unsigned int count;
  char temp[20];

  if (!fout)
    return;
  if (flag & 0x0F)
    fprintf (fout, "MP2_MP_dump   %P *******************************\n", mp);
  if (flag & 0x01) {
    fprintf (fout, "    sequence  %d\n", mp->sequence);
    fprintf (fout, "    index     %d\n", mp->mindex);
    fprintf (fout, "    flags     %02X %02X %02X %02X %s\n",
             mp->flag_1, mp->flag_2, mp->flag_3, mp->flag_4,
             step_[mp->flag_1 & 0x07]);
  }
  if (flag & 0x02) {
    fprintf (fout, "  data_buffer %P\n", mp->data_buffer);
    fprintf (fout, "    links     %8P %8P %8P\n",
             mp->data_buffer->forward,
             mp->data_buffer->reverse, mp->data_buffer->link);
    fprintf (fout, "    q elm cnt %d\n", mp->data_buffer->element_count);
    fprintf (fout, "    sclk      %08X\n", mp->data_buffer->sclk);
    fprintf (fout, "    rti       %3d\n", mp->data_buffer->rti);
    fprintf (fout, "    type      %d\n", mp->data_buffer->type);
  }
  if (flag & 0x04) {
    fprintf (fout, "  cds_record  %P\n", &mp->data_buffer->cds_record);
    fprintf (fout,
             "    cds_hdr   %02X%02X %02X%02X %02X%02X %02X%02X"
             " %02X%02X %02X%02X\n",
             mp->data_buffer->cds_record.packet.cds.header[1],
             mp->data_buffer->cds_record.packet.cds.header[0],
             mp->data_buffer->cds_record.packet.cds.header[3],
             mp->data_buffer->cds_record.packet.cds.header[2],
             mp->data_buffer->cds_record.packet.cds.header[5],
             mp->data_buffer->cds_record.packet.cds.header[4],
             mp->data_buffer->cds_record.packet.cds.header[7],
             mp->data_buffer->cds_record.packet.cds.header[6],
             mp->data_buffer->cds_record.packet.cds.header[9],
             mp->data_buffer->cds_record.packet.cds.header[8],
             mp->data_buffer->cds_record.packet.cds.header[11],
             mp->data_buffer->cds_record.packet.cds.header[10]);
    fprintf (fout,
             "    rpws_data %4d: %02X %02X %02X %02X %02X %02X %02X"
             " %02X\n", index,
             mp->data_buffer->cds_record.packet.rpws.data[index + 0],
             mp->data_buffer->cds_record.packet.rpws.data[index + 1],
             mp->data_buffer->cds_record.packet.rpws.data[index + 2],
             mp->data_buffer->cds_record.packet.rpws.data[index + 3],
             mp->data_buffer->cds_record.packet.rpws.data[index + 4],
             mp->data_buffer->cds_record.packet.rpws.data[index + 5],
             mp->data_buffer->cds_record.packet.rpws.data[index + 6],
             mp->data_buffer->cds_record.packet.rpws.data[index + 7]);
  }
  if (flag & 0x08) {
    fprintf (fout, "  mp_buffer   %P\n", mp->mp_buffer);
    fprintf (fout, "    F_len   %d\n", mp->mp_buffer->f_length);
    fprintf (fout, "    type    %04X\n", mp->mp_buffer->record_type);
    fprintf (fout, "    status  %d\n", mp->mp_buffer->status);
    fprintf (fout,
             "    mp_data  %02X %02X %02X %02X %02X %02X %02X %02X"
             " %02X %02X %02X %02X %02X %02X %02X %02X",
             mp->mp_buffer->packet.mpp.mini_packet[0],
             mp->mp_buffer->packet.mpp.mini_packet[1],
             mp->mp_buffer->packet.mpp.mini_packet[2],
             mp->mp_buffer->packet.mpp.mini_packet[3],
             mp->mp_buffer->packet.mpp.mini_packet[4],
             mp->mp_buffer->packet.mpp.mini_packet[5],
             mp->mp_buffer->packet.mpp.mini_packet[6],
             mp->mp_buffer->packet.mpp.mini_packet[7],
             mp->mp_buffer->packet.mpp.mini_packet[8],
             mp->mp_buffer->packet.mpp.mini_packet[9],
             mp->mp_buffer->packet.mpp.mini_packet[10],
             mp->mp_buffer->packet.mpp.mini_packet[11],
             mp->mp_buffer->packet.mpp.mini_packet[12],
             mp->mp_buffer->packet.mpp.mini_packet[13],
             mp->mp_buffer->packet.mpp.mini_packet[14],
             mp->mp_buffer->packet.mpp.mini_packet[15]);
  }
  if (!(flag & 0x10)) {
    count = (mp->mp_buffer->packet.mpp.mini_packet[0] & 0x0F) << 8;
    count |= (mp->mp_buffer->packet.mpp.mini_packet[1] & 0xFF) << 0;
    count += 3;
    if (count > 15)
      for (i = 16; i < count; i++) {
        if (!(i & 0x0F))
          fprintf (fout, "\n       %04X:", i);
        fprintf (fout, " %02X", mp->mp_buffer->packet.mpp.mini_packet[i]);
      }
  }
  if (flag & 0x08) {
    fprintf (fout, "\n");
  }
  if (flag & 0x20) {
    type = (mp->mp_buffer->packet.mpp.mini_packet[0] >> 4) & 0x0F;
    icnt[type] += 1;
    icnt[16] += 1;
    strcpy (temp, UTIL_extract_MP_packet_type (mp->mp_buffer));
    strcat (temp, "      ");
    temp[5] = 0;
    fprintf (fout, "%6d %s %5d ", icnt[16], temp, icnt[type]);
    fprintf (fout, "%02X%02X%02X%02X.%d.%d ",
             mp->mp_buffer->packet.cds_tag.begin[0],
             mp->mp_buffer->packet.cds_tag.begin[1],
             mp->mp_buffer->packet.cds_tag.begin[2],
             mp->mp_buffer->packet.cds_tag.begin[3],
             mp->mp_buffer->packet.cds_tag.begin[4] >> 5,
             mp->mp_buffer->packet.cds_tag.begin[4] >> 1 & 0x0F);
    fprintf (fout, "(%4d) ", mp->mp_buffer->packet.index.data_start);
    imax = mp->mp_buffer->packet.mpp.mini_packet[0] & 0x0F;
    imax = imax << 8;
    imax = imax | mp->mp_buffer->packet.mpp.mini_packet[1];
    imax = imax & 0x0FFF;

    fprintf (fout, "%02X %03X %02X%02X ",
             type,
             imax,
             mp->mp_buffer->packet.mpp.mini_packet[3],
             mp->mp_buffer->packet.mpp.mini_packet[2]
      );
    fprintf (fout, "%02X %02X %02X %02X %02X %02X %02X %02X %02X %02X ",
             mp->mp_buffer->packet.mpp.mini_packet[4],
             mp->mp_buffer->packet.mpp.mini_packet[5],
             mp->mp_buffer->packet.mpp.mini_packet[6],
             mp->mp_buffer->packet.mpp.mini_packet[7],
             mp->mp_buffer->packet.mpp.mini_packet[8],
             mp->mp_buffer->packet.mpp.mini_packet[9],
             mp->mp_buffer->packet.mpp.mini_packet[10],
             mp->mp_buffer->packet.mpp.mini_packet[11],
             mp->mp_buffer->packet.mpp.mini_packet[12],
             mp->mp_buffer->packet.mpp.mini_packet[13]
      );
    if (flag & 0x0200)
      for (i = 14; i < imax + 3; i++) {
        if (!(i & 31))
          fprintf (fout, "\n"
                   "                " "          %5d  " "%4X: ", i, i);
        fprintf (fout, "%02X ", mp->mp_buffer->packet.mpp.mini_packet[i]);
      }

    if (flag & 0x0400)
      for (i = 0; i < imax + 3; i++) {
        if (!(i & 31))
          fprintf (fout, "\n"
                   "                " "         %5d  " "%4X: ", i, i);
        fprintf (fout, "%02X ",
                 mp->data_buffer->cds_record.packet.cds.header[i]);
      }

    if (flag & 0x0100)
      fprintf (fout, " Not Written");
    fprintf (fout, "\n");
  }




  if (flag & 0x40) {
    fprintf (fout,
             "%02X%02X %02X%02X %02X%02X %02X %8X.%d",
             mp->data_buffer->cds_record.packet.cds.header[1],
             mp->data_buffer->cds_record.packet.cds.header[0],
             mp->data_buffer->cds_record.packet.cds.header[3] & 0x3F,
             mp->data_buffer->cds_record.packet.cds.header[2],
             mp->data_buffer->cds_record.packet.cds.header[5],
             mp->data_buffer->cds_record.packet.cds.header[4],
             mp->data_buffer->cds_record.packet.cds.header[7],
             mp->data_buffer->sclk, (mp->data_buffer->rti >> 5) & 0x7);
    fprintf (fout,
             " %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X"
             " %02X %02X %02X %02X %02X",
             mp->data_buffer->cds_record.packet.cds.header[12],
             mp->data_buffer->cds_record.packet.cds.header[13],
             mp->data_buffer->cds_record.packet.cds.header[14],
             mp->data_buffer->cds_record.packet.cds.header[15],
             mp->data_buffer->cds_record.packet.cds.header[16],
             mp->data_buffer->cds_record.packet.cds.header[17],
             mp->data_buffer->cds_record.packet.cds.header[18],
             mp->data_buffer->cds_record.packet.cds.header[19],
             mp->data_buffer->cds_record.packet.cds.header[20],
             mp->data_buffer->cds_record.packet.cds.header[21],
             mp->data_buffer->cds_record.packet.cds.header[22],
             mp->data_buffer->cds_record.packet.cds.header[23],
             mp->data_buffer->cds_record.packet.cds.header[24],
             mp->data_buffer->cds_record.packet.cds.header[25],
             mp->data_buffer->cds_record.packet.cds.header[26],
             mp->data_buffer->cds_record.packet.cds.header[27]
      );
    fprintf (fout, "\n");
  }

  fflush (fout);
}

 /*
  * 
  */

 /*******************************************************
  *	Is this a valid MP ident ?!?
  *	1st. argument is the 1st. byte of a minipacket
  *		(contains M.P. Identification bits)
  *	2nd argument 0, 1 or 2
  *		0 or 1 returns type field, shifted down
  *		2 returns non-zero for valid packet types
  *******************************************************/
int MP2_mp_type_valid (unsigned char type, int itm)
{
  int item = 1;
  static int type_array[][3] = { PACKET_TYPE_stim, DATA_MPP_STIM, 2,
    PACKET_TYPE_mfr, DATA_MPP_MFR, 1,
    PACKET_TYPE_hfr, DATA_MPP_HFR, 1,
    PACKET_TYPE_lp, DATA_MPP_LP, 1,
    PACKET_TYPE_lfdr, DATA_MPP_LFDR, 1,
    PACKET_TYPE_wfr, DATA_MPP_WFR, 1,
    PACKET_TYPE_dust, DATA_MPP_DUST, 1,
    PACKET_TYPE_mro, DATA_MPP_MRO, 1,
    PACKET_TYPE_wbr, DATA_MPP_WBR, 1,
    PACKET_TYPE_fill, DATA_MP_packet, 1,
    PACKET_TYPE_invalid, DATA_MP_packet, 0
  };
  int i = 0;
  int status = 0;

  if (itm > 0)
    if (itm < 3)
      item = itm;
  type = (type & 0xF0) >> 4;
  while (type_array[i][0] != PACKET_TYPE_invalid) {
    if (type == type_array[i][0]) {
      status = type_array[i][item];
      break;
    }
    i += 1;
  }
  return status;
}

 /*
  * 
  */

 /*******************************************************
  *  Check STIM packets.
  *	1st. arg	science stream data structure
  *	return ZERO if bad data
  *
  *	Stim packets look very similar to fill data
  *	  end of CDS record may contain several zero bytes
  *	  M.P. ID for stim is ZERO so check length to see
  *	  if this is fill or STIM...
  *******************************************************/
int MP2_mp_stim_check (struct MP *mp)
{
  unsigned int len;

  len = (mp->mp_buffer->packet.mpp.mini_packet[0] & 0x0F) << 8;
  len |= mp->mp_buffer->packet.mpp.mini_packet[1];
  if (MP2_mp_type_valid (mp->mp_buffer->packet.mpp.mini_packet[0], 0) ==
      DATA_MPP_STIM)
    if (len != 0x0D)
      return 0;
  return 1;
}

 /*
  * 
  */

 /*******************************************************
  *  ---- July 2000 ---- WTR
  *  See if the packet is appropriate for the stream we're
  *	writing to
  *	1st. arg	science stream data structure
  *	return ZERO if bad data
  *
  *******************************************************/
int MP2_mp_valid (struct MP *mp)
{
  unsigned int mptype;
  unsigned int mpbit;

  mptype = (mp->mp_buffer->packet.mpp.mini_packet[0] >> 4) % 0x0F;
  mpbit = 1 << mptype;

  if (mp->valid_mask & mpbit)
    return 1;
  return 0;
}

 /*
  * 
  */

 /*******************************************************
  *  implement the "-filter" capability
  *  1st. argument is minipacket data structure
  *
  *	See if filter is turned on and return
  *	GO / NO-GO 
  *******************************************************/
int MP2_filter (struct MP_buffer *mp_buffer)
{
  int temp;
  int temp_mask;
  int status = 0;

  if (item_mask == -1)
    status |= 1;
  temp = (mp_buffer->packet.mpp.mini_packet[0] & 0xF0) >> 4;
  temp_mask = 1 << temp;
  if (item_mask & temp_mask)
    status |= 1;
  return status;
}

 /*
  * 
  */

 /*******************************************************
  *	Check CDS packet length				*
  *******************************************************/
int MP2_mp_fill (struct MP *mp, char stream)
{
  mp->mp_buffer->packet.stream.CDS_fill_flag |= stream;

  if (mp->mp_buffer->packet.chdo_ancillary.type_94.non_fill_length != 952)
    mp->mp_buffer->packet.stream.CDS_fill_flag |= CDS_FILL_FLAG_1;

  if (mp->data_buffer->cds_record.packet.chdo_ancillary.type_94.
      non_fill_length != 952)
    mp->mp_buffer->packet.stream.CDS_fill_flag |= CDS_FILL_FLAG_2;

  return 0;
}

 /*******************************************************
  *	Move the header fields from cds to mp		*
  *	  2nd. routine moves special fields from	*
  *	  successive cds records (i.e. cds time, etc)	*
  *	1st. arg stream data structure			*
  *							*
  *	1st. time, move the CCSDS header time into	*
  *	  a reserved location.  This allows the MP	*
  *	  to be accurately time-tagged later		*
  *	each additional ccsds record used to build	*
  *	  this a minpacket will have it's time tag	*
  *	  moved to the 2nd. postion.  Thius will make	*
  *	  the 1st. and last time tag available to the	*
  *	  user.						*
  *******************************************************/
int MP2_mp_head (struct MP *mp)
{
  char stream;

  stream = mp->mp_buffer->packet.stream.CDS_fill_flag;
  memcpy (mp->mp_buffer->packet.header.data,
          mp->data_buffer->cds_record.packet.header.data, 256);

  mp->mp_buffer->record_type = DATA_MP_packet;
  mp->mp_buffer->packet.cds_tag.begin[0] =
    mp->data_buffer->cds_record.packet.cds.header[6];
  mp->mp_buffer->packet.cds_tag.begin[1] =
    mp->data_buffer->cds_record.packet.cds.header[9];
  mp->mp_buffer->packet.cds_tag.begin[2] =
    mp->data_buffer->cds_record.packet.cds.header[8];
  mp->mp_buffer->packet.cds_tag.begin[3] =
    mp->data_buffer->cds_record.packet.cds.header[11];
  mp->mp_buffer->packet.cds_tag.begin[4] =
    mp->data_buffer->cds_record.packet.cds.header[10];
  mp->mp_buffer->packet.cds_tag.begin[5] = 0x0;
  mp->mp_buffer->packet.cds_tag.begin[6] = 0x0;
  mp->mp_buffer->packet.cds_tag.begin[7] = 0x0;
  mp->mp_buffer->packet.cds_tag.sequence[0] = 0;
  mp->mp_buffer->packet.cds_tag.sequence[1] = 0;
  mp->mp_buffer->packet.cds_tag.sequence[2] =
    mp->data_buffer->cds_record.packet.cds.header[3] & 0x3F;
  mp->mp_buffer->packet.cds_tag.sequence[3] =
    mp->data_buffer->cds_record.packet.cds.header[2];
  MP2_mp_fill (mp, stream);
}
int MP2_mp_head2 (struct MP *mp)
{
  char stream;

  stream = mp->mp_buffer->packet.stream.CDS_fill_flag;
  mp->mp_buffer->packet.cds_tag.end[0] =
    mp->data_buffer->cds_record.packet.cds.header[6];
  mp->mp_buffer->packet.cds_tag.end[1] =
    mp->data_buffer->cds_record.packet.cds.header[9];
  mp->mp_buffer->packet.cds_tag.end[2] =
    mp->data_buffer->cds_record.packet.cds.header[8];
  mp->mp_buffer->packet.cds_tag.end[3] =
    mp->data_buffer->cds_record.packet.cds.header[11];
  mp->mp_buffer->packet.cds_tag.end[4] =
    mp->data_buffer->cds_record.packet.cds.header[10];
  MP2_mp_fill (mp, stream);
}

 /*
  * 
  */

 /*******************************************************
  * check to see if this is a good ccsds packet
  *	The ccsds-ID check is redundant... Add something later ?
  *	NOT 0 -> good
  *	0     -> bad
  *******************************************************/
unsigned short valid_ccsds[] = { 0x0A81,        /* Low Rate  UNsegmented */
  0x0A82,                               /* High Rate UNsegmented */
  0x0AA0,                               /* Low Rate  Segmented   */
  0x0AA3,                               /* High Rate Segmented   */
  0x0A90,                               /* ROM Housekeeping      */
  0x0A93,                               /* Maintenance/Deploy    */
  0x0A95,                               /* Science Housekeeping  */
  0x0000
};                                      /* end marker            */
int MP2_mp_validate (struct MP *mp, int flag)
{
  int i = 0;
  unsigned short temp[2];

  while (valid_ccsds[i]) {
    temp[0] = mp->data_buffer->cds_record.packet.cds.header[0] << 0 |
      mp->data_buffer->cds_record.packet.cds.header[1] << 8;
    if (temp[0] == valid_ccsds[i])
      break;
    i += 1;
  }
  if (!valid_ccsds[i])

    return 0;                           /******************/
  if (!flag)

    return valid_ccsds[i];              /******************/

                 /*******************************************************
		  * check to see if this is a proper continuation	*
		  *  of a minipacket.  The ID's should match		*
		  *******************************************************/
  temp[0] = mp->data_buffer->cds_record.packet.cds.mp_data[0] & 0xF0;
  temp[1] = mp->mp_buffer->packet.mpp.mini_packet[0] & 0xF0;
  if (temp[0] != temp[1])

    return 0;                           /******************/

  return valid_ccsds[i];                /******************/
}

 /*
  * 
  */

 /*******************************************************
  * calculate the number of data bytes to suck out of 
  * the ccsds record
  *
  *	1st arg		stream data structure
  *	2nd arg		offset into cds record
  *	3rd arg		how to count...
  *******************************************************/
int MP2_mp_count (struct MP *mp, int index, int sw)
{
  int count;
  unsigned count_fill;
  unsigned count_cds;
  unsigned count_mp;
  int type;

  count_cds =
    ((mp->data_buffer->cds_record.packet.cds.header[5] & 0x3F) << 8) |
    (mp->data_buffer->cds_record.packet.cds.header[4] & 0xFF) + 7;

  if (mp->data_buffer->cds_record.packet.chdo_ancillary.type_94.chdo_type ==
      94)
    count_fill =
      mp->data_buffer->cds_record.packet.chdo_ancillary.type_94.
      non_fill_length;
  else
    count_fill = 0;

  count_mp =
    (mp->data_buffer->cds_record.packet.rpws.data[index + 0] & 0x0F) << 8;
  count_mp |=
    (mp->data_buffer->cds_record.packet.rpws.data[index + 1] & 0xFF);
  count_mp += 3;

  count_cds -= index;
  count_fill -= index;

  count_cds -= 12;
  count_fill -= 12;

  switch (sw) {
   case COUNT_MOVE_XX:
     count = count_mp;
     if (count_cds < count)
       count = count_cds;
     if (count_fill > 0)
       if (count_fill < count)
         count = count_fill;
     break;
   case COUNT_MOVE:
     count = count_mp;
     if (count_cds < count)
       count = count_cds;
     break;
   case COUNT_REMAIN:
     count = count_cds;
     if (count_fill > 0)
       if (count_fill < count)
         count = count_fill;
     break;
   case COUNT_MP:
     count = count_mp;
     break;
   case COUNT_CDS:
     count = count_cds;
     break;
   case COUNT_FILL:
     count = count_fill;
     break;
  }

  return count;
}

 /*
  * 
  */

 /*******************************************************
  *	Move data from cds to mp			*
  *	  index indicates where in cds to start		*
  *	"index" is the position, within the cds record	*
  *	    to start collecting data			*
  *	"mp->mindex" is the position, within the 	*
  *	    minipacket to start placing the data	*
  *	After the move, update "mp->count" and return	*
  *	  the new index					*
  *
  *	What can happen?
  *	    1.	M/P finishes
  *			move data
  *			write M/P to stdout
  *	    2.	M/P zero filled
  *			move data
  *			write M/P to stdout
  *	    3.	M/P incomplete
  *			move data
  *******************************************************/
int MP2_mp_move (struct MP *mp,         /* minipacket */
                 int index,             /* position in CDS record */
                 int skip,              /* Segmented / Unsegmentd */
                 int shit)
{                                       /* debugging assist */
  int count = 0;
  int type;

  MP2_mp_fill (mp, 0);
  count = MP2_mp_count (mp, index, COUNT_MOVE);
  memcpy (&mp->mp_buffer->packet.mpp.mini_packet[mp->mindex - mp->flag_2],
          &mp->data_buffer->cds_record.packet.rpws.data[index + skip],
          count - skip);
  /*
   *        Mess with mp->mp_buffer->packet.index.data_length here ???
   */

#ifdef DATA_LENGTH
  mp->mp_buffer->packet.index.data_length += count - skip;      /* reflect actual count */
#endif

  if (debug)
    fprintf (debug,
             "mp_move%d  memcpy(%X[%4d], %X[%4d], %4d);  index(%4d)  skip(%4d) flag_2(%d)\n",
             shit, &mp->mp_buffer->packet.mpp.mini_packet[0], mp->mindex,
             &mp->data_buffer->cds_record.packet.rpws.data[0], index + skip,
             count - skip, index, skip, mp->flag_2);
  mp->mp_buffer->packet.index.data_start += count;
  mp->mp_buffer->packet.index.data_start -= mp->mindex - mp->flag_2;    /* 7/18/2000 WTR */
  mp->mindex += count;                  /* move pointer up */
  count += index;                       /* move othr pntr  */
  if (MP2_mp_count (mp, count, COUNT_REMAIN) <= 0) {
    count = -1;
  }
  return count;
}

 /*
  * 
  */

 /*******************************************************
  *	Scan to eliminate a fragment of a minipacket	*
  *	  just like a data move without the move	*
  *	  just update the pointers/length 		*
  *******************************************************/
int MP2_mp_scan (struct MP *mp)
{
  int count, type;
  int index = 0;

  type =
    MP2_mp_type_valid (mp->data_buffer->cds_record.packet.rpws.data[0], 0);
  count = MP2_mp_count (mp, index, COUNT_MOVE);
  if (diag)
    fprintf (diag, "Discard ");
  MP2_MP_dump (mp, diag, 0x450, 0);
  if (type)
    return count;
  else
    return -1;
}

 /*
  * 
  */

 /*******************************************************
  *	Clear the minipacket data structure		*
  *	zero the minipacket data structure in
  *	  order to accomodate partial movements...
  *******************************************************/
void MP2_mp_clear (struct MP *mp)
{
  memset (mp->mp_buffer, 0, sizeof (struct MP_buffer));
  memcpy (mp->mp_buffer->packet.header.data,
          mp->data_buffer->cds_record.packet.header.data, 256);
  mp->mindex = 0;
  return;
}

 /*
  * 
  */

 /*******************************************************
  *	Write the MP data to the output stream		*
  *	The length variable indicates when data is 	*
  *	  present in the MP data structure		*
  *******************************************************/
void MP2_flush (FILE * mpout, struct MP *mp)
{
  int length = 0;
  int rlength = 0;
  int flag;

                        /************************************************
			 *	mp->mp_buffer->packet.index.data_start
			 *	  is the number of good data bytes
			 *	mp->mp_buffer->packet.mpp.mini_packet[0]
			 *	  is the minpacket size and should be
			 *	  used to generate the output record
			 *	  OR the r_length field will be located
			 *	  in the zero-fill area!!!
			 ************************************************/
  length = (mp->mp_buffer->packet.mpp.mini_packet[0] & 0x0F) << 8;
  length |= (mp->mp_buffer->packet.mpp.mini_packet[1] & 0xFF) << 0;
  length += 3;

                        /************************************************
			 *	Adjust back to M.P. count (i.e. 3
			 *	  less than actual
			 *	Time tage the MP activity
			 ************************************************/
  mp->mp_buffer->packet.index.data_start -= 3;  /* this way seems screwed  */
  mp->mp_buffer->packet.index.data_start = length - 3;  /* 7/18/2000 WTR           */

#ifndef DATA_LENGTH
  mp->mp_buffer->packet.index.data_length =     /* eliminate this to       */
    mp->mp_buffer->packet.index.data_start;     /*  count actual bytes ??? */
#endif

  UTIL_get_time (&(mp->mp_buffer->packet.ws_tag.B));

                        /************************************************
			 *	Overhead				*
			 *	  12 bytes length/type/status
			 *	  256 header bytes
			 *	   4 for good measure
			 ************************************************/
  if (length < Minimum_Buffer_Size)
    rlength = Minimum_Buffer_Size + 272;
  else
    rlength = length + 272;

  if (length > 3) {

                        /************************************************
			 *	Pad out to make MP break on		*
			 *	  16 byte boundary			*
			 *	MUCH easier to examine in dump that way	*
			 ************************************************/
    MP2_mp_head2 (mp);
    rlength += 15;
    rlength += Minimum_Buffer_Pad;
    rlength &= 0x0000FFF0;
    rlength -= 4;

                        /************************************************
			 *	Update the MP type field		*
			 ************************************************/
    mp->mp_buffer->record_type =
      MP2_mp_type_valid (mp->mp_buffer->packet.mpp.mini_packet[0], 0);

                        /************************************************
			 *	Might even think about writing		*
			 *	  the record at this point		*
			 ************************************************/
    flag = 0x330;
    if (MP2_mp_valid (mp))              /* appropraite for HRS/LRS ??? */
      if (MP2_mp_stim_check (mp))       /* dumb-ass stim (i.e. zero fill) */
        if (MP2_filter (mp->mp_buffer)) {       /* apply filter */
          UTIL_putbuffr2_MP (mp->mp_buffer, mpout, rlength);
          flag = 0x230;
        }
    MP2_MP_dump (mp, debug, flag, 0);
  }
  mp->mp_buffer->packet.index.data_start = 0;
  MP2_mp_clear (mp);
  MP2_mp_head (mp);
  return;
}

 /*
  * 
  */

 /*******************************************************
  *	Using the MP data structure, process the CDS
  *	    data and generate minipackets as required
  *	THREE PROCESSING CASES:
  *		1. Unsegmented
  *			Flush any existing M.P.
  *			and begin new
  *		2. Segmented, sequence indicates no
  *				lost CDS records
  *		3. Segmented, sequence indicates a
  *				lost CDS record
  *******************************************************/
void MP2_process (FILE * mpout, struct MP *mp)
{
  int busy = 1;
  int length = 0;
  int cindex = 0;                       /*index into CDS record */
  unsigned new_sequence;
  unsigned old_sequence;


                        /************************************************
			 *	Check for zero fill & mark it here	*
			 ************************************************/
  if (mp->data_buffer->cds_record.packet.chdo_ancillary.type_94.
      non_fill_length != 952)
    mp->mp_buffer->packet.stream.CDS_fill_flag |= CDS_FILL_FLAG;

                        /************************************************
			 *	Perform sequence verification		*
			 *	UnSegmented is easy, flush old MP and	*
			 *	  begin processing.			*
			 *	Segmented needs an extra check		*
			 *	  IF the sequence is broken, we		*
			 *	  will simply scan the 1st. partial	*
			 *	  and discard it before passing on	* 
			 *	  good MP DATA				*
			 ************************************************/
  old_sequence = mp->sequence + 1;
  old_sequence &= 0x3FFF;               /* fold back */
  new_sequence = UTIL_extract_CDS_sequence (&mp->data_buffer->cds_record);
  new_sequence &= 0x3FFF;

  /*
   * if(!new_sequence)                    /* accomodate roll 
   */
  /*
   * new_sequence |= 0x4000;              /* xxx             
   */



                        /************************************************
			 *	Here is the sequence check!		*
			 ************************************************/

  mp->flag_1 = STEP_Segmented;
  if (old_sequence != new_sequence) {
    mp->mp_buffer->packet.stream.CDS_fill_flag |= CDS_SEQUENCE_FLAG;
    mp->flag_1 = STEP_Segmented_missing;
    if (diag)
      fprintf (diag, "Gap %05d/%05d   ", old_sequence, new_sequence);
    MP2_MP_dump (mp, diag, 0x450, 0);
  }

  if ((UTIL_extract_CDS_type (&mp->data_buffer->cds_record) & SEG_BIT) == 0x0)
    mp->flag_1 = STEP_Unsegmented;

                        /************************************************
			 *	Process all the stuff in this CCSDS	*
			 *	  block, returning when more data	*
			 *	  is required				*
			 ************************************************/
  while (busy) {
    switch (mp->flag_1) {
     case STEP_Unsegmented:
       MP2_flush (mpout, mp);           /*flush existng data */
       if (!MP2_mp_validate (mp, 0))
         return;
       mp->flag_2 = 0;                  /* move up a line 12/2001 */
       cindex = MP2_mp_move (mp, cindex, 0, 0); /* move this packet */
       mp->flag_2 = 0;                  /* */
       if (cindex < 0) {                /* need more data   */
         mp->sequence = new_sequence & 0x3FFF;
         return;
       }
       mp->flag_1 = STEP_Continue;      /* indicate 'cry on' */
       goto end_switch;                 /* with next minipkt */

     case STEP_Segmented:
       if (!MP2_mp_validate (mp, 1))
         return;                        /* cont, from   */
       cindex = MP2_mp_move (mp, cindex, 2, 1); /* prev minipacket, */
       mp->flag_2 = 2;
       if (cindex < 0) {                /* keep rcvrng data */
         mp->sequence = new_sequence & 0x3FFF;
         return;
       }
       mp->flag_1 = STEP_Continue;      /* indicate 'cry on' */
       goto end_switch;                 /* with next minipkt */

     case STEP_Segmented_missing:
       MP2_flush (mpout, mp);           /* flush existg data */
       if (!MP2_mp_validate (mp, 0))
         return;
       mp->flag_2 = 0;
       cindex = MP2_mp_scan (mp);       /* eliminate lost   */
       MP2_mp_head (mp);
       mp->flag_1 = STEP_Segmented;     /* minipacket       */
       if (cindex < 0) {
         mp->sequence = new_sequence & 0x3FFF;
         return;
       }
       mp->flag_1 = STEP_Continue;      /* indicate 'cry on' */
       goto end_switch;                 /* with next minipkt */

     case STEP_Continue:               /* NEXT MiniPacket  */
       if (!MP2_mp_validate (mp, 0))
         return;
       /*
        * scn for FILL @ end
        */
       if (!(mp->data_buffer->cds_record.packet.rpws.data[cindex + 0] |
             mp->data_buffer->cds_record.packet.rpws.data[cindex + 1]))
         return;

       mp->flag_2 = 0;
       cindex = MP2_mp_move (mp, cindex, 0, 2); /* move data        */
       if (cindex < 0) {                /* keep recvrng data */
         mp->sequence = new_sequence & 0x3FFF;
         return;
       }

       mp->flag_1 = STEP_Continue_Again;        /* indict 'carry on' */
       goto end_switch;                 /* with next minipkt */

     case STEP_Continue_Again:         /* NEXT MiniPacket  */
       if (!MP2_mp_validate (mp, 0))
         return;
       /*
        * scn for FILL @ end
        */
       if (!(mp->data_buffer->cds_record.packet.rpws.data[cindex + 0] |
             mp->data_buffer->cds_record.packet.rpws.data[cindex + 1]))
         return;

       mp->flag_2 = 0;
       cindex = MP2_mp_move (mp, cindex, 0, 3); /* move data        */
       if (cindex < 0) {                /* keep recvrng data */
         mp->sequence = new_sequence & 0x3FFF;
         return;
       }

       mp->flag_1 = STEP_Continue_Again;        /* indict 'carry on' */
       goto end_switch;                 /* with next minipkt */

     default:
       mp->sequence = new_sequence & 0x3FFF;
       return;
    }
  end_switch:
    MP2_flush (mpout, mp);
  }
  mp->sequence = new_sequence & 0x3FFF;
  return;
}

 /*
  * 
  */

 /*******************************************************
  *	Allocate memory for a minipacket buffer		*
  *******************************************************/
struct MP_buffer *MP2_allocate (struct MP *mp)
{
  mp->mp_buffer =
    (struct MP_buffer *) malloc (sizeof (struct MP_buffer) + 2048);
  return mp->mp_buffer;
}

 /*
  * 
  */

 /*******************************************************
  *	Count the number of elements in a queue		*
  *	  pass in the queue head			*
  *******************************************************/
int MP2_scan (struct DATA_RECORD *link)
{
  int count = 0;

  while (link->forward) {
    count += 1;
    link = link->forward;
  }
  return count;
}

 /*
  * 
  */

 /*******************************************************
  *	This entry generates minipackets from the	*
  *	  queue "head" until 8 packets remain in the	*
  *	  queue.  No packets are generated if less than	* 
  *	  8 elements remain				*
  *******************************************************/
int MP2_generate_mp (FILE * mpout,
                     int count,
                     int minimum,
                     int mask,
                     char *queue_name,
                     char *free_name, FILE * debug_file, FILE * diag_file)
{
  struct DATA_RECORD *temp;
  struct DATA_RECORD *head;

  if (!debug)
    debug = debug_file;
  if (!diag)
    diag = diag_file;
  item_mask = mask;
  head = mpii_find_head (queue_name);

  if (first_time) {
    for (first_time = 1; first_time <= MP_THREADS; first_time++)
      MP2_allocate (&mp[first_time]);
    first_time = 0;
  }

  while (MP2_scan (head) > minimum) {
    mp[head->type].data_buffer = mpii_dequeue (queue_name, NULL, HEAD);
    MP2_process (mpout, &mp[head->type]);
    mpii_enqueue (free_name, mp[head->type].data_buffer, HEAD);
  }
  return count;
}
