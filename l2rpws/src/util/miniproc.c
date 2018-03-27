
/* miniproc - minipacket processing */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define _miniproc_h
#include <rtiu.h>
#include <util.h>
#include <utilmp.h>



#define  UNSEG_LOW_RATE     0x01
#define  UNSEG_HIGH_RATE    0x02
#define  SEG_LOW_RATE       0x20
#define  SEG_HIGH_RATE      0x23

/* the table of all the valid data source id's */

/* when anything new is added, add it to the source id switch further on */

#define  STIM		  0x0
#define  MFR		  0x1
#define  HFR              0x2
#define  Langmuir_Probe   0x4
#define  LFDR 		  0x7
#define  WFR              0x8
#define  DUST 		  0xB
#define  MRO 		  0xD
#define  WBR              0xE
#define  FILL             0xF

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

union mp_header
{
  unsigned char id;
  unsigned char length[2];
};

/* */
static char ident[] = { "IDENT miniproc.c 16-June-2000" };

 /*
  * Version string 
  */
char UTIL_get_mp_version[] = { "miniproc 2.1" };
static struct CDS_buffer misc_buffer;
static int util_misc_move_save (struct CDS_buffer *buffer)
{
  int i;

  for (i = 0; i < 256; i++) {
    misc_buffer.packet.header.data[i] = buffer->packet.header.data[i];
  }
  return 0;
}
static int util_misc_move_update (struct MP_buffer *user_variable)
{
  int j;

  user_variable->packet.chdo_tag.sclk.seconds =
    misc_buffer.packet.chdo_tag.sclk.seconds;
  user_variable->packet.chdo_tag.sclk.fine =
    misc_buffer.packet.chdo_tag.sclk.fine;
  user_variable->packet.chdo_tag.scet.days =
    misc_buffer.packet.chdo_tag.scet.days;
  user_variable->packet.chdo_tag.scet.milliseconds =
    misc_buffer.packet.chdo_tag.scet.milliseconds;
  user_variable->packet.chdo_tag.ert.days =
    misc_buffer.packet.chdo_tag.ert.days;
  user_variable->packet.chdo_tag.ert.milliseconds =
    misc_buffer.packet.chdo_tag.ert.milliseconds;
  user_variable->packet.chdo_tag.rct.days =
    misc_buffer.packet.chdo_tag.rct.days;
  user_variable->packet.chdo_tag.rct.milliseconds =
    misc_buffer.packet.chdo_tag.rct.milliseconds;

  memcpy (&user_variable->packet.chdo_ancillary.type_92,
          &misc_buffer.packet.chdo_ancillary.type_92,
          sizeof (struct CHDO_type_92));
  memcpy (&user_variable->packet.chdo_ancillary.type_94,
          &misc_buffer.packet.chdo_ancillary.type_94,
          sizeof (struct CHDO_type_94));

  for (j = 0; j < 4; j++) {
    user_variable->packet.cds_tag.epoch[j] =
      misc_buffer.packet.cds_tag.epoch[j];
    /**/
      user_variable->packet.cds_tag.sequence[j] =
      misc_buffer.packet.cds_tag.sequence[j];
  }

  for (j = 0; j < 2; j++) {
    user_variable->packet.ws_tag.A.year[j] =
      misc_buffer.packet.ws_tag.A.year[j];
    user_variable->packet.ws_tag.A.doy[j] =
      misc_buffer.packet.ws_tag.A.doy[j];
  }
  user_variable->packet.ws_tag.A.hour = misc_buffer.packet.ws_tag.A.hour;
  user_variable->packet.ws_tag.A.minute = misc_buffer.packet.ws_tag.A.minute;
  user_variable->packet.ws_tag.A.second = misc_buffer.packet.ws_tag.A.second;
  for (j = 0; j < 2; j++) {
    user_variable->packet.ws_tag.B.year[j] =
      misc_buffer.packet.ws_tag.B.year[j];
    user_variable->packet.ws_tag.B.doy[j] =
      misc_buffer.packet.ws_tag.B.doy[j];
    user_variable->packet.ws_tag.B.hour = misc_buffer.packet.ws_tag.B.hour;
  }
  user_variable->packet.ws_tag.B.minute = misc_buffer.packet.ws_tag.B.minute;
  user_variable->packet.ws_tag.B.second = misc_buffer.packet.ws_tag.B.second;

  user_variable->packet.compress.method = misc_buffer.packet.compress.method;
  user_variable->packet.compress.bit_count =
    misc_buffer.packet.compress.bit_count;
  user_variable->packet.compress.result = misc_buffer.packet.compress.result;

  return 0;
}
static int util_correct_cds_packet_length (struct CDS_buffer *buffer)
{
  /*
   * true if the cds packet is a valid length, else false 
   */
  /*
   * 12 is the size of the cds header and must be changed if the
   * size of the header is changed, which is highly unlikely 
   */
  int ilen;

  ilen = UTIL_extract_CDS_length (buffer) + 7;
  if (ilen < CDS_PKTMIN)
    return 0;
  if (ilen > CDS_PKTMAX)
    return 0;
  return ilen;
}

static int UTIL_valid_CDS_subsystem_id (struct CDS_buffer *buffer)
{
  /*
   * returns true if the subsystem id is valid, else false 
   */
  return ((buffer->packet.cds.header[1] == 0x0A) &&
          ((buffer->packet.cds.header[0] & 0x80) == 0x80));
}

#define big_endian

/* take an array of bytes and convert it to an array of 12-bit data */
static int UTIL_byte_to_12_bit (unsigned char *byte_buffer,
                                unsigned short *twelve_bit_buffer,
                                int byte_length)
{
  int i, j = 0;

  for (i = 0; i + 1 < byte_length;) {
    if (j % 2) {

#ifdef big_endian
      twelve_bit_buffer[j] = ((unsigned short) byte_buffer[i] & 0x0F) << 8
        + (unsigned short) byte_buffer[i + 1];
#else
      twelve_bit_buffer[j] = ((unsigned short) byte_buffer[i + 1] & 0x0F) << 8
        + (unsigned short) byte_buffer[i];
#endif
    } else {

#ifdef big_endian
      twelve_bit_buffer[j] = (unsigned short) byte_buffer[i] << 4
        + ((unsigned short) byte_buffer[i + 1] & 0xF0) >> 4;
#else
      twelve_bit_buffer[j] = (unsigned short) byte_buffer[i + 1] << 4
        + ((unsigned short) byte_buffer[i] & 0xF0) >> 4;
#endif
    }
    i += 2;
    j++;
  }
  return j;
}

/* status flags */
#define PROCESSING_SEGMENTED_FLAG        0x0008

#define ABORT_CURRENT_PROCESSING 			\
   status_mask[cds_source_number]|=REQUEST_DATA_FLAG;             	\
   status_mask[cds_source_number]&=~PROCESSING_SEGMENTED_FLAG; 	\
   mp_data_counter[cds_source_number]=0;            			\
   mp_offset[cds_source_number]=0;                  			\
   return status_mask[cds_source_number]

static int UTIL_CDS_segmented (struct CDS_buffer *buffer)
{
  /*
   * ^ is a bitwise XOR in c 
   */
  /*
   * true is the cds packet is segmented, else false 
   */
  return (buffer->packet.cds.header[1] & 0x3f) ^ 0x21;
}

/* the following define must be incremented if a new source is added */

/* for example, housekeeping */
#define MAX_NUMBER_OF_CDS_SOURCES 2
 /*
  * this procedure associates a unique integer with a source,
  * * which allows the sources to be treated totally seperately. 
  * * returns 0 for HRS, 1 for LRS, -1 for error
  */
int UTIL_get_CDS_source_number (struct CDS_buffer *buffer)
{
  /*
   * this procedure associates a unique integer with a source,
   * which allows the sources to be treated totally seperately. 
   */
  /*
   * when new sources are added, just add the next integer
   */
  if (UTIL_extract_HRS (buffer))
    return 0;
  if (UTIL_extract_LRS (buffer))
    return 1;
  return -1;                            /* an error */
}



/* the return value is the status mask after modifying it */

/* recover minipackets 
 * returns modified status mask 
 */
int UTIL_get_mp (struct CDS_buffer *cds_buffer /* input buffer */ ,
                 struct MP_buffer *m_buffer /* output buffer */ ,
                 int cds_source_number)
{
  static unsigned int mp_offset[MAX_NUMBER_OF_CDS_SOURCES];
  static int mp_data_counter[MAX_NUMBER_OF_CDS_SOURCES];
  static int status_mask[MAX_NUMBER_OF_CDS_SOURCES];
  union mp_header *m_header;
  static int data_length = 0;
  static int mp_data_length;
  static int i;
  static unsigned short sequence[MAX_NUMBER_OF_CDS_SOURCES];
  static int first_time = 1;
  static unsigned long cds_time;
  static unsigned long epoch;

/* ***************************************************************** */

/* 		TAKE CARE OF ERROR CONDITIONS:                       */

/* ***************************************************************** */


  if (cds_source_number == -1) {
    /*
     * an unrecognized cds source 
     */
    return REQUEST_DATA_FLAG | UNKNOWN_CDS_SOURCE_FLAG;
  }

  if (first_time) {
    first_time = 0;
  } else {
    if ((sequence[cds_source_number] ==
         UTIL_extract_CDS_sequence (cds_buffer))
        && (status_mask[cds_source_number] & REQUEST_DATA_FLAG))
      return REQUEST_DATA_FLAG;

  }
  sequence[cds_source_number] = UTIL_extract_CDS_sequence (cds_buffer);

  /*
   * clear error conditions: 
   */
  status_mask[cds_source_number] &= PROCESSING_SEGMENTED_FLAG;
  data_length = UTIL_extract_CDS_length (cds_buffer) - 5;

  if (!util_correct_cds_packet_length (cds_buffer)) {
    status_mask[cds_source_number] |= INCORRECT_CDS_LENGTH_FLAG;
    ABORT_CURRENT_PROCESSING;
  }

  if (!UTIL_valid_CDS_subsystem_id (cds_buffer)) {
    status_mask[cds_source_number] |= INCORRECT_SUBSYSTEM_ID_FLAG;
    ABORT_CURRENT_PROCESSING;
  }
  switch (UTIL_extract_CDS_type (cds_buffer)) {
   case SEG_LOW_RATE:
   case SEG_HIGH_RATE:
   case UNSEG_LOW_RATE:
   case UNSEG_HIGH_RATE:
     break;
   default:
     /*
      * if the packet type is unknown,abort 
      */
     status_mask[cds_source_number] |= INVALID_CDS_PACKET_TYPE_FLAG;
     ABORT_CURRENT_PROCESSING;
  }

  if ((UTIL_CDS_segmented (cds_buffer) &&
       (mp_offset[cds_source_number] == 0)) &&
      !(status_mask[cds_source_number] & PROCESSING_SEGMENTED_FLAG)) {
    status_mask[cds_source_number] = DATA_LOST_FLAG;

    /*
     * skip to the next mini-packet: 
     */
    if (mp_offset[cds_source_number] < data_length) {
      m_header = (union mp_header *) (cds_buffer->packet.rpws.data +
                                      mp_offset[cds_source_number]);
      mp_offset[cds_source_number] += 2;
    } else {
      mp_offset[cds_source_number] = 0;
      status_mask[cds_source_number] |= REQUEST_DATA_FLAG;
      return status_mask[cds_source_number];
    }
    mp_data_length = ((unsigned short) m_header->length[1] +
                      (unsigned short) (m_header->length[0] & 0x0F) * 256 +
                      1);
    mp_offset[cds_source_number] += mp_data_length;
  }

/* ***************************************************************** */

/*                  process the mini-packets:                        */

/* ***************************************************************** */

  if (mp_offset[cds_source_number] < data_length) {
    m_header = (union mp_header *) (cds_buffer->packet.rpws.data +
                                    mp_offset[cds_source_number]);
    mp_offset[cds_source_number] += 2;
  } else {
    mp_offset[cds_source_number] = 0;
    status_mask[cds_source_number] |= REQUEST_DATA_FLAG;
    return status_mask[cds_source_number];
  }

  mp_data_length = ((unsigned short) m_header->length[1] +
                    (unsigned short) (m_header->length[0] & 0x0F) * 256 + 1);

  /*
   * check for valid data source id in the mp header: 
   */
  switch ((m_header->id & 0xF0) >> 4) {
   case MFR:
   case HFR:
   case Langmuir_Probe:
   case WFR:
   case WBR:
   case MRO:
   case FILL:
   case LFDR:
   case DUST:
   case STIM:
     break;
   default:
     status_mask[cds_source_number] |= INVALID_SOURCE_ID_FLAG;
     ABORT_CURRENT_PROCESSING;
  }

  if (mp_data_counter[cds_source_number] == 0) {
    /*
     * add the 2 byte mini-packet header: 
     */
    m_buffer->packet.mpx.mini_packet[0]
      = cds_buffer->packet.rpws.data[mp_offset[cds_source_number] - 2];
    m_buffer->packet.mpx.mini_packet[1]
      = cds_buffer->packet.rpws.data[mp_offset[cds_source_number] - 1];

    /*
     * add the internal record type flag 
     */
    m_buffer->record_type = m_buffer->packet.mpx.mini_packet[0] >> 4;
    m_buffer->record_type = m_buffer->record_type + DATA_MP_packet;
    /*
     * flip the record_type field into MSB order 
     */
    if (m_buffer->record_type == DATA_MP_packet)
      m_buffer->record_type = UTIL_long_to_MSB (DATA_MPP_STIM);
    else
      m_buffer->record_type = UTIL_long_to_MSB (m_buffer->record_type);

/* first cds_packet */
    cds_time = UTIL_extract_TIME (cds_buffer);
    util_misc_move_save (cds_buffer);
    epoch = UTIL_extract_EPOCH (cds_buffer);

/*    cds_time &= 0xffffffff;  */

/* fprintf(stderr,"cds_time %X\n", cds_time);   */
    m_buffer->packet.cds_tag.begin[3] = (cds_time >> 0) & 0xff;
    m_buffer->packet.cds_tag.begin[2] = (cds_time >> 8) & 0xff;
    m_buffer->packet.cds_tag.begin[1] = (cds_time >> 16) & 0xff;
    m_buffer->packet.cds_tag.begin[0] = (cds_time >> 24) & 0xff;
    m_buffer->packet.cds_tag.begin[4] =
      UTIL_extract_CDS_RTI (cds_buffer) & 0xFF;
    m_buffer->packet.cds_tag.begin[5] = 0x0;
    m_buffer->packet.cds_tag.begin[6] = 0x0;
    m_buffer->packet.cds_tag.begin[7] = 0x0;
    util_misc_move_update (m_buffer);
  }

  /*
   * move the mp data from the cds packet into the minipacket 
   */
  for (i = 0; i < mp_data_length &&
       mp_offset[cds_source_number] < data_length; i++) {
    m_buffer->packet.mpx.mini_packet[mp_data_counter[cds_source_number] + 2]
      = cds_buffer->packet.rpws.data[mp_offset[cds_source_number]];
    mp_offset[cds_source_number]++;
    mp_data_counter[cds_source_number]++;
  }

  if (i == mp_data_length) {
    status_mask[cds_source_number] |= MP_PACKET_COMPLETED_FLAG;
    mp_data_counter[cds_source_number] = 0;

/* last cds_packet */
    cds_time = UTIL_extract_TIME (cds_buffer);

/* fprintf(stderr,"cds_time2 %X\n", cds_time);  */

/*   cds_time &= 0xffffffff; */

    m_buffer->packet.cds_tag.end[3] = (cds_time >> 0) & 0xff;
    m_buffer->packet.cds_tag.end[2] = (cds_time >> 8) & 0xff;
    m_buffer->packet.cds_tag.end[1] = (cds_time >> 16) & 0xff;
    m_buffer->packet.cds_tag.end[0] = (cds_time >> 24) & 0xff;
    m_buffer->packet.cds_tag.end[4] =
      UTIL_extract_CDS_RTI (cds_buffer) & 0xFF;
    m_buffer->packet.cds_tag.end[5] = 0x0;
    m_buffer->packet.cds_tag.end[6] = 0x0;
    m_buffer->packet.cds_tag.end[7] = 0x0;
    util_misc_move_update (m_buffer);

    UTIL_get_time (&(m_buffer->packet.ws_tag.B));
    m_buffer->packet.ws_tag.A = cds_buffer->packet.ws_tag.A;


  } else {
    status_mask[cds_source_number] |= PROCESSING_SEGMENTED_FLAG;
  }

  if (mp_offset[cds_source_number] >= data_length) {
    status_mask[cds_source_number] |= REQUEST_DATA_FLAG;
    mp_offset[cds_source_number] = 0;
  }

  /*
   * fill out the rest of the info: 
   */
  m_buffer->f_length = sizeof (struct MP_buffer) - 4;
  m_buffer->r_length = sizeof (struct MP_buffer) - 4;

  /*
   * NEED TO COMPLETE FILLING OUT THE HEADERS AND CHECKING
   * FOR VALID SEGMENTED PACKET SEGMENTS                    
   */

  m_buffer->packet.cds_tag.epoch[0] = (epoch >> 24) & 0xFF;
  /**/ m_buffer->packet.cds_tag.epoch[1] = (epoch >> 16) & 0xFF;
  /**/ m_buffer->packet.cds_tag.epoch[2] = (epoch >> 8) & 0xFF;
  /**/ m_buffer->packet.cds_tag.epoch[3] = (epoch >> 0) & 0xFF;
  /**/
    /*
     * m_buffer->packet.cds_tag.sequence[0] = 0;   
     */
    /*
     * m_buffer->packet.cds_tag.sequence[1] = 0;   
     */
    /*
     * m_buffer->packet.cds_tag.sequence[2] = (sequence[cds_source_number] >> 8) & 0xFF;   
     */
    /*
     * m_buffer->packet.cds_tag.sequence[3] = (sequence[cds_source_number] >> 0) & 0xFF;   
     */
    return status_mask[cds_source_number];
}
