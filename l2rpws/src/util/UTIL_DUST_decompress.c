
/* UTIL_DUST_decompress - dust decompression */
#include <stdlib.h>
#include <stdio.h>
#include <rtiu.h>
#include <string.h>

/* Handles dust data at any point in the minipacket stream.
 * Dust packets are funny looking WBR records of a limited length.
 * Since the WBR records are of a predetermined length, the decompress
 * routine may appear anywhere minipackets occur in the processing
 * stream (mpus does not need to resegment dust data, so there isn't
 * a restriction on where decompression occurs).  
 * 
 * returns status (STATUS_COMPRESS*)
 * STATUS_COMPRESS_not_my_data when non-dust WBR packets occur,
 * STATUS_COMPRESS_success on successful operation,
 * STATUS_COMPRESS_fail when a error occurs.
 */
long UTIL_DUST_decompress (struct MP_buffer *in_buffer /* input buffer */ ,
                           struct RPWS_buffer *r_buffer)
{                                       /* output buffer */
  int i, in, out;
  int h_nibble, l_nibble, tmp;
  unsigned char expand_buf[MP_PKTMAX];

  int inst_type, length, f_length, MSF;
  int new_length;
  FILE *fp_temp;

/**********end declarations*************************/

  f_length = in_buffer->f_length;

  inst_type = (in_buffer->packet.mpx.mini_packet[0] >> 4) & 0xf;

/* if not WBR data */
  if (inst_type != PACKET_TYPE_wbr) {
    return (STATUS_COMPRESS_not_my_data);
  }

 /******************************************************************************
 Decompress data that has been 4-bit packed by the DCP
  /*****************************************************************************/

       /*******************************************************
       In the following code the union 'packet' in the MP_buffer
       structure is assumed to be of type MPMAX.(refer rtiu.h)
       ********************************************************/

       /******** Check if the Data is already decompressed ******/
  if (!(in_buffer->packet.mpx.mini_packet[6] & 0x40)) {
    memcpy (r_buffer, in_buffer, sizeof (struct MP_buffer));
    return (STATUS_COMPRESS_success);
  }



      /******************** Read Length ************************/

  length = ((in_buffer->packet.mpx.mini_packet[0] & 0xf) << 8) |
    (in_buffer->packet.mpx.mini_packet[1] & 0xff);
  MSF = in_buffer->packet.mpx.mini_packet[6] & 0x08;

  if ((MSF == 0) && (length < 6)) {
    printf ("Bad WBR Packet\n");
    return (STATUS_COMPRESS_fail);
  }
  if ((MSF == 0x08) && (length < 8)) {
    printf ("Bad WBR Packet\n");
    return (STATUS_COMPRESS_fail);
  }

  if (MSF == 0)
    out = 8;
  else if (MSF == 0x08)
    out = 10;

      /***********load the status bits directly*****************/
  for (i = 0; i < out; ++i) {
    expand_buf[i] = in_buffer->packet.mpx.mini_packet[i];
  }


     /******************* Clear DUST Compression bit *****************/
  expand_buf[6] = expand_buf[6] & 0xBF;

  length = length + 3;
  for (in = out; in < length; ++in) {
    l_nibble = in_buffer->packet.mpx.mini_packet[in] & 0xf;
    h_nibble = in_buffer->packet.mpx.mini_packet[in] & 0xf0;
    expand_buf[out] = h_nibble;
    expand_buf[out + 1] = l_nibble << 4;
    out++;
    out++;
  }

  new_length = out - 3;  /** # of bytes of data */
  r_buffer->f_length = new_length + 3 + 272 + 15;
  r_buffer->f_length = r_buffer->f_length & 0xFFFFFFF0;
  r_buffer->f_length = r_buffer->f_length - 4;
  r_buffer->packet.index.data_length = new_length;

  /** Plug in the new (length-1) into the minipacket **/

  tmp = (new_length >> 8) & 0xf;
  expand_buf[0] &= 0xf0;
  expand_buf[0] |= tmp;
  expand_buf[1] = new_length & 0xff;
  /*
   * fp_temp=fopen("f_temp","w"); /* 
   */
  for (i = 0; i < (new_length + 3); i++) {
    r_buffer->packet.mpx.mini_packet[i] = expand_buf[i];
  }
  inst_type = (r_buffer->packet.mpx.mini_packet[0] >> 4) & 0xf;

  return (STATUS_COMPRESS_success);

}

 /***********************************************************************/

 /***********************************************************************/
