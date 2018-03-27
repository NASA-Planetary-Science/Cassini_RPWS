
/* UTIL_MFR_decompress - MFR decompression code */
#include <stdlib.h>
#include <stdio.h>
#include <rtiu.h>
#include <string.h>

/* Decompress MFR minipacket
 * MFR records are always small, as such, come down in a single minipacket.
 * This routine will decompress MFR data at any point in the chain after
 * initial minipacket reformatting occurs.
 * returns completion status (STATUS_COMPRESS*) 
 */
long UTIL_MFR_decompress (struct MP_buffer *in_buffer,  /* input buffer */
                          struct RPWS_buffer *r_buffer)
{                                       /* output buffer */
  int i, j = 0, k, in, e_count, out, h_flag, l_flag;
  int accumulator = 0;
  int h_nibble, l_nibble, tmp;
  unsigned char expand_buf[MP_PKTMAX];

  int inst_type, length, f_length;
  int new_length;
  FILE *fp_temp;

/**********end declarations*************************/

  f_length = in_buffer->f_length;

  inst_type = (in_buffer->packet.mpx.mini_packet[0] >> 4) & 0xf;

/* if not MFR data */
  if (inst_type != PACKET_TYPE_mfr) {
    return (STATUS_COMPRESS_not_my_data);
  }

 /******************************************************************************
 Decompress data that has been compressed using the Delta Modulation technique.
 The variable Accumulator represents the hardware accumulator in a Delta Demodulator.   
 /*****************************************************************************/

       /*******************************************************
       In the following code the union 'packet' in the MP_buffer
       structure is assumed to be of type MPMAX.(refer rtiu.h)
       ********************************************************/

       /******** Check if the Data is already decompressed ******/
  if (!(in_buffer->packet.mpx.mini_packet[4] & 0x1)) {
    memcpy (r_buffer, in_buffer, sizeof (struct MP_buffer));
    return (STATUS_COMPRESS_success);
  }



      /******************** Read Length ************************/

  length = ((in_buffer->packet.mpx.mini_packet[0] & 0xf) << 8) |
    (in_buffer->packet.mpx.mini_packet[1] & 0xff);

  if (length < 3) {
    printf ("Bad MFR Packet\n");
    return (STATUS_COMPRESS_fail);
  }

      /***********load the status bits directly*****************/
  for (i = 0; i < 5; ++i) {
    expand_buf[i] = in_buffer->packet.mpx.mini_packet[i];
  }


     /******************* Clear Compression bit *****************/
  expand_buf[4] = expand_buf[4] & 0xfe;

  out = 5;                              /* index for output buffer; Data starts from byte 5 onward */
  h_flag = 0;
  l_flag = 0;
  accumulator = 0;
  e_count = 0;

    /**********************************************************
     The length  does not include bytes 3,4 and 5   i.e.
     the length in the length field is TWO more than length
     of DATA alone.
    *********************************************************/
  length -= 2;

  for (in = 5, j = 0; in < (length + 5); ++in) {
    ++j;
    l_nibble = in_buffer->packet.mpx.mini_packet[in] & 0xf;
    h_nibble = (in_buffer->packet.mpx.mini_packet[in] >> 4) & 0xf;



    if (h_flag) {
      accumulator = (tmp << 4) | h_nibble;
      expand_buf[out] = accumulator;
      ++out;
      h_flag = 0;

      if (l_nibble == 8) {
        ++e_count;
        l_flag = 1;
      } else {
        if (l_nibble < 8)
          accumulator += l_nibble;
        else
          accumulator -= ((~l_nibble & 0xf) + 1);
        expand_buf[out] = accumulator;
        ++out;
      }

    }
    /*
     * if(h_flag) 
     */
    else {                              /*1 */

      if (l_flag) {
        accumulator = in_buffer->packet.mpx.mini_packet[in] & 0xff;
        expand_buf[out] = accumulator;
        ++out;
        l_flag = 0;
      } else {                          /*2 */

        if (h_nibble == 8) {
          ++e_count;
          tmp = l_nibble;
          h_flag = 1;
        } else {                        /*3 */

          if (h_nibble < 8)
            accumulator += h_nibble;
          else
            accumulator -= ((~h_nibble & 0xf) + 1);
          expand_buf[out] = accumulator;
          ++out;


          if (l_nibble == 8) {
            ++e_count;
            l_flag = 1;
          } else {
            if (l_nibble < 8)
              accumulator += l_nibble;
            else
              accumulator -= ((~l_nibble & 0xf) + 1);
            expand_buf[out] = accumulator;
            ++out;
          }
        }                               /*3 */
      }                                 /*2 */
    }                                   /*1 */
  }                                     /* for */


  new_length = out - 5 - 1; /** # of bytes of data */
  new_length += 3;                      /* 3 header bytes also included */

/*
 f_length = f_length + new_length - length;
*/

  r_buffer->f_length = f_length;
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
