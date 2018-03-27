
/* UTIL_DCP_decompress - DCP decompression routine */

/*	Modification History
	29-April-1996	TFA	modified PACK_decompress to handle odd lengths
        03-Nov-1999     TFA     modified WFR Walsh decompress to adjust for WDGF,
                                the Walsh Digital Gain Factor (FSW Load 2.4)
	28-NOV-2001	WTR	ADDED COMMENT TEXT FOR C2MAN
*/

#include <stdio.h>
#include <stdlib.h>
#include "rtiu.h"
#include "util.h"
#include "UTIL_DCP_decompress.h"

struct MPP temp_mpp;
struct MPP *get_mpp, *put_mpp;

int get_bits, get_curr, get_last, get_fail;
int put_bits, put_curr;
int wdgf;                               /* 3-Nov-1999 TFA */

long walsh1_buffer[MP_PKTMAX];
long walsh2_buffer[MP_PKTMAX];

int get_mpp_indx (struct MPP *mpp /* segmented minipacket */ ,
                  int indx)
{
  return mpp->mini_packet[indx];
}

void get_mpp_init (struct MPP *mpp /* segmented minipacket */ ,
                   int curr, int last)
{
  get_mpp = mpp;
  get_curr = curr;
  get_last = last;
  get_bits = 7;
  get_fail = 0;
}

int get_mpp_left (void)
{
  return ((1 + get_bits) + 8 * (get_last - get_curr - 1)) > 0;
}

int get_mpp_bit (void)
{
  int ret;

  if (get_mpp_left ()) {
    ret = (get_mpp_indx (get_mpp, get_curr) >> get_bits) & 1;
    if (get_bits == 0) {
      get_curr++;
      get_bits = 7;
    } else {
      get_bits--;
    };
  } else {
    get_fail = 1;
    ret = 0;
  }
  return ret;
}

int get_mpp_bits (int num)
{
  int ret, bits;

  ret = 0;
  bits = num;
  while (bits != 0) {
    ret = ret * 2 + get_mpp_bit ();
    bits--;
  };
  return ret;
}

int get_mpp_fs ()
{
  int bit, cnt, ret;

  cnt = 0;
  bit = 0;
  while ((bit == 0) && (get_mpp_left ())) {
    bit = get_mpp_bit ();
    cnt++;
  };
  if (bit == 0) {
    get_fail = 1;
    ret = 0;
  } else {
    ret = cnt - 1;
  };
  return ret;
}

int get_mpp_byte (void)
{
  int ret;

  if (get_mpp_left ()) {
    ret = get_mpp_indx (get_mpp, get_curr);
    get_curr++;
    get_bits = 7;
  } else {
    get_fail = 1;
    ret = 0;
  }
  return ret;
}

int get_mpp_word (void)
{
  int ret;

  ret = get_mpp_byte ();
  ret = ret + 256 * get_mpp_byte ();
  return ret;
}


int put_mpp_indx (struct MPP *mpp /* segmented minipacket */ ,
                  int indx, int val)
{
  mpp->mini_packet[indx] = val;
}

void put_mpp_init (struct MPP *mpp /* segmented minipacket */ ,
                   int curr)
{
  put_mpp = mpp;
  put_curr = curr;
  put_bits = 7;
}

void put_mpp_fill (void)
{
  if (put_bits != 7) {
    put_curr++;
  };
}

void put_mpp_bit (int val)
{
  if (put_bits == 7) {
    put_mpp_indx (put_mpp, put_curr, (val & 1) << 7);
  } else {
    put_mpp_indx (put_mpp, put_curr,
                  get_mpp_indx (put_mpp, put_curr) | ((val & 1) << put_bits));
  };
  if (put_bits == 0) {
    put_curr++;
    put_bits = 7;
  } else {
    put_bits--;
  };
}

void put_mpp_bits (int num, int val)
{
  int bits;

  bits = num;
  while (bits != 0) {
    put_mpp_bit (val >> (bits - 1));
    bits--;
  };
}

void put_mpp_byte (int val)
{
  put_mpp_indx (put_mpp, put_curr, val & 255);
  put_curr++;
  put_bits = 7;
}

void put_mpp_word (int val)
{
  put_mpp_byte (val);
  put_mpp_byte (val >> 8);
}

long NONE_decompress (struct MPP *in /* segmented minipacket */ ,
                      struct MPP *out,
                      int bits, int pstart, int gstart, int glast)
{
  int n;

  get_mpp_init (in, gstart, glast);
  put_mpp_init (out, pstart);
  while (get_mpp_left ()) {
    put_mpp_byte (get_mpp_byte () << (12 - bits));
  };
  put_mpp_fill ();
  return STATUS_COMPRESS_success;
}

void WLSH_Prepare (long *wb, int size)
{
  int n;

  wb[0] = 2 * wb[0];                    /* TFA 09-15-98 */
  for (n = 1; n < size; n++) {
    wb[n] = 2 * wb[n] - (long) 4095;    /* TFA 09-15-98 */

/*    wb[n]=wb[n]-(long)2048;              TFA 09-15-98 */
  };
}

void WLSH_Transform (long *iwb, long *owb, int blks, int size)
{
  int b, n, i1, i2, o1, o2, half;

  if (size != 1) {
    half = size / 2;
    o1 = 0;
    o2 = half;
    i1 = 0;
    i2 = 1;
    for (b = 0; b < blks; b++) {
      for (n = 0; n < half; n++) {
        owb[o1] = iwb[i1] + iwb[i2];
        owb[o2] = iwb[i1] - iwb[i2];
        o1 = o1 + 1;
        o2 = o2 + 1;
        i1 = i1 + 2;
        i2 = i2 + 2;
      };
      o1 = o1 + half;
      o2 = o2 + half;
    };
    WLSH_Transform (owb, iwb, blks * 2, size / 2);
  };
}

long WLSH_decompress (struct MPP *in /* segmented minipacket */ ,
                      struct MPP *out,
                      int bits, int pstart, int gstart, int glast)
{
  int indx, size, test, pow2;
  long ret;

  get_mpp_init (in, gstart, glast);
  put_mpp_init (out, pstart);
  indx = 0;
  while (get_mpp_left ()) {
    walsh1_buffer[indx] = get_mpp_word ();
    indx++;
  };
  size = indx;
  if (!get_fail) {
    test = 16;
    pow2 = 4;
    while (test < size) {
      test = test * 2;
      pow2++;
    };
    if (size == test) {
      WLSH_Prepare (walsh1_buffer, size);
      WLSH_Transform (walsh1_buffer, walsh2_buffer, 1, size);
      if ((pow2 & 1) == 0) {
        for (indx = 0; indx < size; indx++) {
          walsh1_buffer[indx] = walsh1_buffer[indx] / 2;        /*      TFA 09-15-98    */
          if (wdgf != 0)                /*      TFA 03-Nov-99    */
            walsh1_buffer[indx] =
              ((walsh1_buffer[indx] - 0x800) >> wdgf) + 0x800;
          if (walsh1_buffer[indx] < 0)
            walsh1_buffer[indx] = 0;
          if (walsh1_buffer[indx] > 4095)
            walsh1_buffer[indx] = 4095;
          put_mpp_word (walsh1_buffer[indx]);
        };
      } else {
        for (indx = 0; indx < size; indx++) {
          walsh2_buffer[indx] = walsh2_buffer[indx] / 2;        /*      TFA 09-15-98    */
          if (wdgf != 0)                /*      TFA 03-Nov-99    */
            walsh2_buffer[indx] =
              ((walsh2_buffer[indx] - 0x800) >> wdgf) + 0x800;
          if (walsh2_buffer[indx] < 0)
            walsh2_buffer[indx] = 0;
          if (walsh2_buffer[indx] > 4095)
            walsh2_buffer[indx] = 4095;
          put_mpp_word (walsh2_buffer[indx]);
        };
      };
      put_mpp_fill ();
      ret = STATUS_COMPRESS_success;
    } else {
      ret = STATUS_COMPRESS_fail;
    };
  } else {
    ret = STATUS_COMPRESS_fail;
  };
  return ret;
}

long RICE_decompress (struct MPP *in /* segmented minipacket */ ,
                      struct MPP *out,
                      int bits, int pstart, int gstart, int glast)
{
  int last, mapv, curr, code, hunk, loop, maxv, half;

  maxv = 1;
  for (loop = 0; loop < bits; loop++) {
    maxv *= 2;
  };
  half = maxv / 2;
  get_mpp_init (in, gstart, glast);
  put_mpp_init (out, pstart);
  last = get_mpp_bits (bits);
  if (!get_fail) {
    while (get_mpp_left ()) {
      code = get_mpp_bits (4);
      if (!get_fail) {
        for (hunk = 0; hunk < 16; hunk++) {
          switch (code) {
           case (0):
             mapv = get_mpp_fs ();
             break;
           case (15):
             mapv = get_mpp_bits (bits);
             break;
           default:
             mapv = get_mpp_fs ();
             mapv = (mapv << code) + get_mpp_bits (code);
          };
          if (!get_fail) {
            if (last < half) {
              if (mapv == 0) {
                curr = last;
              } else {
                if (mapv > 2 * last) {
                  curr = mapv;
                } else {
                  if ((mapv & 1) == 0) {
                    curr = last + (mapv >> 1);
                  } else {
                    curr = last - ((1 + mapv) >> 1);
                  };
                };
              };
            } else {
              if (mapv == 0) {
                curr = last;
              } else {
                if (mapv > 2 * (maxv - 1 - last)) {
                  curr = maxv - 1 - mapv;
                } else {
                  if ((mapv & 1) == 0) {
                    curr = last + (mapv >> 1);
                  } else {
                    curr = last - ((1 + mapv) >> 1);
                  };
                };
              };
            };
            put_mpp_word (curr << (12 - bits));
            last = curr;
          };
        };
      };
    };
  };
  put_mpp_fill ();
  return STATUS_COMPRESS_success;
}

long WLRC_decompress (struct MPP *in /* segmented minipacket */ ,
                      struct MPP *out,
                      int bits, int pstart, int gstart, int glast)
{
  int newlength;
  long ret;

  ret = RICE_decompress (in, &temp_mpp, bits, pstart, gstart, glast);
  if (ret == STATUS_COMPRESS_success) {
    ret = WLSH_decompress (&temp_mpp, out, bits, pstart, pstart, put_curr);
  };
  return ret;
}

long PACK_decompress (struct MPP *in, struct MPP *out,
                      int bits, int pstart, int gstart, int glast)
{
  int temp;

  get_mpp_init (in, gstart, glast);
  put_mpp_init (out, pstart);
  while (get_mpp_left ()) {
    temp = get_mpp_bits (bits) << (12 - bits);
    if (!get_fail)
      put_mpp_word (temp);
  };
  put_mpp_fill ();
  return STATUS_COMPRESS_success;
}

long MAIN_decompress (struct MPP *in /* segmented minipacket */ ,
                      struct MPP *out,
                      int meth, int bits, int start, int last)
{
  long status;

  switch (meth) {
   case (0):
     status = NONE_decompress (in, out, bits, start - 1, start, last);
     break;
   case (1):
     status = WLSH_decompress (in, out, bits, start - 1, start, last);
     break;
   case (2):
     status = RICE_decompress (in, out, bits, start - 1, start, last);
     break;
   case (3):
     status = WLRC_decompress (in, out, bits, start - 1, start, last);
     break;
   case (4):
     status = PACK_decompress (in, out, bits, start - 1, start, last);
     break;
   default:
     status = STATUS_COMPRESS_unknown;
  };
  return status;
}

long LP_decompress (struct MP_buffer *mpb /* segmented minipacket */ ,
                    struct RPWS_buffer *rpwsb)
{
  int ilen, olen, off, msf, meth, bits, start;
  long status;

  ilen = 3 + (get_mpp_indx (&mpb->packet.mpp, 0) & 15) * 256
    + get_mpp_indx (&mpb->packet.mpp, 1);
  off = 0;
  meth = (get_mpp_indx (&mpb->packet.mpp, 10) >> 4) & 15;
  bits = get_mpp_indx (&mpb->packet.mpp, 10) & 15;
  off = off + 1;
  start = 10 + off;
  put_mpp_indx (&rpwsb->packet.mpp, 2, get_mpp_indx (&mpb->packet.mpp, 2));
  put_mpp_indx (&rpwsb->packet.mpp, 3, get_mpp_indx (&mpb->packet.mpp, 3));
  put_mpp_indx (&rpwsb->packet.mpp, 4,
                get_mpp_indx (&mpb->packet.mpp, 4) & 243);
  put_mpp_indx (&rpwsb->packet.mpp, 5, get_mpp_indx (&mpb->packet.mpp, 5));
  put_mpp_indx (&rpwsb->packet.mpp, 6, get_mpp_indx (&mpb->packet.mpp, 6));
  put_mpp_indx (&rpwsb->packet.mpp, 7, get_mpp_indx (&mpb->packet.mpp, 7));
  put_mpp_indx (&rpwsb->packet.mpp, 8, get_mpp_indx (&mpb->packet.mpp, 8));
  put_mpp_indx (&rpwsb->packet.mpp, 9, get_mpp_indx (&mpb->packet.mpp, 9));
  status = MAIN_decompress (&mpb->packet.mpp, &rpwsb->packet.mpp,
                            meth, bits, start, ilen);
  olen = put_curr - 3;
  put_mpp_indx (&rpwsb->packet.mpp, 0, (PACKET_TYPE_lp << 4) + (olen >> 8));
  put_mpp_indx (&rpwsb->packet.mpp, 1, olen);
  rpwsb->f_length = 8 + 256 + put_curr;
  rpwsb->record_type = DATA_MPS_LP;     /* 9/00 WTR  Segment size limited to 1K by current software */
  rpwsb->status = DATA_DECOMP_DCP;
  return status;
}

long WFR_decompress (struct MP_buffer *mpb /* segmented minipacket */ ,
                     struct RPWS_buffer *rpwsb)
{
  int ilen, olen, off, msf, meth, bits, start;
  long status;

  ilen = 3 + (get_mpp_indx (&mpb->packet.mpp, 0) & 15) * 256
    + get_mpp_indx (&mpb->packet.mpp, 1);
  msf = (get_mpp_indx (&mpb->packet.mpp, 6) >> 3) & 1;
  off = 0;
  if (msf == 1) {
    off = off + 2;
  };
  meth = (get_mpp_indx (&mpb->packet.mpp, 8 + off) >> 4) & 15;
  bits = get_mpp_indx (&mpb->packet.mpp, 8 + off) & 15;
  if ((meth == 1) || (meth == 3))       /* Walsh or Walsh-Rice ? */
    wdgf = (get_mpp_indx (&mpb->packet.mpp, 7) >> 6) & 3;       /* 03-Nov-99 TFA */
  off = off + 1;
  start = 8 + off;
  put_mpp_indx (&rpwsb->packet.mpp, 2, get_mpp_indx (&mpb->packet.mpp, 2));
  put_mpp_indx (&rpwsb->packet.mpp, 3, get_mpp_indx (&mpb->packet.mpp, 3));
  put_mpp_indx (&rpwsb->packet.mpp, 4, get_mpp_indx (&mpb->packet.mpp, 4));
  put_mpp_indx (&rpwsb->packet.mpp, 5, get_mpp_indx (&mpb->packet.mpp, 5));
  put_mpp_indx (&rpwsb->packet.mpp, 6,
                get_mpp_indx (&mpb->packet.mpp, 6) & 15);
  put_mpp_indx (&rpwsb->packet.mpp, 7, get_mpp_indx (&mpb->packet.mpp, 7));
  if (msf == 1) {
    put_mpp_indx (&rpwsb->packet.mpp, 8, get_mpp_indx (&mpb->packet.mpp, 8));
    put_mpp_indx (&rpwsb->packet.mpp, 9, get_mpp_indx (&mpb->packet.mpp, 9));
  };
  status = MAIN_decompress (&mpb->packet.mpp, &rpwsb->packet.mpp,
                            meth, bits, start, ilen);
  olen = put_curr - 3;
  put_mpp_indx (&rpwsb->packet.mpp, 0, (PACKET_TYPE_wfr << 4) + (olen >> 8));
  put_mpp_indx (&rpwsb->packet.mpp, 1, olen);
  rpwsb->f_length = 8 + 256 + put_curr;
  rpwsb->record_type = DATA_MPS_WFR;    /* 9/00 WTR  Segment size limited to 1K by current software */
  rpwsb->status = DATA_DECOMP_DCP;
  return status;
}

long LP_process (struct MP_buffer *mpb /* segmented minipacket */ ,
                 struct RPWS_buffer *rpwsb)
{
  int comp;
  long status;

  comp = (get_mpp_indx (&mpb->packet.mpp, 4) >> 2) & 3;
  switch (comp) {
   case (0):
     status = STATUS_COMPRESS_uncompressed;
     break;
   case (3):
     status = LP_decompress (mpb, rpwsb);
     break;
   default:
     status = STATUS_COMPRESS_not_my_data;
  };
  return status;
}

long WFR_process (struct MP_buffer *mpb /* segmented minipacket */ ,
                  struct RPWS_buffer *rpwsb)
{
  int comp;
  long status;

  comp = (get_mpp_indx (&mpb->packet.mpp, 6) >> 4) & 15;
  switch (comp) {
   case (0):
     status = STATUS_COMPRESS_uncompressed;
     break;
   case (4):
     status = WFR_decompress (mpb, rpwsb);
     break;
   default:
     status = STATUS_COMPRESS_not_my_data;
  };
  return status;
}

 /*
  * Decompress any data sets that were compressed on the DCP
  * * Dataproducts handled on the DCP are all in the form of 
  * * segmented minipackets.  Since these are small datasets,
  * * they must be decompressed prior to minipacket desegmenting.
  * * returns compression status (STATUS_COMPRESS*)
  */
long UTIL_DCP_decompress (struct MP_buffer *mpb /* segmented minipacket */ ,
                          struct RPWS_buffer *rpwsb)
{                                       /* RPWS minipacket buffer */
  long status;

  /*
   *   The following makes decompress ONLY look at unsegmented
   *   mini packets.  Once passxed through MPUS, we can't
   *   correctly decompress 1/96 WTR
   */
  switch (mpb->record_type & 0x0FF00) {
   default:
     return STATUS_COMPRESS_not_my_data;
   case 0x100:
   case 0x200:
   case 0x300:
     break;
  }

  wdgf = 0;                             /* 03-Nov-99 TFA */
  switch ((get_mpp_indx (&mpb->packet.mpp, 0) >> 4) & 15) {
   case (PACKET_TYPE_lp):              /* Langmuir Probe */
     status = LP_process (mpb, rpwsb);
     break;
   case (PACKET_TYPE_wfr):             /* Waveform Reciever */
     status = WFR_process (mpb, rpwsb);
     break;
   default:
     status = STATUS_COMPRESS_not_my_data;
  };
  return status;
}
