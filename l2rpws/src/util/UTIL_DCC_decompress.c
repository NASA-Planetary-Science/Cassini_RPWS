
#include <stdio.h>
#include <stdlib.h>

#include "telemetry.h"

extern int RICE_DEBUG;
extern int RICE_DEBUG_ALL;
extern int RICE_STATS;

struct
{
  long inbits, outbits;
  long inbytes, outbytes;
  long bits_per_sample;
} Stats;

static int DONE;
static FILE *debug_handle;

#define THETA( x,n ) ((long)(((x)<(((long int)(1<<n)-1)-x))?(x):(((1<<n)-1)-x)))

/* 			if x < [(2^n-1)-x] then x, else [(2^n-1)-x] 	*/

typedef struct
{
  long bit_index, word_index;
  long Fo, K, J, n, N;                  /* Fo=sum of mapped differences,K=PSI index,J=block size,n=bits/sample,N=number of code options */
  long ref_sample, ref_sample_len, code_id, code_id_len;        /* reference sample length in bits, code id length in bits */
  long psi[16], inlen, outlen, inbit_count, outbit_count;
  unsigned *indata;
  unsigned *outdata;
} DATA_STREAM;

void initialize_isflip_table (DATA_STREAM * pds, unsigned bits);        /* specific to RPWS Cassini Compression Chip */
void decmp_fs (DATA_STREAM * pds, unsigned *rcb);
void decmp_ss (DATA_STREAM * pds, unsigned *rcb);
void decmp_bo (DATA_STREAM * pds, unsigned *rcb);
void unmap_samples (DATA_STREAM * pds, unsigned *rcb);
unsigned get_comp_bits (DATA_STREAM * pds, unsigned num_bits);
long rice_decompress (unsigned *inbuffer, unsigned inlength,
                      unsigned *outbuffer, unsigned *outlength,
                      unsigned bits_per_sample);
long dcc_decompress (TELEMETRY * tlm);
long dcc_unleave (TELEMETRY * tlmin, TELEMETRY *** tlmout);
void dump_buffer (unsigned *b, unsigned len, FILE * outhandle);

long dcc_decompress_wrapper (void *pvoid)
{
  long rtnsts = 0;
  TELEMETRY *tlm = pvoid;
  UNSEGMENTED_MINI_PACKET *usmp;
  static int wfr_channels;
  static TELEMETRY **wfrtlm;

  usmp = &(tlm->packet.usmp);
  switch (get_mini_packet_type (usmp)) {
   case MINI_PACKET_TYPE_wbr:
     if (MP_COMPRESSION_dcc_wcin == get_mp_wbr_compression (usmp) || MP_COMPRESSION_dcc_wcout == get_mp_wbr_compression (usmp) || (get_mp_wbr_compression (usmp) & 0x080)) {    /* Willy's Compression Status 1xxx */
       dcc_decompress (tlm);            /* 1=Attempt to take out 1st word */
     }                                  /* of junk decompression */
     break;
   case MINI_PACKET_TYPE_wfr:
     if (MP_COMPRESSION_dcc_wcin == get_mp_wfr_compression (usmp) ||
         MP_COMPRESSION_dcc_wcout == get_mp_wfr_compression (usmp)) {
       dcc_decompress (tlm);
       rtnsts = wfr_channels = dcc_unleave (tlm, &wfrtlm);      /* a NULL terminated array with seperated channels */
     }
     break;
   default:
     break;
  }

  return 0;
}


long dcc_decompress (TELEMETRY * tlm)
{
  unsigned bits_per_sample;
  long l, m, mp_data_offset, mp_data_length;
  static unsigned inbuffer[DCC_DECOMPRESSION_BUFFER_SIZE], inlength;
  static unsigned outbuffer[DCC_DECOMPRESSION_BUFFER_SIZE], outlength;
  UNSEGMENTED_MINI_PACKET *usmp = &(tlm->packet.usmp);

  switch (get_mini_packet_type (usmp)) {
   case MINI_PACKET_TYPE_wbr:
     bits_per_sample = 8;
     Stats.bits_per_sample = 8;
     if (get_mp_wbr_msf (usmp))
       mp_data_offset = 10;
     else
       mp_data_offset = 8;
     if (get_mp_wbr_band (usmp))
       mp_data_offset += 2;             /* 80KHz Mode, 1st sample is allways bad */
     if (get_mp_wbr_compression (usmp) & 0x08)
       mp_data_offset -= 2;             /* 80KHz Mode temporary compression status */
     break;
   case MINI_PACKET_TYPE_wfr:
     bits_per_sample = 12;
     Stats.bits_per_sample = 12;
     if (get_mp_wfr_msf (usmp))
       mp_data_offset = 10;
     else
       mp_data_offset = 8;
     break;
   default:
     return 0;
  }

  mp_data_length = get_telemetry_packet_length (tlm);
  for (l = mp_data_offset, inlength = 0; l < mp_data_length; inlength++, l++) {
    inbuffer[inlength] = (usmp->Data[l++] << 8);        /* High Byte */
    inbuffer[inlength] |= usmp->Data[l];        /* Low Byte */
  }

  Stats.inbytes = l;                    /* the real packet length (mp_len+3) */
  Stats.inbits = inlength * 16;

  rice_decompress (inbuffer, inlength, outbuffer, &outlength,
                   bits_per_sample);

  switch (get_mini_packet_type (usmp)) {
   case MINI_PACKET_TYPE_wbr:
     if (get_mp_wbr_band (usmp))
       mp_data_offset -= 2;             /* 80KHz Mode, 1st sample discarded */
     if (get_mp_wbr_compression (usmp) & 0x08)
       mp_data_offset += 2;             /* 80KHz Mode temporary compression status */
     for (l = mp_data_offset, m = 0; m < outlength; l++, m++)
       usmp->Data[l] = outbuffer[m] & 0x00FF;
     put_mp_wbr_compression (usmp, MP_COMPRESSION_unpacked);
     break;
   case MINI_PACKET_TYPE_wfr:
     for (l = mp_data_offset, m = 0; m < outlength; l++, m++) {
       usmp->Data[l++] = outbuffer[m] & 0x00FF; /* Low Byte */
       usmp->Data[l] = (outbuffer[m] >> 8) & 0x00FF;    /* High Byte */
     }
     put_mp_wfr_compression (usmp, MP_COMPRESSION_unpacked);
     break;
   default:
     return 0;
  }
  Stats.outbytes = l;
  Stats.outbits = outlength * Stats.bits_per_sample;
  stamp_telemetry_packet_lengths (tlm, l);

  if (RICE_STATS) {
    fprintf (stderr, "%s Compression ",
             (get_mini_packet_type (usmp) ==
              MINI_PACKET_TYPE_wbr ? "WBR" : "WFR"));
    fprintf (stderr, "%.3f [bits] = %ld/%ld \t\t",
             (double) Stats.inbits / (double) Stats.outbits, Stats.inbits,
             Stats.outbits);
    fprintf (stderr, "%.3f [bytes] = %ld/%ld ",
             (double) Stats.inbytes / (double) Stats.outbytes, Stats.inbytes,
             Stats.outbytes);
    fprintf (stderr, "\n");
  }

  return 0;
}


long rice_decompress (unsigned *inbuffer, unsigned inlength,
                      unsigned *outbuffer, unsigned *outlength,
                      unsigned bits_per_sample)
{
  int loop;
  unsigned *delta_tilda;
  DATA_STREAM deprs;

  if (RICE_DEBUG && !RICE_DEBUG_ALL)
    debug_handle = fopen ("ricedebug.txt", "wt");
  else if (RICE_DEBUG_ALL && !debug_handle)
    debug_handle = fopen ("ricedebug.txt", "wt");

  deprs.bit_index = 15;                 /* bit 15 is earliest in time */
  deprs.word_index = 0;                 /* ie, first bit in bitstream */
  deprs.indata = inbuffer;              /* compressed data */
  deprs.inlen = inlength;
  deprs.inbit_count = 0;
  deprs.outdata = outbuffer;            /* decompressed data */
  deprs.outlen = 0;
  deprs.outbit_count = 0;
  *outlength = 0;
  DONE = 0;

  initialize_isflip_table (&deprs, bits_per_sample);

  if ((delta_tilda =
       (unsigned *) malloc ((deprs.J + 1) * sizeof (unsigned))) == NULL) {
    exit (1);
  }

  deprs.ref_sample = get_comp_bits (&deprs, deprs.ref_sample_len);
  if (RICE_DEBUG) {
    fprintf (debug_handle, "\n\n\nReference Sample = %lX \n",
             deprs.ref_sample);
  }

  while (!DONE) {
    deprs.code_id = get_comp_bits (&deprs, deprs.code_id_len);
    if (deprs.code_id >= deprs.n) {     /* backup operator */
      decmp_bo (&deprs, delta_tilda);
      if (RICE_DEBUG) {
        fprintf (debug_handle, "Back-up Operator, code_id=%x\n",
                 (unsigned) deprs.code_id);
        for (loop = 0; loop <= 16; loop++)
          fprintf (debug_handle, "%5d ", delta_tilda[loop]);
        fprintf (debug_handle, "\n");
      }
    } else if (deprs.code_id < deprs.n) {       /* all other operators */
      decmp_fs (&deprs, delta_tilda);
      if (RICE_DEBUG) {
        fprintf (debug_handle, "Fundamental Sequence, code_id=%x\n",
                 (unsigned) deprs.code_id);
        for (loop = 0; loop <= 16; loop++)
          fprintf (debug_handle, "%5d ", delta_tilda[loop] >> deprs.code_id);
        fprintf (debug_handle, "\n");
      }
      decmp_ss (&deprs, delta_tilda);
      if (RICE_DEBUG && deprs.code_id) {
        fprintf (debug_handle, "Mapped Delta, code_id=%x\n",
                 (unsigned) deprs.code_id);
        for (loop = 0; loop <= 16; loop++)
          fprintf (debug_handle, "%5d ", delta_tilda[loop]);
        fprintf (debug_handle, "\n");
      }
    } else {
      return NULL;
    }
    unmap_samples (&deprs, delta_tilda);
    deprs.ref_sample = delta_tilda[deprs.J];
    for (loop = 1; loop <= deprs.J; loop++)
      outbuffer[(*outlength)++] = delta_tilda[loop];
  }
  if (RICE_DEBUG && !RICE_DEBUG_ALL)
    fclose (debug_handle);
  (*outlength) -= 16;

  return 0;
}



void decmp_fs (DATA_STREAM * pds, unsigned *rcb)
{
  unsigned value, loop;

  for (loop = 1; loop <= pds->J; loop++) {
    value = 0;
    while (!(get_comp_bits (pds, 1)))
      value++;
    rcb[loop] = value << pds->code_id;
  }

  return;
}



void decmp_ss (DATA_STREAM * pds, unsigned *rcb)
{
  unsigned loop, index, tmp;

  for (loop = 1; loop <= pds->code_id; loop++) {        /* if code_id==0 return */
    for (index = 1; index <= pds->J; index++) {
      tmp = get_comp_bits (pds, 1);
      if (tmp)
        rcb[index] |= 0x0001 << (pds->code_id - loop);
      else
        rcb[index] &= 0xFFFE << (pds->code_id - loop);
    }
  }

  return;
}



void decmp_bo (DATA_STREAM * pds, unsigned *rcb)
{
  int loop;

  for (loop = 1; loop <= pds->J; loop++)
    rcb[loop] = get_comp_bits (pds, pds->n);

  return;
}



void unmap_samples (DATA_STREAM * pds, unsigned *rcb)
{
  long int delta, theta;
  unsigned index;

  rcb[0] = pds->ref_sample;             /* reference sample */

  for (index = 1; index <= pds->J; index++) {
    theta = THETA (rcb[index - 1], pds->n);
    if (rcb[index] % 2) {
      delta = (long) (rcb[index] + 1) / 2;      /* assume a delta */
      if (0 < delta && delta <= theta) {
        rcb[index] = delta + rcb[index - 1];    /* positive delta */
      } else {
        delta = (long) rcb[index] - theta;      /* absolute value of delta */
        delta = (theta == rcb[index - 1] ? delta : -delta);
        rcb[index] = rcb[index - 1] + delta;
      }
    } else {
      delta = (long) rcb[index] / -2;   /* assume a delta */
      if (-theta <= delta && delta <= 0) {
        rcb[index] = delta + rcb[index - 1];
      } else {
        delta = (long) rcb[index] - theta;      /* absolute value of delta */
        delta = (theta == rcb[index - 1] ? delta : -delta);
        rcb[index] = rcb[index - 1] + delta;
      }
    }
  }

  return;
}



unsigned get_comp_bits (DATA_STREAM * pds, unsigned num_bits)
{
  unsigned tmp, data = 0;               /* initialization is important */

  pds->inbit_count += num_bits;         /* number of bits taken from the buffer so far */

  if ((pds->inlen * 16) < pds->inbit_count) {
    DONE = 1;
    return 1;
  }

  while (num_bits > 0) {                /* can never be negative */
    --num_bits;                         /* decreament for shift operations */
    tmp = (pds->indata[pds->word_index]) & (0x0001 << pds->bit_index);
    if (tmp)                            /* tmp is a one */
      data |= 0x0001 << num_bits;
    else                                /* tmp is a zero */
      data &= 0xFFFE << num_bits;
    --pds->bit_index;
    if (pds->bit_index < 0) {
      pds->bit_index = 15;
      ++pds->word_index;
    }
  }

  return data;
}



/* specific to RPWS Cassini Data Compression Chip from ISFLIP (Honeywell ASIC) */
void initialize_isflip_table (DATA_STREAM * pds, unsigned bits)
{

  pds->J = 16;                          /* 16 samples per block */
  pds->N = 11;                          /* psi1,0 - psi1,9 & psi3,0 */
  pds->n = bits;                        /* 8-12 bits per sample */
  pds->ref_sample_len = 12;             /* reference sample length is 12 bits for 8-12 bit modes */
  pds->code_id_len = 4;                 /* code id length is 4 bits for 8-12 bit modes */

  for (pds->K = 0; pds->K < 9; pds->K++)
    pds->psi[pds->K] = -(pds->J / 2) + pds->J * (1 << (pds->K + 1));
  pds->psi[pds->K] = (pds->J / 2) * ((pds->n - pds->K) * (1 << (pds->K + 1)) - 1 - (1 << pds->K));      /* K=9 */
  for (pds->K = 10; pds->K < 15; pds->K++)
    pds->psi[pds->K] = pds->psi[9] + 1; /* back-up operator is psi[15],10-14 are unused */

  return;
}



void dump_buffer (unsigned *b, unsigned len, FILE * outhandle)
{
  long l;

  for (l = 0; l < len; l++) {
    if (!(l % 8))
      fprintf (outhandle, "\n%04lX: ", l);
    fprintf (outhandle, "%04X ", b[l]);
  }
  fprintf (outhandle, "\n");

  return;
}



long dcc_unleave (TELEMETRY * in, TELEMETRY *** out)
{
  long index, rtn_sts;
  long pktidx, inpktlen, mplen[5];
  UNSEGMENTED_MINI_PACKET *mpin = &in->packet.usmp;
  static TELEMETRY data[5], *pkt[6];

  for (index = 0; index < 5; index++)
    pkt[index] = &data[index];
  pkt[index] = NULL;
  *out = pkt;

  switch (get_mp_wfr_mode (mpin)) {
   case MP_WFR_MODE_0:                 /* One channel mode */
   case MP_WFR_MODE_1:
   case MP_WFR_MODE_2:
   case MP_WFR_MODE_3:
   case MP_WFR_MODE_4:
     pkt[0] = in;
     pkt[1] = NULL;
     return 1;
     break;
   case MP_WFR_MODE_5:                 /* Two channel mode */
     pkt[2] = NULL;
     break;
   case MP_WFR_MODE_6:                 /* Three channel mode */
     pkt[3] = NULL;
     break;
   case MP_WFR_MODE_7:                 /* Five channel mode */
     pkt[5] = NULL;
     break;
   default:
     return 0;
  }

  /*
   * Copy Status Information 
   */
  pktidx = 0;
  do {
    for (index = 0; index < 256; index++)
      pkt[pktidx]->status.fill0[index] = in->status.fill0[index];
    ++pktidx;
  } while (pkt[pktidx]);

  /*
   * Copy Mini-Packet Header 
   */
  pktidx = 0;
  do {
    for (index = 0; index < 16; index++)
      pkt[pktidx]->packet.usmp.Data[index] = in->packet.usmp.Data[index];
    ++pktidx;
  } while (pkt[pktidx]);

  /*
   * Find Data Area Offset 
   */
  if (get_mp_wfr_msf (mpin)) {
    index = 10;
    mplen[0] = index;
    mplen[1] = index;
    mplen[2] = index;
    mplen[3] = index;
    mplen[4] = index;
  } else {
    index = 8;
    mplen[0] = index;
    mplen[1] = index;
    mplen[2] = index;
    mplen[3] = index;
    mplen[4] = index;
  }

  /*
   * Seperate the Data and Copy 
   */
  inpktlen = get_telemetry_packet_length (in);
  while (index < inpktlen) {
    pktidx = 0;
    do {
      pkt[pktidx]->packet.usmp.Data[mplen[pktidx]++] =
        in->packet.usmp.Data[index++];
      pkt[pktidx]->packet.usmp.Data[mplen[pktidx]++] =
        in->packet.usmp.Data[index++];
      ++pktidx;
    } while (pkt[pktidx]);
  }

  pktidx = 0;
  do {
    stamp_telemetry_packet_lengths (pkt[pktidx], mplen[pktidx]);
    ++pktidx;
  } while (pkt[pktidx]);

  /*
   * Put Single Channel Number in for Seperated Data 
   */
  switch (get_mp_wfr_mode (mpin)) {
   case MP_WFR_MODE_5:
     put_mp_wfr_channel ((&pkt[0]->packet.usmp), MP_WFR_CHANNEL_0);
     put_mp_wfr_channel ((&pkt[1]->packet.usmp), MP_WFR_CHANNEL_1);
     rtn_sts = 2;
     break;
   case MP_WFR_MODE_6:
     put_mp_wfr_channel ((&pkt[0]->packet.usmp),
                         (get_mp_wfr_mode_lp (mpin) ? MP_WFR_CHANNEL_1 :
                          MP_WFR_CHANNEL_3));
     put_mp_wfr_channel ((&pkt[1]->packet.usmp),
                         (get_mp_wfr_mode_lp (mpin) ? MP_WFR_CHANNEL_2 :
                          MP_WFR_CHANNEL_4));
     put_mp_wfr_channel ((&pkt[2]->packet.usmp),
                         (get_mp_wfr_mode_lp (mpin) ? MP_WFR_CHANNEL_0 :
                          MP_WFR_CHANNEL_2));
     rtn_sts = 3;
     break;
   case MP_WFR_MODE_7:
     put_mp_wfr_channel ((&pkt[0]->packet.usmp), MP_WFR_CHANNEL_4);
     put_mp_wfr_channel ((&pkt[1]->packet.usmp), MP_WFR_CHANNEL_0);
     put_mp_wfr_channel ((&pkt[2]->packet.usmp), MP_WFR_CHANNEL_1);
     put_mp_wfr_channel ((&pkt[3]->packet.usmp), MP_WFR_CHANNEL_2);
     put_mp_wfr_channel ((&pkt[4]->packet.usmp), MP_WFR_CHANNEL_3);
     rtn_sts = 5;
     break;
   default:
     fprintf (stderr, "\nInvalid WFR Mode DCC Unleaving( )\n");
     exit (0);                          /* Disaster if we are here */
     break;
  }

  return rtn_sts;
}
