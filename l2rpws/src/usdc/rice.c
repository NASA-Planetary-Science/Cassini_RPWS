#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "rice.h"

char *RICE_FILENAME = NULL;
int arHist_Psi[16];
unsigned long nWCin, nWCout;
unsigned long RICE_DEBUG = 0;

static int DONE, DEBUG, nERROR;
static unsigned long nAddress;
static FILE *debug_handle;

#define THETA( x,n ) ((long)(((x)<(((long int)(1<<n)-1)-x))?(x):(((1<<n)-1)-x)))

/* 			if x < [(2^n-1)-x] then x, else [(2^n-1)-x] 	*/

typedef struct
{
  long bit_index, word_index;
  long Fo, K, J;                        /* Fo=sum of mapped differences,K=PSI index,J=block size */
  long n, N;                            /* n=bits/sample,N=number of code options */
  long ref_sample, ref_sample_len;      /* reference sample length in bits */
  long code_id, code_id_len;            /* code id length in bits */
  long psi[16], inlen, outlen, inbit_count, outbit_count;
  unsigned *indata;
  unsigned *outdata;
} DATA_STREAM;

/* specific to RPWS Cassini Compression Chip */
void initialize_isflip_table (DATA_STREAM * pds, unsigned bits);
void decmp_fs (DATA_STREAM * pds, unsigned *rcb);
void decmp_ss (DATA_STREAM * pds, unsigned *rcb);
void decmp_bo (DATA_STREAM * pds, unsigned *rcb);
void unmap_samples (DATA_STREAM * pds, unsigned *rcb);
unsigned get_comp_bits (DATA_STREAM * pds, unsigned num_bits);



unsigned long rice_decompress (unsigned *inbuffer, unsigned inlength,
                               unsigned *outbuffer, unsigned *outlength,
                               unsigned bits_per_sample)
{
  int loop;
  unsigned *delta_tilda, mask;
  DATA_STREAM deprs;


  if (RICE_DEBUG && debug_handle == NULL) {
    if (RICE_FILENAME == NULL) {
      RICE_FILENAME = malloc (32);
      strcpy (RICE_FILENAME, "stderr");
      debug_handle = stderr;
    } else if ((debug_handle = fopen (RICE_FILENAME, "wt")) == NULL) {
      fprintf (stderr,
               "rice_decompress(), error - unable to open debug file " "%s\n",
               RICE_FILENAME);
      exit (1);
    }
  }


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
  nERROR = 0;                           /* no errors yet */
  nWCin = nWCout = 0;
  initialize_isflip_table (&deprs, bits_per_sample);

  for (loop = 0; loop < 16; loop++)
    arHist_Psi[loop] = 0;
  nAddress = 0;

  if (inlength == 0)
    return 1;

  if ((delta_tilda =
       (unsigned *) malloc ((deprs.J + 1) * sizeof (unsigned))) == NULL) {
    exit (1);
  }


  if (RICE_DEBUG & RICEDEBUG_REFSMP)
    DEBUG = 1;
  else
    DEBUG = 0;
  deprs.ref_sample = get_comp_bits (&deprs, (unsigned) deprs.ref_sample_len);
  if (DEBUG) {
    fprintf (debug_handle, "Reference Sample = %lX \n", deprs.ref_sample);
    DEBUG = 0;
  }

  while (!DONE) {

    if (RICE_DEBUG & RICEDEBUG_CODEID)
      DEBUG = 1;
    deprs.code_id = get_comp_bits (&deprs, (unsigned) deprs.code_id_len);
    ++arHist_Psi[deprs.code_id];
    if (DEBUG) {
      fprintf (debug_handle, "Code Id:%ld(0x%lX)\n", deprs.code_id,
               deprs.code_id);
      DEBUG = 0;
    }



    /*
     * The dcc chip uses the 12 bit mapping for 8 bit samples, which means   
     */
    /*
     * the backup operator for 8 bit samples is unattainable; for operators  
     */
    /*
     * 6 & 7, there can be more bits out than produced by the backup         
     */
    /*
     * operator.  4080 is the maximum summed difference for 8 bits with the  
     */
    /*
     * maximum number of bits out being 148.                                 
     */
    /*
     * Summed Differencs  Code Id    max. bits (8)   max. bits (12)          
     */
    /*
     * fo <=    24      0         44                                 
     */
    /*
     * 24 < fo <=    56      1         64                                 
     */
    /*
     * 56 < fo <=   120      2         83                                 
     */
    /*
     * 120 < fo <=   248      3         99                                 
     */
    /*
     * 248 < fo <=   504      4        115                                 
     */
    /*
     * 504 < fo <=  1016      5        131                                 
     */
    /*
     * 1016 < fo <=  2040      6        147                                 
     */
    /*
     * 2040 < fo <=  4088      7        163                                 
     */
    /*
     * 4088 < fo <=  8184      8        179 *                               
     */
    /*
     * 8184 < fo <= 20472      9            *                               
     */
    /*
     * 20472 < fo            Backup          *                               
     */

    if (deprs.n == 12) {                /* 12 bits per sample */
      if ((deprs.code_id > 9) && (deprs.code_id != 15)) {       /* 0-9 & 15 valid */
        if (nERROR == 0) {
          nWCin = deprs.word_index;
          nWCout = *outlength;
        }
        ++nERROR;

        if (RICE_DEBUG & RICEDEBUG_ERRORS)
          fprintf (stderr, "error code id=%ld, word idx=%ld, bit idx=%ld"
                   ", byte %ld(0x%lX)\n",
                   deprs.code_id, deprs.word_index, deprs.bit_index,
                   deprs.word_index * 2, deprs.word_index * 2);
      }
    } else if (deprs.n == 8) {          /*  8 bits per sample */
      if ((deprs.code_id > 7)) {        /* 0-9 & 15 valid, but only 0-7 are realizable */
        if (nERROR == 0) {
          nWCin = deprs.word_index;
          nWCout = *outlength;
        }
        ++nERROR;
        if (RICE_DEBUG & RICEDEBUG_ERRORS)
          fprintf (stderr, "error code id=%ld, word idx=%ld, bit idx=%ld"
                   ", byte %ld(0x%lX)\n",
                   deprs.code_id, deprs.word_index, deprs.bit_index,
                   deprs.word_index * 2, deprs.word_index * 2);
      } else if ((deprs.code_id > 5)) {
        if (RICE_DEBUG & RICEDEBUG_ERRORS)
          fprintf (stderr, "warning code id=%ld, word idx=%ld, bit idx=%ld"
                   ", byte %ld(0x%lX)\n",
                   deprs.code_id, deprs.word_index, deprs.bit_index,
                   deprs.word_index * 2, deprs.word_index * 2);
      }
    } else {                            /* error condition */
      fprintf (stderr, "Rice Decompression() invalid bits/sampel=%ld\n",
               deprs.n);
    }

    if (deprs.code_id >= deprs.n) {     /* backup operator */
      if (RICE_DEBUG & RICEDEBUG_BOBITS)
        DEBUG = 1;
      decmp_bo (&deprs, delta_tilda);
      DEBUG = 0;

      if (RICE_DEBUG & RICEDEBUG_BODATA) {
        fprintf (debug_handle, "Back-up Operator, code_id=%x, ref=%ld(0x%lX)",
                 (unsigned) deprs.code_id, deprs.ref_sample,
                 deprs.ref_sample);
        fprintf (debug_handle, ", delta_tilda[0]=%d(0x%X)\n", delta_tilda[0],
                 delta_tilda[0]);
        for (loop = 1; loop <= 16; loop++)
          fprintf (debug_handle, "%5d", delta_tilda[loop]);
        fprintf (debug_handle, "\n");
      }
    } else if (deprs.code_id < deprs.n) {       /* all other operators */
      if (RICE_DEBUG & RICEDEBUG_FSBITS)
        DEBUG = 1;
      decmp_fs (&deprs, delta_tilda);
      DEBUG = 0;

      if (RICE_DEBUG & RICEDEBUG_FSDATA) {
        fprintf (debug_handle,
                 "Fundamental Sequence, code_id=%x, ref=%ld(0x%lX)",
                 (unsigned) deprs.code_id, deprs.ref_sample,
                 deprs.ref_sample);
        fprintf (debug_handle, ", delta_tilda[0]=%d(0x%X)\n", delta_tilda[0],
                 delta_tilda[0]);
        for (loop = 1; loop <= 16; loop++)
          fprintf (debug_handle, "%5d", delta_tilda[loop] >> deprs.code_id);
        fprintf (debug_handle, "\n");
      }

      if (RICE_DEBUG & RICEDEBUG_SSBITS)
        DEBUG = 1;
      decmp_ss (&deprs, delta_tilda);
      DEBUG = 0;

      if (RICE_DEBUG & RICEDEBUG_SSDATA) {
        fprintf (debug_handle, "Split Sequence, code_id=%x, ref=%ld(0x%lX)",
                 (unsigned) deprs.code_id, deprs.ref_sample,
                 deprs.ref_sample);
        fprintf (debug_handle, ", delta_tilda[0]=%d(0x%X)\n", delta_tilda[0],
                 delta_tilda[0]);
        mask = 0xFFFF;
        mask <<= deprs.code_id;
        mask = ~mask;
        for (loop = 1; loop <= 16; loop++)
          fprintf (debug_handle, "%5d", delta_tilda[loop] & mask);
        fprintf (debug_handle, "\n");
      }
    } /* esle fundamental,split sequences */
    else {
      fprintf (stderr, "rice(), fatal error\n");
      return 1;
    }

    if (RICE_DEBUG & RICEDEBUG_MAPDELTA) {
      fprintf (debug_handle, "Mapped Delta, code_id=%x, ref=%ld(0x%lX)",
               (unsigned) deprs.code_id, deprs.ref_sample, deprs.ref_sample);
      fprintf (debug_handle, "delta_tilda[0]=%d(0x%X)\n", delta_tilda[0],
               delta_tilda[0]);
      for (loop = 1; loop <= 16; loop++)
        fprintf (debug_handle, "%5d", delta_tilda[loop]);
      fprintf (debug_handle, "\n");
    }

    unmap_samples (&deprs, delta_tilda);
    deprs.ref_sample = delta_tilda[deprs.J];
    for (loop = 1; loop <= deprs.J; loop++)
      outbuffer[(*outlength)++] = delta_tilda[loop];

    if (RICE_DEBUG & RICEDEBUG_OUTDATA) {
      fprintf (debug_handle, "outdata %04lX: ", nAddress);
      for (loop = 1; loop <= deprs.J; loop++)
        fprintf (debug_handle, "%02X ", delta_tilda[loop]);
      fprintf (debug_handle, "\n");
      nAddress += 16;
    }

  }                                     /* elihw not done */

  --arHist_Psi[deprs.code_id];          /* last code id should be bogus */
  if (*outlength < 16)
    *outlength = 0;
  else
    (*outlength) -= 16;
  if (nERROR == 0) {                    /* no errors during decompression */
    nWCin = deprs.word_index;
    nWCout = *outlength;
  }

  if (RICE_DEBUG & RICEDEBUG_PSIHIST) {
    char sErr[32], sWrn[32], sBck[32];
    char *pErr, *pWrn, *pBck;
    int sum = 0;

    pErr = sErr;
    pWrn = sWrn;
    pBck = sBck;
    /*
     * code id 0-9 & 15 are valid for 12 bits, code id 8 is unrealizable for 
     */
    /*
     * 8 bits, 6,7 are more bits out than the backup operator 
     */

    fprintf (debug_handle, "Psi Histogram ");

    if (deprs.n == 8) {
      for (loop = 0; loop < 16; loop++) {       /* 8,9,10,11,12,13,14 */
        sum += arHist_Psi[loop];
        if ((loop > 7) && (loop != 15)) {
          if (arHist_Psi[loop] == 0)
            continue;
          if (pErr == sErr)
            pErr += sprintf (pErr, "error code id = ");
          pErr += sprintf (pErr, "%d ", loop);
        }
        if ((loop > 5) && (loop < 8)) { /* 6 & 7 */
          if (arHist_Psi[loop] == 0)
            continue;
          if (pWrn == sWrn)
            pWrn += sprintf (pWrn, "warning code id = ");
          pWrn += sprintf (pWrn, "%d ", loop);
        }
      }
      if (arHist_Psi[15] != 0)
        pBck += sprintf (pBck, "Psi3,1=%dx", arHist_Psi[15]);
    } else if (deprs.n == 12) {
      for (loop = 0; loop < 16; loop++) {
        sum += arHist_Psi[loop];
        if ((loop > 9) && (loop != 15)) {
          if (arHist_Psi[loop] == 0)
            continue;
          if (pErr == sErr)
            pErr += sprintf (pErr, "error code id = ");
          pErr += sprintf (pErr, "%d ", loop);
        }
      }
      if (arHist_Psi[loop] == 9)
        pWrn += sprintf (pWrn, "warning code id = 9 ");
      if (arHist_Psi[15] != 0)
        pBck += sprintf (pBck, "Psi3,1=%dx", arHist_Psi[15]);
    } else {
      fprintf (debug_handle, "error - invalid sample width of %ld\n",
               deprs.n);
    }

    fprintf (debug_handle, "%d blocks ", sum);
    if (pErr > sErr)
      fprintf (debug_handle, "%s", sErr);
    if (pWrn > sWrn)
      fprintf (debug_handle, "%s", sWrn);
    if (pBck > sBck)
      fprintf (debug_handle, "%s", sBck);
    fprintf (debug_handle, "\n");

    for (loop = 0; loop < 16; loop++)
      fprintf (debug_handle, "%5d", loop);
    fprintf (debug_handle, "\n");
    for (loop = 0; loop < 16; loop++)
      fprintf (debug_handle, "%5d", arHist_Psi[loop]);
    fprintf (debug_handle, "\n");
  }

  return nERROR;
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
    rcb[loop] = get_comp_bits (pds, (unsigned) pds->n);

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

  if (DEBUG) {
    fprintf (debug_handle, "get %2d bits: word[%4ld](0x%04lX)=%04X, ",
             num_bits, pds->word_index, pds->word_index,
             pds->indata[pds->word_index]);
    fprintf (debug_handle, "@ bit idx %2ld = ", pds->bit_index);
  }


  pds->inbit_count += num_bits;         /* number of bits taken from the buffer so far */

  if ((pds->inlen * 16) < pds->inbit_count) {
    DONE = 1;
    if (DEBUG)
      fprintf (debug_handle, "done\n");
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
  }                                     /* elihw bits */


  if (DEBUG) {
    fprintf (debug_handle, "0x%X (%d)\n", data, data);
  }


  return data;
}



/* specific to RPWS Cassini Data Compression Chip from ISFLIP (Honeywell ASIC) */
void initialize_isflip_table (DATA_STREAM * pds, unsigned bits)
{

  pds->J = 16;                          /* 16 samples per block */
  pds->N = 11;                          /* psi1,0 - psi1,9 & psi3,0 */
  pds->n = bits;                        /* 8-12 bits per sample */
  pds->ref_sample_len = 12;             /* ref sample length is 12 bits for 8-12 bit modes */
  pds->code_id_len = 4;                 /* code id length is 4 bits for 8-12 bit modes */

  for (pds->K = 0; pds->K < 9; pds->K++)
    pds->psi[pds->K] = -(pds->J / 2) + pds->J * (1 << (pds->K + 1));
  pds->psi[pds->K] = (pds->J / 2) * ((pds->n - pds->K) * (1 << (pds->K + 1)) - 1 - (1 << pds->K));      /* K=9 */
  for (pds->K = 10; pds->K < 15; pds->K++)
    pds->psi[pds->K] = pds->psi[9] + 1; /* back-up operator is psi[15],10-14 are unused */

  return;
}
