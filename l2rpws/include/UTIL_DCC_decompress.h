                              /*
                               * DCCDPRS.H 
                               */

#ifndef _UTIL_DCC_decompress
#define _UTIL_DCC_decompress

#define THETA( x,n ) ((long int)(((x)<(((long int)(1<<n)-1)-x))?(x):(((1<<n)-1)-x)))

/* 			if x < [(2^n-1)-x] then x, else [(2^n-1)-x] 	*/
#define TRUE 	(1)
#define FALSE 	(0)

typedef struct
{
  long bit_index, word_index;
  long Fo, K, J, n, N;                  /* Fo=sum of mapped differences,K=PSI index,J=block size,n=bits/sample,N=number of code options */
  long ref_sample, ref_sample_len, code_id, code_id_len;        /* reference sample length in bits, code id length in bits */
  long psi[16], inlen, outlen, inbit_count, outbit_count;
  unsigned *indata;
  unsigned *outdata;
} DATA_STREAM;

static void initialize_isflip_table (DATA_STREAM * pds, unsigned bits); /* specific to RPWS Cassini Compression Chip */
static void decmp_fs (DATA_STREAM * pds, unsigned *rcb);
static void decmp_ss (DATA_STREAM * pds, unsigned *rcb);
static void decmp_bo (DATA_STREAM * pds, unsigned *rcb);
static void unmap_samples (DATA_STREAM * pds, unsigned *rcb);
static unsigned get_comp_bits (DATA_STREAM * pds, unsigned num_bits);
static long rice_decompress (unsigned *in_buffer, unsigned in_length,
                             struct RPWS_buffer *rpwsout,
                             long buffer_max_bytes, unsigned bits_per_sample);
#endif
