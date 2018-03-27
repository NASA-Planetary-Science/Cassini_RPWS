 /*
  *     General decompression routines
  *
  *     result =        one of the following:   
  *                                     STATUS_COMPRESS_not_my_data
  *                                             do not know this compression
  *                                             method
  *                                     STATUS_COMPRESS_fail
  *                                             catastrophic failure
  *                                     STATUS_COMPRESS_success
  *                                             output contains data
  *                                     STATUS_COMPRESS_more_data
  *                                             decompression requires more
  *                                             mini-packets to complete
  *                                             current compression cycle
  *
  *             ???_decompress (struct MP_buffer*,
  *                                             pointer to input
  *                             struct RPWS_buffer*,
  *                                             pointer to output buffer
  *                             routine-specific-argument...)
  *                                             any extra data required
  *
  *             ???     identifies decompression routine
  *                     UTIL_DCP_decompress
  *                             Sheffield
  *                     UTIL_DCC_decompress
  *                                     arg 3 is BIT MODE (8 or 12)
  *                                     arg 4 is available bytes in RPWS_buffer
  *                             ISFLIP
  *                     UTIL_MFR_decompress
  *                             Iowa
  *                     UTIL_HFR_decompress
  *                             Meudon
  */

long UTIL_DCP_decompress (struct MP_buffer *, struct RPWS_buffer *);
long UTIL_DCC_decompress (struct MP_buffer *, struct RPWS_buffer *, long);
long UTIL_MFR_decompress (struct MP_buffer *, struct RPWS_buffer *);
