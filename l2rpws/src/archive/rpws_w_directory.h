
#ifdef _RPWS_W_DIRECTORY_
char *w_directory[] = {
  "DATA/RPWS_WIDEBAND_FULL",            /* 0 */
  "DATA/RPWS_WAVEFORM_FULL",            /* 1 */
  "DATA/ANCILLARY",                     /* 2 */
  "EXTRAS",                             /* 3 */
  "EXTRAS",                             /* 4 */
  "BROWSE",                             /* 5 */
  "RPWS_WIDEBAND_FULL",                 /* 6 */
  "RPWS_WAVEFORM_FULL",                 /* 7 */
  "HTML",                               /* 8 */
  "DATA/RPWS_RAW_COMPLETE",             /* 9 */
  "script",                             /* 10 */
  "DATA/HOUSEKEEPING",                  /* 11 */
  ""
};
#else
extern char **w_directory;
#endif
