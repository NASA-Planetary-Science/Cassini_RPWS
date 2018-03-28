
        /************************************************
	 *	DUST housekeeping			*
	 *	Dump dust micro packets			*
	 *	(i.e. DUST housekeeping)		*
	 ************************************************/
if (dust_data_flag) {
  int flag = 0;
  unsigned short header;
  unsigned short address;
  unsigned static char previous[8];

  header = msb_short (archive->time_series.word_sample[0]);
  address = msb_short (archive->time_series.word_sample[2]);
  if ((header == 0xD00D) && (address == 0x27D0)) {
    fprintf (stdout, "dust  ");
    fprintf (stdout, "data set count:%5d  ",
             msb_short (archive->time_series.word_sample[3])
             & 0xFFFF);
    for (i = 0; i < 8; i++) {
      if (archive->time_series.byte_sample[i + 8] != previous[i])
        flag |= 1 << i;
      previous[i] = archive->time_series.byte_sample[i + 8];

      fprintf (stdout, "%d%s:%3d ",
               i * 10,
               (flag & i << i) ? "DB" : "dB",
               archive->time_series.byte_sample[i + 8]
               & 0xFF);
    }
    fprintf (stdout, "\n");
  }
}
