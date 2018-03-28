
        /************************************************
	 *	BFDL housekeeping			*
	 *	Dump bfdl micro packets			*
	 *	(i.e. BFDL housekeeping)		*
	 ************************************************/
if (ipc_data_flag) {
  int flag = 0;
  unsigned short header;
  unsigned short address;
  unsigned static char previous[8];

  header = msb_short (archive->time_series.word_sample[0]);
  address = msb_short (archive->time_series.word_sample[2]);
  if ((header == 0xD00D)) {
    if (address == 0x1170) {
      fprintf (stdout, "ipc   ");
      fprintf (stdout, "Addr:%04X ",
               msb_short (archive->time_series.word_sample[2]));
      fprintf (stdout, "Tx lost:%5d ",
               msb_short (archive->time_series.word_sample[3]));
      fprintf (stdout, "Tx Retry:%5d ",
               msb_short (archive->time_series.word_sample[4]));
      fprintf (stdout, "Tx Abort:%2d ",
               msb_short (archive->time_series.word_sample[5]));
      fprintf (stdout, "Rx Lost:%d ",
               msb_short (archive->time_series.word_sample[6]));
      fprintf (stdout, "Rx F5:%d ",
               msb_short (archive->time_series.word_sample[7]));
    }
    if (address == 0x117A) {
      fprintf (stdout, "ipc   ");
      fprintf (stdout, "Addr:%04X ",
               msb_short (archive->time_series.word_sample[2]));
      fprintf (stdout, "Tx lost:%5d ",
               msb_short (archive->time_series.word_sample[3]));
      fprintf (stdout, "Tx Retry:%5d ",
               msb_short (archive->time_series.word_sample[4]));
      fprintf (stdout, "Tx Abort:%2d ",
               msb_short (archive->time_series.word_sample[5]));
      fprintf (stdout, "Rx Lost:%d ",
               msb_short (archive->time_series.word_sample[6]));
      fprintf (stdout, "Rx F5:%d ",
               msb_short (archive->time_series.word_sample[7]));
      fprintf (stdout, "Rx Abort:%d ",
               msb_short (archive->time_series.word_sample[8]));
    }
    i = archive->fsw_ver;
    if (i)
      fprintf (stdout, " FswV%d.%d ", i / 100, i % 100);
    fprintf (stdout, "\n");
  }
}
