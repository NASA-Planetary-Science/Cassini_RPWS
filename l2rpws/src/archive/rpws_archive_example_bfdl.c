
        /************************************************
	 *	BFDL housekeeping			*
	 *	Dump bfdl micro packets			*
	 *	(i.e. BFDL housekeeping)		*
	 ************************************************/
if (bfdl_data_flag) {
  int flag = 0;
  unsigned short header;
  unsigned short address;
  static int previous[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  static int current[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  header = msb_short (archive->time_series.word_sample[0]);
  if ((header == 0xC10D)) {
    current[0] = msb_short (archive->time_series.word_sample[2]);
    current[1] = msb_short (archive->time_series.word_sample[3]);
    current[2] = msb_short (archive->time_series.word_sample[4]);
    current[3] = msb_short (archive->time_series.word_sample[5]);
    current[4] = msb_short (archive->time_series.word_sample[6]);
    current[5] = msb_short (archive->time_series.word_sample[7]);

    fprintf (stdout, "bfdl  ");
    fprintf (stdout, "Cmd %s:%5d ",
             (current[0] - previous[0]) ? "CNT" : "cnt", current[0]);
    fprintf (stdout, "Msg %s:%5d ",
             (current[1] - previous[1]) ? "CNT" : "cnt", current[1]);
    fprintf (stdout, "Bad %s:%2d ",
             (current[2] - previous[2]) ? "CNT" : "cnt", current[2]);
    fprintf (stdout, "Bad %s:%2d ",
             (current[3] - previous[3]) ? "CMD" : "cmd", current[3]);
    fprintf (stdout, "WDT %s:%2d ",
             (current[4] - previous[4]) ? "CNT" : "cnt", current[4]);
    fprintf (stdout, "WDT %s:%5d ",
             (current[5] - previous[5] - 1) ? "PKT" : "pkt", current[5]);

    previous[0] = msb_short (archive->time_series.word_sample[2]);
    previous[1] = msb_short (archive->time_series.word_sample[3]);
    previous[2] = msb_short (archive->time_series.word_sample[4]);
    previous[3] = msb_short (archive->time_series.word_sample[5]);
    previous[4] = msb_short (archive->time_series.word_sample[6]);
    previous[5] = msb_short (archive->time_series.word_sample[7]);

    fprintf (stdout, "\n");
  }
}
