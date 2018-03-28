#include <stdlib.h>
#include <stdio.h>
#include <audio/Alib.h>


#define BUFFER_MAX 65536

static Audio *audio;
static char *pSpeaker;
static AGainEntry gainEntry[4];
static SBPlayParams playParams;
static AFileFormat fileFormat;
static AErrorHandler prevHandler;
static AudioAttrMask userAttribMask;
static AudioAttributes userAttribs;
static ATransID xid;
static int useIntSpeaker;
static unsigned long sampleRate;
static unsigned long userRate;
static char *buffer;

static FILE *stdmsg;

 /*
  * error handler for player
  */
long up_handler (Audio * audio, AErrorEvent * err_event)
{
  static char errorbuf[132];

  AGetErrorText (audio, err_event->error_code, errorbuf, 131);
  /*
   * fprintf(stdmsg,"uplay: ERROR EVENT NOTIFICATION: %s\n", errorbuf);
   */
  return AENoError;
}

int up_open (void)
{
  static int first = 1;
  static int inactive = 1;
  long status;
  int amask;
  char server[80] = { NULL };

  fileFormat = AFFUnknown;
  playParams.priority = APriorityNormal;        /* normal priority */
  userRate = 0;

  /*
   *  open audio connection
   */
  if (!first)
    return inactive;
  first = 0;
  stdmsg = stdout;
  audio = AOpenAudio (server, &status);
  if (status)
    return inactive;

  prevHandler = ASetErrorHandler (up_handler);  /* replace default error handler */
  pSpeaker = getenv ("SPEAKER");        /* get user speaker preference */
  if (pSpeaker) {
    useIntSpeaker = ((*pSpeaker == 'i') || (*pSpeaker == 'I'));
  } else {                              /* SPEAKER environment variable not found - use internal speaker */
    useIntSpeaker = 1;
  }

  gainEntry[0].u.o.out_ch = AOCTMono;
  gainEntry[0].gain = AUnityGain;
  gainEntry[0].u.o.out_dst = (useIntSpeaker) ?
    AODTMonoIntSpeaker : AODTMonoJack;

  playParams.gain_matrix.type = AGMTOutput;     /* gain matrix */
  playParams.gain_matrix.num_entries = 1;
  playParams.gain_matrix.gain_entries = gainEntry;
  playParams.play_volume = AUnityGain;  /* play volume */
  playParams.pause_first = False;       /* don't pause */
  playParams.start_offset.type = ATTSamples;    /* start offset 0 */
  playParams.start_offset.u.samples = 0;
  playParams.duration.type = ATTFullLength;     /* play entire sample */
  playParams.loop_count = 0;            /* play sample just once */
  playParams.previous_transaction = 0;  /* no linked transaction */
  playParams.event_mask = ATransCompletedMask;

  inactive = 0;
  return inactive;
}

#define UP_WAIT 0
#define UP_NEW  1
int up_event (int flag)
{
  long status;
  static AEvent event;
  static int up_flag = 0;

  switch (flag) {
   case UP_NEW:
     up_flag = 1;
     break;
   case UP_WAIT:
     if (up_flag) {
       ANextEvent (audio, &event, &status);
       up_flag = 0;
     }
  }
  return up_flag;
}

int up_dumpPa (SBPlayParams * play)
{
  fprintf (stdmsg, "play->gain_matrix.type=%d\n", play->gain_matrix.type);
  fprintf (stdmsg, "play->gain_matrix.num_entries=%d\n",
           play->gain_matrix.num_entries);
  fprintf (stdmsg, "play->gain_matrix.gain_entries=%d\n",
           play->gain_matrix.gain_entries);
  fprintf (stdmsg, "play->play_volume=%d\n", play->play_volume);
  fprintf (stdmsg, "play->pause_first=%d\n", play->pause_first);
  fprintf (stdmsg, "play->start_offset.type=%d\n", play->start_offset.type);
  fprintf (stdmsg, "play->start_offset.u.samples=%d\n",
           play->start_offset.u.samples);
  fprintf (stdmsg, "play->duration.type =%d\n", play->duration.type);
  fprintf (stdmsg, "play->loop_count=%d\n", play->loop_count);
  fprintf (stdmsg, "play->previous_transaction=%d\n",
           play->previous_transaction);
  fprintf (stdmsg, "play->event_mask=%d\n", play->event_mask);
}

int up_dumpAudio (Audio * audio)
{
  fprintf (stdmsg, "audio->fd %d\n", audio->fd);
  fprintf (stdmsg, "audio->proto_major_version %d\n",
           audio->proto_major_version);
  fprintf (stdmsg, "audio->proto_minor_version %d\n",
           audio->proto_minor_version);
  fprintf (stdmsg, "audio->vendor %s\n", audio->vendor);
  fprintf (stdmsg, "audio->vnumber %d\n", audio->vnumber);
  fprintf (stdmsg, "audio->release %d\n", audio->release);
  fprintf (stdmsg, "audio->audio_name %s\n", audio->audio_name);
  fprintf (stdmsg, "audio->byte_order %d\n", audio->byte_order);
  fprintf (stdmsg, "audio->sound_bit_order %d\n", audio->sound_bit_order);
  fprintf (stdmsg, "audio->n_data_format %d\n", audio->n_data_format);
  fprintf (stdmsg, "audio->data_format_list %P\n", audio->data_format_list);
  fprintf (stdmsg, "audio->n_sampling_rate %d\n", audio->n_sampling_rate);
  fprintf (stdmsg, "audio->sampling_rate_list %P\n",
           audio->sampling_rate_list);
  fprintf (stdmsg, "audio->best_audio_attr %P\n", audio->best_audio_attr);
  fprintf (stdmsg, "audio->qlen %d\n", audio->qlen);
  fprintf (stdmsg, "audio->input_sources %d\n", audio->input_sources);
  fprintf (stdmsg, "audio->input_channels %d\n", audio->input_channels);
  fprintf (stdmsg, "audio->output_channels %d\n", audio->output_channels);
  fprintf (stdmsg, "audio->output_destinations %d\n",
           audio->output_destinations);
  fprintf (stdmsg, "audio->max_input_gain %d\n", audio->max_input_gain);
  fprintf (stdmsg, "audio->min_input_gain %d\n", audio->min_input_gain);
  fprintf (stdmsg, "audio->max_output_gain %d\n", audio->max_output_gain);
  fprintf (stdmsg, "audio->min_output_gain %d\n", audio->min_output_gain);
  fprintf (stdmsg, "audio->max_monitor_gain %d\n", audio->max_monitor_gain);
  fprintf (stdmsg, "audio->min_monitor_gain %d\n", audio->min_monitor_gain);
  fprintf (stdmsg, "audio->simple_player_gm %P\n", audio->simple_player_gm);
  fprintf (stdmsg, "audio->simple_recorder_gm %P\n",
           audio->simple_recorder_gm);
  fprintf (stdmsg, "audio->gm_gain_restricted %d\n",
           audio->gm_gain_restricted);
  fprintf (stdmsg, "audio->sample_rate_hi_tolerance %f\n",
           audio->sample_rate_hi_tolerance);
  fprintf (stdmsg, "audio->sample_rate_lo_tolerance %f\n",
           audio->sample_rate_lo_tolerance);
  fprintf (stdmsg, "audio->alib_major_version %d\n",
           audio->alib_major_version);
  fprintf (stdmsg, "audio->alib_minor_version %d\n",
           audio->alib_minor_version);
  fprintf (stdmsg, "audio->file_format_names %s\n", audio->file_format_names);
  fprintf (stdmsg, "audio->n_file_format_names %d\n",
           audio->n_file_format_names);
  fprintf (stdmsg, "audio->data_format_names %s\n", audio->data_format_names);
  fprintf (stdmsg, "Audio->n_data_format_names %d\n",
           audio->n_data_format_names);
  fprintf (stdmsg, "audio->adefaults %P\n", audio->adefaults);
  fprintf (stdmsg, "audio->block_size %d\n", audio->block_size);
}

int up_dumpSB (SBucket * Bucket)
{
  char *x;
  long *y;
  int i, j;

  fprintf (stdmsg, "SBucket=%P\n", Bucket);
  fprintf (stdmsg, "SBucket.audio_attr.type=%d\n", Bucket->audio_attr.type);
  fprintf (stdmsg, "SBucket.audio_attr.attr.sampled_attr.data_format=%d\n",
           Bucket->audio_attr.attr.sampled_attr.data_format);
  fprintf (stdmsg,
           "SBucket.audio_attr.attr.sampled_attr.bits_per_sample=%d\n",
           Bucket->audio_attr.attr.sampled_attr.bits_per_sample);
  fprintf (stdmsg, "SBucket.audio_attr.attr.sampled_attr.sampling_rate=%d\n",
           Bucket->audio_attr.attr.sampled_attr.sampling_rate);
  fprintf (stdmsg, "SBucket.audio_attr.attr.sampled_attr.channels=%d\n",
           Bucket->audio_attr.attr.sampled_attr.channels);
  fprintf (stdmsg, "SBucket.audio_attr.attr.sampled_attr.interleave=%d\n",
           Bucket->audio_attr.attr.sampled_attr.interleave);
  fprintf (stdmsg, "SBucket.audio_attr.attr.sampled_attr.duration.type=%d\n",
           Bucket->audio_attr.attr.sampled_attr.duration.type);
  fprintf (stdmsg,
           "SBucket.audio_attr.attr.sampled_attr.duration.u.samples=%d\n",
           Bucket->audio_attr.attr.sampled_attr.duration.u.samples);
  fprintf (stdmsg, "SBucket.audio_data=%P\n", Bucket->audio_data);
  fprintf (stdmsg, "SBucket.max_length=%d\n", Bucket->max_length);

  y = (long) Bucket;
  for (j = 0; j < 5; j++) {
    for (i = 0; i < 6; i++) {
      fprintf (stdmsg, "%9.8X", *y++);
    }
    fprintf (stdmsg, "\n");
  }

  x = Bucket->audio_data;
  for (j = 0; j < 4; j++) {
    for (i = 0; i < 16; i++) {
      fprintf (stdmsg, "%3.2X", (unsigned char) x[j * 16 + i]);
    }
    fprintf (stdmsg, "\n");
  }

}
int up_play (SBucket * bucket)
{
  long status;
  AEvent event;

  xid = APlaySBucket (audio, bucket, &playParams, &status);
  /*
   * fprintf(stdmsg,"%X=APlaySBucket(%P,%P,%P,%d)\n",
   * xid, audio, bucket, playParams, status);
   */
  status = ACheckEvent (audio, &event, NULL);
  if (!status)
    up_event (UP_NEW);
  /*
   * else
   * fprintf(stdmsg,"ACheckEvent %d\n",
   * event.type);
   */
  return;
}
int uplay_wait (void)
{
  up_event (UP_WAIT);
  return;
}
int uplay_file (char *afile)
{
  long status;
  static SBucket *fileBucket = NULL;

  if (up_open ())
    return 0;
  up_event (UP_WAIT);
  if (fileBucket)
    ADestroySBucket (audio, fileBucket, &status);
  fileBucket = NULL;
  fileBucket = ALoadAFile (audio,
                           afile,
                           fileFormat, userAttribMask, &userAttribs, &status);
  playParams.event_mask = ATransCompletedMask;
  if (!status)
    up_play (fileBucket);
  return 1;
}
int uplay_8_bit (char *buf, int length)
{
  long status;
  int i;
  long mask;
  AudioAttributes attr;
  static SBucket *dataBucket = NULL;
  static int first = 1;

  if (first) {
    first = 0;
    dataBucket = malloc (sizeof (SBucket) + 32);
    memset (dataBucket, 0xFF, sizeof (SBucket) + 32);
    dataBucket->audio_data = malloc (BUFFER_MAX);
    dataBucket->audio_attr.type = ATSampled;
    dataBucket->audio_attr.attr.sampled_attr.data_format = ADFLin8;
    dataBucket->audio_attr.attr.sampled_attr.bits_per_sample = 8;
    dataBucket->audio_attr.attr.sampled_attr.sampling_rate = 8000;
    dataBucket->audio_attr.attr.sampled_attr.channels = 1;
    dataBucket->audio_attr.attr.sampled_attr.interleave = 1;
    dataBucket->audio_attr.attr.sampled_attr.duration.type = ATTSamples;
  }
  if (up_open ())
    return 0;
  up_event (UP_WAIT);

  dataBucket->audio_attr.attr.sampled_attr.duration.u.samples = length;
  dataBucket->max_length = BUFFER_MAX;
  i = APutSBucketData (audio, dataBucket, 0, buf, length, &status);
  fprintf (stdmsg, "%X=APutSBucketDATA(%P,%P,%d,%P,%d,%d)\n", i,
           audio, dataBucket, 0, buf, length, status);
  playParams.event_mask = ATransCompletedMask;
  up_play (dataBucket);
  return 1;
}
int uplay_12_bit (short unsigned *buf, int length)
{
  long status;
  int i;
  long mask;
  AudioAttributes attr;
  static SBucket *dataBucket = NULL;
  static int first = 1;

  if (first) {
    first = 0;
    dataBucket = malloc (sizeof (SBucket));
    memset (dataBucket, 0, sizeof (SBucket));
    dataBucket->audio_data = malloc (BUFFER_MAX);
    dataBucket->audio_attr.type = ATSampled;
    dataBucket->audio_attr.attr.sampled_attr.data_format = ADFLin16;
    dataBucket->audio_attr.attr.sampled_attr.bits_per_sample = 16;
    dataBucket->audio_attr.attr.sampled_attr.sampling_rate = 8000;
    dataBucket->audio_attr.attr.sampled_attr.channels = 1;
    dataBucket->audio_attr.attr.sampled_attr.interleave = 1;
    dataBucket->audio_attr.attr.sampled_attr.duration.type = ATTSamples;
  }
  if (up_open ())
    return 0;
  up_event (UP_WAIT);

  dataBucket->audio_attr.attr.sampled_attr.duration.u.samples = length;
  dataBucket->max_length = length * 2;
  memcpy (dataBucket->audio_data, buf, length * 2);
  up_play (dataBucket);
  return 1;
}
