
/*
(c)Copyright 1992 Hewlett-Packard Co.,  All Rights Reserved.
*/

/*
 * player - command-line audio file player
 *   Aug. 28 1991
 *    by three unknown, unsung audio programmers
 *     (well, only two are unsung)
 */

#include <stdlib.h>
#include <stdio.h>
#include <audio/Alib.h>
#include "autils.h"

/*
 * Invocation:
 *   player <fname.format>     (audio format is determined by .format)
 *   player <fname> <-format>  (audio format is specified by -format)
 *
 * You may also specify a server and a loop count.
 *
 * The format_switch names are the same as the file extensions, so:
 *   player growl -l16      plays file "growl" with format linear 16
 *   player growl.l16       plays file "growl.l16" with format linear 16
 *
 *  You may add new file formats to player, provided you can extract
 * raw data corresponding to one of the raw data types supported from the
 * new file format.
 *
 *   The currently supported raw file formats are:
 *  al      Alaw 8-bit
 *  u       Mulaw 8-bit 
 *  l8      linear 8-bit, value 0 is minimum value
 *  lo8     linear offset 8-bit, value 128 is minimum value
 *  l16     linear 16-bit
 *
 * To add new file format types to player, you must:
 *  1. Add the format type to name2format in autils.c & autils.h
 *  2. add code to extract raw data from the file and convert it to a
 *     raw data format we recognize, and create a sound bucket from the
 *     converted raw data
 *
 *  We have added one user-defined type, "spl", which is just another name
 * for linear offset 8-bit, as a simple example.
 *
 * The environment variable SPEAKER is used to select the "internal" or "external"
 * speaker for playback.  The default is internal speaker.
 */

/*
 * command line switches (not including format switches)
 */


/*
 * error handler for player
 */

#ifdef __STDC__
long myHandler (Audio * audio, AErrorEvent * err_event)
#else
long myHandler (audio, err_event)
Audio *audio;
AErrorEvent *err_event;
#endif
{
  /*
   * printAudioError( audio, "Ooops", err_event->error_code );
   */
  return 1;
}

/****************************************************************************
 *
 *   player main program
 */

#ifdef __STDC__
int player (char *afile)
#else
int player (afile)
char *afile;
#endif
{
  long status;
  SBucket *pSBucket;
  SBPlayParams playParams;
  AGainEntry gainEntry[4];
  ATransID xid;
  char server[80], c;
  int arg;
  AEvent event;
  AFileFormat fileFormat;
  Audio *audio;
  int n, switchCode, loopCount;
  char *pSpeaker;
  unsigned long sampleRate, userRate;
  AudioAttributes *pBestAttribs, userAttribs;
  AudioAttrMask userAttribMask;
  AErrorHandler prevHandler;            /* pointer to previous handler */
  int useIntSpeaker;


  /*
   * process command line switches
   */
  fileFormat = AFFUnknown;
  server[0] = '\0';
  loopCount = 0;
  playParams.priority = APriorityNormal;        /* normal priority */
  userRate = 0;

  /*
   *  open audio connection
   */
  audio = AOpenAudio (server, &status);
  if (status) {
    printAudioError (audio, "Open audio failed", status);
    exit (1);
  }

  /*
   * replace default error handler 
   */
  prevHandler = ASetErrorHandler (myHandler);

  /*
   * initialize the audio attributes structure & mask
   */
  userAttribMask = 0;
  if (userRate) {
    userAttribMask |= ASSamplingRateMask;
    userAttribs.attr.sampled_attr.sampling_rate = userRate;
  }

  /*
   *  Load the audio file into a sound bucket
   */
  if (fileFormat < USER_FORMAT_BASE) {
    /*
     * file is in a format Alib already understands, just load it
     */
    pSBucket =
      ALoadAFile (audio, afile, fileFormat, userAttribMask, &userAttribs,
                  NULL);
  } else {
    /*
     * file is in a user-defined format
     */
    switch (fileFormat) {

     case USER_FORMAT_spl:
       /*
        * spl is just another name for linear offset 8, so load it 
        */
       fileFormat = AFFRawLin8Offset;
       pSBucket =
         ALoadAFile (audio, afile, fileFormat, userAttribMask, &userAttribs,
                     NULL);
       break;

       /*
        * STEP 4. ADD USER-DEFINED FILE FORMAT CASES HERE 
        */

     default:
       /*
        * unrecognized user format 
        */
       fprintf (stderr, "%s has an unrecognized format!/n", afile);
       exit (1);
       break;
    }
  }

  /*
   * setup the playback parameters
   */
  pSpeaker = getenv ("SPEAKER");        /* get user speaker preference */
  if (pSpeaker) {
    useIntSpeaker = ((*pSpeaker == 'i') || (*pSpeaker == 'I'));
  } else {
    /*
     * SPEAKER environment variable not found - use internal speaker 
     */
    useIntSpeaker = 1;
  }
  gainEntry[0].u.o.out_ch = AOCTMono;
  gainEntry[0].gain = AUnityGain;
  gainEntry[0].u.o.out_dst =
    (useIntSpeaker) ? AODTMonoIntSpeaker : AODTMonoJack;

  playParams.gain_matrix.type = AGMTOutput;     /* gain matrix */
  playParams.gain_matrix.num_entries = 1;
  playParams.gain_matrix.gain_entries = gainEntry;
  playParams.play_volume = AUnityGain;  /* play volume */
  playParams.pause_first = False;       /* don't pause */
  playParams.start_offset.type = ATTSamples;    /* start offset 0 */
  playParams.start_offset.u.samples = 0;
  playParams.duration.type = ATTFullLength;     /* play entire sample */
  playParams.loop_count = loopCount;    /* play sample just once */
  playParams.previous_transaction = 0;  /* no linked transaction */
  playParams.event_mask = 0;            /* don't solicit any events */

  /*
   * play the sound bucket
   */
  xid = APlaySBucket (audio, pSBucket, &playParams, NULL);

  /*
   * set close mode to prevent playback from stopping 
   *  when we close audio connection
   */
  ASetCloseDownMode (audio, AKeepTransactions, &status);

  /*
   *  That's all, folks!
   */
  ADestroySBucket (audio, pSBucket, &status);
  ACloseAudio (audio, &status);
  return 0;
}
