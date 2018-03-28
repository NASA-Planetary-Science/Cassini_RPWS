#define HFR_XLATE_HF1 1
#define HFR_XLATE_HF2 0
enum
{
  HFR_MODE_ANALYSIS = 1,
  HFR_MODE_MILLISECOND = 2,
  HFR_MODE_SOUNDER = 3,
  HFR_MODE_CALIBRATE = 4,
  HFR_MODE_DUMP = 5
} HFR_MODE;

enum
{
  HFR_RECEIVER_ABC = 0,
  HFR_RECEIVER_H1 = 1,
  HFR_RECEIVER_H2 = 2
} HFR_RECEIVER;

struct HFR_STATUS
{
  int hfr_mode;
  int hfr_event_clock;
  int hfr_event_fine;
  int hfr_event_duration;
  int ABC_Ex_antenna;
  int ABC_Ez_antenna;
  int H1_Ex_antenna;
  int H1_Ez_antenna;
  int H2_Ex_antenna;
  int H2_Ez_antenna;
  int Sounder_event_clock;
  int Sounder_event_fine;
  int Sounder_event_duration;
  int Calibrate_event_clock;
  int Calibrate_event_fine;
  int Calibrate_event_duration;
  int Sounder_antenna;
  int MS_event_clock;
  int MS_event_fine;
  int MS_event_duration;
  int MS_antenna;
  int MS_receiver;
  int MS_frequency;
};

#ifndef _rpws_hfr_status_
struct HFR_STATUS *rpws_hfr_status (struct ARCHIVE_TIME_SERIES *archive,
                                    struct RPWS_buffer *buffer);
char *rpws_hfr_xlate (int hfr_xlate);
int rpws_hfr_xlate_center_freq (int hfr_xlate);
int rpws_hfr_sound_valid (struct RPWS_buffer *buffer);
int rpws_hfr_cal_valid (struct RPWS_buffer *buffer);
int rpws_hfr_dump_valid (struct RPWS_buffer *buffer);

#define _rpws_hfr_status_
#endif
