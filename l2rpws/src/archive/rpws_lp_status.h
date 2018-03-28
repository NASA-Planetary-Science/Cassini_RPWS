enum
{
  LP_MODE_UNKNOWN = 0,
  LP_MODE_DENSITY = 1,
  LP_MODE_SWEEP = 2,
} LP_MODE;

enum
{
  LP_RECEIVER_SPHERE = 0,
  LP_RECEIVER_CYL_PLUS = 1,
  LP_RECEIVER_CYL_MINUS = 2
} LP_RECEIVER;

struct LP_STATUS
{
  int lp_mode;
  int lp_event_clock;
  int lp_event_fine;
  int lp_event_duration;
  int Sweep_antenna;
  int Sweep_event_clock;
  int Sweep_event_fine;
  int Sweep_event_duration;
  int Density_antenna;
  int Density_event_clock;
  int Density_event_fine;
  int Density_event_duration;
};

#ifndef _rpws_lp_status_
struct LP_STATUS *rpws_lp_status (struct ARCHIVE_TIME_SERIES *archive,
                                  struct RPWS_buffer *buffer);
#define _rpws_lp_status_
#endif
