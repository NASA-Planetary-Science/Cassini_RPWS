#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <SpiceUsr.h>

/* Cas Stuff */

/****************************************************************************/
/* Compiled in config file directory locations */

#include "rpws_label.h"
#include "rpws_time_patch.h"


static char *Version = { "V1.1" };

int initialize_label (struct RPWS_LABEL *rpws_label)
{
  int i;
  static char temp[128];

  if (!rpws_label) {
    fprintf (stdout, "NO LABEL\n");
    return 0;
  }
  rpws_label->link = NULL;
  rpws_label->instrument = -1;
  rpws_label->fband = -1;

#ifdef HFR_XLATE
  for (i = 0; i < HFR_XLATE; i++)
    rpws_label->hfr_xlate[i] = 0;
#else
  rpws_label->hfr_xlate = 0;
#endif

  rpws_label->span = -1.0;
  rpws_label->duration = 0.0;
  rpws_label->dataset_size[0] = 0;
  rpws_label->dataset_size[1] = 0;
  rpws_label->sample_count = -1;
  rpws_label->pad_class = -1;
  rpws_label->record_size = 0;
  rpws_label->record_count[0] = -1;
  rpws_label->record_count[1] = -1;
  rpws_label->Label_Line_Pad = 0;
  rpws_label->filehandle = NULL;
  rpws_label->filename = rpws_label->text_area[0];
  rpws_label->filepath1 = rpws_label->text_area[1];
  rpws_label->filepath2 = rpws_label->text_area[2];
  rpws_label->filepath3 = rpws_label->text_area[3];
  rpws_label->thumbname = rpws_label->text_area[4];
  rpws_label->utc_date = rpws_label->text_area[5];
  rpws_label->utc_time = rpws_label->text_area[6];
  rpws_label->sclk_start = rpws_label->text_area[7];
  rpws_label->sclk_stop = rpws_label->text_area[8];
  rpws_label->scet_start = rpws_label->text_area[9];
  rpws_label->scet_start_2 = rpws_label->text_area[10];
  rpws_label->scet_stop = rpws_label->text_area[11];
  rpws_label->ephem_start = rpws_label->text_area[12];
  rpws_label->ephem_stop = rpws_label->text_area[13];

  memset (rpws_label->text_area, 0, sizeof (rpws_label->text_area));

  return 1;
}
static int dump (struct RPWS_LABEL *_label)
{
  fprintf (stdout, "dump _master_label");
  fprintf (stdout, "    %8d = int instrument;         ", _label->instrument);
  fprintf (stdout, "\n");
  fprintf (stdout, "    %8p = char *utc_date;         ", _label->utc_date);
  if (_label->utc_date)
    fprintf (stdout, "%s", _label->utc_date);
  fprintf (stdout, "\n");
  fprintf (stdout, "    %8p = char *utc_time;         ", _label->utc_time);
  if (_label->utc_time)
    fprintf (stdout, "%s", _label->utc_time);
  fprintf (stdout, "\n");
  fprintf (stdout, "    %8p = char *sclk_start;       ", _label->sclk_start);
  if (_label->sclk_start)
    fprintf (stdout, "%s", _label->sclk_start);
  fprintf (stdout, "\n");
  fprintf (stdout, "    %8p = char *sclk_stop;        ", _label->sclk_stop);
  if (_label->sclk_stop)
    fprintf (stdout, "%s", _label->sclk_stop);
  fprintf (stdout, "\n");
  fprintf (stdout, "    %8p = char *scet_start;       ", _label->scet_start);
  if (_label->scet_start)
    fprintf (stdout, "%s", _label->scet_start);
  fprintf (stdout, "\n");
  fprintf (stdout, "    %8p = char *scet_start_2;     ",
           _label->scet_start_2);
  if (_label->scet_start_2)
    fprintf (stdout, "%s", _label->scet_start_2);
  fprintf (stdout, "\n");
  fprintf (stdout, "    %8p = char *scet_stop;        ", _label->scet_stop);
  if (_label->scet_stop)
    fprintf (stdout, "%s", _label->scet_stop);
  fprintf (stdout, "\n");
  fprintf (stdout, "    %8p = char *ephem_start;      ", _label->ephem_start);
  if (_label->ephem_start)
    fprintf (stdout, "%s", _label->ephem_start);
  fprintf (stdout, "\n");
  fprintf (stdout, "    %8p = char *ephem_stop;       ", _label->ephem_stop);
  if (_label->ephem_stop)
    fprintf (stdout, "%s", _label->ephem_stop);
  fprintf (stdout, "\n");

}
int main (int argc, char **argv)
{
	
  struct RPWS_LABEL *label;
  
  char* sMetaFile = getenv("CAS_TIME_KERNELS");
  
  if(sMetaFile == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }

  furnsh_c(sMetaFile);

  label = malloc (sizeof (struct RPWS_LABEL));
  initialize_label (label);
  sprintf (label->scet_start, "2003-001T12:23:00.123");
  sprintf (label->scet_stop, "2003-001T12:43:30.235");
  fprintf (stdout, "WBR\n");
  dump (label);
  label->instrument = RPWS_LABEL_WBR;
  rpws_time_patch (label);
  fprintf (stdout, "\n");
  dump (label);

  label = malloc (sizeof (struct RPWS_LABEL));
  initialize_label (label);
  sprintf (label->scet_start, "1999-301T02:23:00.123");
  sprintf (label->scet_stop, "1999-301T22:43:30.235");
  fprintf (stdout, "WFR\n");
  dump (label);
  label->instrument = RPWS_LABEL_WFR;
  rpws_time_patch (label);
  fprintf (stdout, "\n");
  dump (label);

  return 0;
}
