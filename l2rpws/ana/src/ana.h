#ifndef _ANA_H
#define _ANA_H

#include "../lib/XmRPWS/XmRPWS.h"
#include <instrument.h>
#include "CTools.h"

#define MAX_DSP_DATA 8192
#define MAX_DSP_AVG_DATA 4096

#define Analysis_Raw		0x00000001
#define Analysis_Time	0x00000002
#define Analysis_Mag		0x00000004
#define Analysis_Phs		0x00000008
#define Analysis_Avg		0x00000010
#define Analysis_Spg		0x00000020
 
#define po_list_max	8
 
typedef struct{
  char top[64],bottom[64],left[64];		/* plot labels */
  char bl1[32],bl2[32],bl3[32];			/* Aux. data */
  char br1[32],br2[32],br3[32];			/* Aux. data */
  int color_index;				/* plot color */
  int32_t length;					/* in data length */
  float x[MAX_DSP_DATA],y[MAX_DSP_DATA];	/* x,y data points */
  float xl,xr,yb,yt;				/* pg_window attributes */
  }PGPLOT;

typedef struct{
  unsigned char phase_reference;
  unsigned char mp_header[16];
  int mp_id,mp_length,mp_rti,mp_size,mp_segment;
  int32_t data_length;
  float data[MAX_DSP_DATA];
  double dc_offset,vrms,fft_peak_magnitude,fft_peak_frequency,fft_peak_phase;
  PGPLOT time;		/* x - sample number, y - counts */
  PGPLOT fft;		/* x - real, y - imaginary 	 */
  PGPLOT mag;		/* x - frequency, y - magnitude  */
  PGPLOT phs;		/* x - frequency, y - phase 	 */
  PGPLOT avg;		/* x - frequency, y - magnitude  */
  }DSPDATA;

    

/*---------------------------------------------*/
/*---------------------------------------------*/
/*---------------------------------------------*/
typedef struct{
  char lnprn[64],psprn[64],filename[64];
  char data_file;
  int pending_command;
  int kbhit,step,data_direction; 
  Widget window;
  int numXpanel,numYpanel;
  int log_inst_type,log_mode,log_immediately;
  int quit;
  unsigned char mfr_raw[224],mfr_valid;
  unsigned char lfdr_raw[32],lfdr_valid;
  int status_pending;
  int32_t sclk_seconds,rti;
  int counter;
  
  int32_t input_filter;
  /* tmp */
  char do_wave_normal_vector;
  float WaveNormalFrequency;
  
  }UserInterface;

typedef struct line_object{
  int color_index,line_style,graph_symbol;	/* ls=0 => use points */
  int32_t length;					/* number of data points */
  float *x,*y;					/* x,y data points */
  float xl,xr,yb,yt;				/* xmin,xmax,ymin,ymax */
  }LINE_OBJECT;

typedef struct spectrogram_object{
  char refresh;
  char show_missing_packets,show_extra_packets;
  unsigned char *image;	
  int xdim,ydim;		/* x,y array of spectrogram */
  int image_index;		/* array index */
  uint32_t *evtclk;	/* Event Clock in Seconds */
  int last_pixel;		/* X coordinate of previous capture */
  int resolution;		/* 1 pixel = 8 seconds */
  int period;			/* Capture period, 0 = none */
  int grace;
  int color;
  float color_bar_min,color_bar_max;	/* Minimum and Maximum values for the Color Bar or Gray Scale */
  uint32_t start_time;	/* first event time in seconds */
  int32_t type;
  }SPECTROGRAM_OBJECT;

typedef struct panel_object{
  int32_t list_count;
  uint32_t instrument_type[po_list_max];
  uint32_t analysis_type[po_list_max];
  void (*display_function[po_list_max])();
  }PANEL_OBJECT;
  
typedef struct{
  Widget pgplot_window;
  char *device_name;
  int device_id;
  int Xpanel,Ypanel;
  int num_modes;
  LINE_OBJECT *lo[10];
  int32_t types[10];
  int32_t analysis[10];
  char update;		
  }DisplayPanel;

/* */

/*---------------------------------------------*/
/*---------------------------------------------*/
/*---------------------------------------------*/





typedef struct{
uint32_t instrument,analysis;		
void (*interperator)();
}InstrumentAnalysis;


/* The data object of CPGPlotWindow, pData.  */
typedef struct{
uint32_t instrument;
uint32_t analysis;
CPGLine *pPlotSetup;			/* Setup for each individual line, (InstrumentAnalysis*)pPlotSetup->pData */

}WindowSetup;




/*---------------------------------------------*/
/*---------------------------------------------*/
/*---------------------------------------------*/
  
/*
// int mp_convert_format(struct RPWS_buffer *in,DSPDATA *out);
*/
int get_time_series(DSPDATA *in);
int get_time_series_vrms(DSPDATA *in);

int get_fft_series(DSPDATA *in);
int get_fft_sliding_average(DSPDATA *in);

int select_window_panel(DSPDATA *in);
void update_display(DSPDATA *in);
void update_display_panel(DSPDATA *in);
void update_display_window(DSPDATA *par[]);

double get_quanitization_levels(DSPDATA *in);
double get_frequency(DSPDATA *in);
double get_instrument_calibration(DSPDATA *in);

void clean_up_curses(int sig_number);		/* cleans up after curses and exits, argument is not processed */
void clean_up_pgplot(void);		/* cleans up after curses and exits, argument is not processed */
void clean_up_ana(void);		/* cleans up after curses and exits, argument is not processed */
void analog_syntax( void );		/* displays the command line syntax */

int get_user_input(DSPDATA *par[]);
int get_keyboard_hit(int blocking);
int get_command_file_input(int blocking);

/* Where are these now ??? */
/*void print_panel(DSPDATA *par[],char *mode);*/
void print_window(DSPDATA *par[],char *mode);	
void put_print_data(PGPLOT *x,unsigned option);
void put_print_auxdata(PGPLOT *x,unsigned option);
void dump_screen(DSPDATA *par[],char *mode);	

/* --------------------------------- */
char *logging_manager(DSPDATA *x,int32_t instrument,int32_t mode,char *filename);


/* GLOBAL DECLARATIONS FOR COMMAND LINE ARGUMENTS */
int CMDLINE_FFT_SERIES,CMDLINE_FFT_SLIDING_AVERAGE;
int CMDLINE_TIME_SERIES,CMDLINE_SINGLE_SEQUENCE,CMDLINE_SLEEP;
int CMDLINE_FILTER,CMDLINE_INPUT_REFERRED,CMDLINE_PHASE,CMDLINE_PHASE_REF;
int CMDLINE_ABSOLUTE,CMDLINE_OUTTEXT;
int32_t CMDLINE_NOISE;

/* under construction */
/*int display_spectogram(DSPDATA *in);*/
void display_spectrogram_object(INSTRUMENT *inst,LINE_OBJECT *lo);
SPECTROGRAM_OBJECT *create_spectrogram_object(int32_t type,int32_t xdim,int32_t ydim);
void initialize_spectrogram_object(SPECTROGRAM_OBJECT *so);
int add_to_list_spectrogram_object(SPECTROGRAM_OBJECT *so);
SPECTROGRAM_OBJECT *find_current_spectrogram_object(int32_t type);
void set_spectrogram_objects(int32_t type,SPECTROGRAM_OBJECT *source);

PANEL_OBJECT *create_panel_object(void);
void initialize_panel_object(PANEL_OBJECT *po);


void display_line_object(LINE_OBJECT *lo);
void display_lp_raw(int32_t length,float *lp);
void display_mfr(int32_t length,float *mfr);
void display_wfr_raw(int32_t length,float *wfr);
void display_wbr_raw(int32_t length,float *wbr);
void display_wbr_magnitude(INSTRUMENT *wfr);
void display_wfr_magnitude(INSTRUMENT *wfr);
LINE_OBJECT *get_instrument_magnitude(INSTRUMENT *inst);

/* Start of Printing Functions and ect. (Under construction) */
void print_panel(uint32_t data_type);
char* PlotTitle(INSTRUMENT *x);






CPGLine* NewLine2List(CPGLine *pList,uint32_t data_type,uint32_t analysis);
CPGLine* BuildLineObject(uint32_t inst,uint32_t anal);
int DestroyLineObject(CPGLine *pLine);

CPGAxis* BuildAxisObject(uint32_t inst,uint32_t anal);
CPGLabel* BuildLabelObject(uint32_t inst);

/*
CPGAxis* NewAxis2List(CPGAxis *pList,char *x_opt,char *y_opt,int x_sub,int y_sub,float x_tick,float y_tick);
CPGLine* BuildAxisObject(uint32_t inst,uint32_t anal);

CPGLine* NewLabel2List(CPGAxis *pList,uint32_t data_type,uint32_t analysis);
CPGLine* BuildLabelObject(uint32_t inst,uint32_t anal);
*/


	
void OnDrawRaw(CPGLine *x);
void OnDrawMag(CPGLine *x);
void DumpCPGLine(CPGLine *x);
void DumpCPGLineList(CPGLine *x);

#endif
