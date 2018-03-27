#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <signal.h>
#include <time.h>
#include <math.h>

#include <Xm/XmAll.h>

#include <cpgplot.h>
#include <XmPgplot.h>

#include "ana.h"
#include <telemetry.h>
#include <instrument.h>
#include "../lib/rpwsdsp/rpwsdsp.h"
#include "../lib/XmRPWS/XmRPWS.h"
#include "CTools.h"


#define ShowBadPackets		0

extern SPECTROGRAM_OBJECT *SpectrogramObject;

extern CPGPlotWindow *MainPlotWindow;
extern CPGPlotWindow *MainPlotWindow2;

extern UserInterface UI;
extern DisplayPanel dsp_pnl[];

static FILE *FFT_Handle;

int do_poynting_vector(INSTRUMENT *x);


/* These Belong Some Where ??? */
static INSTRUMENT *LinkedListHead;

char *allocate_line_object(int32_t length,LINE_OBJECT *lo);
char *deallocate_line_object(LINE_OBJECT *lo);
char *copy_line_object(LINE_OBJECT *source,LINE_OBJECT *destination);



static char *P_TMP_TXT="Temporary Debug";

/*
static int32_t RawLength,FFTlength;
static float RawData[RAW_DATA_LENGTH_MAX],FFTdata[RAW_DATA_LENGTH_MAX],Avg;
*/

float LFDR_CntFrq[]={ 	
	0.195, 0.390, 0.586, 0.781, 0.977, 1.172, 1.367, 1.563, 1.758, 1.953,
	2.148, 2.344, 2.539, 2.734, 2.930, 3.125, 3.320, 3.515, 3.711, 4.004,
	4.599, 5.371, 6.250, 7.227, 8.398, 9.766,11.328,13.184,15.332,17.871,
	20.898,24.316
	};	/* Center Frequencies for LFDR bins */

static float LFDR_pbw[32]={
	1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,
	1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,
	1.000,1.000,1.000,1.414,2.000,2.000,2.236,2.236,
	2.654,2.654,3.000,3.162,3.464,3.741,4.123,4.242
	};



/* Linked List Functions */
INSTRUMENT *create_list_element(void);
void destroy_list_element(INSTRUMENT *i);
void add_element(INSTRUMENT *i);
void delete_element(INSTRUMENT *i);
void dump_list_element(FILE *handle,INSTRUMENT *i);

/* Data Handling Functions */
INSTRUMENT *read_packet(void);
INSTRUMENT *data_receiving(INSTRUMENT *x);
void data_processing(INSTRUMENT *x);
INSTRUMENT *display_processing(INSTRUMENT *x);
void data_logging_evtlp(INSTRUMENT *x);

/* Display Functions */
void plot_data(PGPLOT *p);
void unplot_data(PGPLOT *p);
void WindowRefresh(void);
void init_color_table(int type);
void plot_spg_data(INSTRUMENT *x);

void DumpInstrumentTime(INSTRUMENT *x);

INSTRUMENT *find_current_instrument(int32_t type);
SPECTROGRAM_OBJECT *find_current_spectrogram_object(int32_t type);
void copy_spectrogram_object(SPECTROGRAM_OBJECT *source,SPECTROGRAM_OBJECT *destination);


int Find_and_Assign(uint32_t data_type,CPGPlotWindow *pWnd);
CPGLine* DeleteLine2List(CPGLine *pLine);
void OnDrawMag_MFR(CPGLine *pLine);
void OnDrawMag_LFDR(CPGLine *pLine);
float Get_dB_Calibration(INSTRUMENT *x);

void ReadArchive(INSTRUMENT *x);
void WriteArchive(INSTRUMENT *x);
void SpectrogramWarehouse(INSTRUMENT *x);


int work(void)
{
char top[128];
int done=0;				/* work is never done */
INSTRUMENT *pInst;
LINE_OBJECT *lo;
SPECTROGRAM_OBJECT *so;
static int first_time;
int DataType;

InstrumentAnalysis *InstAnal;


  if((pInst=read_packet( )) ){			/* Control Input Data Flow */
    /*dump_list_element(stderr,pInst);/ * */
    if(UI.do_wave_normal_vector)  do_poynting_vector(pInst);
    }
  /*-----------------------------
       Control Data Analysis 
  I.  Averaging and Logging should not be suppressed
  II.  Displays
  -------------------------------*/
  
  if(pInst){
  
/*
DumpInstrumentTime(pInst);
fprintf(stderr,"Top Evt Lp:   ");
StopWatch(STOP_WATCH_START);
*/

    if(IsManagerPanel_CPGPlotWindow(MainPlotWindow)){
    CPGPlotWindow *pWnd;
    WindowSetup *pSetup;
    
      pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,1,1);
      while(pWnd){
        pSetup=(WindowSetup*)pWnd->pUserData;
        if( (pSetup->instrument) & Instrument_Mask & (pInst->data_type)){
          if(Find_and_Assign(pInst->data_type,pWnd)){
/* Get and assign current axis title, Temporary hack */
	    pWnd->pLabel->text=sEquals(PlotTitle(pInst));/* PlotTitle is one static char [128]; */
            cpgbbuf();
            UpdateManagerWindow_CPGPlotWindow(pWnd);
            cpgebuf();
          }/* if a window has an update */
        }
        pWnd=(CPGPlotWindow*)NextListElement(pWnd);
      }/* while pWnd */
    }/* Is Manager */
  }/* If new data */

/*
{
double tmp;
FILE *handle;

  handle=fopen("tmp.bin","wb");
  fwrite(1,sizeof(float),2048,handle);
  fclose(handle);

fprintf(stderr,"Btm Evt Lp:     "); 
  tmp=StopWatch(STOP_WATCH_STOP);
}
*/

return done;
}




INSTRUMENT *read_packet(void)
{
char *errmsg;
int pktsts;
int32_t l,lmax,nTmp;
INSTRUMENT *inst,inst_filter;
static int first_time;
static TELEMETRY tlm;
UNSEGMENTED_MINI_PACKET *usmp=&(tlm.packet.usmp);

  switch(UI.data_direction){
    case RecorderDirectionPlayForward :
    case RecorderDirectionStepForward :
      pktsts=get_next_packet(&tlm);
      break;
    case RecorderDirectionPlayRewind :
    case RecorderDirectionStepRewind :
      pktsts=get_previous_packet(&tlm);
      break;
    case RecorderDirectionFastForward :
      for(l=0;l<10;l++)  pktsts=get_next_packet(&tlm);
      break;
    case RecorderDirectionRewind :
      for(l=0;l<10;l++)  pktsts=get_previous_packet(&tlm);
      break;
    case RecorderDirectionStop :
    default :
      pktsts=PACKET_READ_HOLD;
      break;
    }
  if(PACKET_READ_SUCCESS!=pktsts){
/*   fprintf(stderr,"No Packets Available %d\n",pktsts);/ **/
    delay_seconds(0.125);
    return NULL;
    }

  
/* filter incomming telemetry ??? */

  
  
  ExtractMiniPacketStatus(&tlm,&inst_filter);     /* get some status */
  /* Update Fast Forward Status and return or set filter for every Nth packet */

  /* There is something weird going on with reading backwards through the
  	/data/cassini/data_flt/t970815.u08 file langmuir probe
  	48 07 0E E6 00 01,  upper length bounds seems to solve this ???
  	Could it be a natural alignment problem?
  	I think there are stim packets getting mixed into the receiver stream.
  */

  if( (inst_filter.raw.length<=0) || (inst_filter.raw.length>64000) ){
    fprintf(stderr,"Time:%08X Length:%08X\n",inst_filter.cdsclk.seconds,
            inst_filter.raw.length);
    dump_list_element(stderr,&inst_filter);/* */
    return  NULL;
  }
    
  /*--------------------------------
       Mini-Packet Filtering  
  I.  Type
  II.  Every Nth Packet
  III.  Time        
  --------------------------------*/
  			
  if(!(inst_filter.data_type&UI.input_filter))
    return NULL;
  
  /*--------------------------------			
      Find Current Raw Data Buffer   
  --------------------------------*/
  if((inst=find_current_instrument(inst_filter.data_type))==NULL){
    if(ShowBadPackets)
      fprintf(stderr,"Can't Handle Packet %s %s\n",
              get_receiver_string(inst->data_type),
              get_antenna_string(inst->data_type));
    return NULL;
    }


  /* May Have to Implement my own Memory Manager */

  /* Check Data Buffers for Memory Allocation */
  if(inst->raw.length!=inst_filter.raw.length){	
    free(inst->raw.data);       /* Realloacate Data Buffer for new length */
    inst->raw.length=inst_filter.raw.length;	
    nTmp=sizeof(float) * (inst->raw.length+1); 
    if( (inst->raw.data=(float*)malloc(nTmp))==NULL ){
      fprintf(stderr,"Unable to malloc() new raw data\nInst:%s Time:%X " 
              "Len:0x%X\n",get_receiver_string(inst->data_type),
              inst->cdsclk.seconds,inst->raw.length);
      exit(0);
    }
    if( (Instrument_WBR&inst_filter.data_type) || 
        (Instrument_WFR&inst_filter.data_type)){
    /* Realloacate Data Buffers */
      free(inst->fft.real);
      free(inst->fft.imag);
      inst->fft.length=inst_filter.raw.length;
      nTmp=sizeof(float) * (inst->fft.length+1);
      if( (inst->fft.real=(float*)malloc(nTmp))==NULL ){
        fprintf(stderr,"Unable to malloc() new fft real data, %d\n",
                inst->fft.length);
        exit(0);
      }
      nTmp=sizeof(float) * (inst->fft.length+1);
      if( (inst->fft.imag=(float*)malloc(nTmp))==NULL ){
        fprintf(stderr,"Unable to malloc() new fft imag data, %d\n",
                inst->fft.length);
        exit(0);
      }
    }
  } /* if - Reallocate memory buffers */

  ExtractMiniPacketStatus(&tlm,inst);		/* copy status */
  ExtractMiniPacketData(&tlm,inst->raw.data);	/* Raw Data */
    
  /*-------------------------*/
  /*   FFT Supression Here   */
  /*-------------------------*/
  if((Instrument_WBR&inst->data_type) || (Instrument_WFR&inst->data_type)){
    float *wndfnc;
    if((wndfnc=(float*)malloc(sizeof(float)*inst->raw.length))==NULL){
      fprintf(stderr,"Unable to malloc() new fft window data\n");
      exit(0);
      }
    WindowFunction_f(inst->raw.length,wndfnc,3);	
    for(l=0;l<inst->raw.length;l++)
      wndfnc[l]*=2.0*inst->raw.data[l];		/* Normalize for Hanning Window */
    inst->fft.length=RealOnlyFFT_f(inst->raw.length,wndfnc,inst->fft.real,inst->fft.imag);
/*    WriteArchive(inst);*/
    free(wndfnc);
    }
  
  switch(UI.data_direction){
    case RecorderDirectionStepForward :
    case RecorderDirectionStepRewind :
      UI.data_direction=RecorderDirectionStop;
      break;
    default :
      break;
    }

    
return inst;
}




void dump_list_element(FILE *handle,INSTRUMENT *i)
{

  fprintf(stderr,"%-5s %-5s (%04X):: SCLK:%08X RTI:%04X EVT:%08X\n",
  	get_receiver_string(i->data_type),get_antenna_string(i->data_type),
  	i->capture_length,i->cdsclk.seconds,i->rti,i->evtclk.seconds);

return;
}



void WriteArchive(INSTRUMENT *x)
{
uint32_t length;

  if(FFT_Handle==NULL){
    if((FFT_Handle=fopen("FFTArchive.bin","wb"))==NULL){
      fprintf(stderr,"Unable to Create Archive File for FFT data\n");
      return; 
    }
  }
  
  length=sizeof(uint32_t);	/* Length Field */
  length+=sizeof(INSTRUMENT);
  length+=sizeof(float)*x->fft.length;	/* FFT real part */
  length+=sizeof(float)*x->fft.length;	/* FFT imag part */
  fwrite(&length,sizeof(uint32_t),1,FFT_Handle);
  fwrite(x,sizeof(INSTRUMENT),1,FFT_Handle);
  fwrite(x->fft.real,sizeof(float),x->fft.length,FFT_Handle);  
  fwrite(x->fft.imag,sizeof(float),x->fft.length,FFT_Handle);  
  fwrite(&length,sizeof(uint32_t),1,FFT_Handle);
  /* let the system deal with flushing the buffers, fflush(FFT_Handle); */ 
  fflush(FFT_Handle);

ReadArchive(NULL);
return;
}

void ReadArchive(INSTRUMENT *x)
{
uint32_t length;
static FILE *handle;
static char buffer[100000];
INSTRUMENT *pInst;
uint32_t *data_type;

  if(handle==NULL){
    if((handle=fopen("FFTArchive.bin","rb"))==NULL){
      fprintf(stderr,"Unable to Read Archive File\n");
      return; 
    }
  }
  
  fread(&length,sizeof(uint32_t),1,handle);
  fread(buffer,1,length,handle);
  
  pInst=(INSTRUMENT*)&buffer;

return;
#ifdef DEBUG
  length=sizeof(uint32_t);	/* Length Field */
  length+=sizeof(INSTRUMENT);
  length+=x->fft.length;	/* FFT real part */
  length+=x->fft.length;	/* FFT imag part */
  
  fwrite(&length,sizeof(uint32_t),1,FFT_Handle);
  fwrite(x,sizeof(INSTRUMENT),1,FFT_Handle);
  fwrite(x->fft.real,sizeof(float),x->fft.length,FFT_Handle);  
  fwrite(x->fft.imag,sizeof(float),x->fft.length,FFT_Handle);  
  fwrite(&length,sizeof(uint32_t),1,FFT_Handle);
  /* let the system deal with flushing the buffers, fflush(FFT_Handle); */ 

#endif
return;
}

/*--------------------------------------------------------------*/
/*								*/
/*--------------------------------------------------------------*/


INSTRUMENT *find_current_instrument(int32_t x)	/* Data organization and storage */
{
int i;
INSTRUMENT *pinst;
static int first_time;
static struct{
  INSTRUMENT *hfr[4],*lfdr[8],*lp[3],*mfr[4],*wbr[5],*wfr[8];
}CurrentInstrument;

/* This should all be dynamic assignment/initialization */
/* Underconstruction, Separate out by mode as well */

  if(!first_time){
    for(i=0;i<4;i++)
      CurrentInstrument.hfr[i]=CreateNewInstrument( );
    for(i=0;i<8;i++)
      CurrentInstrument.lfdr[i]=CreateNewInstrument( );
    for(i=0;i<3;i++)
      CurrentInstrument.lp[i]=CreateNewInstrument( );
    for(i=0;i<4;i++)
      CurrentInstrument.mfr[i]=CreateNewInstrument( );
    for(i=0;i<5;i++)
      CurrentInstrument.wbr[i]=CreateNewInstrument( );
    for(i=0;i<8;i++)
      CurrentInstrument.wfr[i]=CreateNewInstrument( );
    ++first_time;
    }
    
  switch(x&Instrument_Mask){
    case Instrument_HFR :
    case Instrument_HFRa :
    case Instrument_HFRb :
    case Instrument_HFRc :
    case Instrument_HFRhf1 :
    case Instrument_HFRhf2 :
      pinst=NULL;
      break;
    case Instrument_LFDR :
    case Instrument_LFDRlowBand :
    case Instrument_LFDRhighBand :
      switch(x&Antenna_Mask){
        case Antenna_ExLo :	pinst=CurrentInstrument.lfdr[0];	break;
        case Antenna_EzLo :	pinst=CurrentInstrument.lfdr[1];	break;
        case Antenna_Bx :	pinst=CurrentInstrument.lfdr[2];	break;
        case Antenna_By :	pinst=CurrentInstrument.lfdr[3];	break;
        case Antenna_Bz :	pinst=CurrentInstrument.lfdr[4];	break;
        case Antenna_LMRp :	pinst=CurrentInstrument.lfdr[5];	break;
        case Antenna_LMRm :	pinst=CurrentInstrument.lfdr[6];	break;
        case Antenna_LP :	pinst=CurrentInstrument.lfdr[7];	break;
        default :		pinst=NULL;	break;
        }
      break;
    case Instrument_LPsweep :	pinst=CurrentInstrument.lp[0];	break;
    case Instrument_LPdensity :	pinst=CurrentInstrument.lp[1];	break;
    case Instrument_LP :
    case Instrument_LPanalyzed :
    case Instrument_LPtbd :	pinst=CurrentInstrument.lp[2];	break;
/*
    case Instrument_LP :
    case Instrument_LPsweep :
    case Instrument_LPdensity :
    case Instrument_LPanalyzed :
    case Instrument_LPtbd :
      switch(x&Antenna_Mask){
        case Antenna_LMRp :	pinst=CurrentInstrument.lp[0];	break;
        case Antenna_LMRm :	pinst=CurrentInstrument.lp[1];	break;
        case Antenna_LP :	pinst=CurrentInstrument.lp[2];	break;
        default :		pinst=NULL;			break;
        }
      break;
*/
    case Instrument_MFR :
    case Instrument_MFRband1 :
    case Instrument_MFRband2 :
    case Instrument_MFRband3 :
      switch(x&Antenna_Mask){
        case Antenna_ExLo :	pinst=CurrentInstrument.mfr[0];	break;
        case Antenna_EzLo :	pinst=CurrentInstrument.mfr[1];	break;
        case Antenna_Bx :	pinst=CurrentInstrument.mfr[2];	break;
        case Antenna_Bz :	pinst=CurrentInstrument.mfr[3];	break;
        default :		pinst=NULL;		break;
        }
      break;
    case Instrument_WBR :
    case Instrument_WBRlowBand :
    case Instrument_WBRhighBand :
      switch(x&Antenna_Mask){
        case Antenna_ExLo :	pinst=CurrentInstrument.wbr[0];	break;
        case Antenna_EzLo :	pinst=CurrentInstrument.wbr[1];	break;
        case Antenna_Bx :	pinst=CurrentInstrument.wbr[2];	break;
        case Antenna_LP :	pinst=CurrentInstrument.wbr[3];	break;
        case Antenna_HF :	pinst=CurrentInstrument.wbr[4];	break;
        default :		pinst=NULL;	break;
        }
      break;
    case Instrument_WFR :
    case Instrument_WFRlowBand :
    case Instrument_WFRhighBand :
      switch(x&Antenna_Mask){
        case Antenna_ExLo :	pinst=CurrentInstrument.wfr[0];	break;
        case Antenna_EzLo :	pinst=CurrentInstrument.wfr[1];	break;
        case Antenna_Bx :	pinst=CurrentInstrument.wfr[2];	break;
        case Antenna_By :	pinst=CurrentInstrument.wfr[3];	break;
        case Antenna_Bz :	pinst=CurrentInstrument.wfr[4];	break;
        case Antenna_LMRp :	pinst=CurrentInstrument.wfr[5];	break;
        case Antenna_LMRm :	pinst=CurrentInstrument.wfr[6];	break;
        case Antenna_LP :	pinst=CurrentInstrument.wfr[7];	break;
        default :		pinst=NULL;	break;
        }
      break;
    case Instrument_DUST :
    default :	
      pinst=NULL;		
      break;
    }

return pinst;
}

/*
	This is where all the FFT data is archived.  For now, there will
  only be 6 spectrograms, each with a 256 x 256 resolution.
*/
void SpectrogramWarehouse(INSTRUMENT *x)
{
static float ar[256][256][10];	/* 2.62144MB */


return;
}






int do_poynting_vector(INSTRUMENT *x)
{
int done=0;

  if(x==NULL)
    return done;

    if((Instrument_WFR&x->data_type) && (Antenna_Bz&x->data_type)){
    INSTRUMENT *Bx,*By,*Bz;
    float *Xpynt,*Ypynt,*Zpynt;
    float Bx_mag,By_mag,Bz_mag,Bx_phs,By_phs,Bz_phs,freq,DltFrq;
    int32_t l,index;
      
      if((Bx=find_current_instrument(Instrument_WFR|Antenna_Bx))==NULL)  return done;
      if((By=find_current_instrument(Instrument_WFR|Antenna_By))==NULL)  return done;
      if((Bz=find_current_instrument(Instrument_WFR|Antenna_Bz))==NULL)  return done;
      if((Bx->evtclk.seconds!=By->evtclk.seconds) || (Bx->evtclk.seconds!=Bz->evtclk.seconds))  return done;
      if((Xpynt=(float*)malloc(sizeof(float)*Bx->fft.length))==NULL)  return done;
      if((Ypynt=(float*)malloc(sizeof(float)*Bx->fft.length))==NULL)  return done;
      if((Zpynt=(float*)malloc(sizeof(float)*Bx->fft.length))==NULL)  return done;
      polarization_vectors(Bx->fft.length,Bx->fft.real,Bx->fft.imag,By->fft.real,By->fft.imag,Bz->fft.real,Bz->fft.imag,Xpynt,Ypynt,Zpynt);
fprintf(stderr,"Wave Normal Vector Frequency=%fHz, Bx=%08X By=%08X Bz=%08X\n",UI.WaveNormalFrequency,
	Bx->evtclk.seconds,By->evtclk.seconds,Bz->evtclk.seconds);
      if(Bx->data_type&Instrument_WFRlowBand){
        DltFrq=((1.0/WFR_LOW_BAND_SAMPLING_PERIOD)/2.0)/(float)Bx->fft.length;
        DltFrq*=2.0;
        index=UI.WaveNormalFrequency/DltFrq;
        freq=DltFrq*(float)index;
        }
      else{
        DltFrq=((1.0/WFR_HIGH_BAND_SAMPLING_PERIOD)/2.0)/(float)Bx->fft.length;
        DltFrq*=2.0;
        index=UI.WaveNormalFrequency/DltFrq;
        freq=DltFrq*(float)index;
        }
      for(l=(index>2)?index-3:0;l<index+4;l++){        
        Bx_mag=sqrt(Bx->fft.real[l]*Bx->fft.real[l] + Bx->fft.imag[l]*Bx->fft.imag[l]);
        Bx_mag=20.0*log10(Bx_mag/2047.5);
        By_mag=sqrt(By->fft.real[l]*By->fft.real[l] + By->fft.imag[l]*By->fft.imag[l]);
        By_mag=20.0*log10(By_mag/2047.5);
        Bz_mag=sqrt(Bz->fft.real[l]*Bz->fft.real[l] + Bz->fft.imag[l]*Bz->fft.imag[l]);
        Bz_mag=20.0*log10(Bz_mag/2047.5);
        Bx_phs=atan2(Bx->fft.imag[l],Bx->fft.real[l])*(360.0/(2.0*3.1415));
        By_phs=atan2(By->fft.imag[l],By->fft.real[l])*(360.0/(2.0*3.1415));
        Bz_phs=atan2(Bz->fft.imag[l],Bz->fft.real[l])*(360.0/(2.0*3.1415));
        Bz_phs-=Bx_phs;
        By_phs-=Bx_phs;
        Bx_phs-=Bx_phs;
        fprintf(stderr,"Bin %4d %7.2fHz - Bx %7.2f<%7.2f : By %7.2f<%7.2f : "
                "Bz %7.2f<%7.2f\n\t\tKx=%7.4f Ky=%7.4f Kz=%7.4f\n",
                l, DltFrq*(float)l, Bx_mag, Bx_phs, By_mag, By_phs, Bz_mag, 
                Bz_phs, Xpynt[l], Ypynt[l], Zpynt[l]);
        }
      
      free(Xpynt);
      free(Ypynt);
      free(Zpynt);
      }

return done;
}


void print_panel(uint32_t data_type)
{
char tmpfile[256],device[256],cmd[256];
INSTRUMENT *pInst;

  if((pInst=find_current_instrument(data_type))==NULL)	return;
  
  if(!(pInst->data_type&Instrument_LP))  return;  
  
  strcpy(tmpfile, "ana_temp_ps_XXXXXX");
  mkstemp(tmpfile);
  strcpy(device,tmpfile);
  strcat(device,"/ps");
  
  cpgend();						/* end display */

  cpgbeg(0,device,1,1);				/* start plot */
  cpgask(0);
  cpgbbuf();
  display_lp_raw(pInst->raw.length,pInst->raw.data);
  cpgbox("bcnstv",0,0,"bcnstv",0,0);
  cpgsch(2.0);
  cpgmtxt("t",0.2,0.5,0.5,PlotTitle(pInst));
  cpgsch(1.0);
  cpgebuf();
  cpgend( );						/* end plot */

  sprintf(cmd,"lp -c -onb %s",tmpfile);
  system(cmd);
  
  cpgbeg(0,"/xs",UI.numXpanel,UI.numYpanel);	/* start display */
  cpgask(0);
  init_color_table(1);

return;
}


/* Return a statically allocated string for the plot title */
char* PlotTitle(INSTRUMENT *x)
{
static char top[128];

  if(x==NULL)  return NULL;
  
  switch(x->data_type&Instrument_Mask){
    case Instrument_HFR :
      sprintf(top,"HFR");
      break;
    case Instrument_LFDR:
    case Instrument_LFDRlowBand:
    case Instrument_LFDRhighBand:
      sprintf(top,"LFDR %s %ddB %dpts %X",get_antenna_string(x->data_type),
                    x->gain,x->capture_length,x->evtclk.seconds);
      break;
    case Instrument_LP :
      sprintf(top,"LP %s %dpts %X",get_antenna_string(x->data_type),
                   x->capture_length,x->evtclk.seconds);
      break;
    case Instrument_LPanalyzed :
      sprintf(top,"LP Analyzed %s %dpts %X",get_antenna_string(x->data_type),
                   x->capture_length,x->evtclk.seconds);
      break;
    case Instrument_LPdensity :
      sprintf(top,"LP Density %s %dpts %X",get_antenna_string(x->data_type),
                   x->capture_length,x->evtclk.seconds);
      break;
    case Instrument_LPsweep :
      sprintf(top,"LP Sweep %s %dpts %X",get_antenna_string(x->data_type),
                   x->capture_length,x->evtclk.seconds);
      break;
    case Instrument_LPtbd :
      sprintf(top,"LP TBD %s %dpt %X",get_antenna_string(x->data_type),
                   x->capture_length,x->evtclk.seconds);
      break;
    case Instrument_MFR :
      sprintf(top,"MFR %s %X",get_antenna_string(x->data_type),
                   x->evtclk.seconds);
      break;
    case Instrument_WBR :
    case Instrument_WBRlowBand :
    case Instrument_WBRhighBand :
      sprintf(top,"WBR %s %ddB %dpts %X",get_antenna_string(x->data_type),
                   x->gain,x->capture_length,x->evtclk.seconds);
      break;
    case Instrument_WFR :
    case Instrument_WFRlowBand :
    case Instrument_WFRhighBand :
      sprintf(top,"WFR %s %ddB %dpts %X",get_antenna_string(x->data_type),
                   x->gain,x->capture_length,x->evtclk.seconds);
      break;
    default:
      sprintf(top,"Virtural %s %dpts %X",get_antenna_string(x->data_type),
                   x->capture_length,x->evtclk.seconds);
      break;
  }


return top;
}



/* This will take a combination of receivers, antennas, and analysis 
   in order to build a linked list of attributes for each line drawn.
   Many calls will be made to this, since it will produce all possible
   combinations, which is usually not the desired result.  Order of the
   elements is important, since all like receiver modes will be grouped
   together.
*/
CPGLine* BuildLineObject(uint32_t inst,uint32_t anal)
{
uint32_t Itmp,Atmp,Ltmp;
CPGLine *pList=NULL,*pNew;

  pList=NULL;
  pNew=NULL;					/* Exit function gracefully if inst==0 */
  while(inst&Instrument_LFDR){
    if(      (Itmp=(inst&Instrument_LFDRlowBand))  ); /* this nulls out the antenna bits */	
    else if( (Itmp=(inst&Instrument_LFDRhighBand)) );	
    else; /* error checking */	
    inst&=~Itmp;				/* Zero out the receiver mode bit */
    Itmp|=(inst&Antenna_Mask);			/* Get back the antenna bits */
    while(Itmp&Valid_LFDR_Antenna){
      if(      (Atmp=(Itmp&Antenna_ExLo)) );
      else if( (Atmp=(Itmp&Antenna_EzLo)) );
      else if( (Atmp=(Itmp&Antenna_Bx))   );
      else if( (Atmp=(Itmp&Antenna_By))   );
      else if( (Atmp=(Itmp&Antenna_Bz))   );
      else if( (Atmp=(Itmp&Antenna_LMRp)) );
      else if( (Atmp=(Itmp&Antenna_LMRm)) );
      else if( (Atmp=(Itmp&Antenna_LP))   );
      else; /* error checking */
      Itmp&=~Atmp;				/* zero out the antenna bit */

      Ltmp=anal&(Analysis_Raw|Analysis_Mag);      
      if(Ltmp&Analysis_Raw){
        Ltmp&=~Analysis_Raw;
        pNew=NewLine2List(pList,(Itmp&Instrument_Mask)|Atmp,Analysis_Raw);  /* Specific receiver/antenna combination */
        pList=pNew;			/* solve initial conditions case, pList=NULL; */
        pNew->xl=0.0;  pNew->xr=0.0; pNew->yb=0.0; pNew->yt=255.0; /* xl=xr => autorange */
        pNew->color_index=2;  pNew->line_style=1;
        pNew->ChildsWhims=1;
        pNew->pOnDraw=OnDrawRaw;
      }
      
      if(Ltmp&Analysis_Mag){
        Ltmp&=~Analysis_Mag;
        pNew=NewLine2List(pList,(Itmp&Instrument_Mask)|Atmp,Analysis_Mag);  /* Specific receiver/antenna combination */
        pList=pNew;			/* solve initial conditions case, pList=NULL; */
        pNew->xl=0.0;  pNew->xr=0.0; pNew->yb=-140.0; pNew->yt=-20.0; /* xl=xr => autorange */
        	/* This is a specialized display function and these options cannot be set */
        pNew->color_index=2;  pNew->line_style=1;  
        pNew->ChildsWhims=1;
        pNew->pOnDraw=OnDrawMag_LFDR;	/* This is an example of a specialized plotting function */
      }					/* This does not conform to the frame work. */
    }/* while valid antenna */
  }/* while lfdr */
  
  while(inst&Instrument_LP){
    if(      (Itmp=(inst&Instrument_LPanalyzed)) ); /* this nulls out the antenna bits */	
    else if( (Itmp=(inst&Instrument_LPdensity))  );	
    else if( (Itmp=(inst&Instrument_LPsweep))    );
    else if( (Itmp=(inst&Instrument_LPtbd))      );	
    else; /* error checking */	
    inst&=~Itmp;				/* Zero out the receiver mode bit */
    Itmp|=(inst&Antenna_Mask);			/* Get back the antenna bits */
    while(Itmp&Valid_LP_Antenna){
      if(      (Atmp=(Itmp&Antenna_LMRp)) );
      else if( (Atmp=(Itmp&Antenna_LMRm)) );
      else if( (Atmp=(Itmp&Antenna_LP))   );
      else; /* error checking */
      Itmp&=~Atmp;				/* zero out the antenna bit */
      pNew=NewLine2List(pList,(Itmp&Instrument_Mask)|Atmp,Analysis_Raw);  /* Specific receiver/antenna combination */
      pList=pNew;			/* solve initial conditions case, pList=NULL; */
      pNew->xl=0.0;  pNew->xr=0.0; pNew->yb=0.0; pNew->yt=4095.0; /* xl=xr => autorange */
      pNew->color_index=2;  pNew->line_style=1;
      pNew->ChildsWhims=1;	
      pNew->pOnDraw=OnDrawRaw;
    }
  }/* While(lp); */
  
  while(inst&Instrument_MFR){
  	/* for now there is only one mfr mode and analysis */
    if( (Itmp=(inst&Instrument_MFR)) );
    inst&=~Itmp;				/* Zero out the receiver mode bit */
    Itmp|=(inst&Antenna_Mask);			/* Get back the antenna bits */
    while(Itmp&Valid_MFR_Antenna){
      if(      (Atmp=(Itmp&Antenna_ExLo)) );
      else if( (Atmp=(Itmp&Antenna_EzLo)) );
      else if( (Atmp=(Itmp&Antenna_Bx))   );
      else if( (Atmp=(Itmp&Antenna_Bz))   );
      else; /* error checking */
      Itmp&=~Atmp;				/* zero out the antenna bit */

      Ltmp=anal&(Analysis_Raw|Analysis_Mag);      
      if(Ltmp&Analysis_Raw){
        Ltmp&=~Analysis_Raw;
        pNew=NewLine2List(pList,(Itmp&Instrument_Mask)|Atmp,Analysis_Raw);  /* Specific receiver/antenna combination */
        pList=pNew;			/* solve initial conditions case, pList=NULL; */
        pNew->xl=0.0;  pNew->xr=0.0; pNew->yb=0.0; pNew->yt=255.0; /* xl=xr => autorange */
        pNew->color_index=2;  pNew->line_style=1;
        pNew->ChildsWhims=1;
        pNew->pOnDraw=OnDrawRaw;
      }
      
      if(Ltmp&Analysis_Mag){
        Ltmp&=~Analysis_Mag;
        pNew=NewLine2List(pList,(Itmp&Instrument_Mask)|Atmp,Analysis_Mag);  /* Specific receiver/antenna combination */
        pList=pNew;			/* solve initial conditions case, pList=NULL; */
        pNew->xl=0.0;  pNew->xr=0; pNew->yb=0.0; pNew->yt=255.0; /* xl=xr => autorange */
        	/* This is a specialized display function and these options cannot be set */
        pNew->color_index=2;  pNew->line_style=1;  
        pNew->ChildsWhims=1;
        pNew->pOnDraw=OnDrawMag_MFR;	/* This is an example of a specialized plotting function */
      }					/* This does not conform to the frame work. */
    }/* while valid antenna */
  }/* While(mfr); */
  
 while(inst&Instrument_WBR){
    if(      (Itmp=(inst&Instrument_WBRlowBand))  ); /* this nulls out the antenna bits */
    else if( (Itmp=(inst&Instrument_WBRhighBand)) );	
    else; /* error checking */	
    inst&=~Itmp;				/* Zero out the receiver mode bit */
    Itmp|=(inst&Antenna_Mask);			/* Get back the antenna bits */
    while(Itmp&Valid_WBR_Antenna){
      if(      (Atmp=(Itmp&Antenna_ExLo)) );
      else if( (Atmp=(Itmp&Antenna_EzLo)) );
      else if( (Atmp=(Itmp&Antenna_Bx))   );
      else if( (Atmp=(Itmp&Antenna_LP))   );
      else if( (Atmp=(Itmp&Antenna_HF))   );
      else; /* error checking */
      Itmp&=~Atmp;				/* zero out the antenna bit */

      Ltmp=anal&(Analysis_Raw|Analysis_Mag);      
      if(Ltmp&Analysis_Raw){
        Ltmp&=~Analysis_Raw;
        pNew=NewLine2List(pList,(Itmp&Instrument_Mask)|Atmp,Analysis_Raw);  /* Specific receiver/antenna combination */
        pList=pNew;			/* solve initial conditions case, pList=NULL; */
        pNew->xl=0.0;  pNew->xr=0.0; pNew->yb=0.0; pNew->yt=255.0; /* xl=xr => autorange */
        pNew->color_index=2;  pNew->line_style=1;
        pNew->ChildsWhims=1;
        pNew->pOnDraw=OnDrawRaw;
      }
      
      if(Ltmp&Analysis_Mag){
        Ltmp&=~Analysis_Raw;
        pNew=NewLine2List(pList,(Itmp&Instrument_Mask)|Atmp,Analysis_Mag);  /* Specific receiver/antenna combination */
        pList=pNew;			/* solve initial conditions case, pList=NULL; */
        pNew->xl=0.0;  pNew->xr=0.0; pNew->yb=-140.0; pNew->yt=-20.0; /* xl=xr => autorange */
        pNew->color_index=3;  pNew->line_style=1;
        pNew->ChildsWhims=1;
        pNew->pOnDraw=OnDrawMag;
      }
    }/* while wbr antenna */
 }/* While(wbr); */
 
 while(inst&Instrument_WFR){
    if(      (Itmp=(inst&Instrument_WFRlowBand))  ); /* this nulls out the antenna bits */
    else if( (Itmp=(inst&Instrument_WFRhighBand)) );	
    else; /* error checking */	
    inst&=~Itmp;				/* Zero out the receiver mode bit */
    Itmp|=(inst&Antenna_Mask);			/* Get back the antenna bits */
    while(Itmp&Valid_WFR_Antenna){
      if(      (Atmp=(Itmp&Antenna_ExLo)) );
      else if( (Atmp=(Itmp&Antenna_EzLo)) );
      else if( (Atmp=(Itmp&Antenna_Bx))   );
      else if( (Atmp=(Itmp&Antenna_By))   );
      else if( (Atmp=(Itmp&Antenna_Bz))   );
      else if( (Atmp=(Itmp&Antenna_LMRp)) );
      else if( (Atmp=(Itmp&Antenna_LMRm)) );
      else if( (Atmp=(Itmp&Antenna_LP))   );
      else; /* error checking */
      Itmp&=~Atmp;				/* zero out the antenna bit */
      
      Ltmp=anal&(Analysis_Raw|Analysis_Mag);      
      if(Ltmp&Analysis_Raw){
        Ltmp&=~Analysis_Raw;
        pNew=NewLine2List(pList,(Itmp&Instrument_Mask)|Atmp,Analysis_Raw);  /* Specific receiver/antenna combination */
        pList=pNew;			/* solve initial conditions case, pList=NULL; */
      	pNew->xl=0.0;  pNew->xr=0.0; pNew->yb=0.0; pNew->yt=4095.0; /* xl=xr => autorange */
        pNew->color_index=2;  pNew->line_style=1;
        pNew->ChildsWhims=1;
        pNew->pOnDraw=OnDrawRaw;
      }
      if(Ltmp&Analysis_Mag){
        Ltmp&=~Analysis_Mag;
        pNew=NewLine2List(pList,(Itmp&Instrument_Mask)|Atmp,Analysis_Mag);  /* Specific receiver/antenna combination */
        pList=pNew;			/* solve initial conditions case, pList=NULL; */
      	pNew->xl=0.0;  pNew->xr=0.0; pNew->yb=-140.0; pNew->yt=-20.0; /* xl=xr => autorange */
        pNew->color_index=3;  pNew->line_style=1;
        pNew->ChildsWhims=1;
        pNew->pOnDraw=OnDrawMag;        
      }
    }/* While(wfr antenna) */
 }/* While(wfr mode); */
   
  /* Return the head of the list */
  return (CPGLine*)HeadListElement(pNew);
}

int DestroyLineObject(CPGLine *pLine)
{
  pLine=HeadListElement(pLine);
  while(pLine)
    pLine=DeleteLine2List(pLine);
  
return 1;
}

/* Must be used in PAIRS...Must be used in PAIRS...Must be used in PAIRS */
CPGLine* NewLine2List(CPGLine *pList,uint32_t data_type,uint32_t analysis)
{
InstrumentAnalysis *pInstAnal;
CPGLine *pNew;

  if((pNew=CreateListElement(CPGLine))==NULL){
    fprintf(stderr,"Unable to CreateListElement(), NewLine(%X,%X)\n",data_type,analysis);
    return NULL;
  }  
  if((pNew->pUserData=(void*)calloc(1,sizeof(InstrumentAnalysis)))==NULL){
    fprintf(stderr,"Unable to calloc(1,%zu),NewLine(%X,%X)\n",
			   sizeof(InstrumentAnalysis), data_type, analysis);
    free(pNew);
    return NULL;
  }
  pInstAnal=(InstrumentAnalysis*)pNew->pUserData;
  pInstAnal->instrument=data_type;
  pInstAnal->analysis=analysis;
  AddListElement(pList,pNew);

return pNew;
}

CPGLine* DeleteLine2List(CPGLine *pLine)
{
  if(!pLine)  return NULL;
  
  free(pLine->pUserData);		/* There better not be any allocated data in here */
  pLine=(CPGLine*)DeleteListElement(pLine);

return pLine;
}
/* Must be used in PAIRS...Must be used in PAIRS...Must be used in PAIRS  */



/* Must be used in PAIRS...Must be used in PAIRS...Must be used in PAIRS  */
CPGAxis* NewAxis2List(CPGAxis *pList,char *x_opt,char *y_opt,int x_sub,int y_sub,float x_tick,float y_tick)
{
CPGAxis *pNew;

  if((pNew=CreateListElement(CPGAxis))==NULL){
    fprintf(stderr,"Unable to CreateListElement(), NewAxis2List(%s,%s)\n",x_opt,y_opt);
    return NULL;
  }  
  pNew->xopt=(char*)calloc(17,sizeof(char));
  pNew->yopt=(char*)calloc(17,sizeof(char));
  strcpy(pNew->xopt,x_opt);	strcpy(pNew->yopt,y_opt);
  pNew->nxsub=x_sub;		pNew->nysub=y_sub;
  pNew->xtick=x_tick;		pNew->ytick=y_tick;
  AddListElement(pList,pNew);

return pNew;
}

CPGAxis* DeleteAxis2List(CPGAxis *pAxis)
{
  if(!pAxis)  return NULL;
  
  free(pAxis->pUserData);	/* There better not be any allocated data in here */
  pAxis=(CPGAxis*)DeleteListElement(pAxis);
  
return pAxis;
}
/* Must be used in PAIRS...Must be used in PAIRS...Must be used in PAIRS  */

CPGAxis* BuildAxisObject(uint32_t inst,uint32_t anal)
{
CPGAxis *pAxis=NULL;
/* Test Hack 
return NULL;
Test Hack */

pAxis=NewAxis2List(NULL,"bcnstv","bcnstv",0,0,0,0);
pAxis->color_index=1;
pAxis->char_height=3;
 
return pAxis;
}



/* Must be used in PAIRS...Must be used in PAIRS...Must be used in PAIRS  */
CPGLabel* NewLabel2List(CPGLabel *pList)
{
CPGLabel *pNew;

  if((pNew=CreateListElement(CPGLabel))==NULL){
    fprintf(stderr,"Unable to CreateListElement(), NewLabel2List()\n");
    return NULL;
  }  
  AddListElement(pList,pNew);
  
return pNew;
}

CPGLabel* DeleteLabel2List(CPGLabel *pLabel)
{
  if(!pLabel)  return NULL;
  
  free(pLabel->pUserData);	/* There better not be any allocated data in here */
  pLabel=(CPGLabel*)DeleteListElement(pLabel);

return pLabel;
}
/* Must be used in PAIRS...Must be used in PAIRS...Must be used in PAIRS  */

CPGLabel* BuildLabelObject(uint32_t inst)
{
CPGLabel *pLabel=NULL;

pLabel=NewLabel2List(pLabel);
pLabel->color_index=1;
pLabel->char_height=3;
pLabel->side=sEquals("T");
pLabel->disp=0.2;
pLabel->coord=0.5;
pLabel->fjust=0.5;
pLabel->text=P_TMP_TXT;
 
return pLabel;
}





void DumpCPGLine(CPGLine *x)
{
InstrumentAnalysis *ia=(InstrumentAnalysis*)x->pUserData;

fprintf(stderr,"%p: len=%X Xdata=%p Ydata=%p Inst=%08X Anal=%08X\n",
	x,x->length,x->x,x->y,ia->instrument,ia->analysis);

return;
}

void DumpCPGLineList(CPGLine *x)
{
  while(x){
    DumpCPGLine(x);
    x=(CPGLine*)NextListElement(x);
  }
}

/* 
Look throught the constant line list of the window to see if the data member has a match.
   Assign the data to it, but it does not own it.  ReadPacket() owns all the incomming data
   associated with pLine (pLine->x[],pLine->y[]).  
   
   This function owns the variables pLabes, pBoxes, and pLine, NOT pUserData.

   Presently, only one type of receiver data may be displayed per window.  To
   do mixed  receiver modes put a new variable in the  WindowSetup  structure
   and   implement   its  function   here.  
Example  :  
  Lines  which  may  be simuletanously  displayed  in a  window;  WFR Low Ex raw,magnitude,phase.
  Lines which may NOT be  simuletanously  displayed  in a window; WFR Low Ex raw, MFR Ex raw.
*/

int Find_and_Assign(uint32_t data_type,CPGPlotWindow *pWnd)
{
InstrumentAnalysis *pInstAnal;
CPGLine *pLine;							/* This funtion owns this, but not the data */
const WindowSetup *pWndSetup=(WindowSetup*)pWnd->pUserData;		
CPGLine *pLineSrc;						/* This function does not own this data */
const INSTRUMENT *pInst=find_current_instrument(data_type);

  if(!pWnd || !pWndSetup || !pInst)  return 0;
/* if either pWnd->pLine or pWnd->pUserData->pPlotSrc are NULL this function
    will return and be re-entered gracefully, without memory leaks. 
*/
  
/* Look in the window's perment list to see if it is capable of displaying this
   type of data.
*/
  if((pLineSrc=(CPGLine*)HeadListElement(pWndSetup->pPlotSetup))==NULL)  return 0;
  while(pLineSrc){
    pInstAnal=(InstrumentAnalysis*)pLineSrc->pUserData;
    if(pInstAnal->instrument==data_type)  break;	/* Have a match */
    pLineSrc=(CPGLine*)NextListElement(pLineSrc);
  }
  if(!pLineSrc)  return 0;	/* No match */


   
/* There is an instrument match, so swap out the old stuff with the new stuff.  Here is
   where you decide what is the current display in the window.  You may choose from the
   elements in the pLineSrc list.  This may be carefully rewritten to allow cross reciever
   displays.  The main thing is swapping in and out the appropiate line objects off the
   current display list pWnd->pLine.  I will not do this at this point in time because
   the graphical user interface, because then it becomes a menu nightmare.  
*/

/* Does pLine exist, and if so is it the same receiver/antenna combination as the current data.
   If all these are true, then there is no need for any new allocations.  All instruments in 
   the list will have the same identical type associated with them.  If there is no sign
   of the list, delete whatever is here and put in a new list.  Or stick in your decision
   code here to decide which line may be displayed together. 
*/


  if((pLine=(CPGLine*)HeadListElement(pWnd->pLine))){	/* Get the head of the current display list */
    pInstAnal=(InstrumentAnalysis*)(pLine->pUserData);/* This should not be null */
    if(pInstAnal->instrument!=data_type){		/* Not a exact match, nix all lines here */
      while(pLine)
        pLine=(CPGLine*)DeleteListElement(pLine);	/* This is ok, since it is a copy of the original */
    } 
    pWnd->pLine=NULL;				/* memory no longer exists */
  }/* If pLine exists */
    
/* Now reload from the source list to the current display list */
  if(pWnd->pLine==NULL){
    pLineSrc=(CPGLine*)HeadListElement(pWndSetup->pPlotSetup);
    while(pLineSrc){
      pInstAnal=(InstrumentAnalysis*)pLineSrc->pUserData;	/* This should not be null */
      if(pInstAnal->instrument==data_type){		/* Exact match, add the line */
        pLine=(CPGLine*)EqualsListElement(pLineSrc);	/* Create exact copy not hooked to list */
        AddListElement(pWnd->pLine,pLine);		/* Add to current display list */
        pWnd->pLine=pLine;				/* make valid list head, since first time pWnd->pLine==NULL */
      }
      pLineSrc=(CPGLine*)NextListElement(pLineSrc);
    }
  }/* If reload needed */
  
  
/* Reset the current line display list to the first element.  At this point, 
  the corresponding lines are in the to be displayed list, pList.  Now find and 
  assign the current data, either the raw or FFTed. 
*/
  	
  pLine=pWnd->pLine=(CPGLine*)HeadListElement(pWnd->pLine);
  while(pLine){
    pInstAnal=(InstrumentAnalysis*)pLine->pUserData;
    if(pInstAnal->instrument==data_type){
      if(pInstAnal->analysis&Analysis_Raw){		/* Raw, Time */
        pLine->length=pInst->raw.length;
        pLine->y=pInst->raw.data;
      }
      else{						/* FFT's */
        pLine->length=pInst->fft.length;
        pLine->x=pInst->fft.real;
        pLine->y=pInst->fft.imag;
      }
    }/* If matching data types */
    pLine=(CPGLine*)NextListElement(pLine);
  }/* While pLine */
  	
return 1;
}




void OnDrawRaw(CPGLine *pLine)
{
int i;
uint32_t data_type;
float *pXdata,fXmin,fXmax,fYmin,fYmax;
INSTRUMENT *pInst;
float xl,xr,yb,yt;
uint32_t max_pts;

  if(!pLine)  return;

  data_type=((InstrumentAnalysis*)pLine->pUserData)->instrument;
  if((pInst=find_current_instrument(data_type))==NULL)  return;

  pLine->length=pInst->raw.length;
  if(( pLine->x=(float*)malloc(sizeof(float)*(pLine->length)) )==NULL){
    fprintf(stderr,"OnDrawRaw(), unable to malloc(%zu*%d)\n",sizeof(float),pLine->length);
    return;
  }
  if(( pLine->y=(float*)malloc(sizeof(float)*(pLine->length)) )==NULL){
    fprintf(stderr,"OnDrawRaw(), unable to malloc(%zu*%d)\n",sizeof(float),pLine->length);
    free(pLine->x);
    return;
  }
  
  fXmin=0;
  fXmax=pLine->length;
  fYmin= 1.0e6;			/* Large positive number */
  fYmax=-1.0e6;			/* Large negative number */
  
  cpgqvp(3,&xl,&xr,&yb,&yt);
  max_pts=xr-xl;
/* Do offset from dcvalue downmap, this will look interesting for compressed data */
if((data_type&Instrument_WBR || data_type&Instrument_WFR) && pLine->length>=max_pts*2.0){
unsigned j;
float dc_value,mapidx;
float tmpIn,tmpOut;

  dc_value=data_type&Instrument_WBR?127.5:2047.5;

	/* Want the compare statement below to evaluate to zero */
  for(i=0;i<max_pts;i++)  pLine->y[i]=dc_value;	

  mapidx=(float)max_pts/(float)pLine->length;	/* outlength/inlength */
  for(i=0;i<pLine->length;i++){
    j=(float)i*mapidx;
    if((tmpIn=pInst->raw.data[i]-dc_value)<0)  	/* Get absolute value of deviation */
      tmpIn=dc_value-pInst->raw.data[i];
    if((tmpOut=pLine->y[j]-dc_value)<0)
      tmpOut=dc_value-pLine->y[j];
    if(tmpIn>tmpOut){
      pLine->y[j]=pInst->raw.data[i];
      pLine->x[j]=i;
    }
    if(pLine->y[j]<fYmin)  fYmin=pLine->y[j];
    if(pLine->y[j]>fYmax)  fYmax=pLine->y[j];
  }
  pLine->length=max_pts;
}
else{
  for(i=0;i<pLine->length;i++){
    pLine->x[i]=i;
    pLine->y[i]=pInst->raw.data[i];
    if(pLine->y[i]<fYmin)  fYmin=pLine->y[i];
    if(pLine->y[i]>fYmax)  fYmax=pLine->y[i];
  }
}

	/*  Auto Ranging Functions */
  if(pLine->xl==pLine->xr){  pLine->xl=fXmin;  pLine->xr=fXmax;  }  
  if(pLine->yb==pLine->yt){  pLine->yb=fYmin;  pLine->yt=fYmax;  }

  DrawPoints(pLine);
  
/* free allocated data */
  free(pLine->x);
  free(pLine->y);
  
return;
}



void OnDrawMag(CPGLine *pLine)
{
int i;
uint32_t type,analysis,data_type;
float fXmin,fXmax,fYmin,fYmax;
INSTRUMENT *pInst;
InstrumentAnalysis* InstAnal;
float *pReal,*pImag,fDeltaF,fScale,fGain;
uint32_t length;
float xl,xr,yb,yt;
uint32_t max_pts;

  if(!pLine)  return;
  
  InstAnal=(InstrumentAnalysis*)pLine->pUserData;
  data_type=InstAnal->instrument;
  analysis =InstAnal->analysis;
  if((pInst=find_current_instrument(InstAnal->instrument))==NULL)  
    return;
  
  pReal=pInst->fft.real;
  pImag=pInst->fft.imag;
  length=pInst->fft.length/2.0;	/* fft is repeated in the 2nd half */
 
  switch(data_type&Instrument_Mask){
    case Instrument_WBRhighBand :
      fDeltaF = 1.0 / (2.0*WBR_HIGH_BAND_SAMPLING_PERIOD*length);
      fScale  = 1.0 / 127.5;
      break;
    case Instrument_WBRlowBand :
      fDeltaF = 1.0 / (2.0*WBR_LOW_BAND_SAMPLING_PERIOD*length);
      fScale  = 1.0 / 127.5;
      break;
    case Instrument_WFRhighBand :
      fDeltaF = 1.0 / (2.0*WFR_HIGH_BAND_SAMPLING_PERIOD*length);
      fScale  = 1.0 / 2047.5;
      break;
    case Instrument_WFRlowBand :
      fDeltaF = 1.0 / (2.0*WFR_LOW_BAND_SAMPLING_PERIOD*length);
      fScale  = 1.0 / 2047.5;
      break;
    default :
      return;
  }
  fGain=Get_dB_Calibration(pInst);
  
  pLine->length=length;
  if((pLine->x=(float*)malloc(sizeof(float)*(length)) )==NULL){
    fprintf(stderr,"OnDrawRaw(), unable to malloc(%zu*%d)\n",sizeof(float),pLine->length);
    return;
  }
  if((pLine->y=(float*)malloc(sizeof(float)*(length)) )==NULL){
    fprintf(stderr,"OnDrawRaw(), unable to malloc(%zu*%d)\n",sizeof(float),pLine->length);
    free(pLine->y);
    return;
  }
    
  fXmin=0;
  fXmax=fDeltaF*pLine->length;
  fYmin= 1.0e6;			/* Large positive number */
  fYmax=-1.0e6;			/* Large negative number */
  for(i=0;i<pLine->length;i++){
    pLine->x[i]=i*fDeltaF;
    pLine->y[i]=pReal[i]*pReal[i] + pImag[i]*pImag[i];
    pLine->y[i]=pLine->y[i]<=0.0?0.0:sqrt(pLine->y[i])*fScale;
    pLine->y[i]=pLine->y[i]<=0.0?-666.0:20.0*log10(pLine->y[i]);
    pLine->y[i]-=fGain;		/* Forward gain, referred back to the input */
    if(pLine->y[i]<fYmin)  fYmin=pLine->y[i];
    if(pLine->y[i]>fYmax)  fYmax=pLine->y[i];
  }

	/*  Auto Ranging Functions */
  if(pLine->xl==pLine->xr){  pLine->xl=fXmin;  pLine->xr=fXmax;  }  
  if(pLine->yb==pLine->yt){  pLine->yb=fYmin;  pLine->yt=fYmax;  }

  cpgqvp(3,&xl,&xr,&yb,&yt);
  max_pts=xr-xl;
  /* Peak down map the points.  This only works if the points being mapped down
  is at least two times greater, due to pLine->y[i]=-666; */
  if(pLine->length>=max_pts*2){		/* peak down map points */
  unsigned j;
  float mapidx=(float)max_pts/(float)pLine->length;	/* outlength/inlength */
    for(i=0;i<pLine->length;i++){
      j=(float)i*mapidx;
      if(pLine->y[i]>pLine->y[j]){
        pLine->y[j]=pLine->y[i];
        pLine->x[j]=pLine->x[i];
      }
      pLine->y[i]=-666.0;		/* Arbitrary large negative number */
    }/* for down map */
  pLine->length=max_pts;		/* assign new valid length */
  }/* if down map */

  DrawPoints(pLine);
  
  free(pLine->x);
  free(pLine->y);

return;
}


void OnDrawMag_LFDR(CPGLine *pLine)
{
int i;
uint32_t type,analysis,data_type;
float fXmin,fXmax,fYmin,fYmax;
INSTRUMENT *pInst;
InstrumentAnalysis* InstAnal;
float *pX,*pY,fDGF,fGain,fScale;
uint32_t length=32;
float xl,xr,yb,yt;
uint32_t ulTmp;

  if(!pLine)  return;
  
  InstAnal=(InstrumentAnalysis*)pLine->pUserData;
  data_type=InstAnal->instrument;
  analysis =InstAnal->analysis;
  if((pInst=find_current_instrument(InstAnal->instrument))==NULL)  
    return;
  
  pLine->length=length;	/* pInst->raw.length */
  if((pLine->x=(float*)malloc(sizeof(float)*(length)) )==NULL){
    fprintf(stderr,"OnDrawRaw(), unable to malloc(%zu*%d)\n",sizeof(float),pLine->length);
    return;
  }
  if((pLine->y=(float*)malloc(sizeof(float)*(length)) )==NULL){
    fprintf(stderr,"OnDrawRaw(), unable to malloc(%zu*%d)\n",sizeof(float),pLine->length);
    free(pLine->y);
    return;
  }
 

  fGain=Get_dB_Calibration(pInst);		/* Same as WFR */

  fScale=2.0/(4095.0*pInst->dgf);		/* Hanning Window, Full Range A/D, Digital Gain Factor */
   
  pX=LFDR_CntFrq;				/* This is only thirty two values long */
  pY=pInst->raw.data;
  
  fXmin=0;
  fXmax=pX[pLine->length-1];
  fYmin= 1.0e6;			/* Large positive number */
  fYmax=-1.0e6;			/* Large negative number */
  for(i=0;i<pLine->length;i++){
    pLine->x[i]=pX[i];
    ulTmp=pY[i];
    pLine->y[i]=get_lfdr_value(ulTmp);	/* This is a macro which only takes unsigned ints */
    pLine->y[i]*=fScale;	/* Hanning Window, Full Range A/D, Digital Gain Factor */
    pLine->y[i]/=LFDR_pbw[i];	/* Divide by Power Bandwidth */
    pLine->y[i]=pLine->y[i]>0.0?20.0*log10(pLine->y[i]):-666.0;
    pLine->y[i]-=fGain;		/* Input refered gain */
    if(fYmin>pLine->y[i])  fYmin=pLine->y[i];
    if(fYmax<pLine->y[i])  fYmax=pLine->y[i];
  }

	/*  Auto Ranging Functions */
  if(pLine->xl==pLine->xr){  pLine->xl=fXmin;  pLine->xr=fXmax;  }  
  if(pLine->yb==pLine->yt){  pLine->yb=fYmin;  pLine->yt=fYmax;  }

  DrawPoints(pLine);
  
  free(pLine->x);
  free(pLine->y);

return;
}


/*  This is an example of a specialized plotting function which does not exist
within the framework.  It is a temporary hack.  If one would like data to be 
displayed a different way, two things have to happen.
  1.  Create a function much like this one.
  2.  Assign it to the window in BuildLineObject.
When all said an done, the plotting function should only exist and be assigned
in the BuildLineObject.
*/
void OnDrawMag_MFR(CPGLine *pLine)
{
int i;
INSTRUMENT *pInst;
InstrumentAnalysis* InstAnal;

  if(!pLine)  return;
  
  InstAnal=(InstrumentAnalysis*)pLine->pUserData;
  if((pInst=find_current_instrument(InstAnal->instrument))==NULL)  return;
  
  /* allocate some temporary storage for the mfr.  It will allways be 224 points in length */
  pLine->length=pInst->raw.length;
  if((pLine->x=(float*)malloc(sizeof(float)*(pLine->length)) )==NULL){
    fprintf(stderr,"OnDrawRaw(), unable to malloc(%zu*%d)\n",sizeof(float),pLine->length);
    return;
  }
  if((pLine->y=(float*)malloc(sizeof(float)*(pLine->length)) )==NULL){
    fprintf(stderr,"OnDrawRaw(), unable to malloc(%zu*%d)\n",sizeof(float),pLine->length);
    free(pLine->y);
    return;
  }

  /* First display all the points as dots on the screen.  Overriding the pLine object
     color, line style should normally not be done, inorder to exist within the framework.
     That is, when it comes time for the user to rescale axis, change colors, line styles,
     it will not work.
   */

  pLine->color_index=1;			/* White */
  pLine->line_style=0;			/* no line to be drawn */
  pLine->plot_symbol=-1;		/* Use a small dot to mark the point */
  pLine->xl=0;
  pLine->xr=79;				/* 16 + 32 + 32 :: */
  pLine->yb=0;
  pLine->yt=255;
  for(i=0;i<pLine->length;i++){		/* Assign the data */
    pLine->y[i]=pInst->raw.data[i];
    if(i<32)		pLine->x[i]=i%16;	/* MFR Band 1, sweeps 1 and 2; 16 pts/sweep */
    else if(i<96)	pLine->x[i]=(i%32)+16;	/* MFR Band 2, sweeps 1 and 2; 32 pts/sweep */
    else		pLine->x[i]=(i%32)+48;	/* MFR Band 3, sweeps 1,2,3, and 4; 32 pts/sweep */
  }
  DrawPoints(pLine);		/* Generic drawing function capable of drawin lines or points */
  
  /* Now for the averaging of the sweeps within each band.  There is no need to reallocate,
     since the same amount of memeory will be used.
  */

	/* Average MFR Band 1, make the line red */
  pLine->color_index=2;
  pLine->plot_symbol=0;					/* no plot symbols to be drawn */
  pLine->line_style=1;					/* see PGPlot manual for different line styles */
  pLine->length=16;					/* only display the averaged band 1 */
  for(i=0;i<16;i++){	 /* Average Band 1 */
    pLine->y[i]=(pLine->y[i]+pLine->y[i+16])/2.0;		/* Or pInst->raw.data[i] ... */
    pLine->x[i]=i;					/* These numbers are already here */
  }
  DrawPoints(pLine);
  
  	/* Average Band 2, make the line color=3 */
  pLine->color_index=3;
  pLine->length=32;
  for(i=0;i<32;i++){
    pLine->y[i]=(pLine->y[i+32]+pLine->y[i+64])/2.0;
    pLine->x[i]=i+16;
  }
  DrawPoints(pLine);
  
  	/* Average Band 3 */
  pLine->color_index=4;
  pLine->length=32;
  for(i=0;i<32;i++){
    pLine->y[i]=(pLine->y[i+96]+pLine->y[i+128]+pLine->y[i+160]+pLine->y[i+192])/4.0;
    pLine->x[i]=i+48;
    }
  DrawPoints(pLine);


/* Important!!! Deallocate anything which was allocated here, otherwise the will
    be a memory leak.
*/
  free(pLine->x);
  free(pLine->y);

return;
}



/* Calculated forward through the system */
/* For now, these will be ideal values */
/* This is the sole place for calibrations of magnitued */
float Get_dB_Calibration(INSTRUMENT *x)
{
float cal;

	/* Correct for receiver gain */
  switch(x->data_type&Instrument_Mask){
    case Instrument_HFR : 		
      cal=0.0;  
      break;
    case Instrument_LFDR :
    case Instrument_LFDRlowBand :
    case Instrument_LFDRhighBand :	
      cal=0.0;				/* System Gain, actually -3.1dB */
      cal+=x->gain;			/* Receiver Gain State */
      cal-=4.91;			/* HRP Analog section: 2x Amp with 5 to -5 A/D */
      break;				/* 2.5Vpeak in yields 0dB */
    case Instrument_MFR :
    case Instrument_MFRband1 :
    case Instrument_MFRband2 :
    case Instrument_MFRband3 :
      cal=0.0;
      break;
    case Instrument_WBR :
    case Instrument_WBRlowBand :
    case Instrument_WBRhighBand :
      cal=0.0;				/* System Gain, actually all fucked up.  Good Luck :) */
      cal+=x->gain;			/* Receiver Gain State, actually all fucked up too.  Good Luch :) */
      cal-=4.91;			/* Approx. 2.5Vpeak in yields 0dB */
      break;    
    case Instrument_WFR :
    case Instrument_WFRlowBand :
    case Instrument_WFRhighBand :
      cal=0.0;				/* System Gain, actually -3.1dB */
      cal+=x->gain;			/* Receiver Gain State */
      cal-=4.91;			/* HRP Analog section: 2x Amp with 5 to -5 A/D */
      break;				/* 2.5Vpeak in yields 0dB */
    default :
      cal=0.0;
      break; 
  }
  
  	/* Correct for the diff amp gain.  Preamp/Sensor gain will be handled elsewhere */
  switch(x->data_type&Antenna_Mask){
    case Antenna_ExLo :      	cal+=6.0;	break;
    case Antenna_EzLo :      	cal+=6.0;	break;
    case Antenna_Bx :    	cal-=9.6;	break;
    case Antenna_By:     	cal-=9.6;	break;
    case Antenna_Bz:      	cal-=9.6;	break;
    case Antenna_LMRp :		cal+=0.0;	break;
    case Antenna_LMRm :      	cal+=0.0;	break;
    case Antenna_LP :      	cal+=0.0;	break;
    case Antenna_HF :      	cal+=0.0;	break;
    default :      		cal+=0.0;	break;  
  }

return cal;
}


void DumpInstrumentTime(INSTRUMENT *x)
{

/*
fprintf(stderr,"%5s %4s::CDSclk %08X %08x; Sclk %08X %08x; EVTclk %08X %08x;\n",
	get_receiver_string(x->data_type),get_antenna_string(x->data_type),
	x->cdsclk.seconds,x->cdsclk.milliseconds,
	x->sclk.seconds,x->sclk.fine,
	x->evtclk.seconds,x->evtclk.milliseconds);
*/

fprintf(stderr,"%5s %4s::CDSclk %08X %08x; Sclk %08X %08x; EVTclk %08X %08x;\n",
	get_receiver_string(x->data_type),get_antenna_string(x->data_type),
	x->cdsclk.seconds,x->cdsclk.milliseconds,
	x->sclk.seconds,0,
	x->evtclk.seconds,x->evtclk.milliseconds);



}
