#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stddef.h>
#include <signal.h>
#include <time.h>
#include <math.h>
#include <stdint.h>

#include "ana.h"
#include <instrument.h>

#include "../lib/rpwsdsp/rpwsdsp.h"
/* #include "pgplotcism.h" */




static int32_t so_list_cnt;
static SPECTROGRAM_OBJECT *so_list[32];
static int32_t so_list_max=XtNumber(so_list);


/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */


/* 2nd & 3rd arguments are Xdim and Ydim of the image array, optional */
SPECTROGRAM_OBJECT *create_spectrogram_object(int32_t type,int32_t xdim,int32_t ydim)
{
SPECTROGRAM_OBJECT *so;

  if((so=(SPECTROGRAM_OBJECT*)malloc(sizeof(SPECTROGRAM_OBJECT)))==NULL){
    fprintf(stderr,"Unable to malloc(), create_spectrogram_object()\n");
    }
  else if((so->image=(unsigned char*)malloc(sizeof(unsigned char)*xdim*ydim))==NULL){
    free(so);
    so=NULL;
    fprintf(stderr,"Unable to malloc(), create_spectrogram_object(image)\n");
    }
  else if((so->evtclk=(uint32_t*)malloc(sizeof(uint32_t)*xdim))==NULL){
    free(so->image);
    free(so);
    so=NULL;
    fprintf(stderr,"Unable to malloc(), create_spectrogram_object(evtclk)\n");
    }
  else{
    so->type=type;
    so->xdim=xdim;
    so->ydim=ydim;
    }

return so;
}

void initialize_spectrogram_object(SPECTROGRAM_OBJECT *so)
{
int i;

  if(so==NULL)  return;
  
  so->refresh=0;
  
  if(so->type&Instrument_LFDR) 		so->resolution=2;  /* 16 Seconds */
  else if(so->type&Instrument_MFR)	so->resolution=4;  /* 32 Seconds */
  else if(so->type&Instrument_WBR) 	so->resolution=1;  /*  8 Seconds */
  else if(so->type&Instrument_WFR) 	so->resolution=3;  /* 24 Seconds */
  else					so->resolution=1;  /*  8 Seconds */
  
  so->show_missing_packets=0;
  so->show_extra_packets=0;
  			  
  if(so->type&Instrument_LFDR)  	so->period=16;
  else if(so->type&Instrument_MFR)  	so->period=32;
  else if(so->type&Instrument_WBR)	so->period=8;
  else if(so->type&Instrument_WFR) 	so->period=24;
  else					so->period=60;
  
  so->grace=0;				/* No Grace */
  so->color=1;

  if(so->type&Instrument_LFDR)		{so->color_bar_min=-120.0;so->color_bar_max=0.0;}
  else if(so->type&Instrument_MFR)	{so->color_bar_min=-120.0;so->color_bar_max=0.0;}
  else if(so->type&Instrument_WBR)	{so->color_bar_min=-120.0;so->color_bar_max=-40.0;}
  else if(so->type&Instrument_WFR)	{so->color_bar_min=-120.0;so->color_bar_max=-20.0;}
  else					{so->color_bar_min=-120.0;so->color_bar_max=0.0;}

  so->last_pixel=0;
  so->start_time=0;
  for(i=0;i<so->xdim;i++)  
    so->evtclk[i]=0;
  so->image_index=1;
  
return;
}

int add_to_list_spectrogram_object(SPECTROGRAM_OBJECT *so)
{
int status;

  if(so_list_cnt>=so_list_max){
    fprintf(stderr,"Exceeded list Maxium, create_spectrogram_object\n");
    status=0;
    }
  else{
    so_list[so_list_cnt]=so;
    status=1;
    ++so_list_cnt;
    }

return status;
}

SPECTROGRAM_OBJECT *find_current_spectrogram_object(int32_t type)
{
int i;
int32_t inst_type,ant_type;
  
  for(i=0;i<so_list_cnt;i++){			/* Check for the specific case */
    if(so_list[i]->type==type)		
      break;
    }

  if(i==so_list_cnt){				/* No match, check for the general case */
    inst_type=type&Instrument_Mask;
    ant_type=type&Antenna_Mask;
    for(i=0;i<so_list_cnt;i++){			
      if(so_list[i]->type&inst_type){		/* Check for valid instrument */
        if(so_list[i]->type&ant_type)		/* Check for valid antenna */
          break;
        }
      }/* for */
    }/* if */

  if(i==so_list_cnt)				/* No Match Found */  
    return NULL;				
    
return so_list[i];
}

void set_spectrogram_objects(int32_t type,SPECTROGRAM_OBJECT *source)
{/* Under Construction ??? */
int i;
int32_t inst_type,ant_type;
SPECTROGRAM_OBJECT *so;
  
  inst_type=type&Instrument_Mask;
  ant_type=type&Antenna_Mask;
  for(i=0;i<so_list_cnt;i++){
    if(so_list[i]->type&inst_type){		/* Check for valid instrument */
      if(so_list[i]->type&ant_type){		/* Check for valid antenna */
        so_list[i]->refresh=source->refresh;
        so_list[i]->show_missing_packets=source->show_missing_packets;
        so_list[i]->show_extra_packets=source->show_extra_packets;
        so_list[i]->resolution=source->resolution;
        so_list[i]->period=source->period;
        so_list[i]->grace=source->grace;
        so_list[i]->color=source->color;
        so_list[i]->color_bar_min=source->color_bar_min;
        so_list[i]->color_bar_max=source->color_bar_max;
        }
      }
    }
    
return;
}

int log_current_spectrogram_object(SPECTROGRAM_OBJECT *so,LINE_OBJECT *lo)
{
unsigned char *outdata;
int32_t i,j,mapidx;
int32_t inlen,outlen;
float *indata;

  /* Map Pixel to the Screen, float range 0dB to -200dB mapped to char 0 to 255 */
  inlen=lo->length;
  outlen=so->ydim;
  indata=lo->y;
  outdata=so->image+so->xdim*so->image_index; 
  if(so->image_index>so->xdim)  so->image_index=1;	/* PGPLOT - fortran arrays start at 1 */
  outdata=so->image+so->xdim*so->last_pixel;
  
  for(i=0;i<512;i++)  			/* Initialize Array */
    outdata[i]=255;			/* 255 = black */	
  if(inlen==outlen){			/* 1:1 Pixel Mapping */
    for(i=0;i<inlen;i++)
      outdata[i]=(indata[i]*-1.0);
    }
  else if(inlen>outlen){		/* Map Down */
    mapidx=(float)outlen/(float)inlen;
    for(i=0;i<inlen;i++){
      j=(float)i*mapidx;
      indata[i]*=-1.0;
      if(indata[i]<outdata[j])  outdata[j]=indata[i];
      }
    }
  else{					/* Map Up */
    mapidx=(float)inlen/(float)outlen;
    for(i=0;i<outlen;i++){
      j=(float)i*mapidx;
      outdata[i]=indata[j];
      }
    }
  ++so->image_index;

return 1;
}

/* -------------------------------------------------------------------- */
/* 		Do the current instrument stuff here	                */
/* -------------------------------------------------------------------- */




/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

PANEL_OBJECT *create_panel_object(void)
{
PANEL_OBJECT *po;

  if((po=(PANEL_OBJECT*)malloc(sizeof(PANEL_OBJECT)))==NULL){
    fprintf(stderr,"Unable to malloc(), create_panel_object( )\n");
    }
  else{
    po->list_count=0;
    }

return po;
}

void initialize_panel_object(PANEL_OBJECT *po)
{/* Does not do anything */
return;
}

int add_analysis_to_panel_object(PANEL_OBJECT *po,uint32_t analysis,uint32_t instrument)
{
int i,status=1;

  if(po->list_count>=po_list_max){
    status=0;
    fprintf(stderr,"Panel List Full\n");
    }
  else{
    po->analysis_type[po->list_count]=analysis;
    po->instrument_type[po->list_count]=instrument;
    ++po->list_count;
    switch(analysis){
      case Analysis_Raw :		/* Time Series */
        break;
      case Analysis_Mag :		/* FFT Magnitude */
        break;
      case Analysis_Phs :		/* FFT Phase */
        break;
      case Analysis_Spg :		/* Spectogram */
        break;
      default :
        status=0;
        fprintf(stderr,"Unknow analysis type, add_analysis_to_panel_object()\n");
        break;
      }
    }

return status;
}

int delete_from_list_panel_object(PANEL_OBJECT *po, int32_t type)
{
int status = 0;

return status;
}


/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

