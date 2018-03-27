#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <signal.h>
#include <time.h>
#include <math.h>

#include "cpgplot.h"

#include "ana.h"
#include "telemetry.h"
#include "instrument.h"



char *allocate_line_object(int32_t length,LINE_OBJECT *lo);
char *deallocate_line_object(LINE_OBJECT *lo);
void display_line_object(LINE_OBJECT *lo);

void init_color_table(int type);


void display_line_object(LINE_OBJECT *lo)
{

  cpgswin(lo->xl,lo->xr,lo->yb,lo->yt);
  cpgsci(lo->color_index);
  if(lo->line_style){
    cpgsls(lo->line_style);
    cpgline(lo->length,lo->x,lo->y);
    }
  else
    cpgpt(lo->length,lo->x,lo->y,lo->graph_symbol);
  cpgsci(1);
  cpgsls(1);

return;
}




void display_lfdr_raw(int32_t length,float *lfdr)
{
int32_t loop;
float max=-1e6,min=1e6;
LINE_OBJECT *lo;  

  lo=(LINE_OBJECT*)malloc(sizeof(LINE_OBJECT));
  allocate_line_object(length,lo);
  lo->color_index=2;
  lo->line_style=1;
  lo->xl=0;lo->xr=lo->length;lo->yb=0;lo->yt=255;
  for(loop=0;loop<length;loop++){
    lo->y[loop]=lfdr[loop];
    if(lo->y[loop]<min)  min=lo->y[loop];
    if(lo->y[loop]>max)  max=lo->y[loop];
    lo->x[loop]=loop;
    }
  lo->yb=min*0.90;
  lo->yt=max*1.10;
  display_line_object(lo);

  deallocate_line_object(lo);
  free(lo);

return;
}

void display_lp_raw(int32_t length,float *lp)
{
int32_t loop;
LINE_OBJECT *lo;  

  lo=(LINE_OBJECT*)malloc(sizeof(LINE_OBJECT));
  allocate_line_object(length,lo);
  lo->color_index=2;
  lo->line_style=1;
  lo->xl=0;lo->xr=lo->length;lo->yb=0;lo->yt=4095;
  for(loop=0;loop<length;loop++){
    lo->y[loop]=lp[loop];
    lo->x[loop]=loop;
    }
  display_line_object(lo);

  deallocate_line_object(lo);
  free(lo);

return;
}


void display_mfr_raw(int32_t length,float *mfr)
{
int32_t loop;
LINE_OBJECT *lo;

  lo=(LINE_OBJECT*)malloc(sizeof(LINE_OBJECT));
  allocate_line_object(length,lo);
  lo->xl=0.0;lo->xr=79.0;lo->yb=0.0;lo->yt=255.0;
  
  /* Display all sweeps of MFR as dots */
  lo->color_index=1;
  lo->line_style=0;
  lo->graph_symbol=-1;
  for(loop=0;loop<length;loop++){
    lo->y[loop]=mfr[loop];
    if(loop<32)       lo->x[loop]=loop%16;
    else if(loop<96)  lo->x[loop]=(loop%32)+16;
    else              lo->x[loop]=(loop%32)+48;
    } 
  display_line_object(lo);
 
  /* Average Band 1 */
  for(loop=0;loop<16;loop++){
    lo->y[loop]=(lo->y[loop]+lo->y[loop+16])/2.0;	/* Band 1, sweep 1 and sweep 2 */
    lo->x[loop]=loop;
    }
  lo->color_index=2;
  lo->line_style=1;
  lo->length=16;
  display_line_object(lo);
  
  /* Average Band 2 */
  for(loop=0;loop<32;loop++){
    lo->y[loop]=(lo->y[loop+32]+lo->y[loop+64])/2.0;
    lo->x[loop]=loop+16;
    }
  lo->color_index=3;
  lo->line_style=1;
  lo->length=32;
  display_line_object(lo);
  
  /* Average Band 3 */
  for(loop=0;loop<32;loop++){
    lo->y[loop]=(lo->y[loop+96]+lo->y[loop+128]+lo->y[loop+160]+lo->y[loop+192])/4.0;
    lo->x[loop]=loop+48;
    }
  lo->color_index=4;
  lo->line_style=1;
  lo->length=32;
  display_line_object(lo);

  deallocate_line_object(lo);
  free(lo);
  
return;
}



void display_wbr_raw(int32_t length,float *wbr)
{
int32_t loop;
LINE_OBJECT *lo;  

  lo=(LINE_OBJECT*)malloc(sizeof(LINE_OBJECT));
  allocate_line_object(length,lo);
  lo->color_index=2;
  lo->line_style=1;
  lo->xl=0;lo->xr=lo->length;lo->yb=0;lo->yt=255;
  for(loop=0;loop<length;loop++){
    lo->y[loop]=wbr[loop];
    lo->x[loop]=loop;
    }
  display_line_object(lo);

  deallocate_line_object(lo);
  free(lo);

return;
}

void display_wfr_raw(int32_t length,float *wfr)
{
int32_t loop;
LINE_OBJECT *lo;  

  lo=(LINE_OBJECT*)malloc(sizeof(LINE_OBJECT));
  allocate_line_object(length,lo);
  lo->color_index=2;
  lo->line_style=1;
  lo->xl=0;lo->xr=lo->length;lo->yb=0;lo->yt=4095;
  for(loop=0;loop<length;loop++){
    lo->y[loop]=wfr[loop];
    lo->x[loop]=loop;
    }
  display_line_object(lo);

  deallocate_line_object(lo);
  free(lo);

return;
}

void display_wbr_magnitude(INSTRUMENT *wbr)
{
int32_t loop,length;
float delta_f;
LINE_OBJECT *lo;  

  length=wbr->fft.length/2.0;
  lo=(LINE_OBJECT*)malloc(sizeof(LINE_OBJECT));
  allocate_line_object(length,lo);
  lo->color_index=3;
  lo->line_style=1;
  if(wbr->band==1)
    delta_f=((1.0/WBR_HIGH_BAND_SAMPLING_PERIOD)/2.0)/(float)length;
  else	
    delta_f=((1.0/WBR_LOW_BAND_SAMPLING_PERIOD)/2.0)/(float)length;
  lo->xl=0;lo->xr=(float)length*delta_f;lo->yb=-120;lo->yt=0;
  for(loop=0;loop<length;loop++){
    lo->y[loop]=sqrt(wbr->fft.real[loop]*wbr->fft.real[loop] + wbr->fft.imag[loop]*wbr->fft.imag[loop]);
    lo->y[loop]=lo->y[loop]?(20.0*log10(lo->y[loop]/128.0)):-666;
    lo->y[loop]-=wbr->gain;
    lo->x[loop]=(float)loop*delta_f;
    }
  display_line_object(lo);

  deallocate_line_object(lo);
  free(lo);

return;
}

void display_wfr_magnitude(INSTRUMENT *wfr)
{
int32_t loop,length;
float delta_f;
LINE_OBJECT *lo;  

  length=wfr->fft.length/2.0;
  lo=(LINE_OBJECT*)malloc(sizeof(LINE_OBJECT));
  allocate_line_object(length,lo);
  lo->color_index=3;
  lo->line_style=1;
  if(wfr->band==1)
    delta_f=((1.0/WFR_HIGH_BAND_SAMPLING_PERIOD)/2.0)/(float)length;
  else	
    delta_f=((1.0/WFR_LOW_BAND_SAMPLING_PERIOD)/2.0)/(float)length;
  lo->xl=0;lo->xr=(float)lo->length*delta_f;lo->yb=-120;lo->yt=0;
  for(loop=0;loop<length;loop++){
    lo->y[loop]=sqrt(wfr->fft.real[loop]*wfr->fft.real[loop] + wfr->fft.imag[loop]*wfr->fft.imag[loop]);
    lo->y[loop]=lo->y[loop]?(20.0*log10(lo->y[loop]/2048.0)):-666;
    lo->y[loop]-=wfr->gain;
    lo->x[loop]=(float)loop*delta_f;
    }
  display_line_object(lo);

  deallocate_line_object(lo);
  free(lo);

return;
}


LINE_OBJECT *get_instrument_magnitude(INSTRUMENT *inst)
{
int32_t loop,length;
float delta_f,quan_levels;
LINE_OBJECT *lo;

  length=inst->fft.length/2.0;
  lo=(LINE_OBJECT*)malloc(sizeof(LINE_OBJECT));
  allocate_line_object(length,lo);
  lo->color_index=3;
  lo->line_style=1;
  switch(inst->data_type&Instrument_Mask){
    case Instrument_WBR :
    case Instrument_WBRlowBand :
    case Instrument_WBRhighBand :
      if(inst->band==1)
        delta_f=((1.0/WBR_HIGH_BAND_SAMPLING_PERIOD)/2.0)/(float)length;
      else	
        delta_f=((1.0/WBR_LOW_BAND_SAMPLING_PERIOD)/2.0)/(float)length;
      quan_levels=128.0;
      break;
    case Instrument_WFR :
    case Instrument_WFRlowBand :
    case Instrument_WFRhighBand :
      if(inst->band==1)
        delta_f=((1.0/WFR_HIGH_BAND_SAMPLING_PERIOD)/2.0)/(float)length;
      else	
        delta_f=((1.0/WFR_LOW_BAND_SAMPLING_PERIOD)/2.0)/(float)length;
      quan_levels=2048.0;
      break;
    }
  lo->xl=0;lo->xr=(float)lo->length*delta_f;lo->yb=-120;lo->yt=0;
  for(loop=0;loop<length;loop++){
    lo->y[loop]=sqrt(inst->fft.real[loop]*inst->fft.real[loop] + inst->fft.imag[loop]*inst->fft.imag[loop]);
    lo->y[loop]=lo->y[loop]?(20.0*log10(lo->y[loop]/quan_levels)):-666;
    lo->y[loop]-=inst->gain;
    lo->x[loop]=(float)loop*delta_f;
    }

return lo;
}




/*--------------------------------------------------------------*/
/*		Spectrogram Display				*/
/*--------------------------------------------------------------*/



void display_spectrogram_object(INSTRUMENT *inst,LINE_OBJECT *lo)
{
char update_axis_labels;
float xl,xr,yt,yb,wxl,wxr,wyb,wyt;
int32_t i,j,inlen,outlen;
float *in,out[1024];		/* 1024 should be the largest screen size */
float mapidx;			/* mapping index */
float evt_offset,cal_evt_offset;

int idim,jdim,i1,i2,j1,j2;
float a1,a2,TR[6]={0.0,1.0,0.0,0.0,0.0,1.0};
float tracer[6]={200.0,200.0,200.0,200.0,200.0,200.0};
SPECTROGRAM_OBJECT *so;

  if((so=find_current_spectrogram_object(inst->data_type))==NULL)
    return;
  if(so==NULL || lo==NULL)  return;
  
  /* See if the Axis Labels need Changed */
  /*  pgqwin_c(&wxl,&wxr,&wyb,&wyt); */
  if(so->refresh){
    cpgeras( );
    so->last_pixel=0;
    so->image_index=0;
    }
    
    
  /* Specific to PGPLOT.  Best guess at 'true' window size in pixels */
  cpgqvp(3,&xl,&xr,&yt,&yb);		/* query window size in pixels */
  xl=(int)(xl+1.50);			/* round xl, add one */
  xr=(int)(xr-0.50);			/* round xr, sub one */
  yb=(int)(yb+1.50);			/* round yb, add one */
  yt=(int)(yt-0.50);			/* round yt, sub one */
  /* Convert to World Coordinates xl=1,xr=?,yb=1,yt=? */
  xr=(xr-xl)+1.0;				
  xl=1.0;				
  yt=(yb-yt)+1.0;
  yb=1.0;
  cpgswin(xl,xr,yb,yt);		

  if(so->last_pixel>(xr-so->resolution) || so->last_pixel>so->xdim || so->last_pixel==0){
    /* record new time ??? */
    so->start_time=inst->evtclk.seconds;	
    /* Temporary Hack */
    if(inst->data_type&Instrument_WFR){
      uint32_t start_time;
      SPECTROGRAM_OBJECT *tmp;
        start_time=inst->evtclk.seconds;
        start_time-=(start_time%so->period);	/* Ensure good start time */
      if( (tmp=find_current_spectrogram_object(Instrument_WFR|Antenna_ExLo)) )
        tmp->start_time=start_time;
      if( (tmp=find_current_spectrogram_object(Instrument_WFR|Antenna_EzLo)) )
        tmp->start_time=start_time;
      if( (tmp=find_current_spectrogram_object(Instrument_WFR|Antenna_Bx))   )
        tmp->start_time=start_time;
      if( (tmp=find_current_spectrogram_object(Instrument_WFR|Antenna_By))   )
        tmp->start_time=start_time;
      if( (tmp=find_current_spectrogram_object(Instrument_WFR|Antenna_Bz))   )
        tmp->start_time=start_time;
/*fprintf(stderr,"New WFR Start Time %08X\n",start_time);*/
      }
    so->last_pixel=0;
    }
    
  if(so->image_index>=so->xdim){
    so->image_index=0;
    }
  so->evtclk[so->image_index]=inst->evtclk.seconds;
	
  evt_offset=(float)so->evtclk[so->image_index]-(float)so->start_time;
  cal_evt_offset=(float)so->last_pixel*(float)so->period;
  if((so->show_missing_packets) && (evt_offset>(cal_evt_offset+(float)so->grace))){
    fprintf(stderr,"Missing Packet %s %s %08X::Start=%08X last=%d per=%d grace=%d\n",
    	get_receiver_string(inst->data_type),get_antenna_string(inst->data_type),so->evtclk[so->image_index],
    	so->start_time,so->last_pixel,so->period,so->grace);
    }
  if((so->show_extra_packets) && (evt_offset<(cal_evt_offset-(float)so->grace))){
    fprintf(stderr,"Extra   Packet %s %s %08X::Start=%08X last=%d per=%d grace=%d\n",
    	get_receiver_string(inst->data_type),get_antenna_string(inst->data_type),so->evtclk[so->image_index],
    	so->start_time,so->last_pixel,so->period,so->grace);
    }




  /* Map time to array of receiver period HERE */  
    
  /* Map pixel edges to Xa.5 - Xb.5 */
  if(so->resolution%2)  TR[0]=(float)so->last_pixel;	
  else			TR[0]=(float)so->last_pixel+0.5;
  TR[1]=so->resolution;
    
  /* Map Pixel to the Screen */
  inlen=lo->length;
  outlen=yt;
  in=lo->y;
  for(i=0;i<512;i++)  			/* Initialize Array */
    out[i]=-600;			/* Large Negative Number */	
  if(inlen==outlen){			/* 1:1 Pixel Mapping */
    for(i=0;i<inlen;i++)
      out[i]=in[i];
    }
  else if(inlen>outlen){		/* Map Down */
    mapidx=(float)outlen/(float)inlen;
    for(i=0;i<inlen;i++){
      j=(float)i*mapidx;
      if(in[i]>out[j])  out[j]=in[i];
      }
    }
  else{					/* Map Up */
    mapidx=(float)inlen/(float)outlen;
    for(i=0;i<outlen;i++){
      j=(float)i*mapidx;
      out[i]=in[j];
      }
    }

  /* Draw Magnitude Slice */
  jdim=outlen;j1=1;j2=jdim;
  cpgsch(1.0);
  cpgwedg("ti",-4,8,so->color_bar_max,so->color_bar_min," ");
  cpgimag(out,1,jdim,1,1,j1,j2,so->color_bar_max,so->color_bar_min,TR);

  /* Draw Tracer (current location) for Spectogram */
  TR[0]+=((float)so->resolution);
  j2=5;
/*
  TR[0]+=(((float)so->resolution/1.0)+3.0);
  TR[1]=1.0;					
*/
  cpgimag(tracer,1,jdim,1,1,1,5,so->color_bar_max,so->color_bar_min,TR);
  


  /* Add Border and Labels */
  cpgsch(2.0);
  cpgswin(xl,(xr*so->period)/so->resolution,lo->xl,lo->xr);		
  cpgtbox("bcnstvzh",0,0,"bcst",0,0);	/* n=label con. local,v=vertically */
  cpgsch(1.0);
  
  so->last_pixel+=so->resolution;
  ++so->image_index;
  so->refresh=0;

return;
}


void init_color_table(int type)
{
int i,j;
float cr,cg,cb;
  /* Need to add file reader ??? */
  cpgscir(16,99);
  if(type==0){			/* Gray Scale */
    for(i=16;i<100;i++){
      cr=1.0-((i-15)/84.0);  cg=cr;  cb=cr;
      cpgscr(i,cr,cg,cb);
      }
    }	
  else{				/* Color Scale */
    for(i=16;i<44;i++){
      cr=1.0;  cg=(i-15)/28.0;  cb=0;  		/* Red to Yellow */
      cpgscr(i,cr,cg,cb);
      }
    for(i=44;i<72;i++){
      cr=1.0-((i-43)/28.0);  cg=1.0;  cb=0; 	/* Yellow to Green */
      cpgscr(i,cr,cg,cb);
      }
    for(i=72;i<100;i++){
      cr=0;  cg=1.0-((i-71)/28.0);  cb=(i-71)/28.0;
      cpgscr(i,cr,cg,cb);
      }
    }
  cpgscr(99,0,0,0);	/* background color */
  
return;
}

/*--------------------------------------------------------------*/
/*								*/
/*--------------------------------------------------------------*/



char *allocate_line_object(int32_t length,LINE_OBJECT *lo)
{
static char errmsg[64];

  if((lo->x=(float*)malloc(sizeof(float)*(length)))==NULL){
    sprintf(errmsg,"Unable to malloc() new data");
fprintf(stderr,"Error : %s\n",errmsg);
    exit(0);
    }
  if((lo->y=(float*)malloc(sizeof(float)*(length)))==NULL){
    sprintf(errmsg,"Unable to malloc() new data");
fprintf(stderr,"Error : %s\n",errmsg);
    exit(0);
    }
  lo->length=length;

return NULL;
}

char *deallocate_line_object(LINE_OBJECT *lo)
{

  free(lo->x);
  free(lo->y);
  lo->length=0;
  
return NULL;
}

char *copy_line_object(LINE_OBJECT *s,LINE_OBJECT *d)
{
int32_t loop;

  for(loop=0;loop<s->length;loop++){
    d->x[loop]=s->x[loop];
    d->y[loop]=s->y[loop];
    }
  d->length=s->length;
  d->line_style=s->line_style;
  d->graph_symbol=s->graph_symbol;
  d->color_index=s->color_index;

return NULL;
}


/*--------------------------------------------------------------*/
/*								*/
/*--------------------------------------------------------------*/
