#include <stdio.h> 
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <string.h>

#include <cpgplot.h>

#include "CTools.h"

#include "../lib/XmRPWS/XmRPWS.h"
#include "ana.h"

#include <instrument.h>
UserInterface UI;

static char ANA_VERSION[]="ana( ) Ver. 9.01"; 

extern DisplayPanel dsp_pnl[];



void anagui(int argc,char *argv[] );

void mf(int sig)
{
raise(SIGSEGV);
}

/* Make this global since it will get used a lot */
CPGPlotWindow *MainPlotWindow;
CPGPlotWindow *MainPlotWindow2;

int main(int argc,char *argv[])
{
/*char *pch,inln[256],pathname[256],filename[128];*/
long loop,j;
/*FILE *inhandle;*/
SPECTROGRAM_OBJECT *so;

/*InstrumentAnalysis InstAnal;*/


  UI.input_filter=(Instrument_LFDR|Instrument_LP|Instrument_HFR|Instrument_MFR|Instrument_WBR|Instrument_WFR);




/*
  while(--argc){
    ++argv;
    if(!strcmp("-get",*argv)){	
      if((inhandle=fopen("filerc","rt"))==NULL){
        fprintf(stderr,"Error: 'filerc' not found in current directory\n");
        }
      else{
        filename[0]=0;
        pathname[0]='\0';
        while(pch=fgets(inln,255,inhandle)){
          if(pch=strstr(inln,"PATH="))  strcpy(pathname,(pch+5));
          else if(pch=strstr(inln,"MPUS="))  strcpy(filename,(pch+5));
          }
        if(pch=strstr(pathname,"\n"))  *pch='/';	
        if(pch=strstr(filename,"\n"))  *pch=0;
        pch=strcat(pathname,filename);
        fprintf(stderr,"Opening MPUS File (%s)\n",pch);
        open_telemetry_infile(pch);
        }
      }
    else if(!strcmp("-filter",*argv)){
      --argc;
      ++argv;
      UI.input_filter=atoi(*argv);
      }
    }
*/


  UI.WaveNormalFrequency=60.0;
  UI.do_wave_normal_vector=0;
 
 
 
  
  MainPlotWindow=CreateListElement(CPGPlotWindow);
  SubDivide_CPGPlotWindow(MainPlotWindow,3,5);
  {
  /*int i,j;*/
  CPGPlotWindow *pWnd;
  WindowSetup *pWndSet;

/* Medium Frequency Receiver */
    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,1,1);
    pWnd->pAxis=BuildAxisObject(0,0);
    pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_MFR|Antenna_ExLo|Antenna_EzLo;
    pWndSet->analysis=Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     

    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,1,2);
    pWnd->pAxis=BuildAxisObject(0,0);
    pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_MFR|Antenna_Bx|Antenna_Bz;
    pWndSet->analysis=Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     


/* Low Frequency Reciever */
    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,1,3);
    pWnd->pAxis=BuildAxisObject(0,0);
    pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_LFDR|Antenna_ExLo|Antenna_EzLo;
    pWndSet->analysis=Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     

    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,1,4);
    pWnd->pAxis=BuildAxisObject(0,0);
    pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_LFDR|Antenna_Bx|Antenna_By|Antenna_Bz;
    pWndSet->analysis=Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     


/* Wideband Receiver */
    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,1,5);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_WBR|Valid_WBR_Antenna;
    pWndSet->analysis=Analysis_Raw|Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     


/* Waveform Receiver */  
    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,2,1);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_WFR|Antenna_ExLo|Antenna_LMRp;
    pWndSet->analysis=Analysis_Raw|Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     
 
    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,2,2);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_WFR|Antenna_EzLo|Antenna_LMRm;
    pWndSet->analysis=Analysis_Raw|Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     

    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,2,3);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_WFR|Antenna_Bx|Antenna_LP;
    pWndSet->analysis=Analysis_Raw|Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     

    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,2,4);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_WFR|Antenna_By;
    pWndSet->analysis=Analysis_Raw|Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     

    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,2,5);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_WFR|Antenna_Bz;
    pWndSet->analysis=Analysis_Raw|Analysis_Mag;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     
    
/* Langmuir Probe */  
    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,3,1);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_LPanalyzed|Valid_LP_Antenna;
    pWndSet->analysis=Analysis_Raw;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     

    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,3,2);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_LPdensity|Valid_LP_Antenna;
    pWndSet->analysis=Analysis_Raw;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     

    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,3,3);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_LPsweep|Valid_LP_Antenna;
    pWndSet->analysis=Analysis_Raw;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     

    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,3,4);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_LPtbd|Valid_LP_Antenna;
    pWndSet->analysis=Analysis_Raw;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     

    pWnd=GetSubPanel_CPGPlotWindow(MainPlotWindow,3,5);
    pWnd->pAxis=BuildAxisObject(0,0);
   pWnd->pLabel=BuildLabelObject(0);
    pWndSet=(WindowSetup*)calloc(1,sizeof(WindowSetup));
    pWndSet->instrument=Instrument_LP|Valid_LP_Antenna;
    pWndSet->analysis=Analysis_Raw;
    pWndSet->pPlotSetup=BuildLineObject(pWndSet->instrument,pWndSet->analysis);
    pWnd->pUserData=(void*)pWndSet;     
 }


fprintf(stderr,"Entering Motif Thread from ana_main calling anagui()\n");
/* Pitch control off to Motif thread */  
  anagui(argc,argv);
   
/* setup/read initilization */

  UI.WaveNormalFrequency=60.0;
  UI.do_wave_normal_vector=0;
  UI.numXpanel=3;
  UI.numYpanel=6;
  for(loop=0;loop<6;loop++){
    dsp_pnl[loop].update=0;
    for(j=0;j<10;j++)
      dsp_pnl[loop].lo[j]=NULL;
    }
  /* Display Panel 0 */
  dsp_pnl[0].num_modes=0;
  dsp_pnl[0].lo[0]=NULL;  
    dsp_pnl[0].analysis[0]=Analysis_Raw;  
    dsp_pnl[0].types[0]=Instrument_WFR|Antenna_ExLo|Antenna_LMRp;
  ++dsp_pnl[0].num_modes;
  /* Display Panel 1 */
  dsp_pnl[1].num_modes=0;
  dsp_pnl[1].lo[0]=NULL;
    dsp_pnl[1].analysis[0]=Analysis_Raw;
      dsp_pnl[1].types[0]=Instrument_WFR|Antenna_EzLo|Antenna_LMRm;
  ++dsp_pnl[1].num_modes;
  /* Display Panel 2 */
  dsp_pnl[2].num_modes=0;
  dsp_pnl[2].lo[0]=NULL;
    dsp_pnl[1].analysis[0]=Analysis_Raw;
      dsp_pnl[2].types[0]=Instrument_WFR|Antenna_Bx|Antenna_LP;
  ++dsp_pnl[2].num_modes;
  /* Display Panel 3 */
  dsp_pnl[3].num_modes=0;
  dsp_pnl[3].lo[0]=NULL;  dsp_pnl[3].analysis[0]=Analysis_Raw;  dsp_pnl[3].types[0]=Instrument_WFR|Antenna_By;
  ++dsp_pnl[3].num_modes;
  /* Display Panel 4 */
  dsp_pnl[4].num_modes=0;
  dsp_pnl[4].lo[0]=NULL;  dsp_pnl[4].analysis[0]=Analysis_Raw;  dsp_pnl[4].types[0]=Instrument_WFR|Antenna_Bz;
  ++dsp_pnl[4].num_modes;
  /* Display Panel 5 */
  dsp_pnl[5].num_modes=0;
  dsp_pnl[5].lo[0]=NULL;  dsp_pnl[5].analysis[0]=Analysis_Raw;  dsp_pnl[5].types[0]=Instrument_WBR|Antenna_Any;
  ++dsp_pnl[5].num_modes;
 

  so=create_spectrogram_object(Instrument_WFR|Antenna_ExLo,256,256);
  initialize_spectrogram_object(so);
  add_to_list_spectrogram_object(so);

  so=create_spectrogram_object(Instrument_WFR|Antenna_EzLo,256,256);
  initialize_spectrogram_object(so);
  add_to_list_spectrogram_object(so);

  so=create_spectrogram_object(Instrument_WFR|Antenna_Bx,256,256);
  initialize_spectrogram_object(so);
  add_to_list_spectrogram_object(so);

  so=create_spectrogram_object(Instrument_WFR|Antenna_By,256,256);
  initialize_spectrogram_object(so);
  add_to_list_spectrogram_object(so);

  so=create_spectrogram_object(Instrument_WFR|Antenna_Bz,256,256);
  initialize_spectrogram_object(so);
  add_to_list_spectrogram_object(so);

  so=create_spectrogram_object(Instrument_WBR|Antenna_Any,256,256);
  initialize_spectrogram_object(so);
  add_to_list_spectrogram_object(so);
     
  anagui(argc,argv);



return 1;
}
