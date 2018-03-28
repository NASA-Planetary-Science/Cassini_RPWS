#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include <X11/IntrinsicP.h>

#include <Xm/XmAll.h>
#include <Xm/Protocols.h>
#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>

#include <Xm/SelectioB.h>

#include <Xm/CascadeB.h>
#include <Xm/Command.h>
#include <Xm/DrawnB.h>	
#include <Xm/FileSB.h>
#include <Xm/Label.h>		
#include <Xm/PushB.h>
#include <Xm/ScrolledW.h>
#include <Xm/Scale.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>



/* raj 020422 #include <X11/bitmaps/woman> */

#include "cpgplot.h"			/* Pgplot c wrappers */
#include "XmPgplot.h"			/* Pgplot window definitions */

#include "../lib/XmRPWS/XmRPWS.h"
#include "CTools.h"
#include "ana.h"
#include <instrument.h>

#ifndef INST_SHARE
#error Defined your equivalent of the /usr/local/share directory before building this file
#endif

/* raj 020422 #include "pgplotcism.h" */

void Hack(Widget w,XtPointer client_data,XtPointer call_data);
void PostWFR(Widget w,XtPointer client_data,XtPointer call_data);
InstrumentAnalysis GInstAnal={
     Instrument_LP|Antenna_Any,
     Analysis_Raw,
     NULL};

extern UserInterface UI;
/*extern SPECTROGRAM_OBJECT *SpectrogramObject;*/
SPECTROGRAM_OBJECT SpectWFR;

typedef struct{
  char refresh;
  Widget shell,pane;
  Widget chkbox_colorbar/*,lbl_min,text_min,lbl_max,text_max*/;
  Widget chkbox_packcapt/*,lbl_period,text_period,lbl_grace,text_grace*/;
  Widget txtent_min,txtent_max,txtent_period,txtent_grace;
  Widget scale_resol;
  SPECTROGRAM_OBJECT *so;
  }SpectrogramSetup;
  
typedef struct{
  Widget dialog,caller,checkbox,radiobox;		/* Dialog Box Attributes */
  Widget pushbutton,textfield,label;	
  }DataLogDialogBox;

typedef struct{
  char *instrument_name;   /* Instrument Name */
  char *filename;	   /* file to be written */
  char **optionlist;	   /* List of checkable options, Null terminated */
  long *modelist;	   /* List of checkable options */
  ulong instrument_type;
  long mode;		   /* Data types to be logged */
  long start_stop_immediate;
  DataLogDialogBox *dldb;
  }DataLog;

/*---------------------------*/
int num_dsp_pnl=18;
DisplayPanel dsp_pnl[18];
/*---------------------------*/

extern CPGPlotWindow *MainPlotWindow;


/*------------------*/


/*
void SelectAnalysis_Post(Widget w,XtPointer client_data,XtPointer call_data);
void SelectAnalysis_Control(Widget w,XtPointer client_data,XtPointer call_data);
*/
typedef struct{
  Widget shell,ant,mode,anal;	
}SelectAnalysis_Struct;

void DisplayLayout(Widget w,XtPointer client_data,XtPointer call_data);
void DisplayConfigure(Widget w,XtPointer client_data,XtPointer call_data);
void DisplayConfigure_Control(Widget w,XtPointer client_data,XtPointer call_data);


void DisplayResize(Widget w,XtPointer client_data,XEvent *event,Boolean *reg);
void DisplayPanelSelect(Widget w,XtPointer client_data,XEvent *event,Boolean *reg);


void MemoryAllocation_Post(Widget w,XtPointer client_data,XtPointer call_data)
{system("ls -l");}

/*------------------*/
/*------------------*/
/* -------- Menu Callback functions -------- */

/*------------------*/
/*------------------*/


void pucdb(Widget w,XtPointer client_data,XtPointer call_data);




void SpectrogramSetupDialog(Widget w,XtPointer client_data,XtPointer call_data);
void SpectrogramSetupDialog_Control(Widget w,XtPointer client_data,XtPointer call_data);
void SpectrogramSetupDialog_Action(Widget w,XtPointer client_data,XtPointer call_data);


void telemetry_source_filename(Widget w,XtPointer client_data,XtPointer call_data);
void telemetry_source_direction(Widget w,XtPointer client_data,XtPointer call_data);
void telemetry_source_filter(Widget w,XtPointer client_data,XtPointer call_data);

/* 020430 raj */
void TstCstDlg(Widget w,XtPointer client_data,XtPointer call_data);

void Refresh(Widget w,XtPointer client_data,XtPointer call_data);

void WaveNormalVector(Widget w,XtPointer client_data,XtPointer call_data);
void tmpwnv(Widget w,XtPointer client_data,XtPointer call_data);

char *mfr_optionlist[]={"Raw Data",NULL};
char *lfdr_optionlist[]={"Raw Data",NULL};
char *wbr_optionlist[]={"Raw Data","FFT Magnitude","FFT Phase","FFT Magnitude Avg.",NULL};
char *wfr_optionlist[]={"Raw Data","FFT Magnitude","FFT Phase","FFT Magnitude Avg.",NULL};
	
long mfr_modelist[]={Analysis_Raw};
long lfdr_modelist[]={Analysis_Raw};
long wbr_modelist[]={Analysis_Raw,Analysis_Mag,Analysis_Phs,Analysis_Avg};
long wfr_modelist[]={Analysis_Raw,Analysis_Mag,Analysis_Phs,Analysis_Avg};

DataLog MFR_DataLog={"MFR",  "mfr.log", mfr_optionlist, mfr_modelist, Instrument_MFR|Antenna_Any, 0,0,NULL};
DataLog LFDR_DataLog={"LFDR","lfdr.log",lfdr_optionlist,lfdr_modelist,Instrument_LFDR|Antenna_Any,0,0,NULL};
DataLog WBR_DataLog={"WBR",  "wbr.log", wbr_optionlist, wbr_modelist, Instrument_WBR|Antenna_Any, 0,0,NULL};
DataLog WFR_DataLog={"WFR",  "wfr.log", wfr_optionlist, wfr_modelist, Instrument_WFR|Antenna_Any, 0,0,NULL};

void DataLogging(Widget w,XtPointer client_data,XtPointer call_data);
MenuItem logging[]={
	{"MFR", &xmPushButtonWidgetClass,'M',NULL,NULL,DataLogging,&MFR_DataLog, NULL},
	{"LFDR",&xmPushButtonWidgetClass,'L',NULL,NULL,DataLogging,&LFDR_DataLog,NULL},
	{"WBR", &xmPushButtonWidgetClass,'B',NULL,NULL,DataLogging,&WBR_DataLog, NULL},
	{"WFR", &xmPushButtonWidgetClass,'F',NULL,NULL,DataLogging,&WFR_DataLog, NULL},
	{NULL}
};	
MenuItem receiver_menu[]={
	{"MFR", &xmPushButtonWidgetClass,'M',NULL,NULL,0,0,NULL},
	{"LFDR",&xmPushButtonWidgetClass,'L',NULL,NULL,0,0,NULL},
	{"WBR", &xmPushButtonWidgetClass,'B',NULL,NULL,0,0,NULL},
	{"WFR", &xmPushButtonWidgetClass,'F',NULL,NULL,0,0,NULL},
	{NULL}
};
MenuItem inputfilter_menu[]={
	{"LFDR",&xmToggleButtonWidgetClass,'L',NULL,NULL,telemetry_source_filter,(XtPointer)Instrument_LFDR,NULL},
	{"LP",  &xmToggleButtonWidgetClass,'P',NULL,NULL,telemetry_source_filter,(XtPointer)Instrument_LP,  NULL},
	{"HFR", &xmToggleButtonWidgetClass,'H',NULL,NULL,telemetry_source_filter,(XtPointer)Instrument_HFR, NULL},
	{"MFR", &xmToggleButtonWidgetClass,'M',NULL,NULL,telemetry_source_filter,(XtPointer)Instrument_MFR, NULL},
	{"WBR", &xmToggleButtonWidgetClass,'B',NULL,NULL,telemetry_source_filter,(XtPointer)Instrument_WBR, NULL},
	{"WFR", &xmToggleButtonWidgetClass,'F',NULL,NULL,telemetry_source_filter,(XtPointer)Instrument_WFR, NULL},
	{NULL}
};
MenuItem spect_menu[]={
	{"LFDR",&xmPushButtonWidgetClass,'L',NULL,NULL,NULL,NULL,NULL},
	{"MFR", &xmPushButtonWidgetClass,'M',NULL,NULL,NULL,NULL,NULL},
	{"WBR", &xmPushButtonWidgetClass,'B',NULL,NULL,SpectrogramSetupDialog,(XtPointer)(Instrument_WBR|Antenna_Any),NULL},
	{"WFR", &xmPushButtonWidgetClass,'F',NULL,NULL,SpectrogramSetupDialog,(XtPointer)(Instrument_WFR|Antenna_Any),NULL},
	{NULL}
};
MenuItem wave_normal_menu[]={
	{"Perform Analysis",&xmToggleButtonWidgetClass,'P',NULL,NULL,tmpwnv,          0,NULL},
  	{"Change Frequency",&xmPushButtonWidgetClass,  'F',NULL,NULL,WaveNormalVector,0,NULL},
	{NULL}
};


void tmpwnv(Widget w,XtPointer client_data,XtPointer call_data)
{
XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct*)call_data;

  UI.do_wave_normal_vector=cbs->set;

return;
}


void tmpprn(Widget w,XtPointer client_data,XtPointer call_data)
{
char *sName;
int nId;
char *sPrn;
char sFile[128];
char sPGPlot[128];
char sCmd[256];
FILE *handle;
CPGPlotWindow *pWnd;

  sPrn=GetPrintManagerPrintCommand(client_data);
  strcpy(sFile, "ana_temp_ps_XXXXXX");
  mkstemp(sFile);
  sprintf(sPGPlot,"%s/ps",sFile);
  sprintf(sCmd,"%s %s",sPrn,sFile);
 
  pWnd=MainPlotWindow;				/* Just going to borrow this memeory for a while */
  sName=pWnd->device_name;			/* Save device context */
  nId=pWnd->device_id;				/* Save device context */
  pWnd->device_name=sEquals(sPGPlot);		/* Change device context */
  Open_CPGPlotWindow(pWnd);			/* Change device context */
  cpgbbuf();
  UpdateManagerWindow_CPGPlotWindow(pWnd);
  cpgebuf();
  Close_CPGPlotWindow(pWnd);
  free(pWnd->device_name);	
  pWnd->device_name=sName;				/* Restore device context */
  pWnd->device_id=nId;					/* Restore device context */
  SetDeviceContext_CPGPlotWindow(pWnd,sName,nId);	/* Restore device context */
  cpgslct(MainPlotWindow->device_id);	/* A possible bug in PGPlot ???, cpgbbuf needs a device selected */

  system(sCmd);
  sprintf(sCmd,"rm %s",sFile);
  system(sCmd);
  
return;
}

void tmpfun(Widget w,XtPointer client_data,XtPointer call_data)
{
char *filename;
XmFileSelectionBoxCallbackStruct *cbs=(XmFileSelectionBoxCallbackStruct*)call_data;

  if(!XmStringGetLtoR(cbs->value,XmFONTLIST_DEFAULT_TAG,&filename))
    return;/* internal error */
fprintf(stderr,"base=%s\n",GetFileSelectionFilename( ));
fprintf(stderr,"1st=%s\n",filename);
fprintf(stderr,"2nd=%s\n",GetFileSelectionFilename( ));
  XtFree(filename);
return;
} 

void DisplayToggle(Widget w, XtPointer client_data,XtPointer call_data)
{
static int first_time;
static char *sWdg,*sWnd;
static int nWdg,nWnd;
CPGPlotWindow *pWnd;

  if(!first_time){
    sWdg=sEquals(MainPlotWindow->device_name);
    nWdg=MainPlotWindow->device_id;
    sWnd=sEquals("/xs");
    nWnd=cpgopen(sWnd);
    cpgsubp(MainPlotWindow->x_panel,MainPlotWindow->y_panel);
  fprintf(stderr,"%s = %d :: %s = %d\n",sWdg,nWdg,sWnd,nWnd);
  }

  pWnd=MainPlotWindow;
    ++first_time;
  if(first_time%2){
    while(pWnd){
      pWnd->device_name=sWdg;
      pWnd->device_id=nWdg;
      pWnd=NextListElement(pWnd);
      }
  }
  else{
    while(pWnd){
      pWnd->device_name=sWnd;
      pWnd->device_id=nWnd;
      pWnd=NextListElement(pWnd);
      }
  }  
  fprintf(stderr,"%s = %d \n",MainPlotWindow->device_name,MainPlotWindow->device_id);
  
return;
}


int work(void);

Boolean work_wrapper(XtPointer client_data)
{return (Boolean)work();}


Widget PltWnd,PltWnd2;
void anagui(int argc,char *argv[])
{
char chtmp[256];
int i,n;
Widget toplevel,mainwindow,MenuBar,Pane,RowColumn,ScrolledText;
Arg args[20];
Atom WM_DELETE_WINDOW;
Widget wtmp,*pw;
XmString string;
XtAppContext app;
XtWorkProcId work_id;
/* raj 020422 Pixmap bitmap; */

MenuItem file_menu[]={ 
	{"Open",&xmPushButtonWidgetClass,'O',NULL,NULL,PostFileManagerDialog,(XtPointer)telemetry_source_filename,NULL},
	/*{"Close",&xmPushButtonWidgetClass,'C',NULL,NULL,0,0,NULL},*/
	{"Logging",&xmPushButtonWidgetClass,'L',NULL,NULL,0,0,logging},
	{"Print",&xmPushButtonWidgetClass,'P',NULL,NULL,PostPrintManagerDialog,(XtPointer)tmpprn,NULL},

	{"Separator",&xmSeparatorWidgetClass,'\0',NULL,NULL,0,0,NULL},
			
	{"Configure",&xmPushButtonWidgetClass,'f',NULL,NULL,DisplayLayout,(XtPointer)NULL,NULL},
	{"Memory",&xmPushButtonWidgetClass,'m',NULL,NULL,MemoryAllocation_Post,(XtPointer)NULL,NULL},
/*
	{"PostWFR",&xmPushButtonWidgetClass,'w',NULL,NULL,SelectAnalysis_Post,(XtPointer)&GInstAnal,NULL},
	{"Fit",&xmPushButtonWidgetClass,'t',NULL,NULL,Hack,(XtPointer)NULL,NULL},
*/
	{"Exit",&xmPushButtonWidgetClass,'x',NULL,NULL,Exit,0,NULL},
	{NULL}
};
MenuItem prn_menu[]={ 
	{"Manager",&xmPushButtonWidgetClass,'M',NULL,NULL,PostPrintManagerDialog,(XtPointer)tmpprn,NULL},
	{NULL}
};
MenuItem step_menu[]={ 
	{"Controls",&xmPushButtonWidgetClass,'C',NULL,NULL,PostRecorderDialog,(XtPointer)telemetry_source_direction,NULL},
	{NULL}
};
 
MenuItem option_menu[]={
	{"Controls",   &xmPushButtonWidgetClass,'C',NULL,NULL,PostRecorderDialog,(XtPointer)telemetry_source_direction,NULL},
	{"Refresh",    &xmPushButtonWidgetClass,'R',NULL,NULL,Refresh,NULL,NULL},
	{"Spectogram", &xmPushButtonWidgetClass,'S',NULL,NULL,NULL,NULL,spect_menu},
	{NULL}
};

/*
MenuItem  view_menu[]={
	{"Scroll Bars",  &xmToggleButtonWidgetClass,'S',NULL,NULL,PostRecorderDialog,(XtPointer)telemetry_source_direction,NULL},
	{"Fit to Screen",&xmPushButtonWidgetClass,'F',NULL,NULL,PostRecorderDialog,(XtPointer)telemetry_source_direction,NULL},
	{"Zoom In",&xmPushButtonWidgetClass,'I',NULL,NULL,PostRecorderDialog,(XtPointer)telemetry_source_direction,NULL},
	{"Zoom Out",&xmPushButtonWidgetClass,'O',NULL,NULL,PostRecorderDialog,(XtPointer)telemetry_source_direction,NULL},
	NULL};
*/
	
MenuItem analysis_menu[]={
  	{"WaveNormalVector",&xmPushButtonWidgetClass,'A',NULL,NULL,NULL,0,wave_normal_menu},
	{NULL}
};
  	
/* Read/Setup Initial Values */

  UI.data_direction=RecorderDirectionPlayForward;
  XtSetLanguageProc(NULL,NULL,NULL);
  toplevel=XtVaAppInitialize(&app,"toplevel",NULL,0,&argc,argv,NULL,NULL); 
/*  toplevel=XtVaAppInitialize(&app,"toplevel",NULL,0,NULL,NULL,NULL,NULL);*/ 

  mainwindow=XtVaCreateWidget("mainwindow",xmMainWindowWidgetClass,toplevel,
  	XmNheight, 400,XmNwidth, 400, 
  	/*XmNscrollBarDisplayPolicy,XmAS_NEEDED,
  	XmNscrollingPolicy, XmAUTOMATIC,*/NULL);

  MenuBar=XmCreateMenuBar(mainwindow,"MenuBar",NULL,0);
   wtmp=BuildMenu(MenuBar,XmMENU_PULLDOWN,"File",'F',True,file_menu);
   XtManageChild(wtmp);
   wtmp=BuildMenu(MenuBar,XmMENU_PULLDOWN,"Printer",'P',True,prn_menu);
   XtManageChild(wtmp);
   wtmp=BuildMenu(MenuBar,XmMENU_PULLDOWN,"Option",'O',True,option_menu);
   XtManageChild(wtmp);
   wtmp=BuildMenu(MenuBar,XmMENU_PULLDOWN,"Filter",'t',True,inputfilter_menu);
   XtManageChild(wtmp);
   wtmp=BuildMenu(MenuBar,XmMENU_PULLDOWN,"Analysis",'A',False,analysis_menu);
   XtManageChild(wtmp);
  XtManageChild(MenuBar);

  Pane=XtVaCreateWidget("PannedWindow",xmPanedWindowWidgetClass,mainwindow,NULL);

   n=0;
/*
   XtSetArg(args[n],XmNrows,20);			n++;
   XtSetArg(args[n],XmNcolumns,40);			n++;
*/

  PltWnd=XmCreateScrolledPgplot(Pane,"pgplot",args,n);
  XtManageChild(PltWnd);
 
/*
  PltWnd=XmCreateScrolledPgplot("pgplot",xmPgplotWidgetClass,Pane,
  	XmNuserData,(XtPointer)MainPlotWindow,
    	XmNheight, 600,XmNwidth, 600,XmpCMaxColors,16,NULL);
*/

  XtManageChild(Pane);
  
  XtVaSetValues(mainwindow,XmNmenuBar,MenuBar,XmNworkWindow,Pane,NULL);
  WM_DELETE_WINDOW=XmInternAtom(XtDisplay(toplevel),"WM_DELETE_WINDOW",False);
  XmAddWMProtocolCallback(toplevel,WM_DELETE_WINDOW,Exit,NULL);

  XtManageChild(mainwindow);

/* raj 020422
  bitmap=XCreatePixmapFromBitmapData(XtDisplay(toplevel),RootWindowOfScreen(XtScreen(toplevel)),
  	sorceress_bits,sorceress_width,sorceress_height,1,0,1);
  XtVaSetValues(toplevel,XmNiconPixmap,bitmap,XmNiconName,"Anna",XmNtitle,"Anna",NULL);
  XtRealizeWidget(toplevel);
*/ 

  XtVaSetValues(toplevel,XmNtitle,"RPWS Anna Version 9.01 - square one",NULL);
  XtRealizeWidget(toplevel);


 
 
  MainPlotWindow->device_name=sEquals(xmp_device_name(PltWnd));
  Open_CPGPlotWindow(MainPlotWindow);


  XtAddEventHandler(PltWnd,ButtonPressMask,False,DisplayPanelSelect,(XtPointer)NULL);
  XtAddEventHandler(toplevel,StructureNotifyMask,False,DisplayResize,(XtPointer)NULL);

work_id=XtAppAddWorkProc(app,work_wrapper,(XtPointer)app);

  XtAppMainLoop(app);

return;
}



/* --------------- Event Handlers --------------- */

void DisplayResize(Widget w,XtPointer client_data,XEvent *event,Boolean *reg)
{
static Dimension width,height;
XConfigureEvent *cfgevt=(XConfigureEvent*)event;


  if(cfgevt->type!=ConfigureNotify)  return;

	/* Window size did not change */
  if(width==cfgevt->width && height==cfgevt->height)  return;
  
  width=cfgevt->width;
  height=cfgevt->height;

  Close_CPGPlotWindow(MainPlotWindow);
  Open_CPGPlotWindow(MainPlotWindow);  
  UpdateManagerWindow_CPGPlotWindow(MainPlotWindow);

return;
}

void DisplayPanelSelect(
	Widget w, XtPointer client_data, XEvent *event, Boolean *reg
){
char sTmpFileName[L_tmpnam+1];   /* temp filename plus Null */
char sCmd[L_tmpnam+64];          /* system command string */
float xl,xr,yb,yt;
static int show_this_again;

  if(event->xbutton.button==1)  return;	/* Do not process left mouse clicks */
  
  if(event->xbutton.button==3){	/* process right mouse click, print panel */
  int x_pnl,y_pnl;
  float x_div,y_div,fTmp,fTmp0;
  Arg args[5];
  int n=0;
  Dimension width,height;
  String str;
  Widget wMsg;
  CPGPlotWindow *pWnd,*pWtmp;
  CPGAxis *pAxis;
  CPGLabel *pLabel;
  
/*
    if(show_this_again>=5){
      str=XmStringCreateLocalized("Don't Show This Annoying Dialog Box Anymore");   
    }
    else{
      str=XmStringCreateLocalized("Printing ...");
    }
    ++show_this_again;
    XtSetArg(args[n],XmNmessageString,str);	++n;
    wMsg=XmCreateMessageDialog(w,"Message",args,n);
    XmStringFree(str);
    XtSetSensitive(XmMessageBoxGetChild(wMsg,XmDIALOG_HELP_BUTTON),False);
    XtUnmanageChild(XmMessageBoxGetChild(wMsg,XmDIALOG_CANCEL_BUTTON));
    XtManageChild(wMsg);	XtPopup(XtParent(wMsg),XtGrabNone);
 */
 
    
    XtVaGetValues(w,XmNwidth,&width,XmNheight,&height,NULL);
    x_div=width/MainPlotWindow->x_panel;
    y_div=height/MainPlotWindow->y_panel;
    x_pnl=event->xbutton.x/x_div;	++x_pnl;	/* range 1 to ... */
    y_pnl=event->xbutton.y/y_div;	++y_pnl;	/* range 1 to ... */

    pWtmp=GetSubPanel_CPGPlotWindow(MainPlotWindow,x_pnl,y_pnl);
    if(pWtmp->pUserData==NULL){fprintf(stderr,"Bailing\n");return;}

    /* Create temporary file for the postscript plot */
    pWnd=EqualsListElement(pWtmp);
	 strcpy(sTmpFileName, "ana_temp_ps_XXXXXX");
    mkstemp(sTmpFileName);
    strcpy(sCmd,sTmpFileName);
    strcat(sCmd,"/ps");

    pWnd->device_name=sEquals(sCmd); /* pgplot.ps/ps */
    pAxis=pWnd->pAxis;     /* just borrowing this memory for a little while */
    pLabel=pWnd->pLabel;   /* just borrowing this memory for a little while */
    Open_CPGPlotWindow(pWnd);
    fTmp=pAxis->char_height;
    fTmp0=pLabel->char_height;
    pAxis->char_height=1.0;
    pLabel->char_height=2.0;
    UpdateWindow_CPGPlotWindow(pWnd);
    Close_CPGPlotWindow(pWnd);
    pAxis->char_height=fTmp;	/* done borrowing */
    pLabel->char_height=fTmp0;	/* done borrowing */

    sprintf(sCmd,"lp -c -onb %s;rm -f %s",sTmpFileName,sTmpFileName);
    system(sCmd);
    free(pWnd->device_name);	/* free malloc data */
    DeleteListElement(pWnd);

    /* A possible bug in PGPlot ???, cpgbbuf needs a device selected */
    cpgslct(MainPlotWindow->device_id);	
  }
  
/*
fprintf(stderr,"Button # %d\n",event->xbutton.button);
fprintf(stderr,"Xwnd:%d %d\t",event->xbutton.x,event->xbutton.y);
cpgqvp(3,&xl,&xr,&yb,&yt);
fprintf(stderr,"VP: %f %f %f %f\t",xl,xr,yb,yt);
cpgqvsz(3,&xl,&xr,&yb,&yt);
fprintf(stderr,"Sur: %f %f %f %f\n",xl,xr,yb,yt);
*/

return;
}

/* --------------- Event Handlers --------------- */




void DataLogging(Widget w,XtPointer client_data,XtPointer call_data)
{
DataLog *dls=(DataLog*)client_data;
DataLogDialogBox *dldb;
void DataLoggingControl(Widget w,XtPointer client_data,XtPointer call_data);
void DataLoggingAction(Widget w,XtPointer client_data,XtPointer call_data);

  if(!dls->dldb){
    int numbtn,n;
    Atom WM_DELETE_WINDOW;
    Arg args[30];
    Widget pane,frame,wtmp;
    XmString str;
    XmStringTable strtbl;
    ActionAreaItem action_area[]={
    	{"Ok",DataLoggingAction,NULL},
      {"Apply",DataLoggingAction,NULL},
      {"Cancel",DataLoggingAction,NULL},
      {NULL}
    };
    
    dls->dldb=(DataLogDialogBox*)malloc(sizeof(DataLogDialogBox));
    dldb=dls->dldb;
    
    dldb->caller=w;
    while(w && !XtIsWMShell(w))  w=XtParent(w);
    dldb->dialog=XtVaCreatePopupShell("dialog",xmDialogShellWidgetClass,w,XmNdeleteResponse,XmDO_NOTHING,NULL);
    pane=XtVaCreateWidget("pane",xmPanedWindowWidgetClass,dldb->dialog,XmNsashWidth,1,XmNsashHeight,1,XmNseparatorOn,False,NULL);

    frame=XtVaCreateWidget("frame",xmFrameWidgetClass,pane,NULL);
     XtVaCreateManagedWidget("Data Logging Options",xmLabelWidgetClass,frame,XmNchildType,XmFRAME_TITLE_CHILD,
     	XmNchildVerticalAlignment,XmALIGNMENT_WIDGET_TOP,NULL);
     numbtn=0;
     while(dls->optionlist[numbtn])  
       ++numbtn;						/* find the number of buttons */
     strtbl=(XmStringTable)XtMalloc(numbtn*sizeof(XmString));
     for(n=0;n<numbtn;n++)
       strtbl[n]=XmStringCreateLocalized(dls->optionlist[n]);
     n=0;
     XtSetArg(args[n],XmNbuttonCount,numbtn);++n;
     XtSetArg(args[n],XmNbuttonType,XmCHECKBUTTON);++n;
     XtSetArg(args[n],XmNbuttons,strtbl);++n;
     XtSetArg(args[n],XmNsimpleCallback,DataLoggingControl);++n;
     dldb->checkbox=XmCreateSimpleCheckBox(frame,"checkbox",args,n);
     XtManageChild(dldb->checkbox);
     for(n=0;n<numbtn;n++)
       XmStringFree(strtbl[n]);
     XtFree((char*)strtbl);
    XtManageChild(frame);

    frame=XtVaCreateWidget("frame",xmFrameWidgetClass,pane,NULL);
     XtVaCreateManagedWidget("Logging Control",xmLabelWidgetClass,frame,XmNchildType,XmFRAME_TITLE_CHILD,
     	XmNchildVerticalAlignment,XmALIGNMENT_BASELINE_TOP,NULL);
     numbtn=2;							/* Start Stop */
     strtbl=(XmStringTable)XtMalloc(numbtn*sizeof(XmString));
     strtbl[0]=XmStringCreateLocalized("Start");
     strtbl[1]=XmStringCreateLocalized("Stop");
     n=0;
     XtSetArg(args[n],XmNbuttonCount,numbtn);++n;
     XtSetArg(args[n],XmNbuttonType,XmRADIOBUTTON);++n;
     XtSetArg(args[n],XmNbuttons,strtbl);++n;
     XtSetArg(args[n],XmNbuttonSet,1);++n;
     XtSetArg(args[n],XmNsimpleCallback,DataLoggingControl);++n;
     dldb->radiobox=XmCreateSimpleRadioBox(frame,"radiobox",args,n);
     XtManageChild(dldb->radiobox);
     for(n=0;n<numbtn;n++)
       XmStringFree(strtbl[n]);
     XtFree((char*)strtbl);
    XtManageChild(frame);

    str=XmStringCreateLocalized("Log Current Data");
    wtmp=XtVaCreateManagedWidget("logimmediate",xmPushButtonWidgetClass,pane,XmNlabelString,str,NULL);
    XmStringFree(str);
    XtAddCallback(wtmp,XmNactivateCallback,DataLoggingAction,(XtPointer)dls);
    
    str=XmStringCreateLocalized(dls->filename);
    dldb->label=XtVaCreateManagedWidget("label",xmLabelWidgetClass,pane,XmNlabelString,str,
    	XmNalignment,XmALIGNMENT_BEGINNING,NULL);
    XmStringFree(str);
    
    dldb->textfield=XtVaCreateManagedWidget("textfield",xmTextWidgetClass,pane,NULL);
    XtAddCallback(dldb->textfield,XmNactivateCallback,DataLoggingControl,(XtPointer)dls);

    action_area[0].data=(XtPointer)dls;
    action_area[1].data=(XtPointer)dls;
    action_area[2].data=(XtPointer)dls;
    CreateActionArea(pane,action_area,20);
    XtManageChild(pane);
    TurnOffSashTraversal(pane);

    WM_DELETE_WINDOW=XmInternAtom(XtDisplay(dldb->dialog),"WM_DELETE_WINDOW",False);
    XmAddWMProtocolCallback(dldb->dialog,WM_DELETE_WINDOW,DataLoggingAction,(XtPointer)dls);
    XtVaSetValues(dldb->dialog,XmNtitle,dls->instrument_name,NULL);
    XtAddCallback(dldb->dialog,XmNpopdownCallback,ToggleSensitivity,(XtPointer)dldb->caller);
    XtAddCallback(dldb->dialog,XmNpopupCallback,ToggleSensitivity,(XtPointer)dldb->caller);
    ToggleSensitivity(dldb->dialog,(XtPointer)dldb->caller,NULL);
    }

  XtPopup(dls->dldb->dialog,XtGrabNone);

return;
}

void DataLoggingControl(Widget w,XtPointer client_data,XtPointer call_data)
{
char *pch;
XmAnyCallbackStruct *cbs=(XmAnyCallbackStruct*)call_data;
XmString string;
DataLog *dls=(DataLog*)client_data;

  switch(cbs->reason){				/* Checkbox Registered but isn't used */
    case XmCR_ACTIVATE :
      pch=XmTextGetString(w);
      string=XmStringCreateLocalized(pch);
      XtVaSetValues(dls->dldb->label,XmNlabelString,string,NULL);
      XmStringFree(string);
      XtFree(pch);
      break;
    default :
      break;
    }
  
return;
}

void DataLoggingAction(Widget w,XtPointer client_data,XtPointer call_data)
{
char *pch;
Boolean set;
int i,wcnt;
Widget *pw;
XmString string;
DataLog *x=(DataLog*)client_data;

  if(!strcmp(XtName(w),"logimmediate")){
    XtVaGetValues(x->dldb->checkbox,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
    UI.log_mode=0;     /* Unnecessary ??? */
    for(i=0;i<wcnt;i++){
      XtVaGetValues(pw[i],XmNset,&set,NULL);
      if(set)	UI.log_mode |=  x->modelist[i];
      else	UI.log_mode &= ~x->modelist[i];
      }
    XtVaGetValues(x->dldb->label,XmNlabelString,&string,NULL);
    XmStringGetLtoR(string,XmFONTLIST_DEFAULT_TAG,&pch);
    strncpy(UI.filename,pch,64);
    XtFree(pch);
    XmStringFree(string);
    UI.log_inst_type=x->instrument_type;
    UI.log_immediately=1;
    UI.log_immediately=0;
    UI.log_inst_type=0;
    UI.log_mode=0;
    return;
    }

  if(strcmp(XtName(w),"Apply"))
    XtPopdown(x->dldb->dialog); 

  XtVaGetValues(x->dldb->checkbox,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
  if((!strcmp(XtName(w),"Ok")) || (!strcmp(XtName(w),"Apply"))){
    for(i=0;i<wcnt;i++){
      XtVaGetValues(pw[i],XmNset,&set,NULL);
      if(set)	x->mode |=  x->modelist[i];
      else	x->mode &= ~x->modelist[i];
      }
    XtVaGetValues(x->dldb->label,XmNlabelString,&string,NULL);
    XmStringGetLtoR(string,XmFONTLIST_DEFAULT_TAG,&pch);
    if(strcmp(x->filename,pch)){			/* filename changed */
      free(x->filename);
      x->filename=(char*)malloc(strlen(pch)+1);
      strcpy(x->filename,pch);
      }
    XtFree(pch);
    XmStringFree(string);
    XtVaGetValues(x->dldb->radiobox,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
    XtVaGetValues(pw[0],XmNset,&set,NULL);             /* Start Radiobutton */
    if(set)	x->start_stop_immediate=1;
    else	x->start_stop_immediate=0;

/*---------------------------------------*/
  strncpy(UI.filename,x->filename,64);
  UI.log_inst_type=x->instrument_type;
  if(x->start_stop_immediate==1)
    UI.log_mode=x->mode;
  else
    UI.log_mode=0;
  UI.log_inst_type=0;	
/*---------------------------------------*/
    
    }  
  else{
    for(i=0;i<wcnt;i++){
      if(x->mode&x->modelist[i])
        XtVaSetValues(pw[i],XmNset,True,NULL);
      else
        XtVaSetValues(pw[i],XmNset,False,NULL);
      }
    string=XmStringCreateLocalized(x->filename);
    XtVaSetValues(x->dldb->label,XmNlabelString,string,NULL);
    XmStringFree(string);
    XtVaGetValues(x->dldb->radiobox,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
    if(x->start_stop_immediate==1){
      XtVaSetValues(pw[0],XmNset,True,NULL);          	/* Start Radiobutton */
      XtVaSetValues(pw[1],XmNset,False,NULL);		/* Stop Radiobutton */
      }
    else{
      XtVaSetValues(pw[0],XmNset,False,NULL);          	/* Start Radiobutton */
      XtVaSetValues(pw[1],XmNset,True,NULL);		/* Stop Radiobutton */
      }
    }
    
return;
}



void WaveNormalVector(Widget w,XtPointer client_data,XtPointer call_data)
{
int n;
Arg args[10];
XmString str;
Widget dialog=0;

void WaveNormalVector_Ok(Widget w,XtPointer client_data,XtPointer call_data);
void WaveNormalVector_Cancel(Widget w,XtPointer client_data,XtPointer call_data);



/*  XtVaGetValues(w,XmNuserData,&dialog,NULL);*/
  if(!dialog){
    n=0;
    str=XmStringCreateLocalized("Enter WFR Frequency in Hz:");
    XtSetArg(args[n],XmNselectionLabelString,str);	n++;
    XtSetArg(args[n],XmNautoUnmanage,False);		n++;
    dialog=XmCreatePromptDialog(w,"prompt",args,n);
    XmStringFree(str);
    XtAddCallback(dialog,XmNokCallback,WaveNormalVector_Ok,(XtPointer)w);
    XtAddCallback(dialog,XmNcancelCallback,WaveNormalVector_Cancel,(XtPointer)w);
    XtManageChild(dialog);
    }
  XtPopup(XtParent(dialog),XtGrabNone);
  XtSetSensitive(w,False);

return;
}


void WaveNormalVector_Ok(Widget w,XtPointer client_data,XtPointer call_data)
{
char *pch;
float freq;
Widget caller=(Widget)client_data;
XmSelectionBoxCallbackStruct *cbs=(XmSelectionBoxCallbackStruct*)call_data;

  XtSetSensitive(caller,True);
  XmStringGetLtoR(cbs->value,XmFONTLIST_DEFAULT_TAG,&pch);
  UI.WaveNormalFrequency=atof(pch);
  XtFree(pch);
  XtDestroyWidget(w);
  
return;
}

void WaveNormalVector_Cancel(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget caller=(Widget)client_data;

  XtSetSensitive(caller,True);
  XtDestroyWidget(w);

return;
}


/*--------------------------------------------------------------*/
/*								*/
/*--------------------------------------------------------------*/
void telemetry_source_filename(Widget w,XtPointer client_data,XtPointer call_data)
{
char *filename,*errmsg;
XmFileSelectionBoxCallbackStruct *cbs=(XmFileSelectionBoxCallbackStruct*)call_data;
static char *old_file_name;

  if(!XmStringGetLtoR(cbs->value,XmFONTLIST_DEFAULT_TAG,&filename))
    return;/* internal error */
  if(filename){
    close_telemetry_infile("doesn't matter");
    if( (errmsg=open_telemetry_infile(filename)) )
      PostErrorDialog(w,errmsg);
    }
fprintf(stderr,"Current File Name (%s)\n",filename);
  XtFree(filename);

return;
}

void telemetry_source_direction(Widget w,XtPointer client_data,XtPointer call_data)
{

  UI.data_direction=GetCurrentRecorderDirection(client_data);

return;
}

void telemetry_source_filter(Widget w,XtPointer client_data,XtPointer call_data)
{
long data_type=(long)client_data;
XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct*)call_data;

  if(cbs->set)  UI.input_filter&=(~data_type);		/* Do Not Pass This Type of Data */
  else		UI.input_filter|=(data_type);		/* Pass This Type of Data */

return;
}

void Refresh(Widget w,XtPointer client_data,XtPointer call_data)
{
/*char *device=xmp_device_name(UI.window);/ **/
char *device="/xs";

UpdateManagerWindow_CPGPlotWindow(MainPlotWindow);


return;
}



void SpectrogramSetupDialog(Widget w,XtPointer client_data,XtPointer call_data)
{
void SSDC_Control_1(Widget w,XtPointer client_data,XtPointer call_data);
void SSDC_Control_2(Widget w,XtPointer client_data,XtPointer call_data);

char rcvtyp[128];
int wcnt;
Atom WM_DELETE_WINDOW;
XmAnyCallbackStruct *cbs=(XmAnyCallbackStruct*)call_data;
Widget *pw;
Widget pane,frame,form,rowcolumn,toggle,checkbox,rc1,rc2,tf1,tf2,wtmp;
Widget label;
XmString str,str0,str1,str2,str3,str4;
ToggleBoxItem color_bar[]={
	{"Gray Scale",         False,NULL,0},
	{"Show Color Wedge",   True, NULL,0},
	{"Use Default Min/Max",False, SSDC_Control_1,0},
	{NULL}
};
ToggleBoxItem packet_capture[]={
	{"Show Missing Packets (Black)",False,NULL,0},
	{"Show Extra Packets (White)",  False,NULL,0},
	{"Use Default Period",          False, SSDC_Control_2,0},
	{"No Grace Period",             False, SSDC_Control_2,0},
	{NULL}
};
ActionAreaItem oac[]={		/* char *label,void (*callback)(),XtPointer data */
	{"Ok",    SpectrogramSetupDialog_Action,NULL},
	{"Apply", SpectrogramSetupDialog_Action,NULL},
	{"Cancel",SpectrogramSetupDialog_Action,NULL},
	{NULL}
};
SPECTROGRAM_OBJECT *so_default;
SpectrogramSetup *ss;
  

  XtVaGetValues(w,XmNuserData,&ss,NULL);
  if(!ss){
    if((ss=(SpectrogramSetup*)malloc(sizeof(SpectrogramSetup)))==NULL){
      fprintf(stderr,"Unable to malloc, spectrogramSetupDialog(ss)\n");
      return;
      }
    if((ss->so=(SPECTROGRAM_OBJECT*)malloc(sizeof(SPECTROGRAM_OBJECT)))==NULL){
      fprintf(stderr,"Unable to malloc, spectrogramSetupDialog(so)\n");
      return;
      }
    XtVaSetValues(w,XmNuserData,(XtPointer)ss,NULL);
    color_bar[2].data=(XtPointer)ss;
    packet_capture[2].data=(XtPointer)ss;
    packet_capture[3].data=(XtPointer)ss;

    ss->so->type=(long)client_data;
/*
    so_default=find_current_spectrogram_object(ss->so->type);
*/
    
    sprintf(rcvtyp,"%s Spectrogram Setup",get_receiver_string((long)client_data));	/* client_data holds the receiver data */
    ss->shell=XtVaCreatePopupShell("shell",xmDialogShellWidgetClass,w,
      		XmNtitle,rcvtyp,XmNdeleteResponse,XmDO_NOTHING,NULL);
    pane=XtVaCreateWidget("paned_window",xmPanedWindowWidgetClass,ss->shell,
      		XmNsashWidth,1,XmNsashHeight,1,XmNseparatorOn,False,NULL);

     rowcolumn=XtVaCreateWidget("rowcolumn",xmRowColumnWidgetClass,pane,
    		XmNorientation,XmHORIZONTAL,/*XmNpacking,XmPACK_COLUMN,XmNnumColumns,2,*/NULL);
      frame=XtVaCreateWidget("frame",xmFrameWidgetClass,rowcolumn,NULL);
      str0=XmStringCreateLocalized("Color Bar");
      XtVaCreateManagedWidget("label",xmLabelWidgetClass,frame,XmNlabelString,str0,
      		XmNchildType,XmFRAME_TITLE_CHILD,XmNchildVerticalAlignment,XmALIGNMENT_CENTER,NULL);
      XmStringFree(str0);	
      rc1=XtVaCreateWidget("row_column",xmRowColumnWidgetClass,frame,NULL);
       ss->chkbox_colorbar=CreateToggleBox(rc1,color_bar,False,False,1);
       XtManageChild(ss->chkbox_colorbar);
       ss->txtent_min=CreateTextEntry(rc1,"     Min : ",7,"left");
       ss->txtent_max=CreateTextEntry(rc1,"     Max : ",7,"left");
      XtManageChild(rc1);
      XtManageChild(frame);

      frame=XtVaCreateWidget("frame",xmFrameWidgetClass,rowcolumn,NULL);
      str0=XmStringCreateLocalized("Packet Capture");
      XtVaCreateManagedWidget("label",xmLabelWidgetClass,frame,XmNlabelString,str0,
      		XmNchildType,XmFRAME_TITLE_CHILD,XmNchildVerticalAlignment,XmALIGNMENT_CENTER,NULL);
      XmStringFree(str0);	
      rc1=XtVaCreateWidget("row_column",xmRowColumnWidgetClass,frame,NULL);
       ss->chkbox_packcapt=CreateToggleBox(rc1,packet_capture,False,False,1);
       XtManageChild(ss->chkbox_packcapt);
       ss->txtent_period=CreateTextEntry(rc1,"     Period (Sec) : ",7,"left");
       ss->txtent_grace=CreateTextEntry(rc1,"     Grace  (Sec) : ",7,"left");
      XtManageChild(rc1);
      XtManageChild(frame);
     XtManageChild(rowcolumn);
     ss->scale_resol=XtVaCreateManagedWidget("scale",xmScaleWidgetClass,pane,XmNorientation,
     		XmHORIZONTAL,XmNminimum,1,XmNmaximum,15,XmNvalue,1,XmNshowValue,True,
     		XmNuserData,1,NULL);
     str0=XmStringCreateLocalized("Resolution [pixels/capture]");
     XtVaCreateManagedWidget("label",xmLabelWidgetClass,pane,XmNlabelString,str0,NULL);
     XmStringFree(str0);

     oac[0].data=(XtPointer)ss;
     oac[1].data=(XtPointer)ss;
     oac[2].data=(XtPointer)ss;
     CreateActionArea(pane,oac,2);
    
    XtManageChild(pane);
    TurnOffSashTraversal(pane);
    ss->pane=pane;

    WM_DELETE_WINDOW=XmInternAtom(XtDisplay(ss->shell),"WM_DELETE_WINDOW",False);
    XmAddWMProtocolCallback(ss->shell,WM_DELETE_WINDOW,PopdownShell,(XtPointer)(ss->shell));
    XtAddCallback(ss->shell,XmNpopdownCallback,ToggleSensitivity,(XtPointer)w);
    XtAddCallback(ss->shell,XmNpopupCallback,  ToggleSensitivity,(XtPointer)w);
    XtSetSensitive(w,False);

    /* Press Some Default Buttons */
    XtVaGetValues(ss->chkbox_colorbar,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
      XtVaSetValues(pw[0],XmNsensitive,False,NULL);		/* Gray out until working */
      XtVaSetValues(pw[1],XmNsensitive,False,NULL);		/* Gray out until working */
    XtCallActionProc(pw[2],"ArmAndActivate",cbs->event,NULL,0); /* Show defaults */
    XtVaGetValues(ss->chkbox_packcapt,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
      XtVaSetValues(pw[0],XmNsensitive,False,NULL);		/* Gray out until working */
      XtVaSetValues(pw[1],XmNsensitive,False,NULL);		/* Gray out until working */
    XtCallActionProc(pw[2],"ArmAndActivate",cbs->event,NULL,0); /* Show defaults */
    XtCallActionProc(pw[3],"ArmAndActivate",cbs->event,NULL,0); /* Show defaults */

/*
    if(XmIsSash(children[num_children]))
      XtVaSetValues(children[num_children],XmNtraversalOn,False,NULL);
*/
/*    XtCallActionProc(default_pb,"ArmAndActivate",cbs->event,NULL,0);*/
    }

  XtPopup(ss->shell,XtGrabNone);

return;
}

void SSDC_Control_1(Widget w,XtPointer client_data,XtPointer call_data)
{/* Set sensitivity of label and text (min and max)*/
char *pch;
Boolean sensitivity;
XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct*)call_data;
SpectrogramSetup *ss=(SpectrogramSetup*)client_data;

  if(cbs->set){
    /* Need to Get Min/Max from somewhere */
    SetTextEntryString(ss->txtent_min,"-120.0");
    SetTextEntryString(ss->txtent_max,"0.0");
    sensitivity=False;
    } 
  else{
    sensitivity=True;
    }		
  SetWidgetSensitivity(ss->txtent_min,sensitivity);
  SetWidgetSensitivity(ss->txtent_max,sensitivity);

return;
}

void SSDC_Control_2(Widget w,XtPointer client_data,XtPointer call_data)
{/* Set sensitivity of label and text (min and max)*/
char *pch;
Boolean sensitivity;
XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct*)call_data;
Widget txtent;
SpectrogramSetup *ss=(SpectrogramSetup*)client_data;

  if(!strcmp("button_2",XtName(w))){		/* Toggle Button Period */
    txtent=ss->txtent_period;
    if(cbs->set)  SetTextEntryString(txtent,"24.0");
    }
  else if(!strcmp("button_3",XtName(w))){		/* Toggle Button Grace Period */
    txtent=ss->txtent_grace;
    if(cbs->set)  SetTextEntryString(txtent,"0.0");
    }
  else{
    return;
    }
  sensitivity=(cbs->set?False:True);
  SetWidgetSensitivity(txtent,sensitivity);

return;
}

void SpectrogramSetupDialog_Action(Widget w,XtPointer client_data,XtPointer call_data)
{
char *pch;
int ival;
XtPointer usrdat;
SpectrogramSetup *ss=(SpectrogramSetup*)client_data;

  ss->so->refresh=0;
  if( !strcmp("Ok",XtName(w)) || !strcmp("Apply",XtName(w)) ){	/* Ok or Apply */
    XmScaleGetValue(ss->scale_resol,&ival);
    if(ival!=(int)ss->so->resolution)
      ss->so->refresh=1;
    ss->so->resolution=ival;
    pch=GetTextEntryString(ss->txtent_min);
    if(ss->so->color_bar_min!=(float)atof(pch))
      ss->so->refresh=1;
    ss->so->color_bar_min=atof(pch);
    XtFree(pch);
    pch=GetTextEntryString(ss->txtent_max);
    if(ss->so->color_bar_max!=(float)atof(pch))
      ss->so->refresh=1;
    ss->so->color_bar_max=atof(pch);
    XtFree(pch);

    ss->so->show_missing_packets=GetToggleBoxButtonState(ss->chkbox_packcapt,0);
    ss->so->show_extra_packets=GetToggleBoxButtonState(ss->chkbox_packcapt,1);

    pch=GetTextEntryString(ss->txtent_period);
    if(ss->so->period!=(float)atof(pch))
      ss->so->refresh=1;
    ss->so->period=atof(pch);
    XtFree(pch);
    pch=GetTextEntryString(ss->txtent_grace);
    ss->so->grace=atof(pch);
    XtFree(pch);

    set_spectrogram_objects(ss->so->type,ss->so);	/* Save state to all of like type */

    SaveWidgetValue(ss->pane);
    if(!strcmp("Ok",XtName(w)))
      XtPopdown(ss->shell);
    }
  else{							/* Cancel */
    RestoreWidgetValue(ss->pane);
    XtPopdown(ss->shell);
    }

return;
}


void Hack(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget wMain;
Dimension width,height,border_width;

  wMain=w;
  while(wMain && !XtIsWMShell(wMain))  wMain=XtParent(wMain);
  
  XtVaGetValues(PltWnd,XmNwidth,&width,XmNheight,&height,XmNborderWidth,&border_width,NULL);
fprintf(stderr,"PltWnd %d %d\n",width,height);

  XtVaGetValues(wMain,XmNwidth,&width,XmNheight,&height,NULL);
fprintf(stderr,"Main %d %d\n",width,height);

  XtResizeWidget(PltWnd,width,height,border_width);
return;
}


/*-------------------------------------------------------------------------
 	                     New and Improved                                            
---------------------------------------------------------------------------*/



void DisplayLayout(Widget w,XtPointer client_data,XtPointer call_data)
{
char chtmp[128];
int i,j,max_rows,max_cols;
uint32_t data_type;
Atom WM_DELETE_WINDOW;
Widget shell,pane,row_column,push_btn;
XmString str;
CPGPlotWindow *pMngWnd,*pWnd;
WindowSetup *WndSetup;
ActionAreaItem oac[]={	/* char *label,void (*callback)(),XtPointer data */	
	{"Ok",    DestroyShell,(XtPointer)NULL},
	{"Apply", DestroyShell,(XtPointer)NULL},
	{"Cancel",DestroyShell,(XtPointer)NULL},
	{NULL}
};

     pMngWnd=MainPlotWindow;
     max_cols = pMngWnd->x_panel;
     max_rows = pMngWnd->y_panel;
fprintf(stderr,"%d %d\n",max_cols,max_rows);

	/* Create the data structure off the Heap and attach it to a manager
	   widget in the user data section.  Handle destruction in the 
	   control callbacks */

  shell=XtVaCreatePopupShell("shell",xmDialogShellWidgetClass,w,
      		XmNtitle,"xxx",XmNdeleteResponse,XmDO_NOTHING,NULL);
  pane=XtVaCreateWidget("paned_window",xmPanedWindowWidgetClass,shell,
      		XmNsashWidth,1,XmNsashHeight,1,XmNseparatorOn,False,
      		/*XmNuserData,(XtPointer)sas,*/NULL);
  row_column=XtVaCreateWidget("row_column",xmRowColumnWidgetClass,pane,
  	XmNorientation,XmHORIZONTAL,
  	XmNnumColumns,max_rows,XmNpacking,XmPACK_COLUMN,NULL);
   for(j=1;j<=max_rows;j++){
     for(i=1;i<=max_cols;i++){
       pWnd=GetSubPanel_CPGPlotWindow(pMngWnd,i,j);
       data_type=((WindowSetup*)(pWnd->pUserData))->instrument;
       sprintf(chtmp,"%X",data_type);
       str=XmStringCreateLocalized(chtmp);
       push_btn=XtVaCreateManagedWidget("push_button",xmPushButtonWidgetClass,row_column,
   		XmNlabelString,str,XmNuserData,(XtPointer)pWnd,NULL);
       if(data_type != 0)
         XtAddCallback(push_btn,XmNactivateCallback,DisplayConfigure,(XtPointer)0);
       XmStringFree(str);	
     }/* for columns */
   }/* for rows */
  XtManageChild(row_column);
    
  oac[0].data=(XtPointer)shell;
  oac[1].data=(XtPointer)shell;
  oac[2].data=(XtPointer)shell;
  CreateActionArea(pane,oac,3);

  XtManageChild(pane);
  TurnOffSashTraversal(pane);

  WM_DELETE_WINDOW=XmInternAtom(XtDisplay(shell),"WM_DELETE_WINDOW",False);
  XmAddWMProtocolCallback(shell,WM_DELETE_WINDOW,DestroyShell,(XtPointer)shell);

  XtPopup(shell,XtGrabNone);


return;
}


void DisplayConfigure(Widget w,XtPointer client_data,XtPointer call_data)
{
SelectAnalysis_Struct *sas;		/* This goes with the life of the dialog */

int i,num_children;
unsigned long ValidAntenna,ultmp;
Atom WM_DELETE_WINDOW;
Widget pane,frame,*children;
XmString str;
XtPointer xtptmp;

/* ---------- */
unsigned long data_type,operation;
CPGPlotWindow *pWnd;

ToggleBoxItem *pMode=NULL;			

ToggleBoxItem antenna[]={
     {"ExLo",False,(XtPointer)Antenna_ExLo,NULL,0},{"LMR+",False,(XtPointer)Antenna_LMRp,NULL,0},
     {"EzLo",False,(XtPointer)Antenna_EzLo,NULL,0},{"LMR-",False,(XtPointer)Antenna_LMRm,NULL,0},
     {"Bx",  False,(XtPointer)Antenna_Bx,  NULL,0},{"LP",  False,(XtPointer)Antenna_LP,  NULL,0},
     {"By",  False,(XtPointer)Antenna_By,  NULL,0},{"HF",  False,(XtPointer)Antenna_HF,  NULL,0},
     {"Bz",  False,(XtPointer)Antenna_Bz,  NULL,0},
	  {NULL}
};	

ToggleBoxItem lfdr_mode[]={
	{"27Hz",  False,(XtPointer)Instrument_LFDRlowBand, NULL,0},
	{"2.5KHz",False,(XtPointer)Instrument_LFDRhighBand,NULL,0},
	{NULL}
};
ToggleBoxItem lp_mode[]={
	{"Sweep",   False,(XtPointer)Instrument_LPsweep,   NULL,0},   
	{"Density", False,(XtPointer)Instrument_LPdensity, NULL,0},
	{"Analyzed",False,(XtPointer)Instrument_LPanalyzed,NULL,0},
	{"TBD",     False,(XtPointer)Instrument_LPtbd,     NULL,0},
	{NULL}
};
ToggleBoxItem wbr_mode[]={
	{"10KHz",False,(XtPointer)Instrument_WBRlowBand, NULL,0},
	{"80KHz",False,(XtPointer)Instrument_WBRhighBand,NULL,0},
	{NULL}
};	
ToggleBoxItem wfr_mode[]={
	{"27Hz",  False,(XtPointer)Instrument_WFRlowBand, NULL,0},
	{"2.5KHz",False,(XtPointer)Instrument_WFRhighBand,NULL,0},
	{NULL}
};
ToggleBoxItem analysis[]={
     {"Raw",      False,(XtPointer)Analysis_Raw,NULL,0}, 
     {"Magnitude",False,(XtPointer)Analysis_Mag,NULL,0},
     {"Phase",    False,(XtPointer)Analysis_Phs,NULL,0},
     {"Average",  False,(XtPointer)Analysis_Avg,NULL,0},
     {"Spectrogram",False,(XtPointer)0,NULL,0},
	  {NULL}
};

ActionAreaItem oac[]={			/* char *label,void (*callback)(),XtPointer data */	
	{"Ok",    DisplayConfigure_Control,(XtPointer)NULL},
	{"Apply", DisplayConfigure_Control,(XtPointer)NULL},
	{"Cancel",DisplayConfigure_Control,(XtPointer)NULL},
	{NULL}
};
  

  XtVaGetValues(w,XmNuserData,&xtptmp,NULL);
  if(!xtptmp)  return;
  pWnd=(CPGPlotWindow*)xtptmp;
  data_type=((WindowSetup*)(pWnd->pUserData))->instrument;
  operation=((WindowSetup*)(pWnd->pUserData))->analysis;
  
  if(data_type&Instrument_HFR){        ValidAntenna=Antenna_None;        pMode=NULL;}
  else if(data_type&Instrument_LFDR){  ValidAntenna=Valid_LFDR_Antenna;  pMode=lfdr_mode;}
  else if(data_type&Instrument_LP){    ValidAntenna=Valid_LP_Antenna;    pMode=lp_mode;}
  else if(data_type&Instrument_MFR){   ValidAntenna=Valid_MFR_Antenna;   pMode=NULL;}
  else if(data_type&Instrument_WBR){   ValidAntenna=Valid_WBR_Antenna;   pMode=wbr_mode;}
  else if(data_type&Instrument_WFR){   ValidAntenna=Valid_WFR_Antenna;   pMode=wfr_mode;}
  else{                                ValidAntenna=Antenna_None;        pMode=NULL;}
     
    
	/* Create the data structure off the Heap and attach it to a manager
	   widget in the user data section.  Handle destruction in the 
	   control callbacks */
  sas=(SelectAnalysis_Struct*)calloc(1,sizeof(SelectAnalysis_Struct));
  
  sas->shell=XtVaCreatePopupShell("shell",xmDialogShellWidgetClass,w,
      		XmNtitle,"xxx",XmNdeleteResponse,XmDO_NOTHING,NULL);
  pane=XtVaCreateWidget("paned_window",xmPanedWindowWidgetClass,sas->shell,
      		XmNsashWidth,1,XmNsashHeight,1,XmNseparatorOn,False,
      		XmNuserData,(XtPointer)sas,NULL);
  
    frame=XtVaCreateWidget("frame",xmFrameWidgetClass,pane,NULL);
    str=XmStringCreateLocalized("Antennas");
    XtVaCreateManagedWidget("label",xmLabelWidgetClass,frame,XmNlabelString,
           str,XmNchildType,XmFRAME_TITLE_CHILD,XmNchildVerticalAlignment,
           XmALIGNMENT_CENTER,NULL);
    XmStringFree(str);	
    sas->ant=CreateToggleBox(frame,antenna,False,False,5);
    XtManageChild(sas->ant);
    XtManageChild(frame);
  	/* Get antenna selection box's children and decide which antennas should be grayed-out and checked */
  XtVaGetValues(sas->ant,XmNchildren,&children,XmNnumChildren,&num_children,NULL);
  while(num_children-->0){
    XtVaGetValues(children[num_children],XmNuserData,&xtptmp,NULL);
    if(!(ValidAntenna&(unsigned long)xtptmp))  
      XtVaSetValues(children[num_children],XmNsensitive,False,NULL);
    if(data_type&(unsigned long)xtptmp)
      XtVaSetValues(children[num_children],XmNset,True,NULL);
  }

  if(pMode){
    frame=XtVaCreateWidget("frame",xmFrameWidgetClass,pane,NULL);
    str=XmStringCreateLocalized("Mode");
    XtVaCreateManagedWidget("label",xmLabelWidgetClass,frame,XmNlabelString,
           str,XmNchildType,XmFRAME_TITLE_CHILD,XmNchildVerticalAlignment,
           XmALIGNMENT_CENTER,NULL);
    XmStringFree(str);	
    sas->mode=CreateToggleBox(frame,pMode,False,False,2);
    XtManageChild(sas->mode);
    XtManageChild(frame);   
	/* Get mode selection box's children and decide which modes should be checked */
    XtVaGetValues(sas->mode,XmNchildren,&children,XmNnumChildren,&num_children,NULL);
    while(num_children-->0){
      XtVaGetValues(children[num_children],XmNuserData,&xtptmp,NULL);
      if(data_type&(unsigned long)xtptmp)
        XtVaSetValues(children[num_children],XmNset,True,NULL);
    }
  }
  
  frame=XtVaCreateWidget("frame",xmFrameWidgetClass,pane,NULL);
  str=XmStringCreateLocalized("Analysis");
  XtVaCreateManagedWidget("label",xmLabelWidgetClass,frame,XmNlabelString,
  	str,XmNchildType,XmFRAME_TITLE_CHILD,XmNchildVerticalAlignment,
  	 XmALIGNMENT_CENTER,NULL);
  XmStringFree(str);	
  sas->anal=CreateToggleBox(frame,analysis,False,False,2);
  XtManageChild(sas->anal);
  XtManageChild(frame);
	/* Get analysis selection box's children and decide which modes should be checked */
  XtVaGetValues(sas->anal,XmNchildren,&children,XmNnumChildren,&num_children,NULL);
  while(num_children-->0){
    XtVaGetValues(children[num_children],XmNuserData,&xtptmp,NULL);
    if(operation&(unsigned long)xtptmp)
      XtVaSetValues(children[num_children],XmNset,True,NULL);
    if( (unsigned long)xtptmp!=Analysis_Raw && (unsigned long)xtptmp!=Analysis_Mag )  
      XtVaSetValues(children[num_children],XmNsensitive,False,NULL);
  }
    
  oac[0].data=(XtPointer)pWnd;
  oac[1].data=(XtPointer)pWnd;
  oac[2].data=(XtPointer)pWnd;
  CreateActionArea(pane,oac,2);

  XtManageChild(pane);
  TurnOffSashTraversal(pane);

  WM_DELETE_WINDOW=XmInternAtom(XtDisplay(sas->shell),"WM_DELETE_WINDOW",False);
  XmAddWMProtocolCallback(sas->shell,WM_DELETE_WINDOW,DisplayConfigure_Control,(XtPointer)0);

  XtPopup(sas->shell,XtGrabNone);

return;
}

void DisplayConfigure_Control(Widget w,XtPointer client_data,XtPointer call_data)
{
CPGPlotWindow *pWnd=(CPGPlotWindow*)client_data;
WindowSetup *pWndSetup=(WindowSetup*)pWnd->pUserData;
CPGLine *pLine;
int num_children;
Boolean set;
Widget shell,*children;
XtPointer user_data;
SelectAnalysis_Struct *pSAS;


  /* Look in the shells child for the structure associated with this particular
     instance of the dialog box. */
  shell=w;
  while(shell && ! XtIsWMShell(shell))  shell=XtParent(shell);
  if(!shell){				/* Big Disaster */
    fprintf(stderr,"SelectAnalysis_Control(), can't find the shell");
    return;	
  }
  XtVaGetValues(shell,XmNchildren,&children,XmNnumChildren,&num_children,NULL);  
  if(num_children!=1){  	/* Sanity Check, the shell better only have one manager child! */
    fprintf(stderr,"SelectAnalysis_Control(),can't find the shell's child.(%d)\n",num_children);
    return;
  }
  XtVaGetValues(*children,XmNuserData,&user_data,NULL);	/* really XmPannedWindowWidgetClass */
  pSAS=(SelectAnalysis_Struct*)user_data;	
  if(!pSAS->ant || !pSAS->anal){
    fprintf(stderr,"SelectAnalysis_Control(), null widget children\n");
    return;
    }
  
	/* On to square one, to really do something. Query all the check buttons of 
		all the check boxes and reflect the set state in the structure */
  if(!strcmp("Ok",XtName(w)) || !strcmp("Apply",XtName(w))){
  	/* Antenna selection check box */
    XtVaGetValues(pSAS->ant,XmNchildren,&children,XmNnumChildren,&num_children,NULL);
    while(num_children-->0){
      XtVaGetValues(children[num_children],XmNset,&set,XmNuserData,&user_data,NULL);
      if(set)	pWndSetup->instrument|= (unsigned long)user_data;
      else	pWndSetup->instrument&=~(unsigned long)user_data;
    }
    	/* Mode selection check box; may or may not have one */
    if(pSAS->mode){
      XtVaGetValues(pSAS->mode,XmNchildren,&children,XmNnumChildren,&num_children,NULL);
      while(num_children-->0){
        XtVaGetValues(children[num_children],XmNset,&set,XmNuserData,&user_data,NULL);
        if(set) pWndSetup->instrument|= (unsigned long)user_data;
        else	pWndSetup->instrument&=~(unsigned long)user_data;
        }
    }
    	/* Data Analysis check box */
    XtVaGetValues(pSAS->anal,XmNchildren,&children,XmNnumChildren,&num_children,NULL);
    while(num_children-->0){
      XtVaGetValues(children[num_children],XmNset,&set,XmNuserData,&user_data,NULL);
      if(set)	pWndSetup->analysis|= (unsigned long)user_data;
      else	pWndSetup->analysis&=~(unsigned long)user_data;
      }
  }

/* Trap out the unselected instrument modes. */
  if(!(pWndSetup->instrument&Instrument_Mask)){
    PostErrorDialog(w,"No Mode is Selected!");
    return;
    }
  
  /* If "Ok", "Cancel", or the Window Manager's 'Close', free up the memory associated
     with the dialog, otherwise there will be a memory leak! */
  if(strcmp("Apply",XtName(w))){	
    XtDestroyWidget(pSAS->shell);		/* free widgets */
    free(pSAS);				/* free memeory allocated with them */
  }
  
  
/* Get the source of all the lines and free the user data associated with each.
   Rebuild the list according to the new 'specs' and assign it to the window.
*/
  pLine=pWndSetup->pPlotSetup;
  DestroyLineObject(pLine);	
  pLine=BuildLineObject(pWndSetup->instrument,pWndSetup->analysis);
  pWndSetup->pPlotSetup=pLine;
  
/* Get the working copy of the source and free only the 'shell', not the user data, since
   it is a pointer to the source's user data, which is already free.  Make sure to NULL it out.
*/
  pLine=HeadListElement(pWnd->pLine);
  while(pLine)
    pLine=DeleteListElement(pLine);	
  pWnd->pLine=NULL;	/* Null the working copy pointer, since nothing exists here.
  			   It will be dynamically copied from the source later */
    

/* 
if(pWndSetup->pPlotSetup)  DumpCPGLineList(pWndSetup->pPlotSetup);
else	   fprintf(stderr,"No Analysis %08X %08X",pWndSetup->instrument,pWndSetup->analysis);
fprintf(stderr,"\n");
*/

return;
}

void TstCstDlg(Widget w,XtPointer client_data,XtPointer call_data)
{
XmAnyCallbackStruct *cbs=(XmAnyCallbackStruct*)call_data;
Widget parent,shell,pane,rowcolumn,button,wtmp;
Widget defpb;

Pixel fg,bg;
char *RecorderBitMaps[]={
  INST_SHARE "/ana/Rewind.bm",
  INST_SHARE "/ana/StepRewind.bm",
  INST_SHARE "/ana/PlayRewind.bm",
  INST_SHARE "/ana/Stop.bm",
  INST_SHARE "/ana/PlayForward.bm",
  INST_SHARE "/ana/StepForward.bm",
  INST_SHARE "/ana/FastForward.bm"
};


int i,nBtns=XtNumber(RecorderBitMaps);

  /* Does a window manager have to be parent of the dialog box? */
  wtmp=w;  
  while(wtmp && !XtIsWMShell(wtmp))  
    wtmp=XtParent(wtmp);

fprintf(stderr,"TstCstDlg(%p,%p,%p)\n",w,client_data,call_data);
 
  /* Dialogs must be a decendant of a realized widgets, not a gadget; 
     the parent must have a window */ 
  while(w && !XtIsWMShell(w))  w=XtParent(w);
   parent=w;

  shell=XtVaCreatePopupShell("custom dialog",xmDialogShellWidgetClass,parent,
        XmNdeleteResponse,XmDESTROY,NULL);
  pane=XtVaCreateWidget("pane",xmPanedWindowWidgetClass,shell,NULL); 
  rowcolumn=XtVaCreateWidget("pane",xmRowColumnWidgetClass,pane,
         XmNorientation,XmHORIZONTAL,NULL);


  XtVaGetValues(rowcolumn,XmNforeground,&fg,XmNbackground,&bg,NULL);

  for(i=0;i<nBtns;i++){  
  Pixmap pixmap;
    pixmap=XmGetPixmap(XtScreen(rowcolumn),RecorderBitMaps[i],bg,fg);
    if(pixmap==XmUNSPECIFIED_PIXMAP)
      fprintf(stderr,"Unable to load pixmap %s \n",RecorderBitMaps[i]);
    else if(i==1 || i==3 || i==5)
      XtVaCreateManagedWidget("pushbutton",xmPushButtonWidgetClass,
           rowcolumn,XmNlabelType,XmPIXMAP,XmNlabelPixmap,pixmap,NULL);
    else if(i==4)
      defpb=XtVaCreateManagedWidget("drawnbutton",xmDrawnButtonWidgetClass,
           rowcolumn,XmNlabelType,XmPIXMAP,XmNlabelPixmap,pixmap,
           XmNpushButtonEnabled,False,XmNshadowType,XmSHADOW_IN,NULL);
    else
      XtVaCreateManagedWidget("drawnbutton",xmDrawnButtonWidgetClass,
           rowcolumn,XmNlabelType,XmPIXMAP,XmNlabelPixmap,pixmap,
           XmNpushButtonEnabled,False,XmNshadowType,XmSHADOW_OUT,NULL);
    
  }

/*
  XtCallActionProc(defpb,"ArmAndActivate",cbs->event,NULL,0);
*/

/*
  XtManageChild(rowcolumn);
  for(i=0;i<6;i++)
    button=XtVaCreateManagedWidget("push_button",xmPushButtonWidgetClass,
         pane,NULL);
*/

  XtManageChild(rowcolumn);
  XtManageChild(pane);
  XtPopup(shell,XtGrabNone);

return;
}

void pucdb(Widget w,XtPointer client_data,XtPointer call_data)
{
Pixel fg,bg;
Widget shell,pane,rowcolumn,pushbutton,drawnbutton,defpb,wtmp;
XmString string;
XmAnyCallbackStruct *cbs=(XmAnyCallbackStruct*)call_data;

char *RecorderBitMaps[]={
  INST_SHARE "/ana/Rewind.bm",
  INST_SHARE "/ana/StepRewind.bm",
  INST_SHARE "/ana/PlayRewind.bm",
  INST_SHARE "/ana/Stop.bm",
  INST_SHARE "/ana/PlayForward.bm",
  INST_SHARE "/ana/StepForward.bm",
  INST_SHARE "/ana/FastForward.bm"
};

int i,nBtns=XtNumber(RecorderBitMaps);

  /* Does a window manager have to be parent of the dialog box? */
  wtmp=w;  
  while(wtmp && !XtIsWMShell(wtmp))  
    wtmp=XtParent(wtmp);

  shell=XtVaCreatePopupShell("dialog",topLevelShellWidgetClass,wtmp,
                                XmNdeleteResponse,XmDO_NOTHING,NULL);
  pane=XtVaCreateWidget("pane",xmPanedWindowWidgetClass,shell,
       XmNsashWidth,1,XmNsashHeight,1,XmNseparatorOn,False,NULL);

  rowcolumn=XtVaCreateWidget("rowcolumn",xmRowColumnWidgetClass,pane,
          XmNorientation,XmHORIZONTAL,XmNpacking,XmPACK_COLUMN,NULL);
 
  XtVaGetValues(rowcolumn,XmNforeground,&fg,XmNbackground,&bg,NULL);

  for(i=0;i<nBtns;i++){  
  Pixmap pixmap;
    pixmap=XmGetPixmap(XtScreen(rowcolumn),RecorderBitMaps[i],bg,fg);
    if(pixmap==XmUNSPECIFIED_PIXMAP)
      fprintf(stderr,"Unable to load pixmap %s \n",RecorderBitMaps[i]);
    else if(i==1 || i==3 || i==5)
      XtVaCreateManagedWidget("pushbutton",xmPushButtonWidgetClass,
           rowcolumn,XmNlabelType,XmPIXMAP,XmNlabelPixmap,pixmap,NULL);
    else if(i==4)
      defpb=XtVaCreateManagedWidget("drawnbutton",xmDrawnButtonWidgetClass,
           rowcolumn,XmNlabelType,XmPIXMAP,XmNlabelPixmap,pixmap,
           XmNpushButtonEnabled,False,XmNshadowType,XmSHADOW_IN,NULL);
    else
      XtVaCreateManagedWidget("drawnbutton",xmDrawnButtonWidgetClass,
           rowcolumn,XmNlabelType,XmPIXMAP,XmNlabelPixmap,pixmap,
           XmNpushButtonEnabled,False,XmNshadowType,XmSHADOW_OUT,NULL);
    
  }
  XtCallActionProc(defpb,"ArmAndActivate",cbs->event,NULL,0);

  XtManageChild(rowcolumn);
  XtManageChild(pane);
  XtPopup(shell,XtGrabNone);  
  
return;
}

