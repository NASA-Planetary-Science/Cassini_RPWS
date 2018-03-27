#include "XmRPWS.h"

/* this needs to be changed to allow multiple instances included */
typedef struct{
  long current_direction;
  Widget shell,pane,last_button;
  CallBackFunction cbf;
  }Recorder;




/* This widget should be independant of its parent */
void PostRecorderDialog(Widget w,XtPointer client_data,XtPointer call_data)
{
int NumberOfButtons=XtNumber(RecorderBitMaps);
int i,mwmfunc;
Recorder *rcd=0;
Atom WM_DELETE_WINDOW; Pixmap image;
Pixel fg,bg;
Widget rowcolumn,pushbutton,drawnbutton,default_pb,wtmp;
XmString string;
XmAnyCallbackStruct *cbs=(XmAnyCallbackStruct*)call_data;
XtWidgetGeometry size,label_size;
int nTmp=0;
void RecorderDialogCallback(Widget w,XtPointer client_data,XtPointer call_data);

  XtVaGetValues(w,XmNuserData,&rcd,NULL);
  if(!rcd){
    if((rcd=(Recorder*)malloc(sizeof(Recorder)))==NULL){
      fprintf(stderr,"Unable to malloc(sizeof(Recorder))\n");
      return;
    }
    XtVaSetValues(w,XmNuserData,(XtPointer)rcd,NULL);
    rcd->last_button=NULL;

    /* Does a window manager have to be parent of the dialog box? */
    wtmp=w;  
    while(wtmp && !XtIsWMShell(wtmp))  
      wtmp=XtParent(wtmp);
/*
    rcd->shell=XtVaCreatePopupShell("dialog",topLevelShellWidgetClass,wtmp,
          XmNdeleteResponse,XmDO_NOTHING,NULL);
*/
    rcd->shell=XtVaCreatePopupShell("dialog",xmDialogShellWidgetClass,wtmp,
          XmNdeleteResponse,XmDO_NOTHING,NULL);

    rcd->pane=XtVaCreateWidget("pane",xmPanedWindowWidgetClass,rcd->shell,
          XmNsashWidth,1,XmNsashHeight,1,XmNseparatorOn,False,NULL);
    rowcolumn=XtVaCreateWidget("rowcolumn",xmRowColumnWidgetClass,rcd->pane,
          XmNorientation,XmHORIZONTAL,XmNpacking,XmPACK_COLUMN,NULL);
/*
    rowcolumn=XtVaCreateWidget("rowcolumn",xmRowColumnWidgetClass,rcd->pane,
          XmNorientation,XmHORIZONTAL,NULL);
*/

    XtVaGetValues(rowcolumn,XmNforeground,&fg,XmNbackground,&bg,NULL);

    for(i=0;i<NumberOfButtons;i++){
      image=XmGetPixmap(XtScreen(rowcolumn),RecorderBitMaps[i],bg,fg);
      if(image==XmUNSPECIFIED_PIXMAP){
        fprintf(stderr,"Unable to load %s pixmap\n",RecorderBitMaps[i]);
        continue;
      }

      if( i==1 || i==3 || i==5 ){     /* StepRewind,Stop,StepForward */
        pushbutton=XtVaCreateManagedWidget("pushbutton",xmPushButtonWidgetClass,
             rowcolumn,XmNlabelType,XmPIXMAP,XmNlabelPixmap,image,
             XmNuserData,(XtPointer)i,NULL);
        XtAddCallback(pushbutton,XmNactivateCallback,RecorderDialogCallback,
             (XtPointer)rcd);
      }
      else{
        drawnbutton=XtVaCreateManagedWidget("drawnbutton",
          xmDrawnButtonWidgetClass,rowcolumn,XmNlabelType,XmPIXMAP,
          XmNlabelPixmap,image,XmNpushButtonEnabled,False,
    	  XmNshadowType,XmSHADOW_OUT,XmNuserData,(XtPointer)i,NULL);
    	if(i==4)  default_pb=drawnbutton;	/* Default Button Play i=4 */
        XtAddCallback(drawnbutton,XmNactivateCallback,RecorderDialogCallback,
             (XtPointer)rcd);
        /* 
        This might be necessary later on if problems occure.
        XtAddCallback(drawnbutton,XmNexposeCallback,DE,(XtPointer)rowcolumn);
        */
        }
      }
    XtManageChild(rowcolumn);

    size.request_mode=CWHeight;
    XtQueryGeometry(rowcolumn,NULL,&size);
    XtVaSetValues(rowcolumn,XmNpaneMaximum,size.height,
         XmNpaneMinimum,size.height,NULL); 
    TurnOffSashTraversal(rcd->pane);
    XtManageChild(rcd->pane);

    WM_DELETE_WINDOW=XmInternAtom(XtDisplay(rcd->shell),"WM_DELETE_WINDOW",
        False);
    XmAddWMProtocolCallback(rcd->shell,WM_DELETE_WINDOW,PopdownShell,
        (XtPointer)rcd->shell);
    mwmfunc=MWM_FUNC_ALL|MWM_FUNC_RESIZE|MWM_FUNC_MAXIMIZE;
    XtVaSetValues(rcd->shell,XmNmwmFunctions,mwmfunc,NULL);
    XtSetSensitive(w,False);
    XtAddCallback(rcd->shell,XmNpopdownCallback,ToggleSensitivity,(XtPointer)w);
    XtAddCallback(rcd->shell,XmNpopupCallback,ToggleSensitivity,(XtPointer)w);
    rcd->cbf=(CallBackFunction)client_data;
    /* Default Button should match initial definition above */
    XtCallActionProc(default_pb,"ArmAndActivate",cbs->event,NULL,0);
    }

  XtPopup(rcd->shell,XtGrabNone);  
  XtSetSensitive(w,False); 

return;
}

void RecorderDialogCallback(Widget w,XtPointer client_data,XtPointer call_data) 
{ 
int btn_num;
Recorder *r=(Recorder*)client_data;


  XtVaGetValues(w,XmNuserData,&btn_num,NULL);

  /* Define Button Press Actions */
  if(btn_num==1 || btn_num==3 || btn_num==5){  /* StepRewind,Stop,StepForward */ 
    if(r->last_button){
      XtVaSetValues(r->last_button,XmNshadowType,XmSHADOW_OUT,NULL);
      r->last_button=0;
      }
    }
  else if(r->last_button!=w){
    XtVaSetValues(w,XmNshadowType,XmSHADOW_IN,NULL);
    if(r->last_button){		
      XtVaSetValues(r->last_button,XmNshadowType,XmSHADOW_OUT,NULL);
      }
    r->last_button=w;
    }
    
  switch(btn_num){
    case 0 :	r->current_direction=RecorderDirectionRewind;		break;
    case 1 :	r->current_direction=RecorderDirectionStepRewind;	break;
    case 2 :	r->current_direction=RecorderDirectionPlayRewind;	break;
    case 3 :	r->current_direction=RecorderDirectionStop;		break;
    case 4 :	r->current_direction=RecorderDirectionPlayForward;	break;
    case 5 :	r->current_direction=RecorderDirectionStepForward;	break;
    case 6 :	r->current_direction=RecorderDirectionFastForward;	break;
    default :	r->current_direction=RecorderDirectionPlayForward;	break;
    }
  if(r->cbf)
    r->cbf(w,client_data,call_data);

return;
}

void DE(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget rc=(Widget)client_data;
Dimension ht,st;
static Pixmap image;
static Pixel fg,bg;

  XtVaGetValues(rc,XmNforeground,&fg,XmNbackground,&bg,NULL);
  image=XmGetPixmap(XtScreen(rc),"Rec.bm",fg,bg);

  XtVaGetValues(w,XmNhighlightThickness,&ht,XmNshadowThickness,&st,NULL);
  XtVaSetValues(w,XmNwidth,2*ht+2*st+64,XmNheight,2*ht+2*st+64,NULL);
  XCopyArea(XtDisplay(w),image,XtWindow(w),XDefaultGCOfScreen(XtScreen(w)),0,0,64,64,ht+st,ht+st);


return;
}


long GetCurrentRecorderDirection(XtPointer client_data)
{
Recorder *r=(Recorder*)client_data;

return r->current_direction;
}

void SetCurrentRecorderDirection(XtPointer client_data,long direction)
{
Recorder *r=(Recorder*)client_data;

  r->current_direction=direction;
  
return;
}
