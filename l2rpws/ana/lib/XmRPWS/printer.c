#include "XmRPWS.h"

static char *DefaultPrinters[]={"lp","rpwshp_laser","rpwshp_ps","rpwshp2_laser","rpwshp2_ps"};
static char LnPrn[128]="lp -c -onb ",PsPrn[128]="lp -c -onb";
static char *Prn[]={LnPrn,PsPrn,NULL};

static struct{
  Widget tf,sl,tba,tbb,la,lb;
  }pd;

static CallBackFunction cbf;

void PrinterControl(Widget w,XtPointer client_data,XtPointer call_data);
void PrinterAction(Widget w,XtPointer client_data,XtPointer call_data);

void PrinterDialogBox(Widget w,XtPointer client_data,XtPointer call_data)
{
int i,prnum=XtNumber(DefaultPrinters);
XmAnyCallbackStruct *cbs=(XmAnyCallbackStruct*)call_data;
Atom WM_DELETE_WINDOW;
XtWidgetGeometry size;
XmString label_str;
XmStringTable str_lst;
Widget pane,form,frame,lb,rc,pb,wtmp;
static Widget dialog,caller;

  if(!dialog){
    cbf=(CallBackFunction)client_data;
    caller=w;
    wtmp=w;
    while(wtmp && !XtIsWMShell(wtmp))  wtmp=XtParent(wtmp);
    dialog=XtVaCreateWidget("Printer",xmDialogShellWidgetClass,wtmp,XmNdeleteResponse,XmDESTROY,NULL);
    pane=XtVaCreateWidget("Printer",xmPanedWindowWidgetClass,dialog,XmNsashWidth,1,XmNsashHeight,1,XmNseparatorOn,False,NULL);
    /* Control Area */
    label_str=XmStringCreateLocalized("Printer Manager");
    wtmp=XtVaCreateManagedWidget("label",xmLabelWidgetClass,pane,XmNlabelString,label_str,NULL);
    XmStringFree(label_str);
    size.request_mode=CWHeight;
    XtQueryGeometry(wtmp,NULL,&size);
    XtVaSetValues(wtmp,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL); 
    
    form=XtVaCreateWidget("form",xmFormWidgetClass,pane,XmNfractionBase,10,NULL);
     rc=XtVaCreateWidget("rowcolumn",xmRowColumnWidgetClass,form,
     		XmNtopAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,
     		XmNleftAttachment,XmATTACH_POSITION,XmNleftPosition,0,
     		XmNrightAttachment,XmATTACH_POSITION,XmNrightPosition,5,NULL);
     pd.tba=XtVaCreateManagedWidget("Change Text Printer",xmToggleButtonWidgetClass,rc,XmNset,True,NULL);
     label_str=XmStringCreateLocalized(DefaultPrinters[0]);
     pd.la=XtVaCreateManagedWidget("Line Printer",xmLabelWidgetClass,rc,XmNlabelString,label_str,NULL);
     XmStringFree(label_str);
     pd.tbb=XtVaCreateManagedWidget("Change PostScript Printer",xmToggleButtonWidgetClass,rc,XmNset,True,NULL);
     label_str=XmStringCreateLocalized(DefaultPrinters[0]);
     pd.lb=XtVaCreateManagedWidget("PostScript Printer",xmLabelWidgetClass,rc,XmNlabelString,label_str,NULL);
     XmStringFree(label_str);
     XtManageChild(rc);
     str_lst=(XmStringTable)XtMalloc(prnum*sizeof(XmString));
     for(i=0;i<prnum;i++)
       str_lst[i]=XmStringCreateLocalized(DefaultPrinters[i]);
     pd.sl=(Widget)XmCreateScrolledList(form,"PrinterList",NULL,0);
     XtVaSetValues(pd.sl,XmNitems,str_lst,XmNitemCount,prnum,XmNvisibleItemCount,3,NULL);
     XtVaSetValues(XtParent(pd.sl),XmNtopAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,
	XmNleftAttachment,XmATTACH_POSITION,XmNleftPosition,5,XmNrightAttachment,XmATTACH_POSITION,XmNrightPosition,10,NULL);
     XtManageChild(pd.sl);
     for(i=0;i<prnum;i++)
       XmStringFree(str_lst[i]);
     XtFree((char*)str_lst);
    XtManageChild(form);
    form=XtVaCreateWidget("form",xmFormWidgetClass,pane,NULL);
     label_str=XmStringCreateLocalized("Printer Name : ");
     lb=XtVaCreateManagedWidget("Label",xmLabelWidgetClass,form,XmNlabelString,label_str,
     		XmNtopAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,
     		XmNleftAttachment,XmATTACH_FORM,NULL);
     XmStringFree(label_str);
     pd.tf=XtVaCreateManagedWidget("TextField",xmTextFieldWidgetClass,form,
     		XmNleftAttachment,XmATTACH_WIDGET,XmNleftWidget,lb,
     		XmNrightAttachment,XmATTACH_FORM,NULL);
    XtManageChild(form);
    size.request_mode=CWHeight;
    XtQueryGeometry(form,NULL,&size);
    XtVaSetValues(form,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL); 
    XtAddCallback(pd.sl,XmNbrowseSelectionCallback,PrinterControl,(XtPointer)pd.tf);
    /* Action Area */
    wtmp=XtVaCreateManagedWidget("Separator",xmSeparatorWidgetClass,pane,NULL);
    size.request_mode=CWHeight;
    XtQueryGeometry(wtmp,NULL,&size);
    XtVaSetValues(wtmp,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL); 
    form=XtVaCreateWidget("form",xmFormWidgetClass,pane,XmNfractionBase,4,NULL);
     pb=XtVaCreateManagedWidget("Apply",xmPushButtonWidgetClass,form,
     	XmNleftAttachment,XmATTACH_POSITION,XmNleftPosition,0,
     	XmNrightAttachment,XmATTACH_POSITION,XmNrightPosition,1,NULL);
     XtAddCallback(pb,XmNactivateCallback,PrinterAction,(XtPointer)&pd);
     pb=XtVaCreateManagedWidget("Add Prn",xmPushButtonWidgetClass,form,
     	XmNleftAttachment,XmATTACH_POSITION,XmNleftPosition,1,
     	XmNrightAttachment,XmATTACH_POSITION,XmNrightPosition,2,NULL);
     XtAddCallback(pb,XmNactivateCallback,PrinterAction,(XtPointer)&pd);
     pb=XtVaCreateManagedWidget("Del Prn",xmPushButtonWidgetClass,form,
     	XmNleftAttachment,XmATTACH_POSITION,XmNleftPosition,2,
     	XmNrightAttachment,XmATTACH_POSITION,XmNrightPosition,3,NULL);
     XtAddCallback(pb,XmNactivateCallback,PrinterAction,(XtPointer)&pd);
     pb=XtVaCreateManagedWidget("Quit",xmPushButtonWidgetClass,form,
     	XmNleftAttachment,XmATTACH_POSITION,XmNleftPosition,3,
     	XmNrightAttachment,XmATTACH_POSITION,XmNrightPosition,4,NULL);
     XtAddCallback(pb,XmNactivateCallback,PrinterDialogBox,NULL);
    XtManageChild(form);
    size.request_mode=CWHeight;
    XtQueryGeometry(form,NULL,&size);
    XtVaSetValues(form,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL); 
    
    XtManageChild(pane);
    WM_DELETE_WINDOW=XmInternAtom(XtDisplay(dialog),"WM_DELETE_WINDOW",False);
    XmAddWMProtocolCallback(dialog,WM_DELETE_WINDOW,PrinterDialogBox,NULL);
    client_data=(XtPointer)Prn;
    }
    
  /* Assign the Array of Printers */

  switch(cbs->reason){
    case XmCR_ACTIVATE : 
      if(caller==w){
        XtSetSensitive(caller,False);
        XtPopup(dialog,XtGrabNone);
        }
      else{
        XtSetSensitive(caller,True);
        XtPopdown(dialog);
        }
      break;
    case XmCR_WM_PROTOCOLS :
      dialog=NULL;
      XtSetSensitive(caller,True);
      break;
    default :
      break;
    }  
  
return;
}



void PrinterControl(Widget w,XtPointer client_data,XtPointer call_data)
{
char *pch;
XmListCallbackStruct *cbs=(XmListCallbackStruct*)call_data;
XmString str;
Widget wcd=(Widget)client_data;

  switch(cbs->reason){
    case XmCR_BROWSE_SELECT :
      XmStringGetLtoR(cbs->item,XmFONTLIST_DEFAULT_TAG,&pch);
      XmTextFieldSetString(wcd,pch);
      XtFree(pch);
      break;
    default :
      break;
    }

return;
}



void PrinterAction(Widget w,XtPointer client_data,XtPointer call_data)
{
char *pch,*pstart,*pend;
XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct*)call_data;
XmString str;

  switch(cbs->reason){
      case XmCR_ACTIVATE :
      pch=XmTextFieldGetString(pd.tf);
      pstart=pch;
      while(*pstart==' ')  ++pstart;
      pend=pstart;
      while(*pend!=' ' && *pend!='\0')  ++pend;
      *pend='\0';
      str=XmStringCreateLocalized(pstart);
      if(!strcmp("Add Prn",XtName(w)) && strlen(pstart)){
        XmListAddItem(pd.sl,str,0);
        }
      else if(!strcmp("Del Prn",XtName(w))){
        if(XmListItemExists(pd.sl,str))
          XmListDeleteItem(pd.sl,str);
        }
      else if(!strcmp("Apply",XtName(w)) && strlen(pstart)){
        if(XmToggleButtonGetState(pd.tba)){
          XtVaSetValues(pd.la,XmNlabelString,str,NULL);
          if(strcmp("lp",pch))  sprintf(LnPrn,"lp -c -onb -d %s",pch);
          else sprintf(LnPrn,"lp -c -onb ");
          }
        if(XmToggleButtonGetState(pd.tbb)){
          XtVaSetValues(pd.lb,XmNlabelString,str,NULL);
          if(strcmp("lp",pch))  sprintf(PsPrn,"lp -c -onb -d %s",pch);
          else sprintf(PsPrn,"lp -c -onb ");
          }
        cbf(w,NULL,call_data);
        }
      XmStringFree(str);
      XtFree(pch);
      break;
    default :
      break;
    }

return;
}



char * PrinterDialogBoxGetLinePrinter(void)
{return LnPrn;}

char * PrinterDialogBoxGetPostScriptPrinter(void)
{return PsPrn;}
