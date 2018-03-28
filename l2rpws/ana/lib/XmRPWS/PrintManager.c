#include "XmRPWS.h"


/* Private access, NOT Public */
typedef struct{
  char *printer_command,*filename;		
  CallBackFunction cbf;
  Widget shell,pane;
  Widget radio_box,prncmd_tf,filename_tf;
  }PrintManager;



/* Create a unique instance of the printer dialog box, which will be managed
  by the calling widget.  Should the parent of the dialog box be a window
  manager? 
*/
void PostPrintManagerDialog(Widget w,XtPointer client_data,XtPointer call_data)
{
char chtmp[32];
PrintManager *prn_mng;
Atom WM_DELETE_WINDOW;
Widget label,form,pushbutton,rowcolumn,separator;
XmAnyCallbackStruct *cbs=(XmAnyCallbackStruct*)call_data;
XmString string,str0,str1;
XtWidgetGeometry size,label_size;

void PrintManagerDialog_Print(Widget w,XtPointer client_data,XtPointer call_data);
void PrintManagerDialog_Cancel(Widget w,XtPointer client_data,XtPointer call_data);



  XtVaGetValues(w,XmNuserData,&prn_mng,NULL);
  if(!prn_mng){
      prn_mng=(PrintManager*)XtMalloc(sizeof(PrintManager));
      XtVaSetValues(w,XmNuserData,(XtPointer)prn_mng,NULL);
      strcpy(chtmp,"lp -c -onb");
      prn_mng->printer_command=(char*)XtMalloc(sizeof(char)*(strlen(chtmp)+1));
      strcpy(prn_mng->printer_command,chtmp);
      strcpy(chtmp,"outputfile.ps");
      prn_mng->filename=(char*)XtMalloc(sizeof(char)*(strlen(chtmp)+1));
      strcpy(prn_mng->filename,chtmp);
      /* Is a pushbutton in a menu a menu item?  Can it be a parent of the shell? */
      	/* ??? XtVaCreatePopupShell( ) ??? */
      prn_mng->shell=XtVaCreateWidget("PrintManager",xmDialogShellWidgetClass,w,
      		XmNtitle,"Printer Manager",XmNdeleteResponse,XmDO_NOTHING,NULL);
      prn_mng->pane=XtVaCreateWidget("PrintManager",xmPanedWindowWidgetClass,prn_mng->shell,
      		XmNsashWidth,1,XmNsashHeight,1,XmNseparatorOn,False,NULL);
      /* Control Area */
string=XmStringCreateLocalized("Print Command:   ");
label=XtVaCreateWidget("test",xmLabelWidgetClass,prn_mng->pane,XmNlabelString,string,NULL);
XmStringFree(string);
size.request_mode=CWWidth;
XtQueryGeometry(label,NULL,&label_size);
XtDestroyWidget(label);

       form=XtVaCreateWidget("form",xmFormWidgetClass,prn_mng->pane,
      		XmNfractionBase,3,NULL);
        string=XmStringCreateLocalized("Print To:   ");
        label=XtVaCreateManagedWidget("label",xmLabelWidgetClass,form,XmNlabelString,string,
       		XmNleftAttachment,XmATTACH_FORM,XmNalignment,XmALIGNMENT_END,XmNwidth,label_size.width,NULL);
        XmStringFree(string);
        str0=XmStringCreateLocalized("Printer");
        str1=XmStringCreateLocalized("File");
        prn_mng->radio_box=XmVaCreateSimpleRadioBox(form,"radio_box",0,NULL,
       		XmVaRADIOBUTTON,str0,NULL,NULL,NULL,XmVaRADIOBUTTON,str1,NULL,NULL,NULL,NULL);
        XtVaSetValues(prn_mng->radio_box,XmNorientation,XmHORIZONTAL,XmNleftAttachment,XmATTACH_WIDGET,
       		XmNleftWidget,label,XmNrightAttachment,XmATTACH_FORM,NULL);     	
        XtManageChild(prn_mng->radio_box);
        XmStringFree(str0);  XmStringFree(str1);
       XtManageChild(form);
       size.request_mode=CWHeight;
       XtQueryGeometry(form,NULL,&size);
       XtVaSetValues(form,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL); 
       form=XtVaCreateWidget("form",xmFormWidgetClass,prn_mng->pane,
      		XmNfractionBase,3,NULL);

        string=XmStringCreateLocalized("Print Command:   ");
        label=XtVaCreateManagedWidget("label",xmLabelWidgetClass,form,XmNlabelString,string,
       		XmNleftAttachment,XmATTACH_FORM,NULL);
        XmStringFree(string);
        prn_mng->prncmd_tf=XtVaCreateManagedWidget("text",xmTextWidgetClass,form,XmNleftAttachment,
        	XmATTACH_WIDGET,XmNleftWidget,label,XmNrightAttachment,XmATTACH_FORM,
        	XmNvalue,prn_mng->printer_command,NULL);
       XtManageChild(form);
       size.request_mode=CWHeight;
       XtQueryGeometry(form,NULL,&size);
       XtVaSetValues(form,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL); 
       form=XtVaCreateWidget("form",xmFormWidgetClass,prn_mng->pane,
      		XmNfractionBase,3,NULL);
        string=XmStringCreateLocalized("File Name:   ");
        label=XtVaCreateManagedWidget("label",xmLabelWidgetClass,form,XmNlabelString,string,
       		XmNleftAttachment,XmATTACH_FORM,XmNalignment,XmALIGNMENT_END,XmNwidth,label_size.width,NULL);
        XmStringFree(string);
        prn_mng->filename_tf=XtVaCreateManagedWidget("text",xmTextWidgetClass,form,XmNleftAttachment,
        	XmATTACH_WIDGET,XmNleftWidget,label,XmNrightAttachment,XmATTACH_FORM,
        	XmNvalue,prn_mng->filename,NULL);
       XtManageChild(form);
       /* Action Area */
       size.request_mode=CWHeight;
       XtQueryGeometry(form,NULL,&size);
       XtVaSetValues(form,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL); 
       separator=XtVaCreateManagedWidget("separator",xmSeparatorWidgetClass,prn_mng->pane,NULL);
       size.request_mode=CWHeight;
       XtQueryGeometry(separator,NULL,&size);
       XtVaSetValues(separator,/*XmNpaneMaximum,size.height,*/XmNpaneMinimum,size.height,NULL); 
       form=XtVaCreateWidget("form",xmFormWidgetClass,prn_mng->pane,NULL);
        string=XmStringCreateLocalized("Cancel");
        pushbutton=XtVaCreateManagedWidget("pushbutton",xmPushButtonWidgetClass,form,XmNlabelString,string,
        	XmNrightAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,NULL);
        XmStringFree(string);
        XtAddCallback(pushbutton,XmNactivateCallback,PrintManagerDialog_Cancel,(XtPointer)prn_mng);
        size.request_mode=CWWidth;
        XtQueryGeometry(pushbutton,NULL,&size);
        string=XmStringCreateLocalized("Print");
        pushbutton=XtVaCreateManagedWidget("pushbutton",xmPushButtonWidgetClass,form,XmNlabelString,string,
        	XmNleftAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,NULL);
        XmStringFree(string);
        XtVaSetValues(pushbutton,XmNwidth,size.width,NULL);	/* Make Print button the same size of Cancel button */
        XtAddCallback(pushbutton,XmNactivateCallback,PrintManagerDialog_Print,(XtPointer)prn_mng);
       XtManageChild(form);
       size.request_mode=CWHeight;
       XtQueryGeometry(form,NULL,&size);
       XtVaSetValues(form,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL); 

    XtManageChild(prn_mng->pane);

    WM_DELETE_WINDOW=XmInternAtom(XtDisplay(prn_mng->shell),"WM_DELETE_WINDOW",False);
    XmAddWMProtocolCallback(prn_mng->shell,WM_DELETE_WINDOW,PrintManagerDialog_Cancel,(XtPointer)prn_mng);
    XtAddCallback(prn_mng->shell,XmNpopdownCallback,ToggleSensitivity,(XtPointer)w);
    XtAddCallback(prn_mng->shell,XmNpopupCallback,ToggleSensitivity,(XtPointer)w);
    prn_mng->cbf=(CallBackFunction)client_data;
    XtSetSensitive(w,False);
    }

XtPopup(prn_mng->shell,XtGrabNone);
  
return;
}

void PrintManagerDialog_Print(Widget w,XtPointer client_data,XtPointer call_data)
{
PrintManager *pm=(PrintManager*)client_data;
Boolean set;
int wcnt;
size_t uBtn = 0;  /* XmNuserData require a sizeof(void*) memory location */
Widget *pw;

  XtFree(pm->printer_command);
  XtFree(pm->filename);
  pm->printer_command=XmTextGetString(pm->prncmd_tf);
  pm->filename=XmTextGetString(pm->filename_tf);
  XtVaGetValues(pm->radio_box,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
  for(uBtn=0;uBtn<wcnt;uBtn++){		/* Find out which radio button is set */
    XtVaGetValues(pw[uBtn],XmNset,&set,NULL);
    if(set)  break;
    }
  XtVaSetValues(pm->radio_box,XmNuserData,uBtn,NULL);
  XtPopdown(pm->shell);
  if(pm->cbf){
    pm->cbf(w,client_data,call_data);				/* call back function here */
    }

return;
}

void PrintManagerDialog_Cancel(
	Widget w, XtPointer client_data, XtPointer call_data
){
PrintManager *pm=(PrintManager*)client_data;
size_t u = 0, wcnt = 0;
size_t uSet = 0; /* XmNuserData require a sizeof(void*) memory location */
Widget *pw = NULL;

  XmTextSetString(pm->prncmd_tf,pm->printer_command);
  XmTextSetString(pm->filename_tf,pm->filename);
  XtVaGetValues(pm->radio_box,XmNuserData,&uSet,NULL);
  XtVaGetValues(pm->radio_box,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
  for(u=0;u<wcnt;u++)
    XtVaSetValues(pw[u],XmNset, u==uSet ? True:False, NULL); 
  XtPopdown(pm->shell);

return;
}

/* raj 02-04-18 const char * changed to char * */
char *GetPrintManagerPrintCommand(XtPointer client_data)
{
PrintManager *pm=(PrintManager*)client_data;

return (char*)pm->printer_command;
}

/* raj 02-04-18 const char * changed to char * */
char *GetPrintManagerFileName(XtPointer client_data)
{
PrintManager *pm=(PrintManager*)client_data;

return (char*)pm->filename;
}

int IsPrintManagerFileSelected(XtPointer client_data)
{
size_t uSet = 0; /* XmNuserData require a sizeof(void*) memory location */
PrintManager *pm=(PrintManager*)client_data;

  XtVaGetValues(pm->radio_box,XmNuserData,&uSet,NULL);
  if(uSet==0)  return False;	/* printer */

return True;
}
