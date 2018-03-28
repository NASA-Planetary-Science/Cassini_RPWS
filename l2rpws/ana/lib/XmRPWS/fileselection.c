#include "XmRPWS.h"

static char *FileName;

void FileSelection_Ok(Widget w,XtPointer client_data,XtPointer call_data);


static CallBackFunction cbf;

void PostFileSelectionDialog(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget fsd,caller;
XmString string;

  cbf=(CallBackFunction)client_data;
  caller=w;
  while(w && !XtIsWMShell(w))  w=XtParent(w);
  fsd=XmCreateFileSelectionDialog(w,"filesd",NULL,0);
  XtVaSetValues(XtParent(fsd),XmNtitle,"Data Source",NULL);
  string=XmStringCreateLocalized("/data/cassini/");
  XtVaSetValues(fsd,XmNdirectory,string,NULL);
  XmStringFree(string);
  string=XmStringCreateLocalized("*.u*");
  XtVaSetValues(fsd,XmNpattern,string,NULL);
  XmStringFree(string);
  
  XtSetSensitive(XmFileSelectionBoxGetChild(fsd,XmDIALOG_HELP_BUTTON),False);
  if(strcmp("johnson",getenv("USER"))){	/* This is not consistent with OSF Motif Style Guide */
    XtAddCallback(fsd,XmNokCallback,PopdownShell,(XtPointer)XtParent(fsd));
    XtAddCallback(fsd,XmNokCallback,DestroyShell,(XtPointer)XtParent(fsd));
    }
  XtAddCallback(fsd,XmNokCallback,FileSelection_Ok,(XtPointer)XtParent(fsd));
  XtAddCallback(fsd,XmNcancelCallback,PopdownShell,(XtPointer)XtParent(fsd));
  XtAddCallback(fsd,XmNcancelCallback,DestroyShell,(XtPointer)XtParent(fsd));
  XtAddCallback(XtParent(fsd),XmNpopupCallback,ToggleSensitivity,(XtPointer)caller);
  XtAddCallback(XtParent(fsd),XmNpopdownCallback,ToggleSensitivity,(XtPointer)caller);
  XtManageChild(fsd);
  XtPopup(XtParent(fsd),XtGrabNone);

}

void FileSelection_Ok(Widget w,XtPointer client_data,XtPointer call_data)
{
char *filename;
XmFileSelectionBoxCallbackStruct *cbs=(XmFileSelectionBoxCallbackStruct*)call_data;

  if(!XmStringGetLtoR(cbs->value,XmFONTLIST_DEFAULT_TAG,&filename))
    return;/* internal error */
  if(filename){
    free(FileName);
    if( ( FileName=(char*)malloc(sizeof(char)*(strlen(filename)+1)) )==NULL)
      PostErrorDialog(w,"FileSelection:Unable to malloc( ) filename\n");
    else
      strcpy(FileName,filename);
    }
  XtFree(filename);

  cbf(w,NULL,call_data);

return;
}

char *GetFileSelectionFilename(void)
{return FileName;}
