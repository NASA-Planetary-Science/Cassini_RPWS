#include "XmRPWS.h"

#ifndef RPWS_DATA
#error Default data directory RPWS_DATA is not defined
#endif


/* Private access, NOT Public OBSOLETE */
typedef struct{
  char *filter,*selection;		
  CallBackFunction cbf;
  Widget shell,dialog;			/* file selection dialog */
  }FileManager_private;


/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */

typedef struct{
  XmString dir,pat;			/* directory and filter pattern */
  Widget parent,caller,shell,mng;	/* Parent, Caller, Shell and File Selection Box */
  }FileSelection_private;

void PostFileSelectionBox(Widget w,XtPointer client_data,XtPointer call_data)
{
FileSelection *filesel=(FileSelection*)client_data;
FileSelection_private *filesel_p;
Atom WM_DELETE_WINDOW;
XmString string;

void FileSelectionBox_Ok(Widget w,XtPointer client_data,XtPointer call_data);
void FileSelectionBox_Cancel(Widget w,XtPointer client_data,XtPointer call_data);
void FileSelectionBox_Close(Widget w,XtPointer client_data,XtPointer call_data);


  XtVaGetValues(w,XmNuserData,&filesel_p,NULL);
  if(!filesel_p){
    filesel_p=(FileSelection_private*)XtMalloc(sizeof(FileSelection_private));
    filesel_p->parent=filesel_p->caller=w;
    XtVaSetValues(filesel_p->caller,XmNuserData,(XtPointer)filesel_p,NULL);

    filesel_p->mng=XmCreateFileSelectionDialog(filesel_p->parent,"filesd",NULL,0);
    filesel_p->shell=XtParent(filesel_p->mng);
    XtVaSetValues(filesel_p->shell,XmNdeleteResponse,XmDO_NOTHING,NULL);

/*
    filesel_p->shell=XtVaCreatePopupShell("shell",xmDialogShellWidgetClass,filesel_p->parent,
    	XmNdeleteResponse,XmDO_NOTHING,NULL);
    filesel_p->mng=XtVaCreateWidget("file_selection_box",xmFileSelectionBoxWidgetClass,filesel_p->shell,NULL);
*/
    if(filesel){
      XtVaSetValues(filesel_p->shell,XmNtitle,(filesel->title?filesel->title:"File Manager"),NULL);
      if(filesel->directory){
        filesel_p->dir=XmStringCreateLocalized(filesel->directory);
        XtVaSetValues(filesel_p->mng,XmNdirectory,filesel_p->dir,NULL);
        /* XmStringFree is handled in the callback */
        }
      else{
        filesel_p->dir=XmStringCreateLocalized("./");
        /* XmStringFree is handled in the callback */
        }
      if(filesel->pattern){
        filesel_p->pat=XmStringCreateLocalized(filesel->pattern);
        XtVaSetValues(filesel_p->mng,XmNpattern,filesel_p->pat,NULL);
        /* XmStringFree is handled in the callback */
        }
      else{
        filesel_p->pat=XmStringCreateLocalized("*");
        /* XmStringFree is handled in the callback */
        }
      if(filesel->cbf_ok)
        XtAddCallback(filesel_p->mng,XmNokCallback,filesel->cbf_ok,(XtPointer)filesel->cbf_ok_data);
      if(filesel->cbf_cancel)
        XtAddCallback(filesel_p->mng,XmNcancelCallback,filesel->cbf_cancel,(XtPointer)filesel->cbf_cancel_data);
      }
    else{
      XtVaSetValues(filesel_p->shell,XmNtitle,"File Manager",NULL);
      filesel_p->dir=XmStringCreateLocalized("./");
      /* XmStringFree is handled in the callback */
      filesel_p->pat=XmStringCreateLocalized("*");
      /* XmStringFree is handled in the callback */
      }
    
    XtVaSetValues(filesel_p->shell,XmNtitle,(filesel->title?filesel->title:"File Manager"),NULL);

    XtAddCallback(filesel_p->mng,XmNokCallback,    FileSelectionBox_Ok,   (XtPointer)filesel_p);
    XtAddCallback(filesel_p->mng,XmNcancelCallback,FileSelectionBox_Cancel,(XtPointer)filesel_p);
    XtAddCallback(filesel_p->mng,XmNokCallback,    PopdownShell,(XtPointer)filesel_p->shell);
    XtAddCallback(filesel_p->mng,XmNcancelCallback,PopdownShell,(XtPointer)filesel_p->shell);
    XtAddCallback(filesel_p->shell,XmNpopdownCallback,ToggleSensitivity,(XtPointer)filesel_p->caller);
    XtAddCallback(filesel_p->shell,XmNpopupCallback,  ToggleSensitivity,(XtPointer)filesel_p->caller);
    WM_DELETE_WINDOW=XmInternAtom(XtDisplay(filesel_p->shell),"WM_DELETE_WINDOW",False);
    XmAddWMProtocolCallback(filesel_p->shell,WM_DELETE_WINDOW,ToggleSensitivity,(XtPointer)filesel_p->caller);
    XmAddWMProtocolCallback(filesel_p->shell,WM_DELETE_WINDOW,FileSelectionBox_Close,(XtPointer)filesel_p);

    XtSetSensitive(XmFileSelectionBoxGetChild(filesel_p->mng,XmDIALOG_HELP_BUTTON),False);
    XtManageChild(filesel_p->mng); 	
    }

  XtPopup(filesel_p->shell,XtGrabNone);

return;
}

void FileSelectionBox_Ok(Widget w,XtPointer client_data,XtPointer call_data)
{
FileSelection_private *fs_p=(FileSelection_private*)client_data;

  XmStringFree(fs_p->dir);
  XtVaGetValues(fs_p->mng,XmNdirectory,&(fs_p->dir),NULL);
  XmStringFree(fs_p->pat);
  XtVaGetValues(fs_p->mng,XmNpattern,&(fs_p->pat),NULL);

return;
}

void FileSelectionBox_Cancel(Widget w,XtPointer client_data,XtPointer call_data)
{
FileSelection_private *fs_p=(FileSelection_private*)client_data;

  XtVaSetValues(fs_p->mng,XmNdirectory,fs_p->dir,NULL);
  XtVaSetValues(fs_p->mng,XmNpattern,  fs_p->pat,NULL);

return;
}

void FileSelectionBox_Close(Widget w,XtPointer client_data,XtPointer call_data)
{
FileSelection_private *fs_p=(FileSelection_private*)client_data;

  XmStringFree(fs_p->dir);					/* Free allocated resources */
  XmStringFree(fs_p->pat);
  XtVaSetValues(fs_p->caller,XmNuserData,(XtPointer)NULL,NULL);	/* Disassocate with caller */
  XtDestroyWidget(fs_p->shell);					/* Destroy all children and resources associated with them */
  XtFree((char*)fs_p);							/* Free Instance */

return;
}



/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */



/* OBSOLETE */
void PostFileManagerDialog(Widget w,XtPointer client_data,XtPointer call_data)
{
FileManager_private *file_mng;
FileSelection *fs=(FileSelection*)client_data;
Atom WM_DELETE_WINDOW;
XmString string;

void FileManagerDialog_Ok(Widget w,XtPointer client_data,XtPointer call_data);
void FileManagerDialog_Cancel(Widget w,XtPointer client_data,XtPointer call_data);


  XtVaGetValues(w,XmNuserData,&file_mng,NULL);
  if(!file_mng){
    file_mng=(FileManager_private*)XtMalloc(sizeof(FileManager_private));
    XtVaSetValues(w,XmNuserData,(XtPointer)file_mng,NULL);
    file_mng->dialog=XmCreateFileSelectionDialog(w,"file_selection_dialog",NULL,0);
    file_mng->shell=XtParent(file_mng->dialog);
    
  XtVaSetValues(file_mng->shell,XmNtitle,"Data Source",NULL);
  
  
  string=XmStringCreateLocalized( RPWS_DATA );
  
  XtVaSetValues(file_mng->dialog,XmNdirectory,string,NULL);
  XmStringFree(string);
  string=XmStringCreateLocalized("*.u*");
  XtVaSetValues(file_mng->dialog,XmNpattern,string,NULL);
  XmStringFree(string);
    
  XtSetSensitive(XmFileSelectionBoxGetChild(file_mng->dialog,XmDIALOG_HELP_BUTTON),False);
  
  XtAddCallback(file_mng->dialog,XmNokCallback,FileManagerDialog_Ok,(XtPointer)file_mng);
  XtAddCallback(file_mng->dialog,XmNcancelCallback,FileManagerDialog_Cancel,(XtPointer)file_mng);




    WM_DELETE_WINDOW=XmInternAtom(XtDisplay(file_mng->shell),"WM_DELETE_WINDOW",False);
    XmAddWMProtocolCallback(file_mng->shell,WM_DELETE_WINDOW,FileManagerDialog_Cancel,(XtPointer)file_mng);
    XtAddCallback(file_mng->shell,XmNpopdownCallback,ToggleSensitivity,(XtPointer)w);
    XtAddCallback(file_mng->shell,XmNpopupCallback,ToggleSensitivity,(XtPointer)w);
    file_mng->cbf=(CallBackFunction)client_data;
    XtManageChild(file_mng->dialog);
    XtSetSensitive(w,False);
    }

  XtPopup(XtParent(file_mng->dialog),XtGrabNone);

return;
}



void FileManagerDialog_Ok(Widget w,XtPointer client_data,XtPointer call_data)
{
FileManager_private *fm=(FileManager_private*)client_data; 

  XtPopdown(fm->shell);
  if(fm->cbf){
    fm->cbf(w,client_data,call_data);				
    }

return;
}

void FileManagerDialog_Cancel(Widget w,XtPointer client_data,XtPointer call_data)
{
FileManager_private *fm=(FileManager_private*)client_data; 

  XtPopdown(fm->shell);

return;
}


