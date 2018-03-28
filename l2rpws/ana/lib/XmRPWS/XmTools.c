#include "XmRPWS.h"
#include <Xm/SashP.h>

void TurnOffSashTraversal(Widget paned)
{
Widget *children;
int num_children;

  XtVaGetValues(paned,XmNchildren,&children,XmNnumChildren,&num_children,NULL);
  while(num_children-->0)
    if(XmIsSash(children[num_children]))
      XtVaSetValues(children[num_children],XmNtraversalOn,False,NULL);

return;
}



void SaveWidgetValue(Widget w)
{
unsigned char rowcol_type=0;
int wcnt;
Widget *pw;
WidgetClass class=XtClass(w);
XtPointer usrdata;

  XtVaGetValues(w,XmNrowColumnType,&rowcol_type,NULL);
  if(class==xmScaleWidgetClass){		
    XmScaleGetValue(w,((int*)&usrdata));
    }
  else if(class==xmToggleButtonWidgetClass){
    usrdata=XmToggleButtonGetState(w) ? (XtPointer)1 : (XtPointer)0;
    }
  else if(class==xmTextWidgetClass){
    XtVaGetValues(w,XmNuserData,&usrdata,NULL);
    XtFree(usrdata);
    usrdata=(XtPointer)XmTextGetString(w);
    }
  else if((class==xmRowColumnWidgetClass) && (rowcol_type==XmMENU_OPTION)){
    XtVaGetValues(w,XmNmenuHistory,(Widget *)&usrdata,NULL);
    }
  else if(XmIsManager(w)){
    XtVaGetValues(w,XmNchildren,(&pw),XmNnumChildren,(&wcnt),NULL);
    while(wcnt--)
      SaveWidgetValue(*pw++);
    /* RowColumn,PanedWindow,Form are XmCReadOnly */
    return;
    }
  else{
    return;
    }
  XtVaSetValues(w,XmNuserData,usrdata,NULL);

return;
}

void RestoreWidgetValue(Widget w)
{
XmAnyCallbackStruct *cbs;
unsigned char rowcol_type=0;
int wcnt;
Widget *pw;
WidgetClass class=XtClass(w);
XtPointer usrdata;

  XtVaGetValues(w,XmNuserData,(&usrdata),NULL);
  XtVaGetValues(w,XmNrowColumnType,&rowcol_type,NULL);
  if(class==xmScaleWidgetClass){
    XmScaleSetValue(w,((ptrdiff_t)usrdata));
    }
  else if(class==xmToggleButtonWidgetClass){
    XmToggleButtonSetState(w,(Boolean)(ptrdiff_t)usrdata,True);
    }
  else if(class==xmTextWidgetClass){
    XmTextSetString(w,((char*)usrdata));
    }
  else if((class==xmRowColumnWidgetClass) && (rowcol_type==XmMENU_OPTION)){
    XtVaSetValues(w,XmNmenuHistory,((Widget)usrdata),NULL);
    XtCallActionProc(((Widget)usrdata),"ArmAndActivate",NULL,NULL,0);
    }
  else if(XmIsManager(w)){
    XtVaGetValues(w,XmNchildren,(&pw),XmNnumChildren,(&wcnt),NULL);
    while(wcnt--)
      RestoreWidgetValue(*pw++);
    /* RowColumn,PanedWindow,Form are XmCReadOnly */
    return;
    }
  else{
    return;
    }

return;
}

void SetWidgetSensitivity(Widget w,Boolean sensitive)
{
int wcnt;
Widget *pw;
WidgetClass class=XtClass(w);

  if(class==xmLabelWidgetClass);		/* Do Nothing Statements */
  else if(class==xmScaleWidgetClass);
  else if(class==xmTextWidgetClass);
  else if(class==xmToggleButtonWidgetClass);
  else if(class==xmPushButtonWidgetClass);
  else if(XmIsManager(w)){
    XtVaGetValues(w,XmNchildren,(&pw),XmNnumChildren,(&wcnt),NULL);
    while(wcnt--)
      SetWidgetSensitivity(*pw++,sensitive);
    /* RowColumn,PanedWindow,Form are XmCReadOnly */
    return;
    }
  else{
    return;
    }
  XtVaSetValues(w,XmNsensitive,sensitive,NULL);

return;
}

void ToggleWidgetSensitivity(Widget w)
{
int wcnt;
Boolean sensitive;
Widget *pw;
WidgetClass class=XtClass(w);

  if(class==xmLabelWidgetClass);		/* Do Nothing Statements */
  else if(class==xmPushButtonWidgetClass);
  else if(class==xmScaleWidgetClass);
  else if(class==xmTextWidgetClass);
  else if(class==xmToggleButtonWidgetClass);
  else if(XmIsManager(w)){
    XtVaGetValues(w,XmNchildren,(&pw),XmNnumChildren,(&wcnt),NULL);
    while(wcnt--)
      SetWidgetSensitivity(*pw++,sensitive);
    /* RowColumn,PanedWindow,Form are XmCReadOnly */
    return;
    }
  else{
    return;
    }
  XtVaGetValues(w,XmNsensitive,(&sensitive),NULL);
  XtVaSetValues(w,XmNsensitive,(!sensitive),NULL);

return;
}
