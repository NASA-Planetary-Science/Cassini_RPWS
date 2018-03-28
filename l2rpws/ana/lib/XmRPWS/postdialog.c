#include "XmRPWS.h"



void PostErrorDialog(Widget w,char *errmsg)
{
Arg args[11];
int n=0;
Widget d;
XmString msg=XmStringCreateLtoR(errmsg,XmFONTLIST_DEFAULT_TAG);

  while(w && !XtIsWMShell(w))  w=XtParent(w);
  XtSetArg(args[n],XmNdeleteResponse,XmDESTROY);++n;
  XtSetArg(args[n],XmNmessageString,msg);++n;
  XtSetArg(args[n],XmNtitle,"Error");++n;
  XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL);++n;
  d=XmCreateErrorDialog(w," ",args,n);
  XtUnmanageChild(XmMessageBoxGetChild(d,XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(d,XmDIALOG_HELP_BUTTON));
  XtManageChild(d);
  XtPopup(XtParent(d),XtGrabNonexclusive);
  XmStringFree(msg);
  
return;
}


