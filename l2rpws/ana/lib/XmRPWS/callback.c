#include "XmRPWS.h"

void Exit(Widget w,XtPointer client_data,XtPointer call_data)
{
exit(0);
  
return;
}

void ToggleSensitivity(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget widget=(Widget)client_data;

  XtSetSensitive(widget,(XtIsSensitive(widget)==True?False:True));

return;
}

void DestroyShell(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget shell=(Widget)client_data;

  XtDestroyWidget(shell);

return;
}

void PopupShell(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget shell=(Widget)client_data;

    XtPopup(shell,XtGrabNone);

return;
}

void PopdownShell(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget shell=(Widget)client_data;

    XtPopdown(shell);

return;
}

void SaveValuesCallback(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget shell=(Widget)client_data;

    SaveWidgetValue(shell);

return;
}

void RestoreValuesCallback(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget shell=(Widget)client_data;

    RestoreWidgetValue(shell);

return;
}
