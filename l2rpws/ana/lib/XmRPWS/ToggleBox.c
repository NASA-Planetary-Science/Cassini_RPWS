#include "XmRPWS.h"



Widget CreateToggleBox(Widget parent,ToggleBoxItem *box,Boolean radio_behavior,Boolean radio_always_one,int num_columns)
{
char btn_name[32];
int i=0;
Widget RowColumn,toggle;
XmString str;

  RowColumn=XtVaCreateWidget(radio_behavior?"radio_box":"check_box",xmRowColumnWidgetClass,parent,
  	XmNisHomogeneous,True,XmNentryClass,xmToggleButtonWidgetClass,
  	XmNradioBehavior,radio_behavior?True:False,XmNradioAlwaysOne,radio_always_one?True:False,
  	XmNpacking,XmPACK_COLUMN,XmNnumColumns,num_columns,NULL);
  while(box[i].label){
    sprintf(btn_name,"button_%d",i);
    str=XmStringCreateLocalized(box[i].label);
    toggle=XtVaCreateManagedWidget(btn_name,xmToggleButtonWidgetClass,RowColumn,
    		XmNlabelString,str,XmNset,box[i].set,XmNuserData,box[i].user_data,NULL);
    XmStringFree(str);
    if(box[i].callback){
      XtAddCallback(toggle,XmNvalueChangedCallback,box[i].callback,box[i].data);
      }
    ++i;
    }


return RowColumn;
}


Boolean GetToggleBoxButtonState(Widget toggle_box,int button_number)
{
int i,wcnt;
Boolean set;
Widget *pw;

  XtVaGetValues(toggle_box,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
  if(button_number>=wcnt){
    set=False;
    fprintf(stderr,"GetToggleBoxButtonState(), button out of range %d\n",button_number);
    }
  else{
    XtVaGetValues((pw[button_number]),XmNset,(&set),NULL);
    }

return set;
}



void SetToggleBoxButtonState(Widget toggle_box,int button_number,Boolean set,Boolean notify)
{
int i,wcnt;
Widget *pw;

  XtVaGetValues(toggle_box,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
  if(button_number>=wcnt){
    fprintf(stderr,"SetToggleBoxButtonState(), button out of range %d\n",button_number);
    }
  else{
    XmToggleButtonSetState(pw[button_number],set,notify);
    }

return;
}



void SaveToggleBoxState(Widget toggle_box)
{
size_t uSet; /* XmNuserData require a sizeof(void*) memory location */

int wcnt;
Widget *pw;

  XtVaGetValues(toggle_box,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
  while(wcnt--){
    XtVaGetValues((*pw),XmNset,(&uSet),NULL);
    XtVaSetValues((*pw),XmNuserData,(XtPointer)uSet,NULL);
    ++pw;
    }

return;
}

void RestoreToggleBoxState(Widget toggle_box,Boolean notify)
{
int wcnt;
Widget *pw;
XtPointer set;

  XtVaGetValues(toggle_box,XmNchildren,&pw,XmNnumChildren,&wcnt,NULL);
  while(wcnt--){
    XtVaGetValues((*pw),XmNuserData,(&set),NULL);
/*    XtVaSetValues(*pw,XmNset,(Boolean)set,NULL); and fake XEvent XtCallActionProc(default_pb,"ArmAndActivate",cbs->event,NULL,0)*/
    XmToggleButtonSetState((*pw), set ? True : False, notify);
    ++pw;
    }

return;
}
