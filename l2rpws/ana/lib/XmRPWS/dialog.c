#include "XmRPWS.h"


Widget CreateActionArea(Widget parent,ActionAreaItem *action,int tightness)
{
Widget action_area,widget;
XtWidgetGeometry size;
int i,num_actions;

  widget=XtVaCreateManagedWidget("Separator",xmSeparatorWidgetClass,parent,NULL);
  size.request_mode=CWHeight;
  XtQueryGeometry(widget,NULL,&size);
  XtVaSetValues(widget,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL);
  
  for(num_actions=0;action[num_actions].label!=NULL;num_actions++);			 
  action_area=XtVaCreateWidget("action_area",xmFormWidgetClass,parent,
  	XmNfractionBase,tightness*num_actions-1,XmNleftOffset,10,XmNrightOffset,10,NULL);

  for(i=0;action[i].label!=NULL;i++){
    widget=XtVaCreateManagedWidget(action[i].label,xmPushButtonWidgetClass,action_area,
    	XmNleftAttachment,i?XmATTACH_POSITION:XmATTACH_FORM,XmNleftPosition,tightness*i,
    	XmNtopAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,
    	XmNrightAttachment,i!=num_actions-1?XmATTACH_POSITION:XmATTACH_FORM,
    	XmNrightPosition,tightness*i+(tightness-1),XmNshowAsDefault,i==0,
    	XmNdefaultButtonShadowThickness,1,NULL);
    if(action[i].callback)
      XtAddCallback(widget,XmNactivateCallback,action[i].callback,action[i].data);
    if(i==0){
      Dimension height,h;
      XtVaGetValues(action_area,XmNmarginHeight,&h,NULL);
      XtVaGetValues(widget,XmNheight,&height,NULL);
      height+=2*h;
      XtVaSetValues(action_area,XmNdefaultButton,widget,XmNpaneMaximum,height,XmNpaneMinimum,height,NULL);
      }
    }
  XtManageChild(action_area);
  
return action_area;
}


Widget CreateDialogButtons(Widget parent,ActionAreaItem *action)
{
int i,j,num_actions;
int tightness=2;
XmString string;
XtWidgetGeometry size;
Widget shell,form,pushbutton,widget;

  shell=XtParent(parent);
  widget=XtVaCreateManagedWidget("Separator",xmSeparatorWidgetClass,parent,NULL);
  size.request_mode=CWHeight;
  XtQueryGeometry(widget,NULL,&size);
  XtVaSetValues(widget,XmNpaneMaximum,size.height,XmNpaneMinimum,size.height,NULL);
  
  for(num_actions=0,i=0;action[num_actions].label!=NULL;num_actions++);
    			 
  form=XtVaCreateWidget("form",xmFormWidgetClass,parent,
  	XmNfractionBase,tightness*num_actions-1,XmNleftOffset,10,XmNrightOffset,10,NULL);
  	
  for(i=0;action[i].label!=NULL;i++){
    string=XmStringCreateLocalized(action[i].label);
    pushbutton=XtVaCreateManagedWidget("push_button",xmPushButtonWidgetClass,form,XmNlabelString,string,
    	XmNleftAttachment,i?XmATTACH_POSITION:XmATTACH_FORM,XmNleftPosition,tightness*i,
    	XmNtopAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,
    	XmNrightAttachment,i!=num_actions-1?XmATTACH_POSITION:XmATTACH_FORM,
    	XmNrightPosition,tightness*i+(tightness-1),XmNshowAsDefault,i==0,
    	XmNdefaultButtonShadowThickness,1,NULL);
    XmStringFree(string);

/* 
size.request_mode=CWWidth;
XmNwidth,size.width,
XtQueryGeometry(pushbutton,NULL,&size);
XtVaSetValues(pushbutton,XmNwidth,size.width,NULL);	
     XtQueryGeometry(pushbutton,NULL,&size);
    XtQueryGeometry(pushbutton,NULL,&biggest);
*/
    if(!strcmp("Ok",action[i].label)){
      XtAddCallback(pushbutton,XmNactivateCallback,SaveValuesCallback,(XtPointer)parent);
      XtAddCallback(pushbutton,XmNactivateCallback,PopdownShell,(XtPointer)shell);
      }
    else if(!strcmp("Apply",action[i].label)){
      XtAddCallback(pushbutton,XmNactivateCallback,SaveValuesCallback,(XtPointer)parent);
      }
    else if(!strcmp("Cancel",action[i].label)){
      XtAddCallback(pushbutton,XmNactivateCallback,RestoreValuesCallback,(XtPointer)parent);
      XtAddCallback(pushbutton,XmNactivateCallback,PopdownShell,(XtPointer)shell);
      }
    if(action[i].callback)
      XtAddCallback(pushbutton,XmNactivateCallback,action[i].callback,action[i].data);

    if(i==0){
      Dimension height,h;
      XtVaGetValues(form,XmNmarginHeight,&h,NULL);
      XtVaGetValues(pushbutton,XmNheight,&height,NULL);
      height+=2*h;
      XtVaSetValues(form,XmNdefaultButton,widget,XmNpaneMaximum,height,XmNpaneMinimum,height,NULL);
      }
    }
  XtManageChild(form);

/*
    WM_DELETE_WINDOW=XmInternAtom(XtDisplay(shell),"WM_DELETE_WINDOW",False);
    XmAddWMProtocolCallback(shell,WM_DELETE_WINDOW,PopdownShell,(XtPointer)shell);
*/

  
return form;
}
