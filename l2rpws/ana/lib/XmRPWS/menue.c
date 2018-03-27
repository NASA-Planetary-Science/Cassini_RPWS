#include "XmRPWS.h"

Widget BuildMenu(Widget parent,int menu_type,char *menu_title,char menu_mnemonic,Boolean tear_off,MenuItem *items)
{
Widget menu,cascade,widget;
int i;
XmString str;

  if(menu_type==XmMENU_PULLDOWN || menu_type==XmMENU_OPTION)
    menu=XmCreatePulldownMenu(parent,"_pulldown",NULL,0);
  else if(menu_type==XmMENU_POPUP)
    menu=XmCreatePopupMenu(parent,"_popup",NULL,0);
  else{
    XtWarning("Invalid menu type passed to BuildMenu()");
    return NULL;
    }
    
  if(tear_off)
    XtVaSetValues(menu,XmNtearOffModel,XmTEAR_OFF_ENABLED,NULL);
    
  if(menu_type==XmMENU_PULLDOWN){
    str=XmStringCreateLocalized(menu_title);
    cascade=XtVaCreateManagedWidget(menu_title,xmCascadeButtonWidgetClass,parent,
    	XmNsubMenuId,menu,XmNlabelString,str,XmNmnemonic,menu_mnemonic,NULL);
    XmStringFree(str);
    }
  else if(menu_type==XmMENU_OPTION){
    Arg args[5];
    int n=0;
    str=XmStringCreateLocalized(menu_title);
    XtSetArg(args[n],XmNsubMenuId,menu);n++;
    XtSetArg(args[n],XmNlabelString,str);n++;
    cascade=XmCreateOptionMenu(parent,menu_title,args,n);
    XmStringFree(str);
    }

  for(i=0;items[i].label!=NULL;i++){
    if(items[i].subitems){
      if(menu_type==XmMENU_OPTION){
        XtWarning("You can't have submenus from option menu items.");
        continue;
        }
      else
        widget=BuildMenu(menu,XmMENU_PULLDOWN,items[i].label,items[i].mnemonic,tear_off,items[i].subitems);
      }
    else{
      if(items[i].widget_class==NULL){				/* Elicit RadioButton Respones */
        XtVaSetValues(menu,XmNradioBehavior,True,NULL);
        items[i].widget_class=&xmToggleButtonWidgetClass;
        widget=XtVaCreateManagedWidget(items[i].label,*items[i].widget_class,menu,NULL);
        if(i==0)  XtVaSetValues(widget,XmNset,True,NULL);
        }
      else
        widget=XtVaCreateManagedWidget(items[i].label,*items[i].widget_class,menu,NULL);
      }
      
/*
    if(items[i].user_data){
      XtVaSetValues(widget,XmNuserData,items[i].user_data,NULL);
      }
*/
      
    if(items[i].mnemonic){
      XtVaSetValues(widget,XmNmnemonic,items[i].mnemonic,NULL);
      }
      
    if(items[i].accelerator){
      str=XmStringCreateLocalized(items[i].accel_text);
      XtVaSetValues(widget,XmNaccelerator,items[i].accelerator,XmNacceleratorText,str,NULL);
      XmStringFree(str);
      }
    if(items[i].callback){
      XtAddCallback(widget,items[i].widget_class==&xmToggleButtonWidgetClass ? XmNvalueChangedCallback : XmNactivateCallback,
      	items[i].callback,items[i].callback_data);
      }
    }
  
return menu_type==XmMENU_POPUP ? menu : cascade;
}

