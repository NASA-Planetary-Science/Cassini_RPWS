#ifndef _XMRPWS_H
#define _XMRPWS_H


#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>


#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/MainW.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/Frame.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>

#include <Xm/CascadeB.h>	
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Label.h>	
#include <Xm/MessageB.h>	
#include <Xm/PushB.h>
#include <Xm/Scale.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

#ifdef __cplusplus
extern "C" {
#endif

#define XmRPWSradioButton	0x00		/* Use as widget class pointer 
						to elicit Radio Button Behavior
						from Build Menu */
 
#define RecorderDirectionRewind		0x01
#define RecorderDirectionStepRewind	0x02
#define RecorderDirectionPlayRewind	0x03
#define RecorderDirectionStop		0x04
#define RecorderDirectionPlayForward	0x05
#define RecorderDirectionStepForward	0x06
#define RecorderDirectionFastForward	0x07

#ifndef INST_SHARE
#error define the install share location (equiv. of /usr/local/share)
#endif
		
static char *RecorderBitMaps[]={
	INST_SHARE "/ana/Rewind.bm",
	INST_SHARE "/ana/StepRewind.bm",
	INST_SHARE "/ana/PlayRewind.bm",
	INST_SHARE "/ana/Stop.bm",
	INST_SHARE "/ana/PlayForward.bm",
	INST_SHARE "/ana/StepForward.bm",
	INST_SHARE "/ana/FastForward.bm"
};
		

typedef void (*CallBackFunction)(Widget,XtPointer,XtPointer);
 
typedef struct _menu_item{
  char *label;
  WidgetClass *widget_class;
/*  XtPointer user_data;*/
  char mnemonic;
  char *accelerator;
  char *accel_text;
  CallBackFunction callback;
  XtPointer callback_data;
  struct _menu_item *subitems;
  }MenuItem;
  
typedef struct{			
  char *title;			/* File Selection Box Title */
  char *directory;		/* Initial Directory */
  char *pattern;		/* Filter Pattern */
  CallBackFunction cbf_ok;	/* 'Ok' button callback, calling widget is of type XmFileSelectionBox */
  XtPointer cbf_ok_data; 	/* client_data for the callback, call_data is of type XmFileSelectionCallbackStruct */
  CallBackFunction cbf_cancel;	/* 'Cancel' button callback, calling widget is of type XmFileSelectionBox */
  XtPointer cbf_cancel_data;	/* client_data for the callback, call_data is of type XmFileSelectionCallbackStruct  */
  }FileSelection;
  
typedef struct{
  char *label;
  void (*callback)();
  XtPointer data;
  }ActionAreaItem;

typedef struct{
  char *label;
  Boolean set;
  XtPointer user_data;
  void (*callback)();
  XtPointer data;
  }ToggleBoxItem;
/* 
Universal Rules for XmRPWS library 

  I.  User data of widgets are reserved, including the calling widget. 
  II.  Access to internal widgets will be limited.
  
*/

Widget BuildMenu(Widget parent,int menu_type,char *menu_title,char menu_mnemonic,Boolean tear_off,MenuItem *items);
/* menu_type = XmMENU_POPUP,XmMENU_PULLDOWN,XmMENU_OPTION */
Widget CreateActionArea(Widget parent,ActionAreaItem *action,int tightness);
Widget CreateDialogButtons(Widget parent,ActionAreaItem *action);

/* User data in Toggle Box is PRIVATE!!! */
Widget CreateToggleBox(Widget parent,ToggleBoxItem *box,Boolean radio_behavior,Boolean radio_always_one,int num_columns);
void SaveToggleBoxState(Widget toggle_box);
/* 	
	notify==True, XmNvalueChanged callback is involked;
	notify==False, silent operation 
*/
void RestoreToggleBoxState(Widget toggle_box,Boolean notify);
Boolean GetToggleBoxButtonState(Widget toggle_box,int button_number);
void SetToggleBoxButtonState(Widget toggle_box,int button_number,Boolean set,Boolean notify);



Widget CreateTextFloatingPoint(Widget parent,float value,int num_columns);
/* Private Callback Function */
void TextModification_FloatingPoint(Widget w,XtPointer client_data,XtPointer call_data);


/* XmTools */
void SaveWidgetValue(Widget w);					/* w may be a manager */
void RestoreWidgetValue(Widget w);				/* w may be a manager */
void SetWidgetSensitivity(Widget w,Boolean sensitive);		/* w may be a manager */
void ToggleWidgetSensitivity(Widget w);				/* w may be a manager */

/* Text : location : "top","right","bottom","left" */
Widget CreateTextEntry(Widget parent,char *leading_text,int num_columns,char *location);
char *GetTextEntryString(Widget text_entry);		/* XtFree returned value */
void SetTextEntryString(Widget text_entry,char *value);

void PostErrorDialog(Widget w,char *errmsg);

void TurnOffSashTraversal(Widget paned);


void ToggleSensitivity(Widget w,XtPointer client_data,XtPointer call_data);/* toggle the sensitivity of client_data */
void DestroyShell(Widget w,XtPointer client_data,XtPointer call_data);/* destroy the client data */
void Exit(Widget w,XtPointer client_data,XtPointer call_data);
void PopupShell(Widget w,XtPointer client_data,XtPointer call_data);
void PopdownShell(Widget w,XtPointer client_data,XtPointer call_data);
/* shell = client_data */
void RestoreValuesCallback(Widget w,XtPointer client_data,XtPointer call_data);
void SaveValuesCallback(Widget w,XtPointer client_data,XtPointer call_data);


void PostPrintManagerDialog(Widget w,XtPointer client_data,XtPointer call_data);
/* call_data will be the callback function to be registered with the print button */
/* From within this callback you may use the following functions.  Use the client_data */
/* as the argument.   */
char *GetPrintManagerPrintCommand(XtPointer client_data);
char *GetPrintManagerFileName(XtPointer client_data);
int IsPrintManagerFileSelected(XtPointer client_data);

/* obsolete */
void PostFileManagerDialog(Widget w,XtPointer client_data,XtPointer call_data);
void PostFileSelectionDialog(Widget w,XtPointer client_data,XtPointer call_data);
char *GetFileSelectionFilename(void);
/* obsolete */

void PostFileSelectionBox(Widget w,XtPointer client_data,XtPointer call_data);
			/* client_data shall be of type FileSelection* */
	/* Do not use any save or restore functions with this.  This is allready
	    handled by the 'Ok', 'Cancel', and WindowManager 'Close' */


void PostRecorderDialog(Widget w,XtPointer client_data,XtPointer call_data);
long GetCurrentRecorderDirection(XtPointer client_data);
void SetCurrentRecorderDirection(XtPointer client_data,long direction);

#ifdef __cplusplus
}
#endif

#endif 
