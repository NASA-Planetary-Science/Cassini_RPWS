#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "XmRPWS.h"

#define form_string		"form"
#define text_string		"text"
#define label_string		"label"

Widget CreateTextFloatingPoint(Widget parent,float value,int num_columns)
{
char ch[128];
XmString str;
Widget Text;

  sprintf(ch,"%f",value);
  Text=XtVaCreateManagedWidget("text",xmTextWidgetClass,parent,
  	XmNcolumns,(num_columns>0?num_columns:20),XmNvalue,ch,NULL);
  XtAddCallback(Text,XmNmodifyVerifyCallback,TextModification_FloatingPoint,NULL);

return Text;
}

void TextModification_FloatingPoint(Widget w,XtPointer client_data,XtPointer call_data)
{
char *pch,ch_period='.',ch_E='E',ch_e='e',ch_plus='+',ch_minus='-',ch_Eminus='-',chEplus='+';
int len;
XmTextVerifyCallbackStruct *cbs=(XmTextVerifyCallbackStruct*)call_data;

  if(cbs->text->ptr==NULL)
    return;
  
  /* See what we have already */
  XtVaGetValues(w,XmNvalue,&pch,NULL);
  if(*pch=='-' || *pch=='+'){		/* Filter all other signs, except special case E */
    ch_plus='0';
    ch_minus='0';
    }
  while(*pch){
    if(*pch=='.'){			/* Filter all other decimal point */
      ch_period='0';
      }				
    else if(*pch=='E' || *pch=='e'){	/* Filter all other E/e */
      ch_E='0';
      ch_e='0';
      ch_period='0';
      if(*(pch+1)=='\0'){		/* next character may be a '+' or a '-' */
        ch_plus='+';
        ch_minus='-';
        }
      else if(*(pch+1)=='+' || *(pch+1)=='-'){
        ch_plus='0';
        ch_minus='0';
        }
      }		
    ++pch;
    }
    
  /* Filter Floating Point Format */
  /* [+|-]nnn.nnn[ E[+|-] | [e[+|-]] ]nnn */
  /* Ex: '-123.456e+333' */
 for(len=0;len<cbs->text->length;len++){    
    if( (!isdigit(cbs->text->ptr[len])) ){		
      int i;
      if(cbs->text->ptr[len]==ch_period){	/* Can have only one decimal point */
        ch_period='0';				/* Filter all others */ 
        }
      else if(cbs->text->ptr[len]==ch_plus || cbs->text->ptr[len]==ch_minus){
        ch_plus='0';
        ch_minus='0';
        }
      else if(cbs->text->ptr[len]==ch_E || cbs->text->ptr[len]==ch_e){
        ch_E='0';
        ch_e='0';
        ch_period='0';
        if( ((len+1)<cbs->text->length) && 
             (cbs->text->ptr[len+1]=='+' || cbs->text->ptr[len+1]=='-') ){
          ch_plus='+';
          ch_minus='-';
          }				
        }
      else{					
        for(i=len;(i+1)<cbs->text->length;i++)
          cbs->text->ptr[i]=cbs->text->ptr[i+1];
        cbs->text->length--;
        len--;
        }
      ch_plus='0';		/* only the first character and following 'E' */
      ch_minus='0';
      }/* if not a digit */
    }/* for */


return;
}

Widget CreateTextEntry(Widget parent,char *leading_text,int num_columns,char *location)
{
XmString str;
Widget form,label,text;

  str=XmStringCreateLocalized(leading_text);
  form=XtVaCreateWidget(form_string,xmFormWidgetClass,parent,NULL);
  if(!strcmp("top",location)){
    label=XtVaCreateManagedWidget(label_string,xmLabelWidgetClass,form,
  	XmNlabelString,str,
  	XmNtopAttachment,XmATTACH_FORM,
  	XmNleftAttachment,XmATTACH_FORM,XmNrightAttachment,XmATTACH_FORM,NULL);
    text=XtVaCreateManagedWidget(text_string,xmTextWidgetClass,form,
        XmNcolumns,num_columns,
  	XmNtopAttachment,XmATTACH_WIDGET,XmNtopWidget,label,XmNbottomAttachment,XmATTACH_FORM,
  	XmNleftAttachment,XmATTACH_FORM,XmNrightAttachment,XmATTACH_FORM,NULL);
    }
  else if(!strcmp("right",location)){
    label=XtVaCreateManagedWidget(label_string,xmLabelWidgetClass,form,
  	XmNlabelString,str,
  	XmNtopAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,
  	XmNrightAttachment,XmATTACH_FORM,NULL);
    text=XtVaCreateManagedWidget(text_string,xmTextWidgetClass,form,
        XmNcolumns,num_columns,
  	XmNtopAttachment,XmATTACH_WIDGET,XmNbottomAttachment,XmATTACH_FORM,
  	XmNleftAttachment,XmATTACH_FORM,XmNrightAttachment,XmATTACH_WIDGET,XmNrightWidget,label,NULL);
    }
  else if(!strcmp("bottom",location)){
    label=XtVaCreateManagedWidget(label_string,xmLabelWidgetClass,form,
  	XmNlabelString,str,
  	XmNbottomAttachment,XmATTACH_FORM,
  	XmNleftAttachment,XmATTACH_FORM,XmNrightAttachment,XmATTACH_FORM,NULL);
    text=XtVaCreateManagedWidget(text_string,xmTextWidgetClass,form,
        XmNcolumns,num_columns,
  	XmNtopAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_WIDGET,XmNbottomWidget,label,
  	XmNleftAttachment,XmATTACH_FORM,XmNrightAttachment,XmATTACH_FORM,NULL);
    }
  else{
    label=XtVaCreateManagedWidget(label_string,xmLabelWidgetClass,form,
  	XmNlabelString,str,
  	XmNtopAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,
  	XmNleftAttachment,XmATTACH_FORM,NULL);
    text=XtVaCreateManagedWidget(text_string,xmTextWidgetClass,form,
        XmNcolumns,num_columns,
  	XmNtopAttachment,XmATTACH_FORM,XmNbottomAttachment,XmATTACH_FORM,
  	XmNleftAttachment,XmATTACH_WIDGET,XmNleftWidget,label,XmNrightAttachment,
  	XmATTACH_FORM,NULL);
    }
  XtManageChild(form);
  XmStringFree(str);
  
return form;
}

char *GetTextEntryString(Widget text_entry)
{
char *value;
int wcnt;
Widget *pw;

  XtVaGetValues(text_entry,XmNchildren,(&pw),XmNnumChildren,(&wcnt),NULL);
  while(wcnt--){
    if(xmTextWidgetClass==XtClass((*pw)))
      break;
    else
      ++pw;
    }
  if(wcnt>=0)			/* Found a Text Widget */
    XtVaGetValues((*pw),XmNvalue,&value,NULL);
      
return value;
}

void SetTextEntryString(Widget text_entry,char *value)
{
int wcnt;
Widget *pw;

  XtVaGetValues(text_entry,XmNchildren,(&pw),XmNnumChildren,(&wcnt),NULL);
  while(wcnt--){
    if(xmTextWidgetClass==XtClass((*pw)))
      break;
    else
      ++pw;
    }
  if(wcnt>=0)			/* Found a Text Widget */
    XtVaSetValues((*pw),XmNvalue,value,NULL);
      
return;
}
