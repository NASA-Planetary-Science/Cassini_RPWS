
Widget FocusInPGPlotWidget;

void ViewScrollBars(Widget w,XtPointer client_data,XtPointer call_data);
void ViewFitToScreen(Widget w,XtPointer client_data,XtPointer call_data);
void ViewZoomIn(Widget w,XtPointer client_data,XtPointer call_data);
void ViewZoomOut(Widget w,XtPointer client_data,XtPointer call_data);


/*
int xmp_arm_cursor(Widget widget, int mode, float xref, float yref,
		   XtCallbackProc callback, void *client_data);
int xmp_disarm_cursor(Widget widget);
XMP_CROSS_CURSOR
*/
/*
  XtAddEventHandler(toplevel,StructureNotifyMask,False,DisplayResize,(XtPointer)NULL);
#define CURSOR_EVENT_MASK ((EventMask)(KeyPressMask | ButtonPressMask | \
				       EnterWindowMask | LeaveWindowMask | \
				       PointerMotionMask))
  xmp_arm_cursor(PltWnd,XMP_CROSS_CURSOR,0.0,0.0,Hack,NULL);
*/




MenuItem  view_menu[]={
	{"Scroll Bars",  &xmToggleButtonWidgetClass,'S',NULL,NULL,ViewScrollBars, (XtPointer)&FocusInPGPlotWidget,NULL},
	{"Fit to Screen",&xmPushButtonWidgetClass,  'F',NULL,NULL,ViewFitToScreen,(XtPointer)&FocusInPGPlotWidget,NULL},
	{"Zoom In",      &xmPushButtonWidgetClass,  'I',NULL,NULL,ViewZoomIn,     (XtPointer)&FocusInPGPlotWidget,NULL},
	{"Zoom Out",     &xmPushButtonWidgetClass,  'O',NULL,NULL,ViewZoomOut,    (XtPointer)&FocusInPGPlotWidget,NULL},
	NULL};


void ViewScrollBars(Widget w,XtPointer client_data,XtPointer call_data)
{
Boolean set;
Widget wPGPlot=*((Widget*)(client_data));
XtPointer xtptmp;
CPGPlotWindow *pWnd;

  XtVaGetValues(w,XmNset,&set,NULL);		/* Toggle button state */
  while(w && !XtIsWMShell(w))  w=XtParent(w);	/* Get Window Manager */
  
	/* The pgplot widget has the pgplot winodow display structure */
  XtVaGetValues(wPGPlot,XmNuserData,&xtptmp,NULL);
  pWnd=(CPGPlotWindow*)xtptmp;

  XtVaSetValues(w,XmNscrollBarDisplayPolicy,XmAS_NEEDED,XmNscrollingPolicy,XmAUTOMATIC,NULL);
  
fprintf(stderr,"FocusInWidget = %s :: top =%s\n",XtName(wPGPlot), XtName(w));

return;
}

void ViewFitToScreen(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget wPGPlot=*((Widget*)(client_data));
XtPointer xtptmp;
CPGPlotWindow *pWnd;

	/* The pgplot widget has the pgplot winodow display structure */
  XtVaGetValues(wPGPlot,XmNuserData,&xtptmp,NULL);
  pWnd=(CPGPlotWindow*)xtptmp;
  
fprintf(stderr,"FocusInWidget = %s\n",XtName(wPGPlot));

return;
}

void ViewZoomIn(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget wPGPlot=*((Widget*)(client_data));
XtPointer xtptmp;
CPGPlotWindow *pWnd;

	/* The pgplot widget has the pgplot winodow display structure */
  XtVaGetValues(wPGPlot,XmNuserData,&xtptmp,NULL);
  pWnd=(CPGPlotWindow*)xtptmp;
  
fprintf(stderr,"FocusInWidget = %s\n",XtName(wPGPlot));

return;
}

void ViewZoomOut(Widget w,XtPointer client_data,XtPointer call_data)
{
Widget wPGPlot=*((Widget*)(client_data));
XtPointer xtptmp;
CPGPlotWindow *pWnd;

	/* The pgplot widget has the pgplot winodow display structure */
  XtVaGetValues(wPGPlot,XmNuserData,&xtptmp,NULL);
  pWnd=(CPGPlotWindow*)xtptmp;
  
fprintf(stderr,"FocusInWidget = %s\n",XtName(wPGPlot));

return;
}
