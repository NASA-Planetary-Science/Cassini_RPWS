#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "Xm/Xm.h"

#include <Cext.h>

#ifndef _CTools_h
#define _CTools_h




#include "CToolsP.h"			/* Private Access Header File, 
					   do NOT screw with it */

#define CPG_MAX_SUBPANELS         24     /* A Window May Have 24 Subpanels */

#define STOP_WATCH_START	0x02
#define STOP_WATCH_STOP		0x03

double StopWatch(unsigned long command);


/* String.c 
char *equals_string(char *source);
char *sEquals(char *source);
*/


/* CLinkedList.c */	
/* Member Functions of CLinkedList_raj 
type *CreateListElement(type);		// as in sizeof(type)
//void InitListElement(type *p);	obsolete	
int AddListElement(type *pList,type *pNewGuy);	
int RemoveListElement(type *pGoner);		
type *HeadListElement(type *pList);		
type *TailListElement(type *pList);		
type *NextListElement(type *p);	
type *PreviousListElement(type *p);
type *DeleteListElement(type *p);
type *EqualsListElement(type *p);	
*/

/*
Example:
	typedef struct{
	  ... stuff ...
	  CLinkedList_raj clinkedlist_raj;	 // Exactly as seen 
	  ... stuff ...
	  }MyStruct;

MyStruct *apple,*orange,*cherry,*p;

  apple=CreateListElement(MyStruct);   InitListElement(apple);
  orange=CreateListElement(MyStruct);  InitListElement(orange);
  cherry=CreateListElement(MyStruct);  InitListElement(cherry);
  
  AddListElement(apple,orange);	// list order: apple,orange
  AddListElement(apple,cherry);	// list order: apple,orange,cherry
//  AddListElement(orange,apple);	// Error: apple is already on the list
  
  p=(MyStruct*)NextListElement(orange);	// p=cherry
  
  DeleteListElement(orange);	// list order: apple,cherry

*/
typedef struct{		/* pgmtxt/pgptxt */
  char *text;		/* character string made by malloc */
  float fjust;		/* left=0, center=0.5, right=1.0 */
  char *side;		/* pgmtxt: "B","L","T","R" */
  float disp;		/* pgmtxt: no. char, + outside, - inside */ 
  float coord;		/* pgmtxt: location as a fraction of length */
  float x,y;		/* pgptxt: real world x,y coord */
  float angle;		/* pgptxt: angle in degrees */
  float char_height;		/* text size */
  int color_index;		/* text color */
  void (*pOnDraw)();		/* Generic Display Function */
  CLinkedList_raj clinkedlist_raj;	/* Good Measure */
  void *pUserData;
  }CPGLabel;
  
typedef struct{
  int length;
  float *x,*y;
  int color_index,line_style,plot_symbol;
  float w_xl,w_xr,w_yb,w_yt;		/* Window coordinates, negotiate with window manager */
  void (*pOnDraw)();			/* Generic draw function: pOnDraw(CPGLine *line); */
  CLinkedList_raj clinkedlist_raj;	/* Good Measure */
  int ChildsWhims;			/* Allow object to set up window */			
  float xl,xr,yb,yt;			/* Object's perferred window coordinates */
  void *pUserData;		/* user date here, not used by the frame work */
  }CPGLine;
  
typedef struct{			/* pgtbox */
  char *xopt,*yopt;	/* should not be 16 options */
  int nxsub,nysub;
  float xtick,ytick;
  float char_height;
  int color_index;
  void *pUserData;
  CLinkedList_raj clinkedlist_raj;
  }CPGAxis;
  
typedef struct{
  Widget widget;
  XtPointer *call_data;
  void *user_data;		/* XmCreatePgplot( ) or XmCreateScrolledPgplot( ) */
  char *device_name;		/* *xmp_device_name(Widget widget); or "/xs" */
  int device_id;		/* int xmp_device_id(Widget widget); */
  int sub_panels;		/* 0 implies no subpanels; otherwise yes */
  int x_panel,y_panel;		/* subpanels of the window */
  void *pUserData;		/* Put what ever you like here, ie your data structures */
  CLinkedList_raj clinkedlist_raj;	/* Plot window subpanels, */
  CPGAxis *pAxis;				/* axis attributes */
  CPGLabel *pLabel;			/* Window labels, may be linked list at later date */
  float w_xl,w_xr,w_yb,w_yt;		/* Window Coordinates */
  float vp_xl,vp_xr,vp_yb,vp_yt;	/* View Port */
  float wr_xl,wr_xr,wr_yb,wr_yt;	/* Window Range, for scroll bars */
  CPGLine *pLine;			/* Data to be displayed, may be linked list */
  }CPGPlotWindow;



CPGPlotWindow *Create_CPGPlotWindow(void);
int SubDivide_CPGPlotWindow(CPGPlotWindow *pwin,int xsub,int ysub);
CPGPlotWindow* GetManagerPanel_CPGPlotWindow(CPGPlotWindow *p);
CPGPlotWindow* GetSubPanel_CPGPlotWindow(CPGPlotWindow *p,int column,int row);
void UpdateManagerWindow_CPGPlotWindow(CPGPlotWindow *p);
int UpdateWindow_CPGPlotWindow(CPGPlotWindow *p);
void DisplayRefresh_CPGPlotWindow(CPGPlotWindow *p);
void Destroy_CPGPlotWindow(CPGPlotWindow *pwin);
#define IsManagerPanel_CPGPlotWindow(p)	( (p->sub_panels>0)?1:0 )
#define IsSubPanel_CPGPlotWindow(p)	( (p->sub_panels==0)?1:0 )

	/* Window Allocation and Copying Function, the new window
	   will not be double parked in the linked list. */

void Open_CPGPlotWindow(CPGPlotWindow *p);
CPGPlotWindow* New_CPGPlotWindow(char *device,int x_div,int y_div);
void Close_CPGPlotWindow(CPGPlotWindow *p);
 void SetDeviceContext_CPGPlotWindow(CPGPlotWindow *p,char *device,int id);

void DrawPoints(CPGLine *line);		/* Generic Drawing Function */
void DisplayLabels(CPGLabel *label);	/* Generic Drawing Function */

#endif
