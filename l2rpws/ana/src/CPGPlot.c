#include <stdio.h>
#include <stdlib.h>

#include "cpgplot.h"		/* C Wrapper Functions */


#include "CTools.h"


/* Do File Logging for spec. y-slice display */

void DrawLabels(CPGLabel *p)
{
  if(!p)  return;

  cpgsci(p->color_index);
  cpgsch(p->char_height);

  if(p->side){		/* use relative positions */
    cpgmtxt(p->side,p->disp,p->coord,p->fjust,p->text);
    }
  else{			/* use absolute positions */
    cpgptxt(p->x,p->y,p->angle,p->fjust,p->text);
    }
    
return;
}

void DrawPoints(CPGLine *p)
{
  if(!p)  return;
  
  cpgsci(p->color_index);

  /* set the world coordinates */
  if(p->ChildsWhims)	cpgswin(p->xl,p->xr,p->yb,p->yt);		
  else			cpgswin(p->w_xl,p->w_xr,p->w_yb,p->w_yt);

  /* draw a line */
  if(p->line_style && p->length>1){	
    cpgsls(p->line_style);
    cpgline(p->length,p->x,p->y);
  }

  /* plot the points */
  if(p->plot_symbol){
    cpgpt(p->length,p->x,p->y,p->plot_symbol);
  }

return;
}

CPGPlotWindow *New_CPGPlotWindow(char *device,int x_div,int y_div)
{
CPGPlotWindow *p;

  if((p=Create_CPGPlotWindow())==NULL)  return NULL;
  
  p->device_name=sEquals(device);
  
  if(x_div<=1 && y_div<=1)
    p->sub_panels=p->x_panel=p->y_panel=0;
  else
    SubDivide_CPGPlotWindow(p,x_div,y_div); /* inherits parents device_name */

return p;
}

void SetDeviceContext_CPGPlotWindow(CPGPlotWindow *p,char *device,int id)
{

  while(p){
    p->device_name=device;
    p->device_id=id;
    p=NextListElement(p);
  }

return;
}

void Open_CPGPlotWindow(CPGPlotWindow *p)
{
int i=0;
CPGPlotWindow *pChild=p;

  if(!p)  return;

  if((p->device_id=cpgopen(p->device_name))==0){
    fprintf(stderr,"Unable to open %d=cpgopend(%s)",
            p->device_id,p->device_name);
    return;
  }

  if(p->sub_panels){
    cpgsubp(p->x_panel,p->y_panel);
    i=0;
    while((pChild=NextListElement(pChild))!=NULL){
      pChild->device_id=p->device_id;
    }
  }
  cpgslct(p->device_id); 

  if(p->vp_xl<0.0 || p->vp_xl>1.0)  p->vp_xl=0.1;
  if(p->vp_xr<0.0 || p->vp_xr>1.0)  p->vp_xr=0.9;
  if(p->vp_yb<0.0 || p->vp_yb>1.0)  p->vp_yb=0.1;
  if(p->vp_yt<0.0 || p->vp_yt>1.0)  p->vp_yt=0.9;

/* 020424 raj Need a better check for the vport parameters, for now hardwire */
  p->vp_xl=0.1; p->vp_xr=0.9; p->vp_yb=0.1; p->vp_yt=0.9;

  cpgsvp(p->vp_xl,p->vp_xr,p->vp_yb,p->vp_yt);	/* set the view port */

return;
}

void Close_CPGPlotWindow(CPGPlotWindow *p)
{
  cpgslct(p->device_id);
  cpgclos();
  
return;
}

/* "/xs" or *xmp_device_name(Widget widget);string is owned by the widget */
CPGPlotWindow *Create_CPGPlotWindow(void)
{
CPGPlotWindow *p;

	/* Potential Linked List Element, so use the canned routines */
  if((p=CreateListElement(CPGPlotWindow))==NULL){
    fprintf(stderr,"Unable to calloc(), CreatePGPlotWindow()\n");
    return NULL;
    }

return p;
}

int SubDivide_CPGPlotWindow(CPGPlotWindow *p,int x_div,int y_div)
{
int i,j;
CPGPlotWindow *pSubPanel;

  if(!p)  return 0;
  p->sub_panels=x_div*y_div;				/* I have sub panels */
  p->x_panel=x_div;  p->y_panel=y_div;			/* but I am not one */
  if(p->sub_panels<2 || p->sub_panels>CPG_MAX_SUBPANELS){
    p->sub_panels=0;     /* Not allowed to have subpanels */
    p->x_panel=p->y_panel=0;
    fprintf(stderr,"Subpanel Range Error, SubDivide_CPGPlotWindow(%p,%d,%d)\n",
            p,x_div,y_div);
    return 0;
    }
  
  for(i=1;i<=x_div;i++){
    for(j=1;j<=y_div;j++){
      if((pSubPanel=EqualsListElement(p))==NULL){	/* Inherit from parent, makes an exact copy with calloc(), but its not connected to the list */
        fprintf(stderr,"Unable to make sub panels, SubDivide_CPGPlotWindow(%d,%d)\n",x_div,y_div);
        p->sub_panels=(i-1)*x_div+j-1;            /* Available subpanels */
        p->x_panel=i;
        p->y_panel=j;
        return 0;
        }
      else{
      	pSubPanel->sub_panels=0;		/* I have no sub panels */
      	pSubPanel->x_panel=i;  			/* but I am one */
      	pSubPanel->y_panel=j;
      	pSubPanel->pAxis=NULL;
      	pSubPanel->pLabel=NULL;     /* Nix any inherited objects which are */
        pSubPanel->pLine=NULL;      /* exclusively owned by the window */
        AddListElement(p,pSubPanel);
        }
      }/* for rows.  Example rows=4 :	1 2 3 4 	*/		
    }/* for columns.         cols=5 :	5 6 7 8 	*/

return 1;
}

CPGPlotWindow* GetManagerPanel_CPGPlotWindow(CPGPlotWindow *p)
{
CPGPlotWindow *pManager;

  if(!p)  return NULL;
  if(p->sub_panels)  return NULL;    /* Already manager window */

  /* Managers are located before children in the list */
  pManager=p;
  while( (pManager=(CPGPlotWindow*)PreviousListElement(pManager)) )
    if(pManager->sub_panels)
      break;
  
return pManager;
}

CPGPlotWindow* GetSubPanel_CPGPlotWindow(CPGPlotWindow *p,int column,int row)
{
CPGPlotWindow *pSubpanel;

  if(!p)  return NULL;
  if(!p->sub_panels)    return NULL;  /* Must be a manager window */
  if(!column || !row)  return NULL;
  
  /* Children are locate after the manager in the list */
  pSubpanel=p;
  while( (pSubpanel=(CPGPlotWindow*)NextListElement(pSubpanel)) )
    if(pSubpanel->x_panel==column && pSubpanel->y_panel==row)
      break;
  
return pSubpanel;  
}


/* This is a dumb window update, it updates whatever its pointed at.
   For window placement use UpdateManagerWindow() */
int UpdateWindow_CPGPlotWindow(CPGPlotWindow *p)
{
CPGAxis *pAxis;
CPGLabel *pLabel;
CPGLine *pLine;

  if(!p)  return 0;
   
    	/* p->pLine may be the head of the list */   
  pLine=p->pLine;	
  while(pLine){
   	/* Renogitate with each pgplot objects here */
    if(pLine->ChildsWhims){	/* Objects perferred display window */
      pLine->w_xl=pLine->xl;  pLine->w_xr=pLine->xr;
      pLine->w_yb=pLine->yb;  pLine->w_yt=pLine->yt;
      }
    else{      			/* Inherit from the window manager */
      pLine->w_xl=p->w_xl;  pLine->w_xr=p->w_xr;
      pLine->w_yb=p->w_yb;  pLine->w_yt=p->w_yt;
      }
    if(pLine->pOnDraw)  pLine->pOnDraw(pLine);	/* display yourself */
    else		DrawPoints(pLine);	/* Default Line Drawer */
    pLine=(CPGLine*)NextListElement(pLine);
  }

    
    	/* p->label may be the head of the list */
  pLabel=p->pLabel;
  while(pLabel!=NULL){
    if(pLabel->pOnDraw)  pLabel->pOnDraw(pLabel);   /* display yourself */
    else                 DrawLabels(pLabel);        /* Default Label Drawer */
    pLabel=(CPGLabel*)NextListElement(pLabel);
  }
	
	/* Slap axis labels on the plot */  
    cpgsci(1);  cpgsch(1);  cpgsls(1);
  pAxis=p->pAxis;
  while(pAxis){
    cpgsci(pAxis->color_index);
    cpgsch(pAxis->char_height);
    if(strlen(pAxis->xopt) && strlen(pAxis->yopt))
      cpgtbox(pAxis->xopt,pAxis->xtick,pAxis->nxsub,pAxis->yopt,pAxis->ytick,pAxis->nysub);
    pAxis=(CPGAxis*)NextListElement(pAxis);
  }


return 1;
}

/* This fucntion only allows one manager and its children windows */
void UpdateManagerWindow_CPGPlotWindow(CPGPlotWindow *p)
{
CPGPlotWindow *pSubPanel;
/* 020424 raj */
int i;

  if(!p)  return;
  
 
  if(p->sub_panels){	/* manager of windows */

	/* Renogitate with all subpanels here */
	/* - are they allowed to configure themselves */

    /* p should allways be the head of the list */
    pSubPanel=(CPGPlotWindow*)HeadListElement(p);
    i=0;
    while((pSubPanel=(CPGPlotWindow*)NextListElement(pSubPanel))){
      UpdateManagerWindow_CPGPlotWindow(pSubPanel);
    }
    return;		/* all children displayed */
  }
  else if(!p->sub_panels && p->x_panel && p->y_panel){	/* subpanel window */
    cpgslct(p->device_id);		 /* select open device */
    cpgpanl(p->x_panel,p->y_panel);
  }
  else if(!p->sub_panels && !p->x_panel && !p->y_panel){/* regular window */
    cpgslct(p->device_id);		 /* select open device */
  }
  else{
    fprintf(stderr,"Someone forgot to manage one of the orphan subpanels\n");
    return;	/* hope we never make it here */
  }
  cpgeras();	/* Erase all graphics */
  cpgetxt();	/* Erase all Text */
  UpdateWindow_CPGPlotWindow(p);      /* this does the actual display of things */
  
return;
}

