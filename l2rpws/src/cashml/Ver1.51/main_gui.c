
/* This is a temilate that I use to start all Motif based programs.*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <string.h>
#include <math.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/MainW.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/SashP.h>

#include <Xm/CascadeB.h>
#include <Xm/Command.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <Xm/MwmUtil.h>
#include <Xm/PushB.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>

#include <Xm/SelectioB.h>
#include "XmPgplot.h"
#include "cpgplot.h"

#include "Cext.h"
#include "CTools.h"

#include "CasTlmDef.h"

/*
#include "CasTime.h"
#include "CasHFR.h"
*/
#include "CasTlmFrontEnd.h"
#include "CasTelemetry.h"


char GUI_VERSION[] = "cashml(), V1.5";


typedef struct firewall_tag
{
  /*
   * Stuff to handle the incomming telemetry 
   */
  Bool bNewTlmFile;
  char sFilename[512];
  CasTlmFrontEnd *pTlmSrc;
  CasTelemetry *pLFDR[8], *pMFR[5], *pHFR[5];   /* Pointer to Data Packets */
  ULONG nLFDRmode, nMFRmode, nHFRmode;  /* Acceptable modes and antennas */

  /*
   * Stuff for the window 
   */
  Bool bXlog, bYlog;
  float fXleft, fXright, fYtop, fYbottom;
  CPGPlotWindow *pMainWindow;

  /*
   * Stuff for the Event Loop 
   */
  char *sLogFile;
  FILE *hLogFile;
} FireWall;

typedef struct packet_mode_tag
{
  ULONG nType, nMode, nEvtSec;
} PacketId;


typedef struct _menu_item
{
  char *label;
  WidgetClass *widget_class;

/*  XtPointer user_data;*/
  char mnemonic;
  char *accelerator;
  char *accel_text;
  void (*callback) ();
  XtPointer callback_data;
  struct _menu_item *subitems;
} MenuItem;



/* This is the actual pgplot window */
Widget wPgPlotWindow;

/* This is the program context */
FireWall FWall;



Boolean Work (XtPointer client_data);
Bool LoadWindow (CPGPlotWindow * pWin);
Bool LoadWindowHeader (void);
void LogLinTransform (CPGLine * pLine);
void OnDraw_LFDR (CPGLine * pLine);
void OnDraw_MFR (CPGLine * pLine);
void OnDraw_HFR (CPGLine * pLine);

/* Recursive call doen't seem to work */
Widget BuildMenu (Widget parent, int menu_type, char *menu_title,
                  char menu_mnemonic, Boolean tear_off, MenuItem * items);

void FileOpen (Widget w, XtPointer client_data, XtPointer call_data);
void FileCallback_Ok (Widget w, XtPointer client_data, XtPointer call_data);
void FileCallback_Cancel (Widget w, XtPointer client_data,
                          XtPointer call_data);
void PostPlayBackDialog (Widget w, XtPointer client_data,
                         XtPointer call_data);
void PlayBack (Widget w, XtPointer client_data, XtPointer call_data);
void PrintWindow (Widget w, XtPointer client_data, XtPointer call_data);
void LogData (Widget w, XtPointer client_data, XtPointer call_data);
void LogDataOnOff (Widget w, XtPointer client_data, XtPointer call_data);
void LogDataFilename (Widget w, XtPointer client_data, XtPointer call_data);
void LogDataFilename_Ok (Widget w, XtPointer client_data,
                         XtPointer call_data);
void OutputLineData (FILE * hHandle, CPGLine * pLine);

void AntSel_LFDR (Widget w, XtPointer client_data, XtPointer call_data);
void AntSel_MFR (Widget w, XtPointer client_data, XtPointer call_data);
void AntSel_HFR (Widget w, XtPointer client_data, XtPointer call_data);
void AxisMinMax (Widget w, XtPointer client_data, XtPointer call_data);
void AxisMinMax_GetValue (Widget w, XtPointer client_data,
                          XtPointer call_data);
void AxisLogLin (Widget w, XtPointer client_data, XtPointer call_data);

void PlotResize (Widget w, XtPointer client_data, XEvent * event,
                 Boolean * reg);
void PlotReHash (void);


void ToggleSensitivity (Widget w, XtPointer client_data, XtPointer call_data);
void TurnOffSashTraversal (Widget paned);
void PopupErrorDialog (Widget w, XtPointer client_data, XtPointer call_data);
void Exit (Widget w, XtPointer client_data, XtPointer call_data);




void DumpPacketId (PacketId * pPktId);


MenuItem log_menu[] = {
  {"Dump", &xmPushButtonWidgetClass, 'D', NULL, NULL, LogData, NULL, NULL},
  {"Seperator", &xmSeparatorWidgetClass, 0, NULL, NULL, NULL, NULL, NULL},
  {"Log OnOff", &xmToggleButtonWidgetClass, 'L', NULL, NULL, LogDataOnOff,
   NULL, NULL},
  {"Filename", &xmPushButtonWidgetClass, 'F', NULL, NULL, LogDataFilename,
   NULL, NULL},
  NULL
};


MenuItem file_menu[] = {
  {"Open", &xmPushButtonWidgetClass, 'O', NULL, NULL, FileOpen, 0, NULL},
  {"Controls", &xmPushButtonWidgetClass, 'C', NULL, NULL, PostPlayBackDialog,
   NULL, NULL},
  {"Seperator", &xmSeparatorWidgetClass, 0, NULL, NULL, 0, 0, NULL},
  {"Print", &xmPushButtonWidgetClass, 'P', NULL, NULL, PrintWindow, NULL,
   NULL},

/*	{"Data Logging", &xmPushButtonWidgetClass,'L',NULL,NULL,LogData,NULL,NULL},*/
  {"Seperator", &xmSeparatorWidgetClass, 0, NULL, NULL, 0, 0, NULL},
  {"Exit", &xmPushButtonWidgetClass, 'x', NULL, NULL, Exit, 0, NULL},
  NULL
};

MenuItem lfdr_menu[] = {
  {"Ex", &xmToggleButtonWidgetClass, 'x', NULL, NULL, AntSel_LFDR,
   (XtPointer) CasAntEx, NULL},
  {"Ez", &xmToggleButtonWidgetClass, 'E', NULL, NULL, AntSel_LFDR,
   (XtPointer) CasAntEz, NULL},
  {"Bx", &xmToggleButtonWidgetClass, 'B', NULL, NULL, AntSel_LFDR,
   (XtPointer) CasAntBx, NULL},
  {"By", &xmToggleButtonWidgetClass, 'y', NULL, NULL, AntSel_LFDR,
   (XtPointer) CasAntBy, NULL},
  {"Bz", &xmToggleButtonWidgetClass, 'z', NULL, NULL, AntSel_LFDR,
   (XtPointer) CasAntBz, NULL},
  {"LMR+", &xmToggleButtonWidgetClass, '+', NULL, NULL, AntSel_LFDR,
   (XtPointer) CasAntLMRp, NULL},
  {"LMR-", &xmToggleButtonWidgetClass, '-', NULL, NULL, AntSel_LFDR,
   (XtPointer) CasAntLMRm, NULL},
  {"LPs", &xmToggleButtonWidgetClass, 's', NULL, NULL, AntSel_LFDR,
   (XtPointer) CasAntLPs, NULL},
  NULL
};

MenuItem mfr_menu[] = {
  {"Ex", &xmToggleButtonWidgetClass, 'x', NULL, NULL, AntSel_MFR,
   (XtPointer) CasAntEx, NULL},
  {"Ez", &xmToggleButtonWidgetClass, 'E', NULL, NULL, AntSel_MFR,
   (XtPointer) CasAntEz, NULL},
  {"Bx", &xmToggleButtonWidgetClass, 'B', NULL, NULL, AntSel_MFR,
   (XtPointer) CasAntBx, NULL},
  {"Bz", &xmToggleButtonWidgetClass, 'z', NULL, NULL, AntSel_MFR,
   (XtPointer) CasAntBz, NULL},
  NULL
};

MenuItem hfr_menu[] = {
  {"Ex+", &xmToggleButtonWidgetClass, '+', NULL, NULL, AntSel_HFR,
   (XtPointer) CasAntExp, NULL},
  {"Ex-", &xmToggleButtonWidgetClass, '-', NULL, NULL, AntSel_HFR,
   (XtPointer) CasAntExm, NULL},
  {"Ex+/-", &xmToggleButtonWidgetClass, 'x', NULL, NULL, AntSel_HFR,
   (XtPointer) CasAntEx, NULL},
  {"Ez", &xmToggleButtonWidgetClass, 'z', NULL, NULL, AntSel_HFR,
   (XtPointer) CasAntEz, NULL},
  NULL
};

MenuItem antenna_menu[] = {
  {"lfdr", &xmPushButtonWidgetClass, 'l', NULL, NULL, NULL, NULL, lfdr_menu},
  {"mfr", &xmPushButtonWidgetClass, 'm', NULL, NULL, NULL, NULL, mfr_menu},
  {"hfr", &xmPushButtonWidgetClass, 'h', NULL, NULL, NULL, NULL, hfr_menu},
  NULL
};

MenuItem axis_menu[] = {
  {"Xmin", &xmPushButtonWidgetClass, 'i', NULL, NULL, AxisMinMax,
   (XtPointer) 0, NULL},
  {"Xmax", &xmPushButtonWidgetClass, 'a', NULL, NULL, AxisMinMax,
   (XtPointer) 1, NULL},
  {"Xlog", &xmToggleButtonWidgetClass, 'o', NULL, NULL, AxisLogLin,
   (XtPointer) & FWall.bXlog, NULL},
  {"Seperator", &xmSeparatorWidgetClass, 0, NULL, NULL, 0, 0, NULL},
  {"Ymin", &xmPushButtonWidgetClass, 'n', NULL, NULL, AxisMinMax,
   (XtPointer) 2, NULL},
  {"Ymax", &xmPushButtonWidgetClass, 'x', NULL, NULL, AxisMinMax,
   (XtPointer) 3, NULL},
  {"Ylog", &xmToggleButtonWidgetClass, 'g', NULL, NULL, AxisLogLin,
   (XtPointer) & FWall.bYlog, NULL},
  NULL
};


int main (int argc, char *argv[])
{
  char **pArgv = argv;
  int nArgc = argc;
  int n;
  Arg args[20];
  Atom WM_DELETE_WINDOW;
  Widget wTopLevel, wMainWindow, wMenuBar, wPane, wTmp;
  XtAppContext AppContext;
  XtWorkProcId WorkId;
  Widget wLFDR, wMFR, wHFR, wAxis;

  XtSetLanguageProc (NULL, NULL, NULL);
  wTopLevel =
    XtVaAppInitialize (&AppContext, "toplevel", NULL, 0, &argc, argv, NULL,
                       NULL);

/*
  while(--argc){
    ++argv;
  }
*/

  /*
   * Init the program interface 
   */
  FWall.bNewTlmFile = False;
  FWall.sFilename[0] = '\0';
  FWall.pMainWindow = NULL;
  FWall.bXlog = True;
  FWall.bYlog = True;
  FWall.fXleft = 0.1;
  FWall.fYtop = 1.0E-3;
  FWall.fXright = 20.0E6;
  FWall.fYbottom = 1.0E-19;
  FWall.sLogFile = (char *) XtNewString ("cashml.log"); /* Free with XtFree() */
  FWall.hLogFile = NULL;

  /*
   * Construct Program Context 
   */
  if ((FWall.pTlmSrc = CasTlmFrontEnd_Constructor (NULL)) == NULL)
    fprintf (stderr, "Failed in FWall.pTlmSrc\n");;
  FWall.pTlmSrc->RecFilter.nType = CasSciLFDR | CasSciMFR | CasSciHFR;

  for (n = 0; n < 8; n++)
    FWall.pLFDR[n] = NULL;
  for (n = 0; n < 5; n++) {
    FWall.pMFR[n] = NULL;
    FWall.pHFR[n] = NULL;
  }
  FWall.nLFDRmode = CasLFDR_BandsAll | CasAntEx | CasAntBx;
  FWall.nMFRmode = CasMFR_BandsAll | CasAntEx | CasAntBx;
  FWall.nHFRmode = CasHFR_Analysis | CasHFR_BandsAll | CasAntExAny | CasAntEz;



  /*
   * Read in Instrument Calibration Files 
   */
  fprintf (stderr, "Reading HFR Calibration Files\n");
  CasHFR_ReadCalibrationFiles ("/usr/cassini/cal/hfr/A123_V0.DAT",
                               "/usr/cassini/cal/hfr/A1HF1.DAT",
                               "/usr/cassini/cal/hfr/A1HF2.DAT",
                               "/usr/cassini/cal/hfr/DB_CAL.DAT");

  fprintf (stderr, "Reading MFR Calibration Files\n");
  if (CasMFR_ReadCalibrationFiles (0, NULL, NULL) == False) {
    fprintf (stderr, "Error reading rpws casssini mfr calibration files.\n"
             "Set the enviroment variable CAS_DIR_CAL to the directory\n"
             "where these files reside.\n");
    exit (127);
  }

  CasTlmFrontEnd_CommandLineArgs (FWall.pTlmSrc, &nArgc, &pArgv);

/*
  XtSetLanguageProc(NULL,NULL,NULL);
  wTopLevel=XtVaAppInitialize(&AppContext,"toplevel",NULL,0,&argc,argv,NULL,NULL);
*/

  wMainWindow =
    XtVaCreateWidget ("mainwindow", xmMainWindowWidgetClass, wTopLevel,
                      XmNcommandWindowLocation, XmCOMMAND_BELOW_WORKSPACE,
                      NULL);

  wMenuBar = XmCreateMenuBar (wMainWindow, "MenuBar", NULL, 0);
  wTmp = BuildMenu (wMenuBar, XmMENU_PULLDOWN, "File", 'F', True, file_menu);
  XtManageChild (wTmp);
  wTmp = BuildMenu (wMenuBar, XmMENU_PULLDOWN, "Log", 'o', True, log_menu);
  XtManageChild (wTmp);
  wLFDR = BuildMenu (wMenuBar, XmMENU_PULLDOWN, "LFDR", 'L', True, lfdr_menu);
  XtManageChild (wLFDR);
  wMFR = BuildMenu (wMenuBar, XmMENU_PULLDOWN, "MFR", 'M', True, mfr_menu);
  XtManageChild (wMFR);
  wHFR = BuildMenu (wMenuBar, XmMENU_PULLDOWN, "HFR", 'H', True, hfr_menu);
  XtManageChild (wHFR);
  wAxis = BuildMenu (wMenuBar, XmMENU_PULLDOWN, "Axis", 'A', True, axis_menu);
  XtManageChild (wAxis);
  XtManageChild (wMenuBar);

  wPane =
    XtVaCreateWidget ("PannedWindow", xmPanedWindowWidgetClass, wMainWindow,
                      NULL);
  n = 0;
  XtSetArg (args[n], XmNheight, 768);
  n++;
  XtSetArg (args[n], XmNwidth, 1024);
  n++;
  wPgPlotWindow = XmCreateScrolledPgplot (wPane, "pgplot", args, n);
  XtManageChild (wPgPlotWindow);
  XtManageChild (wPane);


/* Applies to a scrolled text window
n=0;
XtSetArg(args[n],XmNrows,20);				n++;
XtSetArg(args[n],XmNcolumns,40);			n++;
XtSetArg(args[n],XmNeditable,False);			n++;
XtSetArg(args[n],XmNeditMode,XmMULTI_LINE_EDIT);	n++;
XtSetArg(args[n],XmNcursorPositionVisible,False);	n++;
wTmp=XmCreateScrolledText(wPane,"scrolledtext",args,n);
XtManageChild(wTmp);
*/



  XtAddEventHandler (wTopLevel, StructureNotifyMask, False, PlotResize,
                     (XtPointer) NULL);
  WorkId = XtAppAddWorkProc (AppContext, Work, (XtPointer) AppContext);

  XtVaSetValues (wMainWindow, XmNmenuBar, wMenuBar, XmNworkWindow, wPane,
                 /*XmNcommandWindow,Command, */ NULL);
  WM_DELETE_WINDOW =
    XmInternAtom (XtDisplay (wTopLevel), "WM_DELETE_WINDOW", False);
  XmAddWMProtocolCallback (wTopLevel, WM_DELETE_WINDOW, Exit, NULL);
  XtManageChild (wMainWindow);
  XtRealizeWidget (wTopLevel);

  /*
   * Set the Buttons in the menu to the correct state.  Is a callback called? 
   */
  {
    Boolean state;
    int i, wcnt;
    ULONG nAntenna;
    Widget *pw, wTmp;
    XtPointer nTmp;

    /*
     * LFDR Menu 
     */
    XtVaGetValues (wLFDR, XmNsubMenuId, &wTmp, NULL);
    XtVaGetValues (wTmp, XmNchildren, &pw, XmNnumChildren, &wcnt, NULL);
    for (i = 0; i < wcnt; i++) {
      if (!strcmp ("Ex", XtName (pw[i])))
        nAntenna = CasAntEx;
      else if (!strcmp ("Ez", XtName (pw[i])))
        nAntenna = CasAntEz;
      else if (!strcmp ("Bx", XtName (pw[i])))
        nAntenna = CasAntBx;
      else if (!strcmp ("By", XtName (pw[i])))
        nAntenna = CasAntBy;
      else if (!strcmp ("Bz", XtName (pw[i])))
        nAntenna = CasAntBz;
      else if (!strcmp ("LMR+", XtName (pw[i])))
        nAntenna = CasAntLMRp;
      else if (!strcmp ("LMR-", XtName (pw[i])))
        nAntenna = CasAntLMRm;
      else if (!strcmp ("LPs", XtName (pw[i])))
        nAntenna = CasAntLPs;
      if (nAntenna & FWall.nLFDRmode)
        XmToggleButtonSetState (pw[i], True, False);
    }

    /*
     * MFR Menu 
     */
    XtVaGetValues (wMFR, XmNsubMenuId, &wTmp, NULL);
    XtVaGetValues (wTmp, XmNchildren, &pw, XmNnumChildren, &wcnt, NULL);
    for (i = 0; i < wcnt; i++) {
      if (!strcmp ("Ex", XtName (pw[i])))
        nAntenna = CasAntEx;
      else if (!strcmp ("Ez", XtName (pw[i])))
        nAntenna = CasAntEz;
      else if (!strcmp ("Bx", XtName (pw[i])))
        nAntenna = CasAntBx;
      else if (!strcmp ("Bz", XtName (pw[i])))
        nAntenna = CasAntBz;
      if (nAntenna & FWall.nMFRmode)
        XmToggleButtonSetState (pw[i], True, False);
    }

    /*
     * HFR Menu 
     */
    XtVaGetValues (wHFR, XmNsubMenuId, &wTmp, NULL);
    XtVaGetValues (wTmp, XmNchildren, &pw, XmNnumChildren, &wcnt, NULL);
    for (i = 0; i < wcnt; i++) {
      if (!strcmp ("Ex+", XtName (pw[i])))
        nAntenna = CasAntExp;
      else if (!strcmp ("Ex-", XtName (pw[i])))
        nAntenna = CasAntExm;
      else if (!strcmp ("Ex+/-", XtName (pw[i])))
        nAntenna = CasAntEx;
      else if (!strcmp ("Ez", XtName (pw[i])))
        nAntenna = CasAntEz;
      if (nAntenna & FWall.nHFRmode)
        XmToggleButtonSetState (pw[i], True, False);
    }
    /*
     * Axis Toggle Buttons 
     */
    XtVaGetValues (wAxis, XmNsubMenuId, &wTmp, NULL);
    XtVaGetValues (wTmp, XmNchildren, &pw, XmNnumChildren, &wcnt, NULL);
    for (i = 0; i < wcnt; i++) {
      state = False;
      if (!strcmp ("Xlog", XtName (pw[i])))
        state = FWall.bXlog;
      else if (!strcmp ("Ylog", XtName (pw[i])))
        state = FWall.bYlog;
      if (state == True)
        XmToggleButtonSetState (pw[i], True, False);
    }

/* 
 XtCallActionProc(pw[0],"ArmAndActivate",,NULL,0);
  XmToggleButtonSetState(pw[i],state,False); 
*/

  }


  fprintf (stderr, "Before app loop\n");

  XtAppMainLoop (AppContext);

  return 1;
}

/* This function runs on its own thread of execution.
  It is registered as an 'on idle' procedure, meaning
  it is only executed when there is no pending X-events.
*/
Boolean Work (XtPointer client_data)
{
  static Bool bInit = False;
  static int nTimes;

  Bool bUpdateWindow = False;
  CasTelemetry *pPkt, **pPktStorage;
  CPGLabel *pLabel;

  float nXdata[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  float nYdata[] = { 1, 4, 9, 16, 25, 36, 49, 64, 81, 100 };
  int nLength = XtNumber (nXdata);

  if (bInit == False) {
    bInit = True;
    FWall.pMainWindow =
      New_CPGPlotWindow (xmp_device_name (wPgPlotWindow), 1, 1);

    if (FWall.bXlog == True) {
      FWall.pMainWindow->w_xl =
        FWall.fXleft <= 0.0 ? log10 (0.1) : log10 (FWall.fXleft);
      FWall.pMainWindow->w_xr =
        FWall.fXright <= 0.0 ? log10 (16.1E6) : log10 (FWall.fXright);
    } else {
      FWall.pMainWindow->w_xl = FWall.fXleft;   /*  0.1  Hz */
      FWall.pMainWindow->w_xr = FWall.fXright;  /* 16.0 MHz */
    }
    if (FWall.bYlog == True) {
      FWall.pMainWindow->w_yt =
        FWall.fYtop <= 0.0 ? log10 (1.0E-6) : log10 (FWall.fYtop);
      FWall.pMainWindow->w_yb =
        FWall.fYbottom <= 0.0 ? log10 (1.0E-19) : log10 (FWall.fYbottom);
    } else {
      FWall.pMainWindow->w_yt = FWall.fYtop;    /* 1.0E-6  V^2/m^2/Hz */
      FWall.pMainWindow->w_yb = FWall.fYbottom; /* 1.0E-19 V^2/m^2/Hz */
    }
    FWall.pMainWindow->vp_xl = 0.1;
    FWall.pMainWindow->vp_xr = 0.9;
    FWall.pMainWindow->vp_yb = 0.1;
    FWall.pMainWindow->vp_yt = 0.9;

    FWall.pMainWindow->pAxis = (CPGAxis *) CreateListElement (CPGAxis);
    if (FWall.bXlog == True)
      FWall.pMainWindow->pAxis->xopt = sEquals ("BCLNSTV2");
    else
      FWall.pMainWindow->pAxis->xopt = sEquals ("BCNSTV2");
    if (FWall.bYlog == True)
      FWall.pMainWindow->pAxis->yopt = sEquals ("BCLNSTV2");
    else
      FWall.pMainWindow->pAxis->yopt = sEquals ("BCNSTV2");
    FWall.pMainWindow->pAxis->char_height = 1.0;
    FWall.pMainWindow->pAxis->color_index = 1;

    pLabel = (CPGLabel *) CreateListElement (CPGLabel);
    pLabel->text = sEquals ("\\fiFrequency \\frHz");
    pLabel->fjust = 0.5;                /* Centered */
    pLabel->side = sEquals ("B");
    pLabel->disp = 3.0;
    pLabel->coord = 0.5;
    pLabel->char_height = 1.0;
    pLabel->color_index = 1;
    FWall.pMainWindow->pLabel = pLabel;

    pLabel = (CPGLabel *) CreateListElement (CPGLabel);
    pLabel->text = sEquals ("\\fiElectric & Magnetic Field Spectral Density: "
                            "\\frV\\u2\\dm\\u-2\\dHz\\u-1\\d & nT\\u2\\dHz\\u-1\\d");
    pLabel->fjust = 0.5;                /* Centered */
    pLabel->side = sEquals ("L");
    pLabel->disp = 4.0;
    pLabel->coord = 0.5;
    pLabel->char_height = 1.0;
    pLabel->color_index = 1;
    AddListElement (FWall.pMainWindow->pLabel, pLabel);

    LoadWindowHeader ();

    Open_CPGPlotWindow (FWall.pMainWindow);
  }

  /*
   * Check to see if files have changed, possible memory leak for each filename? 
   */
  if (FWall.bNewTlmFile == True) {
    FWall.bNewTlmFile = False;          /* Acknowledge file change */
    RecordFile_Close (&(FWall.pTlmSrc->InStream));      /* Close Old File */
    /*
     * Assign File to Open 
     */
    FWall.pTlmSrc->sNewInFile[FWall.pTlmSrc->nInFileIndex] =
      sEquals (FWall.sFilename);
  }

  /*
   * Allocate an store current data 
   */
  if ((pPkt = CasTlmFrontEnd_ReadPacket (FWall.pTlmSrc)) != NULL) {

    if (pPkt->nType & CasBadPacket) {
      fprintf (stderr, "Bad Packet @ cds=%08X, Record Length=%08X\n"
               "nType=%08X, nMode=%08X ", pPkt->tlm.Time.cds.seconds,
               pPkt->tlm.Packet.nLength, pPkt->tlm.nType, pPkt->tlm.nMode);
    }

    else if (pPkt->nType & CasSciLFDR) {
      if (pPkt->lfdr.nMode & CasAntEz) {
        if (FWall.pLFDR[1] != NULL)
          CasTelemetry_Destructor (FWall.pLFDR[1]);
        FWall.pLFDR[1] = pPkt;
      } else if (pPkt->lfdr.nMode & CasAntBx) {
        if (FWall.pLFDR[2] != NULL)
          CasTelemetry_Destructor (FWall.pLFDR[2]);
        FWall.pLFDR[2] = pPkt;
      } else if (pPkt->lfdr.nMode & CasAntBy) {
        if (FWall.pLFDR[3] != NULL)
          CasTelemetry_Destructor (FWall.pLFDR[3]);
        FWall.pLFDR[3] = pPkt;
      } else if (pPkt->lfdr.nMode & CasAntBz) {
        if (FWall.pLFDR[4] != NULL)
          CasTelemetry_Destructor (FWall.pLFDR[4]);
        FWall.pLFDR[4] = pPkt;
      } else if (pPkt->lfdr.nMode & CasAntLMRp) {
        if (FWall.pLFDR[5] != NULL)
          CasTelemetry_Destructor (FWall.pLFDR[5]);
        FWall.pLFDR[5] = pPkt;
      } else if (pPkt->lfdr.nMode & CasAntLMRm) {
        if (FWall.pLFDR[6] != NULL)
          CasTelemetry_Destructor (FWall.pLFDR[6]);
        FWall.pLFDR[6] = pPkt;
      } else if (pPkt->lfdr.nMode & CasAntLPs) {
        if (FWall.pLFDR[7] != NULL)
          CasTelemetry_Destructor (FWall.pLFDR[7]);
        FWall.pLFDR[7] = pPkt;
      } else {                          /* Ex */
        if (FWall.pLFDR[0] != NULL)
          CasTelemetry_Destructor (FWall.pLFDR[0]);
        FWall.pLFDR[0] = pPkt;
      }
    }
    /*
     * if lfdr packets 
     */
    else if (pPkt->nType & CasSciMFR) {
      if (pPkt->mfr.nMode & CasAntEz) {
        if (FWall.pMFR[1] != NULL)
          CasTelemetry_Destructor (FWall.pMFR[1]);
        FWall.pMFR[1] = pPkt;
      } else if (pPkt->mfr.nMode & CasAntBx) {
        if (FWall.pMFR[2] != NULL)
          CasTelemetry_Destructor (FWall.pMFR[2]);
        FWall.pMFR[2] = pPkt;
      } else if (pPkt->mfr.nMode & CasAntBz) {
        if (FWall.pMFR[3] != NULL)
          CasTelemetry_Destructor (FWall.pMFR[3]);
        FWall.pMFR[3] = pPkt;
      } else {
        if (FWall.pMFR[0] != NULL)
          CasTelemetry_Destructor (FWall.pMFR[0]);
        FWall.pMFR[0] = pPkt;
      }
    }
    /*
     * MFR Packets 
     */
    else if ((pPkt->nType & CasSciHFR) && (pPkt->hfr.nMode & CasHFR_Analysis)) {
      if (pPkt->hfr.nMode & CasHFR_Analysis) {
        if (pPkt->hfr.nMode & CasAntExp) {
          if (FWall.pHFR[0] != NULL)
            CasTelemetry_Destructor (FWall.pHFR[0]);
          FWall.pHFR[0] = pPkt;
        }
        if (pPkt->hfr.nMode & CasAntExm) {
          if (FWall.pHFR[1] != NULL)
            CasTelemetry_Destructor (FWall.pHFR[1]);
          FWall.pHFR[1] = pPkt;
        }
        if (pPkt->hfr.nMode & CasAntEx) {
          if (FWall.pHFR[2] != NULL)
            CasTelemetry_Destructor (FWall.pHFR[2]);
          FWall.pHFR[2] = pPkt;
        }
        if (pPkt->hfr.nMode & CasAntEz) {
          if (FWall.pHFR[3] != NULL)
            CasTelemetry_Destructor (FWall.pHFR[3]);
          FWall.pHFR[3] = pPkt;
        }
      } /* if analysis packets */
      else if (pPkt->hfr.nMode & CasHFR_Millisecond)
        pPktStorage = &(FWall.pHFR[4]);
    }
    /*
     * if hfr Packets 
     */
    else {

/*      fprintf(stderr,"UnHandled Packet of type %08X\n",pPkt->nType); */

/*      CasTelemetry_Dump(pPkt,stderr); */
    }

    LoadWindowHeader ();

    if (LoadWindow (FWall.pMainWindow) == True) {       /* New data to display */
      cpgbbuf ();
      FWall.pMainWindow->pLine =
        (CPGLine *) HeadListElement (FWall.pMainWindow->pLine);
      UpdateManagerWindow_CPGPlotWindow (FWall.pMainWindow);
      cpgebuf ();

/*
      {
      CPGLine *pLine=(CPGLine*)HeadListElement(FWall.pMainWindow->pLine);
        fprintf(stderr,"UpdateWindow()\n");
        while(pLine!=NULL){
          DumpPacketId((PacketId*)(pLine->pUserData));
          pLine=(CPGLine*)NextListElement(pLine);
        }
      }
*/
      if (FWall.hLogFile != NULL) {
        int i;
        long nLen;
        float fMag[16000], fFrq[16000];

        fprintf (FWall.hLogFile, "\n# EVTCLK %08X.%02X  RTI %04X\n",
                 pPkt->sci.Time.evtclk.seconds,
                 pPkt->sci.Time.evtclk.milliseconds, pPkt->sci.Time.rti);

        switch (pPkt->nType & CasSciMask) {

         case CasSciLFDR:
           nLen = CasLFDR_GetPhysicalUnits (&(pPkt->lfdr),
                                            FWall.nLFDRmode, fMag, fFrq,
                                            NULL);
           if (nLen > 0)
             fprintf (FWall.hLogFile, "# LFDR\n");
           for (i = 0; i < nLen; i++)
             fprintf (FWall.hLogFile, "%#-4.4E      %#-4.4E\n", fFrq[i],
                      fMag[i]);
           break;

         case CasSciMFR:
           nLen = CasMFR_GetPhysicalUnits (&(pPkt->mfr),
                                           FWall.nMFRmode, fMag, fFrq, NULL);

           if (nLen > 0)
             fprintf (FWall.hLogFile, "# MFR\n");
           for (i = 0; i < 16; i++)
             fprintf (FWall.hLogFile, "%#-4.4E      %#-4.4E  %#-4.4E\n",
                      fFrq[i], fMag[i], fMag[i + 16]);

           for (i = 0; i < 32; i++)
             fprintf (FWall.hLogFile, "%#-4.4E      %#-4.4E  %#-4.4E\n",
                      fFrq[i], fMag[i + 32], fMag[i + 64]);

           for (i = 0; i < 32; i++)
             fprintf (FWall.hLogFile, "%#-4.4E      %#-4.4E  %#-4.4E  "
                      "%#-4.4E  %#-4.4E\n",
                      fFrq[i], fMag[i + 96], fMag[i + 128],
                      fMag[i + 160], fMag[i + 192]);
           break;
         case CasSciHFR:

           nLen = CasHFR_GetPhysicalUnits (&(pPkt->hfr),
                                           (FWall.
                                            nHFRmode & CasHFR_BandMask) |
                                           (FWall.nHFRmode & CasAntEx), fMag,
                                           fFrq, NULL);
           if (nLen > 0)
             fprintf (FWall.hLogFile, "# HFR Ex+/-\n");
           for (i = 0; i < nLen; i++) {
             fMag[i] = pow (10.0, fMag[i] / 20.0);      /* from dBVrms/rtHz */
             fMag[i] *= 1.0 / 9.26;     /* Vrms / m*rtHz */
             fMag[i] *= fMag[i];        /* Vrms*Vrms / m*m*Hz */
             fprintf (FWall.hLogFile, "%#-4.4E      %#-4.4E\n",
                      fFrq[i], fMag[i]);
           }

           nLen = CasHFR_GetPhysicalUnits (&(pPkt->hfr),
                                           (FWall.
                                            nHFRmode & CasHFR_BandMask) |
                                           (FWall.nHFRmode & CasAntEz), fMag,
                                           fFrq, NULL);
           if (nLen > 0)
             fprintf (FWall.hLogFile, "# HFR Ez\n");
           for (i = 0; i < nLen; i++) {
             fMag[i] = pow (10.0, fMag[i] / 20.0);      /* from dBVrms/rtHz */
             fMag[i] *= 1.0 / 5.00;     /* Vrms / m*rtHz */
             fMag[i] *= fMag[i];        /* Vrms*Vrms / m*m*Hz */
             fprintf (FWall.hLogFile, "%#-4.4E      %#-4.4E\n",
                      fFrq[i], fMag[i]);
           }

           nLen = CasHFR_GetPhysicalUnits (&(pPkt->hfr),
                                           (FWall.
                                            nHFRmode & CasHFR_BandMask) |
                                           (FWall.
                                            nHFRmode & (CasAntExp |
                                                        CasAntExm)), fMag,
                                           fFrq, NULL);
           if (nLen > 0)
             fprintf (FWall.hLogFile, "# HFR Ex+,Ex-\n");
           for (i = 0; i < nLen; i++) {
             fMag[i] = pow (10.0, fMag[i] / 20.0);      /* from dBVrms/rtHz */
             fMag[i] *= 1.0 / 5.00;     /* Vrms / m*rtHz */
             fMag[i] *= fMag[i];        /* Vrms*Vrms / m*m*Hz */
             fprintf (FWall.hLogFile, "%#-4.4E      %#-4.4E\n",
                      fFrq[i], fMag[i]);
           }

           break;
         default:
           nLen = 0;
           break;
        }

      }
      /*
       * if log new data 
       */
    }
    /*
     * if window update 
     */
  }
  /*
   * if new packets 
   */
  else {
    static int nCount;

    if ((nCount % 8) == 7)
      fprintf (stderr, ".");
    ++nCount;

/*
  fprintf(stderr,"Work()=%d, %s\n",
    nTimes++,FWall.pTlmSrc->sNewInFile[FWall.pTlmSrc->nInFileIndex]); 
*/

/* 
Example Calls to the PgPlot Motif Widget
Since the device is allready opened and
it is currenly being pointed at, every call
applies to it.

  cpgbbuf(); 
  cpgswin(0,10,0,100); 
  cpgbox("bcnts",0,0,"bcnts",0,0);
  cpglab("Bottom Label","Left Label","Top Label");
  cpgline(nLength,nXdata,nYdata); 
  cpgebuf(); 
*/

    SleepFor_Seconds (0.125);
  }

  return False;                         /* True means Work is done, False means Work is not done */
}



Bool LoadWindow (CPGPlotWindow * pWin)
{
  ULONG nAnt, nBnd, nMode, nEvtSec;

  CPGAxis *pAxis;
  CPGLabel *pLabel;
  CPGLine *pLine;

  CasTelemetry *pPkt;

  PacketId *pLineId;


  ULONG nLen, nLoop;
  Bool bStatus = False;
  Bool bNewData = False;

/* 
  Look at the data in the display window to see if it matches
  the control list for antenna/receiver mode display, if it 
  doesn't, remove it from the list.
*/

  pLine = (CPGLine *) HeadListElement (pWin->pLine);    /* Should be the head anyway */
  while (pLine != NULL) {
    pLineId = (PacketId *) pLine->pUserData;
    if (pLineId->nType & CasSciLFDR) {
      if (!(FWall.nLFDRmode & pLineId->nMode & CasAntMask))
        bStatus = True;
    } else if (pLineId->nType & CasSciMFR) {
      if (!(FWall.nMFRmode & pLineId->nMode & CasAntMask))
        bStatus = True;
    } else if (pLineId->nType & CasSciHFR) {
      if (!(FWall.nHFRmode & pLineId->nMode & CasAntMask))
        bStatus = True;
    } else {
      bStatus = True;
    }
    if (bStatus == True) {
      pLineId = (PacketId *) pLine->pUserData;
      free (pLine->x);                  /* Free Allocated Memory */
      free (pLine->y);
      free (pLine->pUserData);

      if (pLine == (CPGLine *) HeadListElement (pWin->pLine))   /* Head of the List */
        pWin->pLine = (CPGLine *) NextListElement (pLine);
      else if (pLine == (CPGLine *) TailListElement (pWin->pLine))      /* Tail of the List */
        pWin->pLine = (CPGLine *) PreviousListElement (pLine);

      pLine = (CPGLine *) DeleteListElement (pLine);

      bStatus = False;
    } else {
      pLine = (CPGLine *) NextListElement (pLine);
    }

  }                                     /* While pLine */





/* LFDR - lfdr - LFDR - lfdr - LFDR - lfdr - LFDR - lfdr -  LFDR */

/* LFDR - lfdr - LFDR - lfdr - LFDR - lfdr - LFDR - lfdr -  LFDR */

/* LFDR - lfdr - LFDR - lfdr - LFDR - lfdr - LFDR - lfdr -  LFDR */
  /*
   * Look in Window List to see if new line need to be created 
   */
  for (nLoop = 0; nLoop < 8; nLoop++) {

    switch (nLoop) {
     case 0:
       nAnt = CasAntEx;
       break;
     case 1:
       nAnt = CasAntEz;
       break;
     case 2:
       nAnt = CasAntBx;
       break;
     case 3:
       nAnt = CasAntBy;
       break;
     case 4:
       nAnt = CasAntBz;
       break;
     case 5:
       nAnt = CasAntLMRp;
       break;
     case 6:
       nAnt = CasAntLMRm;
       break;
     case 7:
       nAnt = CasAntLPs;
       break;
     default:
       nAnt = CasAntEx;
       break;
    }

    /*
     * If Data Exists in memory and is supposed to be displayed,
     * Check with the window to see if it is:
     * 1. Already being displayed
     * 2. Needs to be updated
     * 3. Needs to be Created
     */
    if ((FWall.pLFDR[nLoop] != NULL) && (FWall.nLFDRmode & nAnt)) {
      pLine = (CPGLine *) HeadListElement (pWin->pLine);
      while (pLine != NULL) {
        pLineId = (PacketId *) pLine->pUserData;
        if ((pLineId->nType & CasSciLFDR) && (pLineId->nMode & nAnt)) {
          if (pLineId->nEvtSec !=
              FWall.pLFDR[nLoop]->lfdr.Time.evtclk.seconds) {
            bNewData = True;
            nLen = CasLFDR_GetSweepByteCount (&(FWall.pLFDR[nLoop]->lfdr),
                                              CasLFDR_BandsAll | nAnt);
            if (nLen != pLine->length) {
              free (pLine->x);
              free (pLine->y);
              pLine->length = nLen;
              pLine->x = (float *) calloc (pLine->length, sizeof (float));
              pLine->y = (float *) calloc (pLine->length, sizeof (float));
            }

            nLen = CasLFDR_GetPhysicalUnits (&(FWall.pLFDR[nLoop]->lfdr),
                                             CasLFDR_BandsAll | nAnt,
                                             pLine->y, pLine->x, NULL);
            pLineId = (PacketId *) pLine->pUserData;
            pLineId->nType = FWall.pLFDR[nLoop]->lfdr.nType;
            pLineId->nMode = FWall.pLFDR[nLoop]->lfdr.nMode;
            pLineId->nEvtSec = FWall.pLFDR[nLoop]->lfdr.Time.evtclk.seconds;
            if (FWall.bXlog == True || FWall.bYlog == True)
              LogLinTransform (pLine);
            break;
          }                             /* Replace the old data with the new */
          break;
        }                               /* if lfdr line */
        pLine = (CPGLine *) NextListElement (pLine);
      }                                 /* While pLine */


      if (pLine == NULL) {              /* No data found, Create it */
        bNewData = True;
        pLine = (CPGLine *) CreateListElement (CPGLine);
        pLine->pUserData = (void *) calloc (1, sizeof (PacketId));
        nLen = CasLFDR_GetSweepByteCount (&(FWall.pLFDR[nLoop]->lfdr),
                                          CasLFDR_BandsAll | nAnt);
        pLine->x = (float *) calloc (nLen, sizeof (float));
        pLine->y = (float *) calloc (nLen, sizeof (float));
        pLine->length = nLen;
        nLen = CasLFDR_GetPhysicalUnits (&(FWall.pLFDR[nLoop]->lfdr),
                                         CasLFDR_BandsAll | nAnt, pLine->y,
                                         pLine->x, NULL);
        pLineId = (PacketId *) pLine->pUserData;
        pLineId->nType = FWall.pLFDR[nLoop]->lfdr.nType;
        pLineId->nMode = FWall.pLFDR[nLoop]->lfdr.nMode;
        pLineId->nEvtSec = FWall.pLFDR[nLoop]->lfdr.Time.evtclk.seconds;
        if (FWall.bXlog == True || FWall.bYlog == True)
          LogLinTransform (pLine);

        /*
         * Add Some Handlers and attributes 
         */
        pLine->pOnDraw = OnDraw_LFDR;

        if (pWin->pLine == NULL)
          pWin->pLine = pLine;
        else
          AddListElement (pWin->pLine, pLine);
      }                                 /* if New lfdr line */
    }                                   /* if LFDR Data Exists in Memory and is supposed to be displayed */
  }                                     /* for all lfdr antennas */

/* LFDR - lfdr - LFDR - lfdr - LFDR - lfdr - LFDR - lfdr -  LFDR */

/* LFDR - lfdr - LFDR - lfdr - LFDR - lfdr - LFDR - lfdr -  LFDR */

/* LFDR - lfdr - LFDR - lfdr - LFDR - lfdr - LFDR - lfdr -  LFDR */




/* MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR */

/* MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR */

/* MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR */

  /*
   * Look in Window List to see if new line need to be created 
   */
  for (nLoop = 0; nLoop < 4; nLoop++) {

    switch (nLoop) {
     case 1:
       nAnt = CasAntEz;
       break;
     case 2:
       nAnt = CasAntBx;
       break;
     case 3:
       nAnt = CasAntBz;
       break;
     default:
       nAnt = CasAntEx;
       break;
    }

    /*
     * If Data Exists in memory and is supposed to be displayed,
     * Check with the window to see if it is:
     * 1. Already being displayed
     * 2. Needs to be updated
     * 3. Needs to be Created
     */
    if ((FWall.pMFR[nLoop] != NULL) && (FWall.nMFRmode & nAnt)) {
      pLine = (CPGLine *) HeadListElement (pWin->pLine);
      while (pLine != NULL) {
        pLineId = (PacketId *) pLine->pUserData;
        if ((pLineId->nType & CasSciMFR) && (pLineId->nMode & nAnt)) {
          if (pLineId->nEvtSec != FWall.pMFR[nLoop]->mfr.Time.evtclk.seconds) {
            bNewData = True;
            nLen = CasMFR_GetSweepByteCount (&(FWall.pMFR[nLoop]->mfr),
                                             CasMFR_BandsAll | nAnt);
            if (nLen != pLine->length) {
              free (pLine->x);
              free (pLine->y);
              pLine->length = nLen;
              pLine->x = (float *) calloc (pLine->length, sizeof (float));
              pLine->y = (float *) calloc (pLine->length, sizeof (float));
            }

            nLen = CasMFR_GetPhysicalUnits (&(FWall.pMFR[nLoop]->mfr),
                                            CasMFR_BandsAll | nAnt, pLine->y,
                                            pLine->x, NULL);
            pLineId = (PacketId *) pLine->pUserData;
            pLineId->nType = FWall.pMFR[nLoop]->mfr.nType;
            pLineId->nMode = FWall.pMFR[nLoop]->mfr.nMode;
            pLineId->nEvtSec = FWall.pMFR[nLoop]->mfr.Time.evtclk.seconds;
            if (FWall.bXlog == True || FWall.bYlog == True)
              LogLinTransform (pLine);
            break;
          }                             /* Replace the old data with the new */
          break;
        }                               /* if mfr line */
        pLine = (CPGLine *) NextListElement (pLine);
      }                                 /* While pLine */

      if (pLine == NULL) {              /* No data found, Create it */
        bNewData = True;
        pLine = (CPGLine *) CreateListElement (CPGLine);
        pLine->pUserData = (void *) calloc (1, sizeof (PacketId));
        nLen = CasMFR_GetSweepByteCount (&(FWall.pMFR[nLoop]->mfr),
                                         CasMFR_BandsAll | nAnt);
        pLine->x = (float *) calloc (nLen, sizeof (float));
        pLine->y = (float *) calloc (nLen, sizeof (float));
        pLine->length = nLen;
        nLen = CasMFR_GetPhysicalUnits (&(FWall.pMFR[nLoop]->mfr),
                                        CasMFR_BandsAll | nAnt, pLine->y,
                                        pLine->x, NULL);
        pLineId = (PacketId *) pLine->pUserData;
        pLineId->nType = FWall.pMFR[nLoop]->mfr.nType;
        pLineId->nMode = FWall.pMFR[nLoop]->mfr.nMode;
        pLineId->nEvtSec = FWall.pMFR[nLoop]->mfr.Time.evtclk.seconds;
        if (FWall.bXlog == True || FWall.bYlog == True)
          LogLinTransform (pLine);

        /*
         * Add Some Handlers and attributes 
         */
        pLine->pOnDraw = OnDraw_MFR;

        if (pWin->pLine == NULL)
          pWin->pLine = pLine;
        else
          AddListElement (pWin->pLine, pLine);
      }                                 /* if New mfr line */
    }                                   /* if MFR Data Exists in Memory and is supposed to be displayed */
  }                                     /* for all mfr antennas */

/* MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR */

/* MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR */

/* MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR - mfr - MFR */


/* HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR */

/* HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR */

/* HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR */

  /*
   * Look in Window List to see if new line need to be created 
   */
  for (nLoop = 0; nLoop < 4; nLoop++) {
    if (nLoop == 0)
      nAnt = CasAntExp;
    else if (nLoop == 1)
      nAnt = CasAntExm;
    else if (nLoop == 2)
      nAnt = CasAntEx;
    else if (nLoop == 3)
      nAnt = CasAntEz;

    /*
     * If Data Exists in memory and is supposed to be displayed,
     * Check with the window to see if it is:
     * 1. Already being displayed
     * 2. Needs to be updated
     * 3. Needs to be Created
     */
    if ((FWall.pHFR[nLoop] != NULL) && (FWall.nHFRmode & nAnt)) {
      pLine = (CPGLine *) HeadListElement (pWin->pLine);
      while (pLine != NULL) {
        pLineId = (PacketId *) pLine->pUserData;
        if ((pLineId->nType & CasSciHFR) && (pLineId->nMode & nAnt)) {
          if (pLineId->nEvtSec != FWall.pHFR[nLoop]->hfr.Time.evtclk.seconds) {
            bNewData = True;
            nLen = CasHFR_GetSweepByteCount (&(FWall.pHFR[nLoop]->hfr),
                                             CasHFR_BandsAll | nAnt);

            if (nLen != pLine->length) {
              free (pLine->x);
              free (pLine->y);
              pLine->length = nLen;
              pLine->x = (float *) calloc (pLine->length, sizeof (float));
              pLine->y = (float *) calloc (pLine->length, sizeof (float));
            }

            nLen = CasHFR_GetPhysicalUnits (&(FWall.pHFR[nLoop]->hfr),
                                            CasHFR_BandsAll | nAnt, pLine->y,
                                            pLine->x, NULL);
            pLineId = (PacketId *) pLine->pUserData;
            pLineId->nType = FWall.pHFR[nLoop]->hfr.nType;
            pLineId->nMode =
              (FWall.pHFR[nLoop]->hfr.nMode & ~CasAntMask) | nAnt;
            pLineId->nEvtSec = FWall.pHFR[nLoop]->hfr.Time.evtclk.seconds;
            if (FWall.bXlog == True || FWall.bYlog == True)
              LogLinTransform (pLine);
            break;
          }                             /* Replace the old data with the new */
          break;
        }                               /* if hfr line */
        pLine = (CPGLine *) NextListElement (pLine);
      }                                 /* While pLine */

      if (pLine == NULL) {              /* No data found, Create it */
        bNewData = True;
        pLine = (CPGLine *) CreateListElement (CPGLine);
        pLine->pUserData = (void *) calloc (1, sizeof (PacketId));
        nLen = CasHFR_GetSweepByteCount (&(FWall.pHFR[nLoop]->hfr),
                                         CasHFR_BandsAll | nAnt);

        pLine->x = (float *) calloc (nLen, sizeof (float));
        pLine->y = (float *) calloc (nLen, sizeof (float));
        pLine->length = nLen;
        nLen = CasHFR_GetPhysicalUnits (&(FWall.pHFR[nLoop]->hfr),
                                        CasHFR_BandsAll | nAnt, pLine->y,
                                        pLine->x, NULL);
        pLineId = (PacketId *) pLine->pUserData;
        pLineId->nType = FWall.pHFR[nLoop]->hfr.nType;
        pLineId->nMode = (FWall.pHFR[nLoop]->hfr.nMode & ~CasAntMask) | nAnt;
        pLineId->nEvtSec = FWall.pHFR[nLoop]->hfr.Time.evtclk.seconds;
        if (FWall.bXlog == True || FWall.bYlog == True)
          LogLinTransform (pLine);

        /*
         * Add Some Handlers and attributes 
         */
        pLine->pOnDraw = OnDraw_HFR;

        if (pWin->pLine == NULL)
          pWin->pLine = pLine;
        else
          AddListElement (pWin->pLine, pLine);
      }                                 /* if New hfr line */
    }                                   /* if HFR Data Exists in Memory and is supposed to be displayed */
  }                                     /* for all hfr antennas */





/* HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR */

/* HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR */

/* HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR - hfr - HFR */


  return bNewData;
}


Bool LoadWindowHeader ()
{
  static Bool bInit = False;
  CPGLabel *pLabel;
  PacketId *pId;
  int i;
  static char sHFR[256], sMFR[256], sLFDR[256];


  if (bInit == False) {
    bInit = True;
    pLabel = (CPGLabel *) CreateListElement (CPGLabel);
    strcpy (sLFDR, "LFDR");
    pLabel->text = sLFDR;
    pLabel->fjust = 0.5;                /* Centered */
    pLabel->side = sEquals ("T");
    pLabel->disp = 1.0;
    pLabel->coord = 0.5;
    pLabel->char_height = 1.0;
    pLabel->color_index = 1;
    pId = (PacketId *) calloc (1, sizeof (PacketId));
    pId->nType = CasSciLFDR;
    pLabel->pUserData = (void *) pId;
    AddListElement (FWall.pMainWindow->pLabel, pLabel);

    pLabel = (CPGLabel *) CreateListElement (CPGLabel);
    strcpy (sMFR, "MFR");
    pLabel->text = sMFR;
    pLabel->fjust = 0.5;                /* Centered */
    pLabel->side = sEquals ("T");
    pLabel->disp = 2.0;
    pLabel->coord = 0.5;
    pLabel->char_height = 1.0;
    pLabel->color_index = 1;
    pId = (PacketId *) calloc (1, sizeof (PacketId));
    pId->nType = CasSciMFR;
    pLabel->pUserData = (void *) pId;
    AddListElement (FWall.pMainWindow->pLabel, pLabel);

    pLabel = (CPGLabel *) CreateListElement (CPGLabel);
    strcpy (sHFR, "HFR");
    pLabel->text = sHFR;
    pLabel->fjust = 0.5;                /* Centered */
    pLabel->side = sEquals ("T");
    pLabel->disp = 3.0;
    pLabel->coord = 0.5;
    pLabel->char_height = 1.0;
    pLabel->color_index = 1;
    pId = (PacketId *) calloc (1, sizeof (PacketId));
    pId->nType = CasSciHFR;
    pLabel->pUserData = (void *) pId;
    AddListElement (FWall.pMainWindow->pLabel, pLabel);
  }

  strcpy (sLFDR, "LFDR:: ");
  for (i = 0; i < 8; i++) {
    if (FWall.pLFDR[i] != NULL) {
      switch (i) {
       case 0:
         strcat (sLFDR, "Ex ");
         break;
       case 1:
         strcat (sLFDR, "Ez ");
         break;
       case 2:
         strcat (sLFDR, "Bx ");
         break;
       case 3:
         strcat (sLFDR, "By ");
         break;
       case 4:
         strcat (sLFDR, "Bz ");
         break;
       case 5:
         strcat (sLFDR, "LMR+ ");
         break;
       case 6:
         strcat (sLFDR, "LMR+ ");
         break;
       case 7:
         strcat (sLFDR, "LPs ");
         break;
      }
      strcatf (sLFDR, "%08X, ", FWall.pLFDR[i]->lfdr.Time.evtclk.seconds);
    }
  }

  strcpy (sMFR, "MFR:: ");
  for (i = 0; i < 4; i++) {
    if (FWall.pMFR[i] != NULL) {
      switch (i) {
       case 0:
         strcat (sMFR, "Ex ");
         break;
       case 1:
         strcat (sMFR, "Ez ");
         break;
       case 2:
         strcat (sMFR, "Bx ");
         break;
       case 3:
         strcat (sMFR, "Bz ");
         break;
      }
      strcatf (sMFR, "%08X, ", FWall.pMFR[i]->mfr.Time.evtclk.seconds);
    }
  }

  strcpy (sHFR, "HFR:: ");
  for (i = 0; i < 4; i++) {
    if (FWall.pHFR[i] != NULL) {
      switch (i) {
       case 0:
         strcat (sHFR, "Ex+ ");
         break;
       case 1:
         strcat (sHFR, "Ex- ");
         break;
       case 2:
         strcat (sHFR, "Ex+/- ");
         break;
       case 3:
         strcat (sHFR, "Ez ");
         break;
      }
      strcatf (sHFR, "%08X, ", FWall.pHFR[i]->hfr.Time.evtclk.seconds);
    }
  }


  return True;
}


void LogLinTransform (CPGLine * pLine)
{
  int nLoop;
  float fTmp;
  PacketId *pPktId = (PacketId *) pLine->pUserData;

  if (pPktId == NULL)
    return;
  else if ((pPktId->nType & CasSciLFDR) ||      /* Default Units or V^2/m^2/Hz */
           (pPktId->nType & CasSciMFR)) {       /* Default Units or V^2/m^2/Hz */
    if (FWall.bXlog == True) {
      for (nLoop = 0; nLoop < pLine->length; nLoop++)
        pLine->x[nLoop] =
          pLine->x[nLoop] <= 0.0 ? log10 (1.0E-120) : log10 (pLine->x[nLoop]);
    }
    if (FWall.bYlog == True) {
      for (nLoop = 0; nLoop < pLine->length; nLoop++)
        pLine->y[nLoop] =
          pLine->y[nLoop] <= 0.0 ? log10 (1.0E-120) : log10 (pLine->y[nLoop]);
    }
  } else if (pPktId->nType & CasSciHFR) {       /* Default Units are dBVrms/rtHz */
    if (pPktId->nMode & CasAntEx)
      fTmp = 1.0 / 9.26;
    else
      fTmp = 1.0 / 5.00;

    for (nLoop = 0; nLoop < pLine->length; nLoop++) {
      pLine->y[nLoop] = pow (10.0, (pLine->y[nLoop] / 20.0));   /* From dB/rtHz to Vrms/rtHz */
      pLine->y[nLoop] *= fTmp;          /* Vrms/m*rtHz */
      pLine->y[nLoop] *= pLine->y[nLoop];       /* Vrms*Vrms/m*m*Hz */
    }

    if (FWall.bXlog == True) {
      for (nLoop = 0; nLoop < pLine->length; nLoop++)
        pLine->x[nLoop] =
          pLine->x[nLoop] <= 0.0 ? log10 (1.0E-120) : log10 (pLine->x[nLoop]);
    }
    if (FWall.bYlog == True) {
      for (nLoop = 0; nLoop < pLine->length; nLoop++)
        pLine->y[nLoop] =
          pLine->y[nLoop] <= 0.0 ? log10 (1.0E-120) : log10 (pLine->y[nLoop]);
    }
  }
  /*
   * if HFR 
   */
  return;
}


Widget BuildMenu (Widget parent, int menu_type, char *menu_title,
                  char menu_mnemonic, Boolean tear_off, MenuItem * items)
{
  Widget menu, cascade, widget;
  int i;
  XmString str;

  if (menu_type == XmMENU_PULLDOWN || menu_type == XmMENU_OPTION)
    menu = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
  else if (menu_type == XmMENU_POPUP)
    menu = XmCreatePopupMenu (parent, "_popup", NULL, 0);
  else {
    XtWarning ("Invalid menu type passed to BuildMenu()");
    return NULL;
  }

  if (tear_off)
    XtVaSetValues (menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);


  if (menu_type == XmMENU_PULLDOWN) {
    str = XmStringCreateLocalized (menu_title);
    cascade = XtVaCreateManagedWidget (menu_title, xmCascadeButtonWidgetClass,
                                       parent, XmNsubMenuId, menu,
                                       XmNlabelString, str, XmNmnemonic,
                                       menu_mnemonic);
    XmStringFree (str);
  } else if (menu_type == XmMENU_OPTION) {
    Arg args[5];
    int n = 0;

    str = XmStringCreateLocalized (menu_title);
    XtSetArg (args[n], XmNsubMenuId, menu);
    n++;
    XtSetArg (args[n], XmNlabelString, str);
    n++;
    cascade = XmCreateOptionMenu (parent, menu_title, args, n);
    XmStringFree (str);
  }



  for (i = 0; items[i].label != NULL; i++) {

    if (items[i].subitems) {
      if (menu_type == XmMENU_OPTION) {
        XtWarning ("You can't have submenus from option menu items.");
        continue;
      } else
        widget = BuildMenu (menu, XmMENU_PULLDOWN, items[i].label,
                            items[i].mnemonic, tear_off, items[i].subitems);
    } else {
      if (items[i].widget_class == NULL) {      /* Elicit RadioButton Respones */
        XtVaSetValues (menu, XmNradioBehavior, True, NULL);
        items[i].widget_class = &xmToggleButtonWidgetClass;
        widget =
          XtVaCreateManagedWidget (items[i].label, *items[i].widget_class,
                                   menu, NULL);
        if (i == 0)
          XtVaSetValues (widget, XmNset, True, NULL);
      } else
        widget =
          XtVaCreateManagedWidget (items[i].label, *items[i].widget_class,
                                   menu, NULL);
    }

/*
    if(items[i].user_data){
      XtVaSetValues(widget,XmNuserData,items[i].user_data,NULL);
      }
*/


    if (items[i].mnemonic) {
      XtVaSetValues (widget, XmNmnemonic, items[i].mnemonic, NULL);
    }


    if (items[i].accelerator) {
      str = XmStringCreateLocalized (items[i].accel_text);
      XtVaSetValues (widget, XmNaccelerator, items[i].accelerator,
                     XmNacceleratorText, str, NULL);
      XmStringFree (str);
    }


    if (items[i].callback) {
      XtAddCallback (widget,
                     items[i].widget_class ==
                     &xmToggleButtonWidgetClass ? XmNvalueChangedCallback :
                     XmNactivateCallback, items[i].callback,
                     items[i].callback_data);
    }
  }

  return menu_type == XmMENU_POPUP ? menu : cascade;
}



void PopupErrorDialog (Widget w, XtPointer client_data, XtPointer call_data)
{
  XmString sMsg =
    XmStringCreateLtoR ((char *) client_data, XmFONTLIST_DEFAULT_TAG);
  XmString sTit =
    XmStringCreateLtoR ((char *) "Error", XmFONTLIST_DEFAULT_TAG);
  Widget wDialog;

/*  
  XmCreateErrorDialog(w,"_error_dialog_",NULL,0);
*/
  wDialog = XmCreateMessageDialog (w, "Error", NULL, 0);
  XtVaSetValues (wDialog, XmNdialogTitle, sTit, XmNdialogType, XmDIALOG_ERROR,
                 XmNmessageString, sMsg, NULL);
  XtUnmanageChild (XmMessageBoxGetChild (wDialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild (XmMessageBoxGetChild (wDialog, XmDIALOG_HELP_BUTTON));
  XtManageChild (wDialog);
  XtPopup (XtParent (wDialog), XtGrabNone);

  return;
}



void FileOpen (Widget w, XtPointer client_data, XtPointer call_data)
{
  Arg args[20];
  Atom WM_DELETE_WINDOW;
  char *pDefDir;
  int n;
  XmString Str;
  XtPointer user_data;
  Widget wDialog, wShell, wMenu, wParent;

  XtVaGetValues (w, XmNuserData, &user_data, NULL);
  if (user_data == NULL) {

    if ((pDefDir = getenv ("HMLDIR")) != NULL)
      Str = XmStringCreateLocalized (pDefDir);
    else
      Str = XmStringCreateLocalized ("/opt/project/cassini/data/");

    n = 0;
    XtSetArg (args[n], XmNdirectory, Str);
    ++n;

    wMenu = w;
    wParent = w;
    wDialog =
      XmCreateFileSelectionDialog (wParent, "file_selection_dialog", args, n);
    /**/

/*    wDialog=XmCreateFileSelectionDialog(wParent,"file_selection_dialog",NULL,0); /**/
      wShell = XtParent (wDialog);
    XmStringFree (Str);
    XtVaSetValues (wShell, XmNtitle, "File Selection", NULL);
    XtVaSetValues (wShell, XmNdeleteResponse, XmDO_NOTHING, NULL);
    XtAddCallback (wDialog, XmNokCallback, FileCallback_Ok,
                   (XtPointer) wShell);
    XtAddCallback (wDialog, XmNcancelCallback, FileCallback_Cancel,
                   (XtPointer) wShell);
    WM_DELETE_WINDOW =
      XmInternAtom (XtDisplay (wShell), "WM_DELETE_WINDOW", False);
    XmAddWMProtocolCallback (wShell, WM_DELETE_WINDOW, FileCallback_Cancel,
                             (XtPointer) wShell);
    XtAddCallback (wShell, XmNpopdownCallback, ToggleSensitivity,
                   (XtPointer) wMenu);
    XtAddCallback (wShell, XmNpopupCallback, ToggleSensitivity,
                   (XtPointer) wMenu);
    XtManageChild (wDialog);
    XtVaSetValues (wMenu, XmNuserData, wShell, NULL);
  } else {
    wShell = (Widget) user_data;
  }

  XtPopup (wShell, XtGrabNone);

  return;
}


void FileCallback_Ok (Widget w, XtPointer client_data, XtPointer call_data)
{
  Widget wShell = (Widget) client_data;
  XmFileSelectionBoxCallbackStruct *cbs =
    (XmFileSelectionBoxCallbackStruct *) call_data;
  char *pTmp;

  if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &pTmp))
    return;                             /* internal error */

  if (*pTmp != NULL) {                  /* There is something typed */
    strcpy (FWall.sFilename, pTmp);
    FWall.bNewTlmFile = True;
  }

  XtFree (pTmp);
  XtPopdown (wShell);


  return;
}


void FileCallback_Cancel (Widget w, XtPointer client_data,
                          XtPointer call_data)
{
  Widget wShell = (Widget) client_data;

  XtPopdown (wShell);

  return;
}

void PostPlayBackDialog (Widget w, XtPointer client_data, XtPointer call_data)
{
  char *pbchar[] = { "/users/raj/cassini/bitmaps/Rewind.bm",
    "/users/raj/cassini/bitmaps/StepRewind.bm",
    "/users/raj/cassini/bitmaps/PlayRewind.bm",
    "/users/raj/cassini/bitmaps/Stop.bm",
    "/users/raj/cassini/bitmaps/PlayForward.bm",
    "/users/raj/cassini/bitmaps/StepForward.bm",
    "/users/raj/cassini/bitmaps/FastForward.bm"
  };
  int num_btn = XtNumber (pbchar);
  int i, mwmfunc;
  Atom WM_DELETE_WINDOW;
  Widget wms = w, pane, rowcolumn, pushbutton, drawnbutton, default_pb;
  XmString string;
  XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
  static Widget Dialog, Caller;
  Pixmap image;
  Pixel fg, bg;

  void DE (Widget w, XtPointer client_data, XtPointer call_data);

  if (!Dialog) {
    Caller = w;
    while (wms && !XtIsWMShell (wms))
      wms = XtParent (wms);
    Dialog =
      XtVaCreatePopupShell ("dialog", topLevelShellWidgetClass, wms,
                            XmNdeleteResponse, XmDO_NOTHING, NULL);

/*    Dialog=XtVaCreatePopupShell("dialog",xmDialogShellWidgetClass,wms,XmNdeleteResponse,XmDO_NOTHING,NULL);*/
    pane =
      XtVaCreateWidget ("pane", xmPanedWindowWidgetClass, Dialog,
                        XmNsashWidth, 1, XmNsashHeight, 1, XmNseparatorOn,
                        False, NULL);

/*------------------
  string=XmStringCreateLocalized("no time");
  CLab=XtVaCreateManagedWidget("label",xmLabelWidgetClass,pane,XmNlabelString,string,NULL);
  XmStringFree(string);
------------------*/

    rowcolumn =
      XtVaCreateWidget ("rowcolumn", xmRowColumnWidgetClass, pane,
                        XmNorientation, XmHORIZONTAL, XmNpacking,
                        XmPACK_COLUMN, NULL);
    XtVaGetValues (rowcolumn, XmNforeground, &fg, XmNbackground, &bg, NULL);
    for (i = 0; i < num_btn; i++) {
      image = XmGetPixmap (XtScreen (rowcolumn), pbchar[i], bg, fg);
      if (i == 1 || i == 3 || i == 5) { /* StepRewind,Stop,StepForward */
        pushbutton =
          XtVaCreateManagedWidget ("pushbutton", xmPushButtonWidgetClass,
                                   rowcolumn, XmNlabelType, XmPIXMAP,
                                   XmNlabelPixmap, image, NULL);
        XtAddCallback (pushbutton, XmNactivateCallback, PlayBack,
                       (XtPointer) i);
      } else {
        drawnbutton =
          XtVaCreateManagedWidget ("drawnbutton", xmDrawnButtonWidgetClass,
                                   rowcolumn, XmNlabelType, XmPIXMAP,
                                   XmNlabelPixmap, image,
                                   XmNpushButtonEnabled, False, XmNshadowType,
                                   XmSHADOW_OUT, NULL);
        if (i == 4)
          default_pb = drawnbutton;     /* Default Button Play i=4 */
        XtAddCallback (drawnbutton, XmNactivateCallback, PlayBack,
                       (XtPointer) i);
        /*
         * This might be necessary later on.
         * XtAddCallback(drawnbutton,XmNexposeCallback,DE,(XtPointer)rowcolumn);
         */
      }
    }
    XtManageChild (rowcolumn);
    TurnOffSashTraversal (pane);
    XtManageChild (pane);
    WM_DELETE_WINDOW =
      XmInternAtom (XtDisplay (Dialog), "WM_DELETE_WINDOW", False);
    XmAddWMProtocolCallback (Dialog, WM_DELETE_WINDOW, PostPlayBackDialog,
                             NULL);
    mwmfunc = MWM_FUNC_ALL | MWM_FUNC_RESIZE | MWM_FUNC_MAXIMIZE;
    XtVaSetValues (Dialog, XmNmwmFunctions, mwmfunc, NULL);
    XtSetSensitive (Caller, False);
    /*
     * Default Button should match initial definition above 
     */
    XtCallActionProc (default_pb, "ArmAndActivate", cbs->event, NULL, 0);
  }

  switch (cbs->reason) {
   case XmCR_ACTIVATE:
     if (Caller == w) {
       XtSetSensitive (Caller, False);
       XtPopup (Dialog, XtGrabNone);
     } else {
       XtSetSensitive (Caller, True);
       XtPopdown (Dialog);
     }
     break;
   case XmCR_WM_PROTOCOLS:
     XtSetSensitive (Caller, True);
     XtPopdown (Dialog);
     break;
   default:
     break;
  }

  return;
}

void PlayBack (Widget w, XtPointer client_data, XtPointer call_data)
{
  int btn_num = (int) client_data;
  static Widget wlast;

  /*
   * Define Button Press Actions 
   */
  if (btn_num == 1 || btn_num == 3 || btn_num == 5) {
    if (wlast) {
      XtVaSetValues (wlast, XmNshadowType, XmSHADOW_OUT, NULL);
      wlast = 0;
    }
  } else if (wlast != w) {
    XtVaSetValues (w, XmNshadowType, XmSHADOW_IN, NULL);
    if (wlast) {
      XtVaSetValues (wlast, XmNshadowType, XmSHADOW_OUT, NULL);
    }
    wlast = w;
  }

  switch (btn_num) {
   case 0:
     FWall.pTlmSrc->nDataDirection = RecorderDirectionFastRewind;
     break;
   case 1:
     FWall.pTlmSrc->nDataDirection = RecorderDirectionStepRewind;
     break;
   case 2:
     FWall.pTlmSrc->nDataDirection = RecorderDirectionPlayRewind;
     break;
   case 3:
     FWall.pTlmSrc->nDataDirection = RecorderDirectionStop;
     break;
   case 4:
     FWall.pTlmSrc->nDataDirection = RecorderDirectionPlayForward;
     break;
   case 5:
     FWall.pTlmSrc->nDataDirection = RecorderDirectionStepForward;
     break;
   case 6:
     FWall.pTlmSrc->nDataDirection = RecorderDirectionFastForward;
     break;
   default:
     FWall.pTlmSrc->nDataDirection = RecorderDirectionPlayForward;
     break;
     break;
  }

  return;
}


void PlotResize (Widget w, XtPointer client_data, XEvent * event,
                 Boolean * reg)
{
  static Dimension width, height;
  XConfigureEvent *cfgevt = (XConfigureEvent *) event;

  if (cfgevt->type != ConfigureNotify)
    return;
  if (FWall.pMainWindow == NULL)
    return;

  /*
   * Window size did not change 
   */
  if (width == cfgevt->width && height == cfgevt->height)
    return;

  width = cfgevt->width;
  height = cfgevt->height;
  Close_CPGPlotWindow (FWall.pMainWindow);
  Open_CPGPlotWindow (FWall.pMainWindow);
  UpdateManagerWindow_CPGPlotWindow (FWall.pMainWindow);

/*
{
float xl,xr,yb,yt;

  cpgqwin(&xl,&xr,&yb,&yt);
  fprintf(stderr,"xl=%#-4.4E; xr=%#-4.4E; yb=%#-4.4E; yt=%#-4.4E\n",xl,xr,yb,yt);
  fprintf(stderr,"fXleft=%#-4.4E; fXright=%#-4.4E; fYtop=%#-4.4E; fYBottom=%#-4.4E\n",
          FWall.fXleft,FWall.fXright,FWall.fYtop,FWall.fYbottom);
  fprintf(stderr,"w_xl=%#-4.4E; w_xr=%#-4.4E; w_yt=%#-4.4E; w_yb=%#-4.4E\n",
          FWall.pMainWindow->w_xl,FWall.pMainWindow->w_xr,
		FWall.pMainWindow->w_yt,FWall.pMainWindow->w_yb);
          
}
*/

  return;
}


void PlotReHash (void)
{

  if (FWall.bXlog == True) {
    FWall.pMainWindow->w_xl =
      FWall.fXleft <= 0.0 ? log10 (0.1) : log10 (FWall.fXleft);
    FWall.pMainWindow->w_xr =
      FWall.fXright <= 0.0 ? log10 (16.1E6) : log10 (FWall.fXright);
  } else {
    FWall.pMainWindow->w_xl = FWall.fXleft;
    FWall.pMainWindow->w_xr = FWall.fXright;
  }
  if (FWall.bYlog == True) {
    FWall.pMainWindow->w_yt =
      FWall.fYtop <= 0.0 ? log10 (1.0E-6) : log10 (FWall.fYtop);
    FWall.pMainWindow->w_yb =
      FWall.fYbottom <= 0.0 ? log10 (1.0E-19) : log10 (FWall.fYbottom);
  } else {
    FWall.pMainWindow->w_yt = FWall.fYtop;
    FWall.pMainWindow->w_yb = FWall.fYbottom;
  }

/*
  free(FWall.pMainWindow->pAxis->xopt);
  free(FWall.pMainWindow->pAxis->yopt);
*/
  if (FWall.bXlog == True)
    FWall.pMainWindow->pAxis->xopt = sEquals ("BCLNSTV2");
  else
    FWall.pMainWindow->pAxis->xopt = sEquals ("BCNSTV2");
  if (FWall.bYlog == True)
    FWall.pMainWindow->pAxis->yopt = sEquals ("BCLNSTV2");
  else
    FWall.pMainWindow->pAxis->yopt = sEquals ("BCNSTV2");

  return;
}


/* Change Window Context from a Widget to a Postscript file and print */
void PrintWindow (Widget w, XtPointer client_data, XtPointer call_data)
{
  char *ptmpfile, chtmp[128], cmd[128];
  int i, x, y;
  char sNewDevice[128], *pOldDevice;
  char sPrintCmd[128], *pPrinter, *pTmpFile;

  if ((pPrinter = getenv ("HMLPRN")) != NULL)
    sprintf (sPrintCmd, "lp -onb -d %s", pPrinter);
  else
    sprintf (sPrintCmd, "lp -onb");

  pTmpFile = tmpnam (NULL);
  sprintf (sNewDevice, "%s/ps", pTmpFile);
  pOldDevice = FWall.pMainWindow->device_name;
  Close_CPGPlotWindow (FWall.pMainWindow);

  FWall.pMainWindow->device_name = sNewDevice;
  Open_CPGPlotWindow (FWall.pMainWindow);
  UpdateManagerWindow_CPGPlotWindow (FWall.pMainWindow);
  Close_CPGPlotWindow (FWall.pMainWindow);

  FWall.pMainWindow->device_name = pOldDevice;
  Open_CPGPlotWindow (FWall.pMainWindow);
  UpdateManagerWindow_CPGPlotWindow (FWall.pMainWindow);

  if ((pPrinter = getenv ("HMLPRN")) != NULL)
    sprintf (sPrintCmd, "lp -c -onb -d %s %s", pPrinter, pTmpFile);
  else
    sprintf (sPrintCmd, "lp -c -onb %s", pTmpFile);
  system (sPrintCmd);
  remove (pTmpFile);

  return;
}


/* Write the Currently Displayed Data to a File */
void LogData (Widget w, XtPointer client_data, XtPointer call_data)
{
  char sFileName[128], sTmp[128];
  int i;
  float *pX, *pY;
  double dX, dX1, dX2, dX3, dY, dY1, dY2, dY3, dFrq;
  FILE *hHandle = stdout;
  CPGLine *pLine = (CPGLine *) HeadListElement (FWall.pMainWindow->pLine);
  PacketId *pPktId;
  static int nFileNumber;

  if (pLine == NULL) {
    sprintf (sTmp, "No Data to Log.");
    PopupErrorDialog (w, sTmp, NULL);
    return;
  }

  sprintf (sFileName, "cashml%03d.dat", nFileNumber);
  if ((hHandle = fopen (sFileName, "wt")) == NULL) {
    sprintf (sTmp, "Unable to open %s.", sFileName);
    PopupErrorDialog (w, sTmp, NULL);
    return;
  } else {
    ++nFileNumber;
  }

  fprintf (hHandle, "#! TOP_LABEL= RPWS Cassini\n");
  fprintf (hHandle, "#! LEFT_LABEL= "
           "\\fiElectric & Magnetic Field Spectral Density: "
           "\\frV\\u2\\dm\\u-2\\dHz\\u-1\\d & nT\\u2\\dHz\\u-1\\d\n");
  fprintf (hHandle, "#! BOTTOM_LABEL=\\fiFrequency \\frHz\n");


  while (pLine != NULL) {
    pPktId = (PacketId *) (pLine->pUserData);

    if (pPktId->nType & CasSciLFDR) {
      fprintf (hHandle, "# LFDR %s, Event Seconds %08X\n",
               Cas_LFDRnModeToString (pPktId->nMode), pPktId->nEvtSec);
      OutputLineData (hHandle, pLine);
    }
    /*
     * if lfdr data 
     */
    else if (pPktId->nType & CasSciMFR) {
      fprintf (hHandle, "# MFR %s, Event Seconds %08X\n",
               Cas_MFRnModeToString (pPktId->nMode), pPktId->nEvtSec);
      /*
       * 224 points in a mfr capture, 16 & 16, 32 & 32, 32 & 32 & 32 & 32 
       */
      if (pLine->length == 224) {
        for (i = 0; i < 16; i++) {      /* Band 1 */
          dX = pLine->x[i];
          dY = pLine->y[i];
          dX1 = pLine->x[i + 16];
          dY1 = pLine->y[i + 16];
          if (FWall.bXlog == True) {
            dX = pow (10, dX);
            dX1 = pow (10, dX1);
          }
          if (FWall.bYlog == True) {
            dY = pow (10, dY);
            dY1 = pow (10, dY1);
          }
          if (dX == dX1) {
            fprintf (hHandle, "%#-4.4E   %#-4.4E   %#-4.4E\n", dX, dY, dY1);
          } else {
            fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX, dY);
            fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX1, dY1);
          }
        }                               /* for band 1 */

        pX = &(pLine->x[32]);
        pY = &(pLine->y[32]);
        for (i = 0; i < 32; i++) {      /* Band 2 */
          dX = pX[i];
          dY = pY[i];
          dX1 = pX[i + 32];
          dY1 = pY[i + 32];
          if (FWall.bXlog == True) {
            dX = pow (10, dX);
            dX1 = pow (10, dX1);
          }
          if (FWall.bYlog == True) {
            dY = pow (10, dY);
            dY1 = pow (10, dY1);
          }
          if (dX == dX1) {
            fprintf (hHandle, "%#-4.4E   %#-4.4E   %#-4.4E\n", dX, dY, dY1);
          } else {
            fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX, dY);
            fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX1, dY1);
          }
        }                               /* for band 2 */

        pX = &(pLine->x[32 + 64]);
        pY = &(pLine->y[32 + 64]);
        for (i = 0; i < 32; i++) {      /* Band 3 */
          dX = pX[i];
          dX1 = pX[i + 32];
          dX2 = pX[i + 64];
          dX3 = pX[i + 96];
          dY = pY[i];
          dY1 = pY[i + 32];
          dY2 = pY[i + 64];
          dY3 = pY[i + 96];
          if (FWall.bXlog == True) {
            dX = pow (10, dX);
            dX1 = pow (10, dX1);
            dX2 = pow (10, dX2);
            dX3 = pow (10, dX3);
          }
          if (FWall.bYlog == True) {
            dY = pow (10, dY);
            dY1 = pow (10, dY1);
            dY2 = pow (10, dY2);
            dY3 = pow (10, dY3);
          }
          if ((dX == dX1) && (dX == dX2) && (dX == dX3)) {
            fprintf (hHandle,
                     "%#-4.4E   %#-4.4E   %#-4.4E   %#-4.4E   %#-4.4E\n", dX,
                     dY, dY1, dY2, dY3);
          } else {
            fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX, dY);
            fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX1, dY1);
            fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX2, dY2);
            fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX3, dY3);
          }
        }                               /* for band 3 */
      } else {
        OutputLineData (hHandle, pLine);
      }
    }
    /*
     * if mfr data 
     */
    else if (pPktId->nType & CasSciHFR) {
      fprintf (hHandle, "# HFR %s, Event Seconds %08X\n",
               Cas_HFRnModeToString (pPktId->nMode), pPktId->nEvtSec);
      for (i = 0; i < pLine->length; i++) {
        dX = pLine->x[i];
        dY = pLine->y[i];
        if (FWall.bXlog == True)
          dX = pow (10, dX);
        if (FWall.bYlog == True)
          dY = pow (10, dY);
        fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX, dY);
      }
      fprintf (hHandle, "\n");
    } /* if hfr data */
    else {
      fprintf (hHandle, "# Unknown Data :: nMode=%08X, nType=%08X, "
               "Event Seconds %08X\n", pPktId->nType,
               pPktId->nMode, pPktId->nEvtSec);
      for (i = 0; i < pLine->length; i++) {
        dX = pLine->x[i];
        dY = pLine->y[i];
        if (FWall.bXlog == True)
          dX = pow (10, dX);
        if (FWall.bYlog == True)
          dY = pow (10, dY);
        fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX, dY);
      }
      fprintf (hHandle, "\n");
    }                                   /* else */

    pLine = (CPGLine *) NextListElement (pLine);
  }                                     /* while pLine */

  if (hHandle != stdout)
    fclose (hHandle);

  return;
}


void LogDataOnOff (Widget w, XtPointer client_data, XtPointer call_data)
{
  XmToggleButtonCallbackStruct *pCbs =
    (XmToggleButtonCallbackStruct *) call_data;
  char sTmp[64];

  if (pCbs->set == True) {
    if (FWall.hLogFile == NULL) {
      if ((FWall.hLogFile = fopen (FWall.sLogFile, "at")) == NULL) {
        sprintf (sTmp, "Unable to Open %s.", FWall.sLogFile);
        PopupErrorDialog (w, sTmp, NULL);
        XmToggleButtonSetState (w, False, False);
      }
    }
  } else {
    if (FWall.hLogFile != NULL) {
      fclose (FWall.hLogFile);
      FWall.hLogFile = NULL;
    }
  }

  return;
}


void LogDataFilename (Widget w, XtPointer client_data, XtPointer call_data)
{
  Widget wDialog;

  XmString sTit = XmStringCreateLocalized ("Logging Filename");
  XmString sMsg =
    XmStringCreateLocalized ("Enter the filename for data logging:");
  XmString sTxt = XmStringCreateLocalized (FWall.sLogFile);

  wDialog = XmCreatePromptDialog (w, "Filename", NULL, 0);
  XtVaSetValues (wDialog, XmNtitleString, sTit, XmNselectionLabelString, sMsg,
                 XmNtextString, sTxt, NULL);

  XtAddCallback (wDialog, XmNokCallback, LogDataFilename_Ok, NULL);
  XtAddCallback (wDialog, XmNcancelCallback, XtDestroyWidget, NULL);
  XtSetSensitive (XmSelectionBoxGetChild (wDialog, XmDIALOG_HELP_BUTTON),
                  False);

  XtManageChild (wDialog);
  XtPopup (XtParent (wDialog), XtGrabNone);

  XmStringFree (sTit);
  XmStringFree (sMsg);

  return;
}


/* XmNautoUnmange should be True => no need to call XtDestroyWidget() */
void LogDataFilename_Ok (Widget w, XtPointer client_data, XtPointer call_data)
{
  XmSelectionBoxCallbackStruct *cbs =
    (XmSelectionBoxCallbackStruct *) call_data;

  if (cbs->value != NULL) {
    XtFree (FWall.sLogFile);
    XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &FWall.sLogFile);
  }

  return;
}


void OutputLineData (FILE * hHandle, CPGLine * pLine)
{
  int i;
  double dX, dY;

  for (i = 0; i < pLine->length; i++) {
    dX = pLine->x[i];
    dY = pLine->y[i];
    if (FWall.bXlog == True)
      dX = pow (10, dX);
    if (FWall.bYlog == True)
      dY = pow (10, dY);
    fprintf (hHandle, "%#-4.4E   %#-4.4E\n", dX, dY);
  }
  fprintf (hHandle, "\n");

  return;
}


void AntSel_LFDR (Widget w, XtPointer client_data, XtPointer call_data)
{
  XmToggleButtonCallbackStruct *pCbs =
    (XmToggleButtonCallbackStruct *) call_data;
  ULONG nAnt = (ULONG) client_data;

  if (pCbs->set == True)
    FWall.nLFDRmode |= nAnt;
  else
    FWall.nLFDRmode &= ~nAnt;

  return;
}

void AntSel_MFR (Widget w, XtPointer client_data, XtPointer call_data)
{
  XmToggleButtonCallbackStruct *pCbs =
    (XmToggleButtonCallbackStruct *) call_data;
  ULONG nAnt = (ULONG) client_data;

  if (pCbs->set == True)
    FWall.nMFRmode |= nAnt;
  else
    FWall.nMFRmode &= ~nAnt;

  return;
}

void AntSel_HFR (Widget w, XtPointer client_data, XtPointer call_data)
{
  XmToggleButtonCallbackStruct *pCbs =
    (XmToggleButtonCallbackStruct *) call_data;
  ULONG nAnt = (ULONG) client_data;

  if (pCbs->set == True)
    FWall.nHFRmode |= nAnt;
  else
    FWall.nHFRmode &= ~nAnt;

  return;
}


void AxisMinMax (Widget w, XtPointer client_data, XtPointer call_data)
{
  char sTmp[64];
  int nWhich = (int) client_data;
  float *pValue;
  Widget wDialog;
  XmString Str, Str1;
  Arg args[20];
  int n = 0;

  if (nWhich == 0) {
    Str = XmStringCreateLocalized ("X Axis Minium Value");
    pValue = &FWall.fXleft;
  } else if (nWhich == 1) {
    Str = XmStringCreateLocalized ("X Axis Maxium Value");
    pValue = &FWall.fXright;
  } else if (nWhich == 2) {
    Str = XmStringCreateLocalized ("Y Axis Minium Value");
    pValue = &FWall.fYbottom;
  } else if (nWhich == 3) {
    Str = XmStringCreateLocalized ("Y Axis Maxium Value");
    pValue = &FWall.fYtop;
  } else {
    return;
  }
  sprintf (sTmp, "%#-4.4E", *pValue);
  Str1 = XmStringCreateLocalized (sTmp);

  XtSetArg (args[n], XmNselectionLabelString, Str);
  ++n;
  XtSetArg (args[n], XmNtextString, Str1);
  ++n;
  XtSetArg (args[n], XmNautoUnmanage, False);
  ++n;
  wDialog = XmCreatePromptDialog (w, "Axis", args, n);
  XtAddCallback (wDialog, XmNokCallback, AxisMinMax_GetValue,
                 (XtPointer) pValue);
  XtAddCallback (wDialog, XmNcancelCallback, XtDestroyWidget, NULL);
  XtSetSensitive (XmSelectionBoxGetChild (wDialog, XmDIALOG_HELP_BUTTON),
                  False);
  XtManageChild (wDialog);
  XtPopup (XtParent (wDialog), XtGrabNone);

  XmStringFree (Str);
  XmStringFree (Str1);

  return;
}


void AxisMinMax_GetValue (Widget w, XtPointer client_data,
                          XtPointer call_data)
{
  char *pTmp;
  float *pFvalue = (float *) client_data;
  float fNewValue;
  XmSelectionBoxCallbackStruct *cbs =
    (XmSelectionBoxCallbackStruct *) call_data;

  if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &pTmp)) {   /* Internal Error */
    return;
  }

  if (pTmp != NULL) {                   /* There is something typed */
    fNewValue = (float) strtod (pTmp, NULL);
    XtFree (pTmp);
    if (fNewValue > 0.0) {              /* Valid Values */
      *pFvalue = fNewValue;
      PlotReHash ();
      UpdateManagerWindow_CPGPlotWindow (FWall.pMainWindow);
    }
  }

  return;
}


void AxisLogLin (Widget w, XtPointer client_data, XtPointer call_data)
{
  Bool *pbIsLog = (Bool *) client_data;
  XmToggleButtonCallbackStruct *cbs =
    (XmToggleButtonCallbackStruct *) call_data;

  if (cbs->set == True)                 /* Log toggle button is checked */
    *pbIsLog = True;
  else
    *pbIsLog = False;

  PlotReHash ();
  UpdateManagerWindow_CPGPlotWindow (FWall.pMainWindow);

  return;
}



void ToggleSensitivity (Widget w, XtPointer client_data, XtPointer call_data)
{
  Widget wTmp = (Widget) client_data;

  XtSetSensitive (wTmp, (XtIsSensitive (wTmp) == True ? False : True));

  return;
}


void TurnOffSashTraversal (Widget paned)
{
  Widget *children;
  int num_children;

  XtVaGetValues (paned, XmNchildren, &children, XmNnumChildren, &num_children,
                 NULL);
  while (num_children-- > 0)
    if (XmIsSash (children[num_children]))
      XtVaSetValues (children[num_children], XmNtraversalOn, False, NULL);

/*
    if(XtIsSubclass(children[num_children],xmSashWidgetClass))
      XtVaSetValues(children[num_children],XmNtraversalOn,False,NULL);
*/

  return;
}

void Exit (Widget w, XtPointer client_data, XtPointer call_data)
{
  int nStatus = (int) client_data;

  exit (nStatus);

  return;
}


void DumpPacketId (PacketId * pPktId)
{

  if (pPktId->nType & CasSciLFDR)
    fprintf (stderr, "LFDR %s, Event Seconds %08X\n",
             Cas_LFDRnModeToString (pPktId->nMode), pPktId->nEvtSec);
  if (pPktId->nType & CasSciMFR)
    fprintf (stderr, "MFR %s\n",
             Cas_MFRnModeToString (pPktId->nMode), pPktId->nEvtSec);
  if (pPktId->nType & CasSciHFR)
    fprintf (stderr, "MFR %s\n",
             Cas_HFRnModeToString (pPktId->nMode), pPktId->nEvtSec);

  fprintf (stderr, "PacketId=%p, nType=%08X, nMode=%0X, nEvtSec=%08X\n",
           pPktId, pPktId->nType, pPktId->nMode, pPktId->nEvtSec);

  return;
}



void OnDraw_LFDR (CPGLine * pLine)
{
  int i, nOldLen;
  float *pXold, *pYold, fXar[256], fYar[256], *pX, *pY;
  PacketId *pLineId = (PacketId *) pLine->pUserData;

  if (!pLine)
    return;

  /*
   * Plot all bands and channels of the mfr with dots 
   */
  if (pLineId->nMode & CasAntEx)
    pLine->color_index = 1;             /* White */
  else if (pLineId->nMode & CasAntEz)
    pLine->color_index = 2;             /* Red */
  else if (pLineId->nMode & CasAntBx)
    pLine->color_index = 3;             /* */
  else if (pLineId->nMode & CasAntBy)
    pLine->color_index = 4;             /* */
  else if (pLineId->nMode & CasAntBz)
    pLine->color_index = 5;             /* */
  else if (pLineId->nMode & CasAntLMRp)
    pLine->color_index = 6;             /* */
  else if (pLineId->nMode & CasAntLMRm)
    pLine->color_index = 7;             /* */
  else if (pLineId->nMode & CasAntLPs)
    pLine->color_index = 8;             /* */
  else
    pLine->color_index = 9;             /* */

  pLine->line_style = 1;                /* no line to be drawn */
  pLine->plot_symbol = 0;               /* Use a small dot to mark the point */
  DrawPoints (pLine);

  return;
}


void OnDraw_MFR (CPGLine * pLine)
{
  int i, nOldLen;
  float *pXold, *pYold, fXar[256], fYar[256], *pX, *pY;
  PacketId *pLineId = (PacketId *) pLine->pUserData;

  if (!pLine)
    return;

  /*
   * Plot all bands and channels of the mfr with dots 
   */
  if (pLineId->nMode & CasAntEx)
    pLine->color_index = 1;             /* White */
  else if (pLineId->nMode & CasAntEz)
    pLine->color_index = 2;             /* Red */
  else if (pLineId->nMode & CasAntBx)
    pLine->color_index = 3;             /* */
  else if (pLineId->nMode & CasAntBz)
    pLine->color_index = 5;             /* */
  else
    pLine->color_index = 6;             /* */

  pLine->line_style = 0;                /* no line to be drawn */
  pLine->plot_symbol = -1;              /* Use a small dot to mark the point */
  DrawPoints (pLine);

  /*
   * Save line context 
   */
  nOldLen = pLine->length;
  pXold = pLine->x;
  pYold = pLine->y;

  /*
   * Allocate some scratch space 
   */
  pLine->x = fXar;
  pLine->y = fYar;


  pX = pXold;
  pY = pYold;
  /*
   * Average MFR Band 1, make the line red 
   */
  if (pLineId->nMode & CasMFR_Band1) {
    for (i = 0; i < 16; i++) {
      fXar[i] = pX[i];
      fYar[i] = (pY[i] + pY[i + 16]) / 2.0;
    }
    pLine->length = 16;

    pLine->color_index = 2;             /* Red */
    pLine->line_style = 1;              /* no line to be drawn */
    pLine->plot_symbol = 0;             /* no plot symbol */
    DrawPoints (pLine);
  }

  /*
   * Average MFR Band 2, make the line ??? 
   */
  pX = pXold + 32;
  pY = pYold + 32;
  if (pLineId->nMode & CasMFR_Band2) {
    for (i = 0; i < 32; i++) {
      fXar[i] = pX[i];
      fYar[i] = (pY[i] + pY[i + 32]) / 2.0;
    }
    pLine->length = 32;

    pLine->color_index = 3;             /* ??? */
    pLine->line_style = 1;              /* no line to be drawn */
    pLine->plot_symbol = 0;             /* no plot symbol */
    DrawPoints (pLine);
  }

  /*
   * Average MFR Band 3, make the line ??? 
   */
  pX = pXold + 96;
  pY = pYold + 96;
  if (pLineId->nMode & CasMFR_Band3) {
    for (i = 0; i < 32; i++) {
      fXar[i] = pX[i];
      fYar[i] = (pY[i] + pY[i + 32] + pY[i + 64] + pY[i + 96]) / 4.0;
    }
    pLine->length = 32;

    pLine->color_index = 4;             /* ??? */
    pLine->line_style = 1;              /* no line to be drawn */
    pLine->plot_symbol = 0;             /* no plot symbol */
    DrawPoints (pLine);
  }



/*
  pLine->color_index=2;
  pLine->plot_symbol=0;
  pLine->line_style=1;
  pLine->length=16;	
  for(i=0;i<16;i++){
    pLine->y[i]=(pLine->y[i]+pLine->y[i+16])/2.0;		
    pLine->x[i]=i;
  }
  DrawPoints(pLine);
  
  pLine->color_index=3;
  pLine->length=32;
  for(i=0;i<32;i++){
    pLine->y[i]=(pLine->y[i+32]+pLine->y[i+64])/2.0;
    pLine->x[i]=i+16;
  }
  DrawPoints(pLine);
  
  pLine->color_index=4;
  pLine->length=32;
  for(i=0;i<32;i++){
    pLine->y[i]=(pLine->y[i+96]+pLine->y[i+128]+pLine->y[i+160]+pLine->y[i+192])/4.0;
    pLine->x[i]=i+48;
    }
  DrawPoints(pLine);

*/

  /*
   * Restore line context 
   */
  pLine->length = nOldLen;
  pLine->x = pXold;
  pLine->y = pYold;

  return;
}


void OnDraw_HFR (CPGLine * pLine)
{
  int i, nOldLen;
  float *pXold, *pYold, fXar[256], fYar[256], *pX, *pY;
  PacketId *pLineId = (PacketId *) pLine->pUserData;

  if (!pLine)
    return;

  /*
   * Plot all bands and channels of the hfr with dots 
   */
  if (pLineId->nMode & CasAntExp)
    pLine->color_index = 10;            /* White */
  else if (pLineId->nMode & CasAntExm)
    pLine->color_index = 11;            /* Red */
  else if (pLineId->nMode & CasAntEx)
    pLine->color_index = 1;             /* */
  else if (pLineId->nMode & CasAntEz)
    pLine->color_index = 2;             /* */
  else
    pLine->color_index = 12;            /* */

  pLine->line_style = 0;                /* no line to be drawn */
  pLine->plot_symbol = -1;              /* Use a small dot to mark the point */
  DrawPoints (pLine);

  /*
   * Save line context 
   */
  nOldLen = pLine->length;
  pXold = pLine->x;
  pYold = pLine->y;

  /*
   * Allocate some scratch space
   * 
   * pLine->x=fXar;
   * pLine->y=fYar;
   * 
   * 
   * pX=pXold;  pY=pYold;
   * if(pLineId->nMode&CasMFR_Band1){
   * for(i=0;i<16;i++){
   * fXar[i]=pX[i];
   * fYar[i]=(pY[i]+pY[i+16])/2.0;              
   * }
   * pLine->length=16;
   * 
   * pLine->color_index=2;              
   * pLine->line_style=1;               
   * pLine->plot_symbol=0;              
   * DrawPoints(pLine);
   * }
   * 
   * pX=pXold+32;  pY=pYold+32;
   * if(pLineId->nMode&CasMFR_Band2){
   * for(i=0;i<32;i++){
   * fXar[i]=pX[i];
   * fYar[i]=(pY[i]+pY[i+32])/2.0;              
   * }
   * pLine->length=32;
   * 
   * pLine->color_index=3;              
   * pLine->line_style=1;                 
   * pLine->plot_symbol=0;              
   * DrawPoints(pLine);
   * }
   * 
   * pX=pXold+96;  pY=pYold+96;
   * if(pLineId->nMode&CasMFR_Band3){
   * for(i=0;i<32;i++){
   * fXar[i]=pX[i];
   * fYar[i]=(pY[i]+pY[i+32]+pY[i+64]+pY[i+96])/4.0;            
   * }
   * pLine->length=32;
   * 
   * pLine->color_index=4;              
   * pLine->line_style=1;                   
   * pLine->plot_symbol=0;              
   * DrawPoints(pLine);
   * }
   */


/*
  pLine->color_index=2;
  pLine->plot_symbol=0;
  pLine->line_style=1;
  pLine->length=16;	
  for(i=0;i<16;i++){
    pLine->y[i]=(pLine->y[i]+pLine->y[i+16])/2.0;		
    pLine->x[i]=i;
  }
  DrawPoints(pLine);
  
  pLine->color_index=3;
  pLine->length=32;
  for(i=0;i<32;i++){
    pLine->y[i]=(pLine->y[i+32]+pLine->y[i+64])/2.0;
    pLine->x[i]=i+16;
  }
  DrawPoints(pLine);
  
  pLine->color_index=4;
  pLine->length=32;
  for(i=0;i<32;i++){
    pLine->y[i]=(pLine->y[i+96]+pLine->y[i+128]+pLine->y[i+160]+pLine->y[i+192])/4.0;
    pLine->x[i]=i+48;
    }
  DrawPoints(pLine);

*/

  /*
   * Restore line context 
   */
  pLine->length = nOldLen;
  pLine->x = pXold;
  pLine->y = pYold;

  return;
}
