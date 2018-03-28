/*
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
*/

#include <stdio.h>
#include <string.h>

#include "cashfr.h"





char* CasHfr_sFormatMiniPacketHeader(unsigned char *p)
{
static char sHeader[1024],sTmp[512];

char sType[16],sCompression[16],sErrorFlag[16];
char sSounderRly[16],sClampRly[16],sMFRExSw[16],sMFREzSw[16];
int nCnt;
char sPacketHeaderFormat[]=
"Packet Type: %11s     Sounder Relay:   %9s\n" 
"Packet Size: %11d     Clamp Relay:     %9s\n" 
"Compression: %11s     MFR Ex Switches: %9s\n" 
"Header Version: %8d     MFR Ez Switch:   %9s\n" 
"Error Flag:  %11s\n";



 
 
  /* Header Information Common to All */
  if(CasHfr_bAnalysis(p))          strcpy(sType,"Analysis");
  else if(CasHfr_bSounder(p))      strcpy(sType,"Sounder");
  else if(CasHfr_bCalibration(p))  strcpy(sType,"Calibration");
  else if(CasHfr_bMsMode(p))       strcpy(sType,"MsMode");
  else                             strcpy(sType,"???");
  
  if(CasHfr_bCompression(p)==False)   strcpy(sCompression,"None");
  else if(CasHfr_bMeander(p))         strcpy(sCompression,"Meander");
  else                                strcpy(sCompression,"???");
  
  if(CasHfr_bErrorFlag(p))         strcpy(sErrorFlag,"Set");
  else                             strcpy(sErrorFlag,"Clear");
  
  if(CasHfr_bSounderRelayOn(p))       strcpy(sSounderRly,"On");
  else                                strcpy(sSounderRly,"Off");
  
  if(CasHfr_bClampRelayClamped(p))    strcpy(sClampRly,"Clamped");
  else                                strcpy(sClampRly,"Unclamped");
  
  if(CasHfr_bMfrExOff(p))     strcpy(sMFRExSw,"Off");
  else if(CasHfr_bMfrExp(p))  strcpy(sMFRExSw,"+");
  else if(CasHfr_bMfrExm(p))  strcpy(sMFRExSw,"-");
  else if(CasHfr_bMfrEx(p))   strcpy(sMFRExSw,"+/-");
  else                        strcpy(sMFRExSw,"???");
  
  if(CasHfr_bMfrEz(p))        strcpy(sMFREzSw,"On");
  else                        strcpy(sMFREzSw,"Off");
  
  
  sprintf(sHeader,sPacketHeaderFormat, sType,sSounderRly, 
          CasHfr_nBytesInPacket(p),sClampRly,sCompression,sMFRExSw, 
          CasHfr_HeaderVersion(p),sMFREzSw,sErrorFlag);


  
  if(CasHfr_bAnalysis(p)){
  char  arBandHeaderFormat[256]="%-5s %-5s %-3s %-3s %-6s %-6s %-6s %-6s %-6s %-6s %-6s %-7s\n";
  char arBandDisplayFormat[256]="%-5s %-5s %-3s %-3s %-6s %-6s %-6s %-6s %-6s %-6s %-6s %-7s\n";
  char sBand[16],sDirFind[16],sExAnt[16],sEzAnt[16];
  char sRepeat[16],sAutos[16],sCross[16];
  char sFilter[16],sStep[16],sStart[16],sDelta[16],sIntTime[16];


    sprintf(sTmp,arBandHeaderFormat,"Band","dFind","Ex","Ez","Repeat","Autos",
            "Cross","Filter","Steps","Start","Delta","Int(mS)");  
    strcat(sHeader,sTmp);

    /* hfr bands abc */
    if(CasHfr_bBandA(p))  strcat(sBand,"A");
    if(CasHfr_bBandB(p))  strcat(sBand,"B");
    if(CasHfr_bBandC(p))  strcat(sBand,"C");
    if(CasHfr_bBandsABC(p)==False)  strcat(sBand,"noABC");

    if(CasHfr_bDirectionFindingABC(p))  strcpy(sDirFind,"On");
    else                                strcpy(sDirFind,"Off");
    
    nCnt=(int)(CasHfr_fIntegrationTimeABC(p)/1000.0);  /* in mSec */
    sprintf(sIntTime,"%d",nCnt);

    switch(CasHfr_ExAntSwitchABC(p)){
      case 0x00 :  strcpy(sExAnt,"Off");  break;
      case 0x01 :  strcpy(sExAnt,"+");    break;
      case 0x02 :  strcpy(sExAnt,"-");    break;
      case 0x03 :  strcpy(sExAnt,"+/-");  break;
      default         :  strcpy(sExAnt,"???");  break;
    }
    if(CasHfr_bEzABC(p))  strcpy(sEzAnt,"On");  
    else                  strcpy(sEzAnt,"Off");    

    sprintf(sRepeat,"%d",CasHfr_nRepeatCountABC(p)*CasHfr_nRepeatCountAll(p));

    if(CasHfr_bAutoABC(p))  strcpy(sAutos,"On");
    else                     strcpy(sAutos,"Off");

    if(CasHfr_bCrossABC(p))  strcpy(sCross,"On");
    else                     strcpy(sCross,"Off");

    sprintf(sFilter,"%d",CasHfr_nFiltersABC(p));
    sprintf(sStep,"%d",0);
    sprintf(sStart,"%d",0);
    sprintf(sDelta,"%d",0);
    
    sprintf(sTmp,arBandDisplayFormat,sBand,sDirFind,sExAnt,sEzAnt,sRepeat,
            sAutos,sCross,sFilter,sStep,sStart,sDelta,sIntTime);  
    strcat(sHeader,sTmp);
    /* end hfr bands abc */



    /* hfr band hf1 */
    if(CasHfr_bBandHF1(p))  strcat(sBand,"HF1");
    else                    strcat(sBand,"noHF1");

    if(CasHfr_bDirectionFindingHF1(p))  strcpy(sDirFind,"On");
    else                                strcpy(sDirFind,"Off");
     
    nCnt=(int)(CasHfr_fIntegrationTimeHF1(p)/1000.0);  /* in mSec */
    sprintf(sIntTime,"%d",nCnt);

    switch(CasHfr_ExAntSwitchHF1(p)){
      case 0x00 :  strcpy(sExAnt,"Off");  break;
      case 0x01 :  strcpy(sExAnt,"+");    break;
      case 0x02 :  strcpy(sExAnt,"-");    break;
      case 0x03 :  strcpy(sExAnt,"+/-");  break;
      default         :  strcpy(sExAnt,"???");  break;
    }
    if(CasHfr_bEzHF1(p))  strcpy(sEzAnt,"On");  
    else                  strcpy(sEzAnt,"Off");    

    sprintf(sRepeat,"%d",CasHfr_nRepeatCountHF1(p)*CasHfr_nRepeatCountAll(p));
    if(CasHfr_bAutoHF1(p))  strcpy(sAutos,"On");
    else                     strcpy(sAutos,"Off");
    if(CasHfr_bCrossHF1(p))  strcpy(sCross,"On");
    else                     strcpy(sCross,"Off");
    sprintf(sFilter,"%d",CasHfr_nFiltersHF1(p));
    sprintf(sStep,"%d",CasHfr_NumberStepsHF1(p));  
    sprintf(sStart,"%d",CasHfr_StartFrequencyHF1(p));
    sprintf(sDelta,"%d",CasHfr_FrequencyStepHF1(p));
    
    sprintf(sTmp,arBandDisplayFormat,sBand,sDirFind,sExAnt,sEzAnt,sRepeat,
            sAutos,sCross,sFilter,sStep,sStart,sDelta,sIntTime);  
    strcat(sHeader,sTmp);
    /* end hfr band hf1 */



    /* hfr band hf2 */
    if(CasHfr_bBandHF2(p))  strcat(sBand,"HF2");
    else                    strcat(sBand,"noHF2");

    if(CasHfr_bDirectionFindingHF2(p))  strcpy(sDirFind,"On");
    else                                strcpy(sDirFind,"Off");
     
    nCnt=(int)(CasHfr_fIntegrationTimeHF2(p)/1000.0);  /* in mSec */
    sprintf(sIntTime,"%d",nCnt);

    switch(CasHfr_ExAntSwitchHF2(p)){
      case 0x00 :  strcpy(sExAnt,"Off");  break;
      case 0x01 :  strcpy(sExAnt,"+");    break;
      case 0x02 :  strcpy(sExAnt,"-");    break;
      case 0x03 :  strcpy(sExAnt,"+/-");  break;
      default         :  strcpy(sExAnt,"???");  break;
    }
    if(CasHfr_bEzHF2(p))  strcpy(sEzAnt,"On");  
    else                  strcpy(sEzAnt,"Off");    

    sprintf(sRepeat,"%d",CasHfr_nRepeatCountHF2(p)*CasHfr_nRepeatCountAll(p));
    if(CasHfr_bAutoHF2(p))  strcpy(sAutos,"On");
    else                     strcpy(sAutos,"Off");
    if(CasHfr_bCrossHF2(p))  strcpy(sCross,"On");
    else                     strcpy(sCross,"Off");
    sprintf(sFilter,"%d",CasHfr_nFiltersHF2(p));
    sprintf(sStep,"%d",CasHfr_NumberStepsHF2(p));  
    sprintf(sStart,"%d",CasHfr_StartFrequencyHF2(p));
    sprintf(sDelta,"%d",CasHfr_FrequencyStepHF2(p));
    
    sprintf(sTmp,arBandDisplayFormat,sBand,sDirFind,sExAnt,sEzAnt,sRepeat,
            sAutos,sCross,sFilter,sStep,sStart,sDelta,sIntTime);  
    strcat(sHeader,sTmp);
    /* end hfr band hf2 */

  }/* Analysis Header */
  else if(CasHfr_bSounder(p)){
  
  }
  else if(CasHfr_bCalibration(p)){
  
  }
  else if(CasHfr_bMsMode(p)){
/*
    sprintf(sTmp,"Band: %s  Antenna: ",pMs->bIsHF1==True?"HF1":"HF2");  strcat(sHeader,sTmp);
    switch(pMs->nAntenna){
      case CasAntExm :  strcpy(sTmp,"Ex-  ");    break;
      case CasAntExp :  strcpy(sTmp,"Ex+  ");    break;
      case CasAntEx  :  strcpy(sTmp,"Ex+/-  ");  break;
      case CasAntEz  :  strcpy(sTmp,"Ez  ");     break;
      default        :  strcpy(sTmp,"???  ");    break;
    }
    strcat(sHeader,sTmp);
    strcatf(sHeader,"Frequency: %dHz\n",pMs->fFrequency);
    strcatf(sHeader,"Sample Rate: %fSec  Number of Samples=%d\n",pMs->fSampleRate,pMs->nSamples);
*/

  }/* MsMode Header */
  else{

  }

  sprintf(sTmp,"\n"); 
  strcat(sHeader,sTmp);

return sHeader;
}


