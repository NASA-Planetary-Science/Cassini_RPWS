#include <stdio.h> 
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
 
#include "telemetry.h"
#include "instrument.h"

char *ExtractMiniPacketStatus(TELEMETRY *tlm,INSTRUMENT *inst)
{
char *perrmsg=NULL;
static char errmsg[128];
UNSEGMENTED_MINI_PACKET *usmp=&(tlm->packet.usmp);


  inst->cdsclk.seconds=get_telemetry_packet_cds_time(tlm);	/* SpaceCraft Clock */
  inst->rti=get_mini_packet_rti(usmp);				/* Mini-Packet Real Time Interrupt */
  inst->evtclk.seconds= ( (inst->cdsclk.seconds - ((inst->rti>>3)&0x1FFF) )&0xFFFFE000) | ((inst->rti>>3)&0x1FFF);
  inst->evtclk.milliseconds=(inst->rti&0x0007)*125;
  inst->packet_length=get_telemetry_packet_length(tlm);		/* Length may be larger than Mini-Packet will allow */
	
  switch(get_mini_packet_type(usmp)){
    case MINI_PACKET_TYPE_hfr :
      inst->data_type=Instrument_HFR;
      inst->packet_length=0;
      inst->capture_length=0;
      inst->raw.length=0;
      inst->gain=0;
      break;
    case MINI_PACKET_TYPE_lfdr :
      inst->capture_length=(0x0100<<get_mp_lfdr_size(usmp));	/* 0=256,1=512,2,=1024 */
      inst->raw.length=inst->packet_length-6;
      if((inst->band=get_mp_lfdr_band(usmp))==1){
        inst->data_type=Instrument_LFDRhighBand;
        inst->capture_duration=inst->capture_length*WFR_HIGH_BAND_SAMPLING_PERIOD;
        }
      else{
        inst->data_type=Instrument_LFDRlowBand;
        inst->capture_duration=inst->capture_length*WFR_LOW_BAND_SAMPLING_PERIOD;
        }
      inst->gain=10*get_mp_lfdr_gain(usmp);     
      inst->dgf=get_lfdr_dgf(usmp);      
      switch(get_mp_lfdr_channel(usmp)){
          case 0 :
            if(get_mp_lfdr_antenna(usmp))	inst->data_type|=Antenna_ExLo;
            else				inst->data_type|=Antenna_LMRp;
            break;
          case 1 :
            if(get_mp_lfdr_antenna(usmp))	inst->data_type|=Antenna_EzLo;
            else				inst->data_type|=Antenna_LMRm;
            break;
          case 2 :
            if(get_mp_lfdr_antenna(usmp))	inst->data_type|=Antenna_LP;
            else				inst->data_type|=Antenna_Bx;
            break;
          case 3 :
            inst->data_type|=Antenna_By;
            break;
          case 4 :
            inst->data_type|=Antenna_Bz;
            break;
          default :
            sprintf(errmsg,"Bad LFDR MP Header, channel (%03X)\n",get_mp_lfdr_channel(usmp));
            break;
          }
      break;
    case MINI_PACKET_TYPE_lp :	
      inst->capture_length=(inst->packet_length-10)/2;		/* ten bytes of header, twelve bit samples */
      inst->raw.length=inst->capture_length;
      inst->capture_duration=inst->capture_length*LP_SAMPLING_PERIOD;
      inst->band=get_mp_lp_band(usmp);
      inst->gain=0;
      switch(get_mp_lp_mode(usmp)){
        case MP_MODE_LP_sweep :    inst->data_type=Instrument_LPsweep;    break;
        case MP_MODE_LP_density :  inst->data_type=Instrument_LPdensity;  break;
        case MP_MODE_LP_analyzed : inst->data_type=Instrument_LPanalyzed; break;
        case MP_MODE_LP_tbd :      inst->data_type=Instrument_LPtbd;      break;
        default :        
          inst->data_type=Instrument_LP;
          sprintf(errmsg,"Bad LP MP Header, mode bits\n");
          break;
        }
      switch(get_mp_lp_antenna(usmp)){
        case 0x00 : inst->data_type|=Antenna_LMRp; break;
        case 0x01 : inst->data_type|=Antenna_LMRm; break;
        case 0x02 : /* A1 A0, if A1=1 => sphere else A1=0 => cylinder */
        case 0x03 : inst->data_type|=Antenna_LP;   break;
        default :
          sprintf(errmsg,"Bad LP MP Header, antenna\n");
          break;
        }
      break;
    case MINI_PACKET_TYPE_mfr :	
      inst->capture_length=inst->packet_length-5;	/* five bytes of header */		
      inst->raw.length=inst->capture_length;
      inst->capture_duration=MFR_CYCLE_PERIOD;		/* 32 Seconds */
      inst->data_type=Instrument_MFR;
      inst->gain=0;
      switch(get_mp_mfr_antenna(usmp)){
        case 0 : inst->data_type|=Antenna_ExLo; break;
        case 1 : inst->data_type|=Antenna_EzLo; break;
        case 2 : inst->data_type|=Antenna_Bx;   break;
        case 3 : inst->data_type|=Antenna_Bz;   break;
        default :
          sprintf(errmsg,"Bad MFR MP Header, antenna\n");
          break;
        }
      break;
    case MINI_PACKET_TYPE_wbr :
      if(get_mp_wbr_msf(usmp))  inst->capture_length=inst->packet_length-10;	/* ten bytes of header */
      else                      inst->capture_length=inst->packet_length-8;  	/* eight bytes of header */
      inst->raw.length=inst->capture_length;
      if((inst->band=get_mp_wbr_band(usmp))==1){
        inst->data_type=Instrument_WBRhighBand;
        inst->capture_duration=inst->capture_length*WBR_HIGH_BAND_SAMPLING_PERIOD;	
        }
      else{
        inst->data_type=Instrument_WBRlowBand;
        inst->capture_duration=inst->capture_length*WBR_LOW_BAND_SAMPLING_PERIOD;
        }
      inst->gain=10*get_mp_wbr_gain(usmp);
      switch(get_mp_wbr_antenna(usmp)){
        case 0 : inst->data_type|=Antenna_ExLo; break;
        case 1 : inst->data_type|=Antenna_Bx;   break;
        case 2 : inst->data_type|=Antenna_EzLo; break;
        case 3 : inst->data_type|=Antenna_HF;   break;
        case 4 : inst->data_type|=Antenna_LP;   break;
        default :
          sprintf(errmsg,"Bad WBR MP Header, antenna\n");
          break;
        }
      break;
    case MINI_PACKET_TYPE_wfr :	
      if(get_mp_wfr_msf(usmp))  inst->capture_length=(inst->packet_length-10)/2;	/* ten bytes of header */
      else                      inst->capture_length=(inst->packet_length-8)/2;  	/* eight bytes of header */
      inst->raw.length=inst->capture_length;
      if((inst->band=get_mp_wfr_band(usmp))==1){
        inst->data_type=Instrument_WFRhighBand;
        inst->capture_duration=inst->capture_length*WFR_HIGH_BAND_SAMPLING_PERIOD;
        }
      else{
        inst->data_type=Instrument_WFRlowBand;
        inst->capture_duration=inst->capture_length*WFR_LOW_BAND_SAMPLING_PERIOD;
        }
      switch(get_mp_wfr_channel(usmp)){
        case 0 :
          if(get_mp_wfr_antenna(usmp)&0x01)  	inst->data_type|=Antenna_ExLo;
          else					inst->data_type|=Antenna_LMRp;
          inst->gain=get_mp_wfr_gain_ch0(usmp)*10;		/* dB */
          break;
        case 1 :
          if(get_mp_wfr_antenna(usmp)&0x02) 	inst->data_type|=Antenna_EzLo;
          else					inst->data_type|=Antenna_LMRm;
          inst->gain=get_mp_wfr_gain_ch1(usmp)*10;		/* dB */
          break;
        case 2 :
          if(get_mp_wfr_antenna(usmp)&0x04)  	inst->data_type|=Antenna_LP;
          else					inst->data_type|=Antenna_Bx;
          inst->gain=get_mp_wfr_gain_ch234(usmp)*10;		/* dB */
          break;
        case 3 :
          inst->data_type|=Antenna_By;
          inst->gain=get_mp_wfr_gain_ch234(usmp)*10;	/* dB */
          break;
        case 4 :
          inst->data_type|=Antenna_Bz;
          inst->gain=get_mp_wfr_gain_ch234(usmp)*10;	/* dB */
          break;
        case 7 :	/* Interleaved Data */
          break;
        default :
          sprintf(errmsg,"Bad WFR MP Header, channel (%03X)\n",get_mp_wfr_channel(usmp));
          break;
        }
      break;						
    case MINI_PACKET_TYPE_stim :		
    case MINI_PACKET_TYPE_3 :							
    case MINI_PACKET_TYPE_5 :					
    case MINI_PACKET_TYPE_6 :					
    case MINI_PACKET_TYPE_9 :						
    case MINI_PACKET_TYPE_10 :					
    case MINI_PACKET_TYPE_dust :						
    case MINI_PACKET_TYPE_12 :					
    case MINI_PACKET_TYPE_mro :						
    case MINI_PACKET_TYPE_fill :
    default :
      inst->data_type=0;
      inst->packet_length=0;
      inst->capture_length=0;
      inst->raw.length=0;
      inst->gain=0;
      break;						
    }

return perrmsg;
}



int32_t ExtractMiniPacketData(TELEMETRY *tlm,float *data)
{
int32_t length,lmax,l;
UNSEGMENTED_MINI_PACKET *usmp=&(tlm->packet.usmp);


  lmax=get_telemetry_packet_length(tlm);
  if((lmax<0) || (lmax>66000)){
    return 0;				/* trap for bad lengths error condition */
    }

  length=0;
  switch(get_mini_packet_type(usmp)){
    case MINI_PACKET_TYPE_hfr :
      break;
    case MINI_PACKET_TYPE_lfdr :
      for(l=6;l<lmax;l++)
        data[length++]=(float)(usmp->Data[l]);
/*        data[length++]=(float)( get_lfdr_value((usmp->Data[l])) ); */
      break;
    case MINI_PACKET_TYPE_lp :	
      for(l=10;l<lmax;l+=2)
        data[length++]=(float)( usmp->Data[l] | ((usmp->Data[l+1]<<8)&0xFF00) );
      break;
    case MINI_PACKET_TYPE_mfr :	
      for(l=5;l<lmax;l++)
        data[length++]=(float)(usmp->Data[l]);
      break;
    case MINI_PACKET_TYPE_wbr :
      for(l=get_mp_wbr_msf(usmp)?10:8;l<lmax;l++)
        data[length++]=(float)(usmp->Data[l]);
      break;
    case MINI_PACKET_TYPE_wfr :	
      for(l=get_mp_wfr_msf(usmp)?10:8;l<lmax;l+=2)
        data[length++]=(float)( usmp->Data[l] | ((usmp->Data[l+1]<<8)&0xFF00) );
      break;						
    case MINI_PACKET_TYPE_stim :		
    case MINI_PACKET_TYPE_3 :							
    case MINI_PACKET_TYPE_5 :					
    case MINI_PACKET_TYPE_6 :					
    case MINI_PACKET_TYPE_9 :						
    case MINI_PACKET_TYPE_10 :					
    case MINI_PACKET_TYPE_dust :						
    case MINI_PACKET_TYPE_12 :					
    case MINI_PACKET_TYPE_mro :						
    case MINI_PACKET_TYPE_fill :
    default :
      break;						
    }

return length;
}




char *get_receiver_string(int32_t x)
{
static char pch[16];

  switch(x&Instrument_Mask){
    case Instrument_HFR :	strcpy(pch,"HFR");	break;
    case Instrument_HFRa :	strcpy(pch,"HFR A");	break;
    case Instrument_HFRb :	strcpy(pch,"HFR B");	break;
    case Instrument_HFRc :	strcpy(pch,"HFR C");	break;
    case Instrument_HFRhf1 :	strcpy(pch,"HFR HF1");	break;
    case Instrument_HFRhf2 :	strcpy(pch,"HFR HF2");	break;
    case Instrument_LFDR :	strcpy(pch,"LFDR");	break;
    case Instrument_LFDRlowBand :strcpy(pch,"LFDR");	break;
    case Instrument_LFDRhighBand :strcpy(pch,"LFDR");	break;
    case Instrument_LPsweep :	
    case Instrument_LPdensity :	
    case Instrument_LPanalyzed :
    case Instrument_LPtbd :
    case Instrument_LP :	strcpy(pch,"LP");	break;
    case Instrument_MFR :	strcpy(pch,"MFR");	break;
    case Instrument_MFRband1 :	strcpy(pch,"MFR 1");	break;
    case Instrument_MFRband2 :	strcpy(pch,"MFR 2");	break;
    case Instrument_MFRband3 :	strcpy(pch,"MFR 3");	break;
    case Instrument_WBRlowBand :	
    case Instrument_WBRhighBand :	
    case Instrument_WBR :	strcpy(pch,"WBR");	break;	
    case Instrument_WFRlowBand :	
    case Instrument_WFRhighBand :
    case Instrument_WFR :	strcpy(pch,"WFR");	break;
    default :			strcpy(pch,"VDR"); 	break;
    }

return pch;
}

char *get_antenna_string(int32_t x)
{
static char pch[16];

  switch(x&Antenna_Mask){
    case Antenna_ExLo :	strcpy(pch,"ExLo");	break;
    case Antenna_EzLo :	strcpy(pch,"EzLo");	break;
    case Antenna_Bx :	strcpy(pch,"Bx");	break;
    case Antenna_By :	strcpy(pch,"By");	break;
    case Antenna_Bz :	strcpy(pch,"Bz");	break;
    case Antenna_LMRp :	strcpy(pch,"LMR+");	break;
    case Antenna_LMRm :	strcpy(pch,"LMR-");	break;
    case Antenna_LP :	strcpy(pch,"LP");	break;
    case Antenna_HF :	strcpy(pch,"HF");	break;
    default :		strcpy(pch,"Radiated"); break;
    }

return pch;
}



INSTRUMENT *CreateNewInstrument(void)
{
INSTRUMENT *pNew;

  if((pNew=(INSTRUMENT*)malloc(sizeof(INSTRUMENT)))==NULL){
    fprintf(stderr,"Error: CreateNewInstrument() malloc failed\n");
    return NULL;
    }
  pNew->next=NULL;
  pNew->previous=NULL;
  pNew->data_type=0;
  pNew->capture_length=0;
  pNew->raw.length=0;
  pNew->raw.data=NULL;
  pNew->fft.length=0;
  pNew->fft.real=NULL;
  pNew->fft.imag=NULL;

return pNew;
}
