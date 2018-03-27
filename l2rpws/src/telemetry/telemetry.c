#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <telemetry.h>


/* Swap 4 bytes in place */
int32_t be_to_native_si32(int32_t in)
{
	unsigned char* p = (unsigned char*)(&in);
	
	return (p[0] << 24) + (p[1] << 16) + (p[2] << 8) + p[3];
}

int32_t get_telemetry_packet_length(TELEMETRY* p){
	return be_to_native_si32(p->status.packet_length) + 3;
}
  
int32_t get_telemetry_packet_cds_time(TELEMETRY* p){
	return be_to_native_si32(p->status.cds_time);
}


char *get_reason_code_string(uint32_t reason_code)
{
static char reason[128];

  /* Parse for the deployment software version */
  if((reason_code&0xFF00)==0x5600){ 			/* 'V' */
    sprintf(reason,"Antenna Flight V%u.%u",((reason_code>>4)&0x0F),(reason_code&0x0F));
    }
  else if((reason_code&0xFF00)==0x5400){		/* 'T' */
    sprintf(reason,"Antenna Test V%u.%u",((reason_code>>4)&0x0F),(reason_code&0x0F));
    }
  else if((reason_code&0xFF00)==0x5800){		/* 'X' */
    sprintf(reason,"Antenna DEBUG V%u.%u",((reason_code>>4)&0x0F),(reason_code&0x0F));
    }
  else{
    switch(reason_code&0x0FFF){
      case 0x0901 :  strcpy(reason,"Bad Command");  	    break;
      case 0x0002 :  strcpy(reason,"Check Stuck Element");  break;
      case 0x0003 :  strcpy(reason,"Drop During Go");  	    break;
      case 0x0005 :  strcpy(reason,"Drop Before Go");  	    break;
      case 0x0104 :  strcpy(reason,"Attempted to Clear Relay");  break;
      case 0x0106 :  strcpy(reason,"On Time Limit");  		 break;
      case 0x0107 :  strcpy(reason,"Extend Limit");    		 break;
      case 0x0108 :  strcpy(reason,"Retract Limit"); 		 break;
      case 0x0109 :  strcpy(reason,"Retract/Extend Limit");      break;
      case 0x0110 :  strcpy(reason,"Current Drop");  		 break;
      case 0x0111 :  strcpy(reason,"Position Drop"); 		 break;
      case 0x0112 :  strcpy(reason,"Element Stop"); 		 break;
      case 0x0113 :  strcpy(reason,"Hi Temperature Drop");	 break;
      case 0x0114 :  strcpy(reason,"Low Temperature Drop");      break;
      case 0x0115 :  strcpy(reason,"BIU Discrete Drop");  	 break;
      case 0x0116 :  strcpy(reason,"BIU Sleep Drop");     	 break;
      case 0x02F0 :  strcpy(reason,"Hold");                    break;
      case 0x02FD :  strcpy(reason,"Attempting Cageing Pin");  break;
      case 0x02FE :  strcpy(reason,"Attempting Extend");       break;
      case 0x02FF :  strcpy(reason,"Attempting Retract");      break;
      case 0x03F1 :  strcpy(reason,"Setup");   break;
      case 0x03F2 :  strcpy(reason,"Status");  break;
      default     :  sprintf(reason,"%04X",reason_code);
      }
    }

return reason;
}

