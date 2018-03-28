 /*
  * DUST data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define DUST_packet_ID_value		0x0B
#define DUST_packet_ID			0x0000040F
#define DUST_minipacket_length		0x000100FF
#define DUST_minipacket_length_MSB	0x0000000F

#define DUST_minipacket_RTI		0x000200FF
#define DUST_minipacket_RTI_MSB		0x000300FF

#define DUST_minipacket_size		0x0004040F
#define DUST_minipacket_segment		0x0004000F

#define DUST_gain			0x00050007
#define DUST_timeout_flag		0x00050401
#define DUST_time_flag			0x00050501
#define DUST_time_inaccurate		0x00050501
#define DUST_AGC_flag			0x00050601
#define DUST_frequency_band		0x00050701

#define DUST_antenna			0x00060007
#define DUST_MSF			0x00060301
#define DUST_More_Shit_Follows		0x00060301
#define DUST_More_Status_Follows	0x00060301
#define DUST_compression		0x0006040F

#define DUST_AGC_value			0x000700FF

#define DUST_LP_DAC_0			0x000800FF
#define DUST_HFR_translate		0x000800FF
#define DUST_sub_RTI			0x000900FF
