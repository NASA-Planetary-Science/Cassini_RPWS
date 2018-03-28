 /*
  * WBR data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define WBR_packet_ID_value		0x0E
#define WBR_packet_ID                   0x0000040F
#define WBR_minipacket_length		0x000100FF
#define WBR_minipacket_length_MSB	0x0000000F

#define WBR_minipacket_RTI		0x000200FF
#define WBR_minipacket_RTI_MSB		0x000300FF

#define WBR_minipacket_size		0x0004040F
#define WBR_minipacket_segment		0x0004000F

#define WBR_gain		0x00050007
#define WBR_timeout_flag	0x00050401
#define WBR_time_flag		0x00050501
#define WBR_time_inaccurate	0x00050501
#define WBR_AGC_flag		0x00050601
#define WBR_frequency_band	0x00050701
#define WBR_BAND_10_KHZ		0
#define WBR_BAND_80_KHZ		1

#define WBR_antenna		0x00060007
#define WBR_ANTENNA_Ex		0
#define WBR_ANTENNA_Bx		1
#define WBR_ANTENNA_Ez		2
#define WBR_ANTENNA_HF		3
#define WBR_ANTENNA_LP		4
#define WBR_MSF			0x00060301
#define WBR_More_Shit_Follows	0x00060301
#define WBR_More_Status_Follows	0x00060301
#define WBR_compression		0x0006040F

#define WBR_AGC_value		0x000700FF

#define WBR_LP_DAC_0		0x000800FF
#define WBR_HFR_translate	0x000800FF
#define WBR_sub_RTI		0x000900FF
