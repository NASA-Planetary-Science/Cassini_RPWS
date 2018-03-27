 /*
  * WFR data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define WFR_packet_ID_value		0x08
#define WFR_packet_ID			0x0000040F
#define WFR_minipacket_length		0x000100FF
#define WFR_minipacket_length_MSB	0x0000000F

#define WFR_minipacket_RTI		0x000200FF
#define WFR_minipacket_RTI_MSB		0x000300FF

#define WFR_minipacket_size		0x0004040F
#define WFR_minipacket_segment		0x0004000F

#define WFR_CH0_gain		0x00050003
#define WFR_CH1_gain		0x00050203
#define WFR_CH234_gain		0x00050403

#define WFR_lp_mode		0x00050601
#define WFR_frequency_band	0x00050701
#define WFR_BAND_26HZ		0
#define WFR_BAND_2500HZ		1

#define WFR_antenna		0x00060007
#define WFR_CH0_antenna		0x00060001
#define WFR_CH1_antenna		0x00060101
#define WFR_CH2_antenna		0x00060201
#define WFR_MSF			0x00060301
#define WFR_More_Shit_Follows	0x00060301
#define WFR_More_Status_Follows	0x00060301
#define WFR_compression		0x0006040F

#define WFR_channel_mode	0x00070007
#define WFR_channel_number	0x00070307
#define WFR_Walsh_DGF		0x00070603
#define WFR_TOGGLE_mode		0x00070701
#define WFR_AGC_enabled		0x00070601

#define WFR_LP_DAC_0		0x000800FF
#define WFR_LP_DAC_1		0x000900FF
