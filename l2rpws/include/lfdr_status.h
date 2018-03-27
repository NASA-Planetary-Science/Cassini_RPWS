 /*
  * LFDR data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define LFDR_packet_ID_value		0x07
#define LFDR_packet_ID			0x0000040F
#define LFDR_minipacket_length		0x000100FF
#define LFDR_minipacket_length_MSB	0x0000000F

#define LFDR_minipacket_RTI		0x000200FF
#define LFDR_minipacket_RTI_MSB		0x000300FF

#define LFDR_log_linear		0x00040001
#define LFDR_antenna		0x00040101
#define LFDR_sample_size	0x00040303
#define LFDR_channel		0x00040507

#define LFDR_digital_prescale	0x0005000F
#define LFDR_gain		0x00050403
#define LFDR_lp_mode		0x00050601
#define LFDR_frequency_band	0x00050701
