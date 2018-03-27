 /*
  * MFR data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define MFR_packet_ID_value		0x01
#define MFR_packet_ID                   0x0000040F
#define MFR_minipacket_length		0x000100FF
#define MFR_minipacket_length_MSB	0x0000000F

#define MFR_minipacket_RTI		0x000200FF
#define MFR_minipacket_RTI_MSB		0x000300FF

#define MFR_compression			0x00040001
#define MFR_antenna			0x00040103
#define MFR_fast_switch			0x00040301
#define MFR_antenna_1			0x00040403
#define MFR_antenna_2			0x00040603
