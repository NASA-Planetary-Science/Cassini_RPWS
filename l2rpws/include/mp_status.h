 /*
  * HFR data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define MP_packet_ID		0x0000040F
#define MP_packet_ID_MFR	0x01
#define MP_packet_ID_HFR	0x02
#define MP_packet_ID_LFDR	0x07
#define MP_packet_ID_WFR	0x08
#define MP_packet_ID_WBR	0x0E
#define MP_packet_ID_DUST	0x0B
#define MP_packet_ID_LP		0x04
#define MP_packet_ID_MRO	0x0D
#define MP_packet_ID_STIM	0x00
#define MP_packet_ID_FILL	0x0F

#define MP_packet_size_MSB	0x0000000F
#define MP_packet_size_LSB	0x000100FF


#define MP_packet_rti_MSB	0x000300FF
#define MP_packet_rti_LSB	0x000200FF
