 /*
  * STIM data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define STIM_packet_ID			0x0000040F
#define STIM_packet_ID_value		0x00
#define STIM_minipacket_length		0x000100FF
#define STIM_minipacket_length_MSB	0x0000000F

#define STIM_minipacket_RTI		0x000200FF
#define STIM_minipacket_RTI_MSB		0x000300FF

#define STIM_initial_RTI		0x000400FF
#define STIM_initial_RTI_MSB		0x000500FF

#define STIM_sequence			0x000600FF
#define STIM_sequence_MSB		0x000700FF

#define STIM_ID_0			0x000800FF
#define STIM_ID_0_MSB			0x000900FF

#define STIM_ID_1			0x000A00FF
#define STIM_ID_1_MSB			0x000B00FF
#define STIM_trigger			0x000A00FF
#define STIM_trigger_MSB		0x000B00FF

#define STIM_ID_2			0x000C00FF
#define STIM_ID_2_MSB			0x000D00FF

#define STIM_ID_3			0x000E00FF
#define STIM_ID_3_MSB			0x000F00FF
#define STIM_date			0x000E00FF
#define STIM_date_MSB			0x000F00FF
