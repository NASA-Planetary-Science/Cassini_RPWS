 /*
  * LP data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define LP_packet_ID_value		0x04
#define LP_packet_ID			0x0000040F
#define LP_minipacket_length		0x000100FF
#define LP_minipacket_length_MSB	0x0000000F

#define LP_minipacket_RTI		0x000200FF
#define LP_minipacket_RTI_MSB		0x000300FF

#define LP_packet_RAW_DENSITY		1
#define LP_packet_RAW_SWEEP		0
#define LP_packet_ANAL_SWEEP		2
#define LP_packet_TBD			3

#define LP_packet_type		0x00040003
#define LP_compression		0x00040203
#define LP_minipacket_size	0x00040603
#define LP_minipacket_segment	0x00040403

#define LP_DAC_0		0x000500FF
#define LP_sweep_table_ID	0x000500FF

#define LP_MUX0_status		0x000600FF
#define LP_input_select		0x00060003
#define LP_Cylinder_input	0x00060001
#define LP_Sensor_select	0x00060101
#define LP_Bandwidth		0x00060201
#define LP_FGA_gain		0x0006030F
#define LP_FGB_gain		0x00060701

#define LP_Relay_status		0x000700FF
#define LP_Relay_Ex_minus	0x00070003
#define LP_Relay_1		0x00070001
#define LP_Relay_2		0x00070101
#define LP_Relay_Ex_plus	0x00070203
#define LP_Relay_3		0x00070201
#define LP_Relay_4		0x00070301
#define LP_Relay_5		0x00070401
#define LP_Sphere_gain		0x00070501

#define LP_Clock		0x000800FF
#define LP_Clock_MSB		0x0009003F
#define LP_Clock_Upper		0x000900FF
#define LP_Clock_Mode		0x00090603
