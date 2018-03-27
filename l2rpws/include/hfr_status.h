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

#define HFR_packet_ID_value		0x02
#define HFR_packet_ID                   0x0000040F
#define HFR_minipacket_Length		0x000100FF
#define HFR_minipacket_Length_MSB	0x0000000F

#define HFR_minipacket_RTI	0x000200FF
#define HFR_minipacket_RTI_MSB	0x000300FF

#define HFR_segment_number	0x0004007F
#define HFR_segment_EOF		0x00040701

#define HFR_packet_size_MSB	0x000500FF
#define HFR_packet_size_LSB	0x000600FF

#define HFR_packet_type		0x00070503

#define HFR_MFR_Ex_antenna	0x00070103
#define HFR_MFR_Ez_antenna	0x00070001

#define HFR_error_flag		0x00070701
#define HFR_SND_relay		0x00070401
#define HFR_SND_clamp		0x00070301

#define HFR_memory_dump		0x00080701
#define HFR_compression		0x00080407
#define HFR_version		0x0008000F

#define HFR_ABC_band_ABC	0x00090007
#define HFR_ABC_band_A		0x00090001
#define HFR_ABC_band_B		0x00090101
#define HFR_ABC_band_C		0x00090201
#define HFR_H1_band		0x00090301
#define HFR_H2_band		0x00090401
#define HFR_ALL_attenuator	0x00090701

#define HFR_ABC_repeat_count	0x000A00FF
#define HFR_H1_repeat_count	0x000B00FF
#define HFR_H2_repeat_count	0x000C00FF
#define HFR_ALL_repeat_count	0x000D00FF

#define HFR_ABC_integration_time	0x000E0603
#define HFR_ABC_direction_finding	0x000E0501
#define HFR_ABC_Ez_antenna		0x000E0401
#define HFR_ABC_Ex_antenna		0x000E0203
#define HFR_ABC_filters			0x000E0003

#define HFR_H1_integration_time		0x000F0603
#define HFR_H1_direction_finding	0x000F0501
#define HFR_H1_Ez_antenna		0x000F0401
#define HFR_H1_Ex_antenna		0x000F0203
#define HFR_H1_filters			0x000F0003

#define HFR_H2_integration_time		0x00100603
#define HFR_H2_direction_finding	0x00100501
#define HFR_H2_Ez_antenna		0x00100401
#define HFR_H2_Ex_antenna		0x00100203
#define HFR_H2_filters			0x00100003

#define HFR_H1_start_frequency		0x001100FF
#define HFR_H1_step_count		0x001200FF
#define HFR_H1_step_size		0x001300FF

#define HFR_H2_start_frequency		0x001400FF
#define HFR_H2_step_count		0x001500FF
#define HFR_H2_step_size		0x001600FF

#define HFR_H2_start_frequency_MSB	0x00170701
#define HFR_H2_step_count_MSB		0x00170601
#define HFR_ABC_auto_correlation	0x00170201
#define HFR_ABC_cross_correlation	0x00170501
#define HFR_H1_auto_correlation		0x00170101
#define HFR_H1_cross_correlation	0x00170401
#define HFR_H2_auto_correlation		0x00170001
#define HFR_H2_cross_correlation	0x00170301

#define HFR_MS_antenna			0x00090603
#define HFR_MS_sample_rate		0x00090307
#define HFR_MS_sample_size		0x00090007
#define HFR_MS_frequency		0x000A00FF
#define HFR_MS_receiver			0x000B0001

#define HFR_SND_number_cycles		0x000900FF
#define HFR_SND_number_passive		0x000A00FF
#define HFR_SND_number_active		0x000B00FF
#define HFR_SND_start_frequency		0x000C00FF
#define HFR_SND_stop_frequency		0x000D00FF
#define HFR_SND_T1_delay		0x000E00FF
#define HFR_SND_T2_delay		0x000F00FF
#define HFR_SND_T3_delay		0x001000FF
#define HFR_SND_antenna			0x00110701
#define HFR_SND_mode			0x00110401
#define HFR_SND_auto_B_MSB		0x00110103
#define HFR_SND_AGC_B_MSB		0x00110103
#define HFR_SND_auto_A_MSB		0x00110001
#define HFR_SND_auto_A			0x001200FF
#define HFR_SND_auto_B			0x0013007F
#define HFR_SND_AGC_B			0x0013007F

#define HFR_SND_spare			0x001400FF

#define HFR_CAL_H2_select		0x00090401
#define HFR_CAL_H1_select		0x00090301
#define HFR_CAL_C_select		0x00090201
#define HFR_CAL_B_select		0x00090101
#define HFR_CAL_A_select		0x00090001
#define HFR_CAL_Iowa			0x000A0301
#define HFR_CAL_MFR_Ez			0x000A0201
#define HFR_CAL_MFR_Ex			0x000A0003
#define HFR_CAL_header_termination	0x000B00FF
#define HFR_CAL_spare			0x000B00FF
