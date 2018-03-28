#include <stdint.h>

#ifndef _TELEMETRY_H
#define _TELEMETRY_H

/*

List of Sources and References
	Documentation : 
		RPWS Cassini Users Guide Software Operations Manual, Rev. 0.0, April 1997
			by W. Robison, T. Averkamp, R. Brechwald, J. Phillips, P. Reid
		RPWS Cassini Housekeeping Telemetry Packet Format
			by W. Robison, T. Averkamp
		Cassini MGSO Documentation
			by Jet Propulsion Labatories (c/o Betsy Wilson)
	Software :
		rtiu.h - structures and record definitions
			by W. Robison
		util.h - library routines
			by W. Robison

*/
#define RECORD_READ_SUCCESS	(1)
#define RECORD_BOF		(2)     /* Beginning of File */
#define RECORD_EOF		(3)     /* End of File */
#define RECORD_READ_ERROR	(4)
#define RECORD_NOT_OPEN		(5)
#define RECORD_READ_HOLD	(6)
#define RECORD_WRITE_SUCCESS	(7)
#define RECORD_WRITE_ERROR	(8)
#define RECORD_SEEK_SUCCESS	(9)

#define CDS_BUFFER_SIZE			1024
#define MP_BUFFER_SIZE			8192
#define USMP_BUFFER_SIZE		65536

#define DCC_DECOMPRESSION_BUFFER_SIZE	65536


#define CDS_PACKET_TYPE_housekeeping_engineering 	(0x0A90)
#define CDS_PACKET_TYPE_housekeeping_deployment 	(0x0A93)
#define CDS_PACKET_TYPE_housekeeping_science 		(0x0A95)
#define CDS_PACKET_TYPE_unsegmented_lrs_type_I		(0x0A81)
#define CDS_PACKET_TYPE_unsegmented_hrs_type_I		(0x0A82)
#define CDS_PACKET_TYPE_segmented_lrs_type_I		(0x0AA0)
#define CDS_PACKET_TYPE_segmented_hrs_type_I		(0x0AA3)
#define CDS_PACKET_LENGTH_Housekeeping			(0x00B9)
#define CDS_PACKET_LENGTH_ScienceData			(0x03B1)

#define MINI_PACKET_TYPE_stim				(0x00)
#define MINI_PACKET_TYPE_mfr				(0x01)
#define MINI_PACKET_TYPE_hfr				(0x02)
#define MINI_PACKET_TYPE_3				(0x03)
#define MINI_PACKET_TYPE_lp				(0x04)
#define MINI_PACKET_TYPE_5				(0x05)
#define MINI_PACKET_TYPE_6				(0x06)
#define MINI_PACKET_TYPE_lfdr				(0x07)
#define MINI_PACKET_TYPE_wfr				(0x08)
#define MINI_PACKET_TYPE_9				(0x09)
#define MINI_PACKET_TYPE_10				(0x0A)
#define MINI_PACKET_TYPE_dust				(0x0B)
#define MINI_PACKET_TYPE_12				(0x0C)
#define MINI_PACKET_TYPE_mro				(0x0D)
#define MINI_PACKET_TYPE_wbr				(0x0E)
#define MINI_PACKET_TYPE_fill				(0x0F)

#define MP_COMPRESSION_unpacked				(0x00)
#define MP_COMPRESSION_packed				(0x01)
#define MP_COMPRESSION_dcc_wcout			(0x02)
#define MP_COMPRESSION_dcc_wcin				(0x03)
#define MP_COMPRESSION_dcp				(0x04)

#define MP_COMPRESSION_LP_unpacked			(0x00)
#define MP_COMPRESSION_LP_packed			(0x01)
#define MP_COMPRESSION_LP_tbd				(0x02)
#define MP_COMPRESSION_LP_dcp				(0x03)

#define MP_MODE_LP_sweep				(0x00)
#define MP_MODE_LP_density				(0x01)
#define MP_MODE_LP_analyzed				(0x02)
#define MP_MODE_LP_tbd					(0x03)

#define MP_WFR_MODE_0					(0x00)
#define MP_WFR_MODE_1					(0x01)
#define MP_WFR_MODE_2					(0x02)
#define MP_WFR_MODE_3					(0x03)
#define MP_WFR_MODE_4					(0x04)
#define MP_WFR_MODE_5					(0x05)
#define MP_WFR_MODE_6					(0x06)
#define MP_WFR_MODE_7					(0x07)
#define MP_WFR_MODE_lp					(0x01)

#define MP_WFR_CHANNEL_0				(0x00)
#define MP_WFR_CHANNEL_1				(0x01)
#define MP_WFR_CHANNEL_2				(0x02)
#define MP_WFR_CHANNEL_3				(0x03)
#define MP_WFR_CHANNEL_4				(0x04)

#define PACKET_READ_SUCCESS	(0)
#define PACKET_BOF		(1)     /* Beginning of File */
#define PACKET_EOF		(2)     /* End of File */
#define PACKET_READ_ERROR	(3)
#define PACKET_NOT_OPEN		(4)
#define PACKET_READ_HOLD	(5)

#define PACKET_WRITE_SUCCESS	(0)
#define PACKET_WRITE_ERROR	(1)


typedef int32_t RTI;

typedef struct
{
  uint32_t seconds, milliseconds;
} CDSCLK;

typedef struct
{
  uint32_t seconds, milliseconds;
} SCLK;

typedef struct
{
  uint32_t seconds, milliseconds;
} EVTCLK;

typedef struct
{
  int32_t year, doy, hour, minute, second, millisecond;
  int32_t month, day;
} SCET;


typedef struct
{
  int32_t cds_time, filler_cds_time;       /*           8 */
  unsigned char fill0[44];              /*          44 */
  int32_t packet_length;                   /*           4 */
  unsigned char fill56[168];            /*         168 */
  int chdo_sclk[2], chdo_scet[2];      /*          16 */
  int chdo_ert[2], chdo_rct[2];        /*          16 */
} STATUS;                               /* Total = 256 */

typedef struct
{
  unsigned char PrimaryHeader[6];
  unsigned char SecondaryHeader[6];
  unsigned char Data[CDS_BUFFER_SIZE];
} CDS_PACKET;

typedef struct
{
  unsigned char Data[MP_BUFFER_SIZE];
} MINI_PACKET;

typedef struct
{
  unsigned char Data[USMP_BUFFER_SIZE];
} UNSEGMENTED_MINI_PACKET;

typedef struct
{
  int32_t forward_length, fill0, fill1;
  STATUS status;
  union
  {
    CDS_PACKET cds;
    MINI_PACKET mp;
    UNSEGMENTED_MINI_PACKET usmp;
  } packet;
} TELEMETRY;




typedef struct record_file
{
  char *filename, *type, errmsg[128];
  int32_t position, status;                /* byte offset for the beginning of file */
  FILE *handle;
} RecordFile;




/*#define get_cds_hsk_sc_temp(x)			( (x->Data[23]) ) April 14, 1997 */

/* Converted to functions so that swapping can be preformed  Dec. 22, 2016 */
int32_t get_telemetry_packet_length(TELEMETRY* p);  
int32_t get_telemetry_packet_cds_time(TELEMETRY* p);

#define get_cds_packet_type(x)	 ( (x->PrimaryHeader[1]<<8)| x->PrimaryHeader[0] )
#define get_cds_packet_ssc(x)	 ( ((x->PrimaryHeader[3]<<8)|x->PrimaryHeader[2])&0x3FFF )
#define get_cds_packet_length(x) ( (x->PrimaryHeader[5]<<8)|x->PrimaryHeader[4] )
#define get_cds_packet_error(x)  ( x->SecondaryHeader[1] )
#define get_cds_packet_time(x)   ( (x->SecondaryHeader[0]<<24)|(x->SecondaryHeader[3]<<16)| \
				   (x->SecondaryHeader[2]<<8)|(x->SecondaryHeader[5]) )
#define get_cds_packet_rti(x) 	 ( (x->SecondaryHeader[4]>>5)&0x07 )


/* Common to all three types of housekeeping packets: ROM, Deploy, Science */
#define get_cds_hsk_time_tag(x)			( (x->Data[1]<<8)|(x->Data[0]) )
#define get_cds_hsk_valid_command_count(x)	( (x->Data[2]) )
#define get_cds_hsk_invalid_command_count(x)	( (x->Data[3]) )
#define get_cds_hsk_command_byte_count(x)	( (x->Data[5]<<8)|(x->Data[4]) )
#define get_cds_hsk_command_loop_count(x)	( (x->Data[6]) )        /* Format Has Changed Since Jan 17, 1997 */
#define get_cds_hsk_BIU_reset_count(x)		( (x->Data[7]) )        /* Format Has Changed Since Jan 17, 1997 */
#define get_cds_hsk_BIU_discrete_status(x)	( (x->Data[8]) )
#define get_cds_hsk_BIU_discrete_command(x)	( (x->Data[9]) )
#define get_cds_hsk_antenna_limit_switches(x)	( (x->Data[10]) )       /* Limit Switches */
#define get_cds_hsk_power_status(x)		( (x->Data[11]) )
#define get_cds_hsk_MFR2_analog(x)		( (x->Data[12]) )       /* Channel 0 LRP Analog  Mux */
#define get_cds_hsk_MFR1_analog(x)		( (x->Data[13]) )       /* Ch 1 */
#define get_cds_hsk_MFR3_analog(x)		( (x->Data[14]) )       /* Ch 2 */
#define get_cds_hsk_spare_14_msb(x)		( (x->Data[15]) )       /* Ch 3 */
#define get_cds_hsk_antenna_motor_current(x)	( (x->Data[16]) )       /* Ch 4 */
#define get_cds_hsk_HFR_analog(x)		( (x->Data[17]) )       /* Ch 5 */
#define get_cds_hsk_spare_16_lsb(x)		( (x->Data[18]) )       /* Ch 6 */
#define get_cds_hsk_motor_temperature_x_plus(x)	( (x->Data[19]) )       /* Ch 7 */
#define get_cds_hsk_motor_temperature_x_minus(x) ( (x->Data[20]) )      /* Ch 8 */
#define get_cds_hsk_motor_temperature_z_minus(x) ( (x->Data[21]) )      /* Ch 9 */
#define get_cds_hsk_spare_18_lsb(x)		( (x->Data[22]) )       /* Ch 10 */
#define get_cds_hsk_search_coil_temperature(x)	( (x->Data[23) )        /* Ch 11 */
#define get_cds_hsk_motor_position_x_plus(x)	( (x->Data[24]) )       /* Ch 12 */
#define get_cds_hsk_motor_position_x_minus(x)	( (x->Data[25]) )       /* Ch 13 */
#define get_cds_hsk_motor_position_z_minus(x)	( (x->Data[26]) )       /* Ch 14 */
#define get_cds_hsk_halt_integrator(x)		( (x->Data[27]) )       /* Ch 15 */
#define get_cds_hsk_HFR_current(x)		( (x->Data[28]) )       /* Ch 0 HFR Current */
#define get_cds_hsk_ME02_current(x)		( (x->Data[29]) )       /* Ch 1 ME02 Analog Current */
#define get_cds_hsk_LP_current(x)		( (x->Data[30]) )       /* Ch 2 Langmuir Probe Current */
#define get_cds_hsk_ME01_current(x)		( (x->Data[31]) )       /* Ch 3 Digital Electronics Current */
#define get_cds_hsk_HFR_p5_volt(x)		( (x->Data[32]) )       /* Ch 4 HFR +5 Volts */
#define get_cds_hsk_HFR_p6_volt(x)		( (x->Data[33]) )       /* Ch 5 HFR +6 Volts */
#define get_cds_hsk_ME02_p12_volt(x)		( (x->Data[34]) )       /* Ch 6 ME02 +12 Volts */
#define get_cds_hsk_ME02_p6_volt(x)		( (x->Data[35]) )       /* Ch 7 ME02 +6 Volts */
#define get_cds_hsk_ME02_p5_volt(x)		( (x->Data[36]) )       /* Ch 8 ME02 +5 Volts */
#define get_cds_hsk_LP_p45_volt(x)		( (x->Data[37]) )       /* Ch 9 Langmuir Probe +45 Volts */
#define get_cds_hsk_ME01_p12_volt(x)		( (x->Data[38]) )       /* Ch 10 Digital Electronics +12 Volts */
#define get_cds_hsk_ME01_p5_volt(x)		( (x->Data[39]) )       /* Ch 11 Digital Electronics +5 Volts */
#define get_cds_hsk_HFR_m5_volt(x)		( (x->Data[40]) )       /* Ch 12 HFR -6 Volts */
#define get_cds_hsk_ME02_m12_volt(x)		( (x->Data[41]) )       /* Ch 13 ME02 -12 Volts */
#define get_cds_hsk_ME02_m6_volt(x)		( (x->Data[42]) )       /* Ch 14 ME02 -6 Volts */
#define get_cds_hsk_LP_m45_volt(x)		( (x->Data[43]) )       /* Ch 15 Langmuir Probe +45 Volts */
#define get_cds_hsk_BIU_discrete_misc(x)	( (x->Data[44]) )
#define get_cds_hsk_BIU_RTI_status(x)		( (x->Data[45]) )
#define get_cds_hsk_LP_probe_bias(x)		( (x->Data[46]) )
#define get_cds_hsk_LP_DAC1_bias(x)		( (x->Data[47]) )
#define get_cds_hsk_LP_MUX_0(x)			( (x->Data[48]) )
#define get_cds_hsk_LP_8155(x)			( (x->Data[49) )

/* BIU Discrete Command Byte Decoding */
#define cds_hsk_discrete_command_0(x)		( (x->Data[9])&0x001 )
#define cds_hsk_discrete_command_1(x)		( (x->Data[9])&0x002 )
#define cds_hsk_discrete_command_2(x)		( (x->Data[9])&0x004 )
#define cds_hsk_ExMinusEnabled(x)		( (x->Data[9])&0x008 )
#define cds_hsk_EzMinusEnabled(x)		( (x->Data[9])&0x010 )
#define cds_hsk_ExPlusEnabled(x)		( (x->Data[9])&0x020 )
#define cds_hsk_discrete_command_6(x)		( (x->Data[9])&0x040 )
#define cds_hsk_discrete_command_7(x)		( (x->Data[9])&0x080 )

/* BIU Discrete Status Byte Decoding */
#define cds_hsk_discrete_status_0(x)		( (x->Data[8])&0x001 )
#define cds_hsk_discrete_status_1(x)		( (x->Data[8])&0x002 )
#define cds_hsk_discrete_status_2(x)		( (x->Data[8])&0x004 )
#define cds_hsk_discrete_status_3(x)		( (x->Data[8])&0x008 )
#define cds_hsk_AntennaMotorEnabled(x)		( (x->Data[8])&0x010 )
#define cds_hsk_discrete_status_5(x)		( (x->Data[8])&0x020 )
#define cds_hsk_discrete_status_6(x)		( (x->Data[8])&0x040 )
#define cds_hsk_discrete_status_7(x)		( (x->Data[8])&0x080 )

/* Antenna Limit Switches Decoding */
#define cds_hsk_LimitSwitch_ExPlus_Extend(x)	( (x->Data[10])&0x001 )
#define cds_hsk_LimitSwitch_ExPlus_Retract(x)	( (x->Data[10]>>1)&0x001 )
#define cds_hsk_LimitSwitch_ExMinus_Extend(x)	( (x->Data[10]>>2)&0x001 )
#define cds_hsk_LimitSwitch_ExMinus_Retract(x)	( (x->Data[10]>>3)&0x001 )
#define cds_hsk_LimitSwitch_EzMinus_Extend(x)	( (x->Data[10]>>4)&0x001 )
#define cds_hsk_LimitSwitch_EzMinus_Retract(x)	( (x->Data[10]>>5)&0x001 )

/* Applies to ROM Mode Housekeeping */
#define get_cds_rom_hsk_memory_address(x)		( (x->Data[51]<<8)|(x->Data[50]) )

/* 52-179 Memory Dump */


/* Applies to Deployment Mode Housekeeping */
#define get_cds_deploy_spare_lsb_msb(x)		( (x->Data[51]<<8)|(x->Data[50]) )
#define get_cds_deploy_run_timer(x)		( (x->Data[53]<<8)|(x->Data[52]) )
#define get_cds_deploy_command_RTI(x)		( (x->Data[55]<<8)|(x->Data[54]) )
#define get_cds_deploy_command_pattern(x)	( (x->Data[57]<<8)|(x->Data[56]) )
#define get_cds_deploy_command_index(x)		( (x->Data[58]) )       /* Decoded Recent Commnad */
#define get_cds_deploy_antenna_id(x)		( (x->Data[59]) )       /* Decoded Recent Antenna */
#define get_cds_deploy_status(x)		( (x->Data[60]) )
#define get_cds_deploy_test_mask(x)		( (x->Data[61]) )
#define get_cds_deploy_motor_current(x)		( (x->Data[62]) )
#define get_cds_deploy_motor_temperature(x)	( (x->Data[63]) )
#define get_cds_deploy_element_position(x)	( (x->Data[64]) )
#define get_cds_deploy_limit_switch_register(x)	( (x->Data[65]) )
#define get_cds_deploy_reason_code(x)		( (x->Data[66]<<8)|(x->Data[67]) )
        /*
         * If the Major Reason Code is 0x03F1 or 0x03F2 (Setup or Status) 
         */
#define get_cds_deploy_max_current(x)		( (x->Data[60]) )       /* 0 in both min and max */
#define get_cds_deploy_min_current(x)		( (x->Data[61]) )       /* implies Test Disabled */
#define get_cds_deploy_max_temperature(x)	( (x->Data[62]) )       /* same as above */
#define get_cds_deploy_min_temperature(x)	( (x->Data[63]) )       /* same as above */
#define get_cds_deploy_max_position(x)		( (x->Data[64]) )       /* same as above */
#define get_cds_deploy_min_position(x)		( (x->Data[65]) )       /* same as above */
        /*
         * Dynamic Area has this type of record repeated 5 times 
         */

/*#define get_cds_deploy_(x)	( (x->Data[69]<<8)|(x->Data[68]) )	 RTI at the beginning of the 16 Second Period */

/*#define get_cds_deploy_(x)	( (x->Data[70] )	 Motor Temperature */

/*#define get_cds_deploy_(x)	( (x->Data[71] )	 Motor Current 2 Seconds */

/*#define get_cds_deploy_(x)	( (x->Data[72] )	Motor Position 2 Seconds */

/*---------------------------------------------------------------------------------------*/

/*#define get_cds_deploy_(x)	( (x->Data[85] )	 Motor Current 2 Seconds */

/*#define get_cds_deploy_(x)	( (x->Data[86] )	 Motor Position 2 Seconds */

/*#define get_cds_deploy_(x)	( (x->Data[87] )	 Average Current */

/*#define get_cds_deploy_(x)	( (x->Data[88] )	 Minimum Current */

/*#define get_cds_deploy_(x)	( (x->Data[89] )	 Maximum Current */

/*
offset+00  to offset+21  Set 1
offset+22  to offset+43  Set 2
offset+44  to offset+65  Set 3
offset+66  to offset+87  Set 4
offset+88  to offset+109 Set 5

*/




#define get_mini_packet_type(x)		 ( (x->Data[0]>>4)&0x0F )
#define get_mini_packet_length(x)	 (((x->Data[0]<<8)|x->Data[1])&0x0FFF )
#define get_mini_packet_rti(x)		 ( (x->Data[3]<<8)|(x->Data[2]) )

#define	MFR_VOLTS_PER_COUNT		 (        0.020	        )
#define get_mp_mfr_antenna(x)		 ( (x->Data[4]>>1)&0x03 )

#define MP_HFR_PKT_analysis				(0x00)
#define MP_HFR_PKT_sounder				(0x01)
#define MP_HFR_PKT_calibration				(0x02)
#define MP_HFR_PKT_not_allowed				(0x03)

#define get_mp_hfr_length(x)	 	 ( (x->Data[5]<<8)|(x->Data[6]) )
#define get_mp_hfr_error_flag(x) 	 ( (x->Data[7]>>7)&0x01 )
#define get_mp_hfr_packet_type(x) 	 ( (x->Data[7]>>5)&0x03 )
#define get_mp_hfr_sounder_rly(x)	 ( (x->Data[7]>>4)&0x01 )
#define get_mp_hfr_clamp_rly(x)		 ( (x->Data[7]>>3)&0x01 )
#define get_mp_hfr_mfr_ex_antenna(x) 	 ( (x->Data[7]>>1)&0x03 )
#define get_mp_hfr_mfr_ez_antenna(x) 	 ( (x->Data[7])&0x01 )
#define get_mp_hfr_compression(x) 	 ( (x->Data[8]>>4)&0x07 )
#define get_mp_hfr_header_ver(x) 	 ( (x->Data[8])&0x0F )
#define get_mp_hfr_bandA_selected(x) 	 ( (x->Data[9])&0x01 )
#define get_mp_hfr_bandB_selected(x) 	 ( (x->Data[9]>>1)&0x01 )
#define get_mp_hfr_bandC_selected(x) 	 ( (x->Data[9]>>2)&0x01 )
#define get_mp_hfr_bandHF1_selected(x) 	 ( (x->Data[9]>>3)&0x01 )
#define get_mp_hfr_bandHF2_selected(x) 	 ( (x->Data[9]>>4)&0x01 )
#define get_mp_hfr_repeat_count_ABC(x) 	 ( (x->Data[10])&0x0FF )
#define get_mp_hfr_repeat_count_HF1(x) 	 ( (x->Data[11])&0x0FF )
#define get_mp_hfr_repeat_count_HF2(x) 	 ( (x->Data[12])&0x0FF )
#define get_mp_hfr_repeat_count_all(x) 	 ( (x->Data[13])&0x0FF )
#define get_mp_hfr_ABC_integ_time(x) 	 ( (x->Data[14]>>6)&0x03 )
                        /*
                         * eeeeemmm               2^E         *    (M+8) 
                         */
#define get_hfr_value(x)	         ( ((int32_t)1<<((x>>3)&0x1F)) * ((x&0x07)+8) )

#define get_mp_lfdr_mode(x)		 (  x->Data[4]&0x01     )
#define get_mp_lfdr_antenna(x)		 ( (x->Data[4]>>1)&0x01 )
#define get_mp_lfdr_size(x)		 ( (x->Data[4]>>3)&0x03 )
#define get_mp_lfdr_channel(x)		 ( (x->Data[4]>>5)&0x07 )
#define get_mp_lfdr_dgf(x)		 (  x->Data[5]&0x0F     )
#define get_mp_lfdr_gain(x)		 ( (x->Data[5]>>4)&0x03 )
#define get_mp_lfdr_mode_lp(x)		 ( (x->Data[5]>>6)&0x01 )
#define get_mp_lfdr_band(x)		 ( (x->Data[5]>>7)&0x01 )
                                        /*
                                         * 2^Exponent     * Mantissa +     Base[Exponent] . {0,32,96,224,480,992,2016,4064} 
                                         */
#define get_lfdr_value(x)	         ( (1<<((x>>5)&0x07)) * (x&0x1F) + ((32<<((x>>5)&0x07))-32) )
#define get_lfdr_dgf(x)	                 ( 1<<(x->Data[5]&0x0F) )

#define get_mp_wbr_gain(x)		 (  x->Data[5]&0x07     )
#define get_mp_wbr_band(x)		 ( (x->Data[5]>>7)&0x01 )       /* 0 = 10KHz, 1 = 80KHz */
#define get_mp_wbr_antenna(x)	 	 (  x->Data[6]&0x07     )
#define get_mp_wbr_msf(x)	 	 ( (x->Data[6]>>3)&0x01 )
#define get_mp_wbr_compression(x)	 ( (x->Data[6]>>4)&0x0F )

#define get_mp_wfr_gain_ch0(x)	 	 (  x->Data[5]&0x03     )
#define get_mp_wfr_gain_ch1(x)	 	 ( (x->Data[5]>>2)&0x03 )
#define get_mp_wfr_gain_ch234(x)	 ( (x->Data[5]>>4)&0x03 )
#define get_mp_wfr_mode_lp(x)	 	 ( (x->Data[5]>>6)&0x01 )
#define get_mp_wfr_band(x)		 ( (x->Data[5]>>7)&0x01 )       /* 0 = 40Hz, 1 = 2.5KHz */
#define get_mp_wfr_antenna(x)	 	 (  x->Data[6]&0x07     )
#define get_mp_wfr_msf(x)	 	 ( (x->Data[6]>>3)&0x01 )
#define get_mp_wfr_compression(x)	 ( (x->Data[6]>>4)&0x0F )
#define get_mp_wfr_mode(x)	 	 (  x->Data[7]&0x07     )
#define get_mp_wfr_channel(x)	 	 ( (x->Data[7]>>3)&0x07 )

#define get_mp_lp_mode(x)	 	 (  x->Data[4]&0x03 )
#define get_mp_lp_compression(x)	 ( (x->Data[4]>>2)&0x03 )
#define get_mp_lp_antenna(x)	 	 (  x->Data[6]&0x03     )
#define get_mp_lp_band(x)		 ( (x->Data[6]>>2)&0x01 )       /* 0 = 6Hz, 1 = No Filter */

#define put_telemetry_packet_length(x,l)  (  x->status.packet_length=(l-3) )

#define put_mini_packet_length(x,l)	 x->Data[0]&=0x0F0;x->Data[0]|=((l>>8)&0x0F);x->Data[1]=l&0x0FF

#define put_mp_wbr_compression(x,l)	 x->Data[6]&=0x0F;x->Data[6]|=((l<<4)&0x0F0)
#define put_mp_wfr_compression(x,l)	 x->Data[6]&=0x0F;x->Data[6]|=((l<<4)&0x0F0)
#define put_mp_wfr_mode(x,l)	 	 x->Data[7]&=0xF8;x->Data[7]|=(l&0x07)
#define put_mp_wfr_mode_lp(x,l)	 	 x->Data[5]&=0xDF;x->Data[5]|=((l<<5)&0x20)
#define put_mp_wfr_channel(x,l)	 	 x->Data[7]&=0xC7;x->Data[7]|=((l<<3)&0x38)

        /*
         * Seconds,RTI: RTI is accurate, Seconds is approximate 
         */
#define get_event_time_seconds(s,r)	( (uint32_t)( ((s-((r>>3)&0x1FFF))&0xFFFFE000) | ((r>>3)&0x1FFF) ) )


char *get_reason_code_string (uint32_t reason_code);

                        /*
                         * Deployment Housekeeping Reason Code 
                         */


void delay_seconds (double dly);

char *open_telemetry_infile (char *filename);
int32_t close_telemetry_infile (char *filename);
char *open_telemetry_outfile (char *filename);
int32_t close_telemetry_outfile (char *filename);
int32_t get_previous_packet (TELEMETRY * t);
int32_t get_current_packet (TELEMETRY * t);
int32_t get_next_packet (TELEMETRY * t);
int32_t put_packet (TELEMETRY * t);
void stamp_telemetry_packet_lengths (TELEMETRY * x, int32_t length);
char *get_current_file (char *path, char *type);        /* path='filerc' and "RAW=", "MP=", "MPUS=", ect */





/* Returns: NULL is success, otherwise an error message */
void init_RecordFile (RecordFile * rf);
char *open_RecordFile (RecordFile * rf, char *filename, char *type);
char *close_RecordFile (RecordFile * rf);
int32_t get_previous_record_RecordFile (RecordFile * rf, void *pv);
int32_t get_current_record_RecordFile (RecordFile * rf, void *pv);
int32_t get_next_record_RecordFile (RecordFile * rf, void *pv);
int32_t move_previous_record_RecordFile (RecordFile * rf);
int32_t move_current_record_RecordFile (RecordFile * rf);
int32_t move_next_record_RecordFile (RecordFile * rf);
#endif
