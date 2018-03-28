/*
 * rpws_archive.c
 */

/* Spice includes */
#include <SpiceUsr.h>

#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <math.h>
#include <ulimit.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <das2/das1.h>          /* Das includes */

#include <fg.h>					  /* Willys Cmd-Line lib */

#include <rtiu.h>               /* Cassini inlcudes */
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <UTIL_status.h>
#include <archive.h>
#include <rpws_sclk.h>

#include "rpws_label.h"			  /* HR PDS includes */
#include "rpws_direct.h"
#include "rpws_time_patch.h"
#include "rpws_fsw_ver.h"
#include "rpws_timing.h"

#define _RPWS_W_DIRECTORY_
#include "rpws_w_directory.h"

#include "bis.h"					  /* app includes */
#include "rpws_browse.h"
#include "rpws_archive_cdrom.h"
#include "rpws_hfr_status.h"
#include "rpws_lp_status.h"
#include "rpws_essay.h"
#include "rpws_help.h"


#define _rpws_archive_
#include "rpws_archive.h"

#include "rpws_master_browse.h"
#include "rpws_duplicate_scan.h"

#define SCIOP_TIMEOUT 10
#define BUFFER_SIZE 65536
#define RTI_MASK 0x00E0
#define A_EPOCH 2436204
#define ALL "this makes LFDR and MFR appear in RAW file"
#define CHANNEL_0 0
#define CHANNEL_1 1
#define CHANNEL_2 2
#define CHANNEL_3 3
#define CHANNEL_4 4

#define SAMPLE_SIZE_WBR 1
#define SAMPLE_SIZE_WFR 2

#define _New_Name_Handling_
#undef _New_Name_Handling_
#define _New_Name_Handling_

/****************************************************************************/
/* Compiled in default file locations */

#ifndef INST_BIN
#error Compiled in default bin directory is missing.
#endif

/****************************************************************************/


char *Title = { "RPWS_HR_AR" };
char *Version = { "V9.1" };
static char *Version_0 = { VERSION_0 };

int bisflag = BISVERSION | BISDAY | BISBAND | BISCELLSPACING;
int Mafi_Flag = 0;

static char *cd_index = NULL;
static char *cd_count = "nn";
static char *cd_format = NULL;

char *namefile = CFG "/names.tab";

char *Label_Ver = { "V1.0" };
char *Mission_Phase = { "" };
char *Target = { "" };
char *Coordinate = { "" };
char *Orbit = { "" };

char *Product[] = { "",			  /* 12345678901234567890 */
	"RPWS_WIDEBAND_FULL",
	"RPWS_WAVEFORM_FULL",
	"RPWS_STIM",
	"RPWS_RAW_COMPLETE"
};

int Plot_Flag = 1;

static int Zero_Flag = 0;
static int SpaceCraft_ID = -82;
static int Partition = 1;
static int Size_Flag = 0;
static int Pad_Class = 0;
static int *record_count_pointer = NULL;
static int open_file_count = 0;
static int maximum_open_file_count = 20;
char Delimiter = { ',' };

int Line_Pad = 0;
/* char database[] = {"/opt/ project/ cassini/ data/ database/ CassiniJPL.db"}; */

char *g_sDatabase = NULL;

struct DATABASE_TIME_TAG database_start_time =
	 { 1997, 10, 24, 297, 22, 27, 02.0 };
char database_sclk_start[64] = { "1/1000000000:000" };

int database_sclk_start_i = CASSINI_STARTING_SCLK;
struct DATABASE_TIME_TAG database_stop_time = { 2035, 1, 1, 1, 0, 0, 0.0 };
char database_sclk_stop[64] = { "1/2147483647:255" };

int database_sclk_stop_i = 0x7FFFFFFF;

struct HFR_STATUS *hfr_status_packet;
struct LP_STATUS *lp_status_packet;

static char new_stim_filename_1[64] = { "" };
static char new_stim_filename_2[64] = { "" };

int split_stim_flag = 0;
int debug_flag = 0;
int maximum_difference = 1001;
int KWBR60 = 60;
int KWFR60 = 1440;
int KANCIL = 1440;
int TIMEFLAG = 0;

double maximum_byte_count = 3000E6;

int buffer_size = BUFFER_SIZE;

/*  static FILE *output_handle = NULL;     */
/*  static FILE *raw_output_handle = NULL; */

static FILE *stim_handle_1 = NULL;
static FILE *stim_handle_2 = NULL;
static int write_flag = 0;

/* #ifdef ARCHIVE_TIME_V1 */
int spice_flag = 0;
static int archive_time_format = 1;
/* #endif
#ifdef ARCHIVE_TIME_V2
int spice_flag = 3;
static int archive_time_format = 2;
#endif
#ifdef ARCHIVE_TIME_V3
int spice_flag = 0;
static int archive_time_format = 3;
#endif
#ifdef ARCHIVE_TIME_V4
int spice_flag = 3;
static int archive_time_format = 4;
#endif
*/

int ephem_flag = 1;
static struct RPWS_buffer *buffer;
static struct ARCHIVE_TIME_SERIES *archive;
FILE *debug_file = stdout;

static struct RPWS_LABEL *rpws_working_label = NULL;
static struct RPWS_LABEL *rpws_working_stim = NULL;
static struct RPWS_LABEL *rpws_working_raw = NULL;

static struct RPWS_LABEL *rpws_label_head = NULL; /* Yet another f***ing global  */
                                                  /* var, thanks  Willy!  Also   */
                                                  /* thanks Fortran for teaching */
                                                  /* people to program this way  */
static struct RPWS_LABEL *rpws_label_raw = NULL;
static struct RPWS_LABEL *rpws_label_stim = NULL;

static struct RPWS_LABEL *rpws_wbr_time;
static struct RPWS_LABEL *rpws_wfr_time;
static struct RPWS_LABEL *rpws_dataset_time;

int WBR_Frequency_Band[] = { ARCH_BAND_10KHZ, ARCH_BAND_75KHZ,  0 };

int WFR_Frequency_Band[] = { ARCH_BAND_25HZ,  ARCH_BAND_2500HZ, 0 };

char *Frequency_Band[] = { "25HZ", "2_5KHZ", "10KHZ", "75KHZ" };

int sub_rti_table[256];

/*****************************************************************************/
int setup_sub_rti_table(int flag)
{
	int i;
	int usec;
	for(i = 255; i > 244; i--) {
		sub_rti_table[i] = 0;
		if(flag)
			fprintf(stdout, "sub_rti_table[%d]=%d\n", i, sub_rti_table[i]);
	}

	for(i = 244; i > 2; i--) {
		usec = (244 - i) * 512;
		sub_rti_table[i] = (usec / 1000) & 0xFF;
		if(flag)
			fprintf(stdout, "sub_rti_table[%d]=%d\n", i, sub_rti_table[i]);
	}

	for(i = 2; i > -1; i--) {
		sub_rti_table[i] = 0;
		if(flag)
			fprintf(stdout, "sub_rti_table[%d]=%d\n", i, sub_rti_table[i]);
	}

	return 0;
}

/*****************************************************************************/
int debug001(int line, char *text, struct RPWS_LABEL *label_head,
				 struct RPWS_LABEL *label)
{
	static int flag = 1;
	static int flag_max = 0x7FFFFFFF;
	struct RPWS_LABEL *temp;
	if(flag > flag_max) {
		fprintf(stderr, "Halt at debug001 in rpws_archive.c");
		exit(1);
	}
	if(flag && (flag < flag_max)) {
		fprintf(stdout, "\n");
		fprintf(stdout, "%5d %12s ", line, text);
		if(label) {
			fprintf(stdout, "0x%08p ", label);
			if(label->filename)
				fprintf(stdout, "\"%s\" ", label->filename);
		}
		fprintf(stdout, "label_head:");
	}
	temp = label_head;
	while(temp) {
		if(flag && (flag < flag_max))
			fprintf(stdout, "0x%08p ", temp);
		temp = temp->link;
	}
	if(flag && (flag < flag_max))
		fprintf(stdout, "\n");
	temp = rpws_label_head;
	while(temp) {
		if(temp->filename)
			if(flag && (flag < flag_max))
				fprintf(stdout, "(%d)[%p]%s ",
						  strlen(temp->filename),
						  temp->filename, temp->filename);
			else {
				flag++;
				if(flag && (flag < flag_max))
					fprintf(stdout, "******** ");
			}
		temp = temp->link;
	}
	if(flag && (flag < flag_max)) {
		fflush(stdout);
	}
	if(flag)
		flag++;
	return 1;
}

/**********************************************************************
 * Got moved...	
 **********************************************************************/
int new_dir_mkdir(const char *path, mode_t mode)
{
	if(strstr(path, "//"))
		return 0;
	return mkdir(path, mode);
}


/**********************************************************************
 *            Compare filename times:		                        *
 *	Return								*
 *		NULL		data before requested time		*
 *		filename	data within requested time		*
 *									*
 *	Discard data that occurs prior to the requested start time	*
 **********************************************************************/
char *discard_before_database(char *filename, char *stop_time)
{
	struct DATABASE_TIME_TAG data_stop_time;
	double d_data_stop_time;
	double d_database_start_time;
	if(!stop_time)
		return NULL;
	if(!stop_time[0])
		return NULL;
	if(!filename)
		return NULL;
	parsetime(stop_time,
				 &data_stop_time.year,
				 &data_stop_time.month,
				 &data_stop_time.mday,
				 &data_stop_time.yday,
				 &data_stop_time.hour,
				 &data_stop_time.minute, &data_stop_time.second);
	d_data_stop_time = ttime(&data_stop_time.year,
									 &data_stop_time.month,
									 &data_stop_time.mday,
									 &data_stop_time.yday,
									 &data_stop_time.hour,
									 &data_stop_time.minute,
									 &data_stop_time.second);
	d_database_start_time = ttime(&database_start_time.year,
											&database_start_time.month,
											&database_start_time.mday,
											&database_start_time.yday,
											&database_start_time.hour,
											&database_start_time.minute,
											&database_start_time.second);
	if(d_data_stop_time < d_database_start_time)
		return NULL;
	return filename;
}

/* ************************************************************************** */
/* Compare filename times:                                                    */
/*	Return                                                                     */
/*		NULL		data after requested time                                      */
/*		filename	data within requested time                                     */
/*                                                                            */
/*	Discard data that occurs after to the requested start time                 */
/* ************************************************************************** */
char *discard_after_database(char *filename, char *start_time)
{
	struct DATABASE_TIME_TAG data_start_time;
	double d_data_start_time;
	double d_database_stop_time;
	if(!start_time)
		return NULL;
	if(!start_time[0])
		return NULL;
	if(!filename)
		return NULL;
	parsetime(start_time,
				 &data_start_time.year,
				 &data_start_time.month,
				 &data_start_time.mday,
				 &data_start_time.yday,
				 &data_start_time.hour,
				 &data_start_time.minute, &data_start_time.second);
	d_data_start_time = ttime(&data_start_time.year,
									  &data_start_time.month,
									  &data_start_time.mday,
									  &data_start_time.yday,
									  &data_start_time.hour,
									  &data_start_time.minute,
									  &data_start_time.second);
	d_database_stop_time = ttime(&database_stop_time.year,
										  &database_stop_time.month,
										  &database_stop_time.mday,
										  &database_stop_time.yday,
										  &database_stop_time.hour,
										  &database_stop_time.minute,
										  &database_stop_time.second);
	if(d_data_start_time > d_database_stop_time)
		return NULL;
	return filename;
}

/**********************************************************************
 *            Compare times:			                        *
 *	Return								*
 *		-1	data before requested time			*
 *		 0	data within requested time			*
 *		+1	data after requested time			*
 **********************************************************************/
int compare_time(struct event_clock *event_sclk)
{
	int status = 0;

	if(event_sclk->seconds < database_sclk_start_i)
		status = -1;
	if(event_sclk->seconds > database_sclk_stop_i)
		status = 1;
	/* fprintf(stdout,"%5d %d %08X < %08X < %08X\n", __LINE__, status,
	   database_sclk_start_i, event_sclk->seconds, database_sclk_stop_i); */
	return status;
}

/**********************************************************************
 *									*
 *	WBR Status:							*
 *		Move status from minipacket to archive packet		*
 *		altering antenna/band select to use common		*
 *		bit patterns (un-ambiguous, to boot)			*
 *									*
 **********************************************************************/
int WBR_Antenna[] = { ARCH_ANTENNA_EX,
	ARCH_ANTENNA_BX,
	ARCH_ANTENNA_EW,
	ARCH_ANTENNA_HF,
	ARCH_ANTENNA_LP,
	0
};

/* ************************************************************************* */
void prep_wbr(struct ARCHIVE_TIME_SERIES *archive,
				  struct RPWS_buffer *buffer)
{
	int WBR_Analog_Gain[] = { ARCH_GAIN_00_DB,
		ARCH_GAIN_10_DB,
		ARCH_GAIN_20_DB,
		ARCH_GAIN_30_DB,
		ARCH_GAIN_40_DB,
		ARCH_GAIN_50_DB,
		ARCH_GAIN_60_DB,
		ARCH_GAIN_70_DB,
		0
	};
	int analog_gain;
	int agc_value;
	int antenna;
	int frequency_band;
	int msf;
	int sub_rti = 0;
	int lp_dac_0 = 0;
	int hfr_xlate = 0;
	int agc_enabled = 0;
	int time_quality = 0;
	int timeout = 0;

	set_status(&archive->validity_flag, ARCH_VALIDITY_FLAG_WBR, 1);
	/******************************************************* GAIN ****/
	analog_gain = get_status(buffer->packet.mpp.mini_packet,
									 WBR_gain, 0) & 0x07;
	set_status(&archive->gain,
				  ARCH_GAIN_ANALOG_GAIN, WBR_Analog_Gain[analog_gain]);
	/******************************************************* ANTENNA ****/
	antenna = get_status(buffer->packet.mpp.mini_packet,
								WBR_antenna, 0) & 0x07;
	archive->antenna = WBR_Antenna[antenna];
	/******************************************************* BAND ****/
	frequency_band = get_status(buffer->packet.mpp.mini_packet,
										 WBR_frequency_band, 0) & 0x01;
	archive->frequency_band = WBR_Frequency_Band[frequency_band];
	/******************************************************* MSF ****/
	msf = get_status(buffer->packet.mpp.mini_packet, WBR_MSF, 0) & 0x01;
#ifdef DEBUG2050
	if(msf) {
		fprintf(debug_file, "%s/%d MSF SET\n", __FILE__, __LINE__);
	}
#endif

	/* AGC */
	/* We have this status bit in the WBR all of the time, so */
	/* it's OK to simply copy it into the target buffer */
	agc_value = get_status(buffer->packet.mpp.mini_packet,
								  WBR_AGC_value, 0);
	agc_enabled = get_status(buffer->packet.mpp.mini_packet,
									 WBR_AGC_flag, 0) & 0x01;
	archive->agc = agc_value;
	set_status(&archive->status_flag,
				  ARCH_STATUS_FLAG_AGC_ENABLE, agc_enabled);
	/* Time Quality */
	time_quality = get_status(buffer->packet.mpp.mini_packet,
									  WBR_time_flag, 0) & 0x01;
	/* Timeout */
	timeout = get_status(buffer->packet.mpp.mini_packet,
								WBR_timeout_flag, 0) & 0x01;
	set_status(&archive->status_flag, ARCH_STATUS_FLAG_TIMEOUT, timeout);
	
	/* MSF fields */
	if(msf) {
		set_status(&archive->validity_flag, ARCH_VALIDITY_FLAG_MSF, 1);
		sub_rti = get_status(buffer->packet.mpp.mini_packet, WBR_sub_RTI, 0);
		archive->sub_rti = sub_rti_table[sub_rti];

		if(archive->fsw_ver > 205) {
			if(archive->sub_rti) {
				set_status(&archive->validity_flag,
							  ARCH_VALIDITY_FLAG_SUB_RTI, 1);
				set_status(&archive->status_flag,
							  ARCH_STATUS_FLAG_FINE_TIME_QUALITY, time_quality);
			}
		}
		if(archive->antenna == ARCH_ANTENNA_LP) {
			lp_dac_0 = get_status(buffer->packet.mpp.mini_packet,
										 WBR_LP_DAC_0, 0);
			set_status(&archive->validity_flag,
						  ARCH_VALIDITY_FLAG_LP_DAC_0, 1);
			archive->lp_dac_0 = lp_dac_0;
		}
		if(archive->antenna == ARCH_ANTENNA_HF) {
			int delta;
			if(hfr_status_packet)
				delta =
					 archive->sclk.seconds - hfr_status_packet->MS_event_clock;
			else
				delta = archive->sclk.seconds;	/* shit shit shit */
			if(delta < 0)
				delta = -delta;
			hfr_xlate =
				 get_status(buffer->packet.mpp.mini_packet, WBR_HFR_translate,
								0);
			set_status(&archive->validity_flag, ARCH_VALIDITY_FLAG_HFR_XLATE,
						  1);
			archive->hfr_xlate = hfr_xlate;
			
			/*********************************************************************/
			/* HFR conversion freq                                               */
			/* We're going to rework this a little to accomodate all HFR         */
			/* down-convert frequencies (LSB won't be used to select H1/H2,      */
			/* we'll have to look into the HFR packet to find H1/H2 and verify   */
			/* the freq in use                                                   */
			if(hfr_xlate) {	                
				if(hfr_xlate & 1)					 
					set_status(&archive->status_flag,			
								  ARCH_STATUS_FLAG_HFR_H1, 1);
				else
					set_status(&archive->status_flag,
								  ARCH_STATUS_FLAG_HFR_H2, 1);
			}


			/*********************************************************************/
			/* If we have close HFR data, take the H1/H2 information directly    */
			/* from the HFR setup.  That way, if we happen to ask for a	strange  */
			/* setting, it will trickle through...                               */

			if(0) {					  /* (delta<16) */
				switch (hfr_status_packet->MS_receiver) {
				case HFR_RECEIVER_H1:
					set_status(&archive->status_flag,
								  ARCH_STATUS_FLAG_HFR_H1, 1);
					set_status(&archive->status_flag,
								  ARCH_STATUS_FLAG_HFR_H2, 0);
					break;
				case HFR_RECEIVER_H2:
					set_status(&archive->status_flag,
								  ARCH_STATUS_FLAG_HFR_H1, 0);
					set_status(&archive->status_flag,
								  ARCH_STATUS_FLAG_HFR_H2, 1);
					break;
				}
			}
		}
	}
}

/* ************************************************************************** */
/*	WFR Status:                                                                */
/*    Move status from minipacket to archive packet                           */
/*    altering antenna/band select to use common                              */
/*    bit patterns (un-ambiguous, to boot)                                    */
/*                                                                            */
/* ************************************************************************** */
void prep_wfr(struct ARCHIVE_TIME_SERIES *archive,
				  struct RPWS_buffer *buffer)
{
	int Analog_Gain[] = { ARCH_GAIN_00_DB,
		ARCH_GAIN_10_DB,
		ARCH_GAIN_20_DB,
		ARCH_GAIN_30_DB,
		0
	};
	int Walsh_Gain[] = { ARCH_WALSH_GAIN_0,
		ARCH_WALSH_GAIN_1,
		ARCH_WALSH_GAIN_2,
		ARCH_WALSH_GAIN_3,
		0
	};
	int Antenna_0[] = {
		ARCH_ANTENNA_EU,			  /* LP Ex+ */
		ARCH_ANTENNA_EX,
		0
	};
	int Antenna_1[] = {
		ARCH_ANTENNA_EV,			  /* LP Ex- */
		ARCH_ANTENNA_EW,			  /* Ez */
		0
	};
	int Antenna_2[] = {
		ARCH_ANTENNA_BX,
		ARCH_ANTENNA_LP,			  /* LP Sphere */
		0
	};
	int channel_index;
	int mode_index;
	int channel;
	int analog_gain;
	int walsh_gain;
	int antenna;
	int frequency_band;
	int msf;
	int lp_dac_0 = 0;
	int lp_dac_1 = 0;

	set_status(&archive->validity_flag, ARCH_VALIDITY_FLAG_WFR, 1);
	/*********************************************** CHANNEL/MODE ****/
	channel_index = get_status(buffer->packet.mpp.mini_packet,
										WFR_channel_number, 0) & 0x07;
	if(get_status(buffer->packet.mpp.mini_packet,	/* This bit indicates */
					  WFR_lp_mode, 0))	/* that the channel mode */
		if(6 == get_status(buffer->packet.mpp.mini_packet,	/* is shifted
																			   (i.e. 012 is */
								 WFR_channel_mode, 0))
			/* really 2-3-4 */
			/* is shifted (i.e. 2-3-4) */
			channel_index += 2;	  /* MODE-6 ONLY !!! */
	mode_index = get_status(buffer->packet.mpp.mini_packet,
									WFR_channel_mode, 0) & 0x07;
	channel = channel_index;	  /* translation, if necessary */
	
	/******************************************************* GAIN ****/
	if(1) {
		walsh_gain = get_status(buffer->packet.mpp.mini_packet,
										WFR_Walsh_DGF, 0);
		if(0)
			set_status(&archive->gain,
						  ARCH_GAIN_WALSH_DGF, Walsh_Gain[walsh_gain]);
	}
	
	switch (channel) {
	case CHANNEL_0:
		analog_gain = get_status(buffer->packet.mpp.mini_packet,
										 WFR_CH0_gain, 0) & 0x03;
		set_status(&archive->gain,
					  ARCH_GAIN_ANALOG_GAIN, Analog_Gain[analog_gain]);
		break;
	case CHANNEL_1:
		analog_gain = get_status(buffer->packet.mpp.mini_packet,
										 WFR_CH1_gain, 0) & 0x03;
		set_status(&archive->gain,
					  ARCH_GAIN_ANALOG_GAIN, Analog_Gain[analog_gain]);
		break;
	case CHANNEL_2:
	case CHANNEL_3:
	case CHANNEL_4:
		analog_gain = get_status(buffer->packet.mpp.mini_packet,
										 WFR_CH234_gain, 0) & 0x03;
		set_status(&archive->gain,
					  ARCH_GAIN_ANALOG_GAIN, Analog_Gain[analog_gain]);
		break;
	}
	/******************************************************* ANTENNA ****/
	switch (channel) {
	case CHANNEL_0:
		antenna = get_status(buffer->packet.mpp.mini_packet,
									WFR_CH0_antenna, 0) & 0x01;
		archive->antenna = Antenna_0[antenna];
		if(!antenna)				  /* 0 indicates cylinder (i.e. from LP
										     I->V) */
			set_status(&archive->status_flag, ARCH_STATUS_FLAG_EU_CURRENT, 1);
		break;
	case CHANNEL_1:
		antenna = get_status(buffer->packet.mpp.mini_packet,
									WFR_CH1_antenna, 0) & 0x01;
		archive->antenna = Antenna_1[antenna];
		if(!antenna)				  /* 0 indicates cylinder (i.e. from LP
										     I->V) */
			set_status(&archive->status_flag, ARCH_STATUS_FLAG_EV_CURRENT, 1);
		break;
	case CHANNEL_2:
		antenna = get_status(buffer->packet.mpp.mini_packet,
									WFR_CH2_antenna, 0) & 0x01;
		archive->antenna = Antenna_2[antenna];
		break;
	case CHANNEL_3:
		archive->antenna = ARCH_ANTENNA_BY;
		break;
	case CHANNEL_4:
		archive->antenna = ARCH_ANTENNA_BZ;
		break;
	}
	/******************************************************* BAND ****/
	frequency_band = get_status(buffer->packet.mpp.mini_packet,
										 WFR_frequency_band, 0) & 0x01;
	archive->frequency_band = WFR_Frequency_Band[frequency_band];
	/******************************************************* MSF ****/
	msf = get_status(buffer->packet.mpp.mini_packet, WFR_MSF, 0) & 0x01;
	/******************************************************* MSF fields ****/
	if(msf) {
		set_status(&archive->validity_flag, ARCH_VALIDITY_FLAG_MSF, 1);
		lp_dac_0 = get_status(buffer->packet.mpp.mini_packet,
									 WFR_LP_DAC_0, 0);
		set_status(&archive->validity_flag, ARCH_VALIDITY_FLAG_LP_DAC_0, 1);
		archive->lp_dac_0 = lp_dac_0;
		lp_dac_1 = get_status(buffer->packet.mpp.mini_packet,
									 WFR_LP_DAC_1, 0);
		set_status(&archive->validity_flag, ARCH_VALIDITY_FLAG_LP_DAC_1, 1);
		archive->lp_dac_1 = lp_dac_1;
	}
}


/**********************************************************************
 *	Take the dataset duration into account when placing 		*
 *		the stop time						*
 **********************************************************************/
int offset_sclk_stop(struct RPWS_LABEL *rpws_working_label)
{
	int duration_seconds;
	int duration_fine;
	int sclk_partition;
	int sclk_seconds;
	int sclk_fine;
	char *temp;

	if(rpws_working_label->duration > 0.0) {
		duration_seconds = rpws_working_label->duration;
		duration_fine =
			 (rpws_working_label->duration - duration_seconds) * 256.;
		if(duration_fine > 255) {
			duration_seconds++;
			duration_fine = 0;
		}
		sclk_partition = strtol(rpws_working_label->sclk_start, &temp, 10);	/* partition 
																									 */
		sclk_seconds = strtol(temp + 1, &temp, 10);	/* skip "/" to seconds */
		sclk_fine = strtol(temp + 1, &temp, 10);	/* skip ":" or "." to fine 
																 */
		sclk_fine += duration_fine;
		sclk_seconds += duration_seconds;
		if(sclk_fine > 255) {
			sclk_seconds++;
			sclk_fine -= 256;
		}
		sprintf(rpws_working_label->sclk_stop, "%d/%d:%03d",
				  sclk_partition, sclk_seconds, sclk_fine);
	} else {
		strcpy(rpws_working_label->sclk_stop,
				 rpws_working_label->sclk_start);
	}
	return 0;
}

/**********************************************************************
 *									*
 *	Start and Stop times are known (expressed in SCLK)		*
 *	  so we'll now convert to SCET (and ET falls out int		*
 *	  the process).							*
 *	scet_start_2 is same time, different format (day, month, year	*
 *									*
 *	flag = 0	format start only				*
 *	flag = 1	format everything				*
 *									*
 **********************************************************************/
int format_scet(struct RPWS_LABEL *rpws_working_label, int flag)
{
	char *month_name[] = { "", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jly", "Aug", "Sep", "Oct", "Nov", "Dec", ""
	};
	int year;
	int month;
	int mday;
	int yday;
	int hour;
	int min;
	double dsec;
	double et;
	char ctemp[256];
	char *temp;
	static char format[] = { "D" };
	static char *ephem_format = { "%.3f" };
	int prec = 3;

	if(rpws_working_label->sclk_start) {
		if(rpws_working_label->sclk_start[0] != '1')
			return 0;
		if(rpws_working_label->sclk_start[1] != '/')
			return 0;

		scs2e_c(SpaceCraft_ID, rpws_working_label->sclk_start, &et);
		et2utc_c(et, format, prec, 32, ctemp);

		year = strtol(ctemp, &temp, 10);
		mday = strtol(temp + 1, &temp, 10);
		yday = 0;					  /* ignored on input */
		month = 1;
		hour = strtol(temp + 3, &temp, 10);
		min = strtol(temp + 1, &temp, 10);
		dsec = strtod(temp + 1, &temp);

		tnorm(&year, &month, &mday, &yday, &hour, &min, &dsec);

		sprintf(rpws_working_label->scet_start, "%04d-%03dT%02d:%02d:%06.3f",
				  year, yday, hour, min, dsec);
		strcpy(rpws_working_label->plot_start,
				 rpws_working_label->scet_start);
		sprintf(rpws_working_label->scet_start_2, "  %02d-%s-%04d  ", mday,
				  month_name[month], year);
		sprintf(rpws_working_label->scet_start_3,
				  "%04d-%02d-%02d(%03d)T%02d", year, month, mday, yday, hour);
		sprintf(rpws_working_label->ephem_start, ephem_format, et);
	}
	if(!flag)
		return 0;
	if(rpws_working_label->sclk_stop) {
		if(rpws_working_label->sclk_start[0] != '1')
			return 0;
		if(rpws_working_label->sclk_start[1] != '/')
			return 0;
		scs2e_c(SpaceCraft_ID, rpws_working_label->sclk_stop, &et);
		et2utc_c(et, format, prec, 32, ctemp);
		year = strtol(ctemp, &temp, 10);
		mday = strtol(temp + 1, &temp, 10);
		yday = 0;					  /* ignored on input */
		month = 1;
		hour = strtol(temp + 3, &temp, 10);
		min = strtol(temp + 1, &temp, 10);
		dsec = strtod(temp + 1, &temp);

		tnorm(&year, &month, &mday, &yday, &hour, &min, &dsec);

		sprintf(rpws_working_label->scet_stop, "%04d-%03dT%02d:%02d:%06.3f",
				  year, yday, hour, min, dsec);
		strcpy(rpws_working_label->plot_stop, rpws_working_label->scet_stop);
		sprintf(rpws_working_label->ephem_stop, ephem_format, et);
	}
	return 0;
}


/**********************************************************************
 *	Header size 8 or 10 bytes ???					*
 *		We have to inspect the MSF bit to determine if the	*
 *		status header in the minipacket is 8 or 10 bytes	*
 *		(Through careful planning WBR & WFR do this		*
 *		  in identical manners)					*
 **********************************************************************/
int prep_index(struct RPWS_buffer *buffer)
{
	int index = 8;
	switch (buffer->record_type & 0xFF) {
	case PACKET_TYPE_wfr:
		if(get_status(buffer->packet.mpp.mini_packet, WFR_MSF, 0)) {
#ifdef DEBUG2050
			fprintf(debug_file, "%s/%d INDEX+=2;\n", __FILE__, __LINE__);
#endif
			index += 2;
		}
		break;
	case PACKET_TYPE_wbr:
		if(get_status(buffer->packet.mpp.mini_packet, WBR_MSF, 0)) {
#ifdef DEBUG2050
			fprintf(debug_file, "%s/%d INDEX+=2;\n", __FILE__, __LINE__);
#endif
			index += 2;
		}
		break;
	}
	return index;
}

/**********************************************************************
 *	Data size (in bytes)						*
 *		Look at all the length indicators in the header		*
 *		and int the minipacket to determine the length		*
 *		of the record						*
 **********************************************************************/
int prep_size(struct RPWS_buffer *buffer, int size_flag, int Line)
{
	int size;
	size = get_status(buffer->packet.mpp.mini_packet,
							MP_packet_size_LSB, MP_packet_size_MSB);
	if(buffer->packet.index.data_start)
		size = buffer->packet.index.data_start;
	if(buffer->packet.index.data_length)
		size = buffer->packet.index.data_length;
	size += 3;						  /* adjust to actual size */
	size -= prep_index(buffer);  /* remove header bytes */
	if(size_flag) {
		switch (size - 2) {
		case 1 * 1024:
		case 2 * 1024:
		case 3 * 1024:
		case 4 * 1024:
		case 5 * 1024:
		case 6 * 1024:
		case 8 * 1024:
		case 10 * 1024:
		case 15 * 1024:
		case 20 * 1024:
			size -= 2;
			break;
		}
	}
#ifdef DEBUG2050
	if(size & 0x0F) {
		fprintf(debug_file,
				  "%s/%d called from line %d: SIZE %d\n",
				  __FILE__, __LINE__, Line, size);
	}
#endif
	return size;
}

/**********************************************************************
 *	Preform validity checks on data					*
 *	Check upper 4 bits of every other byte.  WBR should have some	*
 *      of these bits set and WFR should have all of these bits	*
 *	  cleared.							*
 **********************************************************************/
void validity_check(struct ARCHIVE_TIME_SERIES *archive)
{
	int i;
	int mask[2] = { 0, 0 };

	for(i = 16; i < archive->samples; i += 2) {
		mask[0] |= archive->time_series.byte_sample[i + 0];
		mask[1] |= archive->time_series.byte_sample[i + 1];
	}

	if(get_status(&archive->validity_flag, ARCH_VALIDITY_FLAG_WBR, 0)) {
		if(!(mask[0] & 0xF0) || !(mask[1] & 0xF0)) {
			set_status(&archive->status_flag, ARCH_STATUS_FLAG_SUSPECT, 1);
			if(!(archive->samples & 0x1FC))
				archive->samples &= 0x7FFC;
		}
	}

	if(get_status(&archive->validity_flag, ARCH_VALIDITY_FLAG_WFR, 0)) {
		if((mask[0] & 0xF0) && (mask[1] & 0xF0)) {
			set_status(&archive->status_flag, ARCH_STATUS_FLAG_SUSPECT, 1);
			if(!(archive->samples & 0x1FC))
				archive->samples &= 0x7FFC;
		}
	}

	return;
}

/**********************************************************************
 *	Move time series data 						*
 *		(have to keep track of MSF to start at the 		*
 *		 appropriate location)					*
 **********************************************************************/
int prep_data(struct ARCHIVE_TIME_SERIES *archive,
				  struct RPWS_buffer *buffer, int sample_size)
{
	int i;
	int mini_packet_offset;
	int size;
	
	size = prep_size(buffer, 0, __LINE__);
	archive->samples = size / sample_size;	/* samples */
		
	if(size & 0xFF) {
		if(debug_flag & 0x80) {
			fprintf(debug_file, "%c/%d ", __FILE__, __LINE__);
			switch (buffer->record_type & 0xFF) {
			case PACKET_TYPE_wfr:
				fprintf(debug_file, "prep_data(WFR)");
				break;
			case PACKET_TYPE_wbr:
				fprintf(debug_file, "prep_data(WBR)");
				break;
			case PACKET_TYPE_stim:
				fprintf(debug_file, "prep_data(STIM)");
				break;
			default:
				fprintf(debug_file, "prep_data()");
				break;
			}
			fprintf(debug_file, "size not appropriate %d\n", size);
			fprintf(debug_file,
					  "    mp(%d) start(%d) length(%d)   MSF bit(%d)%02X\n",
					  get_status(buffer->packet.mpp.mini_packet,
									 MP_packet_size_LSB, MP_packet_size_MSB),
					  buffer->packet.index.data_start,
					  buffer->packet.index.data_length,
					  get_status(buffer->packet.mpp.mini_packet, WBR_MSF, 0),
					  buffer->packet.mpp.mini_packet[6]);
			fprintf(debug_file, "    %d = length(%d) + 3\n",
					  buffer->packet.index.data_length + 3,
					  buffer->packet.index.data_length);
			fprintf(debug_file, "    %d = length(%d) - 8\n",
					  buffer->packet.index.data_length + 3 - 8,
					  buffer->packet.index.data_length);
			fprintf(debug_file, "   ");
			for(i = 0; i < 32; i += 2) {
				if(i == 10)
					fprintf(debug_file, "|");
				if(i < 10)
					fprintf(debug_file, " %02X %02X ",
							  buffer->packet.mpp.mini_packet[i + 0] & 0xFF,
							  buffer->packet.mpp.mini_packet[i + 1] & 0xFF);
				else
					fprintf(debug_file, " %02X %02X",
							  buffer->packet.mpp.mini_packet[i + 0] & 0xFF,
							  buffer->packet.mpp.mini_packet[i + 1] & 0xFF);
			}
			fprintf(debug_file, "\n");
			fprintf(debug_file, "\n");
		}
	}
	mini_packet_offset = prep_index(buffer);
	switch (sample_size) {
	case SAMPLE_SIZE_WBR:
		for(i = 0; i < archive->samples; i += sample_size) {
			archive->time_series.byte_sample[i] =
				 buffer->packet.mpp.mini_packet[mini_packet_offset + i];
		}
		break;
	case SAMPLE_SIZE_WFR:
		for(i = 0; i < archive->samples * 2; i += sample_size) {
			archive->time_series.byte_sample[i + 0] =
				 buffer->packet.mpp.mini_packet[mini_packet_offset + i + 1];
			archive->time_series.byte_sample[i + 1] =
				 buffer->packet.mpp.mini_packet[mini_packet_offset + i + 0];
		}
		break;
	}
	return 0;
}


/**********************************************************************
 *	DEBUG								*
 *		Dump the record we are writing				*
 **********************************************************************/
void write_dump_char(FILE * debug_file, char *title, char *buf, int count)
{
	int i;
	fprintf(debug_file, "%s:", title);
	for(i = 0; i < count; i++)
		fprintf(debug_file, " %02X", buf[i] & 0xFF);
	fprintf(debug_file, " ");
}

void write_dump(struct ARCHIVE_TIME_SERIES *buf)
{
	int i;
	int index;
/* #ifdef ARCHIVE_TIME_V1 */
	fprintf(debug_file, "V1 ");
	write_dump_char(debug_file, "scet", (char *) &buf->scet, 6);
	write_dump_char(debug_file, "sclk", (char *) &buf->sclk, 6);
/* #endif
#ifdef ARCHIVE_TIME_V2
      fprintf(debug_file,"V2 ");
      write_dump_char(debug_file, "et", (char *)&buf->et, 8);
      write_dump_char(debug_file, "sclk", (char *)&buf->sclk, 6);
#endif
#ifdef ARCHIVE_TIME_V3
      fprintf(debug_file,"V3 ");
      fprintf(debug_file, "scet: %s ", buf->scet);
      write_dump_char(debug_file, "sclk", (char *)&buf->sclk, 6);
#endif
#ifdef ARCHIVE_TIME_V4
      fprintf(debug_file,"V4 ");
      write_dump_char(debug_file, "et", (char *)&buf->et, 8);
      write_dump_char(debug_file, "scet", (char *)&buf->scet, 6);
      write_dump_char(debug_file, "sclk", (char *)&buf->sclk, 6);
#endif*/


	write_dump_char(debug_file, "rb", (char *) &buf->record_bytes, 2);
	write_dump_char(debug_file, "sc", (char *) &buf->samples, 2);
	write_dump_char(debug_file, "RT", (char *) &buf->data_rti, 2);
	fprintf(debug_file, "\n");

	write_dump_char(debug_file, "val", (char *) &buf->validity_flag, 1);
	write_dump_char(debug_file, "sts", (char *) &buf->status_flag, 1);
	write_dump_char(debug_file, "fb", (char *) &buf->frequency_band, 1);
	write_dump_char(debug_file, "gn", (char *) &buf->gain, 1);
	write_dump_char(debug_file, "ant", (char *) &buf->antenna, 1);
	write_dump_char(debug_file, "agc", (char *) &buf->agc, 1);
	write_dump_char(debug_file, "hfr", (char *) &buf->hfr_xlate, 1);
	write_dump_char(debug_file, "sub", (char *) &buf->sub_rti, 1);
	write_dump_char(debug_file, "dc0", (char *) &buf->lp_dac_0, 1);
	write_dump_char(debug_file, "dc1", (char *) &buf->lp_dac_0, 1);
	fprintf(debug_file, "\n");

	write_dump_char(debug_file, "data",
						 (char *) buf->time_series.byte_sample, 32);
	fprintf(debug_file, "\n");

	return;
}

/**********************************************************************
 *	DEBUG								*
 *		Dump the record we are writing				*
 **********************************************************************/
char *decode_fb[] = { "25Hz  ", "2.5KHz", "10Khz ", "75Khz ", 	""  };

char *decode_gn[] = {" 0dB", "10dB", "20dB", "30dB", "40dB", "50dB", "60dB", 
		               "70dB", "" };

char *decode_ant[] = {
	"Ex ",
	"Eu ",
	"Ev ",
	"Ew ",
	"Bx ",
	"By ",
	"Bz ",
	"   ",
	"HF ",
	"   ",
	"   ",
	"LP ",
	"   ",
	"   ",
	"   ",
	"INV"
};

void write_dump_decoded(struct ARCHIVE_TIME_SERIES *buf)
{
	fprintf(debug_file, "%s ", decode_fb[buf->frequency_band]);
	fprintf(debug_file, "%s ", decode_gn[buf->gain]);
	fprintf(debug_file, "%s ", decode_ant[buf->antenna]);
	fprintf(debug_file, "\n");
}

/**********************************************************************
 *	Determine the appropriate record size (i.e. file)		*
 *		to place this record.					*
 **********************************************************************/
static int file_size(int size, int packet_ID_value)
{
	int length = 4 * 1024;
	switch (packet_ID_value) {
	case ARCH_VALIDITY_FLAG_INST_ID_WBR:
	case ARCH_VALIDITY_FLAG_INST_ID_WFR:
		length = 20 * 1024;
		if(size <= 8 * 1024)		  /* */
			length = 8 * 1024;
		if(size <= 6 * 1024)		  /* */
			length = 6 * 1024;
		if(size <= 4 * 1024)		  /* */
			length = 4 * 1024;
		if(size <= 2 * 1024)		  /* */
			length = 2 * 1024;
		if(size <= 1 * 1024)		  /* */
			length = 1 * 1024;
		break;
	}
	return length;
}

/**********************************************************************
 *	Determine the appropriate record class (i.e. file)		*
 *		to place this record.					*
 **********************************************************************/
static int file_class(int size, int packet_ID_value)
{
	Pad_Class = 0;					  /* was RPWS_LABEL_PAD_4096; should match
										     file_size(length) variable) */
	switch (packet_ID_value) {
	case ARCH_VALIDITY_FLAG_INST_ID_WBR:
	case ARCH_VALIDITY_FLAG_INST_ID_WFR:
		Pad_Class = RPWS_LABEL_PAD_20480;
		if(size <= 8 * 1024)		  /* */
			Pad_Class = RPWS_LABEL_PAD_8192;
		if(size <= 6 * 1024)		  /* */
			Pad_Class = RPWS_LABEL_PAD_6144;
		if(size <= 4 * 1024)		  /* */
			Pad_Class = RPWS_LABEL_PAD_4096;
		if(size <= 2 * 1024)		  /* */
			Pad_Class = RPWS_LABEL_PAD_2048;
		if(size <= 1024)			  /* */
			Pad_Class = RPWS_LABEL_PAD_1024;
		break;
	}
	return Pad_Class;
}

/***********************************************************************
*	This little fragment makes sure we have the file open		*
*	that we're about to write to...  This is a problem immediately	*
*	after we've closed a bunch of files.				*
***********************************************************************/
static char *rpws_file_rename(struct RPWS_LABEL *label)
{
	static char filename[256];
	int windex = 10;
	memset(filename, 0, 256);
	if(strstr(label->filename, "WBR"))
		windex = RPWS_ARCHIVE_DATA_WBRFULL;
	if(strstr(label->filename, "WFR"))
		windex = RPWS_ARCHIVE_DATA_WFRFULL;
	if(strstr(label->filename, "RAW"))
		windex = RPWS_ARCHIVE_DATA_RAW;
	sprintf(filename, "%s/%s/%s/%s.%s",
			  w_directory[windex],
			  label->filepath1,
			  label->filepath2,
			  label->filename,
			  strstr(label->filename, "RAW") ? "PKT" : "DAT");
	return filename;
}

/*****************************************************************************/
static char *rpws_file_reopen(struct RPWS_LABEL *label, int line)
{
	char *filename;
	if(!label) {
		fprintf(debug_file, "%s/%d reopen bad label %d\n", __FILE__,
				  __LINE__, line);
		fflush(debug_file);
	}
	if(label->filehandle)
		return NULL;
	filename = rpws_file_rename(label);
	label->filehandle = fopen(filename, "a");
	if(!label->filehandle) {
		fprintf(debug_file, "%s/%d reopen failed %s (%s)\n",
				  __FILE__, __LINE__, strerror(errno), filename);
	}
	return filename;
}


/**********************************************************************
 *		Accumulate data size					*
 *	This is to allow us to stop writing at an appropriate		*
 *	point in the data set						*
 *	RETURN negative value when we reach/exceed maximum_byte_count	*
 **********************************************************************/
double rpws_accumulate(int byte_count)
{
	static FILE *result = stderr;
	static double accumulated_byte_count = 0.0;
	if(maximum_byte_count < 0)
		return 1.0;
	if(accumulated_byte_count >= maximum_byte_count) {
		if(result) {
			fprintf(result, "CD/DVD FULL %d\n", accumulated_byte_count);
			result = NULL;
		}
		return -accumulated_byte_count;
	}
	accumulated_byte_count += byte_count;
	return accumulated_byte_count;
}


/**********************************************************************
 *		WRITE DATA						*
 *	Some debugging goodies buried herer too...			*
 **********************************************************************/
int write_data(
	struct RPWS_LABEL* label, struct ARCHIVE_TIME_SERIES* archive, 
	int sample_size
){
	static char *text[] = { "Bad ",	/* 0 */
		"WFR ",						  /* 1 */
		"WBR ",						  /* 2 */
		"BAD "
	};									  /* 3 */
	static int count = 0;

	/**********************************************************************************/
	/* archive->record_bytes = archive->samples * sample_size; */
	/* archive->record_bytes = file_size(archive ->record_bytes, */
	/* get_status(&archive->validity_flag, */
	/* ARCH_VALIDITY_FLAG_INST_ID, */
	/* 0)) + 32; */
	/**********************************************************************************/

	archive->record_bytes = pad_class[Pad_Class];
	if(archive->record_bytes)
		archive->record_bytes += 32;
	else {
		archive->record_bytes = archive->samples * sample_size;
		archive->record_bytes = file_size(archive->record_bytes,
													 get_status(&archive->validity_flag,
																	ARCH_VALIDITY_FLAG_INST_ID,
																	0)) + 32;
	}
		
	if((debug_flag & 0x0800)) {
		fprintf(debug_file, "\n>>>   ");
		fprintf(debug_file, "%8d %4d = write record %s    %p:%s \n",
				  count++,
				  archive->record_bytes,
				  text[get_status(&archive->validity_flag,
										ARCH_VALIDITY_FLAG_INST_ID, 0)],
				  label->filehandle, label->filename);
		write_dump_decoded(archive);
		write_dump(archive);
	}
	if(!write_flag)
		return 0;

	if(!label->filehandle)
		rpws_file_reopen(label, __LINE__);

	if(!label->filehandle) {
		fprintf(debug_file, "%s/%d !label->filehandle ", __FILE__, __LINE__);
		fprintf(debug_file, "(%s)\n", label->filename);
	} else if(archive->record_bytes)
		if(rpws_accumulate(archive->record_bytes) >= 0.0) {
			label->record_count[0]++;
			label->dataset_size[0] += archive->record_bytes;
			if(!(debug_flag & 0x0800000))
				fwrite(archive, archive->record_bytes, 1, label->filehandle);
			/**/ if(record_count_pointer) {
				*record_count_pointer++;
				record_count_pointer = NULL;
			}

		}
	
	/* Doing the line below ties in knowledge of the specific malloc near line
	   4907 in main().  I would not hire a programmer who wrote this, unless
		they were straight out of school and weren't taught any better. */
	memset(archive, 0x00, 4096 + 32);
	
	return 1;
}

/**********************************************************************
 *		WRITE STIM DATA						*
 *	Some debugging goodies buried herer too...			*
 **********************************************************************/
int write_stim_data(FILE * write_file, char *buffer, char *filename)
{
	int len;
	if(!write_flag)
		return 0;
	if(debug_flag & 0x20000)
		fprintf(debug_file, "%s", buffer);
	if(write_file)
		if(len = strlen(buffer))
			if(rpws_accumulate(len) >= 0.0) {
				if(!(debug_flag & 0x0400000)) {
					fwrite(buffer, len, 1, write_file);
					/**/ fflush(write_file);
				}
			}
	return 1;
}

/**********************************************************************
 *		WRITE STIM LABEL					*
 **********************************************************************/
int clear_label(struct RPWS_LABEL *rpws_working_label)
{
	int i;
	rpws_working_label->instrument = RPWS_LABEL_STIM;
	rpws_working_label->span = 0.0;
	rpws_working_label->duration = 0.0;
	rpws_working_label->fband = 0;
	rpws_working_label->mode_mask = 0;
#ifdef HFR_XLATE
	for(i = 0; i < HFR_XLATE; i++)
		rpws_working_label->hfr_xlate[i] = 0;
#else
	rpws_working_label->hfr_xlate = 0;
#endif
	rpws_working_label->sample_count = 80;
	rpws_working_label->pad_class = RPWS_LABEL_PAD_STIM;
	rpws_working_label->record_size = 0;
	rpws_working_label->record_count[0] = 0;
	rpws_working_label->record_count[1] = 1;
	rpws_working_label->dataset_size[0] = 0;
	rpws_working_label->dataset_size[1] = 80;
	rpws_working_label->Product = Product[3];
	strcpy(rpws_working_label->filename, "");
	strcpy(rpws_working_label->filepath1, "");
	strcpy(rpws_working_label->filepath2, "");
	strcpy(rpws_working_label->sclk_start, "1/2147483647:255");
	strcpy(rpws_working_label->sclk_stop, "1/1000000000:000");
	strcpy(rpws_working_label->scet_start, "2999-001T00:00:00.000");
	strcpy(rpws_working_label->scet_start_2, "2999-01-01T00:00:00");
	strcpy(rpws_working_label->scet_start_3, "2999-01-01T00:00:00");
	strcpy(rpws_working_label->scet_stop, "1997-001T00:00:00.000");
	strcpy(rpws_working_label->ephem_start, "9999999999.0");
	strcpy(rpws_working_label->ephem_stop, "-9999999999.0");
	strcpy(rpws_working_label->plot_start, "2999-001T00:00:00.000");
	strcpy(rpws_working_label->plot_stop, "1997-001T00:00:00.000");
	return 1;
}

/**********************************************************************
 *		WRITE STIM LABEL					*
 **********************************************************************/
int write_stim_label(void)
{
	format_scet(rpws_label_stim, 1);
	if(!rpws_label_stim->Mission_Phase[0])
		rpws_label_stim->Mission_Phase = rpws_mpn_ver(rpws_label_stim);
	if(!rpws_label_stim->Target[0])
		rpws_label_stim->Target = rpws_target_ver(rpws_label_stim);
	if(!rpws_label_stim->Coordinate[0])
		rpws_label_stim->Coordinate = rpws_coordinate_ver(rpws_label_stim);
	if(!rpws_label_stim->Orbit[0])
		rpws_label_stim->Orbit = rpws_orbit_ver(rpws_label_stim);
#ifdef _New_Name_Handling_
	rpws_label_stim->Mission_Phase =
		 rpws_mission_phase_name(rpws_label_stim->sclk_start,
										 rpws_label_stim->sclk_stop, 0,
										 RPWS_LABEL_WFR);

	rpws_label_stim->Target = rpws_target_name(rpws_label_stim->sclk_start,
															 rpws_label_stim->sclk_stop,
															 0, RPWS_LABEL_WFR);

	rpws_label_stim->Coordinate =
		 rpws_coordinate_name(rpws_label_stim->sclk_start,
									 rpws_label_stim->sclk_stop, 0, RPWS_LABEL_WFR);
	rpws_label_stim->Orbit =
		 rpws_orbit_number(rpws_label_stim->sclk_start,
								 rpws_label_stim->sclk_stop, 0);
#endif
		  /****************************************************/
	/* account for the header line in the STIM.TAB file */
		  /****************************************************/
	rpws_label_stim->record_count[0]++;
	rpws_label_stim->record_count[1]++;
	if(Zero_Flag)
		rpws_time_patch(rpws_label_stim);
	rpws_label_write(rpws_label_stim, w_directory);

	clear_label(rpws_label_stim);
	return 0;
}

/**********************************************************************
 *
 *	
 *
 **********************************************************************/
FILE *new_stim_handle(FILE * stim_handle, char *new_name_, int flag,
							 int delimiter)
{
	FILE *temp;
	char stemp[256];
	static char stim_filename[4][64] = { "", "", "", "" };
	time_t time_t_uttime;
	struct tm *tm_uttime;
	int status;						  /* chmod */

	time_t_uttime = time(NULL);
	tm_uttime = gmtime(&time_t_uttime);

	if(stim_handle) {
		if(!strcmp(stim_filename[flag], new_name_))
			return stim_handle;
		fclose(stim_handle);
		stim_handle = NULL;
	}
	strcpy(stim_filename[flag], new_name_);
	temp = fopen(stim_filename[flag], "a");
	if(!temp) {
		fprintf(debug_file, "%s/%d Open fail:\"%s\" %s\n", __FILE__,
				  __LINE__, stim_filename[flag], strerror(errno));
		return NULL;
	}
	status =
		 chmod(stim_filename[flag], S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	if(status < 0)
		fprintf(debug_file, "%s/%d chmod failed (%s) %s\n", __FILE__,
				  __LINE__, stim_filename[flag], strerror(errno));
	if((flag & 1)) {
		fprintf(temp, "PDS_VERSION_ID       = PDS3\r\n");
		fprintf(temp, "RECORD_TYPE          = STREAM\r\n");
		fprintf(temp, "OBJECT               = TEXT\r\n");
		fprintf(temp, "  PUBLICATION_DATE   = %4d-%02d-%02d\r\n",
				  tm_uttime->tm_year + 1900,
				  tm_uttime->tm_mon + 1, tm_uttime->tm_mday);
		fprintf(temp, "  NOTE               = \"");
		fprintf(temp, "    Event Log, format description follows\"\r\n");
		fprintf(temp, "END_OBJECT           = TEXT\r\n");
		fprintf(temp, "END\r\n");
		fprintf(temp, "/*******************************************************/\r\n");
		fprintf(temp, "/**** %45s ****/\r\n", stim_filename[flag]);
		fprintf(temp, "/*******************************************************/\r\n");
		fprintf(temp, "/**** Decoded STIM records                          ****/\r\n");
		fprintf(temp, "/**** These records contain trigger numbers found   ****/\r\n");
		fprintf(temp, "/****   in the source dataset used to produce files ****/\r\n");
		fprintf(temp, "/****   in WBRFULL/WFRFULL                          ****/\r\n");
		fprintf(temp, "/**** HFR Sounder event records                     ****/\r\n");
		fprintf(temp, "/****   These records have the start time and       ****/\r\n");
		fprintf(temp, "/****   duration of sounder events that can         ****/\r\n");
		fprintf(temp, "/****   be the source of interference in other      ****/\r\n");
		fprintf(temp, "/****   receivers in the instrument.                ****/\r\n");
		fprintf(temp, "/**** Langmuir Probe Sweep event records            ****/\r\n");
		fprintf(temp, "/****   These records have the start time and       ****/\r\n");
		fprintf(temp, "/****   duration of sweep events that can           ****/\r\n");
		fprintf(temp, "/****   be the source of interference in other      ****/\r\n");
		fprintf(temp, "/****   receivers in the instrument.                ****/\r\n");
		fprintf(temp, "/*******************************************************/\r\n");
		fprintf(temp, "/**** This is a whitespace delimited table.  Line   ****/\r\n");
		fprintf(temp, "/****   lengths are variable.  The first 5 columns  ****/\r\n");
		fprintf(temp, "/****   are uniform, they all contain the same data ****/\r\n");
		fprintf(temp, "/****   in the same format.  Columns 6 and beyond   ****/\r\n");
		fprintf(temp, "/****   are unique (i.e. specific to the receiver)  ****/\r\n");
		fprintf(temp, "/*******************************************************/\r\n");
		fprintf(temp, "/**** Column 1 is SCLK expressed in decimal in the  ****/\r\n");
		fprintf(temp, "/****   format used by SPICE (as well as some of    ****/\r\n");
		fprintf(temp, "/****   the dump routines on the SOPC)              ****/\r\n");
		fprintf(temp, "/****   Note the slash character is unique to       ****/\r\n");
		fprintf(temp, "/****   this column of values.                      ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/**** Column 2 is SCLK expressed in hexadecimal.    ****/\r\n");
		fprintf(temp, "/****   The RTI number (0-7) follows the decimal    ****/\r\n");
		fprintf(temp, "/****   point.                                      ****/\r\n");
		fprintf(temp, "/****   Note that the lower case 'x' is unique      ****/\r\n");
		fprintf(temp, "/****   to this column of values (columns 1 to 5).  ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/**** Column 3 is SCET expressed in a form produced ****/\r\n");
		fprintf(temp, "/****   by SPICE (i.e. format \"D\")                  ****/\r\n");
		fprintf(temp, "/****   Note that the dash and the 'T' are unique   ****/\r\n");
		fprintf(temp, "/****   to this column of values.                   ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/**** Column 4 is the flight software version that  ****/\r\n");
		fprintf(temp, "/****   was present in the RPWS instrument when the ****/\r\n");
		fprintf(temp, "/****   data was collected                          ****/\r\n");
		fprintf(temp, "/****   Note that the character 'V' is unique       ****/\r\n");
		fprintf(temp, "/****   to this column of values.                   ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/**** Column 5 is a content flag                    ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/****   HCAL indicates the start of an HFR          ****/\r\n");
		fprintf(temp, "/****     calibration cycle                         ****/\r\n");
		fprintf(temp, "/****     Column 6 is the estimated duration        ****/\r\n");
		fprintf(temp, "/****       of the calibration cycle (in            ****/\r\n");
		fprintf(temp, "/****       seconds).  A value of 'unknown'         ****/\r\n");
		fprintf(temp, "/****       indicates the duration could not be     ****/\r\n");
		fprintf(temp, "/****       correctly calculated.                   ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/****   HSND indicates the start of an HFR          ****/\r\n");
		fprintf(temp, "/****     sounder activity                          ****/\r\n");
		fprintf(temp, "/****     Column 6 is the estimated duration        ****/\r\n");
		fprintf(temp, "/****       of the sounder cycle (in seconds).      ****/\r\n");
		fprintf(temp, "/****       A value of 'unknown' indicates the      ****/\r\n");
		fprintf(temp, "/****       duration could not be correctly         ****/\r\n");
		fprintf(temp, "/****       calculated (this is probably an         ****/\r\n");
		fprintf(temp, "/****       indication of a fragmented HFR          ****/\r\n");
		fprintf(temp, "/****       packet, the beginning of the packet     ****/\r\n");
		fprintf(temp, "/****       having been lost, data is shifted       ****/\r\n");
		fprintf(temp, "/****       into the status area and is             ****/\r\n");
		fprintf(temp, "/****       incorrectly decoded).                   ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/****   HDMP indicates the start of an HFR          ****/\r\n");
		fprintf(temp, "/****     memory dump (no additional data)          ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/****   LPSW indicates the start of an L/P sweep    ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/****     Column 6 is the estimated duration of the ****/\r\n");
		fprintf(temp, "/****       sweep cycle (in seconds).  A value      ****/\r\n");
		fprintf(temp, "/****       of 'unknown' indicates the duration     ****/\r\n");
		fprintf(temp, "/****       could not be correctly calculated.      ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/****   XSTM indicates a useless STIM record        ****/\r\n");
		fprintf(temp, "/****     probably as a result of FILL data         ****/\r\n");
		fprintf(temp, "/****     (no additional columnar data)             ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/****   STIM indicates a STIM record                ****/\r\n");
		fprintf(temp, "/****     Column 6 is the sequence number from the  ****/\r\n");
		fprintf(temp, "/****       STIM record.  Typically will be \"1\"     ****/\r\n");
		fprintf(temp, "/****     Column 7 are the 3 ID fields from the     ****/\r\n");
		fprintf(temp, "/****       STIM record, the middle number is used  ****/\r\n");
		fprintf(temp, "/****       to indicate what IEB trigger is         ****/\r\n");
		fprintf(temp, "/****       executing.                              ****/\r\n");
		fprintf(temp, "/****     Column 8 is the day-of-year field from    ****/\r\n");
		fprintf(temp, "/****       the STIM record.  This may be used to   ****/\r\n");
		fprintf(temp, "/****       double check the FSW version.           ****/\r\n");
		fprintf(temp, "/****     Column 9 is the IEB that is active        ****/\r\n");
		fprintf(temp, "/****       this may be useful for decoding the     ****/\r\n");
		fprintf(temp, "/****       trigger numbers in column 7.            ****/\r\n");
		fprintf(temp, "/*****                                             *****/\r\n");
		fprintf(temp, "/****     Triggers:                                 ****/\r\n");
		fprintf(temp, "/****        Trigger 10 is the basic configuration  ****/\r\n");
		fprintf(temp, "/****      trigger and appears as 0010 and 8010 in  ****/\r\n");
		fprintf(temp, "/****      this file.  This trigger places the      ****/\r\n");
		fprintf(temp, "/****      instrument in a known configuration and  ****/\r\n");
		fprintf(temp, "/****      enables only the MFR.  It is rarely used ****/\r\n");
		fprintf(temp, "/****      alone, usually occuring at the beginning ****/\r\n");
		fprintf(temp, "/****      of another trigger.                      ****/\r\n");
		fprintf(temp, "/****        Many triggers make use of setups in    ****/\r\n");
		fprintf(temp, "/****      other triggers.  This re-use of parts    ****/\r\n");
		fprintf(temp, "/****      of other triggers may result in STIM     ****/\r\n");
		fprintf(temp, "/****      record being emitted.  A good example    ****/\r\n");
		fprintf(temp, "/****      is trigger 12, it is used by many other  ****/\r\n");
		fprintf(temp, "/****      triggers, showing up as a 0012 trigger.  ****/\r\n");
		fprintf(temp, "/****        Cyclic triggers will typically send    ****/\r\n");
		fprintf(temp, "/****      STIM records with an indication of where ****/\r\n");
		fprintf(temp, "/****      in the cycle the trigger is executing.   ****/\r\n");
		fprintf(temp, "/****      An example is one of the trigger 5C      ****/\r\n");
		fprintf(temp, "/****      setups that caused STIM records of       ****/\r\n");
		fprintf(temp, "/****      805C, 815C, 825C, ... 8A5C, 815C, ...    ****/\r\n");
		fprintf(temp, "/****      as the trigger cycled through about      ****/\r\n");
		fprintf(temp, "/****      10 different configurations.             ****/\r\n");
		fprintf(temp, "/****        Most triggers require about 60 seconds ****/\r\n");
		fprintf(temp, "/****      to execute (this allows data collection  ****/\r\n");
		fprintf(temp, "/****      and delivery cycles to complete).  If    ****/\r\n");
		fprintf(temp, "/****      the delta-T is close to (or smaller      ****/\r\n");
		fprintf(temp, "/****      than) this time, the STIM is probably    ****/\r\n");
		fprintf(temp, "/****      part of another trigger.                 ****/\r\n");
		fprintf(temp, "/****                                               ****/\r\n");
		fprintf(temp, "/*******************************************************/\r\n");
		fprintf(temp, "/****  A note about time.                           ****/\r\n");
		fprintf(temp, "/****    Neither HFR nor L/P have a hardware        ****/\r\n");
		fprintf(temp, "/****    method to synchronize with the RTI         ****/\r\n");
		fprintf(temp, "/****    signal from CDS.  This means that the      ****/\r\n");
		fprintf(temp, "/****    time tags for the onset of an event is     ****/\r\n");
		fprintf(temp, "/****    approximate (accurate to only about        ****/\r\n");
		fprintf(temp, "/****    100 mSec).                                 ****/\r\n");
		fprintf(temp, "/*******************************************************/\r\n");
		if(debug_flag & 0x0400000) {
			fprintf(temp, "debug flag set to suppress STIM writes\n");
			fprintf(temp, "  (so you don't get nuthin' here!)\n");
		}
	} 
	else {
		sprintf(stemp, "\"-- SCLK/SPICE --\"%c"
				  "\" SCLK/RPWS  \"%c"
				  "\"-------- SCET -------\"%c"
				  "\"FSWVER\"%c"
				  "\"FLAG\"%c"
				  "\"- DUR -\"%c"
				  "\"SEQ\"%c"
				  "\"ID-0\"%c"
				  "\" ID-1 \"%c"
				  "\"ID-2\"%c"
				  "\"DOY\"%c"
				  "\"S/C SEQ\""
				  "                "
				  "                "
				  "                "
				  "                ",
				  delimiter,
				  delimiter,
				  delimiter,
				  delimiter,
				  delimiter,
				  delimiter,
				  delimiter, delimiter, delimiter, delimiter, delimiter);
		stemp[128] = 0x0D;
		stemp[129] = 0x0A;
		stemp[130] = 0;
		fprintf(temp, "%s", stemp);
		if(debug_flag & 0x0400000) {
			sprintf(stemp, "debug flag set to suppress STIM writes  "
					  "(so you don't get nuthin' here!)        "
					  "                                        " "           ");
			stemp[128] = 0x0D;
			stemp[129] = 0x0A;
			stemp[130] = 0;
			fprintf(temp, "%s", stemp);
		}
	}
	fflush(temp);
	return temp;
}

/*********************************************************
 **** If you're familiar with SPICE S- and P-kernels,	**** 
 **** you know that NAIF codes for spacecraft are 	****
 **** negative integers: -31 for Voyager 1, -32 for	****
 **** Voyager 2, -94 for Mars Observer, and so on. We	****
 **** borrow from this convention in defining		****
 **** instrument codes.				****
 ****							****
 **** Well, who'd-a thunk-it... negative numbers...	****
 **** sheesh, I guess that means Cassini is -82, then	****
 **** isn't it ?!?   Doooh				****
 *********************************************************/
double spice_time_ext(int seconds, int fine, int partition, int line)
{
	double et;
	char sclk_temp[256];
		/************************************************
		 *	Convert SCLK to string (spice format)	*
		 ************************************************/
	sprintf(sclk_temp, "%d/%d:%03d", partition, seconds, fine & RTI_MASK);	/* only 
																									   valid 
																									   to 
																									   RTI 
																									 */
		/************************************************
		 *	Conversion to SPICE internal	 	*
		 ************************************************/
	scs2e_c(SpaceCraft_ID, sclk_temp, &et);
	return et;
}

/**********************************************************************
 * Modified: 2007-09-20 by cwp to fix days since 1958 calclation.
 *	
 *	
 **********************************************************************/
double spice_time_x(struct archive_event_clock *sclk,
						  struct SPICE_TIME *spice_time_array,
						  char *new_utcout, int line)
{
	double et;
	int i;
	int prec = 3;
	char *format[2] = { "D", "J" };
	static char utcout[64];
	char utcTmp[64];
	static char jdout[64];
	double etTmp;

	memset(utcout, 0, 33);
	memset(jdout, 0, 33);
		/************************************************
		 *	Convert SCLK to spice format		*
		 ************************************************/
	et = spice_time_ext(sclk->seconds, sclk->fine, Partition, line);
		/************************************************
       * Conversion to data string  
       * Both julian ddd.fff to get days		
       * and yyy-ddd // hh:mm:ss.mmm to get milliseconds of day	
       *
       * NOTE: Julian day's change at noon to be one higher than the corresponding
       * UTC day, so we're taking the julian day calculation always at 2 am.
       * This depends on strtol to do the truncating. -cwp
       ************************************************/
	et2utc_c(et, format[0], prec, 32, utcout);
	for(i = 0; i < 8; i++)
		utcTmp[i] = utcout[i];
	strcpy(utcTmp + 8, "T02:00");
	utc2et_c(utcTmp, &etTmp);
	et2utc_c(etTmp, format[1], prec, 32, jdout);

		/************************************************
       * Goofey-ass, isn't it, BUT...
       * now we have spice's opinion so convert back yo internal
       *
       * SPICE JULIAN day is epoch 1/1/1 AD  subtract number of days to 
       * CASSINI epoch
       ************************************************/
	spice_time_array->days = strtol(&jdout[3], NULL, 10) - A_EPOCH;
	/************************************************
	 *	Now use HH:MM:SS.mmm to get 		*
	 *	seconds of day.				*
	 ************************************************/
	spice_time_array->hours = strtol(&utcout[12], NULL, 10);
	spice_time_array->minutes = strtol(&utcout[15], NULL, 10);
	spice_time_array->seconds = strtol(&utcout[18], NULL, 10);
	spice_time_array->milliseconds = strtol(&utcout[21], NULL, 10);

	spice_time_array->msec_of_day = spice_time_array->milliseconds;
	spice_time_array->msec_of_day += spice_time_array->seconds * 1000;
	spice_time_array->msec_of_day += spice_time_array->minutes * 1000 * 60;
	spice_time_array->msec_of_day +=
		 spice_time_array->hours * 1000 * 60 * 60;
	if(new_utcout) {
		memset(new_utcout, 0, 32);
		strncat(new_utcout, &utcout[0], 8);
		strncat(new_utcout, "T", 1);
		strncat(new_utcout, &utcout[12], 12);
		/* if(0) { fprintf(debug_file,"%5d 1 2 3\n", __LINE__);
		   fprintf(debug_file,"%5d 012345678901234567890123456789012\n",
		   __LINE__); fprintf(debug_file,"%5d UTC >%s<\n", __LINE__,
		   new_utcout); } */
	}
	return et;
}

/*****************************************************************************/

int spice_time(struct ARCHIVE_TIME_SERIES *archive, char *mssg, int line)
{
	char scet[128];
	double spice_et;
	int diff;
	struct SPICE_TIME spice_time_array;

	if(!spice_flag & 0x01)
		return 0;

	spice_et = spice_time_x(&archive->sclk, &spice_time_array, scet, line);

		/************************************************
		 *	Now calculate the difference 		*
		 *	between what was delivered with		*
		 *	the data and what the latest es_tim_ate	*
		 *	is...  Shouldn't be more than about	*
		 *	1 second different.			*
		 ************************************************/

/* #ifdef ARCHIVE_TIME_V1 */
	diff = spice_time_array.msec_of_day - archive->scet.msec_of_day;
	/**/
/* #endif
#ifdef ARCHIVE_TIME_V2
      diff  = (spice_et - archive->et) * 1000.;	/ * in milli-seconds * /
#endif
#ifdef ARCHIVE_TIME_V3
      diff = 0;
#endif
#ifdef ARCHIVE_TIME_V4
      diff  = (spice_et - archive->et) * 1000.;	/ * in milli-seconds * /
#endif
*/
		 /* diff -= (spice_time_array.days - archive->scet.days) * 86400 *
		    1000; */
		 if((abs(diff) > maximum_difference) && maximum_difference) {
		fprintf(debug_file, "%s/%d %s Event %08X.%02X  ",
				  __FILE__, __LINE__,
				  mssg, archive->sclk.seconds, archive->sclk.fine);
/* #ifdef ARCHIVE_TIME_V1 */
		fprintf(debug_file, "scet %d.%08d  ",
				  archive->scet.days, archive->scet.msec_of_day);
/* #endif
#ifdef ARCHIVE_TIME_V2
          fprintf(debug_file, "et %.3f  ", 
      				archive->et);
#endif
#ifdef ARCHIVE_TIME_V3
          fprintf(debug_file, "scet %s  ", 
      				scet);
#endif
#ifdef ARCHIVE_TIME_V4
          fprintf(debug_file, "et %.3f  ", 
      				archive->et);
          fprintf(debug_file, "scet %d.%08d  ", 
      				archive->scet.days,
      				archive->scet.msec_of_day);
#endif
*/
		fprintf(debug_file, "spice %d.%08d  ",
				  spice_time_array.days, spice_time_array.msec_of_day);
		fprintf(debug_file, "%02d:%02d:%02d.%03d  ",
				  spice_time_array.hours,
				  spice_time_array.minutes,
				  spice_time_array.seconds, spice_time_array.milliseconds);
		fprintf(debug_file, "difference milliseconds %4d ", diff);
		fprintf(debug_file, "\n");
	}

		/***************************************************
		 *	    Now update the record		   *
		 ***************************************************/

/* #ifdef ARCHIVE_TIME_V1 */
	archive->scet.days = spice_time_array.days;
	archive->scet.msec_of_day = spice_time_array.msec_of_day;
/* #endif
#ifdef ARCHIVE_TIME_V2
      archive->et = spice_et;
#endif
#ifdef ARCHIVE_TIME_V3
      memcpy(archive->scet, scet, sizeof(archive->scet));
#endif
#ifdef ARCHIVE_TIME_V4
      archive->scet.days         = spice_time_array.days;
      archive->scet.msec_of_day = spice_time_array.msec_of_day;
      archive->et = spice_et;
#endif
*/
	return 1;
}

/*****************************************************************************/
int dump_master_label(struct RPWS_LABEL *temp_label, int flag, int line)
{
	int i;
	if(0)
		return 0;
	if(flag)
		fprintf(stderr, "dump _master_label");
	fprintf(stderr, "    %8p <- RPWS_LABEL    line:%d", temp_label, line);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = struct RPWS_LABEL *link;", temp_label->link);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8d = int instrument;         ", temp_label->instrument);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8d = int fband               ", temp_label->fband);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8d = int mode_mask           ", temp_label->mode_mask);
	fprintf(stderr, "\n");
#ifdef RPWS_XLATE
	for(i = 0; i < HFR_XLATE; i++) {
		fprintf(stderr, "    %8d = int hfr_xlate[%d]       ", temp_label->hfr_xlate[i], i);
		fprintf(stderr, "\n");
	}
#else
	fprintf(stderr, "    %8d = int hfr_xlate           ", temp_label->hfr_xlate);
	fprintf(stderr, "\n");
#endif
	fprintf(stderr, "    %8f = float span;             ", temp_label->span);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8f = float duration;         ", temp_label->duration);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8X = int dataset_size %X;    ", temp_label->dataset_size[0], temp_label->dataset_size[1]);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8X = int sample_count;       ", temp_label->sample_count);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8X = int pad_class;          ", temp_label->pad_class);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8X = int record_size         ", temp_label->record_size);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8X = int record_count %8X;   ",
			  temp_label->record_count[0], temp_label->record_count[1]);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8d = int Label_Line_Pad;     ", temp_label->Label_Line_Pad);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = FILE *filehandle;       ", temp_label->filehandle);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *filename;         ", temp_label->filename);
	if(temp_label->filename)
		fprintf(stderr, "%s", temp_label->filename);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *filepath(n);      ", temp_label->filepath1);
	if(temp_label->filepath1)
		fprintf(stderr, "%s", temp_label->filepath1);
	if(temp_label->filepath2)
		fprintf(stderr, "/%s", temp_label->filepath2);
	if(temp_label->filepath3)
		fprintf(stderr, "/%s", temp_label->filepath3);
	fprintf(stderr, "\n");

	fprintf(stderr, "    %8p = char *thumbname;         ", temp_label->thumbname);
	if(temp_label->thumbname)
		fprintf(stderr, "%s", temp_label->thumbname);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *utc_date;         ",  temp_label->utc_date);
	if(temp_label->utc_date)
		fprintf(stderr, "%s", temp_label->utc_date);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *utc_time;         ", temp_label->utc_time);
	if(temp_label->utc_time)
		fprintf(stderr, "%s", temp_label->utc_time);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *sclk_start;       ", temp_label->sclk_start);
	if(temp_label->sclk_start)
		fprintf(stderr, "%s", temp_label->sclk_start);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *sclk_stop;        ", temp_label->sclk_stop);
	if(temp_label->sclk_stop)
		fprintf(stderr, "%s", temp_label->sclk_stop);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *scet_start;       ", temp_label->scet_start);
	if(temp_label->scet_start)
		fprintf(stderr, "%s", temp_label->scet_start);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *scet_start_2;     ", temp_label->scet_start_2);
	if(temp_label->scet_start_2)
		fprintf(stderr, "%s", temp_label->scet_start_2);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *scet_start_3;     ", temp_label->scet_start_2);
	if(temp_label->scet_start_3)
		fprintf(stderr, "%s", temp_label->scet_start_3);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *scet_stop;        ", temp_label->scet_stop);
	if(temp_label->scet_stop)
		fprintf(stderr, "%s", temp_label->scet_stop);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *ephem_start;      ",  temp_label->ephem_start);
	if(temp_label->ephem_start)
		fprintf(stderr, "%s", temp_label->ephem_start);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *ephem_stop;       ",  temp_label->ephem_stop);
	if(temp_label->ephem_stop)
		fprintf(stderr, "%s", temp_label->ephem_stop);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *plot_start;       ", temp_label->plot_start);
	if(temp_label->plot_start)
		fprintf(stderr, "%s", temp_label->plot_start);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *plot_stop;        ", temp_label->plot_stop);
	if(temp_label->plot_stop)
		fprintf(stderr, "%s", temp_label->plot_stop);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *Label_Version;    ", temp_label->Label_Version);
	if(temp_label->Label_Version)
		fprintf(stderr, "%s", temp_label->Label_Version);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *Mission_Phase;    ", temp_label->Mission_Phase);
	if(temp_label->Mission_Phase)
		fprintf(stderr, "%s", temp_label->Mission_Phase);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *Target;           ", temp_label->Target);
	if(temp_label->Target)
		fprintf(stderr, "%s", temp_label->Target);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *Coordinate;       ", temp_label->Coordinate);
	if(temp_label->Coordinate)
		fprintf(stderr, "%s", temp_label->Coordinate);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *Orbit;            ", temp_label->Orbit);
	if(temp_label->Orbit)
		fprintf(stderr, "%s", temp_label->Orbit);
	fprintf(stderr, "\n");
	fprintf(stderr, "    %8p = char *Product;          ", temp_label->Product);
	if(temp_label->Product)
		fprintf(stderr, "%s", temp_label->Product);
	fprintf(stderr, "\n");
	return 1;
}

/*****************************************************************************/
int build_master_label(struct RPWS_LABEL *temp_label)
{
	double start_et = -86400000.0;
	double stop_et = -86400000.0;
	static int buffer_size = 32;
	char temp[64];
	char *pReplace = NULL;

	if(!temp_label)
		return 0;
	/* dump_master_label(temp_label, 1, __LINE__); */

	/********************************************************
	 *	Conversion: UT to SCLK/ET		 	*
	 ********************************************************/
	utc2et_c(temp_label->scet_start, &start_et);
	strcpy(temp_label->plot_start, temp_label->scet_start);
	utc2et_c(temp_label->scet_stop, &stop_et);
	strcpy(temp_label->plot_stop, temp_label->scet_stop);

	sce2s_c(SpaceCraft_ID, start_et, buffer_size, temp_label->sclk_start);
	sce2s_c(SpaceCraft_ID, stop_et, buffer_size, temp_label->sclk_stop);

	sprintf(temp_label->ephem_start, "%-3f", start_et);
	sprintf(temp_label->ephem_stop, "%-3f", stop_et);

	if((pReplace = strchr(temp_label->sclk_start, '.')) != NULL)
		*pReplace = ':';			  /* change to to colon */

	if((pReplace = strchr(temp_label->sclk_stop, '.')) != NULL)
		*pReplace = ':';			  /* change to to colon */

	if((pReplace = strchr(temp_label->sclk_start, ' ')) != NULL)
		*pReplace = '\0';			  /* shorten the line a bit */

	if((pReplace = strchr(temp_label->sclk_stop, ' ')) != NULL)
		*pReplace = '\0';			  /* shorten the line a bit */

	/* dump_master_label(temp_label, 0, __LINE__); */
	return 1;
}

/*****************************************************************************/
void display_sclk_scet(struct RPWS_buffer *buffer)
{
	double et;
	char sclk_temp[256];
	char format[] = { "D" };
	char utcout[33];
	int prec = 3;
	int hour, min, sec;

	memset(utcout, 0, 33);

	sprintf(sclk_temp, "%d:%03d",
			  buffer->packet.chdo_tag.sclk.seconds,
			  buffer->packet.chdo_tag.sclk.fine);
	scs2e_c(SpaceCraft_ID, sclk_temp, &et);
	et2utc_c(et, format, prec, 32, utcout);

	fprintf(debug_file, "Scraft %08X.%02X  ",
			  buffer->packet.chdo_tag.sclk.seconds,
			  buffer->packet.chdo_tag.sclk.fine);
	sec = buffer->packet.chdo_tag.scet.milliseconds / 1000;
	hour = sec / 3600;
	min = sec / 60 - hour * 60;
	sec = sec % 60;
	fprintf(debug_file, "%s ", utcout);
	fprintf(debug_file, "%02d:%02d:%02d.%03d ",
			  hour, min, sec,
			  buffer->packet.chdo_tag.scet.milliseconds % 1000);
	fprintf(debug_file, "\n");
}


/*********************************************************
 *****	Prepare the ARCHIVE buffer		****
 *****	pre-clear and move common fields	****
 *********************************************************/
int prep(struct ARCHIVE_TIME_SERIES *archive,
			struct RPWS_buffer *buffer,
			struct event_clock *event_sclk,
			struct event_time *event_scet, double event_et)
{

	memset(archive, 0, buffer_size);

	if(event_sclk->seconds < CASSINI_STARTING_SCLK)
		return 0;
	if(event_sclk->seconds > CASSINI_MAXIMUM_CLOCK)
		return 0;


	archive->sclk.seconds = event_sclk->seconds;
	archive->sclk.partition = Partition;
	archive->sclk.fine = event_sclk->fine & RTI_MASK;


/* #ifdef ARCHIVE_TIME_V2
      archive->et = event_et;
#endif*/
/* #ifdef ARCHIVE_TIME_V1*/
	archive->scet.days = event_scet->days;
	archive->scet.msec_of_day = event_scet->milliseconds;
/* #endif
#ifdef ARCHIVE_TIME_V2
#endif*/

	archive->data_rti = get_status(buffer->packet.mpp.mini_packet,
											 MP_packet_rti_LSB, MP_packet_rti_MSB);
	return 1;
}

/**********************************************************************
 *									*
 *	STIM Status:							*
 *		Format fixed strings					*
 *									*
 *	1/123456789 0x12345678 2003-201T10:00:12.000 FSW=V2.x		*
 *									*
 **********************************************************************/
static char *prep_format[2] = { "D", "J" };

void prep_stim_time(char *buffer_1,
						  char *buffer_2,
						  struct ARCHIVE_TIME_SERIES *archive, char delimiter)
{
	char delimiter_string[16];
	char temp[64];
	static double et;
	int prec = 3;
	static char *ctemp;

	sprintf(delimiter_string, "%c", delimiter);
	/* SCLK/SPICE */

	sprintf(temp, "%d/%10d:%03d", Partition,
/*    			rpws->packet.chdo_tag.sclk.seconds,		*/
/*			rpws->packet.chdo_tag.sclk.fine & 0xE0);	*/
			  archive->sclk.seconds, archive->sclk.fine & 0xE0);
	/**/ strcat(buffer_1, temp);
	strcat(buffer_1, " ");
	strcat(buffer_2, "\"");
	strcat(buffer_2, temp);
	strcat(buffer_2, "\"");
	strcat(buffer_2, delimiter_string);

	/* SCLK/RPWS */

	sprintf(temp, "0x%08X.%1X",
/*			rpws->packet.chdo_tag.sclk.seconds, 		*/
/*			rpws->packet.chdo_tag.sclk.fine >> 5);		*/
			  archive->sclk.seconds, archive->sclk.fine >> 5);
	/**/ strcat(buffer_1, temp);
	strcat(buffer_1, " ");
	strcat(buffer_2, "\"");
	strcat(buffer_2, temp);
	strcat(buffer_2, "\"");
	strcat(buffer_2, delimiter_string);

	/* SCET */

	memset(temp, 0, 32);
/*    et = spice_time_ext(rpws->packet.chdo_tag.sclk.seconds, rpws->packet.chdo_tag.sclk.fine, Partition, __LINE__);*/
	et = spice_time_ext(archive->sclk.seconds, archive->sclk.fine,
							  Partition, __LINE__);
	/**/ et2utc_c(et, prep_format[0], prec, 32, temp);

	temp[8] = 0;
	temp[24] = 0;
	strcat(buffer_1, &temp[0]);
	strcat(buffer_1, "T");
	strcat(buffer_1, &temp[12]);
	strcat(buffer_1, " ");
	strcat(buffer_2, "\"");
	strcat(buffer_2, &temp[0]);
	strcat(buffer_2, "T");
	strcat(buffer_2, &temp[12]);
	strcat(buffer_2, "\"");
	strcat(buffer_2, delimiter_string);

	/* FSWVER */

	rpws_fsw_ver(archive, &Partition, NULL, &ctemp);
	strcat(buffer_1, ctemp);
	strcat(buffer_1, " ");
	strcat(buffer_2, "\"");
	strcat(buffer_2, ctemp);
	strcat(buffer_2, "\"");
	strcat(buffer_2, delimiter_string);
}

int prep_stim_fill(char *stemp, char *sequence, char delimiter)
{
	char temp[128];
	sprintf(temp, "\"   \"%c"	  /* "SEQ" */
			  "\"    \"%c"			  /* "ID-0" */
			  "\"      \"%c"		  /* "ID-1" */
			  "\"    \"%c"			  /* "ID-2" */
			  "\"   \"%c"			  /* "DOY" */
			  "\"%7s\""				  /* "S/C SEQ */
			  "                "	  /* */
			  "                ",  /* */
			  delimiter,
			  delimiter, delimiter, delimiter, delimiter, sequence);
	strcat(stemp, temp);
	return 0;
}

/**********************************************************************
 *									*
 *	STIM Status:							*
 *		Format strings						*
 *									*
 *	sclk scet STIM FSW=V2.n ID0=nn ID1=nn				*
 *	1/123456789 2003-201T10:00:12.000 FSW=V2.6			*
 *									*
 **********************************************************************/
int prep_stim(char *buffer_1,
				  char *buffer_2,
				  struct ARCHIVE_TIME_SERIES *archive,
				  struct RPWS_buffer *rpws, char delimiter)
{
	int Date;
	static char temp[32];
	static char *itemp;
	char delimiter_string[16];
	*buffer_1 = 0;
	*buffer_2 = 0;

	sprintf(delimiter_string, "%c", delimiter);

	rpws_ieb_ver(archive, &itemp);

	prep_stim_time(buffer_1, buffer_2, archive, delimiter);

	Date = get_status(rpws->packet.mpp.mini_packet, STIM_date, STIM_date_MSB);
	if(Date) {
		/* FLAG */
		strcat(buffer_1, "STIM ");
		strcat(buffer_2, "\"STIM\"");
		strcat(buffer_2, delimiter_string);

		/* - DUR - */
		strcat(buffer_2, "\"       \"");	/* N/A here */
		strcat(buffer_2, delimiter_string);

		/* SEQ */
		strcat(buffer_1, "Seq=");
		sprintf(temp, "%d", get_status(rpws->packet.mpp.mini_packet,
												 STIM_sequence, STIM_sequence_MSB));
		strcat(buffer_1, temp);
		strcat(buffer_1, " ");

		sprintf(temp, "%3d", get_status(rpws->packet.mpp.mini_packet,
												  STIM_sequence, STIM_sequence_MSB));
		strcat(buffer_2, "\"");
		strcat(buffer_2, temp);
		strcat(buffer_2, "\"");
		strcat(buffer_2, delimiter_string);

		/* ID-0 */
		strcat(buffer_1, "ID=");
		sprintf(temp, "%d",
				  get_status(rpws->packet.mpp.mini_packet, STIM_ID_0, STIM_ID_0_MSB));
		strcat(buffer_1, temp);
		strcat(buffer_1, ",");

		sprintf(temp, "%4d",
				  get_status(rpws->packet.mpp.mini_packet, STIM_ID_0, STIM_ID_0_MSB));
		strcat(buffer_2, "\"");
		strcat(buffer_2, temp);
		strcat(buffer_2, "\"");
		strcat(buffer_2, delimiter_string);

		/* ID-1 */
		sprintf(temp, "0x%04X",
				  get_status(rpws->packet.mpp.mini_packet, STIM_ID_1, STIM_ID_1_MSB)
			 );
		strcat(buffer_1, temp);
		strcat(buffer_1, ",");
		strcat(buffer_2, "\"");
		strcat(buffer_2, temp);
		strcat(buffer_2, "\"");
		strcat(buffer_2, delimiter_string);

		/* ID-2 */
		sprintf(temp, "%d",
				  get_status(rpws->packet.mpp.mini_packet, STIM_ID_2, STIM_ID_2_MSB));
		strcat(buffer_1, temp);
		strcat(buffer_1, " ");
		sprintf(temp, "%4d",
				  get_status(rpws->packet.mpp.mini_packet, STIM_ID_2, STIM_ID_2_MSB));
		strcat(buffer_2, "\"");
		strcat(buffer_2, temp);
		strcat(buffer_2, "\"");
		strcat(buffer_2, delimiter_string);

		sprintf(temp, "%d", Date);
		strcat(buffer_1, "Date=");
		strcat(buffer_1, temp);
		strcat(buffer_1, " ");
		sprintf(temp, "%3d", Date);
		strcat(buffer_2, "\"");
		strcat(buffer_2, temp);
		strcat(buffer_2, "\"");
		strcat(buffer_2, delimiter_string);

		/* S/C SEQ */
		strcat(buffer_1, itemp);
		sprintf(temp, "%7s", itemp);
		strcat(buffer_2, "\"");
		strcat(buffer_2, temp);
		strcat(buffer_2, "\"");
	} else {
		strcat(buffer_1, "XSTM ");
		strcat(buffer_2, "\"XSTM\"");	/* " FLAG " */
		strcat(buffer_2, delimiter_string);
		 /*UNKNOWN*/ strcat(buffer_2, "\"       \"");	/* " -DUR- " */
		strcat(buffer_2, delimiter_string);
		prep_stim_fill(buffer_2, itemp, delimiter);
	}

	strcat(buffer_1, "\r\n");

	buffer_2[128] = 0x0D;
	buffer_2[129] = 0x0A;
	buffer_2[130] = 0;
	return 0;
}

/**********************************************************************
 *	
 *	
 *	
 **********************************************************************/
void prep_hfr_sound(char *buffer,
						  char *buffer2,
						  struct ARCHIVE_TIME_SERIES *archive,
						  struct RPWS_buffer *rpws,
						  struct HFR_STATUS *hfr_status_packet,
						  char *flag, char delimiter)
{
	static char temp[32];
	int duration;
	char *sequence;

	*buffer = 0;
	*buffer2 = 0;

	rpws_ieb_ver(archive, &sequence);
	prep_stim_time(buffer, buffer2, archive, delimiter);

	strcat(buffer, flag);
	strcat(buffer, " ");

	sprintf(temp, "\"%s\"%c", flag, delimiter);
	strcat(buffer2, temp);

	duration = hfr_status_packet->hfr_event_duration;

	if(strcmp(flag, "HDMP")) {
		if(duration > 120000)
			sprintf(temp, "Dur=unknown ");
		else
			sprintf(temp, "Dur=%d.%03d ", duration / 1000, duration % 1000);
		strcat(buffer, temp);
	}
	strcat(buffer, "\r\n");

	if(strcmp(flag, "HDMP")) {
		if(duration > 120000)
			sprintf(temp, "\"UNKNOWN\"%c", delimiter);
		else
			sprintf(temp, "\"%3d.%03d\"%c", duration / 1000, duration % 1000,
					  delimiter);
		strcat(buffer2, temp);
	} else {
		sprintf(temp, "\"       \"%c", delimiter);
		strcat(buffer2, temp);
	}
	prep_stim_fill(buffer2, sequence, delimiter);
	buffer2[128] = 0x0D;
	buffer2[129] = 0x0A;
	buffer2[130] = 0;
}

/**********************************************************************
 *	
 *	
 *	
 **********************************************************************/
void prep_lp_sound(char *buffer,
						 char *buffer2,
						 struct ARCHIVE_TIME_SERIES *archive,
						 struct RPWS_buffer *rpws,
						 struct LP_STATUS *lp_status_packet,
						 char *flag, char delimiter)
{
	static char temp[32];
	int duration;
	char *sequence;

	*buffer = 0;
	*buffer2 = 0;

	rpws_ieb_ver(archive, &sequence);
	prep_stim_time(buffer, buffer2, archive, delimiter);

	strcat(buffer, flag);
	strcat(buffer, " ");

	sprintf(temp, "\"%s\"%c", flag, delimiter);
	strcat(buffer2, temp);

	duration = lp_status_packet->lp_event_duration;
	if(duration > 120000)
		sprintf(temp, "Dur=unknown ");
	else
		sprintf(temp, "Dur=%d.%03d ", duration / 1000, duration % 1000);
	strcat(buffer, temp);
	strcat(buffer, "\r\n");

	if(duration > 120000)
		sprintf(temp, "\"UNKNOWN\"%c", delimiter);
	else
		sprintf(temp, "\"%3d.%03d\"%c", duration / 1000, duration % 1000,
				  delimiter);
	strcat(buffer2, temp);
	prep_stim_fill(buffer2, sequence, delimiter);
	buffer2[128] = 0x0D;
	buffer2[129] = 0x0A;
	buffer2[130] = 0;

}

/**********************************************************************
 * Keep track of overall start/sopt time                              *
 **********************************************************************/
int update_time_wbr_wfr(struct RPWS_LABEL *rpws_working_label, int flag)
{

	if(flag == ARCH_VALIDITY_FLAG_WBR) {

		if(rpws_wbr_time->duration < rpws_working_label->duration)
			rpws_wbr_time->duration = rpws_working_label->duration;

		/***********************************************************/
		/* if the new record is less than (before) save start time */
		/***********************************************************/
		if(strcmp(rpws_working_label->sclk_start, rpws_wbr_time->sclk_start)
			< 0) {
			strcpy(rpws_wbr_time->sclk_start, rpws_working_label->sclk_start);
			strcpy(rpws_wbr_time->scet_start, rpws_working_label->scet_start);
			strcpy(rpws_wbr_time->plot_start, rpws_working_label->scet_start);
			if(rpws_working_label->utc_date)
				if(rpws_working_label->utc_date[0]) {
					strcpy(rpws_wbr_time->utc_date,
							 rpws_working_label->utc_date);
					strcat(rpws_wbr_time->utc_date, " ");
					strcat(rpws_wbr_time->utc_date,
							 rpws_working_label->utc_time);
				}
			return ARCH_VALIDITY_FLAG_WBR;
		}

		/***********************************************************/
		/* if the new record is greater than (after) save stop time */
		/***********************************************************/
		if(strcmp(rpws_working_label->sclk_stop, rpws_wbr_time->sclk_stop) > 0) {
			strcpy(rpws_wbr_time->sclk_stop, rpws_working_label->sclk_stop);
			strcpy(rpws_wbr_time->scet_stop, rpws_working_label->scet_stop);
			strcpy(rpws_wbr_time->plot_stop, rpws_working_label->scet_stop);
			if(rpws_working_label->utc_date)
				if(rpws_working_label->utc_date[0]) {
					strcpy(rpws_wbr_time->utc_time,
							 rpws_working_label->utc_date);
					strcat(rpws_wbr_time->utc_time, " ");
					strcat(rpws_wbr_time->utc_time,
							 rpws_working_label->utc_time);
				}
			return ARCH_VALIDITY_FLAG_WBR;
		}
	}
	if(flag == ARCH_VALIDITY_FLAG_WFR) {
		if(rpws_wfr_time->duration < rpws_working_label->duration)
			rpws_wfr_time->duration = rpws_working_label->duration;
		if(strcmp(rpws_working_label->sclk_start, rpws_wfr_time->sclk_start)
			< 0) {
			strcpy(rpws_wfr_time->sclk_start, rpws_working_label->sclk_start);
			strcpy(rpws_wfr_time->scet_start, rpws_working_label->scet_start);
			strcpy(rpws_wfr_time->plot_start, rpws_working_label->scet_start);
			if(rpws_working_label->utc_date)
				if(rpws_working_label->utc_date[0]) {
					strcpy(rpws_wfr_time->utc_date,
							 rpws_working_label->utc_date);
					strcat(rpws_wfr_time->utc_date, " ");
					strcat(rpws_wfr_time->utc_date,
							 rpws_working_label->utc_time);
				}
			return ARCH_VALIDITY_FLAG_WFR;
		}
		if(strcmp(rpws_working_label->sclk_stop, rpws_wfr_time->sclk_stop) >
			0) {
			strcpy(rpws_wfr_time->sclk_stop, rpws_working_label->sclk_stop);
			strcpy(rpws_wfr_time->scet_stop, rpws_working_label->scet_stop);
			strcpy(rpws_wfr_time->plot_stop, rpws_working_label->scet_stop);
			if(rpws_working_label->utc_date)
				if(rpws_working_label->utc_date[0]) {
					strcpy(rpws_wfr_time->utc_time,
							 rpws_working_label->utc_date);
					strcat(rpws_wfr_time->utc_time, " ");
					strcat(rpws_wfr_time->utc_time,
							 rpws_working_label->utc_time);
				}
			return ARCH_VALIDITY_FLAG_WFR;
		}
	}
	return 1;
}

/**********************************************************************
 *	
 *	
 *	
 **********************************************************************/
int update_time_stim(struct RPWS_LABEL *rpws_working_label)
{
	if(1) {
		if(strcmp
			(rpws_working_label->sclk_start,
			 rpws_label_stim->sclk_start) < 0) {
			strcpy(rpws_label_stim->sclk_start,
					 rpws_working_label->sclk_start);
			if(rpws_working_label->utc_date)
				if(rpws_working_label->utc_date[0]) {
					strcpy(rpws_label_stim->utc_date,
							 rpws_working_label->utc_date);
					strcat(rpws_label_stim->utc_date, " ");
					strcat(rpws_label_stim->utc_date,
							 rpws_working_label->utc_time);
				}
			return ARCH_VALIDITY_FLAG_STIM;
		}
		if(strcmp(rpws_working_label->sclk_stop, rpws_label_stim->sclk_stop)
			> 0) {
			strcpy(rpws_label_stim->sclk_stop, rpws_working_label->sclk_stop);
			if(rpws_working_label->utc_date)
				if(rpws_working_label->utc_date[0]) {
					strcpy(rpws_label_stim->utc_time,
							 rpws_working_label->utc_date);
					strcat(rpws_label_stim->utc_time, " ");
					strcat(rpws_label_stim->utc_time,
							 rpws_working_label->utc_time);
				}
			return ARCH_VALIDITY_FLAG_STIM;
		}
	}
	return 1;
}

/**********************************************************************
 *	Build label information						*
 **********************************************************************/
void write_label(struct RPWS_LABEL *label_head)
{
	struct RPWS_LABEL *label_temp;
	label_temp = label_head;
	while(label_temp) {
		if(debug_flag & 0x0400)
			fprintf(debug_file, "write_label %s\n", label_temp->filename);

		if(!label_temp->Mission_Phase[0])
			label_temp->Mission_Phase = rpws_mpn_ver(label_temp);

		if(!label_temp->Target[0])
			label_temp->Target = rpws_target_ver(label_temp);

		if(!label_temp->Coordinate[0])
			label_temp->Coordinate = rpws_coordinate_ver(label_temp);

		if(!label_temp->Orbit[0])
			label_temp->Orbit = rpws_orbit_ver(label_temp);

		format_scet(label_temp, 1);
#ifdef _New_Name_Handling_
		label_temp->Mission_Phase =
			 rpws_mission_phase_name(label_temp->sclk_start,
											 label_temp->sclk_stop, 0,
											 label_temp->instrument);
		label_temp->Target =
			 rpws_target_name(label_temp->sclk_start, label_temp->sclk_stop,
									0, label_temp->instrument);
		label_temp->Coordinate =
			 rpws_coordinate_name(label_temp->sclk_start,
										 label_temp->sclk_stop, 0,
										 label_temp->instrument);
		label_temp->Orbit =
			 rpws_orbit_number(label_temp->sclk_start, label_temp->sclk_stop,
									 0);
#endif
		if(Zero_Flag)
			rpws_time_patch(label_temp);
		rpws_label_write(label_temp, w_directory);

		label_temp = label_temp->link;

	}
}

/**********************************************************************
 *            Initialize label data structure:	                *
 *									*
 **********************************************************************/
int initialize_label(struct RPWS_LABEL *rpws_label, int nProdVerId)
{
	int i;
	static char temp[128];
	if(!rpws_label)
		return 0;
	rpws_label->link = NULL;
	rpws_label->instrument = -1;
	rpws_label->fband = -1;
	rpws_label->mode_mask = 0;
#ifdef HFR_XLATE
	for(i = 0; i < HFR_XLATE; i++)
		rpws_label->hfr_xlate[i] = 0;
#else
	rpws_label->hfr_xlate = 0;
#endif
	rpws_label->span = -1.0;
	rpws_label->duration = 0.0;
	rpws_label->dataset_size[0] = 0;
	rpws_label->dataset_size[1] = 0;
	rpws_label->sample_count = -1;
	rpws_label->pad_class = -1;
	rpws_label->record_size = 0;
	rpws_label->record_count[0] = -1;
	rpws_label->record_count[1] = -1;
	rpws_label->Label_Line_Pad = Line_Pad;
	rpws_label->filehandle = NULL;
	rpws_label->filename = rpws_label->text_area[0];
	rpws_label->filepath1 = rpws_label->text_area[1];
	rpws_label->filepath2 = rpws_label->text_area[2];
	rpws_label->filepath3 = rpws_label->text_area[3];
	rpws_label->thumbname = rpws_label->text_area[4];
	rpws_label->utc_date = rpws_label->text_area[5];
	rpws_label->utc_time = rpws_label->text_area[6];
	rpws_label->sclk_start = rpws_label->text_area[7];
	rpws_label->sclk_stop = rpws_label->text_area[8];
	rpws_label->scet_start = rpws_label->text_area[9];
	rpws_label->scet_start_2 = rpws_label->text_area[10];
	rpws_label->scet_stop = rpws_label->text_area[11];
	rpws_label->ephem_start = rpws_label->text_area[12];
	rpws_label->ephem_stop = rpws_label->text_area[13];
	rpws_label->plot_start = rpws_label->text_area[14];
	rpws_label->plot_stop = rpws_label->text_area[15];
	rpws_label->scet_start_3 = rpws_label->text_area[16];

	rpws_label->Label_Version = Label_Ver;

	rpws_label->Mission_Phase = Mission_Phase;
	rpws_label->Target = Target;
	rpws_label->Coordinate = Coordinate;
	rpws_label->Orbit = Orbit;

	rpws_label->Product = Product[0];
	rpws_label->ProdVerId = nProdVerId;

	memset(rpws_label->text_area, 0, sizeof(rpws_label->text_area));

	return 1;
}

/***********************************************************************
 *			Load label fields (new label)			*
 ***********************************************************************/
int load_label(struct RPWS_LABEL *label_temp, struct RPWS_LABEL *label_)
{

	int i;
	label_temp->instrument = label_->instrument;
	label_temp->fband = label_->fband;
	label_temp->mode_mask = label_->mode_mask;
#ifdef HFR_XLATE
	for(i = 0; i < HFR_XLATE; i++)
		label_temp->hfr_xlate[i] = label_->hfr_xlate[i];
#else
	label_temp->hfr_xlate = label_->hfr_xlate;
#endif
	label_temp->span = label_->span;
	label_temp->duration = label_->duration;
	label_temp->dataset_size[0] = label_->dataset_size[0];
	label_temp->dataset_size[1] = label_->dataset_size[1];
	label_temp->sample_count = label_->sample_count;
	label_temp->pad_class = label_->pad_class;
	label_temp->record_size = label_->record_size;
	label_temp->record_count[0] = label_->record_count[0];
	label_temp->record_count[1] = label_->record_count[1];
	label_temp->Label_Line_Pad = label_->Label_Line_Pad;
	label_temp->filehandle = label_->filehandle;
	strcpy(label_temp->filename, label_->filename);
	strcpy(label_temp->filepath1, label_->filepath1);
	strcpy(label_temp->filepath2, label_->filepath2);
	strcpy(label_temp->filepath3, label_->filepath3);
	strcpy(label_temp->thumbname, label_->thumbname);
	strcpy(label_temp->sclk_start, label_->sclk_start);
	offset_sclk_stop(label_temp);
	label_temp->Label_Version = label_->Label_Version;

	label_->Mission_Phase = Mission_Phase;
	label_temp->Target = label_->Target;
	label_temp->Coordinate = label_->Coordinate;
	label_temp->Orbit = label_->Orbit;

	label_temp->Product = label_->Product;
	return 1;
}

/***********************************************************************
 *			Insert a new label, if needed			*
 ***********************************************************************/
int sort_label_sclk(struct RPWS_LABEL *label_,
						  struct RPWS_LABEL **sort_label_head)
{
	struct RPWS_LABEL *label_temp = sort_label_head[0];

	if(!sort_label_head[0]) {
		*sort_label_head = label_;
		label_->link = NULL;
		return 1;
	}
	if(!sort_label_head[0]->sclk_start) {
		fprintf(stdout, "%s %d fault\n", __FILE__, __LINE__);
		dump_master_label(sort_label_head[0], 0, __LINE__);
		exit(1);
	}

	if(!label_) {
		fprintf(stdout, "%s %d fault\n", __FILE__, __LINE__);
		exit(1);
	}
	if(!label_->sclk_start) {
		dump_master_label(label_, 0, __LINE__);
		fprintf(stdout, "%s %d fault\n", __FILE__, __LINE__);
		exit(1);
	}

	if(strcmp(label_->sclk_start, sort_label_head[0]->sclk_start) < 0) {
		label_->link = sort_label_head[0]->link;
		sort_label_head[0]->link = label_;
		return 1;
	}
	label_temp = sort_label_head[0]->link;
	while(label_temp) {
		if(!label_temp->link) {
			label_temp->link = label_;
			return 2;
		}
		if(strcmp(label_->sclk_start, label_temp->link->sclk_start) < 0) {
			label_->link = label_temp->link;
			label_temp->link = label_;
			return 3;
		}
		label_temp = label_temp->link;
	}

	label_->link = sort_label_head[0]->link;
	sort_label_head[0]->link = label_;
	return 0;
}

/*****************************************************************************/
int sort_label(struct RPWS_LABEL *label_,
					struct RPWS_LABEL **sort_label_head, int flag)
{
	struct RPWS_LABEL *label_temp = sort_label_head[0];
	char left_label[256];
	char right_label[256];

	if(!sort_label_head[0]) {
		*sort_label_head = label_;
		label_->link = NULL;
		return 1;
	}
	if(!sort_label_head[0]->sclk_start) {
		fprintf(stdout, "%s %d fault\n", __FILE__, __LINE__);
		dump_master_label(sort_label_head[0], 0, __LINE__);
		exit(1);
	}

	if(!label_) {
		fprintf(stdout, "%s %d fault\n", __FILE__, __LINE__);
		exit(1);
	}
	if(!label_->sclk_start) {
		dump_master_label(label_, 0, __LINE__);
		fprintf(stdout, "%s %d fault\n", __FILE__, __LINE__);
		exit(1);
	}

	if(flag) {
		sprintf(left_label, "%d_%s", label_->fband, label_->sclk_start);
		sprintf(right_label, "%d_%s", sort_label_head[0]->fband,
				  sort_label_head[0]->sclk_start);
	} else {
		sprintf(left_label, "%s_%d", label_->sclk_start, label_->fband);
		sprintf(right_label, "%s_%d", sort_label_head[0]->sclk_start,
				  sort_label_head[0]->fband);
	}
	/* fprintf(debug_file, "SORT_LABEL (%d)%s (%d)%s\n",
	   label_->instrument, left_label, sort_label_head[0]->instrument,
	   right_label); */
	/* fflush(debug_file); */
	if(strcmp(left_label, right_label) < 0) {
		label_->link = sort_label_head[0]->link;
		sort_label_head[0]->link = label_;
		return 1;
	}
	label_temp = sort_label_head[0]->link;
	while(label_temp) {
		if(!label_temp->link) {
			label_temp->link = label_;
			return 2;
		}
		if(flag) {
			sprintf(left_label, "%d_%s", label_->fband, label_->sclk_start);
			sprintf(right_label, "%d_%s", label_temp->link->fband,
					  label_temp->link->sclk_start);
		} else {
			sprintf(left_label, "%s_%d", label_->sclk_start, label_->fband);
			sprintf(right_label, "%s_%d", label_temp->link->sclk_start,
					  label_temp->link->fband);
		}
		/* fprintf(debug_file, "SORT_LABEL (%d)%s (%d)%s\n",
		   label_->instrument, left_label, sort_label_head[0]->instrument,
		   right_label); */
		/* fflush(debug_file); */
		if(strcmp(left_label, right_label) < 0) {
			label_->link = label_temp->link;
			label_temp->link = label_;
			return 3;
		}
		label_temp = label_temp->link;
	}

	label_->link = sort_label_head[0]->link;
	sort_label_head[0]->link = label_;
	return 0;
}

/***********************************************************************
 *	Check to see if a new label is required, as a side-effect	*
 *	update the start/stop times					*
 ***********************************************************************/
struct RPWS_LABEL *update_label_scan(struct RPWS_LABEL *label_in,
												 int dataset_size,
												 struct RPWS_LABEL *label_head)
{
	int temp;
	char *filename;
	struct RPWS_LABEL *label_temp;
	label_temp = label_head;
	while(label_temp) {
		if(!strcmp(label_temp->filename, label_in->filename)) {
			/* if(!label_temp->filehandle) { filename =
			   rpws_file_reopen(label_temp, __LINE__); } */
			label_temp->record_count[1]++;
			label_temp->dataset_size[1] += dataset_size;
			if(label_temp->duration < label_in->duration)
				label_temp->duration = label_in->duration;
			if(strcmp(label_in->sclk_start, label_temp->sclk_start) < 0) {
				strcpy(label_temp->sclk_start, label_in->sclk_start);
			}
			if(strcmp(label_in->sclk_stop, label_temp->sclk_stop) > 0) {
				strcpy(label_temp->sclk_stop, label_in->sclk_stop);
			}
			return label_temp;
		}
		label_temp = label_temp->link;
	}
	return NULL;
}

/**********************************************************************
 *	
 **********************************************************************/
char *update_label(struct RPWS_LABEL *label_, int dataset_size,
						 struct RPWS_LABEL **label_head, int nProdVerId)
{
	static struct RPWS_LABEL *label_temp;
	int i;
	if(!label_head[0]) {
		*label_head = malloc(sizeof(struct RPWS_LABEL));
		initialize_label(label_head[0], nProdVerId);
		load_label(label_head[0], label_);
		if(debug_flag & 0x0400) {
			fprintf(debug_file, "update_label MALLOC 1 %s\n",
					  label_head[0]->filename);
		}
		return label_head[0]->filename;
	} else {
		if(!
			(label_temp =
			 update_label_scan(label_, dataset_size, label_head[0]))) {
			label_temp = malloc(sizeof(struct RPWS_LABEL));
			initialize_label(label_temp, nProdVerId);
			load_label(label_temp, label_);
/*             fprintf(stdout,"%5d SORT %p\n", __LINE__, label_head[0]); */
			sort_label(label_temp, label_head, 0);

			/* label_temp = label_head[0]; */
			/* *label_head = malloc(sizeof(struct RPWS_LABEL)); */
			/* initialize_label(label_head[0]); */
			/* load_label(label_head[0], label_); */
			/* label_head[0]->link = label_temp; */
			if(debug_flag & 0x0400) {
				fprintf(debug_file, "update_label MALLOC 2 %s\n",
						  label_temp->filename);
			}
			return label_->filename;
		}
	}
	return label_->filename;
}

/**********************************************************************
 *									*
 **********************************************************************/
static struct event_time *insert_event_scet(int days, int milli)
{
	static struct event_time event_scet;
	event_scet.days = days;
	event_scet.milliseconds = milli;
	return &event_scet;
}

/**********************************************************************
 *		look at the WBR packet and see if it's dust		*
 *		IF SO. change the name a little				*
 **********************************************************************/
char test_for_dust(struct RPWS_LABEL *rpws_working_label,
						 struct RPWS_buffer *buffer)
{
	int clue = 0;
	int clue_max = 0;
	int i;
	int msf;
	int x;
	char pad_class = '_';
	pad_class = (rpws_working_label->pad_class & 0x0F) + 0x30;

		/**********************************
       *	Tally up the clues	*
       **********************************/
	clue_max++;						  /* 1K data set */
	if(rpws_working_label->pad_class == 1)
		clue++;
	clue_max++;						  /* really ??? */
	if(rpws_working_label->sample_count == 1024)
		clue++;

	msf = get_status(buffer->packet.mpp.mini_packet, WBR_MSF, 0) & 0x01;
	msf *= 2;
	for(i = 0; i < 1024; i++) {
		clue_max++;					  /* data look good ? */
		if((buffer->packet.mpp.mini_packet[i + 8 + msf] & 0x0F) == 0)
			clue++;
	}

	if(clue == clue_max)
		pad_class = 'D';
	return pad_class;
}

/**********************************************************************
 *	Figure out an appropriate frequency string for the WBR filename	*
 *									*
 **********************************************************************/
static char *Universal_Frequency_Band(struct RPWS_LABEL
												  *rpws_working_label)
{
	char *temp;
#ifdef HFR_XLATE
	int i = 0;
	while(rpws_working_label->hfr_xlate[i]) {
		if(i >= HFR_XLATE)
			break;
		i++;
	}
	if(rpws_working_label->hfr_xlate[0])
#endif
	{
#ifdef HFR_XLATE
		temp = rpws_hfr_xlate(rpws_working_label->hfr_xlate[i]);
#else
		temp = rpws_hfr_xlate(rpws_working_label->hfr_xlate);
#endif
		if(temp)
			return temp;
	}
	return Frequency_Band[rpws_working_label->fband];
}

/*****************************************************************************/
/* Calculate the name of the file the current data record should reside in   */
/*****************************************************************************/
static char *new_file_name(
	struct RPWS_buffer *buffer, struct event_clock *event_sclk, 
	struct event_time *event_scet[], int record_length, char *file_path1,
  char *file_path2, int nProdVerId
){
	static char *str;
	static char str0[128];
	static char filename[17][256];
	static int filename_index = 0x0F;
	int i;
	int dataset_size = 0;
	int hour_of_day;
	int minute_of_day;
	int second_of_day;
	unsigned long epoch;
	time_t pkt_etime, pkt_epoc;
	struct tm tm_temp;
	struct tm *pkt_ev = &tm_temp;
	double et;
	double dsec;
	char *sformat[] = { "D", "J" };
	int prec = 3;
	int days, milli;
	char yet_another_direcory_path[64];

	switch (buffer->record_type & 0xFF) {

	default:
		return NULL;
	case PACKET_TYPE_wbr:
		break;
	case PACKET_TYPE_wfr:
		break;
	case PACKET_TYPE_stim:
		break;
	case PACKET_TYPE_hfr:
		break;
	case PACKET_TYPE_lp:
		break;
#ifdef ALL
	case PACKET_TYPE_mro:
		break;
	case PACKET_TYPE_mfr:
		break;
	case PACKET_TYPE_lfdr:
		break;
#endif
	}


	epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
		 (buffer->packet.cds_tag.epoch[1] << 16) |
		 (buffer->packet.cds_tag.epoch[2] << 8) |
		 (buffer->packet.cds_tag.epoch[3] << 0);
	pkt_etime = UTIL_event_time((struct MP_buffer *) buffer, 0);
	pkt_epoc = pkt_etime + epoch;

	event_sclk->seconds = pkt_etime;
	event_sclk->fine =
		 (UTIL_extract_MP_RTI((struct MP_buffer *) buffer) << 5) & 0x00E0;

	if(compare_time(event_sclk)) {
		return NULL;
	}

	if(spice_flag) {
		sprintf(str0, "%d/%d:%03d", Partition, event_sclk->seconds,
				  event_sclk->fine);
		scs2e_c(SpaceCraft_ID, str0, &et);
		et2utc_c(et, sformat[0], prec, 32, str0);

		pkt_ev->tm_year = strtol(str0, &str, 10);
		pkt_ev->tm_mday = strtol(str + 1, &str, 10);
		pkt_ev->tm_yday = 0;		  /* ignored */
		pkt_ev->tm_mon = 1;
		pkt_ev->tm_hour = strtol(str + 3, &str, 10);
		pkt_ev->tm_min = strtol(str + 1, &str, 10);
		dsec = strtod(str + 1, &str);

		tnorm(&pkt_ev->tm_year, &pkt_ev->tm_mon, &pkt_ev->tm_mday,
				&pkt_ev->tm_yday, &pkt_ev->tm_hour, &pkt_ev->tm_min, &dsec);

		pkt_ev->tm_year -= 1900;  /* back to tm structure */
		pkt_ev->tm_yday -= 1;	  /* back to tm structure */
		pkt_ev->tm_mon -= 1;		  /* back to tm structure */
		pkt_ev->tm_sec = dsec;	  /* back to tm structure */

		milli = pkt_ev->tm_hour * 3600 * 1000;
		milli += pkt_ev->tm_min * 60 * 1000;
		milli += dsec * 1000.0;

		et2utc_c(et, sformat[1], prec, 32, str0);
		days = strtol(&str0[3], NULL, 10) - A_EPOCH;
		event_scet[0] = insert_event_scet(days, milli);
		/* fprintf(stdout, "%5d %d.%d\n", __LINE__, event_scet[0]->days,
		   event_scet[0]->milliseconds); */
	} else {
		event_scet[0] =
			 UTIL_event_scet((struct MP_buffer *) buffer, *event_sclk);
		pkt_ev = UTIL_event_scet_tm(*event_scet[0], 0);
	}

	if(debug_flag & 0x4000) {
		sprintf(str0,
				  " SC Event  %08X.%01X %02X%02X %d (%4d-%03dT%2.2d:%2.2d:%2.2d.%3.3d) new",
				  event_sclk->seconds, event_sclk->fine >> 5,
				  buffer->packet.mpp.mini_packet[3],
				  buffer->packet.mpp.mini_packet[2],
				  (buffer->packet.cds_tag.begin[4] & 0x1E) >> 1,
				  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1, pkt_ev->tm_hour,
				  pkt_ev->tm_min, pkt_ev->tm_sec,
				  event_scet[0]->milliseconds % 1000);

		fprintf(debug_file, "%s (%d)\n", str0, __LINE__);
	}

	second_of_day =
		 pkt_ev->tm_hour * 3600 + pkt_ev->tm_min * 60 + pkt_ev->tm_sec;

	switch (buffer->record_type & 0xFF) {
	case PACKET_TYPE_wbr:
		minute_of_day = second_of_day / 60;
		minute_of_day = minute_of_day / KWBR60;
		minute_of_day = minute_of_day * KWBR60;
		hour_of_day = minute_of_day / 60;
		break;

	case PACKET_TYPE_wfr:
		minute_of_day = second_of_day / 60;
		minute_of_day = minute_of_day / KWFR60;
		minute_of_day = minute_of_day * KWFR60;
		hour_of_day = minute_of_day / 60;
		break;

	case PACKET_TYPE_stim:
	case PACKET_TYPE_hfr:
	case PACKET_TYPE_lp:
#ifdef ALL
	case PACKET_TYPE_mro:
	case PACKET_TYPE_mfr:
	case PACKET_TYPE_lfdr:
#endif
		minute_of_day = second_of_day / 60;
		minute_of_day = minute_of_day / KANCIL;
		minute_of_day = minute_of_day * KANCIL;
		hour_of_day = minute_of_day / 60;
		break;
	}

	initialize_label(rpws_working_label, nProdVerId);

	switch (buffer->record_type & 0xFF) {

	/******************************/
	/* File names for WBR Packets */
	/******************************/
	case PACKET_TYPE_wbr:
		rpws_working_label->instrument = RPWS_LABEL_WBR;
		rpws_working_label->span = (KWBR60 + 1.0) / 60.0;
		rpws_working_label->duration = sample_wbr_wfr(buffer);
		rpws_working_label->duration += sample_wbr_sub(buffer);
		rpws_working_label->fband =
			 get_status(buffer->packet.mpp.mini_packet, WBR_frequency_band,
							0) & 0x01;

		rpws_working_label->fband =
			 WBR_Frequency_Band[rpws_working_label->fband];

		rpws_working_label->mode_mask |= MODE_MASK_WBR;
		switch (rpws_working_label->fband) {
		case ARCH_BAND_10KHZ:
			rpws_working_label->mode_mask |= MODE_MASK_WBR_LO;
			break;
		case ARCH_BAND_75KHZ:
			rpws_working_label->mode_mask |= MODE_MASK_WBR_HI;
			break;
		case ARCH_BAND_25HZ:
		case ARCH_BAND_2500HZ:
			break;
		}

		/** HFR translation mode affects the filename */
		if(ARCH_ANTENNA_HF ==
			WBR_Antenna[get_status
							(buffer->packet.mpp.mini_packet, WBR_antenna,
							 0) & 0x07]) {
			int hfr_xlate;
			int i = 0;
			if(get_status(buffer->packet.mpp.mini_packet, WBR_MSF, 0)) {
				hfr_xlate =
					 get_status(buffer->packet.mpp.mini_packet,
									WBR_HFR_translate, 0);
#ifdef HFR_XLATE
				for(i = 0; i < HFR_XLATE; i++) {
					if(rpws_working_label->hfr_xlate[i] == hfr_xlate)
						break;

					if(!rpws_working_label->hfr_xlate[i])
						rpws_working_label->hfr_xlate[i] = hfr_xlate;
				}
#else
				rpws_working_label->hfr_xlate = hfr_xlate;
#endif
				rpws_working_label->mode_mask |= MODE_MASK_HFR;
			}
		}

		if(rpws_working_label->sample_count < record_length) {
#ifdef DEBUG2050
			if(record_length & 0x0F)
				fprintf(debug_file,
						  "%s %d CHANGE record_length from %d to %d\n", __FILE__,
						  __LINE__, rpws_working_label->sample_count,
						  record_length);
#endif
			rpws_working_label->sample_count = record_length;
		}
		rpws_working_label->pad_class =   file_class(record_length, ARCH_VALIDITY_FLAG_INST_ID_WBR);
		rpws_working_label->record_size = pad_class[rpws_working_label->pad_class] + 32;
		rpws_working_label->record_count[0] = 0;
		rpws_working_label->record_count[1] = 1;
		rpws_working_label->dataset_size[0] = 0;
		rpws_working_label->dataset_size[1] = prep_size(buffer, 0, __LINE__);
		rpws_working_label->Product = Product[1];
		dataset_size = prep_size(buffer, 0, __LINE__);

		sprintf(rpws_working_label->filename, "T%04d%03d%_%02d_%s%c_%s", 
				  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1, hour_of_day, 
				  Universal_Frequency_Band(rpws_working_label),	/* Holy Shit Batman */
				  test_for_dust(rpws_working_label, buffer), "WBRFR");

		sprintf(rpws_working_label->filepath1, "T%04d%01dXX",
				  pkt_ev->tm_year + 1900, (pkt_ev->tm_yday + 1) / 100);

		sprintf(rpws_working_label->filepath2, "T%04d%03d",
				  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1);

		sprintf(rpws_working_label->thumbname, "T%04d%03d%_%02d_%s_%s",
				  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1, hour_of_day,
				  Universal_Frequency_Band(rpws_working_label), "WBB");

		sprintf(rpws_working_label->sclk_start, "%d/%d:%03d", Partition,
				  event_sclk->seconds, event_sclk->fine);

		offset_sclk_stop(rpws_working_label);

		sprintf(rpws_working_label->scet_start,
				  "%4d-%03dT%2.2d:%2.2d:%2.2d.%3.3d", pkt_ev->tm_year + 1900,
				  pkt_ev->tm_yday + 1, pkt_ev->tm_hour, pkt_ev->tm_min,
				  pkt_ev->tm_sec, event_scet[0]->milliseconds % 1000);

		strcpy(rpws_working_label->plot_start,
				 rpws_working_label->scet_start);

		sprintf(rpws_working_label->scet_start_2,
				  "%4d-%02d-%02dT%2.2d:%2.2d:%2.2d", pkt_ev->tm_year + 1900,
				  pkt_ev->tm_mon, pkt_ev->tm_mday, pkt_ev->tm_hour,
				  pkt_ev->tm_min, pkt_ev->tm_sec);

		sprintf(rpws_working_label->scet_start_3,
				  "%04d-%02d-%02d(%03d)T%02d", pkt_ev->tm_year + 1900,
				  pkt_ev->tm_mon, pkt_ev->tm_mday, pkt_ev->tm_yday + 1,
				  pkt_ev->tm_hour);

		update_time_wbr_wfr(rpws_working_label, ARCH_VALIDITY_FLAG_WBR);
		break;


	/******************************/
	/* File names for WFR Packets */
	/******************************/
	case PACKET_TYPE_wfr:
		rpws_working_label->instrument = RPWS_LABEL_WFR;
		rpws_working_label->span = (KWFR60 + 1.0) / 60.0;
		rpws_working_label->duration = sample_wbr_wfr(buffer);
		rpws_working_label->fband =
			 get_status(buffer->packet.mpp.mini_packet, WFR_frequency_band,
							0) & 0x01;
		rpws_working_label->fband =
			 WFR_Frequency_Band[rpws_working_label->fband];
		rpws_working_label->mode_mask |= MODE_MASK_WFR;
		switch (rpws_working_label->fband) {
		case ARCH_BAND_10KHZ:
		case ARCH_BAND_75KHZ:
			break;
		case ARCH_BAND_25HZ:
			rpws_working_label->mode_mask |= MODE_MASK_WFR_LO;
			break;
		case ARCH_BAND_2500HZ:
			rpws_working_label->mode_mask |= MODE_MASK_WFR_HI;
			break;
		}
		if(rpws_working_label->sample_count < record_length / 2)
			rpws_working_label->sample_count = record_length / 2;
		rpws_working_label->pad_class =
			 file_class(record_length, ARCH_VALIDITY_FLAG_INST_ID_WFR);
		rpws_working_label->record_size =
			 pad_class[rpws_working_label->pad_class] + 32;
		rpws_working_label->record_count[0] = 0;
		rpws_working_label->record_count[1] = 1;
		rpws_working_label->dataset_size[0] = 0;
		rpws_working_label->dataset_size[1] = prep_size(buffer, 0, __LINE__);
		rpws_working_label->Product = Product[2];
		
		dataset_size = prep_size(buffer, 0, __LINE__);

		sprintf(rpws_working_label->filename, "T%04d%03d%_%s%d_%s",
				  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1,
				  Frequency_Band[rpws_working_label->fband],
				  rpws_working_label->pad_class, "WFRFR");
		
		sprintf(rpws_working_label->filepath1, "T%04d%01dXX",
				  pkt_ev->tm_year + 1900, (pkt_ev->tm_yday + 1) / 100);
		
		sprintf(rpws_working_label->filepath2, "T%04d%03d",
				  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1);
		
		sprintf(rpws_working_label->thumbname, "T%04d%03d%_%s_%s",
				  pkt_ev->tm_year + 1900,
				  pkt_ev->tm_yday + 1,
				  Frequency_Band[rpws_working_label->fband], "WFB");
		
		sprintf(rpws_working_label->sclk_start, "%d/%d:%03d",
				  Partition, event_sclk->seconds, event_sclk->fine);
		
		offset_sclk_stop(rpws_working_label);
		
		sprintf(rpws_working_label->scet_start, "%4d-%03dT"
				  "%2.2d:%2.2d:%2.2d."
				  "%3.3d",
				  pkt_ev->tm_year + 1900,
				  pkt_ev->tm_yday + 1,
				  pkt_ev->tm_hour,
				  pkt_ev->tm_min,
				  pkt_ev->tm_sec, event_scet[0]->milliseconds % 1000);
		
		strcpy(rpws_working_label->plot_start,
				 rpws_working_label->scet_start);
		
		sprintf(rpws_working_label->scet_start_2,
				  "%4d-%02d-%02dT" "%2.2d:%2.2d:%2.2d", pkt_ev->tm_year + 1900,
				  pkt_ev->tm_mon, pkt_ev->tm_mday, pkt_ev->tm_hour,
				  pkt_ev->tm_min, pkt_ev->tm_sec);
		
		sprintf(rpws_working_label->scet_start_3,
				  "%04d-%02d-%02d(%03d)T%02d", pkt_ev->tm_year + 1900,
				  pkt_ev->tm_mon, pkt_ev->tm_mday, pkt_ev->tm_yday + 1,
				  pkt_ev->tm_hour);
		
		update_time_wbr_wfr(rpws_working_label, ARCH_VALIDITY_FLAG_WFR);
		break;


	case PACKET_TYPE_stim:
		dataset_size = 0;
		switch (split_stim_flag) {
		case 1:
			sprintf(rpws_label_stim->filename, "T%04d%03d_STIM",
					  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1);

			break;
		case 100:
			sprintf(rpws_label_stim->filename, "T%04d%01dXX_STIM",
					  pkt_ev->tm_year + 1900, (pkt_ev->tm_yday + 1) / 100);
			break;
		case 365:
			sprintf(rpws_label_stim->filename, "T%04d_STIM",
					  pkt_ev->tm_year + 1900);
			break;
			/* add 1000 to "split_stim_flag" to put stuff in stim directory
			   down 1 directoy level */
		case 1100:
			sprintf(rpws_label_stim->filename, "T%04d%01dXX/T%04d%01dXX_STIM",
					  pkt_ev->tm_year + 1900,
					  (pkt_ev->tm_yday + 1) / 100,
					  pkt_ev->tm_year + 1900, (pkt_ev->tm_yday + 1) / 100);
			sprintf(yet_another_direcory_path, "%s/T%04d%01dXX",
					  w_directory[RPWS_ARCHIVE_DATA_ANCIL],
					  pkt_ev->tm_year + 1900, (pkt_ev->tm_yday + 1) / 100);
			new_dir_mkdir(yet_another_direcory_path,
							  S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
			break;
		case 1365:
			sprintf(rpws_label_stim->filename, "T%04d/T%04d_STIM",
					  pkt_ev->tm_year + 1900, pkt_ev->tm_year + 1900);
			sprintf(yet_another_direcory_path, "%s/T%04d",
					  w_directory[RPWS_ARCHIVE_DATA_ANCIL],
					  pkt_ev->tm_year + 1900);
			new_dir_mkdir(yet_another_direcory_path,
							  S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
			break;

		case 0:
			sprintf(rpws_label_stim->filename, "STIM");
			break;
		}
		sprintf(new_stim_filename_1, "%s/%s.TXT",
				  w_directory[RPWS_ARCHIVE_EXTRAS], rpws_label_stim->filename);
		sprintf(new_stim_filename_2, "%s/%s.TAB",
				  w_directory[RPWS_ARCHIVE_DATA_ANCIL],
				  rpws_label_stim->filename);

		rpws_working_label->instrument = RPWS_LABEL_STIM;
		rpws_working_label->span = 0.0;
		rpws_working_label->duration = 0.0;
		rpws_working_label->fband = 0;
		rpws_working_label->mode_mask = 0;
#ifdef HFR_XLATE
		for(i = 0; i < HFR_XLATE; i++)
			rpws_working_label->hfr_xlate[i] = 0;
#else
		rpws_working_label->hfr_xlate = 0;
#endif
		rpws_working_label->sample_count = 80;
		rpws_working_label->pad_class = RPWS_LABEL_PAD_STIM;
		rpws_working_label->record_size = 0;
		rpws_working_label->record_count[0] = 0;
		rpws_working_label->record_count[1] = 1;
		rpws_working_label->dataset_size[0] = 0;
		rpws_working_label->dataset_size[1] = 80;
		rpws_working_label->Product = Product[3];
		dataset_size = 80;
		sprintf(rpws_working_label->filename, "STIM");
		sprintf(rpws_working_label->filepath1, "T%04d%01dXX",
				  pkt_ev->tm_year + 1900, (pkt_ev->tm_yday + 1) / 100);
		sprintf(rpws_working_label->filepath2, "T%04d%03d",
				  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1);
		sprintf(rpws_working_label->sclk_start, "%d/%d:%03d",
				  Partition, event_sclk->seconds, event_sclk->fine);
		offset_sclk_stop(rpws_working_label);
		sprintf(rpws_working_label->scet_start, "%4d-%03dT"
				  "%2.2d:%2.2d:%2.2d."
				  "%3.3d",
				  pkt_ev->tm_year + 1900,
				  pkt_ev->tm_yday + 1,
				  pkt_ev->tm_hour,
				  pkt_ev->tm_min,
				  pkt_ev->tm_sec, event_scet[0]->milliseconds % 1000);
		strcpy(rpws_working_label->plot_start,
				 rpws_working_label->scet_start);
		sprintf(rpws_working_label->scet_start_2,
				  "%4d-%02d-%02dT" "%2.2d:%2.2d:%2.2d", pkt_ev->tm_year + 1900,
				  pkt_ev->tm_mon, pkt_ev->tm_mday, pkt_ev->tm_hour,
				  pkt_ev->tm_min, pkt_ev->tm_sec);
		sprintf(rpws_working_label->scet_start_3,
				  "%04d-%02d-%02d(%03d)T%02d", pkt_ev->tm_year + 1900,
				  pkt_ev->tm_mon, pkt_ev->tm_mday, pkt_ev->tm_yday + 1,
				  pkt_ev->tm_hour);
		/* dump_master_label(rpws_working_label, 1, __LINE__); */
		update_time_stim(rpws_working_label);
		/* dump_master_label(rpws_label_stim, 0, __LINE__); */
		return NULL;
	case PACKET_TYPE_hfr:
	case PACKET_TYPE_lp:
#ifdef ALL
	case PACKET_TYPE_mro:
	case PACKET_TYPE_mfr:
	case PACKET_TYPE_lfdr:
#endif
		dataset_size = 0;
		return NULL;
	}
	if(rpws_working_label->instrument >= 0) {
		filename_index += 1;
		filename_index &= 0x0F;
		str =
			 update_label(rpws_working_label, dataset_size, &rpws_label_head,
							  nProdVerId);
		/**/ strcpy(filename[filename_index], str);
	/**/}
	strcpy(file_path1, rpws_working_label->filepath1);
	strcpy(file_path2, rpws_working_label->filepath2);

/*   fprintf(stdout,"%s %d new_file_name(%s)\n", __FILE__, __LINE__, filename[filename_index]); */
/*   fflush(stdout); */
	return filename[filename_index];
}


/**********************************************************************
 *	Put INFO.TXT that explains empty directories			*
 *	  We'll remove it later, after we discovered that we 		*
 *	  have written files						*
 **********************************************************************/

int remove_info_txt(struct RPWS_LABEL *label)
{
	int windex = -1;
	int status;
	char filename[256];
	char browse[256];
	char browse_html[256];
	char *temp;

	if(!label->record_count[0])
		return 0;

	if(strstr(label->filename, "WBR"))
		windex = RPWS_ARCHIVE_DATA_WBRFULL;
	if(strstr(label->filename, "WFR"))
		windex = RPWS_ARCHIVE_DATA_WFRFULL;
	if(strstr(label->filename, "RAW"))
		windex = RPWS_ARCHIVE_DATA_RAW;
	if(windex < 0)
		return 0;
	strcpy(filename, w_directory[windex]);
	strcat(filename, "/");
	if(label->filepath1) {
		if(label->filepath1[0]) {
			strcat(filename, label->filepath1);
			strcat(filename, "/");
			if(label->filepath2) {
				if(label->filepath2[0]) {
					strcat(filename, label->filepath2);
					strcat(filename, "/");
					if(label->filepath3) {
						if(label->filepath3[0]) {
							strcat(filename, label->filepath3);
							strcat(filename, "/");
						}
					}
				}
			}
		}
	}
	temp = strchr(filename, '/');
	strcpy(browse, "BROWSE");
	strcat(browse, temp);
	strcpy(browse_html, "BROWSE");
	strcat(browse_html, temp);
	strcat(filename, "INFO.TXT");
	strcat(browse, "INFO.TXT");
	strcat(browse_html, "HTML/INFO.TXT");
	status = unlink(filename);
	if(!strstr(browse, "RAW")) {
		status = unlink(browse);
		status = unlink(browse_html);
	}
	return 0;
}

int make_info_txt(char *filepath)
{
	int first = 0;
	int i;
	int depth;
	time_t time_t_uttime;
	struct tm *tm_uttime;
	FILE *file;
	char filename[256];
	char *temp;
	static char *text[] = { "PDS_VERSION_ID          = PDS3",	/* 0 */
		"RECORD_TYPE             = STREAM",	/* 1 */
		"OBJECT                  = TEXT",	/* 2 */
		"DOCUMENT_NAME           = \"INFO.TXT\"",	/* 3 */
		"  PUBLICATION_DATE        = xxxx-xx-xx",	/* 4 */
		"  DESCRIPTION             = \"Processing produced",	/* 5 */
		"      no data files for the time interval covered",	/* 6 */
		"      by this directory.\"",	/* 7 */
		"END_OBJECT              = TEXT",	/* 8 */
		"END",						  /* 9 */
		NULL, NULL
	};									  /* 10 */


	while(text[first]) {
		if(temp = strstr(text[first], "xxxx-xx-xx")) {
			time_t_uttime = time(NULL);
			tm_uttime = gmtime(&time_t_uttime);
			sprintf(temp, "%04d-%02d-%02d",
					  tm_uttime->tm_year + 1900,
					  tm_uttime->tm_mon + 1, tm_uttime->tm_mday);
		}
		first++;
	}
	depth = 0;
	for(i = 0; i < strlen(filepath); i++)
		if(filepath[i] == '/')
			depth++;
	if(depth < 4)
		return 0;
	strcpy(filename, filepath);
	strcat(filename, "INFO.TXT");
	file = fopen(filename, "w");
	if(!file) {
		fprintf(stderr, "%s/%d Unable to open %s\n", __FILE__, __LINE__,
				  filename);
		exit(1);
	}
	i = 0;
	while(text[i]) {
		fprintf(file, "%s\r\n", text[i]);
		i++;
	}
	fclose(file);
	return i;
}

/*****************************************************************************/
static char *raw_file_name(
	struct RPWS_buffer *buffer, struct event_clock *event_sclk,
	struct event_time *event_scet[],	int record_length, char *file_path1,
  char *file_path2, int nProdVerId
){
	static char *str;
	static char str0[128];
	static char filename[17][256];
	static int filename_index = 0x0F;
	int i;
	int dataset_size = 0;
	int hour_of_day;
	int minute_of_day;
	int second_of_day;
	unsigned long epoch;
	time_t pkt_etime, pkt_epoc;
	struct tm tm_temp;
	struct tm *pkt_ev = &tm_temp;
	double et;
	double dsec;
	char *sformat[] = { "D", "J" };
	char temp[128];
	int prec = 3;
	int days, milli;

	epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
		 (buffer->packet.cds_tag.epoch[1] << 16) |
		 (buffer->packet.cds_tag.epoch[2] << 8) |
		 (buffer->packet.cds_tag.epoch[3] << 0);

	pkt_etime = UTIL_event_time((struct MP_buffer *) buffer, 0);
	pkt_epoc = pkt_etime + epoch;

	event_sclk->seconds = pkt_etime;
	event_sclk->fine =
		 (UTIL_extract_MP_RTI((struct MP_buffer *) buffer) << 5) & 0x00E0;

	if(compare_time(event_sclk))
		return NULL;
	/**/ if(spice_flag) {
		sprintf(str0, "%d/%d:%03d", Partition, event_sclk->seconds,
				  event_sclk->fine);
		scs2e_c(SpaceCraft_ID, str0, &et);
		et2utc_c(et, sformat[0], prec, 32, str0);

		pkt_ev->tm_year = strtol(str0, &str, 10);
		pkt_ev->tm_mday = strtol(str + 1, &str, 10);
		pkt_ev->tm_yday = 0;		  /* ignored */
		pkt_ev->tm_mon = 1;
		pkt_ev->tm_hour = strtol(str + 3, &str, 10);
		pkt_ev->tm_min = strtol(str + 1, &str, 10);
		dsec = strtod(str + 1, &str);

		tnorm(&pkt_ev->tm_year,
				&pkt_ev->tm_mon,
				&pkt_ev->tm_mday,
				&pkt_ev->tm_yday, &pkt_ev->tm_hour, &pkt_ev->tm_min, &dsec);

		pkt_ev->tm_year -= 1900;  /* back to tm structure */
		pkt_ev->tm_yday -= 1;	  /* back to tm structure */
		pkt_ev->tm_mon -= 1;		  /* back to tm structure */
		pkt_ev->tm_sec = dsec;	  /* back to tm structure */

		milli = pkt_ev->tm_hour * 3600 * 1000;
		milli += pkt_ev->tm_min * 60 * 1000;
		milli += dsec * 1000.0;

		et2utc_c(et, sformat[1], prec, 32, str0);
		days = strtol(&str0[3], NULL, 10) - A_EPOCH;
		event_scet[0] = insert_event_scet(days, milli);
		/* fprintf(stdout, "%5d %d.%d\n", __LINE__, event_scet[0]->days,
		   event_scet[0]->milliseconds); */
	} else {
		event_scet[0] =
			 UTIL_event_scet((struct MP_buffer *) buffer, *event_sclk);
		pkt_ev = UTIL_event_scet_tm(*event_scet[0], 0);
	}
	second_of_day = pkt_ev->tm_hour * 3600 +
		 pkt_ev->tm_min * 60 + pkt_ev->tm_sec;

	minute_of_day = second_of_day / 60;
	minute_of_day = minute_of_day / KWBR60;
	minute_of_day = minute_of_day * KWBR60;
	hour_of_day = minute_of_day / 60;

	initialize_label(rpws_working_label, nProdVerId);
	sprintf(rpws_working_label->filename, "T%04d%03d_%02d_RAW",
			  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1, hour_of_day);
	/**/ sprintf(rpws_working_label->filepath1, "T%04d%01dXX",
					 pkt_ev->tm_year + 1900, (pkt_ev->tm_yday + 1) / 100);
	sprintf(rpws_working_label->filepath2, "T%04d%03d",
			  pkt_ev->tm_year + 1900, pkt_ev->tm_yday + 1);
	sprintf(rpws_working_label->sclk_start, "%d/%d:%03d",
			  Partition, event_sclk->seconds, event_sclk->fine);
	sprintf(rpws_working_label->scet_start, "%4d-%03dT"
			  "%2.2d:%2.2d:%2.2d."
			  "%3.3d",
			  pkt_ev->tm_year + 1900,
			  pkt_ev->tm_yday + 1,
			  pkt_ev->tm_hour,
			  pkt_ev->tm_min,
			  pkt_ev->tm_sec, event_scet[0]->milliseconds % 1000);
	strcpy(rpws_working_label->plot_start, rpws_working_label->scet_start);
	sprintf(rpws_working_label->scet_start_2, "%4d-%02d-%02dT"
			  "%2.2d:%2.2d:%2.2d",
			  pkt_ev->tm_year + 1900,
			  pkt_ev->tm_mon,
			  pkt_ev->tm_mday,
			  pkt_ev->tm_hour, pkt_ev->tm_min, pkt_ev->tm_sec);
	sprintf(rpws_working_label->scet_start_3, "%04d-%02d-%02d(%03d)T%02d",
			  pkt_ev->tm_year + 1900,
			  pkt_ev->tm_mon,
			  pkt_ev->tm_mday, pkt_ev->tm_yday + 1, pkt_ev->tm_hour);

	rpws_working_label->instrument = RPWS_LABEL_RAW;
	rpws_working_label->span = 1.0;
	rpws_working_label->duration = 3600.0;
	rpws_working_label->fband = ARCH_BAND_UNKNOWN;
	rpws_working_label->mode_mask = 0;
#ifdef HFR_XLATE
	for(i = 0; i < HFR_XLATE; i++)
		rpws_working_label->hfr_xlate[i] = 0;
#else
	rpws_working_label->hfr_xlate = 0;
#endif
	rpws_working_label->sample_count = 0;
	rpws_working_label->pad_class = 0;
	rpws_working_label->record_size = 0;
	rpws_working_label->record_count[0] = 0;
	rpws_working_label->record_count[1] = 1;
	rpws_working_label->dataset_size[0] = 0;
	rpws_working_label->dataset_size[1] = dataset_size;
	rpws_working_label->Product = Product[4];

	update_label(rpws_working_label, dataset_size, &rpws_label_raw,
					 nProdVerId);
	strcpy(file_path1, rpws_working_label->filepath1);
	strcpy(file_path2, rpws_working_label->filepath2);

	sprintf(temp, "raw filename %s\n", filename[filename_index]);

	filename_index += 1;
	filename_index &= 0x0F;
	strcpy(filename[filename_index], rpws_working_label->filename);

	return filename[filename_index];
}

  /**********************************************************************
   *	Calculate the name of the file the current data 		*
   *		record shoulkd reside in				*
   **********************************************************************/
int close_file_handle(struct RPWS_LABEL *label_head)
{
	struct RPWS_LABEL *temp;
	int status;
	temp = label_head;
#ifdef DEBUG_CLOSE
	fprintf(debug_file, "Close Cycle *******************\n");
#endif
	while(temp) {
		if(temp->filehandle) {
#ifdef DEBUG_CLOSE
			struct stat sbuf;
			char *filename;
			int sstatus;
			int serrno;
#endif
			status = fclose(temp->filehandle);
			remove_info_txt(temp);
#ifdef DEBUG_CLOSE
			filename = rpws_file_rename(temp);
			errno = 0;
			sstatus = stat(filename, &sbuf);
			serrno = errno;
			fprintf(debug_file, "%d ", __LINE__);
			fprintf(debug_file, "%s[%s] Size(", filename, temp->filename);
			fflush(debug_file);
			if(sstatus) {
				fprintf(debug_file, "%X-%s/", sstatus, strerror(serrno));
			} else {
				fprintf(debug_file, "act:%d/", sbuf.st_size);
				fflush(debug_file);
			}
			if(!strstr(filename, "RAW"))
				fprintf(debug_file, "lbl:%d ",
						  temp->record_count[0] * temp->record_size);
			fflush(debug_file);
			fprintf(debug_file, ")Rec %6d  ", temp->record_count[0]);
			fflush(debug_file);
			fprintf(debug_file, "\n");
			fflush(debug_file);
#endif
			if(status < 0)
				fprintf(debug_file, "%s/%d close failed %s\n", __FILE__,
						  __LINE__, strerror(errno));
			temp->filehandle = NULL;
		}
		temp = temp->link;
	}
	return 0;
}

/**********************************************************************
 *	
 **********************************************************************/
FILE *new_file_open(
	char *directory[], int d_index, char *path[], char *fname, char *mode
#ifdef DEBUG_CLOSE
	, struct RPWS_LABEL * label
#endif
){
	FILE *temp_file;
	int i;
	int status;						  /* chmod */
	char data_filename[1024] = { "" };
	char browse_filename[1024] = { "" };
	char raw_filename[1024] = { "" };
	volatile int directory_index;

	if(d_index < 0)
		directory_index = 0;
	else
		directory_index = d_index;

	if(debug_flag & 0x1000){
		fprintf(debug_file, "%5d new_file_open %d  %d\n",
				  __LINE__,
				  directory_index, RPWS_ARCHIVE_BROWSE_WBR + directory_index);
		fflush(debug_file);
	}

	strcpy(data_filename, directory[directory_index]);
	strcat(data_filename, "/");

	strcpy(raw_filename, directory[RPWS_ARCHIVE_DATA_RAW]);
	strcat(raw_filename, "/");

	if(directory_index < 3) {
		strcpy(browse_filename, directory[RPWS_ARCHIVE_BROWSE]);
		strcat(browse_filename, "/");
		strcat(browse_filename,
				 directory[RPWS_ARCHIVE_BROWSE_WBR + directory_index]);
		strcat(browse_filename, "/");
	}
	
	for(i = 0; i < 8; i++) {
		if(path[i] && path[i][0]) {
			strcat(data_filename, path[i]);
			strcat(data_filename, "/");
			status = new_dir_mkdir(data_filename, S_IRWXU |
										  S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);

			strcat(raw_filename, path[i]);
			strcat(raw_filename, "/");
			status = new_dir_mkdir(raw_filename, S_IRWXU |
										  S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);

			if(browse_filename[0]) {
				strcat(browse_filename, path[i]);
				strcat(browse_filename, "/");
				status = new_dir_mkdir(browse_filename, S_IRWXU |
											  S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
			}
		} 
		else {
			make_info_txt(data_filename);
			make_info_txt(raw_filename);
			make_info_txt(browse_filename);
			strcat(browse_filename, "HTML");
			strcat(browse_filename, "/");
			status = new_dir_mkdir(browse_filename, S_IRWXU |
										  S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
			make_info_txt(browse_filename);
			break;
		}
	}
	strcat(data_filename, fname);
	temp_file = NULL;
#ifdef DEBUG_CLOSE
	fprintf(debug_file, "\n%s/%d Opening handle %d (%d/%d) %s\n",
			  __FILE__, __LINE__, open_file_count, label->record_count[0],
			  label->record_count[1], label->filename);
#endif
	if(strcmp(mode, "a")) {
		fprintf(debug_file,
				  "\n%s %d Internal error: Invalid Mode Specified:\"%s\"\n",
				  __FILE__, __LINE__, mode);
	}
	if(open_file_count < maximum_open_file_count) {
		temp_file = fopen(data_filename, mode);
		if(!temp_file)
			fprintf(debug_file, "\n%s %d Internal error: Open Failed: %s\n",
					  __FILE__, __LINE__, strerror(errno));
	}

	if(temp_file) {
		open_file_count++;
		status = chmod(data_filename, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
		
		if(status < 0)
			fprintf(debug_file, "%s/%d chmod failed (%s) %s\n", __FILE__,
					  __LINE__, data_filename, strerror(errno));
		if(debug_flag & 0x1000)
			fprintf(debug_file, "Data File: %s\n", data_filename);
		return temp_file;
	}
	
	close_file_handle(rpws_label_head);
	close_file_handle(rpws_label_raw);
	open_file_count = 0;
	temp_file = fopen(data_filename, mode);
	
	if(temp_file) {
		open_file_count++;
		status =
			 chmod(data_filename,
					 S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH);
		if(status < 0)
			fprintf(debug_file, "%s/%d chmod failed (%s) %s\n", __FILE__,
					  __LINE__, data_filename, strerror(errno));
		if(debug_flag & 0x1000)
			fprintf(debug_file, "Data File: %s\n", data_filename);
		return temp_file;
	}
	fprintf(debug_file, "%s %d Open fail:\"%s\" %s\n", __FILE__, __LINE__,
			  data_filename, strerror(errno));
	exit(errno);
}

/**********************************************************************/
/*  FILE *new_file_handle(struct RPWS_buffer *buffer,			          */
/**********************************************************************/
struct RPWS_LABEL *new_file_handle(
	struct RPWS_buffer *buffer, struct event_clock *event_sclk,
	struct event_time *event_scet[], int record_length, int i_index,
	int nProdVerId
){

	static struct RPWS_LABEL *current_label[2] = { NULL, NULL };
	char *filename;
	static char tfilepath1[128];
	static char tfilepath2[128];
	static char tfile[256];
	char tyear[64];
	char tdoy[64];
	char *tpath[8];
	int index;
	int status;						  /* chmod */

	index = i_index & 1;

	/*****************************************/
	/* Build up the filename for this record */
	/*****************************************/
	if(event_sclk->seconds > CASSINI_MAXIMUM_CLOCK) {
		fprintf(stdout, "%s/%d %08X > %08X\n", __FILE__, __LINE__,
				  event_sclk->seconds, CASSINI_MAXIMUM_CLOCK);
	}

	switch (index) {
	case 1:
		filename =
			 new_file_name(buffer, event_sclk, event_scet, record_length,
								tfilepath1, tfilepath2, nProdVerId);
		break;

	case 0:
		filename =
			 raw_file_name(buffer, event_sclk, event_scet, record_length,
								tfilepath1, tfilepath2, nProdVerId);
		if(!filename)
			fprintf(debug_file, "\n%s %d raw_file_name NULL %X\n", __FILE__,
					  __LINE__, event_sclk->seconds);
		else if(!filename[0])
			fprintf(debug_file, "\n%s %d raw_file_name EMPTY %X\n", __FILE__,
					  __LINE__, event_sclk->seconds);
		break;
	}

	if(record_length & 0x0F) {
#ifdef DEBUG2050
		fprintf(debug_file, "%s %d FILENAME  ", __FILE__, __LINE__);
		if(filename)
			fprintf(debug_file, "%s", filename);
		fprintf(debug_file, "\n");
#endif
	}

	if(!filename)
		return NULL;
	if(!filename[0])
		return NULL;
	if(debug_flag & 0x1000)
		fprintf(debug_file, "%5d new_file_handle %s\n", __LINE__, filename);

	/**************************************************/
	/* see if it is the same as the last time through */
	/**************************************************/
	if(current_label[index]) {
/*         fprintf(stdout,"%5d %p-%p(%32s) %p(%32s) \n", __LINE__, 
             			current_label[index], 
             			current_label[index]->filename, 
             			current_label[index]->filename, 
             			filename, 
             			filename); */
/*         fflush(stdout); */
		if(!strcmp(current_label[index]->filename, filename)) {
			/* rpws_file_reopen(current_label[index], __LINE__); */
			return current_label[index];	/* ->filehandle; */
		}
	}

	/************************************/
	/* Otherwise, search the open files */
	/************************************/
	switch (index) {
	case 1:
		current_label[index] = rpws_label_head;
		break;

	case 0:
		current_label[index] = rpws_label_raw;
		break;
	}

	while(current_label[index]) {
		if(!strcmp(current_label[index]->filename, filename)) {	/* MATCH ? */

			if(!current_label[index]->filehandle) {	/* OPEN ? */

				tfile[0] = 0;
				tpath[0] = tfilepath1;
				tpath[1] = tfilepath2;
				tpath[2] = NULL;
				strcat(tfile, filename);
				switch (index) {

				case 1:
					strcat(tfile, ".DAT");
					break;

				case 0:
					strcat(tfile, ".PKT");
					break;
				}

				current_label[index]->filehandle = 
				      new_file_open(w_directory, current_label[index]->instrument,
				                    tpath, tfile, "a"
#ifdef DEBUG_CLOSE
				                     ,current_label[index]
#endif
				      );
/*          if(0){
					fprintf(stdout, "%5d OPEN ", __LINE__);
					fprintf(stdout, "%p=new_file_open", 
					current_label[index]->filehandle);
					fprintf(stdout, "%s,", w_directory);
					fprintf(stdout, "%d,", current_label[index]->instrument);
					fprintf(stdout, "%s,", tpath);
					fprintf(stdout, "\n");
				}
*/
			}
/*       fprintf(stdout, " %5d handle:0x%08p\n", __LINE__, current_label[index]->filehandle); */
			fflush(stdout);

			/* rpws_file_reopen(current_label[index], __LINE__); */
			return current_label[index];	/* ->filehandle; */
		}
		current_label[index] = current_label[index]->link;
	}
	
	fprintf(debug_file, "%5d Filehandle not found for file \"%s\"\n", __LINE__, filename);
	current_label[index] = NULL;
	exit(1);
}



/**********************************************************************
 *	WTF?								*

char *make_path(char *buffer, char type)
{
	static char buf[1024];
	char *temp;
	strcpy(buf, buffer);
	temp = strchr(buf, '.');
	if(temp) {
		temp[1] = type;
	}
	return buf;
}
 **********************************************************************/

/**********************************************************************
 *	Read next record						*
 *		skip bad records in the minipacket files		*
 *		skip after output accumulation is exceeded		*
 **********************************************************************/

/* int shit(unsigned char *buffer, int index){
	return buffer[index] & 0x00FF;
}*/

int util_getbuffer_RPWS(struct RPWS_buffer *buffer, FILE * input,
								int eof_flag)
{
	int ilen = 0;
	int bad_record = 1;
	int i, j, k;
	int mp_len;
	int local_eof_flag = eof_flag;
#ifdef DEBUG2050
	fprintf(debug_file, "\n");
#endif
	if(rpws_accumulate(0) >= 0.0) {
		while(bad_record) {
			bad_record = 0;
			ilen = UTIL_getbuffer_RPWS(buffer, input, local_eof_flag);
			if(ilen > 0) {			  /* data to read ??? */
				mp_len = UTIL_MP_length((struct MP_buffer *) buffer);
				if(mp_len == 0){	  /* funny record encountered ? */
					bad_record = 1;  /* then dump it 1 */
				}
				else {				  /* mp_len > 0 */

					/* fprintf(debug_file, "Record %8s %02X\n",
					   UTIL_extract_MP_packet_type((struct MP_buffer*)buffer),
					   UTIL_extract_MP_bits((struct MP_buffer *)buffer) ); */
					switch (UTIL_extract_MP_bits((struct MP_buffer *) buffer)) {
					case PACKET_TYPE_stim:
					case PACKET_TYPE_lp:
					case PACKET_TYPE_hfr:
					case PACKET_TYPE_wbr:
					case PACKET_TYPE_wfr:
#ifdef ALL
					case PACKET_TYPE_mro:
					case PACKET_TYPE_mfr:
					case PACKET_TYPE_lfdr:
#endif
						bad_record = 0;	/* accept this record */
						break;
					default:
						bad_record = 1;	/* not interested */
						break;		  /* in this record */
					}
				}						  /* mp_len > 0 */
			} /* if (ilen>0) */
			else {					  /* if(ilen>0) */
				/* EOF EOF EOF EOF */
				bad_record = 0;
				ilen = 0;
			}							  /* if(ilen>0) */
		}								  /* bad record */
	}									  /* if(rpws_accumulate(0) > 0.0) */
	return ilen;
}

/* ************************************************************************* */
/* Process 1 input file                                                      */
/*	   this handles the case where we feed data from stdin                    */
/*                                                                           */
/* ************************************************************************* */

int main_file(FILE * input, int write_flag, int nProdVerId)
{
	struct RPWS_LABEL *output_label;
	struct RPWS_LABEL *raw_output_label;
	double event_et;
	int record_count = 0;
	int ilen;
	int klen;
	int HFR_Valid;
	int eof_flag = UTIL_GET_NON_BLOCKING;
	static char old_name_wbr[128] = { "" };
	static char old_name_wfr[128] = { "" };
	char full_name[256];
	char stim_buffer_1[1024];
	char stim_buffer_2[1024];
	int bBadSize = 0;

	struct SPICE_TIME spice_time_array;

	struct event_clock event_sclk;
	struct event_time *event_scet;

	union {
		char *char_temp;
		struct RPWS_buffer *RPWS_temp;
	} tmp;

	/* rpws_working_label = malloc(sizeof(struct RPWS_LABEL)); */

	ilen = 0;
	ilen = util_getbuffer_RPWS(buffer, input, eof_flag);
	while(ilen > 0) {
				
		/************************************************************************
		 *    We can muck with time here to correct for really F**K*D		*
		 *    up time-tags.  This occurred shortly after launch... At		*
		 *    least SPICE doesn't seem to agree with what we have		*
		 *    in the CHDO headers...						*
		 *									*
		 *    IF this time manipulation is turned on, it is still  necessary	*
		 *    later on (about 30 lines down from here...  We're only fixing	*
		 *    the CDS clock here, not fixing up the event time for the		*
		 *    data record...							*
		 ************************************************************************/
		if(spice_flag & 0x02) {	  /* spfix flag is set */
			event_et =
				 spice_time_x((struct archive_event_clock *) &buffer->packet.
								  chdo_tag.sclk.seconds, &spice_time_array, NULL,
								  __LINE__);
			buffer->packet.chdo_tag.scet.days = spice_time_array.days;
			buffer->packet.chdo_tag.scet.milliseconds = spice_time_array.msec_of_day;
		}
		
		memset(archive, 0, buffer_size);

		/* output_handle */
		/* In the rare case that a packet has no valid samples, don't include   */
		/* it in the main product output                                        */
		int size = prep_size(buffer, 0, __LINE__);
		if(size > 0)
			output_label = new_file_handle(
			                  buffer, &event_sclk, &event_scet,
			                  prep_size(buffer, Size_Flag, __LINE__), 1, nProdVerId
			               );
		else
			output_label = NULL;
		
		
		/* raw_output_handle */
		raw_output_label = new_file_handle(
		                      buffer, &event_sclk, &event_scet,
		                      prep_size(buffer, Size_Flag, __LINE__), 0,
		                      nProdVerId
		                   );

					 
		if(raw_output_label){
			if(buffer->f_length){
				if(rpws_accumulate(buffer->f_length) >= 0.0) {
					if(!raw_output_label->filehandle)
						rpws_file_reopen(raw_output_label, __LINE__);
					raw_output_label->record_count[0]++;
					raw_output_label->dataset_size[0] += buffer->f_length + 4;
					if(!(debug_flag & 0x0200000))
						UTIL_putbuffer_RPWS(buffer, raw_output_label->filehandle);	/* raw_output_handle); 
																										 */
				}
			}
		}


		 /***********************************************\
      < **	Split into FILE OPS and NON-FILE OPS     ** >
       \***********************************************/
		
		if(output_label) {		  /* output_handle) */
			
			switch (buffer->record_type & 0xFF) {
					 
			case PACKET_TYPE_wbr:
				/* reformat and write */
				/* display_sclk_scet(buffer); */
				if(prep(archive, buffer, &event_sclk, event_scet, event_et)) {
					spice_time(archive, "WBR", __LINE__);
					rpws_fsw_ver(archive, NULL, NULL, NULL);
					prep_wbr(archive, buffer);
					prep_data(archive, buffer, SAMPLE_SIZE_WBR);
					validity_check(archive);
					write_data(output_label, archive, 1);
				}
				break;
					 
				
			case PACKET_TYPE_wfr:
				/* reformat and write */
				/* display_sclk_scet(buffer); */
				if(prep(archive, buffer, &event_sclk, event_scet, event_et)) {
					spice_time(archive, "WFR", __LINE__);
					rpws_fsw_ver(archive, NULL, NULL, NULL);
					prep_wfr(archive, buffer);
					prep_data(archive, buffer, SAMPLE_SIZE_WFR);
					validity_check(archive);
					write_data(output_label, archive, 2);
				}
				break;
			}							  
		} 
		else {
			
			switch (buffer->record_type & 0xFF) {
			case PACKET_TYPE_wbr:
				fprintf(debug_file, "WARNING: %s/%d, WBR Write Skip (bad packet)\n", __FILE__, __LINE__);
				break;
			case PACKET_TYPE_wfr:
				fprintf(debug_file, "WARNING: %s/%d, WFR Write Skip (bad packet)\n", __FILE__, __LINE__);
				break;
				
			  /***************************************************\
			 / ****	NOT archiving L/P here, but we do need   **** \
			<  ****	to snoop a little to find out what L/P   ****  >
			 \ ****	is doing when WBR antennal select to LP  **** /
			  \***************************************************/
			case PACKET_TYPE_lp:
				if(prep(archive, buffer, &event_sclk, event_scet, event_et)) {
					spice_time(archive, "L/P", __LINE__);
					lp_status_packet = rpws_lp_status(archive, buffer);
					if(lp_status_packet->lp_mode == LP_MODE_SWEEP) {
						prep_lp_sound(stim_buffer_1, stim_buffer_2, archive,
										  buffer, lp_status_packet, "LPSW",
										  Delimiter);
						rpws_label_stim->record_count[0] += 1;
						if(stim_handle_1) {
							write_stim_data(stim_handle_1, stim_buffer_1,
												 new_stim_filename_1);
							fflush(stim_handle_1);
						}
						if(stim_handle_2) {
							write_stim_data(stim_handle_2, stim_buffer_2,
												 new_stim_filename_2);
							fflush(stim_handle_2);
						}
					}
				}
				break;
				
			  /***************************************************\
          / ****	NOT archiving HFR here, but we do need   **** \
         <  ****	to snoop a little to find out what HFR   ****  >
          \ ****	is doing when WBR antennal select to HF  **** /
           \***************************************************/
			case PACKET_TYPE_hfr:
				if(prep(archive, buffer, &event_sclk, event_scet, event_et)) {
					spice_time(archive, "HFR", __LINE__);
					hfr_status_packet = rpws_hfr_status(archive, buffer);
					if(hfr_status_packet->hfr_mode == HFR_MODE_SOUNDER) {
						if(rpws_hfr_sound_valid(buffer)) {
							prep_hfr_sound(stim_buffer_1, stim_buffer_2, archive,
												buffer, hfr_status_packet, "HSND",
												Delimiter);
							rpws_label_stim->record_count[0] += 1;
							if(stim_handle_1) {
								write_stim_data(stim_handle_1, stim_buffer_1,
													 new_stim_filename_1);
								fflush(stim_handle_1);
							}
							if(stim_handle_2) {
								write_stim_data(stim_handle_2, stim_buffer_2,
													 new_stim_filename_2);
								fflush(stim_handle_2);
							}
						}
					}
				}
				if(hfr_status_packet->hfr_mode == HFR_MODE_CALIBRATE) {
					if(rpws_hfr_cal_valid(buffer)) {
						prep_hfr_sound(stim_buffer_1, stim_buffer_2, archive,
											buffer, hfr_status_packet, "HCAL",
											Delimiter);
						rpws_label_stim->record_count[0] += 1;
						if(stim_handle_1) {
							write_stim_data(stim_handle_1, stim_buffer_1,
												 new_stim_filename_1);
							fflush(stim_handle_1);
						}
						if(stim_handle_2) {
							write_stim_data(stim_handle_2, stim_buffer_2,
												 new_stim_filename_2);
							fflush(stim_handle_2);
						}
					}
				}
				if(hfr_status_packet->hfr_mode == HFR_MODE_DUMP) {
					if(rpws_hfr_dump_valid(buffer)) {
						prep_hfr_sound(stim_buffer_1, stim_buffer_2, archive,
											buffer, hfr_status_packet, "HDMP",
											Delimiter);
						rpws_label_stim->record_count[0] += 1;
						if(stim_handle_1) {
							write_stim_data(stim_handle_1, stim_buffer_1,
												 new_stim_filename_1);
							fflush(stim_handle_1);
						}
						if(stim_handle_2) {
							write_stim_data(stim_handle_2, stim_buffer_2,
												 new_stim_filename_2);
							fflush(stim_handle_2);
						}
					}
				}
				break;
				
			  /***************************************************\
          / **       Make STIM records, if requested         ** \
         <  ** These are funny, kind-of hacked in            **  >
          \ ** for now...                                    ** /
           \***************************************************/
			case PACKET_TYPE_stim:
				if(prep(archive, buffer, &event_sclk, event_scet, event_et)) {
					spice_time(archive, "STIM", __LINE__);
					rpws_fsw_ver(archive, NULL, NULL, NULL);
					prep_stim(stim_buffer_1, stim_buffer_2, archive, buffer,
								 Delimiter);
					rpws_label_stim->record_count[0] += 1;

					stim_handle_1 =
						 new_stim_handle(stim_handle_1, new_stim_filename_1, 1,
											  Delimiter);
					if(stim_handle_1) {
						write_stim_data(stim_handle_1, stim_buffer_1,
											 new_stim_filename_1);
						fflush(stim_handle_1);
					}

					stim_handle_2 =
						 new_stim_handle(stim_handle_2, new_stim_filename_2, 2,
											  Delimiter);
					if(stim_handle_2) {
						write_stim_data(stim_handle_2, stim_buffer_2,
											 new_stim_filename_2);
						fflush(stim_handle_2);
					}
				}
				break;
			}
		}								  /* if(output_handle) */

		record_count++;
		
		/* Read next record & do timeout (for real-time ops) */
		ilen = util_getbuffer_RPWS(buffer, input, eof_flag);
	}
	return record_count;
}

/**********************************************************************
 *	Discard any data records with a SCLK that is prior to		*
 *	  launch.  The database contains some pre-launch test data.	*
 *	ALSO any bench model data is discarded here as the UNIX time	*
 *	appears to be prior to launch (i.e. SCLK epoch differ enought	*
 *	 to make UNIX time-tags look like they're before launch)	*
 **********************************************************************/
char *discard_prelaunch(char *filename, unsigned int *sclk)
{
	if(sclk[0] < CASSINI_FLIGHT_SCLK)
		return NULL;
	return filename;
}

char *discard_invalid(char *filename, unsigned int *sclk)
{
	if(sclk[0] > CASSINI_MAXIMUM_CLOCK)
		return NULL;
	return filename;
}

/*****************************************************************************
 *	Extract a filename from the database file the database file is a simple
 * list of files we have available.  Each detail line has a start and stop
 * time expressed in both SCLK and SCET.
 */
char *extract_filename(char *buf, unsigned int *sclk, char **start,
							  char **stop)
{
	static char *delim = { " \t" };
	static char buffer[256];
	char *tokens[6];
	int i;

	memset(buffer, 0, 256);		  /* pre clear */
	strcpy(buffer, buf);			  /* line from database */
	tokens[0] = strtok(buffer, delim);

	for(i = 1; i < 6; i++) {
		tokens[i] = NULL;
		if(tokens[i - 1])
			tokens[i] = strtok(NULL, delim);
	}

	sclk[0] = 0;
	*start = tokens[0];
	*stop = tokens[1];

	if(tokens[3])
		sclk[0] = strtol(tokens[3], NULL, 16);

	return tokens[4];
}

/**********************************************************************
 *	Process list of files						*
 *		read each successive entry in the database file		*
 *		and collect the records and deliver them to the 	*
 *		archive.						*
 **********************************************************************/
void main_file_list(int write_flag, char *list_file, int nProdVerId)
{
	unsigned int sclk;
	static char buffer[512];
	FILE *list = NULL;
	FILE *input = NULL;
	int icnt;
	int status;
	char *filename;
	char *fp;
	char *start_time;
	char *stop_time;

	rpws_working_label = malloc(sizeof(struct RPWS_LABEL));
	/**/ if(debug_flag & 0x8000)
		fprintf(debug_file, "Using database  %s\n", list_file);

	list = fopen(list_file, "r");

	if(!list) {
		fprintf(debug_file, "%s %d Open fail:\"%s\" %s\n", __FILE__,
				  __LINE__, list_file, strerror(errno));
		return;
	}

	while(1) {
		fp = fgets(buffer, 256, list);

		if(!fp)
			break;

		filename = extract_filename(buffer, &sclk, &start_time, &stop_time);
		filename = discard_prelaunch(filename, &sclk);
		filename = discard_invalid(filename, &sclk);
		filename = discard_before_database(filename, stop_time);
		filename = discard_after_database(filename, start_time);

		if(filename) {

			input = fopen(filename, "rb");
			if(input) {
				if(debug_flag & 0x8000) {
					fprintf(debug_file, "Using file      %s", filename);
					fflush(debug_file);
				}

				status = main_file(input, write_flag, nProdVerId);
				if(debug_flag & 0x8000) {
					if(!status)
						fprintf(debug_file, " SKIP/media FULL %X\n", status);
				}

				icnt += 1;
				fclose(input);
				filename = NULL;
				input = NULL;

				if(debug_flag & 0x8000)
					fprintf(debug_file, "\n");

				fflush(debug_file);
			} else {
				fprintf(debug_file, "%s %d Open fail:\"%s\" %s\n", __FILE__,
						  __LINE__, filename, strerror(errno));
			}
		}
	}
	return;
}


/*****************************************************************************/
int main(int argc, char *argv[])
{
	FILE *input = stdin;
	FILE *stsfile = stderr;
	char *body = { "" };
	char fname[128] = { '\0' };
	char sBrScriptDir[256] = { '\0' };
	int icnt;
	struct RPWS_LABEL *label_temp;
	int i;
	char err_dir[] = { "SET" };
	char err_action[] = { "REPORT" };
	double dtemp;
	double et;
	int nProdVerId = 1;
	char *sKernelFile = NULL;


	/* initialize the MP database location, if I can */
	if(getenv("RPWS_MPDB")) {
		g_sDatabase =
			 (char *) calloc(strlen(getenv("RPWS_MPDB")) + 8, sizeof(char));
		strcpy(g_sDatabase, getenv("RPWS_MPDB"));
	}

	write_flag = 1;
	fg_flags(argc, argv);

	if(fg_flag("mask"))
		Size_Flag = 1;
	if(fg_flag("zero"))
		Zero_Flag = 1;
	if(fg_flag("plot_full"))
		Plot_Flag = 0;

	debug_flag = fg_int("debug", debug_flag);

	TIMEFLAG = fg_int("timeflag", TIMEFLAG);

	if(fg_flag("html401"))
		debug_flag |= 0x100000;

	if(debug_flag & 0x80000)
		write_flag = 0;
	else
		write_flag = 1;

	if(debug_flag)
		fprintf(debug_file, "Debug_Flag = 0x%08X\n", debug_flag);

	fprintf(stdout, "bisflag before(%04X)", bisflag);
	bisflag = fg_int("bisflag", bisflag);
	fprintf(stdout, " after(%04X)\n", bisflag);

	KWBR60 = fg_int("wbr_size", KWBR60);
	KWFR60 = fg_int("wfr_size", KWFR60);

	/* maximum_open_file_count = ulimit(UL_GDESLIM) / 2; */
	maximum_open_file_count = 60;	/* --cwp */

	if(maximum_open_file_count < 20) {
		fprintf(debug_file, "file descriptors (too low!) %d\n",
				  maximum_open_file_count);
	} else
		fprintf(debug_file, "file descriptors %d\n",
				  maximum_open_file_count);

	setup_sub_rti_table(fg_flag("subrti"));
	if(fg_flag("kernel")) {
		sKernelFile = malloc(256);
		strcpy(sKernelFile, fg_flagc("kernel"));
		fprintf(debug_file, "Spice meta-kernel filename: %s\n", sKernelFile);
		fflush(debug_file);
	}

	if(fg_flag("diff")) {
		maximum_difference = fg_int("diff", maximum_difference);
	}

	if(fg_flag("cd_index")) {
		cd_index = malloc(256);
		strcpy(cd_index, fg_flagc("cd_index"));
		fprintf(debug_file, "cd_index String: %s\n", cd_index);
		fflush(debug_file);
	}
	if(fg_flag("cd_count")) {
		cd_count = malloc(256);
		strcpy(cd_count, fg_flagc("cd_count"));
		fprintf(debug_file, "cd_count String: %s\n", cd_count);
		fflush(debug_file);
	}
	if(fg_flag("cd_format")) {
		cd_format = malloc(256);
		strcpy(cd_format, fg_flagc("cd_format"));
		fprintf(debug_file, "cd_format String: %s\n", cd_format);
		fflush(debug_file);
	}

	if(fg_flag("lblver")) {
		Label_Ver = malloc(256);
		strcpy(Label_Ver, fg_flagc("lblver"));
		fprintf(debug_file, "Label Version String: %s\n", Label_Ver);
		fflush(debug_file);
	}

	if(fg_flag("phase")) {
		Mission_Phase = malloc(256);
		strcpy(Mission_Phase, fg_flagc("phase"));
		fprintf(debug_file, "Mission Phase String: %s\n", Mission_Phase);
		fflush(debug_file);
	}

	if(fg_flag("rawdir")) {
		w_directory[RPWS_ARCHIVE_DATA_RAW] = fg_flagc("rawdir");
	}
	if(fg_flag("wbrdir")) {
		w_directory[RPWS_ARCHIVE_DATA_WBRFULL] = fg_flagc("wbrdir");
	}
	if(fg_flag("wfrdir")) {
		w_directory[RPWS_ARCHIVE_DATA_WFRFULL] = fg_flagc("wfrdir");
	}
	if(fg_flag("ancildir")) {
		w_directory[RPWS_ARCHIVE_DATA_ANCIL] = fg_flagc("ancildir");
	}
	if(fg_flag("extradir")) {
		w_directory[RPWS_ARCHIVE_EXTRAS] = fg_flagc("extradir");
	}
	if(fg_flag("scriptdir")) {
		w_directory[RPWS_ARCHIVE_script] = fg_flagc("scriptdir");
	}

	if(fg_flag("target")) {
		char *target;
		Target = malloc(256);
		memset(Target, 0, 256);
		target = fg_flagc("target");
		strcat(Target, target);
	}

	if(fg_flag("coordinate")) {
		char *coordinate;
		Coordinate = malloc(256);
		memset(Coordinate, 0, 256);
		coordinate = fg_flagc("coordinate");
		strcat(Coordinate, coordinate);
	}
	if(fg_flag("orbit")) {
		char *orbit;
		orbit = malloc(256);
		memset(orbit, 0, 256);
		orbit = fg_flagc("orbit");
		strcat(Orbit, orbit);
	}

	if(fg_flag("body")) {
		static char Body[32] = { "_" };
		strcat(Body, fg_flagc("body"));
		body = Body;
	}



	if(fg_flag("mafi") == '+') {
		Mafi_Flag = 1;
	}
	if(fg_flag("ephem") == '+') {
		ephem_flag = 1;
	}
	if(fg_flag("ephem") == '-') {
		ephem_flag = 0;
	}
	if(fg_flag("splitstim") == '-') {
		split_stim_flag = 1;
	}
	if(fg_flag("splitstim100") == '-') {
		split_stim_flag = 100;
	}
	if(fg_flag("splitstim365") == '-') {
		split_stim_flag = 365;
	}
	if(fg_flag("splitstim1100") == '-') {
		split_stim_flag = 1100;
	}
	if(fg_flag("splitstim1365") == '-') {
		split_stim_flag = 1365;
	}

	if(fg_flag("size") == '-') {
		int size;
		size = maximum_byte_count / 1000000;
		size = fg_int("size", size);
		maximum_byte_count = (float) size *1000000.0;
		if(maximum_byte_count < 1.0)
			maximum_byte_count = -1.0;
	}

	if(fg_flag("pad") == '+')
		Line_Pad = LINE_PAD;
	if(fg_flag("pad") == '-')
		Line_Pad = 0;

	if(fg_flag("stim_delim")) {
		char string[256];
		strcpy(string, fg_flagc("stim_delim"));
		Delimiter = string[0];
	}

	if(fg_flag("spice") || fg_flag("spfix")) {
		spice_flag = 1;
		if(fg_flag("spfix"))
			spice_flag = 3;
	}

	if(sKernelFile == NULL) {
		if(getenv("CAS_TIME_KERNELS") == NULL) {
			fprintf(stderr,
					  "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
					  "is not defined.\n");
			return 13;
		}
		sKernelFile = getenv("CAS_TIME_KERNELS");
	}

	furnsh_c(sKernelFile);
	erract_c(err_dir, strlen(err_action), err_action);
	fprintf(debug_file, "%s: spice: FURNSH\n", argv[0]);
	fflush(debug_file);


	if(fg_flag("help") || fg_flag("h") || fg_flag("cdrom")
		|| fg_flag("essay")) {
		rpws_help(argc, argv, stdout);
		fprintf(stdout, "try using \"%s -h | lpr \"\n", argv[0]);
		fflush(stdout);

		if(fg_flag("essay")) {
			rpws_essay(stdout, archive_time_format);
			fprintf(stdout, "\f");
			rpws_ver_help(stdout, 1);
		}

		if(fg_flag("cdrom")) {
			int i, j;
			fprintf(stdout, "  -------- File Arrangements ------------\n");
			fprintf(stdout, "Directory Structure: %s\n",
					  directory_structure[0].text);
			i = 1;
			while(directory_structure[i].flag) {
				for(j = 0; j < directory_structure[i].flag; j++)
					fprintf(stdout, "\t");
				fprintf(stdout, "%s\n", directory_structure[i].text);
				i++;
			}
			fprintf(stdout, "\n");
			fflush(stdout);
		}
		return 0;					  /* doc dump */
	}


	buffer = malloc(buffer_size);
	/**/ archive = malloc(buffer_size);
	/**/ rpws_wbr_time = malloc(sizeof(struct RPWS_LABEL));
	initialize_label(rpws_wbr_time, nProdVerId);
	strcpy(rpws_wbr_time->sclk_start, "9/999999999:255");
	strcpy(rpws_wbr_time->sclk_stop, "0/000000000:000");

	rpws_wfr_time = malloc(sizeof(struct RPWS_LABEL));
	initialize_label(rpws_wfr_time, nProdVerId);
	strcpy(rpws_wfr_time->sclk_start, "9/999999999:255");
	strcpy(rpws_wfr_time->sclk_stop, "0/000000000:000");

	rpws_dataset_time = malloc(sizeof(struct RPWS_LABEL));
	initialize_label(rpws_dataset_time, nProdVerId);
	/* memset(rpws_dataset_time->sclk_start, 0, 32); / * not required,
	   init_lab does this */
	/* memset(rpws_dataset_time->sclk_stop, 0, 32); / * not required,
	   init_lab does this */
	strcpy(rpws_dataset_time->sclk_start, "9/999999999:255");
	strcpy(rpws_dataset_time->sclk_stop, "0/000000000:000");

	if(fg_flag("browsescriptdir"))
		strncpy(sBrScriptDir, fg_flagc("browesscriptdir"), 254);
	else
		strncpy(sBrScriptDir, INST_BIN, 254);

	if(fg_flag("database"))
		fg_flagx("-files", g_sDatabase);

	if(fg_flag("prodverid"))
		nProdVerId = fg_int("prodverid", 1);

	if(fg_flag("times")) {
		char *temp;
		temp = fg_flagc("times");
		if(temp)
			if(strlen(temp))
				namefile = temp;
		if(!rpws_ver_load(namefile))
			fprintf(stderr, "IEB/FSW times file:\"%s\" not found\n",
					  namefile);
	}

	if(fg_flag("tdump")) {
		rpws_ver_dump(stdout, RPWS_LABEL_WFR);
		return 0;					  /* doc dump */
	}
	if(fg_flag("t2dump")) {
		rpws_ver_dump(stdout, RPWS_LABEL_WBR);
		return 0;					  /* doc dump */
	}

	if(fg_flag("dbase_st")) {
		char temp[64];
		parsetime(fg_flagc("dbase_st"),
					 &database_start_time.year,
					 &database_start_time.month,
					 &database_start_time.mday,
					 &database_start_time.yday,
					 &database_start_time.hour,
					 &database_start_time.minute, &database_start_time.second);
		sprintf(rpws_dataset_time->scet_start, "%04d-%03dT%02d:%02d:%02d",
				  database_start_time.year,
				  database_start_time.yday,
				  database_start_time.hour,
				  database_start_time.minute,
				  (int) database_start_time.second);
		strcpy(rpws_dataset_time->plot_start, rpws_dataset_time->scet_start);
		sprintf(rpws_dataset_time->scet_start_2,
				  "%04d-%02d-%02dT%02d:%02d:%02d", database_start_time.year,
				  database_start_time.month, database_start_time.mday,
				  database_start_time.hour, database_start_time.minute,
				  (int) database_start_time.second);
		sprintf(rpws_dataset_time->scet_start_3, "%04d-%02d-%02d(%03d)T%02d",
				  database_start_time.year, database_start_time.month,
				  database_start_time.mday, database_start_time.yday,
				  database_start_time.hour);
		if((database_start_time.year < 1997)
			|| (database_start_time.year > 2037)) {
			fprintf(stderr, "Year out of range %s\n",
					  rpws_dataset_time->scet_start);
			return 1;				  /* error to shell */
		}

		utc2et_c(rpws_dataset_time->scet_start, &et);
		strcpy(rpws_dataset_time->plot_start, rpws_dataset_time->scet_start);
		sce2s_c(SpaceCraft_ID, et, 20, database_sclk_start);
		database_sclk_start_i = strtol(&database_sclk_start[2], NULL, 10);
	}
	if(fg_flag("dbase_sp")) {
		parsetime(fg_flagc("dbase_sp"),
					 &database_stop_time.year,
					 &database_stop_time.month,
					 &database_stop_time.mday,
					 &database_stop_time.yday,
					 &database_stop_time.hour,
					 &database_stop_time.minute, &database_stop_time.second);
		sprintf(rpws_dataset_time->scet_stop, "%04d-%03dT%02d:%02d:%02d",
				  database_stop_time.year,
				  database_stop_time.yday,
				  database_stop_time.hour,
				  database_stop_time.minute, (int) database_stop_time.second);
		strcpy(rpws_dataset_time->plot_stop, rpws_dataset_time->scet_stop);
		if((database_stop_time.year < 1997)
			|| (database_stop_time.year > 2037)) {
			fprintf(stderr, "Year out of range %s\n",
					  rpws_dataset_time->scet_stop);
			return 1;				  /* error to shell */
		}

		utc2et_c(rpws_dataset_time->scet_stop, &et);
		strcpy(rpws_dataset_time->plot_stop, rpws_dataset_time->scet_stop);
		sce2s_c(SpaceCraft_ID, et, 20, database_sclk_stop);
		database_sclk_stop_i = strtol(&database_sclk_stop[2], NULL, 10);
	}

	if(debug_flag & 0x8000) {
		fprintf(debug_file, "Using database start time %s\n",
				  rpws_dataset_time->scet_start);
		fprintf(debug_file, "Using database stop time %s\n",
				  rpws_dataset_time->scet_stop);
		fflush(debug_file);
	}

	if(!split_stim_flag) {
		sprintf(new_stim_filename_1, "%s/STIM.TXT",
				  w_directory[RPWS_ARCHIVE_EXTRAS]);
		stim_handle_1 =
			 new_stim_handle(stim_handle_1, new_stim_filename_1, 1,
								  Delimiter);

		sprintf(new_stim_filename_2, "%s/STIM.TAB",
				  w_directory[RPWS_ARCHIVE_DATA_ANCIL]);
		stim_handle_2 =
			 new_stim_handle(stim_handle_2, new_stim_filename_2, 2,
								  Delimiter);
	}

	rpws_label_stim = malloc(sizeof(struct RPWS_LABEL));
	if(rpws_label_stim) {
		initialize_label(rpws_label_stim, nProdVerId);
		strcpy(rpws_label_stim->filename, "STIM");	/* patch this up !!! */
		strcpy(rpws_label_stim->sclk_start, "1/2147483647:255");
		strcpy(rpws_label_stim->sclk_stop, "1/1000000000:000");
		strcpy(rpws_label_stim->scet_start, "2999-001T00:00:00.000");
		strcpy(rpws_label_stim->scet_start_2, "2999-01-01T00:00:00");
		strcpy(rpws_label_stim->scet_start_3, "2999-01-01T00:00:00");
		strcpy(rpws_label_stim->scet_stop, "1997-001T00:00:00.000");
		strcpy(rpws_label_stim->ephem_start, "9999999999.0");
		strcpy(rpws_label_stim->ephem_stop, "-9999999999.0");
		strcpy(rpws_label_stim->plot_start, "2999-001T00:00:00.000");
		strcpy(rpws_label_stim->plot_stop, "1997-001T00:00:00.000");

		rpws_label_stim->instrument = RPWS_LABEL_STIM;
		rpws_label_stim->pad_class = RPWS_LABEL_PAD_STIM;
		rpws_label_stim->span = 0;
		rpws_label_stim->duration = 0;
		rpws_label_stim->Product = Product[3];
		rpws_label_stim->record_size = 0;
		rpws_label_stim->record_count[0] = 0;
		rpws_label_stim->record_count[1] = 1;

		rpws_label_stim->dataset_size[0] = 0;
		rpws_label_stim->dataset_size[1] = 0;
		rpws_label_stim->sample_count = 80;
		/* dump_master_label(rpws_label_stim, 1, __LINE__); */

	}

	/********************************************
	 *		Reformat things here...		*
	 ********************************************/
	if(fg_flag("files") == '-') {
		main_file_list(write_flag, fg_flagc("files"), nProdVerId);
	} else {
		fprintf(debug_file, " main_file\n");
		fflush(debug_file);
		icnt = main_file(input, write_flag, nProdVerId);
		fclose(input);
	}

	if(debug_flag & 0x0440) {
		fprintf(debug_file, "Begining LBL label file creation\n");
		fflush(debug_file);
	}

	close_file_handle(rpws_label_head);
	close_file_handle(rpws_label_raw);
	open_file_count = 0;
	if(stim_handle_1) {
		fprintf(stim_handle_1,
				  "/*******************************************************/\r\n");
		fprintf(stim_handle_1,
				  "/****             END OF STIM RECORDS               ****/\r\n");
		fprintf(stim_handle_1,
				  "/*******************************************************/\r\n");
		fclose(stim_handle_1);
	}
	if(stim_handle_2) {
		fclose(stim_handle_2);
		write_stim_label();
	}

	build_master_label(rpws_dataset_time);
	master_label(rpws_dataset_time, "DATASET", w_directory);

	format_scet(rpws_wbr_time, 1);
	master_label(rpws_wbr_time, "WBR", w_directory);

	format_scet(rpws_wfr_time, 1);
	master_label(rpws_wfr_time, "WFR", w_directory);

	if(fg_flag("stsp"))
		dtemp = fabs(rpws_accumulate(0));
	else
		dtemp = rpws_accumulate(0);

	if(debug_flag & 0x0440) {
		fprintf(debug_file, "PDS label creation\n");
		fflush(debug_file);
	}

	master_cd_label(rpws_dataset_time,
						 rpws_wbr_time,
						 rpws_wfr_time,
						 w_directory, cd_index, cd_count, cd_format, dtemp);

	for(i = 0; i < 2; i++) {
		label_temp = rpws_label_head;
		while(label_temp) {
			/* fprintf(stdout, "%s/%d %d\n", __FILE__, __LINE__,
			   label_temp->instrument); */
			if(label_temp->instrument == i) {
				format_scet(label_temp, 1);
				master_detail(label_temp, w_directory);
			}
			label_temp = label_temp->link;
		}
	}
	write_label(rpws_label_head);
	write_label(rpws_label_raw);

	rpws_duplicate_scan(rpws_label_head);

	for(i = 0; i < 2; i++) {
		label_temp = rpws_label_head;
		while(label_temp) {
			/* fprintf(stdout, "%s/%d %d\n", __FILE__, __LINE__,
			   label_temp->instrument); */
			/* fprintf(stdout, "%s/%d %s\n", __FILE__, __LINE__,
			   label_temp->filename); */
			if(label_temp->instrument == i) {
				rpws_browse_detail(label_temp);
			}
			label_temp = label_temp->link;
		}
	}

	fprintf(debug_file, "Browse script path is: %s\n", sBrScriptDir);
	fflush(debug_file);

	rpws_browse((const char **) w_directory, body, sBrScriptDir);

	if(debug_flag & 0x0440) {
		fprintf(debug_file, "BROWSE post-processing script creation\n");
		fflush(debug_file);
	}
	rpws_browse_html(w_directory, RPWS_LABEL_WBR, debug_flag);
	rpws_browse_html(w_directory, RPWS_LABEL_WFR, debug_flag);
	rpws_master_browse_html(w_directory, debug_flag);

	fprintf(stdout, "WBR Stop Time %s\n", rpws_wbr_time->scet_stop);
	fprintf(stdout, "WFR Stop Time %s\n", rpws_wfr_time->scet_stop);
	fflush(stdout);
	return 0;
}
