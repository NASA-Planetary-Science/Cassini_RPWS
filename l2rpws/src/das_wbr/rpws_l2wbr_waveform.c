/*
        new10k.c        written 10/21/98 by TFA to process
                        Cassini RPWS 10 KHz WBR data for input to das
        Modifications:
                        Ex dipole antenna length changed from 8.66 meters
                        to 9.26 meters. TFA 12/4/98
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
			RFFT module replaces fft. TFA 09/20/00
			Add variable length FFT input, also add variable
			percent-overlap input. 
			Add option to toss data if MFR interferes. TFA 29-Mar-02d
			
			Made re-locatable and merged in with rest of Cassini SVN code,  
			   CWP 2016-08-17
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include <math.h>

#include <fg.h>

#include <rtiu.h>
#include <util.h>
#include <utilt.h>

#include <das2/das1.h>

#include "file_list.h"
#include "find_mfr.h"
#include "dec.h"

#define  MAXFILES 1024

static char *progname;
static int toss_mfr = 0;
static bool header_sent = false;

typedef struct pkt_entry{
	int nsamp;
	double deltat;
	int pkt_id;
} PktEntry;

bool process_data(double start, double stop, struct file_structure *dbase, 
		           int n_files);

void print_stream_header();
void print_nodata();
int print_packet_header(int nsamp, double deltatime);
void print_oom_exception();

int getPktId(PktEntry* id_list, int nsamp, double deltatime);

/* ************************************************************************* */
int main(int argc, char **argv)
{
	int num_files;
	int year, month, day, doy, hr, mn;
	double sec, start_sec, stop_sec;
	struct file_structure files[MAXFILES];
	const char* dataset = NULL;
	
	if(argc <= 2) {
		printf("Usage is: %s start stop [-dataset mpdb_file]\n", argv[0]);
		exit(0);
	}
	progname = argv[0];
	if(parsetime(argv[1], &year, &month, &day, &doy, &hr, &mn, &sec)) {
		fprintf(stderr, "%s: error parsing %s\n", argv[0], argv[1]);
		exit(1);
	}
	start_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	if(parsetime(argv[2], &year, &month, &day, &doy, &hr, &mn, &sec)) {
		fprintf(stderr, "%s: error parsing %s\n", argv[0], argv[2]);
		exit(1);
	}
	stop_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	if(argc >= 4) {
		argc--;
		argc--;
		fg_flags(argc, &argv[2]);
		if(!(dataset = fg_flagc("dataset"))) dataset = NULL;
		
		/* ugg, empty string is not a null string */
		if((dataset != NULL)&&(dataset[0] == '\0')) dataset = NULL;

		toss_mfr = fg_int("toss_mfr", 0);
	}

	fprintf(stderr, "Selected data set   is %s\n", dataset);

/* do some rudimentary checks */
	fprintf(stderr, "Selected toss_mfr is %d\n", toss_mfr);

	start_sec -= (4096*3.6e-5);

	make_dbase(dataset, start_sec, stop_sec, files, &num_files);
	if(num_files == 0) {
		print_nodata();
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		exit(1);
	}

	if(process_data(start_sec, stop_sec, files, num_files)) {
		if (!header_sent) print_stream_header();
		print_nodata();
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		exit(1);
	}
	return (0);
}

/* ************************************************************************* */

bool process_data(
	double start_sec, double stop_sec, struct file_structure *dbase, int n_files
){
	int epoch, suppress = 0;
	int inst_type, mp_len;
	int wbrdata, nsamp, nsamp_last=-1, pkt_id=-1;
	int i, idx, RTI, igain, agc, iant, icount, idx0;
	float fant,fgain,sample_period;
	int year, month, day, doy, hr, mn, mon, mday;
	int YEAR, MON, MDAY, DOY, HR, MN;
	int return_status, num_files;
	double secs;
	double Hanning[8192];
	double pwr, ant_length;
	float dc;
	float xreal[8192];
	float wsave[2 * 8192 + 15];
	char header[8] = { ":b0:300C" };
	float x_y_z[3];
	double deltat, gain;
	double cal_factor, bandw;
	double TWO_PI = (2.0 * 3.14159265358979324);
	double pkt_sec = 0, samp_sec;
	double t2000_epoch;
	double sec;
	float remainder, offset, new_time;
	float micro_offset = 0.0;
	int this_chunk = 0, last_chunk = -1;
	time_t pkt_sclk, pkt_epoc, start_sclk, stop_sclk;
	struct tm *pkt_event;
	struct event_time *evt_tim;
	struct event_clock evt_clk;
	struct bigbuf {
		struct RPWS_buffer big1;
		struct RPWS_buffer big2;
	};
	union {
		struct RPWS_buffer rbuffer;
		struct MP_buffer buffer;
		struct bigbuf banana;
	} m;
	struct MP_buffer mbuffer;

	int st_inx;
	bool no_data;
	int index, index2;
	float spec[2050], freq[2050];
	FILE *finput;
	double requested_sec;
	double MFR_time;
	int MFR_Ant;

	PktEntry id_list[99];

	for (index = 0; index < 99; index++) {
		id_list[index].nsamp=0;
	}

	year = 2000; month = 1; day = 1; doy = 1; hr = 0; mn = 0; sec = 0.0;
	t2000_epoch = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	no_data = true;
	if(stop_sec < dbase->st_sec)
		return (1);

	if(start_sec > (dbase + (n_files - 1))->sp_sec)
		return (1);

	for(index = 0; index < n_files; index++)
		if(start_sec < (dbase + index)->sp_sec)
			break;

	if(index == n_files)
		return (1);

	if(stop_sec < (dbase + index)->st_sec)
		return (1);

	st_inx = index;

	if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
		fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
		exit(-1);
	}
	else fprintf(stderr, "%s: opened %s\n", progname, (dbase + st_inx)->line);

	num_files = n_files;

	do {
		if((UTIL_getbuffer_MP(&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED) {
			fclose(finput);		  /* done with this file, time for next */
			st_inx++;
			if(st_inx < n_files) {
				if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
					fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
					exit(-1);
				}
				else fprintf(stderr, "%s: opened %s\n", progname, (dbase + st_inx)->line);
				if((UTIL_getbuffer_MP(&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED) {
					fclose(finput);
					fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
					exit(-1);
				}
			} else
				break;				  /* ran out of files, exit do loop */
		}
		inst_type = (m.buffer.packet.mpx.mini_packet[0] >> 4) & 0xf;
		if(inst_type == WBR) {
			mp_len = UTIL_MP_length(&m.buffer);
			RTI = (m.buffer.packet.mpp.mini_packet[2] & 0xff) |
				 ((m.buffer.packet.mpp.mini_packet[3] & 0xff) << 8);
			pkt_sclk = UTIL_event_time(&m.buffer, 0);
			epoch = (m.buffer.packet.cds_tag.epoch[0] << 24) |
				 (m.buffer.packet.cds_tag.epoch[1] << 16) |
				 (m.buffer.packet.cds_tag.epoch[2] << 8) | (m.buffer.packet.cds_tag.epoch[3] << 0);
			pkt_epoc = pkt_sclk + epoch;
/*                                                              */
/*       Newest SCET calculation scheme: 6-Jan-1999 TFA         */
/*                                                              */
			if(epoch) {				  /* JPL data has non-zero epoch */
				evt_clk.seconds = pkt_sclk;	/* SCLK seconds */
				evt_clk.fine = UTIL_extract_MP_RTI(&m.buffer) << 5;	/* SCLK fine */
				evt_tim = UTIL_event_scet(&m.buffer, evt_clk);
				pkt_event = UTIL_event_scet_tm(*evt_tim, 0);
				pkt_event->tm_yday++;	/* days after Jan. 1 */
				pkt_event->tm_mon++;	/* months since Jan */
				year = pkt_event->tm_year + 1900;
				mon = pkt_event->tm_mon;	/* month, 1...12 */
				mday = pkt_event->tm_mday;	/* day of month */
				doy = pkt_event->tm_yday;
				hr = pkt_event->tm_hour;
				mn = pkt_event->tm_min;
				sec = (double) pkt_event->tm_sec + (double) (evt_tim->milliseconds % 1000) / 1000.;
			} else {					  /* IOWA data has epoch=0 */

				pkt_event = gmtime(&pkt_epoc);
				pkt_event->tm_yday++;	/* gmtime returns days after Jan. 1 */
				pkt_event->tm_mon++;	/* months since Jan */
				year = pkt_event->tm_year + 1900;
				mon = pkt_event->tm_mon;	/* month, 1...12 */
				mday = pkt_event->tm_mday;	/* day of month */
				doy = pkt_event->tm_yday;
				hr = pkt_event->tm_hour;
				mn = pkt_event->tm_min;
				sec = (double) pkt_event->tm_sec +
					 ((double) (UTIL_extract_MP_RTI(&m.buffer) * 125)) / 1000.;
			}

			pkt_sec = ttime(&year, &mon, &mday, &doy, &hr, &mn, &sec);

			if(pkt_sec < stop_sec) {

/* Look for special SOI length */

				if(mp_len == 1444)
					mp_len = 1024 + 7;
/* Look for special RPXING mode. TFA 12-23-02 */
				iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
				if(((mbuffer.packet.mpp.mini_packet[5] & 0x80) != 0x80) && (mp_len != 0x0405) && (mp_len != 0x0407) &&	/* 1Ksamp */
					(mp_len != 0x0805) && (mp_len != 0x0807) &&	/* 2Ksamp */
					(mp_len != 0x0c05) && (mp_len != 0x0c07) &&	/* 3Ksamp */
					(mp_len != 0x1005) && (mp_len != 0x1007) &&	/* 4Ksamp */
					(mp_len != 0x1405) && (mp_len != 0x1407)) {	/* 5Ksamp */
					if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08) {
						mp_len = (mp_len & 0x3c00) | 0x07;	/* MSF bit set */
						if((mp_len & 0x3c00) > 0x1000)
							mp_len = 0x1007;
					} else {
						mp_len = (mp_len & 0x3c00) | 0x05;	/* MSF bit clear */
						if((mp_len & 0x3c00) > 0x1000)
							mp_len = 0x1005;
					}
				}
				if((mp_len == 0x0405) || (mp_len == 0x0407) ||	/* 1Ksamp */
					(mp_len == 0x0805) || (mp_len == 0x0807) ||	/* 2Ksamp */
					(mp_len == 0x0c05) || (mp_len == 0x0c07) ||	/* 3Ksamp */
					(mp_len == 0x1005) || (mp_len == 0x1007) ||	/* 4Ksamp */
					(mp_len == 0x1405) || (mp_len == 0x1407)) {	/* 5Ksamp */
					if(((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) && 
						((iant == 0) || (iant == 1) || (iant == 2) || (iant == 4)) &&	
							/* Ex or Bx bogus cals) or LP (bogus cals) or Ez */
						((m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x00)) {	/* 10 KHz ? */
						if(no_data) {
							memcpy(&mbuffer, &m.buffer, sizeof(mbuffer));
							no_data = false;
						} else {
							mp_len = UTIL_MP_length(&mbuffer);
							/* Look for special RPXING mode. TFA 12-23-02 */
							if(((mbuffer.packet.mpp.mini_packet[5] & 0x80) != 0x80) && (mp_len != 0x0405) && (mp_len != 0x0407) &&	/* 1Ksamp */
								(mp_len != 0x0805) && (mp_len != 0x0807) &&	/* 2Ksamp */
								(mp_len != 0x0c05) && (mp_len != 0x0c07) &&	/* 3Ksamp */
								(mp_len != 0x1005) && (mp_len != 0x1007) &&	/* 4Ksamp */
								(mp_len != 0x1405) && (mp_len != 0x1407)) {	/* 5Ksamp */
								if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08) {
									mp_len = (mp_len & 0x3c00) | 0x07;	/* MSF bit set */
									if((mp_len & 0x3c00) > 0x1000)
										mp_len = 0x1007;
								} else {
									mp_len = (mp_len & 0x3c00) | 0x05;	/* MSF bit clear */
									if((mp_len & 0x3c00) > 0x1000)
										mp_len = 0x1005;
								}
							}
							if((mp_len == 0x0405) || (mp_len == 0x0407) ||	/* 1Ksamp */
								(mp_len == 0x0805) || (mp_len == 0x0807) ||	/* 2Ksamp */
								(mp_len == 0x0c05) || (mp_len == 0x0c07) ||	/* 3Ksamp */
								(mp_len == 0x1005) || (mp_len == 0x1007) ||	/* 4Ksamp */
								(mp_len == 0x1405) || (mp_len == 0x1407)) {	/* 5Ksamp */
								if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
									mp_len = (mp_len & 0x1c00) | 0x07;	/* MSF bit set */
								else
									mp_len = (mp_len & 0x1c00) | 0x05;	/* MSF bit clear */
							}
							nsamp = (mp_len & 0x1f00);
							iant = mbuffer.packet.mpp.mini_packet[6] & 0x07;
							if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
								idx = 10;	/* MSF bit set */
							else
								idx = 8;	/* MSF bit clear */
							if((mbuffer.packet.mpp.mini_packet[5] & 0x80) == 0x80) {
								deltat = 4.5e-06;
								cal_factor = 6.43;	/* dBmax factor */
							} else {
								deltat = 36.e-06;
								cal_factor = 6.33;	/* dBmax factor */
							}
							if(iant == 0)
								ant_length = 9.26;	/* Ex dipole */
							else if((iant == 1) || (iant == 4))	/* Bx search coil */
								ant_length = 1.00;	/* bogus cals */
							else
								ant_length = 5.00;	/* Ez monopole */
							agc = mbuffer.packet.mpp.mini_packet[7];
							igain = 10 * (mbuffer.packet.mpp.mini_packet[5] & 0x07);
							gain = igain;
							gain = gain + cal_factor;
							gain = pow(10., gain / 20.);

							icount = 0;
							idx0 = idx;
							while((idx + nsamp) < (mp_len + 4)) {
								dc = 0.0;
								if((mp_len == 0x0405) || (mp_len == 0x0407)) {	/* 1Ksamp */
									for(i = idx; i < (idx + nsamp); i++) {
										wbrdata = mbuffer.packet.mpp.mini_packet[i];
										dc = dc + (float) wbrdata;
										xreal[i - idx] = (float) wbrdata;
									}
								} else {
									for(i = idx; i < (idx + nsamp); i++) {
										wbrdata = mbuffer.packet.mpp.mini_packet[i];
										dc = dc + (float) wbrdata;
										xreal[i - idx] = (float) wbrdata;
									}
								}

								dc = dc / (float) nsamp;

								suppress = 0;
								if(toss_mfr) {
									requested_sec = pkt_sec;
									return_status =
										 process_mfr(start_sec, stop_sec, requested_sec, dbase, num_files,
														 &MFR_time, &MFR_Ant);

									if(return_status == 2) {
										emitt(MFR_time, &YEAR, &MON, &MDAY, &DOY, &HR, &MN, &secs);
										fprintf(stderr, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f ",
												  YEAR, DOY, HR, MN, secs);
										emitt(requested_sec, &YEAR, &MON, &MDAY, &DOY, &HR, &MN, &secs);
										fprintf(stderr, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f ",
												  YEAR, DOY, HR, MN, secs);
										fprintf(stderr, " %2d ", MFR_Ant);
										fprintf(stderr, "***");
										fprintf(stderr, "  \n");
										if((MFR_Ant == 0) && (iant == 0))
											suppress = 1;
									}
								}

	/* I unindented this for readability reasons. Code nested this much
	 * should probably be re-written. */
	if(!suppress) {
		if (!header_sent) {
			print_stream_header();
			header_sent = true;
		}
		pkt_id = getPktId(&id_list[0], nsamp, deltat);
		printf(":%02d:", pkt_id);
		samp_sec = pkt_sec - t2000_epoch;
		fwrite(&samp_sec, sizeof(samp_sec), 1, stdout);
		fant=iant;
		fwrite(&fant, sizeof(fant), 1, stdout);
		fgain=igain;
		fwrite(&fgain, sizeof(fgain), 1, stdout);
		sample_period=deltat;
		fwrite(&sample_period, sizeof(sample_period), 1, stdout);
		for (i = 0; i < nsamp; i++) {
			fwrite(&xreal[i], sizeof(xreal[i]), 1, stdout);
		}
		fflush(stdout);
	}
								icount++;
								no_data = false;
								idx += nsamp;
							}			  /* while */
							memcpy(&mbuffer, &m.buffer, sizeof(mbuffer));
						}
					}					  /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
				}						  /* if ((mp_len == 0x805) || (mp_len == 0x807)) */
			}							  /* if (pkt_sec < stop_sec) */
		}								  /* if( inst_type == WBR ) */
	}
	while(pkt_sec < stop_sec);
	if(!no_data)
	{									  /* flush last mini-packet */
		mp_len = UTIL_MP_length(&mbuffer);
/* Look for special RPXING mode. TFA 12-23-02 */
		iant = mbuffer.packet.mpp.mini_packet[6] & 0x07;
		if(((mbuffer.packet.mpp.mini_packet[5] & 0x80) != 0x80) && (mp_len != 0x0405) && (mp_len != 0x0407) &&	/* 1Ksamp 
																																					 */
			(mp_len != 0x0805) && (mp_len != 0x0807) &&	/* 2Ksamp */
			(mp_len != 0x0c05) && (mp_len != 0x0c07) &&	/* 3Ksamp */
			(mp_len != 0x1005) && (mp_len != 0x1007) &&	/* 4Ksamp */
			(mp_len != 0x1405) && (mp_len != 0x1407)) {	/* 5Ksamp */
			
			if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08) {
				mp_len = (mp_len & 0x3c00) | 0x07;	/* MSF bit set */
				if((mp_len & 0x3c00) > 0x1000)
					mp_len = 0x1007;
			} else {
				mp_len = (mp_len & 0x3c00) | 0x05;	/* MSF bit clear */
				if((mp_len & 0x3c00) > 0x1000)
					mp_len = 0x1005;
			}
		}
		nsamp = (mp_len & 0x1f00);
		iant = mbuffer.packet.mpp.mini_packet[6] & 0x07;
		if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
			idx = 10;				  /* MSF bit set */
		else
			idx = 8;					  /* MSF bit clear */
		if((mbuffer.packet.mpp.mini_packet[5] & 0x80) == 0x80) {
			deltat = 4.5e-06;
			cal_factor = 6.43;	  /* dBmax factor */
		} else {
			deltat = 36.e-06;
			cal_factor = 6.33;	  /* dBmax factor */
		}
		if(iant == 0)
			ant_length = 9.26;	  /* Ex dipole */
		else if(iant == 1)		  /* Bx search coil */
			ant_length = 1.00;	  /* bogus cals */
		else
			ant_length = 5.00;	  /* Ez monopole */
		agc = mbuffer.packet.mpp.mini_packet[7];
		igain = 10 * (mbuffer.packet.mpp.mini_packet[5] & 0x07);
		gain = igain;
		gain = gain + cal_factor;
		gain = pow(10., gain / 20.);

		icount = 0;
		idx0 = idx;
		while((idx + nsamp) < (mp_len + 4)) {
			dc = 0.0;
			if((mp_len == 0x0405) || (mp_len == 0x0407)) {	/* 1Ksamp */
				for(i = idx; i < (idx + nsamp); i++) {
					wbrdata = mbuffer.packet.mpp.mini_packet[i];
					dc = dc + (float) wbrdata;
					xreal[i - idx] = (float) wbrdata;
				}
			} else {
				for(i = idx; i < (idx + nsamp); i++) {
					wbrdata = mbuffer.packet.mpp.mini_packet[i];
					dc = dc + (float) wbrdata;
					xreal[i - idx] = (float) wbrdata;
				}
			}

			dc = dc / (float) nsamp;

			if (!header_sent) {
				print_stream_header();
				header_sent = true;
			}
			pkt_id = getPktId(&id_list[0], nsamp, deltat);
			printf(":%02d:", pkt_id);
			samp_sec = pkt_sec - t2000_epoch;
			fwrite(&samp_sec, sizeof(samp_sec), 1, stdout);
			fant=iant;
			fwrite(&fant, sizeof(fant), 1, stdout);
			fgain=igain;
			fwrite(&fgain, sizeof(fgain), 1, stdout);
			sample_period=deltat;
			fwrite(&sample_period, sizeof(sample_period), 1, stdout);
			for (i = 0; i < nsamp; i++) {
				fwrite(&xreal[i], sizeof(xreal[i]), 1, stdout);
			}

			fflush(stdout);
			icount++;
			no_data = false;
			idx++;
		}								  /* while */
	}

	fclose(finput);
	return (no_data);
}

/*****************************************************************************/
/* Handle endian_ness of output binary data */
#ifdef HOST_IS_LSB_FIRST
#define DAS_TYPE "little_endian_real"
#else
#define DAS_TYPE "sun_real"
#endif

void print_stream_header() {
	char sBuf[4096] = { '\0' };
	sprintf(sBuf,
"<stream dataset_id=\"wbr_join\">\n"
"  <properties\n"
"    String:xLabel=\"SCET (UTC)\"\n"
"    String:summary=\"Cassini 10 kHz Uncalibrated WBR\"\n"
"    Datum:xTagWidth=\"3.6e-5 s\"\n"
"  />\n"
"</stream>\n");

	printf("[00]%06zd%s", strlen(sBuf), sBuf);

	sprintf(sBuf,
"<packet>\n"
"	<qdataset id=\"wbr_join\" rank=\"3\">\n"
"		<values join=\"wbr\"/>\n"\
"		<properties>\n"
"			<property name=\"JOIN_0\" type=\"rank0dataset\" value=\"0.0\"/>\n"
"		</properties>\n"
"	</qdataset>\n"
"</packet>\n");

	printf("[01]%06zd%s", strlen(sBuf), sBuf);
}

static int _ds_count = 1;

int getPktId(PktEntry* id_list, int nsamp, double deltat) {
	
	int i = 0;
	int pkt_id;

	while (id_list[i].nsamp != 0) {
		if (id_list[i].nsamp == nsamp && id_list[i].deltat == deltat) {
			return id_list[i].pkt_id;
		}
		i++;
	}
	
	pkt_id = print_packet_header(nsamp, deltat);
	id_list[i].nsamp = nsamp;
	id_list[i].deltat = deltat;
	id_list[i].pkt_id = pkt_id;

	return pkt_id;
}

int print_packet_header(int nsamp, double deltatime) {

	char sBuf[4096] = "\0";
	int ds_id = _ds_count++;
	int id1 = ds_id*2;
	int id2 = id1+1;
	float offset;
	int i;

	fprintf(stderr, "print_packet_header(%d,%f)\n", nsamp, deltatime);

	sprintf(sBuf,
"<packet>\n"
"	<qdataset id=\"offsets_%02d\" rank=\"1\">\n"
"		<properties>\n"
"			<property name=\"UNITS\" type=\"units\" value=\"s\"/>\n"
"			<property name=\"NAME\" type=\"String\" value=\"sample_offset\"/>\n"
"		</properties>\n"
"		<values encoding=\"float\" length=\"%d\"/>\n"
"	</qdataset>\n"
"</packet>\n", ds_id, nsamp);

	printf("[%02d]%06zd%s", id1, strlen(sBuf), sBuf);

	printf(":%02d:", id1);

	for (int i = 0; i < nsamp; i++) {
		offset = i * deltatime;
		fwrite(&offset, sizeof(float), 1, stdout);
	}

	sprintf(sBuf,
"<packet>\n"
"	<qdataset id=\"time_%02d\" rank=\"1\">\n"
"		<properties>\n"
"			<property name=\"UNITS\" type=\"units\" value=\"t2000\"/>\n"
"			<property name=\"MONOTONIC\" type=\"Boolean\" value=\"true\"/>\n"
"			<property name=\"NAME\" type=\"String\" value=\"time\"/>\n"
"        <property name=\"LABEL\" type=\"String\" value=\"SCET %%{RANGE}\"/>\n"
"		</properties>\n"
"		<values encoding=\"double\" length=\"\"/>\n"
"	</qdataset>\n"
"	<qdataset id=\"ant_%02d\" rank=\"1\">\n"
"		<properties>\n"
"			<property name=\"NAME\" type=\"String\" value=\"antenna\"/>\n"
"        <property name=\"LABEL\" type=\"String\" value=\"Raw ADC Value\"/>\n"
"		</properties>\n"
"		<values encoding=\"float\" length=\"\"/>\n"
"	</qdataset>\n"
"	<qdataset id=\"gain_%02d\" rank=\"1\">\n"
"		<properties>\n"
"			<property name=\"NAME\" type=\"String\" value=\"gain\"/>\n"
"		</properties>\n"
"		<values encoding=\"float\" length=\"\"/>\n"
"	</qdataset>\n"
"	<qdataset id=\"sample_period_%02d\" rank=\"1\">\n"
"		<properties>\n"
"			<property name=\"UNITS\" type=\"units\" value=\"s\"/>\n"
"			<property name=\"NAME\" type=\"String\" value=\"sample_period\"/>\n"
"		</properties>\n"
"		<values encoding=\"float\" length=\"\"/>\n"
"	</qdataset>\n"
"	<qdataset id=\"wbr\" joinId=\"wbr_join\" rank=\"2\">\n"
"		<properties>\n"
"			<property name=\"DEPEND_0\" type=\"qdataset\"\n"
"				value=\"time_%02d\"/>\n"
"			<property name=\"DEPEND_1\" type=\"qdataset\"\n"
"				value=\"offsets_%02d\"/>\n"
"			<property name=\"PLANE_0\" type=\"qdataset\"\n"
"				value=\"ant_%02d\"/>\n"
"			<property name=\"PLANE_1\" type=\"qdataset\"\n"
"				value=\"gain_%02d\"/>\n"
"			<property name=\"PLANE_2\" type=\"qdataset\"\n"
"				value=\"sample_period_%02d\"/>\n"
"		</properties>\n"
"		<values encoding=\"float\" length=\"%d\"/>\n"
"	</qdataset>\n"
"</packet>", ds_id, ds_id, ds_id, ds_id, ds_id, ds_id, ds_id, ds_id, ds_id,
		nsamp);

	printf("[%02d]%06zd%s", id2, strlen(sBuf), sBuf);

	return id2;
}

void print_nodata() {
	char* msg =
"<exception type=\"NoDataInInterval\"\n"
"  message=\"No data in the requested interval\" />\n";
	fprintf(stdout, "[xx]%06zd%s", strlen(msg), msg);
}

void print_oom_exception() {
	char * msg =
"<exception type=\"InternalServerError\"\n"
"  message=\"memory allocation failed\" />\n";

	fprintf(stdout, "[xx]%06d%s", strlen(msg), msg);
}
