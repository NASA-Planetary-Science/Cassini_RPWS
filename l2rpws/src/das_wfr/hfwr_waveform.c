/*
        hfwr.c          written 10/29/98 by TFA to process
                        Cassini RPWS Hi-Band WFR data for input to das
        Modifications:
                        Ex dipole antenna length changed from 8.66 meters
                        to 9.26 meters. TFA 12/4/98
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
								
								Integrated with the rest of the Cassini SVN source
								code by CWP, 2016-08-16
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include <math.h>

#include <rtiu.h>
#include <util.h>
#include <utilt.h>

#include <das2/das1.h>

#include "dec.h"
#include "file_list.h"

#define  MAXFILES 1024

#define UNCALIBRATED 0
#define CALIBRATED 1

static char *progname;
static int chan_select;
static char* channel[] = { "Ex", "Ez", "Bx", "By", "Bz" };
static bool header_sent = false;

int process_data(double start, double stop, struct file_structure *dbase, int n_files);
void print_header(int);
void print_nodata();

/* ************************************************************************* */
int main(int argc, char **argv)
{
	int num_files;
	int year, month, day, doy, hr, mn;
	double sec, start_sec, stop_sec;
	struct file_structure files[MAXFILES];
	const char* dataset = NULL;

	if(argc <= 3) {
		fprintf(stderr, "Usage is: %s channel_num start stop [minipacket_database_file]\n", argv[0]);
		return 7;
	}
	progname = argv[0];

	/*
	chan_select = atol(argv[1]);
	if((chan_select < 0) || (chan_select > 4)) {
		fprintf(stderr, "%s: bad channel # %s\n", argv[0], argv[1]);
		return 8;
	}
	*/
	if (strcmp(argv[1],"Ex")) {
		chan_select = 0;
	}
	else if (strcmp(argv[1],"Ez")) {
		chan_select = 1;
	}
	else if (strcmp(argv[1],"Bx")) {
		chan_select = 2;
	}
	else if (strcmp(argv[1],"By")) {
		chan_select = 3;
	}
	else if (strcmp(argv[1],"Bz")) {
		chan_select = 4;
	}
	else {
		fprintf(stderr, "%s: bad channel name %s\n", argv[0], argv[1]);
		return 8;
	}

	if(parsetime(argv[2], &year, &month, &day, &doy, &hr, &mn, &sec)) {
		fprintf(stderr, "%s: error parsing %s\n", argv[0], argv[2]);
		return 9;
	}
	start_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	if(parsetime(argv[3], &year, &month, &day, &doy, &hr, &mn, &sec)) {
		fprintf(stderr, "%s: error parsing %s\n", argv[0], argv[3]);
		return 10;
	}
	stop_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	if(argc == 5){
		dataset = argv[4];
		fprintf(stderr, "Mini-packet index set to: %s\n", dataset);
	}

	/* subtract 2048 times the samples period from the start time
 	 * in case the start time is in the middle of a capture.
 	 */
	start_sec -= (2048*140e-6);
	make_dbase(dataset, start_sec, stop_sec, files, &num_files);
	if(num_files == 0) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		print_header(UNCALIBRATED);
		print_nodata();
		/* This condition is not an error */
		return 0;
	}

	if(process_data(start_sec, stop_sec, files, num_files)) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		if (!header_sent) print_header(UNCALIBRATED);
		print_nodata();
		/* This condition is not an error */
		return 0;
	}
	return (0);
}

/* ************************************************************************** */
int process_data(
	double start_sec, double stop_sec, struct file_structure* dbase, int n_files
){
	int epoch;
	int inst_type, mp_len, chan, band;
	int wfrdata, nsamp;
	int i, j, idx, RTI, igain, agc, iant;
	int year, month, day, doy, hr, mn, mon, mday;
	double Hanning[16384];
	double dc, pwr, ant_length;
	double xreal[2048];
	double sample_period, deltaf, gain;
	double cal_factor, bandw;
	double TWO_PI = (2.0 * 3.14159265358979324);
	double pkt_sec = 0, samp_sec;
	double t2000_epoch;
	double sec;
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
	int st_inx; 
	bool no_data;
	int index, index2;
	float offset;
	FILE *finput;

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
		exit(18);
	}
	else
		fprintf(stderr, "INFO: Reading %s\n", (dbase + st_inx)->line);

	year = 2000; month = 1; day = 1; doy = 1; hr = 0; mn = 0; sec = 0.0;
	t2000_epoch = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	do {
		if((UTIL_getbuffer_MP(&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED) {
			fclose(finput);		  /* done with this file, time for next */
			st_inx++;
			if(st_inx < n_files) {
				if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
					fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
					exit(19);
				}
				if((UTIL_getbuffer_MP(&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED) {
					fclose(finput);
					fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
					exit(20);
				}
			} else
				break;				  /* ran out of files, exit do loop */
		}
		inst_type = (m.buffer.packet.mpx.mini_packet[0] >> 4) & 0xf;
		chan = (m.buffer.packet.mpx.mini_packet[7] >> 3) & 0x7;
		if(chan == 7)				  /* Combined Mode ? */
			chan = m.buffer.packet.mpx.mini_packet[7] & 0x7;
		band = (m.buffer.packet.mpx.mini_packet[5] >> 7) & 0x1;
		if((inst_type == WFR) && (chan == chan_select) && band) {
			mp_len = UTIL_MP_length(&m.buffer);
			if((m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08) {
				if(mp_len > 0x1007)
					mp_len = 0x1007; /* truncate, MSF bit set */
			} else {
				if(mp_len > 0x1005)
					mp_len = 0x1005; /* truncate, MSF bit clear */
			}
			RTI =(m.buffer.packet.mpp.mini_packet[2] & 0xff) | 
				  ((m.buffer.packet.mpp.mini_packet[3] & 0xff) << 8);
			
			pkt_sclk = UTIL_event_time(&m.buffer, 0);
			
			epoch = (m.buffer.packet.cds_tag.epoch[0] << 24) | 
					  (m.buffer.packet.cds_tag.epoch[1] << 16) | 
					  (m.buffer.packet.cds_tag.epoch[2] << 8)  |
					  (m.buffer.packet.cds_tag.epoch[3] << 0);
			
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
				sec = (double) pkt_event->tm_sec + 
						(double) (evt_tim->milliseconds % 1000) / 1000.;
			}
			else {					  /* IOWA data has epoch=0 */

				pkt_event = gmtime(&pkt_epoc);
				pkt_event->tm_yday++;	/* gmtime returns days after Jan. 1 */
				pkt_event->tm_mon++;	/* months since Jan */
				year = pkt_event->tm_year + 1900;
				mon = pkt_event->tm_mon;	/* month, 1...12 */
				mday = pkt_event->tm_mday;	/* day of month */
				doy = pkt_event->tm_yday;
				hr = pkt_event->tm_hour;
				mn = pkt_event->tm_min;
				sec =
					 (double) pkt_event->tm_sec +
					 ((double) (UTIL_extract_MP_RTI(&m.buffer) * 125)) / 1000.;
			}

			pkt_sec = ttime(&year, &mon, &mday, &doy, &hr, &mn, &sec);

			if(pkt_sec < stop_sec) {
				if((mp_len == 0x0405) || (mp_len == 0x0407) ||	/* 512samp */
					(mp_len == 0x0805) || (mp_len == 0x0807) ||	/* 1Ksamp */
					(mp_len == 0x1005) || (mp_len == 0x1007)) {	/* 2Ksamp */
					iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
					
					if(((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) && (
							((chan_select == 0) && ((iant & 1) == 1)) ||	/* Ex */
							((chan_select == 1) && ((iant & 2) == 2)) ||	/* Ez */
							((chan_select == 2) && ((iant & 4) == 0)) ||	/* Bx */
							(chan_select == 3) ||	/* By */
							(chan_select == 4) /* Bz */
						)) 
						{	
						
						if((m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
							idx = 10;  /* MSF bit set */
						else
							idx = 8;	  /* MSF bit clear */
						if((m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80) {
							sample_period = 140.e-06;	/* Hi-Band */
							cal_factor = 9.45;	/* dBmax factor */
						} else {
							sample_period = 10e-3; /* Lo-Band */
							cal_factor = 9.45;	/* dBmax factor ?? */
						}
						if(chan_select == 0)
							ant_length = 9.26;	/* Ex dipole */
						else if(chan_select == 1)
							ant_length = 5.00;	/* Ez monopole */
						else if(chan_select == 2) {
							ant_length = 1. / 24.0;	/* mag. pre-amp gain */
							ant_length = ant_length * 0.1474;	/* Bx volts per nT @ 1 KHz */
						} else if(chan_select == 3) {
							ant_length = 1. / 24.0;	/* mag. pre-amp gain */
							ant_length = ant_length * 0.1467;	/* By volts per nT @ 1 KHz */
						} else if(chan_select == 4) {
							ant_length = 1. / 24.0;	/* mag. pre-amp gain */
							ant_length = ant_length * 0.1466;	/* Bz volts per nT @ 1 KHz */
						}

						if(chan_select == 0)
							igain = 10 * (m.buffer.packet.mpp.mini_packet[5] & 0x03);
						else if(chan_select == 1)
							igain = 10 * ((m.buffer.packet.mpp.mini_packet[5] >> 2) & 0x03);
						else
							igain = 10 * ((m.buffer.packet.mpp.mini_packet[5] >> 4) & 0x03);
						gain = igain;
						gain = gain + cal_factor;
						gain = pow(10., gain / 20.);

						nsamp = 512;
						if((mp_len == 0x0405) || (mp_len == 0x0407))
							nsamp = 512;	/* 512samp */
						else if((mp_len == 0x0805) || (mp_len == 0x0807))
							nsamp = 1024;	/* 1Ksamp */
						else if((mp_len == 0x1005) || (mp_len == 0x1007))
							nsamp = 2048;	/* 2Ksamp */
						dc = 0;
						for(i = idx; i < (idx + 2 * nsamp); (i = i + 2)) {
							wfrdata =  m.buffer.packet.mpp.mini_packet[i] | 
							           ((m.buffer.packet.mpp.mini_packet[i + 1] & 0x0f)
															  << 8);
							dc = dc + (double) wfrdata;
							xreal[(i - idx) / 2] = (double) wfrdata;
						}
/*
	Try to detect loss of a segment by looking for zeroes. 15-Oct-2007. TFA
*/
/*
	lost segments won't be output
*/


						dc = dc / (float) nsamp;

/* first item to be output is seconds after start time */
						if (!header_sent) {
							print_header(UNCALIBRATED);
							header_sent = true;
						}
						for (i = 0; i < nsamp; i++) {
							samp_sec = (pkt_sec - t2000_epoch) + (i*sample_period);
							fwrite(":01:", 4, 1, stdout);
							fwrite(&samp_sec, sizeof(double), 1, stdout);
							fwrite(&xreal[i], sizeof(float), 1, stdout);
						}
						no_data = false;
					}					  /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
				}						  /* if ((mp_len == 0x805) || (mp_len == 0x807)) etc. */
			}							  /* if (pkt_sec < stop_sec) */
		}								  /* if( inst_type == WFR ) */
	}
	while(pkt_sec < stop_sec);
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

void print_header(int calibrated) {
	char sBuf[4096] = { '\0' };
	char* cal_text[] = { "Uncalibrated", "Calibrated" };
	char* units[] = { "", "V/m" };
	sprintf(sBuf,
"<stream version=\"2.2\">\n"
"  <properties\n"
"    String:xLabel=\"SCET (UTC)\"\n"
"    String:summary=\"%s Cassini WFR 2.6 kHz %s channel waveforms\"\n"
"    Datum:xTagWidth=\"140e-6 s\"\n"
"  />\n"
"</stream>\n", cal_text[calibrated], channel[chan_select]);

	printf("[00]%06zd%s", strlen(sBuf), sBuf);

	sprintf(sBuf,
"<packet>\n"
"  <x type=\"" DAS_TYPE "8\" units=\"t2000\" />\n"
"  <y type=\"" DAS_TYPE "4\" units=\"\" >\n"
"    <properties\n"
"      String:yLabel=\"%s Data Number\"\n"
"    />\n"
"  </y>\n"
"</packet>\n", channel[chan_select]);

	printf("[01]%06zd%s", strlen(sBuf), sBuf);

}

void print_nodata() {
	char* msg =
"<exception type=\"NoDataInInterval\"\n"
"  message=\"No data in the requested interval\" />\n";
	fprintf(stdout, "[xx]%06zd%s", strlen(msg), msg);
}

