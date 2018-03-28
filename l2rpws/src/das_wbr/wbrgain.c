/*
        wbrgain.c       written 10/20/00 by TFA to process
                        Cassini RPWS WBR gain data for input to das
        Modifications:
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
			RFFT module replaces fft. TFA 09/20/00
			
			Merged in with rest of Cassini SVN code,  CWP 2016-08-17
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <rtiu.h>
#include <util.h>
#include <utilt.h>
#include <fg.h>
#include <time.h>
#include <math.h>

#include <das2/das1.h>

#include "dec.h"
#include "file_list.h"

#define  MAXFILES 1024

static char *progname;
static int fft_length = 2048;
static int fft_overlap = 0;
static int looper_period = 0;
static int wbr_duration = 0;
static int desired_gap = 0;

void rffti(int *n, float *wsave);

void rfftf(int *n, float *r, float *wsave);

int window(double *windo, int lenw, int weight);

bool process_data(double start, double stop, struct file_structure *dbase, int n_files);

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
				
		fft_length = fg_int("fft_length", 2048);
		fft_overlap = fg_int("fft_overlap", 0);
		looper_period = fg_int("looper_period", 0);
		wbr_duration = fg_int("wbr_duration", 0);
		desired_gap = fg_int("desired_gap", 0);
	}

	fprintf(stderr, "Selected data set is    '%s'\n", dataset);
	fprintf(stderr, "Selected fft length is  '%d'\n", fft_length);
	fprintf(stderr, "Selected fft overlap is '%d'\n", fft_overlap);

/* do some rudimentary checks */
	if(looper_period < 0)
		looper_period = 0;
	if(wbr_duration < 0)
		looper_period = 0;
	if(wbr_duration > looper_period)
		looper_period = 0;
	if(desired_gap < 0)
		looper_period = 0;
	if(desired_gap > (looper_period - wbr_duration))
		looper_period = 0;
	fprintf(stderr, "Selected looper period is %d\n", looper_period);
	fprintf(stderr, "Selected wbr duration is %d\n", wbr_duration);
	fprintf(stderr, "Selected desired gap is %d\n", desired_gap);

	make_dbase(dataset, start_sec, stop_sec, files, &num_files);
	if(num_files == 0) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		exit(1);
	}

	if(process_data(start_sec, stop_sec, files, num_files)) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		exit(1);
	}
	return (0);
}

/* ************************************************************************* */
bool process_data(double start_sec, double stop_sec, struct file_structure *dbase, int n_files)
{
	int epoch;
	int inst_type, mp_len;
	int wbrdata, nsamp, nlast = 2048;
	int i, idx, RTI, igain, agc, iant;
	int year, month, day, doy, hr, mn, mon, mday;
	double Hanning[16384];
	double pwr, ant_length;
	float dc;
	float xreal[2048];
	float wsave[2 * 2048 + 15];
	double fs, deltaf, gain;
	double cal_factor, bandw;
	double TWO_PI = (2.0 * 3.14159265358979324);
	double pkt_sec = 0;
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
	float offset, spec[2];
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
		exit(-1);
	}

	do {
		if((UTIL_getbuffer_MP(&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED) {
			fclose(finput);		  /* done with this file, time for next */
			st_inx++;
			if(st_inx < n_files) {
				if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
					fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
					exit(-1);
				}
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
				if((mp_len == 0x0405) || (mp_len == 0x0407) ||	/* 1Ksamp */
					(mp_len == 0x0805) || (mp_len == 0x0807) ||	/* 2Ksamp */
					(mp_len == 0x0c05) || (mp_len == 0x0c07) ||	/* 3Ksamp */
					(mp_len == 0x1005) || (mp_len == 0x1007) ||	/* 4Ksamp */
					(mp_len == 0x1405) || (mp_len == 0x1407)) {	/* 5Ksamp */
					iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
					if((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) {
						igain = 10 * (m.buffer.packet.mpp.mini_packet[5] & 0x07);
						gain = igain;

/* first item to be output is seconds after start time */

						spec[0] = (pkt_sec - start_sec);
						spec[1] = gain;
						if(!fwrite(spec, sizeof(spec), 1, stdout)) {
							fprintf(stderr, "%s:  error writing output\n", progname);
							exit(-1);
						}
						no_data = false;
					}					  /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
				}						  /* if ((mp_len == 0x805) || (mp_len == 0x807)) */
			}							  /* if (pkt_sec < stop_sec) */
		}								  /* if( inst_type == WBR ) */
	}
	while(pkt_sec < stop_sec);
	fclose(finput);
	return (no_data);
}
