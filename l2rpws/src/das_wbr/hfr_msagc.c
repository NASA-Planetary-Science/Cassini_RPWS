/*
        hfr_msagc.c     written 10/20/00 by TFA to process
                        Cassini RPWS HFR millisecond for input to
			das plot
        Modifications:
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
								
        SVN Incorporation:  Added into the Cassini SVN by CWP on
		                  2016-08-18, original location was:
								/home/tfa/cassini/hfr/web/hfr_msagc.c
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include <das2/das1.h>

#include <rtiu.h>
#include <util.h>
#include <utilt.h>

#include "file_list.h"
#include "dec.h"
#include "hfr_cal.h"

#define  MAXFILES 1024

/* ************************************************************************* */
/* Compiled in file locations */

#define _QDEF(x) #x
#define QDEF(x) _QDEF(x)

#ifndef RPWS_HFR_CALDIR
#error Compiled in default RPWS HFR Cal file directory missing
#endif

static char *progname;

int process_data(
	double start_sec, double stop_sec, struct file_structure *dbase,
	int n_files
);


/* ************************************************************************* */
int main(int argc, char **argv)
{
	int num_files, i, j, return_status;
	int year, month, day, doy, hr, mn;
	double sec, start_sec, stop_sec;
	int YEAR, MON, MDAY, DOY, HR, MN;
	double secs;
	struct file_structure files[MAXFILES];
	const char *dataset = NULL;
	
	if(argc <= 2) {
		printf("Usage is: %s start stop [mpdb_file]\n", argv[0]);
		return 13;
	}
	progname = argv[0];
	if(parsetime(argv[1], &year, &month, &day, &doy, &hr, &mn, &sec)) {
		fprintf(stderr, "%s: error parsing %s\n", argv[0], argv[1]);
		return 13;
	}
	start_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	if(parsetime(argv[2], &year, &month, &day, &doy, &hr, &mn, &sec)) {
		fprintf(stderr, "%s: error parsing %s\n", argv[0], argv[2]);
		return 13;
	}
	stop_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	if(argc == 4){
		dataset = argv[3];
		fprintf(stderr, "Mini-packet index set to: %s\n", dataset);
	}

	if( strlen( QDEF(RPWS_HFR_CALDIR) ) == 0){
		fprintf(stderr, "Programmer error, compiled in HFR cal directory missing\n");
		return 5;
	}
	read_A1HF_calibration_file( QDEF(RPWS_HFR_CALDIR) "/A1HF1.DAT", 1);
	read_A1HF_calibration_file( QDEF(RPWS_HFR_CALDIR) "/A1HF2.DAT", 0);

	make_dbase(dataset, start_sec, stop_sec, files, &num_files);
	if(num_files == 0) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
	}

	return_status = process_data(start_sec, stop_sec, files, num_files);
	if(return_status == 1) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
	}

	return 0;
}

/* ************************************************************************* */
int process_data(
	double start_sec, double stop_sec, struct file_structure *dbase, int n_files
){
	static bool first = true, nomore = false;
	static int epoch, error_flag;
	static int inst_type, mp_len, nsamp;
	static int i, idx, RTI, igain, agc, band, iant;
	static int year, month, day, doy, hr, mn, mon, mday;
	static int YEAR, MON, MDAY, DOY, HR, MN;
	double MS_time, MS_freq, MS_agc, ant_length;
	static double secs;
	static double pkt_sec = 0;
	static double sample_time, deltat, DELTAT = 0.0;
	static double sec;
	float spec[2];
	static time_t pkt_sclk, pkt_epoc, start_sclk, stop_sclk;
	static struct tm *pkt_event;
	static struct event_time *evt_tim;
	static struct event_clock evt_clk;
	struct bigbuf {
		struct RPWS_buffer big1;
		struct RPWS_buffer big2;
	};
	static union {
		struct RPWS_buffer rbuffer;
		struct MP_buffer buffer;
		struct bigbuf banana;
	} m;
	static int st_inx;
	bool no_data;
	static int index, index2, freq_step;
	static float freq, hfrcal[16384];
	static FILE *finput;
	static double x, temp, dBagc, A1, A2, A3;
	static double dBV0 = -73.7;

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
	no_data = true;

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
			} else {
				return (no_data);	  /* ran out of files, exit do loop */
			}
		}
		inst_type = (m.buffer.packet.mpx.mini_packet[0] >> 4) & 0xf;
		if(inst_type == HFR) {
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
			if(pkt_sec >= stop_sec)
				return (no_data);
			else if(((pkt_sec + 16.384) >= start_sec)) {
				error_flag = 0;
				if(mp_len != (256 * (int) m.buffer.packet.mpx.mini_packet[5] +
								  (int) m.buffer.packet.mpx.mini_packet[6] + 4))
					error_flag = 1;  /* correct length ? */
				else if(m.buffer.packet.mpx.mini_packet[4] != 0x80)
					error_flag = 1;  /* unsegmented ? */
				else if(m.buffer.packet.mpx.mini_packet[mp_len + 2] != 0x5a)
					error_flag = 1;  /* HFR eof byte ? */
				else if(m.buffer.packet.mpx.mini_packet[7] != 0x6F)
					error_flag = 1;  /* ms mode ? */
				if(!error_flag) {
					nsamp = (m.buffer.packet.mpx.mini_packet[9] & 0x07);
					nsamp = 256 * (1 << nsamp);
					if(nsamp != (mp_len - 10))
						error_flag = 1;	/* length is goofy */
					else {
						deltat = (m.buffer.packet.mpx.mini_packet[9] & 0x38) / 8;
						deltat = 0.5 * pow(2., deltat) / 1000.0;
						DELTAT = (double) (nsamp - 1) * deltat;
					}
				}

				if(!error_flag) {
					if((m.buffer.packet.mpx.mini_packet[9] & 0xC0) == 0x00) {
						iant = 2;
						ant_length = 5.0;	/* EZ */
					} else if((m.buffer.packet.mpx.mini_packet[9] & 0xC0) == 0x40) {
						iant = 0;
						ant_length = 5.0;	/* EX+ */
					} else if((m.buffer.packet.mpx.mini_packet[9] & 0xC0) == 0x80) {
						iant = 1;
						ant_length = 5.0;	/* EX- */
					} else if((m.buffer.packet.mpx.mini_packet[9] & 0xC0) == 0xC0) {
						iant = 3;
						ant_length = 9.26;	/* EXdipole */
					}
					if((m.buffer.packet.mpx.mini_packet[11] & 0x01) == 0x00)
						band = 0;
					else if((m.buffer.packet.mpx.mini_packet[11] & 0x01) == 0x01)
						band = 1;
					freq_step = m.buffer.packet.mpx.mini_packet[10];
					if(band == 0) {
						freq = 25.0 * (float) freq_step;
						A1 = a1HF1[iant][freq_step];
					} else {
						freq = 100.0 * (float) freq_step + 25.0;
						A1 = a1HF2[iant][2 * freq_step];
					}
					if(iant != 2) {  /* Ex ? */
						A2 = 97.8;
						A3 = -0.32;
					} else {			  /* Ez */
						A2 = 96.8;
						A3 = -0.23;
					}
					for(i = 0; i < nsamp; i++) {
						sample_time = pkt_sec + ((double) i) * deltat;
						if((sample_time >= start_sec) && (sample_time < stop_sec)) {
							x = m.buffer.packet.mpx.mini_packet[i + 12];
							x = (x - A3) / A2;
							temp = 40.0 * log10(pow(10., x) + 1.0);
							dBagc = A1 - temp;
							hfrcal[i] = -dBagc + dBV0;
							MS_time = (sample_time - start_sec);
							MS_freq = (double) freq;
							MS_agc = pow(10., (double) hfrcal[i] / 10.) / (ant_length * ant_length);

/* first item to be output is seconds after start time */

							spec[0] = MS_time;
							spec[1] = MS_agc;

							if(!fwrite(spec, sizeof(spec), 1, stdout)) {
								fprintf(stderr, "%s:  error writing output\n", progname);
								exit(-1);
							}

							no_data = false;
						}
					}
				}
			}							  /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
		}								  /* if( inst_type == HFR ) */
	}
	while(stop_sec > pkt_sec);
	return no_data;
}
