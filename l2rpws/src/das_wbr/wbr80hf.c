/*
        wbr80k.c        written 10/21/98 by TFA to process
                        Cassini RPWS 80 KHz WBR data for input to das
        Modifications:
                        Ex dipole antenna length changed from 8.66 meters
                        to 9.26 meters. TFA 12/4/98
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
			WBR/HF mode added. Calibrations bogus. TFA 10/20/99
			
			Incorporated into Cassini RPWS SVN repository: CWP 2016-08-18
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include <rtiu.h>
#include <util.h>
#include <utilt.h>

#include <das2/das1.h>

#include "dec.h"
#include "file_list.h"

#define  MAXFILES 1024

static char *progname = NULL;

int fft(double *xreal, double *ximag, int n);

int window(double *windo, int lenw, int weight);

bool process_data(double start, double stop, struct file_structure *dbase, int n_files);

/* ************************************************************************* */

int main(int argc, char **argv)
{
	int num_files;
	int year, month, day, doy, hr, mn;
	double sec, start_sec, stop_sec;
	struct file_structure files[MAXFILES];
	const char *dataset = NULL;

	if(argc <= 2) {
		printf("Usage is: %s start stop [mpdb_file]\n", argv[0]);
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

	dataset = NULL;
	if(argc == 4) {
		dataset = argv[3];
		fprintf(stderr, "Mini-packet index set to: %s\n", dataset);
	}

	make_dbase(dataset, start_sec, stop_sec, files, &num_files);
	if(num_files == 0) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		return 0;  /* not an error */
	}

	if(process_data(start_sec, stop_sec, files, num_files)) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		return 0;  /* not an error */
	}
	return 0;
}

/* ************************************************************************* */

bool process_data(
	double start_sec, double stop_sec, struct file_structure *dbase, int n_files
){

	int epoch;
	int inst_type, mp_len;
	int wbrdata, nsamp, nlast = 2048;
	int i, idx, RTI, igain, agc, iant;
	int year, month, day, doy, hr, mn, mon, mday;
	double Hanning[16384];
	double dc, pwr, ant_length;
	double xreal[2048], ximag[2048];
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
	float offset, spec[1026], freq[1026], x_y_z[3], freq_offset;
	FILE *finput;
	unsigned char header[8] = { ":b0:300C" };

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

	window(Hanning, 2048, 3);

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
				if((mp_len == 0x0805) || (mp_len == 0x0807) ||	/* 2Ksamp */
					(mp_len == 0x0C05) || (mp_len == 0x0C07) ||	/* 3Ksamp */
					(mp_len == 0x1005) || (mp_len == 0x1007) ||	/* 4Ksamp */
					(mp_len == 0x1405) || (mp_len == 0x1407)) {	/* 5Ksamp */
					iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
					if(((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) && ((iant == 0) || (iant == 2) || (iant == 3)) &&	/* Ex 
																																							   or 
																																							   Ez 
																																							   or 
																																							   HF 
																																							 */
						((m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80)) {	/* 80 KHz ? */
						if((m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
							idx = 10;  /* MSF bit set */
						else
							idx = 8;	  /* MSF bit clear */
						if((m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80) {
							fs = 1. / 4.5e-06;	/* Hi-Band */
							cal_factor = 6.43;	/* dBmax factor */
						} else {
							fs = 1. / 36.e-06;	/* Lo-Band */
							cal_factor = 6.33;	/* dBmax factor */
						}
						if(iant == 0)
							ant_length = 9.26;	/* Ex dipole */
						else
							ant_length = 5.00;	/* Ez monopole */
						agc = m.buffer.packet.mpp.mini_packet[7];
						bandw = 1.5 * fs / 2048;	/* Hanning ENBW */
						igain = 10 * (m.buffer.packet.mpp.mini_packet[5] & 0x07);
						gain = igain;
						gain = gain + cal_factor;
						gain = pow(10., gain / 20.);

						nsamp = 0;
						dc = 0;
						for(i = idx; i < (idx + 2048); i++) {
							wbrdata = m.buffer.packet.mpp.mini_packet[i];
							dc = dc + (float) wbrdata;
							xreal[nsamp] = (double) wbrdata;
							ximag[nsamp] = 0.0;
							nsamp++;
						}
						dc = dc / (float) nsamp;
						deltaf = fs / (double) nsamp;

						if(nsamp != nlast) {
							window(Hanning, nsamp, 3);
							nlast = nsamp;
						}
						for(i = 0; i < nsamp; i++) {	/* window data */
							xreal[i] = (xreal[i] - dc) * Hanning[i] / (127.5 * gain);
							xreal[i] = xreal[i] / ant_length;	/* volts per meter */
						}

						fft(xreal, ximag, nsamp);
						freq_offset = 0.0;
						if(iant == 3) {
							if((m.buffer.packet.mpp.mini_packet[8] & 1) == 1)
								freq_offset = -62.5 + 25.0 * (float) (m.buffer.packet.mpp.mini_packet[8]);
							else
								freq_offset =
									 -62.5 + 4025.0 + 50.0 * (float) (m.buffer.packet.mpp.mini_packet[8]);
						}
						for(i = 0; i <= (nsamp / 2); i++) {
							spec[i + 1] = (xreal[i] * xreal[i] + ximag[i] * ximag[i]) / bandw;
							freq[i + 1] = freq_offset + (deltaf * (double) i) / 1000.0;
						}

						spec[0] = (pkt_sec - start_sec);
						x_y_z[0] = spec[0];	/* seconds since start */

						fwrite(header, 8, 1, stdout);
						for(i = 0; i <= (nsamp / 2); i++) {
							x_y_z[1] = freq[i + 1];
							x_y_z[2] = spec[i + 1];
							fwrite(x_y_z, sizeof(x_y_z), 1, stdout);
						}

						no_data = false;
					}					  /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
				}						  /* if ((mp_len == 0x805) || (mp_len == 0x807)) etc. */
			}							  /* if (pkt_sec < stop_sec) */
		}								  /* if( inst_type == WBR ) */
	}
	while(pkt_sec < stop_sec);
	fclose(finput);
	return no_data;
}

