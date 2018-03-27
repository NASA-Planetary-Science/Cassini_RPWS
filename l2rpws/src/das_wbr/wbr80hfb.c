/*
        wbr80k.c        written 10/21/98 by TFA to process
                        Cassini RPWS 80 KHz WBR data for input to das
        Modifications:
                        Ex dipole antenna length changed from 8.66 meters
                        to 9.26 meters. TFA 12/4/98
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
			WBR/HF mode added. Calibrations bogus. TFA 10/20/99
			Replace fft module with RFFT (CLAMS). TFA 09/20/00
			Add time stretch capability. TFA 06/03/04
			
			Incorporated into Cassini RPWS SVN repository: CWP 2016-08-24
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include <fftpack.h>

#include <das2/das1.h>

#include <fg.h>

#include <rtiu.h>   /* Cassini specific */
#include <util.h>
#include <utilt.h>

#include "dec.h"
#include "file_list.h"
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

static double ant_length;

static int looper_period = 0;

static int wbr_duration = 0;

static int desired_gap = 0;

int window(double *windo, int lenw, int weight);

bool process_data(
	double start, double stop, struct file_structure *dbase, int n_files
);

int process_agc(
	double start, double stop, double sec, struct file_structure *dbase,
	int n_files, double *MS_time, double *MS_freq, double *MS_agc
);


/* ************************************************************************* */
int main(int argc, char **argv)
{
	int num_files;
	int year, month, day, doy, hr, mn;
	double sec, start_sec, stop_sec;
	struct file_structure files[MAXFILES];
	const char *dataset = NULL;

	if(argc <= 2) {
		printf("Usage is: %s start stop [-dataset mpdb_file]\n", argv[0]);
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
	
	if(argc >= 4) {
		argc--;
		argc--;
		fg_flags(argc, &argv[2]);
		if(!(dataset = fg_flagc("dataset"))) dataset = NULL;
		
		/* ugg, empty string is not a null string */
		if((dataset != NULL)&&(dataset[0] == '\0')) dataset = NULL;		
		
		looper_period = fg_int("looper_period", 0);
		wbr_duration = fg_int("wbr_duration", 0);
		desired_gap = fg_int("desired_gap", 0);
	}

	fprintf(stderr, "Selected data set   is %s\n", dataset);

/* do some rudimentary checks */
	if(looper_period < 0)                            looper_period = 0;
	if(wbr_duration < 0)                             looper_period = 0;
	if(wbr_duration > looper_period)                 looper_period = 0;
	if(desired_gap < 0)                              looper_period = 0;
	if(desired_gap > (looper_period - wbr_duration)) looper_period = 0;
	
	fprintf(stderr, "Selected looper period is %d\n", looper_period);
	fprintf(stderr, "Selected wbr duration is %d\n", wbr_duration);
	fprintf(stderr, "Selected desired gap is %d\n", desired_gap);

	if( strlen( QDEF(RPWS_HFR_CALDIR) ) == 0){
		fprintf(stderr, "Programmer error, compiled in HFR cal directory missing\n");
		return 5;
	}
	read_A1HF_calibration_file( QDEF(RPWS_HFR_CALDIR) "/A1HF1.DAT", 1);
	read_A1HF_calibration_file( QDEF(RPWS_HFR_CALDIR) "/A1HF2.DAT", 0);

	make_dbase(dataset, start_sec, stop_sec, files, &num_files);
	if(num_files == 0) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		fprintf(stderr, "num_files = 0\n");
		
		/* This is not an error and should not return non-zero, but this is 
		   the only way we have to flag to the batch plotters that there is nothing
			to plot here */
		return 127;
	}

	if(!process_data(start_sec, stop_sec, files, num_files)) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		fprintf(stderr, "process_data found no data\n");
	}
	
	/* Again not an error, but no other way to communicate that plotting shouldn't 
	   occur */
	return 127;
}

/* ************************************************************************* */
bool process_data(
	double start_sec, double stop_sec, struct file_structure *dbase, int n_files
){
	int epoch, num_files, return_status;
	int inst_type, mp_len;
	int wbrdata, nsamp, nlast = 2048;
	int i, idx, RTI, igain, agc, iant;
	int year, month, day, doy, hr, mn, mon, mday;
	int YEAR, MON, MDAY, DOY, HR, MN;
	double secs;
	float remainder, offset, new_time;
	float micro_offset = 0.0;
	int this_chunk = 0, last_chunk = -1;
	double Hanning[16384];
	double pwr;
	float dc;
	float xreal[2048];
	float wsave[2 * 2048 + 15];
	double fs, deltaf, gain;
	double cal_factor, bandw;
	double TWO_PI = (2.0 * 3.14159265358979324);
	double pkt_sec = 0;
	double sec, requested_sec;
	double MS_time, MS_freq, MS_agc, HFWBR_freq;
	int ilo, ihi;
	double psd_sum, psd_avg, hfr_psd, hfr_gain;
	int st_inx, no_data;
	int index, index2;
	float spec[1026], freq[1026], x_y_z[3], freq_offset;
	FILE *finput;
	unsigned char header[8] = { ":b0:300C" };
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

	no_data = true;
	if(stop_sec < dbase->st_sec) {
		fprintf(stderr, "stop_sec < dbase->start_sec\n");
		return false;
	}

	if(start_sec > (dbase + (n_files - 1))->sp_sec) {
		fprintf(stderr, "start_sec < (last dbase->stop_sec\n");
		return false;
	}
	for(index = 0; index < n_files; index++)
		if(start_sec < (dbase + index)->sp_sec)
			break;

	if(index == n_files) {
		fprintf(stderr, "index = n_files\n");
		return false;
	}
	if(stop_sec < (dbase + index)->st_sec) {
		fprintf(stderr, "stop_sec < dbase->start_sec\n");
		return false;
	}
	st_inx = index;

	if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
		fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
		exit(17);
	}

	num_files = n_files;
	window(Hanning, 2048, 3);
	nsamp = 2048;					  /* default length */
	rffti_(&nsamp, wsave);		  /* initialize work array */

	do {
		
		if((UTIL_getbuffer_MP(&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED) {
			fclose(finput);		  /* done with this file, time for next */
			st_inx++;
			if(st_inx < n_files) {
				if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
					fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
					exit(17);
				}
				if((UTIL_getbuffer_MP(&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED) {
					fclose(finput);
					fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
					exit(17);
				}
			} else
				break;				  /* ran out of files, exit do loop */
		}
		
		inst_type = (m.buffer.packet.mpx.mini_packet[0] >> 4) & 0xf;
		
		/* Only care about WBR Data */
		if(inst_type != WBR) continue;
			
		
		mp_len = UTIL_MP_length(&m.buffer);
		RTI = (m.buffer.packet.mpp.mini_packet[2] & 0xff) | ((m.buffer.packet.mpp.mini_packet[3] & 0xff)<<8);
		pkt_sclk = UTIL_event_time(&m.buffer, 0);
		
		epoch = (m.buffer.packet.cds_tag.epoch[0] << 24) |
			 (m.buffer.packet.cds_tag.epoch[1] << 16) |
			 (m.buffer.packet.cds_tag.epoch[2] << 8) | (m.buffer.packet.cds_tag.epoch[3] << 0);
		
		pkt_epoc = pkt_sclk + epoch;
		
		/*                                                        */
		/*	Newest SCET calculation scheme: 6-Jan-1999 TFA         */
		/*                                                        */
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
		
		
		/* Only care about data in the proper time range */
		if(pkt_sec >= stop_sec) continue;
		
		
		/* Only care about packets that are the proper length */
		if((mp_len != 0x0805) && (mp_len != 0x0807) &&	/* 2Ksamp */
			(mp_len != 0x0C05) && (mp_len != 0x0C07) &&	/* 3Ksamp */
			(mp_len != 0x1005) && (mp_len != 0x1007) &&	/* 4Ksamp */
			(mp_len != 0x1405) && (mp_len != 0x1407)) 	/* 5Ksamp */
			continue;
		
					
		iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
					
		if(((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) && 
			((iant == 0) || (iant == 2) || (iant == 3)) &&	/* Ex or Ez or  HF */
			((m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80)) {	
					
			/* 80 KHz ? */
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
			else if(iant == 2)
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
				xreal[nsamp] = (float) wbrdata;
				nsamp++;
			}
			dc = dc / (float) nsamp;
			deltaf = fs / (double) nsamp;

			if(nsamp != nlast) {
				window(Hanning, nsamp, 3);
				rffti_(&nsamp, wsave);	/* initialize work array */
				nlast = nsamp;
			}
			for(i = 0; i < nsamp; i++) {	/* window data */
				xreal[i] = (xreal[i] - dc) * Hanning[i] / (127.5 * gain);
			}

			rfftf_(&nsamp, xreal, wsave);
			xreal[0] = xreal[0] / (float) nsamp;
			for(i = 0; i < nsamp; i++)
				xreal[i] = xreal[i] / (float) (nsamp / 2);
			xreal[nsamp - 1] = xreal[nsamp - 1] / 2.0;

			freq_offset = 0.0;
			if(iant == 3) {
				if((m.buffer.packet.mpp.mini_packet[8] & 1) == 1)
					freq_offset = -62.5 + 25.0 * (float) (m.buffer.packet.mpp.mini_packet[8]);
				else
					freq_offset =
						 -62.5 + 4025.0 + 50.0 * (float) (m.buffer.packet.mpp.mini_packet[8]);
				HFWBR_freq = freq_offset + 62.5;
			}

			spec[1] = xreal[0] * xreal[0] / bandw;
			for(i = 1; i < (nsamp / 2); i++)
				spec[i + 1] = (xreal[2 * i - 1] * xreal[2 * i - 1] +
									xreal[2 * i] * xreal[2 * i]) / bandw;
			spec[nsamp / 2] = xreal[nsamp - 1] * xreal[nsamp - 1] / bandw;

			for(i = 0; i <= (nsamp / 2); i++) {
				freq[i + 1] = freq_offset + (deltaf * (double) i) / 1000.0;
			}

			spec[0] = (pkt_sec - start_sec);

			/* Fake the time to spread the spectra across the plot. TFA 06/03/04 */
			
			if(looper_period != 0) {
				remainder = fmod((double) spec[0], (double) looper_period);
				offset = spec[0] - remainder;

				/* If this is the first sweep in this looper_period interval,
				   save the offset from the start of the interval, plot this
				   sweep at the true time, and plot the rest following it. */
				
				this_chunk = (int) (offset + 0.5) / looper_period;
				if(this_chunk != last_chunk) {
					last_chunk = this_chunk;
					micro_offset = remainder;
				}
				remainder = remainder - micro_offset;
				new_time = remainder * (float) (looper_period - desired_gap) /
					 (float) wbr_duration;
				spec[0] = offset + new_time + micro_offset;	/* spread them out */
			}

			x_y_z[0] = spec[0];	/* seconds since start */

			requested_sec = pkt_sec + (double) nsamp / (2. * fs);	/* take middle of snapshot */
			return_status =
				 process_agc(start_sec, stop_sec, requested_sec, dbase, num_files, &MS_time,
								 &MS_freq, &MS_agc);
			/*
			if (return_status != 2){
				emitt (MS_time, &YEAR, &MON, &MDAY, &DOY, &HR, &MN, &secs );
				fprintf (stderr, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f ",
				        YEAR, DOY, HR, MN, secs);
				emitt (requested_sec, &YEAR, &MON, &MDAY, &DOY, &HR, &MN, &secs );
				fprintf (stderr, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f ",
				        YEAR, DOY, HR, MN, secs);
				fprintf (stderr, " %8.0f %8.2f ", MS_freq, MS_agc);
				fprintf (stderr, "***");
				fprintf (stderr, "  \n");
			}
			*/
					
			if(return_status == 1) {
				fprintf(stderr, "No data coverage of the requested time interval.\n");
				fprintf(stderr, "hfr agc return status = 1\n");
				exit(17);
			} 
			
			
			if(((return_status == 2) && (MS_freq == HFWBR_freq)) ||
			   ( (return_status == 3) && (MS_freq == HFWBR_freq) &&
			     (abs(MS_time - requested_sec) < 1.0)  )
			  ){
					
				/*
				emitt (MS_time, &YEAR, &MON, &MDAY, &DOY, &HR, &MN, &secs );
				fprintf (stderr, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f ",
                   YEAR, DOY, HR, MN, secs);
				emitt (requested_sec, &YEAR, &MON, &MDAY, &DOY, &HR, &MN, &secs );
				fprintf (stderr, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f ",
                   YEAR, DOY, HR, MN, secs);
				fprintf (stderr, " %8.0f %8.2f ", MS_freq, MS_agc);
				fprintf (stderr, "<<<");
				fprintf (stderr, "  \n");
				*/
								
				/* Calculate gain factor using HFR measurement */
				ilo = 50000. / deltaf;	/* passband starts at 50 KHz */
				ihi = 75000. / deltaf;	/* passband stops at 75 KHz */
				psd_sum = 0.0;
				for(i = ilo; i < ihi; i++)
					psd_sum = psd_sum + spec[i + 1];
				psd_avg = psd_sum / (double) (ihi - ilo);	/* average V^2/Hz */
				hfr_psd = pow(10., (MS_agc / 10.));	/* true V^2/Hz */
				hfr_gain = hfr_psd / psd_avg;
				hfr_gain = hfr_gain / (ant_length * ant_length);	/* convert to V^2/m^2/Hz */

				/*
				fprintf (stderr,"WBR psd: %10.3e   HFR gain: %10.3e    ant_len: %10.3f\n",
				        psd_avg, hfr_gain, ant_length);
				*/
						
				/* Adjust WBR calibrations */
				fwrite(header, 8, 1, stdout);
	
				for(i = 0; i <= (nsamp / 2); i++) {
					x_y_z[1] = freq[i + 1];
					x_y_z[2] = spec[i + 1] * hfr_gain;
					fwrite(x_y_z, sizeof(x_y_z), 1, stdout);
				}
	
				no_data = false;
			}		
					
		}	  /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */	
			
	}
	while(pkt_sec < stop_sec);
	fclose(finput);
	return !no_data;
}


/* ************************************************************************* */

int process_agc(
	double start_sec, double stop_sec, double requested_sec, 
	struct file_structure *dbase, int n_files, double *MS_time, double *MS_freq,
	double *MS_agc
){
	static bool first = true, nomore = false;
	static int epoch, error_flag;
	static int inst_type, mp_len, nsamp;
	static int i, idx, RTI, igain, agc, band, iant;
	static int year, month, day, doy, hr, mn, mon, mday;
	static int YEAR, MON, MDAY, DOY, HR, MN;
	static double secs;
	static double pkt_sec = 0;
	static double sample_time, deltat, DELTAT = 0.0;
	static double sec;
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

	if(first) {
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
			exit(17);
		}
		first = false;
		no_data = true;
		goto read_first;
	} else
		goto not_first;

 read_first:
	if(nomore)
		return (1);
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
				nomore = true;
				return (1);			  /* ran out of files, exit do loop */
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
			
not_first:
			if((requested_sec >= pkt_sec)) {
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
						deltat = (int) (m.buffer.packet.mpx.mini_packet[9] & 0x38) / 8;
						deltat = 0.5 * pow(2., deltat) / 1000.0;
						DELTAT = (double) (nsamp - 1) * deltat;
					}
				}

				if((!error_flag) && (requested_sec <= (pkt_sec + DELTAT))) {	/* data ok ? */
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
						if((requested_sec >= sample_time) &&
							(requested_sec < (sample_time + 2. * deltat))) {
							x = m.buffer.packet.mpx.mini_packet[i + 12];
							x = (x - A3) / A2;
							temp = 40.0 * log10(pow(10., x) + 1.0);
							dBagc = A1 - temp;
							hfrcal[i] = -dBagc + dBV0;
							*MS_time = sample_time;
							*MS_freq = (double) freq;
							*MS_agc = (double) hfrcal[i];
							no_data = false;
							return (2);
						}
					}
				}
			}							  /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
		}								  /* if( inst_type == HFR ) */
	}
	while(requested_sec > pkt_sec);
	return (3);
}
