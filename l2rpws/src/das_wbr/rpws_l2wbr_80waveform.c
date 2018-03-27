/*
        new80k.c        written 10/21/98 by TFA to process
                        Cassini RPWS 80 KHz WBR data for input to das
        Modifications:
                        Ex dipole antenna length changed from 8.66 meters
                        to 9.26 meters. TFA 12/4/98
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
			RFFT module replaces fft. TFA 09/20/00
			Add variable length FFT input, also add variable
			percent-overlap input. 
			Add option to toss data if MFR interferes. TFA 29-Mar-02

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
#include <fftpack.h>

#include <rtiu.h>  /* RPWS stuff */
#include <util.h>
#include <utilt.h>


#include <das2/das1.h>

#include "file_list.h"
#include "find_mfr.h"
#include "dec.h"

#define  MAXFILES 1024

static char *progname;
static int toss_mfr = 0;

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
		
		fft_length = fg_int("fft_length", 2048);
		fft_overlap = fg_int("fft_overlap", 0);
		looper_period = fg_int("looper_period", 0);
		wbr_duration = fg_int("wbr_duration", 0);
		desired_gap = fg_int("desired_gap", 0);
		toss_mfr = fg_int("toss_mfr", 0);
	}

	fprintf(stderr, "Selected data set   is %s\n", dataset);
	fprintf(stderr, "Selected fft length is %d\n", fft_length);
	fprintf(stderr, "Selected fft overlap  is %d\n", fft_overlap);

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
	fprintf(stderr, "Selected toss_mfr is %d\n", toss_mfr);

	make_dbase(dataset, start_sec, stop_sec, files, &num_files);
	if(num_files == 0) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
		return 0;
	}

	if(process_data(start_sec, stop_sec, files, num_files)) {
		fprintf(stderr, "No data coverage of the requested time interval.\n");
	}
	return 0;
}

/* ************************************************************************* */

bool process_data(
	double start_sec, double stop_sec, struct file_structure *dbase, int n_files
){
	int epoch, suppress = 0;
	int inst_type, mp_len, nFFT, FF_count;
	int wbrdata, nsamp, nlast = 2048;
	int i, idx, RTI, last_RTI, igain, agc, iant, icount, idx0;
	int year, month, day, doy, hr, mn, mon, mday;
	int YEAR, MON, MDAY, DOY, HR, MN;
	int return_status, num_files;
	double secs;
	double Hanning[4096];
	double pwr, ant_length;
	float dc;
	float xreal[4096];
	float wsave[2 * 4096 + 15];
	char header[8] = { ":b0:300C" };
	float x_y_z[3];
	double deltat, fs, deltaf, gain;
	double cal_factor, bandw;
	double TWO_PI = (2.0 * 3.14159265358979324);
	double pkt_sec = 0;
	double last_sec = 0;
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
	int modulus, if_index;
/*
	These are the modulus numbers used by MFR2. The interference
	lines in the WBR when at 70 dB gain are directly related to these
	modulus numbers. Equation of fundamental interfering frequency is
	F = 6MHz / (mfr2mods[((RTI-1)&127)/4], where RTI if the 16-bit
	RTI given in the WBR mini-packet.
*/
	int mfr2mods[32] = {
		366, 364, 363, 362, 360, 359, 357, 356,
		354, 352, 350, 348, 346, 344, 341, 339,
		336, 333, 330, 327, 324, 321, 317, 314,
		310, 306, 302, 297, 293, 288, 284, 279
	};
	double f_interfere;

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
		exit(17);
	}

	num_files = n_files;
	window(Hanning, fft_length, 3);
	nsamp = fft_length;			  /* user-specified length */
	nlast = nsamp;
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
/* Look for special B6 length of 1536 */

				if(mp_len == 1543)
					mp_len = 1024 + 7;
/* Look for special BE length of 1450 */

				if(mp_len == 1457)
					mp_len = 1024 + 7;
/* Look for special SOI length of 1444 */

				if(mp_len == 1444)
					mp_len = 1024 + 7;

/* Look for special SOI mode with variable lengths. TFA 06-24-02 */
				iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
				if(((mbuffer.packet.mpp.mini_packet[5] & 0x80) == 0x80) && (mp_len != 0x0405) && (mp_len != 0x0407) &&	/* 1Ksamp 
																																							 */
					(mp_len != 0x0805) && (mp_len != 0x0807) &&	/* 2Ksamp */
					(mp_len != 0x0c05) && (mp_len != 0x0c07) &&	/* 3Ksamp */
					(mp_len != 0x1005) && (mp_len != 0x1007) &&	/* 4Ksamp */
					(mp_len != 0x1405) && (mp_len != 0x1407)) {	/* 5Ksamp */
					if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
						mp_len = (mp_len & 0x1c00) | 0x07;	/* MSF bit set */
					else
						mp_len = (mp_len & 0x1c00) | 0x05;	/* MSF bit clear */
				}

				if((mp_len == 0x0405) || (mp_len == 0x0407) ||	/* 1Ksamp */
					(mp_len == 0x0805) || (mp_len == 0x0807) ||	/* 2Ksamp */
					(mp_len == 0x0c05) || (mp_len == 0x0c07) ||	/* 3Ksamp */
					(mp_len == 0x1005) || (mp_len == 0x1007) ||	/* 4Ksamp */
					(mp_len == 0x1405) || (mp_len == 0x1407)) {	/* 5Ksamp */
					iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
					if(((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) && ((iant == 0) || (iant == 2)) &&	/* Ex 
																																		   or 
																																		   Ez 
																																		 */
						((m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80)) {	/* 80 KHz ? */
						if(no_data) {
							last_sec = pkt_sec;
							last_RTI = RTI;
							memcpy(&mbuffer, &m.buffer, sizeof(mbuffer));
							no_data = false;
						} else {
							mp_len = UTIL_MP_length(&mbuffer);
							if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
								mp_len = (mp_len & 0x1c00) | 0x07;	/* MSF bit set */
							else
								mp_len = (mp_len & 0x1c00) | 0x05;	/* MSF bit clear */

							if((mp_len & 0x1f00) < fft_length)
								nsamp = (mp_len & 0x1f00);
							else
								nsamp = fft_length;	/* user selection if possible */
							iant = mbuffer.packet.mpp.mini_packet[6] & 0x07;
							if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
								idx = 10;	/* MSF bit set */
							else
								idx = 8;	/* MSF bit clear */
							if((mbuffer.packet.mpp.mini_packet[5] & 0x80) == 0x80) {
								deltat = 4.5e-06;
								fs = 1. / 4.5e-06;	/* Hi-Band */
								cal_factor = 6.43;	/* dBmax factor */
							} else {
								deltat = 36.e-06;
								fs = 1. / 36.e-06;	/* Lo-Band */
								cal_factor = 6.33;	/* dBmax factor */
							}
							if(iant == 0)
								ant_length = 9.26;	/* Ex dipole */
							else
								ant_length = 5.00;	/* Ez monopole */
							agc = mbuffer.packet.mpp.mini_packet[7];
							bandw = 1.5 * fs / nsamp;	/* Hanning ENBW */
							deltaf = fs / nsamp;
							igain = 10 * (mbuffer.packet.mpp.mini_packet[5] & 0x07);
							gain = igain;
							gain = gain + cal_factor;
							gain = pow(10., gain / 20.);

/* Attempt to do multiple FFT's for this mini-packet */

/*
	       nFFT = (mp_len & 0x1F00)/nsamp;
*/
							if(fft_overlap > 0)
								nFFT = (mp_len & 0x1F00) / (nsamp - nsamp * fft_overlap / 100);	/* this
																														   many */
							else
								nFFT = (mp_len & 0x1F00) / nsamp;
							icount = 0;
							idx0 = idx;
							while((idx + nsamp) < (mp_len + 4)) {
								dc = 0.0;
								FF_count = 0;
								if((mp_len == 0x0405) || (mp_len == 0x0407)) {	/* 1Ksamp */
									for(i = idx; i < (idx + nsamp); i++) {
										wbrdata = mbuffer.packet.mpp.mini_packet[i];
										if(wbrdata == 255)
											FF_count++;	/* clipped? */
										dc = dc + (float) wbrdata;
										xreal[i - idx] = (float) wbrdata;
									}
								} else {
									for(i = idx; i < (idx + nsamp); i++) {
										wbrdata = mbuffer.packet.mpp.mini_packet[i];
										if(wbrdata == 255)
											FF_count++;	/* clipped? */
										dc = dc + (float) wbrdata;
										xreal[i - idx] = (float) wbrdata;
									}
								}

								dc = dc / (float) nsamp;

								if(nsamp != nlast) {
									window(Hanning, nsamp, 3);
									rffti_(&nsamp, wsave);
									nlast = nsamp;
								}
								for(i = 0; i < nsamp; i++) {	/* window data */
									xreal[i] = (xreal[i] - dc) * Hanning[i] / (127.5 * gain);
									xreal[i] = xreal[i] / ant_length;	/* volts per meter */
								}

								rfftf_(&nsamp, xreal, wsave);
								xreal[0] = xreal[0] / (float) nsamp;
								for(i = 1; i < nsamp; i++)
									xreal[i] = xreal[i] / (float) (nsamp / 2);
								xreal[nsamp - 1] = xreal[nsamp - 1] / 2.0;

								spec[1] = xreal[0] * xreal[0] / bandw;
								spec[(nsamp / 2) + 1] = (xreal[nsamp - 1] * xreal[nsamp - 1]) / bandw;
								for(i = 1; i < (nsamp / 2); i++)
									spec[i + 1] = (xreal[2 * i - 1] * xreal[2 * i - 1] +
														xreal[2 * i] * xreal[2 * i]) / bandw;


/*
	If 80-KHz and 70 dB gain and EX or EZ, mask MFR interference lines.
*/
								if((igain == 70) && (iant != 3) &&
									((mbuffer.packet.mpp.mini_packet[5] & 0x80) == 0x80)) {
									modulus = mfr2mods[(int) (((last_RTI - 1) & 0x7F) / 4)];
									f_interfere = 6.e6 / (float) modulus;
									if_index = 0.5 + f_interfere / deltaf;
									for(i = 1; i < 3; i++) {
										spec[i * if_index] = spec[i * if_index - 1];	/* below */
										spec[i * if_index + 2] = spec[i * if_index + 3];	/* above */
										spec[i * if_index + 1] = 0.5 * (spec[i * if_index] + spec[i * if_index + 2]);	/* avg 
																																				 */
									}
									for(i = 3; i < 6; i++) {
										spec[i * if_index - 1] = spec[i * if_index - 2];	/* below */
										spec[i * if_index] = spec[i * if_index - 1];	/* below */
										spec[i * if_index + 3] = spec[i * if_index + 4];	/* above */
										spec[i * if_index + 2] = spec[i * if_index + 3];	/* above */
										spec[i * if_index + 1] = 0.5 * (spec[i * if_index] + spec[i * if_index + 2]);	/* avg 
																																				 */
									}
								}


								for(i = 1; i <= (nsamp / 2); i++) {
									freq[i + 1] = deltaf * (double) i / 1000.0;
								}

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
/*
               if ( return_status == 1)
               {
                 fprintf (stderr, "No data coverage of the requested time interval.\n");
                 fprintf (stderr, "mfr antenna return status = 1\n");
                 exit (1);
               } 
*/
								}
/*
	Look for clipped data. Evident when switching from dust to WBR.
*/
								if((igain == 70) && (iant == 0) && (FF_count > 8) &&
									((mbuffer.packet.mpp.mini_packet[5] & 0x80) == 0x80)) {
									emitt(pkt_sec, &YEAR, &MON, &MDAY, &DOY, &HR, &MN, &secs);
									fprintf(stderr, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f suppressed\n",
											  YEAR, DOY, HR, MN, secs);
									suppress = 1;
								}

								sprintf(header, ":b0:%4.4X", 12 * (1 + nsamp / 2));
								if(!suppress) {
									if(!fwrite(header, 8, 1, stdout)) {
										fprintf(stderr, "%s:  error writing output\n", progname);
										exit(17);
									}
								}
								if((pkt_sec - last_sec) <= 8.00) {
									if(fft_overlap > 0)
										spec[0] = (last_sec +
													  (pkt_sec - last_sec) *
													  (double) icount / (double) nFFT - start_sec);
									else
										spec[0] = (last_sec +
													  (pkt_sec - last_sec) *
													  (double) icount / (double) nFFT - start_sec);
								} else {
									spec[0] = (last_sec + (deltat * (double) (idx - idx0)) - start_sec);
								}
/*
	Fake the time to spread the spectra across the plot. TFA 12/20/01
*/
/*
        fprintf (stderr, "old offset: %10.3f", spec[0]);
*/
								if(looper_period != 0) {
									remainder = fmod((double) spec[0], (double) looper_period);
									offset = spec[0] - remainder;
/*
	If this is the first sweep in this looper_period interval,
	save the offset from the start of the interval, plot this
	sweep at the true time, and plot the rest following it.
*/
									this_chunk = (int) (offset + 0.5) / looper_period;
									if(this_chunk != last_chunk) {
										last_chunk = this_chunk;
										micro_offset = remainder;
									}
									remainder = remainder - micro_offset;
									new_time = remainder * (float) (looper_period - desired_gap) /
										 (float) wbr_duration;
									spec[0] = offset + new_time + micro_offset;	/* spread them out */
/*
        fprintf (stderr, "       new offset: %10.3f", spec[0]);
*/
								}
/*
        fprintf (stderr, "\n");
*/
								if(!suppress) {
									x_y_z[0] = spec[0];
									for(i = 0; i <= (nsamp / 2); i++) {
										x_y_z[1] = freq[i + 1];
										x_y_z[2] = spec[i + 1];
										if(!fwrite(x_y_z, sizeof(x_y_z), 1, stdout)) {
											fprintf(stderr, "%s:  error writing output\n", progname);
											exit(18);
										}
									}
								}
								icount++;
								no_data = false;
								if(fft_overlap > 0)
									idx += (nsamp - nsamp * fft_overlap / 100);
								else
									idx += nsamp;
							}			  /* while */
							last_sec = pkt_sec;
							last_RTI = RTI;
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
		if((mp_len & 0x1f00) < fft_length)
			nsamp = (mp_len & 0x1f00);
		else
			nsamp = fft_length;	  /* user selection if possible */
		iant = mbuffer.packet.mpp.mini_packet[6] & 0x07;
		if((mbuffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
			idx = 10;				  /* MSF bit set */
		else
			idx = 8;					  /* MSF bit clear */
		if((mbuffer.packet.mpp.mini_packet[5] & 0x80) == 0x80) {
			deltat = 4.5e-06;
			fs = 1. / 4.5e-06;	  /* Hi-Band */
			cal_factor = 6.43;	  /* dBmax factor */
		} else {
			deltat = 36.e-06;
			fs = 1. / 36.e-06;	  /* Lo-Band */
			cal_factor = 6.33;	  /* dBmax factor */
		}
		if(iant == 0)
			ant_length = 9.26;	  /* Ex dipole */
		else
			ant_length = 5.00;	  /* Ez monopole */
		agc = mbuffer.packet.mpp.mini_packet[7];
		bandw = 1.5 * fs / nsamp; /* Hanning ENBW */
		deltaf = fs / nsamp;
		igain = 10 * (mbuffer.packet.mpp.mini_packet[5] & 0x07);
		gain = igain;
		gain = gain + cal_factor;
		gain = pow(10., gain / 20.);

/* Attempt to do multiple FFT's for this mini-packet */
		if(fft_overlap > 0)
			nFFT = (mp_len & 0x1F00) / (nsamp - nsamp * fft_overlap / 100);	/* this many */
		else
			nFFT = (mp_len & 0x1F00) / nsamp;
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

			if(nsamp != nlast) {
				window(Hanning, nsamp, 3);
				rffti_(&nsamp, wsave);
				nlast = nsamp;
			}
			for(i = 0; i < nsamp; i++) {	/* window data */
				xreal[i] = (xreal[i] - dc) * Hanning[i] / (127.5 * gain);
				xreal[i] = xreal[i] / ant_length;	/* volts per meter */
			}

			rfftf_(&nsamp, xreal, wsave);
			xreal[0] = xreal[0] / (float) nsamp;
			for(i = 1; i < nsamp; i++)
				xreal[i] = xreal[i] / (float) (nsamp / 2);
			xreal[nsamp - 1] = xreal[nsamp - 1] / 2.0;

			spec[1] = xreal[0] * xreal[0] / bandw;
			spec[(nsamp / 2) + 1] = (xreal[nsamp - 1] * xreal[nsamp - 1]) / bandw;
			for(i = 1; i < (nsamp / 2); i++)
				spec[i + 1] = (xreal[2 * i - 1] * xreal[2 * i - 1] +
									xreal[2 * i] * xreal[2 * i]) / bandw;
			for(i = 1; i <= (nsamp / 2); i++) {
				freq[i + 1] = deltaf * (double) i / 1000.0;
			}

			sprintf(header, ":b0:%4.4X", 12 * (1 + nsamp / 2));
			if(!fwrite(header, 8, 1, stdout)) {
				fprintf(stderr, "%s:  error writing output\n", progname);
				exit(18);
			}
			if((pkt_sec - last_sec) <= 8.00) {
				if(fft_overlap > 0)
					spec[0] = (last_sec +
								  (pkt_sec - last_sec) * (double) icount / (double) nFFT - start_sec);
				else
					spec[0] = (last_sec +
								  (pkt_sec - last_sec) * (double) icount / (double) nFFT - start_sec);
			} else {
				spec[0] = (last_sec + (deltat * (double) (idx - idx0)) - start_sec);
			}
/*
	Fake the time to spread the spectra across the plot. TFA 12/20/01
*/
/*
        fprintf (stderr, "old offset: %10.3f", spec[0]);
*/
			if(looper_period != 0) {
				remainder = fmod((double) spec[0], (double) looper_period);
				offset = spec[0] - remainder;
/*
	If this is the first sweep in this looper_period interval,
	save the offset from the start of the interval, plot this
	sweep at the true time, and plot the rest following it.
*/
				this_chunk = (int) (offset + 0.5) / looper_period;
				if(this_chunk != last_chunk) {
					last_chunk = this_chunk;
					micro_offset = remainder;
				}
				remainder = remainder - micro_offset;
				new_time = remainder * (float) (looper_period - desired_gap) / (float) wbr_duration;
				spec[0] = offset + new_time + micro_offset;	/* spread them out */
/*
        fprintf (stderr, "       new offset: %10.3f", spec[0]);
*/
			}
/*
        fprintf (stderr, "\n");
*/
			x_y_z[0] = spec[0];
			for(i = 0; i <= (nsamp / 2); i++) {
				x_y_z[1] = freq[i + 1];
				x_y_z[2] = spec[i + 1];
				if(!fwrite(x_y_z, sizeof(x_y_z), 1, stdout)) {
					fprintf(stderr, "%s:  error writing output\n", progname);
					exit(18);
				}
			}
			icount++;
			no_data = false;
			if(fft_overlap > 0)
				idx += (nsamp - nsamp * fft_overlap / 100);
			else
				idx += nsamp;
		}								  /* while */
	}

	fclose(finput);
	return (no_data);
}
