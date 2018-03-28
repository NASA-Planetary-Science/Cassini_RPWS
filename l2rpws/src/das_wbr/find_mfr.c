/* Common code from both the new10k_V4 and new80k_v4 readers */

#include <stdio.h>
#include <strings.h>
#include <stdlib.h>
#include <stdbool.h>

#include <das2/das1.h>

#include <rtiu.h>
#include <util.h>
#include <utilt.h>

#include "file_list.h"
#include "dec.h"

/* ************************************************************************* */
/* MFR data can interfere */

int process_mfr(
	double start_sec, double stop_sec, double requested_sec,
	struct file_structure *dbase, int n_files, double *MFR_time, int *MFR_Ant
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
	static int st_inx, no_data;
	static int index, index2, freq_step;
	static FILE *finput;

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
			fprintf(stderr, "ERROR: opening %s\n", (dbase + st_inx)->line);
			exit(-1);
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
					fprintf(stderr, "ERROR: opening %s\n", (dbase + st_inx)->line);
					exit(-1);
				}
				if((UTIL_getbuffer_MP(&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED) {
					fclose(finput);
					fprintf(stderr, "ERROR: error opening %s\n", (dbase + st_inx)->line);
					exit(-1);
				}
			} else {
				nomore = true;
				return (1);			  /* ran out of files, exit do loop */
			}
		}
		inst_type = (m.buffer.packet.mpx.mini_packet[0] >> 4) & 0xf;
		if(inst_type == MFR) {
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
				if(mp_len != 226)
					error_flag = 1;  /* correct length ? */
				if((!error_flag) && (requested_sec < (pkt_sec + 32.0))) {	/* data ok ? */
					*MFR_Ant = (m.buffer.packet.mpp.mini_packet[4] >> 1) & 3;
					*MFR_time = pkt_sec;
					no_data = false;
					return (2);
				} else {
					*MFR_Ant = (m.buffer.packet.mpp.mini_packet[4] >> 1) & 3;
					emitt(pkt_sec, &YEAR, &MON, &MDAY, &DOY, &HR, &MN, &secs);
					fprintf(stderr, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f ", YEAR, DOY, HR, MN, secs);
					emitt(requested_sec, &YEAR, &MON, &MDAY, &DOY, &HR, &MN, &secs);
					fprintf(stderr, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f ", YEAR, DOY, HR, MN, secs);
					fprintf(stderr, " %2d ", MFR_Ant);
					fprintf(stderr, " ****NOT FOUND");
					fprintf(stderr, "  \n");
				}

			}							  /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
		}								  /* if( inst_type == MFR ) */
	}
	while(requested_sec > pkt_sec);
	return (3);
}
