/*
        wfrgain.c       written 03/27/01 by TFA to get WFR gains
        Modifications:
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <rtiu.h>
#include <util.h>
#include <utilt.h>
#include <time.h>
#include <curses.h>
#include <math.h>
#include <das.h>
#include "dec.h"

#define  MAXFILES 8192
#define  MAXCHARS 200
#define  True   1
#define  False  0

struct file_structure {
	char line[MAXCHARS];
	int year;
	int day;
	int hr;
	int mn;
	int num_recs;
	float sec;
	double ms;
	double st_sec;
	double sp_sec;
};

static char *progname;
static char *dataset = "JPL";
static int chan_select;

void make_dbase(double start, double stop, struct file_structure *dbase,
					 int *num_files);

int process_data(double start, double stop, struct file_structure *dbase,
					  int n_files);

main(int argc, char **argv)
{
	int num_files;
	int year, month, day, doy, hr, mn;
	double sec, start_sec, stop_sec;
	struct file_structure files[MAXFILES];

/***************************************************************/

	if(argc <= 3) {
		fprintf(stderr, "Usage is: %s channel# start stop [dataset]\n",
				  argv[0]);
		exit(1);
	}
	progname = argv[0];

	chan_select = atol(argv[1]);
	if((chan_select < 0) || (chan_select > 4)) {
		fprintf(stderr, "%s: bad channel # %s\n", argv[0], argv[1]);
		exit(1);
	}

	if(parsetime(argv[2], &year, &month, &day, &doy, &hr, &mn, &sec)) {
		fprintf(stderr, "%s: error parsing %s\n", argv[0], argv[2]);
		exit(1);
	}
	start_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	if(parsetime(argv[3], &year, &month, &day, &doy, &hr, &mn, &sec)) {
		fprintf(stderr, "%s: error parsing %s\n", argv[0], argv[3]);
		exit(1);
	}
	stop_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	if(argc == 5)
		dataset = argv[4];
	fprintf(stderr, "Selected data set is %s\n", dataset);

	make_dbase(start_sec, stop_sec, files, &num_files);
	if(num_files == 0) {
		fprintf(stderr,
				  "No data coverage of the requested time interval.\n");
		exit(1);
	}

	if(process_data(start_sec, stop_sec, files, num_files)) {
		fprintf(stderr,
				  "No data coverage of the requested time interval.\n");
		exit(1);
	}
	return (0);
}

int process_data(double start_sec, double stop_sec,
					  struct file_structure *dbase, int n_files)
{
	int epoch;
	int inst_type, mp_len, chan, band;
	int wfrdata, nsamp, nlast = 2048;
	int i, idx, RTI, igain, agc, iant, first = 1;
	int year, month, day, doy, hr, mn, mon, mday;
	int Zero = 0, One = 1;
	char *toplbl =
		 "WFR Raw Data 1999-230T03:30:00.000  Sensor: EX  Gain: 00 dB";
	char *time_string = "1999-230T03:30:00.000";
	char *gain_string = "  Gain: 00 dB";
	char *sensor_string = "  Sensor: EX ";
	float x[2048], y[2048];
	float xmin, xmax, ymin, ymax, deltat;
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
	int st_inx, no_data;
	int index, index2;
	float offset, spec[1026], spec_out[1026];
	FILE *finput;

	no_data = True;
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
		fprintf(stderr, "%s:  error opening %s\n",
				  progname, (dbase + st_inx)->line);
		exit(-1);
	}

	do {
		if((UTIL_getbuffer_MP(&m.buffer, finput, UTIL_GET_NON_BLOCKING)) ==
			EOF_REACHED) {
			fclose(finput);		  /* done with this file, time for next */
			st_inx++;
			if(st_inx < n_files) {
				if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
					fprintf(stderr, "%s:  error opening %s\n",
							  progname, (dbase + st_inx)->line);
					exit(-1);
				}
				if((UTIL_getbuffer_MP
					 (&m.buffer, finput,
					  UTIL_GET_NON_BLOCKING)) == EOF_REACHED) {
					fclose(finput);
					fprintf(stderr, "%s:  error opening %s\n",
							  progname, (dbase + st_inx)->line);
					exit(-1);
				}
			} else
				break;				  /* ran out of files, exit do loop */
		}
		inst_type = (m.buffer.packet.mpx.mini_packet[0] >> 4) & 0xf;
		chan = (m.buffer.packet.mpx.mini_packet[7] >> 3) & 0x7;
		if(chan == 7)				  /* Combined Mode ? */
			chan = m.buffer.packet.mpx.mini_packet[7] & 0x7;
		band = (m.buffer.packet.mpx.mini_packet[5] >> 7) & 0x1;
		if((inst_type == WFR) && (chan == chan_select)) {
			mp_len = UTIL_MP_length(&m.buffer);
			if((m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08) {
				if(mp_len > 0x1007)
					mp_len = 0x1007; /* truncate, MSF bit set */
			} else {
				if(mp_len > 0x1005)
					mp_len = 0x1005; /* truncate, MSF bit clear */
			}
			RTI = (m.buffer.packet.mpp.mini_packet[2] & 0xff) |
				 ((m.buffer.packet.mpp.mini_packet[3] & 0xff) << 8);
			pkt_sclk = UTIL_event_time(&m.buffer, 0);
			epoch = (m.buffer.packet.cds_tag.epoch[0] << 24) |
				 (m.buffer.packet.cds_tag.epoch[1] << 16) |
				 (m.buffer.packet.cds_tag.epoch[2] << 8) |
				 (m.buffer.packet.cds_tag.epoch[3] << 0);
			pkt_epoc = pkt_sclk + epoch;
/*                                                              */
/*       Newest SCET calculation scheme: 6-Jan-1999 TFA         */
/*                                                              */
			if(epoch) {				  /* JPL data has non-zero epoch */
				evt_clk.seconds = pkt_sclk;	/* SCLK seconds */
				evt_clk.fine = UTIL_extract_MP_RTI(&m.buffer) << 5;	/* SCLK
																						   fine */
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
				if((mp_len == 0x0405) || (mp_len == 0x0407) ||	/* 512samp */
					(mp_len == 0x0805) || (mp_len == 0x0807) ||	/* 1Ksamp */
					(mp_len == 0x1005) || (mp_len == 0x1007)) {	/* 2Ksamp */
					iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
					if((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) {
						sprintf(time_string, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f",
								  year, doy, hr, mn, sec);
						strcpy(toplbl, "WFR Raw Data ");
						strcat(toplbl, time_string);
						if(band)
							deltat = 140.e-6;	/* Hi-Band ? */
						else
							deltat = 0.010;	/* Lo-Band ? */
						if((m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
							idx = 10;  /* MSF bit set */
						else
							idx = 8;	  /* MSF bit clear */

						if(chan_select == 0)
							igain =
								 10 * (m.buffer.packet.mpp.mini_packet[5] & 0x03);
						else if(chan_select == 1)
							igain =
								 10 *
								 ((m.buffer.packet.mpp.
									mini_packet[5] >> 2) & 0x03);
						else
							igain =
								 10 *
								 ((m.buffer.packet.mpp.
									mini_packet[5] >> 4) & 0x03);

						if(chan_select == 0) {
							if(m.buffer.packet.mpp.mini_packet[6] & 0x01)
								strcpy(sensor_string, "  Sensor: EX ");
							else
								strcpy(sensor_string, "  Sensor: EX+");
						} else if(chan_select == 1) {
							if(m.buffer.packet.mpp.mini_packet[6] & 0x02)
								strcpy(sensor_string, "  Sensor: EZ ");
							else
								strcpy(sensor_string, "  Sensor: EX-");
						} else if(chan_select == 2) {
							if(m.buffer.packet.mpp.mini_packet[6] & 0x04)
								strcpy(sensor_string, "  Sensor: LP ");
							else
								strcpy(sensor_string, "  Sensor: BX ");
						} else if(chan_select == 3)
							strcpy(sensor_string, "  Sensor: BY ");
						else if(chan_select == 4)
							strcpy(sensor_string, "  Sensor: BZ ");

						strcat(toplbl, sensor_string);

						sprintf(gain_string, "  Gain: %2.2d dB", igain);
						strcat(toplbl, gain_string);

						fprintf(stderr, "%s\n", toplbl);

						nsamp = 512;
						if((mp_len == 0x0405) || (mp_len == 0x0407))
							nsamp = 512;	/* 512samp */
						else if((mp_len == 0x0805) || (mp_len == 0x0807))
							nsamp = 1024;	/* 1Ksamp */
						else if((mp_len == 0x1005) || (mp_len == 0x1007))
							nsamp = 2048;	/* 2Ksamp */

						fprintf(stdout, "%s\n", toplbl);
						no_data = False;
					}					  /* if ((pkt_sec >= start_sec) && (pkt_sec
										     < stop_sec)) */
				}						  /* if ((mp_len == 0x805) || (mp_len ==
										     0x807)) etc. */
			}							  /* if (pkt_sec < stop_sec) */
		}								  /* if( inst_type == WFR ) */
	}
	while(pkt_sec < stop_sec);
	fclose(finput);
	return (no_data);
}

void make_dbase(double start_sec, double stop_sec,
					 struct file_structure *dbase, int *num_files)
{
	int index, year, month, day, doy, hr, mn;
	int begin_sclk, end_sclk, databits;
	double sec, total_secs, file_start, file_stop;
	char *file_name = "/opt/project/cassini/data/cassini.db";
	FILE *finput;
	char inline[MAXCHARS];
	char start[80], stop[80];

	if(strcmp(dataset, "IOWA") == 0)
		file_name = "/opt/project/cassini/data/CassiniIOWA.db";
	else
		file_name = "/opt/project/cassini/data/CassiniJPL.db";
	if(!(finput = fopen(file_name, "r"))) {
		fprintf(stderr, "%s:  error opening %s\n", progname, file_name);
		exit(-1);
	}

	*num_files = 0;
	do {
		if(fgets(inline, MAXCHARS, finput) != NULL) {
			index = *num_files;
			sscanf(inline, "%s %s %x %x %s %x",
					 start, stop,
					 &begin_sclk, &end_sclk, (dbase + index)->line, &databits);

/* get file start time and insert into data base */

			if(parsetime(start, &year, &month, &day, &doy, &hr, &mn, &sec)) {
				fprintf(stderr, "%s: error parsing %s\n", progname, start);
				exit(1);
			}
			(dbase + index)->year = year;
			(dbase + index)->day = doy;
			(dbase + index)->hr = hr;
			(dbase + index)->mn = mn;
			(dbase + index)->sec = sec;
			(dbase + index)->ms =
				 1000. * ((double) (hr * 3600 + mn * 60) + (double) sec);

			total_secs = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);
			(dbase + index)->st_sec = total_secs;
			file_start = total_secs;

/* get file stop time and insert into data base */

			if(parsetime(stop, &year, &month, &day, &doy, &hr, &mn, &sec)) {
				fprintf(stderr, "%s: error parsing %s\n", progname, stop);
				exit(1);
			}

			total_secs = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);
			(dbase + index)->sp_sec = total_secs;
			file_stop = total_secs;

			if((file_start < stop_sec) && (file_stop >= start_sec))
				(*num_files)++;
		}
	}
	while(feof(finput) == 0);

	fclose(finput);

}

