/*
        getatt.c        written 02/23/99 by TFA to process
                        Cassini AACS Quaternion SFDU data files
        Modifications:
			08/20/99 TFA added GSE coordinate option for ESB
                        11/04/04 TFA added ECLIP option for J2000 Ecliptic Quaternions
				and for Ecliptic Matrix
*/
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <stdbool.h>

#include <SpiceUsr.h>			  /* Intended interface */
#include <SpiceZfc.h>			  /* direct access to fortran functions */

#include <rtiu.h>
#include <util.h>
#include <utilt.h>

#include <das2/das1.h>

#include "webutil.h"
#include "gei2gse.h"

#define MAXFILES	1024
#define MAXCHARS	200
#define MAXENTRIES	100
#define CASSINI	-82
#define ROTOR		-82000
#define Venus		299
#define Earth		399
#define Jupiter	599
#define Saturn		699
#define SaturnBC       6		  /* barycentric */
#define Sun            10


/* ************************************************************************* */
/* Compiled in config file directory locations */

#define _QDEF(x) #x
#define QDEF(x) _QDEF(x)

#ifndef CAS_QUAT_DB
#error Compiled in default quaternions file table is missing.
#endif

#ifndef CAS_EPHEMERIS_KERNELS
#error Compiled in default spice metakernal file is missing.
#endif



typedef struct {
	char *name;
	char *val;
} entry;

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

/* Depends on packing, only works on big-endian 32-bit computers */

struct sfdu_header {
	char authority[4];
	short version_class;
	short spare;
	short ddp_id[2];
	long length_msw;
	long length;
};



static FILE *fperror;
static char htxt[] = { "NJPL2I00C" };

static char *progname;
int chdo_MATCH(char *hstg, int index_max);
int chdo_SYNC(char *primary_header, FILE * file);
int planet = Earth;
int num_spice = 0;
void unescape_url(char *url);
void plustospace(char *str);
void make_dbase(double start, double stop, struct file_structure *dbase, int *num_files);
char *makeword(char *line, char stop);
char *fmakeword(FILE * f, char stop, int *len);

double J2000_to_ECLIP[3][3];
long int J2000_FRAME = 1;
long int ECLIPJ2000_FRAME = 17;

int process_data(double start_sec, double stop_sec, double deltat, struct file_structure *dbase,
					  int n_files, char *att_type);
double interp_angle(double t1, double t2, double t, double a1, double a2);
void gei2gse(int yr, int dy, int hr, int mn, int ss, double vec_in[], double vec_out[], int planet);
void gei2ssq(int yr, int dy, int hr, int mn, int ss, double vec_in[], double vec_out[], int planet);


void att_print(double target_sec, double matrix[3][3], bool no_data, char *att_type,
					char *data_source);

/* 
void utc2et_ (char *fstring, double *et, int len);
void sce2s_ (int *spacecraft, double *et, char *sclk, int sclklen);
void scs2e_ (int *spacecraft, char *sclk, double *et, int sclklen);
void et2utc_ (double *et, char *form, int *prec, char *utc, int formlen, int utclen);
void sctiks_ (int *spacecraft, char *tolstr, double *tol, int namelen);
void scencd_ (int *spacecraft, char *sclk, double *tick0, int sclklen);
void mxv_ (double tipm [][], double vin [], double vout []);
void irfrot_ (int *refa, int *refb, double rotab[][]);
void xpose_ (double m1[][], double mout[][]);
*/
/* void bodmat (int body, double et, double tipm[][]); */

/* ************************************************************************* */

int main(int argc, char **argv)
{
	int num_files, cl, x;
	int year, month, day, doy, hr, mn;
	double sec, start_sec, stop_sec, deltat;
	struct file_structure files[MAXFILES];
	entry entries[MAXENTRIES];

	/* printf ("HTTP/1.1 200 OK\r\n"); */
	printf("Content-type: text/plain\n\n");

	progname = argv[0];
	if(!(fperror = fopen("/home/Web/tmp/cas_cgi_attitude.log", "w"))) {
		fprintf(fperror, "%s:  error opening /home/Web/tmp/cas_cgi_attitude.log\n", progname);
		exit(11);
	}

	if(!getenv("REQUEST_METHOD")) {
		printf("This script should be referenced with a METHOD of POST.\n");
		printf("If you don't understand this, see this ");
		printf("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/"
				 "overview.html\">forms overview</A>.%c", 10);
		exit(12);
	} else if(strcmp(getenv("REQUEST_METHOD"), "POST")) {
		printf("This script should be referenced with a METHOD of POST.\n");
		printf("If you don't understand this, see this ");
		printf("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/"
				 "overview.html\">forms overview</A>.%c", 10);
		exit(13);
	}

	if(!getenv("CONTENT_TYPE")) {
		printf("This script can only be used to decode form results. \n");
		exit(14);
	}
	if(strcmp(getenv("CONTENT_TYPE"), "application/x-www-form-urlencoded")) {
		printf("This script can only be used to decode form results. \n");
		exit(15);
	}

	cl = atoi(getenv("CONTENT_LENGTH"));

	for(x = 0; cl && (!feof(stdin)); x++) {
		entries[x].val = fmakeword(stdin, '&', &cl);
		plustospace(entries[x].val);
		unescape_url(entries[x].val);
		entries[x].name = makeword(entries[x].val, '=');
		fprintf(fperror, " %s %s \n", entries[x].name, entries[x].val);
	}

	if(parsetime(entries[0].val, &year, &month, &day, &doy, &hr, &mn, &sec)) {
		printf("%s: error parsing %s\n", progname, entries[0].val);
		exit(16);
	}
	start_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	fprintf(fperror, " %d %d %d %d %d %d %lf \n", year, month, day, doy, hr, mn, sec);

	if(parsetime(entries[1].val, &year, &month, &day, &doy, &hr, &mn, &sec)) {
		printf("%s: error parsing %s\n", progname, entries[1].val);
		exit(17);
	}
	stop_sec = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);

	fprintf(fperror, " %d %d %d %d %d %d %lf \n", year, month, day, doy, hr, mn, sec);

	deltat = (double) atol(entries[2].val);

	if(deltat <= 0.0) {
		printf("%s: error delta-T must be >0\n", progname);
		exit(30);
	}

	fprintf(fperror, " deltat-t = %lf \n", deltat);
	fflush(fperror);

	/* Get regular ephemeris kernels */
	if(strlen(QDEF(CAS_EPHEMERIS_KERNELS)) == 0) {
		fprintf(fperror,
				  "%s: Bad software build, looks like the environment variable "
				  "CAS_EPHEMERIS_KERNELS was not defined at compile time.\n", progname);
		exit(18);
	}


	furnsh_c(QDEF(CAS_EPHEMERIS_KERNELS));

	/* furnsh_c("/home/tfa/cassini/aacs/web/kernels.txt"); */
	/* Loading Spice Pointing Kernels (CK) here, but the Quaternions files are the main source */
	/* of information for this program.  SPICE is used if the proper kernel range is loaded    */
	/* otherwise spice pointing data is ignored.                                               */
	furnsh_c("/opt/project/cassini/spice/kernels/ck2008.list");

	make_dbase(start_sec, stop_sec, files, &num_files);

	fprintf(fperror, " num_files = %d \n", num_files);
	fflush(fperror);

	if(!strcmp(entries[3].val, "uvse"))
		planet = Venus;
	else if(!strcmp(entries[3].val, "ugse"))
		planet = Earth;
	else if(!strcmp(entries[3].val, "ujse"))
		planet = Jupiter;
	else if(!strcmp(entries[3].val, "usse"))
		planet = Saturn;
	else if(!strcmp(entries[3].val, "ussq"))
		planet = SaturnBC;
	else								  /* selected planet unrecognized */
		planet = Earth;

	if(process_data(start_sec, stop_sec, deltat, files, num_files, entries[3].val)) {
		printf("Content-type: text/plain\n\n");
		printf("No data for the time range selected.\n");
	}
	fprintf(fperror, " num_spice = %d \n", num_spice);
	if(num_spice > 0) {
		printf(" \n");
		printf(" *  Source of attitude info is SPICE C-kernel\n");
	}
	fflush(stdout);

	return 0;
}

/* ************************************************************************* */

int process_data(double start_sec, double stop_sec, double deltat, struct file_structure *dbase,
					  int n_files, char *att_type)
{
	int i, j, good, items = 130;
	long int found;
	SpiceInt inst = ROTOR;
	short length, result;
	struct event_time *evt_tim;
	struct tm *pkt_event;
	int year, doy, mon, mday, hr, mn;
	short quat1, quat2, quat3, quat4;
	double quaternion[4], sumquat;
	double last_quaternion[4];
	double matrix[3][3];
	double rmat[3][3];
	char *data_source;
	char *spice = "*";
	char *sfdu = " ";
	double vec_in[3], xgse[3], ygse[3], zgse[3];
	double phi, delta, omega;
	double phi1, delta1, omega1;
	double phi2, delta2, omega2;
	double sec;
	double target_sec;
	double last_sec = 0.0;
	double et, tol, tick0, tickr;
	SpiceInt Cassini = CASSINI;
	char scstr[32] = "1/123456789A.000";
	char *frame = "J2000";
	char scetstr[80] = "1958-001-00:00:00.000";
	unsigned char quat1_id[4] = { 0x08, 0x01, 0x03, 0xE9 };
	unsigned char quat2_id[4] = { 0x08, 0x01, 0x03, 0xEA };
	unsigned char quat3_id[4] = { 0x08, 0x01, 0x03, 0xEB };
	unsigned char quat4_id[4] = { 0x08, 0x01, 0x03, 0xEC };
	double pkt_sec = 0;
	static int st_inx, index;
	bool no_data;
	FILE *finput;
	struct sfdu_header *primary_header;
	unsigned char *buf;
	char *ibuf;

/*
   printf ("Content-type: multipart/x-mixed-replace;boundary=goober\n\n");
   printf ("\n--goober\n");
*/

	fprintf(fperror, " Type of Attitude Info = %s \n", att_type);
	if((!strcmp(att_type, "qecl")) || (!strcmp(att_type, "mecl")))
		irfrot_(&J2000_FRAME, &ECLIPJ2000_FRAME, (double *) J2000_to_ECLIP);

	no_data = true;
	num_spice = 0;
	buf = (unsigned char *) malloc(65536);
	if(!buf)
		return -1;
	primary_header = (struct sfdu_header *) buf;
	ibuf = (char *) buf;
	evt_tim = (struct event_time *) malloc(8);

	if(n_files < 1)
		return (1);

	if(stop_sec < dbase->st_sec)
		return (1);

	if(start_sec > (dbase + (n_files - 1))->sp_sec)
		return (1);

	for(index = 0; index < n_files; index++)
		if(start_sec <= (dbase + index)->sp_sec)
			break;

	if(index == n_files)
		return (1);

	if(stop_sec < (dbase + index)->st_sec)
		return (1);

	st_inx = index;

	if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
		fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
		exit(19);
	}

	target_sec = start_sec;

	sctiks_(&Cassini, "1:000", &tol, 5L);	/* want Spice data within 1 second */
/* See if we have C-Kernel data for this time */
	emitt(target_sec, &year, &mon, &mday, &doy, &hr, &mn, &sec);

	sprintf(scetstr, "%4d-%3.3d // %2.2d:%2.2d:%6.3f", year, doy, hr, mn, sec);
	utc2et_(scetstr, &et, strlen(scetstr));
	sce2s_(&Cassini, &et, scstr, strlen(scstr));
	scencd_(&Cassini, scstr, &tick0, strlen(scstr));

	do {
		do {
			if(!chdo_SYNC(ibuf, finput)) {
				fclose(finput);	  /* done with this file, time for next */
				st_inx++;
				if(st_inx < n_files) {
					if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
						fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
						exit(20);
					}
					if(!chdo_SYNC(ibuf, finput)) {
						fclose(finput);
						fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
						exit(21);
					}
				} else
					return (no_data);	/* ran out of files, exit do loop */
			}

/*        Found a good SFDU Label, now look for rest of AACS data */

			good = 1;
			length = primary_header->length;
			result = fread(buf + 20, 1, length, finput);	/* read rest of SFDU */
			if(!result) {
				fclose(finput);	  /* done with this file, time for next */
				st_inx++;
				if(st_inx < n_files) {
					if(!(finput = fopen((dbase + st_inx)->line, "r"))) {
						fprintf(stderr, "%s:  error opening %s\n", progname, (dbase + st_inx)->line);
						exit(22);
					}
					break;			  /* re-SYNC */
				} else
					return (no_data);	/* ran out of files, exit do loop */
			}
			if(length != (items - 20))
				break;				  /* bad SFDU, re-SYNC */

			for(i = 106; i < 110; i++) {
				if(buf[i] != quat1_id[i - 106])
					good = 0;
			}
			for(i = 112; i < 116; i++) {
				if(buf[i] != quat2_id[i - 112])
					good = 0;
			}
			for(i = 118; i < 122; i++) {
				if(buf[i] != quat3_id[i - 118])
					good = 0;
			}
			for(i = 124; i < 128; i++) {
				if(buf[i] != quat4_id[i - 124])
					good = 0;
			}
			quat1 = (((unsigned int) buf[110]) * 256) | ((unsigned int) buf[111]);
			quat2 = (((unsigned int) buf[116]) * 256) | ((unsigned int) buf[117]);
			quat3 = (((unsigned int) buf[122]) * 256) | ((unsigned int) buf[123]);
			quat4 = (((unsigned int) buf[128]) * 256) | ((unsigned int) buf[129]);

			quaternion[0] = ((double) quat4) / 32767.0;
			quaternion[1] = ((double) quat1) / 32767.0;
			quaternion[2] = ((double) quat2) / 32767.0;
			quaternion[3] = ((double) quat3) / 32767.0;
			sumquat =
				 (quaternion[0] * quaternion[0] + quaternion[1] * quaternion[1] +
				  quaternion[2] * quaternion[2] + quaternion[3] * quaternion[3]);

			if(((int) (sumquat * 100. + 0.5) / 100) != 1)	/* error < 1% ? */
				good = 0;			  /* if not, bad record */

			if(good) {
				evt_tim->days = (((unsigned int) buf[72]) << 8) | ((unsigned int) buf[73]);

				evt_tim->milliseconds =
					 (((unsigned int) buf[74]) << 24) | (((unsigned int) buf[75]) << 16) |
					 (((unsigned int) buf[76]) << 8) | ((unsigned int) buf[77]);
				pkt_event = UTIL_event_scet_tm(*evt_tim, 0);
				pkt_event->tm_yday++;	/* days after Jan. 1 */
				pkt_event->tm_mon++;	/* months since Jan */
				year = pkt_event->tm_year + 1900;
				doy = pkt_event->tm_yday;
				mon = pkt_event->tm_mon;	/* month, 1...12 */
				mday = pkt_event->tm_mday;	/* day of month */
				hr = pkt_event->tm_hour;
				mn = pkt_event->tm_min;
				sec = (double) pkt_event->tm_sec + (double) (evt_tim->milliseconds % 1000) / 1000.;

				pkt_sec = ttime(&year, &mon, &mday, &doy, &hr, &mn, &sec);

				if(pkt_sec < target_sec) {
					last_sec = pkt_sec;
					last_quaternion[0] = quaternion[0];
					last_quaternion[1] = quaternion[1];
					last_quaternion[2] = quaternion[2];
					last_quaternion[3] = quaternion[3];
				} else if(last_sec == 0.0) {
					last_sec = pkt_sec;
					last_quaternion[0] = quaternion[0];
					last_quaternion[1] = quaternion[1];
					last_quaternion[2] = quaternion[2];
					last_quaternion[3] = quaternion[3];
					do {
						target_sec = target_sec + deltat;
					} while(target_sec < pkt_sec);
				} else if((pkt_sec >= target_sec) && (target_sec < stop_sec)) {
					if((pkt_sec - last_sec) > 1024.0) {
						do {
							target_sec = target_sec + deltat;
						} while(target_sec < pkt_sec);
						last_sec = pkt_sec;
						last_quaternion[0] = quaternion[0];
						last_quaternion[1] = quaternion[1];
						last_quaternion[2] = quaternion[2];
						last_quaternion[3] = quaternion[3];
					} else
						do {
/* Should be able to interpolate across 2 measurements */
							q2m_(quaternion, (double *) matrix);

/* Calculate Euler angles */

							phi = atan2(matrix[2][0], -matrix[2][1]);
							delta = acos(matrix[2][2]);
							omega = atan2(matrix[0][2], matrix[1][2]);

							phi2 = phi;	/* angles at pkt_sec */
							delta2 = delta;
							omega2 = omega;

							q2m_(last_quaternion, (double *) matrix);

/* Calculate Euler angles */

							phi = atan2(matrix[2][0], -matrix[2][1]);
							delta = acos(matrix[2][2]);
							omega = atan2(matrix[0][2], matrix[1][2]);

							phi1 = phi;	/* angles at last_sec */
							delta1 = delta;
							omega1 = omega;

							phi = interp_angle(last_sec, pkt_sec, target_sec, phi1, phi2);
							delta = interp_angle(last_sec, pkt_sec, target_sec, delta1, delta2);
							omega = interp_angle(last_sec, pkt_sec, target_sec, omega1, omega2);

							matrix[0][0] = cos(phi) * cos(omega) - sin(phi) * sin(omega) * cos(delta);
							matrix[1][0] = -cos(phi) * sin(omega) - sin(phi) * cos(omega) * cos(delta);
							matrix[2][0] = sin(phi) * sin(delta);

							matrix[0][1] = sin(phi) * cos(omega) + cos(phi) * sin(omega) * cos(delta);
							matrix[1][1] = -sin(phi) * sin(omega) + cos(phi) * cos(omega) * cos(delta);
							matrix[2][1] = -cos(phi) * sin(delta);

							matrix[0][2] = sin(omega) * sin(delta);
							matrix[1][2] = cos(omega) * sin(delta);
							matrix[2][2] = cos(delta);
/*
	Attempt to retrieve C-Kernel data from the Spice data set.
*/
							ckgp_(&inst, &tick0, &tol, frame, (double *) rmat, &tickr, &found,
									strlen(frame));

							data_source = sfdu;
							if((found == 1) && (tickr == tick0)) {
/*		SPICE matrix from CKGP is transpose of AACS matrix. */

								matrix[0][0] = rmat[0][0];
								matrix[1][0] = rmat[0][1];
								matrix[2][0] = rmat[0][2];
								matrix[0][1] = rmat[1][0];
								matrix[1][1] = rmat[1][1];
								matrix[2][1] = rmat[1][2];
								matrix[0][2] = rmat[2][0];
								matrix[1][2] = rmat[2][1];
								matrix[2][2] = rmat[2][2];
								data_source = spice;
								num_spice++;
							}

							att_print(target_sec, matrix, no_data, att_type, data_source);
							no_data = false;
							target_sec = target_sec + deltat;

/* recalculate TICK0, in whatever the heck units SPICE uses */

							emitt(target_sec, &year, &mon, &mday, &doy, &hr, &mn, &sec);
							sprintf(scetstr, "%4d-%3.3d // %2.2d:%2.2d:%6.3f", year, doy, hr, mn, sec);
							utc2et_(scetstr, &et, strlen(scetstr));
							sce2s_(&Cassini, &et, scstr, strlen(scstr));
							scencd_(&Cassini, scstr, &tick0, strlen(scstr));

						}
						while(target_sec < pkt_sec);
				}						  /* if ( (pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
			}							  /* if (good) */
		}
		while(target_sec < stop_sec);	/* inner do loop */
	}
	while(pkt_sec < stop_sec);
	fclose(finput);
	return (no_data);
}

/* ************************************************************************* */

void make_dbase(double start_sec, double stop_sec, struct file_structure *dbase, int *num_files)
{
	int index, year, month, day, doy, hr, mn;
	int begin_sclk, end_sclk, databits;
	double sec, total_secs, file_start, file_stop;
	char *file_name = QDEF(CAS_QUAT_DB);
	FILE *finput;
	char sline[MAXCHARS];
	char start[80], stop[80];


	if(strlen(QDEF(CAS_QUAT_DB)) == 0) {
		fprintf(fperror,
				  "%s: Bad software build, looks like the environment "
				  "variable CAS_QUAT_DB was defined at compile time.\n", progname);
		exit(23);
	}


	if(!(finput = fopen(file_name, "r"))) {
		fprintf(fperror, "%s:  error opening %s\n", progname, file_name);
		exit(24);
	}

	*num_files = 0;
	do {
		if(fgets(sline, MAXCHARS, finput) != NULL) {
			index = *num_files;
			sscanf(sline, "%s %s %x %x %s %x", start, stop, &begin_sclk, &end_sclk,
					 (dbase + index)->line, &databits);

/* get file start time and insert into data base */

			if(parsetime(start, &year, &month, &day, &doy, &hr, &mn, &sec)) {
				fprintf(stderr, "%s: error parsing %s\n", progname, start);
				exit(25);
			}
			(dbase + index)->year = year;
			(dbase + index)->day = doy;
			(dbase + index)->hr = hr;
			(dbase + index)->mn = mn;
			(dbase + index)->sec = sec;
			(dbase + index)->ms = 1000. * ((double) (hr * 3600 + mn * 60) + (double) sec);
			total_secs = ttime(&year, &month, &day, &doy, &hr, &mn, &sec);
			(dbase + index)->st_sec = total_secs;
			file_start = total_secs;

/* get file stop time and insert into data base */

			if(parsetime(stop, &year, &month, &day, &doy, &hr, &mn, &sec)) {
				fprintf(stderr, "%s: error parsing %s\n", progname, stop);
				exit(26);
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

/* ************************************************************************* */
 /* 
  *     Beginig of check for CHDO record alignment....
  */
int chdo_SYNC(char *primary_header, FILE * file)
{
	int index = 0;
	char hstg[21];
	memset(hstg, 0, 21);
	while(index < 20) {
		hstg[index++] = fgetc(file);
		if(feof(file))
			return 0;
		switch (chdo_MATCH(hstg, index)) {
		case 0:						  /* GOOD !!! */
		case 2:						  /* so far, so good */
			break;
		case 1:						  /* mis-match */
			hstg[0] = hstg[index - 1];
			memset(&hstg[1], 0, 20);
			index = 1;
			break;
		}
	}
	memcpy(primary_header, hstg, 20);
	return 1;
}

/* ************************************************************************* */
/*
  *     Look for the "htxt" string at the begining
  *       of a CHDO record.  Seems like cTOT occasionally
  *       spews chunks (i.e. status messages) between
  *       data records...
  */
int chdo_MATCH(char *hstg, int index_max)
{
	int index;
	if(index_max >= strlen(htxt))
		return 0;
	for(index = 0; index < index_max; index++) {
		if(hstg[index] != htxt[index])
			return 1;
	}
	return 2;
}

/* ************************************************************************* */

double interp_angle(double t1, double t2, double t, double a1, double a2)
{
	double x1, x2, y1, y2, x, y;
	if(t1 >= t2)
		return (a1);				  /* punt */
	x1 = cos(a1);
	x2 = cos(a2);
	y1 = sin(a1);
	y2 = sin(a2);
	x = x1 + ((t - t1) / (t2 - t1)) * (x2 - x1);
	y = y1 + ((t - t1) / (t2 - t1)) * (y2 - y1);
	return (atan2(y, x));
}

/* ************************************************************************* */

void att_print(double target_sec, double matrix[3][3], bool no_data, char *att_type,
					char *data_source)
{
	double quaternion[4];
	double phi, delta, omega;
	int year, doy, mon, mday, hr, mn, i;
	double sec;
	double vec_in[3], xgse[3], ygse[3], zgse[3];
	double vec_out[3], dummy;
	double e_matrix[3][3], e_quaternion[4];

	m2q_((double *) matrix, quaternion);

/* Calculate Euler angles */

	fprintf(fperror, "no_data=%d         att_type=%s\n", no_data, att_type);

	phi = atan2(matrix[2][0], -matrix[2][1]);
	delta = acos(matrix[2][2]);
	omega = atan2(matrix[0][2], matrix[1][2]);
	phi = 90. * phi / atan2(1.0, 0);
	if(phi < 0.0)
		phi = phi + 360.0;
	delta = 90. * delta / atan2(1.0, 0);
	if(delta < 0.0)
		delta = delta + 360.0;
	omega = 90. * omega / atan2(1.0, 0);
	if(omega < 0.0)
		omega = omega + 360.0;

/* If request was for Ecliptic coordinates, rotate from J2000 to ECLIPJ2000 */

	if((!strcmp(att_type, "qecl")) || (!strcmp(att_type, "mecl"))) {
/*			Rotate S/C X-axis		*/
		vec_in[0] = matrix[0][0];
		vec_in[1] = matrix[0][1];
		vec_in[2] = matrix[0][2];
		mxv_((double *) J2000_to_ECLIP, vec_in, vec_out);
		e_matrix[0][0] = vec_out[0];
		e_matrix[0][1] = vec_out[1];
		e_matrix[0][2] = vec_out[2];
/*			Rotate S/C Y-axis		*/
		vec_in[0] = matrix[1][0];
		vec_in[1] = matrix[1][1];
		vec_in[2] = matrix[1][2];
		mxv_((double *) J2000_to_ECLIP, vec_in, vec_out);
		e_matrix[1][0] = vec_out[0];
		e_matrix[1][1] = vec_out[1];
		e_matrix[1][2] = vec_out[2];
/*			Rotate S/C Z-axis		*/
		vec_in[0] = matrix[2][0];
		vec_in[1] = matrix[2][1];
		vec_in[2] = matrix[2][2];
		mxv_((double *) J2000_to_ECLIP, vec_in, vec_out);
		e_matrix[2][0] = vec_out[0];
		e_matrix[2][1] = vec_out[1];
		e_matrix[2][2] = vec_out[2];
/*			Convert to ECLIPJ2000 quaternions	*/
		m2q_((double *) e_matrix, e_quaternion);
		xpose_((double *) e_matrix, (double *) e_matrix);	/* get transpose */
	}

	emitt(target_sec, &year, &mon, &mday, &doy, &hr, &mn, &sec);

	if(no_data) {					  /* first time only */

		if(!strcmp(att_type, "quat")) {
			printf("         SCET           Quaternion1    Quaternion2   Quaternion3   Quaternion4\n");
			printf("---------------------   -----------    -----------   -----------   -----------\n");
		} else if(!strcmp(att_type, "qecl")) {
			printf("         SCET            EclipQuat1     EclipQuat2    EclipQuat3     EclipQuat4\n");
			printf("---------------------   -----------    -----------   -----------   -----------\n");
		} else if(!strcmp(att_type, "eule")) {
			printf("         SCET               Phi, deg.      Theta, deg.     Omega, deg.     \n");
			printf("---------------------     -----------      -----------     -----------     \n");
		} else if(!strcmp(att_type, "matr")) {
			printf("         SCET             T11,T21,T31      T12,T22,T32     T13,T23,T33                \n");
			printf("---------------------     -----------      -----------     -----------     \n");
		} else if(!strcmp(att_type, "mecl")) {
			printf("         SCET             S/C X (ECL)      S/C Y (ECL)     S/C Z (ECL)\n");
			printf("---------------------     -----------      -----------     -----------     \n");
		} else if(!strcmp(att_type, "ugse")) {
			printf("         SCET             S/C X (GSE)      S/C Y (GSE)     S/C Z (GSE)\n");
			printf("---------------------     -----------      -----------     -----------     \n");
		} else if(!strcmp(att_type, "uvse")) {
			printf("         SCET             S/C X (VSE)      S/C Y (VSE)     S/C Z (VSE)\n");
			printf("---------------------     -----------      -----------     -----------     \n");
		} else if(!strcmp(att_type, "ujse")) {
			printf("         SCET             S/C X (JSE)      S/C Y (JSE)     S/C Z (JSE)\n");
			printf("---------------------     -----------      -----------     -----------     \n");
		} else if(!strcmp(att_type, "usse")) {
			printf("         SCET             S/C X (SSE)      S/C Y (SSE)     S/C Z (SSE)\n");
			printf("---------------------     -----------      -----------     -----------     \n");
		} else if(!strcmp(att_type, "ussq")) {
			printf("         SCET             S/C X (SSQ)      S/C Y (SSQ)     S/C Z (SSQ)\n");
			printf("---------------------     -----------      -----------     -----------     \n");
		}
	}
	printf("%4d-%3.3dT%2.2d:%2.2d:%06.3f  ", year, doy, hr, mn, sec);
	if(!strcmp(att_type, "quat"))
		printf("   %9.6lf      %9.6lf     %9.6lf     %9.6lf %s\n", quaternion[1], quaternion[2],
				 quaternion[3], quaternion[0], data_source);
	else if(!strcmp(att_type, "qecl"))
		printf("   %9.6lf      %9.6lf     %9.6lf     %9.6lf %s\n", e_quaternion[1], e_quaternion[2],
				 e_quaternion[3], e_quaternion[0], data_source);
	else if(!strcmp(att_type, "eule"))
		printf("     %9.3lf        %9.3lf       %9.3lf %s\n", phi, delta, omega, data_source);
	else if(!strcmp(att_type, "matr")) {
		printf("     %9.6lf        %9.6lf       %9.6lf %s\n", matrix[0][0], matrix[0][1],
				 matrix[0][2], data_source);
		printf("                       ");
		printf("     %9.6lf        %9.6lf       %9.6lf \n", matrix[1][0], matrix[1][1], matrix[1][2]);
		printf("                       ");
		printf("     %9.6lf        %9.6lf       %9.6lf \n", matrix[2][0], matrix[2][1], matrix[2][2]);
		printf("                          -----------      -----------");
		printf("     -----------     \n");
	} else if(!strcmp(att_type, "mecl")) {
		printf("     %9.6lf        %9.6lf       %9.6lf %s\n", e_matrix[0][0], e_matrix[0][1],
				 e_matrix[0][2], data_source);
		printf("                       ");
		printf("     %9.6lf        %9.6lf       %9.6lf \n", e_matrix[1][0], e_matrix[1][1],
				 e_matrix[1][2]);
		printf("                       ");
		printf("     %9.6lf        %9.6lf       %9.6lf \n", e_matrix[2][0], e_matrix[2][1],
				 e_matrix[2][2]);
		printf("                          -----------      -----------");
		printf("     -----------     \n");
	} else if(!strcmp(att_type, "ugse") || !strcmp(att_type, "uvse") || !strcmp(att_type, "ujse")
				 || !strcmp(att_type, "usse")) {
		for(i = 0; i < 3; i++)
			vec_in[i] = matrix[0][i];
		gei2gse(year, doy, hr, mn, (int) sec, vec_in, xgse, planet);
		for(i = 0; i < 3; i++)
			vec_in[i] = matrix[1][i];
		gei2gse(year, doy, hr, mn, (int) sec, vec_in, ygse, planet);
		for(i = 0; i < 3; i++)
			vec_in[i] = matrix[2][i];
		gei2gse(year, doy, hr, mn, (int) sec, vec_in, zgse, planet);
		printf("     %9.6lf        %9.6lf       %9.6lf %s\n", xgse[0], ygse[0], zgse[0], data_source);
		printf("                       ");
		printf("     %9.6lf        %9.6lf       %9.6lf \n", xgse[1], ygse[1], zgse[1]);
		printf("                       ");
		printf("     %9.6lf        %9.6lf       %9.6lf \n", xgse[2], ygse[2], zgse[2]);
		printf("                          -----------      -----------");
		printf("     -----------     \n");
	} else if(!strcmp(att_type, "ussq")) {
/* Use same coordinate transformation that Joe uses for Equatorial Coordinates */
		for(i = 0; i < 3; i++)
			vec_in[i] = matrix[0][i];
		gei2ssq(year, doy, hr, mn, (int) sec, vec_in, xgse, planet);
		for(i = 0; i < 3; i++)
			vec_in[i] = matrix[1][i];
		gei2ssq(year, doy, hr, mn, (int) sec, vec_in, ygse, planet);
		for(i = 0; i < 3; i++)
			vec_in[i] = matrix[2][i];
		gei2ssq(year, doy, hr, mn, (int) sec, vec_in, zgse, planet);
		printf("     %9.6lf        %9.6lf       %9.6lf %s\n", xgse[0], ygse[0], zgse[0], data_source);
		printf("                       ");
		printf("     %9.6lf        %9.6lf       %9.6lf \n", xgse[1], ygse[1], zgse[1]);
		printf("                       ");
		printf("     %9.6lf        %9.6lf       %9.6lf \n", xgse[2], ygse[2], zgse[2]);
		printf("                          -----------      -----------");
		printf("     -----------     \n");
	}
}

/* ************************************************************************* */

void gei2ssq(int yr, int dy, int hr, int mn, int ss, double vec_in[], double vec_out[], int planet)
{
	char scstr[80], utstr2[25];
	int Cassini = -82;
	int index;
	double sun_pos[6], sun_pos2[3];
	double pos[3];
	double x_vec[3], y_vec[3], z_vec[3];
	double tipm[3][3];
	double lt, et;
	char *frame = "J2000";
	char *aberr = "LT+S";

/* vec_in should be J2000 vector, vec_out will be in Saturn Solar Equatorial coordinates */
/* planet should be Saturn BaryCenter, to match the ephemeris web page */

	sprintf(scstr, "%04d %03d // %02d:%02d:%02d.0", yr, dy, hr, mn, ss);
	utc2et(scstr, &et);

	spkez(Sun, et, frame, aberr, planet, sun_pos, &lt);

	for(index = 0; index < 3; index++)
		pos[index] = sun_pos[index];

	vhat_(pos, sun_pos2);

/* This is limited to Saturn */

	bodmat(Saturn, (et - lt), (double *) tipm);

	for(index = 0; index < 3; index++) {
		x_vec[index] = sun_pos2[index];
		z_vec[index] = tipm[2][index];
	}

	ucrss_(z_vec, x_vec, y_vec);
	ucrss_(y_vec, z_vec, x_vec);

	vec_out[0] = x_vec[0] * vec_in[0] + x_vec[1] * vec_in[1] + x_vec[2] * vec_in[2];
	vec_out[1] = y_vec[0] * vec_in[0] + y_vec[1] * vec_in[1] + y_vec[2] * vec_in[2];
	vec_out[2] = z_vec[0] * vec_in[0] + z_vec[1] * vec_in[1] + z_vec[2] * vec_in[2];
	return;
}
