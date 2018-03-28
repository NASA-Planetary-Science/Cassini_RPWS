#include <stdlib.h>
#include <stdio.h>
#include <strings.h>

#include <das2/das1.h>

#include "file_list.h"


/* ************************************************************************* */
/* Compiled in file locations */

#define _QDEF(x) #x
#define QDEF(x) _QDEF(x)

#ifndef RPWS_MPDB
#error Compiled in default RPWS U-file table is missing, set RPWS_MPDB
#endif


/* ************************************************************************* */
void make_dbase(
	const char* dataset, double start_sec, double stop_sec,
	struct file_structure *dbase, int *num_files
){
	int index, year, month, day, doy, hr, mn;
	int begin_sclk, end_sclk, databits;
	double sec, total_secs, file_start, file_stop;
	const char *file_name = NULL;
	FILE *finput;
	char input_line[MAXCHARS];
	char start[80], stop[80];

	
	if(dataset != NULL)
		file_name = dataset;
	else
		file_name = QDEF( RPWS_MPDB);
	
	if((file_name == NULL)||(strlen(file_name) == 0)){
		fprintf(stderr, "ERROR: No minipacket file master index given, can't "
		        "find data files.\n");
		exit(17);
	} else{
		fprintf(stderr, "INFO: Finding minipacket files via: %s\n", file_name);
	}
	
	if(!(finput = fopen(file_name, "r"))) {
		fprintf(stderr, "ERROR: opening %s\n", file_name);
		exit(-1);
	}

	*num_files = 0;
	do {
		if(fgets(input_line, MAXCHARS, finput) != NULL) {
			index = *num_files;
			sscanf(input_line, "%s %s %x %x %s %x", start, stop, &begin_sclk, &end_sclk,
					 (dbase + index)->line, &databits);

         /* get file start time and insert into data base */

			if(parsetime(start, &year, &month, &day, &doy, &hr, &mn, &sec)) {
				fprintf(stderr, "ERROR: parsing %s\n", start);
				exit(1);
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
				fprintf(stderr, "ERROR: parsing %s\n", stop);
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
