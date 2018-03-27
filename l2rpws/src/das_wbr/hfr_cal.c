#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#define hfr_cal_C_
#include "hfr_cal.h"

/* ************************************************************************* */
/* The calibration arrays */

double a1HF1[4][A1_HF1_MAX_VALUES];
double a1HF2[4][A1_HF2_MAX_VALUES];

/* ************************************************************************* */

double next_double(char **pCh)
{
	char *pEnd;
	double dVal;

	dVal = strtod(*pCh, &pEnd);
	*pCh = pEnd;

	return dVal;
}


/* ************************************************************************* */

int read_A1HF_calibration_file(char *filename, int band)
{
	char LineIn[512], *pLineIn;
	char sFilename[256];
	long i = 0, j = 0;
	FILE *handle;

	if(filename) {
		strcpy(sFilename, filename);
		if((handle = fopen(sFilename, "rt")) == NULL) {
			fprintf(stderr, "Unable to open HFR Calibration File(A1HF%d.DAT) %s.\n", band > 0 ? 1 : 2,
					  sFilename);
			return 0;
		}
		fprintf(stderr, "INFO: Loading HFR band %d calibration: %s\n", 
		        band, filename);
	}

	if(band > 0) {
		while((pLineIn = fgets(LineIn, 512, handle)) != NULL) {
			for(j = 0; j < 4; j++)
				a1HF1[j][i] = next_double(&pLineIn);
			if(i++ >= A1_HF1_MAX_VALUES)
				break;
		}
	} else {
		while((pLineIn = fgets(LineIn, 512, handle)) != NULL) {
			for(j = 0; j < 4; j++)
				a1HF2[j][i] = next_double(&pLineIn);
			if(i++ >= A1_HF2_MAX_VALUES)
				break;
		}
	}

	fclose(handle);

	return 1;
}
