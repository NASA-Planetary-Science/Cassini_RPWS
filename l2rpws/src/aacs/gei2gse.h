#ifndef _gei2gse_H_
#define _gei2gse_H_


void bodmat (int body, double et, double *tipm);

void gei2gse(
	int yr, int dy, int hr, int mn, int ss, double vec_in[], double vec_out[], 
	int planet
); 

/* Joe's Fortran Spice wrappers */

void spkez (
   int    targ,
   double et,
   char   *ref,
   char   *abcorr,
   int    obs,
   double *starg, /* 6 element array */
   double *lt);

void utc2et (char *utcstr, double *et);
		
#endif /* _gei2gse_H_ */
