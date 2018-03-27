
#ifndef _find_mfr_H_
#define _find_mfr_H_

/* Sure would be nice to have the return values documented, or heck even
 * say why this function exists.
 */
int process_mfr(
	double start_sec, double stop_sec, double requested_sec, 
	struct file_structure *dbase, int n_files, double *MFR_time, int *MFR_Ant
);

#endif
