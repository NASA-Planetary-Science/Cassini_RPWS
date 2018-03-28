
#ifndef hfr_cal_H_
#define hfr_cal_H_

#define A1_HF1_MAX_VALUES	166
#define A1_HF2_MAX_VALUES	322

/* These contain the calibrations after they are loaded */
#ifndef hfr_cal_C_
extern double a1HF1[4][A1_HF1_MAX_VALUES];
extern double a1HF2[4][A1_HF2_MAX_VALUES];
#endif

/* Load the calibration tables above */
int read_A1HF_calibration_file(char *filename, int band);

#endif /* _hfr_cal_h_ */
