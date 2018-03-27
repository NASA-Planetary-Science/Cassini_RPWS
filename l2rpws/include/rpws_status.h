
/************************************************/

/*	Include all of the rpws status files	*/

/************************************************/
#include <cds_status.h>
#include <dcp_status.h>
#include <dust_status.h>
#include <hfr_status.h>
#include <lfdr_status.h>
#include <lp_status.h>
#include <mfr_status.h>
#include <mp_status.h>
#include <stim_status.h>
#include <wbr_status.h>
#include <wfr_status.h>
#include <mro_status.h>

/* Copied in from archive/rpws_status, the source had two files
   with the same name, both referenced across mutliple programs
	but with different contents. --cwp */

int get_status (unsigned char *mp, int control1, int control2);
int get_status_32 (unsigned char *mp, int control[4], int);
unsigned long set_status (unsigned char *status, int control, int pattern);
