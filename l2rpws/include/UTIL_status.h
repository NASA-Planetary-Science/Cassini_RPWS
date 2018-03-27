
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
	
	
  /**********************************************************************
   *            Extract status fields from mini-packet                  *
   *    control1 and control2 are select specifiers where               *
   *            bits 28-30      accomodate split bit fields             *
   *            bits 16-27      index into minipacket (byte offset)     *
   *            bits 8-15       contain a shift count                   *
   *            bits 0-7        contain a select mask                   *
   *    control2 will be zero when the field is 1 to 8 bits long        *
   *            or will specify MSB bits                                *
   *                                                                    *
   *            Extract the specified bits from the minipacket,         *
   *            shifting and masking to get the bit field justified     *
   *            into the low bits of an integer.  Up to 16 bits of      *
   *            status may be extracted at one time                     * 
   *                                                                    *
   *            We hope this makes field selection in the rest          *
   *            of the program a little easier to follow through        *
   *            the use of appropriate header files that                *
   *            define the correct patterns for control1/control2       *
   *                                                                    *
   *      All of the HFR_* patterns used for control1/control2          *
   *    arguments are taken from the "HFR Header" that came from        *
   *    Pierre...                                                       *
   *                                                                    *
   **********************************************************************/

int get_status (unsigned char *mp, int control1, int control2);
int get_status_32 (unsigned char *mp, int control[4], int);

/**********************************************************************
 *            INsert status fields into archive record                *
 *    control is the select specifiers where (same as get_status)	*
 *            bits 16-23      index into field (byte offset)	        *
 *            bits 8-15       contain a shift count                   *
 *            bits 0-7        contain a select mask                   *
 *                                                                    *
 **********************************************************************/
unsigned long set_status (unsigned char *status, int control, int pattern);
