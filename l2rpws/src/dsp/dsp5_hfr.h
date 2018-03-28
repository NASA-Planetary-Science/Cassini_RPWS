
  /**********************************************************************
   *									*
   *	HFR Sounder timing:		MILLISECONDS			*
   *		This is a routine available to external users,		*
   *		it is NOT used internally.  Pass it a character array	*
   *		that contains a mini-packet (unsigned char *mp)		*
   *		and it will return:					*
   *			A: overall repeat count (int fuinction return)	*
   *			B: passive sounder timing (how long HFR does	*
   *				passive sweeps prior to exciting the	*
   *				local plasma				*
   *			C: active sounder timing (how long HFR spends	*
   *				exciting the local plasma, disturbing	*
   *				everything (transmitting)		*
   *		B and C repeat "A" times (B C B C B C not B B B C C C)	*
   *									*
   *									*
   *	DO NOT change this interface, it is used by everyone !!!	*								*
   *									*
   **********************************************************************/
extern float opmode_sounder (unsigned char *mp, float *passive, float *active,
                             int *cycles);

  /**********************************************************************
   *									*
   *	HFR status display, french format				*
   *	  returns TEXT STRINGS to DSP5 (might change)			*
   *	  keep calling while incrementing "request_index" (from 0)	*
   *	  until a NULL string is returned				*
   *									*
   **********************************************************************/
extern char *opmodes_hfr (unsigned char *mp, int request_index);

  /**********************************************************************
   *									*
   *	Pass in a mini-packet and 1 or two control words		*
   *	Get back the value						*
   *									*
   **********************************************************************/
extern int get_status (unsigned char *mp, int control1, int control2);
