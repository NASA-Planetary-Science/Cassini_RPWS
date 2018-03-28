
 /***************************************************************
  *								*
  *	macro decode for stimulus control			*
  *								*
  *	mcmd_init(filename)					*
  *		name of stim control macro file			*
  *		resides in /usr/cassini/cfg			*
  *		default applied if this routine not called	*
  *		(or called with NULL argument)			*
  *								*
  *	mcmd_init(command_text)					*
  *		returns a double indicating the estimated	*
  *		execution time of a command			*
  *								*
  *	mcmd_main(command_text)					*
  *		text string to be translated and		*
  *		delivered to GPIB system.			*
  *		parameter substitution allowed			*
  *		(just like typed into "mcmd"			*
  *								*
  *	status return:						*
  *		0 indicates a problem (i.e. no macro file	*
  *					or no entry		*
  *		1 indicates something went out			*
  *								*
  *	ONLY "$" directives are processed (i.e. NO delays)	*
  *								*
  *	mcmd_gpib_help(index)					*
  *		returns text string with help text for		*
  *		'index'.  index should start at zero and	*
  *		increment until a null string is returned	*
  *	    index = -1 to cause help to print to screen		*
  *								*
  ***************************************************************/
extern int mcmd_init (char *);
extern int mcmd_main (char *);
extern double mcmd_dly (char *);
extern char *mcmd_gpib_help (int);

 /***************************************
  *	These entry points are for	* 
  *	exclusive use of mcmd		*
  ***************************************/
extern int mcmd_gpib (char *, char *);
extern int mcmd_misc (char *, char *);
