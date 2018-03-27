
 /***
  *	OFFSET FILE HANDLER
  *	use to obtain lines from the offset file in
  *	/usr/cassini/cfg
  *
  *	EPOCH=S/C epoch time
  *	OFFSET=offset (in seconds) from S/C to W/S
  *
  *	the utilo_epoch call decodes the time string:
  *		yyyy/doy-hh:mm:ss[dst]{tz}(sclk)<fudge>
  *		yyyy/mm/dd-hh:mm:ss[dst]{tz}(sclk)<fudge>
  *		yy/doy-hh:mm:ss[dst]{tz}(sclk)<fudge>
  *		yy/mm/dd-hh:mm:ss[dst]{tz}(sclk)<fudge>
  ***/
extern void utilo_init (char *);        /* offset file path */
extern char *utilo_char (char *);       /* returns string after keyword */
extern long utilo_int (char *);         /* returns integer after keyword */
extern double utilo_double (char *);    /* returns double after keyword */
extern long utilo_epoch (char *);       /* returns epoch after keyword */
extern int utilo_update (char *, char *, int);  /* update offset file */
