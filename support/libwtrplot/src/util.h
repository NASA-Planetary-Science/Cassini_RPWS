
 /************************************************
  *                                              *
  *	Misc. utility routines                   *
  *	Text processing				 *
  *                                              *
  *	'token array'                            *
  *		string pointers that point to    *
  *		upper case tokens from           *
  *			input string		 *
  ************************************************/
#define util_tok_items 8

#ifndef UTIL

     /***************************************
      *  this routine copies and converts   *
      *  input string to uppercase.  NULL   *
      *  fills all of the 'whitespace' and  *
      *  loads the token array with address *
      *  of tokens stored in the internal   *
      *  array				    *
      ***************************************/
extern int util_tok (char *[],          /* token array          */
                     const char *,      /* input string         */
                     const char);       /* delimiter flag       */

                                                /*
                                                 * g for GCMD.DAT      
                                                 */
                                                /*
                                                 * m for MCMD.DAT       
                                                 */

     /*****************************************
      *  this routine uses the input string   *
      *  loads the token array with address   *
      *  of tokens stored in the input string *
      *  such that each token points to       *
      *  remaining portion of input string    *
      *****************************************/
extern int util_txt (char *[],          /* token array          */
                     const char *);     /* input string         */

      /******************************************
       *      return an environment variable    *
       ******************************************/
extern char *util_env (char *);         /* variable name     */
extern char *util_envd (char *, char *);        /* variable name     */
extern char *util_stg (char *);         /* binary hacks     */

        /****************************************
	 *	Filename processing		*
	 ****************************************/
extern char *util_fpath (char *);
extern char *util_fname (char *);
extern char *util_sname (char *);

        /****************************************
	 *     PORTABLILITY & NUMERIC		*
	 ****************************************/
extern int util_ctlz (char *);
extern int util_numeric (char *);
extern float Max (float, float);
extern float Min (float, float);
extern int iMax (int, int);
extern int iMin (int, int);
extern void util_strupr (char *);
extern char *util_strupr1 (char *);
extern int util_strncmpi (char *, char *, int);
extern int util_strcmpi (const char *, char *);
extern float util_fabs (float);
extern int util_iabs (int);
extern char *UTIL_filetype;
#endif
