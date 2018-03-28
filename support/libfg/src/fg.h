 /*
  *                             command line flags 
  *
  *     Call "fg_flags" with argc/argv to initialize
  */
extern void fg_flags (int, char *[]);   /* initialize the parser */
extern void fg_flagx (char *, char *);  /* fake a command line item */
extern void fg_dump (void);             /* dump the parser */
extern char fg_flag (const char *);           /* + - or . */
extern int fg_flagi (char *);           /* position on line (i.e. argv index) */
extern char *fg_flagc (char *);         /* character string following flag */
extern char *fg_flagm (char *);         /*  flag */
extern int fg_int (char *, int);        /* integer following flag */
extern float fg_float (char *, float);  /* float following flag */
extern char *fg_gets (int);             /* get string from stdin */
