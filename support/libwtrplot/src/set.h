
 /**************************************
  *                                    *
  *	Misc routines		       *
  *                                    *
  **************************************/
void set_error (const char *,           /*  module name   */
                const char *,           /*  routine name  */
                const char *);          /*  error text    */
void set_warn (const char *,            /*  module name   */
               const char *,            /*  routine name  */
               const char *);           /*  error text    */
void set_information (const char *,     /*  module name   */
                      const char *,     /*  routine name  */
                      const char *);    /*  error text    */
void set_dump (void);                   /* DEBUG, dumps a bar */
