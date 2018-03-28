#define PEN_DOT  4
#define PEN_UP   3
#define PEN_DOWN 2
#define PEN_TOGGLE 1
#define BINARY 1
#define TEXT 0
 /*
  *     PLotter initilization strings
  */
extern int plot_init (FILE *, int);     /* PLT_INInn */
extern int plot_pcl_init (FILE *, int); /* PLT_PCLnn */
extern int plot_clear (FILE *, int);    /* PLT_CLRnn */
extern int plot_page (FILE *, int);     /* PLT_PGEnn */
extern int plot_delay (int);            /* PLT_DLY */
extern FILE *plot_fopen (const char *, int);    /* plot output file */
