#define PEN_DOT  4
#define PEN_UP   3
#define PEN_DOWN 2
#define JUST_LEFT  -1
#define JUST_CENTER 0
#define JUST_RIGHT  1

enum
{
  p_NONE = 0,
  p_RED = 1,
  p_GREEN = 2,
  p_BLUE = 3,
  p_CYAN = 4,
  p_MAGENTA = 5,
  p_YELLOW = 6,
  p_BLACK = 7,
  pm_BLACK = 8,
  pm_RED = 9,
  pm_GREEN = 10,
  pm_BLUE = 11,
  pm_CYAN = 12,
  pm_MAGENTA = 13,
  pm_YELLOW = 14,
  pf_BLACK = 15,
  pff_BLACK = 16,
  pf_RED = 17,
  pf_GREEN = 18,
  pf_BLUE = 19,
  pf_CYAN = 20,
  pf_MAGENTA = 21,
  pf_YELLOW = 22,
  pw_BLACK = 23,
  p_ORANGE = 24,
  p_PURPLE = 25,
  pm_ORANGE = 28,
  pf_ORANGE = 29
} p_colors;

#define p_PORTRAIT	1
#define p_LANDSCAPE	0

extern char *p_setplt (char *, char *, char *, char *, int);
extern int p_wrapup (void);
extern int p_plot (float, float, int);
extern int p_zinc (int, int);
extern int p_zrgb (int, float *);
extern int p_zcmyk (int, float *);
extern int p_zmark (char *);
extern int p_macro (float, float, float, char *);
extern int p_factor (float);
extern int p_symbol (float,             /* X            */
                     float,             /* Y            */
                     float,             /* height       */
                     const char *,      /* text string  */
                     float,             /* angle (not impl) */
                     int,               /* justification */
                     char *);           /* font select  */
extern int p_frame (void);
extern int p_insert (int count,         /* char count */
                     char *text);       /* text to insert in buffer */
