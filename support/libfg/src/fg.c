
/* fg - command line flag manipulation */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <Winsock2.h>
#endif


  /*
   * fg data structure 
   */
struct fg
{
  struct fg *link /* link to next element */ ;
  int index /* position index */ ;
  int value /* presence indicator "+-." */ ;
  char *text /* text value */ ;
  char *mnemonic /* mnemonic */ ;
};
/* static char ident[] = { "IDENT fg.c 16-June-2000" }; */
static struct fg *head;
struct fg *flag;
static int init = 0;
static char buf[256];
static char null_string[1] = { 0 };

static void fg_error (char *a1, char *a2, char *a3)
{
  fprintf (stderr, "%s:%s(%s)\n", a1, a2, a3);
  return;
}
static int fg_init (void)
{
  if (init)
    return 0;
  fg_error ("FG", "fg_init", "not initilized\n");
  return 1;
}

 /*
  * Dump internal data structures
  * * Prints, to stderr, the contents of the internal
  * * data structures.
  * * returns (void)
  */
void fg_dump (void)
{
  fprintf (stderr, "fg_dump\n");
  if (fg_init ())
    return;
  flag = head;
  fprintf (stderr, "    head %p\n\n", flag);
  while (flag != NULL) {
    fprintf (stderr, " ---  index %d\n", flag->index);
    fprintf (stderr, "      value %c\n", flag->value);
    fprintf (stderr, "       text [%s]\n", flag->text);
    fprintf (stderr, "    mnemonc [%s]\n", flag->mnemonic);
    fprintf (stderr, "\n");
    flag = flag->link;
  }
  return;
}

  /*
   * indicates presence of mnemonic on line
   * * Mnemonics may be of the for +mnemonic -mnemonic or mnemonic
   * * This call may be used as a true flag (if
   * * returns presence indicator "+", "-", or "."
   */
char fg_flag (const char *mne)
{                                       /* mnemonic (command line flag) */
  if (fg_init ())
    return 0;
  flag = head;
  while (flag != NULL) {
    if (!strcmp (flag->mnemonic, mne))
      return flag->value;
    flag = flag->link;
  }
  return 0;
}

  /*
   * Position on command line
   * * returns integer index
   */
int fg_flagi (char *mne)
{                                       /* mnemonic (command line flag) */
  if (fg_init ())
    return 0;
  flag = head;
  while (flag != NULL) {
    if (!strcmp (flag->mnemonic, mne))
      return flag->index;
    flag = flag->link;
  }
  return 0;
}

  /*
   * text string
   * * returns text argument
   */
char *fg_flagc (char *mne)
{                                       /* mnemonic (command line flag) */
  if (fg_init ())
    return 0;
  flag = head;
  while (flag != NULL) {
    if (!strcmp (flag->mnemonic, mne))
      return flag->text;
    flag = flag->link;
  }
  return null_string;
}

  /*
   * obtain mnemonic
   * * returns mnemonic
   */
char *fg_flagm (char *mne)
{                                       /* mnemonic (command line flag) */
  if (fg_init ())
    return 0;
  flag = head;
  while (flag != NULL) {
    if (!strcmp (flag->mnemonic, mne))
      return flag->mnemonic;
    flag = flag->link;
  }
  return null_string;
}

  /*
   * integer value
   * * returns integer argument
   */
long fg_int (char *mne /* mnemonic (command line flag) */ ,
             int def)
{                                       /* default value */
  char ch;

  if (fg_init ())
    return 0;
  ch = fg_flag (mne);
  if (ch != '-')
    return def;
  return strtol (fg_flagc (mne), NULL, 0);
}

  /*
   * floating point value
   * * returns floating point argument
   */
float fg_float (char *mne /* mnemonic (command line flag) */ ,
                float def)
{                                       /* default value */
  char ch;
  float result;

  if (fg_init ())
    return 0;
  ch = fg_flag (mne);
  if (ch != '-')
    return def;
  result = atof (fg_flagc (mne));
  return result;
}

  /*
   * insert command line flag
   * * This routine is used to insert a command line argument into
   * * the internal data structure so that subsequent calls will
   * * act as though this were entered on the command line 
   * * returns (void)
   */
void fg_flagx (char *arg1,              /* command line flag */
               char *arg2)
{                                       /* command line valuse */
  struct fg *newfg;
  char sign = 0;

  if (fg_init ())
    return;
  if (arg1) {
    if (arg1[0] == '+')
      sign = '+';
    if (arg1[0] == '-')
      sign = '-';
    newfg = malloc (sizeof (struct fg));
    if (newfg == NULL) {
      fg_error ("FG", "fg_flagx", "out of memory *fg");
      exit (0);
    }
    newfg->link = head;
    head = newfg;

    newfg->mnemonic = malloc (strlen (arg1) + 1);
    if (newfg->mnemonic == NULL) {
      fg_error ("FG", "fg_flagx", "out of memory *mnemonic");
      exit (0);
    }
    strcpy (newfg->mnemonic, arg1);

    newfg->text = malloc (strlen (arg2) + 1);
    if (newfg->text == NULL) {
      fg_error ("FG", "fg_flagx", "out of memory *text");
      exit (0);
    }
    strcpy (newfg->text, arg2);

    if (sign) {
      strcpy (newfg->mnemonic, &arg1[1]);
      newfg->value = sign;
      newfg->index = 0;
    } else {
      strcpy (newfg->mnemonic, arg1);
      newfg->value = '.';
      newfg->index = 0;
    }
  }
  return;
}

  /*
   * Initialization routine.
   * This routine must be called at the start to decode and
   * save parameters.
   * returns (void)
   */
void fg_flags (int argc,                /* command line argument count */
               char *argv[])
{                                       /* command line arguments */
  char sign;
  struct fg *newfg;
  int i;

  head = NULL;
  init = 1;
  for (i = 1; i < argc; i++) {
    sign = 0;
    if (argv[i][0] == '+')
      sign = '+';
    if (argv[i][0] == '-')
      sign = '-';
    newfg = malloc (sizeof (struct fg));
    if (newfg == NULL) {
      fg_error ("FG", "fg_flags", "out of memory *fg");
      exit (0);
    }
    newfg->link = head;
    head = newfg;

    newfg->mnemonic = malloc (strlen (argv[i]) + 1);
    if (newfg->mnemonic == NULL) {
      fg_error ("FG", "fg_flags", "out of memory *mnemonic");
      exit (0);
    }
    strcpy (newfg->mnemonic, argv[i]);

    newfg->text = &null_string[0];
    if (argc > i + 1) {
      newfg->text = malloc (strlen (argv[i + 1]) + 1);
      if (newfg->text == NULL) {
        fg_error ("FG", "fg_flags", "out of memory *text");
        exit (0);
      }
      strcpy (newfg->text, argv[i + 1]);
    }

    if (sign) {
      strcpy (newfg->mnemonic, &argv[i][1]);
      newfg->value = sign;
      newfg->index = i;
    } else {
      strcpy (newfg->mnemonic, argv[i]);
      newfg->value = '.';
      newfg->index = i;
    }
  }
  return;
}

  /*
   *    fg_fgets attempts to obtain a string from stdin
   *    it will wait up to 'delay' milliseconds for data
   *    to arrive before returning:
   *    NULL string indicates no data
   *    otherwise the string is returned...
   *    will return 0x0D if null string on stdin
   */
char *fg_gets (int delay)
{                                       /* delay in milliseconds */
  fd_set readfs = {{0}};
  struct timeval timeout;

  timeout.tv_sec = delay / 1000;
  timeout.tv_usec = (delay % 1000) * 1000;
  FD_SET (0, &readfs);

  select (1, &readfs, 0, 0, &timeout);

  if (FD_ISSET (0, &readfs)) {
    gets (buf);
    if (!strlen (buf)) {
      buf[0] = 0x0D;
      buf[1] = 0;
    }
    return buf;
  }
  return NULL;
}
