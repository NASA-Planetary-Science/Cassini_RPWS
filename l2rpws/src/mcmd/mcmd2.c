#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include "pcrtiu.h"
#include "util.h"
#include "fg.h"

#define TRANS 256
#define BUFSZ 511

static char *argv_0;
extern int mcmd_gpib (char *, char *);
extern int mcmd_misc (char *, char *);
extern void mcmd_gpib_help (void);

char path[256] = { "/usr/cassini/cfg/" };
struct trans
{
  struct trans *link;
  int valid;                            /* 0 = end of list */
  char token_type;                      /* =, &, * or $ */
  char mnemonic[16];
  char target[128];
};
static struct trans *head = NULL;

static int disp_tr (struct trans *tr, int flag)
{
  if (flag)
    fprintf (stderr, "\ndisp_tr\n");
  fprintf (stderr, "  link         %p\n", tr->link);
  fprintf (stderr, "  valid        %i\n", tr->valid);
  fprintf (stderr, "  token_type   %c\n", tr->token_type);
  fprintf (stderr, "  mnemonic(%d) %s\n", strlen (tr->mnemonic),
           tr->mnemonic);
  fprintf (stderr, "  target       %s\n", tr->target);
  return 1;
}

  /*
   *    There is NO mechanism to release memory
   *    once allocated.  MULTIPLE CALLS to init_tr
   *    are intended to aid in debugging macro
   *    control file, and should not be used in
   *    production!!!!!!!
   */
static int init_tr (int flag)
{
  char buf[BUFSZ + 1];
  char *tok;
  FILE *trn;
  char *newpath;
  char type[] = { "r" };
  int count = 0;
  int i;
  struct trans *link;
  head = malloc (sizeof (struct trans));
  link = head;
  head->link = NULL;
  trn = fopen (path, type);
  if (!trn) {
    fprintf (stderr, "macro file not found: %s\n", path);
    exit (0);
  }
  while (fgets (buf, BUFSZ, trn)) {
    buf[strlen (buf) - 1] = 0;

    if (tok[0] != '#') {
      link->token_type = 0;
      for (i = 0; i < strlen (buf); i++)
        if (buf[i] == '\t')
          buf[i] = ' ';
      if (strstr (buf, "="))
        link->token_type = '=';
      else if (strstr (buf, "$"))
        link->token_type = '$';
      else if (strstr (buf, "&"))
        link->token_type = '&';
      else if (strstr (buf, "*"))
        link->token_type = '*';

      tok = strtok (buf, "*&$=");
      strcpy (link->mnemonic, tok);
      if (link->mnemonic[0] == 0x20)
        link->mnemonic[0] = 0;
      strcpy (link->target, &buf[strlen (tok) + 1]);
      link->link = malloc (sizeof (struct trans));
      link = link->link;
      count = count + 1;
    }
  }
  fclose (trn);
  if (flag) {
    fprintf (stderr, "init_tr(%s)\n", path);
  }
  return count;
}
static struct trans *find_tr (char *token)
{
  struct trans *link;

  link = head;
  if (strlen (token))
    while (link->link) {
      if (!strcasecmp (token, link->mnemonic))
        return link;
      link = link->link;
    }
  return NULL;
}
static char *target_tr (struct trans *link, char *token_typ)
{
  if (link == NULL)
    return NULL;
  token_typ[0] = link->token_type;
  return link->target;
}
static char *next_tr (struct trans *link, char *token_typ)
{
  if (link == NULL)
    return NULL;
  if (strlen (link->mnemonic))
    return NULL;
  token_typ[0] = link->token_type;
  return link->target;
}
static void message (char *text, int flag)
{
  char mssg[] = { "mcmd: " };
  if (flag) {
    fputs (mssg, stderr);
    fputs (text, stderr);
  }
  return;
}
char *substitute (char *target, char *token[])
{
  static char buf[BUFSZ + 1];
  char *tok0, *tok, ch;
  int to, from, i;
  char delim[] = { "%" };
  memset (buf, 0, BUFSZ + 1);

  buf[0] = target[0];
  to = 1;
  for (from = 1; from < strlen (target); from++) {
    if (target[from - 1] == '%') {
      ch = target[from] - '0';
      for (i = 0; i < strlen (token[ch]); i++) {
        buf[to++] = token[ch][i];
      }
    } else if (target[from] != '%')
      buf[to++] = target[from];
  }

  if (buf[strlen (buf) - 1] != '\n')
    buf[strlen (buf)] = '\n';
  return buf;
}
static void build_token (const char *buf_in, char *token[8])
{
  static char delim[] = { " ,\n" };
  static char buf[BUFSZ + 1];
  int i;

  strcpy (buf, buf_in);
  for (i = 0; i < 8; i++)
    token[i] = NULL;
  token[0] = strtok (buf, delim);
  for (i = 1; i < 8; i++) {
    if (token[i - 1]) {
      token[i] = strtok (NULL, delim);
    }
  }
  return;
}
int mcmd_fgets (int argc, char *argv[], char *buf)
{
  static int first = 1;
  int i;
  char *chr;

  if (!fg_flag ("cmdline")) {
    chr = fgets (buf, BUFSZ, stdin);
    for (i = 0; i < strlen (chr); i++)
      if (chr[i] == '\t')
        chr[i] = ' ';
    if (chr[0])
      return 1;
    else
      return 0;
  } else if (first) {
    memset (buf, 0, BUFSZ);
    for (i = fg_flagi ("cmdline") + 1; i < argc; i++) {
      strcat (buf, argv[i]);
      strcat (buf, " ");
    }
    first = 0;
    if (buf[0])
      return 1;
    else
      return 0;
  } else
    return 0;
}
void mcmd_log (char *text)
{
  /*
   * UTIL_stmlog(argv_0, NULL, NULL, text); /*
   */
  return;
}
int mcmd_help (void)
{
  if (fg_flag ("help")) {
    fprintf (stderr, " mcmd help   (Version 3.0)\n");
    fprintf (stderr, "   -- Macro Expander --\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "     -mne xxx  macro file\n");
    fprintf (stderr, "               %s\n", path);
    fprintf (stderr, "     -cmdline  one time execution (for scripts)\n");
    fprintf (stderr, "               must be last param on line\n");
    fprintf (stderr, "     -stim     read from stim control file\n");
    fprintf (stderr, "     -nosend   supress automaitc -send\n");
    fprintf (stderr, "     parameter substitution like DOS\n");
    fprintf (stderr, "         i.e. %$1 %2 %3, etc.\n");
    fprintf (stderr, "     un-recgonized commands passed\n");
    fprintf (stderr, "         through stdout\n");
    fprintf (stderr, "     mcmd decodes ONE directive internally\n");
    fprintf (stderr, "          to allow reloading of the control\n");
    fprintf (stderr, "          tables.  The directive is sensitive\n");
    fprintf (stderr, "          to the '-stim' modifier\n");
    fprintf (stderr, "        mcmd_reload    reloads cmd.mne\n");
    fprintf (stderr, "        stim_reload    reloads stim.mne\n");
    fprintf (stderr, "\n");
    mcmd_gpib_help ();
    return 1;
  } else
    return 0;
}
void main (int argc, char *argv[])
{
#define TOKENS 8
  static char token_type;
  static char *target;
  static char *trans;
  static char buf[BUFSZ + 1];
  static char *token[TOKENS] = { TOKENS * NULL };
  static char send[] = { "-send\n" };   /* "send\n" */
  static int trcnt;
  static int flag = 0;
  static int reload;
  static struct trans *xlt;

  argv_0 = argv[0];
  fg_flags (argc, argv);
  if (fg_flag ("nosend"))
    send[0] = 0;
  if (fg_flag ("mne"))
    strcat (path, fg_flagc ("mne"));
  else if (fg_flag ("stim")) {
    strcat (path, "stim");
    send[0] = 0;
  } else
    strcat (path, "cmd");
  strcat (path, ".mne");
  if (mcmd_help ())
    exit (0);
  if (init_tr (0)) {
    while (mcmd_fgets (argc, argv, buf)) {
      buf[strlen (buf) - 1] = 0;
      build_token (buf, token);
      xlt = find_tr (token[0]);
      if (xlt) {
        target = target_tr (xlt, &token_type);
        if (target) {
          trans = substitute (target, token);   /* %1, %2 substitution */
          switch (token_type) {
           case '=':
             fputs (trans, stdout);
             fflush (stdout);
             break;
           case '$':
             mcmd_log (trans);
             mcmd_gpib (trans, buf);
             break;
           case '&':
             mcmd_misc (trans, buf);
             break;
           case '*':
             break;
           default:
             break;
          }
          message (target, flag);
        }
        target = next_tr (xlt->link, &token_type);
        while (target) {
          xlt = xlt->link;
          if (target) {
            trans = substitute (target, token); /* %1, %2 substitution */
            switch (token_type) {
             case '=':
               fputs (trans, stdout);
               fflush (stdout);
               break;
             case '$':
               mcmd_log (trans);
               mcmd_gpib (trans, buf);
               break;
             case '&':
               mcmd_misc (trans, buf);
               break;
             case '*':
               break;
             default:
               break;
            }
            message (target, flag);
          }
          target = next_tr (xlt->link, &token_type);
        }
      } else {
        if (strlen (buf)) {             /* not in transaction table */
          reload = 0;
          if (!strcasecmp (buf, "stim_reload"))
            if (fg_flag ("stim"))
              reload += init_tr (1);
          if (!strcasecmp (buf, "mcmd_reload"))
            reload += init_tr (1);
          if (!reload) {
            strcat (buf, "\n");
            fputs (buf, stdout);
            fflush (stdout);
            message (buf, flag);
          }
        }
      }
      if (send) {
        fputs (send, stdout);
        fflush (stdout);
      }
    }
    fputs ("-exit\n", stdout);
    fflush (stdout);
    return;
  } else
    fprintf (stderr, "No menmonic file: %s\n", path);
}
