#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <pcrtiu.h>
#include <util.h>
#include "mcmd.h"

#define TRANS 256
#define BUFSZ 511

char path[256] = { "/usr/cassini/cfg/cmd.mne" };
struct trans
{
  struct trans *link;
  int valid;                            /* 0 = end of list */
  char token_type;                      /* =, & or $ */
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
static int init_tr (int argc, char *argv[])
{
  char buf[BUFSZ + 1];
  char *tok;
  FILE *trn;
  char *newpath;
  char type[] = { "r" };
  int count = 0;
  int i;
  struct trans *link;

  newpath = getenv ("mne");
  if (newpath)
    strcpy (path, newpath);
  head = malloc (sizeof (struct trans));
  link = head;
  head->link = NULL;
  trn = fopen (path, type);
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

  if (strcmp (argv[1], "-cmd")) {
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
    for (i = 2; i < argc; i++) {
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
int mcmd_help (int argc, char *argv[])
{
  if (!strcmp (argv[1], "-help")) {
    fprintf (stderr, " mcmd help   (Version 2.0)\n");
    fprintf (stderr, "   -- Macro Expander --\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "     -cmd    one time execution (for scripts)\n");
    fprintf (stderr, "     parameter substitution like DOS\n");
    fprintf (stderr, "         i.e. %$1 %2 %3, etc.\n");
    fprintf (stderr, "     un-recgonized commands passed\n");
    fprintf (stderr, "         through stdout\n");
    fprintf (stderr, "\n");
    mcmd_gpib_help (-1);
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
  static struct trans *xlt;

  if (mcmd_help (argc, argv))
    exit (0);
  if (init_tr (argc, argv)) {
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
             mcmd_gpib (trans, buf);
             break;
           case '&':
             mcmd_misc (trans, buf);
             break;
           case '*':
             break;                     /* not needed here */
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
        if (strlen (buf)) {
          strcat (buf, "\n");
          fputs (buf, stdout);
          fflush (stdout);
          message (buf, flag);
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
