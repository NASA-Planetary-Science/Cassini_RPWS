#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>

#define TRANS 256
#define BUFSZ 511
#define TOKENS 8

extern int mcmd_gpib (char *, char *);
extern int mcmd_misc (char *, char *);
extern void mcmd_gpib_help (void);

static int first = 1;
char path[256] = { "/usr/cassini/cfg/" };
char stim[] = { "stim.mne" };

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
static int init_tr (void)
{
  char buf[BUFSZ + 1];
  char *tok;
  FILE *trn;
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

      tok = strtok (buf, "&$=");
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
int mcmd_init (char *fname)
{
  if (first) {
    first = 0;
    if (fname)
      strcat (path, fname);
    else
      strcat (path, stim);
    if (!init_tr ()) {
      fprintf (stderr, "No menmonic file: %s\n", path);
      return 0;
    }
  }
  return 1;
}
int mcmd_main (char *buf)
{
  struct trans *xlt;
  int trcnt;
  int flag = 0;
  int index = 0;
  char token_type;
  char *target;
  char *trans;
  char *token[TOKENS] = { TOKENS * NULL };

  if (!mcmd_init (NULL))
    return 0;

  build_token (buf, token);
  xlt = find_tr (token[0]);
  if (xlt) {
    target = target_tr (xlt, &token_type);
    if (target) {
      trans = substitute (target, token);       /* %1, %2 substitution */
      switch (token_type) {
       case '$':
         mcmd_gpib (trans, buf);
         index += 1;
         break;
       case '=':
         mcmd_main (trans);
         break;
       default:
         break;
      }
    }
    target = next_tr (xlt->link, &token_type);
    while (target) {
      xlt = xlt->link;
      if (target) {
        trans = substitute (target, token);     /* %1, %2 substitution */
        switch (token_type) {
         case '$':
           mcmd_gpib (trans, buf);
           index += 1;
           break;
         case '=':
           mcmd_main (trans);
           break;
         default:
           break;
        }
      }
      target = next_tr (xlt->link, &token_type);
    }                                   /*while(target) */
    return index;
  } /*if(xlt) */
  else
    return 0;

}                                       /*mcmd_main */
