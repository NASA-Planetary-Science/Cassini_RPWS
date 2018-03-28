#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define TOKENS 8
#define BUFSZ 256

extern void mcmd_delay (int);
enum
{ MISC_,
  MISC_TEXT,
  MISC_DELAY
};
static struct msc
{
  int index;
  char *mne;
} misc[] = {
MISC_DELAY, "DELAY", MISC_TEXT, "TEXT", MISC_, "", 0, ""};

static int build_token (const char *buf_in, char *token[TOKENS])
{
  static char delim[] = { " ,\t\n" };
  static char buf[BUFSZ + 1];
  int i = 0;
  int cnt = 1;

  strcpy (buf, buf_in);
  for (i = 0; i < 8; i++)
    token[i] = NULL;
  token[0] = strtok (buf, delim);
  for (i = 1; i < 8; i++) {
    if (token[i - 1]) {
      token[i] = strtok (NULL, delim);
      if (token[i])
        cnt = i + 1;
    }
  }
  return cnt;
}
static int decode (char *token)
{
  int i = 0;

  while (misc[i].index) {
    if (!strcmp (misc[i].mne, token))
      return misc[i].index;
    i++;
  }
  return -1;
}
int mcmd_misc (char *trans)
{
  char *token[TOKENS];
  char buf[256];
  int i, j, k;
  int val, cnt;

  cnt = build_token (trans, token);
  switch (decode (token[0])) {
   case MISC_TEXT:
     memset (buf, 0, strlen (trans));
     k = 0;
     for (i = 0; i < strlen (trans); i++)
       if (trans[i] == '"')
         for (j = i + 1; j < strlen (trans); j++)
           if (trans[j] != '"')
             buf[k++] = trans[j];
           else
             buf[k++] = 0;
     fprintf (stderr, "mcmd_misc \"%s\"\n", buf);
     break;
   case MISC_DELAY:
     val = strtol (token[1], NULL, 0);
     mcmd_delay (val);
     break;
   default:
     break;
  }
  return 0;
}
