#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int main (int argc, char *argv[])
{
  char last[1024] = { "" };
  char this[1024];

  while (gets (this)) {
    if (!strcmp (this, last))
      printf ("%s\n", this);
    strcpy (last, this);
  }

  exit (0);
}
