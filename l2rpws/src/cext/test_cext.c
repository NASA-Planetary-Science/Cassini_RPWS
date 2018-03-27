#include <stdio.h>
#include <stdlib.h>

#include "Cext.c"

int main (int argc, char *argv[])
{
  char *pStr;
  int nTotal;
  double dOne, dTwo, dThree, dFour, dFive;
  FILE *hHandle = stdin;

/*  hHandle=fopen("test.txt","rt");
*/
  nTotal = 1;
  dOne = dTwo = dThree = 0.0;
  while (nTotal > 0) {
    nTotal =
      LineParseNumbers (hHandle, NULL, 5, &dOne, &dTwo, &dThree, &dFour,
                        &dFive);
    fprintf (stderr,
             "%d :: One=%#-4.4E, Two=%#-4.4E, Three=%#-4.4E,"
             "Four=%#-4.4E, Five=%#-4.4E\n", nTotal, dOne, dTwo, dThree,
             dFour, dFive);
    dOne = dTwo = dThree = dFour = dFive = 0.0;
  }

  return 1;
}
