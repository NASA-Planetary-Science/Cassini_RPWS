#include <stdio.h>
#include "das.h"
#include "timestruct.h"

int main(int nArgs, char** args) {

  char * start;
  char * end;
  TimeStruct ts;
  double dstart, dend, diff;
  double date;

  if (nArgs >= 3) {
      start = args[1];
      end = args[2];

      parsetime(start, UNPACK(ts));
      dstart = ttime(UNPACK(ts));
      parsetime(end, UNPACK(ts));
      dend = ttime(UNPACK(ts));

      diff = dend - dstart;

      printf("'%s' - '%s' = %f\n", start, end, diff);
  }

  parsetime("2000-001", UNPACK(ts));
  date = ttime(UNPACK(ts));
  printf("%f\n", date);

  return 0;
}
