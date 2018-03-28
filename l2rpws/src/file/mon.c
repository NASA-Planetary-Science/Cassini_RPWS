static int UTIL_mon (int Doy, int Feb, int *Month, int *Day, int start_point)
{
  static int _month[13][2] =
    { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365,
    0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366
  };
  int index = 0;

  while (Doy >= _month[index][Feb])
    index++;
  index--;
  *Month = index + start_point;
  *Day = _month[index][Feb] + 1;
  return 0;
}

int main (int argc, char *argv[])
{
  static int _month[2][13] =
    { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365,
    0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366
  };
  int i, j;

  for (i = 0; i < 2; i++) {
    for (j = 0; j < 12; j++) {
      printf ("  %3d", _month[i][j]);
    }
    printf ("\n");
  }
}
