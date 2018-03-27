#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *Version = { "V1.0" };
static char *Version_0 = { VERSION_0 };

int main (int argc, char *argv[])
{
  char buffer[4097];

  printf ("  static char *image_logo[] = {\n");
  while (gets (buffer)) {
    if (buffer[strlen (buffer) - 1] < 0x20)
      buffer[strlen (buffer) - 1] = 0;
    printf ("        \"%s\\n\",\n", buffer);
  }
  printf ("        NULL };\n");
}
