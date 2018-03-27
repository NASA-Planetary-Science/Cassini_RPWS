#include <stdio.h>
#include <stdlib.h>

#include <rpwstlm/CasType.h>

static char sVersion[64] = "libCasTlm.a version 2.91";

char *CasTlm_Version (void)
{
  static char s[128];

  sprintf (s, "%s on %s @ %s", sVersion, __DATE__, __TIME__);
  return s;
}
