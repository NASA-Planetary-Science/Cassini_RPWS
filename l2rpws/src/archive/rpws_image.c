#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* #include "plot.h" */

#include "rpws_image_0.h"
#include "logo.h"

#define MAKE_CDROM_LABEL
#undef MAKE_CDROM_LABEL


static char *Version = { "V1.0" };

#ifdef MAKE_CDROM_LABEL
static char *image_1[] = {
  "stroke\n",
  "gsave\n",
  "% Translate for offset\n",
  "120.0 396.0 translate\n",
  ROTATE,
  SCALE,
  "/DeviceRGB setcolorspace\n",
  NULL
};

static char *image_2[] = {
  "stroke\n",
  "gsave\n",
  "% Translate for offset\n",
  "120.0 33.0 translate\n",
  ROTATE,
  SCALE,
  "/DeviceRGB setcolorspace\n",
  NULL
};

static char *image_3[] = {
  "stroke\n",
  "gsave\n",
  "% Translate for offset\n",
  "120.0 396.0 translate\n",
  ROTATE,
  SCALE,
  "/DeviceRGB setcolorspace\n",
  NULL
};

static char *image_4[] = {
  "stroke\n",
  "gsave\n",
  "% Translate for offset\n",
  "120.0 33.0 translate\n",
  ROTATE,
  SCALE,
  "/DeviceRGB setcolorspace\n",
  NULL
};

static char *image_99[] = {
  "grestore\n",
  "/DeviceCMYK setcolorspace\n",
  NULL
};
#endif

#ifdef MAKE_CDROM_LABEL
int rpws_image (int index)
{
  int len;
  int i = 0;

  switch (index) {
   case 1:
     i = 0;
     while (image_1[i]) {
       len = strlen (image_1[i]);
       p_insert (len, image_1[i]);
       i++;
     }
     break;
   case 2:
     i = 0;
     while (image_2[i]) {
       len = strlen (image_2[i]);
       p_insert (len, image_2[i]);
       i++;
     }
     break;
   case 3:
     i = 0;
     while (image_3[i]) {
       len = strlen (image_3[i]);
       p_insert (len, image_3[i]);
       i++;
     }
     break;
   case 4:
     i = 0;
     while (image_4[i]) {
       len = strlen (image_4[i]);
       p_insert (len, image_4[i]);
       i++;
     }
     break;
  }

  switch (index) {
   case 1:
   case 2:
     i = 0;
     while (image[i]) {
       len = strlen (image[i]);
       p_insert (len, image[i]);
       i++;
     }
     break;
   case 3:
   case 4:
     i = 0;
     while (image_logo[i]) {
       len = strlen (image_logo[i]);
       p_insert (len, image_logo[i]);
       i++;
     }
     break;
  }

  i = 0;
  while (image_99[i]) {
    len = strlen (image_99[i]);
    p_insert (len, image_99[i]);
    i++;
  }

  return i;
}
#else
int rpws_image (int index)
{
  return 1;
}
#endif
