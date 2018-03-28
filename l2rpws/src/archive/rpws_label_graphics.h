struct GRAPHICS_TEXT
{
  char *upper_text[6];
  char *lower_text[6];
  char *left_text[12];
  char *right_text[12];
};

#ifndef _rpws_label_graphics_
int gr_circle (float x, float y, float r);
int gr_main (int flag, struct GRAPHICS_TEXT *text, char *directory);
#endif
