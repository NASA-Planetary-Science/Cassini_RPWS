
void main (void)
{
  int i;
  double f, ang;
  char buf1[16384];
  short buf2[8192];

  uplay_file ("/usr/cassini/audio/alf_L.wav");
  printf ("------ /usr/cassini/audio/alf_L.wav\n");
  uplay_file ("/usr/cassini/audio/alffail.wav");
  printf ("------ /usr/cassini/audio/alffail.wav\n");


  for (i = 0; i < 16384; i++) {
    ang - i;
    ang = ang / 50.0;
    buf1[i] = f * 100;
    buf1[i] = i % 0x7F;
    buf1[i] = buf1[i] + 0x40;
  }
  uplay_8_bit (buf1, 8100);
  printf ("------ 8 bit data\n");
  return;

  for (i = 0; i < 8192; i++) {
    ang - i;
    ang = ang / 50.0;
    buf2[i] = f * 100;
    buf2[i] = i % 0xFF;
  }
  uplay_12_bit (buf2, 8192);
  printf ("------ 12 bit data\n");
  uplay_wait ();
  return;
}
