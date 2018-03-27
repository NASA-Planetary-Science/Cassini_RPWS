#include <stdlib.h>
#include <stdio.h>

static char *Version = { "V1.0" };

  /**********************************************************************
   *            Extract status fields from mini-packet                  *
   *    control1 and control2 are select specifiers where               *
   *            bits 28-30      accomodate split bit fields             *
   *            bits 16-27      index into minipacket (byte offset)     *
   *            bits 8-15       contain a shift count                   *
   *            bits 0-7        contain a select mask                   *
   *    control2 will be zero when the field is 1 to 8 bits long        *
   *            or will specify MSB bits                                *
   *                                                                    *
   *            Extract the specified bits from the minipacket,         *
   *            shifting and masking to get the bit field justified     *
   *            into the low bits of an integer.  Up to 16 bits of      *
   *            status may be extracted at one time                     * 
   *                                                                    *
   *            We hope this makes field selection in the rest          *
   *            of the program a little easier to follow through        *
   *            the use of appropriate header files that                *
   *            define the correct patterns for control1/control2       *
   *                                                                    *
   *      All of the HFR_* patterns used for control1/control2          *
   *    arguments are taken from the "HFR Header" that came from        *
   *    Pierre...                                                       *
   *                                                                    *
   **********************************************************************/
int get_status (unsigned char *mp, int control1, int control2)
{
  int index;
  int shift;
  int reshift;
  int mask;
  int result;

  reshift = (control1 >> 28) & 0x7;
  index = (control1 >> 16) & 0xFFF;
  shift = (control1 >> 8) & 0xFF;
  mask = (control1 >> 0) & 0xFF;
  result = (mp[index] >> shift) & mask;
  result &= 0xFF;
  if (control2) {
    reshift = (control2 >> 28) & 0x7;
    index = (control2 >> 16) & 0xFFF;
    shift = (control2 >> 8) & 0xFF;
    mask = (control2 >> 0) & 0xFF;
    if (!reshift)
      reshift = 8;
    result |= ((mp[index] >> shift) & mask) << reshift;
  }
  result &= 0x0000FFFF;
  return result;
}

int get_status_32 (unsigned char *mp, int control[4], int Line)
{
  int reshift;
  int index;
  int shift;
  int mask;
  int result;
  int i;
  volatile int line = Line;

  if (!mp) {
    fprintf (stderr, "%s/get_status_32 (calling line %d) no data buffer\n",
             __FILE__, Line);
    return -1;
  }
  if (!control) {
    fprintf (stderr, "%s/get_status_32 (calling line %d) no control buffer\n",
             __FILE__, Line);
    return -1;
  }
  result = 0;
  for (i = 0; i < 4; i++) {
    reshift = (control[i] >> 28) & 0x7;
    index = (control[i] >> 16) & 0xFFF;
    shift = (control[i] >> 8) & 0xFF;
    mask = (control[i] >> 0) & 0xFF;
    if (!reshift)
      reshift = 8;
    result |= ((mp[index] >> shift) & mask) << (i * 8);
  }
  return result;
}

  /**********************************************************************
   *            INsert status fields into archive record                *
   *    control is the select specifiers where (same as get_status)	*
   *            bits 16-23      index into field (byte offset)	        *
   *            bits 8-15       contain a shift count                   *
   *            bits 0-7        contain a select mask                   *
   *                                                                    *
   **********************************************************************/
unsigned long set_status (unsigned char *status, int control, int pattern)
{
  int index;
  int shift;
  int reshift;
  int mask;
  int result;

  reshift = (control >> 28) & 0x7;
  index = (control >> 16) & 0xFFF;
  shift = (control >> 8) & 0xFF;
  mask = (control >> 0) & 0xFF;

  result = status[index];               /* fetch */
  result &= ~(mask << shift);           /* pre clear all mask bits */
  result |= (pattern & mask) << shift;  /* set only masked pattern */
  status[index] = result;
  return result;
}
