#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
/* #include <curses.h> */
#include <math.h>

#include <rtiu.h>
#include <util.h>


#define EOF_REACHED        -1   /** If UTIL_getbuffer_CDS **/

/*
	define telemetry data structures
*/
struct bigbuf
{
  struct RPWS_buffer big1;
  struct RPWS_buffer big2;
};
union
{
  struct RPWS_buffer rbuffer;
  struct MP_buffer buffer;
  struct bigbuf banana;
} m;
struct RPWS_buffer *r_buffer;

/* define LFDR simulation variables */
static short N, N2, NU, Nnew, DCVal;
static short Scale_Factor;
static short Sine[1024];                /* dynamic sine lookup table */
static short Map[33] =
  { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  17, 18, 19, 21, 25, 29, 34, 39, 46, 53, 62, 72, 84, 98, 115, 133, 0
};
float counts[256];

int Ifft (short *xreal, short *ximag, int n);

short Ibitrev (short i, short j);

int build (int lenw);

void prescale (short *data);

void getdc (short *data);

void Hanning (short *data);

short getamp (short *xreal, short *ximag, unsigned char *Tbuf);

unsigned char LogCmp (unsigned short value);

void fake_lfdr ();

void byte_print (unsigned char byte_array[]);

int main (int argc, char **argv)
{
  long option;                          /* blocking or non-blocking file read */
  int CH, FB, mp_len, iant;


/***************************************************************/

  if (argc < 2)
    option = UTIL_GET_BLOCKING;
  else if (!strcmp (argv[1], "+eof"))
    option = UTIL_GET_NON_BLOCKING;

  r_buffer = malloc (65 * 1024);
  while ((UTIL_getbuffer_MP (&m.buffer, stdin, option)) != EOF_REACHED) {
    memcpy (r_buffer, &m.buffer, sizeof (struct RPWS_buffer));
    UTIL_putbuffr2_RPWS (r_buffer, stdout, r_buffer->f_length);

/*
	See if we have LoBand WFR data. If so, fake an LFDR packet also.
*/

/*
	Include Hiband also. 30-Apr-2003 TFA
*/
    if ((PACKET_TYPE_wfr == UTIL_extract_MP_type (&m.buffer)))

/*
        (((m.buffer.packet.mpx.mini_packet[5]>>7)&0x1) == 0) )
*/
    {
      FB = (m.buffer.packet.mpx.mini_packet[5] >> 7) & 0x1;     /* band */
      CH = (m.buffer.packet.mpx.mini_packet[7] >> 3) & 0x7;     /* channel  */
      mp_len = UTIL_MP_length (&m.buffer);
      iant = m.buffer.packet.mpx.mini_packet[6] & 0x07;
      if (((CH == 0) || (CH == 2)) &&   /* Ex or Bx */
          ((mp_len == 0x0405) ||        /* only nice sizes */
           (mp_len == 0x0805) ||
           (mp_len == 0x1005)) &&
          (((CH == 0) && ((iant & 1) == 1)) ||
           ((CH == 2) && ((iant & 4) == 0)))) {
        fake_lfdr ();
        UTIL_putbuffr2_RPWS (r_buffer, stdout, r_buffer->f_length);
      }
    }
  }                                     /*while */
  
  return 0;
}

void fake_lfdr ()
{
  int epoch;
  int inst_type, mp_len, chan, band, stat0, stat1;
  int i, j, idx, RTI, igain, agc, iant;
  int year, month, day, doy, hr, mn, mon, mday;
  short dc, Nbytes, ch_bits;
  short wfrdata, nsamp, nlast = 0;
  short xreal[1024], ximag[1024];
  unsigned char Tbuf[38];               /* output buffer */
  double Dbuf[38];                      /* double precision output */
  double dreal[1024], dimag[1024];
  double ddc, dtemp, hahn[1024];
  double pkt_sec = 0;
  double sec;
  time_t pkt_sclk, pkt_epoc, start_sclk, stop_sclk;
  struct tm *pkt_event;
  struct event_time *evt_tim;
  struct event_clock evt_clk;
  int index, index2;

  chan = (m.buffer.packet.mpx.mini_packet[7] >> 3) & 0x7;
  mp_len = UTIL_MP_length (&m.buffer);
  iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
  if ((m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
    idx = 10;                           /* MSF bit set */
  else
    idx = 8;                            /* MSF bit clear */
  nsamp = 512;

  j = 0;
  for (i = idx; i < (idx + 2 * nsamp); (i = i + 2)) {
    xreal[j] = m.buffer.packet.mpp.mini_packet[i] |
      ((m.buffer.packet.mpp.mini_packet[i + 1] & 0x0f) << 8);
    j++;
  }

  if (nsamp != nlast) {
    build (nsamp);                      /* build sine lookup table */
    nlast = nsamp;
  }

  prescale (xreal);                     /* see if we can scale data up */
  getdc (xreal);                        /* subtract DC, scale data up */
  Hanning (xreal);                      /* apply Hanning window */
  Ifft (xreal, ximag, nsamp);           /* simulate LFDR FFT */

  Nbytes = getamp (xreal, ximag, Tbuf); /* get LFDR values */

  if (chan == 0)
    stat0 = 0x0A;
  else if (chan == 2)
    stat0 = 0x48;
  else
    stat0 = 0;                          /* not quite correct, but it will do */

  stat1 = m.buffer.packet.mpp.mini_packet[5] & 0xC0;    /* band,LPmode */
  if (chan == 0)                        /* get gain bits */
    stat1 = stat1 | ((m.buffer.packet.mpp.mini_packet[5] & 0x03) << 4);
  else if (chan == 1)
    stat1 = stat1 | ((m.buffer.packet.mpp.mini_packet[5] & 0x0C) << 2);
  else
    stat1 = stat1 | (m.buffer.packet.mpp.mini_packet[5] & 0x30);
  stat1 = stat1 | (Scale_Factor & 0x0F);

  r_buffer->f_length = 0x013C;
  r_buffer->record_type = DATA_MPC_LFDR;
  r_buffer->packet.index.data_start = 0x23;
  r_buffer->packet.index.data_length = 0x23;
  r_buffer->packet.mpx.mini_packet[0] = 0x70;
  r_buffer->packet.mpx.mini_packet[1] = 0x23;
  r_buffer->packet.mpx.mini_packet[4] = stat0 | 0x04;   /* bit indicates fake */
  r_buffer->packet.mpx.mini_packet[5] = stat1;
  for (i = 0; i < 32; i++)
    r_buffer->packet.mpx.mini_packet[i + 6] = Tbuf[i];
  for (i = 38; i < 48; i++)
    r_buffer->packet.mpx.mini_packet[i] = 0;

/*
 byte_print (r_buffer);
*/
}

void byte_print (unsigned char byte_array[])
{
  int i;

  for (i = 0; i < 0x140; i++) {
    if ((i % 16) == 0)
      fprintf (stdout, "%4.4X:", i);
    fprintf (stdout, " %2.2X", byte_array[i]);
    if ((i % 16) == 15)
      fprintf (stdout, "\n");
  }
}

int Ifft (short *xreal, short *ximag, int n)
{                                       /* Integer FFT */
  short N4, NU1, i, k, ll;
  short cval, sval, p;
  short a1, a2, b1, b2;
  short x1real[1024], x1imag[1024];
  int ttreal, ttimag;
  short treal, timag;

  N = N / 2;                            /* real-only fft */
  N4 = N / 4;
  NU--;
  for (i = 0; i < N; i++) {             /* shuffle data */
    xreal[i] = xreal[2 * i];
    ximag[i] = xreal[2 * i + 1];
  }

  N2 = N / 2;
  NU1 = NU - 1;
  k = 0;

/*
	Here starts the butterfly looping
*/
  for (ll = 0; ll < NU; ll++) {
    do {
      for (i = 0; i < N2; i++) {
        p = Ibitrev ((k >> NU1), NU);
        cval = Sine[2 * (p + N4)];      /* get cosine twiddle factor */
        sval = Sine[2 * p];             /* get sine twiddle factor */
        ttreal = ((int) xreal[k + N2] * (int) cval +
                  (int) ximag[k + N2] * (int) sval);
        ttreal = ttreal + ttreal;       /* shift up, sines scaled by 32767 */
        ttreal = ttreal + 0x8000;       /* round */
        treal = (int) ((ttreal >> 16) & 0xFFFF);        /* result is in MSW */
        ttimag = ((int) ximag[k + N2] * (int) cval -
                  (int) xreal[k + N2] * (int) sval);
        ttimag = ttimag + ttimag;       /* shift up, sines scaled by 32767 */
        ttimag = ttimag + 0x8000;       /* round */
        timag = (int) ((ttimag >> 16) & 0xFFFF);        /* result is in MSW */
        xreal[k + N2] = ((int) xreal[k] - (int) treal) / 2;
        ximag[k + N2] = ((int) ximag[k] - (int) timag) / 2;
        ximag[k] = ((int) ximag[k] + (int) timag) / 2;
        xreal[k] = ((int) xreal[k] + (int) treal) / 2;
        k++;
      }
      k = k + N2;
    } while (k < N);
    k = 0;
    NU1--;
    N2 = N2 >> 1;
  }

/*
	Now the data must be unscrambled.
*/
  for (k = 1; k <= N; k++) {            /* unscramble data */
    i = Ibitrev ((k - 1), NU) + 1;
    if (i > k) {
      treal = xreal[k - 1];
      timag = ximag[k - 1];
      xreal[k - 1] = xreal[i - 1];
      ximag[k - 1] = ximag[i - 1];
      xreal[i - 1] = treal;
      ximag[i - 1] = timag;
    }
  }

/*
	Now we must fiddle data since the N/2 complex FFT was performed
	on the real-only data.
*/

  N2 = N >> 1;
  for (i = 1; i < N; i++) {
    sval = Sine[i];
    cval = Sine[i + N2];
    a1 = ((int) xreal[i] + (int) xreal[N - i]);
    a2 = ((int) xreal[i] - (int) xreal[N - i]);
    b1 = ((int) ximag[i] + (int) ximag[N - i]);
    b2 = ((int) ximag[i] - (int) ximag[N - i]);
    ttreal = ((int) b1 * (int) cval - (int) a2 * (int) sval);
    ttreal = (ttreal >> 16);
    ttreal = ttreal + (a1 / 2);
    x1real[i] = (ttreal / 2);

    ttimag = ((int) b1 * (int) sval + (int) a2 * (int) cval);
    ttimag = (ttimag >> 16);
    ttimag = (b2 / 2) - ttimag;
    x1imag[i] = (ttimag / 2);
  }

/*
	Finally place data back in original arrays.
*/
  xreal[N] = (xreal[0] - ximag[0]) / 2;
  xreal[0] = (xreal[0] + ximag[0]) / 2;
  ximag[N] = 0;
  ximag[0] = 0;
  for (i = 1; i < N; i++) {
    xreal[i] = x1real[i];
    ximag[i] = x1imag[i];
  }
  N = N << 1;
  NU++;
  return (0);
}

short Ibitrev (short i, short j)
{
  short x, bit, count, result;

  result = 0;
  x = i;
  count = j;
  do {
    bit = x & 1;
    x = x >> 1;
    result = result << 1;
    result = result | bit;
    count--;
  } while (count != 0);
  return (result);
}

int build (int len)
{
  int i, index, word_offset;

/*
	128-sample 1/8-wave sine lookup table scaled by 32767.
*/
  short SineT[129] = {
    0, 201, 402, 603, 804, 1005, 1206, 1407,
    1608, 1809, 2009, 2210, 2410, 2611, 2811, 3012,
    3212, 3412, 3612, 3811, 4011, 4210, 4410, 4609,
    4808, 5007, 5205, 5404, 5602, 5800, 5998, 6195,
    6393, 6590, 6786, 6983, 7179, 7375, 7571, 7767,
    7962, 8157, 8351, 8545, 8739, 8933, 9126, 9319,
    9512, 9704, 9896, 10087, 10278, 10469, 10659, 10849,
    11039, 11228, 11417, 11605, 11793, 11980, 12167, 12353,
    12539, 12725, 12910, 13094, 13279, 13462, 13645, 13828,
    14010, 14191, 14372, 14553, 14732, 14912, 15090, 15269,
    15446, 15623, 15800, 15976, 16151, 16325, 16499, 16673,
    16846, 17018, 17189, 17360, 17530, 17700, 17869, 18037,
    18204, 18371, 18537, 18703, 18868, 19032, 19195, 19357,
    19519, 19680, 19841, 20000, 20159, 20317, 20475, 20631,
    20787, 20942, 21096, 21250, 21403, 21554, 21705, 21856,
    22005, 22154, 22301, 22448, 22594, 22739, 22884, 23027,
    23170
  };                                    /* PI/4 value */

  if (len == 1024) {
    N = 1024;
    NU = 10;
    word_offset = 1;                    /* use every sine entry */
  } else if (len == 512) {
    N = 512;
    NU = 9;
    word_offset = 2;                    /* use every other sine entry */
  } else if (len == 256) {
    N = 256;
    NU = 8;
    word_offset = 4;                    /* use every 4th sine entry */
  } else {                              /* default shall be 512 */
    N = 512;
    NU = 9;
  }
  Nnew = N;
  index = 0;
  for (i = 0; i < 129; i = i + word_offset) {
    Sine[index] = SineT[i];             /* load table from 0 thru PI/4 */
    index++;
  }
  for (i = index - 2; i >= 0; i--) {    /* load table from PI/4 thru PI/2 */
    Sine[index] =
      (short) (sqrt
               ((32767. * 32767.) - (double) (Sine[i]) * (double) (Sine[i])));
    index++;
  }
  for (i = index - 2; i >= 0; i--) {    /* load table from PI/2 thru PI/4 */
    Sine[index] = Sine[i];
    index++;
  }
  for (i = index - 2; i > 0; i--) {     /* load table from PI thru 2*PI */
    Sine[index] = -Sine[i];
    index++;
  }
  return (0);
}

void prescale (short *xreal)
{
  short Max_Val, Min_Val;
  short Max_Win, Min_Win, Delta;
  short i, counter;

  Max_Val = 0;
  Min_Val = 0x0FFF;
  for (i = 0; i < N; i++) {
    if (xreal[i] > Max_Val)
      Max_Val = xreal[i];
    if (xreal[i] < Min_Val)
      Min_Val = xreal[i];
  }
  Delta = 0x0200;
  Min_Win = 0x0400;
  Max_Win = 0x0C00;
  Scale_Factor = 0;
  counter = 0;
  do {
    if (Min_Win > Min_Val)
      break;
    if (Max_Win <= Max_Val)
      break;
    Scale_Factor++;
    Min_Win = Min_Win + Delta;
    Max_Win = Max_Win - Delta;
    Delta = Delta >> 1;
    counter++;
  } while (counter < 10);
  return;
}

void getdc (short *xreal)
{
  int Sum, Round, i, j, nu1;

  Sum = 0;
  for (i = 0; i < N; i++)
    Sum = Sum + (int) xreal[i];
  nu1 = NU;
  if (nu1 > 0) {
    do {
      nu1--;
      if (nu1 == 0)
        break;
      else
        Sum = Sum >> 1;
    } while (1);
  }
  Round = Sum & 1;
  Sum = (Sum >> 1) + Round;
  DCVal = Sum;                          /* have calculated DC offset */

/*
	Now subtract DC offset from all data items.
*/
  for (i = 0; i < N; i++)
    xreal[i] = xreal[i] - DCVal;

/*
	Now apply scale factor (DGF) to all data items.
*/
  for (i = 0; i < N; i++) {
    if (Scale_Factor > 0) {
      for (j = 0; j < Scale_Factor; j++)
        xreal[i] = xreal[i] + xreal[i];
    }
    xreal[i] = xreal[i] + xreal[i];     /* make sure data is 14-bits */
    xreal[i] = xreal[i] + xreal[i];     /* this will reserve 2 guard bits */
  }
  return;
}

void Hanning (short *xreal)
{
  int i, index, win;

/*
	Note that this Hanning window is not a symmetric window.
*/
  N2 = N / 2;
  index = 3 * N / 4;
  for (i = 0; i < N2; i++) {
    win = (0x7FFF + (int) Sine[index]) / 2;
    xreal[i] = ((((int) (2 * xreal[i]) * win) >> 16) & 0xFFFF);
    index--;                            /* move down the Sine wave to get the window function */
  }
  for (i = N2; i < N; i++) {
    win = (0x7FFF + (int) Sine[index]) / 2;
    xreal[i] = ((((int) (2 * xreal[i]) * win) >> 16) & 0xFFFF);
    index++;                            /* move up the Sine wave to get the window function */
  }
  return;
}

short getamp (short *xreal, short *ximag, unsigned char *Tbuf)

/*
	get amplitudes of intervals defined by Map
*/
{
  short i, j, p, BC;
  int Sum;
  short MP_Nout;
  unsigned short result;

  i = 0;                                /* summing index */
  p = 0;                                /* output index */
  MP_Nout = 0;                          /* output data counter */
  do {
    BC = (Map[p] - i);                  /* calculate Bin Counter */
    Sum = 0;
    for (j = 0; j < BC; j++) {
      i++;
      Sum = Sum + (int) xreal[i] * (int) xreal[i]
        + (int) ximag[i] * (int) ximag[i];      /* square & sum */
    }
    result = (int) (sqrt ((double) Sum)) & 0xFFFF;      /* take sqrt of sum of squares */
    Tbuf[p] = LogCmp (result);          /* log-compress result */
    p++;                                /* bump output pointer */
    MP_Nout++;                          /* bump output byte counter */
  } while (Map[p] != 0);
  return (MP_Nout);
}

unsigned char LogCmp (unsigned short value)
{
  unsigned char result;
  unsigned short A1, B1, B2;

  B1 = 0;                               /* Base */
  A1 = 0;                               /* Exponent */
  B2 = 32;                              /* Dbase */

  do {
    if (((int) value & 0x0000FFFF) < ((int) (B1 + B2) & 0x0000FFFF))
      break;                            /* done figuring */
    B1 = B1 + B2;                       /* figure new Base: 0,32,96,... */
    B2 = 2 * B2;                        /* figure new Dbase: 32,64,128,... */
    A1++;                               /* bump Exponent */
  } while (A1 < 8);

  if (A1 >= 8)
    return (255);                       /* value is out-of-range */
  else if (A1 == 0)
    return (value);                     /* Exponent is zero, Mantissa is value */
  else {
    result = (value - B1);
    result = result >> A1;
  }
  result = (result & 0x1F) | (A1 << 5); /* f.p. format is EEEMMMMM */
  return (result);
}
