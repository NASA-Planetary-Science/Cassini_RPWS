/*                                                                       */
/* written 10-Mar-1999 by TFA to read Cassini AACS Quaternion SFDU files */
/*                                                                       */
#include <math.h>
#include <stdio.h>
#include <time.h>
#include <utilt.h>

struct sfdu_header{
                char authority[4];
                short version_class;
                short spare;
                short ddp_id[2];
                long length_msw;
                long length;
                };
static char htxt[] = {"NJPL2I00C"};
int chdo_MATCH(char *hstg, int index_max);
int chdo_SYNC(char *primary_header, FILE *file);

int
main (int argc, char *argv[])
{
  int           items=130, i, good;
  short         length, result;
  char          *hdr="NJPL2I00";
  struct        event_time      *evt_tim;
  struct        tm *pkt_event;
  int           year, doy, mon, mday, hr, mn;
  short         quat1, quat2, quat3, quat4;
  double        quaternion[4], sumquat;
  double        matrix[3][3];
  double	phi, delta, omega;
  double        sec;
  unsigned      char    quat1_id[4] = { 0x08, 0x01, 0x03, 0xE9 };
  unsigned      char    quat2_id[4] = { 0x08, 0x01, 0x03, 0xEA };
  unsigned      char    quat3_id[4] = { 0x08, 0x01, 0x03, 0xEB };
  unsigned      char    quat4_id[4] = { 0x08, 0x01, 0x03, 0xEC };
  struct sfdu_header *primary_header;
  unsigned      char          *buf;
  char          *ibuf;
        
  buf = (unsigned char *) malloc (65536);
  if (!buf) return -1;
  primary_header = (struct sfdu_header *)buf;
  ibuf = (char *)buf;

  evt_tim = (struct event_time *) malloc (8);

 while (1)
 {
    do {
    if ( !chdo_SYNC (ibuf,stdin) )
      exit (-1);
    good=1;

    length = primary_header->length;               
    result = fread (buf+20, 1, length, stdin);      /* read rest of SFDU */
    if (!result)
        exit (-1);
        
    if (length != 0x006E)                     /* proper length ? */
    {
        fprintf (stderr, "Length L1 Bad\n");
        break;                          /* keep searching */
    }

    for (i=106; i<110; i++)
    {   if (*(buf+i) != quat1_id[i-106])
                good = 0;
    }
    for (i=112; i<116; i++)
    {   if (*(buf+i) != quat2_id[i-112])
                good = 0;
    }
    for (i=118; i<122; i++)
    {   if (*(buf+i) != quat3_id[i-118])
                good = 0;
    }
    for (i=124; i<128; i++)
    {   if (*(buf+i) != quat4_id[i-124])
                good = 0;
    }
    
    if (good)
    {
      evt_tim->days = (((unsigned long)buf[72])*256) | ((unsigned long)buf[73]);

      evt_tim->milliseconds =   (((unsigned int)buf[74])<<24) |
                                (((unsigned int)buf[75])<<16) |
                                (((unsigned int)buf[76])<<8)  |
                                 ((unsigned int)buf[77]);
           pkt_event = UTIL_event_scet_tm (*evt_tim, 0);
           pkt_event->tm_yday++;                /* days after Jan. 1 */
           pkt_event->tm_mon++;                 /* months since Jan */
           year = pkt_event->tm_year + 1900;
           doy  = pkt_event->tm_yday;
           mon  = pkt_event->tm_mon;            /* month, 1...12 */
           mday = pkt_event->tm_mday;           /* day of month */
           hr   = pkt_event->tm_hour;
           mn   = pkt_event->tm_min;
           sec  = (double) pkt_event->tm_sec +
                  (double)(evt_tim->milliseconds % 1000)/1000.;

           quat1 = (((unsigned int)buf[110])*256) | ((unsigned int)buf[111]);
           quat2 = (((unsigned int)buf[116])*256) | ((unsigned int)buf[117]);
           quat3 = (((unsigned int)buf[122])*256) | ((unsigned int)buf[123]);
           quat4 = (((unsigned int)buf[128])*256) | ((unsigned int)buf[129]);

           quaternion[0] = ((double)quat4) / 32767.0;
           quaternion[1] = ((double)quat1) / 32767.0;
           quaternion[2] = ((double)quat2) / 32767.0;
           quaternion[3] = ((double)quat3) / 32767.0;
           sumquat =   ( quaternion[0]*quaternion[0] +
                         quaternion[1]*quaternion[1] +
                         quaternion[2]*quaternion[2] +
                         quaternion[3]*quaternion[3] );

      if (((int)(sumquat*100.+0.5)/100) == 1)           /*  error < 1% ?  */
      {
        q2m_ (quaternion, matrix);
        printf ("Good CHDO %4d-%3.3dT%2.2d:%2.2d:%6.3f  %8.5f  %8.5f  %8.5f  %8.5f\n",
                 year, doy, hr, mn, sec, quaternion[1],
                                         quaternion[2],
                                         quaternion[3],
                                         quaternion[0]);
        printf ("                                 ");
        printf ("%8.5f  %8.5f  %8.5f\n", matrix[0][0], matrix[0][1], matrix[0][2]);
        printf ("                                 ");
        printf ("%8.5f  %8.5f  %8.5f\n", matrix[1][0], matrix[1][1], matrix[1][2]);
        printf ("                                 ");
        printf ("%8.5f  %8.5f  %8.5f\n", matrix[2][0], matrix[2][1], matrix[2][2]);
	
/* Calculate Euler angles */

	phi = atan2 (matrix[2][0], -matrix[2][1]);
	phi = 90. * phi / atan2 (1., 0.);
	delta = acos (matrix[2][2]);
	delta = 90. * delta / atan2 (1., 0.);
	omega = atan2 (matrix[0][2], matrix[1][2]);
	omega = 90. * omega / atan2 (1., 0.);
	printf ("       Phi = %8.5f   Delta = %8.5f   Omega = %8.5f \n",
			phi, delta, omega);
      }
    }
    else
      printf ("Bad CHDO\n");

    }  while (1);
 }

}
 /*
  *     Look for the "htxt" string at the begining
  *       of a CHDO record.  Seems like cTOT occasionally
  *       spews chunks (i.e. status messages) between
  *       data records...
  */
 int chdo_MATCH(char *hstg, int index_max)
   {
     int index;
     if(index_max>=strlen(htxt))
       return 0;
     for(index=0; index<index_max; index++)
       {
         if(hstg[index] != htxt[index])
           return 1;
        }
     return 2;
    }
 /*
  *     Beginig of check for CHDO record alignment....
  */
 int chdo_SYNC(char *primary_header, FILE *file)
  {
    int index = 0;
    char hstg[21];
    memset(hstg, 0, 21);
    while(index<20)
      {
        hstg[index++] = fgetc(file);
        if (feof(file))
          return 0;
        switch(chdo_MATCH(hstg, index))
          {
            case 0:		/* GOOD !!! */
            case 2:		/* so far, so good */
                break;
            case 1:		/* mis-match */
		hstg[0] = hstg[index-1];
                memset(&hstg[1], 0, 20);
                index = 1;
                break;
	   }
       }
    memcpy(primary_header, hstg, 20);
    return 1;
   }
