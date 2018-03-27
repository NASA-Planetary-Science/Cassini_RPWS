
/*				  UTILI.H				*/

/* 	These are macros will return values of the bit fields in the    */

/*  mini-packet header area.  These will work with pointer to any of    */

/*  the following structures: CDS_buffer, MP_buffer, or RPWS_buffer.    */

#ifndef _UTILI_H
#define _UTILI_H

#define UTIL_MP_type( x )		((x->packet.mpx.mini_packet[0]&0x00F0)>>4)
#define UTIL_MP_length( x )	      ( ((x->packet.mpx.mini_packet[0]&0x000F)<<8) | \
					 (x->packet.mpx.mini_packet[1]&0x00FF)   )
#define UTIL_MP_size( x )	      ( (x->packet.mpx.mini_packet[4]&0x00F0)>>4 )
#define UTIL_MP_segment( x )	      (	(x->packet.mpx.mini_packet[4]&0x000F)    )

/************************************************************************/

/*           WBR MINI PACKET HEADER INFORMATION 11-29-94		*/

/************************************************************************/
#define UTIL_WBR_length		      ( ((x->packet.mpx.mini_packet[0]&0x000F)<<8) | \
					 (x->packet.mpx.mini_packet[1]&0x00FF)   )
#define UTIL_WBR_size( x )	      ( (x->packet.mpx.mini_packet[4]&0x00F0)>>4 )
#define UTIL_WBR_segment( x )	      (	(x->packet.mpx.mini_packet[4]&0x000F)    )
#define UTIL_WBR_fb( x )	      ( (x->packet.mpx.mini_packet[5]&0x0080)>>7 )
#define UTIL_WBR_gain( x )	      (	(x->packet.mpx.mini_packet[5]&0x0007)    )
#define UTIL_WBR_compression( x )     (	(x->packet.mpx.mini_packet[6]&0x00F0)>>4 )
#define UTIL_WBR_msf( x )	      (	(x->packet.mpx.mini_packet[6]&0x0008)>>3 )
#define UTIL_WBR_antenna( x )	      (	(x->packet.mpx.mini_packet[6]&0x0007)    )
#define UTIL_WBR_agc_value( x )	      (	(x->packet.mpx.mini_packet[7]&0x00FF)    )
#define UTIL_WBR_hfr_xlate( x )	      (	(x->packet.mpx.mini_packet[8]&0x00FF)    )
#define UTIL_WBR_lp_dac_0( x )	      (	(x->packet.mpx.mini_packet[8]&0x00FF)    )

#define UTIL_WBR_compression_byte( x )     (  x->packet.mpx.mini_packet[6]  )


/************************************************************************/

/*           WfR MINI PACKET HEADER INFORMATION 11-29-94		*/

/************************************************************************/
#define UTIL_WBR_length		      ( ((x->packet.mpx.mini_packet[0]&0x000F)<<8) | \
					 (x->packet.mpx.mini_packet[1]&0x00FF)   )
#define UTIL_WFR_size( x )	      ( (x->packet.mpx.mini_packet[4]&0x00F0)>>4 )
#define UTIL_WFR_segment( x )	      (	(x->packet.mpx.mini_packet[4]&0x000F)    )
#define UTIL_WFR_fb( x )	      ( (x->packet.mpx.mini_packet[5]&0x0080)>>7 )
#define UTIL_WFR_lp( x )	      ( (x->packet.mpx.mini_packet[5]&0x0040)>>6 )
#define UTIL_WFR_gain( x )	      (	(x->packet.mpx.mini_packet[5]&0x003F)    )
#define UTIL_WFR_compression( x )     (	(x->packet.mpx.mini_packet[6]&0x00F0)>>4 )
#define UTIL_WFR_msf( x )	      (	(x->packet.mpx.mini_packet[6]&0x0008)>>3 )
#define UTIL_WFR_antenna( x )	      (	(x->packet.mpx.mini_packet[6]&0x0007)    )
#define UTIL_WFR_chan( x )	      (	(x->packet.mpx.mini_packet[7]&0x0038)>>3 )
#define UTIL_WFR_mode( x )	      (	(x->packet.mpx.mini_packet[7]&0x0007)    )
#define UTIL_WFR_lp_dac_0( x )	      (	(x->packet.mpx.mini_packet[8]&0x00FF)    )
#define UTIL_WFR_lp_dac_1( x )	      (	(x->packet.mpx.mini_packet[9]&0x00FF)    )

#define UTIL_WFR_compression_byte( x )     (  x->packet.mpx.mini_packet[6]  )

/* Absolute mp length, not (length-1) in mp header */

/* 3 bytes longer the the length in the mp header */
#define UTIL_put_mp_length(x,n)					\
  if(n<4096){							\
    x->packet.mpx.mini_packet[0] &= 0x0F0;			\
    x->packet.mpx.mini_packet[0] |= (((n-3)&0x0F00)>>8);	\
    x->packet.mpx.mini_packet[1]  =  ((n-3)&0x00FF);		\
    }								\
  else{								\
    x->packet.mpx.mini_packet[0] &= 0x0F0;			\
    x->packet.mpx.mini_packet[1]  = 0x000;			\
    }								\
  x->packet.index.data_start  = (n-3); 				\
  x->packet.index.data_length = (n-3);

/* Absolute mp length, not (length-1) in mp header */

/* 3 bytes longer the the length in the mp header */
#define UTIL_mp_len2f_length(x)		( 3*sizeof(long) + 256 + x )

/*  End of Macros */


#define UNPACKED	(0x00)
#define PACKED		(0x01)
#define DCC_INT_EOP	(0x02)
#define DCC_EXT_EOP	(0x03)
#define DCC_COMPRESSED 	(0x02)
#endif
