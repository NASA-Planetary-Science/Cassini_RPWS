
/*
 *      UTIL.H  Utility routine definitions
 *      MSDOS compatible
 */

#ifndef _util_h
#define _util_h
 /*
  *     get a data record
  *
  * PC USERS  PC USERS PC USERS PC USERS  PC USERS PC USERS PC USERS  PC USERS PC USERS
  * PC USERS                                                                   PC USERS
  * PC USERS    do NOT use stdin/stdout for UTIL_get / UTIL_put                PC USERS
  * PC USERS                                                                   PC USERS
  * PC USERS  PC USERS PC USERS PC USERS  PC USERS PC USERS PC USERS  PC USERS PC USERS
  */

 /*
  * blocking flag (3rd. argument to UTIL_get*)
  * *
  * *   UTIL_GET_NON_BLOCKING   returns data or bad status
  * *   UTIL_GET_BLOCKING       return data (blocks if no data available)
  * *   UTIL_GET_RELEASE        return data if available or releases CPU
  * *                             for a short period and returns bad status
  */
#define UTIL_GET_NON_BLOCKING 0
#define UTIL_GET_BLOCKING 1
#define UTIL_GET_RELEASE 2

void UTIL_r_length (long *, long);
long UTIL_getbuffer_CDS (struct CDS_buffer *,   /* data buffer address            */
                         FILE *,        /* file on PC, file/stdin on UNIX */
                         long);         /* 1 = BLOCKING (shared file)     */
long UTIL_putbuffer_CDS (struct CDS_buffer *,   /* data buffer address            */
                         FILE *);       /* file, usually stdout           */
long UTIL_putbuffr2_CDS (struct CDS_buffer *,   /* data buffer address            */
                         FILE *,        /* file, usually stdout           */
                         int);          /* record length (in bytes)       */
long UTIL_lenbuffr2_CDS (struct CDS_buffer *,   /* data buffer address            */
                         int);          /* record length (in bytes)       */
long UTIL_putbuffer_eof (FILE *);       /* file on PC, file/stdout on UNIX */

long UTIL_getbuffer_MP (struct MP_buffer *,     /* data buffer address            */
                        FILE *,         /* file on PC, file/stdin on UNIX */
                        long);          /* 1 = BLOCKING (shared file)     */
long UTIL_putbuffer_MP (struct MP_buffer *,     /* data buffer address            */
                        FILE *);        /* file on PC, file/stdout on UNIX */
long UTIL_putbuffr2_MP (struct MP_buffer *,     /* data buffer address            */
                        FILE *,         /* file, usually stdout           */
                        int);           /* record length (in bytes)       */
long UTIL_lenbuffr2_MP (struct MP_buffer *,     /* data buffer address             */
                        int);           /* record length (in bytes)       */

long UTIL_getbuffer_RPWS (struct RPWS_buffer *, /* data buffer address            */
                          FILE *,       /* file on PC, file/stdin on UNIX */
                          long);        /* 1 = BLOCKING (shared file)     */
long UTIL_putbuffer_RPWS (struct RPWS_buffer *, /* data buffer address            */
                          FILE *);      /* file on PC, file/stdout on UNIX */
long UTIL_putbuffr2_RPWS (struct RPWS_buffer *, /* data buffer address            */
                          FILE *,       /* file, usually stdout           */
                          int);         /* record length (in bytes)       */
long UTIL_lenbuffr2_RPWS (struct RPWS_buffer *, /* data buffer address            */
                          int);         /* record length (in bytes)       */

 /*
  *     Extract routines return native integers
  */
        /*
         * 6 word CDS primary/secondary header     
         */
void UTIL_extract_CDS (struct CDS_buffer *, long *);    /* CDS header array     */

        /*
         * length field in PC-RTIU record                    
         */
long UTIL_extract_RTIU_length (struct CDS_buffer *);

        /*
         * length field in CDS header                        
         */
long UTIL_extract_CDS_length (struct CDS_buffer *);

        /*
         * sequence word in CDS header (lower 14 bits)       
         */
long UTIL_extract_CDS_sequence (struct CDS_buffer *);

        /*
         * 5 bit CDS packet type in CDS primary header       
         */
long UTIL_extract_CDS_type (struct CDS_buffer *);

        /*
         * TRUE when CDS_buffer contains High Rate Science stream    
         */
long UTIL_extract_HRS (struct CDS_buffer *);

        /*
         * TRUE when CDS_buffer contains Low Rate Science stream     
         */
long UTIL_extract_LRS (struct CDS_buffer *);

        /*
         * TRUE when CDS_buffer contains Housekeepinge stream     
         */
long UTIL_extract_HSK (struct CDS_buffer *);

        /*
         * TRUE when CDS_buffer contains MRO stream     
         */
long UTIL_extract_MRO (struct CDS_buffer *);

        /*
         * Character string indicating CDS packet type             
         */
char *UTIL_extract_packet_type (struct CDS_buffer *);
char *UTIL_extract_packet_brief (struct CDS_buffer *);
int UTIL_extract_packet_sindex (struct CDS_buffer *);

        /*
         * 32 bit time field (in seconds)                          
         */
long UTIL_extract_TIME (struct CDS_buffer *);
long UTIL_extract_EPOCH (struct CDS_buffer *);
long UTIL_extract_CDS_RTI (struct CDS_buffer *);        /* 8 bit RTI/sub-RTI */

        /*
         * 32 bit time field (in seconds)                          
         */
 /*
  * long UTIL_extract_PKT_TIME(struct CDS_buffer*);     / *
  */
long UTIL_extract_PKT_TIME (struct MP_buffer *);
/**/ long UTIL_extract_PKT_TIME2 (struct MP_buffer *);

        /*
         * RTI number 0-7                                          
         */
        /*
         * subRTI number 0-15                                      
         */
long UTIL_extract_MP_RTI (struct MP_buffer *);
long UTIL_extract_RTI (struct CDS_buffer *);
long UTIL_extract_subRTI (struct CDS_buffer *);
long UTIL_extract_Qflag (struct CDS_buffer *buffer);

        /*
         * Returns current time in BCD time structure           
         */
long UTIL_get_time (struct BCD_Time_Tag *);

         /*
          * 4 bit MP packet type in mini packet       
          */
long UTIL_extract_MP_bits (struct MP_buffer *);
long UTIL_extract_MP_type (struct MP_buffer *);
char *UTIL_extract_MP_packet_type (struct MP_buffer *);
char *UTIL_get_MP_packet_type (long);

        /*
         * length field in MP header                        
         */
long UTIL_extract_MP_length (struct MP_buffer *);
long UTIL_MP_length (struct MP_buffer *);       /* range > 4K  */

        /*
         * 16 bit time field (in fractional seconds)               
         */
long UTIL_extract_MP_TIME (struct MP_buffer *);

        /*
         * byte order utilities, convert to/from MSB    
         */

long UTIL_short_to_MSB (unsigned char *);
unsigned short UTIL_long_to_PC16 (unsigned long);
unsigned long UTIL_long_to_MSB (unsigned long);
unsigned long UTIL_MSB_to_long (unsigned long);
#endif

#ifndef _utild_h
#define _utild_h
 /*
  *     COMPRESSION control (i.e.; decompress mini packet )
  *             return NULL if more data needed
  */

struct RPWS_buffer *UTIL_decompress (struct MP_buffer *);
#endif

#ifndef _miniproc_h
#define _miniproc_h
        /*
         * extract mini packets                 
         */
long UTIL_get_mp (struct CDS_buffer *cds_buffer,
                  struct MP_buffer *m_buffer, int cds_source_number);
#endif
