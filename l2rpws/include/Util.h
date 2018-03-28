
/* From Util.c */

void Util_extract_CDS (struct CDS_buffer *buffer /* data buffer */ ,
                       long *cds /* 6 word (16 bit) CDS header */ );

unsigned long Util_extract_TIME (struct CDS_buffer *buffer);    /* data buffer */

long Util_extract_RTI (struct CDS_buffer *buffer);

int Util_16 (short buf);

int Util_32 (long buf);

/* From Util_event_time.c */
struct tm *Util_event_scet_tm (struct event_time scet,  /* instrument event time */
                               int start_point);

struct event_time *Util_event_scet (struct MP_buffer *buf /* data buffer */ ,
                                    struct event_clock sclk);   /* spacecraft clock */
