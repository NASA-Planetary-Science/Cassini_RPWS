
        /************************************************
	 *		RTIU.H				*
	 *	Command/Data System packet formats	*
	 *	(packets arriving from various sources)	*
	 ***********************************************/

#ifndef _rtiu_h
#define _rtiu_h
#define CDS_PKTSIZE 952
#define CDS_PKTMIN  950
#define CDS_PKTMAX  1152
#define MP_PKTMAX   4096
#define RTIU_header_length 32
#define RTIU68K_header_length 16
#define RTIU68K_status_length 38
#define RPWS_PKTMAX 32768

/********************************************************
 *	DEFINITIONS for the various record types	*
 ********************************************************/

                                /*
                                 * use this if you don't know   
                                 */
                                /*
                                 * if the data-set is segmented 
                                 */
#define DATA_MP_packet  0x100           /* packetized products          */
#define DATA_MPP_MFR     0x101          /* MFR packet                  */
#define DATA_MPP_HFR     0x102          /* HFR packet                  */
#define DATA_MPP_WFR     0x108          /* WFR packet                  */
#define DATA_MPP_WBR     0x10E          /* WBR packet                  */
#define DATA_MPP_DUST    0x10B          /* WFR packet                  */
#define DATA_MPP_LFDR    0x107          /* WBR packet                  */
#define DATA_MPP_LP      0x104          /* LP  packet                  */
#define DATA_MPP_HK      0x110          /* Housekeeping packet         */
#define DATA_MPP_STIM    0x120          /* STIM packets */

#define DATA_MP_segment 0x200           /* segmented data products      */
#define DATA_MPS_HFR     0x202          /* HFR SEGMENT (1K segment)    */
#define DATA_MPS_WFR     0x208          /* WFR SEGMENT (1K segment)    */
#define DATA_MPS_LFDR    0x207          /* WFR SEGMENT (1K segment)    */
#define DATA_MPS_WBR     0x20E          /* WBR SEGMENT (1K segment)    */
#define DATA_MPS_DUST    0x20B          /* WBR SEGMENT (1K segment)    */
#define DATA_MPS_LP      0x204          /* LP SEGMENT  (1K segment)    */

#define DATA_MP_large_segment 0x300     /* segmented data products      */
#define DATA_MPL_HFR     0x302          /* HFR SEGMENT (4K segment)    */
#define DATA_MPL_WFR     0x308          /* WBR SEGMENT (4K segment)    */
#define DATA_MPL_LFDR    0x307          /* WBR SEGMENT (4K segment)    */
#define DATA_MPL_WBR     0x30E          /* WBR SEGMENT (4K segment)    */
#define DATA_MPL_DUST    0x30B          /* WBR SEGMENT (4K segment)    */
#define DATA_MPL_LP      0x304          /* LP SEGMENT  (4K segment)    */

                                        /*
                                         * complete cycle of data       
                                         */
                                        /*
                                         * may be LARGE (65K)           
                                         */
#define DATA_MP_complete_segment 0x400  /* complete data products      */
#define DATA_MPC_HFR     0x402          /* HFR SEGMENT (64K segment)    */
#define DATA_MPC_WFR     0x408          /* WFR SEGMENT (64K segment)    */
#define DATA_MPC_LFDR    0x407          /* LFDR SEGMENT (64K segment)    */
#define DATA_MPC_WBR     0x40E          /* WBR SEGMENT (64K segment)    */
#define DATA_MPC_DUST    0x40B          /* DUST SEGMENT (64K segment)    */
#define DATA_MPC_LP      0x404          /* LP SEGMENT  (64K segment)    */

#define DATA_telemetry  0x800           /*  telemetry products          */
#define	DATA_RTIU	0x801           /*  Traffic from PC based RTIU  */
#define	DATA_RTIU_biust	0x803           /*  Ancillary data  from PC based RTIU  */
#define	DATA_RTIU_sa3	0x803           /*  Ancillary data  from PC based RTIU  */
#define	DATA_RTIU_nomcmd 0x807          /*        PC based RTIU */
#define	DATA_RTIU_sa7	0x807           /*        PC based RTIU */
#define	DATA_RTIU_crtcmd 0x808          /*        PC based RTIU */
#define	DATA_RTIU_sa8	0x808           /*        PC based RTIU */
#define	DATA_RTIU_fpcmd 0x809           /*        PC based RTIU */
#define	DATA_RTIU_sa9	0x809           /*        PC based RTIU */
#define	DATA_RTIU_ancil	0x80A           /*  BIU discrete/status from PC based RTIU      */
#define	DATA_RTIU_sa10	0x80A           /*  BIU discrete/status from PC based RTIU      */
#define	DATA_RTIU_telem	0x80B           /*  Traffic from PC based RTIU  */
#define	DATA_RTIU_sa11	0x80B           /*  Traffic from PC based RTIU  */
#define	DATA_RTIU_hsk	0x80C           /*  Traffic from PC based RTIU  */
#define	DATA_RTIU_sa12	0x80C           /*  Traffic from PC based RTIU  */
#define DATA_BIU_SIM	0x841           /* U of I BIU simulator         */

#define BIU_sa3			0x03
#define BIU_sub_address_biust	0x03
#define BIU_sa7			0x07
#define BIU_sub_address_nomcmd	0x07
#define BIU_sa8			0x08
#define BIU_sub_address_crtcmd	0x08
#define BIU_sa9			0x09
#define BIU_sub_address_fpcmd	0x09
#define BIU_sa10		0x0A
#define BIU_sub_address_ancil	0x0A
#define BIU_sa11		0x0B
#define BIU_sub_address_telem	0x0B
#define BIU_sa12		0x0C
#define BIU_sub_address_hsk	0x0C

#define DATA_DECOMP_DCP 1
#define DATA_DECOMP_DCC 2
#define DATA_DECOMP_MFR 4
#define DATA_DECOMP_HFR 8

#define STATUS_COMPRESS_MASK			    1
#define STATUS_COMPRESS_success			    1
#define STATUS_COMPRESS_uncompressed   		    3
#define	STATUS_COMPRESS_unknown 		    5
#define	STATUS_COMPRESS_more_data		0x201
#define STATUS_COMPRESS_fail			    0
#define	STATUS_COMPRESS_not_my_data		0x100
#define	STATUS_COMPRESS_decompress_overrun	0x401
#define	STATUS_COMPRESS_no_resource		0x800

#define PACKET_TYPE_invalid -1
#define PACKET_TYPE_stim 0
#define PACKET_TYPE_mfr	 1
#define PACKET_TYPE_hfr	 2
#define PACKET_TYPE_lp	 4
#define PACKET_TYPE_lfdr 7
#define PACKET_TYPE_wfr	 8
#define PACKET_TYPE_dust 11
#define PACKET_TYPE_mro	 13
#define PACKET_TYPE_wbr	 14
#define PACKET_TYPE_fill 15


/********************************************************
 *   user-variable.packet.ancillary.ancil		*
 *   user-variable.packet.ancillary.biust		*
 ********************************************************/
struct Ancillary_header
{
  unsigned char filler1[208];
  unsigned char ancil[8];
  unsigned char biust[8];
};
struct Ancillary_packet
{
  unsigned char filler1[256];
  unsigned char data[16];
  unsigned char filler2[CDS_PKTMAX - 16 + 256];
};
struct BIU_status_packet
{
  unsigned char filler1[256];
  unsigned char data[8];
  unsigned char filler2[CDS_PKTMAX - 8 + 256];
};

/********************************************************
 *	This defines the CDS packet delivered by	*
 *	the LRP to CDS.  This definition is concerned	*
 *	only with the CDS header			*
 *  access it like this:				*
 *   user-variable.packet.cds.header			*
 ********************************************************/
struct CDS_packet
{
  unsigned char filler1[256];
  unsigned char header[12];
  unsigned char filler2[CDS_PKTMAX - 12 + 256];
};

/********************************************************
 *	This defines the CDS packet delivered by	*
 *	the LRP to CDS.  This definition is concerned	*
 *	with looking at a housekeeping packet           *
 *  access it like this:				*
 *   user-variable.packet.housekeeping.header		*
 *   user-variable.packet.rom_housekeeping.header	*
 ********************************************************/
struct CDS_housekeeping
{
  unsigned char filler1[256];
  unsigned char header[12];
  unsigned char Time_Tag[2];
  unsigned char Valid_Command_Count[1];
  unsigned char Invalid_Command_Count[1];
  unsigned char Command_Byte_Count[2];
  unsigned char Command_Loop_Count[1];
  unsigned char BIU_Reset_Count[1];
  unsigned char BIU_Discrete_Status[1];
  unsigned char BIU_Discrete_Command[1];
  unsigned char Antenna_Status[1];
  unsigned char Power_Status[1];
  unsigned char LRP_analog[16];
  unsigned char HFR_analog[16];
  unsigned char BIU_Discrete_misc[1];
  unsigned char BIU_RTI_Status[1];
  unsigned char LP_Probe_Bias[1];
  unsigned char LP_DAC1_Bias[1];
  unsigned char LP_MUX_0[1];
  unsigned char LP_P8155[1];
  unsigned char Memory_Address[2];
  unsigned char micro_packet[8][16];
};
struct CDS_ROM_housekeeping
{
  unsigned char filler1[256];
  unsigned char header[12];
  unsigned char Time_Tag[2];
  unsigned char Valid_Command_Count[1];
  unsigned char Invalid_Command_Count[1];
  unsigned char Command_Byte_Count[2];
  unsigned char Command_Loop_Count[1];
  unsigned char BIU_Reset_Count[1];
  unsigned char BIU_Discrete_Status[1];
  unsigned char BIU_Discrete_Command[1];
  unsigned char Antenna_Status[1];
  unsigned char Power_Status[1];
  unsigned char LRP_analog[16];
  unsigned char HFR_analog[16];
  unsigned char BIU_Discrete_misc[1];
  unsigned char BIU_RTI_Status[1];
  unsigned char LP_Probe_Bias[1];
  unsigned char LP_DAC1_Bias[1];
  unsigned char LP_MUX_0[1];
  unsigned char LP_P8155[1];
  unsigned char Memory_Address[2];
  unsigned char Memory_Data[32];
  unsigned char lrp[12];
  unsigned char lrp_ipc[4];
  unsigned char hrp[12];
  unsigned char hrp_bulk[4];
  unsigned char dcp[12];
  unsigned char alf_stat[4];
  unsigned char hrp_mode[2];
  unsigned char filler3[2];
  unsigned char ALF_record[44];
};
struct CDS_Deploy_0_housekeeping
{
  unsigned char filler1[256];
  unsigned char header[64];
  unsigned char Run_Time[2];
  unsigned char Command_Time[2];
  unsigned char CDS_Command[2];
  unsigned char Command;
  unsigned char Antenna_ID;
  unsigned char _status;
  unsigned char _test;
  unsigned char _current;
  unsigned char _temp;
  unsigned char _position;
  unsigned char _Limit;
  unsigned char _reason[2];
};
struct CDS_Deploy_3_housekeeping
{
  unsigned char filler1[256];
  unsigned char header[64];
  unsigned char Run_Time[2];
  unsigned char Command_Time[2];
  unsigned char CDS_Command[2];
  unsigned char Command;
  unsigned char Antenna_ID;
  unsigned char Max_Current;
  unsigned char Min_Current;
  unsigned char Max_Temp;
  unsigned char Min_Temp;
  unsigned char Max_Position;
  unsigned char Min_Position;
  unsigned char _reason[2];
};

/************************************************
 *	This defines the RPWS data area		*
 *	  it only places the RPWS data		*
 *  access it like this:			*
 *   user-variable.packet.rpws.data		*
 ************************************************/
struct RPWS_stream
{
  unsigned char filler1[256 + 12];
  unsigned char data[CDS_PKTMAX - 12];
  unsigned char filler2[256];
};

/************************************************
 *	This defines the RTIU data areas	*
 *  access it like this:			*
 *   user-variable.packet.rtiu.header		*
 *   user-variable.packet.pcrtiu.header		*
 ************************************************/
struct RTIU_packet
{
  unsigned char filler1[256 - RTIU_header_length - RTIU68K_status_length];
  unsigned char status[RTIU68K_status_length];
  unsigned char header[RTIU_header_length];
  unsigned char filler2[CDS_PKTMAX + 256];
};

struct PCRTIU_packet
{
  unsigned char filler1[256 - RTIU_header_length];
  unsigned char header[RTIU_header_length / 2][2];
  unsigned char filler2[CDS_PKTMAX + 256];
};

/************************************************
 *	This defines the trailer area		*
 *       and header area                        *
 *  access it like this:			*
 *   user-variable.packet.trailer.data		*
 *   user-variable.packet.header.length		*
 *   user-variable.packet.header.data		*
 ************************************************/
struct TRAILER_area
{
  unsigned char filler1[256 + CDS_PKTMAX];
  unsigned char data[256];
};
struct HEADER_area
{
  unsigned char data[256];
};

/********************************************************
 *	time structures					*
 *	(compiler will always do 32 bit alignment)	*
 *	user-variable.packet.chdo_tag.sclk.seconds	*
 *	user-variable.packet.chdo_tag.sclk.fine		*
 *	user-variable.packet.chdo_tag.scet.days		*
 *	user-variable.packet.chdo_tag.scet.milliseconds	*
 *	user-variable.packet.chdo_tag.ert.days		*
 *	user-variable.packet.chdo_tag.ert.milliseconds	*
 *	user-variable.packet.chdo_tag.rct.days		*
 *	user-variable.packet.chdo_tag.rct.milliseconds	*
 ********************************************************/
struct event_time
{
  unsigned short filler;
  unsigned short days;
  unsigned long milliseconds;
};
struct event_clock
{
  unsigned long seconds;
  unsigned short fine;
  unsigned short filler;
};
struct CHDO_Time_Tag
{
  unsigned char filler[256 - 32];
  struct event_clock sclk;
  struct event_time scet;
  struct event_time ert;
  struct event_time rct;
};

/************************************************
 *	CDS time tag (for minipackets)		*
 *	  tag data packet with CDS time from	* 
 *	    and last CDS segment that the	* 
 *	 	mini packet is built up from	*
 *	user-variable.packet.cds_tag.begin	*
 *	user-variable.packet.cds_tag.end	*
 *	user-variable.packet.cds_tag.epoch	*
 *	user-variable.packet.cds_tag.sequence	*
 ************************************************/
struct CDS_Time_Tag
{
  unsigned char begin[8];
  unsigned char end[8];
  unsigned char filler[56];
  unsigned char epoch[4];
  unsigned char sequence[4];
};

/************************************************
 *	WS time tag (for any packets)		*
 *	  tag data packet with UT from W.S.	* 
 *	    and last CDS segment that the	* 
 *	 	mini packet is built up from	*
 *	user-variable.packet.ws_tag.A		*
 *	user-variable.packet.ws_tag.B		*
 ************************************************/
struct BCD_Time_Tag
{
  unsigned char year[2];
  unsigned char doy[2];
  unsigned char hour;
  unsigned char minute;
  unsigned char second;
  unsigned char spare;
};
struct WS_Time_Tag
{
  unsigned char filler1[16];
  struct BCD_Time_Tag A;
  struct BCD_Time_Tag B;
};

 /*******************************************************
  *	Compression status field			*
  *	user-variable.packet.compress.method		*
  *	user-variable.packet.compress.bit_count		*
  *	user-variable.packet.compress.result		*
  *******************************************************/
struct COMPRESS_status
{
  unsigned char filler1[32];
  unsigned char method;
  unsigned char filler[3];
  long bit_count;
  long result;
};

 /*******************************************************
  *	Data Length indicator				*
  		data_start is original length (of a compressed dataset)
  		data_length is number of bytes in minipacket (same meaning as 
  		  the minipacket length field, but without the 4K restriction)
  *							*
  *	user-variable.packet.index.data_start		*
  *	user-variable.packet.index.data_length		*
  *******************************************************/
struct LENGTH_status
{
  unsigned char filler1[48];
  long data_start;
  long data_length;
};

 /*******************************************************
  *                                                     *
  *     Mini-packet definitions                         *
  *       mini packets passed around will appear in     *
  *       2 flavors:                                    *
  *             Segmented data, where it takes several  *
  *                 buffers to hold a complete cycle    *
  *             Packetized data (in other words,        *
  *                 un-segmented), where the instrument *
  *                 cycle fits within a single packet   *
  *         NOTE that it may take several MPP/MPS       *
  *             buffers to hold 			*
  *	user-variable.packet.mpp.mini_packert[x]	*
  *                                                     *
  *******************************************************/
struct MPMAX
{
  unsigned char filler1[56];
  long segment_count;
  long segment_number;
  long record_count;
  long record_number;
  unsigned char filler2[256 - 72];
  unsigned char mini_packet[MP_PKTMAX];
  unsigned char filler3[256];
};

struct MPP
{
  unsigned char filler1[56];
  long segment_count;
  long segment_number;
  long record_count;
  long record_number;
  unsigned char filler2[256 - 72];
  unsigned char mini_packet[1024];
  unsigned char filler3[256];
};

struct MPS
{
  unsigned char filler1[56];
  long segment_count;
  long segment_number;
  long record_count;
  long record_number;
  unsigned char filler2[256 - 72];
  unsigned char mini_packet[1024];
  unsigned char filler3[256];
};

/********************************************************
 *	This defines the data structure passed around	*
 *	the various pre-processing steps (through the	*
 *	UNIX 'pipe' gismo.				*
 *  access it like this:				*
 *   user-variable.f_length				*
 *   user-variable.r_length				*
 *   user-variable.record_type				*
 ********************************************************/
struct CDS_buffer
{
  long f_length;
  long record_type;
  long status;
  union
  {
    struct CDS_packet cds;
    struct Ancillary_header ancillary;
    struct Ancillary_packet ancil;
    struct BIU_status_packet biust;
    struct CDS_housekeeping housekeeping;
    struct CDS_ROM_housekeeping rom_housekeeping;
    struct CDS_Deploy_0_housekeeping deploy_0_hsk;
    struct CDS_Deploy_3_housekeeping deploy_3_hsk;
    struct RTIU_packet rtiu;
    struct PCRTIU_packet pcrtiu;
    struct RPWS_stream rpws;
    struct TRAILER_area trailer;
    struct HEADER_area header;
    struct WS_Time_Tag ws_tag;
    struct COMPRESS_status compress;
    struct CDS_Time_Tag cds_tag;
    struct CHDO_Time_Tag chdo_tag;
    struct LENGTH_status index;
    struct MPP mpp;
    struct MPS mps;
  } packet;
  long r_length;
};
struct MP_buffer
{
  long f_length;
  long record_type;
  long status;
  union
  {
    struct MPMAX mpx;
    struct HEADER_area header;
    struct CDS_Time_Tag cds_tag;
    struct Ancillary_header ancillary;
    struct CHDO_Time_Tag chdo_tag;
    struct WS_Time_Tag ws_tag;
    struct COMPRESS_status compress;
    struct LENGTH_status index;
    struct RPWS_stream rpws;
    struct MPP mpp;
    struct MPS mps;
  } packet;
  long r_length;
};
struct RPWS_buffer
{
  long f_length;
  long record_type;
  long status;
  union
  {
    unsigned char filler[RPWS_PKTMAX + 512];
    struct MPMAX mpx;
    struct HEADER_area header;
    struct CDS_Time_Tag cds_tag;
    struct Ancillary_header ancillary;
    struct CHDO_Time_Tag chdo_tag;
    struct WS_Time_Tag ws_tag;
    struct COMPRESS_status compress;
    struct LENGTH_status index;
    struct RPWS_stream rpws;
    struct MPP mpp;
    struct MPS mps;
  } packet;
  long r_length;
};
struct GENERIC_buffer
{
  long f_length;
  long record_type;
  long status;
  char cds_time_tag_a[8];               /* time of 1st. segment */
  char cds_time_tag_b[8];               /* time of last segment */
  char ws_time_tag_a[8];                /* when data arrived in WS */
  char ws_time_tag_b[8];                /* when data last formatted in WS */
  char compress_method[4];              /* */
  char compress_bit_count[4];           /* */
  char compress_result[4];              /* */
  char spare[4];                        /* */
  char length_data_start[4];            /* unrestricted compressed m.p. length */
  char length_data_length[4];           /* unrestricted minipacket length */
  char segment_count[4];
  char segment_number[4];
  char record_count[4];
  char record_number[4];
  char epoch[4];                        /* epoch of data */
  char sequence[4];                     /* sequence field from cds record */
  char filler[128];                     /* not exact yet... */
  char ancil[8];                        /* ancillary broadcast, when available */
  char biust[8];                        /* biu status words, when available */
  char f_sclk[8];
  char f_scet[8];
  char f_ert[8];
  char f_rct[8];
};
#endif
