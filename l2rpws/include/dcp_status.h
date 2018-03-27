 /*
  * WFR data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define DCP_compression_status		0x0008040F
#define DCP_walsh_number		0x0008000F
#define DCP_MSF_compression_status	0x000A040F
#define DCP_MSF_walsh_number		0x000A000F

#define DCP_compression_WBR_WFR		0x00060601
#define DCP_WBR_WFR_MSF			0x00060301
