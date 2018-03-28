 /*
  * CDS data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 28-30      reshift (split bit fields)
  *     bits 16-27      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define	CDS_CCSDS_Version			0x00010507
#define	CDS_Packet_Type				0x00010401
#define	CDS_Secondary_Header			0x00010301
#define	CDS_SSID_upper				0x30010007
#define	CDS_SSID_lower				0x00000603
#define	CDS_Packet_ID				0x0000003F

#define CDS_Packet_ID_UnSegmented_LRS_I		0x01
#define CDS_Packet_ID_UnSegmented_HRS_I		0x02
#define CDS_Packet_ID_Segmented_LRS_I		0x20
#define CDS_Packet_ID_Segmented_HRS_I		0x23
#define CDS_Packet_ID_Housekeeping_ROM		0x10
#define CDS_Packet_ID_Housekeeping_Maintenance	0x13
#define CDS_Packet_ID_Housekeeping_Deploy	0x13
#define CDS_Packet_ID_Housekeeping_Science	0x15

#define CDS_Packet_Sequence_Segmentation	0x00030503
#define CDS_Packet_Sequence_MSB			0x0003003F
#define CDS_Packet_Sequence_LSB			0x000200FF

#define CDS_Length_MSB				0x000500FF
#define CDS_Length_LSB				0x000400FF

#define CDS_Error_Bits				0x000700FF
#define CDS_Illegal_Length			0x00070701
#define CDS_Illegal_APP_ID			0x00070601
#define CDS_Block_Error				0x00070501
#define CDS_1553_Error_Block_ID			0x0007001F

#define CDS_Time_Bits_31_24			0x000600FF
#define CDS_Time_Bits_23_16			0x000900FF
#define CDS_Time_Bits_15_08			0x000800FF
#define CDS_Time_Bits_07_00			0x000B00FF
#define CDS_Time_Bits_SUB_Second		0x000A00FF
#define CDS_Time_Bits_SUB			0x000A00FE
#define CDS_Time_Bits_RTI			0x000A0507
#define CDS_Time_Bits_SEQ			0x000A010F
#define CDS_Time_Bits_TQF			0x000A0001
