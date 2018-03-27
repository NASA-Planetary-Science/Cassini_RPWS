 /*
  * LFDR data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are reative to the minipacket    
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define MRO_packet_ID_value		0x0D
#define MRO_packet_ID                   0x0000040F
#define MRO_minipacket_length		0x000100FF
#define MRO_minipacket_length_MSB	0x0000000F

#define MRO_address_MSB		0x000400FF
#define MRO_address_TLM		0x000500E0
#define MRO_address_HSK		0x000500FF
#define MRO_Bank		0x00050401
#define MRO_processor_mask	0x00050107
#define MRO_source_mask		0x0005010F
#define MRO_HRP			0x00050301
#define MRO_DCP			0x00050201
#define MRO_LRP			0x00050101
#define MRO_ROM			0x00050001
