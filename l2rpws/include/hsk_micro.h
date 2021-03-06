 /*
  *     bits 28-30      reshift (split bit fields)
  *     bits 16-27      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

#define MHSK_ID			0x000000FF
#define MHSK_LEN		0x000100FF

#define MHSK_RTI_ONLY		0x00020007
#define MHSK_RTI_MSB		0x000300FF
#define MHSK_RTI		0x000200FF

#define MHSK_MRO_ID		0x00D0
#define MHSK_MRO_ADDR_MSB	0x000400FF
#define MHSK_MRO_ADDR		0x000500FF

#define MHSK_MRO_DUMP		0x000600FF

#define MHSK_BFDL_ID		0x00C1
#define MHSK_BFDL_CMD_MSB	0x000500FF
#define MHSK_BFDL_CMD		0x000400FF

#define MHSK_BFDL_MSG_MSB	0x000700FF
#define MHSK_BFDL_MSG		0x000600FF

#define MHSK_BFDL_BAD_MSB	0x000900FF
#define MHSK_BFDL_BAD		0x000800FF

#define MHSK_BFDL_BCMD_MSB	0x000B00FF
#define MHSK_BFDL_BCMD		0x000A00FF

#define MHSK_BFDL_WDT_MSB	0x000D00FF
#define MHSK_BFDL_WDT		0x000C00FF

#define MHSK_BFDL_WPKT_MSB	0x000F00FF
#define MHSK_BFDL_WPKT		0x000E00FF

#define MHSK_DUST_ID		0x00D0
#define MHSK_DUST_ADDRESS	0x27E0
#define MHSK_DUST_ADDR_MSB	0x000400FF
#define MHSK_DUST_ADDR		0x000500FF

#define MHSK_DUST_COUNT_MSB	0x000700FF
#define MHSK_DUST_COUNT		0x000600FF
#define MHSK_DUST_0DB		0x000800FF
#define MHSK_DUST_10DB		0x000900FF
#define MHSK_DUST_20DB		0x000A00FF
#define MHSK_DUST_30DB		0x000B00FF
#define MHSK_DUST_40DB		0x000C00FF
#define MHSK_DUST_50DB		0x000D00FF
#define MHSK_DUST_60DB		0x000E00FF
#define MHSK_DUST_70DB		0x000F00FF

#define MHSK_IPC_ID		0x00D0
#define MHSK_IPC_ADDRESS	0x1170
#define MHSK_IPC_ADDRESS2	0x117A
#define MHSK_IPC_ADDR_MSB	0x000400FF
#define MHSK_IPC_ADDR		0x000500FF

#define MHSK_IPC_TX_LOST_MSB	0x000700FF
#define MHSK_IPC_TX_LOST	0x000600FF
#define MHSK_IPC_TX_RETRY_MSB	0x000900FF
#define MHSK_IPC_TX_RETRY	0x000800FF
#define MHSK_IPC_TX_ABORT_MSB	0x000B00FF
#define MHSK_IPC_TX_ABORT	0x000A00FF
#define MHSK_IPC_RX_LOST_MSB	0x000D00FF
#define MHSK_IPC_RX_LOST	0x000C00FF
#define MHSK_IPC_RX_F5_MSB	0x000F00FF
#define MHSK_IPC_RX_F5		0x000E00FF

#define MHSK_IPC_RX_ABORT_MSB	0x000700FF
#define MHSK_IPC_RX_ABORT	0x000600FF

#define MHSK_WORD_0		0x000000FF
#define MHSK_WORD_0_MSB		0x000100FF
#define MHSK_WORD_1		0x000200FF
#define MHSK_WORD_1_MSB		0x000300FF
#define MHSK_WORD_2		0x000400FF
#define MHSK_WORD_2_MSB		0x000500FF
#define MHSK_WORD_3		0x000600FF
#define MHSK_WORD_3_MSB		0x000700FF
#define MHSK_WORD_4		0x000800FF
#define MHSK_WORD_4_MSB		0x000900FF
#define MHSK_WORD_5		0x000A00FF
#define MHSK_WORD_5_MSB		0x000B00FF
#define MHSK_WORD_6		0x000C00FF
#define MHSK_WORD_6_MSB		0x000D00FF
#define MHSK_WORD_7		0x000E00FF
#define MHSK_WORD_7_MSB		0x000F00FF
