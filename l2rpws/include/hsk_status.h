 /*
  * HSK data frame offsets, shifts and mask values 
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


#define HSK_IEB_status			0x000C00FF
#define HSK_IEB_status_MSB		0x000D00FF
#define HSK_Valid_Command		0X000E00FF
#define HSK_Invalid_Command		0X000F00FF
#define HSK_Command_Byte_Count		0X001000FF
#define HSK_Command_Byte_Count_MSB	0X001100FF
#define HSK_Loop_Count			0X001200FF
#define HSK_BIU_Soft_Reset		0X001300FF
#define HSK_BIU_Discreet_Status		0X001400FF
#define HSK_BIU_Discreet_Command	0X001500FF
#define HSK_Antenna_Limit_Sw		0X001600FF
#define HSK_Power_Status		0X001700FF
#define HSK_Power_Status_HFR		0X00170001
#define HSK_Power_Status_ME02		0X00170101
#define HSK_Power_Status_LP		0X00170201
#define HSK_LRP_Mux			0X001800FF
#define HSK_HFR_Mux			0X002800FF
#define HSK_BIU_Misc_Status		0X003800FF
#define HSK_BIU_RTI_Status		0X003900FF
#define HSK_LP_Sphere_Bias		0X003A00FF
#define HSK_LP_Cylinder_Bias		0X003B00FF
#define HSK_LP_Multiplex		0X003C00FF
#define HSK_LP_8155			0X003D00FF
