/*******************************************************************************
**   Header file containing essential declarations for pgdraw, display programs.
**               Author: Karthi Vadivelu 
*******************************************************************************/

#ifndef _dec_h_
#define _dec_h_

#define  MFR		  1
#define  HFR              2
#define  Langmuir_Probe   4
#define  WFR              8
#define  WBR              0xE
#define  FILL             0xF
#define  LFDR 		  0x7
#define  DUST 		  0xB
#define  STIM		  0x0

#define NOT_WANTED         -2   /*** If packet is of a different type **/ 

 #define DECOMP_NOT_REQD     0   /*** No action performed ****/
 #define INVALID_INST_TYPE  -3 
#define DONE                1 

#define EOF_REACHED        -1   /** If UTIL_getbuffer_CDS **/
                                /** returns -1            **/
int debug;


struct Time
    {
     char minute;
     char second;
    };


#endif
