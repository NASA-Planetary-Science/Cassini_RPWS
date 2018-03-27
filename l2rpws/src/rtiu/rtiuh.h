 /*
  *     FL_ expressed in BYTES
  *     FP_ expressed in words
  *     FC_ is a 'function code' for the RTIU
  */

#define   FL_msg_size		1024
#define   FL_header_size	16
#define   FL_status_size	36
#define   FL_time		 6
#define   FL_time_second	 4


#define   FC_return_data 0xFC20
#define   FC_return_mode 0xFC21
#define   FL_return_mode 16
#define   FC_return_hand 0xFC22
#define   FL_return_hand 16

#define   FC_define_RT_address  0xFC03
#define   FL_define_RT_address  16
#define   FC_define_SCT         0xFC05
#define   FL_define_SCT         22
#define   FD_define_SCT         6
#define   FC_define_SCT_enable  0xFC07
#define   FL_define_SCT_enable  16
#define   FC_define_handshake   0xFC0B
#define   FL_define_handshake   16
#define   FC_define_TLM_mode    0xFA07
#define   FL_define_TLM_mode    196
#define   FD_define_TLM_mode    180
#define   FC_define_collection  0xFA0B
#define   FL_define_collection  16
#define   FC_define_mode        0xFA14
#define   FL_define_mode        16

#define   FC_send_CDS_command   0xFC0A
#define   FC_send_mode_code     0xFA10
#define   FL_send_mode_code     16

struct rtiu_command
{
  unsigned short length;
  unsigned short function_code;
  unsigned short status;
  unsigned short sequence;
  unsigned short value;
  unsigned short time[2];
  unsigned short rti;
  unsigned short data[1];
};

#define   FP_header		0
#define   FP_length		0
#define   FP_function_code	1
#define   FP_status		2
#define   FP_sequence_number	3
#define   FP_value		4
#define   FP_from_time		5
#define   FP_seconds_msw	5
#define   FP_seconds_lsw	6
#define   FP_rti		7

#define   FP_data		8
#define   FP_1553_status	8
#define   FP_to_time		8
#define   FP_time_seconds_msw	8

#define   FP_time_seconds_lsw	9
#define   FP_time_rti		10
#define   FP_command		19
#define   FP_telemetry		26
#define   FP_tlm_CCSDS		26
#define   FP_tlm_seq		28
#define   FP_tlm_len		30

#define   FPppc_Length		 0
#define   FPppc_CmdType		 1
#define   FPppc_CmdTime		 2
#define   FPppc_CmdString	 5


#define RTIU_BAD_SOCKET		-1
#define RTIU_BAD_SOURCE		-2
#define RTIU_BAD_TARGET		-3
#define RTIU_BAD_CONNECT	-4
#define RTIU_BAD_TRANSACTION	-5
#define RTIU_BAD_SEND		-6
#define RTIU_BAD_RECV		-7
#define RTIU_BAD_TRANS_LOAD	-8
#define RTIU_BAD_TRANS_FILE	-9
#define RTIU_BAD_BIND		-10
#define RTIU_REDUNDANT_INIT	-11
#define RTIU_BAD_TRANS_TABLE	-12
#define RTIU_BAD_DATA		-13
#define RTIU_INVALID_MODE	-14
#define RTIU_direction_in	1
#define RTIU_direction_out	2
#define RTIU_direction_both	3

enum
{
  RTIU_DUMP_current_transaction,
  RTIU_DUMP_current_transaction_table,
  RTIU_DUMP_hostname,
  RTIU_DUMP_targetname,
  RTIU_DUMP_hostaddr,
  RTIU_DUMP_targetaddr,
  RTIU_DUMP_sct,
  RTIU_DUMP_sct_table,
  RTIU_DUMP_trans_file,
  RTIU_STRING_hostname,
  RTIU_STRING_targetname,
  RTIU_STRING_sct,
  RTIU_STRING_version
};

#ifndef _rtiuh_
#define _rtiuh_
extern int rtiu_get (struct rtiu_command *);
extern int rtiu_put (struct rtiu_command *);
extern int rtiu_put5 (struct rtiu_command *);
extern int rtiu_init (char *);
extern int rtiu_debug (int);
extern int rtiu_tlm_init (char *, int, int);
extern int rtiu_table (char *, char *, unsigned long, int);
extern int rtiu_table_scan (char *, char *, unsigned long, int);
extern int rtiu_close (int);
extern int rtiu_echo (int);
extern int rtiu_data (int);
extern int rtiu_run (int);
extern char *rtiu_dump (int);
extern char *rtiu_mssg (int);
extern void rtiu_dump_header (struct rtiu_command *, char *);
extern int rtiu_swap (struct rtiu_command *);
extern int rtiu_time (unsigned short *, unsigned short *, int);
extern long rtiu_time_sct (long);
extern int rtiucmd_put (char *cmd, int len);
extern int cdsseq_put (char *cmd, int len);
extern int rtiucmd_put5 (char *cmd, int len, int time);
extern int cdsseq_put5 (char *cmd, int len, int time);
extern int rtiu_time5 (unsigned short *, unsigned short *, int);
extern int rtiu_init5 (char *);
extern int rtiu_tlm_init5 (char *, int, int);
extern int rtiu_table_scan5 (char *, char *, unsigned long, int);

void rtiu_dump_packet (unsigned short *header, char *string);
char *rtiu_fc (unsigned short *buf);
#endif
