
/*#
  #	biucmd5.c for the PPCRTIU system
  #*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/time.h>
#include <time.h>
#include <strings.h>

#include <fg.h>

#include <rtiu.h>
#include <util.h>

#include "rtiuh.h"

#define DLEN 32
#define TOK 20
#define TRANS 256
#define TELECMD_LEN 1024
#define STRING_BUFFER_SIZE 8192
#define USEC 1
#define MSEC 1000
#define _SEC  1000000

/****************************************************************************/
/* Compiled in config file directory */

#ifndef CFG
#error Compiled in configuration directory is missing.
#endif

/****************************************************************************/


char Version[] = { "V5.1" };
int alf = 0;
int oper = 0;
int delay = 0;
int line_length = 0;
time_t time_zero = 0;
static int time_mode;

struct trans
{
  int valid;                            /* 0 = end of list */
  char mnemonic[16];
  int rt1;                              /* source 1553 address                  */
  int rt2;                              /* destination 1553 address             */
  int sub1;                             /* 1553 sub address                     */
  int sub2;                             /*  secondary subaddress (RT->RT)       */
  int op;                               /* bus op 1= RT SEND                    */
  int tick;                             /* tick (0=inject)                      */
  int len;                              /* length                               */
  unsigned short data[DLEN];            /*  data values                         */
};
struct opcmd
{
  int fc;                               /* 0 = end of list */
  char mnemonic[16];
  struct rtiu_command command;
  char *comment;
};
enum
{
  OPCMD_END,
  OPCMD_control,
  OPCMD_table,
  OPCMD_time,
  OPCMD_init,
  OPCMD_data,
  OPCMD_delay,
  OPCMD_debug,
  OPCMD_load,
  OPCMD_rtiucmd,
  OPCMD_secmd,
  OPCMD_exit
};
struct opcmd op[] = {
  {OPCMD_exit, "quit", {NULL, NULL, 0, 0, 0, 0, 0, 0, 0},
   "exit biucmd5"},

  {OPCMD_exit, "exit", {NULL, NULL, 0, 0, 0, 0, 0, 0, 0},
   "exit biucmd5"},

  {OPCMD_debug, "debug", {NULL, NULL, 0, 0, 0, 0, 0, 0, 0},
   "set debugging flags"},

  {OPCMD_delay, "delay_alf", {NULL, NULL, 0, 0, 250, 0, 0, 0, 0},
   "inter-command delay 1/4 sec"},

  {OPCMD_delay, "delay_alf_slow", {NULL, NULL, 0, 0, 500, 0, 0, 0, 0},
   "inter-command delay 1/4 sec"},

  {OPCMD_delay, "delay_cmd", {NULL, NULL, 0, 0, 1000, 0, 0, 0, 0},
   "inter-command delay 1 sec"},

  {OPCMD_load, "load_alf", {NULL, NULL, 0, 0, 0, 0, 0, 0, 0},
   "load ALF file (RTIU)"},

  {OPCMD_rtiucmd, "rtiucmd", {NULL, NULL, 0, 0, 0, 0, 0, 0, 0},
   "ppcrtiu rtiucmd (RTIU)"},

  {OPCMD_secmd, "secmd", {NULL, NULL, 0, 0, 0, 0, 0, 0, 0},
   "ppcrtiu secmd (RTIU)"},

  {OPCMD_init, "init", {NULL, NULL, 0, 0, 0, 0, 0, 0, 0},
   "initialize rtiu"},

  {OPCMD_control, "time_off",
   {FL_define_SCT_enable,
    FC_define_SCT_enable,
    0, 0, 0, 0, 0, 0, 0},
   "disable S/C time bdcst"},

  {OPCMD_control, "time_on",
   {FL_define_SCT_enable,
    FC_define_SCT_enable,
    0, 0, 1, 0, 0, 0, 0},
   "enable S/C time bdcst"},

  {OPCMD_control, "data_off",
   {FL_define_collection,
    FC_define_collection,
    0, 0, 0, 0, 0, 0, 0},
   "disable data"},

  {OPCMD_control, "data_on",
   {FL_define_collection,
    FC_define_collection,
    0, 0, 1, 0, 0, 0, 0},
   "telemeter data"},

  {OPCMD_control, "run", {FL_define_mode,
                          FC_define_mode,
                          0, 0, 1, 0, 0, 0, 0},
   "run CDS simulator"},

  {OPCMD_control, "stop", {FL_define_mode,
                           FC_define_mode,
                           0, 0, 0, 0, 0, 0, 0},
   "stop CDS imulator"},

  {OPCMD_time, "zero", {FL_define_SCT,
                        FC_define_SCT,
                        0, 0, 0, 0, 0, 0, 0},
   "clear time field"},

  {OPCMD_time, "time", {FL_define_SCT,
                        FC_define_SCT,
                        0, 0, 1, 0, 0, 0, 0},
   "sec SCT to UT"},

  {OPCMD_time, "zulu", {FL_define_SCT,
                        FC_define_SCT,
                        0, 0, 2, 0, 0, 0, 0},
   "sec SCT to UT"},

  {OPCMD_table, "mode", {NULL, NULL, 0, 0, 0, 0, 0, 0, 0},
   "CDS Telemetry mode select"},

  {OPCMD_END, "", {NULL, NULL, 0, 0, 0, 0, 0, 0, 0},
   ""}
};

static int sub_address = { -1 };
static struct timeval timeout[3] = { 0, 100000, /* 0 */
  0, 100000,                            /* 1 */
  0, 375000                             /* 2 for secmd/rtiucmd */
};
static int rtiu_open = 0;
static int master_sequence = 1;
static int a_flag = 0;
static int x_flag = 0;
static char input_string[STRING_BUFFER_SIZE];
static char working_string[STRING_BUFFER_SIZE];


int build_opr (struct opcmd *op, struct rtiu_command *telecmd, char *token,
               int net_flag);

void pc_send (struct rtiu_command *telecmd, int net_flag)
{
  int i;
  int status = 0;
  static struct rtiu_command *xcmd = NULL;

  if (!xcmd)
    xcmd = malloc (TELECMD_LEN);

  telecmd->sequence = master_sequence++;
  /**/ if (a_flag) {
    fprintf (stderr,
             "pc_send l:%d fc:%4.4X st:%X sq:%d val:%X t:%04X%04X.%d\n",
             telecmd->length,
             telecmd->function_code,
             telecmd->status,
             telecmd->sequence,
             telecmd->value,
             telecmd->time[0], telecmd->time[1], telecmd->rti);
  }
  if (telecmd->function_code) {
    if (net_flag)
      status = rtiu_put5 (telecmd);
    /**/ if (status < 0) {
      printf ("BIUCMD %s=rtiu_put5(pc_send);\n", rtiu_mssg (status));
      rtiu_close (0);
      exit (0);
    }
  }
}
void pc_send_table (char *mode, int flag)
{
  int status;
  int i;

  if (flag)
    i = flag;
  else
    i = time_mode;
  status = rtiu_table_scan (NULL, mode, i, 1);
  if (status < 0) {
    printf
      ("BIUCMD %s %d=rtiu_table_scan(pc_send_table); TRANSACTION NOT FOUND\n",
       rtiu_mssg (status), status);
  }
}
void pc_send_time (int flag, int net_flag)
{
  int enable = 0;
  int status = 0;
  unsigned short from[3] = { 0, 0, 0 };
  unsigned short to[3] = { 0, 0, 0 };

  switch (flag) {
   case 0:
     if (net_flag)
       enable = 1;
     status = rtiu_time5 (to, from, enable);
     break;
   default:
     if (net_flag)
       enable = 3;
     status = rtiu_time5 (NULL, NULL, enable);
     break;
  }
  if (status < 0) {
    printf ("BIUCMD %X %s=rtiu_time5(pc_send_time);\n",
            status, rtiu_mssg (status));
    rtiu_close (0);
    exit (0);
  }
}
void pc_send_init ()
{
  int status;

  status = rtiu_init5 (NULL);
  if (status < 0) {
    printf ("BIUCMD %s=rtiu_init5(pc_send_init);\n", rtiu_mssg (status));
    rtiu_close (0);
    exit (0);
  }
}
struct trans *find_tr (char *token, struct trans *tr)
{
  int index = 0;

  while (tr[index].valid) {
    if (!strcasecmp (token, tr[index].mnemonic))
      return &tr[index];
    index += 1;
  }
  return NULL;
}

struct opcmd *find_op (char *token, struct opcmd *op)
{
  int index = 0;

  while (op[index].fc) {
    if (!strcasecmp (token, op[index].mnemonic))
      return &op[index];
    index += 1;
  }
  return NULL;
}

void x_send (struct rtiu_command *telecmd, char *type)
{
  int i;
  int length;

  if (!x_flag)
    return;
  if (x_flag)
    if (telecmd->length)
      rtiu_dump_header (telecmd, "x_send");
  length = (telecmd->length - 16) / 2;
  for (i = 0; i < length; i++) {
    printf ("  %4.4X", telecmd->data[i]);
    if ((i & 7) == 7) {
      switch (i) {
       case 7:
         if (telecmd->time[1])
           printf ("  SA%d %X%04X.%d\n",
                   telecmd->value & 0x1F,
                   telecmd->time[0], telecmd->time[1], telecmd->rti);
         else
           printf ("  SA%d\n", telecmd->value & 0x1F);
         break;
       default:
         printf ("\n");
         break;
      }
    }
  }
  if (i < 7)
    if (telecmd->time[1])
      printf ("  SA%d %X%04X.%d",
              telecmd->value & 0x1F,
              telecmd->time[0], telecmd->time[1], telecmd->rti);
    else
      printf ("  SA%d", telecmd->value & 0x1F);
  if (i != 7)
    printf ("\n");
  return;
}
void xc_send (struct rtiu_command *telecmd, char *type)
{
  int i;
  int length;
  int sa;
  unsigned int val;

  sa = telecmd->value & 0xFF;
  length = (telecmd->length - 16) / 2;
  for (i = 0; i < length; i++) {
    printf ("%4.4X ", telecmd->data[i]);
    if ((i % 16) == 14)
      if (i < 48)
        if (i)
          printf ("(SA %d)\n", sa);
        else
          printf ("\n");
  }
  if ((i % 16) != 14) {
    if (i < 48)
      if (i)
        printf ("(SA %d)", sa);
  }
  printf ("\n");
  return;
}

char *get_next_tok ()
{
  int more = 1;
  static char *str = NULL;
  static char *s1 = NULL;
  static char *s2 = NULL;

  while (more) {
    if (!str) {
      working_string[0] = 0;
      str = fgets (working_string, STRING_BUFFER_SIZE - 32, stdin);
      strcpy (input_string, working_string);
      working_string[strlen (working_string) - 1] = 0;
      if (!working_string)
        return NULL;
      s1 = str;
    }
    s2 = strtok (s1, " ,");
    s1 = NULL;
    if (s2)
      more = 0;
    else
      str = NULL;
  }
  return s2;
}
int get_next (int oper,
              struct opcmd *op,
              struct rtiu_command *newcmd, int flag, int net_flag)
{
  int opsts;
  int status;
  long i;
  char *term;
  char *tok;
  struct opcmd *o1;
  static struct rtiu_command *telecmd = NULL;
  char *mode = NULL;
  char md[128];

  if (!telecmd)
    telecmd = malloc (TELECMD_LEN);

  tok = get_next_tok ();
  if (!tok)
    return -2;


  i = -delay;
  if (time_mode) {
    i = time_mode;
  }
  if (rtiu_table_scan (NULL, tok, i, 0) > 0) {
    strcpy (md, tok);
    strcpy (tok, "mode");
    mode = md;
  }
  o1 = find_op (tok, op);
  if (oper)
    if (o1 || status) {
      bzero (telecmd, TELECMD_LEN);
      while (o1) {
        build_opr (o1, telecmd, mode, net_flag);
        tok = get_next_tok ();
        if (!tok)
          return -2;
        o1 = find_op (tok, op);
      }
    }
  if ((tok[0] == 's') || (tok[0] == 'S')) {
    if ((tok[1] == 't') || (tok[1] == 'T')) {
      int time_now, rti, offset;
      char *ptr = NULL;

      switch (tok[2]) {
       case 'z':
       case 'Z':
         time (&time_zero);
         offset = strtol (&tok[3], &ptr, 0);
         rti = strtol (&ptr[1], NULL, 0);
         time_zero += offset;
         newcmd->time[0] = time_zero >> 16;
         newcmd->time[1] = time_zero & 0x0FFFF;
         newcmd->rti = rti;
         break;
       case 'm':
       case 'M':
         offset = strtol (&tok[3], &ptr, 0);
         rti = strtol (&ptr[1], NULL, 0);
         if (offset)
           time_mode = offset + time_zero;
         else
           time_mode = 0;
         break;
       default:
         offset = strtol (&tok[2], &ptr, 0);
         rti = strtol (&ptr[1], NULL, 0);
         time_now = offset + time_zero;
         newcmd->time[0] = time_now >> 16;
         newcmd->time[1] = time_now & 0x0FFFF;
         newcmd->rti = rti;
         break;
      }
      if (flag)
        fprintf (stderr,
                 "ST flag <%s>, offset=%d rti=%d time_zero=%8X %04X%04X.%d\n",
                 tok, offset, rti, time_zero, newcmd->time[0],
                 newcmd->time[1], newcmd->rti);
    }
    if ((tok[1] == 'a') || (tok[1] == 'A')) {
      sub_address = strtol (&tok[2], &term, 10);
    }
    return -1;
  }
  if (tok[0] == 'q')
    return -2;
  if (tok[0] == 'Q')
    return -2;
  if (!strcmp (tok, "-exit"))
    return -2;
  i = strtol (tok, &term, 16);
  return i;
}
int min (int a, int b)
{
  if (a > b)
    return b;
  return a;
}
int unword (unsigned char *c)
{
  return c[0] + (c[1] * 256);
}
void word (unsigned char *x, int v)
{
  x[0] = (unsigned char) v & 0xFF;
  x[1] = (unsigned char) (v >> 8) & 0xFF;
  return;
}

void build_trs (struct trans * tr, struct rtiu_command * telecmd)
{
  int i;

  bzero (telecmd, TELECMD_LEN);

  telecmd->length = FL_header_size + (2 * tr->len);
  telecmd->function_code = FC_send_CDS_command;
  telecmd->value = (tr->rt1 << 8) | (tr->sub1 & 0xFF);
  for (i = 0; i < tr->len; i++)
    telecmd->data[i] = tr->data[i];
  return;
}

int build_opr (struct opcmd *op, struct rtiu_command *telecmd, char *token,
               int net_flag)
{
  if (op) {
    switch (op->fc) {
     case OPCMD_delay:
       timeout[1].tv_sec = op->command.value / 1000;
       timeout[1].tv_usec = (op->command.value % 1000) * MSEC;
       if (!timeout[1].tv_sec)
         if (timeout[1].tv_usec < (125 * MSEC))
           timeout[1].tv_usec = 125 * MSEC;
       fprintf (stderr, "OPCMD_delay %3d.%03d\n", timeout[1].tv_sec,
                timeout[1].tv_usec / 1000);
       break;
     case OPCMD_time:
       pc_send_time (op->command.value, net_flag);
       break;
     case OPCMD_table:
       pc_send_table (token, -delay);
       break;
     case OPCMD_init:
       if (0)
         pc_send_init ();
       else
         cdsseq_put5 ("cdsseq activate initrpws 0:0", line_length, 0);
       break;
     case OPCMD_debug:
       rtiu_debug (0xFFFF);
       break;
     case OPCMD_control:
       pc_send (&op->command, net_flag);
       break;
     case OPCMD_data:
       pc_send (telecmd, net_flag);
       break;
       /*
        *      commands that make use of the PPCRTIU protocol to send
        *      
        */
     case OPCMD_load:                  /* load */
       cdsseq_put5 ("CDSSEQ ACTIVATE ldrpws 0:0", line_length, 0);
       memset (working_string, 0, STRING_BUFFER_SIZE);
       select (0, NULL, NULL, NULL, &timeout[2]);
       break;

     case OPCMD_rtiucmd:               /* rtiucmd */
       /*
        * fprintf(stderr,"XXX rtiucmd %s\n", input_string); /*
        */
       rtiucmd_put5 (&input_string[8], line_length, 0);
       memset (working_string, 0, STRING_BUFFER_SIZE);
       select (0, NULL, NULL, NULL, &timeout[2]);
       break;

     case OPCMD_secmd:                 /* secmd */
       cdsseq_put5 (&input_string[6], line_length, 0);
       memset (working_string, 0, STRING_BUFFER_SIZE);
       select (0, NULL, NULL, NULL, &timeout[2]);
       break;

     default:
       bzero (telecmd, TELECMD_LEN);
       break;
    }
    bzero (telecmd, FL_header_size);
  }
  return op->fc;
}
void display_tr (struct trans *tr, int index)
{
  if (tr) {
    printf ("Index  %i\n", index);
    printf ("  valid flag               %i\n", tr->valid);
    printf ("  mnemonic                 %s\n", tr->mnemonic);
    printf ("  source 1553 address      %i\n", tr->rt1);
    printf ("  destination 1553 address %i\n", tr->rt2);
    printf ("  1553 sub address         %i\n", tr->sub1);
    printf ("  secondary subaddress     %i\n", tr->sub2);
    printf ("  bus op 1= RT SEND        %i\n", tr->op);
    printf ("  tick (0=inject           %i\n", tr->tick);
    printf ("  length                   %i\n", tr->len);
    for (index = 0; index < min (DLEN, tr->len); index++)
      printf ("%4.4X  ", tr->data[index]);
    printf ("\n\n");
  }
  return;
}


void display_op (struct opcmd *op, int index)
{
  if (op) {
    printf ("Operator Command dump\n");
    printf ("  fc flag        %i\n", op->fc);
    printf ("  mnemonic       %s\n", op->mnemonic);
    printf ("  function       %s\n", op->comment);
  }
  return;
}

void dump_op (void)
{
  int j;
  int i = 0;
  char buffer[17] = { "                " };

  printf ("Operator commands:\n");
  while (op[i].fc != OPCMD_END) {
    strcpy (buffer, op[i].mnemonic);
    for (j = strlen (buffer); j < 17; j++)
      buffer[j] = 0x20;
    printf ("    %s %s\n", buffer, op[i].comment);
    i += 1;
  }
  return;
}

void tokens (char *buf, char *tok[])
{
  char sep[] = { " \t" };
  char *c;
  int i;

  for (i = 0; i < TOK; i++)
    tok[i] = NULL;
  if (tok[0] = strtok (buf, sep))
    for (i = 1; i < TOK; i++) {
      tok[i] = strtok (NULL, sep);
      if (!tok[i])
        break;
    }
  return;
}
struct parse_table
{
  int ctl;
  char mne[32];
} parse[] = {
1, "TRANS", 2, "DATA1", 3, "DATA2", 99, "#", 0, ""};
int init_tr (struct trans *tr)
{
  char buf[128];
  char *tok[TOK];
  FILE *trs;

 /*
#ifdef __RPWSHP2__
  char path[256] = { "/usr/cassini/cfg/rpws2.trs" };
#else
  char path[256] = { "/opt /project /cassini /cfg /rpws2.trs" };
#endif
 */
  
	char path[256] = {CFG "/rpws2.trs"};
 
  char *newpath;
  char type[] = { "r" };
  int index;
  int trans_count = -1;

  newpath = getenv ("trs");
  if (newpath)
    strcpy (path, newpath);
  trs = fopen (path, type);
  while (1) {
    if (!fgets (buf, 127, trs))
      break;
    tokens (buf, tok);
    index = 0;
    while (parse[index].ctl) {
      if (!strcasecmp (parse[index].mne, tok[0]))
        break;
      index += 1;
    }
    switch (parse[index].ctl) {
     default:
     case 99:
       break;
     case 1:
       trans_count += 1;
       tr[trans_count].valid = 1;
       strcpy (tr[trans_count].mnemonic, tok[1]);
       tr[trans_count].rt1 = strtol (tok[2], NULL, 0);
       tr[trans_count].rt2 = strtol (tok[3], NULL, 0);
       tr[trans_count].sub1 = strtol (tok[4], NULL, 0);
       tr[trans_count].sub2 = strtol (tok[5], NULL, 0);
       tr[trans_count].op = strtol (tok[6], NULL, 0);
       tr[trans_count].tick = strtol (tok[7], NULL, 0);
       tr[trans_count].len = strtol (tok[8], NULL, 0);
       break;
     case 2:
       for (index = 0; index < 16; index++) {
         if (index >= TOK)
           break;
         if (tok[index + 1])
           tr[trans_count].data[index] = strtol (tok[index + 1], NULL, 16);
         else
           tr[trans_count].data[index] = 0;
       }
       break;
     case 3:
       for (index = 0; index < 16; index++) {
         if (index >= TOK)
           break;
         if (tok[index + 1])
           tr[trans_count].data[index + 16] =
             strtol (tok[index + 1], NULL, 16);
         else
           tr[trans_count].data[index + 16] = 0;
       }
       break;
    }
  }
  return trans_count;
}
char *biu_gets (char *in)
{
  int internal = 1;
  char *s;

  while (internal) {
    internal = 0;
    s = gets (in);
    if (strstr (in, "-exit"))
      return 0;
  }
  return s;
}
void main (int argc, char *argv[])
{
  int net_flag = 1;
  static struct rtiu_command *telecmd = NULL;
  int interactive = 1;
  int p_flag = 1;
  int m_flag = 0;
  int c_flag = 0;
  int d_flag = 0;
  int t_flag = 0;
  int o_flag = 0;
  int bell = 0;
  int one_shot = 0;
  int delta = 0;
  int sa = 7;
  int i;
  int trcnt, opcnt;
  char *tok[TOK];
  char cmd[STRING_BUFFER_SIZE] = { 0 };
  char *host = "default";
  int port = 0;
  struct trans tr[TRANS];
  struct trans *t0;
  struct opcmd *o0;
  struct timeval tpold, tpnew;
  struct timezone tpz;

  fg_flags (argc, argv);
  timeout[1].tv_sec = 0;
  timeout[1].tv_usec = 250 * MSEC;
  fprintf (stderr, "main delay %3d.%03d\n", timeout[1].tv_sec,
           timeout[1].tv_usec / 1000);

  fprintf (stdout, "biucmd5");
  fflush (stdout);
  for (i = 0; i < TRANS; i++) {
    tr[i].valid = 0;
  }

  fprintf (stdout, "-PPC");
  fflush (stdout);

  if (fg_flag ("host"))
    host = fg_flagc ("host");

  fprintf (stdout, "RT");
  fflush (stdout);

  port = strtol (fg_flagc ("port"), NULL, 0);


  fprintf (stdout, "IU");
  fflush (stdout);

  trcnt = init_tr (tr);

  fprintf (stdout, " %s", Version);
  fflush (stdout);

  if (fg_flag ("debug"))
    rtiu_debug (0xFFFF);

  fprintf (stdout, ", %s\n", rtiu_dump (RTIU_STRING_version));
  fflush (stdout);
  {
    if (fg_flag ("doper")) {
      dump_op ();
      exit (0);
    }
    if (fg_flag ("help") | fg_flag ("h")) {
      printf ("    xxxx xxxx S     send data\n");
      printf ("    xxxx xxxx SAn   send data, new sub address\n");
      printf ("    xxxx xxxx STZn  send data, zero start time now+n\n");
      printf ("                    skip STZ to do all in absolute time\n");
      printf ("    xxxx xxxx STMn  sets time_mode (for mode change?)\n");
      printf ("    xxxx xxxx STn   send data, delta from STZ\n");
      printf ("  \n");
      printf ("    +pflag (d)   display packet delivered to PCRTIU\n");
      printf ("    -pflag         supress\n");
      printf ("    +cflag       display packet delivered to BIU\n");
      printf ("    -cflag         supress\n");
      printf ("    +dflag       display transaction table entry\n");
      printf ("    -dflag (d)     supress\n");
      printf ("    +mflag       display status messages\n");
      printf ("    -mflag (d)     supress\n");
      printf ("    +tflag       display timing information\n");
      printf ("    -tflag (d)     supress\n");
      printf ("    +aflag       display pc_send\n");
      printf ("    -aflag (d)     supress\n");
      printf ("    +xflag (d)   display x_send\n");
      printf ("    -xflag         supress\n");
      printf ("    +oflag       display timed command fields\n");
      printf ("    -oflag (d)     supress\n");
      printf ("    -debug       enable some debugging features\n");
      printf ("    -oper        accept operator commands\n");
      printf ("    -doper       dump operator command list\n");
      printf ("    -t nnn (%i)  mSec between ALF blocks\n",
              timeout[1].tv_usec / 1000);
      printf ("                   (this may appear in stdin stream)\n");
      printf ("    -delta nnn   Delta Current SCT Value\n");
      printf ("    -delay nnn   Mode change delay (30 sec)\n");
      printf ("    -cmd xxx     one-shot command(script)\n");
      printf ("    -exit        exit program\n");
      printf ("    -host xxx    specify host (setenv PCRTIU xxx)\n");
      printf ("    +bell        sound bell after command delivered\n");
      printf ("    -bell  (d)     supress\n");
      printf ("    -net         supress network activity (testing)\n");
      printf
        ("    -hex         accept command patterns from 'stdin' (command )\n");
      printf ("                   display command in word format\n");
      printf
        ("                   supress packet delivery display (-pflag)\n");
      printf ("\n");
      printf
        ("    -alf              accept command patterns from 'stdin' (ALF loads)\n");
      printf
        ("                         terminate commands with 's' to send group\n");
      printf ("                           hhhh hhh hhhh\n");
      printf ("                           hhhh hhh hhhh s\n");
      printf ("    -sa nnn            Change BIU sub-address (d=7)\n");
      printf ("\n");
      printf ("    -load              CDS FSW load simulation\n");
      printf ("                         FSW resides on pprcrtiu\n");
      printf ("    -ppcrtiulen nnn    PPCRTIU line length\n");
      printf ("                         line breaks are inserted at\n");
      printf ("                         the next space character that\n");
      printf ("                         occurs AFTER this column\n");
      printf ("    -rtiucmd string    RTIUCMD command to ppcrtiu\n");
      printf ("                         RTIUCMD \"string\"\n");
      printf ("    -secmd string      SECMD command to ppcrtiu\n");
      printf ("                         SECMD \"string\"\n");
      printf (" \n");
      printf ("  environment variables\n");
      printf ("     trs has the path to the command .TRS file\n");
      printf (" \n");
      printf (" NETWORK ACTIVITY DISABLED -host(%s)!!!!\a\n", host);
      printf (" \n");
      net_flag = 0;
      p_flag = 1;
    }
    if (fg_flag ("bell") == '+')
      bell = 1;
    if (fg_flag ("bell") == '-')
      bell = 0;
    if (fg_flag ("pflag") == '+')
      p_flag = 1;
    if (fg_flag ("dflag") == '+')
      d_flag = 1;
    if (fg_flag ("mflag") == '+')
      m_flag = 1;
    if (fg_flag ("aflag") == '+')
      a_flag = 1;
    if (fg_flag ("xflag") == '-')
      x_flag = 1;
    if (fg_flag ("xflag") == '+')
      x_flag = 2;
    if (fg_flag ("tflag") == '+')
      t_flag = 1;
    if (fg_flag ("pflag") == '-')
      p_flag = 0;
    if (fg_flag ("dflag") == '-')
      d_flag = 0;
    if (fg_flag ("mflag") == '+')
      m_flag = 1;
    if (fg_flag ("tflag") == '-')
      t_flag = 0;
    delta = fg_int ("delta", delta);
    delay = fg_int ("delay", delay);
    sa = fg_int ("sa", sa);
    line_length = fg_int ("ppcrtiulen", line_length);
    if (fg_flag ("net") == '-') {
      net_flag = 0;
      if (m_flag)
        printf (" NETWORK ACTIVITY DISABLED !!!! %X\a\n", net_flag);
    }

    if (fg_flag ("alf") == '-') {
      interactive = 0;
      alf = 1;
    }
    if (fg_flag ("hex") == '-') {
      c_flag = 1;
      p_flag = 0;
      interactive = 0;
      alf = 1;
    }
    if (fg_flag ("cflag"))
      if (fg_flag ("pflag") == '+')
        p_flag = 1;

    if (fg_flag ("cflag") == '-')
      c_flag = 0;
    if (fg_flag ("cflag") == '+')
      c_flag = 1;

    if (fg_flag ("oflag") == '-')
      o_flag = 0;
    if (fg_flag ("oflag") == '+')
      o_flag = 1;

    if (fg_flag ("oper")) {
      oper = 1;
    }
    /*
     *          Delay in mSec
     */
    if (fg_flag ("t") == '-') {
      timeout[1].tv_sec = fg_int ("t", 0) / MSEC;
      timeout[1].tv_usec = ((MSEC * fg_int ("t", 0)) % _SEC);
      if (!timeout[1].tv_sec)
        if (timeout[1].tv_usec < 125 * MSEC)
          timeout[1].tv_usec = 125 * MSEC;
      if (m_flag)
        printf ("main(timeout) %i.%03i\n", timeout[1].tv_sec,
                timeout[1].tv_usec / MSEC);
      fprintf (stderr, "-t delay %3d.%03d\n", timeout[1].tv_sec,
               timeout[1].tv_usec / 1000);
    }
    if (fg_flag ("cmd") == '-') {
      strcpy (cmd, fg_flagc ("cmd"));
      one_shot = 1;
      interactive = 1;
      oper = 1;
      alf = 0;

    }
  }

  if (telecmd)
    fprintf (stderr, "Double Allocation \"telecmd\"\n");
  else
    telecmd = malloc (TELECMD_LEN);

  if (!telecmd)
    printf
      ("BIUCMD unable to allocate memory buffer for sendiong commands\n");


  if (net_flag) {
    int status;

    status = rtiu_tlm_init5 (host, 0, RTIU_direction_out);
    if (status < 0) {
      printf ("BIUCMD rtiu_tlm_init5(%s, 0);\n", rtiu_mssg (status), host);
      return;
    }
  } else
    printf ("BIUCMD rtiu_tlm_init5(NOT CALLED);\n");

  if (interactive) {
    int active = 1;

    if (m_flag) {
      if (oper)
        printf ("biucmd5, biu & oper commands ready\n");
      else
        printf ("biucmd5, biu commands ready\n");
    }
    if (!one_shot)
      if (!biu_gets (cmd))
        active = 0;
    while (active) {
      char type[16];

      tokens (cmd, tok);

      t0 = find_tr (tok[0], tr);
      {
        tok[1] = tok[0];
        tok[0] = "mode";
      }
      if (rtiu_table_scan (NULL, tok[0], -delay, 0) > 0) {
        tok[1] = tok[0];
        tok[0] = "mode";
      }
      o0 = find_op (tok[0], op);
      /*
       * Allow exit/quit 
       */
      if (o0)                           /* command to be processed */
        if (!oper)                      /* even if not running in */
          if (op->fc != OPCMD_exit)     /* operator mode */
            o0 = NULL;                  /*  */

      if (d_flag) {
        if (t0)
          display_tr (t0, 0);
        if (o0)
          display_op (o0, 0);
      }
      type[0] = 0;
      if (t0) {
        build_trs (t0, telecmd);
        strcpy (type, "biu");
      }
      if (o0) {
        build_opr (o0, telecmd, tok[1], 0);
        strcpy (type, "oper");
        if (o0->fc == OPCMD_exit) {
          active = 0;
          strcpy (type, "_");
        }
      }
      if (type[0]) {
        if (type[1]) {
          if (net_flag)
            pc_send (telecmd, net_flag);
          if (p_flag)
            x_send (telecmd, type);
          if (c_flag)
            xc_send (telecmd, type);
        }
      } else {
        if (m_flag)
          printf ("main() INVALID COMMAND\n");
      }
      if (one_shot)
        active = 0;
      else {
        if (active)
          if (!biu_gets (cmd))
            active = 0;
      }
      op->command.time[0] = 0;
      op->command.time[1] = 0;
      op->command.rti = 0;
    }
  }
  if (alf) {
    int active = 1;
    int value;
    int status;

    gettimeofday (&tpold, &tpz);
    while (active) {
      int recnt = 1;

      i = 0;
      bzero (telecmd, TELECMD_LEN);
      if (delta) {
        /*
         * Delta Current SCT Value 
         */
      }
      telecmd->length = FL_header_size;
      telecmd->function_code = FC_send_CDS_command;
      telecmd->value = (25 << 8) | (sa & 0xFF);
      if (m_flag)
        printf ("biucmd5, rpws commands\n");
      value = get_next (oper, op, telecmd, o_flag, net_flag);
      while (value >= 0) {              /* while (value != 0xffff) */
        telecmd->data[i++] = value;
        telecmd->length = telecmd->length + 2;

        value = get_next (oper, op, telecmd, o_flag, net_flag);
        if (sub_address != -1)
          telecmd->value = (25 << 8) | (sub_address & 0xFF);
        if (i > 128) {
          x_send (telecmd, "ERROR");
          active = 0;
          break;
        }
      }
      if (telecmd->length > 16) {
        static char type[] = { "inst" };
        active = 1;
        if (delta)
          delta = delta + 1;
        if (net_flag)
          pc_send (telecmd, net_flag);
        if (bell)
          fprintf (stderr, "\a");
        if (telecmd->time[0] | telecmd->time[1])
          select (0, NULL, NULL, NULL, &timeout[2]);
        else
          select (0, NULL, NULL, NULL, &timeout[1]);
        if (p_flag)
          x_send (telecmd, type);
        if (c_flag)
          xc_send (telecmd, type);
        gettimeofday (&tpnew, &tpz);
        if (t_flag)
          printf ("fwrite %d %d %d\n",
                  status,
                  recnt++,
                  _SEC *
                  (tpnew.tv_sec - tpold.tv_sec) +
                  (tpnew.tv_usec - tpold.tv_usec));
        tpold = tpnew;
      }
      if (value == -2)
        active = 0;
      op->command.time[0] = 0;
      op->command.time[1] = 0;
      op->command.rti = 0;
    }
  }
  if (interactive) {
    printf ("biucmd5, exiting\n");
  }
  if (net_flag) {
    int status;

    status = rtiu_close (0);
    if (interactive)
      printf ("BIUCMD rtiu_closew(0)\n");
  }
  return;
}
