#
#	biucmd1.c for the PC-RTIU
#
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <pcrtiu.h>
#include <util.h>
#include <utilt.h>

#define DLEN 32
#define TOK 20
#define TRANS 256

char Version[] = { "V1.2" };

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
  int valid;                            /* 0 = end of list */
  char mnemonic[16];
  char ident[16];
  int command;
  int flag;                             /* bytes 12/13 */
  int vec;
  int T1;
  int T2;
  int op[4];
  int mode;
  int begin;
  int rti;
  int mask;                             /*  time mask */
  int delta;                            /*  Sec offset */
  double offset;                        /* mSec offset */
};
static char op_mne[][32] = { "",
  "Set CDS Mode",
  "Enable Net Data Logging",
  "Set Debug Mode",
  "Set S/C Time",
  "CDS services",
  "Change S/C Telemetry mode",
  "Flush Tables",
  "Set RTI/DTS Ship COunters",
  ""
};

static int sub_address = { -1 };
static struct timeval timeout[3];

void pc_send (char *host, unsigned char *telecmd)
{
  int status;

  status = pcrtiu_init_socket (host, 1024);
  status = pcrtiu_putCDS (telecmd);
  status = pcrtiu_close_socket ();
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

  while (op[index].valid) {
    if (!strcasecmp (token, op[index].mnemonic))
      return &op[index];
    index += 1;
  }
  return NULL;
}

void x_send (char *host, unsigned char *telecmd, char *type)
{
  int i;

  printf ("x_send (%i) to %s  type(%s)", unword (&telecmd[2]), host, type);
  if (telecmd[10] == 4) {
    printf (" T1:%d T2:%d",
            (unsigned int) telecmd[14] | (unsigned int) telecmd[15] << 8,
            (unsigned int) telecmd[16] | (unsigned int) telecmd[17] << 8);
  }
  for (i = 0; i < unword (&telecmd[2]); i++) {
    if (!(i % 16))
      printf ("\n");
    printf ("%2.2X ", telecmd[i]);

  }
  printf ("\n");
  return;
}
void xc_send (char *host, unsigned char *telecmd, char *type)
{
  int i;
  unsigned int val;

  for (i = 32; i < unword (&telecmd[2]); i = i + 2) {
    val = telecmd[i] + (telecmd[i + 1] * 256);
    printf ("%4.4X ", val);
    if ((i % 16) == 14)
      if (i < 48)
        printf ("(SA %d)\n", telecmd[13]);
      else
        printf ("\n");
  }
  if ((i % 16) != 14) {
    if (i < 48)
      printf ("(SA %d)", telecmd[13]);
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
  static char string[1024];

  while (more) {
    if (!str) {
      string[0] = 0;
      str = fgets (string, 1000, stdin);
      string[strlen (string) - 1] = 0;
      if (!string)
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
int get_next (int oper, struct opcmd *op, char *host, unsigned char *tcmd,
              int disconnect)
{
  int status;
  long i;
  char *term;
  char *tok;
  struct opcmd *o1;
  unsigned char telecmd[256];

  tok = get_next_tok ();
  if (!tok)
    return -2;

  o1 = find_op (tok, op);
  if (oper)
    if (o1) {
      memcpy (telecmd, tcmd, 256);
      while (o1) {
        build_opr (o1, telecmd);
        if (disconnect)
          pc_send (host, telecmd);
        else
          status = pcrtiu_putCDS (telecmd);
        if (o1->command < 0)
          printf ("ALIGN ");
        else
          printf ("      ");
        x_send (host, telecmd, "PC-RTIU control");
        tok = get_next_tok ();
        if (!tok)
          return -2;
        o1 = find_op (tok, op);
      }
    }
  if ((tok[0] == 's') || (tok[0] == 'S')) {
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

build_trs (struct trans * tr, unsigned char *telecmd)
{
  int i;

  memset (telecmd, 0, 32);
  telecmd[0] = 2;
  word (&telecmd[2], min (32 + DLEN * 2, (tr->len * 2) + 32));
  telecmd[12] = (unsigned char) tr->rt1;
  telecmd[13] = (unsigned char) tr->sub1;
  telecmd[14] = (unsigned char) tr->op;
  telecmd[16] = (unsigned char) tr->tick;
  word (&telecmd[20], min (DLEN * 2, tr->len * 2));
  telecmd[15] = 0;
  for (i = 0; i < DLEN; i++) {
    word (&telecmd[i * 2 + 32], tr->data[i]);
  }
}
build_opr (struct opcmd *op, unsigned char *telecmd)
{
  int i;
  struct timeval ut;
  struct timezone tz;

  memset (telecmd, 0, 32);
  word (&telecmd[0], 1);
  word (&telecmd[2], 32);
  word (&telecmd[10], op->command);
  word (&telecmd[12], op->flag);
  switch (op->command) {
   case -1:
     UTIL_align_on_second ((double) 0.0, op->offset);   /* */
     word (&telecmd[10], 1);
     break;
   case -4:
     UTIL_align_on_second ((double) 0.0, op->offset);   /* */
     gettimeofday (&ut, &tz);
     word (&telecmd[10], 4);
     ut.tv_sec += op->delta;
     word (&telecmd[14], ((ut.tv_sec & op->mask) >> 16) & 0xFFFF);
     word (&telecmd[16], ((ut.tv_sec & op->mask) >> 00) & 0xFFFF);
     printf ("UT %X %s", ut.tv_sec, ctime (&ut.tv_sec));
     break;
   case 4:
     word (&telecmd[14], op->T1 & 0x1FFF);
     word (&telecmd[16], op->T2);
     break;
   case 5:
     word (&telecmd[12], op->vec);
     for (i = 0; i < min (strlen (op->ident), 10); i++)
       telecmd[i + 14] = op->ident[i];
     word (&telecmd[24], op->op[0]);
     word (&telecmd[26], op->op[1]);
     word (&telecmd[28], op->op[2]);
     word (&telecmd[30], op->op[3]);
     break;
   case 6:
     word (&telecmd[14], op->mode);
     break;
   case 8:
     word (&telecmd[14], op->begin);
     word (&telecmd[16], op->rti);
  }
  return;
}
void display_op (struct opcmd *op, int index)
{
  if (op) {
    printf ("Index  %i\n", index);
    printf ("  valid flag             %i\n", op->valid);
    printf ("  mnemonic               %s\n", op->mnemonic);
    printf ("  command                %i %s\n", op->command,
            op_mne[op->command]);
    printf ("  trans ident            %s\n", op->ident);
    printf ("  flag                   %i\n", op->flag);
    printf ("  cds service vector     %i\n", op->vec);
    printf ("  T1 13 bit              %i\n", op->T1);
    printf ("  T2 16 bit              %i\n", op->T2);
    printf ("  optional parms         %2.4X %2.4X %2.4X %2.4X\n", op->op[0],
            op->op[1], op->op[2], op->op[3]);
    printf ("  mode                   %i\n", op->mode);
    printf ("  rti/dts skip count     %i\n", op->begin);
    printf ("  rti number             %i\n", op->rti);
  }
  return;
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
int init_op (int argc, char *argv[], struct opcmd *op)
{
  char buf[128];
  char *tok[TOK];
  FILE *opr;
  char path[256] = { "/usr/cassini/cfg/rpws2.opr" };
  char *newpath;
  char type[] = { "r" };
  int index;
  int opr_count = -1;

  newpath = getenv ("opr");
  if (newpath)
    strcpy (path, newpath);
  opr = fopen (path, type);
  while (1) {
    if (!fgets (buf, 127, opr))
      break;
    tokens (buf, tok);
    if (!strcasecmp (tok[0], "trans")) {
      index = strtol (tok[2], NULL, 0);
      opr_count = opr_count + 1;
      op[opr_count].command = index;
      switch (index) {
       case -1:
       case 1:
       case 2:
       case 3:
       case 4:
       case -4:
       case 5:
       case 6:
       case 7:
       case 8:
         op[opr_count].flag = strtol (tok[3], NULL, 0);
         op[opr_count].valid = 1;
         strcpy (op[opr_count].mnemonic, tok[1]);
         break;
      }
      switch (index) {
       case 6:
         op[opr_count].mode = strtol (tok[4], NULL, 0);
         break;
      }
      switch (index) {
         int i;

       case -1:
         op[opr_count].offset = strtod (tok[4], NULL);
         break;
       case -4:
         op[opr_count].mask = strtol (tok[4], NULL, 0);
         op[opr_count].delta = strtol (tok[5], NULL, 0);
         op[opr_count].offset = strtod (tok[6], NULL);
         break;
       case 4:
         op[opr_count].T1 = strtol (tok[4], NULL, 0);
         op[opr_count].T2 = strtol (tok[5], NULL, 0);
         break;
       case 5:
         op[opr_count].vec = strtol (tok[3], NULL, 0);
         strcpy (op[opr_count].ident, tok[4]);
         for (i = 0; i < 4; i++)
           op[opr_count].op[i] = strtol (tok[i + 5], NULL, 0);
         break;
       case 8:
         op[opr_count].begin = strtol (tok[4], NULL, 0);
         op[opr_count].rti = strtol (tok[5], NULL, 0);
         break;
      }
    }
  }
  return opr_count;
}
struct parse_table
{
  int ctl;
  char mne[32];
} parse[] = {
1, "TRANS", 2, "DATA1", 3, "DATA2", 99, "#", 0, ""};
int init_tr (int argc, char *argv[], struct trans *tr)
{
  char buf[128];
  char *tok[TOK];
  FILE *trs;
  char path[256] = { "/usr/cassini/cfg/rpws2.trs" };
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
    if (strstr (in, "-t")) {
      internal = 1;
      timeout[1].tv_usec = (1000 * atoi (&in[3]) % 1000000);
      timeout[1].tv_sec = atoi (&in[3]) / 1000;
      if (timeout[1].tv_usec < 125000)
        timeout[1].tv_usec = 125000;
    }
  }
  return s;
}
void main (int argc, char *argv[])
{
  unsigned char telecmd[16384] = { 2, 0,        /* 0  1  1 msg id 2=telecommand */
    0x20, 0,                            /* 2  2 pkt len (changed below) */
    0, 0,                               /* 4  3 execute time T1 */
    0, 0,                               /* 5  4 execute time T2 */
    0, 0,                               /* 6  5 execute type 0=immed */
    0, 0,                               /*10  6 command id (inject/delete) */
    0,                                  /*12  7 Bus RT (RPWS) */
    0,                                  /*13  8 SubAdr (Nominal Cmds) */
    0,                                  /*14  9 1553 op (BC to RT) */
    0,                                  /*15 10 data length (use 15) */
    0,                                  /*16 11 RTI tick (execute ASAP) */
    0,                                  /*17 12 RTI periodic code (use F-11) */
    0,                                  /*18 13 data log (none) */
    0,                                  /*19 14 data format (who cares) */
    0, 0,                               /*20 15 augmented data length */
    0, 0,                               /*22 16 transaction demand (k) */
    0, 0,                               /*24 17 trans indiv rep cyc (m) */
    0, 0,                               /*26 18 augmented rti tick */
    0, 0,                               /*28 spare */
    0, 0
  };                                    /*30 spare */
  int interactive = 1;
  int alf = 0;
  int oper = 0;
  int p_flag = 1;
  int m_flag = 1;
  int c_flag = 0;
  int d_flag = 0;
  int t_flag = 0;
  int disconnect = 0;
  int net_flag = 1;
  int bell = 0;
  int one_shot = 0;
  int delta = 0;
  int sa = 7;
  int i;
  int trcnt, opcnt;
  char *tok[TOK];
  char cmd[512] = { 0 };
  char *host;
  struct trans tr[TRANS];
  struct trans *t0;
  struct opcmd op[TRANS];
  struct opcmd *o0;
  struct timeval tpold, tpnew;
  struct timezone tpz;

  timeout[1].tv_sec = 0;
  timeout[1].tv_usec = 400000;

  for (i = 0; i < TRANS; i++) {
    tr[i].valid = 0;
    op[i].valid = 0;
  }

  host = pcrtiu_hostname (NULL, argc, argv);
  trcnt = init_tr (argc, argv, tr);
  opcnt = init_op (argc, argv, op);

  for (i = 1; i < argc; i++) {
    if (!strcmp ("-h", argv[i]) || !strcmp ("-help", argv[i])) {
      printf ("biucmd %s flags:\n", Version);
      printf ("    +pflag (d)   display packet delivered to PCRTIU\n");
      printf ("    -pflag         supress\n");
      printf ("    +cflag       display packet delivered to BIU\n");
      printf ("    -cflag         supress\n");
      printf ("    +dflag       display transaction table entry\n");
      printf ("    -dflag (d)     supress\n");
      printf ("    +mflag (d)   display status messages\n");
      printf ("    -mflag         supress\n");
      printf ("    +tflag       display timing information\n");
      printf ("    -tflag (d)     supress\n");
      printf ("    -oper        accept operator commands\n");
      printf ("    -t nnn (%i)  mSec between ALF blocks\n",
              timeout[1].tv_usec / 1000);
      printf ("                   (this may appear in stdin stream)\n");
      printf ("    -delta nnn   Delta Current SCT Value\n");
      printf ("    -cmd xxx     one-shot command(script)\n");
      printf ("    -exit        exit program\n");
      printf ("    -host xxx    specify host (setenv PCRTIU xxx)\n");
      printf ("    +bell        sound bell after command delivered\n");
      printf ("    -bell  (d)     supress\n");
      printf ("    -disc        disconnect after each transaction\n");
      printf ("    -net         supress network activity (testing)\n");
      printf
        ("    -hex         accept command patterns from 'stdin' (command )\n");
      printf ("                   display command in word format\n");
      printf
        ("                   supress packet delivery display (-pflag)\n");
      printf
        ("    -alf         accept command patterns from 'stdin' (ALF loads)\n");
      printf
        ("                  terminate commands with 's' to send group\n");
      printf ("                     hhhh hhh hhhh\n");
      printf ("                     hhhh hhh hhhh s\n");
      printf ("    -sa nnn      Change BIU sub-address (d=7)\n");
      printf (" \n");
      printf ("  environment variables\n");
      printf ("     opr has the path to the operator .TRS file\n");
      printf ("     trs has the path to the command .TRS file\n");
      printf (" \n");
      printf (" NETWORK ACTIVITY DISABLED -host(%s)!!!!\a\n", host);
      printf (" \n");
      net_flag = 0;
      p_flag = 1;
    }
    if (!strcmp ("+bell", argv[i]))
      bell = 1;
    if (!strcmp ("-bell", argv[i]))
      bell = 0;
    if (!strcmp ("+pflag", argv[i]))
      p_flag = 1;
    if (!strcmp ("+dflag", argv[i]))
      d_flag = 1;
    if (!strcmp ("+mflag", argv[i]))
      m_flag = 1;
    if (!strcmp ("+tflag", argv[i]))
      t_flag = 1;
    if (!strcmp ("-pflag", argv[i]))
      p_flag = 0;
    if (!strcmp ("-dflag", argv[i]))
      d_flag = 0;
    if (!strcmp ("-mflag", argv[i]))
      m_flag = 0;
    if (!strcmp ("-tflag", argv[i]))
      t_flag = 0;
    if (!strcmp ("-delta", argv[i]))
      delta = atoi (argv[i + 1]);
    if (!strcmp ("-sa", argv[i]))
      sa = atoi (argv[i + 1]);
    if (!strcmp ("-disc", argv[i]))
      disconnect = 1;
    if (!strcmp ("-net", argv[i])) {
      net_flag = 0;
      if (m_flag)
        printf (" NETWORK ACTIVITY DISABLED !!!!\a\n");
    }

    if (!strcmp ("-alf", argv[i])) {
      interactive = 0;
      alf = 1;
    }
    if (!strcmp ("-hex", argv[i])) {
      c_flag = 1;
      p_flag = 0;
      interactive = 0;
      alf = 1;
    }
    if (!strcmp ("-cflag", argv[i]))
      c_flag = 0;
    if (!strcmp ("+cflag", argv[i]))
      c_flag = 1;
    if (!strcmp ("-oper", argv[i])) {
      oper = 1;
    }
    if (!strcmp ("+oper", argv[i])) {
      oper = 1;
    }
    if (!strcmp ("-t", argv[i])) {
      timeout[1].tv_usec = (1000 * atoi (argv[i + 1]) % 1000000);
      timeout[1].tv_sec = atoi (argv[i + 1]) / 1000;
      if (timeout[1].tv_usec < 125000)
        timeout[1].tv_usec = 125000;
      if (m_flag)
        printf ("main(timeout) %i.%3.3i\n", timeout[1].tv_sec,
                timeout[1].tv_usec / 1000);
    }
    if (!strcmp ("-cmd", argv[i])) {
      strcpy (cmd, argv[i + 1]);
      one_shot = 1;
      interactive = 1;
      oper = 1;
      alf = 0;

    }
  }

  if (interactive) {
    int active = 1;

    if (m_flag) {
      if (oper)
        printf ("biucmd, biu & oper commands ready\n");
      else
        printf ("biucmd, biu commands ready\n");
    }
    if (!one_shot)
      if (!biu_gets (cmd))
        active = 0;
    while (active) {
      char type[16];

      tokens (cmd, tok);
      t0 = find_tr (tok[0], tr);
      if (oper)
        o0 = find_op (tok[0], op);
      else
        o0 = NULL;
      if (d_flag) {
        if (t0)
          display_tr (t0, 0);
        if (o0)
          display_op (o0, 0);
      }
      type[0] = 0;
      if (t0 != NULL) {
        build_trs (t0, telecmd);
        strcpy (type, "biu");
      }
      if (o0 != NULL) {
        build_opr (o0, telecmd);
        strcpy (type, "oper");
      }
      if (type[0]) {
        if (net_flag)
          pc_send (host, telecmd);
        if (p_flag)
          x_send (host, telecmd, type);
        if (c_flag)
          xc_send (host, telecmd, type);
      } else {
        if (m_flag)
          printf ("main() INVALID COMMAND\n");
      }
      if (one_shot)
        active = 0;
      else if (!biu_gets (cmd))
        active = 0;
    }
  }
  if (alf) {
    int active = 1;
    int value;
    int status;

    gettimeofday (&tpold, &tpz);
    if (net_flag)
      if (!disconnect)
        status = pcrtiu_init_socket (host, 1024);
    while (active) {
      int recnt = 1;

      i = 0;
      telecmd[2] = 32;                  /* base length */
      if (delta) {
        telecmd[6] = delta & 0xFF;
        telecmd[7] = (delta >> 8) & 0xFF;
        telecmd[8] = 2;                 /* Delta Current SCT Value */
      }
      telecmd[12] = 25;                 /* RPWS */
      telecmd[13] = sa;                 /* nominal commands (usually 7) */
      telecmd[15] = 0;
      if (m_flag)
        printf ("biucmd, rpws commands\n");
      value = get_next (oper, op, host, telecmd, disconnect);   /* scanf ("%x", &value);   */
      while (value >= 0) {              /* while (value != 0xffff) */
        telecmd[i + 32] = value & 255;
        telecmd[i + 33] = value / 256;
        telecmd[2]++;
        telecmd[2]++;
        telecmd[15]++;
        i++;
        i++;
        value = get_next (oper, op, host, telecmd, disconnect); /* scanf ("%x", &value);   */
        if (sub_address != -1)
          telecmd[13] = sub_address;
        if (i > 16384) {
          x_send (host, telecmd, "ERROR");
          telecmd[15] = 0;
          active = 0;
          break;
        }
      }
      if (telecmd[15] != 0) {
        static char type[] = { "inst" };
        active = 1;
        if (delta)
          delta = delta + 1;
        if (net_flag)
          if (disconnect)
            pc_send (host, telecmd);
          else
            status = pcrtiu_putCDS (telecmd);
        if (bell)
          fprintf (stderr, "\a");
        select (0, NULL, NULL, NULL, &timeout[1]);
        if (p_flag)
          x_send (host, telecmd, type);
        if (c_flag)
          xc_send (host, telecmd, type);
        gettimeofday (&tpnew, &tpz);
        if (t_flag)
          printf ("fwrite %d %d %d\n",
                  status,
                  recnt++,
                  1000000 *
                  (tpnew.tv_sec - tpold.tv_sec) +
                  (tpnew.tv_usec - tpold.tv_usec));
        tpold = tpnew;
      }
      if (value == -2)
        active = 0;
    }
    if (net_flag)
      if (!disconnect)
        status = pcrtiu_close_socket ();
  }                                     /* end of console loop */
  return;
}
