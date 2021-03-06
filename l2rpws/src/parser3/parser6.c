#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <time.h>
#include "parser.h"

#define BATCH_FLAG 0x10
#define ALF_FLAG   0x20

#define SUB_ADDRESS_BIU		 3
#define SUB_ADDRESS_INSTRUMENT	 7
#define SUB_ADDRESS_MEMORY	 7
#define SUB_ADDRESS_NONE	32

static char title[] = { "Parser" };
static char version[] = { "V6.3" };
static char defw[] = { "  Dw" };
static char Sequence_File_Path[] =
  { "/opt/project/cassini/rpwsrtiu/sequence/" };
static int IEB_word_count = 0;
static char tabs[32];

int time_base = 0;
int time_offset = 0;
int time_offset_flag = 0;
int time_rti = 0;

int cdsseq_flag = 0;                    /* set to switch from HEURIKON to  SECMD */
int cdsseq_line_flag = 0;
int cdsseq_rti = 0;
int cdsseq_time = 0;
int cdsseq_delta = 0;
char cdsseq_name[256];
char cdsseq_file_name[256];
char text_enum_string[1025];

FILE *in_file = NULL;
FILE *cdsseq_file = stdout;
int in_file_active = 0;
int page_length = 44;
int page_width = 128;
char sub_address[8] = { "SA7 " };       /* "type=" determines sub-address to send to */
static int alf_sub_address = SUB_ADDRESS_MEMORY;
static int cmd_sub_address = SUB_ADDRESS_INSTRUMENT;
static unsigned long alf_count_limit;
static int sub_address_previous = 7;
static int sub_address_current = 7;
static char sub_address_list[34][8] = { "SA0  ",
  "SA1  ",
  "SA2  ",
  "SA3  ",
  "SA4  ",
  "SA5  ",
  "SA6  ",
  "SA7  ",
  "SA8  ",
  "SA9  ",
  "SA10 ",
  "SA11 ",
  "SA12 ",
  "SA13 ",
  "SA14 ",
  "SA15 ",
  "SA16 ",
  "SA17 ",
  "SA18 ",
  "SA19 ",
  "SA20 ",
  "SA21 ",
  "SA22 ",
  "SA23 ",
  "SA24 ",
  "SA25 ",
  "SA26 ",
  "SA27 ",
  "SA28 ",
  "SA29 ",
  "SA30 ",
  "SA31 ",
  "NONE ",
  ""
};

void print_help (FILE * otfile)
{
  fprintf (otfile, "%s %s\n", title, version);
  fprintf (otfile, "  Command Line  Options:\n");
  fprintf (otfile, "\t-help          This message.\n");
  fprintf (otfile, "\t-h             This message.\n");
  fprintf (otfile, "\t-send          Appends 'SAx' after every command.\n");
  fprintf (otfile,
           "\t-alf xxx       Appends 'SAx' after every xxx commands.\n");
  fprintf (otfile, "\t-sub_address_alf xxx\n");
  fprintf (otfile,
           "\t               Alters sub-address used for ALF records\n");
  fprintf (otfile, "\t-sub_address_cmd xxx\n");
  fprintf (otfile,
           "\t               Alters sub-address used for instrument commands\n");
  fprintf (otfile, "\t-wrap xxx      Generates wrapped commands.\n");
  fprintf (otfile,
           "\t-lwrap xxx     Generates wrapped commands (with length).\n");
  fprintf (otfile,
           "\t-pwrap xxx     Generates wrapped commands (with parands).\n");
  fprintf (otfile, "\t-ieb           Generates assembler lines (%s).\n",
           defw);
  fprintf (otfile, "\t-ieb xxx         assembler lines with label.\n");
  fprintf (otfile, "\t-pagelength n  Set desired page width\n");
  fprintf (otfile, "\t-pagewidth n   Set desired page width\n");
  fprintf (otfile, "\t-avocet xxx    AVOCET assembler. (xxx is label)\n");
  fprintf (otfile,
           "\t-masm xxx      Microsoft MASM assembler. (xxx is label)\n");
  fprintf (otfile, "\t-batch n       Appends 's' after every n words.\n");
  fprintf (otfile, "\t                 Commands are not segmented.\n");
  fprintf (otfile, "\t-cdsfile       Adds line breaks to cdsseq lines\n");
  fprintf (otfile, "                    \n");
  fprintf (otfile, "Input Options:\n");
  fprintf (otfile, "\tA valid command.\n");
  fprintf (otfile, "\t-in            read a file with commands\n");
  fprintf (otfile, "\t                   e.g. '-in map.alf'\n");
  fprintf (otfile, "\t-send          Appends 'SAx'\n");
  fprintf (otfile, "\n");
  fprintf (otfile,
           "\t-rti n         set RTI number for timed commands (0-7)\n");
  fprintf (otfile,
           "\t-zero xxx      Like '-send' sets start time (offset from now)\n");
  fprintf (otfile,
           "\t                   Time zero is 'xxx' seconds from now\n");
  fprintf (otfile, "\t                   Emits an invalid command\n");
  fprintf (otfile,
           "\t-time xxx      Like '-send' but timed in rtiu (offset from zero)\n");
  fprintf (otfile,
           "\t00:00 command  Timed commands, like -time (offset from zero)\n");
  fprintf (otfile,
           "\t00:00 -mode x  Timed S/C mode command (Heurikon only)\n");
  fprintf (otfile, "\n");
  fprintf (otfile, "\t-alf_count xxx\n");
  fprintf (otfile, "\t               Alters the size of ALF commands\n");
  fprintf (otfile, "\t-sub_address_alf xxx\n");
  fprintf (otfile,
           "\t               Alters sub-address used for ALF records\n");
  fprintf (otfile, "\t-sub_address_cmd xxx\n");
  fprintf (otfile,
           "\t               Alters sub-address used for instrument commands\n");
  fprintf (otfile,
           "\t-list          command_name Outputs the command definition\n");
  fprintf (otfile, "\t                  corresponding to command_name.\n");
  fprintf (otfile, "\t                  e.g. '-list 73wbr_mode_cntl'\n");
  fprintf (otfile,
           "\t-list          pattern* Outputs command names containing pattern.\n");
  fprintf (otfile, "\t                  e.g. '-list 73w*', '-list alf*'\n");
  fprintf (otfile, "\t-help (-h)     This message.\n");
  fprintf (otfile, "\n");
  fprintf (otfile, "SECMD Sequence Control:\n");
  fprintf (otfile,
           "\t-cdsseq_begin name         Start Sequence and Set sequence-flag\n");
  fprintf (otfile,
           "\t-cdsseq_register name      Register Sequence and Clear sequence-flag\n");
  fprintf (otfile,
           "\t                             secmd CDSSEQ REGISTER name\n");
  fprintf (otfile, "\t-cdsseq_activate name time Activate Sequence\n");
  fprintf (otfile, "\t                             close file 'name'.seq\n");
  fprintf (otfile,
           "\t                             secmd CDSSEQ ACTIVATE name time:0\n");
  fprintf (otfile,
           "\t-cdsseq_deregister name    Deregister Sequence and Clear sequence-flag\n");
  fprintf (otfile,
           "\t                             secmd CDSSEQ DEREGISTER name\n");
  fprintf (otfile,
           "\t-cdsseq_flush name         Flush Sequence and Clear sequence-flag\n");
  fprintf (otfile,
           "\t                             secmd CDSSEQ FLUSH name\n");
  fprintf (otfile, "\t-cdsseq_load name         Load Sequence\n");
  fprintf (otfile, "\t                             secmd CDSSEQ SHOW name\n");
  fprintf (otfile,
           "\t-cdsseq_file name delta   Start Sequence and Set sequence-flag\n");
  fprintf (otfile,
           "\t                             emit sequence to file 'name'.seq\n");
  fprintf (otfile,
           "\t                               file premissions: (U:RW G:RW O:R)\n");
  fprintf (otfile,
           "\t                               sequence file path: (PPCRTIU_IEB)\n");
  fprintf (otfile,
           "\t                               delta: use \"fast\" for 1/RTI loading\n");
  fprintf (otfile, "\t                               default: %s\n",
           Sequence_File_Path);
  fprintf (otfile,
           "\t-cdsseq_end                End of Sequence and clear sequence-flag\n");
  fprintf (otfile, "\t                             close file 'name'.seq\n");
  fprintf (otfile, "\n");
  fprintf (otfile, "\t---- To make a file for the PPCRTIU ----\n");
  fprintf (otfile, "\t\t-cdsseq_file <name> <delta>\n");
  fprintf (otfile, "\t\t... timed commands\n");
  fprintf (otfile, "\t\t-cdsseq_register <name>\n");
  fprintf (otfile, "\t\t-cdsseq_end\n");
  fprintf (otfile, "\t\t-cdsseq_load <name>\n");
  fprintf (otfile, "\t\t-cdsseq_activate <name> <time>\n");
  fprintf (otfile,
           "\t\t\tUse Activate control on the command screen with <name>\n");
  fprintf (otfile, "\t Once a sequence has bee reformatted (and palced  \n");
  fprintf (otfile, "\t   into the PPCRTIU sequence directory\n");
  fprintf (otfile, "\t   \n");
  fprintf (otfile, "\t---- Direct delivery to PPCRTIU ----\n");
  fprintf (otfile, "\t\t-cdsseq_begin <name>\n");
  fprintf (otfile, "\t\t... timed commands\n");
  fprintf (otfile, "\t\t-cdsseq_register <name>\n");
  fprintf (otfile, "\t\t-cdsseq_activate <name> <time>\n");
  fprintf (otfile, "\t   \n");
  fprintf (otfile, "\t   \n");
  fprintf (otfile, "\n");
  fprintf (otfile, "Environmental Variables:\n");
  fprintf (otfile, "\tcmd_txt       <full path name of cmd.txt file>.\n");

#ifdef __RPWSHP2__
  fprintf (otfile,
           "\t                  e.g. 'setenv cmd_txt /usr/cassini/cfg/cmd.txt'\n");
#else
  fprintf (otfile,
           "\t                  e.g. 'setenv cmd_txt /opt/project/cassini/cfg/cmd.txt'\n");
#endif

  fprintf (otfile,
           "\t                  This is the file that contains the command\n");
  fprintf (otfile,
           "\t                  translation table.  This define the bit\n");
  fprintf (otfile,
           "\t                  pattern produced by the 3-271 command stems\n");
  fprintf (otfile, "\t                  \n");
  fprintf (otfile,
           "\tPPCRTIU_IEB       Directory to save reformatted IEB files\n");
  fprintf (otfile,
           "\t                    that will be loaded into PPCRTIU sequence\n");
  fprintf (otfile,
           "\t                    memory.  Default /opt/project/cassini/rpwsrtiu/ieb\n");
  fprintf (otfile, "\n");
  fprintf (otfile, "Special Keys:\n");
  fprintf (otfile, "\t<Control d> exits program.\n");
  fprintf (otfile, "\n");
  fprintf (otfile, "File Format changes\n");
  fprintf (otfile, "  Input Files:\n");
  fprintf (otfile, "              Paranthesis are treated like whitespace\n");
  fprintf (otfile, "                to allow compatibility with JPL/TTACS\n");
  fprintf (otfile, "  Output File:\n");
  fprintf (otfile, "              Paranthesis in wrap commands (-pwrap)\n");
  fprintf (otfile, "\n");
  fprintf (otfile, "  Sequence File:\n");
  fprintf (otfile,
           "              Translates from our timed command format\n");
  fprintf (otfile,
           "                to that accepted by the PPCRTIU to build\n");
  fprintf (otfile,
           "                a sequence.  PPCRTIU requires hexadecimal\n");
  fprintf (otfile,
           "                command patterns in a similar but different\n");
  fprintf (otfile, "                format from the Heurikon\n");
  fprintf (otfile, "\n");
  return;
}
void send_record (char *sub_address, int flag)
{
  if (cdsseq_flag)
    return;

  switch (flag) {
   case 0:
   case 1:
     if (time_base) {
       printf ("0000 SA7 STZ%d.%d\n", time_base, time_rti);
       time_base = 0;
       return;
     }
     if (time_offset_flag) {
       printf ("ST%d.%d\n", time_offset, time_rti);
       return;
     }

     printf ("%s", sub_address);
     return;
   case 2:
     printf ("STM%d.%d\n", time_offset, time_rti);
     return;
   case 3:
     printf ("STM0.0\n");
     return;
  }
}
int calculate_offset (char *line, int *sec, int *rti)
{
  char *temp;
  int delta;

  switch (line[0]) {
   case '0':                           /*  */
   case '1':                           /* If it a number             */
   case '2':                           /*  just go ahead and convert */
   case '3':                           /*  and accumulate            */
   case '4':                           /*  */
   case '5':                           /*  */
   case '6':                           /*  */
   case '7':                           /*  */
   case '8':                           /*  */
   case '9':                           /*  */
     sec[0] += strtol (line, &temp, 10);
     calculate_offset (temp, sec, rti);
     break;
   case ':':                           /* colon means shift and call again */
     sec[0] *= 60;
     calculate_offset (&line[1], sec, rti);
     break;
   case '.':                           /* dot indicates RTI, so just do it */
     rti[0] = strtol (&line[1], &temp, 10);
     break;
   default:                            /* doesn't belong, so return */
     break;
  }
  return 0;
}
int old_calculate_offset (char *line)
{
  char *tmp;
  int delta = 0;
  int hours = 0;
  int minutes = 0;
  int seconds = 0;

  tmp = line;
  delta = strtol (tmp, &tmp, 10);
  if (tmp[0] == ':') {
    minutes = delta;
    tmp++;
    delta = strtol (tmp, &tmp, 10);
    if (tmp[0] == ':') {
      hours = minutes;
      minutes = delta;
      tmp++;
      delta = strtol (tmp, &tmp, 10);
    }
  }
  seconds = delta;

  delta = hours * 3600 + minutes * 60 + seconds;        /* expressed in decimal RTI's */
  return delta;
}

char *pars_length (char *txt)
{
  static char strtemp1[256];
  static char strtemp2[256];
  static int index;

  strcpy (tabs, "\t\t\t\t\t\t\t\t");
  sprintf (strtemp1, "%s_len equ ($-%s)/2", txt, txt);
  index = 6 - (strlen (strtemp1) / 8);
  if (index <= 0)
    index = 1;
  tabs[index] = 0;
  sprintf (strtemp2, "%s%s; number of words in this trigger\n",
           strtemp1, tabs);
  return strtemp2;
}
int wrapieb (int f_)
{
  static int flag = 0;

  if (f_ != -1)
    flag = f_;
  return flag;
}
int wrapl (int f_)
{
  static int flag = 0;

  if (f_ != -1)
    flag = f_;
  return flag;
}
int wrapp (int f_)
{
  static int flag = 0;

  if (f_ != -1)
    flag = f_;
  return flag;
}
char *wrap (char *Command_Name)
{
  static char C_Name[80];

  if (Command_Name != NULL) {
    if (C_Name[0] != (char) 0) {
      print_input_error (0,
                         "Programming or command line",
                         DT_UNSIGNED,
                         NULL, "wrap initialized twice", __FILE__, __LINE__);
      exit (0);
    }
    strcpy (C_Name, Command_Name);
  }
  if (C_Name[0] != 0) {
    return C_Name;
  }
  return NULL;
}
char *wraplbl (char *Label_Name)
{
  static char L_Name[80];
  static char result[82];
  static int index = 1;

  if (!Label_Name)
    return NULL;                        /* what the ?!? */
  if (!strcmp (Label_Name, "***")) {
    index += 1;
    return L_Name;
  }
  if (!strcmp (Label_Name, "???")) {
    return L_Name;
  }
  if (Label_Name != NULL) {
    if (L_Name[0] != (char) 0) {

      print_input_error (0,
                         "Programming or command line",
                         DT_UNSIGNED,
                         NULL,
                         "wraplbl initialized twice", __FILE__, __LINE__);
      exit (0);
    }
    strcpy (L_Name, Label_Name);
  }

  if (L_Name[0] != 0) {
    sprintf (result, "%s_%d", L_Name, index);
    return result;
  }
  return NULL;
}

int end_output_line (unsigned long count, unsigned short size, char *line,
                     char *argv[], unsigned long options)
{
  static unsigned long control_flags = 0;
  static unsigned long call_count;
  static long batch_count_limit;
  union
  {
    unsigned short unsig;
    short sign;
  } short_word;
  union
  {
    unsigned long unsig;
    long sign;
  } long_word;
  int i = 1;
  char c[MAX_DECODE_FILE_LINE];
  char *s;
  int j;


  if (argv != NULL) {
    /*
     * set up the command line options ... this should be the first time through 
     */
    while (i < count) {
      strcpy (c, argv[i]);
      convert_to_lower_case (c);
      if (s = strstr (c, "-cdsfile")) {
        i++;
        if (i > count) {
          print_input_error (i,
                             "Command line",
                             DT_CHAR,
                             c,
                             "CDS File select all FUBAR'd",
                             __FILE__, __LINE__);
          exit (0);
        }
        cdsseq_line_flag = -1;
        continue;
      }

      if (s = strstr (c, "-wrap")) {
        i++;
        if (i >= count) {
          print_input_error (i,
                             "Command line",
                             DT_CHAR,
                             c,
                             "Command name expected after -wrap",
                             __FILE__, __LINE__);
          exit (0);
        }
        wrap (argv[i]);
        i++;
        continue;
      }
      if (s = strstr (c, "-pwrap")) {
        i++;
        if (i >= count) {
          print_input_error (i,
                             "Command line",
                             DT_CHAR,
                             c,
                             "Command name expected after -pwrap",
                             __FILE__, __LINE__);
          exit (0);
        }
        wrap (argv[i]);
        wrapp (1);
        i++;
        continue;
      }
      if (s = strstr (c, "-lwrap")) {
        i++;
        if (i >= count) {
          print_input_error (i,
                             "Command line",
                             DT_CHAR,
                             c,
                             "Command name expected after -lwrap",
                             __FILE__, __LINE__);
          exit (0);
        }
        wrap (argv[i]);
        wrapl (1);
        i++;
        continue;
      }
      /*
       *    -ieb
       *    -ieb label
       */
      if (s = strstr (c, "-ieb")) {
        wrap (defw);
        wrapieb (1);
        i++;
        if (i < count) {
          if (argv[i][0] != '-') {
            wraplbl (argv[i]);
            printf (";\n");
            printf ("; IEB translation from parser %s\n", version);
            printf (";\n");
            printf ("%s:\n", wraplbl (NULL));
            i++;
          }
        }
        continue;
      }
      /*
       *    -pagelength label
       */
      if (s = strstr (c, "-pagelength")) {
        i++;
        if (i >= count) {
          print_input_error (i,
                             "Command line",
                             DT_CHAR,
                             c,
                             "Pagelength expected after -pagewidth",
                             __FILE__, __LINE__);
          exit (0);
        }
        page_length = atoi (argv[i]);
        i++;
        continue;
      }
      /*
       *    -pagewidth label
       */
      if (s = strstr (c, "-pagewidth")) {
        i++;
        if (i >= count) {
          print_input_error (i,
                             "Command line",
                             DT_CHAR,
                             c,
                             "Pagewidth name expected after -pagewidth",
                             __FILE__, __LINE__);
          exit (0);
        }
        page_width = atoi (argv[i]);
        i++;
        continue;
      }
      /*
       *    -avocet label
       */
      if (s = strstr (c, "-avocet")) {
        wrap (defw);
        wrapieb (0x42);
        i++;
        if (i >= count) {
          print_input_error (i,
                             "Command line",
                             DT_CHAR,
                             c,
                             "Command name expected after -lwrap",
                             __FILE__, __LINE__);
          exit (0);
        }
        wraplbl (argv[i]);
        printf ("$Paginate\n");
        printf ("$Title(%s: IEB block)\n", wraplbl ("???"));
        printf ("$Date()\n");
        printf ("$Version(%s)\n", version);
        printf ("$Pagewidth=%d\n", page_width);
        printf ("$Pagelength=%d\n", page_length);
        printf ("\n");
        printf ("\n");
        printf (";\n");
        printf ("; IEB translation from parser %s\n", version);
        printf (";\n");
        printf ("\n");
        printf ("\n");
        printf ("\tGlobal\t%s\n", wraplbl (NULL));
        printf ("\tGlobal\t%s_len\n", wraplbl (NULL));
        printf ("\n");
        printf ("\tDefSeg\tIEB_Commands\n");
        printf ("\tSeg\tIEB_Commands\n");
        printf ("\n");
        printf ("%s:\n", wraplbl (NULL));
        i++;
        continue;
      }
      /*
       *    -masm label
       */
      if (s = strstr (c, "-masm")) {
        wrap (defw);
        wrapieb (0x43);
        i++;
        if (i >= count) {
          print_input_error (i,
                             "Command line",
                             DT_CHAR,
                             c,
                             "Command name expected after -lwrap",
                             __FILE__, __LINE__);
          exit (0);
        }
        wraplbl (argv[i]);
        printf ("\tTITLE\t%s: IEB block\n", wraplbl ("???"));
        printf ("\tPAGE\t%d,%d\n", page_length, page_width);
        printf ("\n");
        printf (";\n");
        printf ("; IEB translation from parser %s\n", version);
        printf (";\n");
        printf ("\tGlobal\t%s\n", wraplbl (NULL));
        printf ("\tGlobal\t%s_len\n", wraplbl (NULL));
        printf ("\n");
        printf ("%s\tProc\n", wraplbl (NULL));
        i++;
        continue;
      }

 /***************************************************************/
      if (s = strstr (c, "-batch")) {
        control_flags |= BATCH_FLAG;
        j = -1;
        if (s[0] != 0) {
          batch_count_limit = read_number (s, &j, 0);
        }
        if (j == -1) {
          i++;
          if (i >= count) {
            print_input_error (i,
                               "Command line",
                               DT_UNSIGNED,
                               c, "Number expected after -batch",
                               __FILE__, __LINE__);
            exit (0);
          }
          strcpy (c, argv[i]);
          batch_count_limit = read_number (c, &j, 0);
          if (j == -1) {
            print_input_error (i,
                               "Command line",
                               DT_UNSIGNED,
                               c,
                               "Number expected after -batch",
                               __FILE__, __LINE__);
            exit (0);
          }
        }
      }
      if (s = strstr (c, "-h")) {
        print_help (stdout);
        exit (0);
      }
      if (s = strstr (c, "-send")) {
        control_flags |= ALF_FLAG;
        alf_count_limit = 0;
      }
      if (s = strstr (c, "-alf")) {
        control_flags |= ALF_FLAG;
        j = -1;
        if (s[0] != 0) {
          alf_count_limit = read_number (s, &j, 0);
        }
        if (j == -1) {
          i++;
          if (i >= count) {
            print_input_error (i, "Command line", DT_UNSIGNED, c,
                               "Number expected after -alf", __FILE__,
                               __LINE__);
            exit (0);
          }
          strcpy (c, argv[i]);
          alf_count_limit = read_number (c, &j, 0);
          if (j == -1) {
            print_input_error (i, "Command line", DT_UNSIGNED, c,
                               "Number expected after -alf", __FILE__,
                               __LINE__);
            exit (0);
          }
        }
      }
      if (s = strstr (c, "-sub_address_alf")) {
        j = -1;
        if (s[0] != 0) {
          alf_sub_address = read_number (s, &j, 0);
        }
        if (j == -1) {
          i++;
          if (i >= count) {
            print_input_error (i, "Command line", DT_UNSIGNED, c,
                               "Number expected after -alf_sub_address",
                               __FILE__, __LINE__);
            exit (0);
          }
          strcpy (c, argv[i]);
          alf_sub_address = read_number (c, &j, 0);
          if (j == -1) {
            print_input_error (i, "Command line", DT_UNSIGNED, c,
                               "Number expected after -alf_sub_address",
                               __FILE__, __LINE__);
            exit (0);
          }
        }
      }
      if (s = strstr (c, "-sub_address_cmd")) {
        j = -1;
        if (s[0] != 0) {
          cmd_sub_address = read_number (s, &j, 0);
        }
        if (j == -1) {
          i++;
          if (i >= count) {
            print_input_error (i, "Command line", DT_UNSIGNED, c,
                               "Number expected after -cmd_sub_address",
                               __FILE__, __LINE__);
            exit (0);
          }
          strcpy (c, argv[i]);
          cmd_sub_address = read_number (c, &j, 0);
          if (j == -1) {
            print_input_error (i, "Command line", DT_UNSIGNED, c,
                               "Number expected after -cmd_sub_address",
                               __FILE__, __LINE__);
            exit (0);
          }
        }
      }
      i++;
    }
    return 0;
  }


  if (wrap (NULL) == NULL) {            /*sub addresses not valid if -wrap */
    if (sub_address_previous != sub_address_current) {
      if (!cdsseq_flag)
        printf ("%s", sub_address_list[sub_address_previous]);
      sub_address_previous = sub_address_current;
    }
  }
  if (options == 0)
    options = ~options;
  if ((options & BATCH_FLAG) & control_flags) {
    if ((count + (unsigned long) size) >= batch_count_limit) {
      if (wrap (NULL) == NULL) {
        send_record (sub_address, 0);
      }
      short_word.unsig = size;
      long_word.sign = short_word.sign;
      if (long_word.sign > batch_count_limit) {
        fprintf (stderr, "Input Warning: Command size(%d) "
                 "exceeds -batch(%d) length.\n",
                 long_word.sign, batch_count_limit);
      }
      return 1;
    }
  }
  if (line == NULL)
    return 0;
  if ((options & ALF_FLAG) & control_flags) {
    call_count++;
    if (call_count >= alf_count_limit) {
      call_count = 0;
      if (wrap (NULL) == NULL) {
        send_record (sub_address, 0);
      }
      return 1;
    }
  }

  strcpy (c, line);
  convert_to_lower_case (c);
  if (strstr (c, "-send")) {
    time_offset = 0;
    if (wrap (NULL) == NULL) {
      send_record (sub_address, 0);
    }
    return 1;
  }
  return 0;
}


/* add to list if add is one, else delimit line */
void comment_delimit (char *line, int add)
{
  static struct comment *first = NULL;
  struct comment *current;
  struct comment *x;

  if (add == 1) {

    current = (struct comment *) malloc (sizeof (struct comment));
    strcpy (current->delimiter, line);
    current->next = NULL;
    if (first == NULL) {
      first = current;
    } else {
      x = first;
      while (x->next != NULL) {
        x = x->next;
      }
      x->next = current;
    }
  } else {
    x = first;
    while (x != NULL) {
      if (strstr (line, x->delimiter) != NULL) {
        strstr (line, x->delimiter)[0] = 0;
      }
      x = x->next;
    }
  }
}

int list_command_name (struct command_field *current, char *match)
{
  if (current == NULL)
    return 0;
  if (match[0]) {
    if (strstr (current->name, match))
      fprintf (stderr, "%s\n", current->name);

  } else {
    fprintf (stderr, "%s\n", current->name);
  }
  return list_command_name (current->next, match);
}



int list_command (struct command_field *first, char *line)
{
  struct command_field *current;
  int i = 0;
  char name[80];

  current = first;
  /*
   * zero out name 
   */
  memset (name, 0, 80);
  /*
   * convert tabs and commas to white spaces 
   */
  while (strstr (line, "\t"))
    strstr (line, "\t")[0] = ' ';
  /*
   * remove leading white spaces 
   */
  while (line[0] == ' ') {
    line++;
  }
  /*
   * get the name from the command line, eat the characters as I get it 
   */
  while (line[0] != ',' && line[0] != ' ' && line[0] != 0) {
    name[i] = line[0];
    i++;
    line++;
  }

  if (i == 0) {
    print_input_error (0, " ", DT_CHAR, NULL, "Need command name to list",
                       __FILE__, __LINE__);
    return 0;
  }

  while (current != NULL) {
    if (!strcmp_case_insensitive (current->name, name)) {
      /*
       * there is a match 
       */
      break;
    } else {
      current = current->next;
    }
  }
  if (current == NULL)
    /**/ {
    print_input_error (0, name, DT_CHAR, NULL, "Command not found", __FILE__,
                       __LINE__);
    return 0;                           /* the command was not found */
    }
  print_command_and_data_structure (current);
  return 1;
}

/* process one line of input: */
int command_input (struct command_field *first, char *line,
                   unsigned short *output_words)
{
  struct command_field *current;
  struct data_field *d_field;
  static unsigned long output_count = 0;
  int i = 0, j = 0;
  char name[80];
  unsigned short us;
  char unmodified_line[1024];
  static char parand = ' ';
  int temp_rti;
  int temp_time;

  strcpy (unmodified_line, line);
  comment_delimit (line, 0);

  /*
   * zero out name 
   */
  memset (name, 0, 80);
  /*
   * convert tabs and commas to white spaces 
   */
  while (strstr (line, "\t"))
    strstr (line, "\t")[0] = ' ';
  /*
   * convert parands to white spaces as well (WTR 2/96)
   */
  while (strstr (line, ")"))
    strstr (line, ")")[0] = ' ';
  while (strstr (line, "("))
    strstr (line, "(")[0] = ' ';
  /*
   * remove leading white spaces 
   */
  while (line[0] == ' ') {
    line++;
  }

  if (line[0] == '-') {
    if (strstr (line, "-help")) {
      print_help (stderr);
      return 1;
    }
    if (end_output_line (0, 0, unmodified_line, NULL, 0)) {
      output_count = 0;
      if (!cdsseq_flag) {

#ifdef FINDCR
        printf ("<%d>", __LINE__);
#endif

        printf ("\n");
      }
    }
    if (strstr (line, "-sub_address_alf")) {
      j = -1;
      alf_sub_address = read_number (&line[17], &j, 0);
      if (j == -1) {
        fprintf (stderr, "ERROR:");
        alf_sub_address = SUB_ADDRESS_MEMORY;
      }
      fprintf (stderr, "  memory load sub-address %d\n", alf_sub_address);

    }
    if (strstr (line, "-sub_address_cmd")) {
      j = -1;
      cmd_sub_address = read_number (&line[17], &j, 0);
      if (j == -1) {
        fprintf (stderr, "ERROR:");
        cmd_sub_address = SUB_ADDRESS_MEMORY;
      }
      fprintf (stderr, "  instrument load sub-address %d\n", cmd_sub_address);

    }
    if (strstr (line, "-alf_count")) {
      int temp;

      j = -1;
      temp = read_number (&line[11], &j, 0);
      if (j == -1) {
        fprintf (stderr, "ERROR:");
        cmd_sub_address = SUB_ADDRESS_MEMORY;
      } else
        alf_count_limit = temp;
      fprintf (stderr, "  %d alf records / command \n", alf_count_limit);
    }

    /*
     ************************************************************* SECMD SHIT ***********
     */

    /*
     * 0123456789012345678901234567890 
     */
    if (strstr (line, "-cdsseq_file")) {
      char *stemp = "0";
      char *rtemp;
      time_t tzero;
      int rlen;

      cdsseq_line_flag = 1;
      cdsseq_flag = 1;
      cdsseq_rti = 0;
      cdsseq_time = 0;
      if (strlen (&line[12]) > 1) {
        rtemp = strtok (&line[13], " \t\n");
        strcpy (cdsseq_name, rtemp);
        strcpy (cdsseq_file_name, getenv ("PPCRTIU_IEB"));
        rlen = strlen (cdsseq_file_name);
        if (rlen) {
          if (cdsseq_file_name[rlen - 1] != '/')
            strcat (cdsseq_file_name, "/");
        } else
          strcat (cdsseq_file_name, Sequence_File_Path);
        strcat (cdsseq_file_name, cdsseq_name);
        strcat (cdsseq_file_name, ".seq");
        fprintf (stderr, "%s IEB reformat %s\n", title, cdsseq_file_name);
        cdsseq_file = fopen (cdsseq_file_name, "w");
        if (!cdsseq_file)
          cdsseq_file = stderr;
        if (cdsseq_name[0]) {
          stemp = strtok (NULL, " \t\n");
          if (stemp) {
            cdsseq_time = strtol (stemp, NULL, 0);
            cdsseq_rti = 0;
            stemp = strtok (NULL, " \t\n");
            if (stemp) {
              if (!strcasecmp (stemp, "fast"))
                cdsseq_delta = 1;
            }
          }
        }
      }
      tzero = time (NULL);
      fprintf (cdsseq_file, "#\n");
      fprintf (cdsseq_file, "#        Sequence name: %s\n", cdsseq_file_name);
      fprintf (cdsseq_file, "#          Produced by: %s %s\n", title,
               version);
      fprintf (cdsseq_file, "#        Creation date: %s", ctime (&tzero));
      fprintf (cdsseq_file, "#\n");
      fprintf (cdsseq_file, "secmd CDSSEQ BEGIN %s %04X:%X \n",
               cdsseq_name, cdsseq_time, cdsseq_rti);
      fflush (cdsseq_file);
      cdsseq_rti = 0;
      cdsseq_time = 0;
    }
    /*
     * 0123456789012345678901234567890 
     */
    if (strstr (line, "-cdsseq_begin")) {
      char *stemp = "0";
      char *rtemp;
      int rlen;

      cdsseq_flag = 1;
      cdsseq_rti = 0;
      cdsseq_time = 0;
      if (strlen (&line[13]) > 1) {
        rtemp = strtok (&line[14], " \t\n");
        strcpy (cdsseq_name, rtemp);
        if (cdsseq_name[0]) {
          stemp = strtok (NULL, " \t\n");
          if (stemp) {
            cdsseq_time = strtol (stemp, NULL, 0);
            cdsseq_rti = 0;
          }
        }
      }
      fprintf (cdsseq_file, "secmd CDSSEQ BEGIN %s %04X:%X \n",
               cdsseq_name, cdsseq_time, cdsseq_rti);
      fflush (cdsseq_file);
      cdsseq_rti = 0;
      cdsseq_time = 0;
    }
    /*
     * 0123456789012345678901234567890 
     */
    if (strstr (line, "-cdsseq_register")) {
      cdsseq_flag = 0;
      if (strlen (&line[16]) > 1)
        strcpy (cdsseq_name, &line[17]);
      fprintf (cdsseq_file, "secmd CDSSEQ REGISTER %s \n", cdsseq_name);
      fflush (cdsseq_file);
      cdsseq_rti = 0;
      cdsseq_time = 0;
    }
    /*
     * 0123456789012345678901234567890 
     */
    if (strstr (line, "-cdsseq_deregister")) {
      cdsseq_flag = 0;
      if (strlen (&line[18]) > 1)
        strcpy (cdsseq_name, &line[19]);
      if (cdsseq_name[0])
        fprintf (cdsseq_file, "secmd CDSSEQ DEREGISTER %s \n", cdsseq_name);
      cdsseq_name[0] = 0;
      fflush (cdsseq_file);
      cdsseq_rti = 0;
      cdsseq_time = 0;
    }
    /*
     * 0123456789012345678901234567890 
     */
    if (strstr (line, "-cdsseq_flush")) {
      cdsseq_flag = 0;
      if (strlen (&line[13]) > 1)
        strcpy (cdsseq_name, &line[14]);
      if (cdsseq_name[0])
        fprintf (cdsseq_file, "secmd CDSSEQ FLUSH %s \n", cdsseq_name);
      cdsseq_name[0] = 0;
      fflush (cdsseq_file);
      cdsseq_rti = 0;
      cdsseq_time = 0;
    }
    /*
     * 0123456789012345678901234567890 
     */
    if (strstr (line, "-cdsseq_show")) {
      char seq_name[32] = { "" };
      cdsseq_flag = 0;
      if (strlen (&line[12]) > 1)
        strcpy (seq_name, &line[13]);
      if (seq_name[0])
        fprintf (cdsseq_file, "secmd CDSSEQ SHOW %s \n", seq_name);
      cdsseq_name[0] = 0;
      fflush (cdsseq_file);
      cdsseq_rti = 0;
      cdsseq_time = 0;
    }
    /*
     * 0123456789012345678901234567890 
     */
    if (strstr (line, "-cdsseq_load")) {
      char seq_name[32] = { "" };
      cdsseq_flag = 0;
      if (strlen (&line[12]) > 1)
        strcpy (seq_name, &line[13]);
      if (seq_name[0])
        fprintf (cdsseq_file, "rtiucmd LOAD spica10:sequence/%s.seq \n",
                 seq_name);
      cdsseq_name[0] = 0;
      fflush (cdsseq_file);
      cdsseq_rti = 0;
      cdsseq_time = 0;
    }
    /*
     * 0123456789012345678901234567890 
     */
    if (strstr (line, "-cdsseq_activate")) {
      char *rtemp;
      char *stemp = "0";

      cdsseq_flag = 0;
      cdsseq_rti = 0;
      cdsseq_time = 0;
      if (strlen (&line[16]) > 1) {
        rtemp = strtok (&line[17], " \t\n");
        strcpy (cdsseq_name, rtemp);
        if (cdsseq_name[0]) {
          stemp = strtok (NULL, " \t\n");
          if (stemp) {
            cdsseq_time = strtol (stemp, NULL, 0);
            cdsseq_rti = 0;
          }
        }
      }
      fprintf (cdsseq_file, "secmd CDSSEQ ACTIVATE %s %04X:%X \n",
               cdsseq_name, cdsseq_time, cdsseq_rti);
      fflush (cdsseq_file);
      if (cdsseq_file != stdout) {
        fclose (cdsseq_file);
        cdsseq_file = stdout;
      }
      cdsseq_rti = 0;
      cdsseq_time = 0;
    }

      /**************************************************
       *	MAKE SURE FILE IS ACCESSIBLE BY		*
       *	OTHERS IN THE GROUP			*
       *              0123456789012345678901234567890   *
       **************************************************/
    if (strstr (line, "-cdsseq_end")) {
      mode_t mode = S_IRUSR | S_IWUSR | /* User  R/W    */
        S_IRGRP | S_IWGRP |             /* Group R/W    */
        S_IROTH;                        /* Other Read Only */

      fflush (cdsseq_file);
      if (cdsseq_file != stdout) {
        fclose (cdsseq_file);
        chmod (cdsseq_file_name, mode);
        cdsseq_file = stdout;
      }
      cdsseq_rti = 0;
      cdsseq_time = 0;
      cdsseq_line_flag = 0;
    }

    if (strstr (line, "-zero")) {
      int temp;

      j = -1;
      temp = read_number (&line[6], &j, 0);
      if (j == -1) {
        fprintf (stderr, "ERROR: -zero needs argument\n");
        time_base = 0;
      } else
        time_base = temp;
      send_record (sub_address, 0);
    }
    if (strstr (line, "-time")) {
      int temp;

      j = -1;
      temp = read_number (&line[6], &j, 0);
      if (j == -1) {
        fprintf (stderr, "ERROR: -time needs argument\n");
        time_offset = 0;
      } else
        time_offset = temp;
      send_record (sub_address, 0);
    }
    if (strstr (line, "-rti")) {
      int temp;

      j = -1;
      temp = read_number (&line[5], &j, 0);
      if (j == -1) {
        fprintf (stderr, "ERROR: -rti needs argument\n");
        time_rti = 0;
      } else
        time_rti = temp;
    }
    convert_to_lower_case (line);
    if (strstr (line, "-in ")) {
      line = strstr (line, "-in") + 4;
      while (line[0] == ' ') {
        line++;
      }

      in_file = fopen (line, "r");
      if (in_file) {
        in_file_active = 1;
        if (wrapieb (-1)) {
          printf (";\n");
          printf (";	Text file: %s\n", line);
          printf (";\n");
        }
      } else
        fprintf (stderr, "File NOT found (-in) '%s'\n", line);

      return 1;
    }
    if (strstr (line, "-list ")) {
      if (strstr (unmodified_line, "*")) {
        line = strstr (line, "-list") + 6;
        while (line[0] == ' ') {
          line++;
        }
        list_command_name (first, line);
      } else {
        line = strstr (line, "-list") + 6;
        list_command (first, line);
        return 1;
      }
    }
    if (strstr (line, "-pass")) {
      printf ("%s\n", strstr (unmodified_line, "-") + 6);
      return 1;
    }
    if (strstr (line, "-h")) {
      print_help (stderr);
      return 1;
    }
    if (strstr (line, "-exit")) {
      printf ("-exit\n");
      return -1;
    }
    return 1;
  }
  comment_delimit (unmodified_line, 0);

  /*
   * get the name from the command line, eat the characters as I get it 
   */
  while (line[0] != ',' && line[0] != ' ' && line[0] != 0) {
    name[i] = line[0];
    if (name[i] == ':')
      time_offset_flag = 1;             /* Look to see if a time tage was present in the first token */
    i++;
    line++;
  }

  if (time_offset_flag) {
    i = 0;
    time_offset = 0;
    time_rti = 0;
    calculate_offset (name, &time_offset, &time_rti);
    while (line[0] == ' ')
      line++;
    while (line[0] != ',' && line[0] != ' ' && line[0] != 0) {
      name[i] = line[0];
      i++;
      name[i] = 0;
      line++;
    }
  }

  if (i == 0) {
    return 1;
  }
  current = first;
  while (current != NULL) {
    if (!strcmp_case_insensitive (current->name, name)) {
      /*
       * there is a match 
       */
      for (i = 0; i < 128; i++) {
        output_words[i] = 0;            /* zero out the first 128 words 80->128 7/2002 WTR */
      }

      for (i = 0; i < 8; i++) {
        output_words[i] = current->one_mask[i];
      }
      switch (current->command_type) {
       default:
       case DT_POWER_SIM:
       case DT_PASS:
       case DT_FSWLOAD:                /* 6EXT_MEM_LOAD */
       case DT_TLMMODE:                /* 6CHG_SC_TM_IMM */
         break;
       case DT_POWER:
         sub_address_current = SUB_ADDRESS_NONE;
         break;
       case DT_BIU:
         sub_address_current = SUB_ADDRESS_BIU;
         break;
       case DT_INSTRUMENT:
         sub_address_current = cmd_sub_address;
         break;
       case DT_MEMORY:
         sub_address_current = alf_sub_address;
         break;
      }
      strcpy (sub_address, sub_address_list[sub_address_current]);
      break;
    } else {
      current = current->next;
    }
  }
  if (time_offset_flag) {
    if (strstr (unmodified_line, "-mode ")) {
      send_record ("M", 2);
      printf ("%s\n", strstr (unmodified_line, "-mode ") + 6);
      printf ("STM0.0\n");
      time_offset_flag = 0;
      return 1;
    }
  }
  if (current == NULL) {                /* Process mode commands with timetag ?!? */
    print_input_error (0, name, DT_CHAR, NULL, "Command not found", __FILE__,
                       __LINE__);
    return 0;                           /* the command was not found */
  }

  /*
   * process the data fields from here: 
   */
  if ((j = process_fields (current->first_child, line, output_words)) < 0) {
    return 0;
  }
  for (i = 0; i < 8; i++) {
    if (odd_parity (output_words[i]))
      output_words[i] |= current->even_parity_mask[i];
    else
      output_words[i] |= current->odd_parity_mask[i];
  }

  for (i = 0; i < 8; i++) {

    us = current->inversion_mask[i] & (~output_words[i + 1]);
    output_words[i] |= us;
  }


  /*
   * the last thing to do is to generate the checksum 
   */
  d_field = current->first_child;
  while (d_field != NULL) {
    if (d_field->data_type == DT_CHECKSUM) {
      if (d_field->word_number % 2) {
        /*
         * odd words 
         */
        output_words[d_field->word_number - 1] =
          JPL_checksum (output_words, (d_field->word_number - 1) / 2, 2);

      } else {
        /*
         * even words 
         */
        output_words[d_field->word_number - 1] =
          JPL_checksum (output_words + 1, (d_field->word_number - 1) / 2, 2);

      }
    }
    d_field = d_field->tail;
  }

  if (end_output_line
      (output_count, current->size + j - 1, NULL, NULL, BATCH_FLAG)) {
    output_count = 0;
    if (!cdsseq_flag) {

#ifdef FINDCR
      printf ("%d", __LINE__);
#endif

      printf ("\n");
    }

  }

  if (time_offset_flag)
    if (!cdsseq_flag)
      printf ("%s", sub_address);

  if (wrap (NULL) != NULL) {
    char comma = ',';

    switch (current->command_type) {
     case DT_INSTRUMENT:
       if (wrapl (-1))
         printf ("%s, %d", wrap (NULL), current->size + j);
       else
         printf ("%s", wrap (NULL));
       if (wrapp (-1))
         parand = '(';
       if (wrapieb (-1))
         comma = ' ';
       strcpy (tabs, "\t\t\t\t\t\t");
       for (i = 0; i < current->size + j; i++) {
         IEB_word_count += 1;
         if (wrapieb (-1)) {
           printf ("%c 0%04Xh", comma, output_words[i]);
           if (tabs[1])
             tabs[strlen (tabs) - 1] = 0;
         } else
           printf ("%c%c%04X", comma, parand, output_words[i]);
         comma = ',';
         parand = ' ';
       }
       if (wrapp (-1))
         parand = ')';
       printf ("%c", parand);
       break;
     case DT_BIU:
     case DT_POWER:
     case DT_MEMORY:
       printf ("%s", unmodified_line);
       break;
     default:
       /*
        * print a warning message 
        */
       fprintf (stderr, "-wrap bad input line\n");
       break;
    }
  } else {

    temp_time = time_offset - cdsseq_time;
    temp_rti = time_rti - cdsseq_rti;
    while (temp_rti < 0) {
      temp_time--;
      temp_rti += 8;
    }
    while (temp_rti > 8) {
      temp_time++;
      temp_rti -= 8;
    }

    switch (current->command_type) {
     case DT_TLMMODE:
       if (cdsseq_flag) {
         fprintf (cdsseq_file, "secmd CDSSEQ ADDCMD %s %04X:%X setstm %s\n",
                  cdsseq_name, temp_time, temp_rti, text_enum_string);
         cdsseq_time = time_offset;
         cdsseq_rti = time_rti;
       } else {
         fprintf (cdsseq_file, "\nsecmd setstm %s\n", text_enum_string);
       }
       text_enum_string[0] = 0;
       break;
     case DT_FSWLOAD:
       if (cdsseq_flag) {
         fprintf (cdsseq_file, "secmd CDSSEQ ADDCMD %s %04X:%X fswload %s\n",
                  cdsseq_name, temp_time, temp_rti, "rpws");
         cdsseq_time = time_offset;
         cdsseq_rti = time_rti;
       } else {
         fprintf (cdsseq_file, "\nsecmd FSWLOAD %s\n", "rpws");
       }
       break;
     case DT_POWER_SIM:
       if (cdsseq_flag) {

              /************* reset ***********/
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0001 1ffe 0000\n",
                  cdsseq_name, temp_time, temp_rti);
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0001 1ffe 0001\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0001 1ffe 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;

              /************* Corrupt *********/
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 7 22 \\\n",
                  cdsseq_name, 10, time_rti);
         time_offset += 10;
         fprintf (cdsseq_file,
                  "2000 0000 1000 0000 0000 0000 0000 0000 0000 0000 0000 \\\n");
         fprintf (cdsseq_file,
                  "0000 0000 0000 0000 0000 0000 0000 0000 0000 85AA 55AA\n");
         time_offset += 10;

              /************* CMD count *******/
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0001 1ffe 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0001 1ffe 0001\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0001 1ffe 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;

              /************* FIX BIU *********/
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0100 feff 0100\n",
                  cdsseq_name, 5, time_rti);
         time_offset += 5;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0400 fbff 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0800 f7ff 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;

         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0002 fffd 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0004 fffb 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0008 fff7 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;

         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0010 ffef 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0020 ffdf 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0040 ffbf 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;
         fprintf (cdsseq_file,
                  "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 3 3 0080 ff7f 0000\n",
                  cdsseq_name, 1, time_rti);
         time_offset += 1;

         cdsseq_time = time_offset;
         cdsseq_rti = time_rti;
       } else {
         fprintf (cdsseq_file, "\nsecmd CDSSEQ ACTIVATE power 0:0\n");
       }
       break;
     default:
       if (cdsseq_flag) {
         if (cdsseq_delta) {
           fprintf (cdsseq_file,
                    "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 %2d %3d ",
                    cdsseq_name, 0, 1, sub_address_current,
                    current->size + j);
         } else {
           if (temp_time | temp_rti)
             fprintf (cdsseq_file,
                      "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 %2d %3d ",
                      cdsseq_name, temp_time, temp_rti, sub_address_current,
                      current->size + j);
           else
             fprintf (cdsseq_file,
                      "secmd CDSSEQ ADDCMD %s %04X:%X CDSCMD 25 %2d %3d ",
                      cdsseq_name, 1, 0, sub_address_current,
                      current->size + j);
         }
         cdsseq_time = time_offset;
         cdsseq_rti = time_rti;
       }
       for (i = 0; i < current->size + j; i++) {
         fprintf (cdsseq_file, "%04X ", output_words[i]);
         if (cdsseq_flag)
           if (cdsseq_line_flag & 1)
             if (i % 16 == 5)
               if (i < current->size + j - 1)
                 fprintf (cdsseq_file, "\\\n ");
       }
       if (cdsseq_flag) {
         fprintf (cdsseq_file, "\n");
       }
       break;
    }
  }
  output_count += current->size + j;

  if (end_output_line (output_count, 0, unmodified_line, NULL, 0)) {
    output_count = 0;
  }
  if (wrapieb (-1)) {
    printf ("%s; %s", tabs, unmodified_line);
    if (IEB_word_count > 100) {
      IEB_word_count = 0;

#ifdef FINDCR
      printf ("%d", __LINE__);
#endif

      printf ("\n");
      printf (pars_length (wraplbl (NULL)));
      if (wrapieb (-1) & 0x40) {
        if ((wrapieb (-1) & 0x0F) == 3)
          printf ("\tEndP\t%s\n", wraplbl (NULL));

#ifdef FINDCR
        printf ("%d", __LINE__);
#endif

        printf ("\n");
      }
      wraplbl ("***");
      printf ("\n");
      printf ("\tGlobal\t%s\n", wraplbl (NULL));
      printf ("\tGlobal\t%s_len\n", wraplbl (NULL));
      printf ("\n");
      switch (wrapieb (-1)) {
       case 0x01:
       case 0x42:
         printf ("%s:", wraplbl (NULL));
         break;
       case 0x43:
         printf ("%s\tProc", wraplbl (NULL));
         break;
       default:
         break;
      }
    }
  }
  if (time_offset_flag) {
    send_record ("S", 1);
    time_offset_flag = 0;
  } else {

#ifdef FINDCR
    printf ("%d", __LINE__);
#endif

    printf ("\n");
  }
  return 1;
}

/* fills the next word and returns a pointer to further on up the line */

/* returns NULL if there are no more words */
char *next_word (char *line, char *the_next_word)
{

  int i;

  the_next_word[0] = 0;

  if (line == NULL)
    return NULL;
  /*
   * remove leading white spaces 
   */
  while (line[0] == ' ') {
    line++;
  }
  if (line[0] == ',') {
    line++;
  }
  /*
   * remove leading white spaces 
   */
  while (line[0] == ' ') {
    line++;
  }

  /*
   * get the name from the command line, eat the characters as I get it 
   */
  i = 0;
  while (line[0] != ',' && line[0] != ' ' && line[0] != 0 && i < 79) {
    the_next_word[i] = line[0];
    i++;
    line++;
  }
  if (i == 0 && line[0] == 0)
    return NULL;

  if (i == 0)
    return line;

  the_next_word[i] = 0;
  return line;
}


/* process the data part of an input line: */
int process_fields (struct data_field *first_child, char *line,
                    unsigned short *output_words)
{
  char location[1024];
  char input[80];                       /* a single input item */
  struct data_field *current;
  struct enumerated_table *e;
  unsigned long ul;
  unsigned short s_words[3];
  int counter = 0;                      /* keeps track of which field I'm in */
  int i = 0;
  int next_offset = 1;

  current = first_child;

  /*
   * walk through the desired data fields: 
   */
  while (current != NULL) {
    counter++;
    if (current->field_type == DT_VARIABLE) {
      while (next_offset >= 0) {
        line = next_word (line, input);
        ul = read_number (input, &next_offset, current->data_type);
        if (next_offset == -1)
          return i;
        /*
         * check the data: 
         */
        if (!in_range (current, ul)) {
          print_input_error (counter, current->name, current->data_type,
                             input, "Data out of range", __FILE__, __LINE__);
          return -1;                    /* an error */
        }

        /*
         * if the data is signed, this strips off the
         * higher bits. If the data is not signed, it
         * does nothing 
         */
        ul = ul & current->data_mask;

        /*
         * move the bits into the right positions 
         */
        positioner (ul, s_words, current->left_shift);
        /*
         * embed the data bits into the output buffer 
         */
        embed (output_words, s_words, current->word_number + i);
        i++;
      }
      return i;
    } else {
      if (current->data_type == DT_SIGNED ||
          current->data_type == DT_UNSIGNED ||
          current->data_type == DT_HEX || current->data_type == DT_OCT) {
        /*
         * get the next input 
         */
        line = next_word (line, input);

        /*
         * fprintf(stderr,
         * "DBG(%d)%s - %s - %s\n", 
         * __LINE__, 
         * convert_enum_to_string(current->data_type),  
         * line, 
         * input); /*
         */

        if (line == NULL) {
          print_input_error (counter, current->name, current->data_type, NULL,
                             "Missing input field", __FILE__, __LINE__);
          return -1;                    /* an error */
        }

        /*
         * fprintf(stderr, 
         * "   (%d) %X\n", 
         * __LINE__, 
         * ul); /*
         */
        ul = 0;
        /*
         * get the data: 
         */
        if (current->data_type == DT_HEX || current->data_type == DT_OCT) {
          if (current->data_type == DT_OCT) {
            if (!sscanf (input, "%O", &ul))
              return -1;
          }
          if (!sscanf (input, "%X", &ul))
            return -1;
        } else {

          if (!get_data (current, input, &ul)) {
            print_input_error (counter, current->name, current->data_type,
                               input, "Numeric input expected", __FILE__,
                               __LINE__);
            /*
             * an error 
             */
            return -1;
          }
        }

        /*
         * fprintf(stderr, 
         * "   (%d) %X\n", 
         * __LINE__, 
         * ul); /*
         */
        /*
         * check the data: 
         */
        if (!in_range (current, ul)) {
          print_input_error (counter, current->name, current->data_type,
                             input, "Data out of range", __FILE__, __LINE__);
          return -1;                    /* an error */
        }

        /*
         * if the data is signed, this strips off the
         * higher bits. If the data is not signed, it
         * does nothing 
         */
        ul = ul & current->data_mask;

        /*
         * move the bits into the right positions 
         */
        positioner (ul, s_words, current->left_shift);
        /*
         * embed the data bits into the output buffer 
         */
        embed (output_words, s_words, current->word_number);
      }

/* room for another type: */

      if (current->data_type == DT_TEXT) {
        /*
         * get the next input 
         */
        line = next_word (line, input);
        strcpy (current->text, input);
      }

      if (current->data_type == DT_CHAR) {
        /*
         * get the next input 
         */
        line = next_word (line, input);
        process_char_field (current, input, output_words, counter, 0);
      }

      if (current->data_type == DT_CHAR_MSB) {
        /*
         * get the next input 
         */
        line = next_word (line, input);
        process_char_field (current, input, output_words, counter, 1);
      }

      if (current->data_type == DT_CHAR_LC) {
        /*
         * get the next input 
         */
        line = next_word (line, input);
        convert_to_lower_case (input);
        process_char_field (current, input, output_words, counter, 0);
      }

      if (current->data_type == DT_CHAR_UC) {
        /*
         * get the next input 
         */
        line = next_word (line, input);
        convert_to_upper_case (input);
        process_char_field (current, input, output_words, counter, 0);
      }

      if (current->data_type == DT_ENUM) {
        /*
         * get the next input 
         */
        line = next_word (line, input);
        if (line == NULL) {
          print_input_error (counter, current->name, current->data_type, NULL,
                             "missing", __FILE__, __LINE__);
          return -1;                    /* an error */
        }
        e = get_enumerated (current->enums, input);
        if (e == NULL) {
          print_input_error (counter,
                             current->name,
                             current->data_type,
                             input, "unknown", __FILE__, __LINE__);
          return -1;                    /* an error */
        }

        ul = ((unsigned long) e->value) & (unsigned long) 0xFFFF;

        /*
         * move the bits into the right positions 
         */
        positioner (ul, s_words, current->left_shift);
        /*
         * embed the data bits into the output buffer 
         */
        embed (output_words, s_words, current->word_number);

      }
      if (current->data_type == DT_TXT_ENUM) {
        /*
         * get the next input 
         */
        line = next_word (line, input);
        if (line == NULL) {
          print_input_error (counter, current->name, current->data_type, NULL,
                             "missing", __FILE__, __LINE__);
          return -1;                    /* an error */
        }
        e = get_enumerated (current->enums, input);
        if (e)
          strcpy (text_enum_string, e->text);
        else
          strcpy (text_enum_string, input);

      }

    }
    current = current->tail;
  }
  return i;
}

/* processes a character input, returns null on error, else it should return a pointer to a seperator field, which is ignored */
struct data_field *process_char_field (struct data_field *current,
                                       char *input,
                                       unsigned short *output_words,
                                       int counter, int natural)
{
  char string[80];
  int i;
  unsigned long ul[20];
  unsigned short s_words[3];

  for (i = 0; i < 20; i++) {
    ul[i] = 0;
  }
  if (strlen (input) > (current->size / 8)) {
    print_input_error (counter, current->name, current->data_type, input,
                       "too long", __FILE__, __LINE__);
    return NULL;                        /* an error */
  }
  for (i = 0; i < (current->size / 8); i++) {
    string[i] = current->fill;
  }
  string[i] = 0;
  if (natural) {
    for (i = 0; i < strlen (input); i++) {
      string[i] = input[i ^ 1];
    }
  } else {
    for (i = 0; i < strlen (input); i++) {
      string[i] = input[i];
    }
  }

  for (i = 0; i < (current->size / 8); i++) {
    ul[i / 4] =
      ul[i / 4] +
      (((unsigned long) string[(current->size / 8) - 1 - i] & (unsigned long)
        0xFF) << ((i % 4) * 8));
  }

  for (i = 0; i < (current->size + 31) / 32; i++) {
    /*
     * move the bits into the right positions 
     */

    positioner (ul[i], s_words, current->left_shift);

    /*
     * embed the data bits into the output buffer 
     */

    embed (output_words, s_words, current->word_number - i);

  }
  return current;                       /* this needs to be changed ... right now, this does nothing */
}

/* search for a match for the enumerated and if found,
   then return a pointer to the structure. If not found call recursively.
   If never found, return NULL and offset_to_next_number=-1 */
struct enumerated_table *get_enumerated (struct enumerated_table *e,
                                         char *input)
{
  if (strcmp_case_insensitive (input, e->name) == 0) {
    /*
     * a match 
     */
    return e;
  }
  if (e->tail == NULL) {
    return NULL;
  }
  return (get_enumerated (e->tail, input));

}


/* checks to see if a number is in range */

/* returns 1 on success, 0 on failure */
int in_range (struct data_field *field, unsigned long number)
{
  int i = 0;
  signed long s_number;

  int data_in_range = 0;

  if (field->data_type == DT_SIGNED) {
    s_number = (signed long) number;
    for (i = 0; i < field->number_of_ranges; i++) {
      if (s_number >= (signed long) field->lower_limit[i]
          && s_number <= (signed long) field->upper_limit[i]) {
        data_in_range = 1;
      }
    }

/* change this: */
    if (field->number_of_ranges == 0) {
      data_in_range = 1;
    }
  } else {
    for (i = 0; i < field->number_of_ranges; i++) {
      if (number >= field->lower_limit[i]
          && number <= field->upper_limit[i]) {
        data_in_range = 1;
      }
    }

/* change this: */
    if (field->number_of_ranges == 0) {
      data_in_range = 1;
    }
  }
  return data_in_range;
}
