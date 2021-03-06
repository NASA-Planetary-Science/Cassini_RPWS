#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "parser.h"

#define BATCH_FLAG 0x10
#define ALF_FLAG   0x20

#define SUB_ADDRESS_BIU		3
#define SUB_ADDRESS_INSTRUMENT	7
#define SUB_ADDRESS_MEMORY	7
#define SUB_ADDRESS_NONE	32

static char version[] = { "V3.1" };

FILE *in_file = NULL;
int in_file_active = 0;
char sub_address[8] = { "SA7 " };       /* "type=" determines sub-address to send to */
static int alf_sub_address = SUB_ADDRESS_MEMORY;
static int cmd_sub_address = SUB_ADDRESS_INSTRUMENT;
static unsigned long alf_count_limit;
static int sub_address_previous = 7;
static int sub_address_current = 7;
static char sub_address_list[33][8] = { "SA0  ",
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
  "NONE "
};

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

      print_input_error (0, "Programming or command line", DT_UNSIGNED, NULL,
                         "wrap initialized twice", __FILE__, __LINE__);
      exit (0);
    }
    strcpy (C_Name, Command_Name);
  }

  if (C_Name[0] != 0) {
    return C_Name;
  }
  return NULL;

}

int end_output_line (unsigned long count, unsigned short size, char *line,
                     char *argv[], unsigned long options)
{
  static unsigned long control_flags = 0;
  static unsigned long call_count;
  static unsigned long batch_count_limit;
  int i = 1;
  char c[120];
  char *s;
  int j;


  if (argv != NULL) {
    /*
     * set up the command line options ... this should be the first time through 
     */
    while (i < count) {
      strcpy (c, argv[i]);
      convert_to_lower_case (c);
      if (s = strstr (c, "-wrap")) {
        i++;
        if (i >= count) {
          print_input_error (i, "Command line", DT_CHAR, c,
                             "Command name expected after -wrap", __FILE__,
                             __LINE__);
          exit (0);
        }
        wrap (argv[i]);
        i++;
        continue;
      }
      if (s = strstr (c, "-pwrap")) {
        i++;
        if (i >= count) {
          print_input_error (i, "Command line", DT_CHAR, c,
                             "Command name expected after -wrap", __FILE__,
                             __LINE__);
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
          print_input_error (i, "Command line", DT_CHAR, c,
                             "Command name expected after -wrap", __FILE__,
                             __LINE__);
          exit (0);
        }
        wrap (argv[i]);
        wrapl (1);
        i++;
        continue;
      }
      if (s = strstr (c, "-batch")) {
        control_flags |= BATCH_FLAG;
        j = -1;
        if (s[0] != 0) {
          batch_count_limit = read_number (s, &j, 0);
        }
        if (j == -1) {
          i++;
          if (i >= count) {
            print_input_error (i, "Command line", DT_UNSIGNED, c,
                               "Number expected after -batch", __FILE__,
                               __LINE__);
            exit (0);

          }
          strcpy (c, argv[i]);
          batch_count_limit = read_number (c, &j, 0);
          if (j == -1) {
            print_input_error (i, "Command line", DT_UNSIGNED, c,
                               "Number expected after -batch", __FILE__,
                               __LINE__);
            exit (0);
          }
        }
      }
      if (s = strstr (c, "-h")) {
        print_help ();
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
      printf ("%s", sub_address_list[sub_address_previous]);
      sub_address_previous = sub_address_current;
    }
  }
  if (options == 0)
    options = ~options;
  if ((options & BATCH_FLAG) & control_flags) {
    if ((count + (unsigned long) size) >= batch_count_limit) {
      if (wrap (NULL) == NULL) {
        printf ("%s", sub_address);
      }
      if ((unsigned long) size > batch_count_limit) {
        fprintf (stderr,
                 "Input Warning: Command size exceeds -batch length.\n");
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
        printf ("%s", sub_address);
      }
      return 1;
    }
  }

  strcpy (c, line);
  convert_to_lower_case (c);
  if (strstr (c, "-send")) {
    if (wrap (NULL) == NULL) {
      printf ("%s", sub_address);
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

void print_help (void)
{
  fprintf (stderr, "Parser %s\n", version);
  fprintf (stderr, "  Command Line  Options:\n");
  fprintf (stderr, "\t-help       This message.\n");
  fprintf (stderr, "\t-h          This message.\n");
  fprintf (stderr, "\t-send       Appends 'SAx' after every command.\n");
  fprintf (stderr, "\t-alf xxx    Appends 'SAx' after every xxx commands.\n");
  fprintf (stderr, "\t-sub_address_alf xxx\n");
  fprintf (stderr, "\t            Alters sub-address used for ALF records\n");
  fprintf (stderr, "\t-sub_address_cmd xxx\n");
  fprintf (stderr,
           "\t            Alters sub-address used for instrument commands\n");
  fprintf (stderr, "\t-wrap xxx   Generates wrapped commands.\n");
  fprintf (stderr,
           "\t-lwrap xxx   Generates wrapped commands (with length).\n");
  fprintf (stderr,
           "\t-pwrap xxx   Generates wrapped commands (with parands).\n");
  fprintf (stderr, "\t-batch xxx  Appends 's' after every xxx words.\n");
  fprintf (stderr, "\t                Commands are not segmented.\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "Input Options:\n");
  fprintf (stderr, "\tA valid command.\n");
  fprintf (stderr, "\t-in         read a file with commands\n");
  fprintf (stderr, "\t                e.g. '-in map.alf'\n");
  fprintf (stderr, "\t-alf_count xxx\n");
  fprintf (stderr, "\t            Alters the size of ALF commands\n");
  fprintf (stderr, "\t-sub_address_alf xxx\n");
  fprintf (stderr, "\t            Alters sub-address used for ALF records\n");
  fprintf (stderr, "\t-sub_address_cmd xxx\n");
  fprintf (stderr,
           "\t            Alters sub-address used for instrument commands\n");
  fprintf (stderr,
           "\t-list       command_name Outputs the command definition\n");
  fprintf (stderr, "\t                corresponding to command_name.\n");
  fprintf (stderr, "\t                e.g. '-list 73wbr_mode_cntl'\n");
  fprintf (stderr,
           "\t-list       pattern* Outputs command names containing pattern.\n");
  fprintf (stderr, "\t                e.g. '-list 73w*', '-list alf*'\n");
  fprintf (stderr, "\t-help (-h)  This message.\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "Environmental Variables:\n");
  fprintf (stderr, "\tcmd_txt     <full path name of cmd.txt file>.\n");
  fprintf (stderr,
           "\t                e.g. 'setenv cmd_txt /usr/cassini/cfg/cmd.txt'\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "Special Keys:\n");
  fprintf (stderr, "\t<Control d> exits program.\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "File Format changes\n");
  fprintf (stderr, "  Input Files:\n");
  fprintf (stderr, "              Paranthesis are treated like whitespace\n");
  fprintf (stderr, "                to allow compatibility with JPL/TTACS\n");
  fprintf (stderr, "  Output File:\n");
  fprintf (stderr, "              Paranthesis in wrap commands (-pwrap)\n");
  fprintf (stderr, "\n");
  return;
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
  if (current == NULL) {
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
  char unmodified_line[100];
  static char parand = ' ';

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
      print_help ();
      return 1;
    }
    if (end_output_line (0, 0, unmodified_line, NULL, 0)) {
      output_count = 0;
      printf ("\n");
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
    convert_to_lower_case (line);
    if (strstr (line, "-in ")) {
      line = strstr (line, "-in") + 4;
      while (line[0] == ' ') {
        line++;
      }

      in_file = fopen (line, "r");
      if (in_file)
        in_file_active = 1;
      else
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
      print_help ();
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
    i++;
    line++;
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
      for (i = 0; i < 80; i++) {
        output_words[i] = 0;            /* zero out the first 80 words */
      }

      for (i = 0; i < 8; i++) {
        output_words[i] = current->one_mask[i];
      }
      switch (current->command_type) {
       default:
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
  if (current == NULL) {
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
    printf ("\n");

  }

  if (wrap (NULL) != NULL) {
    switch (current->command_type) {
     case DT_INSTRUMENT:
       if (wrapl (-1))
         printf ("%s, %d", wrap (NULL), current->size + j);
       else
         printf ("%s", wrap (NULL));
       if (wrapp (-1))
         parand = '(';
       for (i = 0; i < current->size + j; i++) {
         printf (",%c%04X", parand, output_words[i]);
         parand = ' ';
       }
       if (wrapp (-1))
         parand = ')';
       printf ("%c", parand);
       break;
     case DT_POWER:
     case DT_BIU:
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
    for (i = 0; i < current->size + j; i++) {
      printf ("%04X ", output_words[i]);
    }
  }
  output_count += current->size + j;

  if (end_output_line (output_count, 0, unmodified_line, NULL, 0)) {
    output_count = 0;
  }
  printf ("\n");
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
  char location[120];
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
          current->data_type == DT_UNSIGNED || current->data_type == DT_HEX
          || current->data_type == DT_OCT) {
        /*
         * get the next input 
         */
        line = next_word (line, input);
        if (line == NULL) {
          print_input_error (counter, current->name, current->data_type, NULL,
                             "Missing input field", __FILE__, __LINE__);
          return -1;                    /* an error */
        }

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
          print_input_error (counter, current->name, current->data_type,
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
