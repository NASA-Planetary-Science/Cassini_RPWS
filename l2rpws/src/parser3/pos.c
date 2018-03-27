#include "parser.h"
#include <stdio.h>
#include <string.h>

/*#include <conio.h>*/
#include <stdlib.h>

/* the positioner:
	takes a number and shifts it accordingly,
	putting it into a word in the correct place
*/

/* figure out which byte to start at. The surest way appears
   to be to break the number into two shorts and then apply
   the shift accordingly. For example:

   0123456789abcdef ghijklmnopqrstuv
   into position 4 of word 5:
xxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx
					       0123456789abcdef ghijklmnopqrstuv
check by overflows by using the mask
also. start with -4 as the first word, not 0
*/

/* CHECKED OUT AS GOOD */
extern char *wraplbl (char *);
extern char *pars_length (char *);
extern char sub_address[8];             /* "type=" determines sub-address to send to */
int positioner (unsigned long l_input, unsigned short *s_output,
                unsigned int left_shift)
{
  unsigned long lb, la[2];
  unsigned short sa[4], sb[3];
  int return_value = 1;

/* break the long into 2 shorts:
   sa[0]=0123456789abcdef
   sa[1]=ghijklmnopqrstuv
*/
  sa[1] = (unsigned short) (l_input & 0xFFFF);
  lb = (l_input & 0xFFFF0000UL) >> 16;
  sa[0] = (unsigned short) (lb & 0xFFFF);

/* make the shorts into longs:
   la[0]=0000000000000000 0123456789abcdef
   la[1]=0000000000000000 ghijklmnopqrstuv
*/
  la[0] = (unsigned long) sa[0];
  la[1] = (unsigned long) sa[1];

/* shift the longs: (eg by 4)
   0000000000000123 456789abcdef0000
   000000000000ghij klmnopqrstuv0000
*/

  la[0] = la[0] << left_shift;
  la[1] = la[1] << left_shift;

/*make 4 shorts:
   0000000000000123
   456789abcdef0000
   000000000000ghij
   klmnopqrstuv0000
*/

  sa[1] = (unsigned short) (la[0] & 0xFFFF);
  lb = (la[0] & 0xFFFF0000UL) >> 16;
  sa[0] = (unsigned short) (lb & 0xFFFF);

  sa[3] = (unsigned short) (la[1] & 0xFFFF);
  lb = (la[1] & 0xFFFF0000UL) >> 16;
  sa[2] = (unsigned short) (lb & 0xFFFF);

/* OR the middle two to get the desired bit pattern:
   0000000000000123
   456789abcdefghij
   klmnopqrstuv0000
*/

  sb[0] = sa[0];
  sb[1] = sa[1] | sa[2];
  sb[2] = sa[3];

  s_output[0] = sb[0];
  s_output[1] = sb[1];
  s_output[2] = sb[2];

  return return_value;

/*
the 3 short array should be filled, and a return of one is success,
   a return of zero is failure
*/
}

/* embed the data into the output buffer */
int embed (unsigned short *out_s_words,
           unsigned short *s_words, int word_number)
{

  /*
   * fprintf(stderr,
   * "embed %p %04X %04X %04X (%d) \n", 
   * out_s_words, 
   * s_words[0], 
   * s_words[1], 
   * s_words[2], 
   * word_number); /*
   */

  word_number--;
  if (word_number >= 0) {
    out_s_words[word_number] |= s_words[2];
    word_number--;
  }
  if (word_number >= 0) {
    out_s_words[word_number] |= s_words[1];
    word_number--;
  }
  if (word_number >= 0)
    out_s_words[word_number] |= s_words[0];
  return 1;
}

/*char *program_name;
*/



char *program_name (char *name)
{
  static char p_name[40] = "NOT INITIALIZED";

  if (name != NULL) {
    strcpy (p_name, name);
  }

  return p_name;
}


int main (int argc, char *argv[])
{
  static char line[MAX_DECODE_FILE_LINE];
  static unsigned short output_words[256];
  struct command_field *first_c_field;

  output_words[0] = 0;                  /* this is the count of how many words have been output */
  program_name (argv[0]);
  end_output_line ((unsigned long) argc, 0, NULL, argv, 0);

  if (getenv ("cmd_txt")) {
    first_c_field = build_decode_table (getenv ("cmd_txt"));
  } else {

#ifdef __RPWSHP2__
    first_c_field = build_decode_table ("/usr/cassini/cfg/cmd.txt");
#else
    first_c_field = build_decode_table ("/opt/project/cassini/cfg/cmd.txt");
    /**/
#endif
  }
  while (RPWS_gets (line, stdin) != NULL) {
    int i;

    i = command_input (first_c_field, line, output_words);
    switch (i) {
     case -1:
       return 1;
     case 0:
       fprintf (stderr, "\"%s\"\n", line);
       break;
     default:
       break;
    }
  }
  if (wrap (NULL) == NULL) {
    printf ("%s", sub_address);
  }
  if (wraplbl (NULL)) {
    /*
     * printf("%s_len equ ($-%s)/2\t; number of words in this trigger\n",
     * wraplbl(NULL), wraplbl(NULL));/*
     */
    printf (pars_length (wraplbl (NULL)));
    if (wrapieb (-1) & 0x40) {
      if ((wrapieb (-1) & 0x0F) == 3)
        printf ("\tEndP\t%s\n", wraplbl (NULL));
      printf ("\n");
      printf ("\tEnd\n");
    }
  }
  return 1;
}

/* print one data structue */
void print_command_and_data_structure (struct command_field *current_command)
{
  int i, j;
  char s[25] = "                      ";

  if (current_command == NULL)
    return;
  fprintf (stderr, "command name: %s\n", current_command->name);
  fprintf (stderr, "   type: %s\n",
           convert_enum_to_string (current_command->command_type));
  fprintf (stderr, "   size: %d\n", current_command->size);
  fprintf (stderr, "   mask: \n");
  for (i = 0; i < current_command->size && i < 8; i++) {
    for (j = 0; j < 16; j++) {
      s[j + j / 4] = (binX (current_command->one_mask[i], '1'))[j] - '0' +
        (binX (current_command->data_mask[i], '0'))[j] - '0' +
        (binX (current_command->inversion_mask[i], 'I'))[j] - '0' +
        (binX (current_command->even_parity_mask[i], 'E'))[j] - '0' +
        (binX (current_command->odd_parity_mask[i], 'O'))[j];
    }
    s[j + j / 4] = 0;
    fprintf (stderr, "          %s \n", s);
  }

  print_data_fields (current_command->first_child);

/*   print_command_and_data_structure(current_command->next); */
  return;
}

struct data_field *print_data_fields (struct data_field *current)
{
  int i;

  fprintf (stderr, "\n");
  if (current == NULL)
    return NULL;
  fprintf (stderr, "      field_name: %s\n", current->name);
  fprintf (stderr, "      field_type: %s\n",
           convert_enum_to_string (current->field_type));
  fprintf (stderr, "      data_type: %s\n",
           convert_enum_to_string (current->data_type));
  fprintf (stderr, "      data mask: %s\n", binLX (current->data_mask, 'D'));
  fprintf (stderr, "      word number: %d\n", current->word_number);
  fprintf (stderr, "      left shift: %d\n", current->left_shift);
  fprintf (stderr, "      size: %ld\n", current->size);
  switch (current->data_type) {
   case DT_SIGNED:
   case DT_UNSIGNED:
     fprintf (stderr, "      RANGES:\n");
     for (i = 0; i < current->number_of_ranges; i++)
       fprintf (stderr, "         %ld ... %ld\n", current->lower_limit[i],
                current->upper_limit[i]);
     break;
   case DT_ENUM:
     print_enums (current->enums, current->size);
     break;
   case DT_TXT_ENUM:
     print_tenums (current->enums);
     break;
   case DT_TEXT:
   case DT_CHAR:
   case DT_CHAR_UC:
   case DT_CHAR_LC:
   case DT_CHAR_MSB:
     fprintf (stderr, "       fill: 0x%X\n", current->fill);
     fprintf (stderr, "       reverse: %d\n", current->reverse);
     break;
  }
  print_data_fields (current->tail);
  return current;
}

char *convert_enum_to_string (int enumerated)
{
  switch (enumerated) {
   case DT_EOF:
     return "DT_EOF";
   case DT_INVALID_COMMAND:
     return "DT_INVALID_COMMAND";
   case DT_COMMAND:
     return "DT_COMMAND";
   case DT_TYPE:
     return "DT_TYPE";
   case DT_POWER:
     return "DT_POWER";
   case DT_POWER_SIM:
     return "DT_POWER_SIM";
   case DT_BIU:
     return "DT_BIU";
   case DT_MEMORY:
     return "DT_MEMORY";
   case DT_INSTRUMENT:
     return "DT_INSTRUMENT";
   case DT_PASS:
     return "DT_PASS";
   case DT_FSWLOAD:
     return "DT_FSWLOAD";
   case DT_TLMMODE:
     return "DT_TLMMODE";
   case DT_SIZE:
     return "DT_SIZE";
   case DT_MASK:
     return "DT_MASK";
   case DT_FIELD:
     return "DT_FIELD";
   case DT_DATA_TYPE:
     return "DT_DATA_TYPE";
   case DT_POSITION:
     return "DT_POSITION";
   case DT_RANGE:
     return "DT_RANGE";
   case DT_KEYWORD:
     return "DT_KEYWORD";
   case DT_TEXTKEY:
     return "DT_TEXTKEY";
   case DT_SCALE:
     return "DT_SCALE";
   case DT_VARIABLE:
     return "DT_VARIABLE";
   case DT_END:
     return "DT_END";
   case DT_REPEAT:
     return "DT_REPEAT";
   case DT_CONTAINS_EQUAL_BEFORE_THIS:
     return "DT_CONTAINS_EQUAL_BEFORE_THIS";
   case DT_COMMENT:
     return "DT_COMMENT";
   case DT_FIXED:
     return "DT_FIXED";
   case DT_SIGNED:
     return "DT_SIGNED";
   case DT_UNSIGNED:
     return "DT_UNSIGNED";
   case DT_HEX:
     return "DT_HEX";
   case DT_OCT:
     return "DT_OCT";
   case DT_ENUM:
     return "DT_ENUM";
   case DT_TXT_ENUM:
     return "DT_TXT_ENUM";
   case DT_TEXT:
     return "DT_TEXT";
   case DT_CHAR:
     return "DT_CHAR";
   case DT_CHAR_UC:
     return "DT_CHAR_UC";
   case DT_CHAR_LC:
     return "DT_CHAR_LC";
   case DT_CHAR_MSB:
     return "DT_CHAR_MSB";
   case DT_CHECKSUM:
     return "DT_CHECKSUM";
   case DT_PLACE:
     return "DT_PLACE";
   case DT_UNKNOWN:
     return "DT_UNKNOWN";
  }
  return "\a\aUNDEFINED TYPE";
}


struct enumerated_table *print_enums (struct enumerated_table *current,
                                      unsigned long size)
{
  char *binary;
  unsigned long i;

  if (current == NULL)
    return current;
  fprintf (stderr, "      %16s", current->name);
  fprintf (stderr, "   ");
  binary = bin (current->value);
  for (i = size; i > 0; i--) {
    fprintf (stderr, "%c", binary[strlen (binary) - (unsigned int) i]);
  }
  fprintf (stderr, "\n");

  print_enums (current->tail, size);
  return current;
}
struct enumerated_table *print_tenums (struct enumerated_table *current)
{
  if (current == NULL)
    return current;

  fprintf (stderr, "     %16s", current->name);
  fprintf (stderr, "     %16s", current->text);
  fprintf (stderr, "\n");

  print_tenums (current->tail);
  return current;
}

/* returns the number converted to binary */
char *bin (unsigned short number)
{
  int i, j = 0;
  static char num_string[256];

  for (i = (sizeof (number) * 8); i;
       sprintf (&num_string[j++], "%d\0", ((1UL << --i) & number) && 1));
  return num_string;
}

/* converts a number to binary format, printing the char X instead of '1' */
char *binX0 (unsigned short number, char X)
{
  char *binary;
  int i;

  binary = bin (number);
  for (i = 0; i < strlen (binary); i++) {
    if (binary[i] != '0') {
      binary[i] = X;
    }
    binary[i] -= '0';
  }
  return binary;
}

/* converts a number to binary format, printing the char X instead of '1' */
char *binX (unsigned short number, char X)
{
  char *binary;
  int i;

  binary = bin (number);
  for (i = 0; i < strlen (binary); i++) {
    if (binary[i] != '0')
      binary[i] = X;
  }
  return binary;
}

/* returns the number converted to binary */
char *binL (unsigned long number)
{
  int i, j = 0;
  static char num_string[80];

  for (i = (sizeof (number) * 8); i;
       sprintf (&num_string[j++], "%d\0", ((1UL << --i) & number) && 1));
  return num_string;
}

/* converts a number to binary format, printing the char X instead of '1' */
char *binLX (unsigned long number, char X)
{
  char *binary;
  int i;

  binary = bin (number);
  for (i = 0; i < strlen (binary); i++) {
    if (binary[i] != '0')
      binary[i] = X;
  }
  return binary;
}
