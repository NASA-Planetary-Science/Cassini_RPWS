#include <stdio.h>

#ifndef PARSER_H
#define PARSER_H

char *RPWS_gets (char *, FILE *);

#define MAX_NUMBER_OF_RANGES 8
#define MAX_COMMAND_SIZE 8
#define MAX_DECODE_FILE_LINE 8192

/* decode table input types and commands: */
enum
{
  DT_EOF, DT_INVALID_COMMAND, DT_COMMAND, DT_TYPE,
  DT_POWER, DT_POWER_SIM, DT_BIU, DT_MEMORY,
  DT_INSTRUMENT, DT_PASS, DT_SIZE, DT_MASK,
  DT_FIELD, DT_DATA_TYPE, DT_POSITION, DT_RANGE,
  DT_KEYWORD, DT_TEXTKEY, DT_SCALE, DT_VARIABLE,
  DT_END,
  DT_REPEAT, DT_FILL, DT_REVERSE, DT_DELIMITER,
  DT_CONTAINS_EQUAL_BEFORE_THIS,

  DT_COMMENT,                           /*   DT_COMMENT MUST NOT BE MOVED!!!!   */
  /*
   * ALL WORDS THAT CONTAIN EQUAL MUST    
   */
  /*
   * COME BEFORE DT_COMMENT!!!!           
   */

  DT_FIXED,
  DT_SIGNED, DT_UNSIGNED, DT_HEX, DT_OCT,
  DT_CHECKSUM, DT_ENUM, DT_PLACE, DT_CHAR_MSB,
  DT_FSWLOAD, DT_TLMMODE, DT_TEXT, DT_TXT_ENUM,
  DT_CHAR_UC, DT_CHAR_LC, DT_CHAR,

  DT_UNKNOWN
};

struct comment
{
  char delimiter[20];
  struct comment *next;
};

struct command_field
{
  char name[80];
  int command_type;
  unsigned short size;
  /*
   * if there aren't enough bit masks in the input,
   * assume zeros 
   */
  /*
   * each bit mask that is present is one, however 
   */
  unsigned short one_mask[8];
  unsigned short data_mask[8];
  unsigned short inversion_mask[8];
  unsigned short even_parity_mask[8];
  unsigned short odd_parity_mask[8];
  struct data_field *first_child;
  struct command_field *next;
};


struct data_field
{
  int field_type;
  int data_type;
  char name[64];
  char text[64];
  unsigned long size;
  unsigned long data_mask;
  int word_number;                      /* the word number of the right most bit */
  int left_shift;
  char fill;                            /* used for DT_CHAR data fields , defaults to zero */
  int reverse;                          /* used to order the DT_CHAR output ...not implemented yet */
  struct enumerated_table *enums;
  float scale;
  long scale_div;
  long scale_mul;
  unsigned long lower_limit[MAX_NUMBER_OF_RANGES];
  unsigned long upper_limit[MAX_NUMBER_OF_RANGES];
  int number_of_ranges;                 /* 1 to MAX_NUMBER_OF_RANGES */
  /*
   * use child if this is a variable command_type field 
   */
  struct data_field *child;
  struct data_field *tail;
};

/* 6chg_sc_tm_imm */

struct enumerated_table
{
  char name[80];
  char text[80];
  unsigned short value;
  struct enumerated_table *tail;
};

/* takes care of the optional end of line stuff willie wants */
int end_output_line (unsigned long count, unsigned short size, char *line,
                     char *argv[], unsigned long options);

unsigned short JPL_checksum (unsigned short *data,
                             int num_words, int skip_factor);

/* add=1 adds the line to the delimiters, else it processes a line */
void comment_delimit (char *line, int add);

void print_help (FILE *);

/* processes a character input, returns null on error, else it should return a pointer to a seperator field, which is ignored */
struct data_field *process_char_field (struct data_field *first, char *line,
                                       unsigned short *output_words,
                                       int counter, int natural);

/* remembers the name of the program, retruns the name if NULL passed */
char *program_name (char *name);

/* takes __FILE__ and __LINE__ and formats them for Willie */
char *location_formatter (char *file, int line);

/* produce a Willie output message: */
int print_input_error (int field_number, char *field_name, int data_type,
                       char *input, char *description, char *file_name,
                       int line_number);

/* takes a string input and converts it to lower case */
int convert_to_lower_case (char *string);

/* use this for compatablility between platforms ... unix and borlandc have different functions */
int strcmp_case_insensitive (char *string1, char *string2);

/* takes a string input and converts it to upper case */
int convert_to_upper_case (char *string);

/* the input is a line with hex, octal, or decimal numbers on it */

/* assumes the first characters are either spaces or numbers,
   not keywords.
/* returns the value read. If there is an error,
   then offset_to_next_number[0]=-1.
   If it is the last number on the line, offset_to_next_number[0]=0
*/

/* side effect: converts tabs and commas to spaces */

/* THIS IS CHECKED OUT AS GOOD */
unsigned long read_number (char *line, int *offset_to_next_number,
                           int data_type);

/* returns an n-bit mask */
unsigned long data_mask (unsigned short number_of_bits);

/* removes leading,  trailing and intervening spaces from a bit pattern */

/* returns the number of bits in the pattern */
int nice_bit_pattern (char *line, char *nice_bits);

/* returns 1 if odd_parity, 0 if even parity */

/* for example, if I want a word to be even parity:
   if(odd_parity(word))
      set_even_parity_bit;
*/
int odd_parity (unsigned short word);

/* returns the number of bits set in a word */

/* used to detect error conditions. For example
   if(number_of_bits_set(even_parity_mask)>1)
      output an error
*/
int number_of_bits_set (unsigned short word);

/* returns a mask that corresponds to the position of the input character */

/* for example:
	if nice_bits={'0','1','0','e','e','1','1','0',
		      '0','0','0','0','0','0','0','o'}
	then bit_mask_to_number(nice_bits,'0') = 1010000111111110b
	then bit_mask_to_number(nice_bits,'1') = 0100011000000000b
	then bit_mask_to_number(nice_bits,'e') = 0001100000000000b
	then bit_mask_to_number(nice_bits,'o') = 0000000000000001b
   (plus another 16 bits)
*/
unsigned short bit_mask_to_number (char *nice_bits, char c);

/* process exactly one data input */

/* returns 1 on success, 0 on failure */
int get_data (struct data_field *current, char *input, unsigned long *ul);

/* process one line of input: */
int command_input (struct command_field *first, char *line,
                   unsigned short *output_words);

/* fills the next word and returns a pointer to further on up the line */

/* returns NULL if there are no more words */
char *next_word (char *line, char *the_next_word);

/* process the data part of an input line: */
int process_fields (struct data_field *first_child, char *line,
                    unsigned short *output_words);

/* search for a match for the enumerated and if found,
   then return a pointer to the structure. If not found call recursively.
   If never found, return NULL and offset_to_next_number=-1 */
struct enumerated_table *get_enumerated (struct enumerated_table *e,
                                         char *input);

/* checks to see if a number is in range */

/* returns 1 on success, 0 on failure */
int in_range (struct data_field *field, unsigned long number);


/* the positioner:
	takes a number and shifts it accordingly,
	putting it into a word in the correct place
*/

/* CHECKED OUT AS GOOD */
int positioner (unsigned long l_input, unsigned short *s_output,
                unsigned int left_shift);

/* embed the data into the output buffer */
int embed (unsigned short *out_s_words, unsigned short *s_words,
           int word_number);

int process_field_eq (struct data_field *current, FILE * decode_file);

int process_command_eq (struct command_field *current, FILE * decode_file);

/* input the name/path of the decode table file */

/* this procedure builds the decode table up */

/* returns a pointer to the first command field */
struct command_field *build_decode_table (char *file_name);

/* strips white space from a string */
int remove_blanks (char *line);






/* routines used to verify the proper building of the command and data field
   structures: */
void print_command_and_data_structure (struct command_field *current_command);
struct data_field *print_data_fields (struct data_field *current);
char *convert_enum_to_string (int enumerated);
struct enumerated_table *print_enums (struct enumerated_table *current,
                                      unsigned long size);

struct enumerated_table *print_tenums (struct enumerated_table *current);

/* returns the number converted to binary */
char *bin (unsigned short number);

/* converts a number to binary format, printing the char X instead of '1' */
char *binX (unsigned short number, char X);

/* returns the number converted to binary */
char *binL (unsigned long number);

/* converts a number to binary format, printing the char X instead of '1' */
char *binLX (unsigned long number, char X);

/* puts in 0 instead of '0' ... not a real string */
char *binX0 (unsigned short number, char X);
#endif
