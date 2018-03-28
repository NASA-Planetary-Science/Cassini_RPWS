#include <string.h>
#include "parser.h"
#include <ctype.h>
#include <stdio.h>

extern FILE *in_file;
extern int in_file_active;
char *RPWS_dos_strip (char *str)
{
  int i;
  int j = 0;

  for (i = 0; i < strlen (str); i++) {
    switch (str[i]) {
     case 0x0D:                        /* strip shit that WordStar leaves */
     case 0x1A:                        /* all over files we edit on PC */
       break;
     case '#':
       str[j++] = ';';
       break;
     default:
       str[j++] = str[i];
       break;
    }
  }
  str[j] = 0;
  return str;
}
char *RPWS_gets (char *str, FILE * source)
{
  char *s;
  int eof_flag = 0;

  fflush (stdout);
  str[0] = 0;
  if (in_file_active) {
    s = fgets (str, 999, in_file);
    if (!s)
      str[0] = 0;
    if (strlen (str))
      str[strlen (str) - 1] = 0;
    if (!strlen (str)) {
      fclose (in_file);
      in_file_active = 0;
      strcpy (str, "-send");
      s = &str[0];
    }
  } else {
    s = fgets (str, 999, source);
    if (!s) {
      eof_flag = 1;
    } else {
      if (!s[0])
        eof_flag = 1;
      else {
        if (strlen (str))
          str[strlen (str) - 1] = 0;
        RPWS_dos_strip (str);
      }
    }
  }
  if (eof_flag)
    return NULL;
  else
    return str;
}

char *hex_error (char *text)
{
  int i;
  static char string[512];
  char temp[8];

  string[0] = 0;
  for (i = 0; i < strlen (text); i++) {
    sprintf (temp, "%3.2X", (unsigned char) text[i]);
    strcat (string, temp);
  }
  return string;
}
int print_input_error (int field_number,
                       char *field_name,
                       int data_type,
                       char *input,
                       char *description, char *file_name, int line_number)
{
  static char location[200];

  if (input != NULL) {
    sprintf (location, ">> Input error. Field %02d: %s [%s] '%s' %s. (%s) ",
             field_number,
             field_name,
             convert_enum_to_string (data_type) + 3,
             input, description, location_formatter (file_name, line_number));
  } else {
    sprintf (location, ">> Input error. Field %02d: %s [%s] %s. (%s) ",
             field_number,
             field_name,
             convert_enum_to_string (data_type) + 3,
             description, location_formatter (file_name, line_number));

  }
  fprintf (stderr, "%s", location);
  return 0;

}

char *location_formatter (char *file, int line)
{
  static char info[100];

  sprintf (info, "%s\b\b_%d", file, line);
  return info;
}



/* takes a string input and converts it to lower case */
int convert_to_lower_case (char *string)
{
  int length, i;

  length = strlen (string);
  for (i = 0; i < length; i++) {
    string[i] = tolower (string[i]);
  }

  return 0;
}

int strcmp_case_insensitive (char *string1, char *string2)
{
  char s1[200], s2[200];
  int length, i;

  length = strlen (string1);
  for (i = 0; i < length; i++) {
    s1[i] = tolower (string1[i]);
  }
  s1[i] = 0;

  length = strlen (string2);
  for (i = 0; i < length; i++) {
    s2[i] = tolower (string2[i]);
  }
  s2[i] = 0;

  return (strcmp (s1, s2));

}


/* takes a string input and converts it to upper case */
int convert_to_upper_case (char *string)
{
  int length, i;

  length = strlen (string);
  for (i = 0; i < length; i++) {
    string[i] = toupper (string[i]);
  }

  return 0;
}


/* the input is a line with hex, octal, or decimal numbers on it */

/* assumes the first characters are either spaces or numbers,
   not keywords.
   returns the value read. If there is an error,
   then offset_to_next_number[0]=-1.
   If it is the last number on the line, offset_to_next_number[0]=0
*/

/* side effect: converts tabs and commas to spaces */

/* THIS IS CHECKED OUT AS GOOD */
unsigned long read_number (char *line, int *offset_to_next_number,
                           int data_type)
{
  char *y;
  int i = 0, scan_return;
  unsigned long number = 0;

  y = line;

  /*
   * convert tabs and commas to white spaces 
   */
  while (strstr (line, "\t"))
    strstr (line, "\t")[0] = ' ';
  while (strstr (line, ","))
    strstr (line, ",")[0] = ' ';
  while (y[0] == ' ') {
    y++;
    i++;
  }

  if ((y[0] == '0' && !(y[1] == ' ' || y[1] == 0)) || (data_type == DT_HEX)) {
    /*
     * either hex or octal 
     */
    if ((y[1] == 'x') || (y[1] == 'X') || (data_type == DT_HEX)) {
      /*
       * hex 
       */
      scan_return = sscanf (y, "%X", &number);
    } else {
      while (y[0] == '0' && !(y[1] == ' ' || y[1] == 0)) {
        y++;
        i++;
      }
      scan_return = sscanf (y, "%O", &number);
    }
  } else {
    /*
     * decimal 
     */
    scan_return = sscanf (y, "%D", &number);
  }

  if (scan_return != 1) {
    offset_to_next_number[0] = -1;      /* an error */
    return 0;
  }

  /*
   * find offset_to_next_number: 
   */
  while (!(y[0] == ' ' || y[0] == 0)) {
    y++;
    i++;
  }
  while (y[0] == ' ') {
    y++;
    i++;
  }

  if (y[0] == 0) {
    offset_to_next_number[0] = 0;
    return number;
  }

  offset_to_next_number[0] = i;
  return number;
}

/* returns an n-bit mask */
unsigned long data_mask (unsigned short number_of_bits)
{
  unsigned long mask = 0;
  int i;
  unsigned long j = 1;
  unsigned long two_power[32];

  for (i = 0; i < 32; i++) {
    two_power[i] = j;
    j *= 2;
  }

  for (i = 0; i < number_of_bits; i++) {
    mask += two_power[i];
  }

  return mask;
}

/* strips white space from a string */
int remove_blanks (char *line)
{
  char y[256];
  int i, j;

  i = 0;
  j = 0;

  while (line[i] != 0) {
    if (line[i] != ' ') {
      y[j] = line[i];
      j++;
    }
    i++;
  }
  y[j] = 0;
  j = 0;
  while (y[j] != 0) {
    line[j] = y[j];
    j++;
  }
  line[j] = 0;
  return 1;
}

/* removes leading,  trailing and intervening spaces from a bit pattern */

/* returns the number of bits in the pattern */
int nice_bit_pattern (char *line, char *nice_bits)
{
  int i, j = 0;

  for (i = 0; i < strlen (line); i++) {
    switch (line[i]) {
     case ' ':
     case '\t':
       break;
     case 'e':
     case 'E':
     case 'I':
     case 'i':
     case 'o':
     case 'O':
     case '0':
     case '1':
       nice_bits[j] = line[i];
       j++;
    }
  }
  nice_bits[j] = 0;
  return j;
}


/* returns 1 if odd_parity, 0 if even parity */

/* for example, if I want a word to be even parity:
   if(odd_parity(word))
      set_even_parity_bit;
*/
int odd_parity (unsigned short word)
{
  int i = 0, j;

  for (j = 0; j < 16; j++) {
    if (word & 0x0001)
      i++;
    word = word >> 1;
  }
  return i % 2;
}

/* returns the number of bits set in a word */

/* used to detect error conditions. For example
   if(number_of_bits_set(even_parity_mask)>1)
      output an error
*/
int number_of_bits_set (unsigned short word)
{
  int i = 0, j;

  for (j = 0; j < 16; j++) {
    if (word & 0x0001)
      i++;
    word = word >> 1;
  }
  return i;
}


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
unsigned short bit_mask_to_number (char *nice_bits, char c)
{
  unsigned short number = 0;
  unsigned short two_power[32];
  unsigned short j = 1;
  short i;

  for (i = 15; i >= 0; i--) {
    two_power[i] = j;
    j *= 2;
  }

  for (i = 0; i < 16; i++) {
    if (nice_bits[i] == c) {
      number += two_power[i];
    }
  }
  return number;
}

/* process exactly one data input */

/* returns 1 on success, 0 on failure */
int get_data (struct data_field *current, char *input, unsigned long *ul)
{
  char location[120];
  int next_offset[1];
  struct enumerated_table *e;

  switch (current->data_type) {
   case DT_SIGNED:
   case DT_UNSIGNED:
     ul[0] = read_number (input, next_offset, 0);
     if (next_offset[0] == -1) {
       return 0;                        /* an error */
     }
     break;
  }
  return 1;
}
