#include <stdio.h>
#include "parser.h"
#include <string.h>
#include <stdlib.h>

#ifdef DEBUG
#undef perror
#define perror printf
#undef stderr
#define stderr stdout
#endif

int next_type (char *next);

int program_info (char *file_string, int line_number)
{
  char location[120];

  sprintf (location, "Program: %s Code File: %s Line: %d ",
           program_name (NULL), file_string, line_number);
  perror (location);
  return 1;

}


int file_info (FILE * decode_file)
{
  fpos_t filetell, f;
  char line[200];

  filetell = ftell (decode_file);
  if (filetell > 300) {
    f = filetell - 300;
  } else {
    f = 0;
  }
  fsetpos (decode_file, &f);

  while (ftell (decode_file) < filetell) {
    fgets (line, 199, decode_file);
    fprintf (stderr, line);
  }

  fprintf (stderr, "\n");
  return 1;
}


/* gets the next input and sets the type it is */

/* returns a pointer to the input string, returns NULL if eof */
char *next_input (int *input_type, FILE * decode_file)
{
  static char line[MAX_DECODE_FILE_LINE] = { 0 };
  static char *y = line;
  static char next[MAX_DECODE_FILE_LINE];

  if (y[0] == 0) {

  read_next:
    /*
     * read in a line 
     */
    if (fgets (line, MAX_DECODE_FILE_LINE, decode_file) == NULL) {
      input_type[0] = DT_EOF;
      /*
       * end of file 
       */
      return NULL;
    }

    /*
     * delete comments: 
     */
    comment_delimit (line, 0);

/*      if(strstr(line,"!"))
	 strstr(line,"!")[0]=0;
      if(strstr(line,";"))
	 strstr(line,";")[0]=0; */

    /*
     * make the command input lower case 
     */
    convert_to_lower_case (line);
    /*
     * convert tabs and commas to white spaces 
     */
    while (strstr (line, "\t"))
      strstr (line, "\t")[0] = ' ';

    /*
     * remove all " " spaces from the line 
     */
    remove_blanks (line);
    while (strstr (line, ","))
      strstr (line, ",")[0] = ' ';

    while (strstr (line, "\n"))
      strstr (line, "\n")[0] = 0;

    y = line;
    while (y[0] == ' ') {
      y++;
    }
    if (y[0] == 0)
      goto read_next;

  }

  sscanf (y, "%s", next);
  if (strstr (next, "=")) {
    (strstr (next, "=") + 1)[0] = 0;
    strstr (y, "=")[0] = ' ';
  }
  input_type[0] = next_type (next);

  if (input_type[0] == DT_COMMENT)
    goto read_next;

  /*
   * point y to the next input: 
   */
  while (y[0] != ' ' && y[0] != 0) {
    y++;
  }
  while (y[0] == ' ' && y[0] != 0) {
    y++;
  }
  if (input_type[0] == DT_DELIMITER) {
    comment_delimit (y, 1);
    goto read_next;
  }

  if (input_type[0] == DT_EOF)
    return NULL;
  return (next);
}



int next_type (char *next)
{
  if (strcmp (next, "command=") == 0)
    return DT_COMMAND;

  if (strcmp (next, "type=") == 0)
    return DT_TYPE;
  if (strcmp (next, "power") == 0)
    return DT_POWER;
  if (strcmp (next, "powersim") == 0)
    return DT_POWER_SIM;
  if (strcmp (next, "biu") == 0)
    return DT_BIU;
  if (strcmp (next, "memory") == 0)
    return DT_MEMORY;
  if (strcmp (next, "instrument") == 0)
    return DT_INSTRUMENT;
  if (strcmp (next, "fswload") == 0)
    return DT_FSWLOAD;
  if (strcmp (next, "tlmmode") == 0)
    return DT_TLMMODE;

  if (strcmp (next, "size=") == 0)
    return DT_SIZE;
  if (strcmp (next, "delimiter=") == 0)
    return DT_DELIMITER;
  if (strcmp (next, "mask=") == 0)
    return DT_MASK;

  if (strcmp (next, "field=") == 0)
    return DT_FIELD;

  if (strcmp (next, "data_type=") == 0)
    return DT_DATA_TYPE;
  if ((strcmp (next, "integer") == 0) ||
      (strcmp (next, "int") == 0) || (strcmp (next, "signed") == 0))
    return DT_SIGNED;
  if (strcmp (next, "unsigned") == 0)
    return DT_UNSIGNED;
  if ((strcmp (next, "hexadecimal") == 0) || (strcmp (next, "hex") == 0))
    return DT_HEX;
  if (strcmp (next, "octal") == 0)
    return DT_OCT;

  if ((strcmp (next, "enumerated") == 0) || (strcmp (next, "enum") == 0))
    return DT_ENUM;

  if ((strcmp (next, "textenum") == 0) || (strcmp (next, "txtenum") == 0))
    return DT_TXT_ENUM;

  if ((strcmp (next, "placeholder") == 0) || (strcmp (next, "place") == 0))
    return DT_PLACE;
  if ((strcmp (next, "char_uc") == 0))
    return DT_CHAR_UC;
  if ((strcmp (next, "char_lc") == 0))
    return DT_CHAR_LC;
  if ((strcmp (next, "fill=") == 0))
    return DT_FILL;
  if ((strcmp (next, "reverse=") == 0))
    return DT_REVERSE;
  if ((strcmp (next, "checksum") == 0))
    return DT_CHECKSUM;

  if ((strcmp (next, "text") == 0))
    return DT_TEXT;
  if ((strcmp (next, "character") == 0) || (strcmp (next, "char") == 0))
    return DT_CHAR;
  if ((strcmp (next, "character_msb") == 0)
      || (strcmp (next, "char_msb") == 0))
    return DT_CHAR_MSB;

  if (strcmp (next, "position=") == 0)
    return DT_POSITION;

  if (strcmp (next, "range=") == 0)
    return DT_RANGE;

  if (strcmp (next, "keyword=") == 0)
    return DT_KEYWORD;
  if (strcmp (next, "textkey=") == 0)
    return DT_TEXTKEY;

  if (strcmp (next, "variable=") == 0)
    return DT_VARIABLE;

  if (strcmp (next, "end=") == 0)
    return DT_END;

  if (strcmp (next, "repeat=") == 0)
    return DT_REPEAT;

  if (strcmp (next, "eof=") == 0)
    return DT_EOF;                      /* end of file */

  if (strcmp (next, "comment=") == 0)
    return DT_COMMENT;

  if (strstr (next, "=") != NULL)
    return DT_INVALID_COMMAND;          /* an equal appears without being in a
                                         * known command */
  return DT_UNKNOWN;                    /* anything else, valid or not */

}



/* fills in most part of the command_field structure */

/* returns the type of the next command */
int process_command_eq (struct command_field *current, FILE * decode_file)
{
  char *next;
  int input_type[1];
  int i, j, k;
  unsigned int t;
  char type_flag = 0;
  char size_flag = 0;
  char mask_flag = 0;
  char mask[200];

  /*
   * get the name of the command: 
   */
  next = next_input (input_type, decode_file);
  if (next == NULL)
    return DT_EOF;                      /* eof reached */
  if (strstr (next, "=") != NULL) {
    /*
     * a valid mnemonic won't contain an equal sign: 
     */
    program_info (__FILE__, __LINE__);
    fprintf (stderr, "Error after command=. Decode file error. \n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);
    exit (EXIT_FAILURE);
  }
  strcpy (current->name, next);

  for (i = 0; i < 3; i++) {
    next = next_input (input_type, decode_file);
    if (next == NULL)
      return DT_EOF;                    /* eof reached */
    switch (input_type[0]) {
     case DT_TYPE:
       if (type_flag) {
         /*
          * a valid mnemonic won't contain an equal sign: 
          */
         program_info (__FILE__, __LINE__);
         fprintf (stderr,
                  "Multiple type= after command=. Decode file error. \n");
         fprintf (stderr, "%s\n", next);
         file_info (decode_file);
         exit (EXIT_FAILURE);
       }
       type_flag = 1;
       next = next_input (input_type, decode_file);
       if (next == NULL)
         return DT_EOF;                 /* eof reached */
       switch (input_type[0]) {
        case DT_POWER_SIM:
        case DT_POWER:
        case DT_BIU:
        case DT_INSTRUMENT:
        case DT_MEMORY:
        case DT_PASS:
        case DT_FSWLOAD:
        case DT_TLMMODE:
          current->command_type = input_type[0];
          break;
        default:
          program_info (__FILE__, __LINE__);
          fprintf (stderr, "Error after type=. Decode file error. \n");
          fprintf (stderr, "%s\n", next);
          file_info (decode_file);
          exit (EXIT_FAILURE);
       }
       break;
     case DT_SIZE:
       if (size_flag) {
         /*
          * a valid mnemonic won't contain an equal sign: 
          */
         program_info (__FILE__, __LINE__);
         fprintf (stderr,
                  "Multiple size= after command=. Decode file error. \n");
         fprintf (stderr, "%s\n", next);
         file_info (decode_file);
         exit (EXIT_FAILURE);
       }
       size_flag = 1;
       next = next_input (input_type, decode_file);
       if (next == NULL)
         return DT_EOF;                 /* eof reached */
       if (sscanf (next, "%d", &t) != 1) {
         /*
          * a decimal number is not after size= 
          */
         program_info (__FILE__, __LINE__);
         fprintf (stderr, "Expect number after size=. Decode file error. \n");
         fprintf (stderr, "%s\n", next);
         file_info (decode_file);
         exit (EXIT_FAILURE);
       }
       current->size = (unsigned short) t;
       break;
     case DT_MASK:
       if (!size_flag) {
         /*
          * a valid mnemonic won't contain an equal sign: 
          */
         program_info (__FILE__, __LINE__);
         fprintf (stderr, "size= must precede mask=. Decode file error. \n");
         fprintf (stderr, "%s\n", next);
         file_info (decode_file);
         exit (EXIT_FAILURE);
       }
       if (mask_flag) {
         /*
          * a valid mnemonic won't contain an equal sign: 
          */
         program_info (__FILE__, __LINE__);
         fprintf (stderr,
                  "Multiple mask= after command=. Decode file error. \n");
         fprintf (stderr, "%s\n", next);
         file_info (decode_file);
         exit (EXIT_FAILURE);
       }
       mask_flag = 1;
       /*
        * put defaults in the masks: 
        */
       for (j = 0; j < 8; j++) {
         current->one_mask[j] = 0;
         current->data_mask[j] = ~0;
         current->inversion_mask[j] = 0;
         current->even_parity_mask[j] = 0;
         current->odd_parity_mask[j] = 0;
       }

       /*
        * suck in the bits for the bit mask: 
        */
       for (j = 0; j < current->size; j++) {
         mask[0] = 0;
         k = 0;
         /*
          * get a 16 bit mask: 
          */
         while (k != 16) {
           next = next_input (input_type, decode_file);
           if (next == NULL)
             return DT_EOF;             /* eof reached */

/*		   if(input_type[0]!=DT_UNKNOWN&&k!=0)
*/
           if (input_type[0] != DT_UNKNOWN) {
             /*
              * a normal return where all the masks are not explicit: 
              */
             return input_type[0];
           }
           /*
            * strip out all bits that don't belong in the mask: 
            */
           nice_bit_pattern (next, &mask[k]);
           k = strlen (mask);
           if (k > 16) {
             program_info (__FILE__, __LINE__);
             fprintf (stderr,
                      "Incorrect mask size or character. Decode file error.\n");
             fprintf (stderr, "%s\n", next);
             file_info (decode_file);
             exit (EXIT_FAILURE);
           }
         }
         current->one_mask[j] = bit_mask_to_number (mask, '1');
         current->data_mask[j] = bit_mask_to_number (mask, '0');
         current->inversion_mask[j] = bit_mask_to_number (mask, 'i');
         current->even_parity_mask[j] = bit_mask_to_number (mask, 'e');
         current->odd_parity_mask[j] = bit_mask_to_number (mask, 'o');
       }
       next = next_input (input_type, decode_file);
       if (next == NULL)
         return DT_EOF;                 /* eof reached */
       /*
        * a normal return where all the masks have been input: 
        */
       return input_type[0];
     default:
       program_info (__FILE__, __LINE__);
       fprintf (stderr, "Error after command=. Decode file error. \n");
       fprintf (stderr, "%s\n", next);
       file_info (decode_file);
       exit (EXIT_FAILURE);
    }
  }
  /*
   * the program should never get here, but if it does... 
   */
  program_info (__FILE__, __LINE__);
  fprintf (stderr, "Program Error.\n");
  fprintf (stderr, "%s\n", next);
  file_info (decode_file);
  exit (EXIT_FAILURE);
  return 0;
}


/* fills in most parts of the data_field structure */

/* returns the type of the next command */
int process_field_eq (struct data_field *current, FILE * decode_file)
{
  char *next;
  int input_type[1];
  int i, j, k;
  unsigned short bit_pattern;
  char bits[80];
  char bit_string[20] = { "0000000000000000" };
  char mnemonic[80];
  char text[80];
  unsigned long ul, low, high;
  int offset[1];
  struct enumerated_table *last_enum = NULL;
  struct enumerated_table *next_enum = NULL;

  current->scale = 0;
  current->scale_div = 0;
  current->scale_mul = 0;

  current->enums = NULL;
  current->fill = 0;
  current->reverse = 0;

  /*
   *** get the name of the field: THIS IS REQUIRED */
  strcpy (current->name, next_input (input_type, decode_file));

  /*
   * get data_type= 
   */
  next = next_input (input_type, decode_file);
  if (next == NULL)
    return DT_EOF;                      /* eof reached */
  if (input_type[0] != DT_DATA_TYPE) {
    program_info (__FILE__, __LINE__);
    fprintf (stderr,
             "Error after field=. data_type= expected.Decode file error. \n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);
    exit (EXIT_FAILURE);
  }
  next = next_input (input_type, decode_file);
  if (next == NULL)
    return DT_EOF;                      /* eof reached */
  if (input_type[0] >= DT_SIGNED && input_type[0] <= DT_CHAR) {
    current->data_type = input_type[0];
  } else {
    program_info (__FILE__, __LINE__);
    fprintf (stderr, "Error after data_type=. Decode file error. \n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);

/*#ifndef DEBUG
*/ exit (EXIT_FAILURE);

/*#endif
*/
  }

  /*
   * get position= 
   */
  next = next_input (input_type, decode_file);
  if (next == NULL)
    return DT_EOF;                      /* eof reached */
  if (input_type[0] != DT_POSITION) {
    program_info (__FILE__, __LINE__);
    fprintf (stderr, "Error: position= expected.Decode file error. \n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);
    exit (EXIT_FAILURE);
  }

  /*
   * get the word number of position= : 
   */
  next = next_input (input_type, decode_file);
  if (next == NULL)
    return DT_EOF;                      /* eof reached */
  if (input_type[0] != DT_UNKNOWN) {
    program_info (__FILE__, __LINE__);
    fprintf (stderr,
             "Error after position=. Data expected. Decode file error.\n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);
    exit (EXIT_FAILURE);
  }
  current->word_number = (unsigned short) read_number (next, offset, 0);
  if (offset[0] == -1) {
    program_info (__FILE__, __LINE__);
    fprintf (stderr,
             "Error after position=. Data expected. Decode file error. \n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);
    exit (EXIT_FAILURE);
  }

  /*
   * get the bit of position= : 
   */
  next = next_input (input_type, decode_file);
  if (next == NULL)
    return DT_EOF;                      /* eof reached */
  if (input_type[0] != DT_UNKNOWN) {
    program_info (__FILE__, __LINE__);
    fprintf (stderr,
             "Error after position=. Data expected. Decode file error. \n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);
    exit (EXIT_FAILURE);
  }
  current->left_shift = (unsigned short) read_number (next, offset, 0);
  if (offset[0] == -1) {
    program_info (__FILE__, __LINE__);
    fprintf (stderr,
             "Error after position=. Data expected. Decode file error. \n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);
    exit (EXIT_FAILURE);
  }

  /*
   * get the size of position= : 
   */
  next = next_input (input_type, decode_file);
  if (next == NULL)
    return DT_EOF;                      /* eof reached */
  if (input_type[0] != DT_UNKNOWN) {
    program_info (__FILE__, __LINE__);
    fprintf (stderr,
             "Error after position=. Data expected. Decode file error. \n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);
    exit (EXIT_FAILURE);
  }
  ul = read_number (next, offset, 0);

  current->size = (unsigned short) ul;
  if (current->data_type == DT_SIGNED) {
    if (((signed long) ul >= 0) && ((signed long) ul <= 32)) {
      current->data_mask = data_mask (ul);
    } else {
      program_info (__FILE__, __LINE__);
      fprintf (stderr,
               "Error after position=. size out of range. Decode file error.\n");
      fprintf (stderr, "%s\n", next);
      file_info (decode_file);
      exit (EXIT_FAILURE);
    }
  } else {
    if (ul <= 32) {
      current->data_mask = data_mask (ul);
    } else {
      if (current->data_type != DT_TEXT &&
          current->data_type != DT_CHAR &&
          current->data_type != DT_CHAR_UC &&
          current->data_type != DT_CHAR_LC &&
          current->data_type != DT_CHAR_MSB) {
        program_info (__FILE__, __LINE__);
        fprintf (stderr,
                 "Error after position=. size out of range. Decode file error.\n");
        fprintf (stderr, "%s\n", next);
        file_info (decode_file);
        exit (EXIT_FAILURE);
      } else {
        if (ul % 8) {
          program_info (__FILE__, __LINE__);
          fprintf (stderr,
                   "Error after position=. size must be a multiple of 8 for char types. Decode file error.\n");
          fprintf (stderr, "%s\n", next);
          file_info (decode_file);
          exit (EXIT_FAILURE);
        }
      }
    }
  }
  if (offset[0] == -1) {
    program_info (__FILE__, __LINE__);
    fprintf (stderr,
             "Error after position=. Data expected. Decode file error. \n");
    fprintf (stderr, "%s\n", next);
    file_info (decode_file);
    exit (EXIT_FAILURE);
  }

  next = next_input (input_type, decode_file);
  if (next == NULL)
    return DT_EOF;                      /* eof reached */

/*
 *	We will tend to use enumerations in type=placeholder
 *	to simply keep track of the data field mnemonics
 */
  j = 0;
  last_enum = NULL;
  if (input_type[0] == DT_KEYWORD) {
    if ((current->data_type != DT_ENUM) && (current->data_type != DT_PLACE)) {
      program_info (__FILE__, __LINE__);
      fprintf (stderr, "    (%d)%s\n",
               current->data_type,
               convert_enum_to_string (current->data_type));
      fprintf (stderr,
               "keyword= on non-enumerated field. Decode file error. \n");
      fprintf (stderr, "%s\n", next);
      file_info (decode_file);
      exit (EXIT_FAILURE);
    }

    next = next_input (input_type, decode_file);
    if (next == NULL)
      return DT_EOF;                    /* eof reached */

    while (input_type[0] > DT_COMMENT) {
      /*
       * copy the mnemonic: 
       */
      strcpy (mnemonic, next);

      k = 0;
      while (k < current->size) {
        /*
         * get the bit mask 
         */
        next = next_input (input_type, decode_file);
        if (next == NULL)
          return DT_EOF;                /* eof reached */
        for (i = 0; i < strlen (next); i++) {
          if (next[i] != '0' && next[i] != '1') {
            /*
             * error: not a bit pattern 
             */
            program_info (__FILE__, __LINE__);
            fprintf (stderr, "Error after enumerated. "
                     "Bit pattern expected. " "Decode file error. " "\n");
            fprintf (stderr, "%s\n", next);
            file_info (decode_file);
            exit (EXIT_FAILURE);
          }
        }
        strcpy (bits + k, next);
        k += strlen (next);
      }

      strcpy (bit_string, "0000000000000000");
      strcpy (bit_string + 8 * sizeof (unsigned short) - strlen (bits), bits);

      bit_pattern = bit_mask_to_number (bit_string, '1');
      /*
       * now I have bit pattern and mnemonic 
       */
      if ((next_enum = (struct enumerated_table *) malloc
           (sizeof (struct enumerated_table))) == NULL) {
        program_info (__FILE__, __LINE__);
        fprintf (stderr, "malloc error!!!!! Program problem!!!\n");
        fprintf (stderr, "%s\n", next);
        file_info (decode_file);
        exit (EXIT_FAILURE);
      }

      if (current->enums == NULL)
        current->enums = next_enum;

      /*
       * copy the mnemonic: 
       */
      strcpy (next_enum->name, mnemonic);
      next_enum->value = bit_pattern;
      next_enum->tail = NULL;
      if (last_enum != NULL) {
        last_enum->tail = next_enum;
      }
      last_enum = next_enum;

      next = next_input (input_type, decode_file);
      if (next == NULL)
        return DT_EOF;                  /* eof reached */
    }
  }

/********* telemetry mode select *********/
  if (input_type[0] == DT_TEXTKEY) {
    if ((current->data_type != DT_TXT_ENUM)) {
      program_info (__FILE__, __LINE__);
      fprintf (stderr, "    (%d)%s\n",
               current->data_type,
               convert_enum_to_string (current->data_type));
      fprintf (stderr,
               "textkey= on non-enumerated field. Decode file error. \n");
      fprintf (stderr, "%s\n", next);
      file_info (decode_file);
      exit (EXIT_FAILURE);
    }

    next = next_input (input_type, decode_file);
    /*
     * fprintf(stderr,"XXX %d %s\n", __LINE__, next); /*
     */
    if (next == NULL)
      return DT_EOF;                    /* eof reached */

    /*
     * fprintf(stderr,"XX1 %d %s(%d)>%s(%d)\n", __LINE__, 
     * convert_enum_to_string(input_type[0]),
     * input_type[0],
     * convert_enum_to_string(DT_COMMENT),
     * DT_COMMENT); /*
     */
    while (input_type[0] > DT_COMMENT) {
      /*
       * copy key and translation 
       */
      strcpy (mnemonic, next);
      next = next_input (input_type, decode_file);
      /*
       * fprintf(stderr,"XXX %d %s\n", __LINE__, next); /*
       */
      if (next == NULL)
        return DT_EOF;                  /* eof reached */
      strcpy (text, next);

      /*
       * now I have translation and mnemonic 
       */
      if ((next_enum = (struct enumerated_table *) malloc
           (sizeof (struct enumerated_table))) == NULL) {
        program_info (__FILE__, __LINE__);
        fprintf (stderr, "malloc error!!!!! Program problem!!!\n");
        fprintf (stderr, "%s\n", next);
        file_info (decode_file);
        exit (EXIT_FAILURE);
      }

      if (current->enums == NULL)
        current->enums = next_enum;

      /*
       * copy the mnemonic: 
       */
      strcpy (next_enum->name, mnemonic);
      strcpy (next_enum->text, text);
      next_enum->value = 0;
      next_enum->tail = NULL;
      if (last_enum != NULL) {
        last_enum->tail = next_enum;
      }
      last_enum = next_enum;

      next = next_input (input_type, decode_file);
      /*
       * fprintf(stderr,"XXX %d %s\n", __LINE__, next); /*
       */
      if (next == NULL)
        return DT_EOF;                  /* eof reached */
      /*
       * fprintf(stderr,"XX1 %d %s(%d)>%s(%d)\n", __LINE__, 
       * convert_enum_to_string(input_type[0]),
       * input_type[0],
       * convert_enum_to_string(DT_COMMENT),
       * DT_COMMENT);/*
       */
    }
  }

  if (input_type[0] == DT_FILL) {
    if (current->data_type != DT_TEXT &&
        current->data_type != DT_CHAR &&
        current->data_type != DT_CHAR_UC &&
        current->data_type != DT_CHAR_LC &&
        current->data_type != DT_CHAR_MSB) {
      program_info (__FILE__, __LINE__);
      fprintf (stderr,
               "Error: fill= encountered, data type not char_xx. Decode file error. \n");
      fprintf (stderr, "%s\n", next);
      file_info (decode_file);
      exit (EXIT_FAILURE);
    }
    /*
     * get fill byte: 
     */
    next = next_input (input_type, decode_file);
    ul = read_number (next, offset, 0);
    if (ul != (ul & 0xFF)) {
      program_info (__FILE__, __LINE__);
      fprintf (stderr,
               "Error after fill=. data not 8 bit. Decode file error. \n");
      fprintf (stderr, "%s\n", next);
      file_info (decode_file);
      exit (EXIT_FAILURE);
    }
    current->fill = (char) ul;
    next = next_input (input_type, decode_file);
  }

  if (input_type[0] == DT_REVERSE) {
    if (current->data_type != DT_TEXT &&
        current->data_type != DT_CHAR &&
        current->data_type != DT_CHAR_UC &&
        current->data_type != DT_CHAR_LC &&
        current->data_type != DT_CHAR_MSB) {
      program_info (__FILE__, __LINE__);
      fprintf (stderr,
               "Error: fill= encountered, data type not char_xx. Decode file error. \n");
      fprintf (stderr, "%s\n", next);
      file_info (decode_file);
      exit (EXIT_FAILURE);
    }
    /*
     * get reverse field: 
     */
    next = next_input (input_type, decode_file);
    ul = read_number (next, offset, 0);
    current->reverse = (int) ul;
  }

  j = 0;
  current->number_of_ranges = 0;
  if (input_type[0] == DT_RANGE) {
    /*
     * get first low: 
     */
    next = next_input (input_type, decode_file);
    if (next == NULL)
      return DT_EOF;                    /* eof reached */
    while (input_type[0] == DT_UNKNOWN) {
      low = read_number (next, offset, 0);
      if (offset[0] == -1) {
        program_info (__FILE__, __LINE__);
        fprintf (stderr,
                 "Error after range=. Number expected. Decode file error. \n");
        fprintf (stderr, "%s\n", next);
        file_info (decode_file);
        exit (EXIT_FAILURE);
      }

      next = next_input (input_type, decode_file);
      if (next == NULL)
        return DT_EOF;                  /* eof reached */

      high = read_number (next, offset, 0);
      if (offset[0] == -1) {
        program_info (__FILE__, __LINE__);
        fprintf (stderr,
                 "Error after range=. Number expected. Decode file error. \n");
        fprintf (stderr, "%s\n", next);
        file_info (decode_file);
        exit (EXIT_FAILURE);
      }
      if (low > high) {
        program_info (__FILE__, __LINE__);
        fprintf (stderr,
                 "Error after range=. low>high. Decode file error. \n");
        fprintf (stderr, "%s\n", next);
        file_info (decode_file);
        exit (EXIT_FAILURE);
      }
      current->lower_limit[j] = low;
      current->upper_limit[j] = high;
      j++;
      current->number_of_ranges = j;

      next = next_input (input_type, decode_file);
      if (next == NULL)
        return DT_EOF;                  /* eof reached */
    }
  }
  return input_type[0];
}


/* input the name/path of the decode table file */

/* this procedure builds the decode table up */

/* returns a pointer to the first command field */
struct command_field *build_decode_table (char *file_name)
{
  struct command_field *c_field = NULL;
  struct command_field *c = NULL;
  struct command_field *first_c_field = NULL;
  struct data_field *d_field = NULL;
  struct data_field *d = NULL;
  FILE *decode_file;
  int token_type;
  int ok_to_make_command_field = 1;
  int ok_to_make_data_field = 0;
  int repeat_count = 0;
  int junk;

  decode_file = fopen (file_name, "r");
  /*
   * fprintf(stderr, "parsbld.c(%d) open(%s)\n", __LINE__, file_name); /*
   */
  token_type = DT_UNKNOWN;

  for (;;) {
    switch (token_type) {
     case DT_COMMAND:
       if (!ok_to_make_command_field) {
         program_info (__FILE__, __LINE__);
         fprintf (stderr, "Unexpected command=. Decode file error. \n");
         file_info (decode_file);
         exit (EXIT_FAILURE);
       }
       c_field = malloc (sizeof (struct command_field));
       /*
        * initiallize the command field 
        */
       c_field->first_child = NULL;
       c_field->next = NULL;
       if (first_c_field == NULL) {
         first_c_field = c_field;
       }
       token_type = process_command_eq (c_field, decode_file);
       ok_to_make_data_field = 1;
       ok_to_make_command_field = 0;
       break;
     case DT_VARIABLE:
       /*
        * fall through 
        */
     case DT_FIELD:
       if (!ok_to_make_data_field) {
         program_info (__FILE__, __LINE__);
         fprintf (stderr, "command= expected. Decode file error. \n");
         file_info (decode_file);
         exit (EXIT_FAILURE);
       }
       d_field = malloc (sizeof (struct data_field));
       d_field->field_type = token_type;
       d_field->tail = NULL;
       /*
        * link in the data_field 
        */
       if (c_field->first_child == NULL) {
         c_field->first_child = d_field;
       } else {
         d = c_field->first_child;
         /*
          * walk down the links 
          */
         while (d->tail != NULL) {
           d = d->tail;
         }
         d->tail = d_field;
       }
       token_type = process_field_eq (d_field, decode_file);
       while (repeat_count > 1) {
         d = malloc (sizeof (struct data_field));
         d[0] = d_field[0];
         d_field->tail = d;
         d->word_number++;
         d_field = d;
         repeat_count--;
       }
       break;

     case DT_END:
       /*
        * link in the command_field 
        */

       c = first_c_field;
       /*
        * walk down the links 
        */
       while (c->next != NULL) {
         c = c->next;
       }
       if (c_field != c)
         c->next = c_field;

       /*
        * set the flags that allow the next field to be entered 
        */
       ok_to_make_command_field = 1;
       ok_to_make_data_field = 0;
       /*
        * fall through 
        */
     case DT_UNKNOWN:
       /*
        * read the next token in 
        */
       next_input (&token_type, decode_file);
       if (token_type == DT_EOF)
         return first_c_field;          /* eof reached */
       break;
     case DT_REPEAT:
       repeat_count =
         (int) read_number (next_input (&token_type, decode_file), &junk, 0);
       if (token_type != DT_UNKNOWN) {
         program_info (__FILE__, __LINE__);
         fprintf (stderr,
                  "Error after repeat=. Number expected. Decode file error. \n");
         file_info (decode_file);
         exit (EXIT_FAILURE);
       }
       break;
     case DT_EOF:
       return first_c_field;
     default:
       token_type = DT_UNKNOWN;         /* get the next token */
       break;
    }
  }
}
