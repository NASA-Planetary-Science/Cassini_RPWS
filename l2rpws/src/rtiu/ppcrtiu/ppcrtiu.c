
/*
 *	input that gets processed
 *
 *	0000 A000		accumulate hexadecimal numbers
 *	SAx			select subaddress & send
 *	STnnn			time-tag & send
 *
 *	bus_a			1553 bus select
 *	bus_b
 *
 */
#include <stdio.h>
#include <string.h>

#define SA_RPWS 25
#define TOKENCNT 128
#define CMD_START 12
char *cmd_format[] = { "secmd CDSCMD %2d %2d %3d" };
int cmd_sub_address = 7;
int cmd_length;
int cmd_words[TOKENCNT];
int cmd_flag = 0;
char cmd_buffer[2048];

struct SPECIAL
{
  char *mnemonic;
  char *translation;
} special[] = {
"exit", "",
    "debug", "",
    "delay_alf", "",
    "delay_alf_slow", "",
    "delay_cmd", "",
    "secmd", "",
    "data_off", "",
    "data_on", "",
    "bus_a", "rtiucmd SETBUS A", "bus_b", "rtiucmd SETBUS B", "" ""};

static int accumulate_words (char *buffer)
{
  char *tokens[TOKENCNT];
  char delim[] = { " \t\n\r" };
  int i;
  int wcnt = 0;

  /*
   *      Extract tokens
   */
  tokens[0] = strtok (buffer, delim);
  for (i = 1; i < TOKENCNT; i++) {
    tokens[i] = strtok (NULL, delim);
    if (!tokens[i])
      break;
  }

  /*
   *      special recgonition
   */
  i = 0;
  while (special[i].mnemonic[0]) {      /* end of list */
    if (!strcasecmp (tokens[0], special[i].mnemonic)) { /* special case */
      if (special[i].translation[0]) {
        fprintf (stdout, "%s\n", special[i].translation);
        wcnt = -2;
      } else
        wcnt = -1;
      break;
    }
    i++;
  }
  if (wcnt)
    return wcnt;

  /*
   *      recgonition
   */
  wcnt = 1;
  switch (tokens[0][0]) {
   case '0':                           /* hexadecimal words */
   case '1':
   case '2':
   case '3':
   case '4':
   case '5':
   case '6':
   case '7':
   case '8':
   case '9':
   case 'a':
   case 'b':
   case 'c':
   case 'd':
   case 'e':
   case 'f':
   case 'A':
   case 'B':
   case 'C':
   case 'D':
   case 'E':
   case 'F':
   case 's':                           /* sub address words */
   case 'S':
     wcnt = 0;
  }

  if (wcnt)
    return 0;

  wcnt = 1;
  for (i = 0; i < TOKENCNT; i++) {
    if (!tokens[i])
      break;                            /* no more tokens */
    switch (tokens[i][0]) {
     default:
       cmd_words[cmd_length] = strtol (tokens[i], NULL, 16);
       cmd_length++;
       wcnt++;
       break;
     case 'S':
     case 's':
       cmd_flag = 1;                    /* send command */
       switch (tokens[i][1]) {
        case 0:
          break;
        case 'A':
        case 'a':
          cmd_sub_address = strtol (&tokens[i][2], NULL, 10);
          break;
        case 'T':
        case 't':
          break;
        case '0':                      /* can't do MISC timed commands */
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
          break;
       }
    }
  }
  return wcnt;
}

int main (int argc, char *argv[])
{
  int i;
  char buffer[1024];
  char tbuf[1024];

  while (fgets (buffer, 1023, stdin)) {
    strcpy (tbuf, buffer);
    switch (accumulate_words (tbuf)) {  /* return >0 if recgonized */
     case 0:
     case -1:
       fputs (buffer, stdout);
    }
    if (cmd_flag) {
      if (cmd_length) {
        fprintf (stdout, cmd_format[0], SA_RPWS, cmd_sub_address, cmd_length);
        for (i = 0; i < cmd_length; i++)
          fprintf (stdout, " %04X", cmd_words[i]);
        fprintf (stdout, "\n");
      }
      cmd_flag = 0;
      cmd_length = 0;
    }
  }
  return 0;
}
