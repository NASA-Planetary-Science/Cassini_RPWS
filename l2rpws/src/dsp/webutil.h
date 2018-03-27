
/* Adding web things to here as they are used */

void plustospace (char *str);
void unescape_url (char *url);
void getword (char *word, char *line, char stop);
char *makeword (char *line, char stop);
char *fmakeword (FILE * f, char stop, int *cl);
char x2c (char *what);
int rind (char *s, char c);
int getline (char *s, int n, FILE * f);
void send_fd (FILE * f, FILE * fd);
int ind (char *s, char c);
void escape_shell_cmd (char *cmd);
