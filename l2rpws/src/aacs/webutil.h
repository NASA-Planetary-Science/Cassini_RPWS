/** @file Utilities used in many Cassini CGI programs.  Origin of the
 * source file is unknown, but it doesn't look like it was written here.
 */

#ifndef _webutil_H_
#define _webutil_H_

void getword(char *word, char *line, char stop);

char *makeword(char *line, char stop);

char *fmakeword(FILE *f, char stop, int *cl);

char x2c(char *what) ;

void unescape_url(char *url);

void plustospace(char *str);

int rind(char *s, char c);

int getline(char *s, int n, FILE *f);

void send_fd(FILE *f, FILE *fd);

int ind(char *s, char c);

void escape_shell_cmd(char *cmd);

#endif /* _webutil_H_ */
