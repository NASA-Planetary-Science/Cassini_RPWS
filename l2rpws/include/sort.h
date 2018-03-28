struct SORT_LIST
{
  struct SORT_LIST *link;
  struct CDS_buffer *buffer;
  double sclk;
};
struct SORT_LIST *SORT_open (FILE * error);
int SORT_insert (struct SORT_LIST *list_head, struct CDS_buffer *buffer);

#ifndef _sort_
extern char *SORT_Version;
#endif
