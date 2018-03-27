 /*
  *     Trick stuff...
  *     new = malloc(sizeof(struct SORT_ELEMENT) * sizeof(struct CDS_buffer)
  *     new->buffer = &new->data[0]
  *
  *     since we allocated everything together, the data[8]
  *     should match a pointer size so memcpy size works
  *     out just fine...
  */
struct SORT_ELEMENT
{
  struct SORT_ELEMENT *link;
  struct CDS_buffer *buffer;
  char data[4];
};
