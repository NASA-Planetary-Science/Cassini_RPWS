struct MEMORY
{
  int pattern;
  int address;
  int bank;
  int processor;
};
struct DATA_RECORD
{
  struct DATA_RECORD *link;
  struct MP_buffer *buffer;
  struct MEMORY memory;
  int rti_itime;
  int rti_temp;
  int rti_delta;
  int valid_flag;
};
extern int venus_enqueue (struct MP_buffer *, struct MP_buffer *, int);
extern int venus_mem_dump (struct DATA_RECORD *, FILE *);
extern struct DATA_RECORD *venus_insert (struct DATA_RECORD *, int);
extern struct DATA_RECORD *venus_dequeue (int, int);
