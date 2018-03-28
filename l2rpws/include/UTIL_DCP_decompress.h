int get_mpp_indx (struct MPP *, int);
void get_mpp_init (struct MPP *, int, int);
void get_mpp_fill (void);
int get_mpp_left (void);
int get_mpp_test (void);
int get_mpp_bit ();
int get_mpp_bits (int);
int get_mpp_byte ();
int get_mpp_word ();

void put_mpp_init (struct MPP *, int);
void put_mpp_fill (void);
void put_mpp_bit (int);
void put_mpp_bits (int, int);
void put_mpp_byte (int);
void put_mpp_word (int);

long DCP_decompress (struct MP_buffer *, struct RPWS_buffer *);
