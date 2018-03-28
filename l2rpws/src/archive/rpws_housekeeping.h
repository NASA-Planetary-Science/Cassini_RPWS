#pragma pack(1)
struct DUST_RECORD
{
  unsigned short micro_id_length;
  unsigned short rti;
  unsigned short address;
  unsigned short packet_count;
  unsigned char count[8];
};

#pragma pack()
