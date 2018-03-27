
#define RICEDEBUG_REFSMP 0x00000001
#define RICEDEBUG_CODEID 0x00000002

#define RICEDEBUG_FSBITS 0x00000004
#define RICEDEBUG_FSDATA 0x00000008
#define RICEDEBUG_SSBITS 0x00000010
#define RICEDEBUG_SSDATA 0x00000020
#define RICEDEBUG_BOBITS 0x00000040
#define RICEDEBUG_BODATA 0x00000080

#define RICEDEBUG_MAPDELTA 0x00000100
#define RICEDEBUG_OUTDATA  0x00000200
#define RICEDEBUG_PSIHIST  0x00000400
#define RICEDEBUG_ERRORS   0x00000800

#define RICEDEBUG_ALL 0xFFFFFFFF

extern char *RICE_FILENAME;
extern int arHist_Psi[];
extern unsigned long nWCin, nWCout;
extern unsigned long RICE_DEBUG;

unsigned long rice_decompress (unsigned *inbuffer, unsigned inlength,
                               unsigned *outbuffer, unsigned *outlength,
                               unsigned bits_per_sample);
