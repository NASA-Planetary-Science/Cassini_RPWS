#ifndef _pdsdat_h
#define _pdsdat_h

#ifdef __cplusplus
extern "C" {
#endif

#include <Cext.h>



/* assumed a pointer to an unsigned char */
#define CasPds_nRecLen(x)     ( *((Ulong*)(pByte+ 8)) )
#define CasPds_nNumRec(x)     ( *((Ulong*)(pByte+12)) )
#define CasPds_nType(x)       ( *((Ulong*)(pByte+72)) )
#define CasPds_nMode(x)       ( *((Ulong*)(pByte+76)) )



#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _pdsdat_h */
