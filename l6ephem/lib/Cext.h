/* Use C99 fixed width integers */
#include <stdint.h>

#ifndef _Cext_h
#define _Cext_h

#ifdef __cplusplus
extern "C"
{
#endif


/*  Switched over to C99 standard boolean type */
/* #ifndef _Bool
#define _Bool
  typedef enum { False, True } Bool;
#endif
*/

#ifndef _UCHAR
#define _UCHAR
  typedef uint8_t UCHAR;
#endif

#ifndef _Uchar
#define _Uchar
  typedef uint8_t Uchar;
#endif

#ifndef _USHORT
#define _USHORT
  typedef uint16_t USHORT;
#endif

#ifndef _Ushort
#define _Ushort
  typedef uint16_t Ushort;
#endif

#ifndef _ULONG
#define _ULONG
  typedef uint32_t ULONG;
#endif

#ifndef _Ulong
#define _Ulong
  typedef uint32_t Ulong;
#endif

#ifndef _Cext_C
#define sEquals(x) ( strcpy(calloc(strlen(x),sizeof(char)),x) )
#endif

#ifdef __cplusplus
}          /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif     /* _Cext_h */
