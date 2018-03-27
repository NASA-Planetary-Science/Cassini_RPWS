#include <stdio.h>
#include <stdlib.h>


#ifndef _Cext_h
#define _Cext_h

#ifdef __cplusplus
extern "C" {
#endif



#define CEXT_FILENAME_MAX	128
#define CEXT_LINE_IN_MAX        512	



#define ErrorCalloc(hHandle,nElements,nSize)  fprintf((hHandle),"%s %d, calloc(0x%lX,0x%X) fail\n",__FILE__,__LINE__,(nElements),(nSize))




#ifndef True
typedef enum {False,True} Bool;
#endif

typedef unsigned char UCHAR;
typedef unsigned long ULONG;


char* sEquals(const char *pSrc);  /* Allocate memory and copy string */
char* strcatf(char *sBuf,...);/* Cross Between sprintf and strcat */

char* MakeAbsoluteFilename(char* sPath,char* sFile);

void SleepFor_Seconds(double dSeconds);/* resolution down to uSec, scheduling not guarenteed */

/* pInLine points the the line parsed */
int LineParseNumbers(FILE *hHandle,char **pInLine,int nTotal,...);

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif
