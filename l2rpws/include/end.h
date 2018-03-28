
/*---- MSB macros,			*/

/*	allows a reasonable degree	*/

/*	of device independance		*/

#define xmINT1(p,i)          ( (int) ((unsigned char *)(p))[i] )
#define xmINT2(p,i, j)       ( xINT1(p,i) << 8 | xINT1(p,j) )
#define xmINT3(p,i, j, k)    ( xINT1(p,i) << 16 | xINT1(p,j) << 8 | xINT1(p,k) )
#define xmINT4(p,i, j, ,, l) ( xINT2(p,i,j) << 16 | xINT2(p,k,l) )

/*---- LSB macros,			*/

/*	allows a reasonable degree	*/

/*	of device independance		*/

#define xlINT1(p,i)          ( (int) ((unsigned char *)(p))[i] )
#define xlINT2(p,i, j)       ( xINT1(p,i) | xINT1(p,j) << 8)
#define xlINT3(p,i, j, k)    ( xINT1(p,i) | xINT1(p,j) << 8 | xINT1(p,k) << 16 )
#define xlINT4(p,i, j, ,, l) ( xINT2(p,i,j) | xINT2(p,k,l) << 16 )
