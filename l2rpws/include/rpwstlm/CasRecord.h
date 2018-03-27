
#ifndef _CasRecord_h
#define _CasRecord_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <Cext.h>

/* CDS_fill_flag */

#define CAS_RECORD_MAX_SIZE 65536

  typedef struct cas_record_tag
  {
	 /* Switched from long to in to keep items at 4 bytes each on 64-bit arch */
    int forward_length, fill0, fill1;  /*  12 bytes */
    struct
    {                                   /* 256 bytes */
      ULONG cds_time, filler_cds_time;  /*   8 bytes */
      UCHAR fill_32[24];                /*  32 total */
      ULONG cmprs_status;               /*  36 total byte 0=method 1-3=length */
      UCHAR fill_45[9];                 /*  45 total */
      UCHAR cds_fill;                   /*  46 total */
      UCHAR fill_48[2];                 /*  48 total */
      ULONG packet_start_length;        /*  52 total */
      ULONG packet_length;              /*  56 total */
      UCHAR fill_204[148];              /* 204 total */
      ULONG gnd_status;                 /* 208 total ground processing status */
      UCHAR fill_224[16];               /* 224 total */
      ULONG chdo_sclk[2], chdo_scet[2]; /* 240 total */
      ULONG chdo_ert[2], chdo_rct[2];   /* 256 total */
    } status;
    UCHAR data[CAS_RECORD_MAX_SIZE];
  } CasRecord;



/*
  June 24, 2004
    cds_fill   0x08 - cds fill mabey; 0x04 - mpii fill 
      0x01 - 1st cds record contains fill - not implemented
      0x02 - 2nd cds record contains fill - not implemented
      0x04 - mpii fill, missing cds records
      0x08 - cds fill maybe, cds record contains fill but reconstructed mini 
               packet may not contain fill.
*/



  typedef struct cas_mp_record_tag
  {
    Ulong forward_length, fill0, fill1; /*  12 bytes */
    struct
    {                                   /* 256 bytes */
      Ulong cds_time, filler_cds_time;  /*   8 bytes */
      Uchar fill0[40];                  /*  48 total */
      Ulong packet_start_length;        /*  52 total */
      Ulong packet_length;              /*  56 total */
      Uchar fill_204[148];              /* 204 total */
      Ulong mpus_status;                /* 208 total */
      Uchar fill_224[16];               /* 224 total */
      Ulong chdo_sclk[2], chdo_scet[2]; /* 240 total */
      Ulong chdo_ert[2], chdo_rct[2];   /* 256 total */
    } status;
    Uchar *data;
  } CasMpRecord;


  CasRecord *CasRecord_Constructor (CasRecord * pSrc);
  bool CasRecord_Initialize (CasRecord * pObj, CasRecord * pSrc);
  bool CasRecord_Copy (CasRecord * pObj, CasRecord * pSrc);
  void CasRecord_Destructor (CasRecord * pObj);






  typedef struct cas_rec_filter_tag
  {
    ULONG nType;
    ULONG nSCLKBegin, nSCLKEnd;
    ULONG nCount, nMaxCount;
    ULONG nTmpCount, nSkipCount;
  } CasRecFilter;


  CasRecFilter *CasRecFilter_Constructor (CasRecFilter * pSrc);
  void CasRecFilter_Destructor (CasRecFilter * pObj);
  bool CasRecFilter_Initialize (CasRecFilter * pObj, CasRecFilter * pSrc);
  bool CasRecFilter_Copy (CasRecFilter * pDst, CasRecFilter * pSrc);
  bool CasRecFilter_Filter (CasRecFilter * pObj, CasRecord * pRec);
  void CasRecFilter_Dump (CasRecFilter * pObj, FILE * hHandle);




#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* end no double includes */
