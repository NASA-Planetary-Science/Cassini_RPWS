#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>

#include <Cext.h>

#include <rpwstlm/CasRecord.h>
#define CasHfrHeander_C
#include <rpwstlm/CasHfr.h>

/* raj May 12, 2000  :: fix output decompressing counting length */

/* raj April 7, 2003 :: integrate into the new library */

/* raj May 6, 2004   :: fixup error status and packet validation */


#define FRAMESIZE     50
#define SIZE_TABLES   44

#define Cmax(a,b) ((a) > (b) ? (a) : (b))
#define Cmin(a,b) ((a) > (b) ? (b) : (a))

const char *sCasHfrMeanderVersion = "CasHfr_Meander(), version 1.1";
int nMeanderDebug = 0;

/* Won't compile on Linux  */
/* static FILE *hCasHfrMeanderErr = stderr; */
FILE *hCasHfrMeanderErr = NULL;

static char arErrMsg[8192];

/* Default meander decompression tables */
int MeanderTable1[] = {
  0x040f, 0x0508, 0x0511, 0x0000, 0x0514, 0x0502, 0x050b, 0x0000,
  0x040c, 0x0505, 0x050e, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x040d, 0x0506, 0x050f, 0x0000, 0x0512, 0x0500, 0x0509, 0x0000,
  0x0515, 0x0503, 0x050c, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x040e, 0x0507, 0x0510, 0x0000, 0x0513, 0x0501, 0x050a, 0x0000,
  0x040b, 0x0504, 0x050d, 0x0000
};

int MeanderTable2[] = {
  0x0000, 0x0000, 0x040a, 0x0000, 0x0000, 0x0502, 0x050a, 0x0000,
  0x040f, 0x0505, 0x0407, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0506, 0x0408, 0x0000, 0x040b, 0x0500, 0x0508, 0x0000,
  0x040d, 0x0503, 0x050b, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0507, 0x0409, 0x0000, 0x040c, 0x0501, 0x0509, 0x0000,
  0x040e, 0x0504, 0x0406, 0x0000
};

int MeanderTable3[] = {
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0407, 0x0000,
  0x0000, 0x0404, 0x0305, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0400, 0x0000, 0x0000, 0x0306, 0x0405, 0x0000,
  0x0000, 0x0402, 0x0408, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0307, 0x0000, 0x0000, 0x0401, 0x0406, 0x0000,
  0x0000, 0x0403, 0x0409, 0x0000
};

bool bMeanderDebug = false;
bool bMeanderHeader = false;
Ulong nMeanderInCnt = 0;                /* meander compressed packets in */
Ulong nMeanderOutCnt = 0;               /* meander packets decompressed */
Ulong nMeanderErrCnt = 0;               /* decompressed errors */

typedef unsigned short WORD;


static bool bDecompressionFinie, bError;
static UCHAR *pRawData, nBitShift;      /* MeanderGetBits() */
static ULONG nDecompressedDataSize;     /* Predicted Decompressed Data Size */
static ULONG nRawLen;                   /* Data to be decompressed */
static ULONG nInLen, nOutLen;           /* Input and Output data lengths */
static ULONG nValidInLen, nValidOutLen; /* No. of valid data bytes */
static ULONG nErrCnt;                   /* Error count durning decompression */
static Ulong nMeanderStatus;


bool MeanderMakeClasses (int *pDataClass, int nFrameSize);
int MeanderGetBits (int nNumBits);
void MeanderGetCode (int *pCodes, int *pCodeTable);
int MeanderMakeDifference (int nCl, int *pDataClass);
bool MeanderLoadTable (int nTable, FILE * hTable);      /* Under Construction */

Ulong CasHfrMeander_ZeroCheck (CasRecord * p);
void CasHfrMeander_DumpError (char *sErrMsg);
void DumpAncillaryStatus (CasRecord * p, FILE * h);


/* 
Inputs  :: Cassini Record File
Outputs :: Cassini Record File
Returns :: error status of the meander decompression 
  TRUE  - means an error occured during meander decompression
  FALSE - meands no errors occured during meander decompression
  type of errors: 
    error with meander packet 
    error with meander decompression 
*/


static CasRecord *pCurRec;

Ulong CasHfr_Meander (CasRecord * pInRec, CasRecord * pOutRec)
{
  int arDataClass[FRAMESIZE + 1], nPreviousData, nThisData, j;
  UCHAR *pInBuf = pInRec->data, *pOutBuf;
  ULONG nPktLen, nHfrLen, nHeaderLen, nTotal;


  pCurRec = pInRec;                     /* current record for debugging information */
  nMeanderStatus = CasMeander_Processed;        /* made it this far */

  if (CasHfr_bHfrPacket (pInBuf) == false)
    bError = true;                      /* Not an HFR Packet */
  else if (CasHfr_bMeanderCompressed (pInBuf) == false)
    bError = true;                      /* Not meander compressed */
  else
    bError = false;

  if (bError == true) {
    memcpy (pOutRec, pInRec, (size_t) (pInRec->forward_length));        /* dst,src */
    pOutRec->status.gnd_status &= ~CasMeander_Mask;     /* zero out status bits */
    pOutRec->status.gnd_status |= nMeanderStatus;
    return nMeanderStatus;
  }

  ++nMeanderInCnt;                      /* valid/invalid meander compressed packet */
  nMeanderStatus |= CasHfrMeander_nValidPacket (pInRec);        /* sets status bits */

  /*
   * don't decompress if any error except data termination 
   */
  if (nMeanderStatus & ~(CasMeander_Processed | CasMeander_DataTermination)) {
    memcpy (pOutRec, pInRec, (size_t) (pInRec->forward_length));        /* dst,src */
    pOutRec->status.gnd_status &= ~CasMeander_Mask;     /* zero out status bits */
    pOutRec->status.gnd_status |= nMeanderStatus;
    return nMeanderStatus;
  }


  /*
   * Have a valid hfr meander compressed packet 
   */
  /*
   * Copy Ancillary Status and Mini Packet Header 
   */
  memcpy (pOutRec, pInRec, 293);        /* dst,src,4+4+4+256+25 */

  if (CasHfr_bAnalysis (pInBuf) == true)
    nHeaderLen = 25;                    /* data[24]=0xAA */
  else if (CasHfr_bSounder (pInBuf) == true)
    nHeaderLen = 21;                    /* data[20]=0xBB */
  else if (CasHfr_bCalibration (pInBuf) == true)
    nHeaderLen = 12;                    /* data[11]=0xCC */
  else
    nHeaderLen = 12;                    /* millisecond */

  /*
   * Variables used in MeanderGetBits() 
   */
  pRawData = &(pInBuf[nHeaderLen]);     /* Start of Meander Compressed Data */
  nRawLen = 0;                          /* Number of Bytes Decompress so far */
  nBitShift = 0;
  nDecompressedDataSize =
    CasHfr_ByteCount (pInRec, CasHfr_BandAll | CasAntAll);

  /*
   * Meander Compressed Data Length = Total MpLen - Header - 5A 
   */
  nPktLen = pInRec->status.packet_length;
  nInLen = (nPktLen + 3) - nHeaderLen - 1;      /* Total mplen - header - 5A */
  nHfrLen = CasHfr_nPacketSize (pInBuf);        /* Length recorded by HFR */
  pOutBuf = &(pOutRec->data[nHeaderLen]);
  nOutLen = 0;
  nValidInLen = nInLen;
  nValidOutLen = 0;

  /*
   * If cds, mpii, of mpus zero fill the packet, find the start of the zeros 
   */
  nValidInLen = CasHfrMeander_ZeroCheck (pInRec);
  if (nValidInLen == (nPktLen + 3))     /* no zeros found */
    nValidInLen = nValidInLen - nHeaderLen - 1; /* Total mplen - header -1 */
  else                                  /* zeros found */
    nValidInLen = nValidInLen - nHeaderLen;     /* data before zeros - header */


  /*
   * Attempt Meander Decompression 
   */
  ++nMeanderOutCnt;                     /* packets attempted to be decompressed */
  nMeanderStatus |= CasMeander_Decompressed;
  bDecompressionFinie = bError = false;

  while ((bDecompressionFinie == false) && (bError == false)) {
    nPreviousData = MeanderGetBits (8); /* Get the first data byte, uncoded */
    *pOutBuf++ = nPreviousData;
    ++nOutLen;

    if (nOutLen >= nDecompressedDataSize)
      bDecompressionFinie = true;

    if ((bMeanderDebug == true) && (hCasHfrMeanderErr != NULL))
      fprintf (hCasHfrMeanderErr, "\t\t\tnOutLen=%5d  *pOutBuf=%02X\n",
               nOutLen - 1, *(pOutBuf - 1));

    if (bDecompressionFinie == false)
      arDataClass[0] = arDataClass[1] = MeanderGetBits (4);     /* Get the 1st class */

    if (bDecompressionFinie == false)
      bError = MeanderMakeClasses (arDataClass, FRAMESIZE);

    arDataClass[FRAMESIZE] = arDataClass[FRAMESIZE - 1];
    for (j = 1; j < FRAMESIZE; j++) {

      /*
       * Suppress errors and keep on decompress'n 
       * if((bDecompressionFinie==true)
       * break; 
       */
      if ((bDecompressionFinie == true) || (bError == true))
        break;

      nThisData = MeanderMakeDifference (j, arDataClass);
      nThisData += nPreviousData;
      *pOutBuf++ = nThisData;
      ++nOutLen;
      nPreviousData = nThisData;

      if ((bMeanderDebug == true) && (hCasHfrMeanderErr != NULL))
        fprintf (hCasHfrMeanderErr, "\t\t\t\t\tnOutLen=%5d  *pOutBuf=%02X\n",
                 nOutLen - 1, *(pOutBuf - 1));

      if (nOutLen >= nDecompressedDataSize)
        bDecompressionFinie = true;

    }                                   /* for j<FRAMESIZE */

  }                                     /* while meander decompressing */


  /*
   * Length checks for decompression success 
   */
  if (nOutLen != nDecompressedDataSize) {
    int i;

    bError = true;
    if (nDecompressedDataSize > (20 * 1024)) {
      nDecompressedDataSize = (20 * 1024);
    }

    nValidOutLen += nHeaderLen;         /* where zero fill begins in packet */
    for (i = nValidOutLen; i < nDecompressedDataSize + nHeaderLen; i++)
      pOutRec->data[i] = 0x00;
    nOutLen = nDecompressedDataSize;    /* for len calculation below */
    nMeanderStatus |= CasMeander_ZeroFilled;
    pOutRec->status.cmprs_status = 0x07000000;
    pOutRec->status.cmprs_status |= (0x00FFFFFF & nValidOutLen);
  }

  /*
   * Fix up the length bytes and header compress bits 
   */
  nTotal = nHeaderLen + nOutLen + 1;    /* Total Mini Packet Length */
  pOutRec->forward_length = 12 + 256 + nTotal;
  pOutRec->status.packet_length = nTotal - 3;
  pOutRec->data[0] &= 0xF0;
  pOutRec->data[1] &= 0x00;             /* Zero mp length */
  if ((nTotal - 3) < 0x1000) {          /* Write length in MiniPacket */
    pOutRec->data[0] |= (nTotal - 3) >> 8;
    pOutRec->data[1] = (nTotal - 3) & 0x0FF;
  }
  /*
   * Hfr Length 
   */
  pOutRec->data[5] = pOutRec->data[6] = 0;
  pOutRec->data[5] = ((nTotal - 7) >> 8) & 0x0FF;
  pOutRec->data[6] = (nTotal - 7) & 0x0FF;
  pOutRec->data[8] &= 0x8F;             /* Zero out bits 6-4 => not meander compressed */
  pOutRec->data[nTotal - 1] = 0x5A;     /* Terminate Packet with a 5A */

  pOutRec->status.gnd_status &= ~CasMeander_Mask;       /* zero out status bits */
  pOutRec->status.gnd_status |= nMeanderStatus;


  /*
   * Output any decompression errors for Terry and Willy 
   */
  if ((bError == true) && (nMeanderDebug > 1)) {
    int i;

    fprintf (hCasHfrMeanderErr,
             "CasHfr_Meander():: %d erro%s durning meander "
             "decompression\n", nErrCnt, nErrCnt > 1 ? "rs" : "r");
    fprintf (hCasHfrMeanderErr,
             "   cds=%08X, sclk=%08X %08X, scet=%08X "
             "%08X, rti=%04X\n", pInRec->status.cds_time,
             pInRec->status.chdo_sclk[0], pInRec->status.chdo_sclk[1],
             pInRec->status.chdo_scet[0], pInRec->status.chdo_scet[1],
             CasHfr_RTI (pInRec->data));

    fprintf (hCasHfrMeanderErr, "   bDecompressionFinie=%s, bError=%s\n",
             bDecompressionFinie == true ? "True" : "False",
             bError == true ? "True" : "False");

    fprintf (hCasHfrMeanderErr,
             "   bytes left to decompress %d, %d out of %d "
             "decompressed\n", nInLen - nRawLen, nRawLen, nInLen);
    for (i = 0; i < ((int) nInLen - (int) nRawLen) + 1; i++) {
      if (!(i % 16))
        fprintf (hCasHfrMeanderErr, "     ");
      fprintf (hCasHfrMeanderErr, "%02X ", pRawData[i]);
      if (!((i + 1) % 16))
        fprintf (hCasHfrMeanderErr, "\n");
    }
    if ((i + 1) % 16)
      fprintf (hCasHfrMeanderErr, "\n");
    fprintf (hCasHfrMeanderErr, "   nRawLen=%d  *pRawData=%02X  nWord=%04X  "
             "nBitShift=%d\n", nRawLen, *pRawData,
             (WORD) ((((*pRawData) & 0xFF) << 8) +
                     ((*(pRawData + 1)) & 0x00FF)), nBitShift);

    fprintf (hCasHfrMeanderErr, "   In  Total = Header + Data + 5A, "
             "%d = %d + %d + 1, Hfr=%d\n",
             nPktLen + 3, nHeaderLen, nInLen, nHfrLen);
    fprintf (hCasHfrMeanderErr, "   Out Total = Header + Data + 5A, "
             "%d = %d + %d + 1, Hfr=%d\n",
             nHeaderLen + nOutLen + 1, nHeaderLen, nOutLen,
             CasHfr_nPacketSize (pOutRec->data));
    fprintf (hCasHfrMeanderErr, "   Exp Total = Header + Data + 5A, "
             "%d = %d + %d + 1\n",
             nHeaderLen + nDecompressedDataSize + 1,
             nHeaderLen, nDecompressedDataSize);

    fprintf (hCasHfrMeanderErr, "%s\n\n\n",
             CasHfrAnalysis_sByteCount (pInRec));
  }
  /*
   * if output decompression errors 
   */
  if (bError == true)
    ++nMeanderErrCnt;

  return nMeanderStatus;
}



int MeanderGetBits (int nNumBits)
{
  WORD i, nWord, nMask, nBits;


  if (nNumBits > 8) {                   /* 0 <= nNumBits <= 8 */
    bError = true;
    ++nErrCnt;
    nMeanderStatus |= CasMeander_GetBitsError;

    if (nMeanderDebug) {
      sprintf (arErrMsg, "GetBits(), error @ byte %d/%d, %d decomp\n",
               nRawLen, nInLen, nOutLen);
      CasHfrMeander_DumpError (arErrMsg);
    }

    return 0;
  }

  /*
   * Start of new compression data set 
   */
  if (bMeanderDebug == true && nRawLen == 0 && nBitShift == 0) {
    nWord =
      (WORD) ((((*pRawData) & 0xFF) << 8) + ((*(pRawData + 1)) & 0x00FF));
    if (hCasHfrMeanderErr != NULL)
      fprintf (hCasHfrMeanderErr,
               "nRawLen=%5d  *pRawData=%02X  nWord=%04X\n", nRawLen,
               *pRawData, nWord);
  }

  /*
   * Test for end of compression. 
   * nRawLen is an index starting at 0, nInLen is a length.  If nRawLen=nInLen,
   * the current byte should be '5a', end of packet.
   */
  if (nRawLen >= nInLen) {
    nMeanderStatus |= CasMeander_BufferOverRun;
    bDecompressionFinie = true;
    return 0;
  }
  if (nOutLen >= nDecompressedDataSize) {
    bDecompressionFinie = true;
    return 0;
  }
  if (nRawLen >= nValidInLen) {
    nMeanderStatus |= CasMeander_ZeroFilled;
    bDecompressionFinie = true;
    return 0;
  }
  nValidOutLen = nOutLen;

  nWord = (WORD) ((((*pRawData) & 0xFF) << 8) + ((*(pRawData + 1)) & 0x00FF));

  for (i = nMask = 0; i < nNumBits; nMask |= (WORD) (1 << i), i++);     /* Make the mask */
  nMask <<= (16 - nNumBits - nBitShift);
  nBits = (WORD) ((nWord & nMask) >> (16 - nNumBits - nBitShift));
  nBitShift += (WORD) nNumBits;
  if (nBitShift >= 8) {
    nBitShift -= (WORD) 8;
    pRawData++;
    nRawLen++;

    if (bMeanderDebug == true) {
      nWord =
        (WORD) ((((*pRawData) & 0xFF) << 8) + ((*(pRawData + 1)) & 0x00FF));
      if (hCasHfrMeanderErr != NULL)
        fprintf (hCasHfrMeanderErr,
                 "nRawLen=%5d  *pRawData=%02X  nWord=%04X\n", nRawLen,
                 *pRawData, nWord);
    }

  }
  /*
   * if nBitShift>=8 
   */
  return nBits;
}



bool MeanderMakeClasses (int *pDataClass, int nFrameSize)
{
  int *locMeanderTable1 = MeanderTable1;
  int *locMeanderTable2 = MeanderTable2;
  int *locMeanderTable3 = MeanderTable3;
  int i, arCdiff[3];

/* Meander Tables are not modified, no need to recopy 
int locMeanderTable1[SIZE_TABLES];
int locMeanderTable2[SIZE_TABLES];
int locMeanderTable3[SIZE_TABLES];

  for(i=0;i<SIZE_TABLES;i++){
    locMeanderTable1[i]=MeanderTable1[i];
    locMeanderTable2[i]=MeanderTable2[i];
    locMeanderTable3[i]=MeanderTable3[i];
  }
*/

  for (i = 2; i < nFrameSize - 2; i += 3) {

    if (pDataClass[i - 1] == 0)
      MeanderGetCode (arCdiff, locMeanderTable3);
    else if (pDataClass[i - 1] == 1)
      MeanderGetCode (arCdiff, locMeanderTable2);
    else if (pDataClass[i - 1] >= 2)
      MeanderGetCode (arCdiff, locMeanderTable1);

/* 
    else
      assert(0);
*/

    pDataClass[i] = pDataClass[i - 1] + arCdiff[0];
    pDataClass[i + 1] = pDataClass[i] + arCdiff[1];
    pDataClass[i + 2] = pDataClass[i + 1] + arCdiff[2];

    if ((pDataClass[i] < 0) || (pDataClass[i + 1] < 0)
        || (pDataClass[i + 2] < 0)) {
      bError = true;
      ++nErrCnt;
      nMeanderStatus |= CasMeander_MakeClassError;

      if (nMeanderDebug) {
        sprintf (arErrMsg,
                 "MakeClasses(), error @ byte %d/%d, %d decomp\n", nRawLen,
                 nInLen, nOutLen);
        CasHfrMeander_DumpError (arErrMsg);
      }

      return true;
    }

  }

  return false;
}


void MeanderGetCode (int *pCodes, int *pCodeTable)
{
  UCHAR nOffset = 0, nInBitCount = 0, nBits, nThisCode;
  int nFound = 0, nCode = 0, nInter, *pStartOfTable;

  pCodes[0] = pCodes[1] = pCodes[2] = 0;

  pStartOfTable = pCodeTable;
  do {
    nCode |= MeanderGetBits (1);
    nInBitCount++;
    nOffset = 0x00;
    do {
      nInter = *pCodeTable++;
      nBits = (WORD) ((nInter & 0x0FF00) >> 8); /* bug??? */
      nThisCode = (WORD) (nInter & 0x00FF);
      if ((nBits == nInBitCount) && (nThisCode == nCode))
        nFound = 1;
      else
        nOffset++;
    } while ((nOffset < SIZE_TABLES) && (!nFound));
    nCode <<= 1;
    pCodeTable = pStartOfTable;
  } while ((nInBitCount < 10) && (!nFound));

  if (!nFound) {
    if (hCasHfrMeanderErr != NULL)
      fprintf (hCasHfrMeanderErr, "MeanderGetCode():: code not found\n");
  }

  pCodes[0] = (nOffset & 0x03) - 1;
  pCodes[1] = ((nOffset & 0x0C) >> 2) - 1;
  pCodes[2] = ((nOffset & 0x30) >> 4) - 1;

  return;
}




int MeanderMakeDifference (int nCl, int *pDataClass)
{
  int nPc, nC, nNc;                     /* previous class, this class, next class */
  int nData = 0;

  nPc = pDataClass[nCl - 1];
  nC = pDataClass[nCl];
  nNc = pDataClass[nCl + 1];

  if ((nC >= nPc) && (nC >= nNc)) {     /* Maxium local */
    if (nC == 0)
      nData = 0;
    else {
      nData = MeanderGetBits (Cmax (1, nC - 1));
      if (nC > 1)
        nData =
          (nData <
           (1 << (nC - 2)) ? nData - (1 << (nC - 1)) : nData + Cmin (1,
                                                                     nC - 1));
      else
        nData = (nData == 0 ? -1 : 1);
    }
  } else {                              /* incertain class */
    if (nC == 0)
      nData = 0;
    else {
      nData = MeanderGetBits (nC);
      if (nData == (1 << nC) - 1) {
        if (nC <= 1)
          nData = (MeanderGetBits (1) == 0 ? -1 : 1);
        else
          nData =
            (MeanderGetBits (1) == 0 ? -(1 << (nC - 1)) : 1 << (nC - 1));
      } else {
        if (floor ((float) nData / 2) == (float) nData / 2)     /* data pair (data even) */
          nData = (nC >= 1 ? -nData / 2 : -(nData + 2) / 2);
        else                            /* data impair (data odd) */
          nData = (nC >= 1 ? (nData + 1) / 2 : (nData + 3) / 2);
      }
    }
  }

  return nData;
}



bool MeanderLoadTable (int nTable, FILE * hTable)
{
  bool bStatus = false;

  fprintf (stderr, "Not yet implemented\n");
  assert (0);

  return bStatus;
}




void DumpAncillaryStatus (CasRecord * p, FILE * h)
{
  Ulong nRecLen, nPktLen, nMpLen, nHfrLen;

  if (h == NULL)
    return;

  nRecLen = p->forward_length;
  nPktLen = p->status.packet_length;
  nMpLen = ((p->data[0] << 8) + p->data[1]) & 0x0FFF;
  nHfrLen = CasHfr_nPacketSize (p->data);

  fprintf (h,
           "RecLen=%d(0x%X),PktLen=%d(0x%X),MpLen=%d(0x%X),HfrLen=%d(0x%X)\n",
           nRecLen, nRecLen, nPktLen, nPktLen, nMpLen, nMpLen, nHfrLen,
           nHfrLen);
  fprintf (h, "  cds=%08X,sclk=%08X %08X,scet=%08X %08X, RTI=%04X\n",
           p->status.cds_time, p->status.chdo_sclk[0], p->status.chdo_sclk[1],
           p->status.chdo_scet[0], p->status.chdo_scet[1],
           CasHfr_RTI (p->data));

  return;
}



Ulong CasHfrMeander_nValidPacket (CasRecord * pIn)
{
  bool bFirstError = false;
  char *pMsg;
  ULONG nPktLen, nMpLen, nHfrLen, nStatus;



  pMsg = arErrMsg;
  nStatus = 0;

  if (CasHfr_bHfrPacket (pIn->data) == false)
    return nStatus;                     /* Not an HFR Packet */

  /*
   * common header area check 
   */
  if (CasHfr_nHeaderVersion (pIn->data) != 0x05) {
    pMsg += sprintf (pMsg, "Header Version=%X\n",
                     CasHfr_nHeaderVersion (pIn->data));
    nStatus |= CasMeander_HeaderErrors;
  } else if (CasHfr_bErrorFlag (pIn->data) == true) {   /* same as Memory Dump */
    pMsg += sprintf (pMsg, "Error Flag, Memory Overrun\n");
    nStatus |= CasMeander_HeaderErrors;
  } else if (CasHfr_bEOF (pIn->data) == false) {
    pMsg += sprintf (pMsg, "Missing EOF Bit\n");
    nStatus |= CasMeander_HeaderErrors;
  } else
    nStatus = 0;

  if ((nStatus != 0) && (bFirstError == false)) {
    bFirstError = true;                 /* only report the first error */
    if (nMeanderDebug)
      CasHfrMeander_DumpError (arErrMsg);
  }



  /*
   * Header Termination Checks 
   */
  if (CasHfr_bAnalysis (pIn->data) == true) {   /* data[24]=0xAA, EOP=0x5A */
    if (pIn->data[24] != 0xAA) {
      pMsg += sprintf (pMsg, "Analysis Header Missing 0xAA\n");
      nStatus |= CasMeander_HeaderErrors;
    }
  } else if (CasHfr_bSounder (pIn->data) == true) {     /* data[20]=0xBB, EOP=0x5A */
    if (pIn->data[20] != 0xBB) {
      pMsg += sprintf (pMsg, "Sounder Header Missing 0xBB\n");
      nStatus |= CasMeander_HeaderErrors;
    }
  } else if (CasHfr_bCalibration (pIn->data) == true) { /* data[11]=0xCC, EOP=0x5A */
    if (pIn->data[11] != 0xCC) {
      pMsg += sprintf (pMsg, "Calibration Header Missing 0xCC\n");
      nStatus |= CasMeander_HeaderErrors;
    }
  } else if (CasHfr_bMillisecond (pIn->data) == true) { /* data[11]=rcvr, EOP=0x5A */
    if (pIn->data[11] & 0xFE) {         /* 7->1 spare; bit 0; 0=HF1, 1=HF2 */
      pMsg += sprintf (pMsg, "Millisecond Spare Bits Set %02X\n",
                       pIn->data[11]);
      nStatus |= CasMeander_HeaderErrors;
    }
  } else {
    pMsg += sprintf (pMsg, "Unknown Hfr Packet Type\n");
  }

  if ((nStatus != 0) && (bFirstError == false)) {
    bFirstError = true;                 /* only report the first error */
    if (nMeanderDebug)
      CasHfrMeander_DumpError (arErrMsg);
  }


  /*
   * Basic Length Integerty Checks 
   */
  nPktLen = pIn->status.packet_length;
  nMpLen = (pIn->data[0] << 8 | pIn->data[1]) & 0x0FFF;
  nHfrLen = CasHfr_nPacketSize (pIn->data);
  /*
   * InRec->forward_length == (nPktLen+3 + 4+4+4+256) 
   * This check fails periodically because some Records are forced to
   * be aligned on 32 bit boundaries.
   */
  if ((nPktLen != nMpLen) && (nPktLen < 0x1000)) {
    pMsg +=
      sprintf (pMsg, "MpLen=%d and Ancillary Len=%d\n", nMpLen, nPktLen);
    nStatus |= CasMeander_HeaderErrors;
  }
  if (nPktLen != (nHfrLen + 4)) {
    pMsg +=
      sprintf (pMsg, "Ancillary Len=%d and Hfr Len=%d\n", nPktLen, nHfrLen);
    nStatus |= CasMeander_HeaderErrors;
  }

  if ((nStatus != 0) && (bFirstError == false)) {
    bFirstError = true;                 /* only report the first error */
    if (nMeanderDebug)
      CasHfrMeander_DumpError (arErrMsg);
  }


  /*
   * Data Termination Checks, 0x5A, packet length should be valid 
   */
  if (pIn->data[nPktLen + 2] != 0x5A) {
    pMsg += sprintf (pMsg, "Data Not Terminated with 0x5A\n");
    nStatus |= CasMeander_DataTermination;
  }

  if ((nStatus != 0) && (bFirstError == false)) {
    bFirstError = true;                 /* only report the first error */
    if (nMeanderDebug)
      CasHfrMeander_DumpError (arErrMsg);
  }


  if ((nStatus != 0) && (nMeanderDebug > 1)) {
    int i;

    fprintf (hCasHfrMeanderErr, "   Header Bytes\n");
    for (i = 0; i < 32; i++) {
      if (!(i % 16))
        fprintf (hCasHfrMeanderErr, "     ");
      fprintf (hCasHfrMeanderErr, "%02X ", pIn->data[i]);
      if (!((i + 1) % 16))
        fprintf (hCasHfrMeanderErr, "\n");
    }
  }



  return nStatus;
}



void CasHfrMeander_DumpError (char *sErrMsg)
{
  Ulong nRti;

  if (hCasHfrMeanderErr == NULL)
    return;

  nRti = pCurRec->data[3];
  nRti <<= 8;
  nRti |= pCurRec->data[2];

  fprintf (hCasHfrMeanderErr, "HFR %08X.%04X 0x%X %s",
           pCurRec->status.cds_time, nRti, pCurRec->status.packet_length,
           sErrMsg);

  return;
}



char *CasHfrMeander_DecodeGndStatus (Ulong nStatus)
{
  char *pMsg;
  static char sMsg[256];

  sMsg[0] = '\0';                       /* initial condition */
  pMsg = sMsg;

  if (nStatus & CasMeander_Decompressed)
    pMsg += sprintf (pMsg, "Decomp...");

  if (nStatus & CasMeander_BufferOverRun)
    pMsg += sprintf (pMsg, "Buffer Over Run,");

  if (nStatus & CasMeander_MakeClassError)
    pMsg += sprintf (pMsg, "MakeClass(),");

  if (nStatus & CasMeander_GetBitsError)
    pMsg += sprintf (pMsg, "GetBits(),");

  if (nStatus & CasMeander_HeaderErrors)
    pMsg += sprintf (pMsg, "Header Errors,");

  if (nStatus & CasMeander_ZeroFilled)
    pMsg += sprintf (pMsg, "Zero Filled,");

  if (nStatus & CasMeander_DataTermination)
    pMsg += sprintf (pMsg, "Data N.T.");

  return sMsg;
}



Ulong CasHfrMeander_ZeroCheck (CasRecord * pRec)
{
  Uchar *pBuf = pRec->data;
  int i, nHdrLen, nPktLen, nValidCnt, nZeroCnt, nZeroThold;
  Ulong nStatus;



  nPktLen = pRec->status.packet_length + 3;
  nStatus = pRec->status.gnd_status;

  if (CasHfr_bAnalysis (pBuf) == true)
    nHdrLen = 25;                       /* data[24]=0xAA, EOP=0x5A */
  else if (CasHfr_bSounder (pBuf) == true)
    nHdrLen = 21;                       /* data[20]=0xBB, EOP=0x5A */
  else if (CasHfr_bCalibration (pBuf) == true)
    nHdrLen = 12;                       /*data[11]=0xCC,EOP=0x5A */
  else if (CasHfr_bMillisecond (pBuf) == true)
    nHdrLen = 12;                       /*data[11]=rcvr,EOP=0x5A */


  /*
   * hfr meander compressed packets may have 5-6 zero bytes at the end.  If
   * the data termination byte is missing, its a good indication of that the
   * zero fill was attached at the end => scan to end of the packet 
   */

  if ((pBuf[nPktLen - 1] != 0x5A) || (nStatus & CasMpus_ErrorMask)) {
    nZeroThold = 3;
    while (nZeroThold > 0) {
      nZeroCnt = 0;
      for (i = nHdrLen; i < nPktLen; i++) {
        if (pBuf[i] == 0x00)
          ++nZeroCnt;
        else
          nZeroCnt = 0;
        if (nZeroCnt > nZeroThold)
          break;
      }

      if (i != nPktLen)                 /* found zeros */
        break;

      --nZeroThold;
    }                                   /* elihw */
    if (i == nPktLen)                   /* no zeros found */
      nValidCnt = nPktLen;
    else
      nValidCnt = i - nZeroCnt + 1;     /* number of valid data bytes */

    if (nMeanderDebug) {
      if (nValidCnt == nPktLen)
        fprintf (stderr, "  Mpus/5A Failed to Find Any Zeros\n");
      else
        fprintf (stderr, "  Mpus/5A Found -> zeros starting at %d, 0x%X\n",
                 nValidCnt, nValidCnt);
    }
  } else {
    if (nStatus & CasMpii_ErrorMask)
      nZeroThold = 8;                   /* cds/mpii zero fill maybe */
    else
      nZeroThold = 8;                   /* */

    nZeroCnt = 0;
    for (i = nHdrLen; i < (nPktLen - 16); i++) {
      if (pBuf[i] == 0x00)
        ++nZeroCnt;
      else
        nZeroCnt = 0;
      if (nZeroCnt > nZeroThold)
        break;
    }

    if (i == (nPktLen - 16))            /* no zeros found */
      nValidCnt = nPktLen;
    else
      nValidCnt = i - nZeroCnt + 1;     /* number of valid data bytes */

    if (nMeanderDebug) {
      if (nValidCnt == nPktLen)
        fprintf (stderr, "  cds/mpii Failed to Find Any Zeros\n");
      else
        fprintf (stderr, "  cds/mpii Found -> zeros starting at %d, 0x%X\n",
                 nValidCnt, nValidCnt);
    }

  }                                     /* esle */



  return nValidCnt;
}
