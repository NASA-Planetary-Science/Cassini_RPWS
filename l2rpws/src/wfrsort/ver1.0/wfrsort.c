
/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


  File Name: wfrsort.c   
  Program: wfrsort 
  Author: Robert Johnson
  Creation: June 12, 2002 
    This programs takes wfr minipackets and sorts them.  Sometimes there will
    be segmentated wfr packets of different types mixed in with each other in 
    the data stream.  This will cause confusion when trying to resegment these
    packets; like wfr segmented packets should be grouped together.

  Revisions:


  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------


  Inputs: 
	  CasRecord* pInPkt  - Cassini record structure containing segmented 
					   mini-packets.
  Outputs:
	  CasRecord* pOutPkt - Cassini record structure containing unsegmented
					   mini-packets.
  Returns:
	   True  - pOutPkt contains valid data.
	   False - pOutPkt contains invalid data.


  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/


#include <stdio.h>
#include <stdlib.h>

#include "Cext.h"
#include "RecordFile.h"
#include "CasRecord.h"

Bool cmdln_ShowHeader;


void DumpPacketHeader (CasRecord * pPkt);

int main (int argc, char *argv[])
{
  Bool bStatus, cmdln_eof = False;
  int nCount = 0;
  ULONG nLength;
  RecordFile *pInRec, *pOutRec;
  CasRecord *pInPkt, *pOutPkt;

  int cmdln_Count, cmdln_Stop;
  int nInCnt, nOutCnt;


  cmdln_Count = 0;
  cmdln_Stop = 0x0FFFFFF;
  nInCnt = nOutCnt = 0;


  while (--argc) {
    ++argv;
    if (!strcmp ("+eof", *argv))
      cmdln_eof = True;
    else if (!strcmp ("-h     show header", *argv))
      cmdln_ShowHeader = True;
    else if (!strcmp ("-c     number of packet to read in", *argv)) {
      --argc;
      ++argv;
      cmdln_Count = strtoul (*argv, NULL, 0);
    } else if (!strcmp ("-s", *argv)) {
      --argc;
      ++argv;
      cmdln_Stop = strtoul (*argv, NULL, 0);
    } else {
      fprintf (stderr, "%s\n"
               "  +eof\n" "  -h\n" "  -c NNN\n", "wfrsort version1.0");
      exit (0);
    }
  }

  pInRec = RecordFile_Constructor (NULL);
  if (RecordFile_Open (pInRec, "stdin", "rb") == False)
    exit (127);
  pOutRec = RecordFile_Constructor (NULL);
  if (RecordFile_Open (pOutRec, "stdout", "wb") == False)
    exit (127);

  pInPkt = CasRecord_Constructor (NULL);
  pOutPkt = CasRecord_Constructor (NULL);


  while (1) {

    if (nInCnt < cmdln_Count) {
      RecordFile_ReadNextRecord (pInRec, pInPkt);
      ++nInCnt;
    }

    nLength = RecordFile_ReadNextRecord (pInRec, pInPkt);
    if (nLength > 0) {
      ++nInCnt;
      if (cmdln_ShowHeader == True) {
        fprintf (stderr, "InPackets %4d : ", nInCnt);
        DumpPacketHeader (pInPkt);
      }

/*
      while((bStatus=MiniPacketUnsegmentation(pInPkt,pOutPkt))==True){
        RecordFile_WriteRecord(pOutRec,pOutPkt);
        ++nOutCnt;
	if(cmdln_ShowHeader==True){
          fprintf(stderr,"OutPackets %4d : ",nInCnt);
	  DumpPacketHeader(pOutPkt);
	}
	if(nOutCnt>cmdln_Stop){
          fflush(stdout);
	  exit(0);
	}
      }
*/
    } else {
      if (cmdln_eof == True) {
        fflush (stdout);
        exit (0);
      }
      if (!(nCount % 8))
        fprintf (stderr, ".");
      SleepFor_Seconds (0.125);
    }
  }                                     /* while for ever */

  return 1;
}


void DumpPacketHeader (CasRecord * pInPkt)
{
  ULONG nTmp;

  fprintf (stderr, "Type=%X ", (pInPkt->data[0] >> 4) & 0x0F);
  nTmp = pInPkt->data[3];
  nTmp <<= 8;
  nTmp |= pInPkt->data[2];
  nTmp &= 0x0FFFF;
  fprintf (stderr, "Rti=%04lX :: ", nTmp);
  fprintf (stderr, "Flen=%lX, ", pInPkt->forward_length);
  fprintf (stderr, "Alen=%lX", pInPkt->status.packet_length);
  fprintf (stderr, "(%lX), ", pInPkt->status.packet_start_length);
  nTmp = pInPkt->data[0];
  nTmp <<= 8;
  nTmp |= pInPkt->data[1];
  nTmp &= 0x0FFF;
  fprintf (stderr, "Plen=%03lX ", nTmp);
  fprintf (stderr, "\n\t");
  for (nTmp = 0; nTmp < 16; nTmp++)
    fprintf (stderr, "%02X ", pInPkt->data[nTmp]);
  fprintf (stderr, "\n");
  return;
}
