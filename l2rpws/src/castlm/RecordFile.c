#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>


#include <rpwstlm/CasRecord.h>
#include <rpwstlm/RecordFile.h>

/*

			Record File Format (binary file)
  byte0 byte1 byte2 byte3	forward length	= N in bytes
  byte4 ... byteN-1		data
  byteN byteN+1 byteN+2 byteN+3	reverse length	= N in bytes


*/


RecordFile *RecordFile_Constructor (RecordFile * pSrc)
{
  RecordFile *pObj;

  if ((pObj = (RecordFile *) calloc (1, sizeof (RecordFile))) == NULL)
    fprintf (stderr, "calloc() failed in RecordFile_Constructor()\n");
  else if (RecordFile_Initialize (pObj, pSrc) == false) {
    RecordFile_Destructor (pObj);
    pObj = NULL;
  }

  return pObj;
}



void RecordFile_Destructor (RecordFile * pObj)
{
  RecordFile_Close (pObj);
  free (pObj->filename);
  free (pObj->type);
  free (pObj);
  return;
}



bool RecordFile_Initialize (RecordFile * pObj, RecordFile * pSrc)
{
  bool bStatus = true;

  if (pSrc != NULL) {
    bStatus = RecordFile_Copy (pObj, pSrc);
  } else {
    pObj->filename = NULL;
    pObj->type = NULL;
    pObj->status = RECORD_FILE_CLOSED;
    pObj->position = 0;
    pObj->handle = NULL;
  }

  return bStatus;
}



bool RecordFile_Copy (RecordFile * pDst, RecordFile * pSrc)
{
  bool bStatus = true;

  if (RecordFile_Open (pDst, pSrc->filename, pSrc->type) != true) {
    fprintf (stderr, "RecordFile_Copy() fail\n");
    bStatus = false;
  } else {
    pDst->position = pSrc->position;
    pDst->status = pSrc->status;
    fseek (pDst->handle, pDst->position, SEEK_SET);
  }

  return bStatus;
}


/* 
Allocated Variables: sFilename,sType
Initialized Variables:  hHandle,nPosition,nStatus
*/
bool RecordFile_Open (RecordFile * pObj, const char *sFilename,
                      const char *sType)
{
  if (pObj->handle != NULL) {           /* There is an open file */
    fprintf (stderr, "RecordFile_Open(), File Already Opened.handle=%p\n",
             (void*)pObj->handle);
    return false;
  }

  if ((pObj->filename = sEquals (sFilename)) == NULL) {

/*
fprintf(stderr,"RecordFile_Open() failed.sFilename=%s\n",sFilename);
*/
    return false;
  }
  if ((pObj->type = sEquals (sType)) == NULL) {

/*
fprintf(stderr,"RecordFile_Open() failed, %s\n",sType);
*/
    return false;
  }

  if (!strcmp (pObj->filename, "stdin"))
    pObj->handle = stdin;
  else if (!strcmp (pObj->filename, "stdout"))
    pObj->handle = stdout;
  else if (!strcmp (pObj->filename, "stderr"))
    pObj->handle = stderr;
  else {
    if ((pObj->handle = fopen (pObj->filename, pObj->type)) == NULL) {

/*
fprintf(stderr,"RecordFile_Open() failed, Unable to open file %s",pObj->filename);
*/
      return false;
    }
  }

  pObj->position = ftell (pObj->handle);        /* Place Marker */
  pObj->status = RECORD_FILE_OPEN;

#ifdef CasTlmDebug
  if (CasTlmDebugLevel & CasTlmDebug_FileActivity)
    fprintf (CasTlmDebugHandle, "RecordFile_Open(%s,%s)\n", pObj->filename,
             pObj->type);
#endif

  return true;
}



bool RecordFile_Close (RecordFile * pObj)
{
  bool bStatus = true;

  if (pObj->filename == NULL)
    return bStatus;

  if (strcmp (pObj->filename, "stdin") && strcmp (pObj->filename, "stdout") &&
      strcmp (pObj->filename, "stderr") && (pObj->handle != NULL)) {
    if (fclose (pObj->handle) != 0) {
      fprintf (stderr, "RecordFile_Close, Unable to close file %s.",
               pObj->filename);
      bStatus = false;
    }
  }
  pObj->handle = NULL;
  pObj->status = RECORD_FILE_CLOSED;

#ifdef CasTlmDebug
  if (CasTlmDebugLevel & CasTlmDebug_FileActivity)
    fprintf (CasTlmDebugHandle, "RecordFile_Close(%s,%s)\n", pObj->filename,
             pObj->type);
#endif

  return bStatus;
}



int32_t RecordFile_ReadPreviousRecord (RecordFile * rf, void *pv)
{
  int32_t length;

  if (rf == NULL || pv == NULL)
    return 0;

  RecordFile_MoveToPreviousRecord (rf); /* Move to Beginning of Prevous Record */
  if (rf->status != RECORD_SEEK_SUCCESS)
    length = 0;
  else
    length = RecordFile_ReadNextRecord (rf, pv);

  return length;
}



int32_t RecordFile_ReadCurrentRecord (RecordFile * rf, void *pv)
{
  int32_t length;

  if (rf == NULL || pv == NULL)
    return 0;

  length = RecordFile_MoveToCurrentRecord (rf); /* Move to Beginning of Current Record */
  if (rf->status != RECORD_SEEK_SUCCESS)
    length = 0;
  else
    length = RecordFile_ReadNextRecord (rf, pv);

  return length;
}



int32_t RecordFile_ReadNextRecord (RecordFile * rf, void *pv)
{
	int32_t *buffer = (int32_t *) pv;
	int32_t length;

	if (rf == NULL || pv == NULL) {
		fprintf (stderr, "ReadNextRecord, rf | pv is NULL\n");
		exit (127);
		return 0;
	}

	/* Get the length */
	if(rf->handle == NULL){
		rf->status = RECORD_FILE_CLOSED;
		length = 0;
	}
	else {
		rf->position = ftell (rf->handle);
		if (1 != fread (buffer, 4, 1, rf->handle)) {
			fseek (rf->handle, rf->position, SEEK_SET);
			rf->status = RECORD_EOF;
			length = 0;
		}
	}
	
	/* For now, handle baloney lengths as being at the end of the file since I
	   don't know of any sync patterns for Cassini RPWS GDS files. -cwp */
	if( *buffer > (CAS_RECORD_MAX_SIZE + 256 + 12) ){
		fprintf(stderr, "ERROR: Impossible record size %d\n", *buffer);
		fseek (rf->handle, rf->position, SEEK_SET);
		rf->status = RECORD_EOF;
		length = 0;
		return length;
	}
	
	if(1 != fread((buffer + 1), (unsigned) (*buffer), 1, rf->handle)) {
		fseek (rf->handle, rf->position, SEEK_SET);
		rf->status = RECORD_EOF;
		length = 0;
	}
	else {
		rf->status = RECORD_READ_SUCCESS;
		length = *buffer;
	}

  return length;
}



int32_t RecordFile_MoveToPreviousRecord (RecordFile * rf)
{
  int32_t nLength;

  if (rf == NULL)
    return 0;

  if ((nLength = RecordFile_MoveToCurrentRecord (rf)) != 0)
    nLength = RecordFile_MoveToCurrentRecord (rf);

  return nLength;
}



int32_t RecordFile_MoveToCurrentRecord (RecordFile * rf)
{
  int32_t length; 
  int64_t total;

  if (rf == NULL)
    return 0;

  rf->position = ftell (rf->handle);

  if ((total = rf->position - 4) < 0) { /* Check for previous rlength */
    rf->status = RECORD_BOF;
    length = 0;
  } else {
    fseek (rf->handle, (rf->position - 4), SEEK_SET);   /* (-4),SEEK_CUR */
    if (1 != fread ((&length), 4, 1, rf->handle)) {     /* reverse length */
      fseek (rf->handle, rf->position, SEEK_SET);
      rf->status = RECORD_READ_ERROR;
      length = 0;
    } else if ((total = rf->position - (length + 4)) < 0) {     /* Beginning of Current Record */
      fseek (rf->handle, rf->position, SEEK_SET);
      rf->status = RECORD_BOF;
      length = 0;
    } else {
      fseek (rf->handle, total, SEEK_SET);
      rf->position = ftell (rf->handle);        /* Should be where we began */
      rf->status = RECORD_SEEK_SUCCESS;
    }
  }

  return length;
}



int32_t RecordFile_MoveToNextRecord (RecordFile * rf)
{
  int32_t length;

  if (rf == NULL)
    return 0;

  rf->position = ftell (rf->handle);

  if (1 != fread ((&length), 4, 1, rf->handle)) {       /* forward length */
    fseek (rf->handle, rf->position, SEEK_SET);
    rf->status = RECORD_EOF;
    length = 0;
  } else {
    fseek (rf->handle, (rf->position + length), SEEK_SET);
    rf->status = RECORD_SEEK_SUCCESS;
  }

  return length;
}


 
int32_t RecordFile_WriteRecord(RecordFile * pObj, void *pBuf)
{
  size_t nStatus;
  int32_t nLength;

  pObj->position = ftell (pObj->handle);

  nLength = *((int32_t *) pBuf);           /* first 4 bytes is out length */
  
  /* This is why you don't use signed values for lengths */
  if(nLength <= 0){
    fprintf(stderr, "RecordFile_WriteRecord() bad input packet, has length < 1");
	 return 0;
  }
  
  if ((nStatus = fwrite (pBuf, (size_t)nLength, 1, pObj->handle)) != 1) {
    pObj->status = RECORD_WRITE_ERROR;
    fseek (pObj->handle, pObj->position, SEEK_SET);
    fprintf (stderr, "RecordFile_WriteRecord() failed.\n");
    return 0;
  }
  
  /* write reverse length */
  if ((nStatus = fwrite (&nLength, 4, 1, pObj->handle)) != 1) { 
    pObj->status = RECORD_WRITE_ERROR;
    fseek (pObj->handle, pObj->position, SEEK_SET);
    fprintf (stderr, "RecordFile_WriteRecord() failed.\n");
    return 0;
  }

  pObj->status = RECORD_WRITE_SUCCESS;
  fflush (pObj->handle);

  return nLength;
}


/***********************************
'filerc' contents
PATH=/data/cassini/data_sopc
RAW=t970319.r01
MP=t970130.m01
MPUS=t961010.u02
KRONOS=kronos.068

bool RecordFile_Filerc(RecordFile *pObj)
{
fprintf(stderr,"bool RecordFile_Filerc(RecordFile *pObj), not implemented\n");
return false;
}

************************************/
