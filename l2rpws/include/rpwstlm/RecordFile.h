#include <Cext.h>

#ifndef _CRecordFile_h
#define _CRecordFile_h


#ifdef __cplusplus
extern "C"
{
#endif


#define RECORD_READ_SUCCESS    (0x01)
#define RECORD_WRITE_SUCCESS   (0x02)
#define RECORD_SEEK_SUCCESS    (0x03)
#define RECORD_READ_ERROR      (0x04)
#define RECORD_FILE_OPEN       (0x05)
#define RECORD_FILE_CLOSED     (0x06)
#define RECORD_BOF             (0x07)   /* Beginning of File */
#define RECORD_EOF             (0x08)   /* End of File */
#define RECORD_WRITE_ERROR     (0x09)
#define RECORD_SEEK_ERROR      (0x0A)
#define RECORD_READ_HOLD       (0x0B)
#define RECORD_INTERNAL_ERROR  (0x0C)   /* Error somewhere */



typedef struct record_file_tag
{
    char *filename, *type;
    int64_t position;               /* byte offset for the beginning of file */
	 int32_t status;
    FILE *handle;
} RecordFile;


/* Returns: NULL is success, otherwise an error message */
RecordFile *RecordFile_Constructor (RecordFile * pSrc);
void RecordFile_Destructor (RecordFile * pObj);
bool RecordFile_Initialize (RecordFile * rf, RecordFile * pSrc);
bool RecordFile_Copy (RecordFile * pDst, RecordFile * pSrc);

bool RecordFile_Open (RecordFile * rf, const char *filename,
                      const char *type);
bool RecordFile_Close (RecordFile * rf);

int32_t RecordFile_ReadPreviousRecord (RecordFile * rf, void *pv);
int32_t RecordFile_ReadCurrentRecord (RecordFile * rf, void *pv);
int32_t RecordFile_ReadNextRecord (RecordFile * rf, void *pv);
int32_t RecordFile_MoveToPreviousRecord (RecordFile * rf);
int32_t RecordFile_MoveToCurrentRecord (RecordFile * rf);
int32_t RecordFile_MoveToNextRecord (RecordFile * rf);

/** Write a record to the file 
 *
 * @return The number of bytes written to the record.  0 indicates a
 *         failure to write anything.
 */
int32_t RecordFile_WriteRecord (RecordFile * pObj, void *pBuf);

#ifdef __cplusplus
} /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif
