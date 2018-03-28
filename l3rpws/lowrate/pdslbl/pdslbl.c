/*
                      pdslbl.c
                     Robert Johnson
                     Janurary 22, 2003

  This program pdslbl() reads the data files produced for pds archiving and 
outputs the corresponding detached label file.  The intent is to be run on
a directory of day long files; in order to produce a sequence number.

Janurary 22, 2003
Version 1.0 is a quick and dirty attempt at parsing the CORPWS01 header to
     produce a pds label file

June 20, 2003
  Version 1.3 is the start of the new header format.
  Byte 17 - is the instrument type
  Byte 18 - is zero fill
  Bytes 25 -32 contain 24bytes of the mini-packet header
  Bytes 49-64 ascii scet
  Bytes 65-80 ascii sclk
July 2, 2003
  Version 1.3
  change quoting around pointers in pds labels, 
    "(file.dat,1)" to ("file.dat",1).

September 18, 2003
  Version 1.4
  mission_phase_name & target_name are taken from table
  PRODUCT_CREATION_TIME changes from yyyy-doy to yy-mm-dd

September 23, 2003
  Increase max line length to accomodate the MISSION_PHASE_NAMES
  There are some other buffer overrun problems associated with line length.
July 9, 2008
  added "EM" mission phase name in header file beginning July 1, 2008 to
  Oct 1, 2010

June 18, 2009
  added "EXTENDED MISSION and EXTENDED-EXTENDED MISSION" mission phase name 
in header file 

October 25, 2012 (cwp)
   Converted to standard bool.  Used Cassini headers on compiler search path
*/


#include <stdio.h> 
#include <stdlib.h>  /* strtoul() */
#include <string.h>  /* strstr() */
#include <assert.h>
#include <time.h>
#include <sys/stat.h>
#include <stdbool.h>

#include <casephem/CasSpice.h>
#include <casephem/CasTables.h>

#define MAX_LINES        1024
#define MAX_LINE_LENGTH  1024

#ifndef INST_ETC
#error Need INST_ETC, compiled in settings directory not defined
#endif

const char *sVersion="pdslbl() Version 1.9";

bool bVerbose=false;
static char arBuf[1024];  /* only expect to read 16 bytes */

char* get_mission_phase_name(char *sScetBeg, char *sScetEnd);
char* get_target_name(char *sScetBeg, char *sScetEnd);
char* get_target_name_special_pds(char *sScetBeg, char *sScetEnd);
void show_help(void);

/* ************************************************************************* */
int main(int argc,char *argv[])
{
	
char sBuf[MAX_LINE_LENGTH];

char sTargetName[MAX_LINE_LENGTH],sMissPhsNm[MAX_LINE_LENGTH];
char sProductId[MAX_LINE_LENGTH],sSectionId[MAX_LINE_LENGTH];
char sLabelFile[256];
char sSclkBeg[32],sSclkEnd[32],sScetBeg[32],sScetEnd[32];
char sProductCreationTime[32];

char sMasLbl[MAX_LINES][MAX_LINE_LENGTH],sPdsLbl[MAX_LINES][MAX_LINE_LENGTH];
int nMasLines,nLen;

char *p;
char *pByte;
int i,j,nList;
FILE *h;

char *pPds_Special_Description;
char *pRecLen,*pNumRec,*pHdrTbl,*pTimeTbl,*pFreqTbl,*pSpecTbl;
char *pTargetName,*pMissPhsNm,*pProductCreationTime,*pProductId,*pSectionId;
char *pSclkEnd,*pSclkBeg,*pScetEnd,*pScetBeg;

char *pLrfRowBytes;
char *pTimeRowBytes,*pTimeColumns,*pTimeBytes,*pTimeItems;
char *pFreqRowBytes,*pFreqColumns,*pFreqBytes,*pFreqItems;
char *pSpecRowBytes,*pSpecRows,*pSpecColumns,*pSpecBytes,*pSpecItems;

unsigned long nItems,nColumns,*pDword;
/* unsigned long nSclk,nFine,nDays,nMils; */


/*   Record File Variables             0 1 2 3 4 5 6 7 8 9 A B C D E F       */
char sFileId[32];                   /*|   CORPWS01    |nRecLen|nNumRec|      */
unsigned char arMiniPkt[32];        /*| nType | blank | mini-packet   |      */ 
uint32_t nRecLen,nNumRec,nType;/*| cont. mini-packet header      |      */
uint32_t nSclkBeg,nFineBeg,nDaysBeg,nMilsBeg;
uint32_t nSclkEnd,nFineEnd,nDaysEnd,nMilsEnd;

struct stat statBuf;
struct tm *tmBuf;

/* command line args */
char *pFileList[1024],*sMasLblFileName;
int nFileList=0,nPadLen=0;
bool bDayLongTimeInt=true;  /* ignore record time, use day long interval */

  
  if(argc>1024){
    fprintf(stderr,"Too many files to process (%d)\n",argc);
    exit(127);
  }



  sMasLblFileName=NULL;
  nFileList=0;
  nColumns=5;  /* 5 columns for the SCLK_SCET.FMT file */
  while(--argc){
    ++argv;
    if(!strcmp("-v",*argv))
      bVerbose=true;
    else if(!strcmp("-c",*argv)){
      --argc;  ++argv;
      nColumns=strtoul(*argv,NULL,0);  /* inhouse rules, no COLUMNS= */
    }
    else if(!strcmp("-d",*argv)){
      bDayLongTimeInt=false;  /* use true record times for intervals */
    }
    else if(!strcmp("-m",*argv)){
      --argc;  ++argv;
      sMasLblFileName=*argv;
    }
    else if(!strcmp("-p",*argv)){
      --argc;  ++argv;
      nPadLen=strtoul(*argv,NULL,0);  /* maximum line length */
    }
    else if(!strcmp("-h",*argv) || !strcmp("-help",*argv)){
      fprintf(stdout,"%s\n",sVersion);
      fprintf(stdout,
        "-d  - used day long intervals for start and stop time\n"
        "-h  - show help\n"
        "-m FILE  - use FILE as the label template, mandatory\n"
        "-p NNN  - pad the columns to NNN length, maximum line length\n"
        );
      exit(0);
    }
    else{
      pFileList[nFileList]=*argv;
      ++nFileList;
    }
  }/* while parsing command line argurments */

  if(bVerbose==true)
    fprintf(stderr,"%s\n",sVersion);

  /* init the spice kernel */
  CasSpice_Init(NULL);


  if(sMasLblFileName==NULL){
    sMasLblFileName=calloc(1,strlen( INST_ETC "pds/LRFC_MASTER.LBL")+1); 
    strcpy(sMasLblFileName, INST_ETC "pds/LRFC_MASTER.LBL");
  }
  /* read the master label file */
  if( (h=fopen(sMasLblFileName,"rt"))==NULL ){
    fprintf(stderr,"unable to read master label file %s\n",sMasLblFileName);
    exit(127); 
  }
  nMasLines=0;
  while( (p=fgets(sMasLbl[nMasLines],MAX_LINE_LENGTH,h))!=NULL){
    if( strlen(p) > MAX_LINE_LENGTH-2 ){
      fprintf(stderr,"line #%d is too long, greater than %d chars.\n",
              nMasLines+1,MAX_LINE_LENGTH);
      exit(1);
    }
    while( (*p!=0x0D) && (*p!=0x0A) && (*p!=0x00) )  ++p;  
    *p='\0';  /* nix cr lf */
    ++nMasLines;
  }
  fclose(h);



  /* parse for items of interest */
  memcpy(sPdsLbl,sMasLbl,MAX_LINES*MAX_LINE_LENGTH);

/*
for(i=0;i<nMasLines;i++)
  fprintf(stderr,"LINE %3d::%s\n",i,sPdsLbl[i]);
fprintf(stderr,"end of buffer\n");
*/


  pRecLen=pNumRec=pHdrTbl=pTimeTbl=pFreqTbl=pSpecTbl=sBuf;
  pTargetName=pMissPhsNm=pProductCreationTime=pProductId=sBuf;
  pSclkEnd=pSclkBeg=pScetEnd=pScetBeg=sBuf;

  /* valid memory address to write to */
  pLrfRowBytes=sBuf;
  pTimeRowBytes=pTimeColumns=pTimeBytes=pTimeItems=sBuf;
  pFreqRowBytes=pFreqColumns=pFreqBytes=pFreqItems=sBuf;
  pSpecRowBytes=pSpecRows=pSpecColumns=pSpecBytes=pSpecItems=sBuf;
  pPds_Special_Description=NULL;

  /* Feb 26, 2004 - pds has a parser which parses the label files for a 
       description and then their web browser displays that description.
       Now, PDS want a special "DESCRIPTION =" to appear in the first few
       lines of the file, containing the following text: 
          "$FILE_NAME contains RPWS low rate data from $START to $STOP".
     It seems quite strange that PDS is not capable of parsing their own
       label files.  Anyway, find the first DESCRIPTION =  and subsitute
       the some text.
  */
  /* All objects and elements are assumend to be unique in the file */
  for(i=0;i<nMasLines;i++){
    if( ((p=strstr(sPdsLbl[i],"DESCRIPTION"))!=NULL) &&
        (pPds_Special_Description==NULL) ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;
      pPds_Special_Description=++p;  *++p=0x00;
    }
    if( (p=strstr(sPdsLbl[i],"RECORD_BYTES"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;
      pRecLen=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"FILE_RECORDS"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pNumRec=++p;  *++p=0x00;
    }

    else if( (p=strstr(sPdsLbl[i],"^LRFULL_TABLE"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pHdrTbl=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"^TIME_TABLE"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pTimeTbl=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"^FREQUENCY_TABLE"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pFreqTbl=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"^SPECTRAL_DENSITY_TABLE"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pSpecTbl=++p;  *++p=0x00;
    }

    else if( (p=strstr(sPdsLbl[i],"MISSION_PHASE_NAME"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pMissPhsNm=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"TARGET_NAME"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pTargetName=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"PRODUCT_ID"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pProductId=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"STANDARD_DATA_PRODUCT_ID"))!=NULL ){
      /* trap out for the sub-strings */
    }
    else if( (p=strstr(sPdsLbl[i],"SECTION_ID"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pSectionId=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"START_TIME"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pScetBeg=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"STOP_TIME"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pScetEnd=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"SPACECRAFT_CLOCK_START_COUNT"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pSclkBeg=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"SPACECRAFT_CLOCK_STOP_COUNT"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pSclkEnd=++p;  *++p=0x00;
    }
    else if( (p=strstr(sPdsLbl[i],"PRODUCT_CREATION_TIME"))!=NULL ){
      while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
      pProductCreationTime=++p;  *++p=0x00;
    }

/*
   do some simple parsing of the column objects in a brut force mannor.
   keyword placement is order dependant and will probably fail for any
   additions to embeded objects in the label file (in the true spirit of
   pds quality software).
*/

    else if( (p=strstr(sPdsLbl[i],"LRFULL_TABLE"))!=NULL ){
      while(++i<nMasLines){
        if( (p=strstr(sPdsLbl[i],"END_OBJECT"))!=NULL ){
          break;
        }
        else if( (p=strstr(sPdsLbl[i],"ROW_BYTES"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pLrfRowBytes=++p;  *++p=0x00;
        }
      }/* while */
    }/* LRFULL_TABLE */

    else if( (p=strstr(sPdsLbl[i],"TIME_TABLE"))!=NULL ){
      while(++i<nMasLines){
        if( (p=strstr(sPdsLbl[i],"END_OBJECT"))!=NULL ){
          if( (p=strstr(sPdsLbl[i],"TIME_TABLE"))!=NULL )  break;
          else                                             continue;
        }
        else if( (p=strstr(sPdsLbl[i],"ITEM_BYTES"))!=NULL ){
          /* trap ITEM_BYTES */
        }
        else if( (p=strstr(sPdsLbl[i],"ROW_BYTES"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pTimeRowBytes=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"COLUMNS"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pTimeColumns=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"BYTES"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pTimeBytes=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"ITEMS"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pTimeItems=++p;  *++p=0x00;
        }
      }/* while */
    }/* TIME_TABLE */
    else if( (p=strstr(sPdsLbl[i],"FREQUENCY_TABLE"))!=NULL ){
      while(++i<nMasLines){
        if( (p=strstr(sPdsLbl[i],"END_OBJECT"))!=NULL ){
          if( (p=strstr(sPdsLbl[i],"FREQUENCY_TABLE"))!=NULL )  break;
          else                                                  continue;
        }
        else if( (p=strstr(sPdsLbl[i],"ITEM_BYTES"))!=NULL ){
          /* trap ITEM_BYTES */
        }
        else if( (p=strstr(sPdsLbl[i],"ROW_BYTES"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pFreqRowBytes=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"COLUMNS"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pFreqColumns=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"BYTES"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pFreqBytes=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"ITEMS"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pFreqItems=++p;  *++p=0x00;
        }
      }/* while */
    }/* FREQUENCY_TABLE */
    else if( (p=strstr(sPdsLbl[i],"SPECTRAL_DENSITY_TABLE"))!=NULL ){
      while(++i<nMasLines){
        if( (p=strstr(sPdsLbl[i],"END_OBJECT"))!=NULL ){
          if( (p=strstr(sPdsLbl[i],"SPECTRAL_DENSITY_TABLE"))!=NULL )  break;
          else                                                      continue;
        }
        else if( (p=strstr(sPdsLbl[i],"ITEM_BYTES"))!=NULL ){
          /* trap ITEM_BYTES */
        }
        else if( (p=strstr(sPdsLbl[i],"ROW_BYTES"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pSpecRowBytes=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"ROWS"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pSpecRows=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"COLUMNS"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pSpecColumns=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"BYTES"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pSpecBytes=++p;  *++p=0x00;
        }
        else if( (p=strstr(sPdsLbl[i],"ITEMS"))!=NULL ){
          while( (*p!='=') && (*p!='\0') )  ++p;  *++p=0x20;       
          pSpecItems=++p;  *++p=0x00;
        }
      }/* while */
    }/* SPECTRAL_DENSITY_TABLE */

  }/* while parsing label file */  


  /* check for known items */
  if(pPds_Special_Description==NULL){ 
     fprintf(stderr,"RECORD_BYTES not found\n"); 
     exit(1); 
  }
/*

  if(pRecLen==NULL){ fprintf(stderr,"RECORD_BYTES not found\n"); exit(127); }
  if(pNumRec==NULL){ fprintf(stderr,"FILE_RECORDS not found\n"); exit(127); }

  if(pHdrTbl==NULL){ fprintf(stderr,"LRFULL_TABLE not found\n");   exit(127);}
  if(pTimeTbl==NULL){fprintf(stderr,"TIME_TABLE not found\n");     exit(127);}
  if(pFreqTbl==NULL){fprintf(stderr,"FREQUENCY_TABLE not found\n");exit(127);}
  if(pSpecTbl==NULL){fprintf(stderr,"SPEC_TABLE not found\n");     exit(127);}

  if(pMissPhsNm==NULL){fprintf(stderr,"MISSION_PHASE_NAME not found\n");exit(127);}
  if(pTargetName==NULL){fprintf(stderr,"TARGET_NAME not found\n");exit(127);}
  if(pProductId==NULL){fprintf(stderr,"PRODUCT_ID not found\n"); exit(127);}
  if(pSectionId==NULL){fprintf(stderr,"SECTION_ID not found\n"); exit(127);}
  if(pScetBeg==NULL){  fprintf(stderr,"START_TIME not found\n"); exit(127);}
  if(pScetEnd==NULL){  fprintf(stderr,"STOP_TIME not found\n");  exit(127);}
  if(pSclkBeg==NULL){fprintf(stderr,"SPACECRAFT_CLOCK_START_COUNT not found\n");exit(127);}
  if(pSclkEnd==NULL){fprintf(stderr,"SPACECRAFT_CLOCK_STOP_COUNT not found\n");exit(127);}
  if(pProductCreationTime==NULL){fprintf(stderr,"PRODUCT_CREATION_TIME not found\n");  exit(127);}
*/

  
  for(nList=0;nList<nFileList;nList++){
    memcpy(sPdsLbl,sMasLbl,MAX_LINES*MAX_LINE_LENGTH);

    /* nix any path information and the '.DAT' */
    p=pFileList[nList];  p+=strlen(pFileList[nList]);  /* points to null */
    while( p>=pFileList[nList] ){
      if(*p=='/')  break;
      --p;
    }
    ++p;
    strcpy(sProductId,p);
    if( (p=strstr(sProductId,".DAT"))==NULL ){
      fprintf(stderr,"%s is not a pds data file.\n",pFileList[nList]);
      continue;
    }
    *p='\0';
   
    if(bVerbose==true)
      fprintf(stderr,"file=%s\n  prodid=%s\n",pFileList[nList],sProductId);

    /* parse the data file (.DAT) */
    if( (h=fopen(pFileList[nList],"rb"))==NULL ){
      fprintf(stderr,"unable to read %s\n",pFileList[nList]);
      continue;
    }
    if( (nLen=fread(arBuf,sizeof(char),96,h))!=96 ){
      fprintf(stderr,"error reading corpws header from %s, read %d of 96\n",
              pFileList[nList],nLen);
      exit(127);  
    }
   
 
    pByte=(char*)arBuf;  /* (char*) */
    strncpy(sFileId,(char*)pByte,8);
    pDword=(void*)(arBuf+8);
    nRecLen=*pDword++;
    nNumRec=*pDword++;
    nType=*pDword++;   /* nType is the mini-packet,logical receiver,antenna */
    for(i=0;i<24;i++)  /* mini-packet */
      arMiniPkt[i]=arBuf[25+i];
    
    if( (nType&0xFF000000)==0x71000000 )       /* lfdr - log ,real */
      strcpy(sSectionId,"LFR");
    else if( (nType&0xFF000000)==0x72000000 )  /* mfdr - log ,real */
      strcpy(sSectionId,"MFDR");
    else if( (nType&0xFF000000)==0x11000000 )  /* mfr  - normal */
      strcpy(sSectionId,"MFR");
    else if( (nType&0xFF000000)==0x12000000 )  /* mfr  - fast toggle */
      strcpy(sSectionId,"MFR");
    else if( (nType&0xFF000000)==0x21000000 )  /* hfr - analysis */
      strcpy(sSectionId,"HFR");
    else if( (nType&0xFF000000)==0x28000000 )  /* hfr - millisecond */
      strcpy(sSectionId,"HFR");
    else{
      strcpy(sSectionId,"error");
      fprintf(stderr,"Unknown type %08X\n",nType);
      for(i=0;i<24;i++)
        fprintf(stderr,"%02X ",arMiniPkt[i]);
      fprintf(stderr,"\n");
      exit(1);
    }


    /* jump to time table, contains the time for the first data record */
    fseek(h,(long)nRecLen,SEEK_SET); 
    if( (nLen=fread(arBuf,sizeof(char),16,h))!=16 ){
      fprintf(stderr,"error reading time table from  %s, read %d of 16\n",
              pFileList[nList],nLen);
      exit(127);  
    }
    pDword=(void*)arBuf;  /* (Ulong*) */
    nSclkBeg=*pDword++;  
    nFineBeg=*pDword++;
    nDaysBeg=nFineBeg;
      nFineBeg>>=24;       /* upper byte */
      nDaysBeg&=0x0FFFF;   /* lower 2 bytes */
    nMilsBeg=*pDword++;


    /* jump to freq table, contains the time for the last data record */
    fseek(h,(long)(nRecLen*2),SEEK_SET); 
    if( (nLen=fread(arBuf,sizeof(char),16,h))!=16 ){
      fprintf(stderr,"error reading frequency table from %s, read %d of 16\n",
              pFileList[nList],nLen);
      exit(127);  
    }
    pDword=(void*)arBuf;  /* (Ulong*) */
    nSclkEnd=*pDword++;  
    nFineEnd=*pDword++;
    nDaysEnd=nFineEnd;
      nFineEnd>>=24;       /* upper byte */
      nDaysEnd&=0x0FFFF;   /* lower 2 bytes */
    nMilsEnd=*pDword++;
 

    fclose(h);  /* close binary data files */

    /* use the DAT file modification as the product creation time */
    assert(stat(pFileList[nList],&statBuf)==0); /* 2003-12-12 */
    tmBuf=gmtime(&(statBuf.st_mtime));
    strftime(sProductCreationTime,32,"%Y-%m-%d",tmBuf); 

    /* look up mission phase name via sclk, use actual file sclk */
    CasSpice_nSclk_to_GOOFY_sSclk(nSclkBeg,nFineBeg,sSclkBeg); 
    CasSpice_nScet_to_sScet(nDaysBeg,nMilsBeg,sScetBeg); 
    CasSpice_nSclk_to_GOOFY_sSclk(nSclkEnd,nFineEnd,sSclkEnd); 
    CasSpice_nScet_to_sScet(nDaysEnd,nMilsEnd,sScetEnd); 
    sprintf(sMissPhsNm,"%s",get_mission_phase_name(sScetBeg,sScetEnd));
    sprintf(sTargetName,"%s",get_target_name(sScetBeg,sScetEnd));

    /* format the start and stop strings for the label file */ 
    if(bDayLongTimeInt==true){  /* ignore record times and use day interval */
      nMilsBeg=0;                  /* nix milliseconds of day */
      CasSpice_nScet_to_sScet(nDaysBeg,nMilsBeg,sScetBeg); 
      CasSpice_sScet_to_nSclk(sScetBeg,&nSclkBeg,&nFineBeg);  
      CasSpice_nSclk_to_GOOFY_sSclk(nSclkBeg,nFineBeg,sSclkBeg); 

      nDaysEnd+=1;  nMilsEnd=0;    /* add 1 to day, nix milliseconds of day */
      CasSpice_nScet_to_sScet(nDaysEnd,nMilsEnd,sScetEnd); 
      CasSpice_sScet_to_nSclk(sScetEnd,&nSclkEnd,&nFineEnd);  
      CasSpice_nSclk_to_GOOFY_sSclk(nSclkEnd,nFineEnd,sSclkEnd); 
    }
    else{
      CasSpice_nSclk_to_GOOFY_sSclk(nSclkBeg,nFineBeg,sSclkBeg); 
      CasSpice_nScet_to_sScet(nDaysBeg,nMilsBeg,sScetBeg); 

      CasSpice_nSclk_to_GOOFY_sSclk(nSclkEnd,nFineEnd,sSclkEnd); 
      CasSpice_nScet_to_sScet(nDaysEnd,nMilsEnd,sScetEnd); 
    }
    
/*
DESCRIPTION = "T1997298_MFR0.DAT contains fully calibrated, full resolution 
     Cassini Radio and Plasma Wave (RPWS) low rate data for the time period 
     between 1997-298T00:00:00.000 and 1997-299T00:00:00.000 and includes the 
     following targets: "SOLAR SYSTEM".
*/

    /* special pds requirement for speical description - since pds
       is unable to parse their own files */
    sprintf(pPds_Special_Description,
      "%c%s.DAT contains fully calibrated, full resolution\n"
      "     Cassini Radio and Plasma Wave (RPWS) low rate data for the time period\n" 
      "     between %s and %s and includes\n"
      "     the following targets: %s.%c\n",0x22,sProductId,sScetBeg,sScetEnd,
      get_target_name_special_pds(sScetBeg,sScetEnd),0x22);

    /* format the label record with values */
    sprintf(pRecLen,"%d",nRecLen);
    sprintf(pNumRec,"%d",nNumRec);
  
    sprintf(pHdrTbl, "(%c%s.DAT%c,1)",0x22,sProductId,0x22);
    sprintf(pTimeTbl,"(%c%s.DAT%c,2)",0x22,sProductId,0x22);
    sprintf(pFreqTbl,"(%c%s.DAT%c,3)",0x22,sProductId,0x22);
    sprintf(pSpecTbl,"(%c%s.DAT%c,4)",0x22,sProductId,0x22);

    sprintf(pMissPhsNm,"%s",sMissPhsNm);
    sprintf(pTargetName,"%s",sTargetName);
    sprintf(pProductId,"%c%s_V1%c",0x22,sProductId,0x22);
    sprintf(pSectionId,"%s",sSectionId);
    sprintf(pScetBeg,"%sZ",sScetBeg);
    sprintf(pScetEnd,"%sZ",sScetEnd);
    sprintf(pSclkBeg,"%c1/%s%c",0x22,sSclkBeg,0x22);
    sprintf(pSclkEnd,"%c1/%s%c",0x22,sSclkEnd,0x22);
    sprintf(pProductCreationTime,"%s",sProductCreationTime);

    /* now the dreaded row,bytes,column mumbo jumbo */
    nItems=(nRecLen-16)/4;
    sprintf(pLrfRowBytes,"%d",nRecLen);

    sprintf(pTimeRowBytes,"%d",nRecLen);
/*  sprintf(pTimeColumns,"%d",nColumns+nItems); inhouse rules, no COLUMMNS=*/
    sprintf(pTimeBytes,"%d",nRecLen-16);
    sprintf(pTimeItems,"%ld",nItems);

    sprintf(pFreqRowBytes,"%d",nRecLen);
/*  sprintf(pFreqColumns,"%d",nColumns+nItems); inhouse rules, no COLUMMNS=*/
    sprintf(pFreqBytes,"%d",nRecLen-16);
    sprintf(pFreqItems,"%ld",nItems);

    sprintf(pSpecRowBytes,"%d",nRecLen);
    sprintf(pSpecRows,"%d",nNumRec-3);
/*  sprintf(pSpecColumns,"%d",nColumns+1+nItems); inhouse rules,no COLUMMNS=*/
    sprintf(pSpecBytes,"%d",nRecLen-16);
    sprintf(pSpecItems,"%ld",nItems);

    for(j=0;j<nMasLines;j++){

      p=sPdsLbl[j];
      nLen=strlen(p);
      if(nPadLen){
        while(nLen<nPadLen)  
          p[nLen++]=0x20;
      }
      p[nLen++]=0x0D;  p[nLen++]=0x0A;  p[nLen]=0x00;     
    }/* for() attach line termination characters */

    /* write label file */
    p=strcpy(sLabelFile,pFileList[nList]);
    if( (p=strstr(sLabelFile,".DAT"))==NULL ){
      fprintf(stderr,"%s is not a pds data file.\n",pFileList[nList]);
      continue;
    }
    sprintf(p,".LBL");
    if( (h=fopen(sLabelFile,"wt"))==NULL ){
      fprintf(stderr,"unable to write %s\n",sLabelFile);
      continue;
    }
    if(bVerbose==true)
      fprintf(stderr,"Label File=%s\n\n",sLabelFile);

    for(i=0;i<nMasLines;i++)
      fprintf(h,"%s",sPdsLbl[i]);
    fclose(h);

  }/* write pds lable files */





return 1;
}



