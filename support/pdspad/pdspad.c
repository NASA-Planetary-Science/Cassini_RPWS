/*
                      pdspad.c
                     Robert Johnson
                     June 19, 2003

  pdspad() is a simple program which transforms tabs into spaces and pads
to the end of line with spaces, and terminates lines with <CR><LF>.

*/


#include <stdio.h> 
#include <stdlib.h>  /* strtoul() */
#include <string.h>  /* strncpy() */
#include <assert.h>  /* assert() */

const char *s80spaces="                                        "
                      "                                        ";
const char *sVersion="version 1.3";
void terminate_line(char *sFiles[],int nMaxLen);      
void show_help(FILE *h);

int main(int argc,char *argv[])
{
char sBuf[10240],*sFiles[10240],*sTmpFile,sCmd[128],sTmp[1024],*pTmp;
char *p,*pBeg,*pEnd;
int nLine=0,nLineMax=0,nLineNo=0,nFiles=0,nFilesMax=0;
int bVerbose=0,bTerminate=0;
int nEquals,bList,bComment;
FILE *hIn=stdin,*hOut=stdout;



  while(--argc){
    ++argv;
    if((!strcmp("-h",*argv))||(!strcmp("-help",*argv))){
      show_help(stdout);
      exit(0);
    }
    else if(!strcmp("-p",*argv)){
      --argc;  ++argv;
      if( (nLineMax=strtoul(*argv,NULL,0)) > 1024 ){
        fprintf(stderr,"error, line length of %d exceeds 1024 limit\n",
                nLineMax);
        exit(127);
      }
    }
    else if(!strcmp("-t",*argv))
      bTerminate=1;
    else if(!strcmp("-v",*argv))
      bVerbose=1;
    else if((!strcmp("-ver",*argv))||(!strcmp("-version",*argv)))
      fprintf(stderr,"%s\n",sVersion);
    else{  /* assume a file */
      sFiles[nFilesMax++]=*argv;  sFiles[nFilesMax]=NULL;
    }
  }

  if(bVerbose)
    fprintf(stderr,"pdspad() %s\n",sVersion);

  if(bTerminate){ /* just terminate line with <CR><LF>, ignore pad length */
    terminate_line(sFiles,nLineMax);      
    exit(0);
  }

  if(nLineMax==0)  nLineMax=80;  /* default to 80 columns */

  while(nFiles<nFilesMax){
    if( (hIn=fopen(sFiles[nFiles],"r"))==NULL ){
      fprintf(stderr,"unable to read %s\n",sFiles[nFiles]);
      ++nFiles;
      continue;
    }
    sTmpFile=tmpnam(NULL);
    if( (hOut=fopen(sTmpFile,"w"))==NULL ){
      fprintf(stderr,"unable to write tmp file %s\n",sTmpFile);
      exit(1);
    }
    nLineNo=0;
    while((p=fgets(sBuf,10240,hIn))!=NULL){/* sBuf always null terminated */
      ++nLineNo; 
      while( (*p!='\r') && (*p!='\n') && (*p!='\0') ){
        if(*p=='\t')  *p=0x20;  /* nix any tabs */
        ++p;
      }
      *p='\0';  /* nix <CR><LF> */
      nLine=strlen(sBuf);

      if(nLine <= (nLineMax-2) ){         /* short line */
        while(nLine++<nLineMax)  *p++=0x20;  /* space */
        sBuf[nLineMax-2]='\r';
        sBuf[nLineMax-1]='\n';
        sBuf[nLineMax]  ='\0';
        fprintf(hOut,"%s",sBuf); 
      }  /* short line */
      else{ /* long line <CR><LF> should be absent */
        nEquals=bList=bComment=0;  p=sBuf;  /* <CR><LF> should be absent */
        while((*p!='\r') && (*p!='\n') && (*p!='\0')){
          if((*p=='=') && (nEquals==0))  nEquals=p-sBuf;/* first equals sign */
          if((*p=='{') || (*p=='}'))  bList=1;  /* need a better list test */
          if((*p=='(') || (*p==')'))  bList=1;  /* need a better list test */
          if((*p=='/') && (*(p+1)=='*'))  bComment=1; 
          ++p;
        }
        *p='\0';  /* replace <CR><LF> with string terminator */
        nLine=strlen(sBuf);
        nEquals+=3;  /* start indent after = { */

        assert(nLineMax>nEquals);
        pBeg=pEnd=sBuf;  pEnd+=nLineMax-3;  pTmp=sTmp; /* initial conditions */
        while(nLine>0){

          /* find where to split the line, for list use ',' otherwise ' ' */
          if(bList){ 
            while((pEnd>pBeg) && (*pEnd!=',') && (*pEnd!='\0'))
              --pEnd;
          }
          else{      
            while((pEnd>pBeg) && (*pEnd!=' ') && (*pEnd!='\0'))
              --pEnd;
          }

          if(pBeg==pEnd){
            fprintf(stderr,"no rule for wrapping line No. %d of length %d\n",
                    nLineNo,nLine);
            fprintf(stderr,"%s\n",pBeg);
            assert(pBeg!=pEnd);
          }

          strncpy(pTmp,pBeg,(size_t)(pEnd-pBeg+1));  
          pTmp[pEnd-pBeg+1]='\0';


          /* pad to end of line */
          pTmp=sTmp+strlen(sTmp);           
          while((pTmp-sTmp) < nLineMax)  *pTmp++=0x20;
          sTmp[nLineMax-2]='\r';  sTmp[nLineMax-1]='\n';
          sTmp[nLineMax]  ='\0';
          fprintf(hOut,"%s",sTmp); 

          /* line up for the next chunk */
          nLine-=(pEnd-pBeg+1);  /* termination condition, orginal string */
          pBeg=pEnd+1;
          if(strlen(pBeg)>(nLineMax-nEquals-3))
            pEnd=pBeg+(nLineMax-nEquals)-3;
          else                           /* we are at the end of the string */
            pEnd=pBeg+strlen(pBeg);

          strcpy(sTmp,s80spaces);
          if(bComment){  sTmp[0]='/';  sTmp[1]='*';  nEquals=3;}
          pTmp=sTmp+nEquals;    /* = "XXX  three chars after equals */
        } 

      }/* long line */


    }
    fclose(hIn);
    fclose(hOut);

    sprintf(sCmd,"mv %s %s",sTmpFile,sFiles[nFiles]);
    if(system(sCmd)){
      fprintf(stderr,"unable to create %s\n",sFiles[nFiles]);
      remove(sTmpFile);
    }
    ++nFiles;
  }/* while list of files */



return 0;
}



void terminate_line(char *sFiles[],int nMaxLen)
{
char *p,sBuf[8192];
char *sTmpFile,sCmd[256];
int nFiles=0,nLine;
FILE *hIn,*hOut;


  while(sFiles[nFiles]!=NULL){
    if( (hIn=fopen(sFiles[nFiles],"r"))==NULL ){
      fprintf(stderr,"unable to read %s\n",sFiles[nFiles]);
      continue;
    }
    sTmpFile=tmpnam(NULL);
    if( (hOut=fopen(sTmpFile,"w"))==NULL ){
      fprintf(stderr,"unable to write tmp file %s\n",sTmpFile);
      exit(1);
    }
    
    if(nMaxLen>0){
      /* find the line maximum length */
      while((p=fgets(sBuf,8192,hIn))!=NULL){
        if(strlen(p)>nMaxLen)
          nMaxLen=strlen(p);
      }
      nMaxLen+=2; /* save room for the <CR><LF> */
      fclose(hIn);
      if( (hIn=fopen(sFiles[nFiles],"r"))==NULL ){
        fprintf(stderr,"unable to read %s\n",sFiles[nFiles]);
        exit(1);
      }  
    }  

    while((p=fgets(sBuf,8192,hIn))!=NULL){     /* sBuf always null terminated */
      while( (*p!='\r') && (*p!='\n') && (*p!='\0') ){
        if(*p=='\t')  *p=0x20;  /* nix any tabs */
        ++p;
      }
      *p='\0';  /* nix <CR><LF> */

      --p;
      while((p>sBuf) && (*p==' ') ) /* delete trailing spaces */
        *p--='\0';
      ++p;

      nLine=strlen(sBuf);

      /* pad with spaces */
      while(nLine++<nMaxLen)  
        *p++=0x20;
      *p++='\r'; *p++='\n'; *p++='\0';

      fprintf(hOut,"%s",sBuf); 

    }
    fclose(hIn);
    fclose(hOut);

    sprintf(sCmd,"mv %s %s",sTmpFile,sFiles[nFiles]);
    if(system(sCmd)){
      fprintf(stderr,"unable to create %s\n",sFiles[nFiles]);
      remove(sTmpFile);
    }
    ++nFiles;
  }/* while list of files */


return;
}      



void show_help(FILE *h)
{
fprintf(h,"pdspad() %s\n",sVersion);
fprintf(h,
"usage: pdspad -p NNN -t\n"
"  -p N   Pad text file to N columns and terminate the line with <CR><LF>\n"
"  -t     Terminate line with <CR><LF> and don't pad.\n"
"         If pad length is specified, all lines will be padded\n"
"         to N columns or the length of the longest line, which ever is\n"
"         greater.\n");
fprintf(h,
"   If a <CR><LF> pair exists already, or multiple <CR> exist, pdspad() will\n"
"terminate the line with only one <CR><LF> pair.\n"
"   If a line length is given and items in a pds set are longer than the line\n"
"length, pdspad() will attempt to wrap these items appropiately.  For example\n"
"html lable files where there is a long list of html files.\n"
"   ^HTML_DOCUMENT={file.htm,file2.htm,file3.htm,file4.htm,file5.htm}\n"
"\n"
);
fprintf(h,
"  Ex: pdspad -t \n"
"   simply terminates line with <CR><LF>\n" 
"\n"
"  Ex: pdspad -p 80 \n"
"   Pads each line to 80 characters total, including the <CR><LF> character.\n"
"   Wraps pds lists, {item,item,item} according to your file format\n"
"\n"
"  Ex: pdspad -t -p 1 \n"
"   Finds the longest line, greater than one, and pad all lines to this \n"
"   length; terminating with a <CR><LF>\n"
"\n"
"  Limits:\n"
"   Line Length must be less than 10K or 10,240 bytes\n"
"   Number of input files must be less than 10K or 10,240 files\n"
); 

return;
}
