/*
                      hfrfmt.c
                     Robert Johnson
                     August 13, 2004

*/


#include <stdio.h> 
#include <stdlib.h>
#include <string.h>

char sError[16]="";

void left_shift(char *pHead,int nBlank);
void fix_hfr_double_header(char *pHead,int nBlank);

int main(int argc,char *argv[])
{
char *pInLine,sInLine[1024];
char sLine[64][512];
int nLineNo,nTableBeg,nTableEnd,nTableLine;
int i,j,nBlank,nHfrDoubleHeader;

  while(--argc){
    ++argv;
  }

  nLineNo=nTableBeg=nTableEnd=0;
  while((pInLine=fgets(sInLine,1024,stdin))!=NULL){
    strcpy(sLine[nLineNo],sInLine);

    if(nTableBeg>0){
      while(*pInLine!='\0'){  /* find blank line */
        if((*pInLine>='0') && (*pInLine<='9'))       break;
        else if((*pInLine>='a') && (*pInLine<='z'))  break;
        else if((*pInLine>='A') && (*pInLine<='Z'))  break;
        else                                         ++pInLine;
      }
      if(*pInLine=='\0'){  
        nTableEnd=nLineNo;
        break;
      }    
    }/* fi */

    if((nTableBeg==0) && (strstr(sInLine,"---"))){
      nTableBeg=nLineNo;
    }


    if(++nLineNo>63){ 
      strcpy(sError,"__error__");
      break;
    }
  }/* elihw reading lines */

  if(nTableBeg==0){ /* no table found */
    for(i=0;i<nLineNo;i++)
      fprintf(stdout,"%s\n",sLine[i]);
    exit(1);
  }
  if(nTableEnd==0)  /* no table end not found */
    nTableEnd=nLineNo; 

/* expecting the hfr table format..

          Correlations     Channels        Integ.                Size Frequency
Band Ant. Auto-  Cross-    per Band   df   Period    Rep   Steps kHz  Start - Stop
---- ---- -----  ------    --------   --   ------    ---   ----- ---- ------------
ABC  2E   y         y        16       n    1000      1                3.6-319 kHz
H1   2E             y         2       n    80        1     30    50   325k-1.8MHz
H2   1E                       1       n    20        1     72    200  1.8M-16MHz


*/

/* find the fields seperated by two or more spaces  '---  ----' */

  nTableLine=nTableBeg--;
  pInLine=sLine[nTableLine];
  if(strstr(sLine[nTableBeg],"Auto-") && strstr(sLine[nTableBeg],"Cross-"))
    nHfrDoubleHeader=1;
   else
    nHfrDoubleHeader=0;

  for(i=0;i<strlen(pInLine);i++){
    if((pInLine[i]=='-') && (pInLine[i+1]==' ') && (pInLine[i+2]==' ')){
      nBlank=0;
      while(*(pInLine+i+2+nBlank)==' ')  /* at least one extra blank */
        ++nBlank;

      if(nHfrDoubleHeader>0)
        fix_hfr_double_header(sLine[nTableBeg-1]+i+1,nBlank);

      for(j=nTableBeg;j<nTableEnd;j++){
        left_shift(sLine[j]+i+1,nBlank);
      } 

      i=0;  
    }/* fi 2 spaces */
  }/* rof */


  if(nHfrDoubleHeader>0)
    --nTableBeg;

  /* lines before table */
  for(i=0;i<nTableBeg;i++)
    fprintf(stdout,"%s",sLine[i]);

  for(i=nTableBeg;i<nTableEnd;i++){
    if(strlen(sLine[i])>78)
      strcpy(sError,"__error__");
  }

  if(!strcmp(sError,""))
    fprintf(stdout,"%s\n",sError);
  for(i=nTableBeg;i<nTableEnd;i++)
    fprintf(stdout,"%s",sLine[i]);
  if(!strcmp(sError,""))
    fprintf(stdout,"%s\n",sError);

  for(i=nTableEnd;i<nLineNo;i++)
    fprintf(stdout,"%s",sLine[i]);

  while((pInLine=fgets(sInLine,1024,stdin))!=NULL)
    fprintf(stdout,"%s",sInLine);

return 0;
}



void left_shift(char *pHead,int nBlank)
{
char *pTail;

  pTail=pHead+nBlank;
  if(*pHead!=' ')  
    strcpy(sError,"__error__");
  if(*pTail!=' ')  
    strcpy(sError,"__error__");

  while(*pTail!='\0')
    *pHead++=*pTail++;
  *pHead='\0';

return;
}



void fix_hfr_double_header(char *pHead,int nBlank)
{
char *pTail;

  while(*pHead!=' '){
    if(*pHead=='\0')
      return;
    ++pHead;
  }
  pTail=pHead+nBlank;

  if(*pHead!=' ')  
    strcpy(sError,"__error__");
  if(*pTail!=' ')  
    strcpy(sError,"__error__");

  while(*pTail!='\0')
    *pHead++=*pTail++;
  *pHead='\0';

return;
}
