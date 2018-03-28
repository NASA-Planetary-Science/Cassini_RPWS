/* Modification history:
 *
 *  - Original - Robert Johnson about 1996
 *  
 *  - Updated to handle byteswapping when reading and writing packet 
 *    lengths - Chris Piker 2016-12-22
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "telemetry.h"

/* static char packet_in_filename[128];	 */
static FILE* packet_in_handle = NULL;
static int32_t packet_in_pos = 0;
/* static char packet_out_filename[128];	 */
static FILE* packet_out_handle = NULL;
static int32_t packet_out_pos = 0;
 

/* ************************************************************************* */
/* Byte Swapping */

/* Read big-endian Packet lengths (4 byte signed integers)  
 *
 * Returns 1 on success, 0 otherwise */ 
size_t read_pkt_len(int32_t* pBuf, FILE* pIn)
{
	unsigned char buf[4];
	size_t uRead = fread(buf, 4, 1, pIn);
	pBuf[0] = (buf[0] << 24) + (buf[1] << 16) + (buf[2] << 8) + buf[3];
	return uRead;
}

/* Swap 4 bytes in place */
void swap4(int32_t* pI)
{
	char c;
	unsigned char* pC = (unsigned char*)pI;
	
	c = pC[0]; pC[0] = pC[3]; pC[3] = c;
	c = pC[1]; pC[1] = pC[2]; pC[2] = c;
}

/* ************************************************************************* */
/* handling telemetry files */

char *open_telemetry_infile(char *filename)
{
static char errmsg[128];

  if(!strcmp(filename,"stdin")){
    packet_in_handle=stdin;
    }
  else{
    if((packet_in_handle=fopen(filename,"rb"))==NULL){
      sprintf(errmsg,"Error : Unable to open PACKET file %s",filename);
      return errmsg;
      }
    }

return NULL;
}

int32_t close_telemetry_infile(char *filename)
{

  if(!strcmp(filename,"stdin"))
    /* do nothing */;
  else
    if(packet_in_handle) fclose(packet_in_handle);
  packet_in_handle=NULL;

return 0;
}
 
char *open_telemetry_outfile(char *filename)
{
static char errmsg[128];

  if(!strcmp(filename,"stdout")){
    packet_out_handle=stdout;
    }
  else{
    if((packet_out_handle=fopen(filename,"wb"))==NULL){
      sprintf(errmsg,"Error : Unable to open PACKET file %s",filename);
      return errmsg;
      }
    }

return NULL;
}

int32_t close_telemetry_outfile(char *filename)
{

  if(!strcmp(filename,"stdout"))
    /* do nothing */;
  else
    fclose(packet_out_handle);
  packet_out_handle=NULL;

return 0;
}


/* ************************************************************************* */


int32_t get_previous_packet(TELEMETRY *id)
{
int32_t *buffer,ilen,total;

  if(packet_in_handle==NULL)
    return PACKET_NOT_OPEN;
    
  buffer=(int32_t*)id;
  packet_in_pos=ftell(packet_in_handle);  			/* Place Marker */

  /* Move to Beginning of Current Packet */
  total=packet_in_pos-4;
  if(total<0){						/* Past Beginning of File ? */
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_BOF;
    }
  fseek(packet_in_handle,(-4),SEEK_CUR);
  /*ilen=fread(buffer,4,1,packet_in_handle);*/		
  ilen = read_pkt_len(buffer, packet_in_handle);  /* Current Packet r_length */
  if (ilen!=1){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_READ_ERROR;
    }
  total-=buffer[0];
  if(total<0){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_BOF;
    }
  fseek(packet_in_handle,-(buffer[0]+4),SEEK_CUR);	/* Beginning of Current Packet */

  /* Move to Beginning of Previous Packet */
  total-=4;
  if(total<0){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_BOF;
    }
  fseek(packet_in_handle,(-4),SEEK_CUR);
  /*ilen=fread(buffer,4,1,packet_in_handle);*/	/* Previous Packet r_length */
  ilen = read_pkt_len(buffer, packet_in_handle);
  if (ilen!=1){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_READ_ERROR;
    }
  total-=buffer[0];
  if(total<0){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_BOF;
    }
  fseek(packet_in_handle,-(buffer[0]+4),SEEK_CUR);	/* Begining of Previous Packet */

  /* Read Previous Packet */
  /* ilen=fread(buffer,4,1,packet_in_handle); */	/* Read Previous f_length */
  ilen = read_pkt_len(buffer, packet_in_handle);
  if (ilen!=1){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_READ_ERROR;
    }
  ilen=fread(&buffer[1],(unsigned)buffer[0],1,packet_in_handle);
  if (ilen!=1){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_READ_ERROR;
    }
      
return PACKET_READ_SUCCESS;
}



int32_t get_current_packet(TELEMETRY *id)
{
int32_t *buffer,ilen,total;

  if(packet_in_handle==NULL)
    return PACKET_NOT_OPEN;
    
  buffer=(int32_t*)id;
  packet_in_pos=ftell(packet_in_handle);

  /* Move to Beginning of Current Packet */
  total=packet_in_pos-4;				
  if(total<0){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_BOF;
    }
  fseek(packet_in_handle,(-4),SEEK_CUR);
  /* ilen=fread(buffer,4,1,packet_in_handle); */
  ilen = read_pkt_len(buffer, packet_in_handle);
  if(ilen!=1){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_READ_ERROR;
    }
  total-=buffer[0];
  if(total<0){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_BOF;
    }
  fseek(packet_in_handle,-(buffer[0]+4),SEEK_CUR);	/* Packet Beginning */

  /* Read Current Packet */
  /* ilen=fread(buffer,4,1,packet_in_handle); */
  ilen = read_pkt_len(buffer, packet_in_handle);
  if(ilen!=1){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_READ_ERROR;
    }
  ilen=fread(&buffer[1],(uint32_t)buffer[0],1,packet_in_handle);
  if(ilen!=1){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_READ_ERROR;
    }
       
return PACKET_READ_SUCCESS;
}

int32_t get_next_packet(TELEMETRY *id)
{
int32_t *buffer;
size_t ilen;

  if(packet_in_handle==NULL)
    return PACKET_NOT_OPEN;
    
  buffer=(int32_t*)id;
  packet_in_pos=ftell(packet_in_handle);
  
  /* Read Next Packet */
  ilen = read_pkt_len(buffer, packet_in_handle);
  if(ilen!=1){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_EOF;
  }
  
  ilen=fread(&(buffer[1]),(uint32_t)buffer[0],1,packet_in_handle);
  if(ilen!=1){
    fseek(packet_in_handle,packet_in_pos,SEEK_SET);
    return PACKET_EOF;
    }
       
return PACKET_READ_SUCCESS;
}

int32_t put_packet(TELEMETRY *id)
{
int32_t *buffer,olen;

  if(packet_out_handle==NULL)
    return PACKET_WRITE_ERROR;
    
  buffer=(int32_t*)id;
  packet_out_pos=ftell(packet_out_handle);
  olen=buffer[0];
  /* Write Packet, write length as big endian */
  swap4(buffer);
  olen=fwrite(buffer,olen,1,packet_out_handle);
  if(olen!=1){
    fseek(packet_out_handle,packet_out_pos,SEEK_SET);
    return PACKET_WRITE_ERROR;
    }
  olen=fwrite(buffer,4,1,packet_out_handle);			/* Reverse Lenght */
  if(olen!=1){
    fseek(packet_out_handle,packet_out_pos,SEEK_SET);
    return PACKET_WRITE_ERROR;
    }
  fflush(packet_out_handle);
  
  /* now convert length back to native endian */
return PACKET_WRITE_SUCCESS;
}


void stamp_telemetry_packet_lengths(TELEMETRY *x,int32_t length)
{
  put_telemetry_packet_length(x,length);
  x->forward_length=3*sizeof(int32_t)+256*sizeof(char)+length*sizeof(char);
  if(length>4098)	length=0;
  else 	  		length-=3;
  put_mini_packet_length((&(x->packet.usmp)),length);

return;
}



/*
PATH=/data/cassini/data_sopc
RAW=t970319.r01
MP=t970130.m01
MPUS=t961010.u02
KRONOS=kronos.068
*/

char *get_current_file(char *path,char *type)
{
char *pch,inln[256];
char *pstart,*pend; /*,typerc[64];*/
int len;
FILE *inhandle;
static char pathname[256],filename[256];

  if((inhandle=fopen(path,"rt"))==NULL){
    return NULL;
    }
  else{
    filename[0]='\0';
    pathname[0]='\0';
    while( (pch=fgets(inln,255,inhandle)) ){
      pstart=pend=pch;
      while(*pstart==' ' || *pstart=='\t')  ++pstart;              /* parse whitespaces */
      pend=pstart;
      while(*pend!=' ' && *pend!='\t' && *pend!='=' && *pend){     /* set termination */
        ++pend;     
        }
      if(*pend=='\0')  return NULL;
      *pend='\0';
      pch=pend+1;
      pend=pch;
      while(*pend!=' ' && *pend!='\t' && *pend!='\n' && *pend)  ++pend;   /* set termination */
      *pend='\0';
      if(!strcmp(pstart,"PATH"))      strcpy(pathname,pch);
      else if(!strcmp(pstart,type))   strcpy(filename,pch);
      }/* while */
    len=strlen(pathname);
    if(pathname[len-1]!='/'){
      pathname[len]='/';  
      pathname[len+1]='\0';
      }
    if((pch=strstr(filename,"\n")))  *pch='\0';
    pch=strcat(pathname,filename);
    }
  
  fclose(inhandle);
      
return pch;
}
