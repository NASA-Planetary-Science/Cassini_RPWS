#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "telemetry.h"

 

/*

			Record File Format (binary file)
  byte0 byte1 byte2 byte3	forward length	= N in bytes
  byte4 ... byteN-1		data
  byteN byteN+1 byteN+2 byteN+3	reverse length	= N in bytes


*/




void init_RecordFile(RecordFile *rf)
{
  rf->filename=NULL;
  rf->type=NULL;
  rf->errmsg[0]='\0';
  rf->status=RECORD_NOT_OPEN;
  rf->position=0;
  rf->handle=NULL;

return;
}

char *open_RecordFile(RecordFile *rf,char *filename,char *type)
{
char *errmsg;

  if(rf==NULL || filename==NULL || type==NULL)  return NULL;
  
  if(rf->handle!=NULL){				/* There is an open file */
    if((errmsg=close_RecordFile(rf))!=NULL)
      return errmsg;
    }

  if((rf->filename=(char*)malloc(sizeof(char)*(strlen(filename)+1)))==NULL){
    sprintf(rf->errmsg,"Unable to malloc, open_record_file(), %s\n",filename);
    return rf->errmsg;
    }
  else{
    strcpy(rf->filename,filename);
    }
  if((rf->type=(char*)malloc(sizeof(char)*(strlen(type)+1)))==NULL){
    sprintf(rf->errmsg,"Unable to malloc, open_record_file(), %s\n",type);
    return rf->errmsg;
    }
  else{
    strcpy(rf->type,type);
    }

  if(!strcmp(rf->filename,"stdin"))
    rf->handle=stdin;
  else if(!strcmp(rf->filename,"stdout"))
    rf->handle=stdout;
  else if(!strcmp(rf->filename,"stderr,"))
    rf->handle=stderr;
  else{
    if((rf->handle=fopen(rf->filename,rf->type))==NULL){
      sprintf(rf->errmsg,"Unable to open record file %s",rf->filename);
      return rf->errmsg;
      }
    }
    
  rf->position=ftell(rf->handle);  			/* Place Marker */
  rf->status=RECORD_BOF;

return NULL;
}

char *close_RecordFile(RecordFile *rf)
{

  if(rf==NULL || rf->filename==NULL || rf->type==NULL)  return NULL;

  if(!strcmp(rf->filename,"stdin"))
    /* do nothing */;
  else if(!strcmp(rf->filename,"stdout"))
    /* do nothing */;
  else if(!strcmp(rf->filename,"stderr"))
    /* do nothing */;
  else{
    if(fclose(rf->handle)!=0){
      sprintf(rf->errmsg,"Unable to close record file %s",rf->filename);
      return rf->errmsg;
      }
    }
  free(rf->filename);
  free(rf->type);
  init_RecordFile(rf);

return NULL;
}
 
int32_t get_previous_record_RecordFile(RecordFile *rf,void *pv)
{
int32_t length;

  if(rf==NULL || pv==NULL)  return 0;

  move_previous_record_RecordFile(rf);	/* Move to Beginning of Prevous Record */
  move_previous_record_RecordFile(rf);	/* Move to Beginning of Prevous Record */
  if(rf->status!=RECORD_SEEK_SUCCESS)
    length=0;
  else
    length=get_next_record_RecordFile(rf,pv);

return length;
}

int32_t get_current_record_RecordFile(RecordFile *rf,void *pv)
{
int32_t length;

  if(rf==NULL || pv==NULL)  return 0;

  length=move_previous_record_RecordFile(rf);		/* Move to Beginning of Current Record */
  if(rf->status!=RECORD_SEEK_SUCCESS)
    length=0;
  else
    length=get_next_record_RecordFile(rf,pv);

return length;
}

int32_t get_next_record_RecordFile(RecordFile *rf,void *pv)
{
int32_t *buffer=(int32_t*)pv;
int32_t length;

  if(rf==NULL || pv==NULL)  return 0;

  if(rf->handle==NULL){
    rf->status=RECORD_NOT_OPEN;
    length=0;
    }
  else{
    rf->position=ftell(rf->handle);
    if(1!=fread(buffer,4,1,rf->handle)){
      fseek(rf->handle,rf->position,SEEK_SET);
      rf->status=RECORD_EOF;
      length=0;
      }
    else if(1!=fread((buffer+1),(unsigned)(*buffer),1,rf->handle)){
      fseek(rf->handle,rf->position,SEEK_SET);
      rf->status=RECORD_EOF;
      length=0;
      }
    else{
      rf->status=RECORD_READ_SUCCESS;
      length=*buffer;
      }
    }   

return length;
}

int32_t move_previous_record_RecordFile(RecordFile *rf)
{
int32_t length,total;

  if(rf==NULL)  return 0;

  rf->position=ftell(rf->handle);

  if((total=rf->position-4)<0){
    rf->status=RECORD_BOF;
    length=0;
    }
  else{
    fseek(rf->handle,total,SEEK_SET);			/* (-4),SEEK_CUR */
    if(1!=fread((&length),4,1,rf->handle)){		/* reverse length */
      fseek(rf->handle,rf->position,SEEK_SET);		
      rf->status=RECORD_READ_ERROR;
      length=0;
      }
    else if((total=rf->position-(length+8))<0){
      fseek(rf->handle,rf->position,SEEK_SET);
      rf->status=RECORD_BOF;
      length=0;
      }
    else{
      fseek(rf->handle,total,SEEK_SET);			
      if(1!=fread((&length),4,1,rf->handle)){		/* reverse length */
        fseek(rf->handle,rf->position,SEEK_SET);	
        rf->status=RECORD_READ_ERROR;
        length=0;
        }
      else{
        rf->position=ftell(rf->handle);
        rf->status=RECORD_SEEK_SUCCESS;
        }
      }  /* 1st layer else */
    } /* main else */ 

return length;
}

int32_t move_current_record_RecordFile(RecordFile *rf)
{
int32_t length,total;

  if(rf==NULL)  return 0;

  rf->position=ftell(rf->handle);

  if((total=rf->position-4)<0){
    rf->status=RECORD_BOF;
    length=0;
    }
  else{
    fseek(rf->handle,(rf->position-4),SEEK_SET);	/* (-4),SEEK_CUR */
    if(1!=fread((&length),4,1,rf->handle)){		/* reverse length */
      fseek(rf->handle,rf->position,SEEK_SET);
      rf->status=RECORD_READ_ERROR;
      length=0;
      }
    else{
      rf->position=ftell(rf->handle);			/* Should be where we began */
      rf->status=RECORD_SEEK_SUCCESS;
      }
    }  

return length;
}

int32_t move_next_record_RecordFile(RecordFile *rf)
{
int32_t length;

  if(rf==NULL)  return 0;

  rf->position=ftell(rf->handle);

  if(1!=fread((&length),4,1,rf->handle)){	/* forward length */
    fseek(rf->handle,rf->position,SEEK_SET);
    rf->status=RECORD_EOF;
    length=0;
    }
  else{
    fseek(rf->handle,(rf->position+length),SEEK_SET);
    rf->status=RECORD_SEEK_SUCCESS;
    }

return length;
}










/***********************************
int32_t put_pt(TELEMETRY *id)
{
int32_t *buffer,olen;

  if(packet_out_handle==NULL)
    return PACKET_WRITE_ERROR;
    
  buffer=(int32_t*)id;
  packet_out_pos=ftell(packet_out_handle);
  olen=buffer[0];

  olen=fwrite(buffer,olen,1,packet_out_handle);
  if(olen!=1){
    fseek(packet_out_handle,packet_out_pos,SEEK_SET);
    return PACKET_WRITE_ERROR;
    }
  olen=fwrite(buffer,4,1,packet_out_handle);			
  if(olen!=1){
    fseek(packet_out_handle,packet_out_pos,SEEK_SET);
    return PACKET_WRITE_ERROR;
    }
  fflush(packet_out_handle);
  
return PACKET_WRITE_SUCCESS;
}
************************************/




/***********************************
PATH=/data/cassini/data_sopc
RAW=t970319.r01
MP=t970130.m01
MPUS=t961010.u02
KRONOS=kronos.068
************************************/

/***********************************
char *the_dash_get_command_line_option(char *path,char *type)
{
char *pch,inln[256];
int len;
FILE *inhandle;
static char pathname[256],filename[256];

  if((inhandle=fopen(path,"rt"))==NULL){
    return NULL;
    }
  else{
    filename[0]=NULL;
    pathname[0]=NULL;
    while(pch=fgets(inln,255,inhandle)){
      if(pch=strstr(inln,"PATH="))    strcpy(pathname,(pch+5));
      else if(pch=strstr(inln,type))  strcpy(filename,(pch+strlen(type)));
      }
    if(pch=strstr(pathname,"\n"))  *pch='/';	
    if(pch=strstr(filename,"\n"))  *pch=NULL;
    pch=strcat(pathname,filename);
    }
      
return pch;
}
************************************/
