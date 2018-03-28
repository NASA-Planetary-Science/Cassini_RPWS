#include <stdio.h>
#include <stdlib.h>

#include "CTools.h"


void *create_element_raj(CLinkedList_raj* newguy,void *pv)
{
  newguy->object=pv;
  newguy->previous=newguy->next=NULL;
  
return pv;
}

int add_element_raj(CLinkedList_raj* list,CLinkedList_raj* newguy)
{
  if(list==NULL || newguy==NULL)  return 0;
   
  while(list->next)  list=list->next;
  list->next=newguy;
  newguy->previous=list;

return 1;
}

void *delete_element_raj(CLinkedList_raj* goner)
{
void *pv;

  pv=remove_element_raj(goner);
  free(goner->object);
  
return pv;
}

void *remove_element_raj(CLinkedList_raj* goner)
{
void *pv;

  if(goner==NULL)  return 0;
  
  if(goner->previous && goner->next){		/* Middle of the List */	
    goner->previous->next=goner->next;		/* Forward Link */
    goner->next->previous=goner->previous;	/* Reverse Link */
    pv=goner->next->object;
    }
  else if(goner->next){				/* Head of the List */
    goner->next->previous=NULL;			/* New Head of the List */
    pv=goner->next->object;
    }
  else if(goner->previous){			/* Tail of the List */
    goner->previous->next=NULL;			/* New Tail of the List */
    pv=NULL;
    }
  else{						/* Orphan */
    pv=NULL;					
    }
  goner->next=NULL;
  goner->previous=NULL;				/* Back to Init state */

return pv;					/* next object in list */
}

void* head_element_raj(CLinkedList_raj* list)
{
  if(list==NULL)  return NULL;
  
  while(list->previous)  
    list=list->previous;  

return list->object;
}

void* tail_element_raj(CLinkedList_raj* list)
{
  if(list==NULL)  return NULL;
  
  while(list->next)
    list=list->next;  

return list->object;
}

/* Creates an exact copy, but is not hooked to the list */
void *equals_element_raj(CLinkedList_raj *src_list,void *src,size_t n)
{
unsigned long offset;
CLinkedList_raj *dst_list;
void *dst;

  if(src==NULL || n<0)  return NULL;
  
  if((dst=calloc(1,n))==NULL){	/* CreateListElement */
    fprintf(stderr,"Unable to calloc(), EqualsListElement(%zu)\n",n);
    }
  else{
    memcpy(dst,src,n);		
    }
    
  /* 
    I know that src has the qualities of a linked list, and I know that the dst
    must also.  I know where the linked list stuff is in the src, but not in 
    the dst.  The offset in the src must equal the offset in the dst.  This is 
    safe if the structure changes. 
  */
  offset=(unsigned long)src_list - (unsigned long)src;
  dst_list=(CLinkedList_raj*)((unsigned long)dst + offset);
  
  /* Now this is possible */
  dst_list->previous=NULL;
  dst_list->next=NULL;
  dst_list->object=dst;
	
return dst;
}

