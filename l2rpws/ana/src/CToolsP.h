/* Private Access File */

typedef struct linked_list_tag{
  struct linked_list_tag *next,*previous;
  void *object;
  }CLinkedList_raj;			

/* Whoa brother, this isn't tread safe... -cwp */
void *pVoid_raj,*pVoid0;

void* create_element_raj(CLinkedList_raj* newguy,void *pv);
/*void* init_element_raj(CLinkedList_raj* newguy);*/
int add_element_raj(CLinkedList_raj* list,CLinkedList_raj* newguy);
void* remove_element_raj(CLinkedList_raj* goner);
void* head_element_raj(CLinkedList_raj* list);
void* tail_element_raj(CLinkedList_raj* list);
void* delete_element_raj(CLinkedList_raj* goner);
void* equals_element_raj(CLinkedList_raj* list,void *src,size_t n);

#define CreateListElement(x)		( pVoid_raj=calloc(1,sizeof(x)) ) ? (x*)create_element_raj(&(((x*)pVoid_raj)->clinkedlist_raj),pVoid_raj) : NULL
/*
#define CreateListElement(x)		(x*)calloc(1,sizeof(x))
*/
/* InitListElement should no be handled by CreateListElement */
/*
#define InitListElement(x)		x->clinkedlist_raj.object=(void*)x; \
					x->clinkedlist_raj.previous=x->clinkedlist_raj.next=NULL
*/
#define AddListElement(x,y)		((x==NULL || y==NULL)?0:add_element_raj(&(x->clinkedlist_raj),(CLinkedList_raj *)(&(y->clinkedlist_raj))))
#define RemoveListElement(x)		((x==NULL)?NULL:remove_element_raj(&(x->clinkedlist_raj)))
#define HeadListElement(x)		((x==NULL)?NULL:head_element_raj(&(x->clinkedlist_raj)))
#define TailListElement(x)		((x==NULL)?NULL:tail_element_raj(&(x->clinkedlist_raj)))
#define NextListElement(x)		((x==NULL || x->clinkedlist_raj.next==NULL)?NULL:x->clinkedlist_raj.next->object)
#define PreviousListElement(x)		((x==NULL || x->clinkedlist_raj.previous==NULL)?NULL:x->clinkedlist_raj.previous->object)
#define DeleteListElement(x)		((x==NULL)?NULL:delete_element_raj(&(x->clinkedlist_raj)))
#define EqualsListElement(x)		equals_element_raj( &(x->clinkedlist_raj),(void*)x,sizeof((*(x))) )



void *equals_raj(void *p,size_t n);

#define Equals(x)			equals_raj(x,sizeof((*x)))
