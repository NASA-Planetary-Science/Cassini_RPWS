#include <stdio.h>
#include <stdlib.h>

#include <das2/das1.h>

#include "rajTime.h"
/* 
#include <string.h>
#include <time.h>

Version 1.0
  Wednesday, Janurary 12, 2005

*/



/* functions accepting normalized values */
int leap_year(int nYear);
int days_in_month(int nYear,int nMonth);
int day_of_week(int nYear,int nDoy);

/* functions accepting unnormalized values */
void doy_to_monthdom(int nDoy,int nYear,int *nMonth,int *nDom);
double dom_to_doy(double dYear,double dMonth,double dDom);



/*                        J  F  M  A  M  J  J  A  S  O  N  D */
static int arDim[12]    ={31,28,31,30,31,30,31,31,30,31,30,31};
static int arDimLeap[12]={31,29,31,30,31,30,31,31,30,31,30,31};

static const int nDaysInMonth[12]={31,28,31,30,31,30,31,31,30,31,30,31};

static const char *sDayOfWeek[7]={
  "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
};
static const char *sMonthOfYear[12]={
  "Janurary","Feburary","March","April","May","June","July","August",
  "September","October","November","December"
};



/*
  returns:
    0 - not iso(T) format
    1 - iso(T) format, yyyy-doyThh:mm:ss.msc
    2 - iso(T) format, yyyy-month-dayThh:mm:ss.msc
*/
int fxParseTime(char *string,rajTime *t)
{
int nIsoT;

  nIsoT=rajParseTime(string,t);
  t->dDoy=t->nDoy=t->dDoy+1.0;
  t->dMonth=t->nMonth=t->dMonth+1.0;
  t->dDom=t->nDom=t->dDom+1.0;

return nIsoT;
}



void fxNormalize(rajTime *t)
{

  t->dDoy-=1.0;
  t->dMonth-=1.0;
  t->dDom-=1.0;

  rajNormalize(t);

  t->dDoy+=1.0;
  t->dMonth+=1.0;
  t->dDom+=1.0;

  t->nDoy=t->dDoy;
  t->nMonth=t->dMonth;
  t->nDom=t->dDom;

return;
}




/*
  returns:
    0 - not iso(T) format
    1 - iso(T) format, yyyy-doyThh:mm:ss.msc
    2 - iso(T) format, yyyy-month-dayThh:mm:ss.msc
*/
int rajParseTime(char *string,rajTime *t)
{
int nIsoT;
int nYear,nMonth,nDom,nDoy,nHour,nMinute;
double dSec;

  if((nIsoT=rajParseIsoTFormat(string,t))==0){  /* NOT IS0-D Format */
    parsetime(string,&nYear,&nMonth,&nDom,&nDoy,&nHour,&nMinute,&dSec);
    if(nDoy>0)    nDoy-=1;
    if(nMonth>0)  nMonth-=1;
    if(nDom>0)    nDom-=1;
    t->dYear=nYear;
    t->dDoy=nDoy;
    t->dMonth=nMonth;
    t->dDom=nDom;
    t->dHour=nHour;
    t->dMinute=nMinute;
    t->dSecond=dSec;
  }

  rajNormalize(t);

return nIsoT;
}



int leap_year(int nYear)
{
int nLeap;

  if(nYear%100){   /* year is NOT a century year */
    if(nYear%4)    /* NOT evenly divisible by 4 */
      nLeap=365;
    else             
      nLeap=366;
  }
  else{            /* year IS a century year */
    if(nYear%400)  /* 1900 is not a leap year */
      nLeap=365;
    else           /* 2000 is a leap year */
      nLeap=366;
  }

return nLeap;
}



void doy_to_monthdom(int nDoy,int nYear,int *nMonth,int *nDom)
{
int *p;

  if((nDoy<0) || (nDoy>=leap_year(nYear))){
    fprintf(stderr,"fxtime() error - doy_to_monthdom(%d,%d,%p,%p)\n",
            nDoy,nYear,nMonth,nDom);
    exit(1);
  }

  if(leap_year(nYear) == 366)
    p=arDimLeap;
  else
    p=arDim;

  *nMonth=0;  *nDom=0;
  while(nDoy>=p[*nMonth])
    nDoy-=p[(*nMonth)++];
  *nDom=nDoy;

return;
}



/* Jan. 1, 0000 is a Saturday, Jan. 1, 001 is a Monday */
/* Doy should be normalized at this point */
int day_of_week(int nYear,int nDoy)
{
int norm,leap,dow;

  if(nYear==0){
    leap=0;
    norm=0;
  }
  else if(nYear==1){
    leap=1;
    norm=0;
  }
  else{
    nYear-=1;
    leap=(nYear/4);         /* maximum number of leap years since 1 B.C. */
    leap=leap-(nYear/100);  /* subtract bogus leap year, century years   */
    leap=leap+(nYear/400);  /* add back real century leap years          */
    norm=nYear-leap;        /* normal years                              */
    leap+=1;                /* count leap year in 1 B.C.                 */
  }

  dow=leap*2;    /* day advancement for leap years                           */
  dow=dow+norm;  /* total day advancement, normal and leap years             */
  dow=dow+7;     /* transform days advanced from saturday to the day of week */
                 /* number: sun=1, mon=2, tue=3, wed=4, thu=5, fri=6, sat=7  */
                 /* dow is the day of week for Janurary 1, xxxx              */
  dow=dow+nDoy;  /* add in current day of year */
  dow=dow%7;
  if(dow==0)
    dow=7;

  dow-=1;  /* normalize day of week to zero: sun=0, mon=1, tues=2, ... */

return dow;
}



/* assumes year, month, day are normalized to zero */
double dom_to_doy(double dYear,double dMonth,double dDom)
{
int i;
double dDoy=0.0;
int *p;


  while(dMonth>12.0){
    dDoy+=(double)leap_year((int)dYear); 
    dMonth-=12.0;
    dYear+=1.0;
  }
  while(dMonth<0.0){
    dYear-=1.0;
    dDoy-=(double)leap_year((int)dYear); 
    dMonth+=12.0;
  }
  if(leap_year((int)dYear)==366)
    p=arDimLeap;
  else
    p=arDim;

  for(i=0;i<(int)dMonth;i++)
   dDoy+=p[i]; 
  dDoy+=(dMonth-(int)dMonth)*p[i]; 

  dDoy+=dDom;


return dDoy;
}



/* 
  -> expects s to be of the form yyyy-doyThh:mm:ss.msc 
  -> normalizes doy to zero
  -> returns 1 if in iso-d format, otherwise 0
*/
int rajParseIsoTFormat(char *s,rajTime *t)
{
char *pBeg,*pEnd;


  t->dDoy=t->dMonth=t->dDom=0.0;

  /* decode year */
  pBeg=s;
  if( (t->dYear=strtod(pBeg,&pEnd)) == 0.0)
    t->dYear=strtol(pBeg,&pEnd,0);

  while(*pEnd==' ' || *pEnd=='\t')  ++pEnd;
  if(*pEnd=='\0')      return 1;
  else if(*pEnd!='-')  return 0;
  else                 ++pEnd;
  pBeg=pEnd;
  if( (t->dDoy=strtod(pBeg,&pEnd)) == 0.0)
    t->dDoy=strtol(pBeg,&pEnd,0);
  if(t->dDoy>=1.0)
    t->dDoy-=1.0;  /* normalize doy to zero */

  while(*pEnd==' ' || *pEnd=='\t')  ++pEnd;  /* allow yyyy-month-dayThh:mm:ss*/
  if(*pEnd=='\0')      ;
  else if(*pEnd=='-'){  /* last token was month, this is day of month */
    t->dMonth=t->dDoy;  /* already normalized to zero */
    t->dDoy=0.0;        
    pBeg=++pEnd;
    if( (t->dDom=strtod(pBeg,&pEnd)) == 0.0)
      t->dDom=strtol(pBeg,&pEnd,0);
    if(t->dDom>=1.0)
      t->dDom-=1.0;  /* normalize dom to zero */
  }

  while(*pEnd==' ' || *pEnd=='\t')  ++pEnd;
  if(*pEnd=='\0')      ;
  else if(*pEnd!='T')  return 0;
  else                 ++pEnd;
  pBeg=pEnd;
  if( (t->dHour=strtod(pBeg,&pEnd)) == 0.0)
    t->dHour=strtol(pBeg,&pEnd,0);

  while(*pEnd==' ' || *pEnd=='\t')  ++pEnd;
  if(*pEnd=='\0')      ;
  else if(*pEnd!=':')  return 0;
  else                 ++pEnd;
  pBeg=pEnd;
  if( (t->dMinute=strtod(pBeg,&pEnd)) == 0.0)
    t->dMinute=strtol(pBeg,&pEnd,0);

  while(*pEnd==' ' || *pEnd=='\t')  ++pEnd;
  if(*pEnd=='\0')      ;
  else if(*pEnd!=':')  return 0;
  else                 ++pEnd;
  pBeg=pEnd;
  if( (t->dSecond=strtod(pBeg,&pEnd)) == 0.0)
    t->dSecond=strtol(pBeg,&pEnd,0);

  /* deal with day of year -vs- month/day of month */
  if((t->dMonth != 0.0) || (t->dDom != 0.0)){
    t->dDoy=dom_to_doy(t->dYear,t->dMonth,t->dDom);
    return 2;
  }

return 1;
}



/* assume years and days have been normalized to include zero in the range   */
/* only works with day of year, NOT month/day of month; but will write month */
/* and day of month to the structure.                                        */
void rajNormalize(rajTime *t)
{
int nMonth,nDom;
double dLeap;


  /* fractions of time */
  t->nYear=t->dYear;
  t->dDoy+=(t->dYear-t->nYear)*leap_year(t->nYear);
  t->dYear=t->nYear;

  t->nDoy=t->dDoy;
  t->dHour+=(t->dDoy-t->nDoy)*24.0;
  t->dDoy=t->nDoy;

  t->nHour=t->dHour;
  t->dMinute+=(t->dHour-t->nHour)*60.0;
  t->dHour=t->nHour;

  t->nMinute=t->dMinute;
  t->dSecond+=(t->dMinute-t->nMinute)*60.0;
  t->dMinute=t->nMinute;


  while(t->dSecond>=60.0){
    t->dSecond-=60.0;
    t->dMinute+=1.0;
  }
  while(t->dSecond<0.0){
    t->dSecond+=60.0;
    t->dMinute-=1.0;
  }

  while(t->dMinute>=60.0){
    t->dMinute-=60.0;
    t->dHour+=1.0;
  }
  while(t->dMinute<0.0){
    t->dMinute+=60.0;
    t->dHour-=1.0;
  }

  while(t->dHour>=24.0){
    t->dHour-=24.0;
    t->dDoy+=1;
  }
  while(t->dHour<0.0){
    t->dHour+=24.0;
    t->dDoy-=1.0;
  }

  dLeap=(double)leap_year((int)t->dYear);
  while(t->dDoy>=dLeap){
    t->dDoy-=dLeap;
    t->dYear+=1.0;
    dLeap=(double)leap_year((int)t->dYear);
  }
  while(t->dDoy<0.0){
    t->dYear-=1.0;
    dLeap=(double)leap_year((int)t->dYear);
    t->dDoy+=dLeap;
  }

  /* all should be normalized */
  t->nYear=t->dYear;
  t->nDoy=t->dDoy;
  t->nHour=t->dHour;
  t->nMinute=t->dMinute;
  t->nSecond=t->dSecond;
  t->nMsec=(t->dSecond-t->nSecond)*1000.0;

  doy_to_monthdom(t->nDoy,t->nYear,&nMonth,&nDom);
  t->dMonth=t->nMonth=nMonth;
  t->dDom=t->nDom=nDom;

  t->nDow=day_of_week(t->nYear,t->nDoy);



return;
}
