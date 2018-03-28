#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>



#ifndef _Bool
#define _Bool
typedef enum {False,True} Bool; 
#endif

#ifndef _Uchar
#define _Uchar
typedef unsigned char Uchar;  /* char is assumed to be 8 bits */ 
#endif 

#ifndef _Uint
#define _Uint
typedef unsigned int Uint;    /* int is assumed to be 32 bits, msb first */ 
#endif

/* Bit patterns need to decode the 'nMode' field in the header */
#define RECEIVER_MASK    0xFF000000  /* */
#define MODE_MASK        0x00FFF000  /* */
#define ANT_MASK         0x00000FFF  /* */
#define LFDR_Normal      0x71000000  /* low frequency digital receiver */
#define MFDR_Normal      0x72000000  /* medium frequency digital receiver */
#define MFR_Normal       0x11000000  /* medium frequency receiver */
#define MFR_FastToggle   0x12000000  /* medium frequency receiver */
#define HFR_Analysis     0x21000000  /* high frequency receiver */
#define HFR_Millisecond  0x28000000  /* high frequency receiver */

#define HFR_BandABC      0x00007000  /* high frequency receiver */
#define HFR_BandHf1      0x00008000  /* high frequency receiver */
#define HFR_BandHf2      0x00010000  /* high frequency receiver */



static Bool Lil_Indian;  /* lsb first for intel machines */
static const char *sVersion="lrf_list(), version 1.3";

static const char *sHeaderFormat1="%21s %15s %17s\n";
static const char *sHeaderFormat2="%21s %15s %17s\n";
static const char *sDataFormat0="%21s %15.2f %#17.5G\n";
static const char *sDataFormat1="%21.3f %15.2f %#17.5G\n";
static const char *sDataFormat2="%21s %15.2f %#17.5G   (bandwidth=%9.3f)\n";
static const char *sDataFormat3="%21.3f %15.2f %#17.5G   (bandwidth=%9.3f)\n";

static float Lfdr_Bandwidth[32]={   /* in Hertz */
  0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 
  0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045,
  0.2045, 0.2045, 0.2045, 0.2871, 0.5719, 0.5719, 0.7373, 0.7373,
  1.0429, 1.0429, 1.3182, 1.4466, 1.8386, 2.0797, 2.5488, 2.6172 
};

static float Mfdr_Bandwidth[32]={  /* in Hertz */
  14.6071, 14.6071, 14.6071,  14.6071,  14.6071,  14.6071,  14.6071,  14.6071,
  14.6071, 14.6071, 14.6071,  14.6071,  14.6071,  14.6071,  14.6071,  14.6071,
  14.6071, 14.6071, 14.6071,  20.5071,  40.8500,  40.8500,  52.6643,  52.6643,
  74.4929, 74.4929, 94.1571, 103.3286, 131.3286, 148.5500, 182.0571, 186.9429
};



Bool Little_End_In_First(void);
char* nScet_to_sScet(Uint nDoy,Uint nMsec);
float *get_bandwidth(float *arFreq,Uint nItems,Uint nMode,Uchar *arMiniPkt);
float get_hfr_ABC_bandwidth(Uchar *arMiniPkt);
float get_hfr_HF1_bandwidth(Uchar *arMiniPkt);
float get_hfr_HF2_bandwidth(Uchar *arMiniPkt);
void show_help(FILE *h);



int main(int argc,char *argv[])
{
Bool bHeader=True,bData=True,bSilent=False,bVerbose=False;
char *sFileName,sReceiver[32],sSensor[32],sDataQuality[128];
char sUnits[32],sUnitsName[32];
Uchar *pBuf,sTmp[128],*arHdr;
int i,nUnits,nDataFormat;
Uint *pDword;
Uchar arMiniPkt[32];
Uint nRecLen,nNumRec,nItems,nMode,nDataQuality,nSensor;
Uint nSclkSec,nSclkFine,nScetDays,nScetMils,nSclkPart;
Uint nMsec;
float *arFreq,*arTime,*arDens,*arBandWidth;
float fAntLen,fBaseCap;
double dFreq,dResCor;
FILE *hFile;
size_t nRead;


/* Initialize variables */
  sFileName=NULL;
  nUnits=0;
  nDataFormat=2;

  while(--argc){
    ++argv;
    if(!strcmp("-d",*argv))  
      bData=False;               /* don't output data values */
    else if(!strcmp("-h",*argv))
      bHeader=False;             /* don't output headers */
    else if(!strcmp("-f",*argv)){
      --argc;  ++argv;
      nDataFormat=atoi(*argv);
    }
    else if(!strcmp("-u",*argv)){
      --argc;  ++argv;
      nUnits=atoi(*argv);
    }
    else if(!strcmp("-s",*argv))
      bSilent=True;
    else if(!strcmp("-v",*argv))
      bVerbose=True;
    else if(!strcmp("-help",*argv)){
      show_help(stdout);
      exit(0);
    }
    else{
      sFileName=*argv++; 
    }
  }

  if(bVerbose==True)  bSilent=False;

  /* check the byte order of the machine */
  Lil_Indian=Little_End_In_First();
  /* check to see if the sizes are what we think they are */
  if( sizeof(Uchar) != 1 ){
    fprintf(stderr,"unsigned char not one byte\n");
    exit(1);
  }
  if( sizeof(Uint) != 4 ){
    fprintf(stderr,"unsigned int not four byte\n");
    exit(1);
  }


  if(bSilent==False)
    fprintf(stdout,"%s\n",sVersion); 

  if(sFileName==NULL){
    sFileName=calloc(strlen("stdin")+1,sizeof(char));
    strcpy(sFileName,"stdin");
    hFile=stdin;
  }
  else if( (hFile=fopen(sFileName,"rb"))==NULL ){
    fprintf(stderr,"Unable to open %s.\n",sFileName);
    exit(1);
  }
  if(bVerbose==True)
    fprintf(stdout,"reading %s\n",sFileName);

  /* determine, the length and number of each record in the file */
  if( fread(sTmp,sizeof(Uchar),16,hFile) != 16 ){
    fprintf(stderr,"Error reading %s\n",sFileName);
    exit(1);
  }

  nRecLen=sTmp[8];   nRecLen<<=24;
  nRecLen|=((Uint)sTmp[9])<<16;  /* explicitly promote to an 32 bit unsigned */
  nRecLen|=((Uint)sTmp[10])<<8;  /* quanity for non-ansi C compiliers. */
  nRecLen|=((Uint)sTmp[11]);  
  nNumRec=sTmp[12];   nNumRec<<=24;
  nNumRec|=((Uint)sTmp[13])<<16;  
  nNumRec|=((Uint)sTmp[14])<<8;  
  nNumRec|=((Uint)sTmp[15]);  
  nItems=(nRecLen-16)/4;

  /* rewind file pointer to the beginning and read the entire header record */
  fseek(hFile,(int)0,SEEK_SET); 
  assert( (arHdr=calloc(nRecLen,sizeof(Uchar))) != NULL );
  if( fread(arHdr,sizeof(Uchar),nRecLen,hFile) != nRecLen ){
    fprintf(stderr,"Error reading %s\n",sFileName);
    exit(1);
  }
  nMode=arHdr[16];  nMode<<=24;   /* receiver type and mode */
  nMode|=((Uint)arHdr[17])<<16;  
  nMode|=((Uint)arHdr[18])<<8;  
  nMode|=((Uint)arHdr[19]);  
 
  for(i=0;i<24;i++)          /* mini-packet header, contains instrument mode */
    arMiniPkt[i]=arHdr[24+i];/* information, see RPWS Users Guide for details */

  switch(nMode&RECEIVER_MASK){
    case LFDR_Normal:
      sprintf(sReceiver,"Lfdr");
      break;
    case MFDR_Normal:
      sprintf(sReceiver,"Mfdr");
      break;
    case MFR_Normal:
      sprintf(sReceiver,"Mfr Normal");
      break;
    case MFR_FastToggle:
      sprintf(sReceiver,"Mfr Fast Toggle");
      break;
    case HFR_Analysis:
      sprintf(sReceiver,"Hfr Analysis");
      break;
    case HFR_Millisecond:
      sprintf(sReceiver,"Hfr Millisecond");
      break;
    default:
      sprintf(sReceiver,"%08X",nMode);
      break;
  }


  /* sanity check */
  assert( (pBuf=calloc(nRecLen,sizeof(Uchar))) != NULL );
  if(bVerbose==True){
    fprintf(stdout,"Record Length = %d (bytes), Number of Records = %d, "
                   "and nItems = %d\n",nRecLen,nNumRec,nItems);

    fprintf(stdout,"first 80 bytes of the archive\n");
    for(i=0;i<16;i++)
      fprintf(stdout,"%02X ",arHdr[i]);
    fprintf(stdout,"  ");
    for(i=0;i<16;i++)
      fprintf(stdout,"%c",((0x1F<arHdr[i])&&(arHdr[i]<0x7F)) ? arHdr[i] : '.');
    fprintf(stdout,"\n");

    for(i=16;i<32;i++)
      fprintf(stdout,"%02X ",arHdr[i]);
    fprintf(stdout,"  ");
    for(i=16;i<32;i++)
      fprintf(stdout,"%c",((0x1F<arHdr[i])&&(arHdr[i]<0x7F)) ? arHdr[i] : '.');
    fprintf(stdout,"\n");

    for(i=32;i<48;i++)
      fprintf(stdout,"%02X ",arHdr[i]);
    fprintf(stdout,"  ");
    for(i=32;i<48;i++)
      fprintf(stdout,"%c",((0x1F<arHdr[i])&&(arHdr[i]<0x7F)) ? arHdr[i] : '.');
    fprintf(stdout,"\n");

    for(i=48;i<64;i++)
      fprintf(stdout,"%02X ",arHdr[i]);
    fprintf(stdout,"  ");
    for(i=48;i<64;i++)
      fprintf(stdout,"%c",((0x1F<arHdr[i])&&(arHdr[i]<0x7F)) ? arHdr[i] : '.');
    fprintf(stdout,"\n");

    for(i=64;i<80;i++)
      fprintf(stdout,"%02X ",arHdr[i]);
    fprintf(stdout,"  ");
    for(i=64;i<80;i++)
      fprintf(stdout,"%c",((0x1F<arHdr[i])&&(arHdr[i]<0x7F)) ? arHdr[i] : '.');
    fprintf(stdout,"\n");

  }

  /* move to the beginning of the second record, time record */
  fseek(hFile,(int)nRecLen,SEEK_SET); 
  if( fread(pBuf,sizeof(Uchar),nRecLen,hFile) != nRecLen ){
    fprintf(stderr,"Error reading %s\n",sFileName);
    exit(1);
  }
  assert( (arTime=calloc(nItems,sizeof(float))) != NULL );

  if(Lil_Indian==True){  /* reorder the IEEE Float so the LSB is first */
  Uchar *p=pBuf;         /* b4 b3 b2 b1 becomes b1 b2 b3 b4 */
    for(i=0;i<nRecLen;i+=4,p+=4){
      *(p+0)^=*(p+3);  *(p+3)^=*(p+0);  *(p+0)^=*(p+3); 
      *(p+1)^=*(p+2);  *(p+2)^=*(p+1);  *(p+1)^=*(p+2); 
    }
  }

  /* skip the first 16 bytes of information, sclk, scet, etc and 
     put the time offset from beginning of capture in an array */
  memcpy(arTime,pBuf+16,nRecLen-16); 

  /* read the third record, frequency */
  if( fread(pBuf,sizeof(Uchar),nRecLen,hFile) != nRecLen ){
    fprintf(stderr,"Error reading %s\n",sFileName);
    exit(1);
  }
  assert( (arFreq=calloc(nItems,sizeof(float))) != NULL );

  if(Lil_Indian==True){  /* reorder the IEEE Float so the LSB is first */
  Uchar *p=pBuf;         /* b4 b3 b2 b1 becomes b1 b2 b3 b4 */
    for(i=0;i<nRecLen;i+=4,p+=4){
      *(p+0)^=*(p+3);  *(p+3)^=*(p+0);  *(p+0)^=*(p+3); 
      *(p+1)^=*(p+2);  *(p+2)^=*(p+1);  *(p+1)^=*(p+2); 
    }
  }

  memcpy(arFreq,pBuf+16,nRecLen-16);

  assert( (arDens=calloc(nItems,sizeof(float))) != NULL );
  while( (nRead=fread(pBuf,sizeof(Uchar),nRecLen,hFile)) == nRecLen ){

    if(Lil_Indian==True){  /* reorder the IEEE Float so the LSB is first */
    Uchar *p=pBuf;         /* b4 b3 b2 b1 becomes b1 b2 b3 b4 */
      for(i=0;i<nRecLen;i+=4,p+=4){
        *(p+0)^=*(p+3);  *(p+3)^=*(p+0);  *(p+0)^=*(p+3); 
        *(p+1)^=*(p+2);  *(p+2)^=*(p+1);  *(p+1)^=*(p+2); 
      }
    }

    /* the beginning of each record contains the time information */
    pDword=(Uint*)pBuf;
    nSclkSec=*pDword++;
    nSclkFine=((*pDword)>>16)&0x0FF;
    nSclkPart=((*pDword)>>24)&0x0FF;
    nScetDays=(*pDword++)&0x0FFFF;
    nScetMils=*pDword++;
    nDataQuality=*pDword++;
    nSensor=nDataQuality;
    nDataQuality&=0xFFFFFFF0;
    nSensor&=     0x0000000F;
    memcpy(arDens,pBuf+16,nRecLen-16);

    sDataQuality[0]='\0';
    if(nDataQuality&0x80000000)  strcat(sDataQuality,"Packet Errors ");
    if(nDataQuality&0x40000000)  strcat(sDataQuality,"Hfr Sounder Active ");
    if(nDataQuality&0x20000000)  strcat(sDataQuality,"Lp Raw Sweep Active ");
    if(nDataQuality&0x10000000)  strcat(sDataQuality,"Ground Generated Data ");

    if(nSensor<=3){  /* Eu,Ev,Ex,Ew */
      switch(nUnits){
        case 1:
          strcpy(sUnitsName,"Power Flux");
          strcpy(sUnits,"W/m**2/hz");
          break;
        case 2:
          strcpy(sUnitsName,"Electric Field");   
          strcpy(sUnits,"V/m");
          break;
        case 3:
          strcpy(sUnitsName,"Electric Density");   
          strcpy(sUnits,"V**2/hz");
          break;
        default:
          strcpy(sUnitsName,"Spectral Density"); 
          strcpy(sUnits,"V**2/m**2/hz");
          break;
      }/* hctiws */
    }
    else if((4<=nSensor) && (nSensor<=6)){  /* Bx,ByBz */
      switch(nUnits){
        case 1:
          strcpy(sUnitsName,"Power Flux");
          strcpy(sUnits,"nT**2/hz");
          break;
        case 2:
          strcpy(sUnitsName,"Magnetic Field");
          strcpy(sUnits,"nT");
          break;
        case 3:
          strcpy(sUnitsName,"Magnetic Density");
          strcpy(sUnits,"nT**2/hz");
          break;
        default:
          strcpy(sUnitsName,"Spectral Density");
          strcpy(sUnits,"nT**2/hz");
          break;
      }/* hctiws */
    }
    else{
      strcpy(sUnits,"TBD");
      switch(nUnits){
        case 1:  strcpy(sUnitsName,"Power Flux");        break;
        case 2:  strcpy(sUnitsName,"Magnetic Field");    break;
        case 3:  strcpy(sUnitsName,"Spectral Density");  break;
        default: strcpy(sUnitsName,"Spectral Density");  break;
      }/* hctiws */
    }

    switch(nSensor){
      case 0:  strcpy(sSensor,"Ex");  break;
      case 1:  strcpy(sSensor,"Eu");  break;
      case 2:  strcpy(sSensor,"Ev");  break;
      case 3:  strcpy(sSensor,"Ew");  break;
      case 4:  strcpy(sSensor,"Bx");  break;
      case 5:  strcpy(sSensor,"By");  break;
      case 6:  strcpy(sSensor,"Bz");  break;
      case 8:  strcpy(sSensor,"Hf");  break;
      case 11: strcpy(sSensor,"Lp");  break;
      default: strcpy(sSensor,"unknown");  break;
    }
    if(nSclkPart==0)  nSclkPart=1; 

    if(bHeader==True){
      fprintf(stdout,"%s %s %s 0x%08X.%02X\n",sSensor,sReceiver,
              nScet_to_sScet(nScetDays,nScetMils),nSclkSec,nSclkFine);
      if(sDataQuality[0]!='\0')
        fprintf(stdout,"  %s\n",sDataQuality);
    }
    
    if(bData==True){
      if(bHeader==True){
        fprintf(stdout,sHeaderFormat1,"S/C Event Time","Frequency",sUnitsName);
        fprintf(stdout,sHeaderFormat2,"UTC","hertz",sUnits);
      }

      arBandWidth=get_bandwidth(arFreq,nItems,nMode,arMiniPkt);
      if(nUnits==1){              /* Power Flux */
        if(nSensor<=3){           /* Eu,Ev,Ex,Ew */    
          for(i=0;i<nItems;i++)   /* divide by the impedance of free space */
            arDens[i]/=377.0;
        }
      }/* fi power flux */
      else if(nUnits==2){            /* Electric and Magnetic Field Strength */
        for(i=0;i<nItems;i++){
          if(arDens[i]<0.0){/* negative data indicates interference present */
            arDens[i]*=-1.0;
            arDens[i]=sqrt(arDens[i]*arBandWidth[i]);
            arDens[i]*=-1.0;
          }
          else{
            arDens[i]=sqrt(arDens[i]*arBandWidth[i]);
          }
        }
      }
      else if(nUnits==3){         /* Electric and Magnetic Field Strength */
        if(nSensor<=3){           /* Eu,Ev,Ex,Ew */    
          if(nSensor==0){
            fAntLen = 9.26 * 9.26;       /* Ex dipole 9.26 meters */
            fBaseCap = 2.49459 * 2.49459;
            fAntLen = fAntLen / fBaseCap;/* save an iterative divide */
            for(i=0;i<nItems;i++){      /* multiply by the antenna resonance */
              dFreq=arFreq[i];
              dResCor=1.0-dFreq*dFreq/((8.775e6)*(8.775e6));
              dResCor=(1.0+(0.58/27.49))/(dResCor*dResCor + (0.58/27.49));
              arDens[i]*=dResCor;
              arDens[i]*=fAntLen;
            }
          }
          else{
            fAntLen = 5.0 * 5.0;            /* Eu,Ev,Ez 5.0 meters */
            fBaseCap = 2.51478 * 2.51478;  
            fAntLen = fAntLen / fBaseCap;  /* save an iterative divide */
            for(i=0;i<nItems;i++){   /* multiply by the antenna resonance */
              dFreq=arFreq[i];
              dResCor=1.0-dFreq*dFreq/((9.575e6)*(9.575e6));
              dResCor=(1.0+(0.1255*0.1255))/(dResCor*dResCor + (0.1255*0.1255));
              arDens[i]*=dResCor;
              arDens[i]*=fAntLen;
            }
          }
        }
      }
      else{  /* spectral density */
        ;  /* do nothing */
      } 

      if(nDataFormat==1){ /* time is seconds from beg. of capture */
        for(i=0;i<nItems;i++){
          fprintf(stdout,sDataFormat1,arTime[i],arFreq[i],arDens[i]);
        } 
      } 
      else if(nDataFormat==2){ /* time is scet, include bandwidths */
        for(i=0;i<nItems;i++){
          nMsec=nScetMils+(Uint)(arTime[i]*1000);
          fprintf(stdout,sDataFormat2,nScet_to_sScet(nScetDays,nMsec),
                  arFreq[i],arDens[i],arBandWidth[i]);
        } 
      } 
      else if(nDataFormat==3){ /* time is second, include bandwidths */
        for(i=0;i<nItems;i++){
          fprintf(stdout,sDataFormat3,arTime[i],arFreq[i],arDens[i],
                  arBandWidth[i]);
        } 
      } 
      else{  /* time is scet, include bandwidths */
        for(i=0;i<nItems;i++){
          nMsec=nScetMils+(Uint)(arTime[i]*1000);
          fprintf(stdout,sDataFormat0,nScet_to_sScet(nScetDays,nMsec),
                  arFreq[i],arDens[i]);
        } 
      } 

    }/* fi data */

  }/* end of while reading records */
  /* check nRead for errors here */

  
return 0;
}



/* check to see if the byte order is lsb or msb first */
Bool Little_End_In_First(void)
{
Bool bStatus;
Uchar buf[4]={0x0C,0x00,0x00,0x00};
Uint *p;

  p=(Uint*)buf;
  if(*p==0x0C)
    bStatus=True;   /* Lsb first */
  else 
    bStatus=False;  /* Msb first */

return bStatus;
}



/*
  Converts the standard JPL binary scet into a string.
nDoy  - is the number of days since Jan. 1, 2958
nMsec - is the milliseconds since the beginning of the day
pSin  - is a pointer to the return string. If pSin is a NULL pointer, the
        string is statically stored in the function.
Returns:
  A pointer to the standard JPL spacecraft event time format.
       year-doyThh:mm:ss.mil 
*/
char* nScet_to_sScet(Uint nDoy,Uint nMsec)
{
int nYear,nHour,nMin,nSec;
int nDays,nTotal;
static char arStr[32];

  /* normalize the incomming data */
  while(nMsec>= 24*60*60*1000){
    nMsec-=(24*60*60*1000);
    ++nDoy;
  }

  ++nDoy;  /* convert days since Jan. 1, 1958 [0-356] to day of year [1-366] */

  nDays=365;  nYear=1959;          /* initial conditions 365 days in 1958 */
  nTotal=nDays;
  while(nDoy>nTotal){
    if(nYear%100)                  /* Year is NOT a century year */
      nDays=(nYear%4)?365:366;     /* if evenly divisible by 4, leap year */
    else                           /* Year is a century year */
      nDays=(nYear%400)?365:366;   /* if evenly divisible by 400, leap year */
    nTotal+=nDays;
    ++nYear;
  }
  nYear-=1; 
  nTotal-=nDays;  
  nDoy-=nTotal;    /* days since jan 1 [0-365] */

  nHour=nMsec/(1000*60*60);  nMsec-=(nHour*1000*60*60);
  nMin= nMsec/(1000*60);     nMsec-=(nMin*1000*60);
  nSec= nMsec/(1000);        nMsec-=(nSec*1000);


             /* yyyy-doy T hh : mm : ss .mil */
  sprintf(arStr,"%04d-%03dT%02d:%02d:%02d.%03d",nYear,nDoy,
          nHour,nMin,nSec,nMsec);


return arStr;
}


/* 
float *get_bandwidth(float *arFreq,Uint nItems,Uint nMode,Uchar *arMiniPkt)
  This function takes calculates the bandwidth of the individual spectral 
density measurements.
  *arFreq     array containing the frequencies of the measurements
  nItems      number of elements in the frequency array
  nMode       instrument id from the archive file header 
  arMiniPkt   engineering status of the receiver from the archive file header
Returns:
  an array, nItems long, of bandwidths corresponding to the frequencies in
arFreq.  The units are in hertz.
*/
float *get_bandwidth(float *arFreq,Uint nItems,Uint nMode,Uchar *arMiniPkt)
{
int i;
float *arBandWidth,fBandWidth,fUpper,fLower;


  assert((arBandWidth=calloc(nItems,sizeof(float))) !=NULL); 

  if(LFDR_Normal==(nMode&RECEIVER_MASK)){             
    assert(nItems==32);
    for(i=0;i<32;i++)                    /* 32 log spaced channels */
      arBandWidth[i]=Lfdr_Bandwidth[i];  /* center freq. range 0.19Hz-24.3Hz */
  }
  else if(MFDR_Normal==(nMode&RECEIVER_MASK)){
    assert(nItems==32);
    for(i=0;i<32;i++)                    /* 32 log spaced channels */
      arBandWidth[i]=Mfdr_Bandwidth[i];  /* center freq. range 13.9Hz-1.7KHz */
  }
  else if(MFR_Normal==(nMode&RECEIVER_MASK)){
    assert(nItems==224);   /* Normal Sweep Mode */
    for(i=0;i<32;i++)      /* band 1 sweeps 1 and 2 (16+16=32) */
      arBandWidth[i]=5.6;  /* center freq. range from 23.80Hz to 169.0Hz */
    for(i=32;i<96;i++)     /* band 2 sweeps 1 and 2 (32+32=64) */
      arBandWidth[i]=19.4; /* center freq. range from 192.11Hz to 1.470KHz */
    for(i=96;i<224;i++)    /* band 3 sweeps 1,2,3, and 4 (32+32+32+32=128) */
      arBandWidth[i]=139.0;/* center freq. range from 1.536KHz to 11.799KHz */
  }
  else if(MFR_FastToggle==(nMode&RECEIVER_MASK)){
    assert(nItems==112);   /* Fast Toggle Mode */
    for(i=0;i<16;i++)      /* band 1 sweeps 1 (16) ) */
      arBandWidth[i]=5.6;  /* center freq. range from 23.80Hz to 169.0Hz */
    for(i=16;i<48;i++)     /* band 2 sweeps 1 (32) */
      arBandWidth[i]=19.4; /* center freq. range from 192.11Hz to 1.470KHz */
    for(i=48;i<112;i++)    /* band 3 sweeps 1 and 2 (32+32=64) */
      arBandWidth[i]=139.0;/* center freq. range from 1.536KHz to 11.799KHz */
  }
  else if(HFR_Analysis==(nMode&RECEIVER_MASK)){
    if(nMode&HFR_BandABC){                        /* log spaced bandwidths  */
      fBandWidth=get_hfr_ABC_bandwidth(arMiniPkt);/* A  3685.61 -  15823.72 */
      for(i=0;i<nItems;i++){                      /* B 16585.23 -  71206.73 */ 
        fUpper=fLower=log10(arFreq[i]);           /* C 74633.53 - 320430.31 */
        fLower-=fBandWidth/2.0;
        fUpper+=fBandWidth/2.0;
        arBandWidth[i]=pow(10.0,fUpper)-pow(10.0,fLower);  
      } 
    }
    else if(nMode&HFR_BandHf1){ /* 25KHz/filters */
      for(i=0;i<nItems;i++)  /* 100KHz - 4.xMHz */
        arBandWidth[i]=get_hfr_HF1_bandwidth(arMiniPkt);
    }
    else if(nMode&HFR_BandHf2){ /* 25KHz/filters */
      for(i=0;i<nItems;i++)  /* 100KHz - 16.xMHz */
        arBandWidth[i]=get_hfr_HF2_bandwidth(arMiniPkt);
    }
    else{
      for(i=0;i<nItems;i++)
        arBandWidth[i]=1.0;  /* this should never be executed */
    }
  }
  else if(HFR_Millisecond==(nMode&RECEIVER_MASK)){
    for(i=0;i<nItems;i++)   /* 25KHz/filters */
      arBandWidth[i]=25.0E3;
  }
  else{
    for(i=0;i<nItems;i++)
      arBandWidth[i]=1.0;  /* this should never be executed */
  }



return arBandWidth;
}


float get_hfr_ABC_bandwidth(Uchar *arMiniPkt)
{
int i;
static int first_time;
static float bandA[3][32];
static float bandB[3][32];
static float bandC[3][32];
float fBandWidth;

  if(!first_time){  /* generate the hfr band ABC frequencies */
    ++first_time;
    for(i=0;i<8;i++){  /* hfr band ABC frequencies, 8 filters */
      bandA[0][i]=3.6*pow(4.5,(0.5+i)/8)*1.0E3;
      bandB[0][i]=3.6*pow(4.5,(0.5+i+8)/8)*1.0E3;
      bandC[0][i]=3.6*pow(4.5,(0.5+i+16)/8)*1.0E3;
    }
    for(i=0;i<16;i++){  /* hfr band ABC freqencies, 16 filters */
      bandA[1][i]=3.6*pow(4.5,(0.5+i)/16)*1.0E3;
      bandB[1][i]=3.6*pow(4.5,(0.5+i+16)/16)*1.0E3;
      bandC[1][i]=3.6*pow(4.5,(0.5+i+32)/16)*1.0E3;
    }
    for(i=0;i<32;i++){  /* hfr band ABC frequencies 32 filters */
      bandA[2][i]=3.6*pow(4.5,(0.5+i)/32)*1.0E3;
      bandB[2][i]=3.6*pow(4.5,(0.5+i+32)/32)*1.0E3;
      bandC[2][i]=3.6*pow(4.5,(0.5+i+64)/32)*1.0E3;
    }
  }/* fi not first time */

  /* the hfr band A,B,C consists of N logrithmicly spaced channels */
  /* for lots of details of decoding the mini-packet header, refer */
  /* to the RPWS Users Guide.  All that we are interested is wheither */
  /* the hfr was in 8, 16, or 32 filter mode */
  if(!(arMiniPkt[9]&0x07)){  /* sanity check: Band A, B, or C selected */
    return 0; 
  } 
 
  if((arMiniPkt[14]&0x03)==0x00){       /* 8 filters per band */
    fBandWidth=log10(bandA[0][1])-log10(bandA[0][0]);  /* ~0.08165 */
  }
  else if((arMiniPkt[14]&0x03)==0x01){  /* 16 filters per band */
    fBandWidth=log10(bandA[1][1])-log10(bandA[1][0]);  /* ~0.04083 */
    
  }
  else{  /* bit patterns 0x03 and 0x02 imply 32 filters per band  */
    fBandWidth=log10(bandA[2][1])-log10(bandA[2][0]);  /* ~0.02041 */
  }
/* since Bands A,B,C are log spaced, it doesn't matter which we choose. 
  The calculation for bandwidth of a frequency of ABC becomes is a 
  constant delta, in log space.
*/

return fBandWidth;
}

float get_hfr_HF1_bandwidth(Uchar *arMiniPkt)
{
int nFilters=0;

  if(!(arMiniPkt[9]&0x08)){  /* sanity check: Band Hf1 selected */
    return 0.0; 
  } 

  if((arMiniPkt[15]&0x03)==0x00)       /* 1 filters per band */
    nFilters=1; 
  else if((arMiniPkt[15]&0x03)==0x01)  /* 2 filters per band */
    nFilters=2; 
  else if((arMiniPkt[15]&0x03)==0x02)  /* 4 filters per band */
    nFilters=4; 
  else if((arMiniPkt[15]&0x03)==0x03)  /* 8 filters per band */
    nFilters=8; 

return 25.0E3/nFilters;  /* Bandwidth in KHz */
}

float get_hfr_HF2_bandwidth(Uchar *arMiniPkt)
{
int nFilters=0;

  if(!(arMiniPkt[9]&0x10)){  /* sanity check: Band Hf2 selected */
    return 0.0; 
  } 

  if((arMiniPkt[16]&0x03)==0x00)       /* 1 filters per band */
    nFilters=1; 
  else if((arMiniPkt[16]&0x03)==0x01)  /* 2 filters per band */
    nFilters=2; 
  else if((arMiniPkt[16]&0x03)==0x02)  /* 4 filters per band */
    nFilters=4; 
  else if((arMiniPkt[16]&0x03)==0x03)  /* 8 filters per band */
    nFilters=8; 

return 25.0E3/nFilters;  /* Bandwidth in KHz */
}


void show_help(FILE *h)
{
  if(h==NULL)  h=stdout;

  fprintf(stdout,"usage: %s [OPTIONS] [FILENAME]\n",sVersion);
  fprintf(stdout,
"  lrf_list() reads RPWS Cassini Low Rate Full Calibration PDS Archive files "
"*.DATk.  If FILENAME is not supplied on the command line, then stdin is "
"assumed."
  );

  fprintf(stdout,
"OPTIONS:\n"
"  -d      don't output data samples\n"
"  -h      don't output header information\n"
"  -f NNN  output format, default NNN=0\n"
"          0   s/c event time (utc) in the form 2003-175T12:45:59.246\n"
"          1   offset from beginning of capture, 5.345 seconds\n"
  );
fprintf(stdout,
"  -u NNN  units for the data samples, default NNN=0\n"
"          0   spectral density [V^2/m^2/Hz] and [nT^2/Hz]\n"
"          1   power flux [W/m^2/Hz] and [nT^2/Hz]\n"
"          2   field [V/m] and [nT]\n"
"          3   spectral density [V^2/Hz] and [nT^2/Hz]\n"
"  -s      silent operation, don't output error mesasges\n"
"  -v      verbose operation, be verbose in the output\n"
  );
 
return;
}
