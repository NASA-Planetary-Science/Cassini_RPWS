/*
 * cassiniKp.c
 *
 * produce a das2Stream from cassini pws KP's.
 * Updated by cwp on 2016-03-22 to use the new Das2 C-library
 */

/* Which POSIX standard version do we match */
#define _POSIX_C_SOURCE 200112L

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <strings.h>  /* Needed for strncasecmp */

#include <das2/core.h>

#include "dbase.h"

/* The following macros and tables could be saved in a separate header */
/* file for general use in other applications. */

#define RECSIZE 1175

#define RECCOUNT 1440

static const double freq[73];

/* make the directory where the data was found globally available
 * so we can easily find the calibration file
 */
char dataDir[256];


char* startTime;
char* endTime;

/* decimal day start and end times */
double jday1, jday2;

/* identify unit to read data in
 */
int iunit;



 /* Stream and Packet Descriptors for producing the das2Stream */
DasIO*      g_pOut = NULL;
StreamDesc* streamDescriptor = NULL;
PktDesc* packetDescriptor = NULL;


/* ************************************************************************** */

double _getSecondsSinceMidnight( double value, UnitType units ) {
    double xx= Units_convertTo( UNIT_T2000, value, units );
    double result;
    if (xx<0) {
        xx= fmod( xx, 86400 );
        if (xx==0) {
            result= 0;
        } else {
            result= 86400+xx;
        }
    } else {
        result= fmod( xx, 86400 );
    }
    return result;
}

int _getJulianDay( double time, UnitType units ) {
    double xx= Units_convertTo( UNIT_MJ1958, time, units );
    return (int)floor( xx ) + 2436205;
}

double _convertFromUS2000( double value, UnitType toUnits ) {
    if ( strcmp(toUnits, UNIT_US2000) == 0 ) {
        return value;
    } else if ( strcmp(toUnits, UNIT_T2000) == 0 ) {
        return value / ( 1e6 );
    } else if ( strcmp(toUnits, UNIT_MJ1958) == 0 ) {
        return value / ( 86400 * 1e6 ) + 15340;
    } else if ( strcmp( toUnits, UNIT_T1970) == 0 ) {
        return value / 1e6 + 946684800;
    } else {
        das2_error( -1, "unsupported conversion from US2000 to %s\n",
                                       Units_toStr( toUnits ) );   abort();
    }
         return FILL_VALUE;
}

double _time2Double(
        int year, int month, int dom, int hour, int minute, double seconds,
        UnitType unit
) {
    int jd = 367 * year - 7 * (year + (month + 9) / 12) / 4 -
    3 * ((year + (month - 9) / 7) / 100 + 1) / 4 +
    275 * month / 9 + dom + 1721029;

    double ssm = seconds + hour*3600.0 + minute*60.0;
    double mj1958= ( jd - 2436205. ) + ssm / 86400.;
    double us2000= ( jd - 2451545 ) * 86400000000. + ssm * 1000000;

    if ( strcmp(unit, UNIT_US2000) == 0 ) {
        return us2000;
    } else if ( strcmp(unit, UNIT_MJ1958) == 0 ) {
        return mj1958;
    } else {
        return _convertFromUS2000( us2000, unit );
    }
}

void _double2Time(
        double time, UnitType units,
        int *pyear, int *pmonth, int *pdom, int *phour, int *pminute, double *psecond
){

    int jalpha, j1, j2, j3, j4, j5;
    int year, month, day, hour, minute;
    double justSeconds;

    int jd= _getJulianDay(time,units);
    double seconds= _getSecondsSinceMidnight(time,units);

    jalpha = (int)(((double)(jd - 1867216) - 0.25)/36524.25);
    j1 = jd + 1 + jalpha - jalpha/4;
    j2 = j1 + 1524;
    j3 = 6680 + (int)(((j2-2439870)-122.1)/365.25);
    j4 = 365*j3 + j3/4;
    j5 = (int)((j2-j4)/30.6001);

    day = j2 - j4 - (int)(30.6001*j5);
    month = j5-1;
    month = ((month - 1) % 12) + 1;
    year = j3 - 4715;
    year = year - (month > 2 ? 1 : 0);
    year = year - (year <= 0 ? 1 : 0);

    hour = (int)(seconds/3600.0);
    minute = (int)((seconds - hour*3600.0)/60.0);
    justSeconds = seconds - hour*(double)3600.0 - minute*(double)60.0;


    *pyear= year;
    *pmonth= month;
    *pdom= day;
    *phour= hour;
    *pminute= minute;
    *psecond= justSeconds;

}

char* _double2TimeStr( double time, UnitType units ) {  
        int year, month, dom;
        int hour, minute;
        double second;
        char* result;
   
        _double2Time(time, units, &year, &month, &dom, &hour, &minute, &second);
        result= (char *) malloc( 100 );
        sprintf( result, "%d-%02d-%02dT%02d:%02d:%06.3f", year, month, dom, hour,
                minute, second );
        return result;
}

/* ************************************************************************** */
/* given the dataroot, year, month, day, return the name of the file.         */

void findFile( char *filename, char*dataRoot, int year, int doy ) {

  /* char monthRange[6]; */
  char *format;
  char yrs[3];
  char doys[3];
  /* int year1900; */

  sprintf( yrs, "%4.4d", year );
  if ( yrs[0]==' ' ) yrs[0]='0';
  sprintf( doys, "%3.3d", doy );
  if ( doys[0]==' ' ) doys[0]='0';
  if ( doys[1]==' ' ) doys[0]='0';

  format= "%sT%s%cXX";
  sprintf( dataDir, format, dataRoot, yrs, doys[0] );

  format= "%s/RPWS_KEY__%s%s_0.TAB";
  sprintf( filename, format, dataDir, yrs, doys );


}

/* ************************************************************************** */

int parseLine (const char *buf, double *sd, int nCols, int nOffset) {

  double data; /*, y0, y1;*/          /* used in linear interpolation */
  int i;
  /* int idata; */

  for (i = 0; i < nCols; i++) {
      sscanf( buf + 24 + ((i+nOffset)*10), "%10lf", &data );
      sd[i]= data;
      /*fprintf( stderr, "float=%le\n", data );*/
  }
  return 0;

}


/* ************************************************************************** */
/* parses the time in chars 0-24 in the format YYYY-JJJT00:00:00.000.         */
/*                                             012345678901234567890          */
double parseTimeZToUs2000( char* buf ) {

    double result= 0;
    int jd;
    int year;

    result+= ( buf[20]-'0' ) / 1000.;
    result+= ( buf[19]-'0' ) / 100.;
    result+= ( buf[18]-'0' ) / 10.;

    /* fprintf( stderr, "%lf\n ", result ); */
    result+= ( buf[16]-'0' ) ;
    result+= ( buf[15]-'0' ) * 10;

    /* fprintf( stderr, "%lf\n ", result ); */
    result+= ( buf[13]-'0' ) * 60;
    result+= ( buf[12]-'0' ) * 10 * 60;

    /* fprintf( stderr, "%lf\n ", result ); */
    result+= ( buf[10]-'0' ) * 3600;
    result+= ( buf[9]-'0' ) * 10 * 3600;

    /* fprintf( stderr, "%lf\n ", result ); */
    result+= ( buf[7]-'0' ) * 1 * 86400;
    result+= ( buf[6]-'0' ) * 10 * 86400;
    result+= ( buf[5]-'0' ) * 100 * 86400;

    /*  doy 1 has offset 0.0... */
    result-= 86400;

    /* fprintf( stderr, "%lf\n ", result ); */
    year= ( buf[0]-'0' ) * 1000 + (buf[1]-'0' ) * 100 + (buf[2]-'0' ) * 10 + (buf[3]-'0' );

    /* offset for the year/jan/1 */

    jd = 367 * year - 7 * (year + ( 1 + 9) / 12) / 4 -
    3 * ((year + (1 - 9) / 7) / 100 + 1) / 4 +
    275 * 1 / 9 + 1 + 1721029 - 2451545;

    /* fprintf( stderr, "%lf %lf %d %d\n ", result, result/86400., year, jd ); */
    return result * 1e6 + jd * 86400e6;

}

/* ************************************************************************** */
/*  return in the data from the file. */
/*  start, end are in UNIT_US2000     */

bool g_bSentPktHdr = false;

void readDay(
	FILE *in, const char* sType, double start, double end, double doffset 
) {

  char buf[RECSIZE];                    /* fixed-length records */
  unsigned long int count = 0;          /* number of records processed */
  double data[73];                      /* 73 channels of spectral density */
  double ytags[73];

  double us2000;

  PlaneDesc* planeDescriptor;

  int taskProgress;
  
  int nColOffset = 0;
  int nCols = 73;
  const char* sPlaneName = "electric_specdens";
  if(toupper(sType[0]) == 'B'){
	  nColOffset = 73;
	  nCols = 42;
	  sPlaneName = "magnetic_specdens";
  }

  /* int recNum=0; */
  /* Loop through all available records from standard input */
  while ( fread (buf, RECSIZE, 1, in) ) {

    if ( count==0 ) {
      parseLine( buf, ytags, nCols, nColOffset);
		  
      if(!g_bSentPktHdr){
        DasEncoding* pXEnc = new_DasEncoding(DAS2DT_HOST_REAL, 8, NULL);
        packetDescriptor = StreamDesc_createPktDesc(streamDescriptor, pXEnc, UNIT_US2000);
	
        DasEncoding* pYEnc = new_DasEncoding(DAS2DT_ASCII, 9, "%8.2e");
        DasEncoding* pZEnc = new_DasEncoding(DAS2DT_HOST_REAL, 4, NULL);
        planeDescriptor = new_PlaneDesc_yscan(
            sPlaneName, pZEnc, UNIT_E_SPECDENS, nCols, pYEnc, (double*)ytags, UNIT_HERTZ
        );
		
        PktDesc_addPlane(packetDescriptor, planeDescriptor);
	
        /* Moved xTagWidth to the stream header -cwp */
		  DasIO_writePktDesc(g_pOut, packetDescriptor);
        g_bSentPktHdr = true;
		}
    } 
	 else{
        us2000= parseTimeZToUs2000( buf );

        if ( ( start <= us2000 ) && ( us2000 < end ) ) {
				PktDesc_setValue(packetDescriptor, 0, 0, us2000);
            parseLine( buf, data, nCols, nColOffset);
				PktDesc_setValues(packetDescriptor, 1, data);

				DasIO_writePktData(g_pOut, packetDescriptor);

            taskProgress= (int)( ( ( us2000 - start ) * 100 ) / ( end - start ) );

            DasIO_setTaskProgress(g_pOut, taskProgress );
        }
    }

    count++;
  } /* while data to read */
}


/* ************************************************************************** */
/* The main program . . . */

int main (int argc, char *argv[]) {

  char *filename = NULL;
  FILE *in = NULL;
  char *dataRoot = NULL;


  int year1, month1, day1, doy1, hour1, min1;
  int year2, month2, day2, doy2, hour2, min2;

  double sec1, sec2;
  double start_t1958, stop_t1958;
  int daysInYear;

  double jday0;
  int iday;
  double doffset;

  #define  MAXFILES 2048
  struct file_structure files[MAXFILES];
  int fileCount;
  int fileIndex;

  if( argc < 4 || strcmp(argv[1], "-h") == 0 ||  strcmp(argv[1], "--help") == 0){
    fprintf(stderr,"Usage: rpws_kp_das2rdr <dataRoot> [B] <startTimeStr> <endTimeStr>\n");
    fprintf(stderr,"  rpws_kp_das2rdr " RPWS_SUPERVOL "/DATA/RPWS_KEY_PARAMETERS"
	                " [E|B] 2000-02-06T00:00 2000-02-07T02:00 > YOUR_FILE.d2t\n ");
    return 4;
  } 
  
  /* Allow for automatic E */

  dataRoot= argv[1];
  fprintf(stderr, "dataRoot:  %s\n",  dataRoot);
    
  startTime= argv[2];
  fprintf(stderr, "startTime: %s\n",   startTime);

  endTime= argv[3];
  fprintf(stderr, "endTime:   %s\n",   endTime);

  const char* sType = "E"; 
  if(argc > 4)
    sType = argv[4];
  
  fprintf(stderr, "dataType:  %s\n",  sType);
  
  fprintf(stderr, "numArgs:   %d\n",   argc-1);


  fputs("das2StreamVersion=" DAS_STREAM_VERSION "\n", stderr);


  parsetime( startTime, &year1, &month1, &day1, &doy1, &hour1, &min1, &sec1 );
  parsetime( endTime, &year2, &month2, &day2, &doy2, &hour2, &min2, &sec2 );

  /* fprintf( stderr, "start year: %d month: %d day: %d\n", year1, month1, day1 ); */
  /* fprintf( stderr, "end   year: %d month: %d day: %d\n", year2, month2, day2 ); */
  
  jday0= _time2Double( year1, 1, doy1, 0.0, 0.0, 0.0, UNIT_US2000 ) ;
  jday1= _time2Double( year1, 1, doy1, hour1, min1, sec1, UNIT_US2000 ) ;
  jday2= _time2Double( year2, 1, doy2, hour2, min2, sec2, UNIT_US2000 ) ;

  start_t1958= 86400 * _time2Double( year1, 1, doy1, hour1, min1, sec1, UNIT_MJ1958 ) ;
  stop_t1958= 86400 * _time2Double( year2, 1, doy2, hour2, min2, sec2, UNIT_MJ1958 ) ;

  /* fprintf( stderr, "jday0=%lf %lf %lf \n", jday0, jday1, jday2 ); */

  iday= 0;
  
  /* Create a new output writer */
  g_pOut = new_DasIO_cfile("rpws_kp_das2rdr", stdout, "w");

  streamDescriptor= new_StreamDesc();

  char sRange[128] = {'\0'};
  snprintf(
    sRange, 127, "%s to %s UTC", _double2TimeStr(jday1, UNIT_US2000),
    _double2TimeStr(jday2, UNIT_US2000)
  );
  Desc_setProp((Descriptor*)streamDescriptor, "DatumRange", "xCacheRange", sRange);
  
  const char* sTitle = "Cassini RPWS -- Key Parameters, Electric";
  const char* sZLabel = "Spectral Density (V!a2!n m!a-2!n Hz!a-1!n)";
  if(toupper(sType[0]) == 'B' || strncasecmp(sType, "mag", 3) == 0){
    sTitle = "Cassini RPWS -- Key Parameters, Magnetic";
    sZLabel = "Spectral Density (nT!a2!n Hz!a-1!n)";
  }
  Desc_setPropStr((Descriptor*)streamDescriptor, "title", sTitle);
  Desc_setPropStr((Descriptor*)streamDescriptor, "yLabel", "Frequency (Hz)");
  Desc_setPropStr((Descriptor*)streamDescriptor, "zLabel", sZLabel);
  Desc_setPropDouble((Descriptor*)streamDescriptor, "zFill", 0.0);
  
  /* Removed to so that 'make test' always works. Could have a test script
     that strips variable information in order to make the test go while 
	  keeping the properties in the stream header */
  
  /* Desc_setPropStr((Descriptor*)streamDescriptor, "readerVersion", "$Id$"); */
  /* setStreamSourceId( streamDescriptor, argv[0] ); */
  /* StreamDesc_addStdProps(streamDescriptor ); */
  /* StreamDesc_addCmdLineProp(streamDescriptor, argc, argv); */

  /* Autoplot Bug: Doesn't support the Das2 boolean property type */
  /* StreamDesc_setMonotonic(streamDescriptor, true); */
  
  Desc_setPropDatum((Descriptor*)streamDescriptor, "xTagWidth", 60.0, UNIT_SECONDS );
	
  DasIO_setTaskSize(g_pOut, 100);

  DasIO_writeStreamDesc(g_pOut, streamDescriptor);

  DasIO_sendLog(g_pOut, LOGLVL_INFO, "doing findFile ", filename);

  /* fprintf(stderr, "enter make_dbase\n" ); */
  make_dbase ( start_t1958, stop_t1958, files, &fileCount );

  if ( fileCount == 0) {
    DasIO_throwException(g_pOut, streamDescriptor,  "ServerError", "no files found");
    return 3;
 } else {
     fprintf( stderr,"INFO: %d files found\n", fileCount );
 }

  for ( fileIndex=0; fileIndex<fileCount; fileIndex++ ) {

    filename= files[fileIndex].line;
    in= fopen(filename,"r");

    if (in==NULL) {
      fprintf( stderr,"WARNING: File not found: %s\n",filename );
      DasIO_sendLog( g_pOut, LOGLVL_INFO, "File not found: %s",filename );

    } else {
      fprintf(stderr,"INFO: Reading file: %s\n", filename);
      DasIO_sendLog( g_pOut, LOGLVL_INFO, "Reading file: %s", filename );

      doffset= jday0 + 86400e6*iday;

      readDay( in, sType,
               jday1,
               jday2,
               doffset);
      fclose(in);
    }

    doy1++;

    daysInYear= jday( year1+1, 1, 1 ) - jday( year1, 1, 1 );

    if ( doy1 > daysInYear ) {
        year1++;
        doy1= 1;
    }

  }

  DasIO_close(g_pOut);

  return 0;
}
