/*
 * cassini_fce.c written by Edward West 2004-07-30
 *
 * The data files read by this program must be accessible under DATA_PATH.
 * The files themselves must be named MAG__SC___yyyyddd_n.TAB where
 * yyyy is the four digit year, ddd is the three digit day of year and
 * n is some digit (version number) or '_'.
 *
 * 2005-02-02 Punxsutawney Ed: Added -mag option to spit out mag data unchanged.
 *
 * 2005-03-15 Edward West: Limit output to 1440 data points by bin-averaging.
 *
 * 2006-11-07 Edward West: Changed argument parsing to use argtable2 library.
 *                         Added option -s with parameter to scale output
 *                         Changed -mag option to -m
 *
 * 2007-12-06 Edward West: Fixed a bug that was causing the first data value
 *                         to be an average of all the preceding values for
 *                         the current day prior to the requested interval.
 *
 * 2012-11-02 Chris Piker: Expect the build environment to set the mag data
 *                         directory.  Switched default mag data macro to 
 *                         be MAG_DATA, to look similar to RPWS_DATA from 
 *                         /opt/project/cassini/etc/setup.sh
 *
 * 2016-12-14 Chirs Piker: * Switch _TimeStruct to das_time_t from libdas2
 *                         * Turned into a combined das1/das2 reader
 *
 */
#define _POSIX_C_SOURCE 200112L

#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <stdio.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <libgen.h>
#include <limits.h>
#include <stdarg.h>

#include <argtable2.h>

#include <das2/core.h>
#include <das2/reader.h>


#ifndef MAG_DATA
#error define the macro MAG_DATA when compiling this file
#endif

#define DEFAULT_DATA_PATH "MAG_DATA"

#define NAME_FORMAT "MAG__SC___%4d%03d_%c.%c%c%c"
#define MAX_NAME_LENGTH 256
#define MAX_FILES 1024
#define DATA_FORMAT "%s %*f %*f %*f %f"

/* #define DEBUG(...) fprintf(stderr, __VA_ARGS__) */
#define DEBUG(...) 

void queryError(int nDasVer, const char* sFmt, ...);
void serverError(int nDasVer, const char* sFmt, ...);

void sendDasHeader(
    int nDasVer, bool bFce, const das_time_t* pBeg, const das_time_t* pEnd
);

void writeOneFile(
     int nDasVer, FILE* file, double d_start, double d_end, double deltaTime,
    double scale
);

void printUsage(char * pname, void *argTable1[], void *argTable2[]);
void resolveFiles(char ** f1, char ** f2, int i);


char* myName;  /* The name of the executable */
int optFlags;

/* ************************************************************************* */
/* arguments are parsed using argtabe2 library */

int main(int argc, char** argv) {
    das_time_t start, end;
    double d_start, d_end;
    double deltaTime;
    int year, day;  /* Year and day of year */
    char fversion;  /* File version character (ignored) */
    char startName[MAX_NAME_LENGTH];
    char endName[MAX_NAME_LENGTH];
    char *fileList[MAX_FILES];
    char *dataPath;
    char *newFile;
    int nfiles;
    int ifile, i;
    DIR *directory;
    FILE *file;
    struct dirent *entry;
    char suffix[3];
    int iVersion; /* index of the filename version */
    double scale;

    /* argtable2 structs used to parse arguments */
    struct arg_lit *arg_help;
    struct arg_lit *arg_das2;
    struct arg_lit *arg_mag;
    struct arg_lit *arg_core;
    struct arg_dbl *arg_scale;
    struct arg_str *arg_tstart;
    struct arg_str *arg_tend;
    struct arg_end *arg_eoa = arg_end(5);  /* Signifies end of argument list */

    void *helpTable[] = {
        arg_help = arg_lit0("h", "help", "Print usage information"),
        arg_eoa,
    };
   
    void *argTable[] = {
        arg_das2 = arg_lit0(NULL, "das2", "output a das2 stream instead of das1"),
        arg_core = arg_lit0(NULL, "core", "abort with a core dump on an error"),
        arg_mag = arg_lit0("m", NULL, "output magnetic field instead of fce"),
        arg_scale = arg_dbl0("s", NULL, NULL, "multiply output by scale factor"),
        arg_tstart = arg_str1(NULL, NULL, "<start time>", "start time of data requested"),
        arg_tend = arg_str1(NULL, NULL, "<end time>", "end time of data requested"),
        arg_eoa,
    };

    /* This is a work-around for a bug in the argtable2 library that causes
     * a crash on 64-bit machines */
    arg_scale->dval = &scale;
    scale = 1.0; /* initial value */

    /* Remember argv[0] */
    myName = basename(argv[0]);

    if (arg_nullcheck(argTable) != 0) {
          
        fprintf(stderr, "%s: Error while configuring argtable2\n", myName);
    }

    if (arg_parse(argc, argv, helpTable) == 0 && arg_help->count == 1) {
        printUsage(myName, argTable, helpTable);
		  return 0;
    }

    /* Print messages and exit if any errors are encountered */
    if (arg_parse(argc, argv, argTable) != 0) {
        printUsage(myName, argTable, helpTable);
        return 3;
    }
     
     /* See if core dumps are desired */
     if(arg_core->count > 0) das2_abort_on_error(); 
     
     int nDasVer = 1;
     if(arg_das2->count > 0) nDasVer = 2;

    /* only multiply the scale factor by 28 if -m is NOT specified */
     bool bFce = false;
    if (arg_mag->count == 0){ 
         scale *= 28.0;
         bFce = true;
     }

    /* Parse the start and end times. */
    if(! dt_parsetime(arg_tstart->sval[0], &start)){
        das_send_stub(nDasVer);
        queryError(nDasVer, "Invalid time string: '%s'", arg_tstart->sval[0]);
    }
             
    if(! dt_parsetime(arg_tend->sval[0], &end)){
        das_send_stub(nDasVer);
        queryError(nDasVer, "Invalid time string: '%s'", arg_tend->sval[0]);
    }
             
    d_start = dt_ttime(&start);
    d_end = dt_ttime(&end);
    sprintf(startName, NAME_FORMAT, start.year, start.yday, '0', 'T', 'A', 'B');
    sprintf(endName, NAME_FORMAT, end.year, end.yday, '_', 'T', 'A', 'B');
    
    if (d_start >= d_end){ 
        das_send_stub(nDasVer);
        queryError(nDasVer, "Start time must be less than the end time.");    
    }
     
    sendDasHeader(nDasVer, bFce, &start, &end);

    dataPath = getenv("MAG_DATA");
    if (dataPath == NULL) {
        dataPath = DEFAULT_DATA_PATH;
    }
    
    /* Change to data directory */
     errno = 0;
    if (chdir(dataPath)) 
        serverError(nDasVer, "%s can't chdir to %s, %s", myName, 
                        dataPath, strerror(errno));
    
    /* Load directory entries */
     errno = 0;
    if (!(directory = opendir("."))) {
         DEBUG("Reading from directory %s\n", dataPath);
         serverError(nDasVer, "%s can't open directory %s, %s", myName, 
                       dataPath, strerror(errno));
    }
    
    nfiles = 0;
    
    while((entry = readdir(directory))) {
        if (entry->d_ino
            && strcmp(entry->d_name, startName) >= 0
            && strcmp(entry->d_name, endName) <= 0
            && strlen(entry->d_name) == strlen(startName)
            && sscanf(entry->d_name, NAME_FORMAT, &year, &day, &fversion,
               &suffix[0], &suffix[1], &suffix[2]) == 6
            && strncmp("TAB", suffix, 3) == 0
            && day > 0 && day <= 366 && year >= 1970 && year <= 2037) {

            if (nfiles >= MAX_FILES) 
                     serverError(nDasVer, "%s encountered too many files in range "
                                 "%s to %s\n", myName, startName, endName);
            
            newFile = (char *)malloc(MAX_NAME_LENGTH);
            if (!newFile) 
                     serverError(nDasVer, "%s: error allocating file name\n", myName);
            
            strcpy(newFile, entry->d_name);
            
            for (ifile = nfiles; ifile > 0; ifile--) {
                if (strcmp (fileList[ifile-1], newFile) > 0) {
                    fileList[ifile] = fileList[ifile-1];
                }
                else {
                    break;
                }
            }
            fileList[ifile] = newFile;
            nfiles++;
        }
    }
    
    closedir(directory);
    
    file = (FILE *)0;

    deltaTime = (d_end - d_start) / 1440.0;
	fprintf(stderr, "deltaTime: %f\n", deltaTime);

    /* remove duplicates */
    iVersion = strlen("MAG__SC___YYYYDDD_"); /* file version index */
    for (ifile = 0; ifile < nfiles; ifile++) {
        if (fileList[ifile] == NULL) continue;
        for (i = ifile+1; i < nfiles; i++) {
            if (strncmp(fileList[ifile], fileList[i], iVersion) == 0) {
                resolveFiles(fileList + ifile, fileList + i, iVersion);
            }
        }
    }
    
    for (ifile = 0; ifile < nfiles; ifile++) {
        if (fileList[ifile] == NULL) continue;
          errno = 0;
        if ((file = fopen(fileList[ifile], "r")) == NULL)
            serverError(nDasVer, "%s couldn't fopen %s, %s", myName, 
                              fileList[ifile], strerror(errno));
        
          errno = 0;
        fprintf(stderr, "INFO: Reading %s\n", fileList[ifile]);
        writeOneFile(nDasVer, file, d_start, d_end, deltaTime, scale);
        
        if (!feof(file) && ferror(file))
            serverError(nDasVer, "%s couldn't read from %s, %s\n", myName,
                        fileList[ifile], strerror(errno));
        
        fclose(file);
        
    }
    
    return 0;
}

/* ************************************************************************* */
void sendDasHeader(
    int nDasVer, bool bFce, const das_time_t* pBeg, const das_time_t* pEnd
){
    char sBuf[4096] = {'\0'};
    char sBeg[32]   = {'\0'};
    char sEnd[32]   = {'\0'};
    
    dt_isoc(sBeg, 31, pBeg, 3);
    dt_isoc(sEnd, 31, pEnd, 3);
    
    const char* sTitle = "Cassini MAG F!dCE!n";
    const char* sYLabel = "F!dce!n (Hz)";
    const char* sName = "Fce";
    const char* sUnits = "Hz";
    if(!bFce){
        sTitle = "Cassini B-Field magnitude";
        sYLabel = "|B| (nT)";
        sName = "B_mag";
        sUnits = "nT";
    }
    
    fprintf(stderr, "INFO: %s, from %s to %s\n", sTitle, sBeg, sEnd);
    if(nDasVer == 1) return;
    
    snprintf(sBuf, 4095,
        "<stream version=\"2.2\">\n"
        "  <properties String:title=\"%s\"\n"
        "              String:xLabel=\"SCET %%{RANGE}\"\n"
        "              String:yLabel=\"%s\"\n"            
        "              DatumRange:xCacheRange=\"%s to %s UTC\"\n"
        "  />\n"
        "</stream>\n", sTitle, sYLabel, sBeg, sEnd
    );
    printf("[00]%06zd%s", strlen(sBuf), sBuf);
    
    snprintf(sBuf, 4095,
        "<packet>\n"
        "  <x type=\"time22\" units=\"us2000\"></x>\n"
        "  <y name=\"%s\" type=\"ascii12\" units=\"%s\"></y>\n"
        "</packet>\n", sName, sUnits
    );
    printf("[01]%06zd%s", strlen(sBuf), sBuf);
}

/* ************************************************************************* */
void writeOneFile(
    int nDasVer, FILE* file, double d_start, double d_end, double deltaTime, 
    double scale
){
    das_time_t current = {0};
    char sCurrent[32] = {'\0'};
    float mag = 0.0f;
    float outBuf[2] = {0.0f, 0.0f};
    char date[32] = {'\0'};
    double d_current = 0.0;
    double out = 0.0;

    while(fscanf(file, DATA_FORMAT, date, &mag) != EOF) {
        DEBUG(".");
        dt_parsetime(date, &current);
        d_current = dt_ttime(&current);
        if (d_current < d_start - 300.0) {
            continue;
        }
        else if (d_current > d_end + 300.0) {
            break;
        }
        out = mag * scale;
						
        if(nDasVer == 1){
            outBuf[0] = swapFloatIfHostLE( (float)(d_current - d_start) );
            outBuf[1] = swapFloatIfHostLE( out );

            errno = 0;
            if (!fwrite(outBuf, sizeof(float), 2, stdout)) 
                serverError(nDasVer, "%s couldn't write to stdout, %s",
                            myName, strerror(errno));
        }
        else{
           dt_emitt(d_current, &current);
           dt_isod(sCurrent, 32, &current, 3);
           errno = 0;
           if( printf(":01:%s %11.4e\n", sCurrent, out) < 0)
               serverError(nDasVer, "%s couldn't write to stdout, %s",
                           myName, strerror(errno));
        }
    }
    DEBUG("\n");
}

/* ************************************************************************* */
/* Error Handling */

void vError(int nDasVer, const char* sType, const char* sFmt, va_list ap)
{
    int nRet = 0;
    va_list _ap;
    char sMsg[1024] = {'\0'};
    
    va_copy(_ap, ap);
    vsnprintf(sMsg, 1023, sFmt, _ap);
    va_end(_ap);
    
    if(strcmp(sType, DAS2_EXCEPT_ILLEGAL_ARGUMENT) == 0){
        nRet = das_send_srverr(nDasVer, sMsg);
    }
    else{
        if(strcmp(sType, DAS2_EXCEPT_SERVER_ERROR) == 0)
            nRet = das_send_srverr(nDasVer, sMsg);
        else{
            fprintf(stderr, "TODO: Fix this logic error\n");
            nRet = 7;
        }
    }
    
    if(das2_error_disposition() == DAS2_ERRDIS_ABORT)
        abort();
    else
        exit(nRet);
}

void queryError(int nDasVer, const char* sFmt, ...)
{
    va_list argp;
    va_start(argp, sFmt);
    vError(nDasVer, DAS2_EXCEPT_ILLEGAL_ARGUMENT, sFmt, argp);
    va_end(argp);
}

void serverError(int nDasVer, const char* sFmt, ...)
{
    va_list argp;
    va_start(argp, sFmt);
    vError(nDasVer, DAS2_EXCEPT_SERVER_ERROR, sFmt, argp);
    va_end(argp);
}

/* ************************************************************************* */

void printUsage(char * pname, void *argTable1[], void *argTable2[]) {
    fprintf(stderr, "Usage:  %s ", pname);
    arg_print_syntaxv(stderr, argTable1, "\n");
    fprintf(stderr, "        %s ", pname);
    arg_print_syntaxv(stderr, argTable2, "\n");

    fprintf(stderr, "\nOptions:\n");

    arg_print_glossary(stderr, argTable1, "        %-30s %s\n");
    arg_print_glossary(stderr, argTable2, "        %-30s %s\n");
}

void resolveFiles(char ** f1, char ** f2, int i) {
    if ( (*f1)[i] > (*f2)[i] ) {
        free(*f2);
        *f2 = NULL;
    }
    else {
        free(*f1);
        *f1 = *f2;
        *f2 = NULL;
    }
}
