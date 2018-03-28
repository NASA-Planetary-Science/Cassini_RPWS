/*
 * cassini_mag.c written by Edward West 2007-01-09
 *    modified from cassini_fce.c written by Edward West
 *
 * The data files read by this program must be accessible under DATA_PATH.
 * The files themselves must be named MAG__SC___yyyyddd_n.TAB where
 * yyyy is the four digit year, ddd is the three digit day of year and
 * n is some digit (version number) or '_'.
 *
 * 2005-02-02 Punxsutawney Ed: Added -mag option to spit out mag data unchanged.
 * 2005-03-15 Edward West: Limit output to 1440 data points by bin-averaging.
 * 2006-11-07 Edward West: Changed argument parsing to use argtable2 library.
 *                         Added option -s with parameter to scale output
 *                         Changed -mag option to -m
 *
 * 2007-01-09 Edward West: Copied to cassini_mag.c and modified to output a
 *                         das2 stream.
 * 2010-11-15 Edward West: Added to svn
 * 2015-08-12 C. Piker: Adjustments for compiling on linux
 *
 * $URL: https://saturn.physics.uiowa.edu/svn/cassini/production/devel/l4mag/src/cassini_mag.c $
 * $Revision: 517 $
 */
#define _POSIX_C_SOURCE 200112L
 
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <libgen.h>
#include <argtable2.h>
#include <regex.h> /* REG_EXTENDED */

#include <das2/core.h>

#include "timestruct.h"

/* #define ABORT_ON_ERROR */

#define DEFAULT_DATA_PATH "/opt/project/cassini/mag"
#define CASSINI_MAG_DATA_PATH "CASSINI_MAG_DATA_PATH"

#define NAME_FORMAT  "MAG__%-3s__%4d%03d_%c.TAB"
#define NAME_SCAN_FORMAT "MAG__%*3s__%4d%03d_%*c.TAB"
#define MAX_NAME_LENGTH 256
#define MAX_FILES 1024
#define DATA_FORMAT "%s %f %f %f %f"

/* Seconds from 1958-001 to 2000-001 */
#define T_2000 1325376000.0

#define DATA_POINTS_PER_FILE 1440
#define PROGRESS_UPDATE_INTERVAL 100

#define DEFAULT_TAG_WIDTH 120

#define HEADER_XML_FORMAT "\
<stream>\n\
  <properties start=\"%s\" end=\"%s\" Datum:xTagWidth=\"%d s\" int:taskSize=\"%d\"/>\n\
</stream>\n"

#define PACKET_XML_FORMAT "\
<packet>\n\
  <x type=\"sun_real8\" units=\"t2000\"/>\n\
  <y type=\"sun_real4\" name=\"\" units=\"\"/>\n\
  <y type=\"sun_real4\" name=\"x\" units=\"\"/>\n\
  <y type=\"sun_real4\" name=\"y\" units=\"\"/>\n\
  <y type=\"sun_real4\" name=\"z\" units=\"\"/>\n\
</packet>\n"

void writeOneFile(
    FILE * file,
    double d_start,
    double d_end,
    double deltaTime);

void printUsage(char * pname, void *argTable[], void *helpTable[]);
void invalidTimeString(const char * time);
void abortOrExit();
void resolveFiles(char ** f1, char ** f2, int i);
void spaceToUnderscore(char * data);
void getFilename(char * filename, const char * system, int year, int day, char version);


/* The name of the executable */
char * myName;
int optFlags;

/* arguments are parsed using argtable2 library */
int main(int argc, char** argv) {
    TimeStruct start, end;
    double d_start, d_end;
    double deltaTime;
    int tagWidthSeconds;
    int success;
    int year, day;  /* Year and day of year */
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
    int iVersion; /* index of the filename version */
    char cBuffer[1024];
    int cSize;
    const char * system;

    /* argtable2 structs used to parse arguments */
    struct arg_lit *arg_help;
    struct arg_rex *arg_c_system;
    struct arg_str *arg_tstart;
    struct arg_str *arg_tend;
    struct arg_end *arg_eoa = arg_end(5);  /* Signifies end of argument list */

    void *helpTable[] = {
        arg_help = arg_lit0("h", "help", "Print usage information"),
        arg_eoa,
    };

    void *argTable[] = {
        arg_tstart = arg_str1(
            NULL,           /* shortopts */
            NULL,           /* longopts */
            "<start time>", /* datatype */
            "start time of data requested" /* glossary */
            ),
        arg_tend = arg_str1(
            NULL,         /* shortopts */
            NULL,         /* longopts */
            "<end time>", /* datatype */
            "end time of data requested" /* glossary */
            ),
        arg_c_system = arg_rex0(
            NULL,                /* shortopts */
            NULL,                /* longopts */
            "KG|KSM|KSO|RTN|SC", /* pattern */
            NULL,                /* datatype */
            REG_EXTENDED,        /* flags */
            "specify coordinate system" /* glossary */
            ),
        arg_eoa,
    };

    /* BUG WORK-AROUND: removing last argument if it is an empty string. */
    if (argv[argc-1][0] == '\0') { /* empty string */
        argc--;
    }

    /* DEFAULT VALUE */
    arg_c_system->sval[0] = "SC";

    /* Remember argv[0] */
    myName = basename(argv[0]);

    if ( (arg_nullcheck(helpTable) != 0)
        && (arg_nullcheck(argTable) != 0))
    {
        fprintf(stderr, "%s: Error while configuring argtable2\n", myName);
        return -1;
    }

    if (arg_parse(argc, argv, helpTable) == 0 && arg_help->count == 1) {
        printUsage(myName, argTable, helpTable);
        return 1;
    }

    /* Print messages and exit if any errors are encountered */
    if ( (arg_parse(argc, argv, argTable) != 0)) {
        arg_print_errors(stderr, arg_eoa, myName);
        fprintf(stderr, "\n");
        printUsage(myName, argTable, helpTable);
        return -1;
    }

    if (arg_c_system->count == 0) {
        fprintf(stderr, "%s: Using default coordinate system(SC)\n", myName);
    }
    system = arg_c_system->sval[0];

    /* Parse the start and end times. */
    success = parsetime(arg_tstart->sval[0], UNPACK(start));
    if (success != 0) {
        invalidTimeString(arg_tstart->sval[0]);
        abortOrExit();
    }
    success = parsetime(arg_tend->sval[0], UNPACK(end));
    if (success != 0) {
        invalidTimeString(arg_tend->sval[0]);
        abortOrExit();
    }
    d_start = ttime(UNPACK(start));
    d_end = ttime(UNPACK(end));

    getFilename(startName, system, start.year, start.yday, '0');
    getFilename(endName, system, end.year, end.yday, '_');
 
    if (d_start >= d_end) {
        fprintf(stderr, "Start time must me less than the end time.\n");
        abortOrExit();
    }

    dataPath = getenv(CASSINI_MAG_DATA_PATH);
    if (dataPath == NULL) {
        dataPath = DEFAULT_DATA_PATH;
    }
    
    /* Change to data directory */
    if (chdir(dataPath)) {
        perror(myName);
        fprintf(stderr, "%s: ERROR chdir %s\n", myName, dataPath);
        abortOrExit();
    }

    /* Change to coordinate system specific directory */
    if (chdir(system)) {
        perror(myName);
        fprintf(stderr, "%s: ERROR chdir %s/%s\n", myName, dataPath, system);
        abortOrExit();
    }
    
    /* Load directory entries */
    if (!(directory = opendir("."))) {
        perror(myName);
        fprintf(stderr, "%s: ERROR opening directory\n", myName);
        abortOrExit();
    }

    nfiles = 0;
    
    while((entry = readdir(directory))) {
        if (entry->d_ino
            && strcmp(entry->d_name, startName) >= 0
            && strcmp(entry->d_name, endName) <= 0
            && strlen(entry->d_name) == strlen(startName)
            && sscanf(entry->d_name, NAME_SCAN_FORMAT, &year, &day) == 2
            && day > 0 && day <= 366 && year >= 1970 && year <= 2037) {

            if (nfiles >= MAX_FILES) {
                fprintf(stderr, "%s: ERROR too many files in range %s to %s\n",
                        myName, startName, endName);
                abortOrExit();
            }
            newFile = (char *)malloc(MAX_NAME_LENGTH);
            if (!newFile) {
                fprintf(stderr, "%s: ERROR allocating file name list\n",
                        myName);
                abortOrExit();
            }
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

    deltaTime = (d_end - d_start) / DATA_POINTS_PER_FILE;
    tagWidthSeconds = (int)(deltaTime + 0.5) * 2;
    if (tagWidthSeconds < DEFAULT_TAG_WIDTH) {
        tagWidthSeconds = DEFAULT_TAG_WIDTH;
    }

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

    /* Print das2 header */
    cSize = snprintf(cBuffer, 1024, HEADER_XML_FORMAT,
        arg_tstart->sval[0], arg_tend->sval[0], tagWidthSeconds, 100);
    if (cSize >= 1024) {
        fprintf(stderr, "%s: ERROR cBuffer too small for xml header\n", myName);
        abortOrExit();
    }
    printf("[00]%06d%s", cSize, cBuffer);

    /* Print packet descriptor */
    cSize = snprintf(cBuffer, 1024, PACKET_XML_FORMAT);
    if (cSize >= 1024) {
        fprintf(stderr, "%s: ERROR cBuffer too small for packet xml\n", myName);
    }
    printf("[01]%06d%s", cSize, cBuffer);
        
    
    for (ifile = 0; ifile < nfiles; ifile++) {
        if (fileList[ifile] == NULL) continue;
        if (!(file = fopen(fileList[ifile], "r"))) {
            perror(myName);
            fprintf(stderr, "%s: ERROR fopen %s\n", myName, fileList[ifile]);
            abortOrExit();
        }
        errno = 0;

        writeOneFile(file, d_start, d_end, deltaTime);
        
        if (!feof(file) && ferror(file)) {
            fprintf(stderr, "%s: ERROR reading from %s\n", myName,
                    fileList[ifile]);
            abortOrExit();
        }
        
        fclose(file);
        
    }
    
    return 0;
}

void writeRecord(double date, float * data, int n) {
    char * header = ":01:";

    if ( !fwrite(header, 4, 1, stdout)
         || !fwrite(&date, sizeof(double), 1, stdout)
         || !fwrite(data, sizeof(float)*n, 1, stdout))
    {
         perror(myName);
         fprintf(stderr, "%s: ERROR writing to stdout\n", myName);
         abortOrExit();
    }
}

void writeOneFile(FILE * file, double d_start, double d_end, double deltaTime) {
    TimeStruct current;
    float mag[4];
    float outBuf[4];
    float count;
    char date[32];
    double d_current, d_previous;
    int i;

    count = 0.0;
    outBuf[0] = outBuf[1] = outBuf[2] = outBuf[3] = 0.0f;

    d_previous = d_start;

    while(fscanf(file, DATA_FORMAT, date, &mag[1], &mag[2], &mag[3], &mag[0]) != EOF) {
        parsetime(date, UNPACK(current));
        d_current = ttime(UNPACK(current));
        if ((d_current - d_previous) < deltaTime) {
            for (i = 0; i < 4; i++) { 
                outBuf[i] += mag[i];
            }
            count += 1.0f;
        }
        else {
            for (i = 0; i < 4; i++) {
                outBuf[i] = (outBuf[i] / count);
            }
            if (count > 0.0) {
                writeRecord(d_current - T_2000, outBuf, 4);
            }
            for (i = 0; i < 4; i++) {
                outBuf[i] = mag[i];
            }
            count = 1.0f;
            d_previous = d_current;
        }
    }
    if (count > 0.0) { 
        for (i = 0; i < 4; i++) {
            outBuf[i] = (outBuf[i] / count);
        }
        writeRecord(d_current - T_2000, outBuf, 4);
    }
}

void abortOrExit() {
#ifdef ABORT_ON_ERROR
    abort();
#else
    exit(-1);
#endif /* ABORT_ON_ERROR */
}

void printUsage(char * pname, void *argTable[], void *helpTable[]) {
    fprintf(stderr, "Usage:  %s ", pname);
    arg_print_syntaxv(stderr, argTable, "\n");

    fprintf(stderr, "        %s ", pname);
    arg_print_syntaxv(stderr, helpTable, "\n");

    fprintf(stderr, "\nOptions:\n");

    arg_print_glossary(stderr, argTable,  "        %-30s %s\n");
    arg_print_glossary(stderr, helpTable, "        %-30s %s\n");
}

void invalidTimeString(const char * time) {
    fprintf(stderr, "Invalid time string: '%s'\n", time);
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

void spaceToUnderscore(char * data) {
  while ( (data = strchr(data, ' ')) ) {
    data[0] = '_';
  }
}

void getFilename(char * filename,
                 const char * system,
                 int year,
                 int yday,
                 char version)
{
    sprintf(filename, NAME_FORMAT, system, year, yday, version);
    spaceToUnderscore(filename);
}
