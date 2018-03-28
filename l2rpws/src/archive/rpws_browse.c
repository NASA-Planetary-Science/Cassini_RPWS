/********************************************************************
 ****		rpws_browse.c					*****
 ****	Build an appropriate browse file for WBR/WFR data	*****
 ****	files.  						*****
 ********************************************************************/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include <fg.h>   

#include <rtiu.h>  /* RPWS general inlcudes */
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <mp_status.h>
#include <wbr_status.h>
#include <wfr_status.h>
#include <archive.h>

#include "rpws_label.h"  /* local lib includes */
#include "rpws_direct.h"

#include "bis.h"         /* App includes */
#include "rpws_browse.h"
#include "rpws_archive.h"
#include "rpws_hfr_status.h"

/****************************************************************************/
/* Compiled in default file locations */


#ifndef CFG
#error Compiled in configuration directory is missing.
#endif

#define _EPHEMERIS_TIME_

extern int Plot_Flag;
extern int bisflag;
extern int TIMEFLAG;
static char *Version = {"V3.5"};
static char comment_flag = 0;
static char Browse_File_Name[] = {"make_hr_browse.sh"};
static char *inst_label[] = {"wbr","wfr", "STIM", "UNK"};
static char *inst_band[] = {"25Hz", "2_5Khz", "10Khz", "75Khz", "HFR"};
char *Inst_Label[] = {"WBR","WFR", "STIM", "UNK"};
static char *Inst_Band[] = {"25Hz", "2.5kHz", "10kHz", "75kHz"};

struct RPWS_LABEL *RPWS_BROWSE_data_record[16] = {	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
 							NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

/* ************************************************************************* */
static char *plot_band(const struct RPWS_LABEL *detail_record)
{
	static char *result;
	result = inst_band[detail_record->fband];
	if(detail_record->mode_mask & MODE_MASK_HFR)
		result = inst_band[4];
	return result;
}

 
/* ************************************************************************* */
/* Write a single section of the huge script make_hr_browse.sh               */

static int rpws_browse_record(
	const struct RPWS_LABEL *detail_record,
	const char *directory[],
	const char *body_in,
	const int end_flag,
	const char* pScriptPath
){
	static int first_time = 1;
	int flag = 0;
	char *body = NULL;
	char *write_flag[2] = {"a", "a"};
	FILE *time_file = NULL;
	char time_file_name[256];
	char filename[128] = {'\0'};
	char thumbname[128] = {'\0'};
	char start_time[128] = {'\0'};
	char stop_time[128] = {'\0'};
	char title[128] = {'\0'};
	char orbit[128] = {'\0'};
	char current_directory[128] = {'\0'};
	
	if(!detail_record)
		return 0;
	
	body = detail_record->Coordinate;;
	if(body_in)
		if(body_in[0])
			body = (char *)body_in;
	
	strcpy(time_file_name, Browse_File_Name);
	if(directory){
		if(directory[RPWS_ARCHIVE_script]){
			if(directory[RPWS_ARCHIVE_script][0]) {
				strcpy(time_file_name, directory[RPWS_ARCHIVE_script]);
				strcat(time_file_name, "/");
				strcat(time_file_name, Browse_File_Name);
			}
		}
	}
		
	errno = 0;
	time_file = fopen(time_file_name, write_flag[first_time]);
	chmod(time_file_name, S_IRWXU|S_IRGRP|S_IXGRP|S_IROTH);
	
	if(!time_file){
		fprintf(stderr, "ERROR: Can't open file %s, reason %s\n", time_file_name, strerror(errno));
		exit(17);
	}
	
	if(first_time) {
		fprintf(time_file, "#!/bin/sh\n");
		fprintf(time_file, "#\n");
		fprintf(time_file, "# This is shell script to produce Quick-Look images\n");
		fprintf(time_file, "#\n");
		fprintf(time_file, "#     %s %s\n", __FILE__, Version);
		fprintf(time_file, "#\n");
		fprintf(time_file, "SCRIPT_PATH=\"%s\"\n", pScriptPath);
		fprintf(time_file, "cd BROWSE\n");
		fprintf(time_file, "rm -f *.log\n");
		fprintf(time_file, "#\n");
		first_time = 0;
	}

	if(end_flag) {
		fprintf(time_file, "cd ..\n");
		fprintf(time_file, "#\n");
		fclose(time_file);
		return 0;
	}
	
	if(detail_record->filename) {
		if(detail_record->filename[0]) {
			strcpy(filename, detail_record->filename);
		}
	}
	if(detail_record->thumbname) {
		if(detail_record->thumbname[0]) {
			strcpy(thumbname, detail_record->thumbname);
		}
	}
	
	if(!filename[0] || !thumbname[0]){
		fprintf(stderr, "ERROR: Couldn't get filename for plot\n");
		exit(17);
	}
	
	if(Plot_Flag) {
		if(detail_record->plot_start) {
			if(detail_record->plot_start[0]) {
				strcpy(start_time, detail_record->plot_start);
				start_time[17] = 0;
			}
		}
	}
	else {
		if(detail_record->scet_start) {
			if(detail_record->scet_start[0]) {
				strcpy(start_time, detail_record->scet_start);
				start_time[17] = 0;
			}
		}
	}
	
	if(!start_time[0]){
		fprintf(stderr, "WARNING: For plot '%s', start_time is empty!", detail_record->filename);
		fclose(time_file);
		return 0;
	}
	
	if(Plot_Flag) {
		if(detail_record->plot_stop) {
			if(detail_record->plot_stop[0]) {
				strcpy(stop_time, detail_record->plot_stop);
				stop_time[17] = 0;
			}
		}
	}
	else {
		if(detail_record->scet_stop) {
			if(detail_record->scet_stop[0]) {
				strcpy(stop_time, detail_record->scet_stop);
				stop_time[17] = 0;
			}
		}
	}
	
	if(!stop_time[0]){
		fprintf(stderr, "WARNING: For plot '%s', stop_time is empty!", detail_record->filename);
		fclose(time_file);
		return 0;
	}
	
	/* If plot is not at least 1-second long, don't bother */
	if(strcmp(start_time, stop_time) == 0){ 
		fprintf(stderr, "INFO: Skipping '%s' plot, it would have been less than "
				  "1 second long\n", detail_record->filename);
		fclose(time_file);
		return 0;
	}
	
	if(!current_directory[0])
		getcwd(current_directory, 127);
	
	sprintf(title, "Cassini %s %s", Inst_Band[detail_record->fband],
			  Inst_Label[detail_record->instrument]);
	
	if(detail_record->Orbit) {
		if(detail_record->Orbit[0]) {
			sprintf(orbit, "Orbit %s",  detail_record->Orbit);
		}
		else {
			sprintf(orbit, "Cruise");
		}
	}

  fprintf(time_file, "#DBG %d  filepath1:  %s\n", __LINE__, detail_record->filepath1);
  fprintf(time_file, "#DBG %d  filepath2:  %s\n", __LINE__, detail_record->filepath2);
  fprintf(time_file, "#DBG %d  filepath3:  %s\n", __LINE__, detail_record->filepath3);
  fprintf(time_file, "#DBG %d  filename:   %s\n", __LINE__, detail_record->filename);
  fprintf(time_file, "#DBG %d  thumbname:  %s\n", __LINE__, detail_record->thumbname);
  fprintf(time_file, "#DBG %d  plot_start: %s\n", __LINE__, detail_record->plot_start);
  fprintf(time_file, "#DBG %d  plot_stop:  %s\n", __LINE__, detail_record->plot_stop);
  
	switch(detail_record->instrument) {
	case RPWS_LABEL_WBR:
	case RPWS_LABEL_WFR:
		fprintf(
			time_file, "cd %s\n", 
		   detail_record->instrument ? directory[RPWS_ARCHIVE_BROWSE_WFR]: 
				      directory[RPWS_ARCHIVE_BROWSE_WBR]
		);
		
		if(detail_record->filepath1[0])
			fprintf(time_file, "cd %s\n", detail_record->filepath1);
		
		if(detail_record->filepath2[0])
			fprintf(time_file, "cd %s\n", detail_record->filepath2);
		  
		if(detail_record->filepath3[0])
			fprintf(time_file, "cd %s\n", detail_record->filepath3);
		
		fprintf(time_file, "echo cd `pwd`/%s\n", filename);
		
		
		fprintf(time_file, "# Line: %d\n", __LINE__);
		fprintf(time_file, "if [ -f %s.PNG ] ; then\n", thumbname);
		fprintf(time_file, "   echo Exists %s.PNG\n", thumbname);
		fprintf(time_file, "else\n");
		fprintf(time_file, "   echo ${SCRIPT_PATH}/rpws_hr_browse_%s_%s %s \"%s\" "
		        "\"%s\" \"%s\" \"%s\"\n", inst_label[detail_record->instrument],
		        plot_band(detail_record), body, start_time, stop_time, title,
		        orbit);
		fprintf(time_file, "   ${SCRIPT_PATH}/rpws_hr_browse_%s_%s %s \"%s\" "
		        "\"%s\" \"%s\" \"%s\"", inst_label[detail_record->instrument],
		        plot_band(detail_record), body, start_time, stop_time, title,
		        orbit);
			
		if(detail_record->mode_mask & MODE_MASK_HFR) {
			int flower=0;
			int fcenter=0;
			int fupper=0;
			fcenter = rpws_hfr_xlate_center_freq(detail_record->hfr_xlate);
			if(fcenter) {
				flower = fcenter - 15;
				fupper = fcenter + 15;
				fprintf(time_file, " %d %d", flower, fupper);
			}
		}
		fprintf(time_file, "\n");
		fprintf(time_file, "fi\n");
		
		fprintf(time_file, "if [ -f HTML/%s_TN.PNG ] ; then\n", thumbname);
		fprintf(time_file, "   echo Exists HTML/%s_TN.PNG\n", thumbname);
		fprintf(time_file, "else\n");
		
		fprintf(time_file, "   echo ${SCRIPT_PATH}/rpws_hr_thumb %s\n", thumbname);
		fprintf(time_file, "   ${SCRIPT_PATH}/rpws_hr_thumb %s\n", thumbname);

		fprintf(time_file, "fi\n");
		  
		if(detail_record->filepath1[0]) fprintf(time_file, "cd ..\n");
		if(detail_record->filepath2[0]) fprintf(time_file, "cd ..\n");
		if(detail_record->filepath3[0]) fprintf(time_file, "cd ..\n");
		  
		fprintf(time_file, "cd ..\n");
		fprintf(time_file, "#\n");
		break;
		  
	default:
		break;
	}
	
  fclose(time_file);
  return 1;
}

/*****************************************************************************/
int rpws_browse_detail(const struct RPWS_LABEL *detail_record)
   {
     struct RPWS_LABEL *new_record;
     struct RPWS_LABEL *link_record;
     char filename[128];
     char *temp[2];
     int status = 1;
     int type = 0;

     /*******************************************
      *		Skip DUST impact data		*
      *******************************************/
     if(strstr(detail_record->filename,"KHZD"))
         return 0;
     if(strstr(detail_record->filename,"RAW"))
         return 0;
     
     switch(detail_record->fband)
       {
         case ARCH_BAND_25HZ:
             type = 1;
             break;
         case ARCH_BAND_2500HZ:
             type = 2;
             break;
         case ARCH_BAND_10KHZ:
             type = 3;
             break;
         case ARCH_BAND_75KHZ:
             type = 4;
             break;
        }

     /***************************************************
      *		Strip out record size character		*
      *		Have to do something special here	*
      *		to handle HFWBR				*
      ***************************************************/
     strcpy(filename, detail_record->filename);
     if(temp[0] = strstr(filename, "HZ"))
       {
         temp[1] = strstr(detail_record->filename, "HZ");
         strcpy(temp[0]+2, temp[1]+3);
        }

     if(RPWS_BROWSE_data_record[type])
       {
         link_record = RPWS_BROWSE_data_record[type];
         while(link_record->link)
           {
             link_record = link_record->link;
            }
         if(link_record->filename)
             if(link_record->filename[0])
               {
                 if(!strcmp(filename, link_record->filename))	/* new filename ??? */
                     status = 0;
		}
        }

     if(status)
       {
         /* We don't need to copy the text buffers, the pointers will continue to
            point to the intended text */
         new_record = malloc(sizeof(struct RPWS_LABEL)         -
         		     sizeof(new_record->text_area)     +
         		            RPWS_LABEL_MISC_BUFFER_SIZE);
         memcpy(new_record, detail_record, sizeof(struct RPWS_LABEL)    - 
         				   sizeof(new_record->text_area));
         new_record->filename = new_record->text_area[0];	/* private filename buffer */
         strcpy(new_record->filename, filename);		/* grab filename */
         new_record->link = NULL;

         if(!RPWS_BROWSE_data_record[type])
             RPWS_BROWSE_data_record[type] = new_record;
         else if(1)
           {
             sort_label(new_record, &RPWS_BROWSE_data_record[type], 0);
            }
         else
           {
             link_record = RPWS_BROWSE_data_record[type];
             while(link_record->link)
               {
                 link_record = link_record->link;
                }
             link_record->link = new_record;
            }

/*       fprintf(stdout, "I %s %d %s %s %s\n", __FILE__, __LINE__, filename, new_record->scet_start, new_record->scet_stop);  / **/
        }
     else
       {
         link_record = RPWS_BROWSE_data_record[type];
         while(link_record->link)
           {
             link_record = link_record->link;         
            }
                                                                      
         if( strcmp(detail_record->sclk_start, link_record->sclk_start) < 0)
           {
             link_record->sclk_start = detail_record->sclk_start;
             link_record->scet_start = detail_record->scet_start;
             link_record->ephem_start = detail_record->ephem_start;
            }
         if( strcmp(detail_record->sclk_stop, link_record->sclk_stop) > 0)
           {
             link_record->sclk_stop = detail_record->sclk_stop;
             link_record->scet_stop = detail_record->scet_stop;
             link_record->ephem_stop = detail_record->ephem_stop;
            }
/*       fprintf(stdout, "U %s %d %s %s %s\n", __FILE__, __LINE__, link_record->filename, link_record->scet_start, link_record->scet_stop);  / **/
        }
     return status;
    }

/*****************************************************************************/
/** Generate script make_hr_browse.bat for browse data generation after this
 * program is run.
 *
 *@param directory directory array
 *
 *@param body EARTH JUPITER SATURN, etc.
 *
 *@param pScriptPath path to sub-scripts directory, should contain the
 *       following scripts:
 *           rpws_wbr_browse_10Khz, rpws_wbr_browse_75Khz, 
 *           rpws_wbr_browse_HFR, rpws_wfr_browse_25Hz,
 *           rpws_wfr_browse_2_5Khz, rpws_thumbnail
 *
 *@return the number of browse images that will be made by the script.
 */
int rpws_browse(const char *directory[], char *body, const char* pScriptPath){
	
	int count = 0;
	struct RPWS_LABEL *detail_record;
	int index;
	for(index=1; index<5; index++) {
		detail_record = RPWS_BROWSE_data_record[index];
		while(detail_record) {
			rpws_browse_record(detail_record, directory, body, 0, pScriptPath);
			/* fprintf(stdout, "P %s %d %s %s %s\n", __FILE__, __LINE__, 
			 * detail_record->filename, detail_record->scet_start, 
			 * detail_record->scet_stop);  */
			
			detail_record = detail_record->link;
			count++;
		}
	}
	rpws_browse_record(NULL, directory, body, 1, pScriptPath);
	return count;
}

/*****************************************************************************/
static char* rpws_duration(double duration)
{
	static char text[128];
	int hours;
	int minutes;
	int seconds;
     
	hours = duration / 3600;
	minutes = duration / 60;
	minutes %= 60;
	seconds = duration;
	seconds %= 60;
	sprintf(text, "%02d:%02d:%02d", hours, minutes, seconds);
	return text;
}


/*****************************************************************************/
static int rpws_browse_PDS_label_1(const int index, char *thumb_flag)
{
	int count = 0;
	struct RPWS_LABEL *detail_record;
     
	detail_record = RPWS_BROWSE_data_record[index];
	while(detail_record){
		/* fprintf(stdout, "%s %D %p=detail_record\n", __FILE__, __LINE__, detail_record); / **/
		/* fflush(stdout); / **/
		rpws_label_browse_image(detail_record, NULL, NULL, thumb_flag, -1);
		detail_record = detail_record->link;
		count++;
	}
	return count;
}

/*****************************************************************************/
 char *rpws_browse_splat(char *buf)
{
	static char temp[128];
     int index;
     int outdex;

     outdex = 0;
     for(index=0; index<strlen(buf); index++)
       {
         temp[outdex++] = buf[index];
         if(buf[index] == 'Z')
             temp[outdex++] = '*';
        }
     temp[outdex] = 0;
     return temp;
}

/*****************************************************************************/
static void rpws_browse_html_record(
	struct RPWS_LABEL *detail_record, const char *directory, 
	const char *sub_directory, const int flag
){
	static FILE *html_file = NULL;
	static char html_filename[128] = {0};
	static char file_path[256] = {0};
	static int index = -1;
	
	switch(flag) {
		case 0:	/* WBR FREQ */
		case 1:	/* WFR FREQ */
			if(sub_directory)
				sprintf(html_filename, "%s/%s/BROWSE_%sFR_FREQ.HTM", directory, sub_directory, Inst_Label[flag%10]);
			else
				sprintf(html_filename, "%s/BROWSE_%sFR_FREQ.HTM", directory, Inst_Label[flag%10]);
			break;
		case 10:	/* WBR TIME */
		case 11:	/* WFR TIME */
			if(sub_directory)
				sprintf(html_filename, "%s/%s/BROWSE_%sFR_TIME.HTM", directory, sub_directory, Inst_Label[flag%10]);
			else
				sprintf(html_filename, "%s/BROWSE_%sFR_TIME.HTM", directory, Inst_Label[flag%10]);
			break;
		case 20:	/* WBR 10KHz	*/
			if(sub_directory)
				sprintf(html_filename, "%s/%s/BROWSE_WBRFR_10KHZ.HTM", directory, sub_directory);
			else
				sprintf(html_filename, "%s/BROWSE_WBRFR_10KHZ.HTM", directory);
			break;
		case 21:	/* WFR 25Kz	*/
			if(sub_directory)
				sprintf(html_filename, "%s/%s/BROWSE_WFRFR_26HZ.HTM", directory, sub_directory);
			else
				sprintf(html_filename, "%s/BROWSE_WFRFR_26HZ.HTM", directory);
			break;
		case 30:	/* WBR 75Khz	*/
			if(sub_directory)
				sprintf(html_filename, "%s/%s/BROWSE_WBRFR_80KHZ.HTM", directory, sub_directory);
			else
				sprintf(html_filename, "%s/BROWSE_WBRFR_80KHZ.HTM", directory);
			break;
		case 31:	/* WFR 2.5Khz	*/
			if(sub_directory)
				sprintf(html_filename, "%s/%s/BROWSE_WFRFR_2500HZ.HTM", directory, sub_directory);
			else
				sprintf(html_filename, "%s/BROWSE_WFRFR_2500HZ.HTM", directory);
			break;
	}
	
	switch(flag) {
		case 0:
		case 1:
		case 10:
		case 11:
		case 20:
		case 21:
		case 30:
		case 31:
			errno = 0;
			if(html_file) fclose(html_file);
			if( (html_file = fopen(html_filename, "a")) == NULL){
				fprintf(stderr, "Couldn't open %s for appending, %s\n", html_filename, strerror(errno));
				exit(17);
			}
			if(comment_flag & 0x100000)
				fprintf(html_file, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n");
			else
				fprintf(html_file, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n");
			fprintf(html_file, "<HTML>\n");
			fprintf(html_file, "<HEAD>\n");
			fprintf(html_file, "<META NAME=\"Keywords\" CONTENT=\"Cassini/RPWS\">\n");
			fprintf(html_file, "<META NAME=\"Producer\" CONTENT=\"%s\">\n", __FILE__);
			fprintf(html_file, "  <META NAME=\"Version\" CONTENT=\"%s\">\n", Version);
#ifdef DEBUG
			fprintf(html_file, "  <META NAME=\"BISflag\" CONTENT=\"0x%X\">\n", bisflag);
#endif
break;
	}
	
	/* by hook or by crook, html_file must defined now */
	if(html_file == NULL){
		fprintf(stderr, "ERROR: in rpws_browse_html_record(), html_file is NULL!\n");
		exit(17);
	}
	
	switch(flag) {
		case 0:
		case 10:
		case 20:
		case 30:
			fprintf(html_file, "<META NAME=\"Keywords\" CONTENT=\"WBR\"\n");
			break;
		case 1:
		case 11:
		case 21:
		case 31:
			fprintf(html_file, "<META NAME=\"Keywords\" CONTENT=\"WFR\"\n");
			break;
	}
	switch(flag) {
	 case 0:
	 case 1:
	 case 10:
	 case 11:
	 case 20:
	 case 21:
	 case 30:
	 case 31:
		fprintf(html_file, "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\">\n");
		fprintf(html_file, "<TITLE>");
		fprintf(html_file, "    %s Browse Image Selection    \n", Inst_Label[flag%10]);
		fprintf(html_file, "  </TITLE>\n");
#ifdef DEBUG
		fprintf(html_file, "    <!-- %s/%d bisflag = %08X-->\n", __FILE__, __LINE__, bisflag);
#endif
		fprintf(html_file, "  </HEAD>\n");
		fprintf(html_file, "<BODY>\n");
#ifdef DEBUG
		fprintf(html_file, "    <!-- %s/%d -->\n", __FILE__, __LINE__);
#endif
		fprintf(html_file, "<H1>\n");
	break;
	}

	switch(flag) {
		case 0:
		case 10:
		case 20:
		case 30:  fprintf(html_file, "WBR Browse Dataset,  "); break;
		
		case 1:
		case 11:
		case 21:
		case 31: fprintf(html_file, "WFR Browse Dataset,  ");  break;
	}
	switch(flag) {
		case 0:
		case 1:  fprintf(html_file, "Frequency Ordered\n");    break;
		case 10:
		case 11: fprintf(html_file, "Time Ordered\n");         break;
		case 20: fprintf(html_file, "WBR Low Band Subset\n");  break;
		case 21: fprintf(html_file, "WFR Low Band Subset\n");  break;
		case 30: fprintf(html_file, "WBR High Band Subset\n"); break;
		case 31: fprintf(html_file, "WFR High Band Subset\n"); break;
	}

	switch(flag) {
		case 0:
		case 1:
		case 10:
		case 11:
		case 20:
		case 21:
		case 30:
		case 31:
			fprintf(html_file, "  </H1>\n");
			fprintf(html_file, "Click on an image to view enlarged (full screen)\n");
			fprintf(html_file, "<TABLE");
			if(bisflag & BISCELLSPACING)
				fprintf(html_file, " cellspacing=10");
			fprintf(html_file, ">\n");
			fprintf(html_file, "\n");
			index = 0;
			break;
	}

	switch(flag) {
		case -1:
			strcpy(file_path, "/");
			if(detail_record->filepath1[0]) {
				strcat(file_path, detail_record->filepath1);
				strcat(file_path, "/");
			}
			if(detail_record->filepath2[0]) {
				strcat(file_path, detail_record->filepath2);
				strcat(file_path, "/");
			}
			if(detail_record->filepath3[0]) {
				strcat(file_path, detail_record->filepath3);
				strcat(file_path, "/");
			}
			if(!index) fprintf(html_file, "<TR ALIGN=\"CENTER\">\n");
			fprintf(html_file, "<TD>\n");
			fprintf(html_file, "    <A HREF=\".%sHTML/%s.HTM\">\n", file_path, detail_record->thumbname);
			fprintf(html_file, "    <IMG \n");
			fprintf(html_file, "     ALT=\"%s Spectrogram\"\n", rpws_browse_splat(detail_record->filename));
			fprintf(html_file, "     SRC=\".%sHTML/%s_TN.PNG\">\n", file_path, detail_record->thumbname);

			if(bisflag & BISFILENAME) {
				fprintf(html_file, "    <BR>\n");
				fprintf(html_file, "        %s\n", detail_record->filename);
			}
			
			fprintf(html_file, "    </A>\n");
			fprintf(html_file, "    <BR>%s %s\n", 
					  Inst_Band[detail_record->fband],
					  Inst_Label[detail_record->instrument]);
			
			if((bisflag & BISDAY) || (bisflag & BISSTART)) {
				switch(TIMEFLAG) {
					case 3:
						fprintf(html_file, "    <BR>%s\n", detail_record->scet_start_3);
						break;
					case 2:
						fprintf(html_file, "    <BR>Start %s\n", detail_record->scet_start_2);
						break;
					case 1:
						fprintf(html_file, "    <BR>Start %s\n", detail_record->scet_start);
						break;
				}
			}
			
			if(bisflag & BISSTOP) {
				fprintf(html_file, "    <BR>\n");
				fprintf(html_file, "       Stop %s\n",
						  detail_record->scet_stop);
			}
			
			/*	     fprintf(stdout, "HTML WRITE %s (%d) %s %s\n",
			 * detail_record->filename, detail_record->instrument,
			 * detail_record->scet_start, detail_record->scet_stop);	/ **/
			/*             fflush(stdout);							/ **/
			index++;
			index &= 0x03;
			break;
	}

     switch(flag)
       {
         case -2:
             fprintf(html_file, "  </TABLE>\n");
             fprintf(html_file, "<BR>\n");
             if(sub_directory)
                 fprintf(html_file, "<A HREF=\"../../%s/%s\">", directory, sub_directory);
             else
                 fprintf(html_file, "<A HREF=\"../../%s\">", directory);
             fprintf(html_file, "  Data file directory link  \n");
             fprintf(html_file, "  </A>\n");
             fprintf(html_file, "<BR>\n");
             fprintf(html_file, "<BR>\n");
             fprintf(html_file, "Some spectrograms may be missing (i.e. they show up\n");
             fprintf(html_file, "with a <i>Data File Questionable</i> icon).  This is typically\n");
             fprintf(html_file, "due to corrupted data.  The <i>potentially</i> corrupt data\n");
             fprintf(html_file, "is retained in the archive in spite of the inability of the\n");
             fprintf(html_file, "display tools to generate a spectrogram for the period.\n");

             fprintf(html_file, "<BR><BR>\n");
             fprintf(html_file, "Occasional waveforms from the DUST analysis routine are \n");
             fprintf(html_file, "delivered to ground as WBR data.  This data is saved\n");
             fprintf(html_file, "in WBRFULL and marked to distinguish it from other WBR\n");
             fprintf(html_file, "data.  We do <b>not</b> attempt to produce spectrograms\n");
             fprintf(html_file, "for this dust data.\n");
             fprintf(html_file, "<BR>\n");
             fprintf(html_file, "<i>(This does not apply to WFR data).</i>\n");
        }

     switch(flag)
       {
         case -2:

             fprintf(html_file, "  <P>\n");

             fprintf(html_file, "    ");

             if(bisflag & BISVERSION)
                 fprintf(html_file, "<!-- ");
             fprintf(html_file, "Produced by %s %s", __FILE__, Version);
             if(bisflag & BISVERSION)
                 fprintf(html_file, " -->");

             fprintf(html_file, "\n");
#ifdef DEBUG
             fprintf(html_file, "    <!-- %s/%d -->\n", __FILE__, __LINE__);
#endif
             fprintf(html_file, "   <p>\n");
             fprintf(html_file, "    <a href=\"http://validator.w3.org/check/referer\">\n");
             fprintf(html_file, "      <img border=\"0\"\n");
	     if(comment_flag & 0x100000)
	       {
                 fprintf(html_file, "       src=\"../../BROWSE/ANCILLARY/VALID_HTML401.PNG\"\n");
                 fprintf(html_file, "       alt=\"Valid HTML 4.01!\" height=\"31\" width=\"88\">\n");
                }
	     else
	       {
                 fprintf(html_file, "       src=\"../../BROWSE/ANCILLARY/VALID_HTML32.PNG\"\n");
                 fprintf(html_file, "       alt=\"Valid HTML 3.2!\" height=\"31\" width=\"88\">\n");
                }
             fprintf(html_file, "      </a>\n");
             fprintf(html_file, "     </p>\n");
             fprintf(html_file, "  </BODY>\n");
             fprintf(html_file, "  </HTML>\n");
/*	     fprintf(stdout, "HTML CLOSE\n");  	/ **/
/*           fflush(stdout);			/ **/
             break;
        }
		  if(html_file!=NULL)fclose(html_file);
}

/*****************************************************************************/
static int rpws_browse_html_record_1(
	const char *directory, const char *sub_directory, const int band,
	const int index
){
	int count = 0;
	struct RPWS_LABEL *detail_record;
	
	detail_record = RPWS_BROWSE_data_record[index];
	
	while(detail_record) {
		/* fprintf(stdout, "%s %D %p=detail_record\n", __FILE__, __LINE__, detail_record); / **/
		/* fflush(stdout); / **/
		if((band == -1)||(band == detail_record->fband))
			rpws_browse_html_record(detail_record, directory, sub_directory, -1);
		detail_record = detail_record->link;
		count++;
	}
	return count;
}

/*****************************************************************************/
int rpws_browse_sort(
	struct RPWS_LABEL **sorted_list, struct RPWS_LABEL *list_1, 
	struct RPWS_LABEL *list_2
){
     int status = 0;
     struct RPWS_LABEL *new_list = NULL;
     struct RPWS_LABEL *new_member;
     struct RPWS_LABEL *temp;

     temp = list_1;
     while(temp)
       {
         new_member = malloc(sizeof(struct RPWS_LABEL) - sizeof(new_member->text_area));
         memcpy(new_member, temp, sizeof(struct RPWS_LABEL) - sizeof(new_member->text_area)); /* data in existing structures */
         new_member->link = NULL;
         /* fprintf(stdout, "%s %d P:%p SCLK:%s/%s\n", __FILE__, __LINE__, new_member, new_member->scet_start, temp->scet_start); / **/
         /* fflush(stdout); / **/
         sort_label(new_member, &new_list, 0);
         temp = temp->link;
         status++;
        }
     temp = list_2;
     while(temp)
       {
         new_member = malloc(sizeof(struct RPWS_LABEL) - sizeof(new_member->text_area));
         memcpy(new_member, temp, sizeof(struct RPWS_LABEL) - sizeof(new_member->text_area)); /* data in existing structures */
         new_member->link = NULL;
         /* fprintf(stdout, "%s %d P:%p SCLK:%s/%s\n", __FILE__, __LINE__, new_member, new_member->scet_start, temp->scet_start); / **/
         /* fflush(stdout); / **/
         sort_label(new_member, &new_list, 1);
         temp = temp->link;
         status++;
        }
     *sorted_list = new_list;
     temp = new_list;
     while(temp)
       {
         /* fprintf(stdout, "%s %d list-P:%p \n", __FILE__, __LINE__, temp);/ **/ 
         temp = temp->link;
        }
     /* fflush(stdout); / **/
     return status;
    }

/*****************************************************************************/
/* iflag: 0=WBR 1=WFR */

int rpws_browse_html( char *directory[], int iflag, int debug)
{
	int count = 0;
	int flag = 0;
	char temp[128];
	
	/* SORT data in time-order *****************/
	comment_flag = debug;
	switch(iflag){
		
	case RPWS_LABEL_WBR:
		flag += rpws_browse_sort(&RPWS_BROWSE_data_record[6],
		                         RPWS_BROWSE_data_record[3], 
		                         RPWS_BROWSE_data_record[4]);
		break;
	
	case RPWS_LABEL_WFR:
		flag += rpws_browse_sort(&RPWS_BROWSE_data_record[5], 
		                         RPWS_BROWSE_data_record[1], 
		                         RPWS_BROWSE_data_record[2]);
		break;
	}
	
	/* Frequency Ordered HTML ******************/
	switch(iflag){
	
	case RPWS_LABEL_WBR:
		rpws_browse_html_record(NULL, directory[RPWS_ARCHIVE_BROWSE],
		                        directory[RPWS_ARCHIVE_BROWSE_WBR], iflag);
		
		count+=rpws_browse_html_record_1(directory[RPWS_ARCHIVE_BROWSE], 
							 directory[RPWS_ARCHIVE_BROWSE_WBR], -1, 3); 
		
		count+=rpws_browse_html_record_1(directory[RPWS_ARCHIVE_BROWSE], 
							 directory[RPWS_ARCHIVE_BROWSE_WBR], -1, 4);
		
		rpws_browse_html_record(NULL, directory[iflag], NULL, -2);
		break; 
		
	case RPWS_LABEL_WFR:
		rpws_browse_html_record(NULL, directory[RPWS_ARCHIVE_BROWSE], 
		                        directory[RPWS_ARCHIVE_BROWSE_WFR], iflag);
		
		count+=rpws_browse_html_record_1(directory[RPWS_ARCHIVE_BROWSE], 
							 directory[RPWS_ARCHIVE_BROWSE_WFR], -1, 1); 
		
		count+=rpws_browse_html_record_1(directory[RPWS_ARCHIVE_BROWSE], 
							 directory[RPWS_ARCHIVE_BROWSE_WFR], -1, 2);
		
		rpws_browse_html_record(NULL, directory[iflag], NULL, -2);
		break; 
	}
		
	/* Time Ordered HTML	 **********************/
     switch(iflag)
       {
         case RPWS_LABEL_WBR:
             rpws_browse_html_record(NULL, 
             				directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WBR], 
             				iflag+10);
             count+=rpws_browse_html_record_1(directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WBR], 
             				-1,
             				6);
             rpws_browse_html_record(NULL, 
             				directory[iflag], 
             				NULL, 
             				-2);
             break; 
         case RPWS_LABEL_WFR:
             rpws_browse_html_record(NULL, 
             				directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WFR], 
             				iflag+10);
             count+=rpws_browse_html_record_1(directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WFR], 
             				-1,
             				5);
             rpws_browse_html_record(NULL, 
             				directory[iflag], 
             				NULL, 
             				-2);
             break; 
        }
		  
	/* Time Ordered HTML, subsets **************/
	switch(iflag)
       {
         case RPWS_LABEL_WBR:
             rpws_browse_html_record(NULL, 
             				directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WBR], 
             				iflag+20);
             count+=rpws_browse_html_record_1(directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WBR], 
             				ARCH_BAND_10KHZ,
             				6);
             rpws_browse_html_record(NULL, 
             				directory[iflag], 
             				NULL, 
             				-2);

             rpws_browse_html_record(NULL, 
             				directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WBR], 
             				iflag+30);
             count+=rpws_browse_html_record_1(directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WBR], 
             				ARCH_BAND_75KHZ,
             				6);
             rpws_browse_html_record(NULL, 
             				directory[iflag], 
             				NULL, 
             				-2);

             break; 

         case RPWS_LABEL_WFR:
             rpws_browse_html_record(NULL, 
             				directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WFR], 
             				iflag+20);
             count+=rpws_browse_html_record_1(directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WFR], 
             				ARCH_BAND_25HZ,
             				5);
             rpws_browse_html_record(NULL, 
             				directory[iflag], 
             				NULL, 
             				-2);

             rpws_browse_html_record(NULL, 
             				directory[RPWS_ARCHIVE_BROWSE], 
             				directory[RPWS_ARCHIVE_BROWSE_WFR], 
             				iflag+30);
				 
             count+=rpws_browse_html_record_1(
						     directory[RPWS_ARCHIVE_BROWSE], 
				           directory[RPWS_ARCHIVE_BROWSE_WFR], 
             				ARCH_BAND_2500HZ, 5
				        );
             rpws_browse_html_record(NULL, directory[iflag], NULL, -2);

             break; 

        }
 
	return count;
}
