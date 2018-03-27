/*
 * decomp.c
 *
 *  An example of a simple filter to decompress mp data
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include <fg.h>
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilt.h>

struct MP_buffer in_buffer;
struct RPWS_buffer *out_buffer;
FILE *input = NULL;
FILE *output = NULL;

static char *version={"decomp1 V1.2"};

static char *reason[] = {"fail            ",	/* 0 */
			 "success         ",	/* 1 */
			 "                ",	/* 2 */
			 "not processed   ",	/* 3 */
			 "                ",	/* 4 */
			 "unknown MP type ",	/* 5 */
			 "                ",	/* 6 */
			 "                "};	/* 7 */

int main(int argc, char *argv[])
{
	int ilen;
	int local_flag = FILE_LOCATE_DEFAULT;
	int debug_flag = 0;
	char *in_name = {"stdin"};
	char *out_name = {"stdout"};
	int eof_flag = UTIL_GET_BLOCKING;

	struct tm *pkt_tm;
	struct tm *pkt_ev;
	int t_mask = -1;
	int time_offset = 0;
	static int epoch;
	time_t pkt_time, pkt_etime, pkt_epoc;
	struct event_time *evt_tim;
	struct event_clock evt_clk;
	char str0[256];
	int flag_yday = 1;
	                                
	fg_flags(argc,argv);
	
	if(fg_flag("help") || fg_flag("h")){
		
		fprintf(stderr,"%s   HELP SCREEN\n", version);
		fprintf(stderr,"\n");
		fprintf(stderr,"     +find fn    find archive file\n");
		fprintf(stderr,"     +getmp      get data from current file\n");
		fprintf(stderr,"     +putmp      put data into current file\n");
		fprintf(stderr,"     +getmpus    get data from current file\n");
		fprintf(stderr,"     +putmpus    put data into current file\n");
		fprintf(stderr,"     +eof        pass eof from input file\n");
		fprintf(stderr,"     +debug      debug information\n");
		fprintf(stderr,"\n");
		fprintf(stderr,"        ---  +putxx and =getxx are mutually exclusive\n");
		fprintf(stderr,"\n");
		
		return 0;
	}

 
	if(fg_flag("eof"))
		eof_flag = UTIL_GET_NON_BLOCKING;

	if(fg_flag("debug"))
		debug_flag = 1;

	if(fg_flag("find")){
		
		input = UTIL_find_open(fg_flagc("find"), "rb");
		if(input){
			fprintf(stderr,"%s find: %s\n", version, UTIL_find_name());
		}
		else{
			fprintf(stderr,"%s find: file not found: %s\n", version,
					  fg_flagc("find"));
	      return 13;
		}
	}
    
	if(fg_flag("getmp")){
		input = UTIL_FILEname(FILE_MP,local_flag);
		in_name = UTIL_filename(FILE_MP,local_flag);
	}
	else{
		if(fg_flag("getmpus")){
			input = UTIL_FILEname(FILE_MPUS,local_flag);
			in_name = UTIL_filename(FILE_MPUS,local_flag);
		}    
	}

	if(fg_flag("putmp")){
		output = UTIL_FILEnew(FILE_MP,local_flag);
		out_name = UTIL_filename(FILE_MP,local_flag);
	}
	else{
		if(fg_flag("putmpus")){
			output = UTIL_FILEnew(FILE_MPUS,local_flag);
			out_name = UTIL_filename(FILE_MPUS,local_flag);
		}
	}

	if((input && output)){
		fprintf(stderr,"%s: don't do 2 files at once !\n", version);
		return 13;
	}
	
	if(!input)	input = stdin;
	if(!output) output = stdout;

	fprintf(stderr,"%s: input file: %s\n", version, in_name);
	fprintf(stderr,"%s: output file: %s\n", version, out_name);
	sleep(3);
	
	ilen = UTIL_getbuffer_MP(&in_buffer,input,eof_flag);
	
    while (ilen>0){
		out_buffer = UTIL_decompress(&in_buffer);
		out_buffer->packet.index.data_length = UTIL_extract_MP_length(
				(struct MP_buffer*)out_buffer );
		
		if(debug_flag){
	    /*
	     *	TIME PROCESSING  ********************************************
	     */

			epoch = (in_buffer.packet.cds_tag.epoch[0]  << 24) |
			        (in_buffer.packet.cds_tag.epoch[1]  << 16) |
			        (in_buffer.packet.cds_tag.epoch[2]  <<  8) |
			        (in_buffer.packet.cds_tag.epoch[3]  <<  0) ;

			pkt_etime = UTIL_event_time(&in_buffer, 0);
			pkt_epoc = pkt_etime + epoch; 
	    
			if(epoch){
				evt_clk.seconds = pkt_etime;
				evt_clk.fine = UTIL_extract_MP_RTI(&in_buffer)<<5;
				evt_tim = UTIL_event_scet(&in_buffer, evt_clk);
				pkt_ev = UTIL_event_scet_tm(*evt_tim, 0);
				sprintf( str0, "%8X %02X%02X (%4d-%03dT%2.2d:%2.2d:%2.2d.%3.3d)",
				        pkt_etime,
				        in_buffer.packet.mpp.mini_packet[3],
				        in_buffer.packet.mpp.mini_packet[2],
				        pkt_ev->tm_year+1900,
				        pkt_ev->tm_yday+1,
				        pkt_ev->tm_hour,
				        pkt_ev->tm_min,
				        pkt_ev->tm_sec,
				        evt_tim->milliseconds % 1000);
			}
			else{
				pkt_ev = gmtime(&pkt_epoc);
				sprintf( str0, "%8X %4X (%2.2d:%2.2d:%2.2d.%3.3d) Epoch %X",
				        pkt_etime,
				        (pkt_etime & 0x1FFF) << 3,
				        pkt_ev->tm_hour,
				        pkt_ev->tm_min,
				        pkt_ev->tm_sec,
				        UTIL_extract_MP_RTI(&in_buffer)*125,
				        epoch);
			}

			fprintf(stderr," %4d=ilen %4d=in.f_len %4d=out.f_len STS_CMP_%s  %s %s\n",
			        ilen,
			        in_buffer.f_length,
			        out_buffer->f_length,
			        reason[out_buffer->packet.compress.result&0x07],
			        UTIL_extract_MP_packet_type(&in_buffer),
			        str0);
		}
		
		if(out_buffer){
 	  
			if(1){
				out_buffer->f_length = out_buffer->packet.index.data_length + 3 + 272 + 15;
				out_buffer->f_length = out_buffer->f_length & 0x3FFF0;
				out_buffer->f_length = out_buffer->f_length - 4;
			}

			if(1)
				UTIL_putbuffr2_RPWS(out_buffer, output, out_buffer->f_length);
			else
				UTIL_putbuffer_RPWS(out_buffer, output);
		}
  
		ilen = UTIL_getbuffer_MP(&in_buffer,input,eof_flag);
	}
	fclose(input);
	fclose(output);
	
	return 0;
}
