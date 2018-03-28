
#ifndef wfr_file_list_H_
#define wfr_file_list_H_


#define  MAXCHARS 200

struct file_structure {
	char line[MAXCHARS];
	int year;
	int day;
	int hr;
	int mn;
	int num_recs;
	float sec;
	double ms;
	double st_sec;
	double sp_sec;
};

/* If dataset == NULL, then compiled in default of 
   /opt/project/cassini/data/database/CassiniJPL.db is used
*/
void make_dbase(
	const char* dataset, double start, double stop, struct file_structure *dbase,
	int *num_files
);


#endif /* wfr_file_list_H_ */
