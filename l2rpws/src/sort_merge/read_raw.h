struct SORT_ELEMENT *read_raw (char *input_list_file, char *start_time,
                               char *stop_time);
struct SORT_ELEMENT *read_mdb_file (char *start, char *stop);

char *read_raw_find_path (char *line, char *type);

extern int raw_read_stat_read;
extern int raw_read_stat_dupe;
extern int raw_read_stat_tail;
extern int raw_read_stat_head;
extern int raw_read_stat_inst;
extern int raw_read_stat_null;
