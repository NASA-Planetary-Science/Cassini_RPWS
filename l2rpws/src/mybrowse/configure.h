#define DSN_FILE	"DSN_FILE"
#define WAIT		"WAIT"
#define SLEEP		"SLEEP"
#define CRON_TIME	"CRON_TIME"
#define CRON_5_DAY	"CRON_5_DAY"
#define CRON_5_DAY_5	"CRON_5_DAY_5"
#define CRON_1_DAY	"CRON_1_DAY"
#define CRON_5_DAY_15	"CRON_5_DAY_15"
#define CRON_5_DAY_20	"CRON_5_DAY_20"
#define CRON_1_DAY_15	"CRON_1_DAY_15"
#define NERT_5_DAY	"NERT_5_DAY"
#define NERT_1_DAY	"NERT_1_DAY"

#define C_VALUES 64
char *configure (char *filename);
int dsn_tracking_file (char *dsn_file_name);
char *dsn_tracking_file_key (char *key);
int dsn_pass_number (int time_now);
