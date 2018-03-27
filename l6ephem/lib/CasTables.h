/*
July 9, 2008
	Added mission Phase of "EM"
*/

#ifndef CasMiss_H_
#define CasMiss_H_

/* For now these functions consult a compiled in table, in the future this
   should change to an external lookup file. -cwp */

char* get_mission_phase_name(char *sScetBeg, char *sScetEnd);
char* get_target_name(char *sScetBeg, char *sScetEnd);
char* get_target_name_special_pds(char *sScetBeg, char *sScetEnd);

#endif /* CasMiss_H_ */
