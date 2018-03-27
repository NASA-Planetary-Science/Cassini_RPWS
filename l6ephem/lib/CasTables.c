#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <assert.h>

/* Which POSIX standard version dose this code match */
#define _POSIX_C_SOURCE 200112L

typedef struct cas_mission_cat_tag{
const char *sList,*sScetBeg,*sScetEnd;
}CasMissionCat; 


CasMissionCat TargetName[]={
  {"VENUS",   "1998-116", "1998-116"},
  {"VENUS",   "1999-175", "1999-175"},
  {"EARTH",   "1999-227", "1999-257"},
  {"JUPITER", "2000-364", "2001-120"},
  {"SATURN",  "2004-010", "2018-001"},
  {"","",""}
};

CasMissionCat MissionPhaseName[]={
  {"LAUNCH",                        "1997-288",  "1997-290"},
  {"TCM 1",                         "1997-291",  "1997-318"},
  {"INTERPLANETARY CRUISE",         "1997-318",  "1999-311"},
  {"VENUS 1 CRUISE",                "1997-318",  "1998-256"},
  {"VENUS 1 ENCOUNTER",             "1998-116",  "1998-116"},
  {"INSTRUMENT CHECKOUT 1",         "1998-257",  "1999-073"},
  {"VENUS 2 - EARTH CRUISE",        "1999-074",  "1999-311"},
  {"VENUS 2 ENCOUNTER",             "1999-175",  "1999-175"},
  {"EARTH ENCOUNTER",               "1999-230",  "1999-230"},
  {"OUTER CRUISE",                  "1999-312",  "2002-188"},
  {"HIGH GAIN ANTENNA TRANSITION",  "1999-312",  "2000-127"},
  {"INSTRUMENT CHECKOUT 2",         "2000-127",  "2000-310"},
  {"JUPITER CRUISE",                "2000-310",  "2001-120"},
  {"JUPITER ENCOUNTER",             "2000-365",  "2000-365"},
  {"QUIET CRUISE",                  "2001-120",  "2002-189"},
  {"SCIENCE CRUISE",                "2002-189",  "2004-162"},
  {"SPACE SCIENCE",                 "2002-189",  "2004-011"},
  {"APPROACH SCIENCE",              "2004-012",  "2004-162"},
  {"TOUR PRE-HUYGENS",              "2004-163",  "2004-359"},
  {"PHOEBE ENCOUNTER",              "2004-163",  "2004-163"},
  {"SATURN ORBIT INSERTION",        "2004-183",  "2004-183"},
  {"TITAN A ENCOUNTER",             "2004-300",  "2004-300"},
  {"TITAN B ENCOUNTER",             "2004-348",  "2004-348"},
  {"HUYGENS DESCENT",               "2004-359",  "2005-014"},
  {"HUYGENS PROBE SEPARATION",      "2004-359",  "2004-359"},
  {"TITAN C HUYGENS",               "2005-014",  "2005-014"},
  {"TOUR",                          "2005-014",  "2008-182"},
  {"EXTENDED MISSION",              "2008-183",  "2010-272"},
  {"EXTENDED-EXTENDED MISSION",     "2010-273",  "2017-258"},
  {"END OF MISSION",                "2017-259",  "2027-001"},
  {"","",""}
  
  /* no sequences for now
  {"S1",  "2004-136", "2004-170"},
  {"S2",  "2004-171", "2004-212"},
  {"S3",  "2004-212", "2004-254"},
  {"S4",  "2004-255", "2004-289"},
  {"S5",  "2004-290", "2004-317"},
  {"S6",  "2004-318", "2004-350"},
  {"S7",  "2004-351", "2005-021"},
  {"S8",  "2005-018", "2005-053"},
  {"S9",  "2005-005", "2005-045"},
  {"S10", "2005-000", "2005-034"},
  {"S11", "2005-134", "2005-168"},
  {"S12", "2005-169", "2005-210"},
  {"S13", "2005-210", "2005-241"},
  {"S14", "2005-242", "2005-280"},
  {"S15", "2005-281", "2005-315"},
  {"S16", "2005-316", "2005-350"},
  {"S17", "2005-351", "2006-027"},
  {"S18", "2006-002", "2006-043"},
  {"S19", "2006-056", "2006-097"},
  {"S20", "2006-112", "2006-153"},
  {"S21", "2006-154", "2006-195"},
  {"S22", "2006-196", "2006-230"},
  {"S23", "2006-230", "2006-268"},
  {"S24", "2006-269", "2006-294"},
  {"S25", "2006-295", "2006-327"},
  {"S26", "2006-328", "2007-004"},
  {"S27", "2007-005", "2007-047"},
  {"S28", "2007-004", "2007-043"},
  {"S29", "2007-000", "2007-036"},
  {"S30", "2007-124", "2007-160"},
  {"S31", "2007-162", "2007-194"},
  {"S32", "2007-195", "2007-223"},
  {"S33", "2007-223", "2007-264"},
  {"S34", "2007-265", "2007-304"},
  {"S35", "2007-305", "2007-346"},
  {"S36", "2007-347", "2008-020"},
  {"S37", "2008-017", "2008-042"},
  {"S38", "2008-039", "2008-074"},
  {"S39", "2008-000", "2008-026"},
  {"S40", "2008-110", "2008-151"},
  {"S41", "2008-152", "2008-186"},
  {"END OF PRIME MISSION",          "2008-182",  "2008-182"},
*/
};

/* When using satalite target names Bill would like:
 *
 *  "Let's make this simple and just use +/- 1 hour for 
 *   any icy satellite and +/- 2 hours for targeted Titan 
 *   flybys."  -wsk 2016-09-09
 *
 * Thus this table needs updates.
 *
 */
CasMissionCat SatTargetName[]={
  {"Phoebe",        "2004-163", "2004-163"},
  {"Mimas",         "2004-183", "2004-183"},
  {"Titan",         "2004-184", "2004-184"},
  {"Titan",         "2004-300", "2004-300"},
  {"Titan",         "2004-348", "2004-348"},
  {"Dione",         "2004-350", "2004-350"},
  {"Iapetus",       "2005-001", "2005-001"},
  {"Titan",         "2005-012", "2005-012"},
  {"Titan",         "2005-038", "2005-038"},
  {"Enceladus",     "2005-004", "2005-004"},
  {"Enceladus",     "2005-006", "2005-006"},
  {"Tethys",        "2005-006", "2005-006"},
  {"Enceladus",     "2005-000", "2005-000"},
  {"Titan",         "2005-000", "2005-000"},
  {"Mimas",         "2005-105", "2005-105"},
  {"Titan",         "2005-106", "2005-106"},
  {"Tethys",        "2005-122", "2005-122"},
  {"Titan",         "2005-124", "2005-124"},
  {"Enceladus",     "2005-141", "2005-141"},
  {"Titan",         "2005-157", "2005-157"},
  {"Titan",         "2005-173", "2005-173"},
  {"Enceladus",     "2005-195", "2005-195"},
  {"Mimas",         "2005-214", "2005-214"},
  {"Titan",         "2005-218", "2005-218"},
  {"Titan",         "2005-234", "2005-234"},
  {"Titan",         "2005-250", "2005-250"},
  {"Tethys",        "2005-267", "2005-267"},
  {"Titan",         "2005-267", "2005-267"},
  {"Hyperion",      "2005-269", "2005-269"},
  {"Titan",         "2005-283", "2005-283"},
  {"Dione",         "2005-284", "2005-284"},
  {"Enceladus",     "2005-285", "2005-285"},
  {"Titan",         "2005-301", "2005-301"},
  {"Rhea",          "2005-330", "2005-330"},
  {"Enceladus",     "2005-358", "2005-358"},
  {"Titan",         "2005-360", "2005-360"},
  {"Titan",         "2006-013", "2006-013"},
  {"Titan",         "2006-005", "2006-005"},
  {"Titan",         "2006-063", "2006-063"},
  {"Rhea",          "2006-000", "2006-000"},
  {"Titan",         "2006-120", "2006-120"},
  {"Titan",         "2006-140", "2006-140"},
  {"Titan",         "2006-183", "2006-183"},
  {"Titan",         "2006-203", "2006-203"},
  {"Titan",         "2006-230", "2006-230"},
  {"Titan",         "2006-250", "2006-250"},
  {"Enceladus",     "2006-252", "2006-252"},
  {"Titan",         "2006-266", "2006-266"},
  {"Titan",         "2006-282", "2006-282"},
  {"Titan",         "2006-298", "2006-298"},
  {"Enceladus",     "2006-313", "2006-313"},
  {"Dione",         "2006-325", "2006-325"},
  {"Titan",         "2006-329", "2006-329"},
  {"Titan",         "2006-346", "2006-346"},
  {"Titan",         "2006-362", "2006-362"},
  {"Titan",         "2007-011", "2007-011"},
  {"Titan",         "2007-002", "2007-002"},
  {"Titan",         "2007-043", "2007-043"},
  {"Titan",         "2007-006", "2007-006"},
  {"Titan",         "2007-000", "2007-000"},
  {"Titan",         "2007-100", "2007-100"},
  {"Titan",         "2007-116", "2007-116"},
  {"Titan",         "2007-132", "2007-132"},
  {"Tethys",        "2007-146", "2007-146"},
  {"Titan",         "2007-148", "2007-148"},
  {"Titan",         "2007-164", "2007-164"},
  {"Tethys",        "2007-178", "2007-178"},
  {"Mimas",         "2007-178", "2007-178"},
  {"Enceladus",     "2007-179", "2007-179"},
  {"Titan",         "2007-180", "2007-180"},
  {"Titan",         "2007-200", "2007-200"},
  {"Tethys",        "2007-241", "2007-241"},
  {"Rhea",          "2007-242", "2007-242"},
  {"Titan",         "2007-243", "2007-243"},
  {"Iapetus",       "2007-253", "2007-253"},
  {"Dione",         "2007-273", "2007-273"},
  {"Enceladus",     "2007-273", "2007-273"},
  {"Titan",         "2007-275", "2007-275"},
  {"Titan",         "2007-295", "2007-295"},
  {"Rhea",          "2007-320", "2007-320"},
  {"Titan",         "2007-323", "2007-323"},
  {"Mimas",         "2007-337", "2007-337"},
  {"Titan",         "2007-339", "2007-339"},
  {"Titan",         "2007-354", "2007-354"},
  {"Titan",         "2008-005", "2008-005"},
  {"Titan",         "2008-018", "2008-018"},
  {"Titan",         "2008-043", "2008-043"},
  {"Titan",         "2008-056", "2008-056"},
  {"Enceladus",     "2008-058", "2008-058"},
  {"Titan",         "2008-000", "2008-000"},
  {"Mimas",         "2008-102", "2008-102"},
  {"Titan",         "2008-117", "2008-117"},
  {"Titan",         "2008-133", "2008-133"},
  {"Titan",         "2008-149", "2008-149"},
  {"Titan",         "2008-165", "2008-165"},
  {"Enceladus",     "2008-182", "2008-182"},
  {"","",""}
};

/* expecting sScet = yyyy-doyThh:mm:ss.msc */
char* get_mission_phase_name(char *sScetBeg, char *sScetEnd)
{
char *p;
int i,nIdx,arIdx[128];
size_t nCmpLen;
CasMissionCat *x=MissionPhaseName; 
static char sStr[256];

  /* use shortest length for comparison */
  nCmpLen=strlen(x[0].sScetBeg);
  if(nCmpLen>strlen(sScetBeg))  nCmpLen=strlen(sScetBeg);
  if(nCmpLen>strlen(sScetEnd))  nCmpLen=strlen(sScetEnd);

  nIdx=i=0;
  while(strlen(x[nIdx].sList)>0){
    if( (strncmp(x[nIdx].sScetEnd,sScetBeg,nCmpLen)>=0) &&
        (strncmp(sScetEnd,x[nIdx].sScetBeg,nCmpLen)>=0) )
        arIdx[i++]=nIdx;
    ++nIdx;
  }

  p=sStr;
  if(i==1){
    p+=sprintf(p,"\"%s\"",x[arIdx[--i]].sList);
  }
  else if(i>1){
    p+=sprintf(p,"{\"%s\"",x[arIdx[--i]].sList);
    while(i>0)
      p+=sprintf(p,",\"%s\"",x[arIdx[--i]].sList);
    p+=sprintf(p,"}");
  }
  else
    assert(0);

  assert((p-sStr)<256);

return sStr;
}



/* expecting sScet = yyyy-doyThh:mm:ss.msc
                     0123456789ABCDEF01234 */
char* get_target_name(char *sScetBeg, char *sScetEnd)
{
char *p;
int i,nIdx,arIdx[128];
size_t nCmpLen;
CasMissionCat *x=TargetName; 
static char sStr[256];

  /* use shortest length for comparison */
  nCmpLen=strlen(x[0].sScetBeg);
  if(nCmpLen>strlen(sScetBeg))  nCmpLen=strlen(sScetBeg);
  if(nCmpLen>strlen(sScetEnd))  nCmpLen=strlen(sScetEnd);

  nIdx=i=0;
  while(strlen(x[nIdx].sList)>0){
    if( (strncmp(x[nIdx].sScetEnd,sScetBeg,nCmpLen)>=0) &&
        (strncmp(sScetEnd,x[nIdx].sScetBeg,nCmpLen)>=0) )
        arIdx[i++]=nIdx;
    ++nIdx;
  }
  
  if(i>0){
    p=sStr;
    p+=sprintf(p,"{\"%s\"",x[arIdx[--i]].sList);
    while(i>0)
      p+=sprintf(p,",\"%s\",",x[arIdx[--i]].sList);
    p+=sprintf(p,",\"%s\"}","SOLAR SYSTEM");
  }
  else
    sprintf(sStr,"\"%s\"","SOLAR SYSTEM");

return sStr;
}


/* expecting sScet = yyyy-doyThh:mm:ss.msc
                     0123456789ABCDEF01234 */
char* get_target_name_special_pds(char *sScetBeg, char *sScetEnd)
{
char *p;
int i,nIdx,arIdx[128];
size_t nCmpLen;
CasMissionCat *x=TargetName; 
static char sStr[256];

  /* use shortest length for comparison */
  nCmpLen=strlen(x[0].sScetBeg);
  if(nCmpLen>strlen(sScetBeg))  nCmpLen=strlen(sScetBeg);
  if(nCmpLen>strlen(sScetEnd))  nCmpLen=strlen(sScetEnd);

  nIdx=i=0;
  while(strlen(x[nIdx].sList)>0){
    if( (strncmp(x[nIdx].sScetEnd,sScetBeg,nCmpLen)>=0) &&
        (strncmp(sScetEnd,x[nIdx].sScetBeg,nCmpLen)>=0) )
        arIdx[i++]=nIdx;
    ++nIdx;
  }
  
  if(i>0){
    p=sStr;
    p+=sprintf(p,"%s",x[arIdx[--i]].sList);
    while(i>0)
      p+=sprintf(p,",%s",x[arIdx[--i]].sList);
    p+=sprintf(p,",%s","SOLAR SYSTEM");
  }
  else
    sprintf(sStr,"%s","SOLAR SYSTEM");

return sStr;
}
