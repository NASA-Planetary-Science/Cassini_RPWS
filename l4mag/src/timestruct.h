#ifndef TIMESTRUCT_H_2007016
#define TIMESTRUCT_H_2007016


/* UNPACK(ts) will 'unpack' a TimeStruct into a comma separated list
 * of its members.
 * 2010-11-15 Edward West: Added to svn
 * $URL: https://saturn.physics.uiowa.edu/svn/cassini/production/devel/l4mag/src/timestruct.h $
 * $Revision: 514 $
 */
#define UNPACK(ts) &ts.year, &ts.month, &ts.mday, &ts.yday, &ts.hour,\
               &ts.minute, &ts.second

typedef struct _TimeStruct {
    int year;
    int month;
    int mday;
    int yday;
    int hour;
    int minute;
    double second;
} TimeStruct;


#endif
