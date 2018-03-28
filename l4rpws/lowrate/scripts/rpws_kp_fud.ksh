#!/bin/ksh

# Transform the standard JPL event time format "2000-257T02:30:59.125" into
# other forms.

sOutput=""
# Get the command line arguments
# -y 1999 -b 1 -e 366
while getopts ":def:lmMnpt:wWyz" opt; do 
  case $opt in
    w ) sList=$sList" w" ;;  # day of week, number
    W ) sList=$sList" W" ;;  # day of week, string
    m ) sList=$sList" m" ;;  # month of year, number
    M ) sList=$sList" M" ;;  # month of year, string
    d ) sList=$sList" d" ;;  # day of month, number
    e ) sList=$sList" e" ;;  # days in month, number
    l ) sList=$sList" l" ;;  # leap year, boolean
    n ) sList=$sList" n" ;;  # day of year, number
    y ) sList=$sList" y" ;;  # year, number
    f ) sList=$sList" f$OPTARG" ;;
    t ) nTruncate=$OPTARG ;;
    z ) bZeroPad="False"  ;;
    p ) bSpacePad="False"  ;;
   \? ) print "usage: $0 [-delmMnwWyz] [-f ARG] [t NNN] STRING"
        print "  "
        print "  Acceptable STRINGS are either YEAR-DOY or YEAR-MM-DD.  There "
        print "    is no range requirements for doy, month, or day.  $0 will "
        print "    normalize as necessary to produce valid dates.  For example"
        print "    $0 2000-367     will output -> Monday, Janurary 01, 2001"
        print "    $0 2000-12-397  will output -> Tuedsay, Janurary 01, 2002"
        print "  "
        print "  wW   day of week, number/string"
        print "  mM   month of year, number/string"
        print "   d   day of month, number"
        print "   e   days in month, number"
        print "   l   leap year, boolean"
        print "   n   day of year, number"
        print "   y   year, number"
        print "   z   zero pad, eliminates the zero padding of dates"
        print "   t   truncate, where NNN is the number of characters"
        print "  "
        print "  Formatting Options -f OPT"
        print "   0  YEAR-DOY"
        print "   1  YEAR_DOY"
        print "   2  YEAR-MM-DD"
        print "   3  DayOfWeek, Month DayOfMonth, Year"
        print "   4  DayOfWeek, Month DayOfMonth, Year (DayOfYear)"
        print "   5  YEAR-MM-DD (DOY)"
        exit 127;;
  esac
done
shift $(($OPTIND - 1))
sTime=$1

sList=${sList:="f4"}  # if not def, set f4 as default

#
# define some functions 
#

LeapYear(){
  if (($nYear%100)); then    # Year is NOT a century year
    if (($nYear%4)); then    # NOT evenly divisible by 4
      bLeapYear="False"      # 
    else                     # Is a leap year
      bLeapYear="True"       #
    fi                       #
  else                       # Year IS a century year
    if (($nYear%400)); then  # 1900 is not a leap year
      bLeapYear="False"      # 
    else                     # 2000 is a leap year
      bLeapYear="True"       #
    fi                       #
  fi                         # 

  # set the global array of days variable for use in other functions
  if [[ $bLeapYear = "True" ]]; then
    set -A arDim 0 31 29 31  30  31  30  31  31  30  31  30  31
    set -A arDiy 0 31 60 91 121 152 182 213 244 274 305 335 366
  else #            J* F  M*  A   M*  J   J*  A*  S   O*  N   D*
    set -A arDiy 0 31 59 90 120 151 181 212 243 273 304 334 365
    set -A arDim 0 31 28 31  30  31  30  31  31  30  31  30  31
  fi
}

Month(){
  set -A arMonths NoMonth Janurary Feburary March April May June July August September October November December
  LeapYear       # sets bLeapYear

  nMonth=0
  while (( $nDoy > ${arDiy[$nMonth]} )) 
  do
    nMonth=$((nMonth+1))
  done
  sMonth=${arMonths[$nMonth]}
}

DayOfMonth(){
  typeset idx
  Month              # sets bLeapYear, arDiy[], and nMonth
  idx=$((nMonth-1))
  nDayOfMonth=$((nDoy-${arDiy[$idx]}))
  nDaysInMonth=${arDim[$nMonth]}
}

DayOfWeek(){  # Assume Janurary 1, 0000 is Saturday (Jan. 1, 0001 is Monday)
  typeset leap           # declare a local variable
  typeset norm           # declare a local variable
  typeset year           # declare a local variable
  typeset days           # declare a local variable
  set -A arDayOfWeek Freeday Sunday Monday Tuesday Wednesday Thursday Friday Saturday

  while (($nDoy < 1 )); do   #> ${arDiy[12]}))  # arDiy[12]=365 or 366
    nYear=$((nYear-1))
    LeapYear
    nDoy=$((${arDiy[12]}-nDoy))  
  done
    
  
  LeapYear;                       # normalize 
  while (($nDoy > ${arDiy[12]}))  # arDiy[12]=365 or 366
  do
    nYear=$((nYear+1))
    nDoy=$((nDoy-${arDiy[12]}))  
    LeapYear
  done

  DayOfMonth              # sets bLeapYear, nMonth, arDiy[], and nDayOfMonth

  # find the day of week for Janurary 1, xxxx
  if (($nYear == 0)); then
    leap=0;  norm=0;
  elif (($nYear == 1)); then
    leap=1;  norm=0;
  else
    year=$((nYear-1))
    leap=$((year/4))           # maximum number of leap years since 1 B.C.
    leap=$((leap-(year/100)))  # subtract bogus leap years, century years
    leap=$((leap+(year/400)))  # add back real century leap years
    norm=$((year-leap))        # normal years
    leap=$((leap+1))           # count leap year in 1 B.C.
  fi
  days=$((leap*2))     # day advancement for leap years
  days=$((days+norm))  # total day advancement, normal and leap years
  days=$((days+7))     # transform days advanced from saturday, to a day of 
                       # week number (sun=1, mon=2, tue=3, wed=4, thur=5, fri=6
                       # sat=7).  days is the day of week for Jan 1, xxxx

  nDayOfWeek=$((days + (nDoy-1)))  # add in current day of year
  nDayOfWeek=$((nDayOfWeek%7))
  if [[ $nDayOfWeek -eq 0 ]]; then
    nDayOfWeek=7   
  fi
  sDayOfWeek=${arDayOfWeek[$nDayOfWeek]}
}

DayOfYear(){                     # assumes nYear, nMonth, nDayOfMonth is set  
  typeset idx                    # declare a local variable

  nMonth=${nMonth:-1}            # default to Janurary
  nDayOfMonth=${nDayOfMonth:-1}  # default to day 1
  
  while (($nMonth < 1)); do   # normalize 
    nMonth=$((nMonth+12))
    nYear=$((nYear-1)) 
  done

  while (($nMonth > 12))       # normalize 
  do
    nMonth=$((nMonth-12))
    nYear=$((nYear+1)) 
  done

  LeapYear                       # sets bLeapYear, arDiy, arDim

  idx=$((nMonth-1))
  nDoy=${arDiy[$idx]}
  nDoy=$((nDoy+nDayOfMonth))     # normalize nDoy in DayOfWeek
}

#
# end of functions definitions
#



# parse time string format 1999-230T12:00:59.125 or 1999-12-25T12:30:58.500
nYear=${sTime%%-*}
nArg2=${sTime%T*};  nArg2=${nArg2#*-};  nArg2=${nArg2%-*}
if ((${#nArg2} == ${#nYear}));then     # year only arg
  nDoy=1
elif ((${#nArg2} == 3));then           # assume year-doy
  nDoy=${sTime%T*};  nDoy=${nDoy#*-}
else                                   # assume year-month-day
  nMonth=${sTime%T*};  nMonth=${nMonth#*-};  nMonth=${nMonth%-*}
  nDayOfMonth=${sTime%T*};  nTmp=${nDayOfMonth#*-};  
  nDayOfMonth=${nDayOfMonth##*-}
  if ((${#nTmp} == ${#nDayOfMonth})); then  # if month day is omitted
    nDayOfMonth=1
  fi
  DayOfYear
fi
DayOfWeek


#echo "string=$sTime"
# Format the output string
sOutput=""
if [[ ${bZeroPad:-"True"} == "True" ]]; then
  typeset -Z4 nYear
  typeset -Z3 nDoy
  typeset -Z2 nMonth
  typeset -Z2 nDayOfMonth
fi
if [[ ${nTruncate:-"not def"} != "not def" ]]; then
  typeset -L$nTruncate sMonth
  typeset -L$nTruncate sDayOfWeek
fi

for f in $sList
do
  case $f in
    l) sOutput=$sOutput"$bLeapYear" ;;
    y) sOutput=$sOutput"$nYear"     ;;
    n) sOutput=$sOutput"$nDoy"      ;;
    m) sOutput=$sOutput"$nMonth"    ;;
    M) sOutput=$sOutput"$sMonth"    ;;
    d) sOutput=$sOutput"$nDayOfMonth"  ;;
    e) sOutput=$sOutput"$nDaysInMonth" ;;
    w) sOutput=$sOutput"$nDayOfWeek"   ;;
    W) sOutput=$sOutput"$sDayOfWeek"   ;;
   f0) sOutput=$sOutput"$nYear"-"$nDoy" ;;
   f1) sOutput=$sOutput"$nYear"_"$nDoy" ;;
   f2) sOutput=$sOutput"$nYear"-"$nMonth"-"$nDayOfMonth" ;;
   f3) sOutput=$sOutput"$sDayOfWeek, $sMonth $nDayOfMonth, $nYear" ;;
   f4) sOutput=$sOutput"$sDayOfWeek, $sMonth $nDayOfMonth, $nYear ($nDoy)" ;;
   f5) sOutput=$sOutput"${nYear}-$nMonth-$nDayOfMonth ($nDoy)" ;;
  esac
  if [[ $bSpacePad != "False" ]]; then
    sOutput=$sOutput" "
  fi
done  

print $sOutput

exit 0
