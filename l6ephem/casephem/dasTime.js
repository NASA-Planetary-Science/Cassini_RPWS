// dasTime.js 
// Version 2.0
// 00-04-17 raj
//      Modified FormatRPWS() to include seconds  

function dasTime_IsLeapYear(nYear)
{
var bStatus=false;

  // If the year is a century year and divisible by 400, 
  // then it is a leap year.
  // If the year is not a century year and divisible by 4, 
  // then it is a leap year.

  dasTime_Truncate(nYear);  // Make sure year is an integer

  if(nYear%100){        // Year is NOT a century year
    if(nYear%4)
      bStatus = false;  // Not evenly divisible by 4
    else 
      bStatus = true;   // Is a leap year
  }
  else{                 // Year is a century year
    if(nYear%400)
      bStatus = false;  // 1900 is not a leap year 
    else 
      bStatus = true;   // 2000 is a leap year
  }

return bStatus;
}


function dasTime_DaysInYear(nYear)
{
var nDaysInYear=365;

  if(dasTime_IsLeapYear(nYear) == true)
     nDaysInYear=366;
  else 
     nDaysInYear=365;

return nDaysInYear;
}


function dasTime_GetDayOfYear(nYear,nMonth,nMday) 
{
var nYday=0;

  nYday  = nMday;
  nYday += dasTime.arDays[nMonth];  // 1 = Jan & 12 = Dec

  // In the month of March and Feb. was a leap year, add one to day of year
  if((nMonth > 2) && (dasTime_IsLeapYear(nYear) == true))
      nYday += 1;

return nYday
}

// return month of the year, Jan = 1, Feb = 2, ... Dec = 12
function dasTime_GetMonthOfYear(nYear,nYday) 
{
var i,nLeap,nMonth;

  if(dasTime_IsLeapYear(nYear) == true)
    nLeap = 1;
  else
    nLeap = 0;

  if(nYday <= 31)
    nMonth = 1;
  else{
    for(i=3;i<dasTime.arDays.length;i++){
      if(nYday <= (dasTime.arDays[i]+nLeap))
        break;
    } 
    nMonth = i - 1;
  }

return nMonth;
}

function dasTime_GetDayOfMonth(nYear,nYday) 
{
var i,nLeap,nMday,nMonth;

  nMonth = dasTime_GetMonthOfYear(nYear,nYday);  // 1 - 12

  nMday = nYday - dasTime.arDays[nMonth];
  if(dasTime_IsLeapYear(nYear)==true && nMonth>2)  
    nMday-=1;

return nMday;
}




// Constructor for the dasTime Object which defaults to gm time
function dasTime()
{
var arLocDays = [0,0,31,59,90,120,151,181,212,243,273,304,334,365]; 
var gmtime = new Date(); 

  // Define some properties for the object
  this.nYear   = gmtime.getUTCFullYear();  
  this.nMonth  = gmtime.getUTCMonth() + 1;  // 0 = Jan. & 11 = Dec.
  this.nMday   = gmtime.getUTCDate();  
  this.nYday   = 0;
  this.nHour   = gmtime.getUTCHours();  
  this.nMinute = gmtime.getUTCMinutes();  
  this.nSecond = gmtime.getUTCSeconds();  
  this.nMillisecond = gmtime.getUTCMilliseconds();  
  
  // Calculate the day of year
  dasTime_IsLeapYear(this.nYear);
  this.nYday  = this.nMday;
  this.nYday += arLocDays[this.nMonth];  // 1 = Jan & 12 = Dec
  if(dasTime_IsLeapYear(this.nYear)==true && this.nMonth>2)
    this.nYday += 1;

return;
}

function dasTime_FormatJPL()
{
var sFormat="";

  sFormat += this.nYear;
  sFormat += "-";
  if(this.nYday<10)        sFormat += "00" + this.nYday;
  else if(this.nYday<100)  sFormat += "0" + this.nYday;
  else                     sFormat += this.nYday;
  sFormat += "T";
  if(this.nHour<10)  sFormat += "0" + this.nHour;
  else               sFormat += this.nHour;
  sFormat += ":";
  if(this.nMinute<10)  sFormat += "0" + this.nMinute;
  else                 sFormat += this.nMinute;
  sFormat += ":";
  if(this.nSecond<10)  sFormat += "0" + this.nSecond;
  else                 sFormat += this.nSecond;  
  sFormat += ".";
  if(this.nMillisecond<10)        sFormat += "00" + this.nMillisecond;
  else if(this.nMillisecond<100)  sFormat += "0" + this.nMillisecond;
  else                            sFormat += this.nMillisecond;

return sFormat;
}

function dasTime_FormatRPWS()
{
var sFormat="";

  sFormat += this.nYear;
  sFormat += "-";
  if(this.nYday<10)        sFormat += "00" + this.nYday;
  else if(this.nYday<100)  sFormat += "0" + this.nYday;
  else                     sFormat += this.nYday;
  sFormat += "T";
  if(this.nHour<10)  sFormat += "0" + this.nHour;
  else               sFormat += this.nHour;
  sFormat += ":";
  if(this.nMinute<10)  sFormat += "0" + this.nMinute;
  else                 sFormat += this.nMinute;
  sFormat += ":";
  if(this.nSecond<10)  sFormat += "0" + this.nSecond;
  else                 sFormat += this.nSecond;


return sFormat;
}

function dasTime_FormatMDY()
{
var sFormat="",sTmp="";

  sFormat = dasTime.arMonths[this.nMonth-1];  // Get the month string
  sTmp = sFormat.match(/[a-z]/);              // and capitalize the first
  sTmp = sTmp[0];                             // letter.
  sTmp = sTmp.toUpperCase();                  
  sFormat = sFormat.replace(/[a-z]/,sTmp) + " ";
  sFormat += this.nMday + ", ";
  sFormat += this.nYear + " ";

  if(this.nHour<10)  sFormat += "0" + this.nHour;
  else               sFormat += this.nHour;
  sFormat += ":";
  if(this.nMinute<10)  sFormat += "0" + this.nMinute;
  else                 sFormat += this.nMinute;

return sFormat;
}


function dasTime_FormatHex()
{
var sFormat;
var n = new Number(this.ttime());

  sFormat = n.toString(16);
  while(sFormat.length < 8){
    sFormat = "0" + sFormat;
  }
  sFormat = "0x" + sFormat;
  
return sFormat;
}

// Takes a Javascript number and truncates it 
function dasTime_Truncate(fNumber)
{

  if(fNumber > 0)
    fNumber = Math.floor(fNumber);
  else
    fNumber = Math.ceil(fNumber);

return fNumber;
}


//  tnorm -- normalize date and time components for the Gregorian calendar
//  ignoring leap seconds. (This is the most likely bug nest.)
// Normalize second, minutes, hours, day of year, years 
// Calculate month and day of month from day of year 
// Note: everything is floating point in JavaScript, ie no interger divisions
function dasTime_tnorm()
{
var nDaysInYear;

  // Make sure we are dealing with integers 
  this.nMillisecond = dasTime_Truncate(this.nMillisecond);
  this.nSecond = dasTime_Truncate(this.nSecond);
  this.nMinute = dasTime_Truncate(this.nMinute);
  this.nHour = dasTime_Truncate(this.nHour);
  this.nYday = dasTime_Truncate(this.nYday);
  this.nYear = dasTime_Truncate(this.nYear);
  
  // again, we're ignoring leap seconds 

  if(this.nMillisecond>=1000 || this.nMillisecond<0 ){
    this.nSecond += dasTime_Truncate(this.nMillisecond / 1000);
    this.nMillisecond = dasTime_Truncate(this.nMillisecond % 1000);
    if( this.nMillisecond<0 ){
      this.nMillisecond += 1000;
      this.nSecond -= 1;
    }
  }

  if(this.nSecond>=60 || this.nSecond<0 ){
    this.nMinute += dasTime_Truncate(this.nSecond / 60);
    this.nSecond = dasTime_Truncate(this.nSecond % 60);
    if( this.nSecond<0 ){
      this.nSecond += 60;
      this.nMinute -= 1;
    }
  }

  if(this.nMinute>=60 || this.nMinute<0 ){
    this.nHour += dasTime_Truncate(this.nMinute / 60);
    this.nMinute = dasTime_Truncate(this.nMinute % 60);
    if( this.nMinute<0 ){
      this.nMinute += 60;
      this.nHour -= 1;
    }
  }

  if( this.nHour>=24 || this.nHour<0 ){
    this.nYday += dasTime_Truncate(this.nHour / 24);
    this.nHour = dasTime_Truncate(this.nHour % 24);
    if( this.nHour<0 ){
      this.nHour += 24;
      this.nYday -= 1;
    }
  }

  // Make sure that the year is a four digit year
  if( this.nYear<1000 )  // Y2K complient
    this.nYear += 1900; 

  // days of year range from 1 - 365 or in leap years 1 - 366
  nDaysInYear = dasTime_DaysInYear(this.nYear);

  while( this.nYday>nDaysInYear || this.nYday<=0 ){
    
    if( this.nYday<=0 ){
      this.nYear -= 1;
      this.nYday += dasTime_DaysInYear(this.nYear);
    }
    else{
      this.nYday -= dasTime_DaysInYear(this.nYear);
      this.nYear += 1;
    }
    nDaysInYear = dasTime_DaysInYear(this.nYear);

  }// while day of year

  this.nMonth = dasTime_GetMonthOfYear(this.nYear,this.nYday);
  this.nMday = dasTime_GetDayOfMonth(this.nYear,this.nYday);

return;
}


// all variables are integers
// return 367 * year - 7 * (year + (month + 9) / 12) / 4 -
//         3 * ((year + (month - 9) / 7) / 100 + 1) / 4 +
//         275 * month / 9 + day + 1721029;
function dasTime_Jday(nYear,nMonth,nMday)
{
var nJday=0;
var nTmp=0;

  nYear  = dasTime_Truncate(nYear);
  nMonth = dasTime_Truncate(nMonth);
  nMday  = dasTime_Truncate(nMday);

  nJday  = 367 * nYear;
   nTmp = 7 * dasTime_Truncate(nYear + (nMonth + 9) / 12);
  nJday -= dasTime_Truncate(nTmp/4);
    nTmp = 3 * dasTime_Truncate((nYear + (nMonth - 9) / 7) / 100 + 1);
  nJday -= dasTime_Truncate(nTmp/4);
  nJday += dasTime_Truncate(275 * nMonth / 9 );
  nJday += nMday + 1721029;   

return nJday;
}



function dasTime_ttime()
{
var dSec=0.0,sjd=0.0;

  // normalize the input values
  this.tnorm(); 

  // Use the difference of Julian Days for the arbitrary epoch
  sjd  = dasTime_Jday(this.nYear,this.nMonth,this.nMday) - dasTime.EPOCH;
  sjd *= 86400.0;

  dSec = this.nSecond + (this.nMinute * 60.0) + (this.nHour * 3600.0) + sjd;

return dSec;
}

function dasTime_parsetime (input)
{
   var sDelimiters,sEndOfDate;  // pointers to strings
   var sTokAr;                  // array of pointers to strings
   var nWantDate, nWantYear, nWantMonth, nWantDay, nWantHour, nWantSecond;
   var nHold;
   var sTime = new String (input);
   var sTmp;

   nWantDate = 0;
   sDelimiters = dasTime.DELIMITERS;

   // handle PDS time format, ie. Truncate string after "Z"
   if (sTime.indexOf ("Z") != -1)
   {
      sTime = sTime.split ("Z");
      sTime = sTime [0];
   }

   // check for a "T" proceded by a number, if so, it is the end of the date
   sEndOfDate = sTime.match (/[0-9]T/);

   // sEndOfDate is the char pos in the array of the end of the date, or null
   if (sEndOfDate != null)
   {
      sDelimiters = dasTime.PDSDELIMITERS;
      sEndOfDate = sTime.indexOf ("T");  // character place of "T" in array
   }

   // default to the current year
   sTmp = new Date ();
   this.nYear = sTmp.getYear ();

   if (this.nYear < 1000)
      this.nYear += 1900;

   this.nMonth = 0;
   this.nMday = 0;
   this.nYday = 0;
   this.nHour = 0;
   this.nMinute = 0;
   this.nSecond = 0;
   this.nMillisecond = 0;

   // java script likes to return null strings as tokens,
   // if strings begin or end with delimiters one is parsing by.
   // One word, braindead.
   sTime = sTime.replace (/^[\s]+/,"");  // leading whitespaces are a problem
   sTime = sTime.replace (/[\s]+$/,"");  // trailing whitespaces are a problem
   sTokAr = sTime.split (dasTime.DELIMITERS);

   if (sTokAr [0] == "")  // no items parsed
      return false;

   nWantDate = nWantYear = nWantMonth = nWantDay = 1;
   nHold = 0;

   nCnt = 0;  // a number - which points the the current token in the string

   for (i = 0; i < sTokAr.length; i++)
   {
      if ((sEndOfDate != null) && (nWantDate != 0) && (nCnt > sEndOfDate))
      {
         nWantDate = 0;
         nWantHour = nWantMinute = nWantSecond = 1;
      }
   
      nNumber = parseFloat (sTokAr [i]);

      // if tok is not a number, ie a month 
      if (isNaN (nNumber) == true)
      {
         // No abbrevations < 3 chars
         if (sTokAr [i].length < 3 || nWantDate == 0)
            return false;

         sTokAr [i] = sTokAr [i].toLowerCase ();

         // search month array for a match
         for (j = 0; j < 12; j++)
         {
            sTmp = dasTime.arMonths [j];

            if (sTmp.indexOf (sTokAr [i]) != -1)
            {
               this.nMonth = j + 1; 
               nWantMonth = 0;

               if (nHold != 0)
               {
                  if (this.nMday != 0)
                     return false;

                  this.nMday = nHold;
                  nHold = 0;
                  nWantDay = 0;
               }

               break;
            }
         }

         if (nWantMonth != 0)  // No month matched
            return false;

         continue;
      }

      if ((nNumber % 1.0) != 0.0)
      {
         if (nWantSecond != 0)
         {
            this.nSecond = nNumber;
            break;
         }
            else
               return false;
      }

      if (nNumber < 0)
         return false;

      if (nWantDate != 0)
      {
         if (nNumber == 0)
            return false;

         if (nNumber > 31)
         {
            // Year 2000 bug (Y2K)
            if (nWantYear != 0)
            {
               this.nYear = nNumber;

               if (this.nYear < 1000)
                  this.nYear += 1900; 

               nWantYear = 0;
            }
            else if (nWantMonth != 0)
            {
               this.nMonth = 0;
               this.nYday = nNumber;
               nWantDay = 0;
               nWantMonth = 0;
            }
            else
            {
               return false;
            }
         }
         else if (nNumber > 12) // if nNumber > 31
         {
            if (nWantDay != 0)
            {
               if (nHold != 0)
               {
                  this.nMonth = nHold;
                  nWantMonth = 0;
               }

               if (sTokAr[i].length == 3) // day of year
               {
                  if (this.nMonth != 0)
                     return false;

                  this.nYday = nNumber;
                  this.nMday = 0;
                  nWantMonth = 0;
               }
               else
                  this.nMday = nNumber;

               nWantDay = 0;
            }
            else
               return false;
         }
         else if (nWantMonth == 0) // if nNumber > 12
         {
            if (this.nMonth != 0)
            {
               this.nMday = nNumber;
               this.nYday = 0;
            }
            else
            {
               this.nYday = nNumber;
               this.nMday = 0;
            }

            nWantDay = 0;
         }
         else if (nWantDay == 0) // nWantMonth
         {
            if (this.nYday != 0)
               return false;

            this.nMonth = nNumber;
            nWantMonth = 0;
         }
         else if (nWantYear == 0) // nWantDay
         {
            if (sTokAr [i].length == 3)
            {
               if (this.nMonth != 0)
                  return false;

               this.nYday = nNumber;
               this.nMonth = 0;
               nWantDay = 0;
            }
            else
            {
               if (this.nYday != 0)
                  return false;

               this.nMonth = nNumber;

               if (nHold != 0)
               {
                  this.nMday = nHold;
                  nWantDay = 0;
               }
            }

            nWantMonth = 0;
         }
         else if (nHold) // nWantYear
         {
            this.nMonth = nHold;
            nHold = 0;
            nWantMonth = 0;
            this.nMday = nNumber;
            nWantDay = 0;
         }
         else // nHold
            nHold = nNumber;
    
         if (!(nWantYear != 0 || nWantMonth != 0 || nWantDay != 0))
         {
            nWantDate = 0;
            nWantHour = nWantMinute = nWantSecond = 1;
         }
      }
      else if (nWantHour != 0) // if nWantDate != 0
      {
         if (sTokAr[i].length == 4) // 2400 hours ???
         {
            nHold = nNumber/100;
            nHold += " ";              // type coerce to string
            nHold = parseInt (nHold);  // parse the integer value

            if (nHold > 23)
               return false;

            this.nHour = nHold;
            nHold = nNumber % 100;

            if (nHold > 59)
               return false;

            this.nMinute = nHold;
            nWantMinute = 0;
         }
         else
         {
            if (nNumber > 23)
               return false;

            this.nHour = nNumber;
         }

         nWantHour = 0;
      }
      else if (nWantMinute != 0) // else if nWantHour
      {
         if (nNumber > 59)
            return false;

         this.nMinute = nNumber;
         nWantMinute = 0;
      }
      else if (nWantSecond != 0) // else if nWantMinute
      {
         if (nNumber > 61)
            return false;

         this.nSecond = nNumber;
         nWantSecond = 0;
      }
      else // else if nWantSecond
         return false;

      nCnt += sTokAr [i].length;
   } // for all tokens

   if (this.nYday != 0)
   {
      this.nMonth = dasTime_GetMonthOfYear (this.nYear, this.nYday);
      this.nMday  = dasTime_GetDayOfMonth (this.nYear, this.nYday);
   }
   else
   {
      this.nYday = dasTime_GetDayOfYear (this.nYear, this.nMonth, this.nMday);
   }

   // test to see if seconds contain a fractional part, ie milliseconds
   this.nMillisecond = this.nSecond - dasTime_Truncate (this.nSecond);
   this.nMillisecond *= 1000;

   return true;
}

function dasTime_PopupParseTimeError()
{
sTmp  = "Error Processing Time";
sTmp += "\nyear = " + this.nYear;
sTmp += "\nYday = " + this.nYday;
sTmp += "\nmonth = " + this.nMonth;
sTmp += "\nMday = " + this.nMday;
sTmp += "\nhour = " + this.nHour;
sTmp += "\nminute = " + this.nMinute;
sTmp += "\nsecond = " + this.nSecond;
sTmp += "\nmilliseconds = " + this.nMillisecond;
sTmp += "\nhere ";
alert(sTmp);
return;
}


// Create and discard initial object instance, for the
// prototype/inheritance voodoo to work (for Navigator 3.0)
new dasTime();  

// instance variables
// nYear (four digit year), nYday(1-366), nMonth(1-12), nMday(1-31),
// nHour(0-23), nMinute(0-59), nSecond(0-59), nMillisecond (0-999)

// instance methods
dasTime.prototype.parsetime = dasTime_parsetime;   
dasTime.prototype.ttime = dasTime_ttime; 
dasTime.prototype.tnorm = dasTime_tnorm;
dasTime.prototype.FormatJPL = dasTime_FormatJPL;
dasTime.prototype.FormatRPWS = dasTime_FormatRPWS;
dasTime.prototype.FormatMDY = dasTime_FormatMDY;
dasTime.prototype.FormatHex = dasTime_FormatHex;
dasTime.prototype.PopupParseTmErr = dasTime_PopupParseTimeError; // error,debug

// class variables
dasTime.EPOCH = 2436205;  // Julian Day at Janurary 1, 1958 12:00 UT
                           // Ja Fb Mr Ap May Jun Jul Aug Sep Oct Nov Dec
dasTime.arDays = new Array( 0, 0,31,59,90,120,151,181,212,243,273,304,334,365);
dasTime.arMonths = new Array("janurary","feburary","march","april","may","june","july","august","september","october","november","december");
dasTime.DELIMITERS = new RegExp("[-,_:;T\/\ \t]+");
dasTime.PDSDELIMITERS = new RegExp("[\s///-T:,_;]+");
//dasTime.NWS_DELIMITERS = "/[///-:,_;]/";

// class methods
dasTime.Jday = dasTime_Jday; 
dasTime.IsLeapYear = dasTime_IsLeapYear;
dasTime.DaysInYear = dasTime_DaysInYear;
dasTime.GetMonthOfYear = dasTime_GetMonthOfYear;
dasTime.GetDayOfYear = dasTime_GetDayOfYear;
dasTime.GetDayOfMonth = dasTime_GetDayOfMonth;
dasTime.Truncate = dasTime_Truncate;  

