#!/usr/bin/rdmd

import std.stdio;
import std.getopt;
import std.experimental.logger;
import std.string;
import std.conv;
import std.array;
import std.format;
import std.process;
import std.algorithm;
import std.path;

//import das2.dwrap;

import dasmini;
import wbr;

extern(C) void furnsh_c (immutable char* filename, int namelen);

/* 
All command line options and their defaults. 
The description for each option can be found in the getopt method in main()
*/
struct Options {
  string sDefKern;
  string sKern;
  string sDefVolRoot;
  string sVolRoot;
  bool b10Rate = true;
  bool b80Rate = false;
  bool bDownMixed = false;
  bool bExAnt = true;
  bool bEwAnt = true;
  bool bCal = false;
}

// Used as a key in dRecType2PktId to access a packetID.
struct tKey{
 string antenna;
 float period;
 float mixingFreq;
 long nPadsTo;
}

// Used for packet Id's. It gets incremented every time it is used.
static uint _g_nNextPktId = 0;

/*
 * Time sorting function for WBR records. Returns comparison by SCLK
*/
bool wbrRecSort(WbrRecord rec1, WbrRecord rec2){
  
  bool nCmp = rec1.nSclkPart == rec2.nSclkPart;
  if(nCmp){
    nCmp = rec1.nSclkSec == rec2.nSclkSec;
    if(nCmp){
        return (rec1.nSclkFine < rec2.nSclkFine);
    }else
      return rec1.nSclkSec < rec2.nSclkSec;
  }else
    return rec1.nSclkPart < rec2.nSclkPart;
}

  

/**
 * Handler function automatically called to set the logging level
*/
void logHandler(string option, string value){

  switch(value){
    case null: sharedLog.logLevel = LogLevel.info; break;
    case "trace": sharedLog.logLevel = LogLevel.trace; break; 
    case "warning": sharedLog.logLevel = LogLevel.warning; break;
    case "error": sharedLog.logLevel = LogLevel.error; break;
    case "critical": sharedLog.logLevel = LogLevel.critical; break;
    case "fatal": sharedLog.logLevel = LogLevel.fatal; break;
    default: 
      sharedLog.logLevel = LogLevel.info;
      logf(LogLevel.warning, "Log level %s is not a valid option\n"~
        "Must be one of [trace, info, warning, error, critical, fatal].\n"~
        "Setting log level to info.", value);
      break;
  }
}

/**
 * Output the das2 stream header
 *
 * @param opts - set of options
 * @param dtBeg - begin time
 * @param dtEnd - stop time
 *
*/
void sendStreamHeader(Options opts, DasTime dtBeg, DasTime dtEnd){
  
  string sTitle = "Cassini RPWS";
  if(opts.bCal)
    sTitle ~= " Calibrated";
  else
    sTitle ~= " Uncalibrated";

  if(opts.bDownMixed)
    sTitle ~= " Downmixed";
  else if(opts.b80Rate)
    sTitle ~= " 222 ksps";
  else
    sTitle ~= " 27.8 ksps";

  sTitle ~= " Waveforms";
  
  if(opts.bExAnt && opts.bEwAnt)
    sTitle ~= " from Ex and Ew";
  else if(opts.bExAnt)
    sTitle ~= " from Ex";
  else if(opts.bEwAnt)
    sTitle ~= " from Ew";
  
  double t2000Begin = dtBeg.epoch(UNIT_T2000);

  string buf;
  auto writer = appender!string();
  formattedWrite(writer,"<stream version=\"2.2\">\n"~
"  <properties String:title=\"%s %%{xCacheResInfo}\"\n"~
"              String:xLabel=\"SCET (UTC)\"\n"~
"              Datum:xTagWidth=\".15 s\"\n"~
"              String:yScaleType=\"linear\"\n"~
"              String:yLabel=\"Electric Intensity (V m!a-1!n)\"\n"~
"              DatumRange:xCacheRange=\"%s to %s UTC\"\n"~
"              double:zFill=\"-1.0e+31\"\n"~
"              String:renderer=\"waveform\"\n"~
"              String:xCacheResInfo =\" (Intrinsic Resolution)\"/>\n"~
"</stream>\n", sTitle, dtBeg.toIsoD(3), dtEnd.toIsoD(3) );
  
  buf = writer.data;
  writefln("[00]%06d%s", buf.length + 1, buf);

  logf(LogLevel.info, "Sent header information for times %s - %s", dtBeg.toIsoD(1), dtEnd.toIsoD(1));

}

/*
 * Checks if a record is worthy of being output
 *
 * @param opts - the program input options
 * @param t2000Beg - begin time
 * @param t2000End - end time
 * @param rec - the record to check
 * @return bool - whether or not the record passed checks 
 *               
*/
bool shouldSendRecord(Options opts, double t2000Beg, double t2000End, WbrRecord rec){

  if(rec.data().length == 0)
    return false;
  if(opts.bDownMixed){
    if(rec.mixingFreq() < 0)
      return false;
  } else if(rec.mixingFreq() > 0)
      return false;
    else if(rec.period == 3.6e-5 && !opts.b10Rate)
      return false;
    else if(rec.period == 4.5e-6 && !opts.b80Rate)
      return false;
  
  if(rec.antenna.among("LP", "UNK"))
    return false;
  if(!rec.antenna.among("Ex", "Ew"))
    return false;
  if(rec.antenna == "Ex" && !opts.bExAnt)
    return false;
  if(rec.antenna == "Ew" && !opts.bEwAnt)
    return false;

  double t2000RecBeg = rec.t2000Beg;

  if(t2000Beg > t2000RecBeg || t2000RecBeg > t2000End){
    return false; 
  }

  return true; 
}

/*
 * Round up packet length to nearest multiple of 1024
 *
 * @param rec - the WbrRecord to get the length of
 * @return ulong - the length of the packet rounded up 
 *                to the nearest multiple of 1024
*/ 
ulong paddedLen(WbrRecord rec){
  ulong nLen = rec.data().length;
  ulong nSegments = nLen / 1024;
  if(nLen % 1024 != 0)
    nSegments++;
  return nSegments*1024;
}

/*
 * Sends out new packet header
*/
int sendPktHdr(Options opts, WbrRecord rec){

  if(_g_nNextPktId > 99){
    log(LogLevel.error, "Packet ID's exceeded maximum value of 99.");
    return -1;
  }
  
  _g_nNextPktId++;
  string sSummary;
  if(rec.period == 3.6e-5f)
    sSummary = "27.8 kilosamples per second waveform from the ";
  else 
    sSummary = "222 kilosamples per second waveform form the ";

  string sZunits = "V m**-1";
  string sField = "Electric Intensity";

  if(rec.antenna == "Ex"){
    sSummary ~= "Eu and Ev electric dipole antennas, aligned along the "~
                "x axis of the spacecraft.";
  }else
    sSummary ~= " Ex electric monopole antenna";

  // If input data were down-mixed in the HFR, send a Signal Definition 
  // property set to aid downstream processors

  // -62500 Hz is just a magic constant you have to know, it's not given
  // in the PDS records. Same is true of the +/- 13,000 Hz ...

  string sSigDef;
  //if(rec.mixingFreq <= 0){
  if(rec.mixingFreq > 0){
    sSummary ~= " Data were downmixed using local oscillator frequency ";
    sSummary ~= format!"%.5e Hz within the HFR prior to digitization"(to!float(rec.mixingFreq));
    sSigDef = format!"Datum:DFT_freqTagMin=\"%.5e Hz\"\n"(to!float(rec.mixingFreq - 62500));
    sSigDef ~= format!"Datum:DFT_freqTrimMin=\"%.5e Hz\"\n"(to!float(rec.mixingFreq - 15000));
    sSigDef ~= format!"Datum:DFT_freqTrimMax=\"%.5e Hz\"\n"(to!float(rec.mixingFreq + 15000));
  }else
    sSigDef = "Datum:DFT_freqTagMin=\"0 Hz\"";
                    
  long nPadsTo = paddedLen(rec); 
  string name = "WBR";
  string buf;

  auto writer = appender!string;
  formattedWrite(writer, "<packet>\n"~
  "  <x type=\"little_endian_real8\" units=\"t2000\"></x>\n"~
  "  <yscan name=\"%s\" type=\"little_endian_real4\" yUnits=\"s\" zUnits=\"%s\" nitems=\"%d\" yTagInterval=\"%.5e\" >\n"~
  "    <properties String:ySummary=\"%s\"\n"~ 
  "                String:zLabel=\"%s (%s)\"\n"~
  "                %s />\n"~
  "  </yscan>\n"~
  "</packet>", name, sZunits, nPadsTo, rec.period, sSummary, sField, sZunits, sSigDef);
  buf = format!"[%02d]%06d%s"(_g_nNextPktId, writer.data.length + 1, writer.data);
  
  stdout.writeln(buf);

  return _g_nNextPktId;
}
  
/*
 * Sends a data record. Packet headers are generated on demand.
 * Data are padded to a multiple of 1024 sample blocks.
 *
 * @param opts - input parameters
 * @param dRecType2PktId - the dictionary to store packet Id's
 * @param rec - the record to output
 * @return bool - Whether or not the record was output 
*/
bool sendRec(Options opts, ref int[tKey] dRecType2PktId, WbrRecord rec){
   ulong nValues = rec.data().length;    
   long nPadsTo = paddedLen(rec);
   float[1] fill = -1e31f;
   int nPktId;
   tKey key = tKey(rec.antenna, rec.period, rec.mixingFreq, nPadsTo);

   if(key !in dRecType2PktId){
     nPktId = sendPktHdr(opts, rec);
     if(nPktId < 1){
      throw new Exception("Ran out of Packet IDs!");
     }
     dRecType2PktId[key] = nPktId;
   }
   nPktId = dRecType2PktId[key];
   stdout.rawWrite(":"~format!"%02d"(nPktId)~":");

   
   double[1] outTime = rec.t2000Beg;
   stdout.rawWrite(outTime);

   if(opts.bCal){
     stdout.rawWrite(to!(float[])(rec.calibrate()));
     foreach(i ; 0 .. (nPadsTo - nValues)) {
        stdout.rawWrite(fill);
     }
   }else{
     stdout.rawWrite(to!(float[])(rec.data()));
     foreach(i ; 0 .. (nPadsTo - nValues)){
        stdout.rawWrite(fill);
     }
   }
   return true;
}

/**
 * Send error message if there is no data in range
 *
 * @param dtBeg - begin time
 * @param dtEnd - end time
 *
*/
/*
void sendNoData(DasTime dtBeg, DasTime dtEnd){
   string buf;  
   auto writer = appender!string;
   //TODO make error message
}
*/


void printHelp(Option[] opts, string usage, string summary){
   auto writer = appender!string;
   formattedWrite(writer, "%s\n\n%s\n\n", summary, usage);
   formattedWrite(writer, "OPTIONS:\n\tNote that all options can be set via "~
       "--option=VALUE and all boolean options can be set via --options=true "~
       "or --options=false\n\n");
   foreach(Option opt; opts){
     formattedWrite(writer, "%5s\t%s\t%s\n", opt.optShort, 
         opt.optLong, opt.help);
   }
   stderr.write(writer.data);

}
/*
void sendProgress(in DasTime dtBegIn, in DasTime dtCurIn, in DasTime dtEndIn){

  DasTime dtBeg = 
    
  // Convert all times to doubles for comparison
  dtBeg.minute = 0, dtBeg.second = 0;
  dtCur.minute = 0, dtCur.second = 0;
  dtEnd.hour++, dtEnd.minute = 0, dtEnd.second = 0;

  double tBeg = dtBeg.epoch(UNIT_T2000);
  double tCur = dtCur.epoch(UNIT_T2000);
  double tEnd = dtEnd.epoch(UNIT_T2000);
 
  double total = tEnd - tBeg;
  double partDone = tCur - tBeg;

  double percDone = 100 * partDone / total ;

  string commentType;
  int value;
  if(percDone == 0){
    commentType = "taskSize"; 
    value = 100;
  }else{
    commentType = "taskProgress";
    value = to!int(percDone);
  }
  string comment = format!"<comment type=\"%s\" value=\"%s\" source=\"\"/>"(commentType, value);
  string output = format!"[xx]%06d%s"(comment.length + 1, comment);

  stdout.writeln(output);

  logf(LogLevel.info, "%3.1f%% complete", percDone);

}
*/

int main(string[] args){

   string usage = "USAGE: \n\t" ~ baseName(args[0]) ~ " [OPTIONS] START_TIME STOP_TIME";
   string summary = "\nSUMMARY:\n\tReads and writes WBR PDS data to a das2 stream \n"~
                    "\t an input time period. Input PDS files\n"~
                    "\tnames similar to:\n\tT2013365_06_10KHZ2_WBRFR.DAT";

   Options options; 

   string env = "RPWS_SUPERVOL";
   if(environment.get(env) !is null)
     options.sVolRoot = environment.get(env) ~ "/DATA/RPWS_WIDEBAND_FULL";

   options.sKern = environment.get("CAS_TIME_KERNELS");

   auto optResult = getopt(args,

     "l|log-level", "Logging level - must be one of [trace, info, \n"~
                    "\t\t\twarning, error, critical, fatal]. The default\n"~
                    "\t\t\t is info.", &logHandler,

     "k|kernel-list", "Specify an alternate SPICE kernel list file, \n"~
                      "\t\t\tthe default is : " ~ options.sKern, &options.sKern,
    
     "v|vol-root", "Set the super volume directory, defaults to \n"~ 
                   "\t\t\t" ~ options.sVolRoot, &options.sVolRoot,

     "1|10khz", "Only transmit 27.8 khz sample rate waveforms when \n"~
                "\t\t\tset to true, by default this is set to " ~ to!string(options.b10Rate), 
                 &options.b10Rate,
            
     "8|80khz", "Only transmit 222 khz sample rate waveforms when \n"~
                "\t\t\tset to true, by default this is set to " ~ to!string(options.b80Rate), 
                &options.b80Rate,

     "D|downmixed", "Transmit down mixed HFR waveforms when set to \n"~
                    "\t\t\ttrue, at present there is no way to know which \n"~
                    "\t\t\tantenna was used for HFR wavesforms so it is \n"~
                    "\t\t\tassumed that Ex is in use for all HFR data. \n"~
                    "\t\t\tDefaults to " ~ to!string(options.bDownMixed), &options.bDownMixed,

     "Ex", "Send Ex data when set to true, by default this is set \n"~
           "\t\t\tto " ~  to!string(options.bExAnt), &options.bExAnt,

     "Ew", "Send Ew data when set to true, by default this is set \n"~
           "\t\t\tto " ~  to!string(options.bEwAnt), &options.bEwAnt,

     "c|calibrate", "Output calibrated data with waveforms when \n"~
           "\t\t\tset to true, by default this is set to " ~  to!string(options.bCal) ~ "\n"~ 
           "See notes in src/wbr.d WbrRecord.calibtate() method.", &options.bCal

     );

  if(optResult.helpWanted){
    printHelp(optResult.options, usage, summary);
    return 3;
  }

  if(args.length < 2){
    logf(LogLevel.error, "Both a start time and a stop time must "~
                         "be specified.\n %s", usage);
    return 4;
  }

  if(options.b80Rate){
    options.b10Rate = false; 
  }

  DasTime dtBeg = DasTime(args[1]);
  DasTime dtEnd = DasTime(args[2]);  
  double t2000Beg = dtBeg.epoch(UNIT_T2000);
  double t2000End = dtEnd.epoch(UNIT_T2000);

  if(options.sKern is null){
    log(LogLevel.error, "Spice Time Meta-Kernel not defined, either "~
       "set the CAS_TIME_KERNELS environment variable or use the -k "~
       "option.");
    return 6;
  }

  //stderr.writeln(options.sVolRoot);
  sendStreamHeader(options, dtBeg, dtEnd);

  // A dictionary to store packet IDs using the key defined above
  static int[tKey] dRecType2PktId;

  furnsh_c (toStringz(options.sKern), to!int(options.sKern.length));
 
  //Loop over time and find fFileArr strings for each hour
  DasTime dtCur = dtBeg;
  WbrRecord lastRecord;
  long nTotalRead, nTotalSent, nTotalErrors, nTotalSkipped;

  //sendProgress(dtBeg, dtCur, dtEnd);    

  while( dtCur.opCmp(dtEnd)  < 0){

    File[] fFileArr = findWbr(options, dtCur);

    if(fFileArr.length == 0){
      dtCur.hour++, dtCur.minute = 0, dtCur.second = 0;
      dtCur.norm();
   //   sendProgress(dtBeg, dtCur, dtEnd);
      continue;
    }

    /* This array always holds one WbrRecord for each file.
       the array is sorted by time and checked for errors and then the 
       newest record is output and its spot in the array is 
       filled with the next record from the same file.*/
    WbrRecord[] wbrRecArr; 
    wbrRecArr.length = fFileArr.length;
    long nRecsRead = 0, nRecsSent = 0, nErrors = 0, nSkipped = 0;
    foreach( i, file; fFileArr){
      wbrRecArr[i] = readWbr(file);
      if(wbrRecArr[i] is null){
        wbrRecArr = wbrRecArr.remove(i); 
      }else{
        nRecsRead++; 
      }
    }

    // Sort records, output a record, read next record 
    while(wbrRecArr.length > 0 ){

      wbrRecArr.sort!((a,b) => wbrRecSort(a,b));// Sort by time
      
      /*
      iLowest = 0;
      for(int i = 1; i < wbrRecArr.length;  ++i){
        if( wbrRecAry[i] <  wbrRecArr[0]) iLowest = i; 
      }
      */

      // Check for duplicate or out of order
      if(lastRecord !is null && 
        wbrRecArr[0].nSclkPart <= lastRecord.nSclkPart &&
        wbrRecArr[0].nSclkSec  <= lastRecord.nSclkSec  &&
        wbrRecArr[0].nSclkFine <= lastRecord.nSclkFine)
      {
        nErrors++;
        wbrRecArr[0] = readWbr(wbrRecArr[0].fFile);
        if(wbrRecArr[0] is null){
          wbrRecArr = wbrRecArr.remove(0); 
          continue;
        }else{
          nRecsRead++; 
          continue;
        }
      }
      
      // Send the record and replace it with a new record
      if(shouldSendRecord(options, t2000Beg, t2000End, wbrRecArr[0])){
          sendRec(options, dRecType2PktId, wbrRecArr[0]);
          nRecsSent++;
          lastRecord = wbrRecArr[0];
          wbrRecArr[0] = readWbr(wbrRecArr[0].fFile);
      }else{
        nSkipped++;     
        wbrRecArr[0] = readWbr(wbrRecArr[0].fFile);
      }
      
      if(wbrRecArr[0] is null){
        wbrRecArr = wbrRecArr.remove(0); 
        continue;
      }else{
        nRecsRead++;
      }
    }

    nTotalRead += nRecsRead; nTotalSent += nRecsSent; nTotalErrors += nErrors, nTotalSkipped += nSkipped;
    logf(LogLevel.info, "Record information:\n Records Read: %d\n Records Sent: %d\n Records Removed:"~
        " %d (duplicates or out of order)\n Records Skipped : %d (not applicable/out of range)", 
        nRecsRead, nRecsSent, nErrors, nSkipped);

    //Increment hour to get a new list of files
    dtCur.hour++, dtCur.minute = 0, dtCur.second = 0;
    dtCur.norm();
    //sendProgress(dtBeg, dtCur, dtEnd);
  }

  logf(LogLevel.info, "Total Records information:\n Total Records Read: %d\n"~
      " Total Records Sent: %d\n Total Records Removed: %d (duplicates or out of order)\n"~
      " Total Records Skipped: %d (not applicable/out of range)",
      nTotalRead , nTotalSent, nTotalErrors, nTotalSkipped);

  /*
  if(nTotalSent == 0){
    sendNoData(stdout, dtBeg, dtEnd); 
  }
  */

  return 0;
}
