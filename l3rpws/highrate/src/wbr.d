import std.stdio;
import std.file;
import std.array;
import std.experimental.logger;
import std.format;
import std.string;
import std.bitmanip;
import std.path;
import std.conv;
import std.range;
import std.algorithm;
import std.math;

//import das2.dwrap;
import dasmini;
import rpws_l3wbr_das2rdr : Options;

extern(C)  void scs2e_c  (int sc, const char* sclkch, double* et);
extern(C)  void et2utc_c (double et, const char* format, int prec, int lenout, char* utcstr);

/*
##############################################################################
#  Table found in /opt/project/cassini/pds/DOCUMENT/RPWSUG/RPWSUG.PDF on pages 
#  285-287
#
#  "In general, odd indicies indicate HFR/H1 is selected with frequency in 
#   25 KHz steps, and even indicies indicate HFR/H2 is selected with frequency
#   in 50 Khz steps with an offset of 4.025 Mhz"
#
# - RPWS_WBR_WFR_ROW_PREFIX.FMT in /opt/project/cassini/pds/LABEL
#
#  Values below in units of Hz
##############################################################################
*/

long[] _g_HFtable = [
  4025000,      -1,  4125000,      -1,  4225000,  125000,  4225000,  175000,  
  4425000,  225000,  4525000,  275000,  4625000,  325000,  4725000,  375000,  
  4825000,  425000,  4925000,  475000,  5025000,  525000,  5125000,  575000,  
  5225000,  625000,  5325000,  675000,  5425000,  725000,  5525000,  775000,  
  5625000,  825000,  5725000,  875000,  5825000,  925000,  5925000,  975000, 
  6025000, 1025000,  6125000, 1075000,  6225000, 1125000,  6325000, 1175000,  
  6425000, 1225000,  6525000, 1275000,  6625000, 1325000,  6725000, 1375000,
  6825000, 1425000,  6925000, 1475000,  7025000, 1525000,  7125000, 1575000,
  7225000, 1625000,  7325000, 1675000,  7425000, 1725000,  7525000, 1775000,
  7625000, 1825000,  7725000, 1875000,  7825000, 1925000,  7925000, 1975000, 
  8025000, 2025000,  8125000, 2075000,  8225000, 2125000,  8325000, 2175000,
  8425000, 2225000,  8525000, 2275000,  8625000, 2325000,  8725000, 2375000,
  8825000, 2425000,  8925000, 2475000,  9025000, 2525000,  9125000, 2575000,
  9225000, 2625000,  9325000, 2675000,  9425000, 2725000,  9525000, 2775000,
  9625000, 2825000,  9725000, 2875000,  9825000, 2925000,  9925000, 2975000, 
 10025000, 3025000, 10125000, 3075000, 10225000, 3125000, 10325000, 3175000,
 15225000,      -1, 15325000,      -1, 15425000,      -1, 15525000,      -1,
 15625000,      -1, 15725000,      -1, 15825000,      -1, 15925000,      -1,
 16025000,      -1,       -1,      -1,       -1,      -1,       -1,      -1,
       -1,      -1,       -1,      -1,       -1,      -1,       -1,      -1
 ];


/**
 * Get all files corresponding to input time and options
 * 
 * @param opts - the input command line options.
 * @param dt - time
 *
 * @return File[] -  the valid files
*/
File[] findWbr(Options opts, DasTime dt){
   
  File[] lFiles;

  string newPat; 
  if(opts.b10Rate){
    newPat = format!"T%4d%1dXX/T%4d%03d/T%4d%03d_%02d_10KHZ*.DAT"(
      dt.year, dt.yday / 100, dt.year, dt.yday, dt.year, 
      dt.yday, dt.hour);
  }else if(opts.b80Rate){
    newPat = format!"T%4d%1dXX/T%4d%03d/T%4d%03d_%02d_75KHZ*.DAT"(
      dt.year, dt.yday / 100, dt.year, dt.yday, dt.year, 
      dt.yday, dt.hour);
  }else if(opts.bDownMixed){
    newPat = format!"T%4d%1dXX/T%4d%03d/T%4d%03d_%02d_???*KHZ*.DAT"(
      dt.year, dt.yday / 100, dt.year, dt.yday, dt.year, 
      dt.yday, dt.hour);
  }
  
  string sFullPath = opts.sVolRoot ~ "/" ~ newPat[0 .. newPat.lastIndexOf("/")];
  newPat = newPat[newPat.lastIndexOf("/") + 1 .. $];
  logf(LogLevel.info, "Searching for files matching %s", newPat);
  
  if(exists(sFullPath) ){
  
    foreach( string file; dirEntries(sFullPath, newPat, SpanMode.depth) ){
      logf(LogLevel.info, "Found file: %s ",baseName(file));
      lFiles ~= File(file, "rb");

    }
  }else{
    logf(LogLevel.info, "Directory %s does not exist.", sFullPath);
  }
 
  return lFiles;
}

/**
 * Read and return the next record in a file
 *
 * @param file - the file to read from
 *
 * @return WbrRecord - the record read from file
*/
WbrRecord readWbr(File file){


  ulong recBegByte = file.tell + 1; 
  if(file.size < recBegByte){
    return null;
  }
  ubyte[32] arrHdr = file.rawRead(new ubyte[32]);
  if(arrHdr.length != 32){
    logf(LogLevel.warning, "Record header length = %d but should be 32", arrHdr.length);
    return null;
  }

  ushort nLen = arrHdr[12..14].peek!ushort;
  ubyte[] arrBody = file.rawRead(new ubyte[nLen - 32]);
  if(arrBody.length != nLen - 32){
    logf(LogLevel.warning, "Record body length = %d but should be %d",
      arrHdr.length, arrBody.length - 32);
    return null;
  }

  ubyte[] rec = arrHdr ~ arrBody;

  // Only append Wbr Records
  if( arrHdr[18] & 0x40){
    string sId = format!"%s, byte %d"(file.name, recBegByte);
    return new WbrRecord(rec, sId, file);
  }

  return null;
}

class WbrRecord {
  
  ubyte[] rec;
  string sId;
  File fFile;
  //das_time_t dtBeg;
  DasTime dtBeg;
  double t2000Beg;
  bool bDtSet;
  uint nSclkSec;
  ubyte nSclkPart;
  ubyte nSclkFine;
  ushort nScetDay;
  uint nScetMilli;
  ushort nLen;
  ushort nSamples;
  ushort nRti;
  bool bMsf;
  bool bIsWbr;
  bool bIsWfr;
  bool bSuspect;
  long rHfrBaseFreq = -1;
  bool bHFR;
  ubyte nBand;
  float fPeriod;
  ubyte nGain;
  ubyte nAnaGain;
  ubyte nAnt;
  ubyte nAGC;
  int nHFR = -1;
  ubyte[] lSamples;
  string[] _getAnt = ["Ex", "Eu", "Ev", "Ew", "Bx", "By", "Bz", null, 
                      "HF", null, null, "LP", null, null, null, "UNK" ];

  // Used for calibration
  static float[2][string] _coilFactor;
  static float[string] _antLen;

  static this(){
      this._coilFactor = [ "Bx" : [0.1325, 1.0], "By" : [1.0, 1.0], "Bz":[1.0, 1.0] ];
      this._antLen = ["Ex" : 9.26, "Ew" : 5.0 ];
  }

  this(ubyte[] rec, string sId, File fFile){


    this.rec = rec;
    this.sId = sId;
    this.fFile = fFile;
    
    this.nSclkSec = rec[0 .. 4].peek!uint;
    this.nSclkPart = rec[4];
    this.nSclkFine = rec[5];

    this.begin(); //Sets the DasTime and double t2000Beg

    this.nScetDay = rec[6..8].peek!ushort;
    this.nScetMilli = rec[8..12].peek!uint;
    this.nLen = rec[12..14].peek!ushort;
    this.nSamples = rec[14..16].peek!ushort;
    this.nRti = rec[16..18].peek!ushort;

    ubyte uFlag = rec[18];
    if(uFlag & 0x80 )
      this.bMsf = true;

    if(uFlag & 0x40)
      this.bIsWbr = true;
    else{
      throw new Exception(format!"Record %s is not a WBR Record"(this.sId));
      bIsWbr = false;
    }

    if(uFlag & 0x20){
      throw new Exception(format!"Record %s is a WFR Record"(this.sId));
      this.bIsWfr = true;
    }
    else{
      this.bIsWfr = false;
    }

    //Skipping Walsh Bit
    //Skipping HFR-XLate
    //Skipping LP-DAC bit

    ubyte uStatus = rec[19];

    //Skipping AGC_ENABLE bit
    //Skipping Timeout bit

    if(uStatus & 0x20)
      this.bSuspect = true; // Timeout condition occured

    if(uStatus & 0x10)
      this.bSuspect =true; // Ground software problem
    
    // H2 Down-converter usage bit
    if(uStatus & 0x08)
      this.bHFR = true; 

    // H1 Down-converter usage bit
    if(uStatus & 0x04)
      this.bHFR = true;

    //Skipping Eu Current Bit
    //Skipping Ev Current Bit

    this.nBand = rec[20];
    if(this.nBand == 2)
      this.fPeriod = 3.6e-5;
    else if(this.nBand == 3)
      this.fPeriod = 4.5e-6;
    else
      throw new Exception(format!"Record %s has invalid WBR sampling period"(this.sId)); 

    this.nGain = rec[21];

    //Skipping Walsh compression factor
    
    //Analog gain
    this.nAnaGain = nGain & 0x7;

    this.nAnt = rec[22]; // Current antenna

    // Signal level that will be used to set next cycles gain setting
    this.nAGC = rec[23]; 

    // HFR_XLATE value, Translation Frequency when HFR is 
    // selected as signal source
    if(this.bHFR){
      this.nHFR = to!int(rec[24]); 
      this.rHfrBaseFreq = _g_HFtable[this.nHFR];
    }

    // Handle Sub-RTI timing, only present if MSF = true, and
    // sub-RTI flag bit is turned on
    //if(this.bMsf && (uFlag & 0x04)){
    if(this.bMsf && (uFlag & 0x08)){
      ubyte nMilliSec = rec[25];
      this.nScetMilli += nMilliSec;
      if(this.nScetMilli >=86400000){
        this.nScetDay += 1;
        this.nScetMilli -= 86400000;
      }

      this.nSclkFine += (nMilliSec * 256) / 1000;
      if(this.nSclkFine >= 256){
        this.nSclkSec += 1;  
        this.nSclkFine -= 256;
      }
    }
  
    // Skipping Voltage on Langmuir Probe sphere
    // Skipping Voltage on Langmuir Probe Cylinder
    // Skipping Flight Software Version

    this.lSamples = rec[32 .. 32 + this.nSamples];
  }

  /*
   * Returns true if ground processing detected an issue with this
   * waveform or if a timeout condition occured within the instrument
   * either way it's best to discard the packet.
  */
  bool suspect() @property{
    return this.bSuspect;
  }

  /*
   * Translation Frequency index when HFR is selected as a signal
   * source. Returns -1 otherwise
  */
  long mixingFreq() @property {
    return this.rHfrBaseFreq;
  }


  /*
   * Get a string indicating the antenna used to collect this waveform
  */
  string antenna() @property {
    if(this._getAnt[this.nAnt] == "HF")
      return "Ex";
    else 
      return this._getAnt[this.nAnt];
  }

  /*
   * Returns the data collection initiation time of hte first sample 
   * in the record as a Das2 Time object. Code assumes you've called
   * cspice furnsh();
  */
  DasTime begin() @property {

    if(!this.bDtSet ){

      // Formats time from records (UTC)   
      string sSclk = format!"%d/%d:%d"(this.nSclkPart, this.nSclkSec, this.nSclkFine);
      double et;
      
      scs2e_c (-82, toStringz(sSclk), &et);
      char[27] sScet;
      et2utc_c (et, "ISOC".ptr, 6, 27, sScet.ptr);
      
      this.dtBeg = DasTime(sScet);
      this.t2000Beg = dtBeg.epoch(UNIT_T2000);
      //stderr.writeln(this.t2000Beg);

      this.bDtSet = true;
    }   

    return this.dtBeg;
  }

  /*
   * Returns a ubyte[] containing uncalibrated waveform samples. Field strength
   * ranges from 0 to 255. Zero amplitudes is nominally 127.5 with 127
   * being just below and 128 just above zero amplitude
  */
  ubyte[] data() @property{
     return this.lSamples;     
  }
  
  /*
   * The timing between samples.
  */
  float period() @property {
    return this.fPeriod;
  }

  /*
   * The gain value in dB. Should range from 40 to 70 dB
  */
  int gain() @property{
    //Note: WBR data are not gained up individually so Walsh flag doesn't matter   
    return this.nAnaGain * 10;
  }

  // For calibration
  float[] lValues; 
  //                    10Khz  75Khz
  float[2] _calFactor = [6.33, 6.43]; // in dB

  /*
   * Returns a ubyte[] of calibrated waveform samples. Procedure used is 
   * documented at: /opt/project/cassini/pds/DOCUMENT/WBRWFR/WBRWFR.TXT
  */ 
  float[] calibrate() {
    
    if(this.lValues !is null){
      return this.lValues;
    }

    // -- Step a -- Find average value for the sensor's dadta set
    float rRawAvg = ( to!float(this.lSamples.sum) / this.lSamples.length ); 

    // -- Step b -- Apply the calibration factor (dB)
    // -- Step c -- Add gain amp setting 
    float db_full_scale = this._calFactor[this.nBand - 2] + this.gain;
    // -- Step c -- convert dB to linear scale
    //float linear_scale = 10f^^( db_full_scale / 20f ) / (2f^^0.5f);
    float linear_scale = pow(10f, ( db_full_scale / 20f )) / sqrt(2f);

    // -- Step d -- Convert to electric field strength (V/m)
    float rCalFactor;
    if(this.antenna.among("Ex", "Ew")){
      rCalFactor = 10^^(7.96 / 20); // Capacitive divider factor ~= 2.5
      rCalFactor /= this._antLen[this.antenna];
    }
    else if(this.antenna.among("Bx", "By", "Bz")){
      // This is because there are amplifiers with a gain of 1/24 
      // following the search coils
      rCalFactor = 24f / _coilFactor[this.antenna][this.nBand - 2]; 
    }

    //TODO check to make sure step documentation is right. 

    // -- Step e -- Normalize by dividing by the maximum amp WBR
    // sine wave. After this step, we have the actual voltage 
    // measured by the receiver
    if(isNaN(rCalFactor))
      return null;
    rCalFactor /= (127.5f * linear_scale);

    // -- Step f -- Normalize about zero, subtract DC value from 
    // all values of the data set
    this.lValues.length = this.lSamples.length;

    this.lValues[] = -1*(this.lSamples[] -rRawAvg) * rCalFactor;
    return this.lValues;
  }
  
}
