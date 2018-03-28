module dasmini;

/** Traditional Das2 functionality we've grown used to having in any 
 * Language
 */

import std.datetime;
import std.algorithm.searching;
import std.array;
import std.stdio;
import std.format;
import std.bitmanip;
import std.traits;
import std.string; 
import std.conv;
import std.experimental.logger;
import core.stdc.string;

public import std.conv: ConvException;

// Use ConvException for error converting times, otherwise this is okay
class DasException : Exception{
	this(string sMsg){ super(sMsg); }
}



// Link with -ldas2 to get these

extern (C) int parsetime (
	const char *string, int *year, int *month, int *day_month, int *day_year,
	int *hour, int *minute, double *second
);

struct das_time_t{
	int year; 
	int month; 
	int mday; 
	int yday; 
	int hour;
	int minute; 
	double second;	
};

extern (C) double dt_diff(const das_time_t* pA, const das_time_t* pB);
extern (C) char* dt_isoc(char* sBuf, size_t nLen, const das_time_t* pDt, int nFracSec);
extern (C) char* dt_isod(char* sBuf, size_t nLen, const das_time_t* pDt, int nFracSec);
extern (C) char* dt_dual_str(char* sBuf, size_t nLen, const das_time_t* pDt, int nFracSec);
extern (C) void  dt_tnorm(das_time_t* dt);

extern(C) extern const(char*) UNIT_US2000; /* microseconds since midnight, Jan 1, 2000 */

extern(C) extern const(char*) UNIT_T2000;

extern(C) double Units_convertFromDt(UnitType epoch_units, const das_time_t* pDt);
extern(C) void Units_convertToDt(das_time_t* pDt, double value, UnitType epoch_units);
extern(C) bool Units_haveCalRep(UnitType unit);
extern(C) const(char*) Units_toStr(UnitType unit);
void tnorm (int *year, int *month, int *mday, int *yday,
            int *hour, int *minute, double *second);

alias UnitType = const(char)*;  // No class for units, just manipulates unsafe 
                               // string pointers

/** Time handling class that drops time zone complexity and sub-second 
 *  integer units.  Has conversions to epoch times */
struct DasTime{
	int year = 1; 
	int month = 1; 
	int mday = 1; 
	int yday = 1;   // Typically read only except for normDoy()
	int hour = 0;   // redundant, but explicit beats implicit
	int minute = 0; // default value for ints is 0 
	double second = 0.0;

	double fEpoch = double.nan;   // Save the epoch value if it has been 
	                             // computed
	UnitType ut = null;

	/** Construct a time value using a string */
	this(const(char)[] s){
		int nRet;
		//infof("Parsting time string: %s", s);
		nRet = parsetime(s.toStringz(),&year,&month,&mday,&yday,&hour,&minute,&second);
		if(nRet != 0)
			throw new ConvException(format("Error parsing %s as a date-time", s));
	}

	void setFromDt(das_time_t* pDt){
		year = pDt.year;
		month = pDt.month;
		mday = pDt.mday;
		yday = pDt.yday;
		hour = pDt.hour;
		minute = pDt.minute;
		second = pDt.second;
	}

	das_time_t toDt() const{
		das_time_t dt;
		dt.year = year;
		dt.month = month;
		dt.mday = mday;
		dt.yday = yday;
		dt.hour = hour;
		dt.minute = minute;
		dt.second = second;
		return dt;
	}

	this(double value, UnitType units){
		das_time_t dt;
		if(! Units_haveCalRep(units)) 
			throw new ConvException(
				format("Unit type %s not convertable to a date-time", Units_toStr(units))
			);
		Units_convertToDt(&dt, value, units);
		setFromDt(&dt);
		ut = units;
		fEpoch = value;
	}

	/** Create a time using a vairable length tuple.
	 * 
	 * Up to 6 arguments will be recognized, at least one must be given
	 * year, month, day, hour, minute, seconds
	 * All items not initialized will recive default values which are
	 *  year = 1, month = 1, day = 1, hour = 0, minute = 0, seconds = 0.0
	 *
	 */
	
	this(T...)(T args){
		static assert(args.length > 0);
		year = args[0];
		static if(args.length > 1) month = args[1];
		static if(args.length > 2) mday = args[2];
		static if(args.length > 3) hour = args[3];
		static if(args.length > 4) minute = args[4];
		static if(args.length > 5) second = args[5];
	}

	double epoch(UnitType units){
		if(ut != units){
			if(! Units_haveCalRep(units)) 
			throw new ConvException(
				format("Unit type %s not convertable to a date-time", Units_toStr(units))
			);
			ut = units;
			das_time_t dt = toDt();
			fEpoch = Units_convertFromDt(units, &dt);
		}
		return fEpoch;
	}

	string toIsoC(int fracdigits) const{
		char[64] aBuf = '\0';
		das_time_t dt = toDt();
		dt_isoc(aBuf.ptr, 63, &dt, fracdigits);
		return aBuf.idup[0..strlen(aBuf.ptr)];
	}

  /*
  string toString() const{
    
  }
  */

  void norm(){
    das_time_t dt = toDt(); 
    dt_tnorm(&dt);
    setFromDt(&dt);
	 if(ut !is null){
		 fEpoch = Units_convertFromDt(ut, &dt);
	 }
  }

	string toIsoD(int fracdigits) const{
		char[64] aBuf = '\0';
		das_time_t dt = toDt();
		dt_isod(aBuf.ptr, 63, &dt, fracdigits);
		return aBuf.idup[0..strlen(aBuf.ptr)];
	}

	string toDual(int fracdigits) const{
		char[64] aBuf = '\0';
		das_time_t dt = toDt();
		dt_dual_str(aBuf.ptr, 63, &dt, fracdigits);
		return aBuf.idup[0..strlen(aBuf.ptr)];
	}

	int opCmp(in DasTime other) const {
		if(year < other.year) return -1; if(year > other.year) return 1;
		if(month < other.month) return -1; if(month > other.month) return 1;
		if(mday < other.mday) return -1; if(mday > other.mday) return 1;
		if(hour < other.hour) return -1; if(hour > other.hour) return 1;
		if(minute < other.minute) return -1; if(minute > other.minute) return 1;
		if(second < other.second) return -1; if(second > other.second) return 1;
		return 0;
	}
};

/************************************************************************** 
 * Handles buffering data and prepending proper header ID's for Das2 Headers
 * All output is in UTF-8.
 */
class HdrBuf{

	enum HeaderType { 
		das2 = 1,   /** Output headers without the <?xml version info */
		qstream = 2 /** Include <?xml version declairation on each header packet */
	};
	
	HeaderType m_type;
	string[] m_lText;
	int m_nPktId;
	
	this(int nPktId, HeaderType ht = HeaderType.das2){
		assert(nPktId > -1 && nPktId < 100, format("Invalid Packet ID: %s", nPktId));
		
		m_nPktId = nPktId;
		m_type = ht;
		if(m_type == HeaderType.qstream)
			m_lText[0] = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
	}
	
	void add(in string sText){ 	m_lText ~= sText;  }
	
	void addf(T...)(T args) { m_lText ~= format(args); }
	
	void send(File fOut){
		string sOut = join(m_lText);
		fOut.writef("[%02d]%06d%s", m_nPktId, sOut.length, sOut);
		fOut.flush();
		m_lText.length = 0;
		if(m_type == HeaderType.qstream)
			m_lText[0] = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
	}
}
/****************************************************************************
 * Handles outputting data packets for das2 streams and qstreams	
 */

class PktBuf{
	enum Endian { big = 1, little = 2};
		
	int m_nPktId;
	ubyte[][] m_aData;
	Endian m_endian = Endian.little;

private:
	final void startPkt(){
		string s = format(":%02d:", m_nPktId);
		m_aData.length = 1;
		m_aData[0] = new ubyte[s.length];
		for(int i = 0; i < s.length; i++) m_aData[0][i] = s[i];	
	}

public:	
	
	this(int nPktId){
		assert(nPktId > 0 && nPktId < 100, format("Invalid Packet ID: %s", nPktId));
		m_nPktId = nPktId;
		startPkt();
	}
	
	void encodeLittleEndian(){
		m_endian = Endian.little;
	}
	void encodeBigEndian(){
		m_endian = Endian.big;
	}
		
	void add(immutable(ubyte)[] uBytes){ 
		ulong u = m_aData.length;
		foreach(int i, ubyte b; uBytes) m_aData[u][i] = b; 
	}
	
	void addf(T...)(T args) { 
		string s = format(args);
		m_aData.length += 1;
		m_aData[$-1] = new ubyte[s.length];
		for(int i = 0; i < s.length; i++) m_aData[$-1][i] = s[i];
	}
	
	void addFloats(T)(in T[] lNums) 
	     if (isAssignable!(T, float))
	{
		float val;
		ubyte[4] bytes;
		ubyte[] allBytes = new ubyte[ lNums.length * 4 ];
		
		foreach(int i, T t; lNums){
			val = t;
			if(m_endian == Endian.little) bytes = nativeToLittleEndian(val);
			else bytes = nativeToBigEndian(val);
			
			for(int j; j < 4; j++) allBytes[i*4 + j] = bytes[j];
		}
		
		m_aData.length += 1;
		m_aData[$-1] = allBytes;
	}

	void addDoubles(T)(in T[] lNums) 
	     if (isAssignable!(T, double))
	{
		double val;
		ubyte[8] bytes;
		ubyte[] allBytes = new ubyte[ lNums.length * 8 ];
		
		foreach(int i, T t; lNums){
			val = t;
			if(m_endian == Endian.little) bytes = nativeToLittleEndian(val);
			else bytes = nativeToBigEndian(val);
			
			for(int j; j < 8; j++) allBytes[i*8 + j] = bytes[j];
		}
		
		m_aData.length += 1;
		m_aData[$-1] = allBytes;
	}
	
	void send(File fOut){
		ubyte[] uOut = join(m_aData);
		fOut.rawWrite(uOut);
		fOut.flush();
		m_aData.length = 0;
		startPkt();
	}
}


immutable char[] DAS2_EXCEPT_NODATA = "NoDataInInterval";
immutable char[] DAS2_EXCEPT_BADARG = "IllegalArgument";
immutable char[] DAS2_EXCEPT_SRVERR = "ServerError";

/************************************************************************** 
 * Send a formatted Das2 exception
 * Params:
 *  fOut = The file object to receive the XML error packet
 *  sType = The exception type. Use one of the pre-defined strings
 *          DAS2_EXCEPT_NODATA
 *          DAS2_EXCEPT_BADARG
 *          DAS2_EXCEPT_SRVERR
 *  sMsg = The error message
 */
void sendException(File fOut, string sType, string sMsg){
	auto sFmt = "<exception type=\"%s\" message=\"%s\" />\n";
	sMsg = sMsg.replace("\n", "&#13;&#10;").replace("\"", "'");		  
	auto sOut = format(sFmt, sType.replace("\"", "'"), sMsg);
	fOut.writef("[xx]%06d%s", sOut.length, sOut);
}

/* ************************************************************************ */
void sendNoData(File fOut, DasTime dtBeg, DasTime dtEnd){
	auto buf = new HdrBuf(0);
	auto sMsg = format("No FGM data in the interval %s to %s", 
	                   dtBeg.toIsoC(3), dtEnd.toIsoC(3));
	warning(sMsg);
	sendException(fOut, "NoDataInInterval", sMsg);
}


/* ************************************************************************ */
/* Help text looks like trash, improve printing */

//void helpPrinter(string sHdr, GetoptResult res){
//	Output output = stdout.lockingTextWriter();
//	
//}

// DFT

struct dft_plan;
alias DftPlan = dft_plan;

extern(C) DftPlan* new_DftPlan(size_t uLen, bool bForward);
bool del_DftPlan(DftPlan* pThis);

struct das2_dft_t{
	void* vpIn;
	void* vpOut;
	size_t uLen;
	bool bRealOnly;
	char* sWindow;
	double* pWnd;
	bool bNewMag;
	double* pMag;
	size_t uMagLen;
	bool[2] bNewCmp;   /* fftw convention: 0 = reals, 1 = img */
	double*[2] pCmpOut;
	size_t[2] uCmpLen;	
};

alias Das2Dft = das2_dft_t;

extern (C) Das2Dft* new_Dft(DftPlan* pPlan, const char* sWindow);
extern (C) void del_Dft(Das2Dft* pThis);
extern (C) int Dft_calculate(
	Das2Dft* pThis, const double* pReal, const double* pImg
);
extern (C) const (double)* Dft_getReal(Das2Dft* pThis, size_t* pLen);
extern (C) const (double)* Dft_getImg(Das2Dft* pThis, size_t* pLen);
extern (C) const (double)* Dft_getMagnitude(Das2Dft* pThis, size_t* pLen);

/** Builder convienance function, warning it saves @b everything  */
/* extern (C) CorDs** build_from_stdin(char* sProgName, size_t* pSets); */




