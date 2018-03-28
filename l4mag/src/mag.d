/** Helpers for dealing with MAVEN MAG data 
 */

import std.conv;
import std.math;
import std.experimental.logger;
import std.typecons;  /* To get the flag No, really... wow */
import std.string;
		
/* Playing with a symantic dictionary of prefixes
 *
 *  b = A value who's only importance is if it evaluates to true or false.
 *
 *  i = An index in some integer type, 0 is first item
 *
 *  n = A count in some integer type, 0 means no items
 *
 *  u = An unsigned integer type, must be careful with subtraction
 *
 *  f = Some floating point value, division doesn't truncate
 *
 *  l = A dynamic array, only dense in the last index, passed by value but
 *      since the value is only the length and the memory pointer it might as
 *      well be passed by reference.  Can assign .length (if mutable) .sizeof
 *      is just the "header" not the data.
 *
 *  a = A Static array, is passed by value to functions, can not be appened
        to.
 *
 *  s = Special 'dynamic' array, utf-8 immutable strings remember, 
 *      1 byte != 1 char.  (not really dynamic if immutable, but is still
 *      efficent on the stack since only the "header" is passed in)
 *
 *  d = An associative array, commonly a dictionary in other languages
 *
 *  r = Some random range.  Since arrays are ranges, use the more specific
 *      notation above if the range is known to be a common specific case
 *
 *  p = Still used for good ole pointers
 *
 *  t = A Tuple.  Named tuples are a nice feature of D.  Usually I only see
 *      these in interpreted languages
 *
 * Some notiations I have used in the past that are being dropped for my D
 * code:
 *
 *  f = File  - Files are an object type that are unlikely to be present in
 *              a high percentage of variables, having a special prefix seems
 *              less useful here.
 *
 * I'm sure other idioms will arise, will try to be consistant
 *
 */

import std.stdio; 
import std.csv;
import std.array;
import std.stdio;

import dasmini;

enum TIME = 0, X = 1, Y = 2, Z = 3;

struct MagRec{
	/** The record's time value */
	DasTime time;
	
	/** The Vector components */
	double[3] comp;

	/** The magnitude from the file might not equal sum of squares */
	double mag;
	
	string toString(){
		return format("%s, mag = %f, comp = %s", time.toIsoC(3), mag, comp);
	}
}


/****************************************************************************
 * Reads records from a Cassini Magnetometer file.
 * Only the current SCET time and the X,Y,Z Mag Vector coordinates are
 * returned.
 */
class MagReader{
private:
	string m_sPath;
	File m_file;
	bool m_bEmpty;
	MagRec m_rec;
	
	/** Internal function to try and init a MagRec object */
	bool initRec(string sLine){
		m_rec = MagRec.init;

		MagRec rec;
		auto aLine = sLine.split();
		//infof("Reading %s", aLine);
		if(aLine.length != 6) return false;
		try{
			rec.comp[0] = to!double(aLine[1]); 
			rec.comp[1] = to!double(aLine[2]); 
			rec.comp[2] = to!double(aLine[3]);
			rec.mag     = to!double(aLine[4]);
		}
		catch(ConvException e){
			//infof("Errr: you broke it %s", e.msg);
			return false;
		}
		
		// Return false if hit fill
		foreach(double f; rec.comp){ if(abs(f) > 44000.0 ) return false; }

		try  // Do time last, it's expensive
			rec.time = DasTime(aLine[0]);
		catch(ConvException e){
			//infof("Err: dastime didn't work: %s", e.msg);
			return false;
		}

		m_rec = rec;
		return true;
	}
	
public:
	/** Create a new mag reader for a MAVEN MAG sts file
	 * 
	 * @param sPath the file to open
	 * @sType the coordinate type 
	 */
	this(string sPath){
		m_sPath = sPath;
		m_bEmpty = true;
		m_file = File(sPath, "rb");
		trace("MagReader: constructor");
		popFront();
	}
	
	/** Reset the file so that it can be iterated over again */
	void reset(){ 
		m_file.seek(0); 
		// m_InRng = m_fIn.byLine(keepTerminator.yes);
		m_bEmpty = true;
	}
	
	/** Close the file, prevents all future iteration. 
	 *
	 * Don't wait for the Garbage Collector to call to destructor, call this
	 * manually when you're done.
	 */
	void close(){ trace("MagReader: close"); m_file.close(); }
	
	/** Step up the mag file for data reading from the beginning.
	 * Standard duck-type method that must exist to support starting a
	 * foreach loop
	 */
	void popFront(){
		// Read down to the next real record
		trace("MagReader: popFront");
		m_bEmpty = true;
		foreach(char[] damn_array; m_file.byLine(KeepTerminator.yes)){
		
			string sLine = to!(string)(damn_array);
			if( initRec(sLine) ){ 
				//infof("Have record: %s", m_rec.toString());
				m_bEmpty = false;
				break;
			}
		}
	}
	
	/** Have we read the last record.
	 * Standard duck-type property that must exist to let foreach loops know
	 * when iteration should stop
	 */
	@property bool empty(){ trace("MagReader: empty"); return m_bEmpty; }

	/** Return the current next mag record structure */
	@property MagRec front(){
		trace("MagReader: front");
		return m_rec;
	}
}

/*****************************************************************************
 * Get a list of MAG data files under a PDS volume directory within a time 
 * range. */
string[] getMagFilesInRng(
	in string sVolRoot, in string sCoord, in DasTime dtBeg, in DasTime dtEnd
){
	string sIdx = join([sVolRoot, "INDEX", "INDEX.TAB"], "/");

	File file = File(sIdx, "rb");
	DasTime dtProdBeg, dtProdEnd;
	string[] lFiles;
	
	bool bFirst = true;
	foreach(char[] line; file.byLine(No.keepTerminator, "\r\n")){
		if(bFirst){
			bFirst = false;
			continue;
		}
		
		// Had to do this in a manual loop since I couldn't figure out how
		// templates would get it done
		char[][] fields;
		const(char)[] bite_me = ['"'];
		foreach(char[] word; split(line, ',')){ 
			if(word[0] == '"') word = word[1..$];
			if(word[$-1] == '"') word = word[0..$-1];
			fields ~= strip(word);
		}
		
		//infof("Field 1: %s", fields[1]);
				
		auto aToks = fields[1].split("_");
		//infof("File is: %s, Tok 4 is: %s", fields[1], aToks[4]);
		//infof("Coord is: %s", sCoord);
		int iCoord = aToks.length == 5 ? 3 : 4;
		
		if((aToks.length < 4) || cmp(aToks[iCoord], sCoord) != 0) continue;

		//infof("File %s might work", fields[1]);
		
		DasTime beg = DasTime(fields[6]);
		DasTime end = DasTime(fields[7]);
		//infof("Will check times on %s", fields[1]);
		writeln(dRow);
		if((beg >= dtEnd ) || (end <= dtBeg)) continue;

		string sProd = sVolRoot;
		sProd ~= fields[1].replace(".LBL", ".TAB");
		lFiles ~= sProd;
		//infof("Candidate file: %s", sProd);
	}
	
	return lFiles;
}
