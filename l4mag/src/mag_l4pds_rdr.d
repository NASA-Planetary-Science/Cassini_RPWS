// Reader for 1-second average cassini manetometer data from the volume:
//
//  CO-E_SW_J_S-MAG-4-SUMM-1SECAVG-V1.0

import std.getopt, std.stdio;
import std.algorithm.searching;  // gets startsWith
import std.string;               // gets split
import std.regex;                // gets replaceAll
import std.experimental.logger;

import dasmini, mag;

struct Opts{ 
	string sVolPath = "/opt/project/cassini/mag/CO-E_SW_J_S-MAG-4-SUMM-1SECAVG-V1.0";
	string sLogLvl = "info";
	string sCoords = "KSO";  //See help text for list of coordinates
};

/* ** Sending a Das2 Header *********************************************** */

void sendHdr(in DasTime dtBeg, in DasTime dtEnd, in string sCoords)
{

	//Static compile information
	version(LittleEndian){
		string sFloatType = "little_endian_real4";
		string sDoubleType = "little_endian_real8";
	}
	else{
		string sFloatType = "sun_real4";
		string sDoubleTime = "sun_real8";
	}

	auto buf = new dasmini.HdrBuf(0);
	
	string sTitle;

	string[4] aVars; 
	string[4] aLbls;
	string[4] aUnits = ["nT", "nT", "nT", "nT"];
	string[4] aSumm;
	
	switch(sCoords){
	case "RTN":
		sTitle = "Cassini - FGM: Heliocentric Radial-Tangential-Normal (RTN)";
		aVars  = ["magnitude", "radial", "tangental", "normal"];
		aLbls  = ["|B|", "B!dR!n", "B!dT!n", "B!dN!n"];
		aSumm = [
		   "Average value of the full-resolution magnitudes over the 1 second interval",
		   "The radial component of the magnetic-field points from the Sun to "~
		   "the spacecraft, positive away from the Sun.",
		   "The tangential component of the magnetic-field s parallel to the "~
		   	"Solar Equatorial plane (Omega Sun x R).",
		   "The normal component of the magnetic-field completes the right "~
		   	"handed set, and is roughly normal to the Solar Equatorial plane (R x Φ)."
		];
		break;

	case "KRTP":
		sTitle = "Cassini - FGM: Kronocentric Magnetic Dipole Fixed (KRTP)";
		aVars  = ["magnitude", "radial", "southward", "azimuthal"];
		aLbls  = ["|B|", "B!dR!n", "B!dΘ!n", "B!dΦ!n"];
		aSumm = [
		   "Average value of the full-resolution magnitudes over the 1 second interval",
		   "The radial component of the magnetic-field is along the Saturn to "~
		   	"spacecraft line, positive away from Saturn.",
		   "The southward (B-theta) component of the magnetic-field completes "~
		   	"the right handed set (Phi x R) and is positive southward.",
		   "The azimuthal (B-phi) component of the magnetic-field is parallel "~
		   	"to Saturn's equator (Omega x R), perpendicular to the Saturn-"~
		   	"spacecraft line, and positive in the direction of corotation."
		];
		break;

	case "KSM":
		sTitle = "Cassini - FGM: Kronocentric Solar Magnetospheric (KSM)";
		aVars  = ["magnitude", "X", "Y", "Z"];
		aLbls  = ["|B|", "B!dX!n", "B!dY!n", "B!dZ!n"];
		aSumm = [
		   "Average value of the full-resolution magnitudes over the 1 second interval",
			"X component of the magnetic-field points from Saturn to the Sun.",
			"Y component of the magnetic-field is perpendicular to the magnetic "~
				"dipole (Omega) and the Saturn-Sun direction (Omega x X).",
			"Z component of the magnetic-field is defined such that the X-Z "~ 
				"plane contains Saturn's centered magnetic dipole axis (M)."
		];
		break;

	case "KSO":
		sTitle = "Cassini - FGM: Kronocentric Solar Orbital Coordinates (KSO)";
		aVars  = ["magnitude", "X", "Y", "Z"];
		aLbls  = ["|B|", "B!dX!n", "B!dY!n", "B!dZ!n"];
		aSumm   = [
		   "Average value of the full-resolution magnitudes over the 1 second interval",
		   "The KSO X comonent of the magnetic-field points from Saturn to the Sun.",
		   "The KSO Y comonent of the magnetic-field is parallel to Saturn's orbital "~
		   	"plane, perpendicular to the Saturn-Sun line.",
		   "The KSO Z comonent of the magnetic-field is parallel to Saturn's orbital "~
		   	"plane upward normal."
		];
		break;

	case "J3":
		sTitle = "Cassini - FGM: Jovicentric Body-Fixed (IAU_JUPITER, J3)";
		aVars  = ["magnitude", "X", "Y", "Z"];
		aLbls  = ["|B|", "B!dX!n", "B!dY!n", "B!dZ!n"];
		aSumm  = [
		   "Average value of the full-resolution magnitudes over the 1 second interval",
		   "The J3 X component of the magnetic-field is in the Jovian equatorial "~
		   	"plane along the line through the System 3 (1997), positive away "~
		   	"from Jupiter",
		   "The J3 Y component of the magnetic-field completes the right handed "~
		   	"set (Z x X).",
		   "The J3 Z component of the magnetic-field is parallel to Jovian spin "~
		   	"axis, Omega, positive north of the equator."
		];
		break;

	case "JMXYZ":
		sTitle = "Cassini - FGM: Jovicentric Body-Fixed Magnetospheric (JMXYZ)";
		aVars  = ["magnitude", "X", "Y", "Z"];
		aLbls  = ["|B|", "B!dX!n", "B!dY!n", "B!dZ!n"];
		aSumm  = [
		   "Average value of the full-resolution magnitudes over the 1 second interval",
		   "The JMXYZ X component of the magnetic-field is in the plane containing "~
		   	"Z and the IAU_JUPITER (S3 1997) Prime Merdian.",
		   "The JMXYZ Y component of the magnetic-field completes the right handed "~
		   	"set (Z x X).",
		   "The JMXYZ Z component of the magnetic-field is the parallel to the "~
		   	"Jupiter dipole."
		];
		break;
	default:
		assert(false);
	}

	buf.addf("<stream version=\"2.2\">\n"~
"<properties String:title=\"%s%%{xCacheResInfo}\"\n"~
"            String:xLabel=\"SCET (UTC)\"\n"~
"            Datum:xTagWidth=\"10 s\"\n"~
"            String:yScaleType=\"linear\"\n"~
"            DatumRange:xCacheRange=\"%s to %s UTC\"\n"~
"            String:xCacheResInfo=\" (Intrinsic Resolution)\"\n"~
"/>\n"~
"</stream>\n", sTitle, dtBeg.toIsoC(3), dtEnd.toIsoC(3) );

	buf.send(stdout);
	
	buf = new dasmini.HdrBuf(1);
	buf.addf("<packet>\n"~
"  <x type=\"%s\" units=\"us2000\"></x>\n", sDoubleType
	);
	
	for(int i = 0; i < 4; ++i){
		buf.addf("  <y type=\"%s\" name=\"%s\" units=\"%s\" >\n"~
"    <properties String:yLabel=\"%s\"\n"~
"                String:ySummary=\"%s\" />\n"~
"  </y>\n", sFloatType, aVars[i], aUnits[i], aLbls[i], aSumm[i]
		);
	}
	
	buf.add("</packet>\n");
	buf.send(stdout);
}

/* ************************************************************************ */

PktBuf l_pkt;   // This "gloabl" is actually thread local, cool!

int sendRec(MagRec rec){
	
	l_pkt.addDoubles!double([rec.time.epoch(UNIT_US2000)]);
	l_pkt.addFloats!double([rec.mag] ~ rec.comp);
	l_pkt.send(stdout);
	
	return 1;
}

/* ** Main and Help ******************************************************** */

shared string l_sCoordDesc = 
"The command line parameter COORDS defaults to KSO the available coordinates\n"~
"systems are defined below, text is from the Volumes DATASET catalog file,\n"~
"CO_MAG_CAL_1SEC_DS.CAT.\n"~
"\n"~
"   RTN    Radial-Tangential-Normal coordinates, a solar wind coordinate\n"~
"          system. RTN coordinates consist of R (radial component, Sun to\n"~
"          the spacecraft), T (tangential component, parallel to the Solar\n"~
"          Equatorial plane and perpendicular to R), and N (normal component,\n"~
"          completes right handed set).\n"~
"\n"~
"   KRTP   Kronocentric body-fixed, J2000 spherical Coordinates, a Saturn-\n"~
"          centered coordinate system. KRTP magnetic field vector components\n"~
"          form the standard right-handed spherical triad (R, Theta, Phi) for\n"~
"          a planet-centered system.  Namely, R is radial (along the line\n"~
"          from the center of Saturn to the center of the spacecraft), and\n"~
"          positive away from Saturn. Phi, the azimuthal component, is \n"~
"          parallel to the Kronographic equator (Omega x R) and positive in\n"~
"          the direction of corotation. Theta, the 'southward' component,\n"~
"          completes the right-handed set.\n"~
"\n"~
"  KSM     Kronocentric Solar Magnetospheric Coordinates, a cartesian Saturn-\n"~
"          center coordinate system where X points from Saturn to the Sun,\n"~
"          the X-Z plane contains Saturn's centered magnetic dipole axis, M,\n"~
"          and Y completes right handed set.\n"~
"\n"~
"  KSO     Kronocentric Solar Orbital Coordinates, a cartesian Saturn-\n"~
"          centered coordinate system where X points from Saturn to the Sun,\n"~
"          Z is parallel to Saturn's orbital plane upward normal, and Y\n"~
"          completes the right handed set.\n"~
"\n"~
"  J3      Jovicentric Body-Fixed (IAU_JUPITER), a body-fixed Jovicentric\n"~
"          cartesian coordinate system consistent with the IAU 1997 definition\n"~ 
"          of the System 3 prime meridian where X is in the Jovian equatorial\n"~
"          plane, along S3 1997 Prime Meridian, Z is parallel to the Jovian\n"~
"          spin axis, and Y completes right-handed set.\n"~
"\n"~
"  JMXYZ   Jovicentric Body-Fixed Magnetospheric Coordinates, a body-fixed\n"~
"          Juptiter centered cartesian coordinate system where X is in the\n"~
"          plane containing Z and the S3 1997 Prime Meridian, Z is the Jupiter\n"~
"          magnetic dipole, and Y completes right handed set.\n"~
"\n";

int main(string[] aArgs)
{
	int nRet = 0;

	Opts opts;
	
	auto cmdLine = getopt(aArgs,
		"volume|v", "Specify an alternate top level directory for the Cassini "~
		"Flux Gate Magnetometer (FGM) PDS volume: CO-E_SW_J_S-MAG-4-SUMM-1SECAVG-V1.0.  "~
		"The default is " ~ opts.sVolPath, &opts.sVolPath,
		
		"log-level|l", "Logging level, one of 'critical', 'error', 'warning', "~
		"'info', or 'debug'.  The default is "~opts.sLogLvl,
		&opts.sLogLvl
	);
	
	if(cmdLine.helpWanted){
		// This function's output looks awful in a terminal, we need a replacement
		defaultGetoptFormatter(
			stderr.lockingTextWriter(), 
			"Usage: fgm_l4pds_rdr [options] BEGIN END [COORDS]\n\n"~ l_sCoordDesc,
			cmdLine.options
		);
		return 0;
	}

	if(opts.sLogLvl.startsWith('c')) globalLogLevel(LogLevel.critical);
	else if(opts.sLogLvl.startsWith('e')) globalLogLevel(LogLevel.error);
	else if(opts.sLogLvl.startsWith('w')) globalLogLevel(LogLevel.warning);
	else if(opts.sLogLvl.startsWith('i')) globalLogLevel(LogLevel.info);
	else if(opts.sLogLvl.startsWith('d')) globalLogLevel(LogLevel.trace);
	else globalLogLevel(LogLevel.fatal);

	// After the call to getopt, inspect the remaining args to get the 
	// begin, end and optional coordinates flag.
	if( aArgs.length > 4){
		errorf("Extraneous arguments on command line: '%s'", aArgs[4..$]);
		return 13;
	}
	if( aArgs.length < 2){
		errorf("Missing BEGIN and/or END time on command line");
		return 13;
	}
	
	string sBeg = aArgs[1];
	string sEnd = aArgs[2];
	DasTime dtBeg, dtEnd;
	try{
		dtBeg = DasTime(sBeg);
		dtEnd = DasTime(sEnd);
	}
	catch(ConvException e){
		errorf(e.msg);
		return 13;
	}
	
	// A Das2 thing, the last argument may actually be a series of arguments
	// that was supplied in quotes and thus looks like a single argument.  
	aArgs = aArgs[3..$];
	if(aArgs.length == 1){
		auto lTmp = aArgs[0].split();
		foreach(int i, string s; lTmp)
			lTmp[i] = s.replaceAll(regex("['\" \t\n\r\v]"), "").toLower();
		aArgs = lTmp;
	}

	string sCoords = "KSO";
 	foreach(string s; aArgs){  // Last one wins 
 		string sU = s.toUpper();
 		switch(sU){
 		case "RTN":   sCoords = sU; break;
 		case "KRTP":  sCoords = sU; break;
 		case "KSM":   sCoords = sU; break;
 		case "KSO":   sCoords = sU; break;
 		case "J3":    sCoords = sU; break;
 		case "JMXYZ": sCoords = sU; break;
 		default:
 			errorf("Unknown coordinate system '"~s~"' specified, use -h for help");
 			return 13;
 		}
 	}
 	
 	sendHdr(dtBeg, dtEnd, sCoords);
 	
 	
 	auto aFiles = getMagFilesInRng(opts.sVolPath, opts.sCoords, dtBeg, dtEnd);
	infof("%d files found", aFiles.length);
	
	int nSent = 0;
	
	l_pkt = new PktBuf(1);
	version(LittleEndian){
		l_pkt.encodeLittleEndian();
	}
	else{
		l_pkt.encodeBigEndian();
	}

	foreach(string sPath; aFiles){
		infof("Processing %s ...", sPath);
		auto rdr = new MagReader(sPath);
		
		foreach(MagRec rec; rdr){
			tracef("Rec time %s, reader interval %s to %s", rec.time.toIsoC(3), 
			       dtBeg.toIsoC(3), dtEnd.toIsoC(3));
		
			if(rec.time >= dtEnd) break;  // Assumes ordered input data
			if(rec.time <= dtBeg) continue;
			
			nSent += sendRec(rec);
		}
		rdr.close();
	}
	
	if(nSent == 0) sendNoData(stdout, dtBeg, dtEnd);
	return 0;
}

