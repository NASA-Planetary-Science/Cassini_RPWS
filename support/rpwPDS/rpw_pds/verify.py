"""Utilities for wrapping lvtool and other PDS command line utilities."""

import logging
import re
import os
import string
import stat

from os.path import join as pjoin

import util #Iowa PDS util module

##############################################################################
def _runCmd(log, sCmd):
	log.debug("exec: %s"%sCmd)
	nExit = os.system(sCmd)
	if nExit != 0:
		sTmp = "Non-zero process exit, command: '%s' returned %i"%(sCmd, nExit)
		raise RuntimeError(sTmp)


def _runCmdWarnCheck(log, sCmd):
	log.debug("exec: %s"%sCmd)
	nExit = os.system(sCmd)
	if nExit != 0:
		sTmp = "Non-zero process exit, command: '%s' returned %i"%(sCmd, nExit)
		log.warning(sTmp)
		

def _runCmdNoCheck(log, sCmd):
	log.debug("exec: %s"%sCmd)
	os.system(sCmd)
	
############################################################################
#def ckLinks(sStageDir, sLinkLint='linklint', nUpperLimit=8000):
#	"""Wrapper around the perl program linklint.
#	
#	Returns number of errors and warnings found, except missing indecies 
#	warnings are ignored.  Logs problems.
#	
#	If the sub process does not exit normally, runtimeerror is thrown
#	"""
#	
#	sOutFile = "%s_linklint.txt"%sStageDir
#	
#	lArgs = ['-root','sStageDir','-limit',str(nUpperLimit),'-error','-warn',
#	         '-xref','-out',sOutFile,'-quiet','-silent','/@']
#	log.info('execute: %s %s'%(sLinkLint, string.join(lArgs))
#	
#	nPid = None
#	if os.path.isabs(sLinkLint):
#		nExit = os.spawnl(os.P_WAIT, sLinkLint, lArgs)
#	else:
#		nExit = os.spawnlp(os.P_WAIT, sLinkLint, lArgs)
#	
#	if nExit < 0:
#		#A signal killed this process
#		raise RuntimeError('linklint process did not exit normally.')
#	
#	lIssue = []
#	for sLine in file(sOutFile, 'rb'):
#	
#		#If I'm not in a issue, then see if I can get into one.
#		if sLine.startswith('#') and len(lIssue) == 0:
#			
#	
#	return None
		



##############################################################################	
def runLinkLint(sRoot, sLog, sLinkLint="/usr/local/bin/linklint", sPerl="perl"):
	"""Run link lint but don't parse the output.  Link lint is run with the
	args:
	
	linklint -root sRoot -limit 16000 -error -warn -xref -out sLog -quiet \ 
	   -silent -no_warn_index /@
		
	If the sub process does not exit normally, an error is logged.
	"""
	log = logging.getLogger('runLinkLint')
	
	# If there is already an old log file delete it so that don't think it
	# is the current output.
	if os.path.exists(sLog):
		os.remove(sLog)
	
	log.info("Generating Link Lint report at: %s"%sLog)
	
	lArgs = ['-root',sRoot,'-limit',str(16000),'-error','-warn',
	         '-xref','-out',sLog,'-quiet','-silent','-no_warn_index','/@']
	
	sExe = '%s %s %s'%(sPerl, sLinkLint, string.join(lArgs))
	log.info('execute: %s'%sExe)
	
	nExit = os.system(sExe)
	if nExit != 0:
		#A signal killed this process
		sTmp = 'linklint process did not exit normally, return val: '
		sTmp +="%i, exec: %s"%(nExit, sExe)
		log.error(sTmp)
	
	return

	
##############################################################################
# Three functions for checking single labels

_regNum      = re.compile('[0-9]+')
_regODLError = re.compile('ODL Errors:\s+[0-9]+')
_regODLWarn  = re.compile('ODL Warnings:\s+[0-9]+')
_regDDError  = re.compile('DD Errors:\s+[0-9]+')
_regDDWarn   = re.compile('DD Warnings:\s+[0-9]+')

_lPtns = [
  (_regODLError, "ODL", "e"),
  (_regODLWarn,  "ODL", "w"),
  (_regDDError,  "DD",  "e"),
  (_regDDWarn,   "DD",  "w")
]

def setupLvtEnv():
	"""An unfortunate side effect of find being called from lvtool and find
	not following symlinks by default.
	"""
	sCur = os.getcwd()
	fOut = file('%s/find'%sCur, 'wb')
	fOut.write("#!/bin/sh\n/usr/bin/find $1 -follow $2 $3 $4 $5 $6 $7 $8\n")
	fOut.close()
	
	os.chmod('%s/find'%sCur, stat.S_IRWXU)
	os.environ['PATH'] = ".:%s"%os.environ['PATH']
	

def _getNonZeroLvtReportErrs(log, sLabel, sLine, bIgnoreWarnings = False):
	"""Read lines that look like:
	   *                ODL Errors:      0  ODL Warnings:      0              *
	   *                 DD Errors:     40   DD Warnings:      0              *
		
		And return tuples that look like (nErrs, nWarn, nStr)
	"""
	sRet = ""
	for tup in _lPtns:
		l = tup[0].findall(sLine)
		if len(l) > 0:
			lNum = _regNum.findall(l[0])
			nNum = int(lNum[0])
			if nNum > 0:
				if tup[2] == "e":
					log.error("LABEL %s - %s Errors: %d"%(sLabel, tup[1], nNum))
					sRet = sLine
				elif not bIgnoreWarnings:
					log.warning("LABEL %s - %s Warnings: %d"%(sLabel, tup[1], nNum))
					sRet = sLine
	
	if len(sRet) > 0 and sRet[-1:] != "\n": #Insure newline
		sRet += "\n"
		
	return sRet
	

def checkLabel(log, fOut, sLabel, sStageDir, sPDStools):
	"""Runs:
	
		$PDSTOOLS/bin/lvtool -ninfo -nwarn -d $PDSTOOLS/data/pdsdd.idx -s -r lvt_temp.log
	     -e -b3 sStage -f sLabel
		
		And merges output into the given stream, then cleans up lvt_temp.log
	"""
	
	#Check to see pwd/find exists.
	sTmp = "%s/find"%(os.getcwd())
	if not os.path.exists(sTmp):
		raise RuntimeError("lvtool environment not configured, %s does not exist!"%sTmp)
	
	sCmd = "%s/bin/lvtool -ninfo -d %s/data/pdsdd.idx"%(sPDStools, sPDStools)
	sCmd +=" -s -r lvt_temp.log -e -b3 %s -f %s > /dev/null"%(sStageDir, sLabel)
	
	_runCmdNoCheck(log, sCmd)
	
	fLog = file('lvt_temp.log', 'rb')
	nParse = 0 #when up to 2 then go
	bNameInserted = False
	for sLine in fLog:
		if nParse < 2:
			if sLine.startswith('*****************************************************************'):
				nParse += 1
				continue
				
		if sLine.find('ERROR') != -1:
			if not bNameInserted:
				fOut.write("LABEL %s:\n"%sLabel)
			fOut.write(sLine)
	
		if sLine.find('ODL Errors:') != -1 or sLine.find('DD Errors:'):
			sOut = _getNonZeroLvtReportErrs(log, sLabel, sLine, True)
			if sOut != "":
				if not bNameInserted:
					fOut.write("LABEL %s:\n"%sLabel)
				fOut.write(sOut)
				
	os.remove('lvt_temp.log')


def teardownLvtEnvCleanStage(log, sStage):
	"""Resets env to normal, gets rid of temp find command and cleans
	up and files that end in .x[0-9][0-9] in the staging area.
	"""

	if os.environ['PATH'][:2] == ".:":
		os.environ['PATH'] = os.environ['PATH'][2:]
	if os.path.exists("%s/find"%os.getcwd()):
		os.remove("%s/find"%os.getcwd())
		
	lRm = util.find('*.x[0-9][0-9]', sStage, util.FILES, util.NODIRS)
	for sRm in lRm:
		# Don't expect this code to run often, so get logger for each 
		# occurance.
		os.remove(sRm)
		log.warning('Removing temp file: %s left by lvtool'%sRm)

def _writeLabelCheckMsg(fFile, sPDStools, sStage):
	fFile.write("\nTo get more verbose output on an individual file issue:\n\n")
	fFile.write("   $ echo \"#!/bin/sh\n   /usr/bin/find $1 -follow $2 $3 $4 $5 $6 $7 $8\" > find\n")
	fFile.write('   $ chmod +x find\n')
	fFile.write('   $ export PATH=.:$PATH\n')
	fFile.write("   $ %s/lvtool -d %s/data/pdsdd.idx -r lvt_temp.log -e -b3 %s -f LABELFILE\n"%(
	           sPDStools, sPDStools, sStage))
	fFile.write("\nAnd open lvt_temp.log in your favorite editor\n\n")

##############################################################################
def runPVTools(sStageDir, sPVTLog, sPDStools, sPerl, sTest="ltrq",
               lLabels=[]):
	"""Run validate.pl and testline.pl on the stage directory.
	
		sTest - What to test string.  Tests are triggered by inclusion
		    of certian letters.  Default='ltrq'
			 
			 l - Run lvtool on all files that end in *.LBL on the volume.
			 t - Run testline.pl (checks line lengths)
			 r - Run checkref.pl (checks references in catalog files)
			 q - Run checkreq.pl (checks for required files)
	
	   lLabels - A specific list of labels to check, use this if not = []
		because label verification takes so long.
	"""
	
	log = logging.getLogger('runVolCheck')
	
	#check to see if sPDStools exists and contains lvtool
	if not os.path.exists(sPDStools):
		raise RuntimeError("PDS Perl Validation Tools directory %s doesn't exist"%(
		                   sPDStools))
								 
	if not os.path.exists(pjoin(sPDStools, "bin", "lvtool")):
		sTmp = "PDS Validation Tools directory %s doesn't contain the lvtool binary"%(
		       sPDStools)
		raise RuntimeError(sTmp)
	
	# Add PDStools binary directory to the path
	sToolsBin = pjoin(sPDStools, "bin")
	bFound = False
	for sDir in os.environ['PATH'].split(':'):
		if sTest == sDir:
			bFound = True
	
	if not bFound:
		sPrePath = os.environ['PATH']
		os.environ['PATH'] = "%s:%s"%(sToolsBin, sPrePath)
	
	#Remove old log files.
	if os.path.exists(sPVTLog):
		os.remove(sPVTLog)
	
	fLog = file(sPVTLog, 'wb')  #Python equivalent of 'touch'
	fLog.close()
	
	# Verify labels: (not simple)
	if "l" in sTest:
		fLog = file(sPVTLog, 'ab')
		fLog.write("\nPDS Tools lvtool log for: %s\n"%os.path.basename(sStageDir))
		fLog.write("===============================================================================\n")
		_writeLabelCheckMsg(fLog, sPDStools, sStageDir)
		setupLvtEnv()
		
		if lLabels == []:
			lLabels = util.find('*.LBL', sStageDir, util.FILES, util.NODIRS, util.CASESEN)
		log.info("Running lvtool on %d labels."%len(lLabels))
		nLabels = 0
		for sLabel in lLabels:
			checkLabel(log, fLog, sLabel, sStageDir, sPDStools)
			nLabels +=1
			if nLabels != 0 and (nLabels % 50) == 0:
				log.info("%5i labels checked..."%nLabels)
		
		teardownLvtEnvCleanStage(log, sStageDir)
		_writeLabelCheckMsg(fLog, sPDStools, sStageDir)
	
		fLog.close()
	
	
	#Test line args (must do this to avoid checking EXTRAS dir)
	if "t" in sTest:
		lArgs = ['AAREADME.LBL','AAREADME.TXT', 'AUTORUN.LBL','BROWSE',
		         'CATALOG','DATA','DOCUMENT','ERRATA.TXT','INDEX','LABEL',
		         'SHRUN.LBL','VOLDESC.CAT']
		
		
		fLog = file(sPVTLog, 'ab')
		fLog.write("PDS Tools testline.pl log for: %s\n"%os.path.basename(sStageDir))
		fLog.write("===============================================================================\n")
		fLog.close()
	
		log.info('Checking volume ASCII file using %s/testline.pl.'%sPDStools)
		for sArg in lArgs:
			if sArg.find('.') == -1:
				_runCmd(log, 'echo "Directory: %s" >> %s'%(sArg, sPVTLog))
		
			sCmd = "%s %s/bin/testline.pl %s/%s >> %s"%(sPerl, sPDStools, sStageDir, sArg,
			                                        sPVTLog)
			_runCmdWarnCheck(log, sCmd)
		
	
	# Check references
	if "r" in sTest:
		fLog = file(sPVTLog, 'ab')
		fLog.write("\nPDS Tools checkref.pl log for: %s\n"%os.path.basename(sStageDir))
		fLog.write("===============================================================================\n")
		fLog.close()
		log.info('Checking volume references using %s/checkref.pl.'%sPDStools)
		sCmd = "%s %s/bin/checkref.pl %s >> %s"%(sPerl, sPDStools, sStageDir, sPVTLog)
		_runCmdWarnCheck(log, sCmd)
	
	
	# Check for required files
	if "q" in sTest:
		fLog = file(sPVTLog, 'ab')
		fLog.write("\nPDS Tools checkreq.pl log for: %s\n"%os.path.basename(sStageDir))
		fLog.write("===============================================================================\n")
		fLog.close()
		log.info('Checking for required files using %s/checkreq.pl.'%sPDStools)
		sCmd = "%s %s/bin/checkreq.pl %s >> %s"%(sPerl, sPDStools, sStageDir, sPVTLog)
		_runCmdWarnCheck(log, sCmd)


##############################################################################

def vtoolCheck(log, sConfDir, sVolRoot, sPdsDict, lInc=['CATALOG']):
	"""Check a PDS volume using VTool
	
	log - a logger object
	
	sConfDir - the top level juno configuration directory, usually
	           /opt/project/juno/etc
				  
	sVolRoot - The root of the volume you wish to verify
	
	sPdsDict - The location of the PDS data dictionary you wish to use for
	       the checks
	
	lInc - The relative path to extra label include directories on the volume.
	       CATALOG is included by default, but you may wish to change this
	"""
	
	if len(lInc) > 0:
		sInc = " -I %s"%( ','.join( pjoin( sVolRoot, sDir) for sDir in lInc ) )
	else:
		sInc = ''
	
	(fd, sPath) = tempfile.mkstemp(suffix='-pds-trans')
	fErr = os.fdopen(fd)
	
	# Don't check catalog files for now
	sPtrn = '"*.LBL","*.CAT"'
	
	sCmd = 'VTool -d %s%s -e %s -t %s -s sum'%(sPdsDict, sInc, sPtrn, sVolRoot)
	
	log.info(sCmd)
		
	nRet = subprocess.call(sCmd, shell=True, stdout=fErr, stderr=fErr)
	if nRet != 0:
		log.error("Printing error messages from VTool...")
		fErr.seek(0)
		for sLine in fErr:
			sys.stderr.write(sLine)
	else:
		log.info("%s passed VTool checks"%sVolRoot)
		
	return nRet
