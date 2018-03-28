# Cassini_RPWS_core Installation
Cassini RPWS data processing code, both working applications and reference archives

This software has been tested on Solaris 10.  Much of the code under the l2rpws directory depends on Big-Endian (Most Significant Byte First) byte order and 32-bit pointer lengths to operate correctly, with the exception of l2rpws/src/castlm which is portable.  To install the software:


Setup your environment
----------------------
The following environment variables must be set:

- **SETUP_FILE**  The location of your bash 'source'-able setup file.  This is  used by some make files to generate a wrapper call script for das2 readers and other programs that may not be invoked interactively.
    Production Value:  /opt/project/cassini/etc/setup_d/solaris10.sh
				
- **PREFIX**      The top level install directory.  
   Production Value:   /opt/project/cassini
				
- **N_ARCH**      A name for the host computer architecture
   Production Value:   solaris10
			  
- **INST_ETC**    The top level settings directory. 
   Production Value:   /opt/project/cassini/etc

- **PATH**  The following need to be added to the front of your path:
     \$PREFIX/bin/\$N_ARCH

- **LD_LIBRARY_PATH**  There are many odd version of software on the suns.  Set this to only pick up the stuff needed by the cassini software.
   Production Value:  \$PREFIX/lib/\$N_ARCH

- **RPWS_DATA**   Location for RPWS raw data ('R' and 'U' files)
   Production Value:   /opt/project/cassini/data

- **MAG_DATA**    Location for MAG data
    Production Value:   /opt/project/cassini/mag

- **RPWS_MPDB**   The name of the file which will contain an index of raw files ('U' files) 
   Production Value:   /opt/project/cassini/data/database/CassiniJPL.db

- **RPWS_TEMP**   The name of the directory for temporary files.
   Production Value:   /opt/project/cassini/scratch
				
- **RPWS_SUPERVOLUME**  The location of PDS formated data 
    Production Value:   /opt/project/cassini/pds
				
- **CAS_TIME_KERNELS**  NAIF Spice SCLK-SCET time correlation metakernel
     Production Value:  \$PREFIX/spice/kernels/cas_kernels.txt

- **CAS_EPHEMERIS_KERNELS**  NAIF Spice spacecraft position metakernel
     Production Value:  \$PREFIX/spice/kernels/CassiniKernels.list

- **CAS_QUAT_DB** Location of the file which will contain the index of quaternion files. 
   Production Value: /opt/project/cassini/data/database/Quaternions.db

- **CSPICE_INC**  The include directory for the CSpice library
   Production Value:  /local/naif/cspice/include

- **CSPICE_LIB**  The path to the actual cspice.a binary
    Production Value:  /local/naif/cspice/lib/cspice.a

- **IDL_BIN**     The location of the IDL interpreter to use
     Production value:   /local/itt/idl/idl81/bin/idl

- **IDL_PATH**    The path for finding IDL *.pro files and shared objects
   Production value:  "\$PREFIX/lib/idl8.1:\$PREFIX/lib/\$N_ARCH/idl8.1:<IDL_DEFAULT>"

- **PYTHONPATH**  Include the pure and extension cassini python libraries location
    Production value: \$PREFIX/lib/python2.7:\$PREFIX/lib/solaris10/python2.7

- **CLASSPATH**   Include the java lib location
    Production Value:  $PREFIX/lib/java1.8


Install the prerequisites
-------------------------
The RPWS Core software depends on eight custom libraries and tools:

  - **liddas2** - The Das2 server side C-library
	 svn co https://saturn.physics.uiowa.edu/svn/das2/core/stable/libdas2


  - **libfg** - Willy's command line parsing library,
	 svn co https://saturn.physics.uiowa.edu/svn/util/C/trunk/libfg

  - **gllspice**  Extra Spice transformations library used by Joe's programs:
     svn co https://saturn.physics.uiowa.edu/svn/util/fortran/trunk/gllspice  

  - **pspice** - Python spice wrappers used by the PDS volume creation software:
      svn co https://saturn.physics.uiowa.edu/svn/util/python/trunk/pspice
	     
  - ** libwtrplot** - Willy's plotting library.  This dependency will disappear in the future.  Only archive needs it and that code is going to the PDS area.
	 svn co https://saturn.physics.uiowa.edu/svn/util/C/trunk/libwtrplot

  - **fftpack** - An FFT library someone downloaded from somewhere
   svn co https://saturn.physics.uiowa.edu/svn/util/fortran/trunk/fftpack

  - **rpwPDS** - Generic PDS3 tools, used by the PDS volume creation software:
     svn co https://saturn.physics.uiowa.edu/svn/util/python/trunk/rpwPDS
	 
  - **Giferator** - Larry's Das1 gifferator:
     svn co https://saturn.physics.uiowa.edu/svn/util/IDL/devel/giferator
    
  - **pdspad** - Robert's PDS label fixup tool:
     svn co https://saturn.physics.uiowa.edu/svn/util/C/trunk/pdspad
  
Do an install of these libraries *USING THE SAME PREFIX* setting as you will use when making the Cassini GSE tools.  When you're done, come back to this directory and continue the build/install.


Build the Iowa Cassini data handling software in this order:


1.  Build the Ancillary information libs  (builds on linux)
    ----------------------------------------------------------
    $ cd l6ephem
    $ gmake && gmake test && gmake install


2. Build the low level data handling libs and programs  (solaris only)
    ----------------------------------------------------------------------
    $ cd l2rpws
    $ gmake && gmake test && gmake install


3. Build the calibrated-level handling libs and programs  (builds on linux)
    ---------------------------------------------------------------------------
    $ cd l3rpws/lowrate
    $ gmake && gmake test && gmake install
    
    $ cd l3rpws/highrate
    $ gmake && gmake test && gmake install
    
    $ cd l3rpws/langmuirprobe
    $ gmake && gmake test && gmake install


4. Build the MAG data handling programs and libs  (builds on linux)
    -------------------------------------------------------------------
    $ cd l3mag
    $ gmake && gmake test && gmake install


5. Build the re-mapped data level handling libs and programs  (builds on linux)
    -------------------------------------------------------------------------------
    $ cd l4rpws/keyparam
    $ gmake && gmake test && gmake install
    
    $ cd l4rpws/std_archive
    $ gmake && gmake test && gmake install
      ( This program also requres the pds_tools library, last know download location is:      https://pds.jpl.nasa.gov/tools/pds-tools-package.shtml )


