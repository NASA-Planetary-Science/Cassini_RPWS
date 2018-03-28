# Cassini_RPWS_core
Cassini RPWS data processing code, both working applications and reference archives

This repository comprises the core of the Cassini Radio and Plasma Wave Science (RPWS) team routine production software.
The software was initially developed on Sun/SPARC SunOS 2.5 and was updated to Oracle/SPARC Solaris 10 through
the life of the project.
Some applications were also ported to be platform independent and will run on X86_64/Linux platforms.
Rough installation instructions follow:

## Environment

The following environment variables must be set:

- **SETUP_FILE**  The location of your bash 'source'-able setup file.  This is  used by some make files to generate a wrapper call script for das2 readers and other programs that may not be invoked interactively.  
    Production Value:  /opt/project/cassini/etc/setup_d/solaris10.sh
				
- **PREFIX**      The top level install directory.  
   Production Value:   /opt/project/cassini
				
- **N_ARCH**      A name for the host computer architecture.  
   Production Value:   solaris10
			  
- **INST_ETC**    The top level settings directory.  
   Production Value:   /opt/project/cassini/etc

- **PATH**  The following needs to be added to the front of your path:
     \$PREFIX/bin/\$N_ARCH

- **LD_LIBRARY_PATH**  There are many odd versions of software on the suns.  Set this to only pick up the stuff needed by the cassini software.  
   Production Value:  \$PREFIX/lib/\$N_ARCH

- **RPWS_DATA**   Location for RPWS raw data ('R' and 'U' files)  
   Production Value:   /opt/project/cassini/data

- **MAG_DATA**    Location for MAG data.  
    Production Value:   /opt/project/cassini/mag

- **RPWS_MPDB**   The name of the file which will contain an index of raw files ('U' files).  
   Production Value:   /opt/project/cassini/data/database/CassiniJPL.db

- **RPWS_TEMP**   The name of the directory for temporary files.  
   Production Value:   /opt/project/cassini/scratch
				
- **RPWS_SUPERVOLUME**  The location of PDS formated data.  
    Production Value:   /opt/project/cassini/pds
				
- **CAS_TIME_KERNELS**  NAIF Spice SCLK-SCET time correlation metakernel.  
     Production Value:  \$PREFIX/spice/kernels/cas_kernels.txt

- **CAS_EPHEMERIS_KERNELS**  NAIF Spice spacecraft position metakernel.  
     Production Value:  \$PREFIX/spice/kernels/CassiniKernels.list

- **CAS_QUAT_DB** Location of the file which will contain the index of quaternion files.  
   Production Value: /opt/project/cassini/data/database/Quaternions.db

- **CSPICE_INC**  The include directory for the CSpice library.  
   Production Value:  /local/naif/cspice/include

- **CSPICE_LIB**  The path to the actual cspice.a static library.  
    Production Value:  /local/naif/cspice/lib/cspice.a

- **IDL_BIN**     The location of the IDL interpreter to use.  
     Production value:   /local/itt/idl/idl81/bin/idl

- **IDL_PATH**    The path for finding IDL \*.pro files and shared objects.  
   Production value:  "\$PREFIX/lib/idl8.1:\$PREFIX/lib/\$N_ARCH/idl8.1:<IDL_DEFAULT>"

- **PYTHONPATH**  Include the pure and extension cassini python libraries location.  
    Production value: \$PREFIX/lib/python2.7:\$PREFIX/lib/solaris10/python2.7

- **CLASSPATH**   Include the java lib location.  
    Production Value:  $PREFIX/lib/java1.8


## Prerequisites

The RPWS Core software depends on these custom libraries and tools which should first be installed using the
same environment variables stated above.

  - **PDS tools** - Tools for PDS label parsing and volume checking.  (optional)  
    https://pds.jpl.nasa.gov/tools/pds-tools-package.shtml 
 
  - **Giferator** - Larry's das1 gifferator.  
    git clone https://github.com/das-developers/das1-giferator.git

  - **libdas2** - The Das2 server side C-library.  
	 git clone https://github.com/das-developers/libdas2.git

  - **pspice** - Python spice wrappers used by the PDS volume creation software.  
    git clone https://github.com/das-developers/pspice.git

## Build

Build the Iowa Cassini data handling software in this order:

1. **Build the support libraries**  
    $ cd support  
    $ gmake  
    $ gmake install  
2. **Build the Ancillary information libs** (Solaris or Linux)  
    $ cd l6ephem  
    $ gmake  
    $ gmake test  
    $ gmake install  
3. **Build the low level data handling libs and programs** (Solaris only)  
    $ cd l2rpws  
    $ gmake  
    $ gmake test  
    $ gmake install  
4. **Build the calibrated-level handling libs and programs**  (Solaris or Linux)  
    $ cd l3rpws/lowrate  
    $ gmake  
    $ gmake test  
    $ gmake install  

    $ cd l3rpws/highrate  
    $ gmake  
    $ gmake test  
    $ gmake install  

    $ cd l3rpws/langmuirprobe  
    $ gmake  
    $ gmake test  
    $ gmake install  
5. **Build the MAG data handling libs and programs**  (Solaris or Linux)  
    $ cd l3mag  
    $ gmake  
    $ gmake test  
    $ gmake install  
6. **Build the re-mapped data level handling libs and programs**  (Solaris or Linux)  
    $ cd l4rpws/keyparam  
    $ gmake  
    $ gmake test  
    $ gmake install  
    
    $ cd l4rpws/std_archive  
    $ gmake  
    $ gmake test  
    $ gmake install  
