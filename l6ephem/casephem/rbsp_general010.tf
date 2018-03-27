KPL/FK

RBSP General Frame Definitions Kernel
==============================================================================

   This frame kernel contains general frames which are needed for the RBSP
   mission. These include spice frame implementations of the standard
   geophysical coordinate systems often used in space physics. These include
   the sytems that have been defined by Russel [1971], and expanded on by
   Hapgood [1992, 1995], and Franz and Harper [2002]. This kernel can
   replace and upgrade many of the coordinate transformation as given in the
   FORTRAN software GEOPACK. However, the tranformations defined here were
   constructed to fit within the established Spice paradigm and therefore
   are not consistent with GEOPACK. Details of these differences are given
   below.


Version and Date
---------------------------------------------------------------

   The TEXT_KERNEL_ID stores version information of loaded project text
   kernels.  Each entry associated with the keyword is a string that
   consists of four parts: the kernel name, version, entry date, and type.
   For example, the frame kernel might have an entry as follows:

      TEXT_KERNEL_ID += 'RBSP_GENERAL V0.1.0 20-JAN-2012 FK'
                                |        |       |        |
                                |        |       |        |
            KERNEL NAME <-------+        |       |        |
                                         |       |        V
                         VERSION <-------+       |   KERNEL TYPE
                                                 |
                                                 V
                                            ENTRY DATE

   RBSP General Frame Kernel Version:

      \begindata

      TEXT_KERNEL_ID += 'RBSP_GENERAL V0.1.0 20-JAN-2012 FK'

      \begintext

   Version 0.1.0 -- January 20, 2012 -- Grant Stephens

      Prototype release of the Magnetic Dipole based frames, MAG, GSM, and 
      SM.

   Version 0.0.0 -- October 12, 2011 -- Grant Stephens

      Initial prototype release. Contains GSE, GSE_MOD, GSE_TOD, MEAN_ECLIP,
      GEO, and GSE


References
---------------------------------------------------------------

      1.   "Frames Required Reading",
      	   at: 'http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/
	   frames.html#True Equator and Equinox of Date Frames'

      2.   "Geophysical Coordinate Transformations", Christopher T. Russel,
           at: http://www-ssc.igpp.ucla.edu/personnel/
           russell/papers/gct1.html/#s3.4

      3.   "Space Physics Coordinate Tranformations: A User Guide", M. A.
      	   Hapgood
	   at: http://www.sciencedirect.com/science/article/pii/003206339290012D

      4.   "Heliospheric Coordinate Systems", M. Franz and D. Harper
      	   at: http://www.sciencedirect.com/science/article/pii/S0032063301001192

      5.   GEOPACK and its documentation provided by Tsyganenko
      	   at: http://geo.phys.spbu.ru/~tsyganenko/modeling.html

      6.   "Reading STEREO Ephemerides as SPICE Kernels", William Thompson

      7.   pck00009.tpc comments contained within this kernel


Contact Information
---------------------------------------------------------------

   Direct questions, comments, or concerns about the contents of this kernel
   to:

      Grant Stephens, JHUAPL/SIS, (443)778-3584, Grant.Stephens@jhuapl.edu

      or

      Scott Turner, JHUAPL/SIS, (443)778-1693, Scott.Turner@jhuapl.edu


Implementation Notes
---------------------------------------------------------------

   This file is used by the SPICE system as follows: programs that make use
   of this frame kernel must "load" the kernel normally during program
   initialization.  Loading the kernel associates the data items with
   their names in a data structure called the "kernel pool".  The SPICELIB
   routine FURNSH loads a kernel into the pool as shown below:

      FORTRAN: (SPICELIB)

         CALL FURNSH ( frame_kernel_name )

      C: (CSPICE)

         furnsh_c ( frame_kernel_name );

      IDL: (ICY)

         cspice_furnsh, frame_kernel_name

      MATLAB: (MICY)

         cspice_furnsh( frame_kernel_name )

   In order for a program or routine to extract data from the pool, the
   SPICELIB routines GDPOOL, GIPOOL, and GCPOOL are used.  See [2] for
   more details.

   This file was created and may be updated with a text editor or word
   processor.


Frames
---------------------------------------------------------------

   The following frames are defined in this kernel file:

      Frame Name                Relative To              Type     NAIF ID
      =======================   ===================      =======  =======
      GEI                       J2000                    FIXED    -362900
      GEI_TOD                   J2000                    DYNAMIC  -362901
      GEI_MOD                   J2000                    DYNAMIC  -362902
      MEAN_ECLIP                J2000                    DYNAMIC  -362903
      GEO                       IAU_EARTH                FIXED    -362920
      GSE                       J2000                    DYNAMIC  -362930
      MAG                       J2000                    DYNAMIC  -362940
      GSM                       J2000                    DYNAMIC  -362945
      SM                        J2000                    DYNAMIC  -362950


      Note on frame integer ID codes:
      Many of the frames defined in this kernel are not specific to the RBSP
      mission, however, the frames defined here use the RBSPA ids, as these
      are guaranteed to not conflict with anyother spice integer id codes.

      Note, that two-vector frames are 'Relative To' J2000, despite J2000
      not being the base frame.


      GEI Frames:
      ---------------------------------------------------------

      Definition From [2]:

      The Geocentric Equatorial Inertial System (GEI) has its X-axis
      pointing from the Earth towards the first point of Aries (the position
      of the Sun at the vernal equinox). This direction is the intersection
      of the Earth's equatorial plane and the ecliptic plane and thus the
      X-axis lies in both planes. The Z-axis is parallel to the rotation
      axis of the Earth and Y completes the right-handed orthogonal set
      (Y = Z x X).

      As discussed in [4], the precession and nutation of the equinox
      complicates the implementation of this frame. Precession is the cyclic
      movement of the first point of Aries about the celestial sphere, which
      has a period of roughly 26,000 years. Superimposed on this is what is
      called the nutation of the equinox, which are higher order
      oscillations which cause deviations off of the mean. The terms
      'Mean-of-date' refers to frame or coordinate system where only
      precession is used. The term 'True-of-date' adds on the nutation and
      is the actual best representation for the first point of Aries at a
      given epoch.

      For a frame to be truly inertial, you must define the first point of
      Aries and the rotation axis for a specific epoch. The Spice built in
      ineretial frame J2000, is a utilization of this concept and is also
      disussed in [1,4]. GEI is therefore defined to be an alias for J2000,
      where this is implemented by using the identity matrix. 

      \begindata

      FRAME_GEI                   = -362900
      FRAME_-362900_NAME          = 'GEI'
      FRAME_-362900_CLASS         = 4
      FRAME_-362900_CLASS_ID      = -362900
      FRAME_-362900_CENTER        = -399
      TKFRAME_-362900_RELATIVE    = 'J2000'
      TKFRAME_-362900_SPEC        = 'MATRIX'
      TKFRAME_-362900_MATRIX      = ( 1.0
      				      0.0
				      0.0
				      0.0
				      1.0
				      0.0
				      0.0
				      0.0
				      1.0 )
      \begintext

      A true of date verion of GEI. This is basically an instaneous GEI for
      a given epoch, meaning that both precession and nutation of the
      equinox (discussed above) are accounted for.

      \begindata

      FRAME_GEI_TOD               =  -362911
      FRAME_-362911_NAME          =  'GEI_TOD'
      FRAME_-362911_CLASS         =  5
      FRAME_-362911_CLASS_ID      =  -362911
      FRAME_-362911_CENTER        =  399
      FRAME_-362911_RELATIVE      = 'J2000'
      FRAME_-362911_DEF_STYLE     = 'PARAMETERIZED'
      FRAME_-362911_FAMILY        = 'TRUE_EQUATOR_AND_EQUINOX_OF_DATE'
      FRAME_-362911_PREC_MODEL    = 'EARTH_IAU_1976'
      FRAME_-362911_NUT_MODEL     = 'EARTH_IAU_1980'
      FRAME_-362911_ROTATION_STATE= 'ROTATING'

      \begintext

      A mean of date version of GEI. This is the GEI for a given epoch,
      where only precession is accounted for.

      \begindata

      FRAME_GEI_MOD               =  -362912
      FRAME_-362912_NAME          =  'GEI_MOD'
      FRAME_-362912_CLASS         =  5
      FRAME_-362912_CLASS_ID      =  -362912
      FRAME_-362912_CENTER        =  399
      FRAME_-362912_RELATIVE      = 'J2000'
      FRAME_-362912_DEF_STYLE     = 'PARAMETERIZED'
      FRAME_-362912_FAMILY        = 'MEAN_EQUATOR_AND_EQUINOX_OF_DATE'
      FRAME_-362912_PREC_MODEL    = 'EARTH_IAU_1976'
      FRAME_-362912_ROTATION_STATE= 'ROTATING'

      \begintext

      From[1]:

      "Mean Ecliptic and Equinox of Date Frames are closely related to mean
      equator and equinox of date frames: for a given body, the former is
      obtained by rotating the latter about the X-axis by the mean obliquity
      of date.

      The term ``mean equator'' indicates that orientation of a body's
      equatorial plane is modeled accounting for precession. The
      ``mean equinox'' is the intersection of the body's mean orbital plane
      with the mean equatorial plane. The X-axis of such a frame is aligned
      with the cross product of the north-pointing vectors normal to the
      body's mean equator and mean orbital plane of date. The Z-axis is
      aligned with the second of these normal vectors. The Y axis is the
      cross product of the Z and X axes. The term ``of date'' means that
      these axes are evaluated at a specified epoch. "

      The Z axis of this frame is the pole of the mean ecliptic, and is used
      to define GSE.

      \begindata

      FRAME_MEAN_ECLIP               =  -362913
      FRAME_-362913_NAME             =  'MEAN_ECLIP'
      FRAME_-362913_CLASS            =  5
      FRAME_-362913_CLASS_ID         =  -362913
      FRAME_-362913_CENTER           =  399
      FRAME_-362913_RELATIVE         = 'J2000'
      FRAME_-362913_DEF_STYLE        = 'PARAMETERIZED'
      FRAME_-362913_FAMILY           = 'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE'
      FRAME_-362913_PREC_MODEL       = 'EARTH_IAU_1976'
      FRAME_-362913_OBLIQ_MODEL      = 'EARTH_IAU_1980'
      FRAME_-362913_ROTATION_STATE   = 'ROTATING'

      \begintext


      GEO Frame:
      ---------------------------------------------------------

      From [2]:

      The geographic coordinate system (GEO) is defined so that its X-axis
      is in the Earth's equatorial plane but is fixed with the rotation of
      the Earth so that it passes through the Greenwich meridian
      (0o longitude). Its Z-axis is parallel to the rotation axis of the
      Earth, and its Y-axis completes a right-handed orthogonal set
      (Y= Z x X).      

      Spice has several earth body fixed approaches already. The simplest is
      the IAU Earth model, whcih although it contains the following warning from [7]:
      	  NAIF strongly cautions against using the earth rotation model
    	  for work demanding high accuracy. This model has been
     	  determined by NAIF to have an error in the prime meridian location
    	  of magnitude at least 150 arcseconds, with a local minimum
     	  occurring during the year 1999.

      \begindata

      FRAME_GEO                   = -362920
      FRAME_-362920_NAME          = 'GEO'
      FRAME_-362920_CLASS         = 4
      FRAME_-362920_CLASS_ID      = -362920
      FRAME_-362920_CENTER        = 399
      TKFRAME_-362920_RELATIVE    = 'IAU_EARTH'
      TKFRAME_-362920_SPEC        = 'MATRIX'
      TKFRAME_-362920_MATRIX      = ( 1.0
      				      0.0
				      0.0
				      0.0
				      1.0
				      0.0
				      0.0
				      0.0
				      1.0 )
      \begintext


      GSE Frame:
      ---------------------------------------------------------

      Definition From [2]:

      "The geocentric solar ecliptic system (GSE) has its X-axis pointing
      from the Earth towards the Sun and its Y-axis is chosen to be in the
      ecliptic plane pointing towards dusk (thus opposing planetary motion).
      Its Z-axis is parallel to the ecliptic pole. Relative to an inertial
      system this system has a yearly rotation."

      His description defines three vectors each with their own bit of
      ambiguity.

      +X, the vector from the Earth to the sun. Russel describes a
      	  formulation (Fortran subroutine) which can be used to calulate
	  this vector, however, this is not consitent with the natural way
	  to handle this in Spice, which is to use a
	  observer-target-position vector. Spice's approach will give an
	  instaneous vector to the sun, rather than Geopack which will
	  provide a more 'average approach'. It was decided to use the
	  geometrical position of the sun, i.e. no light time correction.
	  Note, this differes from Geopack, where this vector is more of an
	  'mean' Earth-Sun vector.
      +Y, this vector opposes the earth's velocity. This can easily be
      	  achieved by spice using an observer-target-velocity vector.
	  However, doing this will violate the +Z axis definition, depending
	  on how the ecliptic is defined. The literature [4,6] has deemed
	  this as the lesser of the axis, and so is the +Z is used as the
	  primary, and this completes the frame.
      +Z, this vector is the pole of the ecliptic plane. This is ambiguous
      	  since the ecliptic planeis not inertial. As already described with
	  precession of the equinox, the ecliptic plane may be the true
	  ecliptic (instanteous ecliptic), the mean ecliptic of date, or the
	  ecliptic as it was at a certain epoch. Following the literature
	  [4, 6], the mean ecliptic of date was utilized.

      \begindata

      FRAME_GSE                    = -362930
      FRAME_-362930_NAME           = 'GSE'
      FRAME_-362930_CLASS          = 5
      FRAME_-362930_CLASS_ID       = -362930
      FRAME_-362930_CENTER         = 399
      FRAME_-362930_RELATIVE       = 'J2000'
      FRAME_-362930_DEF_STYLE      = 'PARAMETERIZED'
      FRAME_-362930_FAMILY         = 'TWO-VECTOR'
      FRAME_-362930_PRI_AXIS       = 'X'
      FRAME_-362930_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
      FRAME_-362930_PRI_OBSERVER   = 'EARTH'
      FRAME_-362930_PRI_TARGET     = 'SUN'
      FRAME_-362930_PRI_ABCORR     = 'NONE'
      FRAME_-362930_SEC_AXIS       = 'Z'
      FRAME_-362930_SEC_VECTOR_DEF = 'CONSTANT'
      FRAME_-362930_SEC_SPEC       = 'RECTANGULAR'
      FRAME_-362930_SEC_FRAME      = 'MEAN_ECLIP'
      FRAME_-362930_SEC_VECTOR     = (0, 0, 1)

      \begintext


      MAG Frame:
      ---------------------------------------------------------

      Definition From [2]:

      "The geomagnetic coordinate system (MAG) is defined so that its Z-axis
      is parallel to the magnetic dipole axis. The geographic coordinates of
      the dipole axis from the International Geomagnetic Reference Field...
      The Y-axis of this system is perpendicular to the geographic poles
      such that if D is the dipole position and S is the south pole Y=DxS.
      Finally, the X-axis completes a right-handed orthogonal set."

      The implementation of this frame is complicated in that the definition
      of the IGRF dipole is a function of time and the IGRF model cannot be
      directly incorporated into Spice. However, Spice does allow to define
      time dependent Euler angles. Meaning, you can define an Euler angle
      that rotates GEO to MAG for a given ephemeris time t:

         V           = r(t) * V
          GEI                  MAG
      
      where r(t) is a time dependent Euler angle representation of a
      rotation. Spice allows for the time dependence to be represented by a
      polynomial expansion. This expansion can be fit using the IGRF model,
      thus representing the IGRF dipole axis.

      IGRF-11 (the 11th version) was fit for the period of 1990-2020, which
      should encompass the mission and will also make this kernel useful for
      performing Magnetic dipole frame transformations for the 1990's and
      the 2000s. However, IGRF-11 is not as accurate for this entire time
      interval. The years between 1945-2005 are labled definitive, although
      only back to 1990 was used in the polynomial fit. 2005-2010 is
      provisional, and may change with IGRF-12. 2010-2015 was only a
      prediction. Beyond 2015, the predict is so far in the future as to not
      be valid. So to make the polynomials behave nicely in this region, the
      2015 predicition was extended until 2020. So for low precision, this
      kernel can be used for the years 2015-2020. Any times less than 1990
      and greater than 2020 were not used in the fit, and therefore may be
      vastly incorrect. These coefficients will be refit when IGRF-12 is
      released.
      
      Also, since the rest of the magnetic dipole frames are defined from
      this one, similar time ranges should be used.

                  Definitive           Provisional   Predict    Not Valid
       |------------------------------|+++++++++++|###########|???????????|
     1990                           2005        2010        2015        2020

      In addition to the error inherit in the model itself, the polynomial
      expansion cannot perfectly capture the IGRF dipole. The maximum error
      on the fit is .2 milliradians, or .01 degrees. 

      The MAG frame is achieved by first rotating the GEO frame by the Z by
      the longitude degrees, and then rotating about the Y axis by the
      amound off latitude. This lines matches the new frame to Russell's
      definition.

      \begindata

      FRAME_MAG                    = -362940
      FRAME_-362940_NAME           = 'MAG'
      FRAME_-362940_CLASS          = 5
      FRAME_-362940_CLASS_ID       = -362940
      FRAME_-362940_CENTER         = 399
      FRAME_-362940_RELATIVE       = 'GEO'
      FRAME_-362940_DEF_STYLE      = 'PARAMETERIZED'
      FRAME_-362940_FAMILY         = 'EULER'
      FRAME_-362940_EPOCH          = @2010-JAN-1/00:00:00
      FRAME_-362940_AXES           = ( 3,  2,  1 )
      FRAME_-362940_UNITS          = 'DEGREES'
      FRAME_-362940_ANGLE_1_COEFFS = ( +72.19592169505606
                                        +2.6506950233619764E-9
                                        +1.6897777301495875E-18
                                        -3.725022474684048E-27
                                        -6.395891803742159E-36 )
      FRAME_-362940_ANGLE_2_COEFFS = (  -9.98363089063021
                                        +1.7304386827492741E-9 
                                        +5.686537610447754E-19 
                                        -5.208835662700353E-28
                                        -9.569975244363123E-37 )
      FRAME_-362940_ANGLE_3_COEFFS = ( 0 )

      \begintext


      GSM Frame:
      ---------------------------------------------------------

      Definition From [2]:

      "The geocentric solar magnetospheric system (GSM), as with both the
      GSE and GSEQ systems, has its X-axis from the Earth to the Sun. The
      Y-axis is defined to be perpendicular to the Earth's magnetic dipole
      so that the X-Z plane contains the dipole axis. The positive Z- axis
      is chosen to be in the same sense as the northern magnetic pole. The
      difference between the GSM system and the GSE and GSEQ is simply a
      rotation about the X-axis."

      Thus, +X is identical as GSE +X and is the primary, and +Z is the
      secondary and is the MAG +Z.

      \begindata

      FRAME_GSM                    =  -362945
      FRAME_-362945_NAME           = 'GSM'
      FRAME_-362945_CLASS          = 5
      FRAME_-362945_CLASS_ID       = -362945
      FRAME_-362945_CENTER         = 399
      FRAME_-362945_RELATIVE       = 'J2000'
      FRAME_-362945_DEF_STYLE      = 'PARAMETERIZED'
      FRAME_-362945_FAMILY         = 'TWO-VECTOR'
      FRAME_-362945_PRI_AXIS       = 'X'
      FRAME_-362945_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
      FRAME_-362945_PRI_OBSERVER   = 'EARTH'
      FRAME_-362945_PRI_TARGET     = 'SUN'
      FRAME_-362945_PRI_ABCORR     = 'NONE'
      FRAME_-362945_SEC_AXIS       = 'Z'
      FRAME_-362945_SEC_VECTOR_DEF = 'CONSTANT'
      FRAME_-362945_SEC_SPEC       = 'RECTANGULAR'
      FRAME_-362945_SEC_FRAME      = 'MAG'
      FRAME_-362945_SEC_VECTOR     = (0, 0, 1)

      \begintext


      SM Frame:
      ---------------------------------------------------------

      Definition From [2]:

      "In solar magnetic coordinates (SM) the Z-axis is chosen parallel to
      the north magnetic pole and the Y-axis perpendicular to the Earth-Sun
      line towards dusk. The difference between this system and the GSM
      system is a rotation about the Y-axis. The amount of rotation is
      simply the dipole tilt angle as defined in the previous section. We
      note that in this system the X-axis does not point directly at the
      Sun. As with the GSM system, the SM system rotates with both a yearly
      and daily period with respect to inertial coordinates."

      Thus, this is much like GSM, except that now the +Z axis is the
      primary, meaning it is parallel to the dipole vector, and +X is the
      secondary. Since the X-Z plane is the same as GSM's X-Z plane, the Y
      axis is the same as GSM.

      \begindata

      FRAME_SM                     =  -362950
      FRAME_-362950_NAME           = 'SM'
      FRAME_-362950_CLASS          = 5
      FRAME_-362950_CLASS_ID       = -362950
      FRAME_-362950_CENTER         = 399
      FRAME_-362950_RELATIVE       = 'J2000'
      FRAME_-362950_DEF_STYLE      = 'PARAMETERIZED'
      FRAME_-362950_FAMILY         = 'TWO-VECTOR'
      FRAME_-362950_PRI_AXIS       = 'Z'
      FRAME_-362950_PRI_VECTOR_DEF = 'CONSTANT'
      FRAME_-362950_PRI_SPEC       = 'RECTANGULAR'
      FRAME_-362950_PRI_FRAME      = 'MAG'
      FRAME_-362950_PRI_VECTOR     = (0, 0, 1)     
      FRAME_-362950_SEC_AXIS       = 'X'
      FRAME_-362950_SEC_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
      FRAME_-362950_SEC_OBSERVER   = 'EARTH'
      FRAME_-362950_SEC_TARGET     = 'SUN'
      FRAME_-362950_SEC_ABCORR     = 'NONE'

      \begintext