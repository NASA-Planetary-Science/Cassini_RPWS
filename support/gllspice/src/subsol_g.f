C$Procedure      SUBSOL_G ( Sub-solar point )
 
      SUBROUTINE SUBSOL_G ( TARGET, OBSRVR, ET, CORR, SSOLPT )
 
C$ Abstract
C
C     Find the sub-solar point on a target body, as seen from a
C     specified body.
C
C$ Required_Reading
C
C     KERNEL
C     SPK
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     GLLSPICE
C
C$ Declarations
 
      INTEGER               TARGET
      INTEGER               OBSRVR
      DOUBLE PRECISION      ET
      CHARACTER*(*)         CORR
      DOUBLE PRECISION      SSOLPT ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARGET     I   NAIF integer code of target body.
C     OBSRVR     I   NAIF integer code of observing body.
C     ET         I   Epoch in ephemeris seconds past J2000.
C     CORR       I   Desired aberration correction.
C     SSOLPT     O   Sub-solar point on target body.
C
C$ Detailed_Input
C
C     TARGET         is the NAIF integer code of the target body.
C
C     OBSRVR         is the NAIF integer code of the observing body.
C                    OBSRVR may be identical to TARGET.
C
C     ET             is the epoch in ephemeris seconds past J2000 for
C                    which the sub-solar point on the target body is to
C                    be computed.
C
C     CORR           is the aberration correction to be used in
C                    computing the location and orientation of the
C                    target body and the location of the Sun.  Possible
C                    values are:
C
C                       'NONE'        No aberration correction.
C
C                       'LT'          Correct the position and
C                                     orientation of target body for
C                                     light time, and correct the
C                                     position of the Sun for light
C                                     time.
C
C                       'LT+S'        Correct the observer-target vector
C                                     for light time and stellar
C                                     aberration, correct the
C                                     orientation of the target body
C                                     for light time, and correct the
C                                     target-Sun vector for light time
C                                     and stellar aberration.
C
C$ Detailed_Output
C
C
C     SSOLPT         is the location, in body-fixed (body equator and
C                    prime meridian) cartesian coordinates, of the
C                    sub-solar point on the target body.  The sub-solar
C                    point is the closest point on the target body to
C                    the Sun.  Note that this point, except in the case
C                    of a spherical target body, is generally not the
C                    surface intercept point of the Sun-target body
C                    center ray.
C
C                    The aberration correction specified by the
C                    input argument CORR is used in computing the
C                    position and orientation of the target body, and
C                    in computing the target-Sun vector.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If no SPK (ephemeris) data is available for the observer,
C         target, and Sun at the time specified by ET, the error will
C         be diagnosed by routines called by this routine.  If light
C         time corrections are used, SPK data for the target body must
C         be available at the time ET - LT, where LT is the one-way
C         light time from the target to the observer at ET.
C         Additionally, SPK data must be available for the Sun at the
C         time ET - LT - LT', where LT' is the light time from the Sun
C         to the target body at time ET - LT.
C
C     2)  If PCK data defining the orientation or shape of the target
C         body is unavailable, the error will be diagnosed by routines
C         called by this routine.
C
C$ Files
C
C     No files are input to this routine.  However, SUBSOL_G expects
C     that the appropriate SPK and PCK files have been loaded using
C     SPKLEF and LDPOOL, respectively.
C
C$ Particulars
C
C     An SPK file containing ephemeris data covering TARGET and OBSRVR
C     at time ET must be loaded via SPKLEF before calling SSOLPT_G.
C     Additionally, if light time correction is used, the SPK file must
C     cover TARGET at time ET - LT, where LT is the light time between
C     the target and observer at ET, and the Sun at time ET - LT - LT',
C     where LT' is the light time from the Sun to the target body at
C     the time ET - LT.
C
C     This routine assumes that the target body is modelled as a
C     triaxial ellipsoid, and that a PCK file containing its radii and
C     rotation model constants has been loaded into the kernel pool
C     using the subroutine LDPOOL.
C
C     Aberration corrections are applied in this routine as follows:
C
C        'NONE'   All states are geometric.  The orientation of the
C                 target body at the input epoch ET is used.
C
C        'LT'     The position and orientation of the target body at
C                 epoch ET - LT are used, where LT is the light time
C                 from the target body to the observer body at time ET.
C                 Let LT' be the light time from the target body to the
C                 Sun at epoch ET - LT; the position of the Sun at
C                 epoch ET - LT - LT' is used.
C
C        'LT+S'   In computing the body-fixed observer-target vector,
C                 the position and orientation of the target body at
C                 epoch ET - LT are used, where LT is the light time
C                 from the target body to the observer body at time ET.
C                 The observer-target vector is also corrected for
C                 stellar aberration.
C
C                 Let LT' be the light time from the target body to the
C                 Sun at epoch ET - LT; the target-Sun vector is
C                 computed using the position of the target at time
C                 ET-LT and the position of the Sun at epoch
C                 ET - LT - LT'.  The position of the Sun is also
C                 corrected for stellar aberration.
C
C
C     This routine computes the location of the closest point on the
C     target body to the Sun.  This surface normal at this point points
C     directly toward the Sun.  When light time and stellar aberration
C     corrections are used, the point found by this routine has the
C     property that there are no shadows cast by the Sun there at epoch
C     ET - LT.  Therefore, this is the `apparent' sub-solar point as
C     seen in an optical image shuttered at time ET.
C
C     Unless the target body is spherical,  this point is generally NOT
C     the surface intercept point of the ray from the Sun to the target
C     body's center.  Therefore, the results found by this routine will
C     differ slightly from those shown in the Astronomical Almanac, for
C     most bodies.  Additionally, the results found by this routine will
C     vary depending on the target body radii and orientation model
C     present in the PCK kernel used with this routine.
C
C
C$ Examples
C
C     1)  Find the sub-solar point on Venus on Feb. 6, 1990 (UTC).  Use
C         light time corrections.
C
C
C                  PROGRAM TESTSUBSOL
C
C            C
C            C     SPICELIB functions
C            C
C                  DOUBLE PRECISION      DPR
C                  DOUBLE PRECISION      TWOPI
C
C            C
C            C     Local parameters
C            C
C
C            C
C            C     The NAIF integer ID code for Venus is 299.
C            C
C                  INTEGER               VENUS
C                  PARAMETER           ( VENUS = 299 )
C
C            C
C            C     Local variables
C            C
C                  INTEGER               HANDLE
C
C                  DOUBLE PRECISION      ET
C                  DOUBLE PRECISION      SSOLPT ( 3 )
C                  DOUBLE PRECISION      LAT
C                  DOUBLE PRECISION      LON
C                  DOUBLE PRECISION      R
C
C            C
C            C     Load an SPK kernel for state information, a
C            C     leapseconds kernel for time conversion, and a
C            C     PCK kernel for Venus radii.  In your own programs,
C            C     you should use names of actual kernel files, not the
C            C     names shown here.
C            C
C                  CALL SPKLEF ( 'VENUS.BSP', HANDLE )
C                  CALL LDPOOL ( 'LEAP.TLS'          )
C                  CALL LDPOOL ( 'PCK.TPC'           )
C
C            C
C            C     The difference between UTC and ephemeris time does
C            C     not make a significant difference in this
C            C     computation, as we'll see when we compare our result
C            C     to that of the Almanac.  The data in the Almanac is
C            C     given for Feb. 6, 1990 TDB, not UTC.
C            C
C            C     Compute the ephemeris time corresponding to
C            C     Feb. 6, 1990 UTC.
C            C
C                  CALL UTC2ET ( 'FEB 6 1990', ET )
C
C            C
C            C     Find the light time corrected sub-solar point on
C            C     Venus at ET, as seen on Venus.
C            C
C                  CALL SUBSOL_G ( VENUS, VENUS, ET, 'NONE', SSOLPT )
C
C            C
C            C     Convert the result to planetocentric latitude and
C            C     longitude.  Since the Almanac uses a range of
C            C
C            C        [ 0, 360 )
C            C
C            C     for longitudes, rather than the range
C            C
C            C        ( -pi, pi ]
C            C
C            C     used by the SPICELIB subroutine RECLAT, we must
C            C     adjust the longitude.
C            C
C                  CALL RECLAT ( SSOLPT, R, LON, LAT )
C
C                  IF ( LON .LT. 0 ) THEN
C                     LON = LON + TWOPI()
C                  END IF
C
C            C
C            C     Convert the angles to degrees.
C            C
C                  LON = DPR() * LON
C                  LAT = DPR() * LAT
C
C            C
C            C     Write out the results.
C            C
C                  WRITE (*,*) 'Location of sub-solar point on Venus'
C                  WRITE (*,*) 'Epoch:  February 6, 1990 (UTC)'
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Longitude: ', LON
C                  WRITE (*,*) 'Latitude:  ', LAT
C                  WRITE (*,*) ' '
C
C                  END
C
C
C     2)  Find the planetocentric latitude and longitude of the subsolar
C         point on Earth, as seen from the Earth, at the epoch
C         specified by ET.  Use light time corrections.  (The NAIF
C         integer code for Earth is 399.)
C
C            CALL SUBSOL_G ( 399,     399,  ET,  'LT',  SSOLPT )
C            CALL RECLAT   ( SSOLPT,  RAD,  LON,  LAT          )
C
C         LON and LAT are returned in radians.  To convert to degrees,
C         you can use the SPICELIB function DPR, as in the code fragment
C
C            LON = DPR() * LON
C            LAT = DPR() * LAT
C
C
C$ Restrictions
C
C     1)  An SPK file containing ephemeris data for the observer and
C         target bodies and a PCK file containing and shape and
C         orientation constants for the target body must be loaded
C         prior to calling this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    GLLSPICE Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 1.0.0, 04-DEC-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     sub-solar point
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
 
C
C     SUN is the NAIF integer ID code of the Sun.
C
      INTEGER               SUN
      PARAMETER           ( SUN = 10 )
 
C
C     Local variables
C
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTSUN
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SSTATE ( 6 )
      DOUBLE PRECISION      SUNPOS ( 3 )
      DOUBLE PRECISION      SUNTIM
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TSTATE ( 6 )
 
      INTEGER               N
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SUBSOL_G' )
      END IF
 
C
C     If the observer and target are not identical and we are using
C     any aberration correction other than 'NONE', we will need
C     to find the light time from the target to the observer.
C
      IF (  ( TARGET .NE. OBSRVR ) .AND. ( CORR .NE. 'NONE' )  ) THEN
C
C        Find the state of the target as seen from the observer at ET.
C
         CALL SPKEZ ( TARGET, ET, 'J2000', CORR, OBSRVR, TSTATE, LT )
 
C
C        Find the state of the Sun as seen from the target at ET - LT.
C
         SUNTIM = ET - LT
 
      ELSE
C
C        The observer and target bodies are identical, or we're not
C        using aberration corrections.
C
         SUNTIM = ET
 
      END IF
 
      CALL SPKEZ ( SUN,  SUNTIM, 'J2000', CORR, TARGET, SSTATE, LTSUN )
 
C
C     Get the transformation from inertial to body-fixed coordinates
C     for the target body.  (TIPM stands for `transformation from
C     inertial to prime meridian'.)  Use light time corrections if
C     necessary.
C
      CALL BODMAT ( TARGET, SUNTIM, TIPM )
 
C
C     Transform the target-Sun vector to body-fixed coordinates.
C
      CALL MXV ( TIPM, SSTATE, SUNPOS )
 
C
C     Find the radii of the target body.
C
      CALL BODVAR ( TARGET, 'RADII', N, RADII )
 
C
C     Find the closest point on the target body to the Sun, using
C     the specified aberration correction.
C
      CALL NEARPT ( SUNPOS, RADII(1), RADII(2), RADII(3), SSOLPT, ALT )
 
      CALL CHKOUT ( 'SUBSOL_G' )
      RETURN
      END
