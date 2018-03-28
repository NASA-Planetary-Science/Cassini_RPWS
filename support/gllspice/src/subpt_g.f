C$Procedure      SUBPT_G ( Sub-observer point )
 
      SUBROUTINE SUBPT_G ( TARGET, OBSRVR, ET, ABCORR, SPOINT, ALT )
 
C$ Abstract
C
C     Determine the coordinates of the sub-observer point on a target
C     body at a particular epoch, optionally corrected for planetary
C     (light time) and stellar aberration.  Also, return the observer's
C     altitude above the target body.
C
C$ Required_Reading
C
C     KERNEL
C     SPK
C
C$ Keywords
C
C     GEOMETRY
C
C$ Declarations
 
      INTEGER               TARGET
      INTEGER               OBSRVR
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR
      DOUBLE PRECISION      SPOINT   ( 3 )
      DOUBLE PRECISION      ALT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARGET     I   NAIF integer code of target body.
C     OBSRVR     I   NAIF integer code of observing body.
C     ET         I   Epoch in ephemeris seconds past J2000.
C     ABCORR     I   Aberration correction.
C     SPOINT     O   Sub-observer point on the target body.
C     ALT        O   Altitude of the observer above the target body.
C
C$ Detailed_Input
C
C     TARGET      is the NAIF integer code of the target body.  This
C                 routine assumes that this body is modelled by a tri-
C                 axial ellipsoid, and that a PCK file containing its
C                 radii and rotation model constants have been loaded
C                 into the kernel pool using the subroutine LDPOOL.
C
C     OBSRVR      is the NAIF integer code of the observing body,
C                 typically a spacecraft.
C
C     ET          is the epoch in ephemeris seconds past J2000 at which
C                 the sub-observer point on the target body is to be
C                 computed.
C
C                 Note that SPK ephemeris file(s) covering these bodies
C                 and this epoch must be loaded using SPKLEF before
C                 calling SUBPT_G.
C
C     ABCORR      indicates the aberration corrections to be applied
C                 when computing the observer-target state.  ABCORR
C                 may be any of the following.
C
C                    'NONE'     Apply no correction.
C
C                    'LT'       Correct for planetary (light time)
C                               aberration.
C
C                    'LT+S'     Correct for planetary (light time)
C                               and stellar aberrations.
C
C$ Detailed_Output
C
C     SPOINT      is the sub-observer point on the target body at ET
C                 in rectangular body-fixed coordinates.
C
C                 The sub-observer point is defined to be the point
C                 on the target body that is closest to the observer.
C                 However, the state of the target body relative to the
C                 observer differs depending on the aberration
C                 corrections requested.
C
C     ALT         is the altitude of the observer above the target
C                 body.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If OBSRVR is equal to TARGET, the error
C        SPICE(BODIESNOTDISTINCT) is signalled.
C
C     2) If no SPK ephemeris file has been loaded prior to calling
C        SUBPT_G, or if the SPK data has insufficient coverage, an
C        error will be diagnosed and signalled by a routine that
C        SUBPT_G calls.
C
C     3) If a PCK file containing the radii and rotation model constants
C        of the target body has not been loaded prior to calling
C        SUBPT_G, an error will be diagnosed and signalled by a
C        routine that SUBPT_G calls.
C
C$ Files
C
C     No files are input to this routine, however, SUBPT_G expects
C     that the appropriate SPK and PCK files have been loaded using
C     SPKLEF and LDPOOL, respectively.
C
C$ Particulars
C
C     SUBPT_G computes the sub-observer point on a target body.
C     (The sub-observer point is commonly called the sub-spacecraft
C     point when the observer is a spacecraft.)  SUBPT_G also
C     determines the altitude of the observer above the target body.
C
C     SUBPT_G expects that the target body is modelled as a tri-axial
C     ellipsoid and that a PCK file containing its radii and rotation
C     model constants has been loaded into the kernel pool via LDPOOL.
C     It also expects that SPK ephemeris data covering the requested
C     epoch for the target and observing bodies has been loaded via
C     SPKLEF.
C
C$ Examples
C
C     In the following example program, the file 'T900927.BSP' is a
C     binary SPK ephemeris file containing data for the Galileo Orbiter,
C     the Earth, the Moon, and the Sun for the time period from
C     November 1 through January 1, 1990.  'PCK00003.TPC' is a
C     planetary constants kernel file, containing radii and rotation
C     model constants.  'GLL00001.TLS' is a leapseconds file.
C
C           INTEGER               HANDLE
C           INTEGER               OBSRVR
C           INTEGER               TARGET
C
C           DOUBLE PRECISION      ALT
C           DOUBLE PRECISION      DPR
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      LAT
C           DOUBLE PRECISION      LON
C           DOUBLE PRECISION      RADIUS
C           DOUBLE PRECISION      SPOINT  ( 3 )
C
C           LOGICAL               FOUND
C
C     C
C     C     Load kernel files.
C     C
C           CALL LDPOOL ( 'GLL00002.TLS' )
C           CALL LDPOOL ( 'PCK00003.TPC' )
C           CALL SPKLEF ( 'T900927.BSP',  HANDLE )
C
C     C
C     C     Set up inputs.
C     C
C           CALL UTC2ET   ( '1990 Dec 09 12:00:00', ET )
C
C           CALL BODN2C_G ( 'Galileo Orbiter', OBSRVR, FOUND )
C           CALL BODN2C_G ( 'Earth',           TARGET, FOUND )
C
C     C
C     C     Compute sub-spacecraft point using light-time corrections.
C     C
C           CALL SUBPT_G ( TARGET, OBSRVR, ET, 'LT', SPOINT, ALT )
C
C     C
C     C     Convert rectangular coordinates to planetocentric
C     C     latitude and longitude.  Convert radians to degrees.
C     C
C           CALL RECLAT ( SPOINT, RADIUS, LON, LAT  )
C
C           LON = LON * DPR ()
C           LAT = LAT * DPR ()
C
C     C
C     C     Write the results.
C     C
C           WRITE (*,*) 'Radius    (km)  = ', RADIUS
C           WRITE (*,*) 'Latitude  (deg) = ', LAT
C           WRITE (*,*) 'Longitude (deg) = ', LON
C           WRITE (*,*) 'Altitude  (km)  = ', ALT
C
C           END
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.E. McLean    (JPL)
C
C$ Version
C
C-    GLLSPICE Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 1.0.0, 05-NOV-1990 (JEM)
C
C-&
 
C$ Index_Entries
C
C     sub-observer point
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      POS   ( 3 )
      DOUBLE PRECISION      RADII ( 3 )
      DOUBLE PRECISION      STATE ( 6 )
      DOUBLE PRECISION      TIPM  ( 3, 3 )
 
      INTEGER               NRADII
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SUBPT_G' )
      END IF
 
 
 
C
C     Check the input body codes.  If they are equal, signal
C     an error.
 
      IF ( OBSRVR .EQ. TARGET ) THEN
 
         CALL SETMSG ( 'In computing the sub-observer point, the '    //
     .                 'observing body and target body are the same. ' )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)' )
         CALL CHKOUT ( 'SUBPT_G' )
         RETURN
 
      END IF
 
C
C     Get the radii of the target body from the kernel pool.
C
 
      CALL BODVAR ( TARGET, 'RADII', NRADII, RADII )
 
C
C     Determine the position of the observer in target
C     body-fixed coordinates.
C
C         -  Call SPKEZ to compute the position of the target
C            body as seen from the observing body and the light time
C            (LT) between them.  SPKEZ returns a state which is
C            the position and velocity, but we'll only use the position
C            which is the first three elements.  We request that the
C            coordinates of POS be returned relative to the J2000
C            inertial reference frame with aberration corrections
C            specified by the input argument ABCORR.
C
C         -  Call BODMAT to compute the rotation matrix (TIPM) that
C            transforms inertial coordinates to target body equator
C            and prime meridian (body-fixed) coordinates at the
C            appropriate epoch.
C
C         -  Call VMINUS to reverse the direction of the vector (POS)
C            so it will be the position of the observer as seen from
C            the target body in inertial coordinates.
C
C            Note that this result is not the same as the result of
C            calling SPKEZ with the target and observer switched.  We
C            computed the vector FROM the observer TO the target in
C            order to get the proper light time and stellar aberration
C            corrections (if requested).  Now we need the inverse of
C            that corrected vector in order to compute the sub-point.
C
C         -  Call MXV to transform the vector, POS, to body-fixed
C            coordinates by multiplying it by the matrix TIPM.
C
 
      CALL SPKEZ  ( TARGET, ET, 'J2000', ABCORR, OBSRVR, STATE, LT )
 
      IF (  EQSTR ( ABCORR, 'NONE' )  ) THEN
         EPOCH = ET
      ELSE
         EPOCH = ET - LT
      END IF
 
      CALL BODMAT ( TARGET, EPOCH, TIPM )
 
      CALL VMINUS ( STATE, POS )
 
      CALL MXV    ( TIPM, POS, POS )
 
 
C
C     Locate the sub-observer point.
C
C         -  Call NEARPT to locate the point on the surface of the
C            target body that is nearest to the observer.  This is
C            the sub-observer point in target body-fixed rectangular
C            coordinates. (The size of the target body is defined by
C            its radii whose values are extracted from the kernel pool
C            with BODVAR above.)
C
C
 
      CALL NEARPT ( POS, RADII(1), RADII(2), RADII(3), SPOINT, ALT )
 
      CALL CHKOUT ( 'SUBPT_G' )
      RETURN
      END
