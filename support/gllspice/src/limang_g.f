C$Procedure      LIMANG_G ( Limb angle )
 
      SUBROUTINE LIMANG_G ( TARGET,  ET,   CORR,  OBSRVR,  DIR,  DREF,
     .                      ANGLE,   MINPT                             )
 
C$ Abstract
C
C     Find the angular separation between the limb of a specified
C     target body and a ray emanating from a specified observation
C     location.
C
C$ Required_Reading
C
C     CK
C     KERNEL
C     SPK
C     TIME
C
C$ Keywords
C
C     ELLIPSOID
C     GEOMETRY
C     GLLSPICE
C
C$ Declarations
 
      INTEGER               TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         CORR
      INTEGER               OBSRVR
      DOUBLE PRECISION      DIR    ( 3 )
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      MINPT  ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARGET     I   NAIF integer code of target body.
C     ET         I   Epoch in ephemeris seconds past J2000.
C     CORR       I   Desired aberration correction.
C     OBSRVR     I   NAIF integer code of observing body.
C     DIR        I   Direction vector of ray.
C     DREF       I   Inertial reference frame of ray's direction vector.
C     ANGLE      O   Angular separation of ray and ellipsoid (radians).
C     MINPT      O   Closest point on ellipsoid to ray.
C
C$ Detailed_Input
C
C     TARGET         is the NAIF integer code of the target body.
C
C     ET             is the epoch in ephemeris seconds past J2000 at
C                    which the angular semi-diameter of the target body
C                    is to be computed.
C
C     CORR           is the aberration correction to be used in
C                    computing the location and orientation of the
C                    target body.  Possible values are:
C
C                       'NONE'        No aberration correction.
C
C                       'LT'          Correct position and orientation
C                                     of target body for light time.
C
C                       'LT+S'        Correct position and orientation
C                                     of target body for light time and
C                                     stellar aberration.
C
C
C     OBSRVR         is the NAIF integer code of the observing body.
C
C     DIR            is the direction vector of a ray in
C                    three-dimensional space.
C
C     DREF           is the name of inertial reference frame in which
C                    the coordinates of the input ray's direction vector
C                    are given.  Any name supported by the SPICELIB
C                    routine SPKEZ may be used.  Examples:
C
C                       'J2000'
C                       'FK4'    (Use this for `EME50')
C                       'B1950'
C
C$ Detailed_Output
C
C     ANGLE          is the minimum angular separation of the input ray
C                    and the target body. This is the minimum angular
C                    separation of the ray and any line segment
C                    extending from the ray's vertex to a point on the
C                    surface of the target body.  Units are radians.
C
C                    If the input ray actually intersects the
C                    target body, ANGLE is set to zero.
C
C
C     MINPT          is the point on the target body where the minimum
C                    angular separation is achieved.  If the input ray
C                    intersects the target body, MINPT is set to the
C                    intercept point closest to the ray's vertex.
C                    MINPT is output in body-fixed (body equator and
C                    prime meridian) coordinates, NOT IN THE COORDINATE
C                    SYSTEM SPECIFIED BY DREF.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the body and target ID codes are identical, the error
C         SPICE(BODIESNOTDISTINCT) is signalled.  ASDIAM_G is set
C         to zero.
C
C     2)  If no SPK (ephemeris) data is available for the observer and
C         target at the time specified by ET, the error will be
C         diagnosed by routines called by this routine.  If light time
C         corrections are used, SPK data for the target body must be
C         available at the time ET - LT, where LT is the one-way light
C         time from the target to the observer at ET.
C
C     3)  If PCK data defining the orientation or shape of the target
C         body is unavailable, the error will be diagnosed by routines
C         called by this routine.
C
C     4)  If DIR is the zero vector, the error will be diagnosed by
C         routines called by this routine.
C
C$ Files
C
C     No files are input to this routine, however, LIMANG_G expects
C     that the appropriate SPK and PCK files have been loaded using
C     SPKLEF and LDPOOL, respectively.
C
C$ Particulars
C
C     For spherical bodies, finding the angle between a ray and the
C     limb of the body is a simple matter.  For bodies modelled as
C     triaxial ellipsoids, it is considerably more difficult.
C
C     Note that for triaxial ellipsoids, the surface point having
C     minimum angular separation from a ray is NOT necessarily the
C     closest limb point to the ray, unlike in the case of the
C     sphere.  You can verify this by considering the case of an
C     extremely oblate spheroid and a ray that passes above it.   The
C     diagram below illustrates this situation.  The diagram shows a
C     side view of the ray and spheroid.  The series of three asterisks
C     rising from left to right represents the ray; the other asterisks
C     represent the side view of the oblate spheroid. The point `c' is
C     the closest limb point to the ray; the point `m' has the minimum
C     angular separation from the ray.
C
C                                                            *
C
C                                  *
C    ray's vertex
C        *
C
C
C    closest limb           * * * * * * * * * m   <-- point of minimum
C  point to the ray-->   c                      *        angular
C                           * * * * * * * * * *          separation
C
C
C
C     An SPK file containing ephemeris data covering TARGET and OBSRVR
C     at time ET must be loaded via SPKLEF before calling LIMANG_G.
C     Additionally, if light time correction is used, the SPK file must
C     cover TARGET at time ET - LT, where LT is the light time between
C     the target and observer at ET.
C
C     This routine assumes that the target body is modelled as a
C     triaxial ellipsoid, and that a PCK file containing its radii and
C     rotation model constants has been loaded into the kernel pool
C     using the subroutine LDPOOL.
C
C$ Examples
C
C     1)  A case that is easy to check.  Since Venus is quite close
C         to being spherical, the results produced by LIMANG_G should
C         compare well with those produced by a direct calculation.
C
C                  PROGRAM LIMTST
C
C            C
C            C     Compare the angular separation between a
C            C     specified ray and the limb of Venus, as seen
C            C     from the Galileo orbiter on Feb.10, 1990.
C            C
C
C
C            C
C            C     SPICELIB functions
C            C
C                  DOUBLE PRECISION      DPR
C                  DOUBLE PRECISION      PI
C                  DOUBLE PRECISION      RPD
C                  DOUBLE PRECISION      VNORM
C
C            C
C            C     Local parameters
C            C
C
C            C
C            C     The NAIF integer ID code for Venus is 299.
C            C     For the Galileo orbiter, the NAIF integer code
C            C     is -77.
C            C
C                  INTEGER               VENUS
C                  PARAMETER           ( VENUS  = 299 )
C
C                  INTEGER               GLL
C                  PARAMETER           ( GLL    = -77 )
C
C
C                  INTEGER               FILEN
C                  PARAMETER           ( FILEN  = 128 )
C
C            C
C            C     Local variables
C            C
C                  CHARACTER*(FILEN)     SPKKER
C                  CHARACTER*(FILEN)     LPSKER
C                  CHARACTER*(FILEN)     PCKKER
C                  CHARACTER*(30)        UTC
C
C                  DOUBLE PRECISION      ANGLE1
C                  DOUBLE PRECISION      ANGLE2
C                  DOUBLE PRECISION      ANGSIZ
C                  DOUBLE PRECISION      NRPT1  ( 3 )
C                  DOUBLE PRECISION      NRPT2  ( 3 )
C                  DOUBLE PRECISION      ET
C                  DOUBLE PRECISION      LT
C                  DOUBLE PRECISION      RADII  ( 3 )
C                  DOUBLE PRECISION      RAYDIR ( 3 )
C                  DOUBLE PRECISION      STATE  ( 6 )
C                  DOUBLE PRECISION      TIPM   ( 3, 3 )
C                  DOUBLE PRECISION      VENPOS ( 3 )
C                  DOUBLE PRECISION      VRAD   ( 3 )
C                  DOUBLE PRECISION      X      ( 3 )
C                  DOUBLE PRECISION      Y      ( 3 )
C                  DOUBLE PRECISION      Z      ( 3 )
C
C                  INTEGER               HANDLE
C                  INTEGER               N
C
C                  DATA UTC / '1990 FEB 10' /
C
C            C
C            C     Get the name of an SPK kernel, and a leapseconds
C            C     kernel covering GLL and Venus at the time of
C            C     interest.  Also get the name of a PCK kernel
C            C     containing Venus radii.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Enter the name of an SPK kernel ' //
C                 .            'covering the time of interest.'
C                  READ  (*,FMT='(A)') SPKKER
C
C                  WRITE (*,*) 'Enter the name of a leapseconds kernel.'
C                  READ  (*,FMT='(A)') LPSKER
C
C                  WRITE (*,*) 'Enter the name of a p_constants kernel.'
C                  READ  (*,FMT='(A)') PCKKER
C
C                  CALL SPKLEF  ( SPKKER,  HANDLE )
C                  WRITE (*,*) 'SPK kernel loaded.'
C
C                  CALL LDPOOL ( LPSKER )
C                  WRITE (*,*) 'Leapseconds kernel loaded.'
C
C                  CALL LDPOOL ( PCKKER )
C                  WRITE (*,*) 'P_constants kernel loaded.'
C
C            C
C            C     Convert our UTC epoch to ET.
C            C
C                  CALL UTC2ET ( UTC, ET )
C
C            C
C            C     Find the state of Venus at epoch, as seen from
C            C     the Galileo orbiter.  Use J2000 coordinates.
C            C     Use light time and stellar aberration correction.
C            C
C                  CALL SPKEZ ( VENUS, ET, 'J2000', 'LT+S', GLL,
C                 .             STATE, LT                        )
C
C            C
C            C     Extract the position part of the state vector.
C            C     The SPICELIB routine VEQU just copies the first three
C            C     elements of a double precision array.
C            C
C                  CALL VEQU ( STATE, VENPOS )
C
C            C
C            C     We're now going to produce a ray that is 10
C            C     degrees off from the Galileo-Venus vector.
C            C     The SPICELIB routine FRAME gives us two unit vectors
C            C     orthogonal to a specified vector.  It also unitizes
C            C     its input vector.  Using it, we can create a basis
C            C     whose x-axis is parallel to the Galileo-Venus
C            C     vector.
C            C
C                  CALL VEQU  ( VENPOS, X )
C                  CALL FRAME ( X,  Y,  Z )
C
C            C
C            C     We create the desired ray by rotating our x vector
C            C     by 10 degrees about the y-axis.  We convert degrees
C            C     to radians before calling the SPICELIB routine
C            C     VROTV.
C            C
C                  CALL VROTV  ( X, Y, 10 * RPD(), RAYDIR )
C
C            C
C            C     Look up the radii of Venus from the PCK kernel.
C            C
C                  CALL BODVAR ( VENUS, 'RADII', N, RADII )
C
C            C
C            C     Find the angle between the ray and Venus, and find
C            C     the surface point of minimum angular separation.
C            C
C                  CALL LIMANG_G ( VENUS,
C                 .                ET,
C                 .               'LT+S',
C                 .                GLL,
C                 .                RAYDIR,
C                 .               'J2000',
C                 .                ANGLE1, NRPT1 )
C
C            C
C            C     Write out the results.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Results from LIMANG:'
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Angle between ray and Venus'' surface--'
C                  WRITE (*,*) 'units are degrees.'
C
C            C
C            C     Note the use of the SPICELIB function DPR to convert
C            C     radians to degrees.
C            C
C                  WRITE (*,*) ANGLE1 * DPR()
C                  WRITE (*,*) 'Point of minimum angular separation.'
C                  WRITE (*,*) '(Cartesian body fixed coordinates--km)'
C                  WRITE (*,*) NRPT1
C                  WRITE (*,*) ' '
C
C            C
C            C     Now do the computation directly, so we can compare
C            C     results.  Because Venus is nearly spherical, this
C            C     computation is simple.
C            C
C
C            C
C            C     The angular size of Venus as seen from Galileo can
C            C     be found from Venus' radius and the Galileo-Venus
C            C     vector.  Since the radii are nearly equal, we pick
C            C     the first radius.  The SPICELIB function VNORM
C            C     returns the norm of a vector.
C            C
C                  ANGSIZ = ASIN ( RADII(1) / VNORM (VENPOS) )
C
C                  ANGLE2 = 10 * RPD()  -  ANGSIZ
C            C
C            C     To find the surface point of minimum angular
C            C     separation, take the radius vector parallel to
C            C     the Z vector that we've constructed and rotate it
C            C     by 10 degrees about the Y vector.  Use the SPICELIB
C            C     routine VSCL for scaling.
C            C
C                  CALL VSCL   ( RADII(1), Z, VRAD )
C
C                  CALL VROTV  ( VRAD,  Y,  ( PI() + ANGSIZ ),  NRPT2 )
C
C            C
C            C     We need NRPT2 in Venus body-fixed (body equator
C            C     and prime meridian) coordinates, if we wish to
C            C     compare it with LIMANG_G's NRPT1.  The SPICELIB
C            C     routine BODMAT returns the J2000-to-body equator
C            C     and prime meridian transformation we want.  We
C            C     must remember to evaluate this transformation
C            C     at the light-time retarded epoch.
C            C
C                  CALL BODMAT ( VENUS, ET-LT, TIPM  )
C                  CALL MXV    ( TIPM,  NRPT2, NRPT2 )
C
C            C
C            C     Write out the results.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Results from direct computation.'
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Angle between ray and Venus'' surface--'
C                  WRITE (*,*) 'units are degrees.'
C                  WRITE (*,*) ANGLE2 * DPR()
C                  WRITE (*,*) 'Point of minimum angular separation.'
C                  WRITE (*,*) '(Cartesian body fixed coordinates--km)'
C                  WRITE (*,*) NRPT2
C                  WRITE (*,*) ' '
C
C                  WRITE (*,*) 'The results should compare closely.'
C                  WRITE (*,*) ' '
C
C                  END
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
C-    GLLSPICE Version 1.0.0, 07-DEC-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     limb angle
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
C
C     Local variables
C
      DOUBLE PRECISION      IPMTRN ( 3, 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      TEPOCH
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TMPDIR ( 3 )
 
      INTEGER               N
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LIMANG_G' )
      END IF
 
C
C     The target and observer must be distinct.
C
      IF ( TARGET .EQ. OBSRVR ) THEN
 
         CALL SETMSG ( 'Target is #, Observer is #.' )
         CALL ERRINT ( '#', TARGET                   )
         CALL ERRINT ( '#', OBSRVR                   )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'       )
         CALL CHKOUT ( 'LIMANG_G'                    )
         RETURN
 
      END IF
 
C
C     Find the position of the target as seen from the observation
C     location at time ET.  Use the indicated aberration correction.
C
      CALL SPKEZ ( TARGET, ET, 'J2000', CORR, OBSRVR, STATE, LT )
 
C
C     Find the transformation from J2000 to body equator and prime
C     meridian coordinates at the epoch corresponding to the
C     aberration correction.
C
      IF ( CORR .EQ. 'NONE' ) THEN
         TEPOCH = ET
      ELSE
         TEPOCH = ET - LT
      END IF
 
C
C     Convert the ray's direction vector to J2000 coordinates.
C
      CALL IRFTRN_G ( DREF,   'J2000',  IPMTRN )
      CALL MXV      ( IPMTRN,  DIR,     TMPDIR )
 
C
C     Negate the observer-target vector and rotate the result into
C     body-fixed coordinates.  Also rotate the ray's direction vector
C     into body-fixed coordinates.
C
      CALL BODMAT ( TARGET, TEPOCH, TIPM )
 
      CALL VMINUS ( STATE, OBSPOS         )
      CALL MXV    ( TIPM,  OBSPOS, OBSPOS )
      CALL MXV    ( TIPM,  TMPDIR, TMPDIR )
 
C
C     Get the target body's radii.
C
      CALL BODVAR ( TARGET, 'RADII', N, RADII )
 
C
C     Find the angular separation between the ray and target body.
C
      CALL ASRYED_G ( RADII(1),  RADII(2),  RADII(3),  OBSPOS,  TMPDIR,
     .                ANGLE,     MINPT                                )
 
 
      CALL CHKOUT ( 'LIMANG_G' )
      RETURN
      END
