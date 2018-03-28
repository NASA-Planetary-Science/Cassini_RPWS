C$Procedure      GSETRN_G ( Geocentric solar ecliptic transformation )
 
      SUBROUTINE GSETRN_G ( BODY, ET, REF, CORR, TI2GSE )
 
C$ Abstract
C
C     Return the matrix that transforms coordinates from a specified
C     inertial reference frame to geocentric solar ecliptic coordinates.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     COORDINATES
C     EPHEMERIS
C     FRAME
C     GLLSPICE
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations
 
      INTEGER               BODY
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         CORR
      DOUBLE PRECISION      TI2GSE ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BODY       I   NAIF integer code of body.
C     ET         I   Epoch, ephemeris seconds past J2000.
C     REF        I   Name of an inertial reference frame.
C     CORR       I   Aberration correction.
C     TI2GSE     O   Transformation from inertial to GSE coordinates.
C
C$ Detailed_Input
C
C     BODY           is a NAIF integer code for the body used to define
C                    the GSE system.  The NAIF integer code for the
C                    Earth is 399.  See the SPK required reading for a
C                    complete list of ID codes.
C
C     REF            is the name of an inertial reference frame.  Any
C                    name acceptable to the GLLSPICE routine IRFTRN_G
C                    may be used.  See the $Particulars section for a
C                    partial list of names.
C
C     ET             is the epoch for which the inertial-to-GSE
C                    transformation is to be computed.  ET is specified
C                    in ephemeris seconds past J2000.
C
C     CORR           is the aberration correction to be used in
C                    computing the body-Sun vector that defines the
C                    x-axis of the GSE system.  Possible values are:
C
C                       'NONE'        No aberration correction.
C
C                       'LT'          Correct position of Sun for
C                                     light time.
C
C                       'LT+S'        Correct position of Sun for light
C                                     time and stellar aberration.
C$ Detailed_Output
C
C     TI2GSE         is the inertial-to-GSE transformation matrix for
C                    the body specified by the input argument BODY, at
C                    the epoch specified by the input argument ET.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If no SPK (ephemeris) data is available for the Sun and
C         specified body at the time specified by ET, the error will be
C         diagnosed by routines called by this routine.
C
C     2)  If REF is not the name of a reference frame supported by
C         IRFTRN_G, the error will be diagnosed by routines called by
C         this routine.
C
C$ Files
C
C     None.  However, see $Restrictions.
C
C$ Particulars
C
C     The axes of the GSE (Geocentric Solar Ecliptic) coordinate system
C     are time-dependent vectors defined as follows for the Earth:
C
C        -- The positive x axis direction points from the Earth to
C           the Sun at the specified epoch.  The vector is geometric
C           (not corrected for light time or stellar aberration).
C
C        -- The positive y axis direction vector is the cross product
C           of the z and x vectors.  This vector is approximately
C           opposite in direction to the Earth's velocity vector.
C
C        -- The positive z axis direction is the cross product of the
C           Earth's velocity vector and the x direction vector.
C
C     This definition implies that the x-y plane of the GSE system
C     is the ecliptic plane of date.  The definition does not depend
C     in any way on the body being the Earth; any other body for
C     which ephemeris information is available will do as well.  To
C     allow for this more general application of GSETRN, the body used
C     in the definition is set via the input argument BODY.
C
C     Also, the definition of the GSE system calls for the use of a
C     geometric body-Sun vector.  Although this choice will normally
C     be used, whatever the body, the routine allows for the use of
C     light time and stellar aberration corrections.
C
C     Among the reference frame names accepted by IRFTRN_G are:
C
C        'ECLIP2000'
C        'ECLIP1950'
C        'J2000'
C        'B1950'
C        'FK4'
C        'DE-96'
C        'DE-102'
C        'DE-108'
C        'DE-111'
C        'DE-114'
C        'DE-118'
C        'DE-122'
C        'DE-125'
C        'DE-130'
C        'DE-200'
C        'DE-202'
C        'GALACTIC'
C
C
C$ Examples
C
C
C     1)  Find the transformation from EME50 to (Earth) GSE coordinates
C         at Dec. 8, 1990 18:00.00 UTC.  The following is a program that
C         you can try.  It must be linked with GLLSPICE as well as
C         SPICELIB.  To use it, you'll have to substitute names of
C         actual kernel files in the SPKLEF and LDPOOL calls.
C
C
C                  PROGRAM GETGSE
C
C            C
C            C     The NAIF ID code for the Earth is 399.  For the Sun,
C            C     the code is 10.
C            C
C                  INTEGER               EARTH
C                  PARAMETER           ( EARTH = 399 )
C
C                  INTEGER               SUN
C                  PARAMETER           ( SUN   =  10 )
C
C                  CHARACTER*30          UTC
C                  INTEGER               HANDLE
C                  DOUBLE PRECISION      ET
C                  DOUBLE PRECISION      LT
C                  DOUBLE PRECISION      STATE  ( 6 )
C                  DOUBLE PRECISION      TI2GSE ( 3, 3 )
C                  DOUBLE PRECISION      GSEPOS ( 3 )
C                  DOUBLE PRECISION      GSEVEL ( 3 )
C
C                  DATA UTC / 'Dec 8 1990 18:00.00' /
C
C            C
C            C     We must start out by loading an SPK file containing
C            C     Earth and Sun states for the requested time.  Also
C            C     at initialization time, we must load a leapseconds
C            C     kernel to support UTC--ET conversion.
C            C
C            C     Loading kernels usually can be done once during
C            C     program initialization.  In your own program, you
C            C     must supply the name of actual SPK and leapseconds
C            C     kernels accessible to your program.
C            C
C                  CALL SPKLEF ( 'MYSPK.BSP',    HANDLE )
C                  CALL LDPOOL ( 'MYKERNEL.KER'         )
C
C            C
C            C     Find the ephemeris time corresponding to the
C            C     UTC time Dec 8, 1990 18:00.00.
C            C
C                  CALL UTC2ET ( UTC, ET )
C
C            C
C            C     Find the transformation from EME50 to GSE coordinates
C            C     at the epoch ET.  In SPICELIB, the FK4 frame
C            C     corresponds to EME50.  We will use geometric states,
C            C     (aberration correction = 'NONE') as per reference
C            C     [2].
C            C
C                  CALL GSETRN_G ( EARTH, ET, 'FK4', 'NONE', TI2GSE )
C
C            C
C            C     TI2GSE is the transformation matrix we were looking
C            C     for.
C            C
C            C     To perform a consistency check, let's find the
C            C     position and velocity of the Earth as seen from the
C            C     Sun at epoch ET.  After transforming the position
C            C     and velocity to GSE coordinates, we should see that
C            C     the position vector of the Earth lies along (except
C            C     for round-off error) the negative x-axis, and that
C            C     the velocity vector of the Earth lies close to the
C            C     negative y-axis, with a magnitude of about 30 km/sec.
C            C
C            C     Since we're using geometric states, the Sun-Earth
C            C     vector at ET is exactly (except for round-off error)
C            C     the negative of the Earth-Sun vector at ET.
C            C
C                  CALL SPKEZ ( EARTH, ET, 'FK4', 'NONE', SUN, STATE,
C                 .             LT                                    )
C
C                  CALL MXV   ( TI2GSE, STATE,    GSEPOS )
C                  CALL MXV   ( TI2GSE, STATE(4), GSEVEL )
C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'State of the Earth relative to the Sun'
C                  WRITE (*,*) 'in GSE coordinates at UTC epoch ', UTC
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Position (km)'
C                  WRITE (*,*) GSEPOS
C                  WRITE (*,*) 'Velocity (km/sec)'
C                  WRITE (*,*) GSEVEL
C                  WRITE (*,*) ' '
C
C                  END
C
C
C     2)  Transform a vector V from GSE coordinates (at epoch ET) to
C         J2000 coordinates.  The body defining the system is the
C         Earth.
C
C            C
C            C     Find the transformation from J2000 to GSE coordinates
C            C     at ET.  Use geometric vectors.
C            C
C                  EARTH = 399
C
C                  CALL GSETRN_G ( EARTH, ET, 'J2000', 'NONE', TI2GSE )
C
C            C
C            C     The matrix that performs the inverse transformation
C            C     is just the transpose of TI2GSE.  The SPICELIB
C            C     routine MTXV left-multiplies a vector by the
C            C     transpose of a specified matrix.  On output from
C            C     MTXV, the elements of V are given in J2000
C            C     coordinates.
C            C
C                  CALL MTXV ( TI2GSE, V, V )
C
C
C$ Restrictions
C
C     1)  An SPK file containing ephemeris data for the Sun and body
C         specified by BODY must be loaded prior to calling this
C         routine.
C
C$ Literature_References
C
C     [1]  `Geophysical Coordinate Transformations', by Christopher
C           T. Russell.  Cosmic Electrodynamics 2 (1971) 184-186.
C           NAIF document 181.0.
C
C     [2]   Personal communication from Steven Joy (UCLA, GLL
C           magnetometer team) and Dr. Margaret Kivelson (UCLA, GLL
C           magnetometer P.I.).  NAIF document 210.0.
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
C-    GLLSPICE Version 1.0.0, 28-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     geocentric solar ecliptic transformation
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               SUN
      PARAMETER           ( SUN   =  10 )
 
C
C     Local variables
C
      DOUBLE PRECISION      ORBUAV ( 3 )
      DOUBLE PRECISION      BODVEL ( 3 )
      DOUBLE PRECISION      TREF2I ( 3, 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      SPOS   ( 3 )
      DOUBLE PRECISION      SSTATE ( 6 )
      DOUBLE PRECISION      YVEC   ( 3 )
 
      INTEGER               COL
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GSETRN_G' )
      END IF
 
C
C     Find the geometric position of the Sun as seen from the body at
C     the specified epoch.  Use J2000 coordinates.  The reason that we
C     cannot supply REF directly to SPKEZ is that REF may have a value
C     supported by IRFTRN_G but not by SPKEZ (such as 'ECLIP2000').
C     Obtain the unit body-Sun vector.
C
      CALL SPKEZ  ( SUN,    ET,  'J2000',  'NONE',  BODY,  SSTATE,  LT )
      CALL VHAT   ( SSTATE, SPOS )
 
C
C     Find the unit orbital angular velocity vector at the specified
C     epoch.  This is the unitized cross product of the body's position
C     and velocity, which is the same as the unitized cross product of
C     the body's velocity and the body-Sun vector.  The SPICELIB
C     routine UCRSS returns a unitized cross product.
C
      CALL VMINUS ( SSTATE(4),  BODVEL          )
      CALL UCRSS  ( BODVEL,     SPOS,    ORBUAV )
 
C
C     The y vector of the GSE system completes the orthonormal set.
C
      CALL UCRSS  ( ORBUAV, SPOS, YVEC )
 
C
C     The rows of the J2000-to-GSE transformation matrix are
C
C        SPOS
C        YVEC
C        ORBUAV
C
C     We'll fill in the entries of this matrix.
C
      DO 50001
     .   COL = 1, 3
 
         TI2GSE(1,COL)  =  SPOS   (COL)
         TI2GSE(2,COL)  =  YVEC   (COL)
         TI2GSE(3,COL)  =  ORBUAV (COL)
 
50001 CONTINUE
 
C
C     The transformation from the specified inertial frame to GSE is
C     the transformation from J2000 to GSE composed with the
C     transformation from the specified frame to J2000.
C
      CALL IRFTRN_G ( REF,   'J2000', TREF2I )
      CALL MXM      ( TI2GSE, TREF2I, TI2GSE )
 
      CALL CHKOUT ( 'GSETRN_G' )
      RETURN
      END
