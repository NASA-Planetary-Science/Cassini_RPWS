C$Procedure  GSMTRN_G ( Geocentric solar magnetospheric transformation )
 
      SUBROUTINE GSMTRN_G ( BODY, ET, REF, CORR, TI2GSM )
 
C$ Abstract
C
C     Return the matrix that transforms coordinates from a specified
C     inertial reference frame to geocentric solar magnetospheric
C     coordinates.
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
      DOUBLE PRECISION      TI2GSM ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BODY       I   NAIF integer code of body.
C     ET         I   Epoch, ephemeris seconds past J2000.
C     REF        I   Name of an inertial reference frame.
C     CORR       I   Aberration correction.
C     TI2GSM     O   Transformation from inertial to GSM coordinates.
C
C$ Detailed_Input
C
C     BODY           is a NAIF integer code for the body used to define
C                    the GSM system.  The NAIF integer code for the
C                    Earth is 399.  See the SPK required reading for a
C                    complete list of ID codes.
C
C     REF            is the name of an inertial reference frame.  Any
C                    name acceptable to the GLLSPICE routine IRFTRN_G
C                    may be used.  See the $Particulars section for a
C                    partial list of names.
C
C     ET             is the epoch for which the inertial-to-GSM
C                    transformation is to be computed.  ET is specified
C                    in ephemeris seconds past J2000.
C
C     CORR           is the aberration correction to be used in
C                    computing the body-Sun vector that defines the
C                    x-axis of the GSM system.  Possible values are:
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
C     TI2GSM         is the inertial-to-GSM transformation matrix for
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
C     3)  If PCK data defining the orientation model for BODY is not
C         found in the kernel pool, the error will be diagnosed by
C         routines called by this routine.
C
C     4)  If PCK data defining the position of the magnetic pole of BODY
C         is not found in the kernel pool, the error
C         SPICE(KERNELVARNOTFOUND) is signalled.
C
C$ Files
C
C     None.  However, see $Restrictions.
C
C$ Particulars
C
C     The axes of the GSM (Geocentric Solar Magnetospheric) coordinate
C     system are time-dependent vectors defined as follows for the
C     Earth:
C
C        -- The positive x axis direction points from the Earth to
C           the Sun at the specified epoch.  The vector is geometric
C           (not corrected for light time or stellar aberration).
C
C        -- The positive y axis direction is the cross product of the
C           Earth's magnetic North pole and the x-axis.
C
C        -- The positive z axis direction is the cross product of the
C           x and y axes.  This implies that the positive z axis
C           direction is coplanar with the x axis and the magnetic
C           dipole, perpendicular to the x axis, and points to the same
C           side of the x-y plane as the Earth's North magnetic pole.
C
C     This definition does not depend in any way on the body being the
C     Earth; any other body for which ephemeris information and the
C     magnetic pole location are available will do as well.  To allow
C     for this more general application of GSMTRN, the body used in the
C     definition is set via the input argument BODY.
C
C     Also, the definition of the GSM system calls for the use of a
C     geometric body-Sun vector.  Although this choice will normally
C     be used, whatever the body, the routine allows for the use of
C     light time and stellar aberration corrections.
C
C     Among the reference frame names accepted by this routine are:
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
C     See the GLLSPICE routine IRFTRN_G for a complete list.
C
C
C$ Examples
C
C
C     1)  Find the transformation from EME50 to (Earth) GSM coordinates
C         at 1990 Mar. 20 12:00 UTC.  The following is a program that
C         you can try.  It must be linked with GLLSPICE as well as
C         SPICELIB.  To use it, you'll have to substitute names of
C         actual kernel files in the SPKLEF and LDPOOL calls.
C
C
C                  PROGRAM GETGSM
C
C            C
C            C     SPICELIB functions
C            C
C                  DOUBLE PRECISION      DPR
C                  DOUBLE PRECISION      RPD
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
C                  DOUBLE PRECISION      MAGLAT
C                  DOUBLE PRECISION      MAGLON
C                  DOUBLE PRECISION      POLLAT
C                  DOUBLE PRECISION      POLLON
C                  DOUBLE PRECISION      POLRAD
C                  DOUBLE PRECISION      POLVEC ( 3 )
C                  DOUBLE PRECISION      STATE  ( 6 )
C                  DOUBLE PRECISION      TIPM   ( 3, 3 )
C                  DOUBLE PRECISION      TI2GSM ( 3, 3 )
C                  DOUBLE PRECISION      TJ2GSM ( 3, 3 )
C                  DOUBLE PRECISION      GSMPOS ( 3 )
C                  DOUBLE PRECISION      GSMVEL ( 3 )
C
C                  INTEGER               N
C
C                  DATA UTC / 'Mar 20 1990 12:00' /
C
C            C
C            C     We must start out by loading an SPK file containing
C            C     Earth and Sun states for the requested time.  Also
C            C     at initialization time, we must load a leapseconds
C            C     kernel to support UTC--ET conversion, and we must
C            C     load a PCK file containing constants defining the
C            C     Earth's orientation model and magnetic North pole
C            C     location.
C            C
C            C     Loading kernels usually can be done once during
C            C     program initialization.  In your own program, you
C            C     must supply the name of actual SPK, PCK, and
C            C     leapseconds kernels accessible to your program.
C            C
C                  CALL SPKLEF ( 'MYSPK.BSP',         HANDLE )
C                  CALL LDPOOL ( 'MYLEAPSECONDS.KER'         )
C                  CALL LDPOOL ( 'MYPCONSTANTS.KER'          )
C
C            C
C            C     Find the ephemeris time corresponding to the
C            C     UTC time Mar. 20, 1990 12:00.
C            C
C                  CALL UTC2ET ( UTC, ET )
C
C            C
C            C     Find the transformation from EME50 to GSM coordinates
C            C     at the epoch ET.  In SPICELIB, the FK4 frame
C            C     corresponds to EME50.  We will use geometric states,
C            C     (aberration correction = 'NONE') as per reference
C            C     [2].
C            C
C                  CALL GSMTRN_G ( EARTH, ET, 'FK4', 'NONE', TI2GSM )
C
C            C
C            C     TI2GSM is the transformation matrix we were looking
C            C     for.
C            C
C
C            C
C            C     To perform a consistency check, let's find the
C            C     position of the Earth as seen from the Sun at epoch
C            C     ET, as well as the location of the Earth's North
C            C     magnetic pole, in GSM coordinates.  After
C            C     transforming the Earth's position relative to the
C            C     Sun to GSM coordinates, we should see that the
C            C     position vector of the Earth lies along (except
C            C     for round-off error) the negative x-axis.
C            C
C            C     Since we're using geometric states, the Sun-Earth
C            C     vector at ET is exactly (except for round-off error)
C            C     the negative of the Earth-Sun vector at ET.
C            C
C                  CALL SPKEZ ( EARTH, ET, 'FK4', 'NONE', SUN, STATE,
C                 .             LT                                    )
C
C                  CALL MXV   ( TI2GSM, STATE,    GSMPOS )
C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Position of the Earth relative to the'//
C                 .            ' Sun'
C                  WRITE (*,*) 'in GSM coordinates at UTC epoch ', UTC
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Position (km)'
C                  WRITE (*,*) GSMPOS
C                  WRITE (*,*) ' '
C
C            C
C            C     The Earth's North magnetic pole should lie in the
C            C     x-z plane in GSM coordinates, so if we convert
C            C     its GSM representation to latitudinal coordinates,
C            C     its longitude should be zero or 180 degrees.  We've
C            C     picked an epoch at which the Earth-Sun vector and
C            C     x-axis of the Earth equator and prime meridian system
C            C     are nearly aligned, so the latitude of the magnetic
C            C     North pole will be approximately the angular
C            C     separation between the pole and the x-axis of the
C            C     Earth equator and prime meridian system.  This
C            C     separation is approximately 86.067 degrees.  Since
C            C     the angular separation is less than 90 degrees, the
C            C     GSM longitude of the pole at this epoch will be zero.
C            C
C            C     Find the longitude and latitude of the Earth's North
C            C     magnetic pole.  Convert the pole vector to
C            C     rectangular body-fixed coordinates and then to GSM
C            C     coordinates.  Find the latitude and longitude of the
C            C     pole in GSM coordinates.
C            C
C                  CALL BODVAR ( EARTH, 'MAG_NORTH_POLE_LON', N, MAGLON)
C                  CALL BODVAR ( EARTH, 'MAG_NORTH_POLE_LAT', N, MAGLAT)
C
C            C
C            C     Convert longitude and latitude to radians.
C            C
C                  MAGLON = RPD() * MAGLON
C                  MAGLAT = RPD() * MAGLAT
C
C                  CALL LATREC ( 1.0D0, MAGLON, MAGLAT, POLVEC )
C
C            C
C            C     BODMAT returns the transformation from J2000 to
C            C     body equator and prime meridian coordinates.
C            C     We convert POLVEC to J2000 coordinates and then
C            C     to GSM coordinates.
C            C
C                  CALL BODMAT   ( EARTH,   ET,      TIPM   )
C                  CALL MTXV     ( TIPM,   POLVEC,  POLVEC )
C
C                  CALL GSMTRN_G ( EARTH,   ET,     'J2000', 'NONE',
C                 .                TJ2GSM                          )
C
C                  CALL MXV      ( TJ2GSM, POLVEC, POLVEC )
C
C            C
C            C     Now we've got POLVEC in GSM coordinates.  Convert
C            C     to latitudinal coordinates.
C            C
C                  CALL RECLAT ( POLVEC, POLRAD, POLLON, POLLAT )
C
C            C
C            C     Convert latitude and longitude to degrees, and
C            C     write out the results.
C            C
C                  POLLON = POLLON * DPR()
C                  POLLAT = POLLAT * DPR()
C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'GSM longitude and latitude of the'
C                  WRITE (*,*) 'Earth''s magnetic pole (degrees)'
C                  WRITE (*,*) 'at epoch ', UTC
C                  WRITE (*,*) ' '
C                  WRITE (*,*) POLLON
C                  WRITE (*,*) POLLAT
C                  WRITE (*,*) ' '
C
C                  END
C
C     2)  Transform a vector V from GSM coordinates (at epoch ET) to
C         J2000 coordinates.  The body defining the system is the
C         Earth.
C
C            C
C            C     Find the transformation from J2000 to GSM coordinates
C            C     at ET.  Use geometric vectors.
C            C
C                  EARTH = 399
C
C                  CALL GSMTRN_G ( EARTH, ET, 'J2000', 'NONE', TI2GSM )
C
C            C
C            C     The matrix that performs the inverse transformation
C            C     is just the transpose of TI2GSM.  The SPICELIB
C            C     routine MTXV left-multiplies a vector by the
C            C     transpose of a specified matrix.  On output from
C            C     MTXV, the elements of V are given in J2000
C            C     coordinates.
C            C
C                  CALL MTXV ( TI2GSM, V, V )
C
C
C$ Restrictions
C
C     1)  An SPK file containing ephemeris data for the Sun and the body
C         specified by BODY must be loaded prior to calling this
C         routine.
C
C     2)  A PCK file containing constants defining the orientation model
C         and location of the North magnetic pole of the body specified
C         by BODY must be loaded prior to calling this routine.
C
C     3)  Although BODY is an input argument to this routine and is
C         intended to be set by the user, it is not clear to the author
C         of this routine at this time (the date of version 1.0.0 of
C         this routine) whether GSM coordinates are defined in a
C         widely-agreed-upon fashion for bodies other than the Earth.
C         It is the user's responsibility to make sure that this routine
C         is appropriate for the intended use.
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
C-    GLLSPICE Version 1.0.0, 03-DEC-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     geocentric solar magnetospheric transformation
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      RPD
 
C
C     Local parameters
C
      INTEGER               SUN
      PARAMETER           ( SUN   =  10 )
 
C
C     Local variables
C
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MAGLAT
      DOUBLE PRECISION      MAGLON
      DOUBLE PRECISION      POLVEC ( 3 )
      DOUBLE PRECISION      SPOS   ( 3 )
      DOUBLE PRECISION      SSTATE ( 6 )
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TREF2I ( 3, 3 )
      DOUBLE PRECISION      YVEC   ( 3 )
      DOUBLE PRECISION      ZVEC   ( 3 )
 
      INTEGER               COL
      INTEGER               N
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GSMTRN_G' )
      END IF
 
C
C     Find the geometric position of the Sun as seen from the body at
C     the specified epoch.  The reason that we cannot supply REF
C     directly to SPKEZ is that REF may have a value supported by
C     IRFTRN_G but not by SPKEZ (such as 'ECLIP2000').  We'll work in
C     J2000 coordinates.  Obtain the unit body-Sun vector.
C
      CALL SPKEZ  ( SUN,    ET,  'J2000',  'NONE',  BODY,  SSTATE,  LT )
      CALL VHAT   ( SSTATE, SPOS )
 
C
C     Obtain the longitude and latitude (in degrees) of BODY's magnetic
C     North pole.
C
      CALL BODVAR ( BODY, 'MAG_NORTH_POLE_LON', N, MAGLON )
      CALL BODVAR ( BODY, 'MAG_NORTH_POLE_LAT', N, MAGLAT )
 
C
C     Convert the magnetic North pole vector to rectangular coordinates.
C     Remember to convert degrees to radians for LATREC inputs.
C
      CALL LATREC ( 1.0D0,  RPD()*MAGLON,  RPD()*MAGLAT,  POLVEC )
 
C
C     Obtain the transformation from J2000 to body-fixed (body
C     equator and prime meridian) coordinates for the epoch ET.
C
      CALL BODMAT ( BODY, ET, TIPM )
 
C
C     Convert the pole vector to J2000 coordinates.
C
      CALL MTXV ( TIPM, POLVEC, POLVEC )
 
C
C     The y vector of the GSM system is the cross product of the
C     North magnetic pole vector and the x-axis.  The SPICELIB
C     routine UCRSS returns a unitized cross product.
C
      CALL UCRSS ( POLVEC, SPOS, YVEC )
 
C
C     The z vector of the GSM system completes the orthonormal set.
C
      CALL UCRSS  ( SPOS,  YVEC, ZVEC )
 
C
C     The rows of the J2000-to-GSM transformation matrix are
C
C        SPOS
C        YVEC
C        ZVEC
C
C     We'll fill in the entries of this matrix.
C
      DO 50001
     .   COL = 1, 3
 
         TI2GSM(1,COL)  =  SPOS (COL)
         TI2GSM(2,COL)  =  YVEC (COL)
         TI2GSM(3,COL)  =  ZVEC (COL)
 
50001 CONTINUE
 
C
C     Find TREF2I, the transformation from the system specified by REF
C     to inertial (J2000) coordinates.
C
      CALL IRFTRN_G ( REF, 'J2000', TREF2I )
 
C
C     The transformation from the specified inertial frame to GSM is
C     the transformation from J2000 to GSM composed with the
C     transformation from the specified frame to J2000.
C
      CALL MXM ( TI2GSM, TREF2I, TI2GSM )
 
 
      CALL CHKOUT ( 'GSMTRN_G' )
      RETURN
      END
