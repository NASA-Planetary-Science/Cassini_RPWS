C$Procedure      SMEARV_G ( Smear velocity )
 
      SUBROUTINE SMEARV_G ( TARGET,
     .                      OBSRVR,
     .                      ET,
     .                      U,
     .                      DU,
     .                      REFIN,
     .                      SP,
     .                      DSP,
     .                      FOUND   )
 
C$ Abstract
C
C     Determine the instantaneous velocity of a surface intercept
C     vector with respect to the surface of the target body at the
C     intercept point and the coordinates of that point.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     GEOMETRY
C
C$ Declarations
 
      INTEGER               TARGET
      INTEGER               OBSRVR
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      U      ( 3 )
      DOUBLE PRECISION      DU     ( 3 )
      CHARACTER*(*)         REFIN
      DOUBLE PRECISION      SP     ( 3 )
      DOUBLE PRECISION      DSP    ( 3 )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARGET     I   NAIF integer code of target body.
C     OBSRVR     I   NAIF integer code of observing body (spacecraft).
C     ET         I   Epoch at which the velocity is to be computed.
C     U          I   Vector from the observer in some direction.
C     DU         I   Velocity of U.
C     REFIN      I   Reference frame of U and DU.
C     SP         O   Location on the target body towards which U points.
C     DSP        O   Velocity of SP.
C     FOUND      O   True if SP exists and DSP can be computed.
C
C$ Detailed_Input
C
C     TARGET     is the NAIF integer code of a target body.
C                This routine assumes that this body is modelled
C                by a tri-axial ellipsoid, and that a PCK file
C                containing its radii and rotation model constants
C                has been loaded into the kernel pool.
C
C     OBSRVR     is the NAIF integer code of the observing body
C                which is normally a spacecraft.
C
C     ET         is the time, measured in ephemeris seconds past
C                J2000, at which the surface intercept point and its
C                velocity are to be computed.
C
C
C                Note that SPK ephemeris data covering the epoch ET
C                for the TARGET and OBSRVR must be loaded via SPKLEF
C                before calling SMEARV_G.
C
C
C     U          is the inertially referenced pointing or line-of-
C                sight vector of an instrument on the observing body
C                at ET.  Presumably, the ray emanating from the
C                observing body in the direction of U intersects
C                the target body.
C
C     DU         is the inertially referenced velocity of the pointing
C                vector U at ET.  Note that if AV is the angular
C                velocity vector, then DU is the cross product of AV
C                and U:
C
C                     DU =  AV  X  U
C
C                The angular velocity vector is the vector whose
C                direction gives the axis about which an instrument-
C                fixed reference frame is rotating, and whose
C                magnitude is equal to the magnitude of the rotation
C                velocity in radians per second.  (In the SPICE system,
C                angular velocity vectors are stored in CK files.)
C
C     REFIN      is the name of the inertial frame to which U and DU
C                are referenced.  This must be a frame supported
C                by the SPICELIB subroutine CHGIRF, such as 'J2000'.
C
C$ Detailed_Output
C
C     SP,
C     DSP        are, respectively, the body-fixed coordinates of the
C                surface intercept point, and its instantaneous
C                velocity vector at time ET.
C
C                That is, if the ray with direction vector U,
C                emanating from the position of the observer,
C                intersects the target, AND if the velocity of
C                the intercept point is small enough to be computed,
C                then SP will be the body-fixed coordinates of the
C                intercept point, and DSP will be the instantaneous
C                velocity of that point on the surface of the target
C                body.  This velocity vector lies in the plane that
C                is tangent to the target body at SP.
C
C                If the ray in the direction of U misses the target,
C                or if the velocity of the intercept point is too large
C                to be computed, as when the intercept point is on the
C                limb where its velocity is infinite, SP and DSP are
C                undefined.
C
C                In the body-fixed coordinate system, the center of
C                the ellipsoid model of the target body is at the
C                origin, and the semi-axes of the ellipsoid are
C                aligned with the x, y, and z-axes.  SP and DSP
C                are referenced to this body-fixed frame.
C
C     FOUND      is a logical flag that is set to .TRUE. if both SP
C                exists and DSP can be computed, and otherwise, .FALSE.
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
C     2) If the pointing vector U is the zero vector, then
C        a subroutine that SMEARV_G calls will diagnose and
C        signal an error.
C
C     3) If the reference frame REFIN is not supported by the
C        current version of CHGIRF, an error is diagnosed and
C        signalled by a routine that SMEARV_G calls.
C
C     4) If no SPK ephemeris file has been loaded prior to calling
C        SMEARV_G, or if the SPK data has insufficient coverage, an
C        error will be diagnosed and signalled by a routine that
C        SMEARV_G calls.
C
C     5) If a PCK file containing the radii and rotation model constants
C        of the target body has not been loaded prior to calling
C        SMEARV_G, an error will be diagnosed and signalled by a
C        routine that SMEARV_G calls.
C
C     6) If SP is on or too close to the limb, FOUND will be returned
C        with the value .FALSE. and both SP and DSP will be unchanged.
C
C$ Files
C
C     No files are input to this routine, however, SMEARV_G expects
C     that the appropriate SPK and PCK files have been loaded using
C     SPKLEF and LDPOOL, respectively.
C
C$ Particulars
C
C     SMEARV_G determines the state of the observing body with respect
C     to the target body at the requested time.  It computes the
C     intercept point of a ray emanating from the position of the
C     observing body in the direction of the pointing vector, U,
C     with the target body.
C
C     SMEARV_G also returns the velocity vector of that intercept point
C     which is tangent to the target body's surface at the intercept
C     point.  This velocity includes the effects of the velocity of the
C     observer, the velocity of the pointing vector, the rotation of
C     the target body, and the light-time between the observing and
C     the target bodies.
C
C$ Examples
C
C     Suppose a camera on the Galileo Orbiter returned an image
C     of Earth in which the features were smeared.  In determining
C     the component of smear due to the motion of the feature,
C     you must first determine the component due to the motion
C     of the spacecraft, the camera boresight, and the planet --
C     SMEARV_G computes just that.  Smear due to actual motion
C     of the feature is what's left after this combination of other
C     smear components is removed.  You may also use the smear
C     velocity to determine the length of that smear.
C
C     The following code example computes the smear velocity vector
C     with respect to the Earth-fixed frame and its magnitude.  The
C     file 'T900927.BSP' is a binary SPK ephemeris file containing
C     data for the Galileo Orbiter, the Earth, the Moon, and the Sun
C     for the time period from November 1 through January 1, 1990.
C     'SSI00002.BC' is a binary CK file containing camera pointing
C     (not scan platform pointing) covering the same time interval.
C     'PCK00003.TPC' is a planetary constants kernel file, containing
C     radii and rotation model constants.  'GLL00002.TLS' is a
C     leapseconds file.  For brevity in this example, we will assume
C     that FOUND is .TRUE. for each routine that returns it.
C
C           CHARACTER*(20)        UTC
C
C           INTEGER               SSI
C           INTEGER               CKHAN
C           INTEGER               SC
C           INTEGER               SPKHAN
C           INTEGER               TARGET
C
C           DOUBLE PRECISION      AV     ( 3 )
C           DOUBLE PRECISION      CLKOUT
C           DOUBLE PRECISION      CMAT   ( 3, 3 )
C           DOUBLE PRECISION      DSP    ( 3 )
C           DOUBLE PRECISION      DU     ( 3 )
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      SCLKDP
C           DOUBLE PRECISION      SP     ( 3 )
C           DOUBLE PRECISION      TOL
C           DOUBLE PRECISION      U      ( 3 )
C           DOUBLE PRECISION      VNORM
C
C           LOGICAL               FOUND
C
C           DATA                  SSI  / -77036              /
C           DATA                  U    / 0.0D0, 0.0D0, 1.0D0 /
C
C     C
C     C     Load kernel files.
C     C
C           CALL LDPOOL ( 'GLL00002.TLS' )
C           CALL LDPOOL ( 'PCK00003.TPC' )
C           CALL SPKLEF ( 'T900927.BSP',  SPKHAN )
C           CALL CKLPF  ( 'SSI00002.BC',  CKHAN  )
C
C     C
C     C     Get body codes
C     C
C           CALL BODN2C_G ( 'Galileo Orbiter', SC,     FOUND )
C           CALL BODN2C_G ( 'Earth',           TARGET, FOUND )
C
C     C
C     C     Get pointing (C-matrix) and angular velocity.
C     C
C     C     Convert a UTC time string to encoded spacecraft clock time,
C     C     create a tolerance, and call CKGPAV.  Convert the encoded
C     C     spacecraft clock time tag of the C-matrix and angular
C     C     velocity vector to ET and then UTC.
C     C
C           CALL UTC2ET ( '1990 Dec 09 12:00:00', ET )
C           CALL SCE2T  ( SC, ET, SCLKDP )
C
C           CALL SCTIKS ( SC, '0:0:6:0', TOL )
C
C           CALL CKGPAV ( CAMERA,
C          .              SCLKDP,
C          .              TOL,
C          .              'J2000',
C          .              CMAT,
C          .              AV,
C          .              CLKOUT,
C          .              FOUND    )
C
C           CALL SCT2E  ( SC, CLKOUT, ET  )
C           CALL ET2UTC ( ET, 'C', 3, UTC )
C
C      C
C      C    Rotate the line-of-sight vector to inertial coordinates
C      C    and compute the velocity of that vector.
C      C
C           CALL MTXV  ( CMAT, U, U  )
C           CALL VCRSS ( AV,   U, DU )
C
C      C
C      C    Now we have all of the inputs.  Compute the smear velocity.
C      C
C           CALL SMEARV_G ( TARGET,
C          .                SC,
C          .                ET,
C          .                U,
C          .                DU,
C          .                'J2000',
C          .                SP,
C          .                DSP,
C          .                FOUND   )
C
C     C
C     C     Write the results.
C     C
C           WRITE (*,*) 'Smear velocity at ', UTC
C           WRITE (*,*)
C           WRITE (*,*) '        x = ', DSP (1)
C           WRITE (*,*) '        y = ', DSP (2)
C           WRITE (*,*) '        z = ', DSP (3)
C           WRITE (*,*) 'magnitude = ', VNORM ( DSP)
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
C-    GLLSPICE Version 1.0.0, 06-NOV-1990 (JEM)
C
C-&
 
C$ Index_Entries
C
C     smear velocity
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C     A state is a 6 dimensional array.  The first three components
C     represent a position, the second three represent the time
C     derivative of that position.  POS and VEL are indices into
C     a state array.
C
      INTEGER               POS
      PARAMETER           ( POS   = 1 )
 
      INTEGER               STDIM
      PARAMETER           ( STDIM = 6 )
 
      INTEGER               VEL
      PARAMETER           ( VEL   = 4 )
 
 
C
C     Local variables
C
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      STIBF  ( STDIM )
      DOUBLE PRECISION      STOBF  ( STDIM )
      DOUBLE PRECISION      STOI   ( STDIM )
      DOUBLE PRECISION      STUBF  ( STDIM )
      DOUBLE PRECISION      STUI   ( STDIM )
      DOUBLE PRECISION      STTI   ( STDIM )
      DOUBLE PRECISION      TSIBF  ( STDIM, STDIM )
 
      INTEGER               NRADII
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SMEARV_G' )
      END IF
 
C
C     Initialization
C
      FOUND = .FALSE.
 
C
C     Check the input body codes.  If they are equal, signal
C     an error.
 
      IF ( OBSRVR .EQ. TARGET ) THEN
 
         CALL SETMSG ( 'In computing smear velocity, the observing'   //
     .                 'and the target body are the same. ' )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)' )
         CALL CHKOUT ( 'SMEARV_G' )
         RETURN
 
      END IF
 
C
C     Get the apparent state of the target body as seen from the
C     observing body at ET in the same inertial frame as the pointing
C     vectors.
C
      CALL SPKEZ ( TARGET, ET, REFIN, 'LT', OBSRVR, STTI, LT )
 
C
C     Negate STTI so that it is the inertially referenced state of
C     the observer as seen by the apparent target.
C
      CALL VMINUG ( STTI, STDIM, STOI )
 
C
C     Now we need the state of the observer in target body-fixed
C     coordinates, STOBF:
C
C     Let POI and VOI be the position and velocity vectors of the
C     observer in inertial coordinate.  That is STOI = ( POI, VOI ).
C     Let TIBF be the 3x3 transformation matrix that rotates vectors
C     from inertial to body-fixed coordinates.  Then the position of
C     the observer in body-fixed coordinates is the product of TIBF and
C     POI:
C
C       POBF = TIBF * POI
C
C     The velocity of the observer in body-fixed coordinates is the
C     derivative of POBF,
C
C                d POBF    d (TIBF * POI)
C       VOBF  =  ------ =  -------------- = DTIBF * POI + TIBF * VOI
C                  dt          dt
C
C     where DTIBF is the derivative of the transformation matrix TIBF.
C     Thus the following 6x6 matrix, TSIBF, when multiplied by STOI,
C     transforms the entire state to body-fixed coordinates.
C
C                        +-           -+   +-   -+   +-    -+
C                        | TIBF     0  |   | POI |   | POBF |
C       TSIBF * STOI  =  |             | * |     | = |      | = STOBF
C                        | DTIBF  TIBF |   | VOI |   | VOBF |
C                        +-           -+   +-   -+   +-    -+
C
C
C     TISBOD generates this 6x6 matrix, TSIBF, to transform an inertial
C     state to body-fixed coordinates at a particular epoch.
C
      CALL TISBOD ( REFIN, TARGET, ET-LT, TSIBF  )
 
      CALL MXVG   ( TSIBF, STOI, STDIM, STDIM, STOBF )
 
C
C     We also need U and DU in body-fixed coordinates.  Pack
C     U and DU into a state, STUI, and multiply by TSIBF.
C
      CALL VEQU (  U, STUI ( POS ) )
      CALL VEQU ( DU, STUI ( VEL ) )
 
      CALL MXVG ( TSIBF, STUI, STDIM, STDIM, STUBF )
 
C
C     Now we get the radii of the ellipsoid model of the target
C     body.
C
      CALL BODVAR ( TARGET, 'RADII', NRADII, RADII )
 
C
C     Finally, we call the geometry routine SURFPV_G, which
C     takes the body-fixed states of the observer and
C     pointing vector and the ellipsoid model, and
C     computes the intercept point and the velocity of
C     that point on the surface of the ellipsoid.
C
      CALL SURFPV_G ( STOBF, STUBF,
     .                RADII(1), RADII(2), RADII(3),
     .                STIBF, FOUND )
 
      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'SMEARV_G' )
         RETURN
      END IF
 
C
C     STIBF, is the body-fixed state of the intercept point.
C
      CALL VEQU ( STIBF ( POS ),  SP )
      CALL VEQU ( STIBF ( VEL ), DSP )
 
      CALL CHKOUT ( 'SMEARV_G' )
      RETURN
      END
