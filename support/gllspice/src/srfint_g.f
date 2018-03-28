C$Procedure      SRFINT_G  (Surface intercept point)
 
      SUBROUTINE SRFINT_G ( IV, REF, ET, OBS, BODY, RADIUS, LONG,
     .                      LAT, FOUND )
 
C$ Abstract
C
C     Find the planetocentric coordinates of the point on a body where
C     an inertial vector from an observer intersects the surface.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     COORDINATES
C     GEOMETRY
C
C$ Declarations
 
      DOUBLE PRECISION      IV    ( * )
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      INTEGER               OBS
      INTEGER               BODY
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      LAT
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      IV        I   Inertial vector from anobserver in some direction.
C      ET        I   Ephemeris time.
C      REF       I   Reference frame of inertial vector.
C      OBS       I   Observing body.
C      BODY      I   Target body.
C      RADIUS    O   Distance to the point from the origin.
C      LONG      O   Angle to the point from the XZ plane in radians.
C      LAT       O   Angle to the point from the XY plane in radians.
C      FOUND     O   True if IV points at the body.
C
C
C$ Detailed_Input
C
C     IV         is the vector from the observer in some direction.
C                Presumably, this vector points towards some point on
C                the body.
C
C     REF        is the reference frame of the inertial vector.  See
C                spicelib subroutine CHGIRF for possible referece
C                frames.
C
C     OBS        is the ID code of the observing body, for example -77.
C
C     BODY       is the ID code of the body for which the surface
C                intercept is requested. Bodies are numbered according
C                to the standard NAIF numbering scheme.
C
C     ET         is the ephemeris time at which the surface intercept
C                point is to be computed.
C
C$ Detailed_Output
C
C     RADIUS     is the distance to the point from the origin of the
C                body fixed XYZ coordinate frame.
C
C     LONG       is the angle to the point from the XZ plane in radians.
C
C     LAT        is the angle to the point from the XY plane in radians.
C
C     FOUND      is a logical flag indicating wether or not the
C                vector from OBS with direction IV intersects
C                BODY.  If the vector does intersect the body, FOUND
C                will be true. If the vector does not intersect the
C                body, FOUND will be false.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the reference frame REF is not supported by the
C         current version of CHGIRF, an error is diagnosed and
C         signalled by a routine that SRFINT_M calls.
C
C     2)  If a PCK file containing the radii and rotation model
C         constants of the target body has not been loaded prior to
C         calling SRFINT_G, an error will be diagnosed and signalled
C         by a routine that SRFINT_G calls.
C
C     3)  If the SPK file (or files) do not contain sufficient data
C         to determine the apparent state (position and velocity) of
C         the body relative to the observer an error is signalled by
C         a routine that this routine calls.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     A planetary constants kernel file must be loaded into the pool
C     by a call to LDPOOL prior to calling this routine.  In addition,
C     an ephemeris file that contains ephemeris for the observer and
C     target at the given epoch must be loaded by a call to SPKLEF
C     prior to calling this routine.
C
C$ Examples
C
C     The following program determines the coordinates of a point
C     on a planet referenced by a line and sample coordinate in the
C     SSI field of view.
C
C           PROGRAM SURFACE_POINT
C
C     C     Compute the planetocentric latitude, longitude, and
C     C     radius coordinates of the point on a target body
C     C     referenced by a line-sample coordinate on a picture
C     C     taken by the SSI camera.
C     C
C     C     The following files are required:
C     C
C     C        1) Kernel file containing planetary constants.
C     C        2) Kernel file containing spacecraft clock coefficients.
C     C        3) SPK file containing planetary and spacecraft ephemeris
C     C           data.
C     C        4) CK file containing instrument pointing data.
C     C        5) I-kernel file containing SSI geometric and optical
C     C           data.
C     C        6) Kernel file containing leapseconds and relativistic
C     C           terms.
C     C
C     C     The following quantities are required:
C     C
C     C        1) NAIF integer planet ID
C     C        2) UTC time when the picture from the SSI camera
C     C           was taken.
C     C
C     C     The following steps are taken to locate the desired point
C     C
C     C        1) Given a line-sample coordinate, an inertial vector
C     C           is found that points from the SSI camera towards the
C     C           point on the planet.
C     C
C     C        2) The vector is passed to routine SRFINT_G along with
C     C           the NAIF integer code of the spacecraft, the NAIF
C     C           integer code of the target body, and the ephemeris
C     C           time.  The output of SRFINT_G is the surface
C     C           intercept point expressed in planetocentric latitude,
C     C           longitude, and radius coordinates.
C     C
C     C$ Declarations
C
C           CHARACTER*(80)        FILE
C           CHARACTER*(80)        UTC
C
C           INTEGER               HANDL1
C           INTEGER               HANDL2
C           INTEGER               OBS
C           INTEGER               INST
C           INTEGER               TARG
C           INTEGER               N
C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      IV      ( 3 )
C           DOUBLE PRECISION      L
C           DOUBLE PRECISION      LAT
C           DOUBLE PRECISION      LONG
C           DOUBLE PRECISION      RADIUS
C           DOUBLE PRECISION      S
C           DOUBLE PRECISION      SCLKIN
C           DOUBLE PRECISION      SCLKPT
C           DOUBLE PRECISION      TOL
C
C           LOGICAL               FOUND
C
C     C
C     C     Initial values.
C     C
C
C     C
C     C     Spacecraft and instrument ID's.
C     C
C           DATA   OBS         /  -77     /
C
C     C
C     C     Get all the files and load them.
C     C
C           CALL CLPOOL
C
C           WRITE (*,*) 'Enter the name of the kernel file '         //
C          .            'containing planetary constans'
C           READ  (*,FMT='(A)') FILE
C
C           CALL LDPOOL ( FILE )
C
C           WRITE (*,*) 'Enter the name of the kernel file '         //
C          .            'containing SCLK coefficients'
C           READ  (*,FMT='(A)') FILE
C
C           CALL LDPOOL ( FILE )
C
C           WRITE (*,*) 'Enter the name of the SPK file containing ' //
C          .            'planetary and spacecraft ephemeris'
C           READ  (*,FMT='(A)') FILE
C
C           CALL SPKLEF ( FILE, HANDL1 )
C
C           WRITE (*,*) 'Enter the name of the CK file containing '  //
C          .            'instrument pointing'
C           READ  (*,FMT='(A)') FILE
C
C           CALL CKLPF ( FILE, HANDL2 )
C
C           WRITE (*,*) 'Enter the name of the I-kernel file '       //
C          .            'containing instrument geometric'
C           WRITE (*,*) 'and optical data'
C           READ  (*,FMT='(A)') FILE
C
C           CALL LDPOOL ( FILE )
C
C           WRITE (*,*) 'Enter the name of the kernel file'          //
C          .            'containing leapseconds and'
C           WRITE (*,*) 'relativistic terms'
C           READ  (*,FMT='(A)') FILE
C
C           CALL LDPOOL ( FILE )
C
C     C
C     C     Get the line and sample coordinate.
C     C
C           WRITE (*,*) 'Enter line and sample coordinate ( L,S )'
C           READ  (*,*) L, S
C
C     C
C     C     Get the integer ID for the planet.
C     C
C           WRITE (*,*) 'Enter NAIF integer ID for the planet'
C           READ  (*,*) TARG
C
C     C
C     C     What time was the picture taken?
C     C
C           WRITE (*,*) 'Enter time at which picture was taken'
C           WRITE (*,*) '-- UTC format --'
C           READ  (*,FMT='(A)') UTC
C           WRITE (*,*) 'Enter tolerance in SCLK format'
C           READ  (*,FMT='(A)') TOLCH
C
C     C
C     C     Convert UTC to ET, then to ticks.  Also convert TOLCH to
C     C     to ticks.
C     C
C           CALL UTC2ET ( UTC, ET )
C           CALL SCE2T  ( OBS, ET, SCLKIN )
C
C           CALL SCTIKS ( OBS, TOLCH, TOL )
C
C     C
C     C     Get the inertial vector in J2000 coordinates pointing
C     C     from the SSI camera to the point on the planet.
C     C
C           CALL SSIL2I_G ( L, S, SCLKIN, TOL, 'J2000', IV, FOUND )
C
C           IF (.NOT. FOUND) THEN
C              WRITE (*,*) 'Could not find pointing in the C-kernel'
C              STOP
C           END
C
C     C
C     C     Now get surface intercept point.
C     C
C           CALL SRFINT_G  ( IV, 'J2000', ET, OBS, TARG, RADIUS, LONG,
C          .                 LAT, FOUND )
C
C     C
C     C     What are the results?
C     C
C           IF ( .NOT. FOUND ) THEN
C              WRITE (*,*) 'Point is not on the body'
C           ELSE
C              WRITE (*,*) 'Point is at body coordinates:'
C              WRITE (*,*) 'Radius    :', RADIUS
C              WRITE (*,*) 'Longitude :', LONG
C              WRITE (*,*) 'Latitude  :', LAT
C           END IF
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
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    GLLSPICE Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 2.0.0, 06-AUG-1991 (MJS)
C
C        The variable VTARG is now declared as a 6-vector. Two new
C        exceptions were added to the exceptions section: the case of
C        an invalid REF, and the case when a PCK (planetary constants)
C        file is not loaded in the kernel pool. One exception was
C        removed: the case of an invalid body.
C
C-    GLLSPICE Version 1.0.0, 12-NOV-1990 (MJS)
C
C-&
 
C$ Index_Entries
C
C     surface intercept point
C
C-&
 
 
C$ Revisions
C
C-    GLLSPICE Version 2.0.0, 06-AUG-1991 (MJS)
C
C        The variable VTARG is now declared as a 6-vector; previously,
C        VTARG was declared as a 3-vector. Two new exceptions were
C        added to the exceptions section: the case of an invalid REF,
C        and the case when a PCK (planetary constants) file is not
C        loaded in the kernel pool. One exception was removed: the
C        case of an invalid body (if there is no ephemeris for a given
C        body, the SPK readers will signal an error).
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               EQSTR
 
C
C     Local variables
C
      DOUBLE PRECISION      R      ( 3    )
      DOUBLE PRECISION      IVJ    ( 3    )
      DOUBLE PRECISION      ROTMAT ( 3, 3 )
      DOUBLE PRECISION      TAU
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      VPOS   ( 3    )
      DOUBLE PRECISION      VSURF  ( 3    )
      DOUBLE PRECISION      VTARG  ( 6    )
 
      INTEGER               N
      INTEGER               REFID
      INTEGER               JID
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SRFINT_G' )
      END IF
 
C
C     If IV, the inertial vector, is not in J2000 coordinates,
C     change it.
C
      IF ( .NOT. EQSTR (REF, 'J2000') ) THEN
 
         CALL IRFNUM ( REF,     REFID )
         CALL IRFNUM ( 'J2000', JID   )
         CALL IRFROT ( REFID,   JID,  ROTMAT )
         CALL MXV    ( ROTMAT,  IV,   IVJ    )
 
      ELSE
 
         CALL MOVED ( IV, 3, IVJ )
 
      END IF
 
C
C     First get the apparent state (position and velocity) of the
C     target body relative to the observer.  Use light time
C     correction.  VTARG is the vector pointing from the observer to
C     the body.  TAU is the light time.
C
      CALL SPKEZ ( BODY, ET, 'J2000', 'LT', OBS, VTARG, TAU )
 
C
C     Get the TIPM matrix and radii of target ellipsoid moodel.
C
C     TIPM is the transformation matrix from Inertial (J2000) to
C     body Equator and Prime Meridian.
C
C     We need TIPM for the target as it appeared when the instrument
C     took its measurment at time ET.  The target was at its apparent
C     location TAU seconds earlier.
C
C     BODMAT and BODVAR will read constants from the planetary constants
C     kernel file.
C
      CALL BODMAT ( BODY,  ET-TAU, TIPM )
      CALL BODVAR ( BODY, 'RADII', N, R )
 
C
C     The position of the observer is just the negative of the position
C     part of the spacecraft-target vector, VTARG.
C
      CALL VMINUS ( VTARG, VPOS )
 
C
C     Put both vectors in body-fixed coordinates.
C
      CALL MXV ( TIPM, VPOS, VPOS )
      CALL MXV ( TIPM, IVJ,  IVJ  )
 
C
C     Compute the point of intersection, if any.
C
      CALL SURFPT ( VPOS, IVJ, R(1), R(2), R(3), VSURF, FOUND )
 
C
C     If not found no use staying around.
C
      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'SRFINT_G' )
         RETURN
      END IF
 
C
C     Convert intersection point from rectangular to lat-lon-radius
C     coordinates.
C
      CALL RECLAT ( VSURF, RADIUS, LONG, LAT )
 
C
C     That's all.
C
      CALL CHKOUT ( 'SRFINT_G' )
      RETURN
      END
 
