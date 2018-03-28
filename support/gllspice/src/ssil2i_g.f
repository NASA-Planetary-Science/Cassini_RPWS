C$Procedure      SSIL2I_G  (SSI line and sample to inertial vector)
 
      SUBROUTINE SSIL2I_G ( L, S, SCLKIN, TOL, REF, VPOINT, SCLKPT,
     .                     FOUND )
 
C$ Abstract
C
C     Find the unit vector expressed relative to an inertial reference
C     frame that points from the SSI camera towards an object
C     referenced by a line and sample coordinate in the SSI FOV.
C
C$ Required_Reading
C
C     CK
C     GLL_IK
C     SCLK
C
C$ Keywords
C
C     COORDINATES
C     GEOMETRY
C     VECTOR
C
C$ Declarations
 
      CHARACTER*(*)         REF
 
      DOUBLE PRECISION      SCLKIN
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      L
      DOUBLE PRECISION      S
      DOUBLE PRECISION      VPOINT    ( 3 )
      DOUBLE PRECISION      SCLKPT
 
      LOGICAL               FOUND
 
      INTEGER               VER
      PARAMETER           ( VER = 1 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      L,
C      S         I    Line and sample coordinates.
C      REF       I    Inertial reference frame.
C      SCLKIN    I    Epoch at picture was taken.
C      TOL       I    C-kernel tolerance.
C      VPOINT    O    Unit vector pointing toward object.
C      FOUND     O    FOUND will be true if pointing is available.
C      SCLKPT    O    Time at which pointing is availble.
C      VER       P    Version of I-kernel needed.
C
C$ Detailed_Input
C
C     L,
C     S           are the line and sample coordinates in the SSI
C                 field of view.
C
C     REF         is one of the supported NAIF inertial reference
C                 frames.  See routine CHGIRF for a complete listing
C                 of supported reference frames.  The pointing vector,
C                 VPOINT, will be expressed relative to this reference
C                 frame.
C
C     SCLKIN      is the encoded spacecraft clock time at which the
C                 picture was taken.  It is this time (+/- TOL)
C                 at which a rotation matrix will try to be found to
C                 transform a vector from instrument to inertial fixed
C                 coordinates.
C
C     TOL         is an encoded tolerance time.
C
C$ Detailed_Output
C
C     VPOINT      is the unit vector which points from the SSI camera
C                 towards the object referenced by the line and sample
C                 coordinate in the SSI field of view.
C
C     SCLKPT      is the encoded spacecraft clock time at which
C                 pointing is available in the C-kernel.  If found
C                 is true, then this time will allways be within
C                 TOL ticks of SCLKIN.
C
C     FOUND       is true if a rotation matrix can be found which will
C                 transform an instrument fixed vector to an inertially
C                 fixed vector.  If FOUND is true, VPOINT will be the
C                 vector describe above.  If FOUND is false, VPOINT
C                 will be meaningless.
C
C$ Parameters
C
C      VER        is the version of the SSI I-kernel that must be
C                 loaded into the kernel pool.  That is, the
C                 I-kenrel must be in the form VER.X.
C
C$ Exceptions
C
C     1)          If any SSI instrument data is not found in the
C                 kernel pool, the error SPICE(KERNELVARNOTFOUND) is
C                 signalled.
C
C     2)          A C-kernel file must be loaded using CKLPF prior to
C                 calling this routine or an error will be signalled by
C                 a routine that this routine calls.
C
C     3)          If the line and sample coordinates given as input are
C                 not within the SSI FOV as specified in the SSI
C                 instrument kernel, an error is signalled by a routine
C                 that this routine calls.
C
C     2)          If the SSI I-kernel currently in the kernel pool is
C                 not compatible with this routine the error
C                 SPICE(IKERNELNOTCOMPAT) is signalled.
C
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine finds the unit vector, expressed in the specified
C     reference frame, that points toward the object referenced by the
C     line and sample coordinate.  This subroutine requires that an SSI
C     I-kernel with the version in the form VER.X is loaded in the
C     kernel pool prior to calling this routine.  In addition, a
C     C-kernel file containing pointing data for the Galileo scan
C     platform during the epoch SCLKIN must be loaded by a call to
C     CKLPF prior to calling this routine.
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
C     The SSI I-kernel version must be in the form VER.X.
C
C$ Literature_References
C
C     1. ``SSI Distortion and Twist Models for NAIF,'' JPL IOM, by
C        Ken Klaasen, 7/2/90.
C
C     2. ``Formula for SSI Geometric Distortion Correction,'' JPL
C        IOM, by Ken Klaasen, 7/21/88.
C
C        The two documents [1] and [2] comprise NAIF document
C        number 202.0.
C
C$ Author_and_Institution
C
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    GLLSPICE Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 1.0.0, 03-OCT-1990 (MJS)
C
C-&
 
C$ Index_Entries
C
C     ssi line and sample to inertial vector
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      PI
 
C
C     Local parameters
C
 
C
C     MAXLEN      is the maximum length of the variable names
C                 that can be stored in the kernel pool.
C
C     NVARS       is the number of variables read from the
C                 kernel pool.
C
C     MAXMSG      is the maximum length of an error message listing
C                 data items that are not found in the kernel pool.
C
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32           )
 
      INTEGER               NVARS
      PARAMETER           ( NVARS  = 4            )
 
      INTEGER               MAXMSG
      PARAMETER           ( MAXMSG = NVARS*MAXLEN )
 
C
C     Local variables
C
      DOUBLE PRECISION      TWIST
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      IKVER
      DOUBLE PRECISION      FOCLEN
      DOUBLE PRECISION      PSIZE
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      R1     ( 3, 3 )
      DOUBLE PRECISION      R2     ( 3, 3 )
      DOUBLE PRECISION      R21    ( 3, 3 )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
 
      INTEGER               I
      INTEGER               IAXIS
      INTEGER               INST
      INTEGER               N
 
      CHARACTER*(MAXLEN)    SSIDAT ( NVARS )
      CHARACTER*(MAXMSG)    OUTERR
 
      LOGICAL               QUIT
      LOGICAL               THERE  ( NVARS )
      LOGICAL               VTHERE
 
C
C     Saved variables
C
      SAVE  SSIDAT
 
C
C     Initial values
C
      DATA  SSIDAT        / 'INS-77036_TWIST_OFFSET',
     .                      'INS-77036_PIXEL_SIZE',
     .                      'INS-77036_FOCAL_LENGTH',
     .                      'INS-77036_VERSION'        /
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SSIL2I_G' )
      END IF
 
C
C     First map line and sample coordinates to millimeter space (X,Y)
C     coordinates.  The units of X and Y are returned in pixels.
C
      CALL SSIL2X_G ( L, S, X, Y )
 
C
C     The origin of the x and y coordinate system is at the center of
C     the field of view.  If '<a,b>' denotes a vector with components
C     a and b, then the vector <x,y> is a vector pointing from the
C     center of the field of view to the coordinate (x,y) as shown as
C     the dotted line in the figure below.
C
C                         ------------------------------------------
C                        /                                         /
C                       /                               +x ---    /
C                      /                                     /   /
C                     /                                     /   /
C                    /                                     +y  /
C                   /                                         /
C                  /              x        Center of the     /
C                 /       _______________. field of view    /
C                /      y/         .                       /
C               /       /    .                            /
C              /       *                                 /
C             /      Image of                           /
C            /       object                            /
C           /                                         /
C          /                           Focal plane   /
C         /                                         /
C         ------------------------------------------
C
C
C     In order to find the inertial vector pointing from the camera
C     to the actual object we first need a vector, call it V, pointing
C     from the x,y coordinate through the focal point to the actual
C     object.  If we let the focal length of the camera be z, then the
C     vector <-x,-y,z> is this vector V.  The figure below may help you
C     picture this (Hint: Put on your 3-D glasses).  The dotted line is
C     the vector <-x,-y,z>; it points from the image of the object to
C     the actual object.
C
C
C                                                   Object
C                                                  *
C                                                .
C                                              .              +z
C                         -------------------.----------------|-----
C                        /                 .                  |    /
C                       /                .              +x ___|   /
C                      /               .                     /   /
C                     /              . |                    /   /
C                    /             .   |                   +y  /
C                   /            .     | z (focal length)     /
C                  /           .       |                     /
C                 /       ___._________|                    /
C                /      y/ .     x     (x=y=z=0)           /
C               /       /.                                /
C              /       *                                 /
C             /      Image of                           /
C            /       object                            /
C           /                                         /
C          /                          Focal plane    /
C         /                                         /
C         ------------------------------------------
C
C
C
C     In order to transform the components of a vector expressed in
C     insturment fixed coordinates (x,y,z) to components expressed in
C     inertial frame coordinates (X,Y,Z) the following sequence of
C     rotations must be performed.
C
C         [       ]       [       ]     [ x ]      [ X ]
C         |   R2  |   *   |   R1  |  *  | y |  =   | Y |
C         [       ]       [       ]     [ z ]      [ Z ]
C
C         Scan Platform    Instrument
C         to               to
C         Inertial         Scan Platform
C
C     R1 is a rotation matrix generated from information in the
C     I-kernel.  It describes the geometric relationship between the
C     SSI coordinate system and the coordinate system of the scan
C     platform, the GLL spacecraft structure on which the SSI
C     instrument is mounted.
C
C     R2, unlike R1, is a time dependent rotation matrix.
C     It describes the gemoetric relationship between the scan platform
C     coordinate system and an inertial reference frame coordinate
C     system at a given time.  R2 is obtained from a GLL C-kernel
C     containing pointing for the scan platform.  The rotation matrix
C     returned from the C-kernel, the C-matrix, describes pointing from
C     an inertial frame to the scan platform.  R2, thus, is the inverse
C     (or transpose--since it is a rotation matrix) of the C-matrix.
C
 
C
C     Check version number of I-kernel.
C
      CALL RTPOOL ( SSIDAT(4), N, IKVER, VTHERE )
 
      IF ( .NOT. VTHERE ) THEN
         CALL SETMSG ( 'SSI I-kernel version not found'            )
         CALL SIGERR ( 'SPICE(KERVARNOTFOUND)'                     )
         CALL CHKOUT ( 'SSIL2I_G'                                  )
         RETURN
      END IF
 
      IF ( INT(IKVER) .NE. VER ) THEN
         CALL SETMSG ( 'SSI I-kernel version # not compatible'     )
         CALL ERRDP  ( '#', IKVER                                  )
         CALL SIGERR ( 'SPICE(IKERNELNOTCOMPAT)'                   )
         CALL CHKOUT ( 'SSIL2I_G'                                  )
         RETURN
      END IF
 
C
C     Get the focal length and the twist offset from the kernel pool.
C     Signal an error if any information is not found and list what was
C     not found.
C
      CALL RTPOOL (SSIDAT(1), N, TWIST,  THERE(1))
      CALL RTPOOL (SSIDAT(2), N, PSIZE,  THERE(2))
      CALL RTPOOL (SSIDAT(3), N, FOCLEN, THERE(3))
 
      QUIT = .FALSE.
      OUTERR = ' '
 
      DO 50001
     .   I = 1, NVARS - 1
 
         IF ( .NOT. THERE(I) ) THEN
            QUIT = .TRUE.
            CALL SUFFIX ( SSIDAT(I), 2, OUTERR )
         END IF
 
50001 CONTINUE
 
      IF ( QUIT ) THEN
 
         CALL SETMSG ( 'Did not find # item(s) in kernel pool'     )
         CALL ERRCH  ( '#', OUTERR                                 )
         CALL SIGERR ( 'SPICE(KERVARNOTFOUND)'                     )
         CALL CHKOUT ( 'SSIL2I_G'                                  )
         RETURN
 
      END IF
 
C
C     Initialize variables.
C
C     The scan platform has an instrument ID of -77001.
C
      INST  = -77001
 
C
C     Convert X and Y from pixels to mm.
C
      X = X * PSIZE
      Y = Y * PSIZE
C
C     Pack -X, -Y, and the focal length (the Z deminsion -- also in mm)
C     into a 3-D vector.
C
      CALL VPACK (-X, -Y, FOCLEN, VPOINT)
 
C
C     Next, find the rotation matrix which transforms components of
C     a vector expressed in SSI coordinates to components expressed in
C     scan platform coordinates.  This is the rotation matrix R1 as
C     mentioned above.
C     The relationship between the xyz frame and the LMN frame is
C     defined as shown in the figure below.
C
C                         + L, + z
C
C                             |
C                             |
C                             |
C                             |     /
C                             |    / -  -  -   . + y
C                             |   /        . `
C                             |  /     . `   /
C                             | /  . `
C                             |. `         /
C                             -----------------------  + N
C                            / .              /
C                           /   .
C                          /     .          /
C                         /       .
C                        / < - -   .      /
C                       /  THETA    .
C                      /             .  /
C                     /  _  _  _  _   .
C                    /                 + x
C
C                 + M
C
C            A negative rotation about +z axis through angle
C            THETA to produce M, N, and L axes where THETA
C            is PI+TWIST.
C
C
C     The rotation is then simply:
C
C     .-   -.     .-                               -.  .-   -.
C     |  M  |     |  cos(THETA)   - sin(THETA)   0  |  |  x  |
C     |  N  |  =  |  sin(THETA)     cos(THETA)   0  |  |  y  |
C     |  L  |     |  0              0            1  |  |  z  |
C     `-   -'     `-                               -'  `-   -'
C     Scan        Rotation matrix = R1                 Millimeter
C     Platform                                         Coordinates
C     Coordinates
C
C
C     The routine ROTATE will be used to calculate the rotation
C     matrix.  The rotation to the LMN coordinate system is
C     a negative rotation about the z axis, therefore, ROTATE must
C     apply a rotation of MINUS THETA radians about the z axis.
C
C     R1 will be the rotation matrix as shown above.
C
      IAXIS = 3
      THETA = (PI() + TWIST)
      CALL ROTATE ( -THETA, IAXIS, R1 )
 
C
C     The next step is to find the C-matrix.  Only the C-matrix is
C     needed (as opposed to a C-matrix AND angular velcity data),
C     therefore, the CK reader routine, CKGP (CK get pointing), is used.
C     SCLKPT is the encoded spacecraft clock time associated with
C     the returned C-matrix.   This value may differ from SCLKIN,
C     but never by more than the input tolerance.
C
      CALL CKGP ( INST, SCLKIN, TOL, REF, CMAT, SCLKPT, FOUND )
 
      CALL XPOSE ( CMAT, R2 )
 
C
C     If pointing was not found,
C     no use staying around.
C
      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'SSIL2I_G' )
         RETURN
      END IF
 
C
C     Now multiply the rotation matrix that rotates coordinates from
C     instrument to scan platform (R1) by the transpose of the C-matrix
C     (R2).  The output rotation matrix (R21) will be able to transform
C     components of a vector expressed in xyz coordinates to
C     components expressed in REF coordinates.
C
      CALL MXM (R2, R1, R21 )
 
C
C     Express VPOINT in REF coordinates.
C
      CALL MXV ( R21, VPOINT, VPOINT )
C
C     Unitize the vector.
C
      CALL VHAT ( VPOINT, VPOINT )
 
C
C     We're all finished.
C
      CALL CHKOUT ( 'SSIL2I_G' )
      RETURN
      END
 
