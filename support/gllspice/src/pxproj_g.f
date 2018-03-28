C$Procedure      PXPROJ_G (Pixel Projection)
 
      SUBROUTINE PXPROJ_G ( L, S, BODY, SCLK, TOL, DL, DS, SCLKPT,
     .                      FOUND )
 
C$ Abstract
C
C     Find the size of the projection of a pixel in the SSI FOV on
C     the surface of a body.
C
C$ Required_Reading
C
C     CK
C     GLL_IK
C
C$ Keywords
C
C     GEOMETRY
C     VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION      L
      DOUBLE PRECISION      S
      INTEGER               BODY
      CHARACTER*(*)         SCLK
      CHARACTER*(*)         TOL
      DOUBLE PRECISION      DL
      DOUBLE PRECISION      DS
      CHARACTER*(*)         SCLKPT
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     L,
C     S          I   Line and sample coordinates.
C     BODY       I   NAIF integer code of body.
C     SCLK       I   Spacecraft clock time.
C     TOL        I   C-kernel tolerance.
C     DL         O   Length of pixel projection in line direction.
C     DS         O   Length of pixel projection in sample direction.
C     SCLKPT     O   Time at which pointing is available.
C     FOUND      O   True if pixel covers body.
C
C$ Detailed_Input
C
C     L,
C     S          are the line and sample coordinates of a pixel in the
C                SSI field of view, presumably showing some portion
C                of BODY.
C
C     BODY       is the NAIF integer ID of the body.
C
C     SCLK       is the spacecraft clock time at which the
C                SSI picture was taken.
C
C     TOL        is the tolerance used to look up pointing in
C                the C-kernel at time SCLK.
C
C$ Detailed_Output
C
C     DL         is the dimension of the pixel's projection on the
C                surface of the body in the line direction.  Units
C                are kilometers.
C
C     DS         is the dimension of the pixel's projection on the
C                surface of the body in the sample direction.  Units
C                are kilometers.
C
C     SCLKPT     is the spacecraft clock time at which pointing is
C                available in the C-kernel.  If pointing is found then
C                this time will always be within the tolerance specified
C                by TOL.  If pointing is not found an error is
C                signalled (see Exceptions).
C
C     FOUND      is true if the projection of the pixel is on the
C                body at time SCLKPT.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If no pointing is found in the C-matrix at time
C          SCLK +/- TOL, the error SPICE(NOPOINTING)
C          is signalled.
C
C     2)   If the line and sample coordinates do not fall within the
C          SSI field of view, an error is signalled by a routine that
C          this routine calls.
C
C$ Files
C
C     See the Particulars section below for the files that
C     must be loaded prior to calling this routine.
C
C$ Particulars
C
C     This routine finds the dimensions of a pixels projection on the
C     surface of a body.  Let the figure below represent a blow-up of
C     the pixel referenced by the input line and sample coordinates.
C     The points A,B,C and D represent points on the pixel with the
C     coordinates given below.  If we let A',B',C' and D' be the
C     projection on the body of A,B,C and D respectively, this
C     routine finds the distance between A' and B' (DS) and the
C     distance between C' and D' (DL).
C
C
C
C                                                       A = (L, S-0.5)
C                         ...........C...........       B = (L, S+0.5)
C                         .                     .       C = (L-0.5, S)
C          ----> sample   .                     .       D = (L+0.5, S)
C         |               .                     .
C         |               .                     .
C         |               A          +          B
C         V               .           (L,S)     .
C        line             .                     .
C                         .                     .
C                         .                     .
C                         ...........D...........
C
C
C     FOUND will be false if any of the points A', B', C' or D' can
C     not be found.
C
C     A Galileo SPK file containing ephemeris data for the body
C     must be opened by a call to SPKLEF prior to calling this
C     routine.
C
C     A Galileo CK file containing pointing for the Galileo scan
C     platform must be opened by a call to CKLPF prior to calling
C     this routine.
C
C     The following files must be loaded into the kernel pool by a
C     call to LDPOOL prior to calling this routine.
C
C       - An SSI I-kernel with a version in the form 1.X.
C       - A planetary constants kernel file.
C       - A Galileo SCLK kernel file.
C
C$ Examples
C
C     The following program shows how this routine may be used in a
C     typical application.
C
C
C           CHARACTER*(1)         ANSWER
C           CHARACTER*(80)        CK
C           CHARACTER*(80)        SCLK
C           CHARACTER*(80)        SCLKPT
C           CHARACTER*(80)        SPK
C           CHARACTER*(80)        TOL
C
C           DOUBLE PRECISION      L
C           DOUBLE PRECISION      S
C           DOUBLE PRECISION      DL
C           DOUBLE PRECISION      DS
C
C           INTEGER               HANDL1
C           INTEGER               HANDL2
C
C           LOGICAL               MORE
C     C
C     C     Spicelib functions
C     C
C           LOGICAL               EQSTR
C
C     C
C     C     Request some input from user.
C     C
C           WRITE (*,*) 'Enter time at which picture was '         //
C          .            'taken (SCLK FORMAT)'
C           READ  (*,FMT='(A)') SCLK
C
C           WRITE (*,*) 'Enter name of an SPK kernel containing '  //
C          .            'ephemeris data for Earth and Galileo at ' //
C          .            'this time.'
C           READ  (*,FMT='(A)') SPK
C           CALL SPKLEF ( SPK, HANDL1 )
C
C           WRITE (*,*) 'Enter name of an CK file containing '    //
C          .            'pointing for the scan platform at this ' //
C          .            'this time.'
C           READ  (*,FMT='(A)') CK
C           CALL CKLPF  ( CK,  HANDL2 )
C
C           WRITE (*,*) 'Enter C-kernel tolerance. (SCLK FORMAT)'
C           READ  (*,FMT='(A)') TOL
C
C     C
C     C     Load the GLL SCLK coefficients file, the planetary
C     C     constants kernel file and the SSI I-kernel file into
C     C     kernel pool.  Assume the files are in the default
C     C     directory.
C     C
C           CALL LDPOOL ('GLL00002.TSC')
C           CALL LDPOOL ('PCK00003.TPC')
C           CALL LDPOOL ('GLL36001.TI' )
C
C     C
C     C     Now request the coordinates of the pixel and determine
C     C     the dimensions of its projection on Earth.
C     C
C           MORE = .TRUE.
C
C           DO WHILE ( MORE )
C
C              WRITE (*,*) 'Enter line coordinate'
C              READ  (*,*) L
C
C              WRITE (*,*) 'Enter sample coordinate'
C              READ  (*,*) S
C
C              CALL PXPROJ_G ( L, S, 399, SCLK, TOL, DL, DS, SCLKPT,
C          .                   FOUND )
C
C              IF ( .NOT. FOUND ) THEN
C                 WRITE (*,*) 'Pixel''s projection is not on body'
C              ELSE
C                 WRITE (*,*) 'Pixel''s projection is :'
C                 WRITE (*,*) '   DL = ', DL
C                 WRITE (*,*) '   DS = ', DS
C              END IF
C
C              WRITE (*,*) 'Do you want to enter another line and ' //
C          .               'sample coordinate? (Y=YES)'
C              READ  (*,FMT='(A)') ANSWER
C
C              IF ( EQSTR(ANSWER, Y) ) THEN
C                 MORE = .TRUE.
C              ELSE
C                 MORE = .FALSE.
C              END IF
C
C           END DO
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
C-    GLLSPICE Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 1.0.0, 28-NOV-1990 (MJS)
C
C-&
 
C$ Index_Entries
C
C     pixel projection
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      SPHSD
 
C
C     Local parameters
C
      INTEGER               OBS
      PARAMETER           ( OBS = -77 )
C
C     Local variables
C
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      IV      ( 3 )
      DOUBLE PRECISION      LAT     ( 4 )
      DOUBLE PRECISION      LINE    ( 4 )
      DOUBLE PRECISION      LONG    ( 4 )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      SAMPLE  ( 4 )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      SCLKOT
      DOUBLE PRECISION      TOLDP
      INTEGER               I
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PXPROJ_G' )
      END IF
 
C
C     The first task of this routine is to find the latitudinal and
C     longitudinal coordinates of the four points A', B', C' and D'.
C     See the particulars section above for the definitions of these
C     points.  The two subroutines that will solve this task are
C     SSIL2I_G and SRFINT_G.
C
C     Subroutine SSIL2I_G finds a unit inertial vector that points
C     from the SSI camera towards a point in space referenced by a
C     line and sample coordinate in the SSI field of view.
C
C     Subroutine SRFINT_G extrapolates the inertial vector to
C     determine where (if at all) it intersects a given body.
C
C     The four line and sample coordinates which will be given as input
C     to SSIL2I_G will be:
C
C                   (L, S-0.5) = A
C                   (L, S+0.5) = B
C                   (L-0.5, S) = C
C                   (L+0.5, S) = D
C
C     Hereafter, any reference to these coordinates will be by their
C     corresponding letters.
C
C     The first time SSIL2I_G is called coordinates A are given as
C     input.  If an inertial vector is found, that is if pointing is
C     available in the C-matrix, SRFINT_G tries to find the latitude
C     and longitude coordinates of the surface intercept point on the
C     body.  If the inertial vector intercepts the body the latitudinal
C     coordinate is stored in the first position in the array LAT and
C     the longitudinal coordinate is stored in the same position in the
C     array LONG.   If the inertial vector does not intercept the body
C     FOUND will be given the value of false and this subroutine will
C     exit.
C
C     A similiar process is repeated for coordinates B, C, and D.  If
C     all goes well, the array LAT will look like this when finished
C
C            ------
C           | LAT  |
C           |    A'|
C            ------
C           | LAT  |
C           |    B'|
C            ------
C           | LAT  |
C           |    C'|
C            ------
C           | LAT  |
C           |    D'|
C            ------
C
C     and the array LONG will look like
C
C           -------
C          | LONG  |
C          |     A'|
C           -------
C          | LONG  |
C          |     B'|
C           -------
C          | LONG  |
C          |     C'|
C           -------
C          | LONG  |
C          |     D'|
C           -------
C
 
C
C     First need to encode SCLK and TOL.
C
      CALL SCENCD ( OBS, SCLK, SCLKDP )
      CALL SCTIKS ( OBS, TOL,  TOLDP  )
 
C
C     Initialize coordinates A, B, C, and D.
C
      LINE(1)   = L
      LINE(2)   = L
      LINE(3)   = L-0.5
      LINE(4)   = L+0.5
 
      SAMPLE(1) = S-0.5
      SAMPLE(2) = S+0.5
      SAMPLE(3) = S
      SAMPLE(4) = S
 
C
C     Find the latitude and longitude of the four coordinates.
C
      DO 50001
     .   I = 1, 4
 
         CALL SSIL2I_G ( LINE(I), SAMPLE(I), SCLKDP, TOLDP, 'J2000',
     .                   IV, SCLKOT, FOUND )
 
         IF ( FOUND ) THEN
 
            CALL SCT2E ( OBS, SCLKOT, ET )
 
            CALL SRFINT_G ( IV, 'J2000', ET, OBS, BODY, RADIUS,
     .                      LONG(I), LAT(I), FOUND )
 
            IF ( .NOT. FOUND ) THEN
               CALL CHKOUT ( 'PXPROJ_G' )
               RETURN
            END IF
 
         ELSE
 
            CALL SETMSG ( 'Pointing is not available in the '    //
     .                    'C-matrix at time # with a tolerance ' //
     .                    'of #'                                )
            CALL ERRCH  ( '#', SCLK                             )
            CALL ERRCH  ( '#', TOL                              )
            CALL SIGERR ( 'SPICE(NOPOINTING)'                   )
            CALL CHKOUT ( 'PXPROJ_G'                            )
            RETURN
 
         END IF
 
50001 CONTINUE
 
      CALL SCDECD ( OBS, SCLKOT, SCLKPT )
 
C
C     Routine SPHSD finds the distance between two points on the surface
C     of a sphere.  The points are defined by a latitude and longitude
C     coordinate and the sphere is defined by a radius.  We will use the
C     local radius at the intercept point as input to SPHSD.
C
C     DS is the distance between A' and B'.  DL is the distance
C     between C' and D'.
C
      DS = SPHSD ( RADIUS, LONG(1), LAT(1),
     .                     LONG(2), LAT(2) )
 
      DL = SPHSD ( RADIUS, LONG(3), LAT(3),
     .                     LONG(4), LAT(4) )
 
 
      CALL CHKOUT ( 'PXPROJ_G' )
      RETURN
      END
