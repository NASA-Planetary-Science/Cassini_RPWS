C$Procedure      SSIL2X_G ( SSI line and sample to X Y )
 
      SUBROUTINE SSIL2X_G ( L, S, X, Y )
 
C$ Abstract
C
C     Map Solid State Imaging (SSI) instrument line and sample
C     coordinates to millimeter (projection) space coordinates.
C
C$ Required_Reading
C
C     GLL_IK
C
C$ Keywords
C
C     CAMERA
C     CONVERSION
C
C$ Declarations
 
      DOUBLE PRECISION      L
      DOUBLE PRECISION      S
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
 
      INTEGER               VER
      PARAMETER           ( VER = 1 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      L,
C      S         I    Line-sample coordinates.
C      X,
C      Y         O    Millimeter space coordinates.
C      VER       P    Version of I-kernel needed.
C
C$ Detailed_Input
C
C     L,
C     S           describe a point in the line-sample coordinate system.
C                 The origin is located in the upper left corner of the
C                 field of view, as shown in the figure below.
C
C                  sample                                  sample
C                    1                         S            800
C                     -------------------------+-------------
C             Line 1 |                                       |
C                    |                                       |
C                    |                                       |
C                    |                                       |
C                    |                                       |
C                    |                                       |
C                    |                                       |
C                    |                                       |
C                    |                         *             + L
C                    |                         (L,S)         |
C                    |                                       |
C                    |                                       |
C                    |                                       |
C                    |                                       |
C                    |                                       |
C           Line 800 |                                       |
C                     ---------------------------------------
C
C$ Detailed_Output
C
C     X,
C     Y           are the coordinates of the point in millimeter
C                 (projection) space.  The origin (where X=Y=0) is
C                 located at the center of the field of view.
C                 Positive X runs in increasing line, positive Y
C                 runs in increasing sample.
C
C$ Parameters
C
C      VER        is the version of the SSI I-kernel that must be
C                 loaded into the kernel pool.  That is, the
C                 I-kenrel must be in the form VER.X.
C
C$ Exceptions
C
C     1) If any of the required parameters are not found in the kernel
C        pool, the error SPICE(KERNELVARNOTFOUND) is signalled.
C
C     2) If L or S are not within the proper range, as specified in
C        the I-kernel, the error SPICE(VALUEOUTOFRANGE) is signalled.
C
C     2) If the SSI I-kernel currently in the kernel pool is not
C        compatible with this routine the error
C        SPICE(IKERNELNOTCOMPAT) is signalled.
C
C$ Files
C
C     A SSI I-kernel with the correct version number must be loaded in
C     the kernel pool prior to calling this routine.
C
C$ Particulars
C
C     This subroutine converts line, sample coordinates to millimeter
C     space coordinates using the distortion model described in
C     reference [1].  An I-kernel containing SSI geometric and optical
C     data must be loaded into the kernel pool prior to calling this
C     routine and the version must be in the form VER.X.
C
C$ Examples
C
C     The following example shows how this routine might be used in a
C     typical application program.  This program reads instrument data
C     from a I-kernel file.  Note that a set of four line and sample
C     values are hard-coded in the program for the purpose of
C     demonstration:  A real application would likely get input
C     coordinates from some external source, such as a file or through
C     interactive user input.
C
C           C
C           C   Sample Mapping program
C           C
C
C                 DOUBLE PRECISION      L   ( 4 )
C                 DOUBLE PRECISION      S   ( 4 )
C                 DOUBLE PRECISION      X
C                 DOUBLE PRECISION      Y
C
C                 INTEGER               I
C                 INTEGER               NPOINT
C
C                 DATA L              / 1.0D0,
C                .                      400.0D0,
C                .                      600.0D0,
C                .                      400.0D0  /
C
C                 DATA S              / 1.0D0,
C                .                      400.0D0,
C                .                      400.0D0,
C                .                      700.0D0  /
C
C                 DATA NPOINT         / 4        /
C
C           C
C           C     Load the I-kernel containing SSI geometric and optical
C           C     data into the kernel pool.  We will assume that this
C           C     file is located in the directory SPICE$KER.
C           C
C                 CALL LDPOOL ( 'SPICE$KER:GLL36001.TI' )
C
C           C
C           C     Convert L,S to X,Y
C           C
C                 DO I = 1, NPOINT
C
C                    CALL SSIL2X_G ( L(I), S(I), X, Y )
C
C                    WRITE (*,*)
C                    WRITE (*,*) ' Input  L : ', L(I)
C                    WRITE (*,*) ' Input  S : ', S(I)
C                    WRITE (*,*) ' Output X : ', X
C                    WRITE (*,*) ' Output Y : ', Y
C                    WRITE (*,*)
C
C                 END DO
C
C
C                 END
C
C      The output from this program looks like this:
C      (Units are in pixels)
C
C           Input  L : 1.0
C           Input  S : 1.0
C           Output X : -398.2
C           Output Y : -398.2
C
C           Input  L : 400.0
C           Input  S : 400.0
C           Output X : 0.0
C           Output Y : 0.0
C
C           Input  L : 600.0
C           Input  S : 400.0
C           Output X : 0.0
C           Output Y : 199.9
C
C           Input  L : 400.0
C           Input  S : 700.0
C           Output X : 299.8
C           Output Y : 0.0
C
C$ Restrictions
C
C     This routine uses the distortion model contained in the
C     references to associate features in an image with their actual
C     geometric direction vectors.  If the model changes, this routine
C     will no longer be valid.
C
C$ Literature_References
C
C     [1]   ``SSI Distortion and Twist Models for NAIF,'' JPL IOM, by
C             Ken Klaasen, 7/2/90.
C
C     [2]   ``Formula for SSI Geometric Distortion Correction,'' JPL
C             IOM, by Ken Klaasen, 7/21/88.
C
C             The two documents [1] and [2] comprise NAIF document
C             number 202.0.
C
C$ Author_and_Institution
C
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    GLLSPICE Beta Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Beta Version 1.0.0, 03-OCT-1990 (MJS)
C
C-&
 
C$ Index_Entries
C
C     ssi line and sample to x y
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      VNORMG
 
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
C     MAXMSG      is the maximum length of the error message.
C
C     TOL         is the accuracy used when calculating the inverse
C                 of the distortion function.  TOL is explained in more
C                 detail below.
C
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32           )
 
      INTEGER               NVARS
      PARAMETER           ( NVARS  = 8            )
 
      INTEGER               MAXMSG
      PARAMETER           ( MAXMSG = NVARS*MAXLEN )
 
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL    = 1.0D-10      )
 
C
C     Local variables
C
      CHARACTER*(MAXMSG)    OUTERR
 
      DOUBLE PRECISION      A
      DOUBLE PRECISION      CENTER ( 2 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      FPRIME
      DOUBLE PRECISION      IKVER
      DOUBLE PRECISION      MAXLNE
      DOUBLE PRECISION      MAXSAM
      DOUBLE PRECISION      MINLNE
      DOUBLE PRECISION      MINSAM
      DOUBLE PRECISION      PSIZE
      DOUBLE PRECISION      RMM
      DOUBLE PRECISION      RMM0
      DOUBLE PRECISION      RMM1
      DOUBLE PRECISION      RLS
      DOUBLE PRECISION      VRLS   ( 2 )
 
      INTEGER               I
      INTEGER               N
 
      LOGICAL               QUIT
 
      CHARACTER*(32)        PLDATA ( NVARS )
 
      LOGICAL               FOUND  ( NVARS )
      LOGICAL               VFOUND
 
C
C     Saved variables
C
      SAVE  PLDATA
 
C
C     Initial values
C
      DATA  PLDATA        / 'INS-77036_FOV_CENTER',
     .                      'INS-77036_PIXEL_SIZE',
     .                      'INS-77036_MIN_LINE',
     .                      'INS-77036_MAX_LINE',
     .                      'INS-77036_MIN_SAMPLE',
     .                      'INS-77036_MAX_SAMPLE',
     .                      'INS-77036_DISTORTION_COEFF',
     .                      'INS-77036_VERSION'           /
 
C
C     Statement functions.  These are used to calculate the inverse
C     of the distortion function.
C
      F(X)      = A*X**3 + X - RLS
      FPRIME(X) = 3*A*X**2 + 1
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SSIL2X_G' )
      END IF
 
C
C     Check version number of I-kernel.
C
      CALL RTPOOL ( PLDATA(8), N, IKVER, VFOUND )
 
      IF ( .NOT. VFOUND ) THEN
         CALL SETMSG ( 'SSI I-kernel version not found'            )
         CALL SIGERR ( 'SPICE(KERVARNOTFOUND)'                     )
         CALL CHKOUT ( 'SSIL2X_G'                                  )
         RETURN
      END IF
 
      IF ( INT(IKVER) .NE. VER ) THEN
         CALL SETMSG ( 'SSI I-kernel version # not compatible'     )
         CALL ERRDP  ( '#', IKVER                                  )
         CALL SIGERR ( 'SPICE(IKERNELNOTCOMPAT)'                   )
         CALL CHKOUT ( 'SSIL2X_G'                                  )
         RETURN
      END IF
 
C
C     Load the kernel pool and extract needed data.  Signal error if
C     any information is not found.
C
      CALL RTPOOL (PLDATA(1), N, CENTER, FOUND(1))
      CALL RTPOOL (PLDATA(2), N, PSIZE,  FOUND(2))
      CALL RTPOOL (PLDATA(3), N, MINLNE, FOUND(3))
      CALL RTPOOL (PLDATA(4), N, MAXLNE, FOUND(4))
      CALL RTPOOL (PLDATA(5), N, MINSAM, FOUND(5))
      CALL RTPOOL (PLDATA(6), N, MAXSAM, FOUND(6))
      CALL RTPOOL (PLDATA(7), N, A,      FOUND(7))
 
      QUIT   = .FALSE.
      OUTERR = ' '
 
      DO 50001
     .   I = 1, NVARS - 1
 
         IF ( .NOT. FOUND(I) ) THEN
            QUIT = .TRUE.
            CALL SUFFIX ( PLDATA(I), 2, OUTERR )
         END IF
 
50001 CONTINUE
 
      IF ( QUIT ) THEN
         CALL SETMSG ( 'Did not find # item(s) in pool'   )
         CALL ERRCH  ( '#', OUTERR                        )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'         )
         CALL CHKOUT ( 'SSIL2X_G'                         )
         RETURN
      END IF
 
C
C     Check if the inputs S and L are within range.  Print out
C     everything if either S or L are invalid.
C
      IF (         (S .LT. MINSAM) .OR. (S .GT. MAXSAM)
     .      .OR.   (L .LT. MINLNE) .OR. (L .GT. MAXLNE)   ) THEN
 
         CALL SETMSG ('The values of L and S are invalid - ' //
     .                'their values are:  L =  #, S  =  #  ' //
     .                'L has limits from # to #, S has '     //
     .                'limits from # to # '                  )
 
         CALL ERRDP  ( '#', L      )
         CALL ERRDP  ( '#', S      )
         CALL ERRDP  ( '#', MINLNE )
         CALL ERRDP  ( '#', MAXLNE )
         CALL ERRDP  ( '#', MINSAM )
         CALL ERRDP  ( '#', MAXSAM )
 
         CALL SIGERR ('SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ('SSIL2X_G'               )
         RETURN
 
      END IF
 
C
C     Find the vector pointing from the center of the field of view
C     to the L,S coordinate.  RLS is the magnitude of the vector and
C     VRLS is its direction. (VHATG converts VRLS into a unit vector.)
C
      VRLS(1) = S - CENTER(1)
      VRLS(2) = L - CENTER(2)
      RLS     = VNORMG ( VRLS, 2 )
 
      CALL VHATG ( VRLS, 2, VRLS )
 
C
C     Now find RMM, the distance in millimeter space from center of the
C     field of view to the point.  RLS and RMM are related to each other
C     by the following equation:
C
C        F(RMM) = A*RMM**3 + RMM - RLS = 0
C
C     where A is the distortion coefficient.  The Newton-Raphson
C     method will be used to to find RMM.  In order to use
C     this method the derivative of F(RMM), F'(RMM), must be found.
C     The above equation is easily differentiable.  F'(RMM) is:
C
C        F'(RMM) = 3*A*RMM**2 + 1
C
C     The Newton-Raphson iteration formula is
C
C        RMM  =  RMM    -  [ F(RMM)/F'(RMM) ]
C           i       i-1
C
C     F'(RMM) will never be zero, therefore, the fraction in the
C     above formula will never explode on us.  The code below will
C     iterate until the absolute difference between RMM-values is less
C     than TOL.
C
C     The first guess, RMM  , will be RLS, since the
C                         0
C     function is very close to linear ( A << 1 ) within the range in
C     question.
C
      RMM0 = RLS
      RMM1 = RMM0 - F(RMM0)/FPRIME(RMM0)
 
50002 IF       ( ABS(RMM1 - RMM0) .GT. TOL )
     .THEN
 
         RMM0 = RMM1
         RMM1 = RMM0 - ( F(RMM0) / FPRIME(RMM0) )
 
         GO TO 50002
      END IF
 
      RMM = RMM1
 
C
C     Now find X and Y.  The origin is at the center of the field of
C     view.  The direction of the vector (X, Y) is the same as VRLS,
C     therefore we only need to scale the vector VRLS by RMM.
C     VSCLG scales a general dimension vector by a scalar.  In this
C     case the dimension is 2 and the scalar is RMM.
C
      CALL VSCLG ( RMM, VRLS, 2, VRLS )
      X = VRLS(1)
      Y = VRLS(2)
 
C
C     That's all.
C
      CALL CHKOUT ( 'SSIL2X_G' )
      RETURN
      END
