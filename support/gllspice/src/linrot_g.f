C$Procedure    LINROT_G ( Linear interpolation between rotations )
 
      SUBROUTINE LINROT_G ( INIT, FINAL, FRAC, R, SCLDAV )
 
C$ Abstract
C
C     Interpolate between two rotations using a constant angular rate.
C
C$ Required_Reading
C
C     ROTATIONS
C
C$ Keywords
C
C     GLLSPICE
C     MATRIX
C     ROTATION
C
C$ Declarations
 
      DOUBLE PRECISION      INIT   ( 3, 3 )
      DOUBLE PRECISION      FINAL  ( 3, 3 )
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      R      ( 3, 3 )
      DOUBLE PRECISION      SCLDAV ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INIT       I   Initial rotation.
C     FINAL      I   Final rotation.
C     FRAC       I   Fraction of rotation from INIT to FINAL by which
C                    to interpolate.
C     R          O   Linearly interpolated rotation.
C     SCLDAV     O   Scaled angular velocity of rotation.
C
C$ Detailed_Input
C
C     INIT,
C     FINAL,
C     FRAC           are, respectively, two rotation matrices between
C                    which to interpolate, and an interpolation
C                    fraction.
C
C$ Detailed_Output
C
C     R              is the matrix resulting from linear interpolation
C                    between INIT and FINAL by the fraction FRAC.  By
C                    `linear interpolation' we mean the following:
C
C                       We view INIT and FINAL as two values of a
C                       time-varying rotation matrix R(t) that rotates
C                       at a constant angular velocity (that is, the
C                       row vector of R(t) rotate with constant angular
C                       velocity).  We can say that
C
C                          INIT   =  R(t0)
C                          FINAL  =  R(t1).
C
C                       `Linear interpolation by the fraction FRAC'
C                       means that we evalute R(t) at time
C
C                          t0   +   FRAC * (t1 - t0).
C
C
C     SCLDAV         is a scaled version of the constant angular
C                    velocity vector used for interpolation.  When
C                    SCLDAV is divided by the scale factor
C
C                       t1 - t0,
C
C                    the result is the constant angular velocity
C                    assumed for the rows of R(t) in order to perform
C                    linear interpolation.
C
C
C                    Note that SCLDAV is NOT parallel to the rotation
C                    axis of
C                                   T
C                       FINAL * INIT ;
C
C                    if this is unclear, see $Particulars below for
C                    details.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either of INIT or FINAL is not a rotation matrix, the error
C         SPICE(NOTAROTATION) is signalled.
C
C     2)  This routine assumes that the rotation that maps INIT to FINAL
C         has a rotation angle THETA radians, where
C
C            0  <  THETA  <  pi.
C               _
C
C         This routine cannot distinguish between rotations of THETA
C         radians, where THETA is in the interval [0, pi), and
C         rotations of
C
C            THETA   +   2 * k * pi
C
C         radians, where k is any integer.  These `large' rotations will
C         yield invalid results when interpolated.  You must ensure that
C         the inputs you provide to this routine will not be subject to
C         this sort of ambiguity.  If in fact you are interpolating the
C         position of a rotating matrix with constant angular velocity
C         AV between times t0 and t1, you must ensure that
C
C            || AV ||  *  |t1 - t0|   <   pi.
C
C         Here we assume that the magnitude of AV is the angular rate
C         of the rotating matrix in units of radians per second.
C
C
C     3)  When FRAC is outside of the interval [0, 1], the process
C         performed is `extrapolation', not interpolation.  Such
C         values of FRAC are permitted.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     In the discussion below, we assume that the conditions specified
C     in item (3) of $ Exceptions have been satisfied.
C
C     The definition of the output of this routine merits a little
C     analysis.  As we've said, we view INIT and FINAL as two values
C     of a time-varying rotation matrix R(t) that rotates at a constant
C     angular velocity; we define R(t), t0, and t1 so that
C
C        INIT   =  R(t0)
C        FINAL  =  R(t1).
C
C     The output matrix R is R(t) evaluated at the time
C
C        t0   +   FRAC * (t1 - t0).
C
C     How do we evaluate R at times between t0 and t1?  Since
C
C                              T
C        FINAL = ( FINAL * INIT  ) * INIT,
C
C     we can write
C
C        FINAL = M * INIT
C
C     or
C
C        R(t1) = M * R(t0),
C
C     and we can find a rotation axis A and an angle THETA such that M
C     is a coordinate system rotation of THETA radians about axis A.
C
C     Let's use the notation
C
C        [ x ]
C             N
C
C     to indicate a coordinate system rotation of x radians about the
C     vector N.  Having found A and THETA, we can write
C
C                            (t  - t0)
C        R(t) =  [  THETA *  ---------  ]     *   R(t0)
C                            (t1 - t0)    A
C
C     By the way, the input argument FRAC plays the role of the quotient
C
C        t  - t0
C        -------
C        t1 - t0
C
C     shown above.
C
C
C     Now, about SCLDAV:  If the rotation matrix M has property that
C
C             T              T
C        FINAL   =   M * INIT ,
C
C     or equivalently
C
C             T               T
C        R(t1)   =   M * R(t0),
C
C     then M maps the rows of INIT to the rows of FINAL.  The rotation
C     axis of M is parallel to the angular velocity vector of the rows
C     of R(t).  The angular rate of R(t) (assuming R(t) has contant
C     angular velocity between times t0 and t1) is the rotation angle
C     of M divided by
C
C        t1 - t0.
C
C     So SCLDAV is a vector parallel to the rotation axis of M and
C     having length equal to the rotation angle of M (which is in
C     the range [0, pi]).
C
C     Now
C                        T
C        FINAL = INIT * M ,
C
C     so
C                    T            T       T
C        FINAL * INIT  =  INIT * M  * INIT .
C
C     Let's define the matrix Q as
C
C                    T
C        FINAL * INIT ;
C
C     then since the right side of the last equation is just a
C     change-of-basis transformation, we know that Q is just
C
C         T
C        M ,
C
C     expressed relative to the basis whose elements are the rows of
C     INIT.  Since these matrices represent the same rotation, the
C     rotation axis of Q is the rotation axis of
C
C         T
C        M,
C
C     expressed relative to the basis whose elements are the rows of
C     INIT.   Call these rotation axes AXIS_Q and AXIS_MT respectively.
C     Then since left multiplication by INIT transforms vectors to the
C     basis whose elements are the rows of INIT,
C
C        AXIS_Q = INIT * AXIS_MT,
C
C     which implies that
C
C              T
C        - INIT  * AXIS_Q
C
C     is the rotation axis of M (this axis is chosen so that the
C     rotation angle is non-negative).
C
C
C$ Examples
C
C     1)  Suppose we want to interpolate between two rotation matrices
C         R1 and R2 that give the orientation of a spacecraft structure
C         at times t1 and t2.  We wish to find an approximation of the
C         structure's orientation at the midpoint of the time interval
C         [t1, t2].  We assume that the angular velocity of the
C         structure equals the constant AV between times t1 and t2.  We
C         also assume that
C
C            || AV ||  *  (t2 - t1)   <   pi.
C
C         Then the code fragment
C
C            CALL LINROT_G ( R1, R2, 0.5D0, R, SCLDAV )
C
C         produces the approximation we desire.
C
C
C     2)  Suppose R1 is the identity and R2 is
C
C            [ pi/2 ] .
C                    3
C
C         Then the code fragment
C
C            CALL LINROT_G ( R1, R2, FRAC, R, SCLDAV )
C
C         returns SCLDAV as the vector
C
C            ( 0, 0, pi/2 ).
C
C
C     3)  As long as R1 and R2 are not equal, the code fragment
C
C            CALL LINROT_G ( R1,     R2,            FRAC,  R,  SCLDAV )
C            CALL AXISAR   ( SCLDAV, VORM(SCLDAV),  M                 )
C            CALL MXMT     ( R1,     M,             R2                )
C
C         should leave R2 unchanged, except for round-off error.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    GLLSPICE Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 1.0.0, 20-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     linear interpolation between rotations
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               ISROT
 
C
C     Local parameters
C
 
C
C     NTOL and DTOL are tolerances used for determining whether INIT
C     and FINAL are rotation matrices.  NTOL is bound on the deviation
C     of the norms of the columns of a matrix from 1, and DTOL is a
C     bound on the deviation of the determinant of a matrix from 1.
C
      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL = 1.D-8 )
 
      DOUBLE PRECISION      NTOL
      PARAMETER           ( NTOL = 1.D-8 )
 
C
C     Local variables
C
      DOUBLE PRECISION      AXIS  ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      DELTA ( 3, 3 )
      DOUBLE PRECISION      Q     ( 3, 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LINROT_G' )
      END IF
 
C
C     INIT and FINAL must both be rotation matrices.
C
      IF (  .NOT.  ISROT ( INIT, NTOL, DTOL )  ) THEN
 
         CALL SETMSG ( 'INIT is not a rotation.' )
         CALL SIGERR ( 'SPICE(LINROT_G)'         )
         CALL CHKOUT ( 'LINROT_G'                )
         RETURN
 
      ELSE IF (  .NOT.  ISROT ( FINAL, NTOL, DTOL )  ) THEN
 
         CALL SETMSG ( 'INIT is not a rotation.' )
         CALL SIGERR ( 'SPICE(LINROT_G)'         )
         CALL CHKOUT ( 'LINROT_G'                )
         RETURN
 
      END IF
 
C
C     Little to do, really.  Let
C
C        FINAL  =   Q      *   INIT;
C
C     then
C                                  T
C        Q      =   FINAL  *   INIT.
C
C
C     Find an axis and angle for the quotient rotation Q, and
C     interpolate the angle.  Form the interpolated rotation DELTA.
C     DELTA is not affected by the fact that RAXISA chooses an axis and
C     angle that describe the effect of Q on vectors rather than on
C     coordinate frames.  Since RAXISA and AXISAR are compatible, the
C     interpolation works anyway.
C
      CALL MXMT   ( FINAL,  INIT,  Q )
 
      CALL RAXISA ( Q,      AXIS,  ANGLE               )
      CALL AXISAR (         AXIS,  ANGLE * FRAC, DELTA )
 
      CALL MXM    ( DELTA,  INIT,  R )
 
C
C     Finding the `constant' angular velocity vector is easy to do;
C     it may take a moment to see why this works, though.  (See the
C     $Particulars section of the header if you find this too obscure).
C
      CALL MTXV   (  INIT,   AXIS,    SCLDAV )
      CALL VSCL   ( -ANGLE,  SCLDAV,  SCLDAV )
 
      CALL CHKOUT ( 'LINROT_G' )
      RETURN
      END
