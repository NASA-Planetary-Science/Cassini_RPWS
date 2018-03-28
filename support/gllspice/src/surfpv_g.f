C$Procedure      SURFPV_G ( Surface point and velocity )
 
      SUBROUTINE SURFPV_G ( STO, STU, A, B, C, STI, FOUND )
 
C$ Abstract
C
C     Determine the intersection of a line-of-sight vector with the
C     surface of an ellipsoid and the velocity of the intercept
C     point on the surface.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     ELLIPSOID
C     GEOMETRY
C
C$ Declarations
 
      DOUBLE PRECISION      STO    ( 6 )
      DOUBLE PRECISION      STU    ( 6 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      STI    ( 6 )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STO        I   State of the observer in body-fixed coordinates
C     STU        I   State of a line-of-sight vector:  U and dU/dt.
C     A          I   Length of the ellipsoid semi-axis along the x-axis
C     B          I   Length of the ellipsoid semi-axis along the y-axis
C     C          I   Length of the ellipsoid semi-axis along the z-axis
C     STI        O   State of the intercept point on the ellipsoid.
C     FOUND      O   Can the state of the intercept point be determined?
C
C$ Detailed_Input
C
C     STO        is a 6-vector giving the body-fixed state of an
C                observer.  The first three elements are the body-fixed
C                coordinates of the position of the observer; the last
C                three elements are the components of the observer's
C                velocity in the body-fixed frame.  In the body-fixed
C                frame, the center of the ellipsoid is at the origin,
C                and the semi-axes of the ellipsoid are aligned with
C                the x, y, and z-axes.
C
C     STU        is a 6-vector giving the body-fixed state of a
C                line-of-sight vector.  The first three elements are
C                a vector, U, that presumably points in the direction
C                of the ellipsoid.  The last three elements are the
C                velocity of the vector, U; we'll call it DU.  So,
C                STU = ( U, DU ).
C
C                Note that if AV is the angular velocity vector,
C                then DU is the cross product of AV and U:
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
C     A          is the length of the semi-axis of the ellipsoid
C                that is parallel to the x-axis of the body-fixed
C                coordinate system.
C
C     B          is the length of the semi-axis of the ellipsoid
C                that is parallel to the y-axis of the body-fixed
C                coordinate system.
C
C     C          is the length of the semi-axis of the ellipsoid
C                that is parallel to the z-axis of the body-fixed
C                coordinate system.
C
C$ Detailed_Output
C
C     STI        is the body-fixed state of the surface intercept point,
C                if it exists.
C
C                That is, if the ray with direction vector U,
C                emanating from the position of the observer,
C                intersects the ellipsoid, AND if the velocity of
C                the intercept point is small enough to be computed,
C                then the first three elements of STI will be the
C                body-fixed coordinates of the first intercept
C                point, and the last three elements of STI will be the
C                instantaneous velocity of that point on the surface
C                of the ellipsoid.  This velocity vector lies in
C                the plane that is tangent to the ellipsoid at the
C                intercept point.
C
C                If there is no intersection, or if the velocity is
C                too large to be computed, as when the intercept point
C                is on the limb where its velocity is infinite, STI
C                is undefined.
C
C     FOUND      is a logical flag indicating whether or not the
C                state of the intercept point exists and can be
C                computed.  If STI is defined, FOUND will be
C                returned as .TRUE. otherwise it will be false.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      1) If the input line-of-sight vector is the zero vector, that
C         is, if the first three elements of STU are all zero, then
C         a subroutine that SURFPV_G calls will diagnose and signal
C         an error.
C
C      2) If any of the body's axes, A, B, or C, is nonpositive,
C         a subroutine that SURFPV_G calls will diagnose and signal
C         an error.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The position and velocity of the observer as well as the
C     line-of-sight vector and its velocity vary with time.  The
C     inputs to SURFPV_G may be considered the values of these
C     vector functions at a particular time, say t0.  Thus
C
C        State of observer:         STO = ( P(t0), P'(t0) )
C
C        State of pointing vector:  STU = ( U(t0), U'(t0) )
C
C     To determine the intercept point, I(t0), we simply compute the
C     geometrical intersection of the ray originating at P(t0)
C     in the direction of U(t0) with the ellipsoid
C
C                    2      2      2
C                   x      y      z
C                  --- +  --- +  ---  =  1
C                    2      2      2
C                   A      B      C
C
C     I(t) is the path of the intercept point along the surface of
C     the ellipsoid.  To determine the velocity of the intercept point,
C     we need to take the time derivative of I(t), and evaluate it at
C     t0.  Unfortunately I(t) is a complicated expression, and its
C     derivative is even more complicated.
C
C     However, we know that the derivative of I(t) at t0, I'(t0),
C     is tangent to I(t) at t0.  Thus I'(t0) lies in the plane that
C     is tangent to the ellipsoid at t0.  Let J(t) be the curve in the
C     in the tangent plane that represents the intersection of the
C     ray emanating from P(t0) with direction U(t0) with that tangent
C     plane.
C
C                J'(t0) = I'(t0).
C
C     The expression for J'(t) is much simpler than that of I'(t);
C     SURFPV_G evaluates J'(t) at t0.
C
C
C     Derivation of J(t) and J'(t)
C     ----------------------------------------------------------------
C
C     I(t0) is the intercept point.  Let N be a surface normal at I(t0).
C     Then the tangent plane at I(t0) is the set of points X such that
C
C                 < X - I(t0), N > = 0.
C
C     J(t) can be expressed as the vector sum of the position of the
C     observer and some scalar multiple of the line-of-sight vector,
C
C                 J(t) = P(t) + s(t) * U(t),
C
C     where s(t) is a scalar function of time.  The derivative of
C     J(t) is given by
C
C                 J'(t) = P'(t) + s(t) * U'(t) + s'(t) * U(t).
C
C     We have P(t0), P'(t0), U(t0), U'(t0), I(t0), and N, but to
C     evaluate J'(t0), we need s(t0) and s'(t0).  We derive an
C     expression for s(t) as follows.
C
C     Because J(t) is in the tangent plane, it must satisfy
C
C                 < J(t) - I(t0), N > = 0.
C
C     Substituting the expression for J(t) into the equation above
C     gives
C
C                 < P(t) + s(t) * U(t) - I(t0), N > = 0.
C
C     Thus
C
C                 < P(t) - I(t0), N > + s(t) * < U(t), N > = 0,
C
C     and
C                         - < P(t) - I(t0), N >
C                 s(t)  =   --------------------
C                               < U(t), N >
C
C     The derivative of s(t) is given by
C
C                 s'(t) =
C
C   - (  < U(t),N > * < P'(t),N >  -  < P(t)-I(t0),N > * < U'(t),N >  )
C     -----------------------------------------------------------------
C                                        2
C                             < U(t), N >
C
C
C$ Examples
C
C     This code example shows how the routine SMEARV_G uses SPKEZ to
C     get a state vector and then transforms it to body-fixed
C     coordinates for input to SURFPV_G.  The output from SURFPV_G
C     and SMEARV_G are the same.  SMEARV_G is really a high level
C     interface to the geometry routine SURFPV_G.
C
C     In this example, we assume U and DU are the direction and
C     velocity of the line-of-sight vector and are referenced to
C     the inertial frame, REFIN.  Also, we assume that the
C     appropriate SPK and PCK files have been loaded.
C
C     C
C     C     Get the apparent state of the target body as seen from the
C     C     observing body at time ET in the same inertial frame as the
C     C     pointing vectors.
C
C           CALL SPKEZ ( TARGET, ET, REFIN, 'LT', OBSRVR, STTI, LT )
C
C     C
C     C     Negate STTI so that it is the inertially referenced state
C     C     of the observer as seen by the apparent target.
C     C
C           CALL VMINUG ( STTI, 6, STOI )
C
C     C
C     C     Now we need the state of the observer in target body-fixed
C     C     coordinates, STOBF.  TISBOD generates the 6x6 matrix TSIBF
C     C     that transforms an inertial state to body-fixed coordinates
C     C     at a particular epoch.
C     C
C           CALL TISBOD ( 'J2000', TARGET, ET-LT, TSIBF  )
C
C           CALL MXVG   ( TSIBF, STOI, 6, 6, STOBF )
C
C     C
C     C     We also need U and DU in body-fixed coordinates.  Pack
C     C     U and DU into a state, STUI, and multiply by TSIBF.
C     C
C           CALL VEQU (  U, STUI ( 1 ) )
C           CALL VEQU ( DU, STUI ( 4 ) )
C
C           CALL MXVG ( TSIBF, STUI, 6, 6, STUBF )
C
C     C
C     C     Now we get the radii of the ellipsoid model of the target
C     C     body from the kernel pool.
C     C
C           CALL BODVAR ( TARGET, 'RADII', NRADII, RADII )
C
C     C
C     C     Finally, we call the geometry routine SURFPV_G, which
C     C     takes the body-fixed states of the observer and
C     C     pointing vector and the ellipsoid model, and
C     C     computes the intercept point and the velocity of
C     C     that point on the surface of the ellipsoid.
C     C
C           CALL SURFPV_G ( STOBF, STUBF,
C          .                RADII(1), RADII(2), RADII(3),
C          .                STIBF, FOUND )
C
C     C
C     C     STIBF, is the body-fixed state of the intercept point.
C     C
C           IF ( FOUND ) THEN
C
C              WRITE (*,*) 'Intercept point:'
C              WRITE (*,*) '                   x = ', STIBF ( 1 )
C              WRITE (*,*) '                   y = ', STIBF ( 2 )
C              WRITE (*,*) '                   z = ', STIBF ( 3 )
C              WRITE (*,*) '               dx/dt = ', STIBF ( 4 )
C              WRITE (*,*) '               dy/dt = ', STIBF ( 5 )
C              WRITE (*,*) '               dz/dt = ', STIBF ( 6 )
C
C           END IF
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
C-    GLLSPICE Version 1.0.0, 19-OCT-1990 (JEM)
C
C-&
 
C$ Index_Entries
C
C     surface point and velocity
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
 
 
C
C     Local variables
C
      DOUBLE PRECISION      DP     ( 3 )
      DOUBLE PRECISION      DU     ( 3 )
      DOUBLE PRECISION      I      ( 3 )
      DOUBLE PRECISION      M
      DOUBLE PRECISION      N      ( 3 )
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      PMI    ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SECOND ( 3 )
      DOUBLE PRECISION      STUHAT ( 6 )
      DOUBLE PRECISION      THIRD  ( 3 )
      DOUBLE PRECISION      U      ( 3 )
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SURFPV_G' )
      END IF
 
 
C
C     Determine the ellipsoid surface intercept point of the
C     ray emanating from the observer in the direction of U.
C     We'll call it I and it will go in the first three
C     elements of STI once we determine the velocity.  If there
C     is no intersection, we check out.
C
C     SURFPT takes care of some error checking too.  It signals
C     an error if U is the zero vector or if A, B, or C are bad
C     axis lengths.
C
      CALL SURFPT ( STO, STU, A, B, C, I, FOUND)
 
      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'SURFPV_G' )
         RETURN
      END IF
 
C
C     U is useful for its direction only.  Normalize U and divide
C     DU by the same scalar.  We know that the norm of U is not zero
C     because SURFPT checked it.
C
      CALL VSCLG ( 1.0D0 / VNORM( STU ), STU, 6, STUHAT )
 
 
C
C     The velocity vector of the intercept point goes in the last three
C     elements of STI.  Let
C
C           I = I(t0)              DI = I'(t0)
C           P = P(t0)              DP = P'(t0)
C           U = U(t0) / | U(t0) |  DU = U'(t0) / | U(t0) |
C
C     and N be the unit normal to the ellipsoid surface at I.
C     Then, from the derivation in $ Particulars above,
C
C           DI  =
C
C
C            < P-I,N >       < U,N > < DP,N > - < P-I,N > < DU,N >
C      DP -  --------- DU -  ------------------------------------- U
C             < U,N >                            2
C                                          < U,N >
C
C
 
C
C     Compute the unit normal at the intercept point, and unpack
C     the input states into P, U, DP, and DU.  Let P-I = PMI
C
      CALL SURFNM ( A, B, C, I, N )
 
      CALL VEQU ( STO,        P )
      CALL VEQU ( STUHAT,     U )
 
      CALL VEQU ( STO(4),    DP )
      CALL VEQU ( STUHAT(4), DU )
 
      CALL VSUB ( P, I,     PMI )
 
C
C     As the intercept point nears the limb, it's velocity goes to
C     infinity.  We must check the value of < U,N > before dividing
C     by it.  If the intercept point is on the limb < U,N > = 0.
C     If it is near the limb, < U,N > may be so small that dividing
C     by would result in a number that is greater than the maximum
C     double precision number for the computer.
C
 
      IF ( VDOT ( U,N ) .EQ. 0.D0 ) THEN
C
C        The intercept point is on the the limb, so its velocity
C        is infinite.  This means we can't "find" the state
C        of the intercept point.
C
         FOUND = .FALSE.
         CALL CHKOUT ( 'SURFPV_G' )
         RETURN
      END IF
 
C
C     Evaluate the second term of the equation for DI, but don't
C     divide by < U,N > just yet.
C
      CALL VSCL ( VDOT ( PMI,N ), DU, SECOND )
C
C                                                         2
C     Evaluate the third term, but don't divide by < U,N >  just yet.
C
      S = VDOT ( U,N ) * VDOT ( DP,N ) - VDOT ( PMI,N ) * VDOT ( DU,N )
 
      CALL VSCL ( S, U, THIRD )
 
C
C     We'll use the following test.
C
C     Let
C
      M = MAX (  VNORM (SECOND), VNORM (THIRD), 1.0D0  )
 
C
C
C     If
C
C             M          DPMAX
C          -------   >   -----
C                 2
C          < U,N >        10
C
C
C     That is, if
C                                  2
C             M  >  DPMAX * < U,N >  / 10
C
C
C     where DPMAX is the largest double precision number,
C     then, the velocity is probably too large to compute.
C     We know that we can perform the multiplication above
C     because U and N are both unit vectors, so the dot
C     product of U and N is less than or equal to one.
C
 
      IF ( M .GT. ( DPMAX() / 10.0D0 ) *  VDOT ( U,N ) ** 2 ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( 'SURFPV_G' )
         RETURN
      END IF
 
 
C
C     If < U,N > passed the tests above, we can solve for the velocity.
C
C                                                      2
C     DI =  DP  -  SECOND / < U,N >  -  THIRD / < U,N >
C
C
      S = 1.0D0 / VDOT ( U, N )
 
      CALL VLCOM3 ( 1.0D0, DP, -S, SECOND, -(S**2), THIRD, STI(4) )
 
C
C     Since we could compute the velocity, we can assign the
C     intercept point, and set the found flag to .TRUE.
C
      CALL VEQU  ( I, STI(1) )
 
      FOUND = .TRUE.
 
      CALL CHKOUT ( 'SURFPV_G' )
      RETURN
      END
 
