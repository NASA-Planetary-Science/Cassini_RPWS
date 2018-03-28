C$Procedure      ASRYED_G ( Angular separation of ray and ellipsoid )
 
      SUBROUTINE ASRYED_G ( A, B, C, VERTEX, DIR, ANGLE, MINPT )
 
C$ Abstract
C
C     Find the angular separation between a specified ray and ellipsoid.
C
C$ Required_Reading
C
C     ELLIPSES
C
C$ Keywords
C
C     ELLIPSOID
C     GEOMETRY
C     GLLSPICE
C
C$ Declarations
 
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      MINPT  ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     A          I   Length of ellipsoid semi-axis lying on the x-axis.
C     B          I   Length of ellipsoid semi-axis lying on the y-axis.
C     C          I   Length of ellipsoid semi-axis lying on the z-axis.
C     VERTEX,
C     DIR        I   Vertex and direction vector of ray.
C     ANGLE      O   Angular separation of ray and ellipsoid (radians).
C     MINPT      O   Closest point on ellipsoid to ray.
C
C$ Detailed_Input
C
C
C     A,
C     B,
C     C              are the lengths of the semi-axes of a triaxial
C                    ellipsoid.  The ellipsoid is centered at the
C                    origin and oriented so that its axes lie on the
C                    x, y and z axes.  A, B, and C are the lengths of
C                    the semi-axes that point in the x, y, and z
C                    directions respectively.
C
C     VERTEX,
C     DIR            are the vertex and direction vector of a ray in
C                    three-dimensional space.
C
C
C$ Detailed_Output
C
C     ANGLE          is the minimum angular separation of the input ray
C                    and the ellipsoid.  This is the minimum angular
C                    separation of the ray and any line segment
C                    extending from the ray's vertex to a point on the
C                    surface of the ellipsoid.  Units are radians.
C
C                    If the input ray actually intersects the
C                    ellipsoid, ANGLE is set to zero.
C
C
C     MINPT          is the point on the ellipsoid where the minimum
C                    angular separation is achieved.  If the input ray
C                    intersects the ellipsoid, MINPT is set to the
C                    intercept point closest to the ray's vertex.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the length of any semi-axis of the ellipsoid is
C         non-positive, the error SPICE(INVALIDAXISLENGTH) is
C         signalled.  ANGLE and MINPT are not modified.
C
C     2)  If VERTEX is inside the ellipsoid, the error
C         SPICE(DEGENERATECASE) is signalled.  ANGLE and MINPT are not
C         modified.
C
C     3)  If DIR is the zero vector, the error SPICE(ZEROVECTOR) is
C         signalled.   ANGLE and MINPT are not modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The angular separation of a ray and ellipsoid is the minimum,
C     taken over all surface points on the ellipsoid, of the angular
C     separation of the ray and the vector from the ray's vertex to
C     the surface point.  Let's presume that the ray does not
C     intersect the ellipsoid.  The point at which the minimum angular
C     separation is achieved will lie on the limb of the ellipsoid, as
C     viewed from the ray's vertex.  This fact, as well as the
C     uniqueness of the minimizing point, can be verified by observing
C     that a right circular cone whose axis is the ray, and which is
C     tangent to the ellipsoid, will be tangent at the minimizing point
C     and no other.
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
C     This subroutine can be used to measure the angular separation of
C     an instrument boresight from a body's limb.  It can also be used
C     to test for occultation of a star by an extended body.  Because
C     ASRYED_G uses a triaxial model, the angular separation it computes
C     will be more accurate than that obtainable with a spherical model,
C     when the modelled body is actually non-spherical.
C
C     A related GLLSPICE routine is LIMANG_G, which takes NAIF ID codes
C     for bodies, an epoch, and an aberration correction specification
C     as inputs, and returns the ray-limb angle and limb point of
C     minimum angular separation as outputs.
C
C$ Examples
C
C     1)  An example that can be readily checked by hand computation.
C
C            Let
C
C               A = 1
C               B = 1
C               C = 1
C
C               V = ( 2,   0,  0       )
C               D = ( -1,  0,  SQRT(3) )
C
C            The limb of the sphere as seen from the ray's vertex will
C            be the circle centered at ( .5, 0, 0 ), parallel to the
C            y-z plane,  with radius SQRT(3)/2.  The ray lies in the
C            x-z plane and passes over the ellipsoid, so the limb point
C            of minimum angular separation should be the highest point
C            on the limb.  This would be the point
C
C               ( .5, 0, SQRT(3)/2 ).
C
C            The tangent segment extending from the ray's vertex to the
C            point of mimimum angular separation makes an angle of
C            30 degrees with the x-axis, and the ray makes angle of 60
C            degrees with the x-axis, so the angular separation of the
C            ray and the limb should be 30 degrees.
C
C            The program below should verify these results.
C
C
C               PROGRAM MINANG
C
C               DOUBLE PRECISION      DPR
C
C               DOUBLE PRECISION      V(3)
C               DOUBLE PRECISION      D(3)
C               DOUBLE PRECISION      A
C               DOUBLE PRECISION      B
C               DOUBLE PRECISION      C
C               DOUBLE PRECISION      ANGLE
C               DOUBLE PRECISION      MINPT ( 3 )
C
C               V(1) =  2.D0
C               V(2) =  0.D0
C               V(3) =  0.D0
C
C               D(1) = -1.D0
C               D(2) =  0.D0
C               D(3) =  SQRT( 3.D0 )
C
C               A    =  1.D0
C               B    =  1.D0
C               C    =  1.D0
C
C               CALL ASRYED_G ( A, B, C, V, D, ANGLE, MINPT )
C
C               PRINT *, ' '
C               PRINT *, 'Angle is'
C               PRINT *, DPR() * ANGLE
C               PRINT *, 'Point of mimimum separation is'
C               PRINT *, MINPT
C
C               END
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]  "Numerical Recipes -- The Art of Scientific Computing" by
C           William H. Press, Brian P. Flannery, Saul A. Teukolsky,
C           Willam T. Vetterling.
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
C     angular separation of ray and ellipsoid
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VSEP
 
      LOGICAL               RETURN
      LOGICAL               VZERO
 
C
C     Local parameters
C
      INTEGER               NPT
      PARAMETER           ( NPT  = 20 )
 
      INTEGER               UBEL
      PARAMETER           ( UBEL =  9 )
 
 
C
C     Local variables
C
      DOUBLE PRECISION      BTWEEN
      DOUBLE PRECISION      BTWSEP
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DIFF   ( 3 )
      DOUBLE PRECISION      GR
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      LPT    ( 3 )
      DOUBLE PRECISION      MINSEP
      DOUBLE PRECISION      NEWPT
      DOUBLE PRECISION      NEWSEP
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      UPPER
      DOUBLE PRECISION      XPT    ( 3 )
 
      INTEGER               I
      INTEGER               MINIDX
 
      LOGICAL               FOUND
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ASRYED_G' )
      END IF
 
C
C     The semi-axes must have positive length.
C
      IF (       ( A .LE. 0.D0 )
     .     .OR.  ( B .LE. 0.D0 )
     .     .OR.  ( C .LE. 0.D0 )   )   THEN
 
         CALL SETMSG ( 'Semi-axis lengths:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                      )
         CALL ERRDP  ( '#', B                                      )
         CALL ERRDP  ( '#', C                                      )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'                  )
         CALL CHKOUT ( 'ASRYED_G'                                  )
         RETURN
 
      END IF
 
C
C     The ray's vertex must be outside of the ellipsoid.
C
      IF (     ( VERTEX(1)**2 / A**2 )
     .      +  ( VERTEX(2)**2 / B**2 )
     .      +  ( VERTEX(3)**2 / C**2 )    .LT.   1.D0  )  THEN
 
         CALL SETMSG ( 'Viewing point is inside the ellipsoid.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                  )
         CALL CHKOUT ( 'ASRYED_G'                               )
         RETURN
 
      END IF
 
C
C     The ray's direction vector must be non-zero.
C
      IF ( VZERO(DIR) ) THEN
 
         CALL SETMSG ( 'Ray''s direction vector must be non-zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                         )
         CALL CHKOUT ( 'ASRYED_G'                                  )
         RETURN
 
      END IF
 
C
C     See whether the ray intersects the ellipsoid.  If it does,
C     set the limb angle to zero and return.
C
      CALL SURFPT ( VERTEX, DIR, A, B, C, XPT, FOUND )
 
      IF ( FOUND ) THEN
 
         ANGLE = 0.D0
         CALL VEQU   ( XPT, MINPT )
         CALL CHKOUT ( 'ASRYED_G' )
         RETURN
 
      END IF
 
C
C     Find the limb of the ellipsoid as seen from the ray's vertex.
C
      CALL EDLIMB ( A, B, C, VERTEX, LIMB )
 
C
C     Get the center and semi-axes of the limb.
C
      CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )
 
C
C     The limb is the set of points
C
C        CENTER   +   cos(theta) SMAJOR   +   sin(theta) SMINOR
C
C     where theta is in the interval (-pi, pi].
C
C     We'll compute the angular separation of the ray and limb at
C     NPT different points on the limb, where the points are
C     generated by taking equally spaced values of theta.  We'll
C     find the minimum angular separation of the ray and limb
C     on this set of points, and then search for the absolute
C     minimum.
C
      MINSEP = PI()
      MINIDX = 0
 
      DO 50001
     .   I = 0, NPT-1
 
         THETA  =  DBLE(I) * TWOPI() / NPT
 
         CALL VLCOM3 ( 1.D0,        CENTER,
     .                 COS(THETA),  SMAJOR,
     .                 SIN(THETA),  SMINOR,    LPT  )
 
         CALL VSUB   (  LPT,        VERTEX,    DIFF )
 
         SEP  =  MIN (  MINSEP,     VSEP(DIFF,DIR)  )
 
         IF ( SEP .LT. MINSEP ) THEN
            MINIDX  =  I
            MINSEP  =  SEP
         END IF
 
50001 CONTINUE
 
C
C     The minimum separation between any of the test limb points and the
C     input ray is SEP, and was obtained at the test point indexed by
C     MINIDX.  We find the values of the separation at the neighboring
C     points and perform a `golden section' search.
C
C     In the following section of code,
C
C        LOWER          is the lower bound of the interval in which
C                       the minimum is bracketed.
C
C        UPPER          is the upper bound of the interval in which
C                       the minimum is bracketed.
C
C        BTWEEN         is a point between LOWER and UPPER.  The
C                       angular separation of the ray and limb point
C                       corresponding to the angle BTWEEN is less than
C                       the angular separation corresponding to LOWER
C                       and UPPER.
C
C        NEWPT          is a point between LOWER and UPPER such that
C                                                                  ___
C                          BTWEEN - LOWER                  3  -  \/ 5
C                          --------------    =   GR   =    ------------
C                          UPPER  - LOWER                        2
C
C
      GR     =  (  3.D0 -  SQRT(5.D0)  )  /  2.D0
 
      LOWER  =  ( TWOPI() / NPT )  *  ( MINIDX - 1 )
      UPPER  =  ( TWOPI() / NPT )  *  ( MINIDX + 1 )
 
C
C     We're going to move LOWER and UPPER closer together at each
C     iteration of the following loop, thus trapping the minimum.
C     The invariant condition that we will maintain is that the
C     angular separation of the ray and the limb point corresponding
C     to the angle BTWEEN is less than the angular separation between
C     the ray and the limb points corresonding to LOWER and UPPER.
C
C     The loop terminates when the offset by which we adjust LOWER
C     or UPPER is too small to cause a change in the affected endpoint.
C     This offset is no larger than the difference between LOWER and
C     BTWEEN.
C
C
      BTWEEN  =  ( TWOPI() / NPT )  *  MINIDX
 
C
C     We'll give the names LOWSEP and UPRSEP to the angular separation
C     at the limb points corresponding to LOWER and UPPER, respectively.
C     We don't actually have to evaluate these separations, however.
C     They are useful for understanding the minimization algorithm we'll
C     use, but are not actually used in the code.
C
C     We already know that the angular separation corresponding to
C     BTWEEN is MINSEP; this was computed above.
C
      BTWSEP  =  MINSEP
 
C
C     Before starting our loop, we're going to shift all of our angles
C     by 2*pi, so that they're bounded away from zero.
C
      LOWER   =  LOWER  + TWOPI()
      UPPER   =  UPPER  + TWOPI()
      BTWEEN  =  BTWEEN + TWOPI()
 
 
50002 IF       (  ( LOWER  +  (UPPER-LOWER) / 10.D0 )   .GT.   LOWER  )
     .THEN
C
C        At this point, the following order relations hold:
C
C           LOWER  <    BTWEEN    <   UPPER
C                  -              -
C
C           BTWSEP <  MIN ( LOWSEP, UPRSEP )
C                  -
C
C        Compute NEWPT.  This point is always located at the fraction
C        GR of the way into the larger of the intervals
C        [ LOWER, BTWEEN ] and [ BTWEEN, UPPER ].
C
C
         IF (  ( BTWEEN - LOWER )  .GT. ( UPPER - BTWEEN )  ) THEN
 
            NEWPT = LOWER   +  GR * ( BTWEEN - LOWER  )
         ELSE
            NEWPT = BTWEEN  +  GR * ( UPPER  - BTWEEN )
         END IF
 
C
C        We are going to shorten our interval by changing LOWER to
C        NEWPT or UPPER to BTWEEN, and if necessary, BTWEEN to NEWPT,
C        while maintaining the order relations of UPPER, LOWER, and
C        BTWEEN, and also the order relations of UPRSEP, LOWSEP, and
C        BTWSEP.  To do this, we need the angular separation at NEWPT.
C
         CALL VLCOM3 ( 1.D0,        CENTER,
     .                 COS(NEWPT),  SMAJOR,
     .                 SIN(NEWPT),  SMINOR,    LPT  )
 
         CALL VSUB   ( LPT,         VERTEX,    DIFF )
 
         NEWSEP    =   VSEP( DIFF,  DIR )
 
 
C
C        Swap NEWPT and BTWEEN if necessary, to ensure that
C
C           NEWPT  <  BTWEEN.
C                  _
C
         IF ( NEWPT .GT. BTWEEN ) THEN
 
            CALL SWAPD ( BTWEEN, NEWPT  )
            CALL SWAPD ( BTWSEP, NEWSEP )
 
         END IF
 
 
         IF ( NEWSEP .GT. BTWSEP ) THEN
 
            LOWER  = NEWPT
 
         ELSE
 
            UPPER  = BTWEEN
            BTWEEN = NEWPT
            BTWSEP = NEWSEP
 
         END IF
 
         GO TO 50002
      END IF
 
C
C     At this point, LPT is a good estimate of the limb point at
C     which the minimum angular separation from the ray occurs, and
C     NEWSEP is a good estimate of the angular separation.
C
      CALL VEQU ( LPT,    MINPT )
      ANGLE   =   NEWSEP
 
      CALL CHKOUT ( 'ASRYED_G' )
      RETURN
      END
