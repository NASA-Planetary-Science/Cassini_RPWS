C  Main program commented out so that the subroutine may be included 
C  in other programs.  -cwp 2012-10-17

C	Double Precision Q(4), R(3,3)
C
C	Print *,'Enter Quaternion:'
C	Read (5,'(4F10.5)') q
C	Print *, Q
C
C	CALL Q2M (Q,R)
C
C	Do I=1,3
C	  Write (*,'(3F10.5)') R(i,1),R(i,2),R(i,3)
C	Enddo
C
C	T31 = R(1,3)
C	T32 = R(2,3)
C	T33 = R(3,3)
C	T13 = R(3,1)
C	T23 = R(3,2)
C
C	RA  = (180./3.14159) * ATAN2 (T31, -T32)
C	DEC = (180./3.14159) * ACOS (T33)
C	TW  = (180./3.14159) * ATAN2 (T13, T23)
C
C	PRINT *, RA, DEC, TW
C
C	end

C$Procedure      Q2M ( Quaternion to matrix )
 
      SUBROUTINE Q2M ( Q, R )
 
C$ Abstract
C
C     Find the rotation matrix corresponding to a specified unit
C     quaternion.
C
C$ Required_Reading
C
C     ROTATION
C
C$ Keywords
C
C     MATH
C     MATRIX
C     ROTATION
C
C$ Declarations
 
      DOUBLE PRECISION      Q ( 0 : 3 )
      DOUBLE PRECISION      R ( 3,  3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     Q          I   A unit quaternion.
C     R          O   A rotation matrix corresponding to Q.
C
C$ Detailed_Input
C
C     Q              is a unit quaternion representing a rotation.  Q
C                    is a 4-dimensional vector.  Q has the property that
C
C                       || Q ||  =  1.
C
C$ Detailed_Output
C
C     R              is a 3 by 3 rotation matrix representing the same
C                    rotation as does Q.  If Q represents a rotation by
C                    r radians about some axis vector A, then for any
C                    vector V, R*V yields V, rotated by r radians
C                    about A.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If Q is not a unit quaternion, the output matrix M is
C         unlikely to be a rotation matrix.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     If a 4-dimensional vector Q satisfies the equality
C
C        || Q ||   =  1
C
C     or equivalently
C
C            2          2          2          2
C        Q(0)   +   Q(1)   +   Q(2)   +   Q(3)   =  1,
C
C     then we can always find a unit vector A and a scalar r such that
C
C        Q = ( cos(r/2), sin(r/2)A(1), sin(r/2)A(2), sin(r/2)A(3) ).
C
C     We can interpret A and r as the axis and rotation angle of a
C     rotation in 3-space.  If we restrict r to the range [0, pi],
C     then r and A are uniquely determined, except if r = pi.  In this
C     special case, A and -A are both valid rotation axes.
C
C     Every rotation is represented by a unique orthogonal matrix; this
C     routine returns that unique rotation matrix corresponding to Q.
C
C     The SPICELIB routine M2Q is a one-sided inverse of this routine:
C     given any rotation matrix R, the calls
C
C        CALL M2Q ( R, Q )
C        CALL Q2M ( Q, R )
C
C     leave R unchanged, except for round-off error.  However, the
C     calls
C
C        CALL Q2M ( Q, R )
C        CALL M2Q ( R, Q )
C
C     might preserve Q or convert Q to -Q.
C
C$ Examples
C
C     1)  A case amenable to checking by hand calculation:
C
C            To convert the quaternion
C
C               Q = ( sqrt(2)/2, 0, 0, -sqrt(2)/2 )
C
C            to a rotation matrix, we can use the code fragment
C
C               Q(0) =  DSQRT(2)/2.D0
C               Q(1) =  0.D0
C               Q(2) =  0.D0
C               Q(3) = -DSQRT(2)/2.D0
C
C               CALL Q2M ( Q, R )
C
C            The matrix R will be set equal to
C
C               +-              -+
C               |  0     1    0  |
C               |                |
C               | -1     0    0  |.
C               |                |
C               |  0     0    1  |
C               +-              -+
C
C            Why?  Well, Q represents a rotation by some angle r about
C            some axis vector A, where r and A satisfy
C
C               Q =
C
C               ( cos(r/2), sin(r/2)A(1), sin(r/2)A(2), sin(r/2)A(3) ).
C
C            In this example,
C
C               Q = ( sqrt(2)/2, 0, 0, -sqrt(2)/2 ),
C
C            so
C
C               cos(r/2) = sqrt(2)/2.
C
C            Assuming that r is in the interval [0, pi], we must have
C
C               r = pi/2,
C
C            so
C
C               sin(r/2) = sqrt(2)/2.
C
C            Since the second through fourth components of Q represent
C
C               sin(r/2) * A,
C
C            it follows that
C
C               A = ( 0, 0, -1 ).
C
C            So Q represents a transformation that rotates vectors by
C            pi/2 about the negative z-axis.  This is equivalent to a
C            coordinate system rotation of pi/2 about the positive
C            z-axis; and we recognize R as the matrix
C
C               [ pi/2 ] .
C                       3
C
C
C     2)  Finding a set of Euler angles that represent a rotation
C         specified by a quaternion:
C
C            Suppose our rotation R is represented by the quaternion
C            Q.  To find angles TAU, ALPHA, DELTA such that
C
C
C               R  =  [ TAU ]  [ pi/2 - DELTA ]  [ ALPHA ] ,
C                            3                 2          3
C
C            we can use the code fragment
C
C
C               CALL Q2M    ( Q, R )
C
C               CALL M2EUL  ( R,   3,      2,       3,
C              .                   TAU,    DELTA,   ALPHA  )
C
C               DELTA = HALFPI() - DELTA
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]    NAIF document 179.0, "Rotations and their Habits", by
C            W. L. Taber.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     quaternion to matrix
C
C-&
 
 
 
C
C     Local variables
C
      DOUBLE PRECISION      Q01
      DOUBLE PRECISION      Q02
      DOUBLE PRECISION      Q03
      DOUBLE PRECISION      Q12
      DOUBLE PRECISION      Q13
      DOUBLE PRECISION      Q23
      DOUBLE PRECISION      Q1S
      DOUBLE PRECISION      Q2S
      DOUBLE PRECISION      Q3S
 
 
C
C     If a matrix R represents a rotation of r radians about the unit
C     vector n, we know that R can be represented as
C
C                                           2
C        I  +  sin(r) N  +  [ 1 - cos(r) ] N ,
C
C     where N is the matrix that satisfies
C
C        Nv = n x v
C
C     for all vectors v, namely
C
C             +-                -+
C             |  0    -n     n   |
C             |         3     2  |
C             |                  |
C        N =  |  n     0    -n   |.
C             |   3           1  |
C             |                  |
C             | -n     n     0   |
C             |   2     1        |
C             +-                -+
C
C
C      Define S as
C
C         sin(r/2) N,
C
C      and let our input quaternion Q be
C
C         ( q ,  q ,  q ,  q ).
C            0    1    2    3
C
C      Using the facts that
C
C                             2
C         1 - cos(r)  =  2 sin (r/2)
C
C      and
C
C         sin(r)      =  2 cos(r/2) sin(r/2),
C
C
C      we can express R as
C
C                                      2
C         I  +  2 cos(r/2) S    +   2 S,
C
C      or
C                                2
C         I  +  2 q  S    +   2 S.
C                  0
C
C      Since S is just
C
C         +-                -+
C         |  0    -q     q   |
C         |         3     2  |
C         |                  |
C         |  q     0    -q   |,
C         |   3           1  |
C         |                  |
C         | -q     q     0   |
C         |   2     1        |
C         +-                -+
C
C      our expression for R comes out to
C
C         +-                                                         -+
C         |          2   2                                            |
C         | 1 - 2 ( q + q  )    2( q q  -  q q )     2 ( q q  + q q ) |
C         |          2   3          1 2     0 3           1 3    0 2  |
C         |                                                           |
C         |                              2   2                        |
C         | 2( q q  +  q q )    1 - 2 ( q + q  )     2 ( q q  - q q ) |.
C         |     1 2     0 3              1   3            2 3    0 1  |
C         |                                                           |
C         |                                                   2   2   |
C         | 2( q q  -  q q )    2 ( q q  + q q )     1 - 2 ( q + q  ) |
C         |     1 3     0 2          2 3    0 1               1   2   |
C         +-                                                         -+
C
C
C      For efficiency, we avoid duplicating calculations where possible.
C
 
      Q01  =  Q(0) * Q(1)
      Q02  =  Q(0) * Q(2)
      Q03  =  Q(0) * Q(3)
 
      Q12  =  Q(1) * Q(2)
      Q13  =  Q(1) * Q(3)
 
      Q23  =  Q(2) * Q(3)
 
      Q1S  =  Q(1) * Q(1)
      Q2S  =  Q(2) * Q(2)
      Q3S  =  Q(3) * Q(3)
 
 
      R(1,1) =  1.D0  -  2.D0 * ( Q2S + Q3S )
      R(2,1) =           2.D0 * ( Q12 + Q03 )
      R(3,1) =           2.D0 * ( Q13 - Q02 )
 
      R(1,2) =           2.D0 * ( Q12 - Q03 )
      R(2,2) =  1.D0  -  2.D0 * ( Q1S + Q3S )
      R(3,2) =           2.D0 * ( Q23 + Q01 )
 
      R(1,3) =           2.D0 * ( Q13 + Q02 )
      R(2,3) =           2.D0 * ( Q23 - Q01 )
      R(3,3) =  1.D0  -  2.D0 * ( Q1S + Q2S )
 
      RETURN
      END

