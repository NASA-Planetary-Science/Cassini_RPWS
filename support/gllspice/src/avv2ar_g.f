C$Procedure   AVV2AR_G ( Angular velocity vector to angular rates )
 
      SUBROUTINE AVV2AR_G ( ANGVEL, CMAT, RATE1, RATE2, RATE3 )
 
C$ Abstract
C
C     Find the angular rates about the coordinate axes of a specified
C     instrument-fixed coordinate system that correspond to a specified
C     angular velocity vector.
C
C$ Required_Reading
C
C     CK
C     ROTATIONS
C
C$ Keywords
C
C     GLLSPICE
C     ROTATION
C
C$ Declarations
 
      DOUBLE PRECISION      ANGVEL ( 3 )
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      RATE1
      DOUBLE PRECISION      RATE2
      DOUBLE PRECISION      RATE3
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ANGVEL     I   Angular velocity relative to inertial frame.
C     CMAT       I   A `C-matrix'.
C     RATE1,
C     RATE2,
C     RATE3      O   Angular rates in CMAT frame (radians/sec).
C
C$ Detailed_Input
C
C     ANGVEL         is an angular velocity vector expressed relative to
C                    the inertial coordinate system that is the basis of
C                    CMAT (see below).  The magnitude of ANGVEL
C                    represents a rotational rate, in radians/second.
C
C     CMAT           A rotation matrix defining a transformation from
C                    an inertial coordinate system to a system fixed to
C                    a spacecraft structure or instrument.  The rows
C                    of CMAT are the basis vectors of the structure or
C                    instrument coordinate system, expressed relative
C                    to the inertial system.
C
C$ Detailed_Output
C
C     RATE1,
C     RATE2,
C     RATE3          are angular rates about the first, second, and
C                    third coordinate axes of the coordinate system
C                    specified by CMAT.  The rates are the components of
C                    the input angular velocity vector, expressed
C                    relative to to the coordinate system specified by
C                    CMAT.  Units are radians/second.
C
C                       Let the notation <a, b> denote the inner product
C                    of vectors a and b.  Then  RATE1, RATE2, and RATE3
C                    are defined by the equations
C
C                       RATE1 = < ANGVEL, row1 >
C                       RATE2 = < ANGVEL, row2 >
C                       RATE3 = < ANGVEL, row3 >,
C
C                    where row1, row2, and row3 are the rows of CMAT.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Note that an angular velocity vector is the sum of its components
C     with respect to any three orthogonal axes.  Given those
C     components, we can construct the original angular velocity vector.
C
C     This routine has an inverse:  the routine AR2AVV converts a set
C     of angular rates about specified axes to an angular velocity
C     vector.
C
C     This routine is supplied as a convenience, to simplify extracting
C     inertially referenced cone and cross-cone SSI boresight rates
C     from Galileo C-kernels.
C
C$ Examples
C
C     1)  Given an inertial angular velocity vector for the scan
C         platform, extract inertial cone and cross-cone rates.
C
C         The basis vectors for the scan platform coordinate system
C         `M, N, L', represented in inertial (for example, EME50)
C         coordinates, are available from the scan platform C-matrix.
C         M is the `cross-cone' axis, N is the `cone' axis, and L is the
C         SSI boresight direction.  In the C-matrix, the first row is
C         M, the second N, and the third L, where all vectors are
C         expressed in inertial coordinates.
C
C         We would use the following code fragment to find the scan
C         platform cone and cross-cone rates:
C
C            C
C            C     Find the (inertial) cone and cross-cone rates of
C            C     the SSI boresight.  Note that only the cone and
C            C     cross-cone components of the angular velocity vector
C            C     are valid.  Here CMAT is the C-matrix for the
C            C     scan platform.  CRSSRT and CONERT are the cross-cone
C            C     and cone rates with respect to inertial space.
C            C
C                  CALL AVV2AR_G ( ANGVEL, CMAT, CRSSRT, CONERT, BOGUS )
C
C
C     2)  If we did use a valid twist rate, the last call would look
C         like this:
C
C                  CALL AVV2AR_G ( ANGVEL, CMAT, CRSSRT, CONERT, TWSRT )
C
C$ Restrictions
C
C     1)  Galileo specific.
C
C     2)  If the input angular velocity vector ANGVEL is constructed
C         from angular rates from telemetry, then ANGVEL must be used
C         with care.  Only the components of the angular velocity
C         for which rates are available will be valid.
C
C         For example, if inertial cone and cross-cone rates are used to
C         construct an angular velocity vector, and no twist information
C         is used, then the only valid information that can be extracted
C         from that vector are cone and cross-cone rates.  Angular rates
C         about any of the axes of the inertial frame will be invalid,
C         in general.
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
C-    GLLSPICE 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE 1.0.0, 06-DEC-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     angular velocity vector to angular rates
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
C
C     Local variables
C
      DOUBLE PRECISION      TRANAV
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'AVV2AR_G' )
      END IF
 
C
C     Transform the angular velocity to the system defined by CMAT:
C
      CALL MXV ( CMAT, ANGVEL, TRANAV )
 
C
C     The rates are the components of TRANAV:
C
      CALL VUPACK ( TRANAV, RATE1, RATE2, RATE3 )
 
 
 
      CALL CHKOUT ( 'AVV2AR_G' )
      RETURN
      END
