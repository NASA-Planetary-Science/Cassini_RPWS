C$Procedure   AR2AVV_G ( Angular rates to angular velocity vector )
 
      SUBROUTINE AR2AVV_G ( RATE1, RATE2, RATE3, CMAT, ANGVEL )
 
C$ Abstract
C
C     Construct an angular velocity vector from angular rates about the
C     coordinate axes of a specified instrument-fixed coordinate system.
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
 
      DOUBLE PRECISION      RATE1
      DOUBLE PRECISION      RATE2
      DOUBLE PRECISION      RATE3
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      ANGVEL ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     RATE1,
C     RATE2,
C     RATE3      I   Angular rates in CMAT frame (radians/sec).
C     CMAT       I   A `C-matrix'.
C     ANGVEL     O   Angular velocity relative to inertial frame.
C
C$ Detailed_Input
C
C     RATE1,
C     RATE2,
C     RATE3          are angular rates about the first, second, and
C                    third coordinate axes of the coordinate system
C                    specified by CMAT.  The rates are the components of
C                    an angular velocity vector, expressed relative to
C                    to the coordinate system specified by CMAT.  Units
C                    are radians/second.
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
C     ANGVEL         is the angular velocity vector defined by the
C                    input rates, expressed relative to the inertial
C                    coordinate system that is the basis of CMAT.
C                    ANGVEL is just
C
C                       RATE1 * row1  +  RATE2 * row2  +  RATE3 * row3,
C
C                    where row1, row2, and row3 are the rows of CMAT.
C
C                    The magnitude of ANGVEL represents a rotational
C                    rate, in radians/second.
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
C     This routine has an inverse:  the routine AVV2AR converts an
C     angular velocity vector to angular rates about specified axes.
C
C     This routine is supplied as a convenience, to simplify the
C     construction of Galileo C kernels.  In this application, the
C     input rates are cross-cone, cone, and twist rates of the scan
C     platform L axis, measured with respect to inertial space.  The
C     twist rate would normally be set to zero, since it is not
C     available from telemetry.
C
C$ Examples
C
C     1)  Construct an inertial angular velocity vector for the scan
C         platform, given inertial cone and cross-cone rates.
C
C         The basis vectors for the scan platform coordinate system
C         `M, N, L', represented in inertial (for example, EME50)
C         coordinates, are available from the scan platform C-matrix.
C         M is the `cross-cone' axis; N is the `cone' axis, and L is the
C         SSI boresight direction.  In the C-matrix, the first row is
C         M, the second N, and the third is L, where all vectors are
C         expressed in inertial coordinates.
C
C         We would use the following code fragment to find the angular
C         velocity of the scan platform, in inertial coordinates:
C
C            C
C            C     Find the angular velocity of the scan platform.
C            C     Note that only the cone and cross-cone components
C            C     are known.  Here CMAT is the C-matrix for the
C            C     scan platform.  CRCONE and CONE are the cross-cone
C            C     and cone rates with respect to inertial space.
C            C
C                  CALL AR2AVV_G ( CRCONE, CONE, 0.D0, CMAT, ANGVEL )
C
C$ Restrictions
C
C     1)  Galileo specific.
C
C
C     2)  If any of the input angular rates is estimated or is not
C         available, then the resulting angular velocity vector must be
C         used with care.  Only the components of the angular velocity
C         for which rates are available will be valid.
C
C         For example, if only inertial cone and cross-cone rates are
C         used to construct an angular velocity vector, the only valid
C         information that can be extracted from that vector are cone
C         and cross-cone rates.  Angular rates about any of the axes of
C         the inertial frame will be invalid, in general.
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
C-    GLLSPICE Version 1.0.0, 06-DEC-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     angular rates to angular velocity vector
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'AR2AVV_G' )
      END IF
 
      CALL VPACK  (  RATE1,  RATE2,   RATE3,  ANGVEL )
      CALL MTXV   (  CMAT,   ANGVEL,  ANGVEL         )
 
      CALL CHKOUT ( 'AR2AVV_G' )
      RETURN
      END
