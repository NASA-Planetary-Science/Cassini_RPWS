C$Procedure      ASDIAM_G ( Angular semi-diameter of body )
 
      DOUBLE PRECISION FUNCTION ASDIAM_G ( TARGET, OBSRVR, ET, CORR )
 
C$ Abstract
C
C     Return the angular semi-diameter of a target body as seen from
C     a specified observing body at a specified ephemeris time.
C
C$ Required_Reading
C
C     KERNEL
C     SPK
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     GLLSPICE
C
C$ Declarations
 
      INTEGER               TARGET
      INTEGER               OBSRVR
      DOUBLE PRECISION      ET
      CHARACTER*(*)         CORR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARGET     I   NAIF integer code of target body.
C     OBSRVR     I   NAIF integer code of observing body.
C     ET         I   Epoch in ephemeris seconds past J2000.
C     CORR       I   Desired aberration correction.
C
C     The function returns the angular semi-diameter of the target body
C     (in radians).
C
C$ Detailed_Input
C
C     TARGET         is the NAIF integer code of the target body.
C
C     OBSRVR         is the NAIF integer code of the observing body.
C
C     ET             is the epoch in ephemeris seconds past J2000 for
C                    which the angular semi-diameter of the target body
C                    is to be computed.
C
C     CORR           is the aberration correction to be used in
C                    computing the location and orientation of the
C                    target body.  Possible values are:
C
C                       'NONE'        No aberration correction.
C
C                       'LT'          Correct position and orientation
C                                     of target body for light time.
C
C                       'LT+S'        Correct position and orientation
C                                     of target body for light time and
C                                     stellar aberration.
C
C$ Detailed_Output
C
C     The function returns the angular semi-diameter of the target
C     body, as seen from the observing body, at the time specified by
C     ET.  The angular semi-diameter of the body is defined as half of
C     the angular separation between the vectors from the observer to
C     the endpoints of the major axis of the limb of the target body.
C     The units are radians.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the body and target ID codes are identical, the error
C         SPICE(BODIESNOTDISTINCT) is signalled.  ASDIAM_G is set
C         to zero.
C
C     2)  If no SPK (ephemeris) data is available for the observer and
C         target at the time specified by ET, the error will be
C         diagnosed by routines called by this routine.  If light time
C         corrections are used, SPK data for the target body must be
C         available at the time ET - LT, where LT is the one-way light
C         time from the target to the observer at ET.
C
C     3)  If PCK data defining the orientation or shape of the target
C         body is unavailable, the error will be diagnosed by routines
C         called by this routine.
C
C$ Files
C
C     No files are input to this routine.  However, ASDIAM_G expects
C     that the appropriate SPK and PCK files have been loaded using
C     SPKLEF and LDPOOL, respectively.
C
C$ Particulars
C
C     An SPK file containing ephemeris data covering TARGET and OBSRVR
C     at time ET must be loaded via SPKLEF before calling ASDIAM_G.
C     Additionally, if light time correction is used, the SPK file must
C     cover TARGET at time ET - LT, where LT is the light time between
C     the target and observer at ET.
C
C     This routine assumes that the target body is modelled as a
C     triaxial ellipsoid, and that a PCK file containing its radii and
C     rotation model constants has been loaded into the kernel pool
C     using the subroutine LDPOOL.
C
C$ Examples
C
C     1)  Find the angular semi-diameter of the Moon as
C         seen from the Earth on Dec. 8, 1990 at 21:00 UTC.
C
C
C            C
C            C     SPICELIB functions
C            C
C                  DOUBLE PRECISION      DPR
C
C            C
C            C     GLLSPICE functions
C            C
C                  DOUBLE PRECISION      ASDIAM_G
C
C            C
C            C     Local variables
C            C
C                  DOUBLE PRECISION      ASD
C                  DOUBLE PRECISION      ET
C
C                  INTEGER               HANDLE
C                  INTEGER               OBSRVR
C                  INTEGER               TARGET
C
C                  LOGICAL               FOUND
C
C            C
C            C     Load an SPK file containing ephemeris data for Earth
C            C     and Moon on Dec. 8, 1990 21:00.  (The names used
C            C     here are fictitious; you must use the names of
C            C     actual files in your own program).
C            C
C                  CALL SPKLEF ( 'EARTHMOON_SPK.BSP', HANDLE )
C
C            C
C            C     Load a PCK kernel containing shape and orientation
C            C     constants for the Moon.
C            C
C                  CALL LDPOOL ( 'MOON_PCK.TPC' )
C
C            C
C            C     Load a leapseconds kernel for UTC-to-ET conversion.
C            C
C                  CALL LDPOOL ( 'LEAP.TLS' )
C
C            C
C            C     Find the ephemeris time corresponding to Dec. 8,
C            C     1990 21:00 UTC.
C            C
C                  CALL UTC2ET ( 'December 8 1990 21:00', ET )
C
C            C
C            C     Find the body codes for Earth and Moon (they are
C            C     399 and 301, respectively).  We don't have to
C            C     check FOUND, since the codes for the Earth and
C            C     Moon are always available from the GLLSPICE routine
C            C     BODN2C_G.
C            C
C                  CALL BODN2C_G ( 'Earth', OBSRVR, FOUND )
C                  CALL BODN2C_G ( 'Moon',  TARGET, FOUND )
C
C            C
C            C     Find the angular semi-diameter of the Moon.  Use
C            C     light time correction.
C            C
C            C     Use the SPICELIB function DPR to convert the
C            C     result to degrees.
C            C
C                  ASD = ASDIAM_G ( TARGET, OBSRVR, ET, 'LT' ) * DPR()
C
C            C
C            C     Print out the result.
C            C
C                  PRINT *, 'Angular semi-diameter of the Moon ' //
C                 .         'as seen from the Earth (degrees): ', ASD
C
C                  END
C
C
C$ Restrictions
C
C     1)  An SPK file containing ephemeris data for the observer and
C         target bodies and a PCK file containing and shape and
C         orientation constants for the target body must be loaded
C         prior to calling this routine.
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
C-    GLLSPICE Version 1.0.0, 04-DEC-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     angular semi-diameter of body
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VSEP
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )
 
C
C     Local variables
C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      ENDPT1 ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
 
      INTEGER               N
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ASDIAM_G' )
      END IF
 
C
C     The observer and target should not be identical.
C
      IF ( TARGET .EQ. OBSRVR ) THEN
 
         ASDIAM_G = 0.D0
 
         CALL SETMSG ( 'Observer and target id codes are both #.' )
         CALL ERRINT ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                 )
         CALL CHKOUT ( 'ASDIAM_G'                                 )
         RETURN
 
      END IF
 
 
C
C     Find the state of the target as seen from the observer at ET.
C
      CALL SPKEZ ( TARGET, ET, 'J2000', CORR, OBSRVR, STATE, LT )
 
C
C     Get the transformation from inertial to body-fixed (body equator
C     and prime meridian) coordinates for the target body.  (TIPM
C     stands for `transformation from inertial to prime meridian'.)
C     Use light time corrections if necessary.
C
      IF ( CORR .EQ. 'NONE' ) THEN
 
         CALL BODMAT ( TARGET, ET,    TIPM )
      ELSE
         CALL BODMAT ( TARGET, ET-LT, TIPM )
      END IF
 
C
C     Negate the observer-target vector, and transform it to
C     body-fixed coordinates.
C
      CALL VMINUS ( STATE, OBSPOS )
      CALL MXV    ( TIPM,  OBSPOS,  OBSPOS )
 
C
C     Find the radii of the target body.
C
      CALL BODVAR ( TARGET, 'RADII', N, RADII )
 
C
C     Find the limb of the body as seen from the observer.
C
      CALL EDLIMB ( RADII(1), RADII(2), RADII(3), OBSPOS, LIMB )
 
C
C     Get the center and semi-axes of the limb.
C
      CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )
 
C
C     Find the endpoints of the major axis.
C
      CALL VADD ( CENTER, SMAJOR, ENDPT1 )
      CALL VSUB ( CENTER, SMAJOR, ENDPT2 )
 
C
C     Find the vectors from the observer to the endpoints of the
C     major axis of the limb.
C
      CALL VSUB ( OBSPOS, ENDPT1, V1 )
      CALL VSUB ( OBSPOS, ENDPT2, V2 )
 
C
C     Find half the angular separation between the vectors from the
C     observer to the endpoints of the major axis of the limb.  This
C     is the angular semi-diameter of the target.
C
      ASDIAM_G  =  0.5D0  *  VSEP ( V1, V2 )
 
      CALL CHKOUT ( 'ASDIAM_G' )
      RETURN
      END
