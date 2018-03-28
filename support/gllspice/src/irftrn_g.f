C$Procedure      IRFTRN_G ( Inertial reference frame transformations )
 
      SUBROUTINE IRFTRN_G ( REFA, REFB, ROTAB )
 
C$ Abstract
C
C     Return the coordinate transformation matrix required to rotate
C     vectors from one specified inertial reference frame to another.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     CONVERSION
C     COORDINATES
C     FRAME
C     GLLSPICE
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations
 
      CHARACTER*(*)         REFA
      CHARACTER*(*)         REFB
      DOUBLE PRECISION      ROTAB ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     REFA       I   Name of reference from to transform vectors FROM.
C     REFB       I   Name of reference from to transform vectors TO.
C     ROTAB      O   REFA-to-REFB transformation matrix.
C
C$ Detailed_Input
C
C     REFA,
C     REFB           are the names of two inertial reference frames.
C                    Any names acceptable to the SPICELIB routine
C                    IRFNAM (an entry point of the SPICELIB routine
C                    CHGIRF) may be used.  See $Particulars for a
C                    list of names.
C
C                    In addition, the names
C
C                       'ECLIP1950'
C                       'ECLIP2000'
C
C                    may be used.  These names refer, respectively, to
C                    the coordinate systems obtained by rotating the
C                    the EME50 and J2000 coordinate frames about their
C                    x-axes by angles equal to the obliquity of the
C                    ecliptic at the epochs of the two systems.  These
C                    epochs are B1950 and J2000, respectively.
C
C$ Detailed_Output
C
C     ROTAB          is a rotation matrix that transforms
C                    the coordinates of a vector V relative to the
C                    reference frame specified by REFA to the
C                    coordinates of V relative to the reference frame
C                    specified by REFB.  The transformation is carried
C                    out by the matrix multiplication
C
C                       V = ROTAB * V.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If ROTA or ROTB are not recognized, the error will be
C         diagnosed by routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Among the reference frame names accepted by IRFNAM are:
C
C        'J2000'
C        'B1950'
C        'FK4'
C        'DE-96'
C        'DE-102'
C        'DE-108'
C        'DE-111'
C        'DE-114'
C        'DE-118'
C        'DE-122'
C        'DE-125'
C        'DE-130'
C        'DE-200'
C        'DE-202'
C        'GALACTIC'
C
C     See the SPICELIB routine GHGIRF for details.
C
C$ Examples
C
C     1)  Transform a vector V from EME50 to J2000 coordinates.
C
C            C
C            C     The SPICELIB name for EME50 is `FK4', the
C            C     name of the star catalog on which the frame
C            C     is based.  So, we'll ask IRFTRN_G for the
C            C     matrix that transforms vectors from FK4 to
C            C     J2000 coordinates.
C            C
C                  CALL IRFTRN_G ( 'J2000', 'FK4', TRANS )
C
C            C
C            C     Now transform V to EME50 coordinates.
C            C
C                  CALL MXV ( TRANS, V, V )
C
C
C     2)  Transform a vector V from EME50 to ecliptic coordinates
C         (at epoch B1950).  Then transform it back to EME50
C         coordinates.
C
C            C
C            C     Get the EME50-to-ecliptic of B1950 transformation.
C            C
C                  CALL IRFTRN_G ( 'FK4', 'ECLIP1950', TRANS )
C
C            C
C            C     Transform V to ecliptic of B1950 coordinates.
C            C
C                  CALL MXV ( TRANS, V, V )
C
C            C
C            C     Transform V from ecliptic of B1950 coordinates to
C            C     EME50 coordinates.
C            C
C            C     The inverse of TRANS is its transpose.  The
C            C     SPICELIB routine MTXV left-multiplies a vector
C            C     by the TRANSPOSE of a specified matrix.
C            C
C                  CALL MTXV ( TRANS, V, V )
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
C-    GLLSPICE Version 1.0.0, 04-DEC-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     inertial reference frame transformations
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      RPD
      LOGICAL               RETURN
 
C
C     Local parameters
C
 
C
C     OB1950 is Newcomb's value for the obliquity of the ecliptic
C     at B1950 (in degrees).
C
      DOUBLE PRECISION      OB1950
      PARAMETER           ( OB1950 = 23.44578787500434 )
 
C
C     OB2000 is Lieske's value for the obliquity of the ecliptic
C     at J2000 (in degrees).
C
      DOUBLE PRECISION      OB2000
      PARAMETER           ( OB2000 = 23.43929111111111 )
 
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 20 )
 
C
C     Local variables
C
      CHARACTER*(NAMLEN)    TEMPA
      CHARACTER*(NAMLEN)    TEMPB
 
      DOUBLE PRECISION      EC1950 ( 3, 3 )
      DOUBLE PRECISION      EC2000 ( 3, 3 )
      DOUBLE PRECISION      IDENT  ( 3, 3 )
      DOUBLE PRECISION      LEFT   ( 3, 3 )
      DOUBLE PRECISION      RIGHT  ( 3, 3 )
 
      INTEGER               NUMA
      INTEGER               NUMB
 
      LOGICAL               FIRST
 
 
C
C     Saved variables
C
      SAVE                  EC1950
      SAVE                  EC2000
      SAVE                  FIRST
      SAVE                  IDENT
 
C
C     Initial values
C
      DATA FIRST / .TRUE.            /
 
      DATA IDENT / 1.D0, 0.D0, 0.D0,
     .             0.D0, 1.D0, 0.D0,
     .             0.D0, 0.D0, 1.D0  /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'IRFTRN_G' )
      END IF
 
C
C     The first time through, compute the EME50-to-ECLIP1950
C     and J2000-to-ECLIP2000 transformations.
C
      IF ( FIRST ) THEN
 
         CALL ROTATE (  RPD() * OB1950, 1, EC1950  )
         CALL ROTATE (  RPD() * OB2000, 1, EC2000  )
         FIRST    =    .FALSE.
 
      END IF
 
C
C     Get left-justified, upper case versions of our reference frame
C     names.
C
      CALL LJUST ( REFA,  TEMPA )
      CALL UCASE ( TEMPA, TEMPA )
 
      CALL LJUST ( REFB,  TEMPB )
      CALL UCASE ( TEMPB, TEMPB )
 
C
C     Ecliptic coordinates are a special case.  ECLIP1950
C     hooks up with FK4; ECLIP2000 with J2000.
C
C     Eventually, this special case should go away; CHGIRF will handle
C     ecliptic coordinates.
C
      IF ( TEMPA .EQ. 'ECLIP1950' ) THEN
 
         CALL MOVED ( EC1950, 9, RIGHT )
         TEMPA    =   'FK4'
 
      ELSE IF ( TEMPA .EQ. 'ECLIP2000' ) THEN
 
         CALL MOVED ( EC2000, 9, RIGHT )
         TEMPA    =   'J2000'
 
      ELSE
         CALL MOVED ( IDENT,  9, RIGHT )
      ENDIF
 
 
 
      IF ( TEMPB .EQ. 'ECLIP1950' ) THEN
 
         CALL MOVED ( EC1950, 9, LEFT )
         TEMPB    =   'FK4'
 
      ELSE IF ( TEMPB .EQ. 'ECLIP2000' ) THEN
 
         CALL MOVED ( EC2000, 9, LEFT )
         TEMPB    =   'J2000'
 
      ELSE
         CALL MOVED ( IDENT,  9, LEFT )
      ENDIF
 
C
C     Find the transformation from TEMPA to TEMPB.
C
      CALL IRFNUM ( TEMPA, NUMA )
      CALL IRFNUM ( TEMPB, NUMB )
      CALL IRFROT ( NUMA,  NUMB,  ROTAB )
 
C
C     Take the ecliptic transformations into account, if necessary.
C
      CALL MXMT   ( ROTAB, RIGHT, ROTAB )
      CALL MXM    ( LEFT,  ROTAB, ROTAB )
 
      CALL CHKOUT ( 'IRFTRN_G' )
      RETURN
      END
