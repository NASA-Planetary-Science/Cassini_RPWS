C$Procedure      SCNOFF_G ( Scan platform offset transformation )
 
      SUBROUTINE SCNOFF_G ( TWIST, XSCAN, SCAN, TRANS )
 
C$ Abstract
C
C     Find the coordinate transformation defined by Galileo scan,
C     cross-scan, and twist offsets.
C
C$ Required_Reading
C
C     CK
C     ROTATIONS
C
C$ Keywords
C
C     CONVERSION
C     COORDINATES
C     GLLSPICE
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations
 
      DOUBLE PRECISION      TWIST
      DOUBLE PRECISION      XSCAN
      DOUBLE PRECISION      SCAN
      DOUBLE PRECISION      TRANS  ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TWIST      I   Twist offset (radians).
C     XSCAN      I   Cross-scan (cross-cone) offset (radians).
C     SCAN       I   Scan (cone) offset (radians).
C     TRANS      O   Transformation from scan platform to offset frame.
C
C$ Detailed_Input
C
C     TWIST,
C     XSCAN,
C     SCAN           are, respectively, twist, cross-scan, and scan
C                    offsets that define the orientation of a scan
C                    platform instrument reference frame with respect
C                    to the Galileo scan platform M-N-L frame.  These
C                    angles are given in units of radians.  The offset
C                    angles constitute a 3-1-2 set of Euler angles.
C                    The transformation matrix they define is
C
C                       [ TWIST ]   [ XSCAN ]   [ SCAN ]
C                                3           1          2
C
C                    The notation
C
C                       [ theta ]
C                                i
C
C                    denotes a coordinate system rotation of theta
C                    radians about the ith coordinate axis.  See the
C                    ROTATIONS required reading for more information
C                    about Euler angles.
C
C                    The terms `cone offset' and `cross-cone offset'
C                    are sometimes used in place of `scan offset' and
C                    `cross-scan offset'.
C
C$ Detailed_Output
C
C     TRANS          is the coordinate transformation matrix defined
C                    by the input offset angles:
C
C                       TRANS   =   [ TWIST ]   [ XSCAN ]   [ SCAN ] .
C                                            3           1          2
C
C                    See the ROTATIONS required reading for more
C                    information about Euler angles.
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
C     The orientations of instrument-fixed coordinate systems on the
C     Galileo scan platform are sometimes described by `offset angles'.
C     These offset angles are Euler angles that define a coordinate
C     transformation from the scan platform M-N-L frame to an
C     instrument-fixed frame.  The particular set of Euler angle
C     rotations supported by this routine is:
C
C        -- First rotation:  an offset of SCAN radians about the
C           scan platform N-axis (which is the second, or `y', axis).
C           This rotation may be called a `scan offset' or a `cone
C           offset'.
C
C        -- Second rotation:  an offset of XSCAN radians about the
C           first axis of the cordinate system obtained by applying
C           the SCAN rotation to the M-N-L frame.  If the axes of
C           the frame resulting from the first rotation are called
C           M'-N'-L', this rotation is about the M' axis.  This rotation
C           may be called a `cross-scan offset' or a `cross-cone
C           offset'.
C
C        -- Third rotation:  an offset of TWIST radians about the
C           third axis of the coordinate system obtained by applying
C           the first two rotations to the M-N-L frame.  If the axes
C           of the frame resulting from the first and second rotations
C           are called M''-N''-L'', this rotation is about the L'' axis.
C
C     This subroutine computes the transformation matrix that converts
C     vectors from scan platform coordinates to those of an
C     instrument-fixed system, when the instrument-fixed system is
C     described by offset angles.  If V is a vector specified in the
C     scan platform system, the output matrix TRANS converts V to
C     instrument-fixed coordinates via the multiplication
C
C        V = TRANS * V.
C
C     To produce a coordinate transformation matrix for an instrument
C     coordinate system that is defined by just TWO offsets (scan and
C     cross-scan), this routine should be called with a TWIST angle of
C     zero.  The boresight direction of the output matrix TRANS will
C     be the third row of TRANS.
C
C$ Examples
C
C     1)  Find the inertial (EME50) pointing of the NIMS boresight at
C         SCLK time 00597190:00:3.  Suppose that the NIMS boresight is
C         offset from the SSI boresight by a scan angle of SCAN radians
C         and a cross-scan angle of XSCAN radians.  The following
C         program carries out this computation.
C
C
C                  PROGRAM NIMSPT
C
C            C
C            C     Local parameters
C            C
C
C            C
C            C     Scan and cross-scan NIMS boresight offsets in
C            C     radians (actual values probably are different):
C            C
C                  DOUBLE PRECISION      SCAN
C                  PARAMETER           ( SCAN  = 0.25D-3 )
C
C                  DOUBLE PRECISION      XSCAN
C                  PARAMETER           ( XSCAN = 0.25D-3 )
C
C                  INTEGER               FILEN
C                  PARAMETER           ( FILEN  = 128 )
C
C                  INTEGER               SCLKLN
C                  PARAMETER           ( SCLKLN =  30 )
C
C
C            C
C            C     Local variables
C            C
C                  CHARACTER*(FILEN)     CKER
C                  CHARACTER*(SCLKLN)    SCLK
C                  CHARACTER*(FILEN)     SCLKER
C
C                  DOUBLE PRECISION      CLKOUT
C                  DOUBLE PRECISION      CMAT   ( 3, 3 )
C                  DOUBLE PRECISION      NIMDIR ( 3 )
C                  DOUBLE PRECISION      SCLKDP
C                  DOUBLE PRECISION      SSIDIR ( 3 )
C                  DOUBLE PRECISION      TI2NIM ( 3, 3 )
C                  DOUBLE PRECISION      TOL
C                  DOUBLE PRECISION      TRANS  ( 3, 3 )
C
C                  INTEGER               HANDLE
C                  INTEGER               I
C
C                  LOGICAL               FOUND
C
C            C
C            C     Get the name of an SCLK kernel and a C kernel
C            C     covering scan platform pointing at the time of
C            C     interest.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Enter the name of a scan platform ' //
C                 .            'C kernel covering the time of interest.'
C                  READ  (*,FMT='(A)') CKER
C                  WRITE (*,*) 'Enter the name of an SCLK kernel.'
C                  READ  (*,FMT='(A)') SCLKER
C
C                  CALL CKLPF  ( CKER,  HANDLE )
C                  WRITE (*,*) 'C kernel loaded.'
C
C                  CALL LDPOOL ( SCLKER        )
C                  WRITE (*,*) 'SCLK kernel loaded.'
C
C            C
C            C     Encode the SCLK time of interest.  Use the Galileo
C            C     orbiter NAIF integer code, which is -77.
C            C
C                  CALL SCENCD ( -77, '00597190:00:3', SCLKDP )
C
C            C
C            C     Use a lookup tolerance of 6 RTI's.
C            C
C                  CALL SCTIKS ( -77, '0:00:6', TOL )
C
C            C
C            C     Find the scan platform pointing at the time of
C            C     interest, in EME50 coordinates.  In SPICELIB, use the
C            C     name FK4 to get EME50 coordinates.  The NAIF integer
C            C     code for the Galileo scan platform is -77001.
C            C
C                  CALL CKGP ( -77001,  SCLKDP,  TOL,  'FK4',
C                 .             CMAT,   CLKOUT,  FOUND        )
C
C                  IF ( .NOT. FOUND ) THEN
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'No pointing found at time ',SCLK
C
C                  ELSE
C
C                     CALL SCNOFF_G  ( 0.D0, XSCAN, SCAN, TRANS )
C
C            C
C            C        The transformation from EME50 to NIMS coordinates
C            C        is
C            C
C            C            TRANS * CMAT.
C            C
C            C        The NIMS boresight direction in EME50 coordinates
C            C        is the third row of this matrix.
C            C
C            C        The SSI boresight direction in EME50 coordinates
C            C        is the third row of the C matrix.
C            C
C                     CALL MXM ( TRANS, CMAT, TI2NIM )
C
C                     DO I = 1, 3
C                        NIMDIR(I) = TI2NIM(3,I)
C                        SSIDIR(I) = CMAT  (3,I)
C                     END DO
C
C                     WRITE (*,*) 'NIMS boresight direction (EME50):'
C                     WRITE (*,*)  NIMDIR
C                     WRITE (*,*) 'SSI boresight direction (EME50):'
C                     WRITE (*,*)  SSIDIR
C                  END IF
C
C                  END
C
C$ Restrictions
C
C     1) Galileo specific.
C
C$ Literature_References
C
C     1) GLL-3-180C, `Functional requirement, Galileo Orbiter Flight
C        Equipment Configuration.'  April 28, 1989.  Pp. 23, 29, 43.
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
C     scan platform offset transformation
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
         CALL CHKIN ( 'SCNOFF_G' )
      END IF
 
C
C     The offsets define a 3-1-2 rotation.
C
      CALL EUL2M ( TWIST, XSCAN, SCAN, 3, 1, 2, TRANS )
 
 
      CALL CHKOUT ( 'SCNOFF_G' )
      RETURN
      END
