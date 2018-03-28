C$Procedure      NIMMP_G (NIMS mirror position)
 
      SUBROUTINE NIMMP_G ( DT, MODE, VPOINT )
 
C$ Abstract
C
C     Find the Near Infrared Mapping Spectrometer (NIMS) mirror
C     position expressed as a vector relative to the scan platform
C     MNL frame.
C
C$ Required_Reading
C
C     GLL_IK
C
C$ Keywords
C
C     POINTING
C
C$ Declarations
 
      DOUBLE PRECISION      DT
      CHARACTER*(*)         MODE
      DOUBLE PRECISION      VPOINT ( 3 )
 
      INTEGER               OKVER
      PARAMETER           ( OKVER = 1 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      DT        I    Time offset from start the of a RIM, in seconds.
C      MODE      I    A name of a NIMS operating mode.
C      VPOINT    O    Pointing vector expressed relative to scan
C                     platform.
C      OKVER     P    Version of I-kernel needed.
C
C$ Detailed_Input
C
C     DT           is the time offset in a RIM at which the mirror
C                  position is desired.  Its value must be between zero
C                  and the length of one minor frame, nominally 60 2/3
C                  seconds.
C
C     MODE         is the name of a valid NIMS operating mode.  The
C                  mode name may be entered in upper or lower case.
C                  There are six modes:
C
C                       'LONG'
C                       'FULL'
C                       'SHORT'
C                       'FIXED'
C                       'BANDEDGE'
C                       'SPECTROMETER'
C
C$ Detailed_Output
C
C     VPOINT      is a unit vector expressing the NIMS mirror
C                 position relative to the scan platform frame (MNL).
C
C$ Parameters
C
C      OKVER      is the version of the NIMS I-kernel that must be
C                 loaded into the kernel pool.  That is, the
C                 I-kenrel must be in the form OKVER.X.
C
C$ Exceptions
C
C     1)          If MODE is not the name of aa valid NIMS RIM mode,
C                 the error SPICE(INVALIDMODE) is signalled.
C
C     2)          If DT is not within the bounds of a RIM, the
C                 error SPICE(INVALIDTIME) is signalled.
C
C     3)          If the NIMS I-kernel currently in the kernel pool is
C                 not compatible with this routine the error
C                 SPICE(IKERNELNOTCOMPAT) is signalled.
C
C$ Files
C
C     A NIMS I-kernel with the correct version number must be loaded in
C     the kernel pool prior to calling this routine.
C
C$ Particulars
C
C     The mirror cone and cross cone offsets from the NIMS nominal
C     boresight are given in the I-kernel as a function of NIMS
C     operating mode and time within a RIM.  This routine looks up
C     these offsets and the boresight cone and cross cone offset from
C     the scan platform L vector and determines the components of the
C     pointing vector in the MNL frame.
C
C     In order for this routine to operate properly, a NIMS I-kernel
C     must be loaded in the kernel pool by a call to LDPOOL prior to
C     calling this routine and the version must be in the form OKVER.X.
C
C$ Examples
C
C     In the following code fragment, the NIMS mirror position is found
C     with respect to the J2000 reference frame as a fuction of absolute
C     spacecraft clock time, the time in seconds since the start of the
C     current RIM, and the NIMS operational mode.
C
C     C
C     C     Load NIMS I-kernel file.
C     C
C           CALL LDPOOL ('GLL37001.TI')
C
C     C
C     C     Get some input.
C     C
C           WRITE (*,*) 'Input encoded spacecraft clock time'
C           READ  (*,*)  SCLKDP
C
C           WRITE (*,*) 'Input valid C-kernel'
C           READ  (*,FMT='(A)') CKERNEL
C           CALL CKLPF  (CKERNEL, HANDLE )
C
C           WRITE (*,*) 'Input C-kernel tolerance'
C           READ  (*,*)  TOL
C
C           WRITE (*,*) 'Input time since start of RIM in seconds'
C           READ  (*,*)  DT
C
C           WRITE (*,*) 'Input NIMS operational mode'
C           READ  (*,FMT='(A)') MODE
C
C     C
C     C     Determine the pointing vector
C     C
C           CALL NIMMP_G ( DT, MODE, VPOINT )
C
C     C
C     C     Find the C matrix that transforms the components of a vector
C     C     expressed in J2000 coordinates to components expressed in
C     C     scan platform coordinates.  -77001 is the instrument number
C     C     of the scan platform.
C     C
C           CKGP ( -77001, SCLKDP, TOL, 'J2000', CMAT, CLKOUT, FOUND )
C
C     C
C     C     We need the inverse of the C matrix to transpose from
C     C     scan platform to inertial.
C     C
C           CALL XPOSE ( CMAT, CMAT )
C
C     C
C     C     X, Y, and Z will be the components of the pointing vector
C     C     expressed relative to the J2000 inertial frame.
C     C
C           CALL MXV    ( CMAT, VPOINT, VPOINT )
C           CALL VUPACK ( VPOINT, X, Y, Z      )
C
C$ Restrictions
C
C     The NIMS I-kernel version must be in the form VER.X.
C
C$ Literature_References
C
C     [1] ``Galileo Orbiter Flight Equipment:  Configuration'' in
C         GALILEO FUNCTIONAL REQUIREMENTS BOOK, JPL Document
C         GLL-3-180C, 28 APR 1989.
C
C$ Author_and_Institution
C
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    GLLSPICE Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 1.0.0, 23-OCT-1990 (MJS)
C
C-&
 
C$ Index_Entries
C
C     nims mirror position
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               ISRCHC
      INTEGER               LSTLED
 
C
C     Local parameters
C
C     MAXLEN      is the maximum length of the variable names
C                 that can be stored in the kernel pool.
C
C     MF          is the time in seconds of one minor frame.
C
C     NDAT        is the number of variable names read from the
C                 kernel pool.
C
C     PSNCOL      is the number of mirror position tables.
C
C     NTSAM       is the number of times in the time sampling
C                 table.
C
C     PSNROW      is the number of cross cone and cone offsets in a
C                 mirror position table.
C
C     MDNCOL      is number of mode RIM tables.
C
C     MDNROW      is the number of values in a mode RIM table.
C
C     MDLEN       is the maximum length of a mode name.
C
C     MAXMSG      is the maximum length of the error message that can
C
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32               )
 
      DOUBLE PRECISION      MF
      PARAMETER           ( MF     = 2D0/3D0          )
 
      INTEGER               NDAT
      PARAMETER           ( NDAT   = 18               )
 
      INTEGER               NTSAM
      PARAMETER           ( NTSAM  = 40               )
 
      INTEGER               PSNCOL
      PARAMETER           ( PSNCOL = 4                )
 
      INTEGER               PSNROW
      PARAMETER           ( PSNROW  = NTSAM/2         )
 
      INTEGER               MDNCOL
      PARAMETER           ( MDNCOL = 6                )
 
      INTEGER               MDNROW
      PARAMETER           ( MDNROW = 182              )
 
      INTEGER               MDLEN
      PARAMETER           ( MDLEN  = 32               )
 
      INTEGER               MAXMSG
      PARAMETER           ( MAXMSG = MAXLEN*(NDAT-1)  )
 
C
C     Local variables
C
 
C
C     Mirror Positions  - PS
C
C     The mirror position tables contain mirror cross cone and cone
C     offsets from the NIMS nominal boresight in milliradians.
C     Each table (first cross cone, then cone) is read from the
C     kernel pool and stored in a column of a two deminsional array
C     called PS.  PS has 2*PSNROW rows:  PSNROW cross cone offsets and
C     PSNROW cone offsets.  PSNCOL is the number of columns in PS
C     which is just the number of mirror position tables.  The variable
C     PSCOL is used reference a particular column or table.  The
C     variable PSROW is used to reference a particular row in the first
C     PSNROW rows (cross cone offsets).
C     The time it takes for the mirror to cycle through one mirror
C     position table is MF/2 seconds.
C
      DOUBLE PRECISION      PS ( 2*PSNROW, PSNCOL )
      INTEGER               PSROW
      INTEGER               PSCOL
 
C
C     Mode RIM's - MR
C
C     Mode RIM tables describe the motion of the mirror in one RIM
C     (91 minor frames).  Each element in a mode RIM table points to
C     one of the mirror position tables.  There are 182 elements in
C     a mode RIM table so the mirror cycles through 182 mirror position
C     tables in one RIM.  Thus, if the time offset is know within a
C     RIM and the mode is known it is possible to determine what
C     mirror position table is used for pointing by looking up in
C     the corresponding mode RIM table.  Each table is read from the
C     kernel pool and stored in a column of a two deminsional array
C     called MD.  The dimensions of MD will be 182 (MDNROW) by the
C     number of mode RIM tables (MDNCOL).  The variables MDROW and MDCOL
C     are used to reference a particular row and column respectively.
C     MDNAME is an array containing the names of the mode RIM tables.
C
      DOUBLE PRECISION      MD     ( MDNROW, MDNCOL )
      INTEGER               MDCOL
      INTEGER               MDROW
      CHARACTER*(MDLEN)     MDNAME (         MDNCOL )
 
C
C     DTMF (Time offset in minor frame) is the time since the start of
C     the current minor frame.
C
      DOUBLE PRECISION      DTMF
 
C
C     TYMSAM is the time sampling table.  It is read from the kernel
C     pool and stored as a one deminsional array.  TYMSAM has 2*PSNROW
C     elements (the number of rows in a mirror position table).
C
      DOUBLE PRECISION      TYMSAM ( 2*PSNROW )
 
C
C     LVER is the version of the NIMS I-kernel that this subroutine
C     last read.  CVER is version of the NIMS I-kernel currently in
C     the pool.  If CVER is ever different from LVER, this subroutine
C     will re-read all NIMS I-kernel data from the kernel pool.  This
C     allows for one to re-load a NIMS I-kernel any time during program
C     execution.
C
      DOUBLE PRECISION      LVER
      DOUBLE PRECISION      CVER
 
C
C     BCONE and BXCONE are the boresight cone and cross cone offsets
C     from the scan platform L vector.
C
      DOUBLE PRECISION      BCONE
      DOUBLE PRECISION      BXCONE
 
C
C     CONE and XCONE are the pointing cone and cross cone offsets from
C     the boresight vector.  MATRIX transforms the pointing vector so
C     that it is expressed in the scan platform frame.
C
      DOUBLE PRECISION      CONE
      DOUBLE PRECISION      XCONE
      DOUBLE PRECISION      MATRIX  ( 3, 3 )
 
C
C     NIMDAT cotains the contains the I-kernel variable names.
C
      CHARACTER*(MAXLEN)    NIMDAT ( NDAT )
 
      CHARACTER*(MDLEN)     MODEBG
      CHARACTER*(MAXMSG)    OUTERR
      INTEGER               I
      INTEGER               N
      LOGICAL               FOUND  ( NDAT )
      LOGICAL               QUIT
 
 
C
C     Saved variables
C
      SAVE    PS
      SAVE    MD
      SAVE    TYMSAM
      SAVE    LVER
      SAVE    MDNAME
      SAVE    NIMDAT
 
C
C     Initial values.
C
      DATA LVER      /  0.0D0                            /
 
      DATA NIMDAT    / 'INS-77037_POS_TBL_XCONE_DOWN',
     .                 'INS-77037_POS_TBL_CONE_DOWN',
     .                 'INS-77037_POS_TBL_XCONE_UP',
     .                 'INS-77037_POS_TBL_CONE_UP',
     .                 'INS-77037_POS_TBL_XCONE_WAIT',
     .                 'INS-77037_POS_TBL_CONE_WAIT',
     .                 'INS-77037_POS_TBL_XCONE_STOP',
     .                 'INS-77037_POS_TBL_CONE_STOP',
     .                 'INS-77037_TIME_TABLE',
     .                 'INS-77037_LONG_MAP',
     .                 'INS-77037_FULL_MAP',
     .                 'INS-77037_SHORT_MAP',
     .                 'INS-77037_FIXED_MAP',
     .                 'INS-77037_BANDEDGE_MAP',
     .                 'INS-77037_SPECTROMETER_MAP',
     .                 'INS-77037_BORESIGHT_XCONE_OFFSET',
     .                 'INS-77037_BORESIGHT_CONE_OFFSET',
     .                 'INS-77037_VERSION'                   /
 
      DATA MDNAME  /   'LONG',
     .                 'FULL',
     .                 'SHORT',
     .                 'FIXED',
     .                 'BANDEDGE',
     .                 'SPECTROMETER'                   /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NIMMP_G' )
      ENDIF
 
C
C     Design Notes
C     ------------
C
C     The mirror position cone and cross cone offsets are found by
C     following these steps.
C
C
C     Step 1.  Extract the NIMS instrument information from the
C              kernel pool if this is the first time this routine
C              is called or if any data has changed in the pool since
C              the previous call to this routine.
C
C
C     In steps 2 thru 7 refer to the following figure.
C
C            |  |
C            |--|MDNAME
C            |  |
C            |--|
C    MODE--->|  | --MDCOL----------------
C            |--|                       |
C            |  |   Index of            V
C            |--|   MODE in          __ __ __
C            |  |   MDNAME       MD |  |  |  |
C                                   |__|__|__|
C                                   |  |  |  |
C              ---                  |__|__|__|  --MD(MDROW, -----
C    DT---+-->|   |--MDROW--------->|  |  |  |       MDCOL)     |
C         |    ---                  |__|__|__|                PSCOL
C         |          Current                                    |
C         |          1/2 minor                                  V
C         |          frame                                   __ __ __
C         |                           |  |               PS |  |  |  |
C         |                           |--|TYMSAM            |__|__|__|
C         |                           |  |                  |  |  |  |
C         |    ---                    |--|                  |__|__|__|
C          -->|   |--DTMF------------>|  | --PSROW--------->|  |  |  |
C              ---                    |--|                  |__|__|__|
C                    Time offset      |  |   Index of       |  |  |  |
C                    in minor         |--|   the greatest   |__|__|__|
C                    frame            |  |   time in
C                                            TYMSAM less         |
C                        DT                  than or             V
C          MDROW = INT( ---- + 1 )           equal to       XCONE, CONE
C                       MF/2                 DTMF
C                             DT
C          DTMF  = DT - INT( ---- )
C                             MF
C
C
C     Step 2.  Find the index of MODE in the array of mode names,
C              MDNAME.  Assign this value to MDCOL.
C
C     Step 3.  Determine which 1/2 minor frame we are in.  Assign this
C              value to MDROW.
C
C     Step 4.  Look up MD(MDROW,MDCOL).  This value points to the valid
C              mirror position table (column of PS) and is named PSCOL.
C
C     Step 5.  Determine the time offset in the current minor frame.
C              This is the value of DTMF.
C
C     Step 6.  Find the index of the greatest time in the time sampling
C              table, TYMSAM, less than or equal to DTMF/MF.  The index
C              (PSROW) tells us which row to look up in PS.
C
C     Step 7.  Look up PS(PSROW,PSCOL).  This is the cross cone offset
C              from the NIMS boresight.  PSNROW is the number of
C              possible mirror positions in a 1/2 minor frame, thus
C              PS(PSROW+PSNROW,PSCOL) is the cone offset since the
C              cone offsets are stored after the cross cone offsets in
C              the rows of PS.
C
C     Step 8.  Add the offset of the NIMS boresight vector from the
C              scan platform L vector.
C
 
C
C     First lets first check if DT is within the bounds of a RIM.
C     One RIM has NMRTI entries and each entry is MF/2 seconds,
C     therefore, DT should be greater or equal to zero, but less
C     than or equal to (MDNROW/2 * MF) seconds.
C
      IF ( (DT .LT. 0.0) .OR. (DT .GT. (MDNROW/2)*MF) ) THEN
         CALL SETMSG ( 'Time # seconds is not within a RIM'  )
         CALL ERRDP  ( '#', DT                               )
         CALL SIGERR ( 'SPICE(INVALIDTIME)'                  )
         CALL CHKOUT ( 'NIMMP_G'                             )
         RETURN
      ENDIF
 
C
C     Step 1.
C
C     Check if the routine has to read NIMS data from the kernel
C     pool.  It will only read in data if LVER, the version of the
C     last NIMS I-kernel data extracted from the pool (or 0.0 if this
C     is the first time through), does not equal CVER, the version
C     of the NIMS I-kernel currently in the kernel pool.
C
      CALL RTPOOL ( NIMDAT(18), N, CVER, FOUND(18) )
 
      IF ( .NOT. FOUND(18) ) THEN
         CALL SETMSG ( 'NIMS I-kernel version not found in ' //
     .                 'the kernel pool'                           )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                  )
         CALL CHKOUT ( 'NIMMP_G'                                   )
         RETURN
      END IF
 
      IF ( INT(CVER) .NE. OKVER ) THEN
         CALL SETMSG ( 'NIMS I-kernel version # not compatible'    )
         CALL ERRDP  ( '#', CVER                                   )
         CALL SIGERR ( 'SPICE(IKERNELNOTCOMPAT)'                   )
         CALL CHKOUT ( 'NIMMP_G'                                   )
         RETURN
      END IF
 
      IF ( CVER .NE. LVER ) THEN
 
C
C        Read position tables
C
         DO 50001
     .      I = 1, 4
            CALL RTPOOL ( NIMDAT(I*2-1), N, PS(1,I),      FOUND(I*2-1) )
            CALL RTPOOL ( NIMDAT(I*2),   N, PS(PSNROW+1,I), FOUND(I*2) )
50001    CONTINUE
 
C
C        Read time table
C
         CALL RTPOOL ( NIMDAT(9), N, TYMSAM, FOUND(9) )
 
C
C        Read mode tables
C
         DO 50002
     .      I = 1, 6
            CALL RTPOOL ( NIMDAT(I+9), N, MD(1,I), FOUND(I+9) )
50002    CONTINUE
 
C
C        Read boresight offset from the scan platform L vector
C
         CALL RTPOOL    ( NIMDAT(16), N, BXCONE, FOUND(16) )
         CALL RTPOOL    ( NIMDAT(17), N, BCONE,  FOUND(17) )
 
C
C        Stop here if any data was not found.
C
         QUIT   = .FALSE.
         OUTERR = ' '
 
         DO 50003
     .      I = 1, NDAT - 1
 
            IF ( .NOT. FOUND(I) ) THEN
               QUIT = .TRUE.
               CALL SUFFIX ( NIMDAT(I), 2, OUTERR )
            END IF
 
50003    CONTINUE
 
         IF ( QUIT ) THEN
            CALL SETMSG ( 'Did not find # item(s) in pool'   )
            CALL ERRCH  ( '#', OUTERR                        )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'         )
            CALL CHKOUT ( 'NIMMP_G'                          )
            RETURN
         END IF
 
C        Update LVER.
C
         LVER = CVER
 
      ENDIF
 
C
C     Step 2.
C
C     Find MDCOL, the index of MODE in the mode names array,
C     MDNAM.  If MODE is not a valid mode name signal an error.
C     (ISRCHC will return zero if MODE is not in MDNAME.)
C
      CALL UCASE     ( MODE,   MODEBG         )
      MDCOL = ISRCHC ( MODEBG, MDNCOL, MDNAME )
 
      IF ( MDCOL .EQ. 0 ) THEN
         CALL SETMSG ( 'Mode # is not a valid NIMS RIM mode'       )
         CALL ERRCH  ( '#', MODE                                   )
         CALL SIGERR ( 'SPICE(INVALIDMODE)'                        )
         CALL CHKOUT ( 'NIMMP_G'                                   )
         RETURN
      ENDIF
 
C
C     Step 3.
C
C     Determine which 1/2 minor frame we are in.  This number will tell
C     us which element to look up in a mode RIM table.  The mode RIM
C     tables are stored in the 2-D array MD - the columns being the
C     mode RIM tables and the elements in a mode RIM table being the
C     rows.  Thus the number we will get is the row of MD, MDROW.
C     A minor frame is MF/2 seconds, so
C
      MDROW = INT(DT/MF * 2 + 1 )
 
C
C     Step 4.
C
C     Find MD(MDROW,MDCOL).  This value points to which mirror position
C     table is being used at time DT.  The mirror position tables are
C     stored in the 2-D array PS - the columns being the tables and
C     the elements in the mirror position tables being the rows.
C     MD(MDROW,MDCOL) is thus PSCOL, the column of PS.
C
      PSCOL = INT ( MD(MDROW, MDCOL) )
 
C
C     Step 5.
C
C     Find the time since the start of the current minor frarme, DTMF.
C     A minor frame is MF seconds so,
C
      DTMF  = DT - INT( DT/MF ) * MF
 
C
C     Step 6.
C
C     The time sampling table contains 40 values between 0 and 1.0.
C     These numbers represent fractions of a minor frame (2/3 seconds).
C     We need to know what fraction (what index) we are in the minor
C     frame.  This index points to which row in PS we need to look up,
C     and is named PSROW.  If the PSROW is greater than PSNROW, then
C     we are in the later half of a minor frame (last 1/3 second) and
C     we need to subtract PSNROW from PSROW.
C
      PSROW = LSTLED ( DTMF/MF, NTSAM, TYMSAM )
 
      PSROW = PSROW - (PSROW-1)/PSNROW * PSNROW
 
C
C     Step 7.
C
C     Look up PS(PSROW,PSCOL).  This is the cross cone offset from the
C     NIMS boresight.  PSNROW is the number of possible mirror positions
C     in a 1/2 minor frame, thus PS(PSROW+PSNROW,PSCOL) is the cone
C     offset since the cone offsets are stored after the cross cone
C     offsets in the rows of PS.  Convert everything to radians while
C     we're at it.
C
      XCONE = PS ( PSROW,        PSCOL ) / 1000
      CONE  = PS ( PSROW+PSNROW, PSCOL ) / 1000
 
      BXCONE = BXCONE / 1000
      BCONE  = BCONE  / 1000
 
C
C     Step 8.
C
C     Transform the pointing vector so that it is expressed relative
C     to the scan platform frame (M,N,L).  The boresight frame is
C     displaced from the scan platform frame by a cone and cross cone
C     offset, BCONE and BXCONE.  The pointing vector is displaced from
C     the boresight frame by CONE and XCONE.
C
C     (CONE is a +2 rotation.  XCONE is a -1 rotation.)
C
      CALL ROTATE (          XCONE,  1, MATRIX )
      CALL ROTMAT ( MATRIX, -CONE,   2, MATRIX )
      CALL ROTMAT ( MATRIX,  BXCONE, 1, MATRIX )
      CALL ROTMAT ( MATRIX, -BCONE,  2, MATRIX )
 
C
C     When the rotation matrix, MATRIX, is multiplied by the pointing
C     vector expressed in the pointing vector frame (0,0,1) it will
C     produce the pointing vector expressed in the scan platform frame.
C
      CALL VPACK ( 0D0, 0D0, 1D0,   VPOINT )
      CALL MXV   ( MATRIX, VPOINT,  VPOINT )
 
      CALL CHKOUT ( 'NIMMP_G' )
      RETURN
      END
