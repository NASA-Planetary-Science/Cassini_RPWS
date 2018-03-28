C$Procedure      EPDPNT_G ( EPD pointing )
 
      SUBROUTINE EPDPNT_G ( DETECT, SECTOR, VPOINT, ANG )
 
C$ Abstract
C
C     Find the acceptance angle and pointing vector of a detector on
C     the Energetic Particle Detector (EPD) instrument.
C
C$ Required_Reading
C
C     GLL_IK
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         DETECT
      INTEGER               SECTOR
      DOUBLE PRECISION      VPOINT   ( 3 )
      DOUBLE PRECISION      ANG
 
      INTEGER               VERSN
      PARAMETER           ( VERSN = 1 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      DETECT    I    A detector on the EPD instrument.
C      SECTOR    I    A sector.
C      VPOINT    O    Pointing vector.
C      ANG       O    Acceptance angle of the detector.
C      VERSN     P    Compatible version of EPD I-kernel.
C
C$ Detailed_Input
C
C      DETECT    is the name of a detector on the EPD instrument.
C                DETECT can be any detector on the CMS or the LEMMS
C                entered in upper or lower case.
C
C      SECTOR    is one of eight (numbered 0 to 7) positions of the
C                platform the EPD is mounted on.
C
C$ Detailed_Output
C
C      VOUT      is the pointing vector of the detector.
C                The vector will be expressed relative to the
C                spacecraft rotor Xr,Yr,Zr axes.
C
C      ANG       is the acceptance angle of the detector.
C
C$ Parameters
C
C     VERSN      is the version of the EPD I-kernel that must be loaded
C                in the kernel pool.  That is, the I-kernel must be in
C                the form VERSN.X.
C
C$ Exceptions
C
C     1) If an invalid detector is given, the error
C        SPICE(INVALIDDETECTOR) is signalled.
C
C     2) If an invalid sector is given, the error SPICE(INVALIDSECTOR)
C        is signalled.
C
C     3) If any information is not found in the kernel pool, the error
C        SPICE(KERNELVARNOTFOUND) is signalled.
C
C     4) If the EPD I-kernel is not compatible with this routine, the
C        error SPICE(IKERNELNOTCOMPAT) is signalled.
C
C$ Files
C
C     An EPD I-kernel must be loaded into the kernel pool by a call
C     to LDPOOL prior to calling this routine.   The version of the
C     I-kernel must be in the form VERSN.X.
C
C$ Particulars
C
C     The energetic particle detector consists of a solid state detector
C     telescope (LEMMS) and a foil type detector telescope (CMS).  Both
C     telescopes are mounted on a rotating platform which, in turn, is
C     mounted on an electronics package.  The platform rotates
C     in cone in eight discrete steps (sectors).  Thus, to find the
C     pointing direction of a detector the name of the detector is
C     needed as well as a sector.  Each detector has an acceptance
C     angle associated with it and this is also given as output to
C     this routine.
C     In order for this routine to look up this data in the kernel pool,
C     a compatible EPD I-kernel must be loaded in the kernel pool via
C     a call to LDPOOL.
C
C$ Examples
C
C     The following code fragment transforms a detector position
C     vector on the EPD instrument into an inertial vector expressed
C     in the 'J2000' inertial reference frame.
C
C     C
C     C     Load the EPD I-kernel, leapseconds kernel, and SCLK coeff.
C     C     kernel file into the kernel pool.  Assume these kernel
C     C     files are located in directory SPICE$KER.  Also load a C-
C     C     kernel file.  Assume the C-kernel file is named
C     C     'C_KERNEL.CK'.
C     C
C           CALL LDPOOL ( 'SPICE$KER:GLL25001.TI'  )
C           CALL LDPOOL ( 'SPICE$KER:GLL00002.TLS' )
C           CALL LDPOOL ( 'SPICE$KER:GLL00001.TSC' )
C
C           CALL CKLPF  ( 'SPICE$KER:C_KERNEL.CK', HANDLE )
C
C     C
C     C     Initialize values.
C     C
C           DETECT  =  'Ja'
C           SECTOR  =   3
C           EPOCH   =  '1 JAN 1991'
C           REF     =  'J2000'
C           INST    =  '-77002'
C
C     C
C     C     Find the boresight axis of TELSCP.
C     C
C           CALL EPDPNT_G ( DETECT, SECTOR, VPOINT, ANG )
C
C     C
C     C     Convert epoch time from UTC format to ticks.
C     C
C           CALL UTC2ET ( EPOCH, ET         )
C           CALL SCE2T  ( SC,    ET, SCLKDP )
C
C     C
C     C     Find C-matrix.  The C-matrix, CMAT, will transform from
C     C     inertial to instrument fixed coordinates.  Transpose of
C     C     CMAT will do the opposite.  Assume SCLK times in C-kernel
C     C     are are spaced at 2*TOL inervals.
C     C
C           CALL CKGP ( INST, SCLKDP, TOL, REF, CMAT, CLKOUT, FOUND )
C
C           CALL MTXV ( CMAT, VPOINT, VPOINT )
C
C     C
C     C     Write results.
C     C
C           WRITE (*,*) 'Pointing vector expressed in J2000 ' //
C          .            'inertial reference frame'
C           WRITE (*,*) 'X = ', VPOINT ( 1 )
C           WRITE (*,*) 'Y = ', VPOINT ( 2 )
C           WRITE (*,*) 'Z = ', VPOINT ( 3 )
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
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    GLLSPICE 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE 1.0.0, 23-OCT-1990 (MJS)
C
C-&
 
C$ Index_Entries
C
C     epd pointing
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               BSRCHC
 
C
C     Local parameters
C
C     NAMLEN       is the maximum lenght of a detector name.
C
C     NAT          is the number of alignment tables.
C
C     NSECT        is the number of sectors minus one.
C
C     NDAT         is the number of variable names read from the
C                  kernel pool.
C
C     NDETEC       is the number of detectors.
C
C     MAXLEN       is the maximum length of the variable names.
C
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 2              )
 
      INTEGER               NAT
      PARAMETER           ( NAT    = 2              )
 
      INTEGER               NSECT
      PARAMETER           ( NSECT  = 7              )
 
      INTEGER               NDAT
      PARAMETER           ( NDAT   = 5              )
 
      INTEGER               NDETEC
      PARAMETER           ( NDETEC = 13             )
 
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32             )
 
      INTEGER               MAXMSG
      PARAMETER           ( MAXMSG = NAMLEN*NDETEC )
 
C
C     Local variables
C
C
      DOUBLE PRECISION      DTPNTG ( 3, 0:NSECT, NAT )
 
      DOUBLE PRECISION      DTTAB  ( NDETEC )
      INTEGER               TAB
 
      DOUBLE PRECISION      DTANG     ( NDETEC )
      INTEGER               NAM
      CHARACTER*(NAMLEN)    DTNAM     ( NDETEC )
      CHARACTER*(NAMLEN)    DET
 
      CHARACTER*(MAXLEN)    EPDDAT ( NDAT )
      CHARACTER*(MAXMSG)    OUTERR
 
      DOUBLE PRECISION      CVERSN
      DOUBLE PRECISION      LVERSN
 
      INTEGER               I
      INTEGER               N
      LOGICAL               FOUND ( NDAT )
      LOGICAL               QUIT
 
C
C     Saved variables
C
      SAVE   EPDDAT
      SAVE   DTNAM
      SAVE   LVERSN
      SAVE   DTPNTG
      SAVE   DTANG
 
C
C     Initial values
C
      DATA LVERSN          /  0.0D0                            /
 
      DATA EPDDAT          /  'INS-77025_DETECTOR_ALIGNMENT',
     .                        'INS-77025_ALIGN_TABLE1',
     .                        'INS-77025_ALIGN_TABLE2',
     .                        'INS-77025_ACCEPTANCE_ANGLE',
     .                        'INS-77025_VERSION'               /
 
      DATA DTNAM           /  'A',
     .                        'B',
     .                        'C',
     .                        'D',
     .                        'E1',
     .                        'E2',
     .                        'F1',
     .                        'F2',
     .                        'JA',
     .                        'JB',
     .                        'KA',
     .                        'KB',
     .                        'KT'                             /
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EPDPNT_G' )
      END IF
 
C
C     Design Notes
C     ------------
C
C     The acceptance angle of the detector and the pointing of the
C     sector on that detector are found by following these steps.
C
C
C     Step 1.  Extract the EPD instrument information from the
C              kernel pool if this is the first time this routine
C              is called or if a different version EPD I-kernel has
C              been loaded since the previous call to this routine.
C
C     In steps 2 thru 5 refer to this figure.
C
C
C  SECTOR --------------------------------------------.
C                                                     |
C                                                     V      DTPNTG
C            |  |DTNAM          |  |DTTAB          __ __ __ __       __
C            |--|               |--|         .--> |  |X |  |  |     |X |
C            |  | --NAM-----+-->|  |         |    |__|__|__|__|     |__|
C            |--|           |   |--|         |    |  |Y |  |  | ==> |Y |
C  DETECT -->|  |   Index   |   |  | --TAB --'  __ __ __ __  _|     |__|
C            |--|   of      |   |--|           |  |  |  |  |  |     |Z |
C            |  |   DETECT  |   |  |   DTTAB   |__|__|__|__| _|     |__|
C            |--|   in      |   |--|   (NAM)   |  |  |  |  |
C            |  |   DTNAM   |   |  |           |__|__|__|__|   DTPNTG(1,
C                           |                  |  |  |  |  |     SECTOR,
C                           |                  |__|__|__|__|     TAB)
C                           |   |  |DTANG
C                           |   |--|
C                           `-->|  |
C                               |--|
C                               |  | ==> ANG
C                               |--|
C                               |  |     DTANG(NAM)
C                               |--|
C                               |  |
C
C     Step 2.  Find the index of DETECT in the array DTNAM.  (DTNAM is
C              a one dimensional array containing the names of the
C              detectors listed in alphabetical order.)  NAM is
C              the index.
C
C     Step 3.  Look up DTTAB(NAM).  This is the value of TAB.  (DTTAB is
C              a one dimensional array containing the alignment table
C              that should be used for the DTNAM(NAM) detector.)
C
C     Step 4.  Look up DTPNTG(1,SECTOR,TAB).  This is the value of X.
C              Similarly, DTPNTG(2,SECTOR,TAB) and DTPNTG(3,SECTOR,TAB)
C              are the values of Y and Z.
C
C     Step 5.  Look up DTANG(NAM).  This is the value of ANG.  (DTNAM
C              is a one dimensional array containing the names of the
C              detectors listed in alphabetical order.)
C
C
 
C
C     Check if SECTOR is valid.  Its value should be between zero and
C     NSECT.
C
      IF ( SECTOR .LT. 0 .OR. SECTOR .GT. NSECT ) THEN
 
         CALL SETMSG ( 'Sector # is not a valid sector on detector # ')
         CALL ERRINT ( '#', SECTOR                                    )
         CALL ERRCH  ( '#', DETECT                                    )
         CALL SIGERR ( 'SPICE(INVALIDSECTOR)'                         )
         CALL CHKOUT ( 'EPDPNT_G'                                     )
         RETURN
 
      END IF
 
C
C     Extract data from kernel pool if version in pool is not equal to
C     the version last read.  Signal error if I-kernel is not in the
C     form VERSN.X.
C
      CALL RTPOOL ( EPDDAT(5), N, CVERSN, FOUND(5) )
 
      IF ( .NOT. FOUND(5) ) THEN
         CALL SETMSG ( 'EPD I-kernel version not found in kernel pool')
         CALL SIGERR ( 'SPICE(NOIKERNELVERSION)'                      )
         CALL CHKOUT ( 'EPDPNT_G'                                     )
         RETURN
      END IF
 
      IF ( INT(CVERSN) .NE. VERSN ) THEN
         CALL SETMSG ( 'EPD I-kernel version # is not compatible'  )
         CALL ERRDP  ( '#', CVERSN                                 )
         CALL SIGERR ( 'SPICE(IKERNELNOTCOMPAT)'                   )
         CALL CHKOUT ( 'EPDPNT_G'                                  )
         RETURN
      END IF
 
      IF ( CVERSN .NE. LVERSN ) THEN
 
C
C        First fill the detector table (DTTAB) array.  The values in
C        given the index of the detector in DTNAM.
C
         CALL RTPOOL ( EPDDAT(1), N, DTTAB, FOUND(1) )
 
C
C        Now we can start filling up DTPNTG, the pointing tables.
C        Start with table 1.
C
         CALL RTPOOL ( EPDDAT(2), N, DTPNTG(1,0,1), FOUND(2) )
 
C
C        And finish with table 2.
C
         CALL RTPOOL ( EPDDAT(3), N, DTPNTG(1,0,2), FOUND(3) )
 
C
C        The acceptace angles are stored in the array DTANG.
C
         CALL RTPOOL ( EPDDAT(4), N, DTANG, FOUND(4) )
 
C
C        Stop here if any data was not found.
C
         QUIT   = .FALSE.
         OUTERR = ' '
 
         DO 50001
     .      I = 1, NDAT - 1
 
            IF ( .NOT. FOUND(I) ) THEN
               QUIT = .TRUE.
               CALL SUFFIX ( EPDDAT(I), 2, OUTERR )
            END IF
 
50001    CONTINUE
 
         IF ( QUIT ) THEN
            CALL SETMSG ( 'Did not find # item(s) in pool'   )
            CALL ERRCH  ( '#', OUTERR                        )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'         )
            CALL CHKOUT ( 'EPDPNT_G'                         )
            RETURN
         END IF
 
C        Update VERSN.
C
         LVERSN = CVERSN
 
      END IF
 
C
C     Find NAM, the index of the detector in DTNAM.
C     Signal an error if DETECT is not found in the DTNAM.  BSRCHC will
C     return zero if this is the case.
C
      CALL UCASE   ( DETECT, DET           )
      NAM = BSRCHC ( DET,    NDETEC, DTNAM )
 
      IF ( NAM .EQ. 0 ) THEN
         CALL SETMSG ( 'Detector # is not a valid detector'  )
         CALL ERRCH  ( '#', DETECT                           )
         CALL SIGERR ( 'SPICE(INVALIDDETECTOR)'              )
         CALL CHKOUT ( 'EPDPNT_G'                            )
         RETURN
      END IF
 
C
C     TAB, the pointing table that should be used for the detector is
C     DTTAB(NAM).
C
      TAB = DTTAB(NAM)
 
C
C     Thats all we need to know to look up the pointing vector for
C     the detector.
C
      CALL MOVED ( DTPNTG(1,SECTOR,TAB), 3, VPOINT )
 
C
C     ANG is just the NAM'th element in DTANG.
C
      ANG = DTANG(NAM)
 
      CALL CHKOUT ( 'EPDPNT_G' )
      RETURN
      END
