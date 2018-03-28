C$Procedure      HICPNT_G ( HIC Pointing )
 
      SUBROUTINE HICPNT_G ( TELSCP, VPNTG, MAA, NMAA )
 
C$ Abstract
C
C    Return the view axis and the maximum acceptance angle(s) of
C    a given telescope on the Heavy Ion Counter (HIC) instrument.
C
C$ Required_Reading
C
C    GLL_IK
C
C$ Keywords
C
C     AXES
C     GEOMETRY
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         TELSCP
      DOUBLE PRECISION      VPNTG   ( * )
      DOUBLE PRECISION      MAA     ( * )
      INTEGER               NMAA
 
      INTEGER               VER
      PARAMETER           ( VER = 1 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      TELSCP    I    A name of a telescope on the HIC.
C      VPNTG     O    Pointing vector.
C      MAA       O    Maximum acceptance angle(s).
C      NMAA      O    The number of acceptance angles returned.
C      VER       P    Version of I-kernel needed.
C
C$ Detailed_Input
C
C     TELSCP      is the name of a Low Energy Telescope (LET) on the
C                 Heavy Ion Counter (HIC) instrument.  There are two
C                 LET telescopes on the HIC:  'LET B' and 'LET E'.
C                 The names may be entered in upper or lower case.
C
C$ Detailed_Output
C
C     VBORST      is the borsight vector of the telescope.  The vector
C                 is expressed relative to the spacecraft Xr, Yr, and
C                 Zr axes.  The vector is a unit vector.
C
C     MAA         is the maximum acceptance angle of TELSCP.  The
C                 LET B telescope has one maximum acceptance angle
C                 which is defined by the geometry of the LB2 and LB3
C                 detectors.  The LET E telescope has two maximum
C                 acceptance angles.  The first is defined by the
C                 geometry of the LE1 and LE2 detectors, the second is
C                 defined by the geometry of the LE2 and LE5 detectors.
C                 All angles are given in degrees off axis.
C
C     NMAA        is the number of acceptance angles returned.  The
C                 LET B telescope has one acceptance angle.  The
C                 LET E telescope has two acceptance angles.
C
C$ Parameters
C
C      VER        is the version of the HIC I-kernel that must be
C                 loaded into the kernel pool.  That is, the
C                 I-kenrel must be in the form VER.X.
C
C$ Exceptions
C
C     1)          If TELSCP is not a valid name of a telescope, the
C                 error SPICE(NOTASCOPE) is signalled.
C
C     2)          If the HIC pointing information is not found in the
C                 kernel pool, the error SPICE(KERNELVARNOTFOUND) is
C                 signalled.
C
C     3)          If the HIC I-kernel currently in the kernel pool is
C                 not compatible with this routine the error
C                 SPICE(IKERNELNOTCOMPAT) is signalled.
C
C$ Files
C
C     A HIC I-kernel with the correct version number must be loaded in
C     the kernel pool prior to calling this routine.
C
C$ Particulars
C
C     This routine returns the telescope boresight vector and its
C     maximum acceptance angle.  There may be more than one acceptance
C     angle for a given telescope.  If this is the case, the angles are
C     returned in the array MAA in the same order as they are stored in
C     the I-kernel.  The HIC I-kernel must be loaded in the kernel pool
C     prior to calling this routine and must be compatible with this
C     routine, that is, the version number must be in the form VER.X.
C
C$ Examples
C
C     The following code fragment transforms a telescope boresight
C     axis on the HIC into an inertial vector expressed in the
C     'J2000' inertial reference frame.
C
C     C
C     C     Load the HIC I-kernel, leapseconds kernel, and SCLK coeff.
C     C     kernel file into the kernel pool.  Assume these kernel
C     C     files are located in directory SPICE$KER.  Also load a C-
C     C     kernel file.  Assume the C-kernel file is named
C     C     'C_KERNEL.CK'.
C     C
C           CALL LDPOOL ( 'SPICE$KER:GLL28001.TI'  )
C           CALL LDPOOL ( 'SPICE$KER:GLL00002.TLS' )
C           CALL LDPOOL ( 'SPICE$KER:GLL00001.TSC' )
C
C           CALL CKLPF  ( 'SPICE$KER:C_KERNEL.CK', HANDLE )
C
C     C
C     C     Initialize values.
C     C
C           TELSCP  =  'let b'
C           EPOCH   =  '1 JAN 1991'
C           REF     =  'J2000'
C           INST    =  '-77002'
C
C     C
C     C     Find the boresight axis of TELSCP.
C     C
C           CALL HICPNT_G ( TELSCP, VPNTG, MAA, NMAA )
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
C           CALL MTXV ( CMAT, VPNTG, VPNTG )
C
C     C
C     C     Write results.
C     C
C           WRITE (*,*) 'Pointing vector expressed in J2000 ' //
C          .            'inertial reference frame'
C           WRITE (*,*) 'X = ', VPNTG ( 1 )
C           WRITE (*,*) 'Y = ', VPNTG ( 2 )
C           WRITE (*,*) 'Z = ', VPNTG ( 3 )
C
C$ Restrictions
C
C     The HIC I-kernel version must be in the form VER.X.
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
C-    GLLSPICE 1.0.0, 29-OCT-1990 (MJS)
C
C-&
 
C$ Index_Entries
C
C     hic pointing
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               ISRCHC
 
C
C     Local parameters
C
C
C     NTEL      is the number of telescopes.
C
C     MAXLEN    is the maximum length of the variable names that
C               can be stored in the kernel pool.
C
C     NVAR      is the number of variables read from the kernel
C               pool.
C
C     NTMAA     is the maximum number of acceptance angles that can
C               be associated with a telescope.
C
C     TELNML    is the maximum length of a telescope name.
C
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32 )
 
      INTEGER               NTEL
      PARAMETER           ( NTEL   = 2  )
 
      INTEGER               TELNML
      PARAMETER           ( TELNML = 5  )
 
C
C     Local variables
C
      CHARACTER*(MAXLEN)    TELBR  ( NTEL   )
      CHARACTER*(MAXLEN)    TELMAA ( NTEL   )
      CHARACTER*(MAXLEN)    VERSN
      CHARACTER*(TELNML)    SCOPE
      CHARACTER*(TELNML)    TELNM  ( NTEL   )
      DOUBLE PRECISION      IKVER
      INTEGER               N
      INTEGER               ISCOPE
      LOGICAL               VFOUND
      LOGICAL               FOUND  ( 2      )
 
 
C
C     Saved variables
C
      SAVE      TELBR
      SAVE      TELMAA
      SAVE      TELNM
 
C
C     Initial values
C
      DATA  TELBR        / 'INS-77028_LET_B_BORESIGHT',
     .                     'INS-77028_LET_E_BORESIGHT'  /
 
 
      DATA  TELMAA       / 'INS-77028_LET_B_ACCEP_ANG',
     .                     'INS-77028_LET_E_ACCEP_ANG'  /
 
      DATA  TELNM        / 'LETB',
     .                     'LETE'                       /
 
      DATA  VERSN        / 'INS-77028_VERSION'          /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'HICPNT_G' )
      END IF
 
C
C     Lets first check if user gave a valid telescope.
C
      CALL UCASE  ( TELSCP, SCOPE        )
      CALL CMPRSS ( ' ', 0, SCOPE, SCOPE )
 
      ISCOPE = ISRCHC ( SCOPE, NTEL, TELNM )
 
      IF ( ISCOPE .EQ. 0 ) THEN
 
         CALL SETMSG ( '# is not a valid telescope on the HIC'     )
         CALL ERRCH  ( '#', TELSCP                                 )
         CALL SIGERR ( 'SPICE(NOTASCOPE)'                          )
         CALL CHKOUT ( 'HICPNT_G'                                  )
         RETURN
 
      END IF
 
C
C     Check version number of I-kernel.
C
      CALL RTPOOL ( VERSN, N, IKVER, VFOUND )
 
      IF ( .NOT. VFOUND ) THEN
         CALL SETMSG ( 'HIC I-kernel version not found'            )
         CALL SIGERR ( 'SPICE(KERVARNOTFOUND)'                     )
         CALL CHKOUT ( 'HICPNT_G'                                  )
         RETURN
      END IF
 
      IF ( INT(IKVER) .NE. VER ) THEN
         CALL SETMSG ( 'HIC I-kernel version # not compatible'     )
         CALL ERRDP  ( '#', IKVER                                  )
         CALL SIGERR ( 'SPICE(IKERNELNOTCOMPAT)'                   )
         CALL CHKOUT ( 'HICPNT_G'                                  )
         RETURN
      END IF
 
C
C     Extract HIC data from pool.  Signal an error if any information
C     does not seem to be there.
C
      CALL RTPOOL ( TELBR(ISCOPE),  N,    VPNTG, FOUND(1) )
      CALL RTPOOL ( TELMAA(ISCOPE), NMAA, MAA,   FOUND(2) )
 
      IF ( .NOT. ( FOUND(1) .AND. FOUND(2) ) ) THEN
         CALL SETMSG ( 'HIC instrument data not found in ' //
     .                 'kernel pool'                               )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                  )
         CALL CHKOUT ( 'HICPNT_G'                                  )
         RETURN
      END IF
 
C
C     Thats all there is to this.
C
      CALL CHKOUT ( 'HICPNT_G' )
      RETURN
      END
