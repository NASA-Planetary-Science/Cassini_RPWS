C$Procedure      DDSPNT_G ( DDS pointing )
 
      SUBROUTINE DDSPNT_G ( VPOINT, MAXANG )
 
C$ Abstract
C
C     Return the boresight vector and the maximum acceptance angle
C     of the Dust Detector Subsystem (DDS) instrument.
C
C$ Required_Reading
C
C     GLL_IK
C
C$ Keywords
C
C     AXES
C     UTILITY
C     VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION      VPOINT ( 3, 3 )
      DOUBLE PRECISION      MAXANG
 
      INTEGER               VER
      PARAMETER           ( VER = 1 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      VPOINT    O    Pointing vector.
C      MAXANG    O    Maximum acceptance angle.
C      VER       P    Version of I-kernel needed.
C
C$ Detailed_Input
C
C      None.
C
C$ Detailed_Output
C
C      VPOINT         is a unit pointing vector defining the boresight
C                     vector of the DDS instrument.
C
C      MAXANG         is the maximum acceptance angle in degress off
C                     axis of the DDS instrument.
C
C$ Parameters
C
C      VER            is the version of the DDS I-kernel that must be
C                     loaded into the kernel pool.  That is, the
C                     I-kenrel must be in the form VER.X.
C
C$ Exceptions
C
C     1)  If the DDS instrument data is not found in the kernel pool the
C         error SPICE(KERNELVARNOTFOUND) is signalled.
C
C     2)  If the DDS I-kernel currently in the kernel pool is not
C         compatible with this routine the error
C         SPICE(IKERNELNOTCOMPAT) is signalled.
C
C$ Files
C
C     A DDS I-kernel with the correct version number must be loaded in
C     the kernel pool prior to calling this routine.
C
C$ Particulars
C
C     This routine looks up the boresight vector and the maximum
C     acceptance angle of the DDS instrument.  The DDS I-kernel
C     must be compatible with this routine, that is, the version
C     number must be in the form VER.X.
C
C$ Examples
C
C     In following code fragment, the DDS boresight vector is
C     converted to an inertial vector expressed relative to the
C     'J2000' inertial reference frame.
C
C     C
C     C     Load the DDS I-kernel, the Galileo SCLK file, and the
C     C     leapseconds kernel file into kernel pool.  These files
C     C     are assumed to be located in the directory SPICE$KER.
C     C
C           CALL LDPOOL ( 'SPICE$KER:GLL29001.TI'  )
C           CALL LDPOOL ( 'SPICE$KER:GLL00001.TSC' )
C           CALL LDPOOL ( 'SPICE$KER:GLL00002.TLS' )
C     C
C     C     Input time and name of C-kernel file containing pointing.
C     C
C           WRITE (*,*) 'Input epoch in UTC format'
C           READ  (*,FMT='(A)') UTC
C           CALL UTC2ET ( UTC, ET)
C           CALL SCE2T  ( -77, ET, SCLKDP )
C
C           WRITE (*,*) 'Input name of C-kernel file containing ' //
C          .            'containing pointing at this epoch'
C           READ  (*,FMT='(A)') CKERNEL
C           CALL CKLPF ( CKERNEL, HANDLE )
C
C           WRITE (*,*) 'Input C-kernel tolerance in SCLK format '
C           READ  (*,FMT='(A)') TOLCH
C           CALL SCTIKS ( -77, TOLCH, TOL )
C
C     C
C     C     Get the DDS pointing vector, VPOINT
C     C
C           CALL DDSPNT_G ( VPOINT, MAXANG )
C
C     C
C     C     Find the C-matrix.  The C-matrix transforms from inertial
C     C     to instrument fixed.
C     C
C           CALL CKGP ( 77002, SCLKDP, TOL, 'J2000', CMAT, CLKOUT,
C          .            FOUND )
C
C     C
C     C     Express VPOINT in the J2000 inertial reference frame.
C     C
C           CALL MTXV ( CMAT, VPOINT, VPOINT )
C
C
C$ Restrictions
C
C     The DDS I-kernel version must be in the form VER.X.
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
C-    GLLSPICE Version 1.0.0, 11-NOV-1990 (MJS)
C
C-&
 
C$ Index_Entries
C
C     dds pointing
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32 )
 
C
C     Local variables
C
      CHARACTER*(MAXLEN)    DDSDAT
 
      DOUBLE PRECISION      IKVER
 
      INTEGER               N
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DDSPNT_G' )
      END IF
 
C
C     Check version number of I-kernel.
C
      CALL RTPOOL ( 'INS-77029_VERSION', N, IKVER, FOUND )
 
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'DDS I-kernel version not found'            )
         CALL SIGERR ( 'SPICE(KERVARNOTFOUND)'                     )
         CALL CHKOUT ( 'DDSPNT_G'                                  )
         RETURN
      END IF
 
      IF ( INT(IKVER) .NE. VER ) THEN
         CALL SETMSG ( 'DDS I-kernel version # not compatible'     )
         CALL ERRDP  ( '#', IKVER                                  )
         CALL SIGERR ( 'SPICE(IKERNELNOTCOMPAT)'                   )
         CALL CHKOUT ( 'DDSPNT_G'                                  )
         RETURN
      END IF
 
C
C     Read in information.
C
 
      DDSDAT = 'INS-77029_BORESIGHT'
      CALL RTPOOL ( DDSDAT, N, VPOINT, FOUND )
 
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( '# NOT FOUND IN KERNEL POOL'                )
         CALL ERRCH  ( '#', DDSDAT                                 )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                  )
         CALL CHKOUT ( 'DDSPNT_G'                                  )
         RETURN
      END IF
 
 
      DDSDAT = 'INS-77029_MAX_ACCEP_ANG'
      CALL RTPOOL ( DDSDAT, N, MAXANG, FOUND )
 
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( '# NOT FOUND IN KERNEL POOL'                )
         CALL ERRCH  ( '#', DDSDAT                                 )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                  )
         CALL CHKOUT ( 'DDSPNT_G'                                  )
         RETURN
      END IF
 
      CALL CHKOUT ( 'DDSPNT_G' )
      RETURN
      END
