C$Procedure      CKSR_G ( CK sequential reader )
 
      SUBROUTINE CKSR_G ( CKFILE,
     .                    HANDLE,
     .                    SCOUNT,
     .                    PCOUNT,
     .                    INST,
     .                    NEEDAV,
     .                    SCLKDP,
     .                    NPREC,
     .                    NSEG,
     .                    SEGBEG,
     .                    SEGEND,
     .                    SEGDSC,
     .                    SEGID,
     .                    PREC    )
 
C$ Abstract
C
C     Read a C-kernel containing only discrete segments, in
C     sequential fashion.
C
C$ Required_Reading
C
C     CK
C     DAF
C     NAIF_IDS
C     SCLK
C     TIME
C
C$ Keywords
C
C     CK
C     FILES
C     POINTING
C     SEARCH
C
C$ Declarations
 
      INTEGER               MAXFIL
      PARAMETER           ( MAXFIL =  20 )
 
      CHARACTER*(*)         CKFILE
      INTEGER               HANDLE
      INTEGER               SCOUNT
      INTEGER               PCOUNT
      INTEGER               INST
      LOGICAL               NEEDAV
      DOUBLE PRECISION      SCLKDP
      INTEGER               NPREC
      INTEGER               NSEG
      INTEGER               SEGBEG
      INTEGER               SEGEND
      DOUBLE PRECISION      SEGDSC ( * )
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      PREC   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     CKFILE     I   CKSLD_G
C     HANDLE    I-O  (All)
C     SCOUNT     O   CKSFIN_G
C     PCOUNT     O   CKSFIN_G
C     INST       I   CKSLPT_G
C     NEEDAV     I   CKSLPT_G
C     SCLKDP     I   CKSLPT_G
C     NPREC     I-O  CKSLPI_G, CKSLPT_G
C     NSEG      I-O  CKSSIN_G, CKSLPI_G, CKSLPT_G
C     SEGBEG     O   CKSSIN_G
C     SEGEND     O   CKSSIN_G
C     SEGDSC     O   CKSSIN_G
C     SEGID      O   CKSSIN_G
C     PREC       O   CKSLPI_G, CKSLPT_G
C     MAXFIL     P   CKSR_G
C
C$ Detailed_Input
C
C     CKFILE         Name of C-kernel to load.  CKFILE must contain
C                    only segments having a discrete data type.
C                    Currently, the only discrete type is type 1.
C
C     HANDLE         Handle of C-kernel.  On input, HANDLE must be a
C                    file handle returned by the routine CKSLD_G.
C
C     INST           NAIF integer code of an instrument or spacecraft
C                    structure.
C
C     NEEDAV         Logical flag indicating whether only segments that
C                    contain angular velocity data should be considered
C                    when attempting to satisfy a request for pointing.
C
C     SCLKDP         Encoded SCLK time.
C
C     NPREC          On input, NPREC is the index of a pointing instance
C                    to be found.
C
C     NSEG           On input, NSEG is the index of a segment for which
C                    information is to be found.
C
C$ Detailed_Output
C
C     SCOUNT         Segment count---the total number of segments in a
C                    specified C-kernel.
C
C     PCOUNT         Pointing instance count---the total number of
C                    pointing instances in a specified C-kernel.
C
C     SEGBEG,
C     SEGEND         Indices of first and last pointing instances in a
C                    specified segment.
C
C     SEGDSC         Descriptor of a specified segment.
C
C     SEGID          Segment identifier (also called `segment name') of
C                    a specified segment.
C
C     PREC           A pointing record.  PREC is an array of double
C                    precision numbers that represent a pointing
C                    instance, a time tag, and optionally, an angular
C                    velocity vector.  The structure of PREC is
C                    dependent on the data type of the current segment;
C                    this type may be found in the segment descriptor
C                    output by CKSSIN_G.
C
C                    For type 1 segments,  PREC has the structure
C
C                       PREC( 1 ) = TIMTAG
C
C                       PREC( 2 ) = q0
C                       PREC( 3 ) = q1
C                       PREC( 4 ) = q2
C                       PREC( 5 ) = q3
C
C                       PREC( 6 ) = Av1  ]
C                       PREC( 7 ) = Av2  |-- Returned optionally
C                       PREC( 8 ) = Av3  ]
C
C                    TIMTAG is the encoded spacecraft clock time
C                    associated with the returned pointing values.
C
C                    The quantities q0 - q3 represent a quaternion.
C                    The quantities Av1, Av2, and Av3 represent the
C                    angular velocity vector, and are returned only if
C                    the segment contains angular velocity data. The
C                    quaternion and the angular velocity vector are
C                    specified relative to the inertial reference
C                    frame of the segment.
C
C$ Parameters
C
C     MAXFIL         is the maximum number of C-kernels that can
C                    be loaded by this suite of subroutines
C                    simultaneously under any circumstances.  In
C                    practice, the maximum number of C-kernels that
C                    can be loaded by this suite of routines may
C                    be lower, depending on how many DAF files are
C                    loaded.
C
C$ Exceptions
C
C     1)  If this routine is called directly, the error
C         SPICE(BOGUSENTRY) is signalled.
C
C         See the entry points for exceptions specific to those
C         routines.
C
C$ Files
C
C     1)  This suite of subroutines operates on C-kernels that contain
C         only discrete segment types.  Currently, only type 1 segments
C         are allowed.
C
C$ Particulars
C
C     CKSR_G and its entry points are a suite of routines supplied to
C     complement the functionality of the SPICELIB CK readers CKGP and
C     CKGPAV.  There are several types of look-up activities that are
C     awkward or next to impossible to accomplish with CKGP or CKGPAV
C     but that can easily be performed with these readers:
C
C        -- Finding the pointing instance preceding or succeeding a
C           particular pointing instance in a discrete C-kernel.
C           By `discrete' we mean that all of the segments in the kernel
C           have discrete data types:  they contain pointing for a
C           discrete set of times.  Currently, the only discrete
C           C-kernel data type is type 1.
C
C        -- Finding the precise set of times for which a discrete
C           C-kernel contains pointing.
C
C        -- Finding the pointing instance in a discrete C-kernel whose
C           time tag differs from a specified time tag by the minimum
C           amount, where the minimum is taken over all time tags of
C           pointing instances for a specified instrument in the
C           C-kernel.
C
C      These routines view a discrete C-kernel as a single, ordered
C      sequence of pointing instances.  By `pointing instance' we
C      mean the pointing and, if present, angular velocity data
C      that corresponds to a time value.  The first pointing instance
C      in the sequence is the first pointing instance in the first
C      segment of the file; the last pointing instance is the last
C      pointing instance in the last segment of the file.  The ordering
C      here is simply the physical ordering of the pointing in the file;
C      if pointing instance A is in segment i, pointing instance B is in
C      segment j, and i < j, then pointing instance A precedes pointing
C      instance B, and for pointing instances B and C within the same
C      segment, B precedes C if B was written into the segment before
C      C.
C
C      Note that the ordering of the pointing by time in a discrete
C      C-kernel may differ from the physical ordering described here,
C      since although within any segment, the time ordering and
C      physical ordering of the pointing coincide, the ordering of
C      segments within a file is at the discretion of the creator
C      of the file.  It is recommended that applications using the
C      sequential readers make use of the time tags associated with
C      the pointing to verify any assumptions made about the time
C      ordering of the data.
C
C      Pictorially, the pointing instances in a discrete C-kernel can
C      be thought of as a sequence of marks on a number line:
C
C        1                                                     n
C        |--|--|--|--|--|--|--|--|--|--|--|--|--|--|--|--|--|--|
C
C        |                                                     |
C
C      first pointing                                     last pointing
C      instance of file                                instance of file
C
C      Pointing indices are counted from the start of file, not the
C      start of segments.  The first index is 1.
C
C      Segment boundaries play no role in this view of the C-kernel,
C      although a routine is provided to return the indices of the
C      first and last pointing instances in any segment.
C
C      Routines are provided to
C
C         -- Tell you how many pointing instances and segments are in a
C            C-kernel.
C
C         -- Look up any pointing instance by its index in the sequence
C            ---for example, you can ask for the 17,304th pointing
C            instance in a C-kernel, as long as the C-kernel contains
C            at least that many pointing instances.
C
C         -- Look up the pointing instance for a specified instrument
C            whose time tag is closest to a specified encoded SCLK
C            value.  Every pointing instance in the file for the
C            specified instrument is examined in this search.
C
C         -- Retrieve the segment descriptor, segment identifier, and
C            beginning and ending pointing indices of any segment
C            specified by index.
C
C         -- Load C-kernels for use by these readers, and unload them.
C
C      Below, we refer to this set of routines as `the sequential
C      readers.'
C
C      The way files are handled by these readers differs somewhat from
C      the way CKGP and CKGPAV work.  While CKGP and CKGPAV can search
C      any file loaded by CKLPF to satisfy a pointing request, the
C      sequential readers only search one file at a time, and the
C      calling program must specify which file that is via a file
C      handle input argument.  Even when a program has several files
C      loaded at the same time for access by the sequential readers,
C      the readers only search a single kernel--the one specified in
C      the readers' argument lists--when they are called to look up
C      pointing.
C
C      The treatment of multiple files employed by these readers is
C      less flexible than that used by CKGP and CKGPAV, but it does
C      enable the calling program to determine exactly which file
C      is used to satisfy a pointing request.
C
C      These readers suffer from the drawback that they are strongly
C      tied to particular C-kernel data types:  namely, discrete ones.
C      The SPICELIB readers CKGP and CKGPAV, in contrast to this, make
C      the data type of the C-kernels they read transparent to the
C      calling application.  For this reason, CKGP and CKGPAV promise
C      to enable programs that use them to maintain upward compatibility
C      with future C-kernels that may use data representations not
C      yet implemented.  CKSR_G does not allow the same kind of upward
C      compatibility.  However, CKSR_G may be upgraded to handle other
C      discrete pointing data types, if any are developed.
C
C      For applications that do not require the sequential-read
C      capability provided by these routines, NAIF recommends that you
C      use the SPICELIB readers CKGP and CKGPAV.
C
C      The names, functions, and calling sequences of the routines in
C      this suite of readers are:
C
C         1)  CKSLD_G  ( CKS, load )
C
C             This routine opens a C-kernel and makes it available to
C             the sequential reader entry points.  Each C-kernel must
C             be loaded by this routine before any of the other entry
C             points can access the kernel.
C
C                CALL CKSLD_G ( CKFILE, HANDLE )
C
C
C         2)  CKSUL_G  ( CKS, unload )
C
C             This routine closes a C-kernel loaded by CKSLD_G, making
C             room available so that other C-kernels can be loaded.
C
C                CALL CKSUL_G ( HANDLE )
C
C
C         3)  CKSFIN_G ( CKS, file information )
C
C             This routine returns the number of segments (SCOUNT)
C             and the number of pointing instances (PCOUNT) in a
C             specified C-kernel.  It is usually necessary to call
C             this routine once after loading a C-kernel, before
C             performing a `lookup by index' on it.
C
C                CALL CKSFIN_G ( HANDLE, SCOUNT, PCOUNT )
C
C
C         4)  CKSSIN_G ( CKS, segment information )
C
C             This routine returns the indices, counted from the start
C             of the file, of the first and last pointing instances in
C             a specified segment of a C-kernel, as well the descriptor
C             and identifier of the segment.
C
C                CALL CKSSIN_G ( HANDLE,
C                                NSEG,
C                                SEGBEG,
C                                SEGEND,
C                                SEGDSC,
C                                SEGID  )
C
C
C         5)  CKSLPI_G  ( CKS, look up pointing by index )
C
C             This routine returns a pointing record containing
C             a time tag, pointing, and optionally angular velocity
C             for the pointing instance having a specified index in a
C             specified C-kernel.  As discussed above, the entire set
C             of pointing instances in a C-kernel is considered to be
C             a single sequence for the purposes of this lookup
C             algorithm.
C
C             The index of the segment containing the pointing is also
C             returned.  This allows users to find the segment
C             descriptor and identifier for the segment containing the
C             returned pointing.
C
C             Regardless of how many files are loaded for use by the
C             sequential readers, only the file specified by the input
C             argument HANDLE is searched by this routine.
C
C             It is an error to specify an invalid index on input to
C             this routine, so CKSINF_G should be called first to
C             obtain the number of pointing instances in the file
C             to be searched.  The first index is always 1.
C
C                CALL CKSLPI_G ( HANDLE,
C                                NPREC,
C                                NSEG,
C                                PREC  )
C
C
C         6)  CKSLPT_G  ( CKS, look up pointing by time )
C
C             This routine finds the pointing instance in a specified
C             C-kernel, for a specified instrument, that has the time
C             tag that matches a specified encoded SCLK value most
C             closely.  Normally, the time tag of every pointing
C             instance in the file, for the specifed instument, is
C             examined in this search.  However, you can choose whether
C             all segments containing data for the instrument, or only
C             those containing both pointing and angular velocity data
C             for the instrument, are searched.
C
C             In the case of multiple pointing instances having equally
C             good matches, the last instance is selected.
C
C             The index of the segment containing the pointing is also
C             returned.  This allows users to find the segment
C             descriptor and identifier for the segment containing the
C             returned pointing.
C
C             Regardless of how many files are loaded for use by the
C             sequential readers, only the file specified by the input
C             argument HANDLE is searched by this routine.
C
C             It is an error to request pointing for an instrument for
C             which there is no data in the specified file.
C
C                CALL CKSLPT_G ( HANDLE,
C                                INST,
C                                SCLKDP,
C                                NEEDAV,
C                                NSEG,
C                                NPREC,
C                                PREC   )
C
C
C$ Examples
C
C     1)  Read every pointing record of a type 1 C-kernel file
C         specified by a user.  Read the file in forward order.
C         Convert the pointing to Galileo RA, Dec and Twist
C         (a 3-2-3 Euler angle sequence).  Also convert the
C         encoded SCLK time tag to a GLL SCLK string.
C
C
C                  PROGRAM EZRDR
C
C            C
C            C     In this simple program, we assume that the
C            C     pointing in the kernel named by the user
C            C     is specified relative to the EME50 coordinate
C            C     frame ( referred to as 'FK4' within SPICELIB).
C            C     We also assume that all of the data in the C-kernel
C            C     gives pointing for a particular, known instrument
C            C     or spacecraft structure, such as the GLL scan
C            C     platform.
C            C
C            C     A more robust program that actually checks the
C            C     reference frame and instrument code for each
C            C     segment is shown in the second example, below.
C            C
C
C            C
C            C     SPICELIB functions
C            C
C                  DOUBLE PRECISION      DPR
C                  DOUBLE PRECISION      HALFPI
C
C            C
C            C     Local parameters
C            C
C                  INTEGER               FILEN
C                  PARAMETER           ( FILEN = 128 )
C
C                  INTEGER               GLL
C                  PARAMETER           ( GLL   = -77 )
C
C            C
C            C     Local variables
C            C
C                  CHARACTER*(FILEN)     CK
C                  CHARACTER*(FILEN)     SCKER
C                  CHARACTER*(30)        SCLKCH
C
C                  DOUBLE PRECISION      CMAT ( 3, 3 )
C                  DOUBLE PRECISION      DEC
C                  DOUBLE PRECISION      PREC ( 8 )
C                  DOUBLE PRECISION      RA
C                  DOUBLE PRECISION      TWIST
C
C                  INTEGER               HANDLE
C                  INTEGER               I
C                  INTEGER               NSEG
C                  INTEGER               PCOUNT
C                  INTEGER               SCOUNT
C
C            C
C            C     Get the name of a C-kernel.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Enter name of C-kernel.'
C                  READ  (*,FMT='(A)') CK
C
C            C
C            C     We'll also need an SCLK kernel for converting
C            C     encoded SCLK to strings.
C            C
C                  WRITE (*,*) 'Enter name of GLL SCLK kernel.'
C                  READ  (*,FMT='(A)') SCKER
C
C            C
C            C     Load the C-kernel for access by the sequential
C            C     readers.
C            C
C                  CALL CKSLD_G  ( CK, HANDLE )
C
C            C
C            C     Load the SCLK kernel.
C            C
C                  CALL LDPOOL ( SCKER )
C
C            C
C            C     Find out how many pointing records are in the kernel;
C            C     this number is returned as PCOUNT.  In this
C            C     application, we will not need the segment count
C            C     SCOUNT.
C            C
C                  CALL CKSFIN_G (  HANDLE,   SCOUNT,    PCOUNT )
C
C            C
C            C     Read every pointing record in turn.  The contents
C            C     of the output array PREC are as described in
C            C     $Detailed_Output, above.
C            C
C
C                  DO I = 1, PCOUNT
C            C
C            C        Use the sequential reader CKSLPI_G (CKS, look
C            C        up pointing by index) to read the Ith pointing
C            C        record.
C            C
C                     CALL CKSLPI_G ( HANDLE, I, NSEG, PREC )
C
C            C
C            C        Convert the time tag to a string.  The time tag
C            C        is the first element of PREC.
C            C
C                     CALL SCDECD ( GLL, PREC(1), SCLKCH )
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'Time tag: ', SCLKCH
C
C            C
C            C        Convert the quaternion to RA, DEC, and TWIST.
C            C        We convert the quaternion to a C-matrix as an
C            C        intermediate step.
C            C
C                     CALL Q2M   ( PREC(2), CMAT )
C                     CALL M2EUL ( CMAT, 3, 2, 3, TWIST, DEC, RA )
C
C                     DEC = HALFPI() - DEC
C
C            C
C            C        Convert the Euler angles from radians to degrees.
C            C        Use the SPICELIB function DPR to obtain the
C            C        conversion factor.
C            C
C                     RA    = DPR() * RA
C                     DEC   = DPR() * DEC
C                     TWIST = DPR() * TWIST
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'Pointing angles (relative to '     //
C                 .               'EME50 frame, in degrees):'
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'RA:    ', RA
C                     WRITE (*,*) 'DEC:   ', DEC
C                     WRITE (*,*) 'TWIST: ', TWIST
C
C                  END DO
C
C                  END
C
C
C
C     2)  This program is a more robust version of the program
C         EZRDR shown above:  this version checks each segment
C         to see what inertial reference frame the pointing is
C         specified relative to.  The instrument ID code for each
C         segment is also retrieved.
C
C
C                  PROGRAM EZ2
C            C
C            C     Read every pointing instance in a GLL C-kernel.
C            C     For each segment, check the instrument ID code
C            C     and the reference frame.
C            C
C
C            C
C            C     SPICELIB functions
C            C
C                  DOUBLE PRECISION      DPR
C                  DOUBLE PRECISION      HALFPI
C
C            C
C            C     Local parameters
C            C
C                  INTEGER               DSCSIZ
C                  PARAMETER           ( DSCSIZ = 5 )
C
C                  INTEGER               FILEN
C                  PARAMETER           ( FILEN  = 128 )
C
C                  INTEGER               GLL
C                  PARAMETER           ( GLL    = -77 )
C
C                  INTEGER               IDLEN
C                  PARAMETER           ( IDLEN  =  40 )
C
C                  INTEGER               ND
C                  PARAMETER           ( ND     =   2 )
C
C                  INTEGER               NI
C                  PARAMETER           ( NI     =   6 )
C
C                  INTEGER               TYPIDX
C                  PARAMETER           ( TYPIDX =   3 )
C
C                  INTEGER               INSIDX
C                  PARAMETER           ( INSIDX =   1 )
C
C                  INTEGER               REFIDX
C                  PARAMETER           ( REFIDX =   4 )
C
C            C
C            C     Local variables
C            C
C                  CHARACTER*(FILEN)     CK
C                  CHARACTER*(FILEN)     SCKER
C                  CHARACTER*(30)        SCLKCH
C                  CHARACTER*(IDLEN)     SEGID
C                  CHARACTER*(30)        REFCH
C
C                  DOUBLE PRECISION      CMAT ( 3, 3 )
C                  DOUBLE PRECISION      DC   ( ND )
C                  DOUBLE PRECISION      DEC
C                  DOUBLE PRECISION      DESC ( DSCSIZ )
C                  DOUBLE PRECISION      PREC ( 8 )
C                  DOUBLE PRECISION      RA
C                  DOUBLE PRECISION      TWIST
C
C                  INTEGER               DTYPE
C                  INTEGER               HANDLE
C                  INTEGER               I
C                  INTEGER               IC     ( NI )
C                  INTEGER               INST
C                  INTEGER               J
C                  INTEGER               NSEG
C                  INTEGER               PCOUNT
C                  INTEGER               REF
C                  INTEGER               SCOUNT
C                  INTEGER               SEGBEG
C                  INTEGER               SEGEND
C
C            C
C            C     Get the name of a C-kernel.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Enter name of C-kernel.'
C                  READ  (*,FMT='(A)') CK
C
C            C
C            C     We'll also need an SCLK kernel for converting
C            C     encoded SCLK to strings.
C            C
C                  WRITE (*,*) 'Enter name of GLL SCLK kernel.'
C                  READ  (*,FMT='(A)') SCKER
C
C            C
C            C     Load the C-kernel for access by the sequential
C            C     readers.
C            C
C                  CALL CKSLD_G  ( CK, HANDLE )
C
C            C
C            C     Load the SCLK kernel.
C            C
C                  CALL LDPOOL ( SCKER )
C
C            C
C            C     Find out how many pointing records are in the kernel;
C            C     this number is returned as PCOUNT.
C            C
C                  CALL CKSFIN_G (  HANDLE,   SCOUNT,    PCOUNT )
C
C            C
C            C     Read every segment in turn; within each segment,
C            C     read every pointing record in turn.  The contents
C            C     of the output array PREC are as described in
C            C     $Detailed_Output, above.
C            C
C
C                  DO I = 1, SCOUNT
C            C
C            C        Obtain the segment descriptor, and from that
C            C        the instrument ID code and reference frame.
C            C
C                     CALL CKSSIN_G ( HANDLE,
C                 .                   I,
C                 .                   SEGBEG,
C                 .                   SEGEND,
C                 .                   DESC,
C                 .                   SEGID    )
C
C                     CALL DAFUS    ( DESC, ND, NI, DC, IC )
C
C                     REF    = IC(REFIDX)
C                     DTYPE  = IC(TYPIDX)
C            C
C            C        Convert the integer code for the reference frame
C            C        to a short character string (such as 'J2000').
C            C
C                     CALL IRFNAM ( REF, REFCH )
C
C
C                     DO J = SEGBEG, SEGEND
C            C
C            C           Use the sequential reader CKSLPI_G (CKS, look
C            C           up pointing by index) to read the Ith pointing
C            C           record.
C            C
C                        CALL CKSLPI_G ( HANDLE, J, NSEG, PREC )
C
C            C
C            C           Convert the time tag to a string.  The time tag
C            C           is the first element of PREC.
C            C
C                        CALL SCDECD ( GLL, PREC(1), SCLKCH )
C
C                        WRITE (*,*) ' '
C                        WRITE (*,*) ' '
C                        WRITE (*,*) 'Time tag: ', SCLKCH
C
C            C
C            C           Convert the quaternion to RA, DEC, and TWIST.
C            C           We convert the quaternion to a C-matrix as an
C            C           intermediate step.
C            C
C                        CALL Q2M   ( PREC(2), CMAT )
C                        CALL M2EUL ( CMAT, 3, 2, 3, TWIST, DEC, RA )
C
C                        DEC = HALFPI() - DEC
C
C            C
C            C           Convert the Euler angles from radians to
C            C           degrees.  Use the SPICELIB function DPR to
C            C           obtain the conversion factor.
C            C
C                        RA    = DPR() * RA
C                        DEC   = DPR() * DEC
C                        TWIST = DPR() * TWIST
C
C                        WRITE (*,*) ' '
C                        WRITE (*,*) 'Instrument code:          ', INST
C                        WRITE (*,*) 'Inertial reference frame: ', REFCH
C                        WRITE (*,*) 'Pointing angles (relative to '//
C                    .               'inertial frame, in degrees):'
C                        WRITE (*,*) ' '
C                        WRITE (*,*) 'RA:    ', RA
C                        WRITE (*,*) 'DEC:   ', DEC
C                        WRITE (*,*) 'TWIST: ', TWIST
C
C                     END DO
C
C                  END DO
C
C                  END
C
C
C
C
C     3)  Read the fourth pointing record of the fifth segment
C         of a C-kernel whose name is given by the variable CK.
C
C            C
C            C     Load the file and obtain a file handle.
C            C
C                  CALL CKSLD_G  ( CK, HANDLE )
C
C            C
C            C     Find out how many segments are in the file.
C            C     (In this case, we're assuming that there are
C            C     at least five segments.)
C            C
C                  CALL CKSFIN_G ( HANDLE, SCOUNT, PCOUNT )
C
C                  IF ( SCOUNT .LT. 5 ) THEN
C                     CALL SETMSG ( 'Only # segments in file #.' )
C                     CALL ERRINT ( '#', SCOUNT                  )
C                     CALL ERRCH  ( '#', CK                      )
C                     CALL SIGERR ( 'Too few segments.'          )
C                  END IF
C
C            C
C            C     Get the start and end indices SEGBEG and SEGEND of
C            C     the pointing instances in the fifth segment in the
C            C     C-kernel.
C            C
C                  CALL CKSSIN_G ( HANDLE,
C                 .                5,
C                 .                SEGBEG,
C                 .                SEGEND,
C                 .                SEGDSC,
C                 .                SEGID  )
C
C            C
C            C     Compute the index of the fourth pointing record
C            C     of the fifth segment.
C            C
C                  NPREC = SEGBEG + 4 - 1
C
C            C
C            C     Look up the pointing record PREC.
C            C
C                  CALL CKSLPI_G ( HANDLE, NPREC, NSEG, PREC )
C
C
C
C     4)  Find the pointing instance for the GLL scan platform whose
C         time tag is closest to 1/00398184:21:0:0.  Also look up
C         the preceding and following pointing instances.
C
C         This procedure is useful for simple linear interpolation of
C         pointing.
C
C         The file name is held in the variable CK.  A GLL SCLK kernel
C         is also required for encoding the SCLK value; the name of this
C         kernel is in the variable SCKER.
C
C            C
C            C     Load the file and obtain a file handle.
C            C
C                  CALL CKSLD_G  ( CK, HANDLE )
C
C            C
C            C     Load a GLL SCLK kernel for SCLK encoding, and
C            C     encode the time of interest.
C            C
C                  CALL LDPOOL ( SCKER )
C
C                  GLL = -77
C                  CALL SCENCD ( GLL, '1/398184:21:0:0', SCLKDP )
C
C            C
C            C     Find pointing at the time of interest PREC2, and also
C            C     obtain the index NPREC of the pointing instance.  For
C            C     our purposes, it is irrelevant whether angular
C            C     velocity data is present, so we set NEEDAV to false.
C            C
C                  SCANPL = -77001
C
C                  CALL CKSLPT_G ( HANDLE, SCANPL, SCLKDP, .FALSE.,
C                 .                NSEG,   NPREC,  PREC2            )
C
C            C
C            C     Find the preceding and following pointing
C            C     instances PREC1 and PREC3.  (In a more robust
C            C     program, the cases where NPREC is the first or last
C            C     pointing instance in the file would be treated.
C            C     Also, if the pointing instance with index NPREC is
C            C     at a segment boundary, a check should be made that
C            C     the neighboring pointing instances contain pointing
C            C     for the correct instrument.  Finally, the differences
C            C     between the time tag of the NPREC pointing instance
C            C     and those of its neighbors should be tested to ensure
C            C     that the differences are not so large as to
C            C     invalidate the interpolation.  For brevity, we leave
C            C     these checks out.)
C            C
C                  CALL CKSLPI_G ( HANDLE, NPREC-1, NSEG, PREC1 )
C                  CALL CKSLPI_G ( HANDLE, NPREC+1, NSEG, PREC3 )
C
C
C
C     5)  Unload a C-kernel, to reduce the number of loaded kernels.
C
C         Here HANDLE is the file handle returned by CKSLD_G when
C         the C-kernel was loaded.
C
C                  CALL CKSUL_G ( HANDLE )
C
C
C
C     6)  Read GLL orbiter rotor and scan platform C-kernels in
C         parallel, and combine the pointing to construct
C         (low-accuracy) C-matrices for the stator.  This example
C         assumes that the rotor and stator z-axes are perfectly
C         aligned, and that the scan platform N axis is perfectly
C         aligned with the stator Y axis.  We also presume that
C         the scan plaform and rotor C-kernels contain pointing
C         for the exact same set of times.
C
C            C
C            C     Load an SCLK kernel for GLL SCLK decoding.
C            C
C                  CALL LDPOOL ( 'SCKER' )
C
C            C
C            C     Load the C-kernels.
C            C
C                  CALL CKSLD_G  ( 'ROTOR_CK.BC',  HANDLE(1) )
C                  CALL CKSLD_G  ( 'SCANPL_CK.BC', HANDLE(2) )
C
C                  DO I = 1, 2
C                     CALL CKSFIN_G (  HANDLE(I), SCOUNT(I), PCOUNT(I) )
C                  END DO
C
C                  IF ( PCOUNT(1) .NE. PCOUNT(2) ) THEN
C                     CALL SETMSG ( 'C-kernel pointing counts ' //
C                 .                 'do not match.'               )
C                     CALL SIGERR ( 'INVALIDKERNELS'              )
C                  END IF
C
C                  DO I = 1, PCOUNT(1)
C            C
C            C        Collect rotor pointing in PREC1, scan platform
C            C        pointing in PREC2.
C            C
C                     CALL CKSLPI_G ( HANDLE(1), I, NSEG, PREC1 )
C                     CALL CKSLPI_G ( HANDLE(2), I, NSEG, PREC2 )
C
C            C
C            C        No go if the time tags don't match up.
C            C
C                     IF ( PREC1(1) .NE. PREC2(1) ) THEN
C                        CALL SETMSG ( 'Time tags of # pointing '
C                 .                    'instances don''t match.'  )
C                        CALL REPMOT ( '#', I                     )
C                        CALL SIGERR ( 'INVALIDTIMETAGS'          )
C                     END IF
C
C            C
C            C        Convert the quaternions to C-matrices:
C            C
C            C           RCMAT  for the rotor
C            C           SPCMAT for the scan plaform
C            C
C                     CALL Q2M ( PREC1(2), RCMAT  )
C                     CALL Q2M ( PREC2(2), SPCMAT )
C
C            C
C            C        Construct the x-, y-, and z-basis vectors
C            C        of the stator coordinate system.
C            C
C            C        The x-vector is the cross product of the
C            C        y- and z-vectors, in that order.
C            C
C                     DO J = 1, 3
C                        Y(J)  =   SPCMAT(2,J)
C                        Z(J)  =   RCMAT (3,J)
C                     END DO
C
C                     CALL VCRSS ( Y, Z, X )
C            C
C            C        Build the stator C-matrix STCMAT and decompose
C            C        it as a series of Euler angles.
C            C
C                     DO J = 1, 3
C                        STCMAT(1,J) = X(J)
C                        STCMAT(2,J) = Y(J)
C                        STCMAT(3,J) = Z(J)
C                     END DO
C
C                     CALL M2EUL ( STCMAT, 3, 2, 3, TWIST, DEC, RA )
C
C                     DEC = HALFPI() - DEC
C
C            C
C            C        Convert the Euler angles to degrees using the
C            C        SPICELIB function DPR.
C            C
C                     RA     =  DPR() * RA
C                     DEC    =  DPR() * DEC
C                     TWIST  =  DPR() * TWIST
C
C            C
C            C        Decode the common time tag of the rotor and
C            C        scan platform pointing.
C            C
C                     GLL = -77
C                     CALL SCDECD ( GLL, PREC1(1), SCLKCH )
C
C            C
C            C        Print out our results.  We are assuming that
C            C        the pointing is given relative to the EME50
C            C        frame.  A more robust program should check
C            C        this.
C            C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'Pointing angles (relative to '     //
C                 .               'EME50 frame, in degrees):'
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'RA:    ', RA
C                     WRITE (*,*) 'DEC:   ', DEC
C                     WRITE (*,*) 'TWIST: ', TWIST
C
C                  END DO
C
C
C$ Restrictions
C
C     1)  This suite of routines will not work on C-kernels that contain
C         segments of any data type other than type 1.
C
C     2)  This suite of routines cannot be used to read C-kernels that
C         are also loaded for use by the SPICELIB readers CKGP or
C         CKGPAV.  Using those readers on kernels loaded by for use
C         by this suite of routines will cause these routines to fail
C         in unpredictable ways.
C
C     3)  This suite of routines will not work on C-kernels having
C         type 1 segments that contain no pointing instances.
C
C     4)  Although pointing lookups `by index' may be performed in
C         random order, such lookups are performed most efficiently
C         in sequential (either forward or backward) order.
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
C-    GLLSPICE Version 1.0.0, 16-SEP-1991 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     CK sequential reader
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               FILEN
      PARAMETER           ( FILEN = 128 )
 
      INTEGER               ND
      PARAMETER           ( ND     = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI     = 6 )
 
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ =  ND + (NI+1)/2 )
 
      INTEGER               NIL
      PARAMETER           ( NIL    = -1 )
 
      INTEGER               RECSIZ
      PARAMETER           ( RECSIZ =  8 )
 
C
C     Indices of elements of the segment descriptor's integer component:
C
      INTEGER               INSIDX
      PARAMETER           ( INSIDX = 1 )
 
      INTEGER               TYPIDX
      PARAMETER           ( TYPIDX = 3 )
 
      INTEGER               AVFIDX
      PARAMETER           ( AVFIDX = 4 )
 
 
C
C     Local variables
C
 
C
C     The file table contains information about each CK loaded
C     by this suite of routines.  There are separate arrays
C     to store each type of information.  The names of the arrays
C     and the information they store are:
C
C        Array                   Description
C        -----                   -----------
C        FTHAN                   CK handle.
C        FTNSEG                  Number of segments.
C        FTNPNT                  Number of pointing instances.
C        FTCSEG                  Current segment number.
C        FTFPRI                  First pointing record index of
C                                current segment.
C        FTLPRI                  Last pointing record index of
C                                current segment.
C
C
      INTEGER               FTHAN  ( MAXFIL )
      INTEGER               FTNSEG ( MAXFIL )
      INTEGER               FTNPNT ( MAXFIL )
      INTEGER               FTCSEG ( MAXFIL )
      INTEGER               FTFPRI ( MAXFIL )
      INTEGER               FTLPRI ( MAXFIL )
 
C
C     The file pool is a singly linked list pool used to index
C     the information in the file table.  The head of the active
C     list is FTHEAD; the head of the free list is FTFREE.
C
      INTEGER               FTPOOL ( MAXFIL )
      INTEGER               FTHEAD
      INTEGER               FTFREE
 
 
C
C     Other local variables
C
      CHARACTER*(FILEN)     NAME
 
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      DC     ( ND     )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIFF
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      SAVSUM ( DSCSIZ )
      DOUBLE PRECISION      SUM    ( DSCSIZ )
      DOUBLE PRECISION      TAG
 
      INTEGER               FRST
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               LAST
      INTEGER               MIDDLE
      INTEGER               NEXT
      INTEGER               NREC
      INTEGER               P
      INTEGER               PNTCNT
      INTEGER               PNTIDX
      INTEGER               PREV
      INTEGER               SAVIDX
      INTEGER               SEGCNT
 
      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               HAVPNT
      LOGICAL               MORE
 
C
C     Saved variables
C
      SAVE
 
 
C
C     Initial values
C
      DATA                  FIRST  /           .TRUE.  /
      DATA                  FTHEAD /            NIL    /
      DATA                  FTFREE /            NIL    /
      DATA                  FTCSEG /   MAXFIL * 0      /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKSR_G' )
      END IF
 
C
C     It is an error to call this routine directly.
C
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'CKSR_G' )
      RETURN
 
 
 
 
 
C$Procedure CKSLD_G ( CKS, load )
 
      ENTRY CKSLD_G ( CKFILE, HANDLE )
 
C$ Abstract
C
C     Make a C-kernel available to the CK sequential reader routines.
C
C$ Required_Reading
C
C     CK
C     DAF
C     NAIF_IDS
C     SCLK
C     TIME
C
C$ Keywords
C
C     CK
C     FILES
C     POINTING
C     SEARCH
C
C$ Declarations
C
C     CHARACTER*(*)         CKFILE
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CKFILE     I   Name of a discrete C-kernel.
C     HANDLE     O   File handle used to identify CKFILE.
C
C$ Detailed_Input
C
C     CKFILE         Name of C-kernel to load.  CKFILE must contain
C                    only segments having a discrete data type.
C                    Currently, the only discrete type is type 1.
C
C$ Detailed_Output
C
C     HANDLE         Handle of the C-kernel named by the input
C                    argument CKFILE.  HANDLE is an integer used
C                    to identify this file to the other entry
C                    points of CKSR_G.
C
C                    HANDLE is a DAF handle returned by DAFOPR;
C                    see the DAF required reading for details.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If an attempt to open CKFILE fails, the error will be
C         signalled by routines called by this routine.
C
C     2)  If the maximum allowed number MAXFIL of C-kernels has
C         been loaded, the error SPICE(CKSFILETABLEFULL) is signalled.
C
C     3)  If CKFILE contains any segments whose data type is not
C         discrete, the error SPICE(CKSINVALIDTYPE) will be signalled.
C
C     4)  If CKFILE contains any `empty' segments, that is, segments
C         having no pointing instances in them, the error
C         SPICE(CKSEMPTYSEGMENT) will be signalled.
C
C$ Files
C
C     1) See description of CKFILE in $Detailed_Input.
C
C$ Particulars
C
C     This routine makes a specified C-kernel known to the sequential
C     C-kernel readers.  Any C-kernel that is to be read by the
C     sequential readers must first be loaded via this routine.
C
C     Multiple C-kernels can be loaded at the same time, up to a limit
C     of MAXFIL different kernels.  Loading a C-kernel has no effect
C     on searches through other loaded C-kernels.
C
C$ Examples
C
C     1)  See the $Examples section of CKSR_G, above.
C
C$ Restrictions
C
C     1)  This suite of routines will not work on C-kernels that contain
C         segments of any data type other than type 1.
C
C     2)  This suite of routines cannot be used to read C-kernels that
C         are also loaded for use by the SPICELIB readers CKGP or
C         CKGPAV.  Using those readers on kernels loaded by for use
C         by this suite of routines will cause these routines to fail
C         in unpredictable ways.
C
C     3)  This suite of routines will not work on C-kernels having
C         type 1 segments that contain no pointing instances.
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
C-    GLLSPICE Version 1.0.0, 16-SEP-1991 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     load CK file for sequential reading
C
C-&
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKSLD_G' )
      END IF
 
C
C     The first time through, initialize the file pool.
C
      IF ( FIRST ) THEN
 
         DO 50001
     .      I = 1, MAXFIL-1
            FTPOOL(I) = I + 1
50001    CONTINUE
 
         FTPOOL(MAXFIL) = NIL
         FTHEAD         = NIL
         FTFREE         = 1
 
         FIRST = .FALSE.
 
      END IF
 
C
C     Open the CK for reading, if possible.
C
      CALL DAFOPR ( CKFILE, HANDLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKSLD_G' )
         RETURN
      END IF
 
C
C     Make sure there's room in the file table.
C
      IF ( FTFREE .EQ. NIL ) THEN
 
         CALL SETMSG ( 'Could not load #.  Maximum number of CKs '    //
     .                 '(#) are already loaded.'                       )
         CALL ERRCH  ( '#', CKFILE                                     )
         CALL ERRINT ( '#', MAXFIL                                     )
         CALL SIGERR ( 'SPICE(CKSFILETABLEFULL)'                       )
         CALL CHKOUT ( 'CKSLD_G'                                       )
         RETURN
 
      END IF
 
C
C     Put the information for this file at the head of the active list.
C
      P            =  FTFREE
      FTFREE       =  FTPOOL( FTFREE )
      FTPOOL( P )  =  FTHEAD
      FTHEAD       =  P
 
C
C     Scan the segments in the file.  Keep track of the total number
C     of pointing instances seen, as the segments are examined.
C
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND )
 
      SEGCNT = 0
      PNTCNT = 0
 
50002 IF       ( FOUND )
     .THEN
 
         SEGCNT = SEGCNT + 1
 
         CALL DAFGS ( SUM )
         CALL DAFUS ( SUM, ND, NI, DC, IC )
 
C        Check the segment's data type.  It's an error if we hit a
C        segment that is not type 1.
C
         IF (  IC(TYPIDX)  .NE.  1  )  THEN
C
C           Reclaim the file table space we saved for this file, then
C           signal an error.
C
C           Make the successor of P the head of the list, and
C           link P into the free list.
C
            FTHEAD       =  FTPOOL( P )
            FTPOOL( P )  =  FTFREE
            FTFREE       =  P
 
            CALL DAFHFN ( HANDLE, NAME )
 
            CALL SETMSG ( 'Segment # in file # is of type #.' )
            CALL ERRINT ( '#', SEGCNT                         )
            CALL ERRCH  ( '#', NAME                           )
            CALL ERRINT ( '#', IC(TYPIDX)                     )
            CALL SIGERR ( 'SPICE(CKSINVALIDTYPE)'             )
            CALL CHKOUT ( 'CKSLD_G'                           )
            RETURN
 
         END IF
 
C
C        Obtain the pointing instance count for this segment.  It's an
C        error if there are no pointing instances in the segment.
C
         CALL CKNR01 ( HANDLE, SUM, NREC )
 
         IF ( NREC .LT. 1 ) THEN
C
C           Reclaim the file table space we saved for this file, then
C           signal an error.
C
C           Make the successor of P the head of the list, and
C           link P into the free list.
C
            FTHEAD       =  FTPOOL( P )
            FTPOOL( P )  =  FTFREE
            FTFREE       =  P
 
            CALL DAFHFN ( HANDLE, NAME )
 
            CALL DAFHFN ( HANDLE, NAME                          )
            CALL SETMSG ( 'No pointing in segment # in file #.' )
            CALL ERRINT ( '#', SEGCNT                           )
            CALL ERRCH  ( '#', NAME                             )
            CALL SIGERR ( 'SPICE(CKSEMPTYSEGMENT)'              )
            CALL CHKOUT ( 'CKSLD_G'                             )
            RETURN
 
         END IF
 
 
         PNTCNT  =  PNTCNT + NREC
 
C
C        Examine the next segment.
C
         CALL DAFFNA ( FOUND )
 
         GO TO 50002
      END IF
 
C
C     Save the handle for this CK.
C
      FTHAN ( P ) = HANDLE
 
C
C     Save the number of segments and the number of pointing instances
C     for this file.
C
      FTNSEG ( P ) = SEGCNT
      FTNPNT ( P ) = PNTCNT
 
C
C     Re-start a forward search.  Find the first array.
C
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )
 
C
C     Set the current segment number to 1.
C
      FTCSEG ( P ) = 1
 
C
C     Find out how many instances are in the segment, and save the
C     initial and final pointing instance indices for the segment.
C
      CALL DAFGS  ( SUM               )
      CALL CKNR01 ( HANDLE, SUM, NREC )
 
      FTFPRI ( P ) = 1
      FTLPRI ( P ) = NREC
 
C
C     Now
C
C        FTHAN
C        FTNSEG
C        FTNPNT
C        FTCSEG
C        FTFPRI
C        FTLPRI
C
C     are all set for the current C-kernel.
C
 
      CALL CHKOUT ( 'CKSLD_G' )
      RETURN
 
 
 
 
 
C$Procedure CKSUL_G ( CKS, unload )
 
      ENTRY CKSUL_G ( HANDLE )
 
C$ Abstract
C
C     Unload a C-kernel that was loaded by the CK sequential reader
C     routines.
C
C$ Required_Reading
C
C     CK
C     DAF
C     NAIF_IDS
C     SCLK
C     TIME
C
C$ Keywords
C
C     CK
C     FILES
C     POINTING
C     SEARCH
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle used to identify C-kernel to unload.
C
C$ Detailed_Input
C
C     HANDLE         Handle of a C-kernel to unload.  HANDLE must be a
C                    file handle returned by the routine CKSLD_G.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input handle does not specify a C-kernel currently
C         loaded for use by this suite of routines, this routine
C         has no effect.  No error is signalled.
C
C$ Files
C
C     1) See description of HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     There are several reasons to unload a C-kernel loaded by
C     CKSLD_G:
C
C        -- There is maximum number (MAXFIL) of C-kernels that can be
C           loaded at any one time, so in order to search more than
C           this number of C-kernels in any program run, some files
C           must be unloaded to make room for others.
C
C       --  There is a maximum number (FTSIZE) of DAF files that can be
C           loaded at any one time.  C-kernels loaded by this routine
C           count against the total number of DAFs loaded.  It may
C           be necessary to unload one or more  C-kernels in order
C           to permit loading of some other type of DAF, such as an
C           SPK file.
C
C       --  There is a maximum number of files than can be opened
C           simultaneously by one process (under most operating
C           systems).  C-kernels loaded for use by the sequential
C           readers count against this limit.  Unloading a C-kernel
C           closes that file and increases by one the number of other
C           files that may be opened.
C
C$ Examples
C
C     1)  See the $Examples section of CKSR_G, above.
C
C$ Restrictions
C
C     1)  This suite of routines will not work on C-kernels that contain
C         segments of any data type other than type 1.
C
C     2)  This suite of routines cannot be used to read C-kernels that
C         are also loaded for use by the SPICELIB readers CKGP or
C         CKGPAV.  Using those readers on kernels loaded by for use
C         by this suite of routines will cause these routines to fail
C         in unpredictable ways.
C
C     3)  This suite of routines will not work on C-kernels having
C         type 1 segments that contain no pointing instances.
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
C-    GLLSPICE Version 1.0.0, 16-SEP-1991 (NJB)
C
C-&
 
C$ Index_Entries
C
C     unload CK file loaded for sequential reading
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKSUL_G' )
      END IF
 
C
C     The first time through, initialize the file pool, if necessary.
C
      IF ( FIRST ) THEN
 
         DO 50003
     .      I = 1, MAXFIL-1
            FTPOOL(I) = I + 1
50003    CONTINUE
 
         FTPOOL(MAXFIL) = NIL
         FTHEAD         = NIL
         FTFREE         = 1
 
         FIRST = .FALSE.
 
      END IF
 
C
C     Search the file table for the specified handle.
C
      P     =  FTHEAD
      FOUND = .FALSE.
      PREV  =  NIL
 
50004 IF       (  ( P .NE. NIL )  .AND.  ( .NOT. FOUND )  )
     .THEN
 
         IF ( FTHAN(P) .EQ. HANDLE ) THEN
            FOUND = .TRUE.
         ELSE
            PREV  =  P
            P     =  FTPOOL(P)
         END IF
 
         GO TO 50004
      END IF
 
 
      IF ( FOUND ) THEN
C
C        Close the kernel.  It is not an error if the kernel is already
C        closed.
C
         CALL DAFCLS ( HANDLE )
 
C
C        Reclaim the file table space we saved for this file.
C
         IF ( P .EQ. FTHEAD ) THEN
C
C           Make the successor of P the head of the list, and
C           link P into the free list.
C
            FTHEAD       =  FTPOOL( P )
            FTPOOL( P )  =  FTFREE
            FTFREE       =  P
 
         ELSE
C
C           Make the predecessor of P point to the successor of P, and
C           link P into the free list.
C
            FTPOOL( PREV )  =  FTPOOL ( P )
            FTPOOL( P    )  =  FTFREE
            FTFREE          =  P
 
         END IF
 
      END IF
 
 
      CALL CHKOUT ( 'CKSUL_G' )
      RETURN
 
 
 
 
 
 
C$Procedure CKSFIN_G ( CKS, file information )
 
      ENTRY CKSFIN_G ( HANDLE, SCOUNT, PCOUNT )
 
C$ Abstract
C
C     Return segment and pointing instance counts for a C-kernel loaded
C     for sequential reading.
C
C$ Required_Reading
C
C     CK
C     DAF
C     NAIF_IDS
C     SCLK
C     TIME
C
C$ Keywords
C
C     CK
C     FILES
C     POINTING
C     SEARCH
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               SCOUNT
C     INTEGER               PCOUNT
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a C-kernel loaded by CKSLD_G.
C     SCOUNT     O   Number of segments in specified C-kernel.
C     PCOUNT     O   Number of pointing instances in specified C-kernel.
C
C$ Detailed_Input
C
C     HANDLE         Handle of C-kernel about which information is
C                    requested.  The C-kernel must already have been
C                    loaded by CKSLD_G.
C
C$ Detailed_Output
C
C     SCOUNT         Segment count; this is the number of segments in
C                    the C-kernel specified by HANDLE.
C
C     PCOUNT         Pointing instance count; this is the number of
C                    pointing instances in the C-kernel specified by
C                    HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called when no C-kernels are
C         loaded by the routine CKSLD_G, the error
C         SPICE(CKSNOKERNELLOADED) will be signalled.
C
C     2)  If the input argument HANDLE does not designate a file
C         currently loaded for use by these readers, the error
C         SPICE(CKSUNKNOWNKERNEL) will be signalled.
C
C$ Files
C
C     1) See description of HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Normally, this routine is called prior to starting a sequential
C     read of a C-kernel.  The number PCOUNT returned by this routine
C     is the upper bound of the range of valid pointing indices.
C
C$ Examples
C
C     1)  See the $Examples section of CKSR_G, above.
C
C$ Restrictions
C
C     1)  This suite of routines will not work on C-kernels that contain
C         segments of any data type other than type 1.
C
C     2)  This suite of routines cannot be used to read C-kernels that
C         are also loaded for use by the SPICELIB readers CKGP or
C         CKGPAV.  Using those readers on kernels loaded by for use
C         by this suite of routines will cause these routines to fail
C         in unpredictable ways.
C
C     3)  This suite of routines will not work on C-kernels having
C         type 1 segments that contain no pointing instances.
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
C-    GLLSPICE Version 1.0.0, 16-SEP-1991 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     return information on CK file opened for sequential reading
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKSFIN_G' )
      END IF
 
C
C     It's an error to call this routine when no files are loaded.
C
      IF ( FTHEAD .EQ. NIL ) THEN
 
         CALL SETMSG ( 'No CK files are currently loaded.' )
         CALL SIGERR ( 'SPICE(CKSNOKERNELLOADED)'          )
         CALL CHKOUT ( 'CKSFIN_G'                          )
         RETURN
 
      END IF
 
C
C     Search the file table for the specified handle.
C
      P     =  FTHEAD
      PREV  =  NIL
      FOUND = .FALSE.
 
50005 IF       (  ( P .NE. NIL )  .AND.  ( .NOT. FOUND )  )
     .THEN
 
         IF ( FTHAN(P) .EQ. HANDLE ) THEN
            FOUND = .TRUE.
         ELSE
            PREV  =  P
            P     =  FTPOOL(P)
         END IF
 
         GO TO 50005
      END IF
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'No file with handle # is loaded for '   //
     .                 'sequential reading.'                   )
         CALL ERRINT ( '#', HANDLE                             )
         CALL SIGERR ( 'SPICE(CKSUNKNOWNKERNEL)'               )
         CALL CHKOUT ( 'CKSFIN_G'                              )
         RETURN
 
      END IF
 
C
C     Move the information about the file to the head of the
C     active list, if it is not already there.
C
      IF ( P .NE. FTHEAD ) THEN
C
C        Link the predecessor of P to the successor of P.  The
C        predecessor is not NIL, since P is not at the head of
C        the list.
C
         FTPOOL(PREV)  =  FTPOOL(P)
 
C
C        The successor of P is the current head of the active list.
C
         FTPOOL(P)     =  FTHEAD
 
C
C        P is the new head of the active list.
C
         FTHEAD        =  P
 
      END IF
 
C
C     Return the segment and pointing instance counts.
C
      SCOUNT = FTNSEG( P )
      PCOUNT = FTNPNT( P )
 
      CALL CHKOUT ( 'CKSFIN_G' )
      RETURN
 
 
 
 
 
C$Procedure CKSSIN_G ( CKS, segment information )
 
      ENTRY CKSSIN_G ( HANDLE,
     .                 NSEG,
     .                 SEGBEG,
     .                 SEGEND,
     .                 SEGDSC,
     .                 SEGID  )
 
C$ Abstract
C
C     Return information about a specified segment in a specified CK
C     file open for sequential reading.
C
C$ Required_Reading
C
C     CK
C     DAF
C     NAIF_IDS
C     SCLK
C     TIME
C
C$ Keywords
C
C     CK
C     FILES
C     POINTING
C     SEARCH
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               NSEG
C     INTEGER               SEGBEG
C     INTEGER               SEGEND
C     DOUBLE PRECISION      SEGDSC ( * )
C     CHARACTER*(*)         SEGID
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a C-kernel loaded by CKSLD_G.
C     NSEG       I   Index of segment to return information about.
C     SEGBEG,
C     SEGEND     O   Indices of first and last pointing instances
C                    in segment.
C     SEGDSC     O   Segment descriptor.
C     SEGID      O   Segment identifier.
C
C$ Detailed_Input
C
C     HANDLE         Handle of C-kernel containing the segment about
C                    which information is requested.  C-kernel must
C                    already have been loaded by CKSLD_G.
C
C     NSEG           Number (index) of segment about which information
C                    is requested.
C
C$ Detailed_Output
C
C     SEGBEG,
C     SEGEND         Pointing instance indices of first and last
C                    pointing instances in a specified segment.
C
C     SEGDSC         Descriptor of a specified segment.
C
C     SEGID          Segment identifier (also called `segment name') of
C                    a specified segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called when no C-kernels are
C         loaded by the routine CKSLD_G, the error
C         SPICE(CKSNOKERNELLOADED) will be signalled.
C
C     2)  If the input argument HANDLE does not designate a file
C         currently loaded for use by these readers, the error
C         SPICE(CKSUNKNOWNKERNEL) will be signalled.
C
C     3)  IF NSEG is out of the range [1, SCOUNT], where SCOUNT is the
C         number of segments in the C-kernel designated by HANDLE, the
C         error SPICE(VALUEOUTOFRANGE) is signalled.
C
C$ Files
C
C     1) See description of HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Normal uses of this routine include:
C
C        -- Finding the range of indices of the pointing instances
C           in a specified segment, so the reader CKSLPI_G can be
C           used to read that segment.
C
C        -- Retrieving the segment descriptor of a specified
C           segment, in order to determine whether angular velocity
C           is present in that segment, what instrument the segment
C           contains pointing for, or what the time bounds of the
C           segment are.
C
C$ Examples
C
C     1)  See the $Examples section of CKSR_G, above.
C
C$ Restrictions
C
C     1)  This suite of routines will not work on C-kernels that contain
C         segments of any data type other than type 1.
C
C     2)  This suite of routines cannot be used to read C-kernels that
C         are also loaded for use by the SPICELIB readers CKGP or
C         CKGPAV.  Using those readers on kernels loaded by for use
C         by this suite of routines will cause these routines to fail
C         in unpredictable ways.
C
C     3)  This suite of routines will not work on C-kernels having
C         type 1 segments that contain no pointing instances.
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
C-    GLLSPICE Version 1.0.0, 16-SEP-1991 (NJB)
C
C-&
 
C$ Index_Entries
C
C     return sequentially read CK segment information
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKSSIN_G' )
      END IF
 
C
C     It's an error to call this routine when no files are loaded.
C
      IF ( FTHEAD .EQ. NIL ) THEN
 
         CALL SETMSG ( 'No CK files are currently loaded.' )
         CALL SIGERR ( 'SPICE(CKSNOKERNELLOADED)'          )
         CALL CHKOUT ( 'CKSSIN_G'                          )
         RETURN
 
      END IF
 
C
C     Search the file table for the specified handle.
C
      P     =  FTHEAD
      FOUND = .FALSE.
      PREV  =  NIL
 
50006 IF       (  ( P .NE. NIL )  .AND.  ( .NOT. FOUND )  )
     .THEN
 
         IF ( FTHAN(P) .EQ. HANDLE ) THEN
            FOUND = .TRUE.
         ELSE
            PREV  =  P
            P     =  FTPOOL(P)
         END IF
 
         GO TO 50006
      END IF
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'No file with handle # is loaded for '   //
     .                 'sequential reading.'                   )
         CALL ERRINT ( '#', HANDLE                             )
         CALL SIGERR ( 'SPICE(CKSUNKNOWNKERNEL)'               )
         CALL CHKOUT ( 'CKSSIN_G'                              )
         RETURN
 
      END IF
 
C
C     Move the information about the file to the head of the
C     active list, if it is not already there.
C
      IF ( P .NE. FTHEAD ) THEN
C
C        Link the predecessor of P to the successor of P.  The
C        predecessor is not NIL, since P is not at the head of
C        the list.
C
         FTPOOL(PREV)  =  FTPOOL(P)
 
C
C        The successor of P is the current head of the active list.
C
         FTPOOL(P)     =  FTHEAD
 
C
C        P is the new head of the active list.
C
         FTHEAD        =  P
 
      END IF
 
C
C     Make sure that the requested segment exists.
C
      IF (  ( NSEG .LT. 1 )  .OR.  ( NSEG .GT. FTNSEG(P) )  ) THEN
 
         CALL DAFHFN (  FTHAN(P), NAME                                 )
         CALL SETMSG ( 'Segment index is out of range.  Index was #. '//
     .                 'Valid range for file # is [1, #].'             )
         CALL ERRINT ( '#', NSEG                                       )
         CALL ERRCH  ( '#', NAME                                       )
         CALL ERRINT ( '#', FTNSEG(P)                                  )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                        )
         CALL CHKOUT ( 'CKSSIN_G'                                      )
         RETURN
 
      END IF
 
 
C
C     Obtain the segment descriptor and name for the segment.  A search
C     has already been started, so just move the DAF segment pointer
C     by NSEG - FTCSEG(P).  Keep track of the segments' beginning and
C     ending pointing instance indices as we go.
C
      CALL DAFCS ( HANDLE )
 
      IF ( NSEG .EQ. FTCSEG(P) ) THEN
 
         CALL DAFGS ( SEGDSC )
 
 
      ELSE IF ( NSEG .GT. FTCSEG(P) ) THEN
 
         DO 50007
     .      I = 1, NSEG - FTCSEG(P)
 
            CALL DAFFNA ( FOUND  )
            CALL DAFGS  ( SEGDSC )
 
            CALL CKNR01 ( HANDLE, SEGDSC, NREC )
 
            FTFPRI( P )  =  FTLPRI( P ) + 1
            FTLPRI( P )  =  FTFPRI( P ) + NREC - 1
            FTCSEG( P )  =  FTCSEG( P ) + 1
 
50007    CONTINUE
 
 
 
      ELSE IF ( NSEG .LT. FTCSEG(P) ) THEN
 
         DO 50008
     .      I = 1, FTCSEG(P) - NSEG
 
            CALL DAFFPA ( FOUND  )
            CALL DAFGS  ( SEGDSC )
 
            CALL CKNR01 ( HANDLE, SEGDSC, NREC )
 
            FTLPRI( P )  =  FTFPRI( P ) - 1
            FTFPRI( P )  =  FTLPRI( P ) - NREC + 1
            FTCSEG( P )  =  FTCSEG( P ) - 1
 
50008    CONTINUE
 
      END IF
 
C
C     Now retrieve the segment name.
C
      CALL DAFGN ( SEGID )
 
C
C     Set the first and last pointing instance indices for the segment.
C
      SEGBEG = FTFPRI( P )
      SEGEND = FTLPRI( P )
 
 
      CALL CHKOUT ( 'CKSSIN_G' )
      RETURN
 
 
 
 
 
C$Procedure CKSLPI_G ( CKS, look up pointing by index )
 
      ENTRY CKSLPI_G ( HANDLE, NPREC, NSEG, PREC )
 
C$ Abstract
C
C     Return a pointing record containing the time tag, pointing,
C     and optionally angular velocity of a pointing instance
C     having a specified index within a discrete C-kernel.
C
C$ Required_Reading
C
C     CK
C     DAF
C     NAIF_IDS
C     SCLK
C     TIME
C
C$ Keywords
C
C     CK
C     FILES
C     POINTING
C     SEARCH
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               NPREC
C     INTEGER               NSEG
C     DOUBLE PRECISION      PREC   ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a C-kernel loaded by CKSLD_G.
C     NPREC      I   Index in file of pointing instance.
C     NSEG       O   Index of segment in which pointing was found.
C     PREC       O   Pointing record.
C
C$ Detailed_Input
C
C     HANDLE         Handle of C-kernel to search.  C-kernel must
C                    already have been loaded by CKSLD_G.
C
C     NPREC          Index, in the file designated by HANDLE, of the
C                    pointing instance PREC.  The range of NPREC is
C                    [1, PCOUNT], where PCOUNT is the count of
C                    pointing instances in the specified C-kernel.
C                    PCOUNT may be obtained via a call to the
C                    entry point CKSFIN_G.
C
C$ Detailed_Output
C
C     NSEG           Number of the segment, in the file designated by
C                    HANDLE, in which the pointing instance PREC
C                    (see below) was found.
C
C     PREC           A pointing record.  PREC is an array of double
C                    precision numbers that represent a pointing
C                    instance, a time tag, and optionally, an angular
C                    velocity vector.  The structure of PREC is
C                    dependent on the data type of the current segment;
C                    this type may be found in the segment descriptor
C                    output by CKSSIN_G.
C
C                    For type 1 segments,  PREC has the structure
C
C                       PREC( 1 ) = TIMTAG
C
C                       PREC( 2 ) = q0
C                       PREC( 3 ) = q1
C                       PREC( 4 ) = q2
C                       PREC( 5 ) = q3
C
C                       PREC( 6 ) = Av1  ]
C                       PREC( 7 ) = Av2  |-- Returned optionally
C                       PREC( 8 ) = Av3  ]
C
C                    TIMTAG is the encoded spacecraft clock time
C                    associated with the returned pointing values.
C
C                    The quantities q0 - q3 represent a quaternion.
C                    The quantities Av1, Av2, and Av3 represent the
C                    angular velocity vector, and are returned only if
C                    the segment contains angular velocity data. The
C                    quaternion and the angular velocity vector are
C                    specified relative to the inertial reference
C                    frame of the segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called when no C-kernels are
C         loaded by the routine CKSLD_G, the error
C         SPICE(CKSNOKERNELLOADED) will be signalled.
C
C     2)  If the input argument HANDLE does not designate a file
C         currently loaded for use by these readers, the error
C         SPICE(CKSUNKNOWNKERNEL) will be signalled.
C
C     3)  If the pointing index NPREC is less than 1 or greater
C         than the number of pointing instances in the file
C         designated by HANDLE, the error SPICE(VALUEOUTOFRANGE)
C         is signalled.
C
C$ Files
C
C     1) See description of HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine returns an array containing the time tag, pointing,
C     and, if present, angular velocity comprising the pointing
C     instance having a specified index within a specified discrete
C     C-kernel.  As discussed in the header of CKSR_G above, this
C     reader views a discrete C-kernel as a single sequence of
C     pointing instances. The index of the first pointing instance in
C     the first segment of the C-kernel is 1; the index of the last
C     pointing instance of the last segment is the total number of
C     pointing instances in the file.
C
C     This routine may be used in circumstances requiring sequential
C     reading of C-kernels.  In fact, this routine can be used to
C     look up pointing instances in random order, but the lookup
C     algorithm is much more efficient when the lookup order is
C     sequential (either forward or backward).
C
C$ Examples
C
C     1)  See the $Examples section of CKSR_G, above.
C
C$ Restrictions
C
C     1)  This suite of routines will not work on C-kernels that contain
C         segments of any data type other than type 1.
C
C     2)  This suite of routines cannot be used to read C-kernels that
C         are also loaded for use by the SPICELIB readers CKGP or
C         CKGPAV.  Using those readers on kernels loaded by for use
C         by this suite of routines will cause these routines to fail
C         in unpredictable ways.
C
C     3)  This suite of routines will not work on C-kernels having
C         type 1 segments that contain no pointing instances.
C
C     4)  Although pointing lookups `by index' may be performed in
C         random order, such lookups are performed most efficiently
C         in sequential (either forward or backward) order.
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
C-    GLLSPICE Version 1.0.0, 16-SEP-1991 (NJB)
C
C-&
 
C$ Index_Entries
C
C     CK sequential reader lookup pointing by index
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKSLPI_G' )
      END IF
 
C
C     It's an error to call this routine when no files are loaded.
C
      IF ( FTHEAD .EQ. NIL ) THEN
 
         CALL SETMSG ( 'No CK files are currently loaded.' )
         CALL SIGERR ( 'SPICE(CKSNOKERNELLOADED)'          )
         CALL CHKOUT ( 'CKSLPI_G'                          )
         RETURN
 
      END IF
 
C
C     Search the file table for the specified handle.
C
      P     =  FTHEAD
      FOUND = .FALSE.
      PREV  =  NIL
 
50009 IF       (  ( P .NE. NIL )  .AND.  ( .NOT. FOUND )  )
     .THEN
 
         IF ( FTHAN(P) .EQ. HANDLE ) THEN
            FOUND = .TRUE.
         ELSE
            PREV  =  P
            P     =  FTPOOL(P)
         END IF
 
         GO TO 50009
      END IF
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'No file with handle # is loaded for '   //
     .                 'sequential reading.'                    )
         CALL ERRINT ( '#', HANDLE                              )
         CALL SIGERR ( 'SPICE(CKSUNKNOWNKERNEL)'                )
         CALL CHKOUT ( 'CKSLPI_G'                               )
         RETURN
 
      END IF
 
C
C     Move the information about the file to the head of the
C     active list, if it is not already there.
C
      IF ( P .NE. FTHEAD ) THEN
C
C        Link the predecessor of P to the successor of P.  The
C        predecessor is not NIL, since P is not at the head of
C        the list.
C
         FTPOOL(PREV)  =  FTPOOL(P)
 
C
C        The successor of P is the current head of the active list.
C
         FTPOOL(P)     =  FTHEAD
 
C
C        P is the new head of the active list.
C
         FTHEAD        =  P
 
      END IF
 
C
C     At this point, P points to the file table information for the
C     file designated by HANDLE.
C
 
C
C     Make sure that the requested pointing instance exists.
C
      IF (  ( NPREC .LT. 1 )  .OR.  ( NPREC .GT. FTNPNT(P) )  ) THEN
 
         CALL DAFHFN (  FTHAN(P), NAME                                 )
         CALL SETMSG ( 'Pointing instance index was #.  Valid range ' //
     .                 'for file # is [1, #].'                         )
         CALL ERRINT ( '#', NPREC                                      )
         CALL ERRCH  ( '#', NAME                                       )
         CALL ERRINT ( '#', FTNPNT(P)                                  )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                        )
         CALL CHKOUT ( 'CKSLPI_G'                                      )
         RETURN
 
      END IF
 
C
C     If the requested instance is in the current segment, just look
C     up the pointing.
C
      IF (       (  NPREC .GE. FTFPRI(P)  )
     .     .AND. (  NPREC .LE. FTLPRI(P)  )   ) THEN
 
         CALL DAFCS  ( HANDLE )
         CALL DAFGS  ( SUM    )
 
         CALL CKGR01 ( HANDLE,
     .                 SUM,
     .                 NPREC - FTFPRI(P) + 1,
     .                 PREC )
 
         NSEG = FTCSEG(P)
 
C
C     Try not to be too stupid about starting our search in the
C     right place.  If the desired instance is the first, just
C     start a new forward search.  If the desired instance is the
C     last, start a backward search.
C
      ELSE IF ( NPREC .EQ. 1 ) THEN
 
         CALL DAFBFS ( HANDLE )
         CALL DAFFNA ( FOUND  )
         CALL DAFGS  ( SUM    )
 
C
C        Re-set the file table variables
C
C           FTCSEG
C           FTFPRI
C           FTLPRI
C
         FTCSEG( P )  =  1
         FTFPRI( P )  =  1
 
         CALL CKNR01 ( HANDLE, SUM, FTLPRI(P) )
 
C
C        Set the output arguments NSEG and PREC.
C
         NSEG = 1
         CALL CKGR01 ( HANDLE, SUM, 1, PREC )
 
 
 
      ELSE IF ( NPREC .EQ. FTNPNT(P) ) THEN
 
         CALL DAFBBS ( HANDLE )
         CALL DAFFPA ( FOUND  )
         CALL DAFGS  ( SUM    )
 
C
C        Re-set the file table variables
C
C           FTCSEG
C           FTFPRI
C           FTLPRI
C
         FTCSEG( P )  =  FTNSEG( P )
         FTLPRI( P )  =  FTNPNT( P )
 
         CALL CKNR01 ( HANDLE, SUM, NREC )
 
         FTFPRI( P )  =  FTLPRI( P )  -  NREC  +  1
 
         NSEG         =  FTNSEG( P )
 
         CALL CKGR01 ( HANDLE, SUM, NREC, PREC )
 
C
C     If the requested instance has an index lower than the lowest
C     index of the instances in the current segment, search the segments
C     in reverse order until we come to the correct one.
C
      ELSE IF (  NPREC  .LT.  FTFPRI( P )  ) THEN
C
C        Back up through the segments until we find the right one.
C        Update the file table variables
C
C           FTCSEG
C           FTFPRI
C           FTLPRI
C
C        as we go.
C
C
         CALL DAFCS  ( HANDLE )
 
50010    IF       (  NPREC  .LT.  FTFPRI( P )  )
     .   THEN
 
            CALL DAFFPA ( FOUND  )
            CALL DAFGS  ( SUM    )
 
            CALL CKNR01 ( HANDLE, SUM, NREC )
 
            FTLPRI( P )  =  FTFPRI( P ) - 1
            FTFPRI( P )  =  FTLPRI( P ) - NREC + 1
            FTCSEG( P )  =  FTCSEG( P ) - 1
 
            GO TO 50010
         END IF
 
C
C        At this point, the current segment contains the pointing
C        instance we're interested in.  The index of the pointing
C        instance relative to the start of the segment is
C
C           NPREC - FTFPRI(P) + 1.
C
         CALL CKGR01 ( HANDLE,   SUM,   NPREC - FTFPRI(P) + 1,  PREC  )
 
         NSEG = FTCSEG(P)
 
C
C     If the requested instance has an index higher than the highest
C     index of the instances in the current segment, search the segments
C     in forward order until we come to the correct one.
C
      ELSE
C
C        Advance up through the segments until we find the right one.
C        Update the file table variables
C
C           FTCSEG
C           FTFPRI
C           FTLPRI
C
C        as we go.
C
         CALL DAFCS  ( HANDLE )
 
50011    IF       (  NPREC  .GT.  FTLPRI( P )  )
     .   THEN
 
            CALL DAFFNA ( FOUND  )
            CALL DAFGS  ( SUM    )
 
            CALL CKNR01 ( HANDLE, SUM, NREC )
 
            FTFPRI( P )  =  FTLPRI( P ) + 1
            FTLPRI( P )  =  FTFPRI( P ) + NREC - 1
            FTCSEG( P )  =  FTCSEG( P ) + 1
 
            GO TO 50011
         END IF
 
C
C        At this point, the current segment contains the pointing
C        instance we're interested in.  The index of the pointing
C        instance relative to the start of the segment is
C
C           NPREC - FTFPRI(P) + 1.
C
         CALL CKGR01 ( HANDLE,   SUM,   NPREC - FTFPRI(P) + 1,  PREC  )
 
         NSEG = FTCSEG(P)
 
      END IF
 
C
C     If we've come this far, we have the pointing instance with index
C     NPREC.  The file table has been updated so that
C
C        FTCSEG
C        FTFPRI
C        FTLPRI
C
C     all apply to the current segment.
C
 
      CALL CHKOUT ( 'CKSLPI_G' )
      RETURN
 
 
 
 
 
C$Procedure CKSLPT_G ( CKS, look up pointing by time )
 
      ENTRY CKSLPT_G ( HANDLE, INST, SCLKDP, NEEDAV, NSEG, NPREC, PREC )
 
C$ Abstract
C
C     For a specified instrument or structure in a specified discrete
C     C-kernel, return a pointing record containing the time tag,
C     pointing, and optionally angular velocity of a pointing instance
C     having a time tag that best matches a specified time.
C
C$ Required_Reading
C
C     CK
C     DAF
C     NAIF_IDS
C     SCLK
C     TIME
C
C$ Keywords
C
C     CK
C     FILES
C     POINTING
C     SEARCH
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               INST
C     DOUBLE PRECISION      SCLKDP
C     LOGICAL               NEEDAV
C     INTEGER               NSEG
C     INTEGER               NPREC
C     DOUBLE PRECISION      PREC   ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a C-kernel loaded by CKSLD_G.
C     INST       I   NAIF integer code of an instrument.
C     SCLKDP     I   Encoded SCLK time tag.
C     NEEDAV     I   Flag indicating whether angular velocity is needed.
C     NSEG       O   Index of segment in which pointing was found.
C     NPREC      O   Index in file of pointing instance.
C     PREC       O   Pointing record.
C
C$ Detailed_Input
C
C     HANDLE         Handle of C-kernel to search.  The C-kernel must
C                    already have been loaded by CKSLD_G.
C
C     INST           NAIF integer code of an instrument or spacecraft
C                    structure for which pointing is desired.
C
C     SCLKDP         Encoded SCLK time for which pointing is desired.
C
C     NEEDAV         Logical flag indicating whether only segments that
C                    contain angular velocity data should be considered
C                    when attempting to satisfy a request for pointing.
C
C$ Detailed_Output
C
C     NSEG           Number of the segment, in the file designated by
C                    HANDLE, in which the pointing instance PREC
C                    (see below) was found.
C
C     NPREC          Index, in the file designated by HANDLE, of the
C                    pointing instance PREC.
C
C     PREC           A pointing record representing the pointing
C                    instance, for the instrument INST, whose time tag
C                    best matches the request time SCLKDP.  If NEEDAV
C                    is true, only segments containing angular velocity
C                    data will be considered in the search.
C
C                    If multiple applicable pointing instances have
C                    time tags that match SCLKDP equally well, the last
C                    such pointing instance in the file will be
C                    selected.
C
C                    PREC is an array of double precision numbers that
C                    represent a pointing instance, a time tag, and
C                    optionally, an angular velocity vector.  The
C                    structure of PREC is dependent on the data type of
C                    the current segment; this type may be found in the
C                    segment descriptor output by CKSSIN_G.
C
C                    For type 1 segments,  PREC has the structure
C
C                       PREC( 1 ) = TIMTAG
C
C                       PREC( 2 ) = q0
C                       PREC( 3 ) = q1
C                       PREC( 4 ) = q2
C                       PREC( 5 ) = q3
C
C                       PREC( 6 ) = Av1  ]
C                       PREC( 7 ) = Av2  |-- Returned optionally
C                       PREC( 8 ) = Av3  ]
C
C                    TIMTAG is the encoded spacecraft clock time
C                    associated with the returned pointing values.
C
C                    The quantities q0 - q3 represent a quaternion.
C                    The quantities Av1, Av2, and Av3 represent the
C                    angular velocity vector, and are returned only if
C                    the segment contains angular velocity data. The
C                    quaternion and the angular velocity vector are
C                    specified relative to the inertial reference
C                    frame of the segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called when no C-kernels are
C         loaded by the routine CKSLD_G, the error
C         SPICE(CKSNOKERNELLOADED) will be signalled.
C
C     2)  If the input argument HANDLE does not designate a file
C         currently loaded for use by these readers, the error
C         SPICE(CKSUNKNOWNKERNEL) will be signalled.
C
C     3)  If no pointing is found for the specified instrument,
C         the error SPICE(CKSNOPOINTING) is signalled.
C
C     4)  If angular velocity is requested and pointing is found for the
C         specified instrument, but no pointing is found in a segment
C         also containing angular velocity data, the error
C         SPICE(CKSNOANGVELOCITY) is signalled.
C
C$ Files
C
C     1) See description of HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine searches a specified discrete C-kernel for a pointing
C     instance whose time tag is the best match with a specified
C     encoded SCLK time.  All of the segments that contain pointing
C     for the specified instrument are searched.
C
C     Because the look-up algorithm employed by this routine involves
C     searching an entire C-kernel, the algorithm is not nearly as
C     efficient as that used by the SPICELIB readers CKGP and CKGPAV:
C     these stop searching when a satisfactory segment is found.  By
C     contrast, this reader finds the best match in the entire file.
C
C     This routine is provided primarily as a convenience for users who
C     wish to start a sequential search at a particular point in a
C     C-kernel, where that point is defined by a time tag.  For example,
C     to perform a linear interpolation between the pointing values
C     whose time tags bracket a specified time, one would find the
C     pointing instance whose time tag matched the specified time
C     most closely, look up the successor and predecessor of that
C     pointing instance in the C-kernel, and then interpolate between
C     whichever pair of these pointing instances has time tags that
C     bracket the time of interest.
C
C$ Examples
C
C     1)  See the $Examples section of CKSR_G, above.
C
C$ Restrictions
C
C     1)  This suite of routines will not work on C-kernels that contain
C         segments of any data type other than type 1.
C
C     2)  This suite of routines cannot be used to read C-kernels that
C         are also loaded for use by the SPICELIB readers CKGP or
C         CKGPAV.  Using those readers on kernels loaded by for use
C         by this suite of routines will cause these routines to fail
C         in unpredictable ways.
C
C     3)  This suite of routines will not work on C-kernels having
C         type 1 segments that contain no pointing instances.
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
C-    GLLSPICE Version 1.0.0, 16-SEP-1991 (NJB)
C
C-&
 
C$ Index_Entries
C
C     CK sequential reader lookup pointing by time
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKSLPT_G' )
      END IF
 
C
C     It's an error to call this routine when no files are loaded.
C
      IF ( FTHEAD .EQ. NIL ) THEN
 
         CALL SETMSG ( 'No CK files are currently loaded.' )
         CALL SIGERR ( 'SPICE(CKSNOKERNELLOADED)'          )
         CALL CHKOUT ( 'CKSLPT_G'                          )
         RETURN
 
      END IF
 
 
C
C     Search the file table for the specified handle.
C
      P     =  FTHEAD
      FOUND = .FALSE.
      PREV  =  NIL
 
50012 IF       (  ( P .NE. NIL )  .AND.  ( .NOT. FOUND )  )
     .THEN
 
         IF ( FTHAN(P) .EQ. HANDLE ) THEN
            FOUND = .TRUE.
         ELSE
            PREV  =  P
            P     =  FTPOOL(P)
         END IF
 
         GO TO 50012
      END IF
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'No file with handle # is loaded for '         //
     .                 'reading.'                             )
         CALL ERRINT ( '#', HANDLE                            )
         CALL SIGERR ( 'SPICE(CKSUNKNOWNKERNEL)'              )
         CALL CHKOUT ( 'CKSLPT_G'                             )
         RETURN
 
      END IF
 
C
C     Move the information about the file to the head of the
C     active list, if it is not already there.
C
      IF ( P .NE. FTHEAD ) THEN
C
C        Link the predecessor of P to the successor of P.  The
C        predecessor is not NIL, since P is not at the head of
C        the list.
C
         FTPOOL(PREV)  =  FTPOOL(P)
 
C
C        The successor of P is the current head of the active list.
C
         FTPOOL(P)     =  FTHEAD
 
C
C        P is the new head of the active list.
C
         FTHEAD        =  P
 
      END IF
 
C
C     At this point, P points to the file table information for the
C     file designated by HANDLE.
C
 
C
C     Our search algorithm is simple:  we examine every segment that
C     contains data for the specified instrument.  We keep track of
C     the index of the pointing instance with the best match as we go.
C
      CALL DAFBFS ( HANDLE )
 
      NSEG      = 0
      NPREC     = 0
      DELTA     = DPMAX()
      FTFPRI(P) = 0
      FTLPRI(P) = 0
      HAVPNT    = .FALSE.
 
      DO 50013
     .   I = 1, FTNSEG(P)
 
         CALL DAFFNA ( FOUND )
         CALL DAFGS  ( SUM   )
         CALL DAFUS  ( SUM,    ND,   NI,  DC,  IC )
         CALL CKNR01 ( HANDLE, SUM,  NREC         )
 
C
C        Keep track of the beginning and ending instance numbers for
C        this segment, as well as the current segment number.
C
         FTCSEG(P)  =  I
         FTFPRI(P)  =  FTLPRI(P) + 1
         FTLPRI(P)  =  FTFPRI(P) + NREC - 1
 
C
C        Keep track of whether any segments with pointing were found
C        for this instrument.  If so, and if we end up without
C        pointing, we can say that the absence of angular velocity
C        data was the culprit.
C
         IF ( IC(INSIDX) .EQ. INST ) THEN
            HAVPNT  = .TRUE.
         END IF
 
C
C        If this is a segment that contains data for instrument INST,
C        and if this segment contains angular velocity if NEEDAV is
C        true, find the data whose time tag is closest to SCLKDP.
C
         IF (          ( IC(INSIDX) .EQ. INST )
     .        .AND. (  ( IC(AVFIDX) .EQ. 1    ) .OR. .NOT. NEEDAV )  )
     .   THEN
 
C
C           Get the beginning and ending time tags from the segment
C           itself, not from the segment identifier.
C
            CALL CKGR01 ( HANDLE, SUM, 1, PREC )
            BEGTIM    =   PREC(1)
 
            CALL CKGR01 ( HANDLE, SUM, NREC, PREC )
            ENDTIM    =   PREC(1)
 
            IF ( SCLKDP .LT. BEGTIM ) THEN
C
C              The time tag for the first instance of the segment is
C              the best match within this segment.
C
               DIFF   =  ABS ( SCLKDP - BEGTIM )
               PNTIDX =  FTFPRI(P)
 
 
            ELSE IF ( SCLKDP .GE. ENDTIM ) THEN
C
C              The time tag for the last instance of the segment is
C              the best match within this segment.
C
               DIFF   =  ABS ( SCLKDP - ENDTIM )
               PNTIDX =  FTLPRI(P)
 
 
            ELSE
C
C              SCLKDP falls within the time bounds for this segment.
C              Though it's not that efficient, we'll just use CKGR01
C              to carry out a binary search for the instance whose time
C              tag is the best match.  The alternative is to copy the
C              algorithm used in CKR01; that method could get ugly fast
C              if new discrete data types are invented.
C
               FRST = 1
               LAST = NREC
 
50014          IF       (   LAST - FRST   .GT.  1  )
     .         THEN
 
                  MIDDLE  =  ( LAST + FRST ) / 2
 
                  CALL CKGR01 ( HANDLE, SUM, MIDDLE, PREC )
 
                  IF ( PREC(1) .GE. SCLKDP ) THEN
                     LAST = MIDDLE
                  ELSE
                     FRST = MIDDLE
                  END IF
 
                  GO TO 50014
               END IF
 
C
C              The best match will be either the time tag of pointing
C              instance LAST or that of pointing instance FRST.  We'll
C              need to collect whichever of these time tags that wasn't
C              picked up on the last pass through the loop.
C
               IF ( FRST .EQ. MIDDLE ) THEN
 
                  BEGTIM    =   PREC(1)
                  CALL CKGR01 ( HANDLE, SUM, LAST, PREC )
                  ENDTIM    =   PREC(1)
 
               ELSE
 
                  ENDTIM    =   PREC(1)
                  CALL CKGR01 ( HANDLE, SUM, FRST, PREC )
                  BEGTIM    =   PREC(1)
 
               END IF
 
C
C              Find the best time tag match to SCLKDP.
C
               IF (       ABS( SCLKDP - BEGTIM )
     .              .LT.  ABS( SCLKDP - ENDTIM )   )  THEN
 
                  TAG      =   BEGTIM
                  DIFF     =   ABS ( SCLKDP - TAG )
                  PNTIDX   =   FTFPRI(P)  +   FRST  -  1
 
               ELSE
 
                  TAG      =   ENDTIM
                  DIFF     =   ABS ( SCLKDP - TAG )
                  PNTIDX   =   FTFPRI(P)  +   LAST  -  1
 
               END IF
 
C
C              There could be multiple records with time tags
C              equal to that of record PNTIDX.  Take the last one.
C
               NEXT  =  PNTIDX - FTFPRI(P) + 2
               MORE  =  NEXT .LT. NREC
 
50015          IF       ( MORE )
     .         THEN
 
                  CALL CKGR01 ( HANDLE, SUM, NEXT, PREC )
 
                  IF ( PREC(1) .GT. TAG ) THEN
                     MORE = .FALSE.
 
                  ELSE IF ( NEXT .LT. NREC ) THEN
                     PNTIDX =  PNTIDX + 1
                     NEXT   =  NEXT   + 1
 
                  ELSE
                     PNTIDX =  PNTIDX + 1
                     MORE   = .FALSE.
 
                  END IF
 
                  GO TO 50015
               END IF
C
C              Now PNTIDX points to the last record in the segment
C              having the time tag that is the best match.
C
 
            END IF
C
C           At this point, DIFF is the smallest time difference
C           between SCLKDP and any time tag in the current array.
C           PNTIDX is the index of the (last) pointing instance at which
C           this difference is attained.
C
C           Among segments with equally good matches, we take the
C           latest.
C
            IF ( DIFF .LE. DELTA ) THEN
 
               DELTA  = DIFF
               NSEG   = I
               NPREC  = PNTIDX
               SAVIDX = NPREC - FTFPRI(P) + 1
C
C              Save the segment summary, so we can rapidly look up
C              the pointing when we've looked at the whole file.
C
               CALL MOVED ( SUM, DSCSIZ, SAVSUM )
 
            END IF
 
         END IF
C
C        At this point, we've found the best match in the 1st through
C        Ith segments.
C
50013 CONTINUE
 
C
C     At this point, NPREC is the index of the pointing instance
C     containing pointing for INST whose time tag was the best match
C     with SCLKDP.  NSEG is the number of the segment containing that
C     pointing instance, SAVIDX is the index of the pointing instance
C     relative to the start of the segment, and SAVSUM is the summary
C     of the segment containing the pointing record itself.
C
 
C
C     It's an error if no pointing for the specified instrument was
C     found.
C
      IF ( NPREC .EQ. 0 ) THEN
 
         NSEG = 0
         CALL CLEARD ( RECSIZ, PREC )
 
         CALL DAFHFN ( HANDLE, NAME )
 
         IF ( HAVPNT ) THEN
 
            CALL SETMSG ( 'No angular velocity found for instrument ' //
     .                    '# in #.'                                    )
            CALL ERRINT ( '#', INST                                    )
            CALL ERRCH  ( '#', NAME                                    )
            CALL SIGERR ( 'SPICE(CKSNOANGVELOCITY)'                    )
 
         ELSE
 
            CALL SETMSG ( 'No pointing found for instrument # in #.'   )
            CALL ERRINT ( '#', INST                                    )
            CALL ERRCH  ( '#', NAME                                    )
            CALL SIGERR ( 'SPICE(CKSNOPOINTING)'                       )
 
         END IF
 
      END IF
 
C
C     Get the pointing.
C
      CALL CKGR01 ( HANDLE, SAVSUM, SAVIDX, PREC )
 
      CALL CHKOUT ( 'CKSLPT_G' )
      RETURN
      END
