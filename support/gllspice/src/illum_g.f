C$Procedure ILLUM_G ( Illumination angles )
 
      SUBROUTINE ILLUM_G (  TARGET,  OBSRVR,  ET,     CORR,   SPOINT,
     .                      PHASE,   SOLAR,  EMISSN                    )
 
C$ Abstract
C
C     Find the illumination angles at a specified surface point of a
C     target body.
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
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      SOLAR
      DOUBLE PRECISION      EMISSN
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARGET     I   NAIF integer code of target body.
C     OBSRVR     I   NAIF integer code of observing body.
C     ET         I   Epoch in ephemeris seconds past J2000.
C     CORR       I   Desired aberration correction.
C     SPOINT     I   Body-fixed coordinates of a target surface point.
C     PHASE      O   Phase angle at the surface point.
C     SOLAR      O   Solar incidence angle at the surface point.
C     EMISSN     O   Emission angle at the surface point.
C
C$ Detailed_Input
C
C     TARGET         is the NAIF integer code of the target body.
C
C     OBSRVR         is the NAIF integer code of the observing body.
C                    OBSRVR may be identical to TARGET.
C
C     ET             is the epoch, specified in ephemeris seconds past
C                    J2000, at which the illumination angles at the
C                    specified surface point on the target body are to
C                    be computed.
C
C     CORR           is the aberration correction to be used in
C                    computing the location and orientation of the
C                    target body and the location of the Sun.  Possible
C                    values are:
C
C                       'NONE'        No aberration correction.
C
C                       'LT'          Correct the position and
C                                     orientation of target body for
C                                     light time, and correct the
C                                     position of the Sun for light
C                                     time.
C
C                       'LT+S'        Correct the observer-target vector
C                                     for light time and stellar
C                                     aberration, correct the
C                                     orientation of the target body
C                                     for light time, and correct the
C                                     target-Sun vector for light time
C                                     and stellar aberration.
C
C
C     SPOINT         is a surface point on the target body, expressed
C                    in rectangular body-fixed (body equator and prime
C                    meridian) coordinates.  SPOINT need not be visible
C                    from the observer's location at time ET.
C
C$ Detailed_Output
C
C
C     PHASE          is the phase angle at SPOINT, as seen from OBSRVR
C                    at time ET.  PHASE is the angle between the
C                    SPOINT-OBSRVR vector and the SPOINT-Sun vector.
C                    Units are radians.  The range of  PHASE is [0, pi].
C                    See $Particulars below for a detailed discussion of
C                    the definition of PHASE.
C
C     SOLAR          is the solar incidence angle at SPOINT, as seen
C                    from OBSRVR at time ET.  SOLAR is the angle
C                    between the surface normal vector at SPOINT and the
C                    SPOINT-Sun vector.  Units are radians.  The range
C                    of SOLAR is [0, pi]. See $Particulars below for a
C                    detailed discussion of the definition of SOLAR.
C
C     EMISSN         is the emission angle at SPOINT, as seen from
C                    OBSRVR at time ET.  EMISSN is the angle between the
C                    surface normal vector at SPOINT and the
C                    SPOINT-observer vector.  Units are radians.  The
C                    range of EMISSN is [0, pi]. See $Particulars below
C                    for a detailed discussion of the definition of
C                    EMISSN.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C
C     1)  If TARGET and OBSRVR are not distinct, the error
C         SPICE(BODIESNOTDISTINCT) will be signalled.
C
C     2)  If no SPK (ephemeris) data is available for the observer,
C         target, and Sun at the time specified by ET, the error will
C         be diagnosed by routines called by this routine.  If light
C         time corrections are used, SPK data for the target body must
C         be available at the time ET - LT, where LT is the one-way
C         light time from the target to the observer at ET.
C         Additionally, SPK data must be available for the Sun at the
C         time ET - LT - LT', where LT' is the light time from the Sun
C         to the target body at time ET - LT.
C
C     3)  If PCK data defining the orientation or shape of the target
C         body is unavailable, the error will be diagnosed by routines
C         called by this routine.
C
C$ Files
C
C     No files are input to this routine.  However, ILLUM_G expects
C     that the appropriate SPK and PCK files have been loaded using
C     SPKLEF and LDPOOL, respectively.
C
C$ Particulars
C
C
C     The term `illumination angles' refers to following set of
C     angles:
C
C
C        solar incidence angle    This is the angle between the surface
C                                 normal at the specified surface point
C                                 and the vector from the surface point
C                                 to the Sun.
C
C        emission angle           This is the angle between the surface
C                                 normal at the specified surface point
C                                 and the vector from the surface point
C                                 to the viewing location.
C
C        phase angle              This is the angle between the vectors
C                                 from the surface point to the viewing
C                                 location and from the surface point to
C                                 the Sun.
C
C
C     The diagram below attempts to illustrate the geometrical
C     relationships defining these angles.  The labels for the
C     solar incidence, emission, and phase angles are `s.i.',
C     `e.', and `phase'.
C
C
C                                                      *
C                                                     Sun
C
C                    surface normal vector
C                              ._                 _.
C                              |\                 /|  Sun vector
C                                \    phase      /
C                                 \   .    .    /
C                                 .            .
C                                   \   ___   /
C                              .     \/     \/
C                                    _\ s.i./
C                             .    /   \   /
C                             .   |  e. \ /
C         *             <--------------- *  surface point on
C      viewing            vector            target body
C      location           to viewing
C                         location
C
C
C
C     All of the above angles can be computed using light time
C     corrections, light time and stellar aberration corrections, or
C     no aberration corrections.  The way that aberration corrections
C     are used is described below.
C
C     Care must be used in computing light time corrections.  The
C     guiding principle used here is `describe what appears in
C     an image'.  We ignore differential light time; the light times
C     from all points on the target to the observer are presumed to be
C     equal.
C
C
C        Observer-target body vector
C        ---------------------------
C
C        Let ET be the epoch at which an observation or remote
C        sensing measurement is made, and let ET - LT (`LT' stands
C        for `light time') be the epoch at which the photons received
C        at ET were emitted from the body (we use the term `emitted'
C        loosely here).
C
C        The correct observer-target vector points from the observer's
C        location at ET to the target body's location at ET - LT.
C        The target-observer vector points in the opposite direction.
C
C        Since light time corrections are not symmetric, the correct
C        target-observer vector CANNOT be found by computing the light
C        time corrected position of the observer as seen from the
C        target body.
C
C
C        Target body's orientation
C        -------------------------
C
C        Using the definitions of ET and LT above, the target
C        body's orientation at ET - LT is used.  The surface
C        normal is dependent on the target body's orientation, so
C        the body's orientation model must be evaluated for the correct
C        epoch.
C
C
C        Target body -- Sun vector
C        -------------------------
C
C        All surface features on the target body will appear in
C        a measurement made at ET as they were at ET-LT.  In
C        particular, lighting on the target body is dependent on
C        the apparent location of the Sun as seen from the target
C        body at ET-LT.  So, a second light time correction is used
C        in finding the apparent location of the Sun.
C
C
C     Stellar aberration corrections, when used, are applied as follows:
C
C
C        Observer-target body vector
C        ---------------------------
C
C        In addition to light time correction, stellar aberration is
C        used in computing the apparent target body position as seen
C        from the observer's location at time ET.  This apparent
C        position defines the observer-target body vector.
C
C
C        Target body-Sun vector
C        ----------------------
C
C        The target body-Sun vector is the apparent position of the Sun,
C        corrected for light time and stellar aberration, as seen from
C        the target body at time ET-LT.  Note that the target body's
C        position is not affected by the stellar aberration correction
C        applied in finding its apparent position as seen by the
C        observer.
C
C
C     Once all of the vectors, as well as the target body's
C     orientation, have been computed with the proper aberration
C     corrections, the element of time is eliminated from the
C     computation.  The problem becomes are purely geometrical one,
C     and is described by the diagram above.
C
C
C$ Examples
C
C     1)  Find the phase, solar incidence, and emission angles at the
C         sub-solar and sub-spacecraft points on Earth as seen from the
C         Galileo orbiter at a user-specified UTC time.  Use light time
C         and stellar aberration corrections.
C
C                  PROGRAM ANGLES
C
C            C
C            C     SPICELIB functions
C            C
C                  DOUBLE PRECISION      DPR
C
C            C
C            C     Local parameters
C            C
C                  INTEGER               FILEN
C                  PARAMETER           ( FILEN = 128 )
C
C            C
C            C     Local variables
C            C
C                  CHARACTER*(30)        UTC
C                  CHARACTER*(FILEN)     SPK
C                  CHARACTER*(FILEN)     PCK
C                  CHARACTER*(FILEN)     LEAP
C
C                  DOUBLE PRECISION      ALT
C                  DOUBLE PRECISION      ET
C                  DOUBLE PRECISION      SSCPHS
C                  DOUBLE PRECISION      SSCEMI
C                  DOUBLE PRECISION      SSCSOL
C                  DOUBLE PRECISION      SSLPHS
C                  DOUBLE PRECISION      SSLEMI
C                  DOUBLE PRECISION      SSLSOL
C                  DOUBLE PRECISION      SSOLPT ( 3 )
C                  DOUBLE PRECISION      SSCPT  ( 3 )
C
C                  INTEGER               HANDLE
C                  INTEGER               TARGET
C                  INTEGER               OBSRVR
C
C                  LOGICAL               CONT
C
C            C
C            C     Set the error handling so that the program doesn't
C            C     abort if an error is encountered.
C            C
C                  CALL ERRACT ( 'SET', 'RETURN' )
C
C            C
C            C     Ask the user for names of SPK, PCK, and leapseconds
C            C     kernels.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Enter name of SPK kernel'
C                  READ  (*,FMT='(A)') SPK
C
C                  WRITE (*,*) 'Enter name of PCK kernel'
C                  READ  (*,FMT='(A)') PCK
C
C                  WRITE (*,*) 'Enter name of leapseconds kernel'
C                  READ  (*,FMT='(A)') LEAP
C
C                  WRITE (*,*) ' '
C
C                  CALL SPKLEF ( SPK, HANDLE )
C                  WRITE (*,*) 'SPK kernel loaded.'
C
C                  CALL LDPOOL ( PCK )
C                  WRITE (*,*) 'PCK kernel loaded.'
C
C                  CALL LDPOOL ( LEAP )
C                  WRITE (*,*) 'Leapseconds kernel loaded.'
C
C                  CONT = .TRUE.
C
C                  DO WHILE ( CONT )
C
C                     WRITE (*,*) ' '
C
C                     WRITE (*,*) 'Enter UTC epoch of observation.'
C                     READ  (*,FMT='(A)') UTC
C
C            C
C            C        Convert our UTC time to ephemeris seconds past
C            C        J2000.
C            C
C                     CALL UTC2ET ( UTC, ET )
C
C            C
C            C        Assign observer and target ID's.  The NAIF integer
C            C        code for the Earth is 399.  For Galileo, it is
C            C        -77.  (The SPK required reading file contains the
C            C        complete list of codes).
C            C
C                     TARGET = 399
C                     OBSRVR = -77
C
C            C
C            C        Find the sub-solar point on the Earth as seen from
C            C        the Galileo orbiter at ET.
C            C
C                     CALL SUBSOL_G ( TARGET, OBSRVR, ET, 'LT+S',
C                 .                   SSOLPT                     )
C
C            C
C            C        Now find the sub-spacecraft point.
C            C
C                     CALL SUBPT_G  ( TARGET, OBSRVR, ET, 'LT+S',
C                 .                   SSCPT,  ALT                )
C
C            C
C            C        Find the phase, solar incidence, and emission
C            C        angles at the sub-solar point on the Earth as seen
C            C        from the Galileo orbiter at time ET.
C            C
C                     CALL ILLUM_G  ( TARGET, OBSRVR, ET, 'LT+S',
C                 .                   SSOLPT,
C                 .                   SSLPHS, SSLSOL, SSLEMI       )
C
C            C
C            C        Do the same for the sub-spacecraft point.
C            C
C                     CALL ILLUM_G  ( TARGET, OBSRVR, ET, 'LT+S',
C                 .                   SSCPT,
C                 .                   SSCPHS, SSCSOL, SSCEMI      )
C
C            C
C            C        Convert the angles to degrees and write them out.
C            C
C                     SSLPHS = DPR() * SSLPHS
C                     SSLSOL = DPR() * SSLSOL
C                     SSLEMI = DPR() * SSLEMI
C
C                     SSCPHS = DPR() * SSCPHS
C                     SSCSOL = DPR() * SSCSOL
C                     SSCEMI = DPR() * SSCEMI
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'UTC epoch is ', UTC
C                     WRITE (*,*) ' '
C                     WRITE (*,*)
C                 .   'Illumination angles at the sub-solar point:'
C                     WRITE (*,*) ' '
C                     WRITE (*,*)
C                 .   'Phase angle           (deg.): ', SSLPHS
C                     WRITE (*,*)
C                 .   'Solar incidence angle (deg.): ', SSLSOL
C                     WRITE (*,*)
C                 .   'Emission angle        (deg.): ', SSLEMI
C                     WRITE (*,*) ' '
C                     WRITE (*,*)
C                 .   'The solar incidence angle should be 0.'
C                     WRITE (*,*)
C                 .   'The emission and phase angles should be equal.'
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*)
C                 .   'Illumination angles at the sub-s/c point:'
C                     WRITE (*,*) ' '
C                     WRITE (*,*)
C                 .   'Phase angle           (deg.): ', SSCPHS
C                     WRITE (*,*)
C                 .   'Solar incidence angle (deg.): ', SSCSOL
C                     WRITE (*,*)
C                 .   'Emission angle        (deg.): ', SSCEMI
C                     WRITE (*,*) ' '
C                     WRITE (*,*)
C                 .   'The emission angle should be 0.'
C                     WRITE (*,*)
C                 .   'The solar incidence and phase angles should '//
C                 .   'be equal.'
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'Do you wish to continue (T/F)?'
C                     READ   *,    CONT
C
C            C
C            C        Reset the error status to 'no error'.
C            C
C                     CALL RESET
C
C                  END DO
C
C                  END
C
C$ Restrictions
C
C     1)  An SPK file containing ephemeris data for Sun and the observer
C         and target bodies and a PCK file containing and shape and
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
C     illumination angles
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
      INTEGER               SUN
      PARAMETER           ( SUN    =  10 )
 
C
C     Local variables
C
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTS
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OBSVEC ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SSTATE ( 6 )
      DOUBLE PRECISION      SUNVEC ( 3 )
      DOUBLE PRECISION      TEPOCH
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TSTATE ( 6 )
 
      INTEGER               N
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ILLUM_G' )
      END IF
 
C
C     The observer and target must be distinct.
C
      IF ( TARGET .EQ. OBSRVR ) THEN
 
         CALL SETMSG ( 'Target is #; observer is #.' )
         CALL ERRINT ( '#', TARGET                   )
         CALL ERRINT ( '#', OBSRVR                   )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'    )
         CALL CHKOUT ( 'ILLUM_G'                     )
         RETURN
 
      END IF
 
C
C     Find the state of the target as seen from the observer at ET.
C
      CALL SPKEZ ( TARGET, ET, 'J2000', CORR, OBSRVR, TSTATE, LT )
 
C
C     Determine the epoch to be used in computing the orientation of
C     the target and the target-Sun vector.
C
      IF ( CORR .EQ. 'NONE' ) THEN
         TEPOCH = ET
      ELSE
         TEPOCH = ET - LT
      END IF
 
C
C     Find the state of the Sun as seen from the target at TEPOCH.
C     Also obtain the J2000-to-body equator and prime meridian
C     transformation for this epoch.
C
      CALL SPKEZ  ( SUN,    TEPOCH, 'J2000', CORR, TARGET, SSTATE, LTS )
 
      CALL BODMAT ( TARGET, TEPOCH, TIPM )
 
C
C     Grab the position portions of the states (the first three
C     elements of each state).  Negate the observer-target vector,
C     since the vector required for the illumination angle
C     computation is the target-observer vector.  The vectors we've
C     found point from the target body center to the observer and
C     Sun, and already take light time corrections into account.
C
      CALL VMINUS ( TSTATE, OBSVEC )
      CALL VEQU   ( SSTATE, SUNVEC )
 
C
C     We need the state vectors in body-fixed coordinates.
C
      CALL MXV ( TIPM, OBSVEC, OBSVEC )
      CALL MXV ( TIPM, SUNVEC, SUNVEC )
 
C
C     Now we'll modify target-observer and target-Sun vectors to
C     take into account the offset between the target center and the
C     surface point of interest; we want the vectors to point from
C     the surface point to the observer and Sun respectively.
 
      CALL VSUB ( OBSVEC, SPOINT, OBSVEC )
      CALL VSUB ( SUNVEC, SPOINT, SUNVEC )
 
C
C     Find the surface normal at SPOINT.  We'll need the radii of the
C     target body.
C
      CALL BODVAR ( TARGET,  'RADII',  N,  RADII )
 
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
 
C
C     Find the illumination angles.  VSEP will give us angular
C     separation in radians.
C
      PHASE   =  VSEP ( SUNVEC, OBSVEC )
      SOLAR   =  VSEP ( NORMAL, SUNVEC )
      EMISSN  =  VSEP ( NORMAL, OBSVEC )
 
      CALL CHKOUT ( 'ILLUM_G' )
      RETURN
      END
