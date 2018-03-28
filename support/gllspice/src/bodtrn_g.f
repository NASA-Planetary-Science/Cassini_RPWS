C$Procedure BODTRN_G ( Body name and code translation )
 
      SUBROUTINE BODTRN_G ( NAME, CODE, FILE, FOUND )
 
C$ Abstract
C
C     This is the umbrella routine that contains entry points for
C     translating between body names and NAIF integer codes and
C     for defining new name/code pairs.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     BODY
C
C$ Declarations
 
      CHARACTER*(*)         NAME
      INTEGER               CODE
      CHARACTER*(*)         FILE
      LOGICAL               FOUND
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE =  80 )
 
      INTEGER               MAXL
      PARAMETER           ( MAXL   =  32 )
 
      INTEGER               MAXP
      PARAMETER           ( MAXP   = 100 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   BODN2C_G and BODDEF_G
C                O   BODC2N_G
C     CODE       I   BODC2N_G and BODDEF_G
C                O   BODN2C_G
C     FILE       I   BODFIL_G
C     FOUND      O   BODN2C_G and BODC2N_G
C     LNSIZE     P   BODFIL_G
C     MAXL       P   (All)
C     MAXP       P   BODDEF_G and BODFIL_G
C
C$ Detailed_Input
C
C     See the entry points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the entry points for a discussion of their arguments.
C
C$ Parameters
C
C     LNSIZE      is the length of a line of data read from FILE.
C                 A line of data contains a name and a code, separated
C                 by an equal sign.  Thus LNSIZE must be large
C                 enough to accommodate the longest name and code
C                 pair in the file.
C
C     MAXL        is the maximum length of a name and the maximum
C                 length of an integer code when that integer is
C                 represented as a character string.  The user may
C                 only increase the value of MAXL, and only that
C                 if necessary.  If MAXL is decreased, the
C                 default names and codes may be truncated.
C
C     MAXP        is the maximum number of name/code pairs that can
C                 be defined via BODDEF_G or BODFIL_G.  It is the limit
C                 on the number of definitions over and above the
C                 number of default definitions.  The user may alter
C                 the the value of MAXP, however, it must remain a
C                 positive integer.
C
C$ Exceptions
C
C     1) If BODTRN_G is called directly, the error SPICE(BOGUSENTRY) is
C        signalled.
C
C$ Files
C
C     See the BODFIL_G entry point header.
C
C$ Particulars
C
C     BODTRN_G should never be called directly, but should instead be
C     accessed through its entry points, which are the following:
C
C        BODN2C_G      Body name to code
C
C        BODC2N_G      Body code to name
C
C        BODDEF_G      Body name/code definition
C
C        BODFIL_G      Body name/code definitions from a file
C
C     BODN2C_G and BODC2N_G perform translations between body names
C     and their corresponding integer codes which are used
C     in SPK and PCK files and routines.  A set of name/code
C     pairs are automatically defined during the first call to
C     one of these entry points.  Additional name/code pairs may
C     be defined via BODDEF_G or BODFIL_G for two purposes:
C
C        1.  to associate another, perhaps more familiar or
C            abbreviated, name with a particular body integer
C            code that has already been defined, or
C
C        2.  to define a new body integer code and name,
C
C     Each body has a unique integer code, but may have several
C     names.  Thus you may associate more than one name with
C     a particular integer code.  However, associating more
C     than one integer code with a particular name creates ambiguity.
C     Therefore, once a name has been defined, it may not be redefined
C     with a different integer code.
C
C     For example, Europa is the name of the second satellite of
C     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502)
C     is one of the default definitions.  Europa is also the name
C     of an asteroid.  Suppose you were able to associate the asteroid
C     integer code with the name EUROPA.  Then when you call BODN2C_G to
C     translate the name EUROPA, which code should be returned?  That
C     of the asteroid or 502?
C
C     BODDEF_G and BODFIL_G prevent this ambiguity by signalling an
C     error if the specified name has already been defined with a
C     different code.  In the case of EUROPA, you may want to use the
C     name ASTEROID EUROPA.  The set of default definitions are listed
C     in DATA statements in the umbrella routine BODTRN_G for easy
C     reference.
C
C$ Examples
C
C     1.  In the following code fragment, SPKEZ computes the state
C     (position and velocity) of Jupiter as seen from the Galileo
C     Orbiter.  It requires the NAIF integer codes of the target and
C     observer, so we use BODN2C_G to convert names to integer codes
C     for those bodies.
C
C       CALL BODN2C_G ( 'JUPITER',         TARGET, FOUND )
C
C       CALL BODN2C_G ( 'GALILEO ORBITER', OBSRVR, FOUND )
C
C       CALL SPKEZ  ( TARGET, EPOCH, FRAME, ABCORR, OBSRVR, STATE, LT )
C
C
C     2.  In this example, we assume that neither BODDEF_G nor BODFIL_G
C     has been called.  Thus, only the set of default name/code pairs
C     has been defined.
C
C     Given these names, BODN2C_G will return the following codes:
C
C        Name                         Code    Found?
C        ------------------------   ------    ------
C        'EARTH'                       399    Yes
C        '  Earth '                    399    Yes
C        'EMB'                           3    Yes
C        'Solar System Barycenter'       0    Yes
C        'SolarSystemBarycenter'         -    No
C        'SSB'                           0    Yes
C        'Voyager 2'                   -32    Yes
C        'U.S.S. Enterprise'             -    No
C        ' '                             -    No
C        'Halley's Comet'                -    No
C
C
C     and, given these codes, BODC2N_G will return the following names:
C
C        Code        Name                        Found?
C        -------     -------------------         ------
C        399         'EARTH'                     Yes
C          0         'SOLAR SYSTEM BARYCENTER'   Yes
C          3         'EARTH BARYCENTER'          Yes
C        -77         'GALILEO ORBITER'           Yes
C         11          -                          No
C         -1          -                          No
C
C
C     3.  This example shows how to define a name/code pair.
C     You may associate a new name with a particular code that
C     has already been defined:
C
C            CALL BODDEF_G ( 'JB', 5 )
C
C     You may also define the name and integer code for a new body:
C
C            CALL BODDEF_G ( 'Asteroid Frank', 20103456 )
C
C     After these calls to BODDEF_G, BODN2C_G would return the following
C     translations:
C
C        Name                         Code    Found?
C        ------------------------   ------    ------
C        'JB'                            5    Yes
C        'Jupiter Barycenter'            5    Yes
C        'ASTEROID FRANK'         20103456    Yes
C        'ASTEROIDFRANK'                 -    No
C        'Frank'                         -    No
C
C     and BODC2N_G will return these translations:
C
C        Code        Name                     Found?
C        -------     -------------------      ------
C               5    'JB'                     Yes
C        20103456    'Asteroid Frank'         Yes
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
C     J.E. McLean    (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    GLLSPICE Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C       Some items that were previously considered errors were removed
C       and some minor modifications were made to improve the
C       robustness of the routines.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C-&
 
C$ Index_Entries
C
C     body name and code translation
C
C-&
 
 
C$ Revisions
C
C-    GLLSPICE Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C       Some items that were previously considered errors were removed
C       and some minor modifications were made to improve the
C       robustness of the routines.
C
C-&
 
 
C
C
C     Data Structure and Design Information
C     -----------------------------------------------------
C
C     We use two symbol tables to store the name/code pairs:
C     a NAM table and a COD table.  During the first call to this
C     module we initialize the symbol tables and load the default
C     set of definitions.
C
C     The NAM table contains in its symbol component (NAMSYM) all of the
C     body names that have been defined.  The values for these name
C     symbols (stored in NAMVAL) are the integer codes associated
C     with the names.  Before it is stored in the NAM table, the string
C     that contains the name is converted to upper case, left justified,
C     and consecutive embedded blanks are compressed to a single blank.
C     As a result, case and extra blanks are insignificant when
C     searching for a particular name in the table.
C
C     The COD table contains in its symbol component (CODSYM) all of the
C     codes that have been defined.  These codes are stored in their
C     character string format because in the definition of a symbol
C     table, symbols must be character strings.  The values for these
C     code symbols (stored in CODVAL) are the names associated with
C     the codes.  When the name is stored, it is NOT converted to
C     upper case, nor are blanks removed.  Thus, the input format
C     is preserved.
C
C     When a name/code pair is defined, the name becomes a symbol
C     in the NAM table with the code as its value and the code
C     becomes a symbol in the COD table with the name as its value.
C     If the specified name is already a symbol in the table, an error
C     is signalled.  The specified code may already be in the table
C     in which case, we just add a new value for it.  SYSETx creates
C     a new symbol, or adds a value if that symbol exists.  It also
C     signals an error if we try to create a symbol once the table
C     is full.  When a symbol is translated, the value that was set
C     most recently is returned.
C
C
C     For example,
C
C         CALL BODDEF_G ( ' home ', 399 )
C
C     creates the following entries in the symbol tables:
C
C
C              NAM table                        COD table
C          -----------------               --------------------
C
C          'HOME' ----> 399                '399'  ----> ' home '
C
C
C     Then
C
C         CALL BODDEF_G ( 'Planet  Earth', 399 )
C
C     affects the symbol tables as follows:
C
C
C              NAM table                        COD table
C          -----------------               --------------------
C
C          'HOME'         ----> 399        '399'  ----> 'Planet  Earth'
C          'PLANET EARTH' ----> 399
C
C
C     The translation for 399 is now 'Planet Earth' instead of
C     ' home ', because the second call to BODDEF_G superseded the
C     definition of 399 in the COD table.  Both 'PLANET EARTH'
C     and 'HOME' will translate to 399.
C
C     Case and leading and trailing blanks in a name are not
C     significant.  However one blank, when embedded in the name,
C     is significant.  That is, all of the following strings
C     are equivalent names and translate to 399:
C
C              'Planet Earth'
C              'PLANET EARTH'
C              'PLANET EARTH   '
C              'PLANET    EARTH'
C              '   PLANET EARTH'
C
C     There must be at least one blank between the first and
C     the second word.  That is, the string 'PLANETEARTH' is
C     not equivalent to the names above and has no translation.
C
C
C     If a name or character string representation of an integer
C     code is longer than MAXL, an error is signalled.  The
C     other option was to just truncate the string, but truncation
C     may potentially cause some undesirable effects.  For example,
C     if a long code is truncated, it might mistakenly match a
C     shorter code.
C
 
 
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTPC
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL =    -5 )
 
C
C     The parameters here are for ease in maintaining the
C     large collection of automatic names that are stored
C     in data statements.  To insert a name/code pair in the
C     block from BEGx to ENDx, redefine ENDx to be
C     one larger than its current definition.  Recompiling
C     will automatically modify all the other parameters.
C
      INTEGER               BEG1
      PARAMETER           ( BEG1 = 1 )
      INTEGER               END1
      PARAMETER           ( END1 = BEG1 + 9 )
 
      INTEGER               BEG2
      PARAMETER           ( BEG2 = END1 + 1 )
      INTEGER               END2
      PARAMETER           ( END2 = BEG2 + 9 )
 
      INTEGER               BEG3
      PARAMETER           ( BEG3 = END2 + 1 )
      INTEGER               END3
      PARAMETER           ( END3 = BEG3 + 9 )
 
      INTEGER               BEG4
      PARAMETER           ( BEG4 = END3 + 1 )
      INTEGER               END4
      PARAMETER           ( END4 = BEG4 + 9 )
 
      INTEGER               BEG5
      PARAMETER           ( BEG5 = END4 + 1 )
      INTEGER               END5
      PARAMETER           ( END5 = BEG5 + 9 )
 
      INTEGER               BEG6
      PARAMETER           ( BEG6 = END5 + 1 )
      INTEGER               END6
      PARAMETER           ( END6 = BEG6 + 9 )
 
      INTEGER               BEG7
      PARAMETER           ( BEG7 = END6 + 1 )
      INTEGER               END7
      PARAMETER           ( END7 = BEG7 + 9 )
 
      INTEGER               BEG8
      PARAMETER           ( BEG8 = END7 + 1 )
      INTEGER               END8
      PARAMETER           ( END8 = BEG8 + 9 )
 
      INTEGER               BEG9
      PARAMETER           ( BEG9 = END8 + 1 )
      INTEGER               END9
      PARAMETER           ( END9 = BEG9 + 9 )
 
      INTEGER               BEG10
      PARAMETER           ( BEG10 = END9  + 1 )
      INTEGER               END10
      PARAMETER           ( END10 = BEG10 + 9 )
 
      INTEGER               BEG11
      PARAMETER           ( BEG11 = END10 + 1 )
      INTEGER               END11
      PARAMETER           ( END11 = BEG11 + 9 )
 
 
      INTEGER               BEG12
      PARAMETER           ( BEG12 = END11 + 1 )
      INTEGER               END12
      PARAMETER           ( END12 = BEG12 + 9 )
 
      INTEGER               BEG13
      PARAMETER           ( BEG13 = END12 + 1 )
      INTEGER               END13
      PARAMETER           ( END13 = BEG13 + 7 )
 
      INTEGER               NPERM
      PARAMETER           ( NPERM = END13 )
 
      INTEGER               MAXE
      PARAMETER           ( MAXE   = MAXP + NPERM )
 
C
C     Local variables
C
 
      CHARACTER*(MAXL)      CODSYM  ( LBCELL:MAXE )
      INTEGER               CODPTR  ( LBCELL:MAXE )
      CHARACTER*(MAXL)      CODVAL  ( LBCELL:MAXE )
 
      CHARACTER*(MAXL)      NAMSYM  ( LBCELL:MAXE )
      INTEGER               NAMPTR  ( LBCELL:MAXE )
      INTEGER               NAMVAL  ( LBCELL:MAXE )
 
 
      CHARACTER*(LNSIZE)    ERROR
      CHARACTER*(LNSIZE)    ITEMS      (     2 )
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(MAXL)      NAMES      ( NPERM )
      CHARACTER*(MAXL)      SYMBOL
      CHARACTER*(LNSIZE)    TCODEC
      CHARACTER*(LNSIZE)    TNAME
 
      INTEGER               CODES      ( NPERM )
      INTEGER               I
      INTEGER               LENGTH
      INTEGER               N
      INTEGER               PNTER
      INTEGER               TCODEI
 
      LOGICAL               EOF
      LOGICAL               FIRST
      LOGICAL               TFOUND
 
C
C     Saved variables
C
 
      SAVE                  FIRST
 
      SAVE                  CODSYM
      SAVE                  CODPTR
      SAVE                  CODVAL
 
      SAVE                  NAMSYM
      SAVE                  NAMPTR
      SAVE                  NAMVAL
 
 
C
C     Initial values
C
 
      DATA                  FIRST   / .TRUE. /
 
C
C     Introducing the permanent collection.
C
      DATA ( CODES(I), NAMES(I), I =  BEG1, END1 )
     .     /
     .     199,    'MERCURY',
     .     299,    'VENUS',
     .     399,    'EARTH',
     .     499,    'MARS',
     .     599,    'JUPITER',
     .     699,    'SATURN',
     .     799,    'URANUS',
     .     899,    'NEPTUNE',
     .     999,    'PLUTO',
     .     301,    'MOON'
     .     /
 
 
      DATA ( CODES(I), NAMES(I), I = BEG2, END2 )
     .     /
     .     401,   'PHOBOS',
     .     402,   'DEIMOS',
     .     501,   'IO',
     .     502,   'EUROPA',
     .     503,   'GANYMEDE',
     .     504,   'CALLISTO',
     .     505,   'AMALTHEA',
     .     506,   'HIMALIA',
     .     507,   'ELARA',
     .     508,   'PASIPHAE'
     .     /
 
      DATA ( CODES(I), NAMES(I), I =  BEG3, END3 )
     .     /
     .     509,   'SINOPE',
     .     510,   'LYSITHEA',
     .     511,   'CARME',
     .     512,   'ANANKE',
     .     513,   'LEDA',
     .     514,   '1979J2',
     .     514,   'THEBE',
     .     515,   '1979J1',
     .     515,   'ADRASTEA',
     .     516,   '1979J3'
     .     /
 
      DATA ( CODES(I), NAMES(I), I =  BEG4, END4 )
     .     /
     .     516,   'METIS',
     .     601,   'MIMAS',
     .     602,   'ENCELADUS',
     .     603,   'TETHYS',
     .     604,   'DIONE',
     .     605,   'RHEA',
     .     606,   'TITAN',
     .     607,   'HYPERION',
     .     608,   'IAPETUS',
     .     609,   'PHOEBE'
     .     /
 
      DATA ( CODES(I), NAMES(I), I =  BEG5, END5 )
     .     /
     .     610,   '1980S1',
     .     610,   'JANUS',
     .     611,   '1980S3',
     .     611,   'EPIMETHEUS',
     .     612,   '1980S6',
     .     612,   'HELENE',
     .     613,   '1980S13',
     .     613,   'TELESTO',
     .     614,   '1980S25',
     .     614,   'CALYPSO'
     .     /
 
 
      DATA ( CODES(I), NAMES(I), I =  BEG6, END6 )
     .     /
     .     615,   '1980S28',
     .     615,   'ATLAS',
     .     616,   '1980S27',
     .     616,   'PROMETHEUS',
     .     617,   '1980S26',
     .     617,   'PANDORA',
     .     701,   'ARIEL',
     .     702,   'UMBRIEL',
     .     703,   'TITANIA',
     .     704,   'OBERON'
     .     /
 
      DATA ( CODES(I), NAMES(I), I =  BEG7, END7 )
     .     /
     .     705,   'MIRANDA',
     .     706,   '1986U7',
     .     706,   'CORDELIA',
     .     707,   '1986U8',
     .     707,   'OPHELIA',
     .     708,   '1986U9',
     .     708,   'BIANCA',
     .     709,   '1986U4',
     .     709,   'CRESSIDA',
     .     710,   '1986U6'
     .     /
 
      DATA ( CODES(I), NAMES(I), I =  BEG8, END8 )
     .     /
     .     710,   'DESDEMONA',
     .     711,   '1986U3',
     .     711,   'JULIET',
     .     712,   '1986U1',
     .     712,   'PORTIA',
     .     713,   '1986U2',
     .     713,   'ROSALIND',
     .     714,   '1986U5',
     .     714,   'BELINDA',
     .     715,   '1985U1'
     .     /
 
      DATA ( CODES(I), NAMES(I), I =  BEG9, END9 )
     .     /
     .     715,   'PUCK',
     .     801,   'TRITON',
     .     802,   'NEREID',
     .     901,   '1978P1',
     .     901,   'CHARON',
     .     -12,   'VENUS ORBITER',
     .     -12,   'P12',
     .     -12,   'PIONEER 12',
     .     -18,   'MGN',
     .     -18,   'MAGELLAN'
     .     /
 
      DATA ( CODES(I), NAMES(I), I =  BEG10, END10 )
     .     /
     .     -27,   'VK1',
     .     -27,   'VIKING 1 ORBITER',
     .     -30,   'VK2',
     .     -30,   'VIKING 2 ORBITER',
     .     -31,   'VG1',
     .     -31,   'VOYAGER 1',
     .     -32,   'VG2',
     .     -32,   'VOYAGER 2',
     .     -46,   'MS-T5',
     .     -46,   'SAKIGAKE'
     .     /
 
      DATA ( CODES(I), NAMES(I), I = BEG11, END11 )
     .     /
     .     -47,   'PLANET-A',
     .     -47,   'SUISEI',
     .     -58,   'PHOBOS 2',
     .     -66,   'VEGA 1',
     .     -67,   'VEGA 2',
     .     -77,   'GLL',
     .     -77,   'GALILEO ORBITER',
     .     -78,   'GIOTTO',
     .     -94,   'MO',
     .     -94,   'MARS OBSERVER'
     .     /
 
      DATA ( CODES(I), NAMES(I), I = BEG12, END12 )
     .     /
     .    -112,   'ICE',
     .       0,   'SSB',
     .       0,   'SOLAR SYSTEM BARYCENTER',
     .       1,   'MERCURY BARYCENTER',
     .       2,   'VENUS BARYCENTER',
     .       3,   'EMB',
     .       3,   'EARTH MOON BARYCENTER',
     .       3,   'EARTH-MOON BARYCENTER',
     .       3,   'EARTH BARYCENTER',
     .       4,   'MARS BARYCENTER'
     .     /
 
      DATA ( CODES(I), NAMES(I), I = BEG13, END13 )
     .     /
     .       5,   'JUPITER BARYCENTER',
     .       6,   'SATURN BARYCENTER',
     .       7,   'URANUS BARYCENTER',
     .       8,   'NEPTUNE BARYCENTER',
     .       9,   'PLUTO BARYCENTER',
     .      10,   'SUN',
     . 9511010,   'GASPRA',
     . 2431010,   'IDA'
     .     /
 
C
C     The 851, 852, ... codes are temporary codes for the newly-
C     discovered satellites of Neptune.  These will go away when
C     the official codes are assigned.  The codes listed above
C     do not include these temporary assignments.
C
C     The proposed names are the following:
C
C        1989N1 = Proteus
C        1989N2 = Larissa
C        1989N3 = Despina
C        1989N4 = Galatea
C        1989N5 = Thalassa
C        1989N6 = Naiad
C
 
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BODTRN_G' )
      END IF
 
C
C     This routine should never be called. If it is called,
C     an error is signalled.
C
      CALL SETMSG ( 'BODTRN_G: You have called an entry which '//
     .              'performs no run-time function. This '     //
     .              'may indicate a bug. Please check the '    //
     .              'documentation for the subroutine BODTRN_G.' )
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'BODTRN_G' )
      RETURN
 
 
 
 
C$Procedure BODN2C_G ( Body name to code )
 
      ENTRY BODN2C_G ( NAME, CODE, FOUND )
 
C$ Abstract
C
C     Translate the name of a body into the integer code for
C     that body.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     BODY
C     CONVERSION
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               CODE
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Body name to be translated.
C     CODE       O   Integer code for that body.
C     FOUND      O   True if translated, otherwise false.
C     MAXL       P   Max name length and max number of digits in code.
C
C$ Detailed_Input
C
C     NAME        is an arbitrary name of a body which could be
C                 a planet, satellite, barycenter, spacecraft,
C                 asteroid, comet, or other ephemeris object.
C
C                 Case and leading and trailing blanks in a name
C                 are not significant.  However when a name is made
C                 up of more than one word, they must be separated by
C                 at least one blank.  That is, all of the following
C                 strings are equivalent names:
C
C                         'JUPITER BARYCENTER'
C                         'Jupiter Barycenter'
C                         'JUPITER BARYCENTER   '
C                         'JUPITER    BARYCENTER'
C                         '   JUPITER BARYCENTER'
C
C                 However, 'JUPITERBARYCENTER' is not equivalent to
C                 the names above.
C
C                 When ignoring trailing blanks, NAME must have fewer
C                 than MAXL characters.
C
C$ Detailed_Output
C
C     CODE        is the NAIF or user-defined integer code for the
C                 named body.  CODE will have at most MAXL digits
C                 including a minus sign if CODE is negative.
C
C     FOUND       is true if NAME has a translation.  Otherwise, FOUND
C                 is false.
C
C$ Parameters
C
C     MAXL        is the maximum length of a name and the maximum
C                 length of an integer code when that integer is
C                 represented as a character string.  The user may
C                 only increase the value of MAXL, and only that
C                 if necessary.  If MAXL is decreased, the
C                 default names and codes may be truncated.
C
C$ Exceptions
C
C     1) If FOUND is false, the value of CODE is not changed.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     BODN2C_G is one of four related entry points,
C
C        BODN2C_G      Body name to code
C
C        BODC2N_G      Body code to name
C
C        BODDEF_G      Body name/code definition
C
C        BODFIL_G      Body name/code definitions from a file
C
C     BODN2C_G and BODC2N_G perform translations between body names
C     and their corresponding integer codes which are used
C     in SPK and PCK files and routines.  A set of name/code
C     pairs are automatically defined during the first call to
C     one of these entry points.  Additional name/code pairs may
C     be defined via BODDEF_G or BODFIL_G.
C
C$ Examples
C
C     1.  In the following code fragment, SPKEZ computes the state
C     (position and velocity) of Jupiter as seen from the Galileo
C     Orbiter.  It requires the NAIF integer codes of the target and
C     observer, so we use BODN2C_G to convert names to integer codes
C     for those bodies.
C
C       CALL BODN2C_G ( 'JUPITER',         TARGET, FOUND )
C
C       CALL BODN2C_G ( 'GALILEO ORBITER', OBSRVR, FOUND )
C
C       CALL SPKEZ  ( TARGET, EPOCH, FRAME, ABCORR, OBSRVR, STATE, LT )
C
C
C     2.  In this example, we assume that neither BODDEF_G nor BODFIL_G
C     has been called.  Thus, only the set of default name/code pairs
C     has been defined.
C
C     Given these names, BODN2C_G will return the following codes:
C
C        Name                         Code    Found?
C        ------------------------   ------    ------
C        'EARTH'                       399    Yes
C        '  Earth '                    399    Yes
C        'EMB'                           3    Yes
C        'Solar System Barycenter'       0    Yes
C        'SolarSystemBarycenter'         -    No
C        'SSB'                           0    Yes
C        'Voyager 2'                   -32    Yes
C        'U.S.S. Enterprise'             -    No
C        ' '                             -    No
C        'Halley's Comet'                -    No
C
C
C     and, given these codes, BODC2N_G will return the following names:
C
C        Code        Name                        Found?
C        -------     -------------------         ------
C        399         'EARTH'                     Yes
C          0         'SOLAR SYSTEM BARYCENTER'   Yes
C          3         'EARTH BARYCENTER'          Yes
C        -77         'GALILEO ORBITER'           Yes
C         11          -                          No
C         -1          -                          No
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
C     J.E. McLean    (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    GLLSPICE Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C       Items that were previously considered errors were downgraded
C       to simply be exceptions.  Any NAME is a legitimate input now.
C       If its not in the table, the FOUND flag is just set to .FALSE.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C
C-&
 
C$ Index_Entries
C
C     body name to code
C
C-&
 
 
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BODN2C_G' )
      END IF
 
 
C
C     During the first call to this module initialize the
C     two symbol tables and load the permanent collection of
C     name/code definitions.  This section of code is repeated
C     after each entry point.
C
      IF ( FIRST ) THEN
 
         CALL SSIZEC ( MAXE, CODSYM )
         CALL SSIZEI ( MAXE, CODPTR )
         CALL SSIZEC ( MAXE, CODVAL )
 
         CALL SSIZEC ( MAXE, NAMSYM )
         CALL SSIZEI ( MAXE, NAMPTR )
         CALL SSIZEI ( MAXE, NAMVAL )
 
         DO 50001
     .      I = 1, NPERM
 
            SYMBOL = NAMES(I)
            CALL LJUST  (         SYMBOL,   SYMBOL )
            CALL UCASE  (         SYMBOL,   SYMBOL )
            CALL CMPRSS ( ' ', 1, SYMBOL,   SYMBOL )
            CALL SYSETI ( SYMBOL, CODES(I), NAMSYM, NAMPTR, NAMVAL )
 
            CALL INTSTR ( CODES(I),         SYMBOL )
            CALL SYSETC ( SYMBOL, NAMES(I), CODSYM, CODPTR, CODVAL )
 
50001    CONTINUE
 
         FIRST = .FALSE.
 
      END IF
 
C
C     Translate the body name to the corresponding integer code:
C
      CALL CMPRSS ( ' ', 1, NAME,     TNAME  )
      CALL LJUST  (         TNAME,    SYMBOL )
      CALL UCASE  (         SYMBOL,   SYMBOL )
 
C
C     If the symbol is in the table, retrieve the first value
C     associated with it.  The first value is the code that was
C     most recently associated with NAME.
C
      CALL SYNTHI ( SYMBOL,
     .              1,
     .              NAMSYM, NAMPTR, NAMVAL,
     .              CODE,
     .              FOUND                   )
 
      CALL CHKOUT ( 'BODN2C_G' )
      RETURN
 
 
 
 
C$Procedure BODC2N_G ( Body code to name )
 
      ENTRY BODC2N_G ( CODE, NAME, FOUND )
 
C$ Abstract
C
C     Translate the integer code of a body into a common name for
C     that body.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     BODY
C     CONVERSION
C
C$ Declarations
C
C     INTEGER               CODE
C     CHARACTER*(*)         NAME
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CODE       I   Integer code to be translated.
C     NAME       O   Common name for the body identified by CODE.
C     FOUND      O   True if translated, otherwise false.
C     MAXL       P   Max name length and max number of digits in code.
C
C$ Detailed_Input
C
C     CODE        is an integer code for a body ---
C                 a planet, satellite, barycenter, spacecraft,
C                 asteroid, comet, or other ephemeris object.
C
C$ Detailed_Output
C
C     NAME        is the common name of the body identified by CODE.
C                 If CODE has more than one translation, then the
C                 most recently defined NAME corresponding to CODE
C                 is returned.  NAME will have the exact format (case
C                 and blanks) as when the name/code pair was defined.
C
C     FOUND       is true if CODE has a translation.  Otherwise, FOUND
C                 is false.
C
C$ Parameters
C
C     MAXL        is the maximum length of a name and the maximum
C                 length of an integer code when that integer is
C                 represented as a character string.  The user may
C                 only increase the value of MAXL, and only that
C                 if necessary.  If MAXL is decreased, the
C                 default names and codes may be truncated.
C
C$ Exceptions
C
C     1) If FOUND is false, the value of NAME is not changed.
C
C     2) If CODE has been defined with two different names, the
C        most recent translation will be returned.  This may occur
C        if there are two names for a particular body like
C        'SOLAR SYSTEM BARYCENTER' and 'SSB' for body 0.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     BODC2N_G is one of four related entry points,
C
C        BODN2C_G      Body name to code
C
C        BODC2N_G      Body code to name
C
C        BODDEF_G      Body name/code definition
C
C        BODFIL_G      Body name/code definitions from a file
C
C     BODN2C_G and BODC2N_G perform translations between body names
C     and their corresponding integer codes which are used
C     in SPK and PCK files and routines.  A set of name/code
C     pairs are automatically defined during the first call to
C     one of these entry points.  Additional name/code pairs may
C     be defined via BODDEF_G or BODFIL_G.
C
C$ Examples
C
C     1.  Suppose you ran the utility program SPACIT to summarize
C     an SPK ephemeris file and the following data was output
C     to the terminal screen.
C
C         ----------------------------------------------------------
C         Segment identifier: JPL archive 21354
C         Body        : -77                         Center     : 399
C         From        : 1990 DEC 08 18:00:00.000
C         To          : 1990 DEC 10 21:10:00.000
C         Reference   : DE-200                      SPK Type    :1
C         ----------------------------------------------------------
C
C     You could write a program to translate the body codes
C     shown in the SPACIT output:
C
C        CALL BODC2N_G ( -77, BODY,   FOUND )
C        CALL BODC2N_G ( 399, CENTER, FOUND )
C
C        IF ( FOUND ) THEN
C
C           WRITE ( *,* ) 'BODY:    -77 = ', BODY
C           WRITE ( *,* ) 'CENTER:  399 = ', CENTER
C
C        END IF
C
C     You could also read the body and center codes directly from
C     the SPK files, using the appropriate DAF routines, and then
C     translate them, as above.
C
C
C     2.  In this example, we assume that neither BODDEF_G nor BODFIL_G
C     has been called.  Thus, only the set of default name/code pairs
C     has been defined.
C
C     Given these names, BODN2C_G will return the following codes:
C
C        Name                         Code    Found?
C        ------------------------   ------    ------
C        'EARTH'                       399    Yes
C        '  Earth '                    399    Yes
C        'EMB'                           3    Yes
C        'Solar System Barycenter'       0    Yes
C        'SolarSystemBarycenter'         -    No
C        'SSB'                           0    Yes
C        'Voyager 2'                   -32    Yes
C        'U.S.S. Enterprise'             -    No
C        ' '                             -    No
C        'Halley's Comet'                -    No
C
C
C     and, given these codes, BODC2N_G will return the following names:
C
C        Code        Name                        Found?
C        -------     -------------------         ------
C        399         'EARTH'                     Yes
C          0         'SOLAR SYSTEM BARYCENTER'   Yes
C          3         'EARTH BARYCENTER'          Yes
C        -77         'GALILEO ORBITER'           Yes
C         11          -                          No
C         -1          -                          No
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
C     J.E. McLean    (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    GLLSPICE Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C       Checks to see that the input integer code can be represented
C       as a character string were removed along with the exceptions
C       associated with these checks.  It is now the responsibility
C       of a maintenance programmer to make sure that MAXL is large
C       enough to allow any integer to be converted to a string
C       representation.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C
C-&
 
C$ Index_Entries
C
C     body code to name
C
C-&
 
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BODC2N_G' )
      END IF
 
C
C     During the first call to this module initialize the
C     two symbol tables and load the permanent collection of
C     name/code definitions.  This section of code is repeated
C     after each entry point.
C
      IF ( FIRST ) THEN
 
         CALL SSIZEC ( MAXE, CODSYM )
         CALL SSIZEI ( MAXE, CODPTR )
         CALL SSIZEC ( MAXE, CODVAL )
 
         CALL SSIZEC ( MAXE, NAMSYM )
         CALL SSIZEI ( MAXE, NAMPTR )
         CALL SSIZEI ( MAXE, NAMVAL )
 
         DO 50002
     .      I = 1, NPERM
 
            SYMBOL = NAMES(I)
            CALL LJUST  (         SYMBOL,   SYMBOL )
            CALL UCASE  (         SYMBOL,   SYMBOL )
            CALL CMPRSS ( ' ', 1, SYMBOL,   SYMBOL )
            CALL SYSETI ( SYMBOL, CODES(I), NAMSYM, NAMPTR, NAMVAL )
 
            CALL INTSTR ( CODES(I),         SYMBOL )
            CALL SYSETC ( SYMBOL, NAMES(I), CODSYM, CODPTR, CODVAL )
 
50002    CONTINUE
 
         FIRST = .FALSE.
 
      END IF
 
C
C     Translate the body integer code to the corresponding
C     body name:
C
C     We store the code symbol as a character string, so we
C     first convert the integer to its character string representation.
C
      CALL INTSTR ( CODE, SYMBOL )
 
C
C     If the symbol is in the table, retrieve first the value
C     associated with it.  The first value is the name that was
C     most recently associated with CODE.
C
      CALL SYNTHC ( SYMBOL,
     .              1,
     .              CODSYM, CODPTR, CODVAL,
     .              NAME,
     .              FOUND                   )
 
 
      CALL CHKOUT ( 'BODC2N_G' )
      RETURN
 
 
 
 
C$Procedure BODDEF_G ( Body name/code definition )
 
      ENTRY BODDEF_G ( NAME, CODE )
 
C$ Abstract
C
C     Define a body name/code pair for later translation by
C     BODN2C_G or BODC2N_G.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     BODY
C     CONVERSION
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               CODE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Common name of some body.
C     CODE       I   Integer code for that body.
C     MAXL       P   Max name length and max number of digits in code.
C     MAXP       P   Maximum number of name/code pair definitions.
C
C$ Detailed_Input
C
C     NAME        is an arbitrary name of a body which could be
C                 a planet, satellite, barycenter, spacecraft,
C                 asteroid, comet, or other ephemeris object.
C
C                 NAME must uniquely identify a body, so NAME must
C                 be distinct from all other names that have been
C                 defined.  (The list of default definitions are
C                 in DATA statements in BODTRN_G for easy reference.)
C
C                 Case and leading and trailing blanks in a name
C                 are not significant.  However when a name is made
C                 up of more than one word, they must be separated by
C                 at least one blank.  That is, all of the following
C                 strings are equivalent names:
C
C                         'JUPITER BARYCENTER'
C                         'Jupiter Barycenter'
C                         'JUPITER BARYCENTER   '
C                         'JUPITER    BARYCENTER'
C                         '   JUPITER BARYCENTER'
C
C                 However, 'JUPITERBARYCENTER' is distinct from
C                 the names above.
C
C                 When ignoring trailing blanks, NAME must have fewer
C                 than MAXL characters.
C
C     CODE        is the integer code for the named body.
C
C                 CODE may already have a name as defined by a previous
C                 call to BODDEF_G or BODFIL_G or as part of the set of
C                 default definitions.  That previous definition will
C                 remain, and a translation of that name will still
C                 give the same CODE.  However, future translations
C                 of CODE will give the new NAME instead of the
C                 previous one.  This feature is useful for assigning
C                 a more familiar or abbreviated name to a body.
C                 For example, in addition to the default name for
C                 body 5, 'JUPITER BARYCENTER', you could define the
C                 abbreviation 'JB' to mean 5.
C
C                 CODE must have at most MAXL digits, where the
C                 minus sign is counted as a digit if CODE is negative.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     MAXL        is the maximum length of a name and the maximum
C                 length of an integer code when that integer is
C                 represented as a character string.  The user may
C                 only increase the value of MAXL, and only that
C                 if necessary.  If MAXL is decreased, the
C                 default names and codes may be truncated.
C
C     MAXP        is the maximum number of name/code pairs that can
C                 be defined via BODDEF_G or BODFIL_G.  It is the limit
C                 on the number of definitions over and above the
C                 number of default definitions.  The user may alter
C                 the the value of MAXP, however, it must remain a
C                 positive integer.
C
C$ Exceptions
C
C     1) If NAME is longer than MAXL, the error SPICE(NAMETOOLONG)
C        is signalled.
C
C     2) If NAME contains no printable characters, the error
C        SPICE(NEEDANAME) is signalled.  Blanks are not printable
C        characters.
C
C     3) If NAME has already been associated with a different CODE,
C        the error SPICE(NAMENOTUNIQUE) is signalled.
C
C     4) If the maximum number of definitions is exceeded, a routine
C        that this routine calls will signal an error.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     BODDEF_G is one of four related entry points,
C
C        BODN2C_G      Body name to code
C
C        BODC2N_G      Body code to name
C
C        BODDEF_G      Body name/code definition
C
C        BODFIL_G      Body name/code definitions from a file
C
C     BODN2C_G and BODC2N_G perform translations between body names
C     and their corresponding integer codes which are used
C     in SPK and PCK files and routines.  A set of name/code
C     pairs are automatically defined during the first call to
C     one of these entry points.  Additional name/code pairs may
C     be defined via BODDEF_G or BODFIL_G for two purposes:
C
C        1.  to associate another, perhaps more familiar or
C            abbreviated, name with a particular body integer
C            code that has already been defined, or
C
C        2.  to define a new body integer code and name,
C
C     Each body has a unique integer code, but may have several
C     names.  Thus you may associate more than one name with
C     a particular integer code.  However, associating more
C     than one integer code with a particular name creates ambiguity.
C     Therefore, once a name has been defined, it may not be redefined
C     with a different integer code.
C
C     For example, Europa is the name of the second satellite of
C     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502)
C     is one of the default definitions.  Europa is also the name
C     of an asteroid.  Suppose you were able to associate the asteroid
C     integer code with the name EUROPA.  Then when you call BODN2C_G to
C     translate the name EUROPA, which code should be returned?  That
C     of the asteroid or 502?
C
C     BODDEF_G and BODFIL_G prevent this ambiguity by signalling an
C     error if the specified name has already been defined with a
C     different code.  In the case of EUROPA, you may want to use the
C     name ASTEROID EUROPA.  The set of default definitions are listed
C     in DATA statements in the umbrella routine BODTRN_G for easy
C     reference.
C
C$ Examples
C
C     You may associate a new name with a particular code that
C     has already been defined:
C
C            CALL BODDEF_G ( 'JB', 5 )
C
C     You may also define the name and integer code for a new body:
C
C            CALL BODDEF_G ( 'Asteroid Frank', 20103456 )
C
C     After these calls to BODDEF_G, BODN2C_G would return the following
C     translations:
C
C        Name                         Code    Found?
C        ------------------------   ------    ------
C        'JB'                            5    Yes
C        'Jupiter Barycenter'            5    Yes
C        'ASTEROID FRANK'         20103456    Yes
C        'ASTEROIDFRANK'                 -    No
C        'Frank'                         -    No
C
C     and BODC2N_G will return these translations:
C
C        Code        Name                     Found?
C        -------     -------------------      ------
C               5    'JB'                     Yes
C        20103456    'Asteroid Frank'         Yes
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
C     J.E. McLean    (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    GLLSPICE Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C       Checks to see that an integer code can be represented
C       as a character string were removed along with the exceptions
C       associated with these checks.  It is now the responsibility
C       of a maintenance programmer to make sure that MAXL is large
C       enough to allow any integer to be converted to a string
C       representation.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C
C-&
 
C$ Index_Entries
C
C     body name/code definition
C
C-&
 
 
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BODDEF_G' )
      END IF
 
C
C     During the first call to this module initialize the
C     two symbol tables and load the permanent collection of
C     name/code definitions.  This section of code is repeated
C     after each entry point.
C
      IF ( FIRST ) THEN
 
         CALL SSIZEC ( MAXE, CODSYM )
         CALL SSIZEI ( MAXE, CODPTR )
         CALL SSIZEC ( MAXE, CODVAL )
 
         CALL SSIZEC ( MAXE, NAMSYM )
         CALL SSIZEI ( MAXE, NAMPTR )
         CALL SSIZEI ( MAXE, NAMVAL )
 
         DO 50003
     .      I = 1, NPERM
 
            SYMBOL = NAMES(I)
            CALL LJUST  (         SYMBOL,   SYMBOL )
            CALL UCASE  (         SYMBOL,   SYMBOL )
            CALL CMPRSS ( ' ', 1, SYMBOL,   SYMBOL )
            CALL SYSETI ( SYMBOL, CODES(I), NAMSYM, NAMPTR, NAMVAL )
 
            CALL INTSTR ( CODES(I),         SYMBOL )
            CALL SYSETC ( SYMBOL, NAMES(I), CODSYM, CODPTR, CODVAL )
 
50003    CONTINUE
 
         FIRST = .FALSE.
 
      END IF
 
C
C     Check to see if NAME is valid.  If it is, left justify,
C     compress consecutive blanks to one, and convert to upper case.
C
      IF ( LASTPC ( NAME ) .EQ. 0 ) THEN
         CALL SETMSG ( 'The input name string has no printable '      //
     .                 'characters.'                                   )
         CALL SIGERR ( 'SPICE(NEEDANAME)' )
         CALL CHKOUT ( 'BODDEF_G' )
         RETURN
      END IF
 
      LENGTH = LASTPC ( NAME )
     .       - FRSTNB ( NAME )
     .       + 1
 
      IF ( LENGTH .GT. MAXL ) THEN
         CALL SETMSG ( 'The input name string is longer than the '    //
     .                 'maximum length, #, set by the parameter MAXL.' )
         CALL ERRINT ( '#', MAXL )
         CALL SIGERR ( 'SPICE(NAMETOOLONG)' )
         CALL CHKOUT ( 'BODDEF_G' )
         RETURN
      END IF
 
      CALL CMPRSS ( ' ', 1, NAME,  TNAME )
      CALL LJUST  (         TNAME, TNAME )
      CALL UCASE  (         TNAME, TNAME )
 
C
C     Now make sure NAME is unique, that is, that it isn't already
C     in the NAM symbol table with a different CODE.
C
      CALL SYNTHI ( TNAME,
     .              1,
     .              NAMSYM, NAMPTR, NAMVAL,
     .              TCODEI,
     .              TFOUND                     )
 
      IF (  TFOUND .AND. ( TCODEI .NE. CODE )  ) THEN
         CALL SETMSG ( 'The NAME, #, has already been used for '      //
     .                 'another body.'                                 )
         CALL ERRCH  ( '#', NAME )
         CALL SIGERR ( 'SPICE(NAMENOTUNIQUE)' )
         CALL CHKOUT ( 'BODDEF_G' )
         RETURN
      END IF
 
C
C     Convert the integer CODE to its character string representation.
C
      CALL INTSTR ( CODE,   SYMBOL )
 
 
C
C     If we've made it this far, the input is valid and
C     TNAME and SYMBOL are in the appropriate symbol format.
C     Set the symbols and values.
C
      CALL SYSETI ( TNAME,  CODE, NAMSYM, NAMPTR, NAMVAL )
 
C
C     Now left justify NAME and put it into the COD symbol
C     table.
C
      CALL LJUST  ( NAME,   TNAME                         )
      CALL SYSETC ( SYMBOL, TNAME, CODSYM, CODPTR, CODVAL )
 
      CALL CHKOUT ( 'BODDEF_G' )
      RETURN
 
 
 
C$Procedure BODFIL_G ( Body name/code definitions from a file )
 
      ENTRY BODFIL_G ( FILE )
 
C$ Abstract
C
C     Read body name/code pairs from a file and define them for later
C     translation by BODN2C_G or BODC2N_G.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     BODY
C     CONVERSION
C
C$ Declarations
C
C     CHARACTER*(*)         FILE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FILE       I   Name of the file containing body names and codes.
C     LNSIZE     P   Length of a line of data read from FILE.
C     MAXL       P   Max name length and max number of digits in code.
C     MAXP       P   Maximum number of name/code definitions.
C
C$ Detailed_Input
C
C     FILE        is the name of a file containing body name/code pairs.
C
C                 Nominally the file must contain the following
C                 line that that marks the beginning of lines of data:
C
C                    \begindata
C
C                 and then lines of data with the format
C
C                    NAME = CODE
C
C                 where NAME is an arbitrary name of a body which
C                 could be a planet, satellite, barycenter, spacecraft,
C                 asteroid, comet, or other ephemeris object.
C
C                 NAME must uniquely identify a body, so NAME must
C                 be distinct from all other names that have been
C                 defined.  (The list of default definitions are
C                 in DATA statements in BODTRN_G for easy reference.)
C
C                 Case and leading and trailing blanks in a name
C                 are not significant.  However when a name is made
C                 up of more than one word, they must be separated by
C                 at least one blank.  That is, all of the following
C                 strings are equivalent names:
C
C                         'JUPITER BARYCENTER'
C                         'Jupiter Barycenter'
C                         'JUPITER BARYCENTER   '
C                         'JUPITER    BARYCENTER'
C                         '   JUPITER BARYCENTER'
C
C                 However, 'JUPITERBARYCENTER' is distinct from
C                 the names above.
C
C                 When ignoring trailing blanks, NAME must have fewer
C                 than MAXL characters.
C
C                 CODE is the integer code for the named body.
C
C                 CODE may already have a name as defined by a previous
C                 call to BODDEF_G or BODFIL_G or as part of the set of
C                 default definitions.  That previous definition will
C                 remain, and a translation of that name will still
C                 give the same CODE.  However, future translations
C                 of CODE will give the new NAME instead of the
C                 previous one.  This feature is useful for assigning
C                 a more familiar or abbreviated name to a body.
C                 For example, in addition to the default name for
C                 body 5, 'JUPITER BARYCENTER', you could define the
C                 abbreviation 'JB' to mean 5.
C
C                 CODE must have at most MAXL digits, where the
C                 minus sign is counted as a digit if CODE is negative.
C
C                 FILE may also contain comments before the first
C                 occurrence of '\begindata' or following the line
C
C                    \begintext
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     LNSIZE      is the length of a line of data read from FILE.
C                 A line of data contains a name and a code, separated
C                 by an equal sign.  Thus LNSIZE must be large
C                 enough to accommodate the longest name and code
C                 pair in the file.
C
C     MAXL        is the maximum length of a name and the maximum
C                 length of an integer code when that integer is
C                 represented as a character string.  The user may
C                 only increase the value of MAXL, and only that
C                 if necessary.  If MAXL is decreased, the
C                 default names and codes may be truncated.
C
C     MAXP        is the maximum number of name/code pairs that can
C                 be defined via BODDEF_G or BODFIL_G.  It is the limit
C                 on the number of definitions over and above the
C                 number of default definitions.  The user may alter
C                 the the value of MAXP, however, it must remain a
C                 positive integer.
C
C$ Exceptions
C
C     1) If the FILE cannot be opened or cannot be read, a routine
C        that this routine calls will signal an appropriate error.
C
C     2) If FILE does not contain the '\begindata' marker
C        before the data, no name/code pairs will be defined.
C
C     3) If there is no equal sign between the name and code, the
C        error SPICE(NEEDEQUALSIGN) is signalled.
C
C     4) Each line of data is read as a character string of length
C        LNSIZE, with the substring to the left of the equal sign
C        taken to be the body name, and the substring to the right
C        of the equal sign taken to be the body code.  If the second
C        substring is not a character string representation of an
C        integer, the error SPICE(NEEDINTEGER) is signalled.
C
C     5) If NAME is longer than MAXL, excluding leading and trailing
C        blanks, the error SPICE(NAMETOOLONG) is signalled.
C
C     6) If NAME contains no printable characters, the error
C        SPICE(NEEDANAME) is signalled.  Blanks are not printable
C        characters.
C
C     7) If NAME has already been associated with a different CODE,
C        the error SPICE(NAMENOTUNIQUE) is signalled.
C
C     8) If the maximum number of definitions is exceeded, a routine
C        that this routine calls will signal an error.
C
C$ Files
C
C     See FILE in the $ Detailed_Input section above.
C
C$ Particulars
C
C     BODFIL_G is one of four related entry points,
C
C        BODN2C_G      Body name to code
C
C        BODC2N_G      Body code to name
C
C        BODDEF_G      Body name/code definition
C
C        BODFIL_G      Body name/code definitions from a file
C
C     BODN2C_G and BODC2N_G perform translations between body names
C     and their corresponding integer codes which are used
C     in SPK and PCK files and routines.  A set of name/code
C     pairs are automatically defined during the first call to
C     one of these entry points.  Additional name/code pairs may
C     be defined via BODDEF_G or BODFIL_G for two purposes:
C
C        1.  to associate another, perhaps more familiar or
C            abbreviated, name with a particular body integer
C            code that has already been defined, or
C
C        2.  to define a new body integer code and name,
C
C     Each body has a unique integer code, but may have several
C     names.  Thus you may associate more than one name with
C     a particular integer code.  However, associating more
C     than one integer code with a particular name creates ambiguity.
C     Therefore, once a name has been defined, it may not be redefined
C     with a different integer code.
C
C     For example, Europa is the name of the second satellite of
C     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502)
C     is one of the default definitions.  Europa is also the name
C     of an asteroid.  Suppose you were able to associate the asteroid
C     integer code with the name EUROPA.  Then when you call BODN2C_G to
C     translate the name EUROPA, which code should be returned?  That
C     of the asteroid or 502?
C
C     BODDEF_G and BODFIL_G prevent this ambiguity by signalling an
C     error if the specified name has already been defined with a
C     different code.  In the case of EUROPA, you may want to use the
C     name ASTEROID EUROPA.  The set of default definitions are listed
C     in DATA statements in the umbrella routine BODTRN_G for easy
C     reference.
C
C$ Examples
C
C     1.  Suppose the file BODIES.TXT has the following contents:
C
C               Definitions of body name/code pairs
C               --------------------------------------
C
C               New names for default codes:
C
C               \begindata
C
C                  JB                = 5
C                  home              = 399
C
C               \begintext
C
C               Names and codes for new bodies:
C
C               \begindata
C
C                  Asteroid Frank    = 20103456
C                  Krypton           = 2099
C                  U.S.S. Enterprise = -8934
C
C
C         To define these pairs call BODFIL_G as follows:
C
C            CALL BODFIL_G ( 'BODIES.TXT' )
C
C         After this call to BODFIL_G, BODN2C_G would return the
C         following translations:
C
C         Name                         Code    Found?
C         ------------------------   ------    ------
C         'JB'                            5    Yes
C         'Jupiter Barycenter'            5    Yes
C         'ASTEROID FRANK'         20103456    Yes
C         'ASTEROIDFRANK'                 -    No
C         'Frank'                         -    No
C         'EARTH'                       399    Yes
C         ' HOME  '                     399    Yes
C
C         and BODC2N_G will return these translations:
C
C         Code        Name                     Found?
C         -------     -------------------      ------
C                5    'JB'                     Yes
C         20103456    'Asteroid Frank'         Yes
C              399    'home'                   Yes
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
C     J.E. McLean    (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    GLLSPICE Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    GLLSPICE Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C       Checks to see that an integer code can be represented
C       as a character string were removed along with the exceptions
C       associated with these checks.  It is now the responsibility
C       of a maintenance programmer to make sure that MAXL is large
C       enough to allow any integer to be converted to a string
C       representation.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C
C-&
 
C$ Index_Entries
C
C     body name/code definitions from a file
C
C-&
 
 
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BODFIL_G' )
      END IF
 
C
C     During the first call to this module initialize the
C     two symbol tables and load the permanent collection of
C     name/code definitions.  This section of code is repeated
C     after each entry point.
C
      IF ( FIRST ) THEN
 
         CALL SSIZEC ( MAXE, CODSYM )
         CALL SSIZEI ( MAXE, CODPTR )
         CALL SSIZEC ( MAXE, CODVAL )
 
         CALL SSIZEC ( MAXE, NAMSYM )
         CALL SSIZEI ( MAXE, NAMPTR )
         CALL SSIZEI ( MAXE, NAMVAL )
 
         DO 50004
     .      I = 1, NPERM
 
            SYMBOL = NAMES(I)
            CALL LJUST  (         SYMBOL,   SYMBOL )
            CALL UCASE  (         SYMBOL,   SYMBOL )
            CALL CMPRSS ( ' ', 1, SYMBOL,   SYMBOL )
            CALL SYSETI ( SYMBOL, CODES(I), NAMSYM, NAMPTR, NAMVAL )
 
            CALL INTSTR ( CODES(I),         SYMBOL )
            CALL SYSETC ( SYMBOL, NAMES(I), CODSYM, CODPTR, CODVAL )
 
50004    CONTINUE
 
         FIRST = .FALSE.
 
      END IF
 
 
C
C     Read the lines of data from FILE, parse them into a name
C     and integer code, and add that pair to the the set of
C     definitions.
C
      CALL RDKNEW ( FILE )
      CALL RDKDAT ( LINE, EOF )
 
50005 IF       ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
     .THEN
 
         IF ( INDEX ( LINE, '=' ) .EQ. 0 ) THEN
            CALL SETMSG ( 'Check the format of #.  Data should be '   //
     .                    'of the form:  NAME = CODE. ' )
            CALL ERRCH  ( '#', FILE  )
            CALL SIGERR ( 'SPICE(NEEDEQUALSIGN)' )
            CALL CHKOUT ( 'BODFIL_G' )
            RETURN
         END IF
 
         CALL LPARSE ( LINE, '=', 2, N, ITEMS )
 
         CALL LJUST ( ITEMS(1), TNAME  )
         CALL LJUST ( ITEMS(2), TCODEC )
 
C
C        Check the second substring.  Is it an integer?
C        Is it too long?
C
         CALL NPARSI ( TCODEC, TCODEI, ERROR, PNTER )
 
         IF ( ERROR .NE. ' ' ) THEN
            CALL SETMSG ( 'While reading #, #')
            CALL ERRCH  ( '#', FILE  )
            CALL ERRCH  ( '#', ERROR )
            CALL SIGERR ( 'SPICE(NEEDINTEGER)' )
            CALL CHKOUT ( 'BODFIL_G' )
            RETURN
         END IF
 
         CALL INTSTR ( TCODEI, TCODEC )
         CALL LJUST  ( TCODEC, TCODEC )
 
C
C        Check the first substring.  Is it a valid name?  If it is,
C        left justify, compress consecutive blanks to one, and
C        convert to upper case.
C
         IF ( LASTPC ( TNAME ) .EQ. 0 ) THEN
            CALL SETMSG ( 'The name string has no printable characters')
            CALL SIGERR ( 'SPICE(NEEDANAME)' )
            CALL CHKOUT ( 'BODFIL_G' )
            RETURN
         END IF
 
         IF ( LASTPC ( TNAME ) .GT. MAXL ) THEN
            CALL SETMSG ( 'The input name string is longer than the ' //
     .                    'maximum length, #, set by the parameter '  //
     .                    'MAXL.'                                      )
            CALL ERRINT ( '#', MAXL )
            CALL SIGERR ( 'SPICE(NAMETOOLONG)' )
            CALL CHKOUT ( 'BODFIL_G' )
            RETURN
         END IF
 
         SYMBOL = TNAME
         CALL UCASE  (         SYMBOL,   SYMBOL )
         CALL CMPRSS ( ' ', 1, SYMBOL,   SYMBOL )
 
C
C        Now make sure the name is unique, that is, that it isn't
C        already in the NAM symbol table with a different code.
C
         CALL SYNTHI ( SYMBOL,
     .                 1,
     .                 NAMSYM, NAMPTR, NAMVAL,
     .                 I,
     .                 TFOUND                  )
 
         IF (  TFOUND .AND. ( I .NE. TCODEI )  ) THEN
            CALL SETMSG ( 'The name, #, has already been used for '   //
     .                    'another body.'                              )
            CALL ERRCH  ( '#', TNAME )
            CALL SIGERR ( 'SPICE(NAMENOTUNIQUE)' )
            CALL CHKOUT ( 'BODFIL_G' )
            RETURN
         END IF
 
 
C
C        If we've made it this far, the line contained a valid name/code
C        pair.  Now we set the symbols and values.
C
 
         CALL SYSETI ( SYMBOL, TCODEI, NAMSYM, NAMPTR, NAMVAL )
         CALL SYSETC ( TCODEC, TNAME,  CODSYM, CODPTR, CODVAL )
 
C
C        Read the next line of data.
C
         CALL RDKDAT ( LINE, EOF )
 
         GO TO 50005
      END IF
 
      CALL CHKOUT ( 'BODFIL_G' )
      RETURN
      END
