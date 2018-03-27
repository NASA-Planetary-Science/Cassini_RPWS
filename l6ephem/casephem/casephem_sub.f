C-----------------------------------------------------------------------
      SUBROUTINE GET_JUPITER_MAG_COORDS ( Observer, Target, ET,
     +                                    Radius, Longitude, MLAT,
     +                                    MLT, LValue, IoPhase )
      
      IMPLICIT NONE
      
      INTEGER               Observer
      INTEGER               Target
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Radius
      DOUBLE PRECISION      Longitude
      DOUBLE PRECISION      MLAT
      DOUBLE PRECISION      MLT
      DOUBLE PRECISION      LValue
      DOUBLE PRECISION      IoPhase

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      HALFPI

C
C   Local function
C
      DOUBLE PRECISION      GET_EQUATORIAL_RADII

C
C     Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      ANGLE1
      DOUBLE PRECISION      ANGLE2
      DOUBLE PRECISION      ANGLE3
      DOUBLE PRECISION      RJupiter
      DOUBLE PRECISION      JupiterRadius
      DOUBLE PRECISION      SCLong
      DOUBLE PRECISION      SunLong
      DOUBLE PRECISION      MLatitude
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      Phi
      DOUBLE PRECISION      Theta
      DOUBLE PRECISION      Lambda
      DOUBLE PRECISION      Delta
      DOUBLE PRECISION      Numerator
      DOUBLE PRECISION      CosLat
      DOUBLE PRECISION      IoLongitude
      DOUBLE PRECISION      ThetaSC
      DOUBLE PRECISION      ThetaGM
      DOUBLE PRECISION      ThetaSU
      DOUBLE PRECISION      GG        ( 9 )
      DOUBLE PRECISION      HH        ( 9 )
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Temp2     ( 3 )
      DOUBLE PRECISION      BPRIME    ( 3 )
      DOUBLE PRECISION      BFINAL    ( 3 )
      DOUBLE PRECISION      Position  ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      BMAT   ( 3, 3 )
      DOUBLE PRECISION      Tipm   ( 3, 3 )

C
C  Local parameters
C
      INTEGER               AXIS3
      INTEGER               AXIS2
      INTEGER               AXIS1

      INTEGER               Sun
      INTEGER               Jupiter
      INTEGER               Io

      PARAMETER           ( AXIS3    =    3 )
      PARAMETER           ( AXIS2    =    1 )
      PARAMETER           ( AXIS1    =    3 )

      PARAMETER           ( Sun      =   10 )
      PARAMETER           ( Jupiter  =  599 )
      PARAMETER           ( Io       =  501 )

      RJupiter = GET_EQUATORIAL_RADII ( Jupiter )

C
C  Spherical harmonic coeffcients for VIP4 model
C
      GG ( 1 ) =  4.205D0
      GG ( 2 ) = -0.659D0
      GG ( 3 ) = -0.051D0
      GG ( 4 ) = -0.619D0
      GG ( 5 ) =  0.497D0
      GG ( 6 ) = -0.016D0
      GG ( 7 ) = -0.520D0
      GG ( 8 ) =  0.244D0
      GG ( 9 ) = -0.176D0

      HH ( 1 ) =  0.000D0
      HH ( 2 ) =  0.250D0
      HH ( 3 ) =  0.000D0
      HH ( 4 ) = -0.361D0
      HH ( 5 ) =  0.053D0
      HH ( 6 ) =  0.000D0
      HH ( 7 ) = -0.088D0
      HH ( 8 ) =  0.408D0
      HH ( 9 ) = -0.316D0

      Numerator = DSQRT ( ( GG ( 2 )*GG ( 2 ) ) +
     1                    ( HH ( 2 )*HH ( 2 ) ) )

      Theta = DATAN2 ( Numerator, GG ( 1 ) )

      Lambda = DATAN2 ( HH ( 2 ), GG ( 2 ) )

      IF ( Lambda .LT. 0.0D0 ) THEN
         Lambda = ( Lambda + TWOPI () )
      ENDIF

      Lambda = 159.2D0
      Lambda = ( Lambda*RPD () )

      Theta = 9.5D0
      Theta = ( Theta*RPD () )

C
C   Convert to Euler angles.
C
      Phi = Lambda + HALFPI ()

      Delta = Lambda + HALFPI ()
      Delta = ( -1.0D0*Delta )
 
      ANGLE1 = Phi

      ANGLE2 = Theta

      ANGLE3 = Delta

      CALL EUL2M ( ANGLE3, ANGLE2, ANGLE1, AXIS3, AXIS2, AXIS1, BMAT )

C
C   determine system III longitude of spacecraft
C
      CALL SPKEZ ( Observer, ET, 'J2000', 'NONE', Jupiter, State, LT )

      DO 10 Index = 1, 3
         Temp2 ( Index ) = State ( Index )
   10 CONTINUE

      CALL BODMAT ( Jupiter, ET, Tipm )

      CALL MXV ( Tipm, Temp2, BPRIME )

      CALL RECLAT ( BPRIME, Dummy1, Longitude, Dummy2 )

      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Longitude = ( TWOPI () - Longitude )
      Longitude = ( Longitude*DPR () )

      CALL MXV ( BMAT, BPRIME, BFINAL )

      CALL RECLAT ( BFINAL, Radius, SCLong, MLatitude )

      JupiterRadius = Radius

      JupiterRadius = ( JupiterRadius/RJupiter )

      IF ( SCLong .LT. 0.0D0 ) THEN
         SCLong = ( SCLong + TWOPI () )
      ENDIF

      SCLong = ( TWOPI () - SCLong )
      SCLong = ( SCLong*DPR () )

C
C   determine system III longitude of Sun
C
      CALL SPKEZ ( Sun, ET, 'J2000', 'NONE', Jupiter, State, Dummy1 )

      DO 20 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   20 CONTINUE

      CALL MXV ( TIPM, Temp1, BPRIME )

      CALL MXV ( BMAT, BPRIME, BFINAL )

      CALL RECLAT ( BFINAL, Dummy1, SunLong, Dummy2 )

      IF ( SunLong .LT. 0.0D0 ) THEN
         SunLong = ( SunLong + TWOPI () )
      ENDIF

      SunLong = ( TWOPI () - SunLong )
      SunLong = ( SunLong*DPR () )

C
C   determine magnetic local time
C
      MLT = ( SunLong - SCLong )
      MLT = ( MLT + 180.0D0 )
      MLT = ( MLT/15.0D0 )

      IF ( MLT .LT. 0.0D0 ) THEN
         MLT = ( MLT + 24.0D0 )
      ENDIF

      IF ( MLT .GE. 24.0D0 ) THEN
         MLT = ( MLT - 24.0D0 )
      ENDIF

C
C   determine L
C
      CosLat = DCOS ( MLatitude )

      LValue = ( JupiterRadius/( CosLat*CosLat ) )

      MLAT = ( MLatitude*DPR () )

C
C   determine phase of Io
C
      CALL SPKEZ ( Observer, ET, 'JSQ', 'NONE', Target, State, Dummy1 )

      DO 30 Index = 1, 3
         Position ( Index ) = State ( Index )
   30 CONTINUE

      CALL RECLAT ( Position, Dummy1, IoLongitude, Dummy2 )

      IF ( IoLongitude .LT. 0.0D0 ) THEN
         IoLongitude = ( IoLongitude + TWOPI () )
      ENDIF

      IoLongitude = ( TWOPI () - IoLongitude )
      IoLongitude = ( IoLongitude*DPR () )

      ThetaSC = IoLongitude

      CALL SPKEZ ( Io, ET, 'JSQ', 'NONE', Target, State, Dummy1 )

      DO 40 Index = 1, 3
         Position ( Index ) = State ( Index )
   40 CONTINUE

      CALL RECLAT ( Position, Dummy1, IoLongitude, Dummy2 )

      IF ( IoLongitude .LT. 0.0D0 ) THEN
         IoLongitude = ( IoLongitude + TWOPI () )
      ENDIF

      IoLongitude = ( TWOPI () - IoLongitude )
      IoLongitude = ( IoLongitude*DPR () )

      ThetaGM = IoLongitude

      ThetaSU = ( ThetaSC + 180.0D0 )

      IF ( ThetaSU .GT. 360.0D0 ) THEN
         ThetaSU = ( ThetaSU - 360.0D0 )
      ENDIF

      IoPhase = ( 360.0D0 - ThetaGM )
      IoPhase = ( IoPhase + ThetaSU )

      IF ( IoPhase .GE. 360.0D0 ) THEN
         IoPhase = ( IoPhase - 360.0D0 )
      ENDIF

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GETCOROT ( Observer, Target, CoPlanet,
     +                      ET, Px, Py, Pz, Vx, Vy, Vz )
      
      IMPLICIT NONE
      
      INTEGER               Observer
      INTEGER               Target
      INTEGER               CoPlanet
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Px, Py, Pz
      DOUBLE PRECISION      Vx, Vy, Vz

C
C  Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Pos       ( 3 )
      DOUBLE PRECISION      Pos1      ( 3 )
      DOUBLE PRECISION      Vel       ( 3 )
      DOUBLE PRECISION      Vel1      ( 3 )
      DOUBLE PRECISION      Temp      ( 3 )
      DOUBLE PRECISION      R_Vec     ( 3 )
      DOUBLE PRECISION      Y_Vec     ( 3 )
      DOUBLE PRECISION      Omega_Vec ( 3 )
      DOUBLE PRECISION      Phi_Vec   ( 3 )
      DOUBLE PRECISION      Tipm   ( 3, 3 )
      DOUBLE PRECISION      Matrix ( 3, 3 )
      DOUBLE PRECISION      LT

      CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', CoPlanet, State, LT )

      DO 10 Index = 1, 3
         Temp ( Index ) = State ( Index )
   10 CONTINUE

C
C   Radial vector from planet -> moon (out)
C
      CALL VHAT ( Temp, R_Vec )
   
C
C   Omega is spin plane axis
C
      CALL BODMAT ( CoPlanet, ( ET - LT ), Tipm )

      DO 20 Index = 1, 3
         Omega_Vec ( Index ) = tipm ( 3, Index )
   20 CONTINUE
   
C
C   Omega x Radial (out) = Phi
C
      CALL UCRSS ( Omega_Vec, R_Vec, Phi_Vec )
      CALL UCRSS ( Omega_Vec, Phi_Vec, Y_Vec )

C
C   load transpose  matrix
C
      DO 30 Index = 1, 3
         Matrix ( Index, 1 ) = Phi_Vec   ( Index )
         Matrix ( Index, 2 ) = Y_Vec     ( Index )
         Matrix ( Index, 3 ) = Omega_Vec ( Index )
   30 CONTINUE

      CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', Observer, State, LT )

      DO 40 Index = 1, 3
         Pos ( Index ) = State ( Index )
         Vel ( Index ) = State ( ( Index + 3 ) )
   40 CONTINUE

      CALL VMINUS ( Pos, Pos1 )
      CALL VMINUS ( Vel, Vel1 )

      CALL MTXV ( Matrix, Pos1, Pos )
      CALL MTXV ( Matrix, Vel1, Vel )

      Px = Pos ( 1 )
      Py = Pos ( 2 )
      Pz = Pos ( 3 )

      Vx = Vel ( 1 )
      Vy = Vel ( 2 )
      Vz = Vel ( 3 )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE SLS2_SLS3 ( Observer, ET, LocalTime, LightTimeSeconds )
      
      IMPLICIT NONE

      INTEGER               Observer
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      LocalTime
      DOUBLE PRECISION      LightTimeSeconds

C
C   Local parameters
C
      INTEGER               Sun
      INTEGER               Saturn

      PARAMETER           ( Sun      =  10 )
      PARAMETER           ( Saturn   = 699 )

C
C   SPICE functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      TWOPI

C
C   Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Longitude
      DOUBLE PRECISION      SC_Longitude
      DOUBLE PRECISION      Sun_Longitude
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Temp2     ( 3 )
      DOUBLE PRECISION      Position  ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Matrix ( 3, 3 )

C
C   determine longitude of the spacecraft
C
      CALL SPKEZ ( Saturn, ET, 'J2000', 'NONE', Observer, State, LT )

      LightTimeSeconds = LT

      CALL BODMAT ( Saturn, ( ET - LT ), Matrix )

      CALL SPKEZ ( Saturn, ( ET - LT ), 'J2000', 'NONE', Observer,
     +             State, Dummy1 )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )

      CALL MXV ( Matrix, Temp2, Position )

      CALL RECLAT ( Position, Dummy1, Longitude, Dummy2 )

C
C   convert to west longitude
C
      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Longitude = ( TWOPI () - Longitude )
      Longitude = ( Longitude*DPR () )

      SC_Longitude = Longitude

C
C   determine longitude of the Sun
C
      CALL SPKEZ ( Saturn, ( ET - LT ), 'J2000', 'NONE', Sun,
     +             State, Dummy1 )

      DO 20 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   20 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )

      CALL MXV ( Matrix, Temp2, Position )

      CALL RECLAT ( Position, Dummy1, Longitude, Dummy2 )

C
C   convert to west longitude
C
      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Longitude = ( TWOPI () - Longitude )
      Longitude = ( Longitude*DPR () )

      Sun_Longitude = Longitude

C
C   determine local time
C
      LocalTime = ( Sun_Longitude - SC_Longitude )

      LocalTime = ( LocalTime + 180.0D0 )

      LocalTime = ( LocalTime/15.0D0 )

      IF ( LocalTime .LT. 0.0D0 ) THEN
         LocalTime = ( LocalTime + 24.0D0 )
      ENDIF

      IF ( LocalTime .GE. 24.0D0 ) THEN
         LocalTime = ( LocalTime - 24.0D0 )
      ENDIF

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GET_GEO ( Observer, Target, BodyName, ET, Radius,
     +                     MLatitude, LValue, LocalTime )
      
      IMPLICIT NONE

      INTEGER               Observer
      INTEGER               Target
      INTEGER               BodyName
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Radius
      DOUBLE PRECISION      MLatitude
      DOUBLE PRECISION      LValue
      DOUBLE PRECISION      LocalTime

C
C   Local parameters
C
      INTEGER               Sun

      PARAMETER           ( Sun = 10 )

C
C   SPICE functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      TWOPI

C
C   Local function
C
      DOUBLE PRECISION      GET_EQUATORIAL_RADII

C
C   Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      SunLong
      DOUBLE PRECISION      Longitude
      DOUBLE PRECISION      CosLat
      DOUBLE PRECISION      REarth
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Position  ( 3 )
      DOUBLE PRECISION      State     ( 6 )

      REarth = GET_EQUATORIAL_RADII ( BodyName )

C
C   determine radial distance
C
      CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', Observer, State, LT )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Position )

      CALL RECLAT ( Position, Radius, Dummy1, Dummy2 )

      Radius = ( Radius/REarth )

C
C   determine magnetic latitude and longitude
C
      CALL SPKEZ ( Target, ET, 'SM', 'LT+S', Observer, State, LT )

      DO 20 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   20 CONTINUE

      CALL VMINUS ( Temp1, Position )

      CALL RECLAT ( Position, Dummy1, Longitude, MLatitude )

C
C   determine L
C
      CosLat = DCOS ( MLatitude )

      LValue = ( Radius/( CosLat*CosLat ) )

      MLatitude = ( MLatitude*DPR () )

C
C   convert to west longitude
C
      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Longitude = ( TWOPI () - Longitude )
      Longitude = ( Longitude*DPR () )

C
C   determine magnetic longitude of the Sun
C
      CALL SPKEZ ( Sun, ( ET - LT ), 'SM', 'NONE', Target, State,
     +             Dummy1 )

      DO 30 Index = 1, 3
         Position ( Index ) = State ( Index )
   30 CONTINUE

      CALL RECLAT ( Position, Dummy1, SunLong, Dummy2 )

C
C   convert to west longitude
C
      IF ( SunLong .LT. 0.0D0 ) THEN
         SunLong = ( SunLong + TWOPI () )
      ENDIF

      SunLong = ( TWOPI () - SunLong )
      SunLong = ( SunLong*DPR () )

C
C   determine magnetic local time
C
      LocalTime = ( SunLong - Longitude )

      LocalTime = ( LocalTime + 180.0D0 )

      LocalTime = ( LocalTime/15.0D0 )

      IF ( LocalTime .LT. 0.0D0 ) THEN
         LocalTime = ( LocalTime + 24.0D0 )
      ENDIF

      IF ( LocalTime .GE. 24.0D0 ) THEN
         LocalTime = ( LocalTime - 24.0D0 )
      ENDIF

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GETKSM ( Observer, Target, ET,
     +                    Px, Py, Pz, Vx, Vy, Vz )
      
      
      IMPLICIT NONE
      
      INTEGER               Observer
      INTEGER               Target
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Px, Py, Pz
      DOUBLE PRECISION      Vx, Vy, Vz

C
C  Local parameters
C
      INTEGER               Sun
      INTEGER               Saturn
      INTEGER               Axis

      PARAMETER           ( Sun      =  10 )
      PARAMETER           ( Saturn   = 699 )
      PARAMETER           ( Axis     =   2 )
      
C
C  Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      State ( 6 )
      DOUBLE PRECISION      Temp ( 3 )
      DOUBLE PRECISION      Temp2 ( 3 )
      DOUBLE PRECISION      Sun_Pos ( 3 )
      DOUBLE PRECISION      Obs_Pos ( 3 )
      DOUBLE PRECISION      Obs_Vel ( 3 )
      DOUBLE PRECISION      X_Vec ( 3 )
      DOUBLE PRECISION      Y_Vec ( 3 )
      DOUBLE PRECISION      Z_Vec ( 3 )
      DOUBLE PRECISION      Tipm ( 3, 3 )
      DOUBLE PRECISION      Matrix ( 3, 3 )
      DOUBLE PRECISION      RMat ( 3, 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      Latitude
      DOUBLE PRECISION      Angle1
      DOUBLE PRECISION      Angle2
      DOUBLE PRECISION      Angle3

C-----------------------------------------------------------------------
      CALL SPKEZ (Sun, ET, 'J2000', 'NONE', Saturn, State, Dummy1)

      DO 10 Index = 1, 3
         Temp ( Index ) = State ( Index )
   10 CONTINUE

      CALL VHAT ( Temp, Sun_Pos )

      CALL SPKEZ (Observer, ET, 'J2000', 'LT+S', Target, State, LT)

      DO 20 Index = 1, 3
         Temp ( Index ) = State ( Index )
         Temp2 ( Index ) = State ( ( Index + 3 ) )
   20 CONTINUE

      CALL VMINUS ( Temp, Obs_Pos )
      CALL VMINUS ( Temp2, Obs_Vel )

      CALL BODMAT ( Saturn, (ET - LT), Tipm )

      DO 30 Index = 1, 3
         X_Vec ( Index ) = Sun_Pos ( Index )
         Z_Vec ( Index ) = Tipm ( 3, Index )
   30 CONTINUE

      CALL UCRSS ( Z_Vec, X_Vec, Y_Vec )
      CALL UCRSS ( Y_Vec, Z_Vec, X_Vec )

      DO 40 Index = 1, 3
         Matrix ( Index, 1 ) = X_Vec ( Index )
         Matrix ( Index, 2 ) = Y_Vec ( Index )
         Matrix ( Index, 3 ) = Z_Vec ( Index )
   40 CONTINUE

      CALL MTXV ( Matrix, Sun_Pos, Temp )
      CALL RECLAT ( Temp, Dummy1, Dummy2, Latitude )

      CALL MTXV ( Matrix, Obs_Pos, Temp )
      CALL MTXV ( Matrix, Obs_Vel, Temp2 )

C
C   do a rotation about the y-axis
C
      Angle3 = 0.0D0
      Angle2 = ( -1.0D0*Latitude )
      Angle1 = 0.0D0

      CALL EUL2M ( Angle3, Angle2, Angle1, Axis, Axis, Axis, RMat )

      CALL MXV ( RMat, Temp, Obs_Pos )
      CALL MXV ( RMat, Temp2, Obs_Vel )

      Px = Obs_Pos ( 1 )
      Py = Obs_Pos ( 2 )
      Pz = Obs_Pos ( 3 )

      Vx = Obs_Vel ( 1 )
      Vy = Obs_Vel ( 2 )
      Vz = Obs_Vel ( 3 )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GET_ALTITUDE ( Satellite, Observer, ET, ALT )

      IMPLICIT NONE

      INTEGER               Satellite
      INTEGER               Observer
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ALT

C
C   SPICELIB functions
C
      DOUBLE PRECISION      VNORM

C
C   Local parameters
C
      CHARACTER*(*)         F1
      PARAMETER           ( F1     = '(A,F21.9)' )

      CHARACTER*(*)         F2
      PARAMETER           ( F2     = '(A)' )

      INTEGER               Voyager1
      INTEGER               Voyager2
      INTEGER               Cassini
      INTEGER               Galileo
      INTEGER               Jupiter
      INTEGER               Io
      INTEGER               Europa
      INTEGER               Ganymede
      INTEGER               Callisto
      INTEGER               Mimas
      INTEGER               Enceladus
      INTEGER               Tethys
      INTEGER               Dione
      INTEGER               Rhea
      INTEGER               Titan
      INTEGER               Hyperion
      INTEGER               Iapetus
      INTEGER               Phoebe
      INTEGER               Helene
      INTEGER               Telesto
      INTEGER               Methone
      INTEGER               Pallene
      INTEGER               Anthe
      INTEGER               CORLEN
      INTEGER               MTHLEN

      PARAMETER           ( Voyager1  = -31 )
      PARAMETER           ( Voyager2  = -32 )
      PARAMETER           ( Cassini   = -82 )
      PARAMETER           ( Galileo   = -77 )
      PARAMETER           ( Jupiter   = 599 )
      PARAMETER           ( Io        = 501 )
      PARAMETER           ( Europa    = 502 )
      PARAMETER           ( Ganymede  = 503 )
      PARAMETER           ( Callisto  = 504 )
      PARAMETER           ( Mimas     = 601 )
      PARAMETER           ( Enceladus = 602 )
      PARAMETER           ( Tethys    = 603 )
      PARAMETER           ( Dione     = 604 )
      PARAMETER           ( Rhea      = 605 )
      PARAMETER           ( Titan     = 606 )
      PARAMETER           ( Hyperion  = 607 )
      PARAMETER           ( Iapetus   = 608 )
      PARAMETER           ( Phoebe    = 609 )
      PARAMETER           ( Helene    = 612 )
      PARAMETER           ( Telesto   = 613 )
      PARAMETER           ( Methone   = 632 )
      PARAMETER           ( Pallene   = 633 )
      PARAMETER           ( Anthe     = 649 )
      PARAMETER           ( CORLEN    =   5 )
      PARAMETER           ( MTHLEN    =  50 )

C
C   Local variables
C
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(MTHLEN)    FIXREF
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    OBSRVR
      CHARACTER*(MTHLEN)    TARGET

C
C   Initial values
C
      DATA                  ABCORR / 'LT+S' /

      IF ( Observer .EQ. Voyager1 ) THEN
         OBSRVR = 'VOYAGER 1'
      ELSE IF ( Observer .EQ. Voyager2 ) THEN
         OBSRVR = 'VOYAGER 2'
      ELSE IF ( Observer .EQ. Galileo ) THEN
         OBSRVR = 'GALILEO ORBITER'
      ELSE IF ( Observer .EQ. Cassini ) THEN
         OBSRVR = 'CASSINI'
      ELSE IF ( Observer .EQ. Jupiter ) THEN
         OBSRVR = 'JUPITER'
      ENDIF

      IF ( Satellite .EQ. Io ) THEN
         TARGET = 'IO'
         FIXREF = 'IAU_IO'
      ELSE IF ( Satellite .EQ. Europa ) THEN
         TARGET = 'EUROPA'
         FIXREF = 'IAU_EUROPA'
      ELSE IF ( Satellite .EQ. Ganymede ) THEN
         TARGET = 'GANYMEDE'
         FIXREF = 'IAU_GANYMEDE'
      ELSE IF ( Satellite .EQ. Callisto ) THEN
         TARGET = 'CALLISTO'
         FIXREF = 'IAU_CALLISTO'
      ELSE IF ( Satellite .EQ. Mimas ) THEN
         TARGET = 'MIMAS'
         FIXREF = 'IAU_MIMAS'
      ELSE IF ( Satellite .EQ. Enceladus ) THEN
         TARGET = 'ENCELADUS'
         FIXREF = 'IAU_ENCELADUS'
      ELSE IF ( Satellite .EQ. Tethys ) THEN
         TARGET = 'TETHYS'
         FIXREF = 'IAU_TETHYS'
      ELSE IF ( Satellite .EQ. Dione ) THEN
         TARGET = 'DIONE'
         FIXREF = 'IAU_DIONE'
      ELSE IF ( Satellite .EQ. Rhea ) THEN
         TARGET = 'RHEA'
         FIXREF = 'IAU_RHEA'
      ELSE IF ( Satellite .EQ. Titan ) THEN
         TARGET = 'TITAN'
         FIXREF = 'IAU_TITAN'
      ELSE IF ( Satellite .EQ. Hyperion ) THEN
         TARGET = 'HYPERION'
         FIXREF = 'IAU_HYPERION'
      ELSE IF ( Satellite .EQ. Iapetus ) THEN
         TARGET = 'IAPETUS'
         FIXREF = 'IAU_IAPETUS'
      ELSE IF ( Satellite .EQ. Phoebe ) THEN
         TARGET = 'PHOEBE'
         FIXREF = 'IAU_PHOEBE'
      ELSE IF ( Satellite .EQ. Helene ) THEN
         TARGET = 'HELENE'
         FIXREF = 'IAU_HELENE'
      ELSE IF ( Satellite .EQ. Telesto ) THEN
         TARGET = 'TELESTO'
         FIXREF = 'IAU_TELESTO'
      ELSE IF ( Satellite .EQ. Methone ) THEN
         TARGET = 'METHONE'
         FIXREF = 'IAU_METHONE'
      ELSE IF ( Satellite .EQ. Pallene ) THEN
         TARGET = 'PALLENE'
         FIXREF = 'IAU_PALLENE'
      ELSE IF ( Satellite .EQ. Anthe ) THEN
         TARGET = 'ANTHE'
         FIXREF = 'IAU_ANTHE'
      ENDIF

C
C   Compute the sub-spacecraft point using the
C   "NEAR POINT: ELLIPSOID" definition.
C   Compute the results using the LT+S
C   aberration correction.
C
      METHOD = 'Near point: ellipsoid'

      CALL SUBPNT ( METHOD,
     +              TARGET, ET, FIXREF, ABCORR,
     +              OBSRVR, SPOINT, TRGEPC, SRFVEC )

C
C   Compute the observer's altitude above SPOINT.
C
      ALT = VNORM ( SRFVEC )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GET_ALTITUDE_S ( Satellite, Observer, ET, ALT, Radius )

      IMPLICIT NONE

      INTEGER               Satellite
      INTEGER               Observer
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      Radius

C
C   SPICELIB functions
C
      DOUBLE PRECISION      VNORM

C
C   Local function
C
      DOUBLE PRECISION      GET_EQUATORIAL_RADII

C
C   Local parameters
C
      INTEGER               Cassini
      INTEGER               Saturn
      INTEGER               Enceladus
      INTEGER               Titan
      INTEGER               CORLEN
      INTEGER               MTHLEN

      PARAMETER           ( Cassini   = -82 )
      PARAMETER           ( Saturn    = 699 )
      PARAMETER           ( Enceladus = 602 )
      PARAMETER           ( Titan     = 606 )
      PARAMETER           ( CORLEN    =   5 )
      PARAMETER           ( MTHLEN    =  50 )

C
C   Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      RSaturn
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SPOINT    ( 3 )
      DOUBLE PRECISION      SRFVEC    ( 3 )
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Position  ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(MTHLEN)    FIXREF
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    OBSRVR
      CHARACTER*(MTHLEN)    TARGET

C
C   Initial values
C
      DATA                  ABCORR / 'LT+S' /

      IF ( Observer .EQ. Cassini ) THEN
         OBSRVR = 'CASSINI'
      ENDIF

      IF ( Satellite .EQ. Saturn ) THEN
         TARGET = 'SATURN'
         FIXREF = 'IAU_SATURN'
      ELSE IF ( Satellite .EQ. Enceladus ) THEN
         TARGET = 'ENCELADUS'
         FIXREF = 'IAU_ENCELADUS'
      ELSE IF ( Satellite .EQ. Titan ) THEN
         TARGET = 'TITAN'
         FIXREF = 'IAU_TITAN'
      ENDIF

C
C   Compute the sub-spacecraft point using the
C   "NEAR POINT: ELLIPSOID" definition.
C   Compute the results using the LT+S
C   aberration correction.
C
      METHOD = 'Near point: ellipsoid'

      CALL SUBPNT ( METHOD,
     +              TARGET, ET, FIXREF, ABCORR,
     +              OBSRVR, SPOINT, TRGEPC, SRFVEC )

C
C   Compute the observer's altitude above SPOINT.
C
      ALT = VNORM ( SRFVEC )

C
C   determine radial distance
C
      RSaturn = GET_EQUATORIAL_RADII ( Satellite )

      CALL SPKEZ ( Satellite, ET, 'J2000', 'LT+S', Observer, State, LT )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Position )

      CALL RECLAT ( Position, Radius, Dummy1, Dummy2 )

      Radius = ( Radius/RSaturn )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_ALTITUDE_J ( Satellite, Observer, ET, ALT, Radius )

      IMPLICIT NONE

      INTEGER               Satellite
      INTEGER               Observer
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      Radius

C
C   SPICELIB functions
C
      DOUBLE PRECISION      VNORM

C
C   Local parameters
C
      INTEGER               Juno
      INTEGER               Jupiter
      INTEGER               CORLEN
      INTEGER               MTHLEN

      PARAMETER           ( Juno      = -61 )
      PARAMETER           ( Jupiter   = 599 )
      PARAMETER           ( CORLEN    =   5 )
      PARAMETER           ( MTHLEN    =  50 )

C
C   Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SPOINT    ( 3 )
      DOUBLE PRECISION      SRFVEC    ( 3 )
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Position  ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(MTHLEN)    FIXREF
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    OBSRVR
      CHARACTER*(MTHLEN)    TARGET

C
C   Initial values
C
      DATA                  ABCORR / 'NONE' /

      IF ( Observer .EQ. Juno ) THEN
         OBSRVR = 'JUNO'
      ENDIF

      IF ( Satellite .EQ. Jupiter ) THEN
         TARGET = 'JUPITER'
         FIXREF = 'IAU_JUPITER'
      ENDIF

C
C   Compute the sub-spacecraft point using the
C   "NEAR POINT: ELLIPSOID" definition.
C   Compute the results using the LT+S
C   aberration correction.
C
      METHOD = 'Near point: ellipsoid'

      CALL SUBPNT ( METHOD,
     +              TARGET, ET, FIXREF, ABCORR,
     +              OBSRVR, SPOINT, TRGEPC, SRFVEC )

C
C   Compute the observer's altitude above SPOINT.
C
      ALT = VNORM ( SRFVEC )

C
C   determine radial distance
C
      CALL SPKEZ ( Satellite, ET, 'J2000', 'NONE', Observer, State, LT )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Position )

      CALL RECLAT ( Position, Radius, Dummy1, Dummy2 )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GET_SOLAR_ECLIPTIC ( Observer, Target, ET,
     +                                Px, Py, Pz, Vx, Vy, Vz )

      IMPLICIT NONE
      
      INTEGER               Observer
      INTEGER               Target
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Px, Py, Pz
      DOUBLE PRECISION      Vx, Vy, Vz

C
C  Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      Dummy
      DOUBLE PRECISION      TempP1    ( 3 )
      DOUBLE PRECISION      TempV1    ( 3 )
      DOUBLE PRECISION      Position  ( 3 )
      DOUBLE PRECISION      Velocity  ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Matrix ( 3, 3 )

C-----------------------------------------------------------------------
      CALL SPKEZ ( Observer, ET, 'J2000', 'NONE', Target, State, Dummy )

      DO 10 Index = 1, 3
         TempP1 ( Index ) = State ( Index )
         TempV1 ( Index ) = State ( ( Index + 3 ) )
   10 CONTINUE

      CALL GSETRN_G ( Target, ET, 'J2000', 'NONE', Matrix )

      CALL MXV ( Matrix, TempP1, Position )

      CALL MXV ( Matrix, TempV1, Velocity )

      Px = position ( 1 )
      Py = position ( 2 )
      Pz = position ( 3 )

      Vx = Velocity ( 1 )
      Vy = Velocity ( 2 )
      Vz = Velocity ( 3 )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GET_SOLAR_EQ ( Observer, Target, ET,
     +                          Px, Py, Pz, Vx, Vy, Vz )

      IMPLICIT NONE
      
      INTEGER               Observer
      INTEGER               Target
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Px, Py, Pz
      DOUBLE PRECISION      Vx, Vy, Vz

C
C  Local parameters
C
      INTEGER               Sun

      PARAMETER           ( Sun      =   10 )

C
C  Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy
      DOUBLE PRECISION      Temp      ( 3 )
      DOUBLE PRECISION      TempP1    ( 3 )
      DOUBLE PRECISION      TempV1    ( 3 )
      DOUBLE PRECISION      Sun_Pos   ( 3 )
      DOUBLE PRECISION      SC_Pos    ( 3 )
      DOUBLE PRECISION      SC_Vel    ( 3 )
      DOUBLE PRECISION      X_Vec     ( 3 )
      DOUBLE PRECISION      Y_Vec     ( 3 )
      DOUBLE PRECISION      Z_Vec     ( 3 )
      DOUBLE PRECISION      Position  ( 6 )
      DOUBLE PRECISION      Velocity  ( 6 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Tipm   ( 3, 3 )
      DOUBLE PRECISION      Matrix ( 3, 3 )

C-----------------------------------------------------------------------
      CALL SPKEZ ( Observer, ET, 'J2000', 'NONE', Target, State, Dummy )

      DO 30 Index = 1, 3
         SC_Pos ( Index ) = State ( Index )
         SC_Vel ( Index ) = State ( ( Index + 3 ) )
   30 CONTINUE

      CALL SPKEZ ( Sun, ET, 'J2000', 'NONE', Target, State, Dummy )

      DO 40 Index = 1, 3
         Temp ( Index ) = State ( Index )
   40 CONTINUE

      CALL VHAT ( Temp, Sun_Pos )
   
      CALL BODMAT ( Target, ET, TIPM )

      DO 50 Index = 1, 3
         X_Vec ( Index ) = Sun_Pos ( Index )
         Z_Vec ( Index ) = Tipm ( 3, Index )
   50 CONTINUE
   
      CALL UCRSS ( Z_Vec, X_Vec, Y_Vec )
      CALL UCRSS ( Y_Vec, Z_Vec, X_Vec )

      DO 60 Index = 1, 3
         Matrix ( Index, 1 ) = X_Vec ( Index )
         Matrix ( Index, 2 ) = Y_Vec ( Index )
         Matrix ( Index, 3 ) = Z_Vec ( Index )
   60 CONTINUE

      CALL MTXV ( Matrix, SC_Pos, Position )

      Px = position ( 1 )
      Py = position ( 2 )
      Pz = position ( 3 )

      CALL MTXV ( Matrix, SC_Vel, Velocity )

      Vx = Velocity ( 1 )
      Vy = Velocity ( 2 )
      Vz = Velocity ( 3 )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION GET_EQUATORIAL_RADII ( Target )

      IMPLICIT NONE

      INTEGER           Target

C
C   local variables
C
      INTEGER           ArrayDimension
      INTEGER           R1, R2, R3
      DOUBLE PRECISION  Equatorial_Radii
      DOUBLE PRECISION  Value ( 3 )

C-----------------------------------------------------------------------
      ArrayDimension = 3

      CALL BODVAR ( Target, 'RADII', ArrayDimension, Value )

      R1 = IDINT ( ( Value ( 1 )*10000.0D0 ) )
      R2 = IDINT ( ( Value ( 2 )*10000.0D0 ) )
      R3 = IDINT ( ( Value ( 3 )*10000.0D0 ) )

      IF ( ( R1 .EQ. R2 ) .AND. ( R1 .EQ. R3 ) ) THEN
         Equatorial_Radii = Value ( 1 )
      ELSE IF ( ( R1 .EQ. R2 ) .AND. ( R1 .NE. R3 ) ) THEN
         Equatorial_Radii = ( Value ( 1 ) + Value ( 2 ) )
         Equatorial_Radii = ( Equatorial_Radii / 2.0D0 )
      ELSE IF ( ( R1 .NE. R2 ) .AND. ( R1 .NE. R3 ) ) THEN
         Equatorial_Radii = ( Value ( 1 ) + Value ( 2 ) + Value ( 3 ) )
         Equatorial_Radii = ( Equatorial_Radii / 3.0D0 )
      ENDIF

      GET_EQUATORIAL_RADII = Equatorial_Radii

C-----------------------------------------------------------------------
      RETURN
      END
