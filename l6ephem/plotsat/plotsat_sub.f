C-----------------------------------------------------------------------
      SUBROUTINE GETCOROT ( Observer, Target, CoPlanet, ET, Px, Py, Pz )
      
      IMPLICIT NONE
      
      INTEGER               Observer
      INTEGER               Target
      INTEGER               CoPlanet
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Px, Py, Pz

C
C  Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      SC_Pos    ( 6 )
      DOUBLE PRECISION      Pos       ( 3 )
      DOUBLE PRECISION      Pos1      ( 3 )
      DOUBLE PRECISION      Temp      ( 3 )
      DOUBLE PRECISION      R_Vec     ( 3 )
      DOUBLE PRECISION      Y_Vec     ( 3 )
      DOUBLE PRECISION      Omega_Vec ( 3 )
      DOUBLE PRECISION      Phi_Vec   ( 3 )
      DOUBLE PRECISION      Tipm   ( 3, 3 )
      DOUBLE PRECISION      Matrix ( 3, 3 )
      DOUBLE PRECISION      LT

C-----------------------------------------------------------------------
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
      CALL BODMAT ( CoPlanet, (ET - LT), Tipm )

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

      CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', Observer, SC_Pos, LT )

      DO 40 Index = 1, 3
         Pos ( Index ) = SC_Pos ( Index )
   40 CONTINUE

      CALL VMINUS ( Pos, Pos1 )
      CALL MTXV ( Matrix, Pos1, Pos )

      Px = Pos ( 1 )
      Py = Pos ( 2 )
      Pz = Pos ( 3 )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GET_SOLAR_EQ ( Observer, Target, ET, Px, Py, Pz )
      
      IMPLICIT NONE
      
      INTEGER               Observer
      INTEGER               Target
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Px, Py, Pz

C
C  Local parameters
C
      INTEGER               Sun
      INTEGER               Venus
      INTEGER               Earth
      INTEGER               Jupiter
      INTEGER               Saturn

      PARAMETER           ( Sun      =   10 )
      PARAMETER           ( Venus    =  299 )
      PARAMETER           ( Earth    =  399 )
      PARAMETER           ( Jupiter  =  599 )
      PARAMETER           ( Saturn   =  699 )

C
C  Local variables
C
      INTEGER               Index
      INTEGER               BodyName
      DOUBLE PRECISION      Dummy
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Temp      ( 3 )
      DOUBLE PRECISION      X_Vec     ( 3 )
      DOUBLE PRECISION      Y_Vec     ( 3 )
      DOUBLE PRECISION      Z_Vec     ( 3 )
      DOUBLE PRECISION      Pos       ( 3 )
      DOUBLE PRECISION      SC_Pos    ( 3 )
      DOUBLE PRECISION      Sun_Pos   ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Tipm   ( 3, 3 )
      DOUBLE PRECISION      Matrix ( 3, 3 )

C-----------------------------------------------------------------------
      IF      ( Target .EQ. 2) THEN
         BodyName = Venus
      ELSE IF ( Target .EQ. 3) THEN
         BodyName = Earth
      ELSE IF ( Target .EQ. 5) THEN
         BodyName = Jupiter
      ELSE IF ( Target .EQ. 6) THEN
         BodyName = Saturn
      ELSE IF ( Target .GE. 100) THEN
         BodyName = Target
      ENDIF

      CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', Observer, State, LT )

      DO 10 Index = 1, 3
         Temp ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp, SC_Pos )

      CALL SPKEZ ( Sun, ( ET - LT ), 'J2000', 'NONE', Target, State,
     +             Dummy )

      DO 20 Index = 1, 3
         Temp ( Index ) = State ( Index )
   20 CONTINUE

      CALL VHAT ( Temp, Sun_Pos )
   
      CALL BODMAT ( BodyName, ( ET - LT ), TIPM )

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

      CALL MTXV ( Matrix, SC_Pos, Pos )

      Px = Pos ( 1 )
      Py = Pos ( 2 )
      Pz = Pos ( 3 )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GET_SOLAR_ECLIPTIC ( Observer, Target, ET, Px, Py, Pz,
     +                                                      Vx, Vy, Vz )

      IMPLICIT NONE
      
      INTEGER               Observer
      INTEGER               Target
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Px, Py, Pz
      DOUBLE PRECISION      Vx, Vy, Vz

C
C  Local parameter
C
      INTEGER               Sun

      PARAMETER           ( Sun      =   10 )

C
C  Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy
      DOUBLE PRECISION      TempP1    ( 3 )
      DOUBLE PRECISION      TempP2    ( 3 )
      DOUBLE PRECISION      TempV1    ( 3 )
      DOUBLE PRECISION      TempV2    ( 3 )
      DOUBLE PRECISION      Position  ( 3 )
      DOUBLE PRECISION      Velocity  ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Matrix ( 3, 3 )

      IF ( Target .EQ. Sun ) THEN
         CALL SPKEZ ( Sun, ET, 'ECLIPJ2000', 'NONE', Target, State,
     +                Dummy )
      ELSE
         CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', Observer, State, LT )
      ENDIF

      DO 10 Index = 1, 3
         TempP1 ( Index ) = State ( Index )
         TempV1 ( Index ) = State ( ( Index + 3 ) )
   10 CONTINUE

      IF ( Target .EQ. Sun ) THEN
         Px = TempP1 ( 1 )
         Py = TempP1 ( 2 )
         Pz = TempP1 ( 3 )

         Vx = TempV1 ( 1 )
         Vy = TempV1 ( 2 )
         Vz = TempV1 ( 3 )
      ELSE
         CALL VMINUS ( TempP1, TempP2 )

         CALL VMINUS ( TempV1, TempV2 )

         CALL GSETRN_G ( Target, ( ET - LT ), 'J2000', 'LT+S', Matrix )

         CALL MXV ( Matrix, TempP2, Position )

         CALL MXV ( Matrix, TempV2, Velocity )

         Px = Position ( 1 )
         Py = Position ( 2 )
         Pz = Position ( 3 )

         Vx = Velocity ( 1 )
         Vy = Velocity ( 2 )
         Vz = Velocity ( 3 )
      ENDIF

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GET_OTHERS ( Observer, Target, ET,
     +                        Radius, Latitude, Longitude, LocalTime )
      
      IMPLICIT NONE
      
      INTEGER               Observer
      INTEGER               Target
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Radius
      DOUBLE PRECISION      Latitude
      DOUBLE PRECISION      Longitude
      DOUBLE PRECISION      LocalTime

C
C   Local parameters
C
      INTEGER               Sun
      INTEGER               Venus
      INTEGER               Earth
      INTEGER               Jupiter
      INTEGER               Saturn

      PARAMETER           ( Sun      =   10 )
      PARAMETER           ( Venus    =  299 )
      PARAMETER           ( Earth    =  399 )
      PARAMETER           ( Jupiter  =  599 )
      PARAMETER           ( Saturn   =  699 )

C
C   SPICE functions
C
      DOUBLE PRECISION DPR
      DOUBLE PRECISION TWOPI

C
C   Local variables
C
      INTEGER               Index
      INTEGER               BodyName
      DOUBLE PRECISION      Sun_Long
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Temp2     ( 3 )
      DOUBLE PRECISION      SC_Pos    ( 3 )
      DOUBLE PRECISION      Sun_Pos   ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Tipm   ( 3, 3 )

C-----------------------------------------------------------------------
C
C   determine geographic longitude and latitude
C
      IF      ( Target .EQ. 2) THEN
         BodyName = Venus
      ELSE IF ( Target .EQ. 3) THEN
         BodyName = Earth
      ELSE IF ( Target .EQ. 5) THEN
         BodyName = Jupiter
      ELSE IF ( Target .EQ. 6) THEN
         BodyName = Saturn
      ELSE IF ( Target .GE. 100) THEN
         BodyName = Target
      ENDIF

      CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', Observer, State, LT )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )
      CALL BODMAT ( BodyName, ( ET - LT ), TIPM )
      CALL MXV ( TIPM, Temp2, SC_Pos )
      CALL RECLAT ( SC_Pos, Radius, Longitude, Latitude )

      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Latitude = ( Latitude*DPR () )

      Longitude = ( TWOPI () - Longitude )
      Longitude = ( Longitude*DPR () )

C
C   determine longitude of Sun
C
      CALL SPKEZ ( Sun, ( ET - LT ), 'J2000', 'NONE', Target, State,
     +             Dummy1 )

      DO 20 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   20 CONTINUE

      CALL MXV ( Tipm, Temp1, Sun_Pos )
      CALL RECLAT ( Sun_Pos, Dummy1, Sun_Long, Dummy2 )

      IF ( Sun_Long .LT. 0.0D0 ) THEN
         Sun_Long = ( Sun_Long + TWOPI () )
      ENDIF

      Sun_Long = ( TWOPI () - Sun_Long )
      Sun_Long = ( Sun_Long*DPR () )

C
C   determine local time
C
      LocalTime = ( Sun_Long - Longitude )
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
      SUBROUTINE GET_JOVE ( Observer, Target, BodyName, ET, Radius,
     +                      Longitude, MLatitude, LocalTime, IoPhase )
      
      IMPLICIT NONE

      INTEGER               Observer
      INTEGER               Target
      INTEGER               BodyName
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Radius
      DOUBLE PRECISION      Longitude
      DOUBLE PRECISION      MLatitude
      DOUBLE PRECISION      LocalTime
      DOUBLE PRECISION      IoPhase

C
C   Local parameters
C
      INTEGER               Sun
      INTEGER               Io
      DOUBLE PRECISION      JDIP

      PARAMETER ( Sun  =  10     )
      PARAMETER ( Io   = 501     )
      PARAMETER ( JDIP =   9.6D0 )

C
C   Local function
C
      DOUBLE PRECISION      GET_EQUATORIAL_RADII

C
C   SPICE functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI

C
C   Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      Value1
      DOUBLE PRECISION      Value2
      DOUBLE PRECISION      Value3
      DOUBLE PRECISION      ThetaSC
      DOUBLE PRECISION      ThetaGM
      DOUBLE PRECISION      ThetaSU
      DOUBLE PRECISION      IoLongitude
      DOUBLE PRECISION      SunLong
      DOUBLE PRECISION      Latitude
      DOUBLE PRECISION      RJupiter
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Temp2     ( 3 )
      DOUBLE PRECISION      Position  ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Tipm   ( 3, 3 )
      DOUBLE PRECISION      Tipm2  ( 3, 3 )

      RJupiter = GET_EQUATORIAL_RADII ( BodyName )

C
C   determine system III coordinates
C
      CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', Observer, State, LT )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )

      CALL BODMAT ( BodyName, ( ET - LT ), Tipm )

      CALL MXV ( Tipm, Temp2, Position )

      CALL RECLAT ( Position, Radius, Longitude, Latitude )

      Radius = ( Radius/RJupiter )

      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Longitude = ( TWOPI () - Longitude )
      Longitude = ( Longitude*DPR () )

      Latitude = ( Latitude*DPR () )

C
C   determine magnetic latitude
C
      Value1 = ( JDIP*RPD () )
      Value1 = DSIN ( Value1 )

      Value2 = ( Longitude - 292.0D0 )
      Value2 = ( Value2*RPD () )
      Value2 = DSIN ( Value2 )

      Value3 = ( Value1*Value2 )
      Value3 = DASIN ( Value3 )
      Value3 = ( Value3*DPR () )

      MLatitude = ( Latitude - Value3 )

C
C   determine system III longitude of Sun
C
      CALL SPKEZ ( Sun, ( ET - LT ), 'J2000', 'NONE',
     +             Target, State, Dummy1 )

      DO 20 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   20 CONTINUE

      CALL MXV ( TIPM, Temp1, Position )

      CALL RECLAT ( Position, Dummy1, SunLong, Dummy2 )

      IF ( SunLong .LT. 0.0D0 ) THEN
         SunLong = ( SunLong + TWOPI () )
      ENDIF

      SunLong = ( TWOPI () - SunLong )
      SunLong = ( SunLong*DPR () )

C
C   determine local time
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

C
C   determine phase of Io
C
C-----------------------------------------------------------------------
      CALL SPKEZ ( Observer, ( ET - LT ), 'GSE', 'NONE', Target, State,
     +             Dummy1 )

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

      CALL SPKEZ ( Io, ( ET - LT ), 'GSE', 'NONE', Target, State,
     +             Dummy1 )

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
      SUBROUTINE GET_KHRONOS ( Observer, Target, ET, CurrentTime,
     +                         Radius, Latitude, Longitude, LocalTime,
     +                         LValue, LSwitch )
      
      IMPLICIT NONE

      INTEGER               Observer
      INTEGER               Target
      INTEGER               LSwitch
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      CurrentTime
      DOUBLE PRECISION      Radius
      DOUBLE PRECISION      Latitude
      DOUBLE PRECISION      Longitude
      DOUBLE PRECISION      LocalTime
      DOUBLE PRECISION      LValue
C
C   Local parameters
C
      INTEGER               Sun
      INTEGER               Saturn
      DOUBLE PRECISION      RSaturn

      PARAMETER           ( Sun      =    10 )
      PARAMETER           ( Saturn   =   699 )
      PARAMETER           ( RSaturn  = 60268.0D0 )

C
C   SPICE functions
C
      DOUBLE PRECISION DPR
      DOUBLE PRECISION TWOPI

C
C   Local functions
C
      DOUBLE PRECISION GETLONGITUDE_SLS2
      DOUBLE PRECISION GETLONGITUDE_SLS3

C
C   Local variables
C
      INTEGER               Index
      INTEGER               BodyName
      DOUBLE PRECISION      Term1
      DOUBLE PRECISION      Term2
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      Sun_Long
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Temp2     ( 3 )
      DOUBLE PRECISION      SC_Pos    ( 3 )
      DOUBLE PRECISION      Sun_Pos   ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Tipm   ( 3, 3 )

C-----------------------------------------------------------------------
C
C   determine radius
C
      BodyName = Saturn

      CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', Observer, State, LT )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )

      CALL BODMAT ( BodyName, ( ET - LT ), TIPM )

      CALL MXV ( TIPM, Temp2, SC_Pos )

      CALL RECLAT ( SC_Pos, Radius, Longitude, Latitude )

C
C   determine L
C
      Term1 = DCOS ( Latitude )
      Term2 = ( Term1*Term1 )

      Term1 = ( Radius/RSaturn )
      LValue = ( Term1/Term2 )

C
C   determine geographic longitude and latitude
C
      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Latitude = ( Latitude*DPR () )

      Longitude = ( TWOPI () - Longitude )
      Longitude = ( Longitude*DPR () )

C
C   determine longitude of Sun
C
      CALL SPKEZ ( Sun, ( ET - LT ), 'J2000', 'NONE', Target, State,
     +             Dummy1 )

      DO 20 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   20 CONTINUE

      CALL MXV ( Tipm, Temp1, Sun_Pos )
      CALL RECLAT ( Sun_Pos, Dummy1, Sun_Long, Dummy2 )

      IF ( Sun_Long .LT. 0.0D0 ) THEN
         Sun_Long = ( Sun_Long + TWOPI () )
      ENDIF

      Sun_Long = ( TWOPI () - Sun_Long )
      Sun_Long = ( Sun_Long*DPR () )

C
C   determine local time
C
      LocalTime = ( Sun_Long - Longitude )
      LocalTime = ( LocalTime + 180.0D0 )
      LocalTime = ( LocalTime/15.0D0 )

      IF ( LocalTime .LT. 0.0D0 ) THEN
         LocalTime = ( LocalTime + 24.0D0 )
      ENDIF

      IF ( LocalTime .GE. 24.0D0 ) THEN
         LocalTime = ( LocalTime - 24.0D0 )
      ENDIF

      IF ( LSwitch .EQ. 1 ) THEN
         Longitude = GETLONGITUDE_SLS2 ( ET, CurrentTime )
      ELSE IF ( LSwitch .EQ. 2 ) THEN
         Longitude = GETLONGITUDE_SLS3 ( ET, CurrentTime )
      ENDIF

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GET_HELIOS ( Observer, Target, ET, Radius,
     +                        Latitude, Longitude )
      
      IMPLICIT NONE

      INTEGER               Observer
      INTEGER               Target
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Radius
      DOUBLE PRECISION      Latitude
      DOUBLE PRECISION      Longitude

C
C   SPICE functions
C
      DOUBLE PRECISION DPR
      DOUBLE PRECISION TWOPI

C
C   Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Temp      ( 3 )

C-----------------------------------------------------------------------
C
C   determine heliographic radial distance, longitude, and latitude
C
      CALL SPKEZ ( Observer, ET, 'ECLIPJ2000', 'LT+S',
     +             Target, State, LT )

      DO 10 Index = 1, 3
         Temp ( Index ) = State ( Index )
   10 CONTINUE

      CALL RECLAT ( Temp, Radius, Longitude, Latitude )

      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Latitude = ( Latitude*DPR () )

      Longitude = ( Longitude*DPR () )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION GETLONGITUDE_SLS2 ( ET, CurrentTime )
      
      IMPLICIT NONE
      
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      CurrentTime

C
C   SPICE functions
C
      DOUBLE PRECISION DPR
      DOUBLE PRECISION TWOPI

C
C   Local parameters
C
      INTEGER               Sun
      INTEGER               SaturnBC
      INTEGER               Saturn
      INTEGER               Cassini
      DOUBLE PRECISION      OldNewDivide
      DOUBLE PRECISION      ConstD1
      DOUBLE PRECISION      ConstD2
      DOUBLE PRECISION      ConstD3
      DOUBLE PRECISION      ConstD4
      DOUBLE PRECISION      fmodConst
      DOUBLE PRECISION      TBar

      PARAMETER           ( Sun           =            10             )
      PARAMETER           ( SaturnBC      =             6             )
      PARAMETER           ( Saturn        =           699             )
      PARAMETER           ( Cassini       =           -82             )
      PARAMETER           ( OldNewDivide  = 1451606400000.0D0         )
      PARAMETER           ( ConstD1       =          -514.11433D0     )
      PARAMETER           ( ConstD2       =             0.089561911D0 )
      PARAMETER           ( ConstD3       =             1.7424768D-3  )
      PARAMETER           ( ConstD4       =            -7.913D-7      )
      PARAMETER           ( fmodConst     =           360.0D0         )
      PARAMETER           ( TBar          =           547.0D0         )

C
C   Local variables
C
      INTEGER               Index
      LOGICAL               TestLoop
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      Longitude
      DOUBLE PRECISION      Sun_Long
      DOUBLE PRECISION      LocalTime
      DOUBLE PRECISION      OWLT
      DOUBLE PRECISION      ElapsedTime
      DOUBLE PRECISION      TPrime
      DOUBLE PRECISION      TPrimeSq
      DOUBLE PRECISION      TPrimeCu
      DOUBLE PRECISION      Term1
      DOUBLE PRECISION      Term2
      DOUBLE PRECISION      Term3
      DOUBLE PRECISION      Part1
      DOUBLE PRECISION      Part2
      DOUBLE PRECISION      RotationRate
      DOUBLE PRECISION      SunLongitude
      DOUBLE PRECISION      SCLongitude
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Temp2     ( 3 )
      DOUBLE PRECISION      SC_Pos    ( 3 )
      DOUBLE PRECISION      Sun_Pos   ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Matrix ( 3, 3 )

C-----------------------------------------------------------------------
C
C   determine spacecraft longitude to determine local time
C
      CALL SPKEZ ( SaturnBC, ET, 'J2000', 'NONE', Cassini, State, LT )

      CALL SPKEZ ( SaturnBC, ( ET - LT ), 'J2000', 'NONE', Cassini,
     +             State, Dummy1 )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )

      CALL BODMAT ( Saturn, ( ET - LT ), Matrix )

      CALL MXV ( Matrix, Temp2, SC_Pos )

      CALL RECLAT ( SC_Pos, Dummy1, Longitude, Dummy2 )

      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Longitude = ( TWOPI () - Longitude )
      Longitude = ( Longitude*DPR () )

C
C   determine system III longitude of the Sun
C
      CALL SPKEZ ( SaturnBC, ( ET - LT ), 'J2000', 'NONE', Sun,
     +             State, Dummy1 )

      DO 20 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   20 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )

      CALL MXV ( Matrix, Temp2, Sun_Pos )

      CALL RECLAT ( Sun_Pos, Dummy1, Sun_Long, Dummy2 )

      IF ( Sun_Long .LT. 0.0D0 ) THEN
         Sun_Long = ( Sun_Long + TWOPI () )
      ENDIF

      Sun_Long = ( TWOPI () - Sun_Long )
      Sun_Long = ( Sun_Long*DPR () )

C
C   determine local time
C
      LocalTime = ( Sun_Long - Longitude )
      LocalTime = ( LocalTime + 180.0D0 )
      LocalTime = ( LocalTime/15.0D0 )

      IF ( LocalTime .LT. 0.0D0 ) THEN
         LocalTime = ( LocalTime + 24.0D0 )
      ENDIF

      IF ( LocalTime .GE. 24.0D0 ) THEN
         LocalTime = ( LocalTime - 24.0D0 )
      ENDIF

      OWLT = ( LT*1000.0D0 )

      ElapsedTime = ( CurrentTime - OldNewDivide - OWLT )
      ElapsedTime = ( ElapsedTime/1000.0D0 )
      ElapsedTime = ( ElapsedTime/86400.0D0 )

      TPrime = ElapsedTime
      TPrime = ( TPrime - TBar )

      TPrimeSq = ( TPrime*TPrime )
      TPrimeCu = ( TPrime*TPrime*TPrime )

      Term1 = ( ConstD2*TPrime )
      Term2 = ( ConstD3*TPrimeSq )
      Term3 = ( ConstD4*TPrimeCu )

      Part1 = ( ConstD1 + Term1 + Term2 + Term3 )
      Part1 = ( -1.0D0*Part1 )

      RotationRate = ( 360.0D0/0.4497D0 )
      Part2 = ( RotationRate*ElapsedTime )

      SunLongitude = ( Part1 + Part2 )
      SunLongitude = ( SunLongitude + 100.0D0 )

      TestLoop = .TRUE.

      IF ( SunLongitude .LT. 0.0D0 ) THEN
         DO WHILE ( TestLoop )
            SunLongitude = ( SunLongitude + 360.0D0 )

            IF ( SunLongitude .LT. 0.0D0 ) THEN
               TestLoop = .TRUE.
            ELSE
               TestLoop = .FALSE.
            ENDIF
         ENDDO
      ENDIF

      SunLongitude = DMOD ( SunLongitude, 360.0D0 )

      Term1 = ( 12.0D0 - LocalTime )
      Term1 = ( Term1*15.0D0 )

      SCLongitude = ( SunLongitude + Term1 )

      TestLoop = .TRUE.

      IF ( SCLongitude .LT. 0.0D0 ) THEN
         DO WHILE ( TestLoop )
            SCLongitude = ( SCLongitude + 360.0D0 )

            IF ( SCLongitude .LT. 0.0D0 ) THEN
               TestLoop = .TRUE.
            ELSE
               TestLoop = .FALSE.
            ENDIF
         ENDDO
      ENDIF

      SCLongitude = DMOD ( SCLongitude, 360.0D0 )

      GETLONGITUDE_SLS2 = SCLongitude

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION GETLONGITUDE_SLS3 ( ET, CurrentTime )
      
      IMPLICIT NONE
      
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      CurrentTime

C
C   SPICE functions
C
      DOUBLE PRECISION DPR
      DOUBLE PRECISION TWOPI

C
C   Local parameters
C
      INTEGER               Sun
      INTEGER               SaturnBC
      INTEGER               Saturn
      INTEGER               Cassini
      DOUBLE PRECISION      OldNewDivide
      DOUBLE PRECISION      ConstA
      DOUBLE PRECISION      ConstB
      DOUBLE PRECISION      ConstC
      DOUBLE PRECISION      ConstD
      DOUBLE PRECISION      ConstE
      DOUBLE PRECISION      ConstF
      DOUBLE PRECISION      fmodConst


      PARAMETER           ( Sun           =            10             )
      PARAMETER           ( SaturnBC      =             6             )
      PARAMETER           ( Saturn        =           699             )
      PARAMETER           ( Cassini       =           -82             )
      PARAMETER           ( OldNewDivide  = 1451606400000.0D0         )
      PARAMETER           ( ConstA        =            86.6681D0      )
      PARAMETER           ( ConstB        =            -2.7537D0      )
      PARAMETER           ( ConstC        =             4.7730D-3     )
      PARAMETER           ( ConstD        =            -4.8755D-6     )
      PARAMETER           ( ConstE        =             3.5653D-9     )
      PARAMETER           ( ConstF        =            -9.1485D-13    )
      PARAMETER           ( fmodConst     =           360.0D0         )

C
C   Local variables
C
      INTEGER               Index
      LOGICAL               TestLoop
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      Longitude
      DOUBLE PRECISION      Sun_Long
      DOUBLE PRECISION      LocalTime
      DOUBLE PRECISION      OWLT
      DOUBLE PRECISION      ElapsedTime
      DOUBLE PRECISION      ElapsedTimeSq
      DOUBLE PRECISION      ElapsedTimeCu
      DOUBLE PRECISION      ElapsedTime4
      DOUBLE PRECISION      ElapsedTime5
      DOUBLE PRECISION      Term1
      DOUBLE PRECISION      Term2
      DOUBLE PRECISION      Term3
      DOUBLE PRECISION      Term4
      DOUBLE PRECISION      Term5
      DOUBLE PRECISION      Part1
      DOUBLE PRECISION      Part2
      DOUBLE PRECISION      RotationRate
      DOUBLE PRECISION      SunLongitude
      DOUBLE PRECISION      SCLongitude
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Temp2     ( 3 )
      DOUBLE PRECISION      SC_Pos    ( 3 )
      DOUBLE PRECISION      Sun_Pos   ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Matrix ( 3, 3 )

C-----------------------------------------------------------------------
C
C   determine spacecraft longitude to determine local time
C
      CALL SPKEZ ( SaturnBC, ET, 'J2000', 'NONE', Cassini, State, LT )

      CALL SPKEZ ( SaturnBC, ( ET - LT ), 'J2000', 'NONE', Cassini,
     +             State, Dummy1 )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )

      CALL BODMAT ( Saturn, ( ET - LT ), Matrix )

      CALL MXV ( Matrix, Temp2, SC_Pos )

      CALL RECLAT ( SC_Pos, Dummy1, Longitude, Dummy2 )

      IF ( Longitude .LT. 0.0D0 ) THEN
         Longitude = ( Longitude + TWOPI () )
      ENDIF

      Longitude = ( TWOPI () - Longitude )
      Longitude = ( Longitude*DPR () )

C
C   determine system III longitude of the Sun
C
      CALL SPKEZ ( SaturnBC, ( ET - LT ), 'J2000', 'NONE', Sun,
     +             State, Dummy1 )

      DO 20 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   20 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )

      CALL MXV ( Matrix, Temp2, Sun_Pos )

      CALL RECLAT ( Sun_Pos, Dummy1, Sun_Long, Dummy2 )

      IF ( Sun_Long .LT. 0.0D0 ) THEN
         Sun_Long = ( Sun_Long + TWOPI () )
      ENDIF

      Sun_Long = ( TWOPI () - Sun_Long )
      Sun_Long = ( Sun_Long*DPR () )

C
C   determine local time
C
      LocalTime = ( Sun_Long - Longitude )
      LocalTime = ( LocalTime + 180.0D0 )
      LocalTime = ( LocalTime/15.0D0 )

      IF ( LocalTime .LT. 0.0D0 ) THEN
         LocalTime = ( LocalTime + 24.0D0 )
      ENDIF

      IF ( LocalTime .GE. 24.0D0 ) THEN
         LocalTime = ( LocalTime - 24.0D0 )
      ENDIF

      OWLT = ( LT*1000.0D0 )

      ElapsedTime = ( CurrentTime - OldNewDivide - OWLT )
      ElapsedTime = ( ElapsedTime/1000.0D0 )
      ElapsedTime = ( ElapsedTime/86400.0D0 )

      ElapsedTimeSq = ( ElapsedTime*ElapsedTime )
      ElapsedTimeCu = ( ElapsedTime*ElapsedTime*ElapsedTime )
      ElapsedTime4 =  ( ElapsedTimeSq*ElapsedTimeSq )
      ElapsedTime5 =  ( ElapsedTimeCu*ElapsedTimeSq )

      Term1 = ( ConstB*ElapsedTime )
      Term2 = ( ConstC*ElapsedTimeSq )
      Term3 = ( ConstD*ElapsedTimeCu )
      Term4 = ( ConstE*ElapsedTime4 )
      Term5 = ( ConstF*ElapsedTime5 )

      Part1 = ( ConstA + Term1 + Term2 + Term3 + Term4 + Term5 )
      Part1 = ( -1.0D0*Part1 )

      RotationRate = ( 360.0D0/0.4497D0 )
      Part2 = ( RotationRate*ElapsedTime )

      SunLongitude = ( Part1 + Part2 )
      SunLongitude = ( SunLongitude + 100.0D0 )

      TestLoop = .TRUE.

      IF ( SunLongitude .LT. 0.0D0 ) THEN
         DO WHILE ( TestLoop )
            SunLongitude = ( SunLongitude + 360.0D0 )

            IF ( SunLongitude .LT. 0.0D0 ) THEN
               TestLoop = .TRUE.
            ELSE
               TestLoop = .FALSE.
            ENDIF
         ENDDO
      ENDIF

      SunLongitude = DMOD ( SunLongitude, 360.0D0 )

      Term1 = ( 12.0D0 - LocalTime )
      Term1 = ( Term1*15.0D0 )

      SCLongitude = ( SunLongitude + Term1 )

      TestLoop = .TRUE.

      IF ( SCLongitude .LT. 0.0D0 ) THEN
         DO WHILE ( TestLoop )
            SCLongitude = ( SCLongitude + 360.0D0 )

            IF ( SCLongitude .LT. 0.0D0 ) THEN
               TestLoop = .TRUE.
            ELSE
               TestLoop = .FALSE.
            ENDIF
         ENDDO
      ENDIF

      SCLongitude = DMOD ( SCLongitude, 360.0D0 )

      GETLONGITUDE_SLS3 = SCLongitude

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GETKSM ( Observer, ET, Px, Py, Pz, Vx, Vy, Vz )
      
      IMPLICIT NONE
      
      INTEGER               Observer
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Px, Py, Pz
      DOUBLE PRECISION      Vx, Vy, Vz

C
C  Local parameters
C
      INTEGER               Sun
      INTEGER               Saturn
      INTEGER               Cassini
      INTEGER               Axis

      PARAMETER           ( Sun      =  10 )
      PARAMETER           ( Saturn   = 699 )
      PARAMETER           ( Cassini  = -82 )
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

      CALL SPKEZ (Observer, ET, 'J2000', 'LT+S', Cassini, State, LT)

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
      SUBROUTINE GET_CEDEN_MAG_FIX ( Observer, Target, ET, Radius,
     +                               CoLatitude )
      
      IMPLICIT NONE

      INTEGER               Observer
      INTEGER               Target
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Radius
      DOUBLE PRECISION      CoLatitude
C
C   Local parameters
C
      INTEGER               Sun
      INTEGER               Saturn
      DOUBLE PRECISION      RSaturn

      PARAMETER           ( Sun      =    10 )
      PARAMETER           ( Saturn   =   699 )
      PARAMETER           ( RSaturn  = 60268.0D0 )

C
C   Local variables
C
      INTEGER               Index
      INTEGER               BodyName
      DOUBLE PRECISION      Dummy
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Temp1     ( 3 )
      DOUBLE PRECISION      Temp2     ( 3 )
      DOUBLE PRECISION      SC_Pos    ( 3 )
      DOUBLE PRECISION      State     ( 6 )
      DOUBLE PRECISION      Tipm   ( 3, 3 )

C-----------------------------------------------------------------------
C
C   determine radius and co-latitude
C
      BodyName = Saturn

      CALL SPKEZ ( Target, ET, 'J2000', 'LT+S', Observer, State, LT )

      DO 10 Index = 1, 3
         Temp1 ( Index ) = State ( Index )
   10 CONTINUE

      CALL VMINUS ( Temp1, Temp2 )

      CALL BODMAT ( BodyName, ( ET - LT ), TIPM )

      CALL MXV ( TIPM, Temp2, SC_Pos )

      CALL RECSPH ( SC_Pos, Radius, CoLatitude, Dummy )

      Radius = ( Radius/RSaturn )

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

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE RotationalAxisToKSO ( CoPlanet, ET, Px, Py, Pz )
      
      IMPLICIT NONE

      INTEGER               CoPlanet
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      Px
      DOUBLE PRECISION      Py
      DOUBLE PRECISION      Pz

C
C  Local variables
C
      DOUBLE PRECISION      RotAxisBodyFixed     ( 3 )
      DOUBLE PRECISION      RotAxisJ2000         ( 3 )
      DOUBLE PRECISION      Position             ( 3 )
      DOUBLE PRECISION      Position             ( 3 )
      DOUBLE PRECISION      Tipm              ( 3, 3 )
      DOUBLE PRECISION      Matrix            ( 3, 3 )

      RotAxisBodyFixed ( 1 ) = 0.0D0
      RotAxisBodyFixed ( 2 ) = 0.0D0
      RotAxisBodyFixed ( 3 ) = 1.0D0

      CALL BODMAT ( CoPlanet, ET, Tipm )

      CALL MTXV ( Tipm, RotAxisBodyFixed, RotAxisJ2000 )

      CALL GSETRN_G ( CoPlanet, ET, 'J2000', 'NONE', Matrix )

      CALL MXV ( Matrix, RotAxisJ2000, Position )

      Px = Position ( 1 )
      Py = Position ( 2 )
      Pz = Position ( 3 )

C-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE SC2KSM ( Observer, ET, M11, M12, M13, M21, M22, M23,
     +                    M31, M32, M33, Px, Py, Pz )
      
      IMPLICIT NONE
      
      INTEGER               Observer
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      M11, M12, M13
      DOUBLE PRECISION      M21, M22, M23
      DOUBLE PRECISION      M31, M32, M33
      DOUBLE PRECISION      Px, Py, Pz

C
C  Local parameters
C
      INTEGER               Sun
      INTEGER               Saturn
      INTEGER               Cassini
      INTEGER               Axis

      PARAMETER           ( Sun      =  10 )
      PARAMETER           ( Saturn   = 699 )
      PARAMETER           ( Cassini  = -82 )
      PARAMETER           ( Axis     =   2 )
      
C
C  Local variables
C
      INTEGER               Index
      DOUBLE PRECISION      State ( 6 )
      DOUBLE PRECISION      Temp ( 3 )
      DOUBLE PRECISION      Sun_Pos ( 3 )
      DOUBLE PRECISION      Obs_Pos ( 3 )
      DOUBLE PRECISION      X_Vec ( 3 )
      DOUBLE PRECISION      Y_Vec ( 3 )
      DOUBLE PRECISION      Z_Vec ( 3 )
      DOUBLE PRECISION      Tipm ( 3, 3 )
      DOUBLE PRECISION      Matrix ( 3, 3 )
      DOUBLE PRECISION      RMat ( 3, 3 )
      DOUBLE PRECISION      AMat ( 3, 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      Dummy1
      DOUBLE PRECISION      Dummy2
      DOUBLE PRECISION      Latitude
      DOUBLE PRECISION      Angle1
      DOUBLE PRECISION      Angle2
      DOUBLE PRECISION      Angle3

C-----------------------------------------------------------------------
      AMat ( 1, 1 ) = M11
      AMat ( 1, 2 ) = M12
      AMat ( 1, 3 ) = M13

      AMat ( 2, 1 ) = M21
      AMat ( 2, 2 ) = M22
      AMat ( 2, 3 ) = M23

      AMat ( 3, 1 ) = M31
      AMat ( 3, 2 ) = M32
      AMat ( 3, 3 ) = M33

      Temp ( 1 ) = 1.0D0
      Temp ( 2 ) = 0.0D0
      Temp ( 3 ) = 0.0D0

      CALL MTXV ( AMat, Temp, Obs_Pos )

      CALL SPKEZ ( Sun, ET, 'J2000', 'NONE', Saturn, State, Dummy1 )

      DO 10 Index = 1, 3
         Temp ( Index ) = State ( Index )
   10 CONTINUE

      CALL VHAT ( Temp, Sun_Pos )

      CALL SPKEZ ( Observer, ET, 'J2000', 'LT+S', Cassini, State, LT )

      CALL BODMAT ( Saturn, ( ET - LT ), Tipm )

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

C
C   do a rotation about the y-axis
C
      Angle3 = 0.0D0
      Angle2 = ( -1.0D0*Latitude )
      Angle1 = 0.0D0

      CALL EUL2M ( Angle3, Angle2, Angle1, Axis, Axis, Axis, RMat )

      CALL MXV ( RMat, Temp, Obs_Pos )

      Px = Obs_Pos ( 1 )
      Py = Obs_Pos ( 2 )
      Pz = Obs_Pos ( 3 )

C-----------------------------------------------------------------------
      RETURN
      END
