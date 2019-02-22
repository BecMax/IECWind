!************* source file iecwind.f90 ***************

! Program to produce user-specified IEC extreme wind condition
!  files (*.WND) that conform to IEC 61400-1 Edition 3 and
!  IEC 61400-3 Edition 1 and are compatible with AeroDyn

! Written by Univ of Utah; Formerly maintained by Windward Engineering, LC
! Now maintained by NREL/NWTC.

! See changes.log for history of modifications.

Module Params

Implicit       NONE

INTEGER        NumWindFile		!Number of wind files to generate
INTEGER        WTC				!Wind Turbine class

!mlb
REAL        :: Alpha          ! Wind shear exponent
REAL           Dia				!Rotor diameter
REAL         ::DT = 0.1			!Time step for the transient
REAL           HH				!Hub-height
REAL         ::LenConvert = 1.0	!conversion for length units
REAL         ::PI  = 3.14159	!Universal constant; close enough for this program
REAL           Slope			!Slope of the inflow
REAL           T1				!Time at which transient starts
REAL           TurbI			!Turbulence intensity
REAL           TurbRat			!Turbulence ratio
REAL           TurbScale		!Turbulence scale
REAL         ::VCG = 15			!Coherent gust specified as 15 m/s
REAL           Ve1				!1-year reference extreme wind speed
REAL           Ve50				!50-year reference extreme wind speed
REAL           Vin				!Cut-in wind speed
REAL           Vout				!Cut-out wind speed
REAL           Vrated			!Rated wind speed
REAL           Vref(3)

CHARACTER( 1)  CATG				!Turbulence category
CHARACTER( 1)::TAB = CHAR(9)	!TAB character
CHARACTER( 9)  WindFile(150)    !String indicating the IEC condition(s) to generate
CHARACTER(21)::VERSION = '5.01.02 (13-Sep-2012)' ! Program version stamp

LOGICAL        SIUnit           !Units logical

DATA Vref /50., 42.5, 37.5/

END Module Params


!------------------------------------------------------------------
PROGRAM IECWind


USE            Params

Implicit       NONE



! Read the input file
CALL ReadInput ('IEC.IPT')

! Calculate some variables
CALL CalcVars

! Generate the wind file(s)
CALL GenWindCond

PRINT*,NumWindFile,' wind files generated.'


END


!-----------------------------------------------------------------
SUBROUTINE ReadInput (FileName)

! Read the input file.

USE            Params

IMPLICIT       NONE


! Passed variables

CHARACTER( *)  FileName


! Local variables
INTEGER        IERR
INTEGER      ::IFile = 12		!Input file unit number
INTEGER        IOS
!mlb Start of proposed change.
INTEGER        Stand           ! Standard to use for wind shear.
!mlb End of proposed change.

LOGICAL        EXISTS

CHARACTER(80)  LINE


! Check for the existence of the input file.

INQUIRE ( FILE=FileName , EXIST=EXISTS )

IF ( .NOT. EXISTS )  THEN
   PRINT*,' Cannot find file "',FileName,'" in the current directory.'
   STOP
ENDIF

OPEN (UNIT=IFile, FILE=FileName, STATUS='OLD', IOSTAT=IERR)

! Skip the header line
READ (IFile,'(A)',IOSTAT=IOS) LINE

IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'file header line 1' )
ENDIF

READ (IFile,'(A)',IOSTAT=IOS) LINE

IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'file header line 2' )
ENDIF

! Read in the unit specifier: SI or English
READ (IFile,'(L2)',IOSTAT=IOS) SIUnit

IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'units specifier' )
ENDIF

! Apply conversion factor for English units
IF (.NOT. SIUnit) THEN
   LenConvert = 3.2808
ENDIF

! Read in the starting time for the transient (steady wind duration)
READ(IFile,*,IOSTAT=IOS) T1
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'start of transient' )
ENDIF

! Skip the header line
READ (IFile,'(A)',IOSTAT=IOS) LINE

IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'line 5 header' )
ENDIF

! Read in the wind turbine class: 1, 2, or 3
READ(IFile,*,IOSTAT=IOS) WTC
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'wind turbine class' )
ENDIF


! Check that the wind turbine class is 1, 2, or 3
IF (WTC .GT. 3 .OR. WTC .LT. 1) THEN
   !Write(*,'(A,I1,A)'),'The turbine class read was: "',WTC,'".'
   PRINT*,'The turbine class must be 1, 2, or 3.'
   PRINT*,'Check your input file.'
   STOP
ENDIF

! Read in the wind turbine category: A, B or C
READ(IFile,"(A1)",IOSTAT=IOS) CATG
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'wind turbulence class' )
ENDIF

! Set the turbulence intensity and slope parameters for the category
IF(CATG .EQ. 'A'.or. CATG .EQ. 'a') THEN
   TurbI = 0.16
ELSEIF(CATG .EQ. 'B' .or. CATG .EQ. 'b') THEN
   TurbI = 0.14
ELSEIF(CATG .EQ. 'C' .or. CATG .EQ. 'c') THEN
   TurbI = 0.12
ELSE
   PRINT*,'The turbulence category read was: "'//CATG//'".'
   PRINT*,'The turbulence category must be A, B, or C.'
   PRINT*,'Check your input file.'
   STOP
ENDIF

! Read in the inflow angle
READ(IFile,*,IOSTAT=IOS) SLOPE
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'wind inclination angle' )
ENDIF
! Check if the slope is within bounds
IF(ABS(SLOPE) .GT. 8.0) THEN
   PRINT*,'----------------------- WARNING -----------------------'
   PRINT*,'The IEC specifies a maximum inclination angle of 8 deg.'
   !WRITE(*,'(A,F7.2,A)'),' You have specified ',SLOPE,' degrees.'
   PRINT*,'-------------------------------------------------------'
ENDIF
! Convert to radians
SLOPE = SLOPE * PI / 180.
!mlb Start of proposed change.

! Read in the IEC standard indicator
READ(IFile,*,IOSTAT=IOS) Stand
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'IEC standard used for wind shear exponent' )
ENDIF
! Check if the standard is 1 or 3.
SELECT CASE ( Stand )
CASE ( 1 )
   Alpha = 0.2
CASE ( 3 )
   Alpha = 0.14
CASE DEFAULT
   PRINT*,'----------------------- WARNING -----------------------'
   PRINT*,'You must specify either "1" or "3" for the IEC standard to use for wind shear exponent.'
   !WRITE(*,'(A,I2,A)'),' You have specified ',Stand,'.'
   PRINT*,'-------------------------------------------------------'
END SELECT
!mlb End of proposed change.

! Skip the header line
READ (IFile,'(A)',IOSTAT=IOS) LINE

IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'line 9 header' )
ENDIF

! Read the hub-height
READ(12,*,IOSTAT=IOS) HH
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'hub-height' )
ENDIF

! Read in the diameter
READ(12,*,IOSTAT=IOS) DIA
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'turbine diameter' )
ENDIF

!Check that the turbine hub-height is at least as large as the diameter
IF (HH .LE. DIA/2.0)THEN
   PRINT*,'The hub-height should be greater than the turbine diameter.'
   !Write(*,'(A,F6.2,A)'),' The hub-height is ',HH,'.'
   !Write(*,'(A,F6.2,A)'),' The diameter is ',Dia,'.'
   PRINT*,'Check your input file.'
   STOP
ENDIF

!Check that the turbine diameter is greater than zero
IF(DIA .LE. 0.0)THEN
   PRINT*,'The diamater should be greater than zero.'
   !Write(*,'(A,F6.2,A)'),' The diameter is ',Dia,'.'
   PRINT*,'Check your input file.'
   STOP
ENDIF

!Convert to SI units
HH = HH / LenConvert
DIA = DIA / LenConvert

! Read in the cut-in wind speed
READ(12,*,IOSTAT=IOS) Vin
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'cut-in wind speed' )
ENDIF
! Read in the rated wind speed
READ(12,*,IOSTAT=IOS) Vrated
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'rated wind speed' )
ENDIF
! Read in the cut-out wind speed
READ(12,*,IOSTAT=IOS) Vout
IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'cut-out wind speed' )
ENDIF

!Check that the wind speeds are logical
IF (Vrated .LE. Vin)THEN
   PRINT*,'The rated wind speed should be greater than cut-in.'
   PRINT*,'The rated speed read is ',Vrated,'.'
   PRINT*,'The cut-in speed read is ',Vin,'.'
   PRINT*,'Check your input file.'
   STOP
ENDIF
IF (Vout .LE. Vrated)THEN
   PRINT*,'The rated wind speed should be less than cut-out.'
   PRINT*,'The rated speed read is ',Vrated,'.'
   PRINT*,'The cut-out speed read is ',Vout,'.'
   PRINT*,'Check your input file.'
   STOP
ENDIF
IF (Vout .LE. Vin)THEN
   PRINT*,'The cut-out wind speed should be greater than cut-in.'
   PRINT*,'The cut-out speed read is ',Vout,'.'
   PRINT*,'The cut-in speed read is ',Vin,'.'
   PRINT*,'Check your input file.'
   STOP
ENDIF

!Convert to SI units
Vin    = Vin    / LenConvert
Vrated = Vrated / LenConvert
Vout   = Vout   / LenConvert

! Skip the header line
READ (IFile,'(A)',IOSTAT=IOS) LINE

IF ( IOS < 0 )  THEN
   CALL PremEOF ( FileName , 'line 15 header' )
ENDIF



! Read in the IEC conditions to generate wind files for
NumWindFile = 1
DO

   READ(IFile, '(A)', IOSTAT=IOS) WindFile(NumWindFile)
   IF ( LEN (TRIM(WindFile(NumWindFile))) == 0 .OR. IOS < 0 ) THEN
      NumWindFile = NumWindFile - 1
      IF (NumWindFile == 0 )  THEN
         CALL PremEOF ( FileName , 'first IEC condition' )
      ENDIF
      PRINT*,'Number of requested wind files = ', NumWindFile
      EXIT
   ENDIF

   CALL Conv2UC (WindFile(NumWindFile))

   NumWindFile = NumWindFile + 1

END DO


RETURN
END SUBROUTINE ReadInput

!-----------------------------------------------------------------
SUBROUTINE CalcVars

! Calculate variables used in calculating wind transients.

USE            Params

IMPLICIT       NONE


! Passed variables



! Local variables


! Calculate the turbulence scale
IF(HH .LT. 60)TurbScale = 0.7*HH
IF(HH .GE. 60)TurbScale = 42.
! Calculate the turbulence ratio
TurbRat = Dia/TurbScale

! Calculate the reference extreme wind speeds
Ve50 = 1.4 * Vref(WTC)
Ve1  = 0.8 * Ve50

RETURN
END SUBROUTINE CalcVars


!-----------------------------------------------------------------
SUBROUTINE GenWindCond

! Generate the wind files requested.

USE            Params

IMPLICIT       NONE


! Passed variables



! Local variables
INTEGER        I				!Generic integer for Do loops

! Choose a routine to call based on the condition
DO I = 1, NumWindFile

   SELECT CASE (WindFile(I)(1:3))
     CASE('ECD')
       CALL GenECD (I)
     CASE('EWS')
       CALL GenEWS (I)
     CASE('EOG')
       CALL GenEOG (I)
     CASE('EDC')
       CALL GenEDC (I)
     CASE('NWP')
       CALL GenNWP (I)
     CASE('EWM')
       CALL GenEWM (I)
     CASE DEFAULT
       PRINT*,'ERROR in specified wind conditions.'
       PRINT*,'Wind condition "',TRIM(WindFile(I)),'" not recognized.'
       PRINT*,'Please check your input file.'
       STOP
   END SELECT

END DO !I



RETURN
END SUBROUTINE GenWindCond


!-----------------------------------------------------------------
SUBROUTINE GenECD (FileNum)

! Generate the ECD condition.

USE            Params

IMPLICIT       NONE


! Passed variables
INTEGER        FileNum

! Local variables
REAL           Dir		!Multiplier defines the wind direction vector
REAL           SpdMod	!Modification to base wind speed
REAL           Theta    !Extreme direction change (deg)
REAL           ThetaT   !Time-varying Extreme direction change (deg)
REAL           T		!Total time for the transient
REAL           Time		!Current time for the transient
REAL           Vgust	!Gust total wind speed
REAL           VgustH	!Gust horizontal wind speed
REAL           Vhub		!Hub-height total wind speed
REAL           VhubH	!Hub-height horizontal wind speed
REAL           VhubV	!Hub-height vertical wind speed

INTEGER        FileUnit !Output file unit specifier
INTEGER        I		!Generic integer for do loops
INTEGER        NT		!Number of time steps to define the transient
INTEGER        IOS



! Determine the details of the condition to be generated
SELECT CASE (WindFile(FileNum)(4:4))
   CASE ('+')
     Dir = +1.0
   CASE ('-')
     Dir = -1.0
   CASE DEFAULT
     PRINT*,'ERROR in specified wind conditions.'
     PRINT*,'Direction specifier (4th character) must be "+" or "-".'
     PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
     PRINT*,'Please check your input file.'
     STOP
END SELECT

SELECT CASE (WindFile(FileNum)(5:5))
   CASE ('R')
     READ (WindFile(FileNum)(6:9),'(F4.1)',IOSTAT=IOS) SpdMod
     IF ( IOS < 0 )  SpdMod = 0.0
     SpdMod = SpdMod / LenConvert
     IF (ABS(SpdMod) > 2.0) THEN
        PRINT*,'ERROR in specified wind conditions.'
        PRINT*,'Wind speed increment (characters 7-9) must not exceed 2.0 m/s (6.5 fps).'
        PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
        PRINT*,'Please check your input file.'
     STOP
     ENDIF
     Vhub = Vrated + SpdMod
   CASE DEFAULT
     PRINT*,'ERROR in specified wind conditions.'
     PRINT*,'Wind speed specifier (5th character) must be "r".'
     PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
     PRINT*,'Please check your input file.'
     STOP
END SELECT


! Ready to create the wind file
PRINT*, 'Generating wind file ', TRIM(WindFile(FileNum)),'.wnd'

! Open the output file
CALL OpenOutFile (FileNum, FileUnit)


! Write out the IEC class summary headers
CALL WindHeader (FileUnit,'Extreme Coherent Gust with Direction Change')

! Set the transient time
T=10.

! Determine the number of steps for the transient
NT=NINT(T/DT) + 1

!Calculate wind speed components based on inclination angle
Vhub  = Vrated + SpdMod
VhubH = Vhub * COS(Slope)
VhubV = Vhub * SIN(Slope)

! Write out the file specfic headers
CALL TimeHeader(FileUnit,T)
CALL GustHead(FileUnit,VCG,Vhub,T1+T)

!Determine the extreme direction change
IF (Vhub .LE. 4.) THEN
   Theta=180.
ELSEIF(Vhub .GT. 4. .AND. Vhub .LE. Vref(WTC)) THEN
   Theta=720.0/Vhub
ELSE
   PRINT*, 'Error generating the direction change for EDC event.'
   PRINT*, ' Vhub should not exceed the reference wind speed.'
   PRINT*, ' Vhub = ', Vhub, ' m/s.'
   PRINT*, ' Vref = ', Vref(WTC), ' m/s.'
   STOP
ENDIF
Theta = Theta * Dir

CALL WndDirHead(FileUnit,Theta,T1+T)
CALL SlopeHead(FileUnit,SLOPE*180./PI)


! Write out column headings to the wind file
CALL ColHeaders(FileUnit)

!Write out the initial wind condition
!mlb Start of proposed change.
WRITE(FileUnit,'(8(F9.3,A1))') &
    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, Alpha, TAB, 0., TAB,0.
!    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, .2, TAB, 0., TAB,0.
!mlb End of proposed change.

DO I=1,NT

   TIME=T1 + FLOAT(I-1)*DT
   Vgust  = 0.5*VCG*(1.-COS(PI*(TIME-T1)/T))
   VgustH = Vgust * COS(SLOPE)
   VhubV  = (Vhub + Vgust) * SIN(SLOPE)
   ThetaT = 0.5*Theta*(1-COS(PI*(TIME-T1)/T))

!mlb Start of proposed change.
   WRITE(FileUnit,'(8(F9.3,A1))') &
    TIME,TAB,VhubH*LenConvert,TAB,ThetaT,TAB,VhubV*LenConvert,TAB,0.,TAB,Alpha,TAB,0.,TAB,VgustH*LenConvert
!    TIME,TAB,VhubH*LenConvert,TAB,ThetaT,TAB,VhubV*LenConvert,TAB,0.,TAB,.2,TAB,0.,TAB,VgustH*LenConvert
!mlb End of proposed change.

END DO !I

! Close the output file
CLOSE(FileUnit)

RETURN
END SUBROUTINE GenECD


!-----------------------------------------------------------------
SUBROUTINE GenEWS (FileNum)

! Generate the EWS condition.

USE            Params

IMPLICIT       NONE


! Passed variables
INTEGER        FileNum

! Local variables
!mlb Start of proposed change.
!REAL         ::Alpha = 0.2	!Shear definition constant
!mlb Start of proposed change.
REAL         ::Beta	= 6.4	!Shear definition constant
REAL           HShr		!Horizontal shear
REAL           ShrDir	!Multiplier defines the shear direction vector
REAL           ShrMax	!Multiplier defines the shear direction vector
REAL           Sig1		!Shear definition variable
REAL           T		!Total time for the transient
REAL           Time		!Current time for the transient
REAL           VG50		!Shear definition variable
REAL           Vhub		!Hub-height total wind speed
REAL           VhubH	!Hub-height horizontal wind speed
REAL           VhubV	!Hub-height vertical wind speed
REAL           VShr		!Vertical shear

INTEGER        FileUnit !Output file unit specifier
INTEGER        I		!Generic integer for do loops
INTEGER        IOS
INTEGER        NT		!Number of time steps to define the transient

LOGICAL        Hshear	!Horizontal shear logical


! Determine the details of the condition to be generated
SELECT CASE (WindFile(FileNum)(4:4))
   CASE ('V') ! Vertical shear
! Read in the hub-height wind speed
    Hshear = .False.
   CASE ('H')
! Read in the hub-height wind speed
    Hshear = .True.
   CASE DEFAULT
     PRINT*,'ERROR in specified wind conditions.'
     PRINT*,'Shear direction specifier (4th character) must be "V" or "H".'
     PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
     PRINT*,'Please check your input file.'
     STOP
END SELECT

! Determine the shear direction
SELECT CASE (WindFile(FileNum)(5:5))
   CASE ('+')
    ShrDir = +1.0
   CASE ('-')
    ShrDir = -1.0
   CASE DEFAULT
    PRINT*,'ERROR in specified wind conditions.'
    PRINT*,'Shear direction specifier (5th character) must be "+" or "-".'
    PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
    PRINT*,'Please check your input file.'
    STOP
END SELECT

! Read the hub-height wind speed
READ(WindFile(FileNum)(6:9),'(F4.1)',IOSTAT=IOS) Vhub
IF ( IOS < 0 )  THEN
    PRINT*,'ERROR in specified wind conditions.'
    PRINT*,'Wind speed should be specified as an "nn.n" number in characters 6-9.'
    PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
    PRINT*,'Please check your input file.'
    STOP
ENDIF

! Convert to SI units
Vhub = Vhub/LenConvert
IF ( Vhub < Vin .OR. Vhub > Vout )  THEN
    PRINT*,'ERROR in specified wind conditions.'
    PRINT*,'Wind speed must be greater than cut-in and less than cut-out, inclusive.'
    PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
    PRINT*,'Please check your input file.'
    STOP
ENDIF

! Ready to create the wind file
PRINT*, 'Generating wind file ', TRIM(WindFile(FileNum)),'.wnd'

! Open the output file
CALL OpenOutFile (FileNum, FileUnit)

! Write out the IEC class summary headers
IF (Hshear) THEN
   CALL WindHeader (FileUnit,'Extreme Horizontal Wind Shear')
ELSE
   CALL WindHeader (FileUnit,'Extreme Vertical Wind Shear')
ENDIF

! Set the transient time
T=12.

! Determine the number of steps for the transient
NT=NINT(T/DT) + 1

!Calculate wind speed components based on inclination angle
VhubH = Vhub * COS(Slope)
VhubV = Vhub * SIN(Slope)

! Write out the file specfic headers
CALL TimeHeader(FileUnit,T)

!Determine the extreme shear
Sig1 = TurbI*(0.75*Vhub+5.6)
VG50 = Beta*Sig1

!Start of proposed change.  2012-09-13 v5.01.02  M. Buhl
!mlb Start of proposed change.
!ShrMax=2.*(2.5+0.2*VG50*(TurbRat)**0.25)/Vhub
!v5.01.02 ShrMax=2.*(2.5+Alpha*VG50*(TurbRat)**0.25)/Vhub
ShrMax = 2.0*( 2.5 + 0.2*VG50*( TurbRat )**0.25 )/Vhub
!mlb End of proposed change.
!End of proposed change.  2012-09-13 v5.01.02  M. Buhl

CALL ShearHead(FileUnit,ShrDir,Hshear,ShrMax,T1+T)

CALL SlopeHead(FileUnit,SLOPE*180./PI)

! Write out column headings to the wind file
CALL ColHeaders(FileUnit)


!Write out the initial wind condition
!mlb Start of proposed change.
WRITE(FileUnit,'(8(F9.3,A1))') &
    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, Alpha, TAB, 0., TAB,0.
!    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, .2, TAB, 0., TAB,0.
!mlb End of proposed change.

DO I=1,NT

   TIME=T1 + FLOAT(I-1)*DT

   IF (Hshear) THEN
      Hshr = ShrDir * (0.5*ShrMax)*(1.-COS(2.*PI*(TIME-T1)/T))
      Vshr = 0.0
   ELSE
      Hshr = 0.0
      Vshr = ShrDir * (0.5*ShrMax)*(1.-COS(2.*PI*(TIME-T1)/T))
   ENDIF

!mlb Start of proposed change.
   WRITE(FileUnit,'(8(F9.3,A1))') &
    TIME,TAB,VHUBH*LenConvert,TAB,0.,TAB,VHUBV*LenConvert,TAB,Hshr,TAB,Alpha,TAB,Vshr,TAB,0.
!    TIME,TAB,VHUBH*LenConvert,TAB,0.,TAB,VHUBV*LenConvert,TAB,Hshr,TAB,.2,TAB,Vshr,TAB,0.
!mlb Start of proposed change.

END DO !I

! Close the output file
CLOSE(FileUnit)


RETURN
END SUBROUTINE GenEWS


!-----------------------------------------------------------------
SUBROUTINE GenEOG (FileNum)

! Generate the EOG condition.

USE            Params

IMPLICIT       NONE


! Passed variables
INTEGER        FileNum

! Local variables
REAL           Sig1		!Shear definition variable
REAL           SpdMod	!Hub-height wind speed modifier
REAL           T		!Total time for the transient
REAL           Time		!Current time for the transient
REAL           Vgust	!Gust total wind speed
REAL           VgustH	!Gust horizontal wind speed
REAL           Vhub		!Hub-height total wind speed
REAL           VhubH	!Hub-height horizontal wind speed
REAL           VhubV	!Hub-height vertical wind speed

INTEGER        FileUnit !Output file unit specifier
INTEGER        I		!Generic integer for do loops
INTEGER        IOS
INTEGER        NT		!Number of time steps to define the transient


! Determine the details of the condition to be generated
SELECT CASE (WindFile(FileNum)(4:4))
   CASE ('I')
     Vhub = Vin
   CASE ('O')
     Vhub = Vout
   CASE ('R')
     READ (WindFile(FileNum)(5:8),'(F4.1)',IOSTAT=IOS) SpdMod
     IF ( IOS < 0 )  THEN
         PRINT*,'ERROR in specified wind conditions.'
         PRINT*,'Wind speed modifier should be specified as an "n.n" number in characters 5-8.'
         PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
         PRINT*,'Please check your input file.'
         STOP
     ENDIF
     SpdMod = SpdMod / LenConvert
     IF (ABS(SpdMod) > 2.0) THEN
        PRINT*,'ERROR in specified wind conditions.'
        PRINT*,'Wind speed increment (characters 6-8) must not exceed 2.0 m/s (6.5 fps).'
        PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
        PRINT*,'Please check your input file.'
     STOP
     ENDIF
     Vhub = Vrated + SpdMod
   CASE DEFAULT
     PRINT*,'ERROR in specified wind conditions.'
     PRINT*,'Wind speed specifier (4th character) must be "i", "o", or "r".'
     PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
     PRINT*,'Please check your input file.'
     STOP
END SELECT


! Convert to SI units
Vhub = Vhub/LenConvert

! Ready to create the wind file
PRINT*, 'Generating wind file ', TRIM(WindFile(FileNum)),'.wnd'

! Open the output file
CALL OpenOutFile (FileNum, FileUnit)

! Write out the IEC class summary headers
CALL WindHeader (FileUnit,'Extreme Operating Gust')

! Set the transient time
T=10.5

! Determine the number of steps for the transient
NT=NINT(T/DT) + 1

!Calculate wind speed components based on inclination angle
VhubH = Vhub * COS(Slope)
VhubV = Vhub * SIN(Slope)

! Calculate the gust maximum
Sig1 = TurbI*(0.75*Vhub+5.6)
Vgust = MIN(1.35*(Ve1 - Vhub), 3.3 * (Sig1/(1 + 0.1 * TurbRat)))

! Write out the file specfic headers
CALL TimeHeader(FileUnit,T)
CALL GustHead(FileUnit,2*0.37*Vgust,Vhub,T1+0.5*T)

CALL SlopeHead(FileUnit,SLOPE*180./PI)

! Write out column headings to the wind file
CALL ColHeaders(FileUnit)


!Write out the initial wind condition
!mlb Start of proposed change.
WRITE(FileUnit,'(8(F9.3,A1))') &
    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, Alpha, TAB, 0., TAB,0.
!    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, .2, TAB, 0., TAB,0.
!mlb End of proposed change.

DO I=1,NT

   TIME=T1 + FLOAT(I-1)*DT

   VgustH = (-0.37*Vgust*SIN(3.*PI*(TIME-T1)/T)*(1-COS(2.*PI*(TIME-T1)/T))) * COS(SLOPE)
   VhubV  = (Vhub -0.37*Vgust*SIN(3.*PI*(TIME-T1)/T)*(1-COS(2.*PI*(TIME-T1)/T))) * SIN(SLOPE)

!mlb Start of proposed change.
   WRITE(FileUnit,'(8(F9.3,A1))') &
    TIME,TAB,VhubH*LenConvert,TAB,0.,TAB,VhubV*LenConvert,TAB,0.,TAB,Alpha,TAB,0.,TAB,VgustH*LenConvert
!    TIME,TAB,VhubH*LenConvert,TAB,0.,TAB,VhubV*LenConvert,TAB,0.,TAB,.2,TAB,0.,TAB,VgustH*LenConvert
!mlb End of proposed change.


END DO !I

! Close the output file
CLOSE(FileUnit)


RETURN
END SUBROUTINE GenEOG


!-----------------------------------------------------------------
SUBROUTINE GenEDC (FileNum)

! Generate the EDC condition.

USE            Params

IMPLICIT       NONE


! Passed variables
INTEGER        FileNum

! Local variables
REAL           Sig1		!Shear definition variable
REAL           SpdMod	!Hub-height wind speed modifier
REAL           T		!Total time for the transient
REAL           Time		!Current time for the transient
REAL           Dir		!Multiplier defines the wind direction vector
REAL           ThetaT   !Time-dependent Extreme direction change (deg)
REAL           Theta    !Extreme direction change (deg)
REAL           Vhub		!Hub-height total wind speed
REAL           VhubH	!Hub-height horizontal wind speed
REAL           VhubV	!Hub-height vertical wind speed

INTEGER        FileUnit !Output file unit specifier
INTEGER        I		!Generic integer for do loops
INTEGER        NT		!Number of time steps to define the transient
INTEGER        IOS



! Determine the details of the condition to be generated
SELECT CASE (WindFile(FileNum)(4:4))
   CASE ('+')
     Dir = +1.0
   CASE ('-')
     Dir = -1.0
   CASE DEFAULT
     PRINT*,'ERROR in specified wind conditions.'
     PRINT*,'Direction specifier (4th character) must be "+" or "-".'
     PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
     PRINT*,'Please check your input file.'
     STOP
END SELECT

SELECT CASE (WindFile(FileNum)(5:5))
   CASE ('I')
     Vhub = Vin
   CASE ('O')
     Vhub = Vout
   CASE ('R')
     READ (WindFile(FileNum)(6:9),'(F4.1)',IOSTAT=IOS) SpdMod
     IF ( IOS < 0 )  THEN
         PRINT*,'ERROR in specified wind conditions.'
         PRINT*,'Wind speed modifier should be specified as an "n.n" number in characters 7-9.'
         PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
         PRINT*,'Please check your input file.'
         STOP
     ENDIF
     SpdMod = SpdMod / LenConvert
     IF (ABS(SpdMod) > 2.0) THEN
        PRINT*,'ERROR in specified wind conditions.'
        PRINT*,'Wind speed increment (characters 7-9) must not exceed 2.0 m/s (6.5 fps).'
        PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
        PRINT*,'Please check your input file.'
     STOP
     ENDIF
     Vhub = Vrated + SpdMod
   CASE DEFAULT
     PRINT*,'ERROR in specified wind conditions.'
     PRINT*,'Wind speed specifier (5th character) must be "i", "o", or "r".'
     PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
     PRINT*,'Please check your input file.'
     STOP
END SELECT


! Ready to create the wind file
PRINT*, 'Generating wind file ', TRIM(WindFile(FileNum)),'.wnd'

! Open the output file
CALL OpenOutFile (FileNum, FileUnit)

! Write out the IEC class summary headers
CALL WindHeader (FileUnit,'Extreme Direction Change')

! Set the transient time
T=6.

! Determine the number of steps for the transient
NT=NINT(T/DT) + 1

!Calculate wind speed components based on inclination angle
VhubH = Vhub * COS(Slope)
VhubV = Vhub * SIN(Slope)

! Write out the file specfic headers
CALL TimeHeader(FileUnit,T)

!Determine the extreme direction change
Sig1 = TurbI*(0.75*Vhub+5.6)
Theta = 4.0 * ATAN(Sig1/(Vhub*(1+0.1*TurbRat)))
Theta = Theta * Dir * 180.0/PI


CALL WndDirHead(FileUnit,Theta,T1+T)
CALL SlopeHead(FileUnit,SLOPE*180./PI)


! Write out column headings to the wind file
CALL ColHeaders(FileUnit)

!Write out the initial wind condition
!mlb Start of proposed change.
WRITE(FileUnit,'(8(F9.3,A1))') &
    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, Alpha, TAB, 0., TAB,0.
!    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, .2, TAB, 0., TAB,0.
!mlb End of proposed change.

DO I=1,NT

   TIME=T1 + FLOAT(I-1)*DT
   ThetaT = 0.5*Theta*(1-COS(PI*(TIME-T1)/T))

!mlb Start of proposed change.
   WRITE(FileUnit,'(8(F9.3,A1))') &
    TIME,TAB,VhubH*LenConvert,TAB,ThetaT,TAB,VhubV*LenConvert,TAB,0.,TAB,Alpha,TAB,0.,TAB,0.
!    TIME,TAB,VhubH*LenConvert,TAB,ThetaT,TAB,VhubV*LenConvert,TAB,0.,TAB,.2,TAB,0.,TAB,0.
!mlb End of proposed change.

END DO !I

! Close the output file
CLOSE(FileUnit)

RETURN
END SUBROUTINE GenEDC


!-----------------------------------------------------------------
SUBROUTINE GenNWP (FileNum)

! Generate the Normal Wind Profile.

USE            Params

IMPLICIT       NONE


! Passed variables
INTEGER        FileNum

! Local variables
REAL           Vhub		!Hub-height total wind speed
REAL           VhubH	!Hub-height horizontal wind speed
REAL           VhubV	!Hub-height vertical wind speed

INTEGER        FileUnit !Output file unit specifier
INTEGER        IOS



! Determine the details of the condition to be generated
! Read the hub-height wind speed
READ(WindFile(FileNum)(4:7),'(F4.1)',IOSTAT=IOS) Vhub
IF ( IOS < 0 )  THEN
    PRINT*,'ERROR in specified wind conditions.'
    PRINT*,'Wind speed should be specified as an "nn.n" number in characters 4-7.'
    PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
    PRINT*,'Please check your input file.'
    STOP
ENDIF

! Ready to create the wind file
PRINT*, 'Generating wind file ', TRIM(WindFile(FileNum)),'.wnd'

! Open the output file
CALL OpenOutFile (FileNum, FileUnit)

! Write out the IEC class summary headers
CALL WindHeader (FileUnit,'Normal Wind Profile')

!Calculate wind speed components based on inclination angle
VhubH = Vhub * COS(Slope)
VhubV = Vhub * SIN(Slope)

! Write out the file specfic headers
CALL NWPHead(FileUnit,Vhub)
CALL SlopeHead(FileUnit,SLOPE*180./PI)


! Write out column headings to the wind file
CALL ColHeaders(FileUnit)

!Write out the wind condition
!mlb Start of proposed change.
WRITE(FileUnit,'(8(F9.3,A1))') &
    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, Alpha, TAB, 0., TAB,0.
!    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, .2, TAB, 0., TAB,0.
!mlb End of proposed change.

! Close the output file
CLOSE(FileUnit)

RETURN
END SUBROUTINE GenNWP


!-----------------------------------------------------------------
SUBROUTINE GenEWM (FileNum)

! Generate the Extreme Wind Model.

USE            Params

IMPLICIT       NONE


! Passed variables
INTEGER        FileNum

! Local variables
REAL           Vhub		!Hub-height total wind speed
REAL           VhubH	!Hub-height horizontal wind speed
REAL           VhubV	!Hub-height vertical wind speed

INTEGER        FileUnit !Output file unit specifier

CHARACTER( 7)  RecInt   !Recurrence interval

! Determine the details of the condition to be generated
SELECT CASE (WindFile(FileNum)(4:5))
   CASE ('50')
     Vhub = Ve50
     RecInt = '50-year'
   CASE ('01')
     Vhub = Ve1
     RecInt = '1-year'
   CASE DEFAULT
     PRINT*,'ERROR in specified wind conditions.'
     PRINT*,'EWM recurrence specifier (characters 4-5) must be "01" or "50".'
     PRINT*,'Wind condition ',TRIM(WindFile(FileNum)),' not recognized.'
     PRINT*,'Please check your input file.'
     STOP
END SELECT

! Ready to create the wind file
PRINT*, 'Generating wind file ', TRIM(WindFile(FileNum)),'.wnd'

! Open the output file
CALL OpenOutFile (FileNum, FileUnit)

! Write out the IEC class summary headers
CALL WindHeader (FileUnit,TRIM(RecInt)//' Extreme Wind Model')

!Calculate wind speed components based on inclination angle
VhubH = Vhub * COS(Slope)
VhubV = Vhub * SIN(Slope)

! Write out the file specfic headers
CALL EWMHead(FileUnit,Vhub)
CALL SlopeHead(FileUnit,SLOPE*180./PI)


! Write out column headings to the wind file
CALL ColHeaders(FileUnit)

!Write out the wind condition
WRITE(FileUnit,'(8(F9.3,A1))') &
    0., TAB, VhubH*LenConvert, TAB, 0., TAB, VhubV*LenConvert, TAB, 0., TAB, .11, TAB, 0., TAB,0.

! Close the output file
CLOSE(FileUnit)

RETURN
END SUBROUTINE GenEWM


!-----------------------------------------------------------------
SUBROUTINE OpenOutFile (FileNum, FileUnit)

! Generate the ECD condition.

USE            Params

IMPLICIT       NONE


! Passed variables
INTEGER        FileNum

! Local variables
INTEGER(4)     FileUnit
INTEGER(4)  :: IERR


FileUnit = 13


OPEN (UNIT = FileUnit, FILE=TRIM(WindFile(FileNum))//'.wnd', FORM='FORMATTED', IOSTAT=IERR)

IF ( IERR /= 0 )  THEN
   Print*,'Error encountered in trying to open file: '//Trim(WindFile(FileNum))//'.wnd'
   Print*,'  An existing file with that name may be open in Excel '
   Print*,'   or other non-sharing program.'
   STOP
ENDIF

WRITE(FileUnit,'(A)')  '!This file was generated by IECwind v'//TRIM(VERSION)//'.'
WRITE(FileUnit,'(A)')  '! Wind condition defined by IEC 61400-1 3rd EDITION.'



RETURN
END SUBROUTINE OpenOutFile


!-----------------------------------------------------------------
SUBROUTINE WindHeader(FileNum, Text)

! Write out the turbine and turbulence class header summaries.

USE            Params

IMPLICIT       NONE


! Passed variables
INTEGER        FileNum

CHARACTER( *)  Text

! Local variables

CHARACTER( 2)  LenUnit


WRITE(FileNum,'(A24, I1, A29, A1)') '! for IEC Turbine Class ', WTC,' and IEC Turbulence Category ',CATG


IF (SIUnit) THEN
   LenUnit = 'm '
ELSE
   LenUnit = 'ft'
ENDIF

WRITE(FileNum,'(A)') &
    '!----------------------------------------------------------'
WRITE(FileNum,'(A19, F5.1, A4)') &
   '! Rotor diameter = ', Dia*LenConvert, TRIM(LenUnit)//'. '
WRITE(FileNum,'(A2, F5.1, A22, F5.1, A21)') &
   '! ',HH*LenConvert, TRIM(LenUnit)//' hub-height yields  ',TurbScale, TRIM(LenUnit)//' turbulence scale. '
WRITE(FileNum,'(A)') &
    '!----------------------------------------------------------'
WRITE(FileNum,'(A)') &
    '! IEC Condition: '//TRIM(Text)//'.'
WRITE(FileNum,'(A)') &
    '! Stats for this wind file:'


RETURN
END SUBROUTINE WindHeader


!-----------------------------------------------------------------
SUBROUTINE TimeHeader(FileNum,Time)

IMPLICIT       NONE


! Passed variables
REAL           Time

INTEGER        FileNum



WRITE(FileNum,'(A32,F4.1,A9)') &
    '!     The transient occurs over ',TIME,' seconds.'

RETURN
END SUBROUTINE TimeHeader


!-----------------------------------------------------------------
SUBROUTINE GustHead(FileNum,Vmax,Vhub,Tmax)


USE            Params

IMPLICIT       NONE


! Passed variables
REAL           Tmax		!Time of maximum gust
REAL           Vmax		!Value of maximum gust
REAL           Vhub		!Nominal hub-height wind speed

INTEGER        FileNum

! Local variables
CHARACTER( 4)  SpdUnit
CHARACTER(10)  Text


IF (SIUNIT) THEN
   SpdUnit = 'm/s'
ELSE
   SpdUnit = 'ft/s'
ENDIF

WRITE(FileNum,'(A)')'!'

WRITE(Text,'(F6.3)') Vmax*LenConvert
WRITE(FileNum,'(A)') &
    '!     The maximum gust speed is '//TRIM(Text)//' '//TRIM(SpdUnit)// '.'

WRITE(Text,'(A)') '          '
WRITE(Text,'(F6.3)') (Vmax + Vhub)*LenConvert
WRITE(FileNum,'(A)') &
    '!     The maximum total wind speed is '//TRIM(Text)//' '//TRIM(SpdUnit)// '.'

WRITE(Text,'(A)') '          '
WRITE(Text,'(F7.3)') Tmax
WRITE(FileNum,'(A)') &
    '!     This maximum occurs at '//TRIM(Text)//' seconds.'

RETURN
END SUBROUTINE GustHead


!-----------------------------------------------------------------
SUBROUTINE WndDirHead(FileNum,MaxDir,Tmax)

USE            Params

IMPLICIT       NONE


! Passed variables
REAL           MaxDir	!Max direction change (deg)
REAL           Tmax		!Time of maximum direction change

INTEGER        FileNum

! Local variables
CHARACTER(10)  Text


WRITE(FileNum,'(A)')'!'

WRITE(Text,'(F7.2)') MaxDir
WRITE(FileNum,'(A)') &
    '!     The extreme direction change is '//TRIM(Text)//' degrees.'

WRITE(Text,'(A)') '          '
WRITE(Text,'(F7.2)') Tmax
WRITE(FileNum,'(A)') &
    '!     This extreme occurs at '//TRIM(Text)//' seconds.'

RETURN
END SUBROUTINE WndDirHead


!-----------------------------------------------------------------
SUBROUTINE SlopeHead(FileNum,Angle)


IMPLICIT       NONE


! Passed variables
REAL           Angle		!Wind direction angle (deg)

INTEGER        FileNum

! Local variables
CHARACTER(10)  Text


WRITE(FileNum,'(A)')'!'
WRITE(Text,'(F4.1)') Angle
WRITE(FileNum,'(A)') &
    '!     The wind inflow inclination angle is '//TRIM(Text)//' degrees to the horizontal.'


RETURN
END SUBROUTINE SlopeHead


!-----------------------------------------------------------------
SUBROUTINE ShearHead(FileNum,Dir,Hshear,ShrMax,Tmax)

USE            Params

IMPLICIT       NONE


! Passed variables
REAL           Dir		!Shear sign
REAL           ShrMax	!Max shear
REAL           Tmax		!Time of maximum direction change

INTEGER        FileNum

LOGICAL        Hshear	!Horizontal shear logical

! Local variables
CHARACTER(10)  Text
CHARACTER(10)  ShrDir	!Direction of shear
CHARACTER( 8)  ShrMult	!Shear sign

IF (Hshear) THEN
   ShrDir = 'horizontal'
ELSE
   ShrDir = 'vertical'
ENDIF

IF (Dir > 0.0) THEN
   ShrMult = 'positive'
ELSE
   ShrMult = 'negative'
ENDIF

WRITE(FileNum,'(A)')'!'

WRITE(Text,'(F6.3)') Dir * ShrMax
WRITE(FileNum,'(A)') &
    '!     The extreme linear '//TRIM(ShrDir)//' wind shear is '//TRIM(Text)//'.'

WRITE(Text,'(A)') '          '
WRITE(Text,'(F6.2)') Tmax
WRITE(FileNum,'(A)') &
    '!     This extreme occurs at '//TRIM(Text)//' seconds.'

WRITE(FileNum,'(A)') &
    '!     This sign of the shear is '//TRIM(ShrMult)//'.'

RETURN
END SUBROUTINE ShearHead


!-----------------------------------------------------------------
SUBROUTINE NWPHead(FileNum,Vhub)


USE            Params

IMPLICIT       NONE


! Passed variables
REAL           Vhub

INTEGER        FileNum

! Local variables
CHARACTER( 4)  SpdUnit
CHARACTER(10)  Text


IF (SIUNIT) THEN
   SpdUnit = 'm/s'
ELSE
   SpdUnit = 'ft/s'
ENDIF


WRITE(Text,'(F6.2)') Vhub*LenConvert
WRITE(FileNum,'(A)') &
    '!     The steady wind speed is '//TRIM(Text)//TRIM(SpdUnit)// '.'

RETURN
END SUBROUTINE NWPHead


!-----------------------------------------------------------------
SUBROUTINE EWMHead(FileNum,Vmax)


USE            Params

IMPLICIT       NONE


! Passed variables
REAL           Vmax

INTEGER        FileNum

! Local variables
CHARACTER( 4)  SpdUnit
CHARACTER(10)  Text


IF (SIUNIT) THEN
   SpdUnit = 'm/s'
ELSE
   SpdUnit = 'ft/s'
ENDIF


WRITE(Text,'(F6.2)') Vmax*LenConvert
WRITE(FileNum,'(A)') &
    '!     The steady extreme wind speed is '//TRIM(Text)//TRIM(SpdUnit)// '.'

RETURN
END SUBROUTINE EWMHead


!-----------------------------------------------------------------
SUBROUTINE ColHeaders(FileNum)

USE            Params

IMPLICIT       NONE


! Passed variables
INTEGER        FileNum

! Local variables
CHARACTER( 6)  SpdUnit


WRITE(FileNum,'(A)') &
    '!----------------------------------------------------------'
WRITE(FileNum,'(A)') &
    '! Time'//TAB//'Wind' //TAB//'Wind'//TAB//'Vertical'//TAB//'Horiz.'//TAB//'Pwr.Law' //TAB//'Lin.Vert.'//TAB//'Gust'
WRITE(FileNum,'(A)') &
    '!'     //TAB//'Speed'//TAB//'Dir' //TAB//'Speed'   //TAB//'Shear' //TAB//'Vert.Shr'//TAB//'Shear'    //TAB//'Speed'

IF (SIUNIT) THEN
   SpdUnit ='(m/s)'
ELSE
   SpdUnit = '(ft/s)'
ENDIF

WRITE(FileNum,'(A)') &
    '!(sec)'//TAB//TRIM(SpdUnit)//TAB//'(deg)' //TAB//TRIM(SpdUnit)//TAB//TAB//TAB//TAB//TRIM(SpdUnit)

RETURN
END SUBROUTINE ColHeaders



!-----------------------------------------------------------------
SUBROUTINE PremEOF ( Fil , VarName )


   ! This routine prints out an invalid-numeric-input message and aborts the program.


IMPLICIT          NONE


   ! Passed Variables:

CHARACTER(  *)	:: Fil
CHARACTER(  *)	:: VarName


   ! Local Variables:

CHARACTER( 42) :: Frmt
CHARACTER(110) :: MESAGE



MESAGE = '"'//TRIM( Fil )//'"'
Frmt   = '('' Premature EOF for file :'', /, 2x, A)'
WRITE (*,'( A )') ' The error occurred while trying to read the '//VarName//'.'

STOP

RETURN
END SUBROUTINE PremEOF


!-----------------------------------------------------------------
SUBROUTINE Conv2UC ( Str )

! This routine converts all the text in a string to upper case.


IMPLICIT        NONE


! Passed Variables:
CHARACTER(*) :: Str


! Local Variables:
INTEGER(4)   :: IC



DO IC=1,LEN_TRIM( Str )

   IF ( ( Str(IC:IC) >= 'a' ).AND.( Str(IC:IC) <= 'z' ) )  THEN
      Str(IC:IC) = CHAR( ICHAR( Str(IC:IC) ) - 32 )
   ELSE
      Str(IC:IC) = Str(IC:IC)
   ENDIF

ENDDO ! IC



RETURN
END SUBROUTINE Conv2UC


! ---END OF FILE-----------------------------------------