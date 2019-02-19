@ECHO OFF

@SET ARCHROOT=IECWind
@SET PROGNAME=IECWind

@SET WINZIP="C:\Program Files (x86)\WinZip\WZZip"
@SET WINZIPSE="C:\Program Files (x86)\WinZip Self-Extractor\wzipse32.exe"

@IF NOT "%1"==""  GOTO DeleteOld

@ECHO 
@ECHO  The syntax for creating an archive is "Archive <version>"
@ECHO.
@ECHO  Example:  "archive 300"

@GOTO Done


:DeleteOld
@IF EXIST ARCHTMP.zip DEL ARCHTMP.zip
@IF EXIST %ARCHNAME%.exe DEL %ARCHNAME%.exe


:DoIt
@ECHO.
@ECHO ------------------------------------------
@ECHO Archiving %PROGNAME% for general distribution.
@ECHO ------------------------------------------
@ECHO.
@%WINZIP% -a -o -P %TEMP%\ARCHTMP @ArcFiles.txt
@%WINZIPSE% %TEMP%\ARCHTMP.zip -d. -y -win32 -le -overwrite -st"Unzipping %PROGNAME%" -m Disclaimer.txt
@COPY %TEMP%\ARCHTMP.exe Archive\%ARCHROOT%_v%1.exe
@DEL %TEMP%\ARCHTMP.zip, %TEMP%\ARCHTMP.exe

@ECHO.
@ECHO ---------------------------------
@ECHO Archiving %PROGNAME% for maintenance.
@ECHO ---------------------------------
@ECHO.
@%WINZIP% -a -o -P %TEMP%\ARCHTMP @ArcFiles.txt @ArcMaint.txt
@%WINZIPSE% %TEMP%\ARCHTMP.zip -d. -y -win32 -le -overwrite -st"Unzipping %PROGNAME%" -m Disclaimer.txt
@COPY %TEMP%\ARCHTMP.exe Archive\%ARCHROOT%_v%1_Maint.exe
@DEL %TEMP%\ARCHTMP.zip, %TEMP%\ARCHTMP.exe


:Done
@SET ARCHROOT=
@SET PROGNAME=
