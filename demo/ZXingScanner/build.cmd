@echo off
rem Builds the demo with the command line compiler (Win32, FMX), without the
rem IDE: demo\ZXingScanner\Win32\Debug\ZXingScanner.exe
rem Uses %BDS% when set (rsvars.bat), otherwise Delphi 13 in its default location.
setlocal
if "%BDS%"=="" set "BDS=C:\Program Files (x86)\Embarcadero\Studio\37.0"

set "LIB=%~dp0..\..\lib\Classes"
set "UNITS=%LIB%;%LIB%\Common;%LIB%\Common\Detector;%LIB%\Common\ReedSolomon;%LIB%\Filtering;%LIB%\1D Barcodes;%LIB%\2D Barcodes;%LIB%\2D Barcodes\Decoder;%LIB%\2D Barcodes\Detector"
set "OUT=%~dp0Win32\Debug"

if not exist "%OUT%\dcu" mkdir "%OUT%\dcu"
"%BDS%\bin\dcc32.exe" -Q -B -DFRAMEWORK_FMX "-NSSystem;FMX;Winapi;System.Win" "-U%UNITS%" "-NU%OUT%\dcu" "-E%OUT%" "%~dp0ZXingScanner.dpr"
