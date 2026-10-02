@echo off
rem Builds the benchmark with the command line compiler (Win32):
rem   build.cmd       FMX version: benchmark\Win32\FMX\ZXingBenchmark.exe
rem   build.cmd vcl   VCL version: benchmark\Win32\VCL\ZXingBenchmark.exe
rem The FMX version uses the same image path as FMX apps and the unit tests.
rem Uses %BDS% when set (rsvars.bat), otherwise Delphi 13 in its default location.
setlocal
if "%BDS%"=="" set "BDS=C:\Program Files (x86)\Embarcadero\Studio\37.0"

set "FRAMEWORK=FMX"
set "NAMESPACES=System;FMX;Winapi;System.Win"
if /i "%~1"=="vcl" (
  set "FRAMEWORK=VCL"
  set "NAMESPACES=System;Vcl;Winapi;Vcl.Imaging"
)

set "LIB=%~dp0..\lib\Classes"
set "UNITS=%LIB%;%LIB%\Common;%LIB%\Common\Detector;%LIB%\Common\ReedSolomon;%LIB%\Filtering;%LIB%\1D Barcodes;%LIB%\2D Barcodes;%LIB%\2D Barcodes\Decoder;%LIB%\2D Barcodes\Detector"
set "OUT=%~dp0Win32\%FRAMEWORK%"

if not exist "%OUT%\dcu" mkdir "%OUT%\dcu"
"%BDS%\bin\dcc32.exe" -Q -B -DFRAMEWORK_%FRAMEWORK% "-NS%NAMESPACES%" "-U%UNITS%" "-NU%OUT%\dcu" "-E%OUT%" "%~dp0ZXingBenchmark.dpr"
