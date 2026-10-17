@echo off
rem Packs the test images for the test app on a device (DUnitXTestMobile):
rem unitTest\images.zip, holding the folder images\ itself. Deployed to the
rem documents folder of the app, unpacked there by TestImages.pas.
setlocal
cd /d "%~dp0"
if exist images.zip del images.zip
powershell -NoProfile -Command "Compress-Archive -Path images -DestinationPath images.zip -CompressionLevel Fastest"
for %%f in (images.zip) do echo images.zip: %%~zf bytes
