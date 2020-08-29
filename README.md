# ZXing.Delphi
ZXing Barcode Scanning Library for Delphi XE to 10.4 Sydney 

<img align="right" src="https://github.com/Spelt/ZXing.Delphi/blob/v_3.0/zxing-logo.png"/>


![ZXing.Delphi Logo](https://github.com/Spelt/ZXing.Delphi/blob/v_3.0/zxing.Delphi.picture2.png )

ZXing.Delphi is a native Object Pascal library that is based on the well known open source Barcode Library: ZXing (Zebra Crossing). This port is based on .Net Redth port of ZXing and the Java one. This is I think the first native FireMonkey barcode lib. It is aimed at all of the FireMonkey mobile platforms and, starting from v3.1, it fully supports also Windows VCL applications (no dependencies on FMX.Graphics unit).

With this library you can scan with native speed without the use of linking in external libraries and avoid compatibility issues and dependencies. It is fast.

Its compatible with in Delphi XE7 - 10 Sydney and tested with IOS 8.x, 9.x, 10.x, 11.x, 12.x, Android, Windows 32/64 and OSX. 
The goal of ZXing.Delphi is to make scanning barcodes effortless, painless, fast and build within your FireMonkey or native Windows (VCL or Firemonkey) applications.  

Just include the source files and add it in your existing projects and build the ZXing.Delphi source within your projects.

## Camera 
The standard camera component is, I think too slow for Android and IOS. You need to find a third party product or cook your own for a smooth experience.

## Supported Formats

| 1D product | 1D industrial | 2D
| ---------- | ------------- | --------------
| UPC-A      | Code 39       | QR Code
| UPC-E      | Code 93       | Data Matrix (Center images only)
| EAN-8      | Code 128      | 
| EAN-13     | ITF           | 


### Features
- Native compiled barcode scanning for all VCL and FireMonkey platforms (IOS/Android/Windows/OSX).
- 100% free. No license fees. Just free.
- Speed
- Simple API
- Unit tests provided
- Test projects provided
	

### Changes
- v3.9.2
	- Removal of advanced test app.
	- fix: Access Violation in Decode https://github.com/Spelt/ZXing.Delphi/issues/100
	- fix: ZXing.Common.BitMatrix Range Check Error - https://github.com/Spelt/ZXing.Delphi/issues/104
- v3.9.0
	- QRCode 64bit Android and IOS fix (Issue #93 and #62)
- v3.8.3
	- Some memleak fixes and simplified Advanced test app.
- v3.8.1
	- In 'Advanced demo' added Rio compatible Android camera optimizing library (thanks to E. van Bilsen). 
- v3.8
	- Fixed missing files for 'advanced test demo app', due to incompatibility with the external used libFastUtils.a and libfastutils-android.a it is not supported for Rio and Android.
	- Added in 'advanced test demo app' new Rio / Android 7+ code for the new permission model.
- v3.7
	- Changes in demo app. Asking Permissions to mobile users. 
- v3.6
	- Fixed QRCode bug and QR Memleak (thanks to C. Pradelli)
- v3.5 
	- Fixed a QRCode bug. Did not find the QRCode in some cases. Bugfix: https://github.com/Spelt/ZXing.Delphi/issues/65 
- v3.4
	- Added an advanced test app. Featuring faster camera, sound, barcode marker,warning for slow camera and a cool HUD. It makes use of huge camera performance tweak. See Readme in the uMain.pas and https://quality.embarcadero.com/browse/RSP-10592 
	- Little cleanup	
	
- v3.3.1 Date: 2017/01/08 (Thanks for Nano103)
	- Bug fix in Code39

- v3.3 Date: 2016/12/10 (Thanks for Nano103 for adding Code 39)
	- Added UPC-A, UPC-E, Code 39
	- Now Delphi is listed at the official zxing page: https://github.com/zxing/zxing	
	- Added tip section.

- v3.2 Date: 2016/11/27 
	- Added EAN8, EAN13 (many requests)
	- v3 becomes master branch

- v3.1 Date: 2016/06/28 (Super many thanks to: Carlo Sirna)
	- Added VCL support (via IFDEF USE_VCL_BITMAP).
	- Memleak fixes for old gen compilers (win32/win64).
	- Fix: QRCode ECI character set + extra unit test.
	- Added 'Load Image from file' command in test project.
	- UTF-8 fixed bug + added unit test
	- Some other bug fixes.

- v3.0 Date: 2016/04/28 (Great many thanks to: Kai Gossens and Raphael Büchler)
	- Massive folder restructuring
	- Added DataMatrix (centered only).
	- ResultPoint event added.
	- Support for inverted 1D/2D code types.
	- Better OneDReader scan strategy
	- Redesigned the file/folder structure for better namespacing.
	- Simplification of adding readers to the TMultiformatReader (just add all your readers here)
	- Small improvements.
	
- v2.4 Date: 2016/04/06
    - Fix in Code128 where code did not scan at all sometimes.    

- v2.3 Date: 2016/02/27
	- Fixed leaks.
    - Android added to compatibility list.

- v2.2 Date: 2016/02/21
	- Fixed IOS crash bug on 32bit only (ITF related).

- v2.1 Date: 2016/01/29
	- Implemented ITF (thanks p. b. Hofstede!) + unit test.
	- Fixed small bug.
	
- v2.0 Date: 2015/11/30
	- Implemented QR-Codes + unit test.

- v1.1 Date: 2015/7/11
	- Implemented Code 93 + unit test.

- v1.0
 	- Init upload
 	- Base classes 1D barcode implemented.	
 	- Implemented Code 128 + unit test.

### Tips - How to optimize an already fast library.
- Try not to scan every incoming frame. 
- Use autoformat scanning with care, with automatic on every frame is passed to every barcode format. For example: If you want to scan only EAN-8, set the scan format for only EAN-8. 
- For mobile: try not to scan every frame, skip every n frame. Scanning 4 frames in a second should be good for most purposes. Safes CPU and battery.
- For mobile: try setting your camera not to a high resolution. 640x480 is for most purposes perfect. More resolutions means more pixels to scan means slower. Saves CPU and battery. 
	
	
	
### Other barcodes?
Although it works extremely well, we still miss a few barcodes.For me there is no immediate need yet for me to implement more types but I like to add all of them! For that I need your help! 

The base classes are already implemented so if you need to have another Barcode like Code39 (already done :-) ) you can see the C# source here: https://github.com/Redth/ZXing.Net.Mobile/blob/master/src/ZXing.Net/oned/Code39Reader.cs and convert it to Pascal. It's pretty easy (or just ask and I convert the raw classes for you). 


**If you want to help:** Let us/me know which barcode you planning to implement. There is no point in converting barcodes multiple times :-)


### 'What is different compared to the original source and what do I need to know if I implement a barcode?' How did you do it?
- I convert C# files to pascal via: 
	- Build it in .NET
	- Decompile it with 'Reflector 6' (which has a Delphi decompile function) to Delphi.NET 
	- Copy and paste the files to the project.
	- Convert the source from Delphi.Net	
- I made use of generic array lists. This is easier and strongly typed.
- I stayed at the architecture and directory structure as implemented in the .Net source.  
- There is a lot of bit shifting going around. Left bit shifting is the same as in C# but right bit shifing is not! I made a helper for this: TMathUtils.Asr 


### Usage
The simplest example of using ZXing.Delphi looks something like this:

Include all the files in your project or use search path like included test application
- Add uses: ScanManager, ZXing.BarcodeFormat, ZXing.ReadResult.
- Add var FScanManager, FReadResult.

```Pascal  

FScanManager := TScanManager.Create(TBarcodeFormat.CODE_128, nil);
FReadResult := FScanManager.Scan(scanBitmap);

```

Of course the real world is not that simple.  To leave your app responsive while scanning you need to run things in parallel. I created a test app to show you how just to do that. Its included.  It makes use of the new Firemonkey parallel lib. In the testApp the resolution of the camera is set to medium (FMX.Media.TVideoCaptureQuality.MediumQuality) on my iPhone 6. This is only possible since XE8 and equivalent Appmethod. Its also good to mention that how higher the resolution the more time it takes to scan a bitmap. Some scaling could probably work too.

Andrea Magni has a very nice blog post about an Android ZXing example from a training excerise of his. You can find it [here](https://blog.andreamagni.eu/2017/06/scannermapp-a-qrbarcode-scanner-app-with-delphi-zxing-and-tframestand/).

### Thanks
ZXing.Delphi is a project that I've put together with the work of others.  So naturally, I'd like to thank everyone who's helped out in any way.  Those of you I know have helped I'm listing here, but anyone else that was involved, please let me know!

- The ZXing main project author - Sean Owen.
- J. Dick at Redth at https://github.com/Redth/ZXing.Net.Mobile
- Carlo Sirna
- P. B. Hofstede
- Kai Gossens
- Raphael Büchler
- Nano103


### ZXing.Delphi
ZXing.Delphi is released under the Apache 2.0 license.
ZXing.Delphi can be found here:https://github.com/Spelt/ZXing.Delphi
A copy of the Apache 2.0 license can be found here: http://www.apache.org/licenses/LICENSE-2.0


### ZXing
ZXing is released under the Apache 2.0 license.
ZXing can be found here: http://code.google.com/p/zxing/
A copy of the Apache 2.0 license can be found here: http://www.apache.org/licenses/LICENSE-2.0


### ZXing.Net
ZXing.Net is released under the Apache 2.0 license.
ZXing.Net can be found here: http://code.google.com/p/zxing/
A copy of the Apache 2.0 license can be found here: http://www.apache.org/licenses/LICENSE-2.0
