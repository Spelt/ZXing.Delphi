# ZXing.Delphi
ZXing Barcode Scanning Library for Delphi XE7/XE8/10 Seattle and Appmethod. 

![ZXing.Delphi Logo](https://github.com/Spelt/ZXing.Delphi/blob/master/zxing.Delphi.picture1.png )

ZXing.Delphi is a native Object Pascal library that is based on the well known open source Barcode Library: ZXing (Zebra Crossing). This port is based on .Net Redth port of ZXing and the Java one. This is I think the first native FireMonkey barcode lib.  It is aimed at all of the FireMonkey mobile platforms.

With this library you can scan with native speed without the use of linking in external libraries and avoid compability issues and dependencies. It is fast.

Its compatible with in Delphi XE7, XE8, 10 Seattle and AppMethod and tested with IOS 8.x, 9.x, Android, Windows 32/64 and OSX. 
The goal of ZXing.Delphi is to make scanning barcodes effortless, painless, fast and build within your FireMonkey or native Windows applications.  
Just include the source files and add it in your existings projects and build the ZXing.Delphi source within your projects.


##Important note:
In Delphi 10 Seattle it is necessary to setting the Focusmode to TFocusMode.ContinuousAutoFocus. Perhaps its necessary in earlier versions too.


###Changes
- v2.3 Date: 2016/02/27
	- Fixed leaks.
    - Android added to compatability list.

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

###Features
- Native compiled barcode scanning for all FireMonkey platforms (IOS/Android/Windows/OSX).
- 100% FREE. No ridiculous licence fees. Just FREE.
- Speed
- Barcodes: QR-Code, Code 128, Code93, ITF
- Simple API
- Unit tests provided
	
	
#Help wanted
Although it works extremely well, it works currently only with a few barcodes. I only needed one barcode so for me there is no immediate need yet for me to implement more types but I like to add all of them! For that I need your help! 

The base classes I already implemented so if you need to have another Barcode like Code39 you can see the C# source here: https://github.com/Redth/ZXing.Net.Mobile/blob/master/src/ZXing.Net/oned/Code39Reader.cs and convert it to Pascal. It's pretty easy. 

Only with your help we can get all the barcodes here! A unit test project is included and all barcodes must be unit tested!

**If you want to help:** Let us/me know which barcode you planning to implement. There is no point in converting barcodes multiple times :-)


###'What is different compared to the original source and what do I need to know if I implement a barcode?' How did you do it?
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

Include all the file in your project. 
- Add uses: ScanManager, BarcodeFormat, ReadResult.
- Add var FScanManager, ReadResult.

```Pascal  

FScanManager := TScanManager.Create(TBarcodeFormat.CODE_128, nil);
ReadResult := FScanManager.Scan(scanBitmap);

```

Of course the real world is not that simple.  To leave your app responsive while scanning you need to run things in parallel. I created a test app to show you how just to do that. Its included.  It makes use of the new Firemonkey parallel lib. In the testApp the resolution of the camera is set to medium (FMX.Media.TVideoCaptureQuality.MediumQuality) on my iPhone 6. This is only possible since XE8 and equivalent Appmethod.  Its also good to mention that how higher the resolution the more time it takes to scan a bitmap. Some scaling could probably work too.


###Thanks
ZXing.Delphi is a project that I've put together with the work of others.  So naturally, I'd like to thank everyone who's helped out in any way.  Those of you I know have helped I'm listing here, but anyone else that was involved, please let me know!

- J. Dick at Redth at https://github.com/Redth/ZXing.Net.Mobile 
- The ZXing Project


###License
Apache ZXing.Net.Mobile Copyright 2012 The Apache Software Foundation
This product includes software developed at The Apache Software Foundation (http://www.apache.org/).


### ZXing.Net
ZXing.Net is released under the Apache 2.0 license.
ZXing.Net can be found here: http://code.google.com/p/zxing/
A copy of the Apache 2.0 license can be found here: http://www.apache.org/licenses/LICENSE-2.0


### ZXing
ZXing is released under the Apache 2.0 license.
ZXing can be found here: http://code.google.com/p/zxing/
A copy of the Apache 2.0 license can be found here: http://www.apache.org/licenses/LICENSE-2.0
