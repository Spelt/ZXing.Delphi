﻿# ZXing.Delphi
ZXing Barcode Scanning Library for Delphi 10.4 Sydney to 13 Florence. 

<img align="right" src="https://github.com/Spelt/ZXing.Delphi/blob/v_3.0/zxing-logo.png"/>


![ZXing.Delphi Logo](https://github.com/Spelt/ZXing.Delphi/blob/v_3.0/zxing.Delphi.picture2.png )

ZXing.Delphi is a native Object Pascal library that is based on the well known open source Barcode Library: ZXing (Zebra Crossing). This port is based on .Net Redth port of ZXing and the Java one. This is I think the first native FireMonkey barcode lib. It is aimed at all of the FireMonkey mobile platforms and, starting from v3.1, it fully supports also Windows VCL applications (no dependencies on FMX.Graphics unit).

With this library you can scan with native speed without the use of linking in external libraries and avoid compatibility issues and dependencies. It is fast.

Its compatible with Delphi 10.4 Sydney - 13 Florence (for XE7 - 10.3 use v3.13.1 or older) and tested with IOS 8.x - 15.x, Android 32/64, Windows 32/64 and OSX. 
The goal of ZXing.Delphi is to make scanning barcodes effortless, painless, fast and build within your FireMonkey or native Windows (VCL or Firemonkey) applications.  

Just include the source files and add it in your existing projects and build the ZXing.Delphi source within your projects.

## Camera 
From Delphi 11 the standard camera component seems much improved.

## Supported Formats

All formats are read with TBarcodeFormat.Auto, except the ones marked "only when asked for" (choose them as TScanManager format or in the hint POSSIBLE_FORMATS).

### 1D

| Format | TBarcodeFormat | Also | Notes
| ------ | -------------- | ---- | -----
| UPC-A | UPC_A | 2 or 5 digit add-on | add-on with the hint ALLOWED_EAN_EXTENSIONS
| UPC-E | UPC_E | 2 or 5 digit add-on |
| EAN-8 | EAN_8 | |
| EAN-13 | EAN_13 | 2 or 5 digit add-on, ISBN (as EAN-13) |
| Code 39 | CODE_39 | Full ASCII (USE_CODE_39_EXTENDED_MODE), check digit (ASSUME_CODE_39_CHECK_DIGIT) |
| Code 32 | CODE_32 | | Italian pharmacy code; only when asked for (Auto: CODE_39)
| PZN | PZN | | German Pharmazentralnummer; only when asked for (Auto: CODE_39)
| Code 93 | CODE_93 | Full ASCII |
| Code 128 | CODE_128 | GS1-128 |
| ITF | ITF | ITF-14 (as ITF), any even length from 6 digits (ALLOWED_LENGTHS) |
| Codabar | CODABAR | start and stop characters with RETURN_CODABAR_START_END |
| Telepen | TELEPEN | full ASCII and compressed numeric |
| GS1 DataBar | RSS_14 | Omnidirectional, Truncated, Stacked, Stacked Omnidirectional |
| GS1 DataBar Expanded | RSS_EXPANDED | Expanded Stacked |
| GS1 DataBar Limited | RSS_LIMITED | |
| DX Film Edge | DX_FILM_EDGE | frame number |
| MSI | MSI | modulo 10 check digit (ASSUME_MSI_CHECK_DIGIT) | only when asked for
| Plessey | PLESSEY | | CRC checked; only when asked for
| Pharmacode | PHARMA_CODE | | Laetus one track; only when asked for

### 2D

| Format | TBarcodeFormat | Also | Notes
| ------ | -------------- | ---- | -----
| QR Code | QR_CODE | Model 1 and 2, ECI, GS1, Structured Append | perspective and not flat symbols
| Micro QR Code | MICRO_QR_CODE | M1 to M4 |
| rMQR Code | RMQR_CODE | ECI, GS1 | rectangular Micro QR Code
| Data Matrix | DATA_MATRIX | square, rectangular and DMRE sizes, ECI, GS1, mirrored | dot-peen codes with TRY_HARDER
| Aztec | AZTEC | compact and full range, Aztec Runes, ECI, GS1, Structured Append, mirrored |
| PDF417 | PDF_417 | Compact PDF417, Macro PDF417, ECI |
| MicroPDF417 | MICRO_PDF417 | Macro PDF417, ECI |
| MaxiCode | MAXICODE | modes 2 to 6, ECI, Structured Append | with detector: anywhere in the image, rotated and in perspective (camera)

### Postal (4-state)

Only when asked for, not in Auto. Horizontal and vertical, upside down, slanted and in perspective.

| Format | TBarcodeFormat | Notes
| ------ | -------------- | -----
| KIX (PostNL) | KIX | starting with a Dutch postcode (it has no start or stop: that tells the direction)
| RM4SCC (Royal Mail 4-State Customer Code) | RM4SCC | check character checked, not in the text
| USPS Intelligent Mail Barcode | IMB | CRC checked; text: tracking code (20 digits) and routing code (ZIP, 0, 5, 9 or 11 digits)
| POSTNET (USPS) | POSTNET | 5, 9 or 11 digits, check digit checked, not in the text
| PLANET (USPS) | PLANET | 11 or 13 digits, check digit checked, not in the text
| Japan Post (Kasutama) | JAPAN_POST | postal code and address (with letters), check character checked
| Australia Post 4-State Customer Barcode | AUSTRALIA_POST | Standard, Customer Barcode 2 and 3, reply, routing and redirection; Reed-Solomon (errors corrected); text: format control code, DPID and customer information

### Features
- Native compiled barcode scanning for all VCL and FireMonkey platforms (IOS/Android/Windows/OSX).
- 100% free. No license fees. Just free.
- Speed
- Simple API
- Unit tests provided
- Test projects provided
	

### Changes
- Next version (branch Development, not released yet)
	- Requires Delphi 10.4 Sydney or newer. For XE7 - 10.3 use v3.13.1 or older.
	- Data Matrix: port of the edge tracing detector of zxing-cpp, with DMRE (rectangular) sizes, mirrored codes, pure codes of 1 pixel per module, and corrections for not flat and curved symbols (LocalGrid and timing pattern correction). The old detector stays as fallback.
	- QR Code: port of the finder pattern detector of zxing-cpp, which handles perspective and not flat symbols much better (alignment patterns located in the image, tiled sampling). The old detector stays as fallback.
	- QR Code model 1 (the original QR Code of ISO 18004:2000 annex M) is read too, like zxing-cpp; its SymbologyIdentifier is ]Q0, the format stays QR_CODE.
	- QR Code: a symbol whose format information is readable but which can not be read is sampled once more half a pixel beside in each direction. Helps for small modules, round dots and wrinkled codes (no extra time on images without a code).
	- Faster: luminance conversion (FMX and VCL), binarizer, inversion, closing for dot-peen codes and the 1D readers (a row cache shared by the readers). Results are unchanged.
	- Image pyramid: when nothing is found, also downscaled copies of the image are scanned (TScanManager.TryDownscale, on by default). Helps for large, blurry and dot-peen codes.
	- New formats: Codabar (without the start and stop characters A-D, unless RETURN_CODABAR_START_END) and Telepen (full ASCII and compressed numeric, TBarcodeFormat.TELEPEN).
	- New format: Aztec (TBarcodeFormat.AZTEC), port of the Aztec reader of zxing-cpp: compact and full range symbols (with the reference grid for large ones), Aztec Runes (the text is the value as 3 digits), mirrored symbols, ECI, GS1 (FNC1) and Structured Append (STRUCTURED_APPEND_SEQUENCE like QR Code: index in the high nibble, count - 1 in the low one).
	- New formats: GS1 DataBar, port of the DataBar readers of zxing-cpp: DataBar Omnidirectional, Truncated and Stacked (TBarcodeFormat.RSS_14), DataBar Expanded and Expanded Stacked (RSS_EXPANDED) and DataBar Limited (new: RSS_LIMITED). The text is the GS1 data without parentheses (GS between variable length fields), SymbologyIdentifier ]e0; IsGS1 and GS1HRI give the readable form like (01)04412345678909. Stacked symbols have a Position with 4 corners.
	- New formats: PDF417 (TBarcodeFormat.PDF_417) and MicroPDF417 (new: MICRO_PDF417), port of the readers of zxing-cpp: the PDF417 detector of start and stop patterns (Compact PDF417 too: its right row indicator is missing) (also several symbols, 90/180/270 degrees with TRY_HARDER) and its pure detector, the MicroPDF417 detector of row address patterns, Reed-Solomon in GF(929) with erasures, text, byte and numeric compaction, ECI (]L1), Reader Initialisation, Macro PDF417 / Structured Append and the Macro 05/06 headers of MicroPDF417. The Macro PDF417 data (segment index and count, file id, file name, ...) is in the metadata PDF417_EXTRA_METADATA (IPDF417ResultMetadata).
	- New formats: Micro QR Code (new: MICRO_QR_CODE, M1 to M4) and rMQR Code (new: RMQR_CODE, the rectangular Micro QR Code of ISO/IEC 23941), port of the readers of zxing-cpp: found by their finder pattern (also several in an image) or as pure symbol, numeric, alphanumeric, byte and Kanji segments, and ECI and FNC1 (GS1) for rMQR. The QR Code reader no longer raises an exception on a pure symbol smaller than a QR Code.
	- New format: MaxiCode (TBarcodeFormat.MAXICODE), port of the reader of zxing-cpp: all modes (2 to 6), the Structured Carrier Message, ECI and Structured Append (STRUCTURED_APPEND_SEQUENCE). zxing-cpp only reads symbols that fill the image (a "pure" image); ZXing.Delphi also has a detector of its own that finds the symbol anywhere in the image by its bullseye, so it works with the camera too: rotated, skewed and in perspective (the grid is fitted to the edges between the modules), also several symbols with ScanAll. It reads all 4 photos of shipping labels of the zxing-cpp samples (zxing-cpp none of them).
	- New format: DX Film Edge (new: DX_FILM_EDGE), the DX code on the edge of 35 mm film, port of the reader of zxing-cpp: the text is the product and generation number and, when there, the frame number, like 115-10/11A.
	- New formats: Code 32 (new: CODE_32, the Italian pharmacy code, text 'A' and 9 digits) and PZN (new: PZN, the German Pharmazentralnummer), both Code 39 codes, port of zxing-cpp. Only when asked for (TScanManager format or POSSIBLE_FORMATS): Auto returns them as CODE_39 like before, so nothing changes for applications that read them as Code 39.
	- New formats: MSI (MSI), Plessey (PLESSEY) and Pharmacode (new: PHARMA_CODE). zxing-cpp and ZXing Java have none of them: MSI and Pharmacode after the readers of ZXing.Net, Plessey new (with its CRC checked), the patterns as in zint. MSI with the hint ASSUME_MSI_CHECK_DIGIT checks the modulo 10 check digit (it stays in the text). MSI reads 5 of the 6 MSI test images of ZXing.Net (like ZXing.Net), also upside down. Only when asked for, not in Auto: MSI and Pharmacode have no check against false positives. Pharmacode is read left to right; upside down it is a different value.
	- New formats, the postal barcodes: KIX (new: KIX), RM4SCC (new: RM4SCC), USPS Intelligent Mail Barcode (new: IMB), POSTNET and PLANET (new: POSTNET, PLANET) , Japan Post (new: JAPAN_POST) and Australia Post (new: AUSTRALIA_POST). zxing-cpp and ZXing Java have none of them: a detector of its own finds the rows of bars (also vertical, upside down, slanted, curved and in perspective) and the heights of the bars, the encodings as in zint. IMb reads 8 of the 10 test images of ZXing.Net (ZXing.Net 1, with TRY_HARDER 7); with a strong check (IMb) the least certain bars are tried the other way too. Only when asked for, not in Auto.
	- Auto (TBarcodeFormat.Auto) reads all formats, also the new ones (Codabar, Telepen, Aztec, GS1 DataBar, PDF417, MicroPDF417, Micro QR Code, rMQR Code, MaxiCode, DX Film Edge), as users expect. On images without a barcode this costs more time than before: about 60% with TRY_HARDER and about 2 times without (a few milliseconds per camera image): choose the formats you need for the best speed.
	- TScanManager.ScanAll: a result whose position was the same array as its result points (Aztec, and the symbols found but not read) got its position scaled twice in the downscaled layers.
	- 1D: port of the row decoders of zxing-cpp for EAN/UPC, Code 128, Code 39, Code 93 and ITF (any even ITF length from 6 digits). A code counts when it is read on 2 rows, so a false positive no longer stops the search; with TRY_HARDER small images are scanned row by row. A code read on one row only counts when the decoder of before reads the same on that row. The decoders of before are no longer used as fallback for these formats (they only added false positives): no false positives on the test images without barcode, and TRY_HARDER is as fast as before on images without barcode.
	- Hints ALLOWED_EAN_EXTENSIONS and ALLOWED_LENGTHS: use TIntegerArrayHint.Create([2, 5]) as value; TScanManager frees it. Before, these hints made TScanManager crash when it freed them (an array cast to TObject still works, but is not freed). ALLOWED_EAN_EXTENSIONS now really requires an add-on: the search goes on until a code with add-on is found.
	- Code 128: extended characters (FNC4) in code set A were all returned as character 160.
	- QR Code: segments in Kanji mode made the decoding fail. The application indicator after FNC1 in second position (AIM) is read, and an unknown ECI no longer stops the decoding (the default character set is used, like zxing-cpp).
	- Code 93: the right result point (and so Position and Orientation) was wrong.
	- TBitArray.setRange set the wrong bits.
	- IByteSegmentsMetadata had the GUID of IIntegerMetadata: Supports could return the wrong one of both (and Value garbage).
	- Reed-Solomon (QR Code, Data Matrix, Aztec): more errors than can be corrected no longer give a wrongly "corrected" codeword. The result is checked again, and with an odd number of error correction codewords one error too many was accepted (like zxing-cpp).
	- TScanManager.ScanAll: all barcodes in an image (QR Code, Data Matrix and 1D on different rows, also vertical ones with TRY_HARDER).
	- TReadResult: Position (4 corners), Orientation, IsInverted, IsMirrored, SymbologyIdentifier (like ]Q1, ]d2, ]C1), IsGS1 and GS1HRI (the human readable form of GS1 data, like (01)...(17)...(10)...).
	- TScanManager.ReturnErrors (off by default): also QR Codes, Micro QR Codes, rMQR Codes, Data Matrix, Aztec, PDF417 and MicroPDF417 codes that were found but could not be read, with TReadResult.Error ('Checksum' or 'Format') and their position, e.g. to tell the user to hold the camera still or closer. Scan returns one only when nothing could be read.
	- On the black box test images of zxing-cpp (benchmark\, compared with zxing-cpp): Data Matrix 365 of 366, QR Code 515 of 525, 1D 368 of 362 (with TRY_HARDER and inversion); without hints Data Matrix 93 of 92, QR Code 299 of 304, 1D 234 of 226.
- v3.13.0
	- Fixes thanks to Robert Jędrzejczyk. https://github.com/Spelt/ZXing.Delphi/issues/170, https://github.com/Spelt/ZXing.Delphi/issues/171, https://github.com/Spelt/ZXing.Delphi/issues/172 
	- 1D with TRY_HARDER: results were lost and memory leaked (#170). Codes are now also searched rotated by 90 degrees, as intended.
	- Images smaller than 40 pixels: 2D codes are now decoded (global histogram fallback) and Data Matrix no longer raises an access violation.
	- Memory leaks fixed in the Data Matrix decoder (blocks after a failed error correction) and in QR Kanji/Hanzi and Data Matrix Base256 decoding.
	- Code 39: the check digit (ASSUME_CODE_39_CHECK_DIGIT) works, and the Full ASCII characters %K to %Z are correct.
	- Code 93: Full ASCII characters (lower case, punctuation) are correct; they were returned twice or cut off the text.
	- Data Matrix: ECI is supported (the character set of the text after it), and the 05/06 macro header and trailer are correct.
	- TMultiFormatReader.decode without hints no longer raises an access violation.
	- EAN/UPC: with ALLOWED_EAN_EXTENSIONS a code without extension is rejected, as documented.
	- New regression tests on the black box test images of zxing-cpp (unitTest\Images\zxing-cpp) and a benchmark that compares with zxing-cpp (benchmark\).
- v3.12.0
	- Data Matrix: rotated codes are read much more reliably. The number of modules is now counted through the centers of the outer modules instead of along the edge.
	- Data Matrix: better reading of dot-peen codes (codes made of separate dots). With TRY_HARDER the dots are merged as a last attempt; this is slower on images without a code.
	- Data Matrix: with the ASSUME_GS1 hint, a GS1 code starts with the symbology identifier ']d2' instead of ASCII 29 (GS). Without the hint nothing changes.
- v3.11.0
	- Data Matrix: codes that are not centered in the image are now found. The detector tries the center first and then a grid of start points. Small codes away from the center need the TRY_HARDER hint.
	- Data Matrix: fixed skewed corner detection for codes left of the image center.
- v3.10.1
	- Data Matrix: fixed decoding of codes with an EDIFACT segment followed by more data. https://github.com/Spelt/ZXing.Delphi/issues/180
	- Data Matrix: fixed upper shift (extended ASCII) characters in C40 and Text mode.
- v3.10.0 Improvements by René Hoffmann. https://github.com/Spelt/ZXing.Delphi/issues/143
	- Avoid unnecessary usage of 'class'-modifier keyword (refactoring only)
	- incorrect use of class var disrupts usage of multiple instances (e.g. in different threads) #174
- v3.9.13
	QR code: Fixed compute dimensions and more accurate results of sizeOfBlackWhiteBlackRunBothWays if outside of image. Replaced integer division with floating point division (Thanks ImperatorZurg).
- v3.9.12
	Fixed a bug in Bresenham's line algorithm
- v3.9.11
	Usage of FRAMEWORK_FMX and FRAMEWORK_VCL. See 'Usage' below.
- v3.9.8
	Fixes datamatrix https://github.com/Spelt/ZXing.Delphi/issues/162 and QR QRCode read error when contains when has char 10 https://github.com/Spelt/ZXing.Delphi/issues/163, pull requests with code optimization from EguitarRed and Rene Pastoors.
- v3.9.7
	updated fix for compatibility: https://github.com/Spelt/ZXing.Delphi/issues/143
- v3.9.6
	- Lots of fixes by Robert Jedrzejczyk https://github.com/Spelt/ZXing.Delphi/issues/142, https://github.com/Spelt/ZXing.Delphi/issues/143, https://github.com/Spelt/ZXing.Delphi/issues/144, https://github.com/Spelt/ZXing.Delphi/issues/145, https://github.com/Spelt/ZXing.Delphi/issues/146, https://github.com/Spelt/ZXing.Delphi/issues/147, https://github.com/Spelt/ZXing.Delphi/issues/148, https://github.com/Spelt/ZXing.Delphi/issues/149, https://github.com/Spelt/ZXing.Delphi/issues/150, https://github.com/Spelt/ZXing.Delphi/issues/151, https://github.com/Spelt/ZXing.Delphi/issues/152, https://github.com/Spelt/ZXing.Delphi/issues/153, https://github.com/Spelt/ZXing.Delphi/issues/154, https://github.com/Spelt/ZXing.Delphi/issues/156   
- v3.9.5
	- fix: overflow Android. https://github.com/Spelt/ZXing.Delphi/issues/136 
- v3.9.4
	- fix: when using TBarcodeFormat.Auto certain QRCodes causes integer overflow in EAN parser. https://github.com/Spelt/ZXing.Delphi/issues/133
- v3.9.3
	- Demo app is Alexandria/Android compatible (Thanks igorbastosib and Patrick Prémartin)
	- fix: Some boundary check added (Thanks igorbastosib)
	- fix: Segmentatition fault	(Thanks Macc2010)
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
If you use a Delphi older then 11.1 then you NEED to set a compiler Define in your 'project options->Delphi compiler->Conditional defines'. Set this in your target platforms or All platforms.  See demo applications.
- FRAMEWORK_VCL - this predefined variable is set to true if the project uses the VCL framework
- FRAMEWORK_FMX - this predefined variable is set to true if the project uses the FireMonkey (FMX) framework


The simplest example of using ZXing.Delphi looks something like this:

Include all the files in your project or use search path like included test application
- Add uses: ScanManager, ZXing.BarcodeFormat, ZXing.ReadResult.
- Add var FScanManager, FReadResult.

```Pascal  

FScanManager := TScanManager.Create(TBarcodeFormat.CODE_128, nil);
FReadResult := FScanManager.Scan(scanBitmap);

```

Create the scan manager once and use it for all frames; it is not meant to be used by two threads at the same time. Hints are passed as a dictionary, which the scan manager frees:

```Pascal

var hints := TDictionary<TDecodeHintType, TObject>.Create;
hints.Add(TDecodeHintType.TRY_HARDER, nil);        // slower, finds more (also rotated 1D codes)
hints.Add(TDecodeHintType.ENABLE_INVERSION, nil);  // also light codes on a dark background
FScanManager := TScanManager.Create(TBarcodeFormat.Auto, hints);

```

All barcodes in an image (next version), with the extra information of a result:

```Pascal

var list := FScanManager.ScanAll(scanBitmap);  // a TObjectList: frees the results
try
  for var r in list do
  begin
    Memo1.Lines.Add(r.SymbologyIdentifier + ' ' + r.Text);
    // r.Position: the 4 corners in the image, r.Orientation: the rotation in degrees
    if r.IsGS1 then
      Memo1.Lines.Add(r.GS1HRI);  // e.g. (01)05909990329717(17)270430(10)NE76571
  end;
finally
  list.Free;
end;

```

A QR Code or Data Matrix that was found but could not be read (next version, off by default):

```Pascal

FScanManager.ReturnErrors := true;
var r := FScanManager.Scan(scanBitmap);
if (r <> nil) and (r.Error <> '') then
  // r.Text is empty, r.Position is where the code is: e.g. ask to hold the camera still or closer
  lblStatus.Text := 'Barcode found, but not readable (' + r.Error + ')';

```

Of course the real world is not that simple.  To leave your app responsive while scanning you need to run things in parallel. I created a test app to show you how just to do that. Its included.  It makes use of the new Firemonkey parallel lib. In the testApp the resolution of the camera is set to medium (FMX.Media.TVideoCaptureQuality.MediumQuality) on my iPhone 6. Its also good to mention that how higher the resolution the more time it takes to scan a bitmap. Some scaling could probably work too.

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
