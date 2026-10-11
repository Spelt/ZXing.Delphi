unit MultiFormatReaderTest;

{
  * Tests for TMultiFormatReader used directly, without TScanManager.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMultiFormatReaderTest = class(TObject)
  public
    /// <summary>decode(image) without hints, twice: it used to raise an
    /// access violation and to leak the readers of the previous call.</summary>
    [Test]
    procedure DecodeWithoutHints;
    /// <summary>Auto reads the formats that were added later too.</summary>
    [Test]
    procedure AutoReadsAllFormats;
    /// <summary>All reads the formats of Auto and the ones only read when
    /// asked for (Auto not).</summary>
    [Test]
    procedure AllReadsAllFormats;
    /// <summary>ScanAsync scans in a task and calls back in the main
    /// thread.</summary>
    [Test]
    procedure ScanAsyncCallsBackInTheMainThread;
    [Test]
    procedure ScanAllAsyncCallsBackWithTheList;
    [Test]
    procedure ScanWithoutAScanManager;
    /// <summary>The DataMatrix directions in tasks give the results of
    /// the sequential detection.</summary>
    [Test]
    procedure ParallelDecodingSameResults;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
{$IFDEF FRAMEWORK_FMX}
  FMX.Graphics,
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  Vcl.Graphics,
{$ENDIF}
  ZXing.LuminanceSource,
  ZXing.RGBLuminanceSource,
  ZXing.HybridBinarizer,
  ZXing.BinaryBitmap,
  ZXing.MultiFormatReader,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.ScanManager,
  ZXing.Parallel,
  ZXing.DecodeHintType,
  Benchmark.Images;

procedure TMultiFormatReaderTest.DecodeWithoutHints;
var
  bitmap: TBitmap;
  source: TLuminanceSource;
  binarizer: THybridBinarizer;
  image: TBinaryBitmap;
  reader: TMultiFormatReader;
  r: TReadResult;
  i: Integer;
begin
  bitmap := LoadImage(ExtractFileDir(ParamStr(0)) + '\..\..\Images\q33.png');
  source := TRGBLuminanceSource.CreateFromBitmap(bitmap, bitmap.Width,
    bitmap.Height);
  binarizer := THybridBinarizer.Create(source);
  image := TBinaryBitmap.Create(binarizer);
  reader := TMultiFormatReader.Create;
  try
    for i := 1 to 2 do
    begin
      r := reader.decode(image);
      try
        Assert.IsNotNull(r, 'Nil result');
        Assert.AreEqual(TBarcodeFormat.QR_CODE, r.BarcodeFormat);
        Assert.IsTrue(r.Text.StartsWith('Never gonna give you up'));
      finally
        r.Free;
      end;
    end;
  finally
    reader.Free;
    image.Free;
    binarizer.Free;
    source.Free;
    bitmap.Free;
  end;
end;

procedure TMultiFormatReaderTest.AutoReadsAllFormats;
type
  TSample = record
    FileName: string;
    Format: TBarcodeFormat;
    Text: string;
  end;
const
  SAMPLES: array [0 .. 12] of TSample = (
    (FileName: 'dxfilmedge-1\2.png'; Format: TBarcodeFormat.DX_FILM_EDGE;
    Text: '80-11/23'),
    (FileName: 'maxicode-1\MODE5.png'; Format: TBarcodeFormat.MAXICODE;
    Text: ''),
    (FileName: 'microqrcode-1\M2-Alpha.png';
    Format: TBarcodeFormat.MICRO_QR_CODE; Text: 'ABC'),
    (FileName: 'rmqrcode-1\R7x43-H.png'; Format: TBarcodeFormat.RMQR_CODE;
    Text: ',,'),
    (FileName: 'codabar-1\01.webp'; Format: TBarcodeFormat.CODABAR;
    Text: '1234567890'),
    (FileName: 'telepen-1\telepen-alpha-2.png'; Format: TBarcodeFormat.TELEPEN;
    Text: 'TELEPEN'),
    (FileName: 'aztec-1\7.png'; Format: TBarcodeFormat.AZTEC;
    Text: 'Code 2D!'),
    (FileName: 'databarOmni-1\1.png'; Format: TBarcodeFormat.RSS_14;
    Text: '0104412345678909'),
    (FileName: 'databarExp-3\1.png'; Format: TBarcodeFormat.RSS_EXPANDED;
    Text: '0190012345678908310301223315991231'),
    (FileName: 'databarLtd-1\00.png'; Format: TBarcodeFormat.RSS_LIMITED;
    Text: '0100000000000000'),
    (FileName: 'pdf417-1\01.png'; Format: TBarcodeFormat.PDF_417;
    Text: 'This is PDF417'),
    (FileName: 'micropdf417-1\MPDF-0.png';
    Format: TBarcodeFormat.MICRO_PDF417; Text: 'I have the best words.'),
    (FileName: 'qrcode-1\1.png'; Format: TBarcodeFormat.QR_CODE; Text: ''));
begin
  for var s in SAMPLES do
  begin
    var bitmap := LoadImage(ExtractFileDir(ParamStr(0)) +
      '\..\..\Images\zxing-cpp\' + s.FileName);
    // (as an interface: freed with the last reference, no Free)
    var scanManager: IScanManager := TScanManager.Create(TBarcodeFormat.Auto,
      nil);
    try
      var r := scanManager.Scan(bitmap);
      try
        Assert.IsNotNull(r, 'Nil result: ' + s.FileName);
        Assert.AreEqual(Ord(s.Format), Ord(r.BarcodeFormat), s.FileName);
        if (s.Text <> '') then
          Assert.AreEqual(s.Text, r.Text, s.FileName);
      finally
        r.Free;
      end;
    finally
      bitmap.Free;
    end;
  end;
end;

procedure TMultiFormatReaderTest.AllReadsAllFormats;
type
  TSample = record
    FileName: string;
    Format: TBarcodeFormat;
    Text: string;
    // read by Auto too
    Auto: Boolean;
  end;
const
  SAMPLES: array [0 .. 12] of TSample = (
    (FileName: 'zxing-cpp\qrcode-1\1.png'; Format: TBarcodeFormat.QR_CODE;
    Text: ''; Auto: true),
    (FileName: 'EAN13.png'; Format: TBarcodeFormat.EAN_13;
    Text: '1646265651114'; Auto: true),
    (FileName: 'code39.png'; Format: TBarcodeFormat.CODE_39; Text: '1234567';
    Auto: true),
    (FileName: 'zxing-cpp\codabar-1\01.webp'; Format: TBarcodeFormat.CODABAR;
    Text: '1234567890'; Auto: true),
    (FileName: 'dotcode\dotcode-23x22-wikimedia.png';
    Format: TBarcodeFormat.DOTCODE; Text: 'Aspose.Barcode'; Auto: false),
    (FileName: 'hanxin\hanxin-v01-wikimedia.png';
    Format: TBarcodeFormat.HAN_XIN; Text: 'Aspose.BarCode'; Auto: false),
    (FileName: 'postal\KIX\KIX_01_1231FZ13XHS.png'; Format: TBarcodeFormat.KIX;
    Text: '1231FZ13XHS'; Auto: false),
    (FileName: 'postal\RM4SCC\RM4SCC_01_SW1A1AA.png';
    Format: TBarcodeFormat.RM4SCC; Text: 'SW1A1AA'; Auto: false),
    (FileName: 'postal\POSTNET\POSTNET_01_12345.png';
    Format: TBarcodeFormat.POSTNET; Text: '12345'; Auto: false),
    (FileName: 'postal\JapanPost\JapanPost_01_1000001.png';
    Format: TBarcodeFormat.JAPAN_POST; Text: '1000001'; Auto: false),
    (FileName: 'zxing-net\msi-1\01.png'; Format: TBarcodeFormat.MSI;
    Text: '123456782'; Auto: false),
    (FileName: 'pharmacode 123456 margin.png';
    Format: TBarcodeFormat.PHARMA_CODE;
    Text: '123456'; Auto: false),
    (FileName: 'pharmacode 123459.png'; Format: TBarcodeFormat.PHARMA_CODE;
    Text: '123459'; Auto: false));
begin
  for var s in SAMPLES do
  begin
    var bitmap := LoadImage(ExtractFileDir(ParamStr(0)) + '\..\..\Images\' +
      s.FileName);
    try
      for var format in [TBarcodeFormat.All, TBarcodeFormat.Auto] do
      begin
        var scanManager := TScanManager.Create(format, nil);
        try
          var r := scanManager.Scan(bitmap);
          try
            if (format = TBarcodeFormat.Auto) then
            begin
              // Auto: not the formats only read when asked for
              if not s.Auto then
                Assert.IsTrue((r = nil) or (r.BarcodeFormat <> s.Format),
                  'Auto: ' + s.FileName);
              continue;
            end;
            Assert.IsNotNull(r, 'Nil result: ' + s.FileName);
            Assert.AreEqual(Ord(s.Format), Ord(r.BarcodeFormat), s.FileName);
            if (s.Text <> '') then
              Assert.AreEqual(s.Text, r.Text, s.FileName);
          finally
            r.Free;
          end;
        finally
          scanManager.Free;
        end;
      end;
    finally
      bitmap.Free;
    end;
  end;
end;

procedure TMultiFormatReaderTest.ScanAsyncCallsBackInTheMainThread;
var
  scanner: IScanManager;
  done: Boolean;
  text: string;
  format: TBarcodeFormat;
  threadId: TThreadID;
begin
  var bitmap := LoadImage(ExtractFileDir(ParamStr(0)) +
    '\..\..\Images\zxing-cpp\qrcode-1\1.png');
  try
    scanner := TScanManager.Create(TBarcodeFormat.QR_CODE, nil);
    done := false;
    text := '';
    threadId := 0;
    Assert.IsFalse(scanner.Scanning, 'idle before');
    scanner.ScanAsync(bitmap,
      procedure(r: TReadResult)
      begin
        threadId := TThread.CurrentThread.ThreadID;
        if (r <> nil) then
        begin
          text := r.Text;
          format := r.BarcodeFormat;
          r.Free;
        end;
        done := true;
      end);
    // the bitmap is free again: the pixels were taken
    bitmap.Free;
    bitmap := nil;
    // the callback comes through Synchronize: wait in the main thread
    var started := Now;
    while not done and (Now - started < 20 / SecsPerDay) do
      CheckSynchronize(10);
    Assert.IsTrue(done, 'completed within 20 s');
    Assert.IsTrue(threadId = MainThreadID, 'callback in the main thread');
    Assert.IsFalse(scanner.Scanning, 'not scanning any more');
    Assert.AreNotEqual('', text, 'a QR Code read');
    Assert.AreEqual(Ord(TBarcodeFormat.QR_CODE), Ord(format));
    // a second scan can be started from the main thread again
    scanner := nil;
  finally
    bitmap.Free;
  end;
end;

procedure TMultiFormatReaderTest.ScanAllAsyncCallsBackWithTheList;
var
  scanner: IScanManager;
  done: Boolean;
  count: Integer;
  threadId: TThreadID;
begin
  var bitmap := LoadImage(ExtractFileDir(ParamStr(0)) +
    '\..\..\Images\zxing-cpp\qrcode-1\1.png');
  try
    scanner := TScanManager.Create(TBarcodeFormat.QR_CODE, nil);
    done := false;
    count := -1;
    threadId := 0;
    scanner.ScanAllAsync(bitmap,
      procedure(results: TObjectList<TReadResult>)
      begin
        threadId := TThread.CurrentThread.ThreadID;
        count := results.Count;
        results.Free;
        done := true;
      end);
    Assert.IsTrue(scanner.Scanning or done, 'scanning');
    var started := Now;
    while not done and (Now - started < 20 / SecsPerDay) do
      CheckSynchronize(10);
    Assert.IsTrue(done, 'completed within 20 s');
    Assert.IsTrue(threadId = MainThreadID, 'callback in the main thread');
    Assert.AreEqual(1, count, 'one QR Code');
    Assert.IsFalse(scanner.Scanning);
  finally
    bitmap.Free;
  end;
end;

procedure TMultiFormatReaderTest.ScanWithoutAScanManager;
var
  done: Boolean;
  text: string;
begin
  var bitmap := LoadImage(ExtractFileDir(ParamStr(0)) +
    '\..\..\Images\zxing-cpp\qrcode-1\1.png');
  try
    // one call, one result
    var r := TScanManager.Scan(bitmap, TBarcodeFormat.QR_CODE);
    try
      Assert.IsNotNull(r);
      Assert.AreNotEqual('', r.Text);
      text := r.Text;
    finally
      r.Free;
    end;
    // all, with Auto
    var all := TScanManager.ScanAll(bitmap, TBarcodeFormat.Auto);
    try
      Assert.AreEqual(1, all.Count);
      Assert.AreEqual(text, all[0].Text);
    finally
      all.Free;
    end;
    // in a task: the scan manager made for the call lives until the callback
    done := false;
    TScanManager.ScanAsync(bitmap, TBarcodeFormat.QR_CODE,
      procedure(r: TReadResult)
      begin
        Assert.IsNotNull(r);
        Assert.AreEqual(text, r.Text);
        r.Free;
        done := true;
      end);
    var started := Now;
    while not done and (Now - started < 20 / SecsPerDay) do
      CheckSynchronize(10);
    Assert.IsTrue(done, 'completed within 20 s');
  finally
    bitmap.Free;
  end;
end;

procedure TMultiFormatReaderTest.ParallelDecodingSameResults;
const
  // Data Matrix samples, also some that need the lines over the whole image
  FILES: array [0 .. 5] of string = ('datamatrix-1\#378.png',
    'datamatrix-1\0123456789.png', 'datamatrix-3\#749.png',
    'datamatrix-3\#794.png', 'datamatrix-4\abcd-120x8.png', 'datamatrix-1\#378.png');

  function ScanTexts(parallel: Boolean): string;
  begin
    Result := '';
    var before := ParallelDecoding;
    ParallelDecoding := parallel;
    try
      for var name in FILES do
      begin
        var bitmap := LoadImage(ExtractFileDir(ParamStr(0)) +
          '\..\..\Images\zxing-cpp\' + name);
        try
          var hints := TDictionary<TDecodeHintType, TObject>.Create;
          hints.Add(TDecodeHintType.TRY_HARDER, nil);
          var scanner: IScanManager := TScanManager.Create
            (TBarcodeFormat.DATA_MATRIX, hints);
          var all := scanner.ScanAll(bitmap);
          try
            for var r in all do
              Result := Result + name + ': ' + r.Text + ' @' +
                r.ResultPoints[0].ToString + #13#10;
            if (all.Count = 0) then
              Result := Result + name + ': -'#13#10;
          finally
            all.Free;
          end;
        finally
          bitmap.Free;
        end;
      end;
    finally
      ParallelDecoding := before;
    end;
  end;

begin
  if (TThread.ProcessorCount < 2) then
    exit;
  var sequential := ScanTexts(false);
  var parallel := ScanTexts(true);
  Assert.AreNotEqual('', sequential);
  Assert.IsFalse(sequential.Contains(': -'), 'all read: ' + sequential);
  Assert.AreEqual(sequential, parallel);
end;

initialization

TDUnitX.RegisterTestFixture(TMultiFormatReaderTest);

end.
