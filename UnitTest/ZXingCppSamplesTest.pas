unit ZXingCppSamplesTest;

{
  * Regression test on the black box test samples of zxing-cpp
  * (Images\zxing-cpp, see the README there).
  *
  * Every folder is read in the modes slow (TRY_HARDER + ENABLE_INVERSION),
  * fast (no hints) and pure (PURE_BARCODE), at the rotations the samples ask
  * for, see benchmark\Benchmark.Runner.pas. The test fails when ZXing.Delphi
  * reads fewer symbols correctly than MinRead, or has more wrong results or
  * exceptions than MaxWrong / MaxErrors.
  *
  * The limits are the current results. When a change reads more, raise
  * them: "benchmark\Win32\FMX\ZXingBenchmark.exe -thresholds" prints the
  * current results in the format of FOLDER_LIMITS.
}

interface

uses
  DUnitX.TestFramework,
  Benchmark.Samples;

type
  TLimit = record
    MinRead: Integer;
    MaxWrong: Integer;
    MaxErrors: Integer;
  end;

  TFolderLimits = record
    Folder: string;
    Limits: array [TTestMode] of TLimit;
  end;

const
  // slow, fast, pure. Results after the 1D decoders of zxing-cpp (phase 7).
  // The wrong results in upce-2 are a misread of 509689-3!.webp, an image
  // that zxing-cpp can not read either.
  FOLDER_LIMITS: array [0 .. 49] of TFolderLimits = (
    (Folder: 'aztec-1'; Limits: ((MinRead: 132; MaxWrong: 0; MaxErrors: 0), (MinRead: 128; MaxWrong: 0; MaxErrors: 0), (MinRead: 128; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'aztec-2'; Limits: ((MinRead: 62; MaxWrong: 0; MaxErrors: 0), (MinRead: 61; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'codabar-1'; Limits: ((MinRead: 22; MaxWrong: 0; MaxErrors: 0), (MinRead: 22; MaxWrong: 0; MaxErrors: 0), (MinRead: 22; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'codabar-2'; Limits: ((MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 4; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'code128-1'; Limits: ((MinRead: 12; MaxWrong: 0; MaxErrors: 0), (MinRead: 12; MaxWrong: 0; MaxErrors: 0), (MinRead: 8; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'code128-2'; Limits: ((MinRead: 32; MaxWrong: 0; MaxErrors: 0), (MinRead: 32; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'code39-1'; Limits: ((MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 6; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'code39-2'; Limits: ((MinRead: 12; MaxWrong: 0; MaxErrors: 0), (MinRead: 12; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'code39ext-1'; Limits: ((MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 6; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'code93-1'; Limits: ((MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'databarExp-1'; Limits: ((MinRead: 74; MaxWrong: 0; MaxErrors: 0), (MinRead: 74; MaxWrong: 0; MaxErrors: 0), (MinRead: 74; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'databarExp-2'; Limits: ((MinRead: 14; MaxWrong: 0; MaxErrors: 0), (MinRead: 14; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'databarExp-3'; Limits: ((MinRead: 236; MaxWrong: 0; MaxErrors: 0), (MinRead: 236; MaxWrong: 0; MaxErrors: 0), (MinRead: 236; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'databarExpStk-1'; Limits: ((MinRead: 128; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'databarLtd-1'; Limits: ((MinRead: 4; MaxWrong: 0; MaxErrors: 0), (MinRead: 4; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'databarOmni-1'; Limits: ((MinRead: 22; MaxWrong: 0; MaxErrors: 0), (MinRead: 20; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'databarStk-1'; Limits: ((MinRead: 12; MaxWrong: 0; MaxErrors: 0), (MinRead: 10; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'datamatrix-1'; Limits: ((MinRead: 110; MaxWrong: 0; MaxErrors: 0), (MinRead: 29; MaxWrong: 0; MaxErrors: 0), (MinRead: 28; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'datamatrix-2'; Limits: ((MinRead: 52; MaxWrong: 0; MaxErrors: 0), (MinRead: 13; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'datamatrix-3'; Limits: ((MinRead: 111; MaxWrong: 0; MaxErrors: 0), (MinRead: 28; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'datamatrix-4'; Limits: ((MinRead: 84; MaxWrong: 0; MaxErrors: 0), (MinRead: 21; MaxWrong: 0; MaxErrors: 0), (MinRead: 19; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'datamatrix-5'; Limits: ((MinRead: 8; MaxWrong: 0; MaxErrors: 0), (MinRead: 2; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'dxfilmedge-1'; Limits: ((MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 1; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'ean13-1'; Limits: ((MinRead: 36; MaxWrong: 0; MaxErrors: 0), (MinRead: 36; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'ean13-2'; Limits: ((MinRead: 44; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'ean13-ext-1'; Limits: ((MinRead: 10; MaxWrong: 0; MaxErrors: 0), (MinRead: 9; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'ean8-1'; Limits: ((MinRead: 18; MaxWrong: 0; MaxErrors: 0), (MinRead: 18; MaxWrong: 0; MaxErrors: 0), (MinRead: 16; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'itf-1'; Limits: ((MinRead: 28; MaxWrong: 0; MaxErrors: 0), (MinRead: 26; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'itf-2'; Limits: ((MinRead: 12; MaxWrong: 0; MaxErrors: 0), (MinRead: 12; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'micropdf417-1'; Limits: ((MinRead: 21; MaxWrong: 0; MaxErrors: 0), (MinRead: 20; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'maxicode-1'; Limits: ((MinRead: 9; MaxWrong: 0; MaxErrors: 0), (MinRead: 9; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'maxicode-2'; Limits: ((MinRead: 8; MaxWrong: 0; MaxErrors: 0), (MinRead: 8; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'microqrcode-1'; Limits: ((MinRead: 32; MaxWrong: 0; MaxErrors: 0), (MinRead: 32; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'multi-1'; Limits: ((MinRead: 20; MaxWrong: 0; MaxErrors: 0), (MinRead: 8; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'none-1'; Limits: ((MinRead: 0; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'none-2'; Limits: ((MinRead: 0; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'pdf417-1'; Limits: ((MinRead: 64; MaxWrong: 0; MaxErrors: 0), (MinRead: 30; MaxWrong: 0; MaxErrors: 0), (MinRead: 62; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'pdf417-2'; Limits: ((MinRead: 14; MaxWrong: 0; MaxErrors: 0), (MinRead: 14; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'pdf417-3'; Limits: ((MinRead: 72; MaxWrong: 0; MaxErrors: 0), (MinRead: 72; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'qrcode-1'; Limits: ((MinRead: 96; MaxWrong: 0; MaxErrors: 0), (MinRead: 96; MaxWrong: 0; MaxErrors: 0), (MinRead: 24; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'qrcode-2'; Limits: ((MinRead: 141; MaxWrong: 0; MaxErrors: 0), (MinRead: 64; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'qrcode-3'; Limits: ((MinRead: 207; MaxWrong: 0; MaxErrors: 0), (MinRead: 104; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'qrcode-4'; Limits: ((MinRead: 71; MaxWrong: 0; MaxErrors: 0), (MinRead: 35; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'rmqrcode-1'; Limits: ((MinRead: 8; MaxWrong: 0; MaxErrors: 0), (MinRead: 4; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'telepen-1'; Limits: ((MinRead: 8; MaxWrong: 0; MaxErrors: 0), (MinRead: 8; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'upca-1'; Limits: ((MinRead: 12; MaxWrong: 0; MaxErrors: 0), (MinRead: 12; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'upca-2'; Limits: ((MinRead: 84; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'upca-ext-1'; Limits: ((MinRead: 10; MaxWrong: 0; MaxErrors: 0), (MinRead: 10; MaxWrong: 0; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'upce-1'; Limits: ((MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 6; MaxWrong: 0; MaxErrors: 0), (MinRead: 6; MaxWrong: 0; MaxErrors: 0))),
    (Folder: 'upce-2'; Limits: ((MinRead: 34; MaxWrong: 2; MaxErrors: 0), (MinRead: 31; MaxWrong: 2; MaxErrors: 0), (MinRead: 0; MaxWrong: 0; MaxErrors: 0)))
  );

type
  [TestFixture]
  TZXingCppSamplesTest = class(TObject)
  public
    [Test]
    [TestCase('aztec-1', 'aztec-1')]
    [TestCase('aztec-2', 'aztec-2')]
    [TestCase('codabar-1', 'codabar-1')]
    [TestCase('codabar-2', 'codabar-2')]
    [TestCase('code128-1', 'code128-1')]
    [TestCase('code128-2', 'code128-2')]
    [TestCase('code39-1', 'code39-1')]
    [TestCase('code39-2', 'code39-2')]
    [TestCase('code39ext-1', 'code39ext-1')]
    [TestCase('code93-1', 'code93-1')]
    [TestCase('databarExp-1', 'databarExp-1')]
    [TestCase('databarExp-2', 'databarExp-2')]
    [TestCase('databarExp-3', 'databarExp-3')]
    [TestCase('databarExpStk-1', 'databarExpStk-1')]
    [TestCase('databarLtd-1', 'databarLtd-1')]
    [TestCase('databarOmni-1', 'databarOmni-1')]
    [TestCase('databarStk-1', 'databarStk-1')]
    [TestCase('datamatrix-1', 'datamatrix-1')]
    [TestCase('datamatrix-2', 'datamatrix-2')]
    [TestCase('datamatrix-3', 'datamatrix-3')]
    [TestCase('datamatrix-4', 'datamatrix-4')]
    [TestCase('datamatrix-5', 'datamatrix-5')]
    [TestCase('dxfilmedge-1', 'dxfilmedge-1')]
    [TestCase('ean13-1', 'ean13-1')]
    [TestCase('ean13-2', 'ean13-2')]
    [TestCase('ean13-ext-1', 'ean13-ext-1')]
    [TestCase('ean8-1', 'ean8-1')]
    [TestCase('itf-1', 'itf-1')]
    [TestCase('itf-2', 'itf-2')]
    [TestCase('micropdf417-1', 'micropdf417-1')]
    [TestCase('maxicode-1', 'maxicode-1')]
    [TestCase('maxicode-2', 'maxicode-2')]
    [TestCase('microqrcode-1', 'microqrcode-1')]
    [TestCase('multi-1', 'multi-1')]
    [TestCase('none-1', 'none-1')]
    [TestCase('none-2', 'none-2')]
    [TestCase('pdf417-1', 'pdf417-1')]
    [TestCase('pdf417-2', 'pdf417-2')]
    [TestCase('pdf417-3', 'pdf417-3')]
    [TestCase('qrcode-1', 'qrcode-1')]
    [TestCase('qrcode-2', 'qrcode-2')]
    [TestCase('qrcode-3', 'qrcode-3')]
    [TestCase('qrcode-4', 'qrcode-4')]
    [TestCase('rmqrcode-1', 'rmqrcode-1')]
    [TestCase('telepen-1', 'telepen-1')]
    [TestCase('upca-1', 'upca-1')]
    [TestCase('upca-2', 'upca-2')]
    [TestCase('upca-ext-1', 'upca-ext-1')]
    [TestCase('upce-1', 'upce-1')]
    [TestCase('upce-2', 'upce-2')]
    procedure SampleFolder(const folderName: string);
  end;

implementation

uses
  System.SysUtils,
  Winapi.ActiveX,
  Benchmark.Runner;

function SamplesDir: string;
begin
  Result := ExtractFileDir(ParamStr(0)) + '\..\..\Images\zxing-cpp\';
end;

function FindLimits(const folderName: string; out limits: TFolderLimits): Boolean;
var
  l: TFolderLimits;
begin
  for l in FOLDER_LIMITS do
    if (l.Folder = folderName) then
    begin
      limits := l;
      exit(true);
    end;
  Result := false;
end;

procedure TZXingCppSamplesTest.SampleFolder(const folderName: string);
var
  folder: TSampleFolder;
  stats: TStats;
  limits: TFolderLimits;
  mode: TTestMode;
  failures, measured: string;
begin
  // WIC (used to load the images) needs COM; S_FALSE when already done
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  Assert.IsTrue(FindLimits(folderName, limits), 'No limits for ' + folderName);

  folder := TSampleFolder.Load(SamplesDir + folderName);
  Assert.IsNotNull(folder, 'Folder not found or format not supported: ' +
    SamplesDir + folderName);
  try
    stats := RunFolder(folder, [tmSlow, tmFast, tmPure], nil);
  finally
    folder.Free;
  end;

  failures := '';
  measured := '';
  for mode := Low(TTestMode) to High(TTestMode) do
  begin
    measured := measured + Format('%s: read %d (cpp %d of %d), wrong %d, errors %d;  ',
      [TestModeName(mode), stats[mode].ReadByDelphi, stats[mode].ReadByCpp,
      stats[mode].Checks, stats[mode].Wrong, stats[mode].Errors]);

    if (stats[mode].ReadByDelphi < limits.Limits[mode].MinRead) then
      failures := failures + Format('%s: read %d, expected at least %d. ',
        [TestModeName(mode), stats[mode].ReadByDelphi,
        limits.Limits[mode].MinRead]);
    if (stats[mode].Wrong > limits.Limits[mode].MaxWrong) then
      failures := failures + Format('%s: %d wrong results, at most %d allowed. ',
        [TestModeName(mode), stats[mode].Wrong, limits.Limits[mode].MaxWrong]);
    if (stats[mode].Errors > limits.Limits[mode].MaxErrors) then
      failures := failures + Format('%s: %d exceptions, at most %d allowed. ',
        [TestModeName(mode), stats[mode].Errors, limits.Limits[mode].MaxErrors]);
  end;

  TDUnitX.CurrentRunner.Log(TLogLevel.Information, folderName + ': ' + measured);
  if (failures <> '') then
    Assert.Fail(failures);
end;

initialization

TDUnitX.RegisterTestFixture(TZXingCppSamplesTest);

end.
