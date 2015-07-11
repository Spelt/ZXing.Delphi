program dUnitXTest;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}


uses
  SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Test in 'Test.pas',
  ScanManager in '..\Lib\Classes\ScanManager.pas',
  ZXingCommon in '..\Lib\Classes\Filtering\ZXingCommon.pas',
  RGBLuminanceSource in '..\Lib\Classes\Filtering\RGBLuminanceSource.Pas',
  LuminanceSource in '..\Lib\Classes\Filtering\LuminanceSource.pas',
  HybridBinarizer in '..\Lib\Classes\Filtering\HybridBinarizer.pas',
  GlobalHistogramBinarizer in '..\Lib\Classes\Filtering\GlobalHistogramBinarizer.pas',
  BinaryBitmap in '..\Lib\Classes\Filtering\BinaryBitmap.pas',
  Binarizer in '..\Lib\Classes\Filtering\Binarizer.pas',
  ResultPoint in '..\Lib\Classes\Common\ResultPoint.pas',
  ResultMetadataType in '..\Lib\Classes\Common\ResultMetadataType.pas',
  ReadResult in '..\Lib\Classes\Common\ReadResult.pas',
  MultiFormatReader in '..\Lib\Classes\Common\MultiFormatReader.pas',
  MathUtils in '..\Lib\Classes\Common\MathUtils.pas',
  DecodeHintType in '..\Lib\Classes\Common\DecodeHintType.pas',
  BitArray in '..\Lib\Classes\Common\BitArray.pas',
  BarcodeFormat in '..\Lib\Classes\Common\BarcodeFormat.pas',
  Reader in '..\Lib\Classes\1D Barcodes\Reader.pas',
  OneDReader in '..\Lib\Classes\1D Barcodes\OneDReader.pas',
  MultiFormatOneDReader in '..\Lib\Classes\1D Barcodes\MultiFormatOneDReader.pas',
  Code128Reader in '..\Lib\Classes\1D Barcodes\Code128Reader.pas',
  Code93Reader in '..\Lib\Classes\1D Barcodes\Code93Reader.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
