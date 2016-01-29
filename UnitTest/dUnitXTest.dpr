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
  Code93Reader in '..\Lib\Classes\1D Barcodes\Code93Reader.pas',
  QRCodeReader in '..\Lib\Classes\2D Barcodes\QRCodeReader.pas',
  QRDecoder in '..\Lib\Classes\2D Barcodes\Decoder\QRDecoder.pas',
  Bitmatrix in '..\Lib\Classes\Common\Bitmatrix.pas',
  Helpers in '..\Lib\Classes\Common\Helpers.pas',
  BitMatrixParser in '..\Lib\Classes\2D Barcodes\Decoder\BitMatrixParser.pas',
  Version in '..\Lib\Classes\2D Barcodes\Decoder\Version.pas',
  ErrorCorrectionLevel in '..\Lib\Classes\2D Barcodes\Decoder\ErrorCorrectionLevel.pas',
  FormatInformation in '..\Lib\Classes\2D Barcodes\Decoder\FormatInformation.pas',
  DecoderResult in '..\Lib\Classes\Common\DecoderResult.pas',
  Detector in '..\Lib\Classes\2D Barcodes\Detector\Detector.pas',
  DetectorResult in '..\Lib\Classes\Common\DetectorResult.pas',
  AlignmentPattern in '..\Lib\Classes\2D Barcodes\Detector\AlignmentPattern.pas',
  PerspectiveTransform in '..\Lib\Classes\Common\PerspectiveTransform.pas',
  FinderPatternInfo in '..\Lib\Classes\2D Barcodes\Detector\FinderPatternInfo.pas',
  FinderPattern in '..\Lib\Classes\2D Barcodes\Detector\FinderPattern.pas',
  QRCodeDecoderMetadata in '..\Lib\Classes\2D Barcodes\Decoder\QRCodeDecoderMetadata.pas',
  FinderPatternFinder in '..\Lib\Classes\2D Barcodes\Detector\FinderPatternFinder.pas',
  AlignmentPatternFinder in '..\Lib\Classes\2D Barcodes\Detector\AlignmentPatternFinder.pas',
  DefaultGridSampler in '..\Lib\Classes\Common\DefaultGridSampler.pas',
  ReedSolomonDecoder in '..\Lib\Classes\2D Barcodes\Decoder\ReedSolomonDecoder.pas',
  GenericGF in '..\Lib\Classes\2D Barcodes\Decoder\GenericGF.pas',
  Datablock in '..\Lib\Classes\2D Barcodes\Decoder\Datablock.pas',
  DecodedBitStreamParser in '..\Lib\Classes\2D Barcodes\Decoder\DecodedBitStreamParser.pas',
  BitSource in '..\Lib\Classes\Common\BitSource.pas',
  CharacterSetECI in '..\Lib\Classes\Common\CharacterSetECI.pas',
  StringUtils in '..\Lib\Classes\Common\StringUtils.pas',
  Mode in '..\Lib\Classes\2D Barcodes\Decoder\Mode.pas',
  Datamask in '..\Lib\Classes\2D Barcodes\Decoder\Datamask.pas',
  ScanManager in '..\Lib\Classes\ScanManager.pas',
  ITFReader in '..\Lib\Classes\1D Barcodes\ITFReader.pas';

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
