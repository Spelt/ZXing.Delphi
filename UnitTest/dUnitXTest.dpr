program Project6;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Test in 'Test.pas',
  ZXing.OneD.Code93Reader in '..\Lib\Classes\1D Barcodes\ZXing.OneD.Code93Reader.pas',
  ZXing.OneD.Code128Reader in '..\Lib\Classes\1D Barcodes\ZXing.OneD.Code128Reader.pas',
  ZXing.OneD.EAN13Reader in '..\Lib\Classes\1D Barcodes\ZXing.OneD.EAN13Reader.pas',
  ZXing.OneD.EANManufacturerOrgSupport in '..\Lib\Classes\1D Barcodes\ZXing.OneD.EANManufacturerOrgSupport.pas',
  ZXing.OneD.ITFReader in '..\Lib\Classes\1D Barcodes\ZXing.OneD.ITFReader.pas',
  ZXing.OneD.OneDReader in '..\Lib\Classes\1D Barcodes\ZXing.OneD.OneDReader.pas',
  ZXing.OneD.UPCEANExtension2Support in '..\Lib\Classes\1D Barcodes\ZXing.OneD.UPCEANExtension2Support.pas',
  ZXing.OneD.UPCEANExtension5Support in '..\Lib\Classes\1D Barcodes\ZXing.OneD.UPCEANExtension5Support.pas',
  ZXing.OneD.UPCEANExtensionSupport in '..\Lib\Classes\1D Barcodes\ZXing.OneD.UPCEANExtensionSupport.pas',
  ZXing.OneD.UPCEANReader in '..\Lib\Classes\1D Barcodes\ZXing.OneD.UPCEANReader.pas',
  ZXing.QrCode.QRCodeReader in '..\Lib\Classes\2D Barcodes\ZXing.QrCode.QRCodeReader.pas',
  ZXing.Datamatrix.DataMatrixReader in '..\Lib\Classes\2D Barcodes\ZXing.Datamatrix.DataMatrixReader.pas',
  ZXing.QrCode.Internal.DataBlock in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.DataBlock.pas',
  ZXing.QrCode.Internal.DataMask in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.DataMask.pas',
  ZXing.QrCode.Internal.DecodedBitStreamParser in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.DecodedBitStreamParser.pas',
  ZXing.QrCode.Internal.Decoder in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.Decoder.pas',
  ZXing.QrCode.Internal.ErrorCorrectionLevel in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.ErrorCorrectionLevel.pas',
  ZXing.QrCode.Internal.FormatInformation in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.FormatInformation.pas',
  ZXing.QrCode.Internal.Mode in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.Mode.pas',
  ZXing.QrCode.Internal.QRCodeDecoderMetaData in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.QRCodeDecoderMetaData.pas',
  ZXing.QrCode.Internal.Version in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.Version.pas',
  ZXing.Datamatrix.Internal.BitMatrixParser in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.Datamatrix.Internal.BitMatrixParser.pas',
  ZXing.Datamatrix.Internal.DataBlock in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.Datamatrix.Internal.DataBlock.pas',
  ZXing.Datamatrix.Internal.DecodedBitStreamParser in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.Datamatrix.Internal.DecodedBitStreamParser.pas',
  ZXing.Datamatrix.Internal.Decoder in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.Datamatrix.Internal.Decoder.pas',
  ZXing.Datamatrix.Internal.Version in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.Datamatrix.Internal.Version.pas',
  ZXing.QrCode.Internal.BitMatrixParser in '..\Lib\Classes\2D Barcodes\Decoder\ZXing.QrCode.Internal.BitMatrixParser.pas',
  ZXing.QrCode.Internal.AlignmentPattern in '..\Lib\Classes\2D Barcodes\Detector\ZXing.QrCode.Internal.AlignmentPattern.pas',
  ZXing.QrCode.Internal.AlignmentPatternFinder in '..\Lib\Classes\2D Barcodes\Detector\ZXing.QrCode.Internal.AlignmentPatternFinder.pas',
  ZXing.QrCode.Internal.Detector in '..\Lib\Classes\2D Barcodes\Detector\ZXing.QrCode.Internal.Detector.pas',
  ZXing.QrCode.Internal.FinderPattern in '..\Lib\Classes\2D Barcodes\Detector\ZXing.QrCode.Internal.FinderPattern.pas',
  ZXing.QrCode.Internal.FinderPatternFinder in '..\Lib\Classes\2D Barcodes\Detector\ZXing.QrCode.Internal.FinderPatternFinder.pas',
  ZXing.QrCode.Internal.FinderPatternInfo in '..\Lib\Classes\2D Barcodes\Detector\ZXing.QrCode.Internal.FinderPatternInfo.pas',
  ZXing.Datamatrix.Internal.Detector in '..\Lib\Classes\2D Barcodes\Detector\ZXing.Datamatrix.Internal.Detector.pas',
  DecoderResult in '..\Lib\Classes\Common\DecoderResult.pas',
  DefaultGridSampler in '..\Lib\Classes\Common\DefaultGridSampler.pas',
  Helpers in '..\Lib\Classes\Common\Helpers.pas',
  MultiFormatReader in '..\Lib\Classes\Common\MultiFormatReader.pas',
  ResultMetadataType in '..\Lib\Classes\Common\ResultMetadataType.pas',
  StringUtils in '..\Lib\Classes\Common\StringUtils.pas',
  ZXing.BarcodeFormat in '..\Lib\Classes\Common\ZXing.BarcodeFormat.pas',
  ZXing.Common.BitArray in '..\Lib\Classes\Common\ZXing.Common.BitArray.pas',
  ZXing.Common.BitMatrix in '..\Lib\Classes\Common\ZXing.Common.BitMatrix.pas',
  ZXing.Common.DetectorResult in '..\Lib\Classes\Common\ZXing.Common.DetectorResult.pas',
  ZXing.Common.GridSampler in '..\Lib\Classes\Common\ZXing.Common.GridSampler.pas',
  ZXing.Common.PerspectiveTransform in '..\Lib\Classes\Common\ZXing.Common.PerspectiveTransform.pas',
  ZXing.EncodeHintType in '..\Lib\Classes\Common\ZXing.EncodeHintType.pas',
  ZXing.Reader in '..\Lib\Classes\Common\ZXing.Reader.pas',
  ZXing.ReadResult in '..\Lib\Classes\Common\ZXing.ReadResult.pas',
  ZXing.ResultPoint in '..\Lib\Classes\Common\ZXing.ResultPoint.pas',
  ZXing.Common.Detector.MathUtils in '..\Lib\Classes\Common\Detector\ZXing.Common.Detector.MathUtils.pas',
  ZXing.Common.Detector.WhiteRectangleDetector in '..\Lib\Classes\Common\Detector\ZXing.Common.Detector.WhiteRectangleDetector.pas',
  ZXing.Common.ReedSolomon.GenericGF in '..\Lib\Classes\Common\ReedSolomon\ZXing.Common.ReedSolomon.GenericGF.pas',
  ZXing.Common.ReedSolomon.ReedSolomonDecoder in '..\Lib\Classes\Common\ReedSolomon\ZXing.Common.ReedSolomon.ReedSolomonDecoder.pas',
  BitSource in '..\Lib\Classes\Common\BitSource.pas',
  CharacterSetECI in '..\Lib\Classes\Common\CharacterSetECI.pas',
  DecodeHintType in '..\Lib\Classes\Common\DecodeHintType.pas',
  Binarizer in '..\Lib\Classes\Filtering\Binarizer.pas',
  BinaryBitmap in '..\Lib\Classes\Filtering\BinaryBitmap.pas',
  GlobalHistogramBinarizer in '..\Lib\Classes\Filtering\GlobalHistogramBinarizer.pas',
  HybridBinarizer in '..\Lib\Classes\Filtering\HybridBinarizer.pas',
  ZXing.BaseLuminanceSource in '..\Lib\Classes\Filtering\ZXing.BaseLuminanceSource.pas',
  ZXing.InvertedLuminanceSource in '..\Lib\Classes\Filtering\ZXing.InvertedLuminanceSource.pas',
  ZXing.LuminanceSource in '..\Lib\Classes\Filtering\ZXing.LuminanceSource.pas',
  ZXing.PlanarYUVLuminanceSource in '..\Lib\Classes\Filtering\ZXing.PlanarYUVLuminanceSource.pas',
  ZXing.RGBLuminanceSource in '..\Lib\Classes\Filtering\ZXing.RGBLuminanceSource.pas',
  ScanManager in '..\Lib\Classes\ScanManager.pas';

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

