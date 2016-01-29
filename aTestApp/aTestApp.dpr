program aTestApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {MainForm},
  Code93Reader in '..\Lib\Classes\1D Barcodes\Code93Reader.pas',
  Code128Reader in '..\Lib\Classes\1D Barcodes\Code128Reader.pas',
  MultiFormatOneDReader in '..\Lib\Classes\1D Barcodes\MultiFormatOneDReader.pas',
  OneDReader in '..\Lib\Classes\1D Barcodes\OneDReader.pas',
  Reader in '..\Lib\Classes\1D Barcodes\Reader.pas',
  BitMatrixParser in '..\Lib\Classes\2D Barcodes\Decoder\BitMatrixParser.pas',
  Datablock in '..\Lib\Classes\2D Barcodes\Decoder\Datablock.pas',
  Datamask in '..\Lib\Classes\2D Barcodes\Decoder\Datamask.pas',
  DecodedBitStreamParser in '..\Lib\Classes\2D Barcodes\Decoder\DecodedBitStreamParser.pas',
  ErrorCorrectionLevel in '..\Lib\Classes\2D Barcodes\Decoder\ErrorCorrectionLevel.pas',
  FormatInformation in '..\Lib\Classes\2D Barcodes\Decoder\FormatInformation.pas',
  GenericGF in '..\Lib\Classes\2D Barcodes\Decoder\GenericGF.pas',
  Mode in '..\Lib\Classes\2D Barcodes\Decoder\Mode.pas',
  QRCodeDecoderMetadata in '..\Lib\Classes\2D Barcodes\Decoder\QRCodeDecoderMetadata.pas',
  QRDecoder in '..\Lib\Classes\2D Barcodes\Decoder\QRDecoder.pas',
  ReedSolomonDecoder in '..\Lib\Classes\2D Barcodes\Decoder\ReedSolomonDecoder.pas',
  Version in '..\Lib\Classes\2D Barcodes\Decoder\Version.pas',
  AlignmentPattern in '..\Lib\Classes\2D Barcodes\Detector\AlignmentPattern.pas',
  AlignmentPatternFinder in '..\Lib\Classes\2D Barcodes\Detector\AlignmentPatternFinder.pas',
  Detector in '..\Lib\Classes\2D Barcodes\Detector\Detector.pas',
  FinderPattern in '..\Lib\Classes\2D Barcodes\Detector\FinderPattern.pas',
  FinderPatternFinder in '..\Lib\Classes\2D Barcodes\Detector\FinderPatternFinder.pas',
  FinderPatternInfo in '..\Lib\Classes\2D Barcodes\Detector\FinderPatternInfo.pas',
  QRCodeReader in '..\Lib\Classes\2D Barcodes\QRCodeReader.pas',
  BarcodeFormat in '..\Lib\Classes\Common\BarcodeFormat.pas',
  BitArray in '..\Lib\Classes\Common\BitArray.pas',
  Bitmatrix in '..\Lib\Classes\Common\Bitmatrix.pas',
  BitSource in '..\Lib\Classes\Common\BitSource.pas',
  CharacterSetECI in '..\Lib\Classes\Common\CharacterSetECI.pas',
  DecodeHintType in '..\Lib\Classes\Common\DecodeHintType.pas',
  DecoderResult in '..\Lib\Classes\Common\DecoderResult.pas',
  DefaultGridSampler in '..\Lib\Classes\Common\DefaultGridSampler.pas',
  DetectorResult in '..\Lib\Classes\Common\DetectorResult.pas',
  Helpers in '..\Lib\Classes\Common\Helpers.pas',
  MathUtils in '..\Lib\Classes\Common\MathUtils.pas',
  MultiFormatReader in '..\Lib\Classes\Common\MultiFormatReader.pas',
  PerspectiveTransform in '..\Lib\Classes\Common\PerspectiveTransform.pas',
  ReadResult in '..\Lib\Classes\Common\ReadResult.pas',
  ResultMetadataType in '..\Lib\Classes\Common\ResultMetadataType.pas',
  ResultPoint in '..\Lib\Classes\Common\ResultPoint.pas',
  StringUtils in '..\Lib\Classes\Common\StringUtils.pas',
  Binarizer in '..\Lib\Classes\Filtering\Binarizer.pas',
  BinaryBitmap in '..\Lib\Classes\Filtering\BinaryBitmap.pas',
  GlobalHistogramBinarizer in '..\Lib\Classes\Filtering\GlobalHistogramBinarizer.pas',
  HybridBinarizer in '..\Lib\Classes\Filtering\HybridBinarizer.pas',
  LuminanceSource in '..\Lib\Classes\Filtering\LuminanceSource.pas',
  RGBLuminanceSource in '..\Lib\Classes\Filtering\RGBLuminanceSource.Pas',
  ScanManager in '..\Lib\Classes\ScanManager.pas',
  ITFReader in '..\Lib\Classes\1D Barcodes\ITFReader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
