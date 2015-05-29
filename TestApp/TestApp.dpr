program TestApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {MainForm},
  Code128Reader in '..\Lib\Classes\1D Barcodes\Code128Reader.pas',
  MultiFormatOneDReader in '..\Lib\Classes\1D Barcodes\MultiFormatOneDReader.pas',
  OneDReader in '..\Lib\Classes\1D Barcodes\OneDReader.pas',
  Reader in '..\Lib\Classes\1D Barcodes\Reader.pas',
  BarcodeFormat in '..\Lib\Classes\Common\BarcodeFormat.pas',
  BitArray in '..\Lib\Classes\Common\BitArray.pas',
  DecodeHintType in '..\Lib\Classes\Common\DecodeHintType.pas',
  MathUtils in '..\Lib\Classes\Common\MathUtils.pas',
  MultiFormatReader in '..\Lib\Classes\Common\MultiFormatReader.pas',
  ReadResult in '..\Lib\Classes\Common\ReadResult.pas',
  ResultMetadataType in '..\Lib\Classes\Common\ResultMetadataType.pas',
  ResultPoint in '..\Lib\Classes\Common\ResultPoint.pas',
  Binarizer in '..\Lib\Classes\Filtering\Binarizer.pas',
  BinaryBitmap in '..\Lib\Classes\Filtering\BinaryBitmap.pas',
  GlobalHistogramBinarizer in '..\Lib\Classes\Filtering\GlobalHistogramBinarizer.pas',
  HybridBinarizer in '..\Lib\Classes\Filtering\HybridBinarizer.pas',
  LuminanceSource in '..\Lib\Classes\Filtering\LuminanceSource.pas',
  RGBLuminanceSource in '..\Lib\Classes\Filtering\RGBLuminanceSource.Pas',
  ZXingCommon in '..\Lib\Classes\Filtering\ZXingCommon.pas',
  ScanManager in '..\Lib\Classes\ScanManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
