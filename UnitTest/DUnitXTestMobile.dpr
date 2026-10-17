program DUnitXTestMobile;

{
  The unit tests on a device (Android, iOS): the same test units as
  DUnitXTest, run from the mobile GUI of DUnitX (a list of the fixtures and
  tests, a Run button, the results). The test images come along as
  images.zip (make it with images.cmd), see TestImages.pas.


  Also runs on Windows, as a FireMonkey app.
}

uses
  System.StartUpCopy,
  FMX.Forms,
  DUnitX.Loggers.MobileGUI,
  DUnitX.TestFramework,
  TestImages in 'TestImages.pas',
  Test in 'Test.pas',
  ZXingCppSamplesTest in 'ZXingCppSamplesTest.pas',
  OneDFullAsciiTest in 'OneDFullAsciiTest.pas',
  DataMatrixBitStreamTest in 'DataMatrixBitStreamTest.pas',
  MultiFormatReaderTest in 'MultiFormatReaderTest.pas',
  ReadResultTest in 'ReadResultTest.pas',
  OneDTest in 'OneDTest.pas',
  QRCodeTest in 'QRCodeTest.pas',
  AztecTest in 'AztecTest.pas',
  ReedSolomonTest in 'ReedSolomonTest.pas',
  CommonTest in 'CommonTest.pas',
  DataBarTest in 'DataBarTest.pas',
  PDF417Test in 'PDF417Test.pas',
  MicroQRTest in 'MicroQRTest.pas',
  MaxiCodeTest in 'MaxiCodeTest.pas',
  PostalTest in 'PostalTest.pas',
  StackedTest in 'StackedTest.pas',
  CompositeTest in 'CompositeTest.pas',
  DotCodeTest in 'DotCodeTest.pas',
  HanXinTest in 'HanXinTest.pas',
  Benchmark.Samples in '..\benchmark\Benchmark.Samples.pas',
  Benchmark.Images in '..\benchmark\Benchmark.Images.pas',
  Benchmark.Runner in '..\benchmark\Benchmark.Runner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMobileGUITestRunner, MobileGUITestRunner);
  Application.Run;
end.
