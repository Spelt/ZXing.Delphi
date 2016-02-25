unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  ScanManager, BarcodeFormat, ReadResult, ResultPoint, FMX.Objects;

type
  TForm2 = class(TForm)
    btnCode128: TButton;
    btnQRCode: TButton;
    btnCode93: TButton;
    btnITF: TButton;
    procedure btnCode128Click(Sender: TObject);
    procedure btnQRCodeClick(Sender: TObject);
    procedure btnITFClick(Sender: TObject);
    procedure btnCode93Click(Sender: TObject);
  private
    function GetImage(Filename: string): TBitmap;
    function Decode(Filename: String; CodeFormat: TBarcodeFormat): TReadResult;

  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}
{ TForm2 }

function TForm2.Decode(Filename: String; CodeFormat: TBarcodeFormat)
  : TReadResult;
var
  bmp: TBitmap;
  ScanManager: TScanManager;
  rs: TReadResult;
  ResultPoint: TResultPoint;
begin

  bmp := GetImage(Filename);
  try
    ScanManager := TScanManager.Create(CodeFormat, nil);
    rs := ScanManager.Scan(bmp);
  finally
    FreeAndNil(bmp);
    FreeAndNil(ScanManager);

    if (rs <> nil) then
    begin
      for ResultPoint in rs.ResultPoints do
        ResultPoint.Free;
      rs.ResultPoints := nil;
      FreeAndNil(rs);
    end;
  end;

  result := nil;

end;

function TForm2.GetImage(Filename: string): TBitmap;
var
  img: TImage;
  fs: string;

begin
  img := TImage.Create(nil);
  try

    fs := ExtractFileDir(ParamStr(0)) + '\..\..\..\unitTest\images\' + Filename;
    img.Bitmap.LoadFromFile(fs);
    result := TBitmap.Create;
    result.Assign(img.Bitmap);

  finally
    FreeAndNil(img);
  end;
end;

procedure TForm2.btnCode128Click(Sender: TObject);
begin
  Decode('Code128.png', TBarcodeFormat.CODE_128);
end;

procedure TForm2.btnCode93Click(Sender: TObject);
begin
  Decode('Code93-1.png', TBarcodeFormat.CODE_93);
end;

procedure TForm2.btnITFClick(Sender: TObject);
begin
  Decode('ITF-1.png', TBarcodeFormat.ITF);
end;

procedure TForm2.btnQRCodeClick(Sender: TObject);
begin
  Decode('QRCode.png', TBarcodeFormat.QR_CODE);
end;

end.
