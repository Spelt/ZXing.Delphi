unit MainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Edit,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  ZXing.ScanManager,
  ZXing.ResultMetaDataType,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.ResultPoint;

type
  TForm2 = class(TForm)
    btnCode128: TButton;
    btnCode93: TButton;
    btnEAN13: TButton;
    btnITF: TButton;
    btnQRCode: TButton;
    btnDataMatrix: TButton;
    btnDummy: TButton;
    imgResult: TImage;
    edResult: TEdit;
    btnEAN8: TButton;
    btnUPC_a: TButton;
    btnUPCE: TButton;
    btnCode39: TButton;
    procedure btnCode128Click(Sender: TObject);
    procedure btnQRCodeClick(Sender: TObject);
    procedure btnDataMatrixClick(Sender: TObject);
    procedure btnITFClick(Sender: TObject);
    procedure btnCode93Click(Sender: TObject);
    procedure btnDummyClick(Sender: TObject);
    procedure btnEAN13Click(Sender: TObject);
    procedure btnEAN8Click(Sender: TObject);
    procedure btnUPC_aClick(Sender: TObject);
    procedure btnUPCEClick(Sender: TObject);
    procedure btnCode39Click(Sender: TObject);
  private
    procedure OnScanManagerResultPoint(const point: IResultPoint);
    function GetImage(const Filename: string): TBitmap;
    function decode(const Filename: String;
      const CodeFormat: TBarcodeFormat): TReadResult;
  end;

var
  Form2: TForm2;

implementation


{$R *.fmx}
{ TForm2 }

function TForm2.decode(const Filename: String;
  const CodeFormat: TBarcodeFormat): TReadResult;
var
  bmp: TBitmap;
  ScanManager: TScanManager;
  rs: TReadResult;
  obj : IMetaData;
  strMetadata: IStringMetaData;
  ResultPoint: IResultPoint;
const
  iSize = 5;
begin
  bmp := GetImage(Filename);
  try
    ScanManager := TScanManager.Create(CodeFormat, nil);
    //ScanManager.OnResultPoint := Self.OnScanManagerResultPoint;

    rs := ScanManager.Scan(bmp);
    if (rs <> nil) then
    begin
      edResult.Text := rs.Text;
      if (rs.ResultMetaData <> nil) and
          rs.ResultMetaData.ContainsKey(TResultMetaDataType.ERROR_CORRECTION_LEVEL) then
      begin
        obj := rs.ResultMetaData.Items[TResultMetaDataType.ERROR_CORRECTION_LEVEL];
        if Supports(obj,IStringMetaData,strMetadata)
        then
           edResult.Text := edResult.Text + ' (ECLevel: ' + strMetadata.Value + ')';
      end;
      bmp.Canvas.BeginScene;
      try
        bmp.Canvas.Fill.Color := TAlphaColors.Darkgoldenrod;
        bmp.Canvas.Fill.Kind := TBrushKind.Solid;
        for ResultPoint in rs.ResultPoints do
          bmp.Canvas.FillEllipse(TRectF.Create((ResultPoint.x - iSize),
                                               (ResultPoint.y - iSize),
                                               (ResultPoint.x + iSize),
                                               (ResultPoint.y + iSize)), 1);
      finally
        bmp.Canvas.EndScene;

      end;
      imgResult.Bitmap.Assign(bmp);
    end;
  finally
    FreeAndNil(bmp);
    FreeAndNil(ScanManager);
    FreeAndNil(rs);
  end;

  Result := nil;
end;

procedure TForm2.OnScanManagerResultPoint(const point: IResultPoint);
begin
  if Assigned(point)
  then
     ShowMessage(point.ToString);
end;

function TForm2.GetImage(const Filename: string): TBitmap;
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

procedure TForm2.btnCode39Click(Sender: TObject);
begin
  Decode('Code39.png', TBarcodeFormat.CODE_39);
end;

procedure TForm2.btnCode93Click(Sender: TObject);
begin
  Decode('Code93-1.png', TBarcodeFormat.CODE_93);
end;

procedure TForm2.btnDummyClick(Sender: TObject);
begin
  Decode('Dummy.png', TBarcodeFormat.Auto);
end;

procedure TForm2.btnEAN13Click(Sender: TObject);
begin
  Decode('EAN13.gif', TBarcodeFormat.EAN_13);
end;

procedure TForm2.btnEAN8Click(Sender: TObject);
begin
  Decode('EAN8.png', TBarcodeFormat.EAN_8);
end;

procedure TForm2.btnITFClick(Sender: TObject);
begin
  Decode('ITF-1.png', TBarcodeFormat.ITF);
end;

procedure TForm2.btnQRCodeClick(Sender: TObject);
begin
  Decode('QRCode.png', TBarcodeFormat.QR_CODE);
end;

procedure TForm2.btnUPCEClick(Sender: TObject);
begin
  Decode('upce.png', TBarcodeFormat.UPC_E);
end;

procedure TForm2.btnUPC_aClick(Sender: TObject);
begin
  Decode('upca.png', TBarcodeFormat.UPC_A);
end;

procedure TForm2.btnDataMatrixClick(Sender: TObject);
begin
  Decode('dmc1.png', TBarcodeFormat.DATA_MATRIX);
end;

end.
