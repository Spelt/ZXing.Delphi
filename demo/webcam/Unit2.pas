unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  VFrames, VSample;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fVideoImage: TVideoImage;

    procedure NewVideoFrameEvent(Sender: TObject; Width, Height: integer; DataPtr: pointer);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
 ZXing.ReadResult,
 ZXing.BarCodeFormat,
 ZXing.ScanManager;

procedure TForm2.Button1Click(Sender: TObject);
var
  listcams: TStringList;
begin
  listcams := TStringList.Create;
  try
    fVideoImage.GetListOfDevices(listcams);
    if not(listcams.count = 0) then
    begin
      fVideoImage.VideoStart(listcams[0]);
    end;
  finally
    listcams.Free;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  fVideoImage.VideoStop;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  fVideoImage := TVideoImage.Create();
  fVideoImage.OnNewVideoFrame := NewVideoFrameEvent;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  fVideoImage.Free;
end;

procedure TForm2.NewVideoFrameEvent(Sender: TObject; Width, Height: integer; DataPtr: pointer);
var
  bitmap: TBitmap;

  ReadResult: TReadResult;
  ScanManager: TScanManager;
begin
  bitmap := TBitmap.Create;
  try
    bitmap.PixelFormat := pf24bit;
    fVideoImage.GetBitmap(bitmap);
    Image1.Picture.Assign(bitmap);

    // scan code
    try
      ScanManager := TScanManager.Create(TBarcodeFormat.AUTO, nil);
      ReadResult := ScanManager.Scan(bitmap);
      if ReadResult<>nil then
        Memo1.Lines.Insert(0, FormatFloat('000', Memo1.Lines.Count+1) + ':  ' + ReadResult.text);
    finally
      FreeAndNil(ScanManager);
      FreeAndNil(ReadResult);
    end;
  finally
    bitmap.Free;
  end;

  Application.ProcessMessages;
end;

end.
