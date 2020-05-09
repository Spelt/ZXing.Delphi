unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TmainFrm = class(TForm)
    image: TImage;
    Log: TMemo;
    bottomPanel: TPanel;
    Splitter_38A8D14A: TSplitter;
    btnLoadFromFile: TButton;
    openDlg: TOpenDialog;
    procedure btnLoadFromFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mainFrm: TmainFrm;

implementation
uses vcl.imaging.pngImage,
     vcl.imaging.jpeg,
     ZXing.ReadResult,
     ZXing.BarCodeFormat,
     ZXing.ScanManager;


{$R *.dfm}

procedure TmainFrm.btnLoadFromFileClick(Sender: TObject);
var  ReadResult: TReadResult;
     ScanManager: TScanManager;
     bmp:VCL.Graphics.TBitmap; // just to be sure we are really using VCL bitmaps
begin
  if not OpenDlg.Execute then exit;
  image.Picture.LoadFromFile(openDlg.FileName);
  ReadResult := nil;
  ScanManager := nil;
  bmp := nil;
  try
    bmp:= TBitmap.Create;

    bmp.assign (image.Picture.Graphic);
    ScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);
    ReadResult := ScanManager.Scan(bmp);
    if ReadResult<>nil then
      log.Lines.Text := ReadResult.text
    else
      log.Lines.Text := 'Unreadable!';
  finally
  	bmp.Free;
    ScanManager.Free;
    ReadResult.Free;
  end;
end;

end.
