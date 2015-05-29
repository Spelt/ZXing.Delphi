unit ReadResult;

interface

uses
  SysUtils, Generics.Collections,
  ResultPoint, ResultMetadataType, BarcodeFormat;

type

  TReadResult = class
  private
    FText: string;
    FResultMetadata: TDictionary<TResultMetadataType, TObject>;
    FRawBytes: TArray<byte>;
    FResultPoints: TArray<TResultPoint>;
    FBarcodeFormat: TBarcodeFormat;

  public


    /// <returns>raw text encoded by the barcode, if applicable, otherwise <code>null</code></returns>
    property Text: string read FText write FText;
    /// <returns>raw bytes encoded by the barcode, if applicable, otherwise <code>null</code></returns>
    property RawBytes: TArray<byte> read FRawBytes write FRawBytes;
    /// <returns>
    /// points related to the barcode in the image. These are typically points
    /// identifying finder patterns or the corners of the barcode. The exact meaning is
    /// specific to the type of barcode that was decoded.
    /// </returns>
    property ResultPoints: TArray<TResultPoint> read FResultPoints
      write FResultPoints;

    property BarcodeFormat: TBarcodeFormat read FBarcodeFormat
      write FBarcodeFormat;

    property ResultMetaData: TDictionary<TResultMetadataType, TObject>
      read FResultMetadata write FResultMetadata;

    constructor Create(Text: string; RawBytes: TArray<byte>;
      ResultPoints: TArray<TResultPoint>; BarcodeFormat: TBarcodeFormat);

    procedure putMetadata(ResultMetadataType: TResultMetadataType;
      value: TObject);

    procedure PutAllMetaData(metaData: TDictionary<TResultMetadataType,
      TObject>);

  end;

implementation

{ TReadResult }

constructor TReadResult.Create(Text: string; RawBytes: TArray<byte>;
  ResultPoints: TArray<TResultPoint>; BarcodeFormat: TBarcodeFormat);
begin
  if ((Text = '') and (RawBytes = nil)) then
  begin
    raise EArgumentException.Create('Text and bytes are null.');
  end;

  FText := Text;
  FRawBytes := RawBytes;
  FResultPoints := ResultPoints;
  FResultMetadata := nil;
  FBarcodeFormat := BarcodeFormat;
end;

procedure TReadResult.PutAllMetaData(metaData: TDictionary<TResultMetadataType,
  TObject>);

var
  key: TResultMetadataType;

begin
  if (metaData <> nil) then
  begin

    if (FResultMetadata = nil) then
    begin
      FResultMetadata := metaData;
    end
    else
    begin

      for key in metaData.Keys do
      begin
        FResultMetadata[key] := metaData[key];
      end;

    end;
  end;
end;

procedure TReadResult.putMetadata(ResultMetadataType: TResultMetadataType;
  value: TObject);
begin
  if (FResultMetadata = nil) then
  begin
    FResultMetadata := TDictionary<TResultMetadataType, TObject>.Create();
  end;

  try
//    FResultMetadata[ResultMetadataType] := value;
  except

  end;


end;

end.
