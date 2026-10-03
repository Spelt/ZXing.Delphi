{
  * Copyright 2008 ZXing authors
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *      http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.

  * Original Author: bbrown@google.com (Brian Brown)
  * Delphi Implementation by E. Spelt and K. Gossens
}
unit ZXing.ReadResult;

interface

uses
  System.SysUtils,
  Generics.Collections,
  ZXing.ResultPoint,
  ZXing.ResultMetadataType,
  ZXing.BarcodeFormat,
  ZXing.ByteSegments;

type

  IMetaData = Interface
    ['{27AEB2C5-D775-44C6-9816-3E69025CA57C}']
  end;

  IStringMetadata = interface(IMetaData)
     ['{8073F897-30AD-49B7-AE0B-B187D3373163}']
     function Value:String;
  end;

  IIntegerMetadata = interface(IMetaData)
     ['{CA60A23A-57ED-4C8E-9886-4F23B832A90C}']
     function Value:Integer;
  end;

  IByteSegmentsMetadata = interface(IMetaData)
     ['{CA60A23A-57ED-4C8E-9886-4F23B832A90C}']
     function Value:IByteSegments;
  end;

  /// <summary>
  ///  represents the metadata contained in a TReadResult and provides the
  ///  helping functions needed to create them
  /// </summary>
  TResultMetaData = class(TDictionary<TResultMetadataType, IMetaData>)
  public
     constructor Create;
     class function CreateStringMetadata(const Value:string) :IStringMetaData;
     class function CreateIntegerMetadata(const Value:integer) :IIntegerMetaData;
     class function CreateByteSegmentsMetadata(const Value:IByteSegments) :IByteSegmentsMetadata;
  end;

  TReadResult = class
  private
    FText: string;
    FTimeStamp: TDateTime;
    FResultMetadata: TResultMetaData;
    FRawBytes: TArray<Byte>;
    FResultPoints: TArray<IResultPoint>;
    FFormat: TBarcodeFormat;
    FPosition: TArray<IResultPoint>;
    FIsInverted: Boolean;
    FIsMirrored: Boolean;
    FSymbologyIdentifier: string;
    function GetPosition: TArray<IResultPoint>;
    function GetOrientation: Integer;

    procedure SetText(const AValue: String);
    procedure SetMetaData( const Value: TResultMetadata);
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="TReadResult"/> class.
    /// </summary>
    /// <param name="text">The text.</param>
    /// <param name="rawBytes">The raw bytes.</param>
    /// <param name="resultPoints">The result points.</param>
    /// <param name="format">The format.</param>
    constructor Create(const text: string; const rawBytes: TArray<Byte>;
      const resultPoints: TArray<IResultPoint>;
      const format: TBarcodeFormat); overload;

    /// <summary>
    /// Initializes a new instance of the <see cref="TReadResult"/> class.
    /// </summary>
    /// <param name="text">The text.</param>
    /// <param name="rawBytes">The raw bytes.</param>
    /// <param name="resultPoints">The result points.</param>
    /// <param name="format">The format.</param>
    /// <param name="timestamp">The timestamp.</param>
    constructor Create(const text: string; const rawBytes: TArray<Byte>;
      const resultPoints: TArray<IResultPoint>; const format: TBarcodeFormat;
      const timeStamp: TDateTime); overload;
    destructor Destroy; override;

    function ToString: String; override;

    // we need a "write" procedure accessor because we need to properly deallocate
    // existing instance if we overwrite it with another one
    property ResultMetaData: TResultMetadata
      read FResultMetadata write SetMetaData;

    /// <summary>
    /// Adds one metadata to the result
    /// </summary>
    /// <param name="type">The type.</param>
    /// <param name="value">The value.</param>
    procedure putMetadata(const ResultMetadataType: TResultMetadataType;
      const Value: IMetaData);

    /// <summary>
    /// Adds a list of metadata to the result
    /// </summary>
    /// <param name="metadata">The metadata.</param>
    procedure putAllMetaData(const metaData: TResultMetadata);

    /// <summary>
    /// Adds the result points.
    /// </summary>
    /// <param name="newPoints">The new points.</param>
    procedure addResultPoints(const newPoints: TArray<IResultPoint>);

    /// <returns>raw text encoded by the barcode, if applicable, otherwise <code>null</code></returns>
    property text: String read FText write SetText;

    /// <returns>raw bytes encoded by the barcode, if applicable, otherwise <code>null</code></returns>
    property rawBytes: TArray<Byte> read FRawBytes write FRawBytes;

    /// <returns>
    /// points related to the barcode in the image. These are typically points
    /// identifying finder patterns or the corners of the barcode. The exact meaning is
    /// specific to the type of barcode that was decoded.
    /// </returns>
    property resultPoints: TArray<IResultPoint> read FResultPoints
      write FResultPoints;

    /// <returns>{@link TBarcodeFormat} representing the format of the barcode that was decoded</returns>
    property BarcodeFormat: TBarcodeFormat read FFormat write FFormat;

    /// <summary>
    /// The 4 corners of the symbol in the image, clockwise from its own top
    /// left: top left, top right, bottom right, bottom left. For a 1D code
    /// the 2 ends of the scanned line (twice). When the reader did not set
    /// the corners, they are estimated from the resultPoints (for a QR Code
    /// from the centers of the finder patterns).
    /// </summary>
    property Position: TArray<IResultPoint> read GetPosition write FPosition;
    /// <summary>The rotation of the symbol in degrees (0 to 359, clockwise),
    /// from the direction of its top edge in Position.</summary>
    property Orientation: Integer read GetOrientation;
    /// <summary>True when the symbol was read as inverted (light on dark),
    /// see ENABLE_INVERSION.</summary>
    property IsInverted: Boolean read FIsInverted write FIsInverted;
    /// <summary>True when the symbol was read mirrored.</summary>
    property IsMirrored: Boolean read FIsMirrored write FIsMirrored;
    /// <summary>The symbology identifier of ISO/IEC 15424, like ']Q1' for a
    /// QR Code, ']d2' for a GS1 Data Matrix or ']C1' for GS1-128. Separate
    /// from text (which can also start with it, see ASSUME_GS1).</summary>
    property SymbologyIdentifier: string read FSymbologyIdentifier
      write FSymbologyIdentifier;

    /// <summary>
    /// Gets the timestamp.
    /// </summary>
    property timeStamp: TDateTime read FTimeStamp;
  end;

implementation

uses
  System.Math;


{$REGION 'IMetaData implementations'}

{ TStringMetadata }
type
  TStringMetadata = class(TInterfacedObject,IMetaData,IStringMetadata)
  strict private
    FValue: String;
    function Value: String;
  private
    constructor Create(const AValue: String);
  end;

function TStringMetadata.Value: String;
begin
   result := FValue
end;

constructor TStringMetadata.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ TIntegerMetadata }
type
  TIntegerMetadata = class(TInterfacedObject,IMetaData,IIntegerMetadata)
  strict private
    FValue: Integer;
    function Value: Integer;
  private
    constructor Create(const AValue: Integer);
  end;

function TIntegerMetadata.Value: Integer;
begin
   result := FValue
end;

constructor TIntegerMetadata.Create(const AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;


{ TByteSegmentsMetadata }
type
  TByteSegmentsMetadata = class(TInterfacedObject,IMetaData,IByteSegmentsMetadata)
  strict private
    FValue: IByteSegments;
    function Value: IByteSegments;
  private
    constructor Create(const AValue: IByteSegments);
  end;

function TByteSegmentsMetadata.Value: IByteSegments;
begin
   result := FValue
end;

constructor TByteSegmentsMetadata.Create(const AValue: IByteSegments);
begin
  inherited Create;
  FValue := AValue;
end;

{ TResultMetadata }
constructor TResultMetaData.Create;
begin
   inherited Create;
end;

class function TResultMetadata.CreateStringMetadata(const Value:string) :IStringMetaData;
begin
   result := TStringMetadata.Create(Value);
end;

class function TResultMetaData.CreateByteSegmentsMetadata(
  const Value: IByteSegments): IByteSegmentsMetadata;
begin
   result := TByteSegmentsMetadata.Create(value);
end;

class function TResultMetadata.CreateIntegerMetadata(const Value:integer) :IIntegerMetaData;
begin
   result := TIntegerMetadata.Create(Value);
end;

{$ENDREGION 'IMetaData implementations'}


{ TReadResult }

function TReadResult.GetPosition: TArray<IResultPoint>;

  function point(x, y: Single): IResultPoint;
  begin
    Result := TResultPointHelpers.CreateResultPoint(x, y);
  end;

begin
  if (FPosition <> nil) then
    exit(FPosition);

  // estimated from the result points, which depend on the format
  var p := FResultPoints;
  var is2D := (FFormat = TBarcodeFormat.QR_CODE) or
    (FFormat = TBarcodeFormat.DATA_MATRIX) or (FFormat = TBarcodeFormat.AZTEC)
    or (FFormat = TBarcodeFormat.PDF_417) or
    (FFormat = TBarcodeFormat.MAXICODE);
  if not is2D and (System.Length(p) >= 2) then
  begin
    // a 1D code: the ends of the scanned line (an add-on adds points behind)
    Result := [p[0], p[High(p)], p[High(p)], p[0]];
    exit;
  end;

  case System.Length(p) of
    0:
      Result := nil;
    1:
      Result := [p[0], p[0], p[0], p[0]];
    2:
      Result := [p[0], p[1], p[1], p[0]];
    3:
      // bottom left, top left and top right, like the finder patterns of a
      // QR Code: complete the parallelogram
      Result := [p[1], p[2], point(p[2].x - p[1].x + p[0].x,
        p[2].y - p[1].y + p[0].y), p[0]];
  else
    if (FFormat = TBarcodeFormat.QR_CODE) then
      // bottom left, top left, top right and the alignment pattern
      Result := [p[1], p[2], point(p[2].x - p[1].x + p[0].x,
        p[2].y - p[1].y + p[0].y), p[0]]
    else
      // top left, bottom left, bottom right and top right, like the corners
      // of a Data Matrix
      Result := [p[0], p[3], p[2], p[1]];
  end;
end;

function TReadResult.GetOrientation: Integer;
begin
  var pos := GetPosition;
  if (System.Length(pos) < 2) then
    exit(0);
  var dx: Double := pos[1].x - pos[0].x;
  var dy: Double := pos[1].y - pos[0].y;
  if (dx = 0) and (dy = 0) then
    exit(0);
  Result := Round(ArcTan2(dy, dx) * 180 / Pi);
  if (Result < 0) then
    Inc(Result, 360);
  Result := Result mod 360;
end;

constructor TReadResult.Create(const text: String; const rawBytes: TArray<Byte>;
  const resultPoints: TArray<IResultPoint>; const format: TBarcodeFormat);
begin
  Self.Create(text, rawBytes, resultPoints, format, Now);
end;

constructor TReadResult.Create(const text: String; const rawBytes: TArray<Byte>;
  const resultPoints: TArray<IResultPoint>; const format: TBarcodeFormat;
  const timeStamp: TDateTime);
begin
  if ((text = '') and (rawBytes = nil)) then
    raise EArgumentException.Create('Text and bytes are null.');

  SetText(text);
  FRawBytes := rawBytes;
  FResultPoints := resultPoints;
  FResultMetadata := nil;
  FFormat := format;
  FResultMetadata := nil;
  FTimeStamp := timeStamp;
end;

destructor TReadResult.Destroy;
var i:Integer;
begin

  for I := Low(FResultPoints) to High(FResultPoints) do
  begin
    FResultPoints[i] := nil;
  end;

  FResultPoints := nil;
  FRawBytes := nil;

  if FResultMetadata <> nil then
  begin
    FResultMetadata.Clear;
    FreeAndNil(FResultMetadata);
  end;

  inherited;
end;

// Added (KG): UTF-8 compatiblity
procedure TReadResult.SetMetaData(const Value: TResultMetadata);
begin
  if value = FResultMetadata then
    exit; // no change
  if FResultMetadata<>nil then
    FResultMetadata.Free; // we need to free existing object instance, for old-gen compilers
  FResultMetadata := Value;
end;

procedure TReadResult.SetText(const AValue: String);
begin
  FText := AValue;
end;

procedure TReadResult.putMetadata(const ResultMetadataType: TResultMetadataType;
  const Value: IMetaData);
begin
  if (FResultMetadata = nil) then
    FResultMetadata := TResultMetadata.Create();
  FResultMetadata.AddOrSetValue(ResultMetadataType, Value);
end;

procedure TReadResult.putAllMetaData(const metaData: TResultMetadata);
var
  key: TResultMetadataType;
begin
  if (metaData <> nil) then
  begin
    if (FResultMetadata = nil) then
      FResultMetadata := TResultMetadata.Create();
    for key in metaData.Keys do
      FResultMetadata.AddOrSetValue(key, metaData[key]);
  end;
end;

procedure TReadResult.addResultPoints(const newPoints: TArray<IResultPoint>);
begin
  FResultPoints := FResultPoints + newPoints;
end;

/// <summary>
/// Returns a <see cref="System.String"/> that represents this instance.
/// </summary>
/// <returns>
/// A <see cref="System.String"/> that represents this instance.
/// </returns>
function TReadResult.ToString(): String;
begin
  if (text = '') then
    Result := '[' + IntToStr(Length(rawBytes)) + ' bytes]'
  else
    Result := text;
end;

end.
