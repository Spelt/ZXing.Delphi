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
  ZXing.BarcodeFormat;

type
  TStringObject = class(TObject)
  private
    FValue: String;
  public
    constructor Create(const AValue: String);
    property Value: String read FValue;
  end;

  TReadResult = class
  private
    FText: string;
    FTimeStamp: TDateTime;
    FResultMetadata: TDictionary<TResultMetadataType, TObject>;
    FRawBytes: TArray<Byte>;
    FResultPoints: TArray<IResultPoint>;
    FFormat: TBarcodeFormat;

    procedure SetText(const AValue: String);
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

    property ResultMetaData: TDictionary<TResultMetadataType, TObject>
      read FResultMetadata write FResultMetadata;

    /// <summary>
    /// Adds one metadata to the result
    /// </summary>
    /// <param name="type">The type.</param>
    /// <param name="value">The value.</param>
    procedure putMetadata(const ResultMetadataType: TResultMetadataType;
      const Value: TObject);

    /// <summary>
    /// Adds a list of metadata to the result
    /// </summary>
    /// <param name="metadata">The metadata.</param>
    procedure putAllMetaData(const metaData: TDictionary<TResultMetadataType,
      TObject>);

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
    /// Gets the timestamp.
    /// </summary>
    property timeStamp: TDateTime read FTimeStamp;
  end;

implementation

{ TStringObject }

constructor TStringObject.Create(const AValue: String);
begin
  FValue := AValue;
end;

{ TReadResult }

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
begin
  resultPoints := nil;
  FRawBytes := nil;

  if FResultMetadata <> nil then
  begin
    FResultMetadata.Clear;
    FreeAndNil(FResultMetadata);
  end;

  inherited;
end;

// Added (KG): UTF-8 compatiblity
procedure TReadResult.SetText(const AValue: String);
begin
  FText := UTF8ToString(AValue);
  if (Length(FText) = 0) then
    FText := AValue;
end;

procedure TReadResult.putMetadata(const ResultMetadataType: TResultMetadataType;
  const Value: TObject);
begin
  if (FResultMetadata = nil) then
    FResultMetadata := TDictionary<TResultMetadataType, TObject>.Create();
  FResultMetadata.AddOrSetValue(ResultMetadataType, Value);
end;

procedure TReadResult.putAllMetaData(const metaData
  : TDictionary<TResultMetadataType, TObject>);
var
  key: TResultMetadataType;
begin
  if (metaData <> nil) then
  begin
    if (FResultMetadata = nil) then
      FResultMetadata := metaData
    else
    begin
      for key in metaData.Keys do
        FResultMetadata.AddOrSetValue(key, metaData[key]);
    end;
  end;
end;

procedure TReadResult.addResultPoints(const newPoints: TArray<IResultPoint>);
var
  oldPoints, allPoints: TArray<IResultPoint>;
begin
  oldPoints := FResultPoints;
  if (oldPoints = nil) then
    FResultPoints := newPoints
  else
  begin
    if (newPoints <> nil) and (Length(newPoints) > 0) then
    begin
      allPoints := TArray<IResultPoint>.Create();
      SetLength(allPoints, (Length(oldPoints) + Length(newPoints)));

      Move(oldPoints[0], newPoints[0], Length(oldPoints));
      Move(newPoints[0], newPoints[Length(oldPoints)], Length(newPoints));

      FResultPoints := allPoints;
    end;
  end;
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
