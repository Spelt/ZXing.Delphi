{
  * Copyright 2007 ZXing authors
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

  * Original Authors: Sean Owen and dswitkin@google.com (Daniel Switkin)
  * Ported from ZXING Java Source: www.Redivivus.in (suraj.supekar@redivivus.in)
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit MultiFormatReader;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  RegularExpressions,
  ZXing.ReadResult,
  ZXing.Reader,
  DecodeHintType,
  BinaryBitmap,
  ZXing.BarcodeFormat,
  ZXing.ResultPoint,
  // 1D Barcodes
  ZXing.OneD.OneDReader,
  ZXing.OneD.MultiFormatOneDReader,
  // 2D Codes
  ZXing.QrCode.QRCodeReader,
  ZXing.Datamatrix.DataMatrixReader;

/// <summary>
/// MultiFormatReader is a convenience class and the main entry point into the library for most uses.
/// By default it attempts to decode all barcode formats that the library supports. Optionally, you
/// can provide a hints object to request different behavior, for example only decoding QR codes.
/// </summary>
type
  TMultiFormatReader = class(TInterfacedObject, IReader)
  private
    FHints: TDictionary<TDecodeHintType, TObject>;
    FEvent : TResultPointCallback;
    readers: TList<IReader>;

    procedure Set_Hints(const Value: TDictionary<TDecodeHintType, TObject>);
    function Get_Hints: TDictionary<TDecodeHintType, TObject>;

    /// <summary> This version of decode honors the intent of Reader.decode(BinaryBitmap) in that it
    /// passes null as a hint to the decoders. However, that makes it inefficient to call repeatedly.
    /// Use setHints() followed by decodeWithState() for continuous scan applications.
    ///
    /// </summary>
    /// <param name="image">The pixel data to decode
    /// </param>
    /// <returns> The contents of the image
    /// </returns>
    /// <throws>  ReaderException Any errors which occurred </throws>

    function DecodeInternal(const image: TBinaryBitmap): TReadResult;
    procedure FreeReaders();
  public
    destructor Destroy; override;

    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap; const WithHints: Boolean): TReadResult; overload;
    /// <summary> Decode an image using the hints provided. Does not honor existing state.
    ///
    /// </summary>
    /// <param name="image">The pixel data to decode
    /// </param>
    /// <param name="hints">The hints to use, clearing the previous state.
    /// </param>
    /// <returns> The contents of the image
    /// </returns>
    /// <throws>  ReaderException Any errors which occurred </throws>
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;

    /// <summary> Decode an image using the state set up by calling setHints() previously. Continuous scan
    /// clients will get a <b>large</b> speed increase by using this instead of decode().
    ///
    /// </summary>
    /// <param name="image">The pixel data to decode
    /// </param>
    /// <returns> The contents of the image
    /// </returns>
    /// <throws>  ReaderException Any errors which occurred </throws>
    function DecodeWithState(const image: TBinaryBitmap): TReadResult;

    /// <summary> This method adds state to the MultiFormatReader. By setting the hints once, subsequent calls
    /// to decodeWithState(image) can reuse the same set of readers without reallocating memory. This
    /// is important for performance in continuous scan clients.
    ///
    /// </summary>
    /// <param name="hints">The set of hints to use for subsequent calls to decode(image)
    /// </param>
    property hints: TDictionary<TDecodeHintType, TObject> read Get_Hints
      write Set_Hints;

    procedure reset;
  end;

implementation

destructor TMultiFormatReader.Destroy;
begin
  FreeReaders;
  inherited;
end;

function TMultiFormatReader.Decode(const image: TBinaryBitmap): TReadResult;
begin
  hints := nil;
  Result := DecodeInternal(image);
end;

function TMultiFormatReader.Decode(const image: TBinaryBitmap;
  const WithHints: Boolean): TReadResult;
begin
  Result := DecodeInternal(image);
end;

function TMultiFormatReader.Decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Self.hints := hints;
  result := DecodeInternal(image)
end;

function TMultiFormatReader.DecodeWithState(
  const image: TBinaryBitmap): TReadResult;
begin
  // Make sure to set up the default state so we don't crash
  if (readers = nil)
  then
     hints := nil;

  Result := DecodeInternal(image);
end;

procedure TMultiFormatReader.FreeReaders;
var
  i: Integer;
begin
  if readers <> nil then
  begin
    for i := Pred(readers.count) downto 0 do
    begin
      readers[i] := nil;
      readers.Delete(i);
    end;
  end;
  FreeAndNil(readers);
end;

function TMultiFormatReader.Get_Hints: TDictionary<TDecodeHintType, TObject>;
begin
  Result := FHints;
end;

procedure TMultiFormatReader.Set_Hints(const Value: TDictionary<TDecodeHintType,
  TObject>);
var
  tryHarder, addOneDReader: Boolean;
  formats: TList<TBarcodeFormat>;
begin
  Readers := TList<IReader>.Create;
  FHints := Value;

  tryHarder := (Value <> nil) and
    (Value.ContainsKey(DecodeHintType.TRY_HARDER));

  if ((Value = nil) or (not Value.ContainsKey(DecodeHintType.POSSIBLE_FORMATS)))
  then
  begin
    formats := nil;
  end
  else
  begin
    formats := Value[DecodeHintType.POSSIBLE_FORMATS] as TList<TBarcodeFormat>
  end;

  if (formats <> nil) then
  begin
    addOneDReader :=
      (formats.Contains(TBarcodeFormat.All_1D)) or
      (formats.Contains(TBarcodeFormat.UPC_A)) or
      (formats.Contains(TBarcodeFormat.UPC_E)) or
      (formats.Contains(TBarcodeFormat.EAN_13)) or
      (formats.Contains(TBarcodeFormat.EAN_8)) or
      (formats.Contains(TBarcodeFormat.CODABAR)) or
      (formats.Contains(TBarcodeFormat.CODE_39)) or
      (formats.Contains(TBarcodeFormat.CODE_93)) or
      (formats.Contains(TBarcodeFormat.CODE_128)) or
      (formats.Contains(TBarcodeFormat.ITF)) or
      (formats.Contains(TBarcodeFormat.RSS_14)) or
      (formats.Contains(TBarcodeFormat.RSS_EXPANDED));

    // TODO: Not all 1D/2D fully supported yet

    // Put 1D readers upfront in "normal" mode
    if (addOneDReader) and (not tryHarder)
    then
       readers.Add(TMultiFormatOneDReader.Create(Value));

    if formats.Contains(TBarcodeFormat.QR_CODE)
    then
       readers.Add(TQRCodeReader.Create);

    if formats.Contains(TBarcodeFormat.DATA_MATRIX)
    then
       readers.Add(TDataMatrixReader.Create)

    {if formats.Contains(TBarcodeFormat.AZTEC)
    then
       readers.Add(TAztecReader.Create};

    {formats.Contains(TBarcodeFormat.PDF_417)
    then
       readers.Add(TPDF417Reader.Create};

    {if formats.Contains(TBarcodeFormat.MAXICODE)
    then
       readers.Add(TMaxiCodeReader.Create}

    // At end in "try harder" mode
    if (addOneDReader) and (tryHarder)
    then
       readers.Add(TMultiFormatOneDReader.Create(Value));
  end;

  if (readers = nil) or
     (readers.Count = 0) then
  begin
    readers.Add(TMultiFormatOneDReader.Create(Value));
    readers.Add(TQRCodeReader.Create);
    readers.Add(TDataMatrixReader.Create);
    // TODO: Not implemented yet
    //readers.Add(TAztecReader.Create);
    //readers.Add(TPDF417Reader.Create);
    //readers.Add(TMaxiCodeReader.Create);
  end
end;

procedure TMultiFormatReader.reset;
var
  Reader: IReader;
begin
  if (readers <> nil) then
  begin
    for Reader in readers do
      Reader.reset();
  end
end;

function TMultiFormatReader.DecodeInternal(const image: TBinaryBitmap): TReadResult;
var
  i: integer;
  Reader: IReader;
begin
  Result := nil;

  if (readers = nil)
  then
     exit;

  for i := 0 to Pred(readers.Count) do
  begin
    Reader := readers[i];
    Reader.reset();

    Result := Reader.decode(image, FHints);
    if (Result <> nil) then
    begin
      // found a barcode, pushing the successful reader up front
      // I assume that the same type of barcode is read multiple times
      // so the reordering of the readers list should speed up the next reading
      // a little bit
      readers.Delete(i);
      readers.Insert(0, Reader);
      exit;
    end;
  end;
end;

end.
