unit MultiFormatReader;

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
}
interface

uses SysUtils, rtti, Generics.Collections,
  ReadResult, Reader, DecodeHintType, BinaryBitmap, BarcodeFormat,
  MultiFormatOneDReader, ResultPoint, OneDReader, QRCodeReader;

/// <summary>
/// MultiFormatReader is a convenience class and the main entry point into the library for most uses.
/// By default it attempts to decode all barcode formats that the library supports. Optionally, you
/// can provide a hints object to request different behavior, for example only decoding QR codes.
/// </summary>
/// <author>Sean Owen</author>
/// <author>dswitkin@google.com (Daniel Switkin)</author>
/// <author>www.Redivivus.in (suraj.supekar@redivivus.in) - Ported from ZXING Java Source</author>
type
   TMultiFormatReader = class(TInterfacedObject, IReader)
  private

    FHints: TDictionary<TDecodeHintType, TObject>;
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
  public
    function Decode(image: TBinaryBitmap): TReadResult; overload;

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
    function Decode(image: TBinaryBitmap;
      pHints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;

    /// <summary> Decode an image using the state set up by calling setHints() previously. Continuous scan
    /// clients will get a <b>large</b> speed increase by using this instead of decode().
    ///
    /// </summary>
    /// <param name="image">The pixel data to decode
    /// </param>
    /// <returns> The contents of the image
    /// </returns>
    /// <throws>  ReaderException Any errors which occurred </throws>
    function DecodeWithState(image: TBinaryBitmap): TReadResult;
    destructor Destroy; override;

    /// <summary> This method adds state to the MultiFormatReader. By setting the hints once, subsequent calls
    /// to decodeWithState(image) can reuse the same set of readers without reallocating memory. This
    /// is important for performance in continuous scan clients.
    ///
    /// </summary>
    /// <param name="hints">The set of hints to use for subsequent calls to decode(image)
    /// </param>
    property hints: TDictionary<TDecodeHintType, TObject> read Get_Hints
      write Set_Hints;

    procedure Reset;

    procedure FreeReaders();

  private
    function DecodeInternal(image: TBinaryBitmap): TReadResult;
  end;

implementation

function TMultiFormatReader.Decode(image: TBinaryBitmap): TReadResult;
begin
  hints := nil;
  result := DecodeInternal(image)
end;

function TMultiFormatReader.Decode(image: TBinaryBitmap;
  pHints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  hints := pHints;
  result := DecodeInternal(image)
end;

function TMultiFormatReader.DecodeWithState(image: TBinaryBitmap): TReadResult;
begin
  // Make sure to set up the default state so we don't crash
  if readers = nil then
  begin
    hints := nil
  end;

  result := DecodeInternal(image);

end;

destructor TMultiFormatReader.Destroy;
begin
  FreeReaders;
  inherited;
end;

procedure TMultiFormatReader.FreeReaders;
var
  Reader: IReader;
  OneDReader: TOneDReader;
begin
  if readers <> nil then
  begin
    for Reader in readers do
    begin

      if (Reader is TOneDReader) then
      begin
        OneDReader := TObject(Reader) as TOneDReader;
        OneDReader := nil;
      end;
    end;
  end;

  readers.Clear();
  readers.Free;
  readers := nil;

end;

function TMultiFormatReader.Get_Hints: TDictionary<TDecodeHintType, TObject>;
begin
  result := FHints;
end;

procedure TMultiFormatReader.Set_Hints(const Value: TDictionary<TDecodeHintType,
  TObject>);
var
  tryHarder, addOneDReader: Boolean;
  formats: TList<TBarcodeFormat>;
begin
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

  if formats <> nil then
  begin

    addOneDReader :=
      (((((((((((formats.Contains(BarcodeFormat.All_1D)) or
      (formats.Contains(BarcodeFormat.UPC_A))) or
      (formats.Contains(BarcodeFormat.UPC_E))) or
      (formats.Contains(BarcodeFormat.EAN_13))) or
      (formats.Contains(BarcodeFormat.EAN_8))) or
      (formats.Contains(BarcodeFormat.CODABAR))) or
      (formats.Contains(BarcodeFormat.CODE_39))) or
      (formats.Contains(BarcodeFormat.CODE_93))) or
      (formats.Contains(BarcodeFormat.CODE_128))) or
      (formats.Contains(BarcodeFormat.ITF))) or
      (formats.Contains(BarcodeFormat.RSS_14))) or
      (formats.Contains(BarcodeFormat.RSS_EXPANDED));

    readers := TList<IReader>.Create;

    // NOT YET SUPPORTED!

    // Put 1D readers upfront in "normal" mode
    if (addOneDReader) and (not tryHarder) then
    begin
      readers.Add(TMultiFormatOneDReader.Create(Value))
    end;
    if formats.Contains(BarcodeFormat.QR_CODE) then
    begin
      readers.Add(TQRCodeReader.Create())
    end;
    if formats.Contains(BarcodeFormat.DATA_MATRIX) then
    begin
      // readers.Add(new DataMatrixReader())
    end;
    if formats.Contains(BarcodeFormat.AZTEC) then
    begin
      // readers.Add(new AztecReader())
    end;
    if formats.Contains(BarcodeFormat.PDF_417) then
    begin
      // readers.Add(new PDF417Reader())
    end;
    if formats.Contains(BarcodeFormat.MAXICODE) then
    begin
      // readers.Add(new MaxiCodeReader())
    end;

    // At end in "try harder" mode
    if (addOneDReader) and (tryHarder) then
    begin
      // readers.Add(new MultiFormatOneDReader(Value))
    end

  end;

  if (readers = nil) or (readers.Count = 0) then
  begin

    if (readers = nil) then
    begin
      readers := TList<IReader>.Create;
    end;

    readers.Add(TMultiFormatOneDReader.Create(Value));
    readers.Add(TQRCodeReader.Create());
    // readers.Add(new DataMatrixReader());
    // readers.Add(new AztecReader());
    // readers.Add(new PDF417Reader());
    // readers.Add(new MaxiCodeReader());


  end

end;

procedure TMultiFormatReader.Reset;
var
  Reader: IReader;
begin
  if readers <> nil then
  begin
    for Reader in readers do
    begin
      Reader.Reset();
    end
  end
end;

function TMultiFormatReader.DecodeInternal(image: TBinaryBitmap): TReadResult;
var
  rpCallBack: TResultPointCallback;
  i: integer;
  Reader: IReader;
begin

  result := nil;
  if (readers = nil) then
  begin
    Exit;
  end;

  rpCallBack := nil;
  if ((FHints <> nil) and
    (FHints.ContainsKey(DecodeHintType.NEED_RESULT_POINT_CALLBACK))) then
  begin
    // rpCallBack := FHints[DecodeHintType.NEED_RESULT_POINT_CALLBACK]
    // as TResultPointCallback(nil);
  end;

  for i := 0 to readers.Count - 1 do
  begin

    Reader := readers[i];
    Reader.Reset();
    result := Reader.Decode(image, FHints);
    if result <> nil then
    begin

      // found a barcode, pushing the successful reader up front
      // I assume that the same type of barcode is read multiple times
      // so the reordering of the readers list should speed up the next reading
      // a little bit

      readers.Delete(i);
      readers.Insert(0, Reader);
      Exit;
    end;

    // if rpCallBack <> nil then
    // rpCallBack(nil)

  end;

end;

end.
