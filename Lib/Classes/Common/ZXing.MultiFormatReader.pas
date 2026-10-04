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

unit ZXing.MultiFormatReader;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  System.RegularExpressions,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.BinaryBitmap,
  ZXing.BarcodeFormat,
  ZXing.ResultPoint,

  // 1D Barcodes
  ZXing.OneD.OneDReader,
  ZXing.OneD.Code128Reader,
  ZXing.OneD.Code93Reader,
  ZXing.OneD.ITFReader,
  ZXing.OneD.EAN13Reader,
  ZXing.OneD.EAN8Reader,
  ZXing.OneD.UPCAReader,
  ZXing.OneD.UPCEReader,
  ZXing.OneD.Code39Reader,
  ZXing.OneD.CodabarReader,
  ZXing.OneD.TelepenReader,
  ZXing.OneD.DataBarReader,
  ZXing.OneD.DataBarExpandedReader,
  ZXing.OneD.DataBarLimitedReader,
  ZXing.OneD.DXFilmEdgeReader,
  ZXing.OneD.MSIReader,
  ZXing.OneD.PlesseyReader,
  ZXing.OneD.Code11Reader,
  ZXing.OneD.Code2of5Reader,
  ZXing.OneD.PharmacodeTwoTrackReader,
  ZXing.OneD.PharmacodeReader,
  ZXing.Postal.PostalReader,

  // 2D Codes
  ZXing.QrCode.QRCodeReader,
  ZXing.Datamatrix.DataMatrixReader,
  ZXing.Aztec.AztecReader,
  ZXing.PDF417.PDF417Reader,
  ZXing.PDF417.MicroPDF417Reader,
  ZXing.QrCode.MicroQRCodeReader,
  ZXing.MaxiCode.MaxiCodeReader;

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

    function DecodeInternal(image: TBinaryBitmap): TReadResult;
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
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap; WithHints: Boolean)
      : TReadResult; overload;
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
    function DecodeWithState(image: TBinaryBitmap): TReadResult;
    /// <summary>
    /// Adds the barcodes of all configured formats found in image to results
    /// (the caller owns them), at most maxCount in total (0: no limit), with
    /// the current hints. Readers that can find only one barcode add at most
    /// one.
    /// </summary>
    procedure decodeMultiple(const image: TBinaryBitmap;
      results: TList<TReadResult>; maxCount: Integer);
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

  end;

implementation

function TMultiFormatReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  hints := nil;
  result := DecodeInternal(image)
end;

function TMultiFormatReader.decode(const image: TBinaryBitmap;
  WithHints: Boolean): TReadResult;
begin
  result := DecodeInternal(image)
end;

function TMultiFormatReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  FHints := hints;
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
begin
  if readers <> nil then
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
  useCode39CheckDigit, useCode39ExtendedMode: Boolean;
  formats: TList<TBarcodeFormat>;
begin
  // the readers of earlier hints
  FreeReaders;
  FHints := Value;

  // also without hints (Value = nil)
  useCode39CheckDigit := (Value <> nil) and
    Value.ContainsKey(TDecodeHintType.ASSUME_CODE_39_CHECK_DIGIT);
  useCode39ExtendedMode := (Value <> nil) and
    Value.ContainsKey(TDecodeHintType.USE_CODE_39_EXTENDED_MODE);

  // tryHarder := (Value <> nil) and
  // (Value.ContainsKey(ZXing.DecodeHintType.TRY_HARDER));

  if ((Value = nil) or
    (not Value.ContainsKey(ZXing.DecodeHintType.POSSIBLE_FORMATS))) then
  begin
    formats := nil;
  end
  else
  begin
    formats := Value[ZXing.DecodeHintType.POSSIBLE_FORMATS]
      as TList<TBarcodeFormat>
  end;

  // add readers from the hints
  readers := TList<IReader>.Create;
  if formats <> nil then
  begin

    // 1D readers

    if (formats.Contains(TBarcodeFormat.CODE_128)) then
    begin
      readers.Add(TCode128Reader.Create())
    end;

    if (formats.Contains(TBarcodeFormat.CODE_93)) then
    begin
      readers.Add(TCode93Reader.Create())
    end;

    if (formats.Contains(TBarcodeFormat.ITF)) then
    begin
      readers.Add(TITFReader.Create())
    end;

    // 2D readers
    if (formats.Contains(TBarcodeFormat.QR_CODE)) then
    begin
      readers.Add(TQRCodeReader.Create())
    end;

    if formats.Contains(TBarcodeFormat.DATA_MATRIX) then
      readers.Add(TDataMatrixReader.Create);

    if formats.Contains(TBarcodeFormat.AZTEC) then
      readers.Add(TAztecReader.Create);

    if formats.Contains(TBarcodeFormat.PDF_417) then
      readers.Add(TPDF417Reader.Create);

    if formats.Contains(TBarcodeFormat.MICRO_PDF417) then
      readers.Add(TMicroPDF417Reader.Create);

    if formats.Contains(TBarcodeFormat.MICRO_QR_CODE) or
      formats.Contains(TBarcodeFormat.RMQR_CODE) then
      readers.Add(TMicroQRCodeReader.Create
        (formats.Contains(TBarcodeFormat.MICRO_QR_CODE),
        formats.Contains(TBarcodeFormat.RMQR_CODE)));

    if formats.Contains(TBarcodeFormat.MAXICODE) then
      readers.Add(TMaxiCodeReader.Create);

    if (formats.Contains(TBarcodeFormat.EAN_13)) then
      readers.Add(TEAN13Reader.Create());

    if (formats.Contains(TBarcodeFormat.EAN_8)) then
      readers.Add(TEAN8Reader.Create());

    // the UPC-A reader uses an EAN-13 reader: after the EAN-13 reader it
    // can not find anything more
    if (formats.Contains(TBarcodeFormat.UPC_A)) and
      not formats.Contains(TBarcodeFormat.EAN_13) then
      readers.Add(TUPCAReader.Create());

    if (formats.Contains(TBarcodeFormat.UPC_E)) then
      readers.Add(TUPCEReader.Create());

    // Code 32 and PZN are Code 39 codes
    if formats.Contains(TBarcodeFormat.CODE_39) or
      formats.Contains(TBarcodeFormat.CODE_32) or
      formats.Contains(TBarcodeFormat.PZN) then
    begin
      var code39 := TCode39Reader.Create(useCode39CheckDigit,
        useCode39ExtendedMode);
      code39.Code32 := formats.Contains(TBarcodeFormat.CODE_32);
      code39.PZN := formats.Contains(TBarcodeFormat.PZN);
      code39.Code39 := formats.Contains(TBarcodeFormat.CODE_39);
      readers.Add(code39);
    end;

    if formats.Contains(TBarcodeFormat.CODABAR) then
      readers.Add(TCodabarReader.Create);

    if formats.Contains(TBarcodeFormat.TELEPEN) then
      readers.Add(TTelepenReader.Create);

    if formats.Contains(TBarcodeFormat.RSS_14) then
      readers.Add(TDataBarReader.Create);

    if formats.Contains(TBarcodeFormat.RSS_EXPANDED) then
      readers.Add(TDataBarExpandedReader.Create);

    if formats.Contains(TBarcodeFormat.RSS_LIMITED) then
      readers.Add(TDataBarLimitedReader.Create);

    if formats.Contains(TBarcodeFormat.DX_FILM_EDGE) then
      readers.Add(TDXFilmEdgeReader.Create);

    // only when asked for: without check (MSI, Pharmacode) they give false
    // positives
    if formats.Contains(TBarcodeFormat.MSI) then
      readers.Add(TMSIReader.Create);

    if formats.Contains(TBarcodeFormat.PLESSEY) then
      readers.Add(TPlesseyReader.Create);

    if formats.Contains(TBarcodeFormat.PHARMA_CODE) then
      readers.Add(TPharmacodeReader.Create);

    if formats.Contains(TBarcodeFormat.PHARMA_CODE_TWO_TRACK) then
      readers.Add(TPharmacodeTwoTrackReader.Create);

    if formats.Contains(TBarcodeFormat.CODE_11) then
      readers.Add(TCode11Reader.Create);

    for var f in [TBarcodeFormat.INDUSTRIAL_2_OF_5, TBarcodeFormat.IATA_2_OF_5,
      TBarcodeFormat.MATRIX_2_OF_5, TBarcodeFormat.DATALOGIC_2_OF_5] do
      if formats.Contains(f) then
        readers.Add(TCode2of5Reader.Create(f));

    // the postal barcodes, also only when asked for
    var postal: TArray<TBarcodeFormat> := [];
    for var f in formats do
      if IsPostalFormat(f) then
        postal := postal + [f];
    if (Length(postal) > 0) then
      readers.Add(TPostalReader.Create(postal));
  end;

  if (readers.Count = 0) then // must be auto, add them all
  begin

    // 1D readers
    readers.Add(TCode128Reader.Create());
    // the UPC-A reader also returns the EAN-13 codes (its EAN-13 reader
    // finds them anyway), so no separate EAN-13 reader that would do the
    // same work again
    var upca := TUPCAReader.Create();
    upca.AlsoEAN13 := true;
    readers.Add(upca);
    readers.Add(TUPCEReader.Create());
    readers.Add(TEAN8Reader.Create());
    readers.Add(TCode93Reader.Create());
    readers.Add(TITFReader.Create());
    readers.Add(TCode39Reader.Create(useCode39CheckDigit,
      useCode39ExtendedMode));
    readers.Add(TCodabarReader.Create);
    readers.Add(TTelepenReader.Create);
    readers.Add(TDataBarReader.Create);
    readers.Add(TDataBarExpandedReader.Create);
    readers.Add(TDataBarLimitedReader.Create);
    readers.Add(TDXFilmEdgeReader.Create);

    // 2D readers
    readers.Add(TQRCodeReader.Create());
    readers.Add(TDataMatrixReader.Create);
    readers.Add(TAztecReader.Create);
    readers.Add(TPDF417Reader.Create);
    readers.Add(TMicroPDF417Reader.Create);
    readers.Add(TMicroQRCodeReader.Create);
    readers.Add(TMaxiCodeReader.Create);
  end;

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

procedure TMultiFormatReader.decodeMultiple(const image: TBinaryBitmap;
  results: TList<TReadResult>; maxCount: Integer);
begin
  if (readers = nil) then
    exit;
  for var reader in readers do
  begin
    if ResultsFull(results, maxCount) then
      exit;
    reader.Reset();
    var multiple: IMultipleReader;
    if Supports(reader, IMultipleReader, multiple) then
      multiple.decodeMultiple(image, FHints, results, maxCount)
    else
    begin
      var r := reader.decode(image, FHints);
      if (r <> nil) then
        if ContainsResult(results, r) then
          r.Free
        else
          results.Add(r);
    end;
  end;
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
    (FHints.ContainsKey(ZXing.DecodeHintType.NEED_RESULT_POINT_CALLBACK))) then
  begin
    // rpCallBack := FHints[DecodeHintType.NEED_RESULT_POINT_CALLBACK]
    // as TResultPointCallback(nil);
  end;

  for i := 0 to readers.Count - 1 do
  begin

    Reader := readers[i];
    Reader.Reset();
    result := Reader.decode(image, FHints);
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
