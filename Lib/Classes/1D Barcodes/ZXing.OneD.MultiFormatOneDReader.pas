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

  * Original Authors: dswitkin@google.com (Daniel Switkin) and Sean Owen
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.OneD.MultiFormatOneDReader;

interface

uses
  System.SysUtils, 
  System.Generics.Collections,
  ZXing.OneD.OneDReader,
  DecodeHintType,
  ZXing.Common.BitArray,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.OneD.MultiFormatUPCEANReader,
  ZXing.OneD.Code128Reader,
  ZXing.OneD.Code93Reader, 
  ZXing.OneD.ITFReader;

type
  TMultiFormatOneDReader = class(TOneDReader)
  private
    readers: TList<TOneDReader>;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="TMultiFormatOneDReader"/> class.
    /// </summary>
    /// <param name="hints">The hints.</param>
    constructor Create(const hints: TDictionary<TDecodeHintType, TObject>);
    destructor Destroy(); override;

    /// <summary>
    /// <p>Attempts to decode a one-dimensional barcode format given a single row of
    /// an image.</p>
    /// </summary>
    /// <param name="rowNumber">row number from top of the row</param>
    /// <param name="row">the black/white pixel data of the row</param>
    /// <param name="hints">decode hints</param>
    /// <returns>
    /// <see cref="Result"/>containing encoded string and start/end of barcode or null, if an error occurs or barcode cannot be found
    /// </returns>
    function decodeRow(const rowNumber: Integer; const row: TBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult; override;

    /// <summary>
    /// Resets any internal state the implementation has after a decode, to prepare it
    /// for reuse.
    /// </summary>
    procedure reset; override;
  end;

implementation

// TODO: Not all 1D codes are fully supported yet
constructor TMultiFormatOneDReader.Create(
  const hints: TDictionary<TDecodeHintType, TObject>);
var
  possibleFormats: TList<TBarcodeFormat>;
  useMsiCheckDigit, useCode39CheckDigit, useCode39ExtendedMode: Boolean;
begin
  if ((hints = nil) or (not hints.ContainsKey(DecodeHintType.POSSIBLE_FORMATS)))
  then
     possibleFormats := nil
  else
     possibleFormats := TList<TBarcodeFormat>(hints[DecodeHintType.POSSIBLE_FORMATS]);

  readers := TList<TOneDReader>.Create();
  if (possibleFormats <> nil) then
  begin
    if ((possibleFormats.Contains(TBarcodeFormat.All_1D)) or
        (possibleFormats.Contains(TBarcodeFormat.EAN_13)) or
        (possibleFormats.Contains(TBarcodeFormat.UPC_A)) or
        (possibleFormats.Contains(TBarcodeFormat.EAN_8)) or
        (possibleFormats.Contains(TBarcodeFormat.UPC_E)))
    then
       readers.Add(TMultiFormatUPCEANReader.Create(hints));

    if possibleFormats.Contains(TBarcodeFormat.MSI) then
    begin
      // MSI needs to be activated explicit
      useMsiCheckDigit := false;
      if (hints.ContainsKey(DecodeHintType.ASSUME_MSI_CHECK_DIGIT)) then
      begin
        useMsiCheckDigit := Boolean(hints[DecodeHintType.ASSUME_MSI_CHECK_DIGIT]);
        // TODO: Not yet implemented!
        // readers.Add(new MSIReader(useMsiCheckDigit))
      end;
    end;
    if (possibleFormats.Contains(TBarcodeFormat.CODE_39)) or
      (possibleFormats.Contains(TBarcodeFormat.All_1D)) then
    begin
      useCode39CheckDigit :=
        (hints.ContainsKey(DecodeHintType.ASSUME_CODE_39_CHECK_DIGIT)) and
         Boolean((hints[DecodeHintType.ASSUME_CODE_39_CHECK_DIGIT]));

      useCode39ExtendedMode :=
        (hints.ContainsKey(DecodeHintType.USE_CODE_39_EXTENDED_MODE)) and
        Boolean(hints[DecodeHintType.USE_CODE_39_EXTENDED_MODE]);

      // TODO: Not yet implemented!
      // readers.Add(new Code39Reader(useCode39CheckDigit, useCode39ExtendedMode))
    end;

    if (possibleFormats.Contains(TBarcodeFormat.CODE_93)) or
       (possibleFormats.Contains(TBarcodeFormat.All_1D))
    then
       readers.Add(TCode93Reader.Create);

    if (possibleFormats.Contains(TBarcodeFormat.CODE_128)) or
       (possibleFormats.Contains(TBarcodeFormat.All_1D))
    then
       readers.Add(TCode128Reader.Create);

    if (possibleFormats.Contains(TBarcodeFormat.ITF)) or
      (possibleFormats.Contains(TBarcodeFormat.All_1D))
    then
       readers.Add(TITFReader.Create);

    // TODO: Not yet implemented!
    {if (possibleFormats.Contains(TBarcodeFormat.CODABAR)) or
       (possibleFormats.Contains(TBarcodeFormat.All_1D))
    then
       readers.Add(TCodaBarReader.Create);}

    // TODO: Not yet implemented!
    {if (possibleFormats.Contains(TBarcodeFormat.RSS_14)) or
       (possibleFormats.Contains(TBarcodeFormat.All_1D))
    then
       readers.Add(TRSS14Reader.Create);}

    // TODO: Not yet implemented!
    {if (possibleFormats.Contains(TBarcodeFormat.RSS_EXPANDED)) or
       (possibleFormats.Contains(TBarcodeFormat.All_1D))
    then
       readers.Add(TRSSExpandedReader.Create);}
  end;

  if (readers.Count = 0) then
  begin
    useCode39CheckDigit :=
      ((hints <> nil) and
      (hints.ContainsKey(DecodeHintType.ASSUME_CODE_39_CHECK_DIGIT))) and
      Boolean(hints[DecodeHintType.ASSUME_CODE_39_CHECK_DIGIT]);

    useCode39ExtendedMode :=
      ((hints <> nil) and
      (hints.ContainsKey(DecodeHintType.USE_CODE_39_EXTENDED_MODE))) and
      Boolean(hints[DecodeHintType.USE_CODE_39_EXTENDED_MODE]);
    // MSI needs to be activated explicit

    readers.Add(TMultiFormatUPCEANReader.Create(hints));
    // TODO: Not yet implemented!
    // readers.Add(TCode39Reader.Create(useCode39CheckDigit, useCode39ExtendedMode));
    // readers.Add(TCodaBarReader.Create);
    readers.Add(TCode93Reader.Create());
    readers.Add(TCode128Reader.Create);
    readers.Add(TITFReader.Create);
    // TODO: Not yet implemented!
    // readers.Add(TRSS14Reader.Create);
    // readers.Add(TRSSExpandedReader.Create)
  end

end;

destructor TMultiFormatOneDReader.Destroy;
var
  reader: TOneDReader;
begin
  for reader in readers do
    reader.Free;

  readers.clear;
  FreeAndNil(readers);
  inherited;
end;

function TMultiFormatOneDReader.DecodeRow(const rowNumber: Integer;
  const row: TBitArray;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  reader: TOneDReader;
begin
  Result := nil;

  for reader in readers do
  begin
    result := reader.DecodeRow(rowNumber, row, hints);
    if (result <> nil)
    then
       break;
  end;
end;

procedure TMultiFormatOneDReader.Reset();
var
  reader: TOneDReader;
begin
  for reader in readers do
    reader.reset();
end;

end.
