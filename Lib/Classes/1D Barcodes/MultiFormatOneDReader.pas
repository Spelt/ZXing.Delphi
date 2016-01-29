unit MultiFormatOneDReader;

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
}
interface

uses

  SysUtils, Generics.Collections,
  OneDReader, DecodeHintType, BitArray, ReadResult, BarcodeFormat,
  Code128Reader, Code93Reader, ITFReader;

/// <summary>
/// <author>dswitkin@google.com (Daniel Switkin)</author>
/// <author>Sean Owen</author>
/// </summary>
type
  TMultiFormatOneDReader = class(TOneDReader)
  private
  var
    readers: TList<TOneDReader>;

    /// <summary>
    /// Initializes a new instance of the <see cref="MultiFormatOneDReader"/> class.
    /// </summary>
    /// <param name="hints">The hints.</param>
  public
    constructor Create(hints: TDictionary<TDecodeHintType, TObject>);

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
    function DecodeRow(rowNumber: Integer; row: TBitArray;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; override;

    /// <summary>
    /// Resets any internal state the implementation has after a decode, to prepare it
    /// for reuse.
    /// </summary>
    Procedure Reset; override;
  end;

implementation

constructor TMultiFormatOneDReader.Create
  (hints: TDictionary<TDecodeHintType, TObject>);

// NOT YET FULLY SUPPORTED

var
  possibleFormats: TList<TBarcodeFormat>;
  useMsiCheckDigit, useCode39CheckDigit, useCode39ExtendedMode: Boolean;

begin

  if ((hints = nil) or (not hints.ContainsKey(DecodeHintType.POSSIBLE_FORMATS)))
  then
  begin
    possibleFormats := nil;
  end
  else
  begin
    possibleFormats := hints[DecodeHintType.POSSIBLE_FORMATS]
      as TList<TBarcodeFormat>;
  end;

  readers := TList<TOneDReader>.Create();

  if possibleFormats <> nil then
  begin
    if ((((possibleFormats.Contains(BarcodeFormat.All_1D)) or
      (possibleFormats.Contains(BarcodeFormat.EAN_13))) or
      (possibleFormats.Contains(BarcodeFormat.UPC_A))) or
      (possibleFormats.Contains(BarcodeFormat.EAN_8))) or
      (possibleFormats.Contains(BarcodeFormat.UPC_E)) then
    begin
      // NOT YET SUPPORTED
      // readers.Add(new MultiFormatUPCEANReader(hints))
    end;
    if possibleFormats.Contains(BarcodeFormat.MSI) then
    begin
      // MSI needs to be activated explicit
      useMsiCheckDigit := false;
      if (hints.ContainsKey(DecodeHintType.ASSUME_MSI_CHECK_DIGIT)) then
      begin
        useMsiCheckDigit :=
          Boolean(hints[DecodeHintType.ASSUME_MSI_CHECK_DIGIT]);

        // readers.Add(new MSIReader(useMsiCheckDigit))
      end;

    end;
    if (possibleFormats.Contains(BarcodeFormat.CODE_39)) or
      (possibleFormats.Contains(BarcodeFormat.All_1D)) then
    begin

      useCode39CheckDigit :=
        (hints.ContainsKey(DecodeHintType.ASSUME_CODE_39_CHECK_DIGIT)) and
        Boolean((hints[DecodeHintType.ASSUME_CODE_39_CHECK_DIGIT]));

      useCode39ExtendedMode :=
        (hints.ContainsKey(DecodeHintType.USE_CODE_39_EXTENDED_MODE)) and
        Boolean(hints[DecodeHintType.USE_CODE_39_EXTENDED_MODE]);

      // readers.Add(new Code39Reader(useCode39CheckDigit, useCode39ExtendedMode))
    end;

    if (possibleFormats.Contains(BarcodeFormat.CODE_93)) or
      (possibleFormats.Contains(BarcodeFormat.All_1D)) then
    begin
      readers.Add(TCode93Reader.Create())
    end;
    if (possibleFormats.Contains(BarcodeFormat.CODE_128)) or
      (possibleFormats.Contains(BarcodeFormat.All_1D)) then
    begin
      readers.Add(TCode128Reader.Create)
    end;
    if (possibleFormats.Contains(BarcodeFormat.ITF)) or
      (possibleFormats.Contains(BarcodeFormat.All_1D)) then
    begin
      readers.Add(TITFReader.Create())
    end;
    if (possibleFormats.Contains(BarcodeFormat.CODABAR)) or
      (possibleFormats.Contains(BarcodeFormat.All_1D)) then
    begin
      // readers.Add(new CodaBarReader())
    end;
    if (possibleFormats.Contains(BarcodeFormat.RSS_14)) or
      (possibleFormats.Contains(BarcodeFormat.All_1D)) then
    begin
      // readers.Add(new RSS14Reader())
    end;
    if (possibleFormats.Contains(BarcodeFormat.RSS_EXPANDED)) or
      (possibleFormats.Contains(BarcodeFormat.All_1D)) then
    begin
      // readers.Add(new RSSExpandedReader())
    end
  end;

  if readers.Count = 0 then
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

    // readers.Add(new MultiFormatUPCEANReader(hints));
    // readers.Add(new Code39Reader(useCode39CheckDigit, useCode39ExtendedMode));
    // readers.Add(new CodaBarReader());
    readers.Add(TCode93Reader.Create());
    readers.Add(TCode128Reader.Create);
    readers.Add(TITFReader.Create);
    // readers.Add(new ITFReader());
    // readers.Add(new RSS14Reader());
    // readers.Add(new RSSExpandedReader())
  end

end;

function TMultiFormatOneDReader.DecodeRow(rowNumber: Integer; row: TBitArray;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  reader: TOneDReader;
begin
  for reader in readers do
  begin
    result := reader.DecodeRow(rowNumber, row, hints);
    if result <> nil then
      exit;
  end;

  result := nil;
end;

procedure TMultiFormatOneDReader.Reset();
var
  reader: TOneDReader;
begin
  for reader in readers do
  begin
    reader.Reset()
  end
end;

end.
