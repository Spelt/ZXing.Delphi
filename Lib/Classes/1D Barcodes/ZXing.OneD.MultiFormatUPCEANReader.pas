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

  * Original Author: Sean Owen
  * Delphi Implementation by K. Gossens
}

unit ZXing.OneD.MultiFormatUPCEANReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  DecodeHintType,
  ZXing.Common.BitArray,
  ZXing.OneD.OneDReader,
  ZXing.OneD.UPCEANReader,
  ZXing.OneD.EAN13Reader;

type
  /// <summary>
  ///   <p>A reader that can read all available UPC/EAN formats. If a caller wants to try to
  /// read all such formats, it is most efficient to use this implementation rather than invoke
  /// individual readers.</p>
  /// </summary>
  TMultiFormatUPCEANReader = class(TOneDReader)
  private
    readers: TList<TUPCEANReader>;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="MultiFormatUPCEANReader"/> class.
    /// </summary>
    /// <param name="hints">The hints.</param>
    constructor Create(const hints: TDictionary<TDecodeHintType, TObject>);
    destructor Destroy; override;

    /// <summary>
    ///   <p>Attempts to decode a one-dimensional barcode format given a single row of
    /// an image.</p>
    /// </summary>
    /// <param name="rowNumber">row number from top of the row</param>
    /// <param name="row">the black/white pixel data of the row</param>
    /// <param name="hints">decode hints</param>
    /// <returns>
    ///   <see cref="TReadResult"/>containing encoded string and start/end of barcode or null if an error occurs or barcode cannot be found
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

// TODO: Not all 1D EAN codes are fully supported yet
constructor TMultiFormatUPCEANReader.Create(
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

  readers := TList<TUPCEANReader>.Create();
  if (possibleFormats <> nil) then
  begin
    if ((possibleFormats.Contains(TBarcodeFormat.EAN_13)) or
        (possibleFormats.Contains(TBarcodeFormat.All_1D)))
    then
       readers.Add(TEAN13Reader.Create())
    else
    begin
      // TODO: Not yet implemented!
      {if ((possibleFormats.Contains(TBarcodeFormat.UPC_A)) or
          (possibleFormats.Contains(TBarcodeFormat.All_1D)))
      then
         readers.Add(TUPCAReader.Create());}
    end;
    // TODO: Not yet implemented!
    {if ((possibleFormats.Contains(TBarcodeFormat.EAN_8)) or
        (possibleFormats.Contains(TBarcodeFormat.All_1D)))
    then
       readers.Add(TEAN8Reader.Create());
    if ((possibleFormats.Contains(TBarcodeFormat.UPC_E)) or
        (possibleFormats.Contains(TBarcodeFormat.All_1D)))
    then
       readers.Add(TUPCEReader.Create());}
  end;

  if (readers.Count = 0) then
  begin
    readers.Add(TEAN13Reader.Create());
    // UPC-A is covered by EAN-13
    // TODO: Not yet implemented!
    //readers.Add(TEAN8Reader.Create());
    //readers.Add(TUPCEReader.Create());
  end;
end;

destructor TMultiFormatUPCEANReader.Destroy;
var
  reader: TUPCEANReader;
begin
  for reader in readers do
    reader.Free;

  readers.clear;
  FreeAndNil(readers);
  inherited;
end;

function TMultiFormatUPCEANReader.decodeRow(const rowNumber: Integer;
  const row: TBitArray; const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  reader: TUPCEANReader;
  startGuardPattern : TArray<Integer>;
  ean13MayBeUPCA,
  canReturnUPCA : Boolean;
  possibleFormats : TList<TBarcodeFormat>;
  ResultUPCA : TReadResult;
begin
  Result := nil;

  // Compute this location once and reuse it on multiple implementations
  startGuardPattern := TUPCEANReader.findStartGuardPattern(row);
  if (startGuardPattern = nil)
  then
     exit;

  for reader in readers do
  begin
    Result := reader.decodeRow(rowNumber, row, startGuardPattern, hints);
    if (Result = nil)
    then
       continue;

    // Special case: a 12-digit code encoded in UPC-A is identical to a "0"
    // followed by those 12 digits encoded as EAN-13. Each will recognize such a code,
    // UPC-A as a 12-digit string and EAN-13 as a 13-digit string starting with "0".
    // Individually these are correct and their readers will both read such a code
    // and correctly call it EAN-13, or UPC-A, respectively.
    //
    // In this case, if we've been looking for both types, we'd like to call it
    // a UPC-A code. But for efficiency we only run the EAN-13 decoder to also read
    // UPC-A. So we special case it here, and convert an EAN-13 result to a UPC-A
    // result if appropriate.
    //
    // But, don't return UPC-A if UPC-A was not a requested format!
    ean13MayBeUPCA := (Result.BarcodeFormat = TBarcodeFormat.EAN_13) and
                      (Result.Text[1] = '0');

    if ((hints = nil) or (not hints.ContainsKey(DecodeHintType.POSSIBLE_FORMATS)))
    then
       possibleFormats := nil
    else
       possibleFormats := TList<TBarcodeFormat>(hints[DecodeHintType.POSSIBLE_FORMATS]);

    canReturnUPCA := (possibleFormats = nil) or
                     (possibleFormats.Contains(TBarcodeFormat.UPC_A)) or
                     (possibleFormats.Contains(TBarcodeFormat.All_1D));

    if (ean13MayBeUPCA) and (canReturnUPCA) then
    begin
      // Transfer the metdata across
      ResultUPCA := TReadResult.Create(Result.Text.Substring(1),
                                       Result.RawBytes,
                                       Result.ResultPoints,
                                       TBarcodeFormat.UPC_A);
      ResultUPCA.putAllMetadata(Result.ResultMetadata);
      Result.Free;
      Result := resultUPCA;
      break;
    end;
  end;
end;

/// <summary>
/// Resets any internal state the implementation has after a decode, to prepare it
/// for reuse.
/// </summary>
procedure TMultiFormatUPCEANReader.reset();
var
  reader: TUPCEANReader;
begin
  for reader in readers do
    reader.reset();
end;

end.
