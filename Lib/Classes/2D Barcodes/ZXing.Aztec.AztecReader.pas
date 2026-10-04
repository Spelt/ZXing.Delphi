{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
  * Copyright 2022 Axel Waggershauser
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

  * Ported from zxing-cpp (AZReader.cpp).
}

unit ZXing.Aztec.AztecReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.DecoderResult,
  ZXing.ResultMetadataType,
  ZXing.ResultPoint,
  ZXing.BinaryBitmap,
  ZXing.Aztec.Internal.Detector,
  ZXing.Aztec.Internal.Decoder;

type
  /// <summary>
  /// Detects and decodes Aztec Codes (compact, full range and Aztec Runes).
  /// </summary>
  TAztecReader = class(TInterfacedObject, IReader, IMultipleReader)
  private
    function createResult(decoderResult: TDecoderResult;
      detected: TAztecDetectorResult): TReadResult;
  public
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    /// <summary>All Aztec Codes in the image, see IMultipleReader.
    /// </summary>
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

implementation

{ TAztecReader }

function TAztecReader.createResult(decoderResult: TDecoderResult;
  detected: TAztecDetectorResult): TReadResult;
begin
  Result := TReadResult.Create(decoderResult.Text, decoderResult.RawBytes,
    detected.Position, TBarcodeFormat.AZTEC);
  // (a copy: the points and the position are mapped separately)
  Result.Position := Copy(detected.Position);
  Result.SymbologyIdentifier := decoderResult.SymbologyIdentifier;
  Result.IsMirrored := detected.IsMirrored;
  if (Length(decoderResult.ECLevel) <> 0) then
    Result.putMetadata(TResultMetadataType.ERROR_CORRECTION_LEVEL,
      TResultMetaData.CreateStringMetadata(decoderResult.ECLevel));
  // like QR Code: the index (0 based) in the high nibble, the count - 1 in
  // the low one; Aztec has no parity
  if (decoderResult.StructuredAppendSequenceNumber >= 0) then
    Result.putMetadata(TResultMetadataType.STRUCTURED_APPEND_SEQUENCE,
      TResultMetaData.CreateIntegerMetadata
      (decoderResult.StructuredAppendSequenceNumber));
end;

function TAztecReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TAztecReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Result := nil;
  var results := TList<TReadResult>.Create;
  try
    decodeMultiple(image, hints, results, 1);
    if (results.Count > 0) then
      Result := results[0];
  finally
    results.Free;
  end;
end;

procedure TAztecReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;

  var isPure := (hints <> nil) and
    hints.ContainsKey(TDecodeHintType.PURE_BARCODE);
  var tryHarder := (hints <> nil) and
    hints.ContainsKey(TDecodeHintType.TRY_HARDER);
  DetectAztec(image.BlackMatrix, isPure, tryHarder,
    function(detected: TAztecDetectorResult): Boolean
    begin
      Result := false;
      var error: string;
      var decoded := DecodeAztec(detected, error);
      if (decoded = nil) then
      begin
        AddFailedResult(hints, error, TBarcodeFormat.AZTEC,
          detected.Position, nil);
        exit;
      end;
      try
        var r := createResult(decoded, detected);
        if ContainsResult(results, r) then
          r.Free
        else
          results.Add(r);
      finally
        decoded.Free;
      end;
      Result := ResultsFull(results, maxCount);
    end);
end;

procedure TAztecReader.reset;
begin
  // do nothing
end;

end.
