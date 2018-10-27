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

  * Original Author: Sean Owen
  * Delphi Implementation by E.Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.Decoder;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.DecodeHintType,
  ZXing.Common.BitMatrix,
  ZXing.QrCode.Internal.BitMatrixParser,
  ZXing.DecoderResult,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.QrCode.Internal.QRCodeDecoderMetaData,
  ZXing.QrCode.Internal.Version,
  ZXing.QrCode.Internal.FormatInformation,
  ZXing.QrCode.Internal.ErrorCorrectionLevel,
  ZXing.QrCode.Internal.DataBlock,
  ZXing.QrCode.Internal.DecodedBitStreamParser;

type
  /// <summary>
  /// <p>The main class which implements QR Code decoding -- as opposed to locating and extracting
  /// the QR Code from an image.</p>
  /// </summary>
  TQRDecoder = class
  private
    rsDecoder: TReedSolomonDecoder;
    /// <summary>
    /// <p>Given data and error-correction codewords received, possibly corrupted by errors, attempts to
    /// correct the errors in-place using Reed-Solomon error correction.</p>
    /// </summary>
    /// <param name="codewordBytes">data and error correction codewords</param>
    /// <param name="numDataCodewords">number of codewords that are data bytes</param>
    /// <returns></returns>
    function correctErrors(const codewordBytes: TArray<Byte>;
      const numDataCodewords: Integer): Boolean;

    function decode(const parser: TBitMatrixParser;
      const hints: TDictionary<TDecodeHintType, TObject>)
      : TDecoderResult; overload;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="Decoder"/> class.
    /// </summary>
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// <p>Convenience method that can decode a QR Code represented as a 2D array of booleans.
    /// "true" is taken to mean a black module.</p>
    /// </summary>
    /// <param name="image">booleans representing white/black QR Code modules</param>
    /// <param name="hints">decoding hints that should be used to influence decoding</param>
    /// <returns>
    /// text and bytes encoded within the QR Code
    /// </returns>
    function decode(const image: TArray<TArray<Boolean>>;
      const hints: TDictionary<TDecodeHintType, TObject>)
      : TDecoderResult; overload;

    /// <summary>
    /// <p>Decodes a QR Code represented as a {@link BitMatrix}. A 1 or "true" is taken to mean a black module.</p>
    /// </summary>
    /// <param name="bits">booleans representing white/black QR Code modules</param>
    /// <param name="hints">decoding hints that should be used to influence decoding</param>
    /// <returns>
    /// text and bytes encoded within the QR Code
    /// </returns>
    function decode(const bits: TBitMatrix;
      const hints: TDictionary<TDecodeHintType, TObject>)
      : TDecoderResult; overload;
  end;

implementation

{ TQRDecoder }
constructor TQRDecoder.Create;
begin
  rsDecoder := TReedSolomonDecoder.Create(TGenericGF.QR_CODE_FIELD_256);
end;

destructor TQRDecoder.Destroy;
begin
  FreeAndNil(rsDecoder);
  inherited;
end;

function TQRDecoder.correctErrors(const codewordBytes: TArray<Byte>;
  const numDataCodewords: Integer): Boolean;
var
  i, numCodewords, numECCodewords: Integer;
  codewordsInts: TArray<Integer>;
begin
  Result := false;

  numCodewords := Length(codewordBytes);
  // First read into an array of ints
  codewordsInts := TArray<Integer>.Create();
  SetLength(codewordsInts, numCodewords);
  for i := 0 to Pred(numCodewords) do
    codewordsInts[i] := (codewordBytes[i] and $FF);
  numECCodewords := Length(codewordBytes) - numDataCodewords;

  if (not rsDecoder.decode(codewordsInts, numECCodewords)) then
    exit;

  // Copy back into array of bytes -- only need to worry about the bytes that were data
  // We don't care about errors in the error-correction codewords
  for i := 0 to Pred(numDataCodewords) do
    codewordBytes[i] := Byte(codewordsInts[i]);

  Result := true;
end;

function TQRDecoder.decode(const image: TArray<TArray<Boolean>>;
  const hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
var
  dimension, i, j: Integer;
  bits: TBitMatrix;
begin
  dimension := Length(image);
  bits := TBitMatrix.Create(dimension);
  try
    for i := 0 to Pred(dimension) do
      for j := 0 to Pred(dimension) do
        bits[j, i] := image[i][j];

    Result := decode(bits, hints);
  finally
    bits.Free;
  end;
end;

function TQRDecoder.decode(const bits: TBitMatrix;
  const hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
var
  parser: TBitMatrixParser;
begin
  Result := nil;

  // Construct a parser and read version, error-correction level
  parser := TBitMatrixParser.createBitMatrixParser(bits);
  if Assigned(parser) then
  begin
    try
      Result := decode(parser, hints);
      if (Result = nil) then
      begin
        // Revert the bit matrix
        parser.remask;

        // Will be attempting a mirrored reading of the version and format info.
        parser.setMirror(true);

        // Preemptively read the version.
        if (parser.readVersion = nil) then
          exit;

        // Preemptively read the format information.
        if (parser.readFormatInformation = nil) then
          exit;

        (*
          * Since we're here, this means we have successfully detected some kind
          * of version and format information when mirrored. This is a good sign,
          * that the QR code may be mirrored, and we should try once more with a
          * mirrored content.
        *)
        // Prepare for a mirrored reading.
        parser.mirror;

        Result := decode(parser, hints);

        if (Result <> nil) then
          // Success! Notify the caller that the code was mirrored.
          Result.Other := TQRCodeDecoderMetaData.Create(true);
      end;
    finally
      parser.Free;
    end;
  end;
end;

function TQRDecoder.decode(const parser: TBitMatrixParser;
  const hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
var
  DataBlock: TDataBlock;
  dataBlocks: TArray<TDataBlock>;
  Version: TVersion;
  formatInfo: TFormatInformation;
  ecLevel: TErrorCorrectionLevel;
  codeWords, resultBytes, codewordBytes: TArray<Byte>;
  totalBytes, resultOffset, i, numDataCodewords: Integer;
begin
  Result := nil;

  Version := parser.readVersion;
  if (Version = nil) then
    exit;

  formatInfo := parser.readFormatInformation;
  if (formatInfo = nil) then
    exit;

  ecLevel := formatInfo.ErrorCorrectionLevel;

  // Read codewords
  codeWords := parser.readCodewords;
  if (codeWords = nil) then
  begin
    FreeAndNil(formatInfo);
    exit;
  end;

  // Separate into data blocks
  dataBlocks := TDataBlock.getDataBlocks(codeWords, Version, ecLevel);

  // Count total number of data bytes
  totalBytes := 0;
  for DataBlock in dataBlocks do
    Inc(totalBytes, DataBlock.numDataCodewords);

  resultBytes := TArray<Byte>.Create();
  SetLength(resultBytes, totalBytes);
  try

    resultOffset := 0;

    // Error-correct and copy data blocks together into a stream of bytes
    for DataBlock in dataBlocks do
    begin
      codewordBytes := DataBlock.codeWords;
      numDataCodewords := DataBlock.numDataCodewords;
      if (not self.correctErrors(codewordBytes, numDataCodewords)) then
      begin
        exit(nil);
      end;

      for i := 0 to Pred(numDataCodewords) do
      begin
        resultBytes[resultOffset] := codewordBytes[i];
        Inc(resultOffset);
      end;
    end;

    // Decode the contents of that stream of bytes
    Result := TDecodedBitStreamParser.decode(resultBytes, Version,
      ecLevel, hints);

  finally
    for DataBlock in dataBlocks do
      DataBlock.Free;
    FreeAndNil(formatInfo);
  end;

end;

end.
