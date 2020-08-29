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
  * Delphi Implementation by K. Gossens
}

unit ZXing.Datamatrix.Internal.Decoder;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.DecodeHintType,
  ZXing.Common.BitMatrix,
  ZXing.Datamatrix.Internal.BitMatrixParser,
  ZXing.Datamatrix.Internal.DataBlock,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.DecoderResult,
  ZXing.Datamatrix.Internal.DecodedBitStreamParser;

type
  /// <summary>
  /// <p>The main class which implements Data Matrix Code decoding -- as opposed to locating and extracting
  /// the Data Matrix Code from an image.</p>
  /// </summary>
  TDataMatrixDecoder = class sealed
  private
    rsDecoder: TReedSolomonDecoder;
  public
    constructor Create;
    destructor Destroy; override;

    function correctErrors(codewordBytes: TArray<Byte>; numDataCodewords: Integer): boolean;
    function decode(bits: TBitMatrix): TDecoderResult; overload;
    function decode(image: TArray < TArray < boolean >> ): TDecoderResult; overload;
  end;

implementation

{ TDataMatrixDecoder }

/// <summary>
/// Initializes a new instance of the <see cref="Decoder"/> class.
/// </summary>
constructor TDataMatrixDecoder.Create;
begin
  rsDecoder := TReedSolomonDecoder.Create(TGenericGF.DATA_MATRIX_FIELD_256);
end;

destructor TDataMatrixDecoder.Destroy;
begin
  FreeAndNil(rsDecoder);
  inherited;
end;

/// <summary>
/// <p>Convenience method that can decode a Data Matrix Code represented as a 2D array of booleans.
/// "true" is taken to mean a black module.</p>
///
/// <param name="image">booleans representing white/black Data Matrix Code modules</param>
/// <returns>text and bytes encoded within the Data Matrix Code</returns>
/// <exception cref="FormatException">if the Data Matrix Code cannot be decoded</exception>
/// <exception cref="ChecksumException">if error correction fails</exception>
/// </summary>
function TDataMatrixDecoder.decode(image: TArray < TArray < boolean >> ): TDecoderResult;
var
  i, j: Integer;
  dimension: Integer;
  bits: TBitMatrix;
begin
  dimension := Length(image);
  bits := TBitMatrix.Create(dimension);
  for i := 0 to Pred(dimension) do
  begin
    for j := 0 to Pred(dimension) do
    begin
      if (image[i][j]) then
        bits[j, i] := true;
    end;
  end;

  Result := decode(bits);
end;

/// <summary>
/// <p>Decodes a Data Matrix Code represented as a <see cref="BitMatrix" />. A 1 or "true" is taken
/// to mean a black module.</p>
/// </summary>
/// <param name="bits">booleans representing white/black Data Matrix Code modules</param>
/// <returns>text and bytes encoded within the Data Matrix Code</returns>
function TDataMatrixDecoder.decode(bits: TBitMatrix): TDecoderResult;
var
  i, j: Integer;
  db: TDataBlock;
  parser: TBitMatrixParser;
  codewords: TArray<Byte>;
  dataBlocks: TArray<TDataBlock>;
  dataBlocksCount, totalBytes: Integer;
  resultBytes: TArray<Byte>;
  DataBlock: TDataBlock;
  codewordBytes: TArray<Byte>;
  numDataCodewords: Integer;
begin
  // Construct a parser and read version, error-correction level
  parser := TBitMatrixParser.Create(bits);
  DataBlock := nil;

  try

    if (parser.Version = nil) then
    begin
      Result := nil;
      exit;
    end;

    // Read codewords
    codewords := parser.readCodewords;
    if (codewords = nil) then
    begin
      Result := nil;
      exit;
    end;
    // Separate into data blocks
    dataBlocks := TDataBlock.getDataBlocks(codewords, parser.Version);

    dataBlocksCount := Length(dataBlocks);

    // Count total number of data bytes
    totalBytes := 0;
    for db in dataBlocks do
    begin
      Inc(totalBytes, db.numDataCodewords)
    end;
    resultBytes := TArray<Byte>.Create();
    SetLength(resultBytes, totalBytes);

    // Error-correct and copy data blocks together into a stream of bytes
    for j := 0 to Pred(dataBlocksCount) do
    begin
      DataBlock := dataBlocks[j];
      codewordBytes := DataBlock.codewords;
      numDataCodewords := DataBlock.numDataCodewords;
      if (not correctErrors(codewordBytes, numDataCodewords)) then
      begin
        if Assigned(DataBlock) then
          DataBlock.Free;
        DataBlock := nil;
        resultBytes := nil;
        codewordBytes := nil;
        Result := nil;
        exit;
      end;
      for i := 0 to Pred(numDataCodewords) do
      begin
        // De-interlace data blocks.
        resultBytes[(i * dataBlocksCount) + j] := codewordBytes[i];
      end;

      DataBlock.Free;
      DataBlock := nil;
    end;

    // Decode the contents of that stream of bytes
    Result := TDecodedBitStreamParser.decode(resultBytes);

  finally

    if Assigned(DataBlock) then
      DataBlock.Free;

    DataBlock := nil;
    resultBytes := nil;
    codewordBytes := nil;
    parser.Free;
  end;

end;

/// <summary>
/// <p>Given data and error-correction codewords received, possibly corrupted by errors, attempts to
/// correct the errors in-place using Reed-Solomon error correction.</p>
///
/// <param name="codewordBytes">data and error correction codewords</param>
/// <param name="numDataCodewords">number of codewords that are data bytes</param>
/// </summary>
function TDataMatrixDecoder.correctErrors(codewordBytes: TArray<Byte>; numDataCodewords: Integer): boolean;
var
  i, numCodewords, numECCodewords: Integer;
  codewordsInts: TArray<Integer>;
begin
  numCodewords := Length(codewordBytes);
  // First read into an array of ints
  codewordsInts := TArray<Integer>.Create();
  SetLength(codewordsInts, numCodewords);
  for i := 0 to Pred(numCodewords) do
  begin
    codewordsInts[i] := (codewordBytes[i] and $FF);
  end;
  numECCodewords := (Length(codewordBytes) - numDataCodewords);
  if (not rsDecoder.decode(codewordsInts, numECCodewords)) then
  begin
    Result := false;
    exit;
  end;
  // Copy back into array of bytes -- only need to worry about the bytes that were data
  // We don't care about errors in the error-correction codewords
  for i := 0 to Pred(numDataCodewords) do
  begin
    codewordBytes[i] := Byte(codewordsInts[i]);
  end;

  Result := true;
end;

end.
