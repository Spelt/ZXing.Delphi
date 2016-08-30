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
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.QrCode.QRCodeReader;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  ZXing.Common.BitArray,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.BarcodeFormat,
  ZXing.ResultPoint,
  ZXing.BinaryBitmap,
  ZXing.QrCode.Internal.Decoder,
  ZXing.Common.BitMatrix,
  ZXing.DecoderResult,
  ZXing.ResultMetadataType,
  ZXing.Common.DetectorResult,
  ZXing.QrCode.Internal.QRCodeDecoderMetaData,
  ZXing.QrCode.Internal.Detector,
  ZXing.QrCode.Internal.DecodedBitStreamParser;

type
  /// <summary>
  /// This implementation can detect and decode QR Codes in an image.
  /// </summary>
  TQRCodeReader = class(TInterfacedObject, IReader)
  private
    FDecoder: TQRDecoder;
    NO_POINTS: TArray<IResultPoint>;

    /// <summary>
    /// This method detects a code in a "pure" image -- that is, pure monochrome image
    /// which contains only an unrotated, unskewed, image of a code, with some white border
    /// around it. This is a specialized method that works exceptionally fast in this special
    /// case.
    ///
    /// <seealso cref="ZXing.Datamatrix.DataMatrixReader.extractPureBits(TBitMatrix)" />
    /// </summary>
    class function extractPureBits(const image: TBitMatrix): TBitMatrix; static;
    class function moduleSize(const leftTopBlack: TArray<Integer>;
      const image: TBitMatrix; var msize: Single): Boolean; static;
  protected
    /// <summary>
    /// Gets the decoder.
    /// </summary>
    /// <returns></returns>
    property Decoder: TQRDecoder read FDecoder;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Locates and decodes a QR code in an image.
    ///
    /// <returns>a String representing the content encoded by the QR code</returns>
    /// </summary>
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    /// <summary>
    /// Locates and decodes a barcode in some format within an image. This method also accepts
    /// hints, each possibly associated to some data, which may help the implementation decode.
    /// </summary>
    /// <param name="image">image of barcode to decode</param>
    /// <param name="hints">passed as a <see cref="TDictionary{TKey, TValue}"/> from <see cref="TDecodeHintType"/>
    /// to arbitrary data. The
    /// meaning of the data depends upon the hint type. The implementation may or may not do
    /// anything with these hints.</param>
    /// <returns>
    /// String which the barcode encodes
    /// </returns>
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;

    /// <summary>
    /// Resets any internal state the implementation has after a decode, to prepare it
    /// for reuse.
    /// </summary>
    procedure reset;
  end;

implementation
uses ZXing.ByteSegments;

{ TQRCodeReader }

constructor TQRCodeReader.Create;
begin
  inherited;
  FDecoder := TQRDecoder.Create;
  NO_POINTS := TArray<IResultPoint>.Create();
end;

destructor TQRCodeReader.Destroy;
begin
  NO_POINTS := nil;
  FDecoder.Free;
  inherited;
end;

function TQRCodeReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := Self.decode(image, nil);
end;

function TQRCodeReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  DecoderResult: TDecoderResult;
  Detector: TDetector;
  points: TArray<IResultPoint>;
  bits: TBitMatrix;
  DetectorResult: TDetectorResult;
  data: TQRCodeDecoderMetaData;
  byteSegments: IByteSegments;
begin
  Result := nil;
  DecoderResult := nil;

  if ((image = nil) or (image.BlackMatrix = nil)) then
    // something is wrong with the image
    exit;
  try

    if ((hints <> nil) and hints.ContainsKey(TDecodeHintType.PURE_BARCODE)) then
    begin
      bits := TQRCodeReader.extractPureBits(image.BlackMatrix);
      if Assigned(bits) then
      begin
        DecoderResult := Decoder.decode(bits, hints);
        points := NO_POINTS;
        bits.Free;
      end
      else
        exit;
    end
    else
    begin
      Detector := TDetector.Create(image.BlackMatrix);
      try
        DetectorResult := Detector.detect(hints);
        if Assigned(DetectorResult) then
        begin
          DecoderResult := Decoder.decode(DetectorResult.bits, hints);
          points := DetectorResult.points;
          DetectorResult.Free;
        end
        else
          exit;
      finally
        Detector.Free;
      end;
    end;

    if (DecoderResult = nil) then
      exit;

    // If the code was mirrored: swap the bottom-left and the top-right points.
    data := TQRCodeDecoderMetaData(DecoderResult.Other);
    if (data <> nil) then
      data.applyMirroredCorrection(points);

    Result := TReadResult.Create(DecoderResult.Text, DecoderResult.RawBytes,
      points, TBarcodeFormat.QR_CODE);

    byteSegments := DecoderResult.byteSegments;

    if (byteSegments <> nil) then
      Result.putMetadata(TResultMetadataType.BYTE_SEGMENTS,  TResultMetaData.CreateByteSegmentsMetadata(byteSegments));

    if (Length(DecoderResult.ecLevel) <> 0) then
      Result.putMetadata(TResultMetadataType.ERROR_CORRECTION_LEVEL, TResultMetaData.CreateStringMetadata(DecoderResult.ecLevel));

    if (DecoderResult.StructuredAppend) then
    begin
      Result.putMetadata(TResultMetadataType.STRUCTURED_APPEND_SEQUENCE,
          TResultMetaData.CreateIntegerMetadata(DecoderResult.StructuredAppendSequenceNumber));
      Result.putMetadata(TResultMetadataType.STRUCTURED_APPEND_PARITY,
        TResultMetaData.CreateIntegerMetadata(DecoderResult.StructuredAppendParity))
    end;

  finally

    if Assigned(DecoderResult) then
      FreeAndNil(DecoderResult);

  end;

end;

procedure TQRCodeReader.reset;
begin
  // do nothing
end;

class function TQRCodeReader.extractPureBits(const image: TBitMatrix)
  : TBitMatrix;
var
  moduleSize: Single;
  leftTopBlack, rightBottomBlack: TArray<Integer>;
  top, bottom, left, right, matrixWidth, matrixHeight, nudge, x, y, iOffset,
    nudgedTooFarRight, nudgedTooFarDown: Integer;
  bits: TBitMatrix;
begin
  Result := nil;

  leftTopBlack := image.getTopLeftOnBit;
  rightBottomBlack := image.getBottomRightOnBit;
  if ((leftTopBlack = nil) or (rightBottomBlack = nil)) then
    exit;

  if (not TQRCodeReader.moduleSize(leftTopBlack, image, moduleSize)) then
    exit;

  top := leftTopBlack[1];
  bottom := rightBottomBlack[1];
  left := leftTopBlack[0];
  right := rightBottomBlack[0];

  // Sanity check!
  if ((left >= right) or (top >= bottom)) then
    exit;

  if ((bottom - top) <> (right - left)) then
    // Special case, where bottom-right module wasn't black so we found something else in the last row
    // Assume it's a square, so use height as the width
    right := (left + (bottom - top));

  matrixWidth := Round(((right - left) + 1) / moduleSize);
  matrixHeight := Round(((bottom - top) + 1) / moduleSize);
  if ((matrixWidth <= 0) or (matrixHeight <= 0)) then
    exit;

  if (matrixHeight <> matrixWidth) then
    // Only possibly decode square regions
    exit;

  // Push in the "border" by half the module width so that we start
  // sampling in the middle of the module. Just in case the image is a
  // little off, this will help recover.
  nudge := Trunc(moduleSize / 2.0);
  Inc(top, nudge);
  Inc(left, nudge);

  // But careful that this does not sample off the edge
  // "right" is the farthest-right valid pixel location -- right+1 is not necessarily
  // This is positive by how much the inner x loop below would be too large
  nudgedTooFarRight := left + Trunc((matrixWidth - 1) * moduleSize) - right;
  if (nudgedTooFarRight > 0) then
  begin
    if (nudgedTooFarRight > nudge) then
      // Neither way fits; abort
      exit;
    Dec(left, nudgedTooFarRight);
  end;
  // See logic above
  nudgedTooFarDown := top + Trunc((matrixHeight - 1) * moduleSize) - bottom;
  if (nudgedTooFarDown > 0) then
  begin
    if (nudgedTooFarDown > nudge) then
      // Neither way fits; abort
      exit;
    Dec(top, nudgedTooFarDown);
  end;

  // Now just read off the bits
  bits := TBitMatrix.Create(matrixWidth, matrixHeight);
  for y := 0 to Pred(matrixHeight) do
  begin
    iOffset := top + Trunc(y * moduleSize);
    for x := 0 to Pred(matrixWidth) do
    begin
      if (image[left + Trunc((x * moduleSize)), iOffset]) then
        bits[x, y] := true;
    end;
  end;

  Result := bits;
end;

class function TQRCodeReader.moduleSize(const leftTopBlack: TArray<Integer>;
  const image: TBitMatrix; var msize: Single): Boolean;
var
  height, width, x, y: Integer;
  inBlack: Boolean;
  transitions: Integer;
begin
  Result := false;

  height := image.height;
  width := image.width;
  x := leftTopBlack[0];
  y := leftTopBlack[1];

  inBlack := true;
  transitions := 0;
  while (((x < width) and (y < height))) do
  begin
    if (inBlack <> image[x, y]) then
    begin
      Inc(transitions);
      if (transitions = 5) then
        break;
      inBlack := not inBlack;
    end;
    Inc(x);
    Inc(y);
  end;

  if ((x = width) or (y = height)) then
  begin
    msize := 0.0;
    exit;
  end;

  msize := ((x - leftTopBlack[0]) / 7.0);
  Result := true;
end;

end.
