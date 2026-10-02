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

  * Original Author: bbrown@google.com (Brian Brown)
  * Delphi Implementation by K. Gossens
}

unit ZXing.Datamatrix.DataMatrixReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.Common.Detector.MathUtils,
  Math,
  ZXing.Common.BitArray,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.DecoderResult,
  ZXing.Common.DetectorResult,
  ZXing.ResultMetadataType,
  ZXing.ResultPoint,
  ZXing.Common.BitMatrix,
  ZXing.BinaryBitmap,
  ZXing.Datamatrix.Internal.Decoder,
  ZXing.Datamatrix.Internal.Detector;

type
  /// <summary>
  /// This implementation can detect and decode Data Matrix codes in an image.
  /// </summary>
  TDataMatrixReader = class(TInterfacedObject, IReader)
  private const
    /// <summary>
    /// Finest grid of detector start points: 4 means start points every 1/4
    /// of the image width and height. With TRY_HARDER a grid twice as fine is
    /// used as well, which finds smaller codes but costs more time on images
    /// without a code.
    /// </summary>
    MAX_GRID_DIVISIONS = 4;
    /// <summary>
    /// Largest radius (in pixels) used to merge the dots of dot-peen codes.
    /// </summary>
    MAX_CLOSING_RADIUS = 2;
  private
    FDecoder: TDataMatrixDecoder;
    NO_POINTS: TArray<IResultPoint>;

    /// <summary>
    /// Tries the center of the image first, then a grid of start points up to
    /// maxDivisions, because the detector only finds a code that covers its
    /// start point.
    /// </summary>
    function searchAndDecode(const image: TBitMatrix; maxDivisions: Integer;
      assumeGS1: Boolean; var points: TArray<IResultPoint>): TDecoderResult;

    /// <summary>
    /// Morphological closing: grows the black areas by radius pixels and
    /// shrinks them again, which merges dots that lie close together.
    /// </summary>
    class function closeMatrix(const image: TBitMatrix; radius: Integer)
      : TBitMatrix; static;

    /// <summary>
    /// Detects a code around start point (x, y) and decodes it.
    /// Returns nil when nothing could be detected or decoded.
    /// </summary>
    function detectAndDecode(const image: TBitMatrix; x, y: Integer;
      assumeGS1: Boolean; var points: TArray<IResultPoint>): TDecoderResult;

    /// <summary>
    /// This method detects a code in a "pure" image -- that is, pure monochrome image
    /// which contains only an unrotated, unskewed, image of a code, with some white border
    /// around it. This is a specialized method that works exceptionally fast in this special
    /// case.
    ///
    /// <seealso cref="ZXing.QrCode.QRCodeReader.extractPureBits(TBitMatrix)" />
    /// </summary>
    function extractPureBits(const image: TBitMatrix): TBitMatrix;

    function moduleSize(const leftTopBlack: TArray<Integer>;
      const image: TBitMatrix; var AModuleSize: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// This implementation can detect and decode Data Matrix codes in an image.
    /// </summary>
    function decode(const image: TBinaryBitmap): TReadResult; overload;

    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;

    procedure reset;
  end;

implementation
uses ZXing.ByteSegments;

{ TDataMatrixReader }

constructor TDataMatrixReader.Create;
begin
  inherited;
  FDecoder := TDataMatrixDecoder.Create;
  NO_POINTS := TArray<IResultPoint>.Create();
end;

destructor TDataMatrixReader.Destroy;
begin
  NO_POINTS := nil;
  FDecoder.Free;
  inherited;
end;

function TDataMatrixReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TDataMatrixReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  DecoderResult: TDecoderResult;
  points: TArray<IResultPoint>;
  bits: TBitMatrix;
  ByteSegments: IByteSegments;
  maxDivisions, radius: Integer;
  assumeGS1, tryHarder: Boolean;
  closedMatrix: TBitMatrix;
begin
  Result := nil;
  DecoderResult := nil;
  assumeGS1 := (hints <> nil) and hints.ContainsKey(TDecodeHintType.ASSUME_GS1);
  try

    if ((hints <> nil) and hints.ContainsKey(TDecodeHintType.PURE_BARCODE)) then
    begin
      bits := extractPureBits(image.BlackMatrix);
      if Assigned(bits) then
      begin
        DecoderResult := FDecoder.decode(bits, assumeGS1);
        points := NO_POINTS;
        FreeAndNil(bits);
      end
      else
        exit;
    end
    else
    begin
      tryHarder := (hints <> nil) and
        hints.ContainsKey(TDecodeHintType.TRY_HARDER);

      maxDivisions := MAX_GRID_DIVISIONS;
      if tryHarder then
        maxDivisions := MAX_GRID_DIVISIONS * 2;

      DecoderResult := searchAndDecode(image.BlackMatrix, maxDivisions,
        assumeGS1, points);

      // Dot-peen codes consist of separate dots, which the detector does not
      // see as solid lines. Merging the dots (closing) helps, but costs
      // considerable time on images without a code, so only with TRY_HARDER.
      if (DecoderResult = nil) and tryHarder then
      begin
        for radius := 1 to MAX_CLOSING_RADIUS do
        begin
          closedMatrix := closeMatrix(image.BlackMatrix, radius);
          try
            DecoderResult := searchAndDecode(closedMatrix, MAX_GRID_DIVISIONS,
              assumeGS1, points);
          finally
            closedMatrix.Free;
          end;
          if (DecoderResult <> nil) then
            break;
        end;
      end;
    end;

    if (DecoderResult = nil) then
      exit;

    Result := TReadResult.Create(DecoderResult.Text, DecoderResult.RawBytes,
      points, TBarcodeFormat.DATA_MATRIX);

    ByteSegments := DecoderResult.ByteSegments;

    if (ByteSegments <> nil) then
      Result.putMetadata(TResultMetadataType.BYTE_SEGMENTS, TResultMetaData.CreateByteSegmentsMetadata( byteSegments));

    if (Length(DecoderResult.ECLevel) <> 0) then
      Result.putMetadata(TResultMetadataType.ERROR_CORRECTION_LEVEL, TResultMetaData.CreateStringMetadata(DecoderResult.ECLevel));

  finally

    byteSegments:=nil;

    if Assigned(DecoderResult) then
      FreeAndNil(DecoderResult);

  end;

end;

function TDataMatrixReader.searchAndDecode(const image: TBitMatrix;
  maxDivisions: Integer; assumeGS1: Boolean; var points: TArray<IResultPoint>)
  : TDecoderResult;
var
  divisions, gridX, gridY: Integer;
begin
  Result := detectAndDecode(image, image.width div 2, image.height div 2,
    assumeGS1, points);

  divisions := 4;
  while (Result = nil) and (divisions <= maxDivisions) do
  begin
    for gridY := 1 to Pred(divisions) do
    begin
      for gridX := 1 to Pred(divisions) do
      begin
        // points with both coordinates even were tried on the coarser grid
        if (not Odd(gridX)) and (not Odd(gridY)) then
          continue;

        Result := detectAndDecode(image, (image.width * gridX) div divisions,
          (image.height * gridY) div divisions, assumeGS1, points);
        if (Result <> nil) then
          exit;
      end;
    end;
    divisions := divisions * 2;
  end;
end;

class function TDataMatrixReader.closeMatrix(const image: TBitMatrix;
  radius: Integer): TBitMatrix;
var
  width, height, x, y: Integer;
  pixels: TArray<Byte>;

  // Sliding window over one row or column of length count, starting at
  // offset first with distance step between pixels. Grow: a pixel becomes
  // black when any pixel in the window is black. Shrink: a pixel stays black
  // only when all pixels of the window inside the image are black.
  procedure pass(first, step, count: Integer; grow: Boolean);
  var
    i, blackCount, inside: Integer;
    line: TArray<Byte>;
  begin
    SetLength(line, count);
    for i := 0 to Pred(count) do
      line[i] := pixels[first + i * step];

    blackCount := 0;
    for i := 0 to Pred(Min(radius, count)) do
      Inc(blackCount, line[i]);

    for i := 0 to Pred(count) do
    begin
      if (i + radius < count) then
        Inc(blackCount, line[i + radius]);
      if (i - radius - 1 >= 0) then
        Dec(blackCount, line[i - radius - 1]);

      inside := Min(i + radius, Pred(count)) - Max(i - radius, 0) + 1;
      if grow then
        pixels[first + i * step] := Ord(blackCount > 0)
      else
        pixels[first + i * step] := Ord(blackCount = inside);
    end;
  end;

begin
  width := image.width;
  height := image.height;
  SetLength(pixels, width * height);
  for y := 0 to Pred(height) do
    for x := 0 to Pred(width) do
      pixels[y * width + x] := Ord(image[x, y]);

  for y := 0 to Pred(height) do
    pass(y * width, 1, width, true);
  for x := 0 to Pred(width) do
    pass(x, width, height, true);
  for y := 0 to Pred(height) do
    pass(y * width, 1, width, false);
  for x := 0 to Pred(width) do
    pass(x, width, height, false);

  Result := TBitMatrix.Create(width, height);
  for y := 0 to Pred(height) do
    for x := 0 to Pred(width) do
      if (pixels[y * width + x] <> 0) then
        Result[x, y] := true;
end;

function TDataMatrixReader.detectAndDecode(const image: TBitMatrix;
  x, y: Integer; assumeGS1: Boolean; var points: TArray<IResultPoint>)
  : TDecoderResult;
var
  matrixDetector: TDataMatrixDetector;
  DetectorResult: TDetectorResult;
  countThroughCenters: Boolean;
  attempt, triedWidth, triedHeight, edgeIndex: Integer;
  bits: TBitMatrix;
const
  RETRY_EDGES: array [0 .. 1] of Single = (0.1, 0.4);
begin
  Result := nil;
  triedWidth := 0;
  triedHeight := 0;

  matrixDetector := TDataMatrixDetector.Create(image, x, y);
  try
    // Counting the modules through their centers works better for rotated
    // codes, counting along the edge better for very small modules.
    for attempt := 0 to 1 do
    begin
      countThroughCenters := (attempt = 0);
      DetectorResult := matrixDetector.detect(countThroughCenters);
      if (DetectorResult = nil) then
        exit;

      try
        // same grid as the previous attempt: decoding again will not help
        if (DetectorResult.bits.width = triedWidth) and
          (DetectorResult.bits.height = triedHeight) then
          exit;
        triedWidth := DetectorResult.bits.width;
        triedHeight := DetectorResult.bits.height;

        Result := FDecoder.decode(DetectorResult.bits, assumeGS1);

        // The corner points are not always equally far from the edge (e.g.
        // with dot-peen codes), so sample again with other edge distances.
        edgeIndex := 0;
        while (Result = nil) and (edgeIndex <= High(RETRY_EDGES)) do
        begin
          bits := matrixDetector.sampleGrid(image, DetectorResult.points[0],
            DetectorResult.points[1], DetectorResult.points[2],
            DetectorResult.points[3], triedWidth, triedHeight,
            RETRY_EDGES[edgeIndex]);
          if (bits <> nil) then
          begin
            Result := FDecoder.decode(bits, assumeGS1);
            bits.Free;
          end;
          Inc(edgeIndex);
        end;

        if (Result <> nil) then
        begin
          points := DetectorResult.points;
          exit;
        end;
      finally
        DetectorResult.Free;
      end;
    end;
  finally
    matrixDetector.Free;
  end;
end;

function TDataMatrixReader.extractPureBits(const image: TBitMatrix): TBitMatrix;
var
  LModuleSize: Integer;
  leftTopBlack, rightBottomBlack: TArray<Integer>;
  top, bottom, left, right: Integer;
  matrixWidth, matrixHeight: Integer;
  nudge: Integer;
  bits: TBitMatrix;
  x, y, iOffset: Integer;
begin
  Result := nil;

  leftTopBlack := image.getTopLeftOnBit;
  rightBottomBlack := image.getBottomRightOnBit;
  if ((leftTopBlack = nil) or (rightBottomBlack = nil)) then
    exit;

  if (not moduleSize(leftTopBlack, image, LModuleSize)) then
    exit;

  top := leftTopBlack[1];
  bottom := rightBottomBlack[1];
  left := leftTopBlack[0];
  right := rightBottomBlack[0];

  matrixWidth := ((right - left + 1) div LModuleSize);
  matrixHeight := ((bottom - top + 1) div LModuleSize);
  if ((matrixWidth <= 0) or (matrixHeight <= 0)) then
    exit;

  // Push in the "border" by half the module width so that we start
  // sampling in the middle of the module. Just in case the image is a
  // little off, this will help recover.
  nudge :=  TMathUtils.Asr(LModuleSize, 1);
  Inc(top, nudge);
  Inc(left, nudge);

  // Now just read off the bits
  bits := TBitMatrix.Create(matrixWidth, matrixHeight);
  for y := 0 to Pred(matrixHeight) do
  begin
    iOffset := (top + (y * LModuleSize));
    for x := 0 to Pred(matrixWidth) do
    begin
      if (image[(left + (x * LModuleSize)), iOffset]) then
        bits[x, y] := true;
    end;
  end;

  Result := bits;
end;

function TDataMatrixReader.moduleSize(const leftTopBlack: TArray<Integer>;
  const image: TBitMatrix; var AModuleSize: Integer): Boolean;
var
  width, x, y: Integer;
begin
  Result := false;

  width := image.width;
  x := leftTopBlack[0];
  y := leftTopBlack[1];

  while ((x < width) and image[x, y]) do
    Inc(x);

  if (x = width) then
  begin
    AModuleSize := 0;
    exit;
  end;

  AModuleSize := (x - leftTopBlack[0]);
  if (AModuleSize = 0) then
    exit;

  Result := true;
end;

procedure TDataMatrixReader.reset;
begin
  // do nothing
end;

end.
