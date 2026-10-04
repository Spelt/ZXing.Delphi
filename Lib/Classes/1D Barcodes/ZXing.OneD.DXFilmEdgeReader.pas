{
  * Copyright 2023 Antoine Merino
  * Copyright 2023 Axel Waggershauser
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

  * Ported from zxing-cpp (ODDXFilmEdgeReader.cpp): the DX code on the edge
  * of 35 mm film (a clock track and a data track).
}

unit ZXing.OneD.DXFilmEdgeReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// Decodes the DX film edge code of 35 mm film: the product number, the
  /// generation number and (when there) the frame number, like '115-10/11A';
  /// SymbologyIdentifier ]XF.
  /// </summary>
  TDXFilmEdgeReader = class(TOneDReader)
  private type
    TClock = record
      HasFrameNr: Boolean; // the longer version, with the frame number
      RowNumber, XStart, XStop: Integer;
      function DataLength: Integer;
      function ModuleSize: Single;
      function IsCloseTo(x, y, cx: Integer): Boolean;
    end;
  private
    FCenterRow: Integer;
    FClocks: TList<TClock>;
    function FindClock(x, y: Integer): Integer;
    procedure AddClock(const clock: TClock);
  protected
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
    function HasPatternDecoder: Boolean; override;
    procedure ResetDecodingState; override;
    function UsesDecodingState: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>There is no decoder of before: nil.</summary>
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

implementation

uses
  System.Math,
  ZXing.ResultPoint;

const
  // the clock track is longer with the frame number
  CLOCK_LENGTH_FN = 31;
  CLOCK_LENGTH_NO_FN = 23;
  // the data track without the start and stop patterns
  DATA_LENGTH_FN = 23;
  DATA_LENGTH_NO_FN = 15;

  CLOCK_PATTERN_FN: array [0 .. 24] of Integer = (5, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3);
  CLOCK_PATTERN_NO_FN: array [0 .. 16] of Integer = (5, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 3);
  DATA_START_PATTERN: array [0 .. 4] of Integer = (1, 1, 1, 1, 1);
  DATA_STOP_PATTERN: array [0 .. 2] of Integer = (1, 1, 1);

{ TDXFilmEdgeReader.TClock }

function TDXFilmEdgeReader.TClock.DataLength: Integer;
begin
  if HasFrameNr then
    Result := DATA_LENGTH_FN
  else
    Result := DATA_LENGTH_NO_FN;
end;

function TDXFilmEdgeReader.TClock.ModuleSize: Single;
begin
  if HasFrameNr then
    Result := (XStop + 1 - XStart) / CLOCK_LENGTH_FN
  else
    Result := (XStop + 1 - XStart) / CLOCK_LENGTH_NO_FN;
end;

function TDXFilmEdgeReader.TClock.IsCloseTo(x, y, cx: Integer): Boolean;
begin
  Result := (Abs(x - cx) <= Trunc(ModuleSize * 0.5)) and
    (Abs(y - RowNumber) <= Trunc(ModuleSize * 4));
end;

/// <summary>Whether the first Length(pattern) elements of view match pattern
/// with a quiet zone in front; view becomes those elements.</summary>
function MatchPattern(var view: TPatternView; const pattern: array of Integer;
  minQuietZone: Double): Boolean;
begin
  view := view.SubView(0, Length(pattern));
  Result := view.IsValid and (IsPattern(view, pattern, false,
    view.SpaceInFront, minQuietZone) > 0);
end;

{ TDXFilmEdgeReader }

constructor TDXFilmEdgeReader.Create;
begin
  inherited;
  FClocks := TList<TClock>.Create;
  FCenterRow := -1;
end;

destructor TDXFilmEdgeReader.Destroy;
begin
  FClocks.Free;
  inherited;
end;

function TDXFilmEdgeReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

procedure TDXFilmEdgeReader.ResetDecodingState;
begin
  FClocks.Clear;
  FCenterRow := -1;
end;

function TDXFilmEdgeReader.UsesDecodingState: Boolean;
begin
  Result := true;
end;

function TDXFilmEdgeReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TDXFilmEdgeReader.FindClock(x, y: Integer): Integer;
begin
  // a clock (on another row) that starts near (x, y)
  for var i := 0 to FClocks.Count - 1 do
    if (FClocks[i].RowNumber <> y) and FClocks[i].IsCloseTo(x, y,
      FClocks[i].XStart) then
      exit(i);
  Result := -1;
end;

procedure TDXFilmEdgeReader.AddClock(const clock: TClock);
begin
  var i := FindClock(clock.XStart, clock.RowNumber);
  if (i >= 0) then
    FClocks[i] := clock
  else
    FClocks.Add(clock);
end;

function TDXFilmEdgeReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
const
  MIN_DATA_QUIET_ZONE = 0.5;
begin
  Result := nil;
  // the rows are scanned from the center out: the first is the center
  if (FCenterRow < 0) then
    FCenterRow := rowNumber;

  // without rotation only the rows below the center of the image
  var tryRotate := (hints <> nil) and
    hints.ContainsKey(TDecodeHintType.TRY_HARDER) and
    not hints.ContainsKey(TDecodeHintType.TRY_HARDER_WITHOUT_ROTATION);
  if not tryRotate and (rowNumber < FCenterRow - 1) then
  begin
    next := TPatternView.Empty;
    exit;
  end;

  // a pattern that is part of both the clock and the data track (without
  // the first bar); 12 is the minimum size of the data track
  next := FindLeftGuard(next, 4, 10,
    function(const view: TPatternView; spaceInPixel: Integer): Boolean
    begin
      // a very rough check that the 4 bars and spaces are about equal
      var a := view[1];
      var b := view[2];
      var c := view[3];
      var d := view[4];
      var diff := Abs(a - b) + Abs(a - c) + Abs(a - d);
      Result := (diff < a) and (spaceInPixel > a div 2);
    end);
  if not next.IsValid then
    exit;

  // part of a clock track?
  var clock: TClock;
  var isClock := true;
  // (on the frame number versions the number can be close to the clock)
  if MatchPattern(next, CLOCK_PATTERN_FN, 0.5) then
    clock.HasFrameNr := true
  else if MatchPattern(next, CLOCK_PATTERN_NO_FN, 2.0) then
    clock.HasFrameNr := false
  else
    isClock := false;
  if isClock then
  begin
    clock.RowNumber := rowNumber;
    clock.XStart := next.PixelsInFront;
    clock.XStop := next.PixelsTillEnd;
    AddClock(clock);
    next.SkipSymbol;
    exit;
  end;

  // not without a clock track
  if (FClocks.Count = 0) then
    exit;

  if not MatchPattern(next, DATA_START_PATTERN, MIN_DATA_QUIET_ZONE) then
    exit;
  var xStart := next.PixelsInFront;

  // only data tracks next to a clock track
  var ci := FindClock(xStart, rowNumber);
  if (ci < 0) then
    exit;
  clock := FClocks[ci];

  // the start pattern about 5 modules
  if (Abs(next.Sum / clock.ModuleSize - 5) > 1.0) then
    exit;

  // skip the data start pattern; the first signal bar is always white
  next.SkipSymbol;

  // the data bits
  var dataBits: TArray<Boolean> := nil;
  while next.IsValid(1) and (Length(dataBits) < clock.DataLength) do
  begin
    // at most 20 modules (spaces in '96-0/0')
    var modules := Round(next[0] / clock.ModuleSize);
    if (modules < 1) or (modules > 20) then
      exit;
    // an even index is a bar
    var isBar := (next.Index mod 2 = 0);
    for var k := 1 to modules do
      dataBits := dataBits + [isBar];
    next.Shift(1);
  end;

  if (Length(dataBits) <> clock.DataLength) then
    exit;

  next := next.SubView(0, Length(DATA_STOP_PATTERN));
  // the stop pattern at the end of the data track
  if not IsRightGuard(next, DATA_STOP_PATTERN, MIN_DATA_QUIET_ZONE) then
    exit;

  // the separators are always white
  if dataBits[0] or dataBits[8] then
    exit;
  if clock.HasFrameNr then
  begin
    if dataBits[20] or dataBits[22] then
      exit;
  end
  else if dataBits[14] then
    exit;

  // the parity bit
  var signalSum := 0;
  for var i := 0 to Length(dataBits) - 3 do
    Inc(signalSum, Ord(dataBits[i]));
  if (signalSum mod 2 <> Ord(dataBits[Length(dataBits) - 2])) then
    exit;

  var toInt: TFunc<Integer, Integer, Integer> :=
    function(start, count: Integer): Integer
    begin
      Result := 0;
      for var i := start to start + count - 1 do
        Result := (Result shl 1) or Ord(dataBits[i]);
    end;

  // DX 1 (the product number) and DX 2 (the generation number)
  var productNumber := toInt(1, 7);
  if (productNumber = 0) then
    exit;
  var generationNumber := toInt(9, 4);

  // like 115-10/11A: DX 1 115, DX 2 10, frame number 11A
  var txt := IntToStr(productNumber) + '-' + IntToStr(generationNumber);
  if clock.HasFrameNr then
  begin
    txt := txt + '/' + IntToStr(toInt(13, 6));
    if dataBits[19] then
      txt := txt + 'A';
  end;

  var xStop := next.PixelsTillEnd;
  // the data track ends near the clock track
  if not clock.IsCloseTo(xStop, rowNumber, clock.XStop) then
    exit;

  // the clock coordinates from the data track, for the next rows
  clock.XStart := xStart;
  clock.XStop := xStop;
  FClocks[ci] := clock;

  Result := TReadResult.Create(txt, nil,
    [TResultPointHelpers.CreateResultPoint(xStart, rowNumber),
    TResultPointHelpers.CreateResultPoint(xStop, rowNumber)],
    TBarcodeFormat.DX_FILM_EDGE);
  // ISO/IEC 15424: X for 'other barcode'
  Result.SymbologyIdentifier := ']XF';
end;

end.
