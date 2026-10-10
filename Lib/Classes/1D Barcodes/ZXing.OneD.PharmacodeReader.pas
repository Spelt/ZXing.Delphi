{
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

  * Pharmacode (Laetus, one track) for ZXing.Delphi. The value as in the
  * PharmaCodeReader of ZXing.Net (zxing-cpp and ZXing Java have none), the
  * bars read like the other 1D readers.
}

unit ZXing.OneD.PharmacodeReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// Decodes Pharmacode (Laetus, one track): 2 to 16 narrow and wide bars
  /// (1:3) with equal spaces (2), the value 3 to 131070. Pharmacode has no
  /// check, almost every row of bars is one, so it is only read when asked
  /// for (not in Auto). Read left to right: the value of a code turned 180
  /// degrees is a different one.
  /// </summary>
  TPharmacodeReader = class(TOneDReader)
  protected
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
    function HasPatternDecoder: Boolean; override;
  public
    /// <summary>There is no decoder of before: nil.</summary>
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

implementation

{ TPharmacodeReader }

function TPharmacodeReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TPharmacodeReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TPharmacodeReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
const
  MIN_BARS = 2;
  MAX_BARS = 16;
  // the quiet zones in spaces (the spec requires 3)
  QUIET_ZONE = 2.5;
begin
  Result := nil;

  // the first bar: with a quiet zone in front (or white up to the edge of
  // the image: an image cut close), the next space about as wide as a
  // narrow bar is (1:2) or a wide one (3:2)
  next := FindLeftGuard(next, 2, 2 * MIN_BARS - 1,
    function(const window: TPatternView; spaceInPixel: Integer): Boolean
    begin
      var bar := window[0];
      var space := window[1];
      Result := (bar >= 0.25 * space) and (bar <= 2.2 * space) and
        ((spaceInPixel >= QUIET_ZONE * space) or
        (spaceInPixel >= window.PixelsInFront));
    end);
  if not next.IsValid then
    exit;
  var xStart := next.PixelsInFront;

  // the bars up to the quiet zone behind them; the spaces about equal
  var space: Double := next[1];
  var bars: TArray<Integer> := [];
  var view := next;
  repeat
    bars := bars + [view[0]];
    if (Length(bars) > MAX_BARS) then
      exit;
    // the space behind the bar: the end (a quiet zone, or white up to the
    // edge of the image), or the next space
    if not view.IsValid(3) or (view[1] >= QUIET_ZONE * space) then
      break;
    if (view[1] < 0.6 * space) or (view[1] > 1.6 * space) then
      exit;
    space := (2 * space + view[1]) / 3;
    if not view.SkipPair or not view.IsValid(1) then
      exit;
  until false;
  if (Length(bars) < MIN_BARS) then
    exit;

  // narrow bars half a space wide, wide bars one and a half; the value:
  // binary 1 followed by a bit per bar (wide 1), minus 1
  var value := 1;
  for var bar in bars do
  begin
    if (bar < 0.25 * space) or (bar > 2.2 * space) then
      exit;
    value := 2 * value + Ord(bar > space);
  end;
  Dec(value);

  next := view.SubView(0, 1);
  var xStop := next.PixelsTillEnd;
  Result := TReadResult.Create(IntToStr(value), nil,
    [TResultPointHelpers.CreateResultPoint(xStart, rowNumber),
    TResultPointHelpers.CreateResultPoint(xStop, rowNumber)],
    TBarcodeFormat.PHARMA_CODE);
end;

end.
