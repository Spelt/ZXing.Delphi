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

  * Deutsche Post Leitcode and Identcode for ZXing.Delphi: ITF codes of 14
  * and 12 digits with a check digit of their own, as in zint
  * (2of5inter_based.c). zxing-cpp, ZXing Java and ZXing.Net read them as
  * ITF.
}

unit ZXing.OneD.DeutschePostReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.OneD.ITFReader,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// Decodes the Deutsche Post Leitcode (14 digits) and Identcode (12
  /// digits): ITF with a check digit of their own (weights 4 and 9), that
  /// is checked and stays in the text. Only when asked for: Auto returns
  /// them as ITF.
  /// </summary>
  TDeutschePostReader = class(TITFReader)
  private
    FLeitcode, FIdentcode: Boolean;
    /// <summary>r as Leitcode or Identcode, or nil (r freed).</summary>
    function Filter(r: TReadResult): TReadResult;
  protected
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  public
    constructor Create(leitcode, identcode: Boolean);
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

/// <summary>Whether the last digit of digits is their Deutsche Post check
/// digit (the weights from the right 4, 9, 4, ...).</summary>
function IsDeutschePostCheckDigitValid(const digits: string): Boolean;

implementation

function IsDeutschePostCheckDigitValid(const digits: string): Boolean;
begin
  var n := Length(digits);
  if (n < 2) then
    exit(false);
  var sum := 0;
  var weight := 4;
  for var i := n - 1 downto 1 do
  begin
    Inc(sum, weight * (Ord(digits[i]) - Ord('0')));
    weight := 13 - weight;
  end;
  Result := ((10 - sum mod 10) mod 10 = Ord(digits[n]) - Ord('0'));
end;

{ TDeutschePostReader }

constructor TDeutschePostReader.Create(leitcode, identcode: Boolean);
begin
  inherited Create;
  FLeitcode := leitcode;
  FIdentcode := identcode;
end;

function TDeutschePostReader.Filter(r: TReadResult): TReadResult;
begin
  Result := r;
  if (r = nil) then
    exit;
  var n := Length(r.Text);
  if FLeitcode and (n = 14) and IsDeutschePostCheckDigitValid(r.Text) then
    r.BarcodeFormat := TBarcodeFormat.DP_LEITCODE
  else if FIdentcode and (n = 12) and IsDeutschePostCheckDigitValid(r.Text)
  then
    r.BarcodeFormat := TBarcodeFormat.DP_IDENTCODE
  else
  begin
    r.Free;
    Result := nil;
  end;
end;

function TDeutschePostReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := Filter(inherited decodePattern(rowNumber, next, hints));
end;

function TDeutschePostReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := Filter(inherited decodeRow(rowNumber, row, hints));
end;

end.
