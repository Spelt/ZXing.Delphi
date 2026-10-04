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

  * GS1 Composite symbols for ZXing.Delphi: the 2D component (CC-A, CC-B,
  * CC-C) above (upside down: below) a linear component (EAN/UPC, GS1
  * DataBar, GS1-128). zxing-cpp, ZXing Java and ZXing.Net have none.
}

unit ZXing.Composite.Linker;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.BinaryBitmap;

/// <summary>Whether r can be the linear component of a GS1 Composite:
/// EAN/UPC, GS1 DataBar or GS1-128.</summary>
function IsCompositeLinear(r: TReadResult): Boolean;

/// <summary>The GS1 text of the 2D component of a GS1 Composite whose
/// linear component is linear; '' when there is none.</summary>
function FindCompositeComponent(const image: TBinaryBitmap;
  linear: TReadResult; hints: TDictionary<TDecodeHintType, TObject>): string;

/// <summary>The GS1 Composite of linear (freed) and the text of its 2D
/// component: the text of both with '|' between them.</summary>
function MakeComposite(linear: TReadResult; const component: string)
  : TReadResult;

implementation

uses
  ZXing.ResultPoint,
  ZXing.Reader,
  ZXing.PDF417.PDF417Reader,
  ZXing.PDF417.MicroPDF417Reader,
  ZXing.Composite.CCA;

function IsCompositeLinear(r: TReadResult): Boolean;
begin
  Result := (r <> nil) and ((r.BarcodeFormat = TBarcodeFormat.EAN_13) or
    (r.BarcodeFormat = TBarcodeFormat.EAN_8) or
    (r.BarcodeFormat = TBarcodeFormat.UPC_A) or
    (r.BarcodeFormat = TBarcodeFormat.UPC_E) or
    (r.BarcodeFormat = TBarcodeFormat.RSS_14) or
    (r.BarcodeFormat = TBarcodeFormat.RSS_LIMITED) or
    (r.BarcodeFormat = TBarcodeFormat.RSS_EXPANDED) or
    (r.BarcodeFormat = TBarcodeFormat.CODE_128) and
    (r.SymbologyIdentifier = ']C1'));
end;

/// <summary>The 2D component (]e1) of the results that overlaps x1 to x2;
/// '' when there is none.</summary>
function ComponentOf(results: TList<TReadResult>; x1, x2: Double): string;
begin
  Result := '';
  for var r in results do
  begin
    if (r.SymbologyIdentifier <> ']e1') then
      continue;
    var minX := MaxDouble;
    var maxX := -MaxDouble;
    for var p in r.ResultPoints do
      if (p <> nil) then
      begin
        minX := Min(minX, p.X);
        maxX := Max(maxX, p.X);
      end;
    if (maxX >= x1) and (minX <= x2) then
      exit(r.Text);
  end;
end;

function FindCompositeComponent(const image: TBinaryBitmap;
  linear: TReadResult; hints: TDictionary<TDecodeHintType, TObject>): string;
begin
  Result := '';
  if not IsCompositeLinear(linear) or (image = nil) or
    (image.BlackMatrix = nil) then
    exit;
  var minX := MaxDouble;
  var maxX := -MaxDouble;
  var minY := MaxDouble;
  var maxY := -MaxDouble;
  for var p in linear.ResultPoints do
    if (p <> nil) then
    begin
      minX := Min(minX, p.X);
      maxX := Max(maxX, p.X);
      minY := Min(minY, p.Y);
      maxY := Max(maxY, p.Y);
    end;
  var width := maxX - minX;
  if (width < 20) then
    exit;
  var matrix := image.BlackMatrix;
  // the areas above the linear component and (upside down) below it
  var areas: array [0 .. 1, 0 .. 1] of Integer;
  areas[0, 0] := Max(Round(minY - 1.5 * width), 0);
  areas[0, 1] := Round(minY);
  areas[1, 0] := Round(maxY);
  areas[1, 1] := Min(Round(maxY + 1.5 * width), matrix.Height - 1);
  var left := Round(minX - width / 4);
  var right := Round(maxX + width / 4);

  // CC-A
  for var a := 0 to 1 do
  begin
    Result := ReadCCA(matrix, left, areas[a, 0], right, areas[a, 1]);
    if (Result <> '') then
      exit;
  end;

  var results := TList<TReadResult>.Create;
  try
    // CC-B (a MicroPDF417, rows of 2 modules)
    for var a := 0 to 1 do
    begin
      var micro := TMicroPDF417Reader.Create;
      var reference: IMultipleReader := micro;
      micro.FineTop := areas[a, 0];
      micro.FineBottom := areas[a, 1];
      reference.decodeMultiple(image, hints, results, 4);
      Result := ComponentOf(results, left, right);
      if (Result <> '') then
        exit;
    end;
    // CC-C (a PDF417, only with GS1-128)
    if (linear.BarcodeFormat = TBarcodeFormat.CODE_128) then
    begin
      var reader: IMultipleReader := TPDF417Reader.Create;
      reader.decodeMultiple(image, hints, results, 4);
      Result := ComponentOf(results, left, right);
    end;
  finally
    for var r in results do
      r.Free;
    results.Free;
  end;
end;

function MakeComposite(linear: TReadResult; const component: string)
  : TReadResult;
begin
  Result := TReadResult.Create(linear.Text + '|' + component, nil,
    linear.ResultPoints, TBarcodeFormat.GS1_COMPOSITE);
  Result.SymbologyIdentifier := linear.SymbologyIdentifier;
  linear.Free;
end;

end.
