{
  * Copyright 2016 ZXing authors
  * Copyright 2017-2026 Axel Waggershauser
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

  * Ported from zxing-cpp (librscpp: decode.h, poly.h, field.h): Reed-Solomon
  * error correction in the prime field GF(929) of PDF417, with erasures
  * (codewords known to be unreadable, which cost only one error correction
  * codeword each).
}

unit ZXing.PDF417.Internal.ErrorCorrection;

interface

/// <summary>
/// Corrects codewords (data and numECC error correction codewords) in place.
/// erasures are the indexes of codewords known to be wrong. Returns false
/// when they can not be corrected; else usedECC is the number of error
/// correction codewords used (2 per error, 1 per erasure).
/// </summary>
function PDF417ReedSolomonDecode(var codewords: TArray<Integer>;
  numECC: Integer; const erasures: TArray<Integer>;
  out usedECC: Integer): Boolean;

implementation

uses
  System.SysUtils,
  System.Math;

const
  FIELD_SIZE = 929;
  GENERATOR = 3;
  FCR = 1; // the first consecutive root

type
  /// <summary>A polynomial over GF(929): the coefficients from the highest
  /// power to the lowest.</summary>
  TPoly = TArray<Integer>;

var
  ExpTable: array [0 .. 2 * FIELD_SIZE - 1] of Integer;
  LogTable: array [0 .. FIELD_SIZE - 1] of Integer;

procedure InitTables;
begin
  var x := 1;
  for var i := 0 to FIELD_SIZE - 1 do
  begin
    ExpTable[i] := x;
    x := (x * GENERATOR) mod FIELD_SIZE;
  end;
  for var i := FIELD_SIZE - 1 to 2 * FIELD_SIZE - 1 do
    ExpTable[i] := ExpTable[i - (FIELD_SIZE - 1)];
  for var i := 0 to FIELD_SIZE - 2 do
    LogTable[ExpTable[i]] := i;
end;

function GFAdd(a, b: Integer): Integer; inline;
begin
  Result := a + b;
  if (Result >= FIELD_SIZE) then
    Dec(Result, FIELD_SIZE);
end;

function GFSub(a, b: Integer): Integer; inline;
begin
  Result := FIELD_SIZE + a - b;
  if (Result >= FIELD_SIZE) then
    Dec(Result, FIELD_SIZE);
end;

function GFNeg(a: Integer): Integer; inline;
begin
  Result := GFSub(0, a);
end;

function GFMul(a, b: Integer): Integer; inline;
begin
  if (a = 0) or (b = 0) then
    Result := 0
  else
    Result := ExpTable[LogTable[a] + LogTable[b]];
end;

function GFInv(a: Integer): Integer; inline;
begin
  Result := ExpTable[FIELD_SIZE - 1 - LogTable[a]];
end;

{ polynomials }

function Deg(const p: TPoly): Integer; inline;
begin
  Result := Length(p) - 1;
end;

/// <summary>The coefficient of x^degree.</summary>
function Coef(const p: TPoly; degree: Integer): Integer; inline;
begin
  Result := p[High(p) - degree];
end;

/// <summary>Removes the leading zero coefficients from start on.</summary>
procedure Normalize(var p: TPoly; start: Integer = 0);
begin
  var i := Min(start, Length(p));
  while (i < Length(p)) and (p[i] = 0) do
    Inc(i);
  Delete(p, 0, i);
end;

/// <summary>The monomial coefficient * x^degree.</summary>
function Monomial(coefficient: Integer; degree: Integer = 0): TPoly;
begin
  if (coefficient = 0) and (degree = 0) then
    exit(nil);
  SetLength(Result, degree + 1);
  for var i := 1 to degree do
    Result[i] := 0;
  Result[0] := coefficient;
end;

procedure PolySub(var p: TPoly; const rhs: TPoly);
begin
  var oldDeg := Deg(p);
  var old := Copy(p);
  SetLength(p, Max(Length(p), Length(rhs)));
  for var i := 0 to Deg(p) do
  begin
    var oldCoef := 0;
    if (i <= oldDeg) then
      oldCoef := old[oldDeg - i];
    if (i <= Deg(rhs)) then
      p[High(p) - i] := GFSub(oldCoef, Coef(rhs, i))
    else
      p[High(p) - i] := oldCoef;
  end;
  Normalize(p);
end;

/// <summary>p * rhs; with trimDeg > 0 only the trimDeg highest
/// coefficients, with trimDeg < 0 only the -trimDeg lowest.</summary>
procedure PolyMul(var p: TPoly; const rhs: TPoly; trimDeg: Integer = 0);
begin
  var lhsSize := Length(p);
  var rhsSize := Length(rhs);
  var productSize := lhsSize + rhsSize - 1;
  var res: TPoly;
  SetLength(res, Max(productSize, 0));
  for var i := 0 to High(res) do
    res[i] := 0;
  for var i := 0 to lhsSize - 1 do
    for var j := 0 to rhsSize - 1 do
      res[i + j] := GFAdd(res[i + j], GFMul(p[i], rhs[j]));
  if (trimDeg = 0) then
    p := res
  else if (trimDeg > 0) then
    p := Copy(res, 0, trimDeg)
  else
    p := Copy(res, Length(res) + trimDeg, -trimDeg);
end;

procedure PolyScale(var p: TPoly; coefficient: Integer);
begin
  if (coefficient = 0) then
    p := nil
  else
    for var i := 0 to High(p) do
      p[i] := GFMul(p[i], coefficient);
end;

/// <summary>p := p mod divisor, with the quotient (expanded synthetic
/// division).</summary>
procedure PolyDiv(var p: TPoly; const divisor: TPoly; out quotient: TPoly);
begin
  if (Deg(p) < Deg(divisor)) then
    SetLength(quotient, 0)
  else
    SetLength(quotient, Length(p) - Deg(divisor));
  var normalizer := GFInv(divisor[0]);
  for var i := 0 to High(quotient) do
  begin
    var ci := p[i];
    if (ci <> 0) then
    begin
      ci := GFMul(ci, normalizer);
      p[i] := ci;
      // the first coefficient of the divisor is only used to normalize
      for var j := 1 to High(divisor) do
        p[i + j] := GFSub(p[i + j], GFMul(divisor[j], ci));
    end;
    quotient[i] := ci;
  end;
  // the normalized remainder
  Normalize(p, Length(quotient));
end;

function Evaluate(const p: TPoly; a: Integer): Integer;
begin
  Result := 0;
  for var c in p do
    Result := GFAdd(GFMul(a, Result), c);
end;

function IsZeroPoly(const p: TPoly): Boolean;
begin
  for var c in p do
    if (c <> 0) then
      exit(false);
  Result := true;
end;

{ decoding }

function ComputeSyndromes(const codewords: TArray<Integer>;
  numECC: Integer): TPoly;
begin
  var roots: TArray<Integer>;
  SetLength(roots, numECC);
  for var i := 0 to numECC - 1 do
    roots[i] := ExpTable[numECC - 1 - i + FCR];
  SetLength(Result, numECC);
  for var i := 0 to numECC - 1 do
    Result[i] := 0;
  for var c in codewords do
    for var i := 0 to numECC - 1 do
      Result[i] := GFAdd(GFMul(roots[i], Result[i]), c);
end;

/// <summary>The error locator and evaluator (extended Euclidean algorithm,
/// the Sugiyama decoder); false when it fails.</summary>
function SugiyamaAlgorithm(syndromes: TPoly; out locator, evaluator: TPoly)
  : Boolean;
begin
  var R := Length(syndromes);
  Normalize(syndromes);
  var rr := syndromes;
  var rLast := Monomial(1, R);
  var tLast: TPoly := nil;
  var t := Monomial(1);
  var q: TPoly;

  // until the degree of r is less than R / 2
  while (Deg(rr) >= R div 2) do
  begin
    var tmp := tLast;
    tLast := t;
    t := tmp;
    tmp := rLast;
    rLast := rr;
    rr := tmp;

    // r := r mod rLast, with the quotient in q
    if (Length(rLast) = 0) or (rLast[0] = 0) then
      exit(false);
    PolyDiv(rr, rLast, q);
    PolyMul(q, tLast);
    PolySub(t, q);
  end;

  if (Length(t) = 0) or (Coef(t, 0) = 0) then
    exit(false);

  var invT0 := GFInv(Coef(t, 0));
  PolyScale(t, invT0);
  PolyScale(rr, invT0);
  locator := t;
  evaluator := rr;
  Result := true;
end;

/// <summary>The roots of the locator (brute force, not Chien's search), as
/// their inverses.</summary>
function FindLocations(const locator: TPoly): TArray<Integer>;
begin
  Result := nil;
  var n := Deg(locator);
  var i := 1;
  while (i < FIELD_SIZE) and (Length(Result) < n) do
  begin
    if (Evaluate(locator, i) = 0) then
      Result := Result + [GFInv(i)];
    Inc(i);
  end;
end;

/// <summary>Forney's formula.</summary>
function FindMagnitudes(const evaluator: TPoly;
  const locations: TArray<Integer>): TArray<Integer>;
begin
  var numErrors := Length(locations);
  SetLength(Result, numErrors);
  for var i := 0 to numErrors - 1 do
  begin
    var xiInverse := GFInv(locations[i]);
    var denom := 1;
    for var j := 0 to numErrors - 1 do
      if (i <> j) then
        denom := GFMul(denom, GFSub(1, GFMul(locations[j], xiInverse)));
    Result[i] := GFMul(Evaluate(evaluator, xiInverse), GFInv(denom));
    if (FCR <> 0) then
      Result[i] := GFMul(Result[i], xiInverse);
  end;
end;

function PDF417ReedSolomonDecode(var codewords: TArray<Integer>;
  numECC: Integer; const erasures: TArray<Integer>;
  out usedECC: Integer): Boolean;
begin
  Result := false;
  usedECC := 0;
  var cwLen := Length(codewords);
  var numErasures := Length(erasures);

  // at most numECC - 2 erasures: with less than 2 parity symbols left, no
  // error can be corrected or even detected (ISO/IEC 24728-2006 5.7.2)
  if (numErasures > numECC - 2) or (numECC <= 0) then
    exit;
  for var e in erasures do
    if (e < 0) or (e >= cwLen) then
      exit;

  // values out of range would index outside the tables
  for var i := 0 to cwLen - 1 do
    codewords[i] := EnsureRange(codewords[i], 0, FIELD_SIZE - 1);

  var syndromes := ComputeSyndromes(codewords, numECC);
  if IsZeroPoly(syndromes) then
    exit(true);

  var oriSyndromes: TPoly := nil;
  var erasureLocator: TPoly := nil;
  // with erasures: the syndromes without their effect
  if (numErasures > 0) then
  begin
    oriSyndromes := Copy(syndromes);
    // erasure locator: the product of (1 - Xi x), Xi = a^(cwLen - 1 - pos)
    erasureLocator := Monomial(1);
    var term: TPoly := [0, 1];
    for var pos in erasures do
    begin
      term[0] := GFNeg(ExpTable[cwLen - 1 - pos]);
      PolyMul(erasureLocator, term);
    end;
    // the highest numECC coefficients of erasureLocator * syndromes, without
    // the highest numErasures (the "error syndromes")
    PolyMul(syndromes, erasureLocator, numECC);
    Delete(syndromes, 0, numErasures);
  end;

  var locator, evaluator: TPoly;
  if not SugiyamaAlgorithm(syndromes, locator, evaluator) then
    exit;

  var numErrors := Deg(locator);
  if (2 * numErrors + numErasures > numECC) then
    exit;

  if (numErasures > 0) then
  begin
    // the erasures merged into the error locator, and the evaluator for it
    PolyMul(locator, erasureLocator);
    evaluator := oriSyndromes;
    PolyMul(evaluator, locator, -numECC);
  end;

  var locations := FindLocations(locator);
  // not as many roots as the degree: most likely more errors than can be
  // corrected
  if (Length(locations) <> numErrors + numErasures) then
    exit;

  var magnitudes := FindMagnitudes(evaluator, locations);
  for var i := 0 to numErrors + numErasures - 1 do
  begin
    var pos := cwLen - 1 - LogTable[locations[i]];
    if (pos < 0) then
      exit;
    codewords[pos] := GFSub(codewords[pos], magnitudes[i]);
  end;

  // the corrected codewords must be a valid codeword (zxing-cpp issue #940)
  if not IsZeroPoly(ComputeSyndromes(codewords, numECC)) then
    exit;

  usedECC := 2 * numErrors + numErasures;
  Result := true;
end;

initialization

InitTables;

end.
