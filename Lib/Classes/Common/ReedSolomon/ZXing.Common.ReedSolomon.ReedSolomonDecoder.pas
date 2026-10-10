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

  * Original Authors: Sean Owen and William Rucklidge
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.Common.ReedSolomon.ReedSolomonDecoder;

interface

uses
  System.SysUtils,
  System.Math,
  ZXing.Common.ReedSolomon.GenericGF;

type
  /// <summary>
  /// Implements Reed-Solomon decoding, as the name implies: the algorithm of
  /// the Java original (syndromes, the Euclidean algorithm for the error
  /// locator and evaluator, a Chien search and Forney's formula), on plain
  /// arrays of coefficients (the coefficient of x^i at index i) instead of
  /// polynomial objects: a decode allocates a few arrays, not one per
  /// operation.
  /// </summary>
  TReedSolomonDecoder = class sealed
  public
    constructor Create(field: TGenericGF);
  public
    /// <summary>Corrects received in place; false when it can not be
    /// corrected (received may be changed then).</summary>
    function decode(received: TArray<Integer>; twoS: Integer): boolean;
  strict private
    field: TGenericGF;
  end;

implementation

type
  // a polynomial: the coefficient of x^i at index i, at least one
  // coefficient; the zero polynomial is [0]
  TPoly = TArray<Integer>;

/// <summary>Drops the zero coefficients of the highest degrees.</summary>
procedure Trim(var p: TPoly);
begin
  var n := Length(p);
  while (n > 1) and (p[n - 1] = 0) do
    Dec(n);
  if (n < 1) then
  begin
    SetLength(p, 1);
    p[0] := 0;
  end
  else if (n <> Length(p)) then
    SetLength(p, n);
end;

function Degree(const p: TPoly): Integer; inline;
begin
  Result := High(p);
end;

function IsZero(const p: TPoly): Boolean; inline;
begin
  Result := (Length(p) = 1) and (p[0] = 0);
end;

/// <summary>Evaluates p at a (Horner).</summary>
function Evaluate(field: TGenericGF; const p: TPoly; a: Integer): Integer;
begin
  Result := 0;
  for var i := High(p) downto 0 do
    Result := field.multiply(a, Result) xor p[i];
end;

/// <summary>p + q (= p - q: the fields have characteristic 2).</summary>
function Add(const p, q: TPoly): TPoly;
begin
  var n := Max(Length(p), Length(q));
  SetLength(Result, n);
  for var i := 0 to n - 1 do
  begin
    var v := 0;
    if (i < Length(p)) then
      v := p[i];
    if (i < Length(q)) then
      v := v xor q[i];
    Result[i] := v;
  end;
  Trim(Result);
end;

/// <summary>p * q.</summary>
function Multiply(field: TGenericGF; const p, q: TPoly): TPoly;
begin
  if IsZero(p) or IsZero(q) then
  begin
    SetLength(Result, 1);
    Result[0] := 0;
    exit;
  end;
  SetLength(Result, Length(p) + Length(q) - 1);
  for var i := 0 to High(Result) do
    Result[i] := 0;
  for var i := 0 to High(p) do
  begin
    var a := p[i];
    if (a = 0) then
      continue;
    for var j := 0 to High(q) do
      Result[i + j] := Result[i + j] xor field.multiply(a, q[j]);
  end;
  Trim(Result);
end;

/// <summary>p * scalar.</summary>
function Scale(field: TGenericGF; const p: TPoly; scalar: Integer): TPoly;
begin
  if (scalar = 0) then
  begin
    SetLength(Result, 1);
    Result[0] := 0;
    exit;
  end;
  SetLength(Result, Length(p));
  for var i := 0 to High(p) do
    Result[i] := field.multiply(p[i], scalar);
  Trim(Result);
end;

/// <summary>p + q * coefficient * x^degree.</summary>
function AddMonomialProduct(field: TGenericGF; const p, q: TPoly;
  degree, coefficient: Integer): TPoly;
begin
  var n := Max(Length(p), Length(q) + degree);
  SetLength(Result, n);
  for var i := 0 to n - 1 do
  begin
    var v := 0;
    if (i < Length(p)) then
      v := p[i];
    if (i >= degree) and (i - degree <= High(q)) then
      v := v xor field.multiply(q[i - degree], coefficient);
    Result[i] := v;
  end;
  Trim(Result);
end;

{ TReedSolomonDecoder }

constructor TReedSolomonDecoder.Create(field: TGenericGF);
begin
  self.field := field
end;

function TReedSolomonDecoder.decode(received: TArray<Integer>;
  twoS: Integer): boolean;
var
  syndromes, rLast, r, tLast, t, q, sigma, omega: TPoly;
begin
  Result := false;
  var n := Length(received);

  // the received word as a polynomial: the first element is the highest
  // degree
  var poly: TPoly;
  SetLength(poly, n);
  for var i := 0 to n - 1 do
    poly[i] := received[n - 1 - i];
  Trim(poly);

  // the syndromes S_i = R(a^(i + base)) as the coefficients of x^i
  SetLength(syndromes, twoS);
  var noError := true;
  for var i := 0 to twoS - 1 do
  begin
    syndromes[i] := Evaluate(field, poly, field.exp(i + field.GeneratorBase));
    if (syndromes[i] <> 0) then
      noError := false;
  end;
  if noError then
    exit(true);
  Trim(syndromes);

  // the Euclidean algorithm on x^twoS and the syndromes, until the degree of
  // the remainder is less than twoS / 2
  SetLength(rLast, twoS + 1);
  for var i := 0 to twoS - 1 do
    rLast[i] := 0;
  rLast[twoS] := 1;
  r := syndromes;
  if (Degree(rLast) < Degree(r)) then
  begin
    var temp := rLast;
    rLast := r;
    r := temp;
  end;
  var zero: TPoly := [0];
  var one: TPoly := [1];
  tLast := zero;
  t := one;
  while (Degree(r) >= twoS div 2) do
  begin
    var rLastLast := rLast;
    var tLastLast := tLast;
    rLast := r;
    tLast := t;

    // division by zero
    if IsZero(rLast) then
      exit;

    r := rLastLast;
    q := zero;
    var denominatorLeadingTerm := rLast[Degree(rLast)];
    var dltInverse := field.inverse(denominatorLeadingTerm);
    while (Degree(r) >= Degree(rLast)) and not IsZero(r) do
    begin
      var degreeDiff := Degree(r) - Degree(rLast);
      var scale := field.multiply(r[Degree(r)], dltInverse);
      q := AddMonomialProduct(field, q, one, degreeDiff, scale);
      r := AddMonomialProduct(field, r, rLast, degreeDiff, scale);
    end;

    t := Add(Multiply(field, q, tLast), tLastLast);

    // division algorithm failed to reduce the polynomial
    if (Degree(r) >= Degree(rLast)) then
      exit;
  end;

  var sigmaTildeAtZero := t[0];
  if (sigmaTildeAtZero = 0) then
    exit;
  var inverse := field.inverse(sigmaTildeAtZero);
  sigma := Scale(field, t, inverse);
  omega := Scale(field, r, inverse);

  // each error costs 2 error correction codewords (with an odd twoS the
  // locator can have one root too many, like zxing-cpp)
  var numErrors := Degree(sigma);
  if (2 * numErrors > twoS) then
    exit;

  // the error locations: the roots of sigma (a Chien search), inverted
  var errorLocations: TArray<Integer>;
  SetLength(errorLocations, numErrors);
  if (numErrors = 1) then
    errorLocations[0] := sigma[1]
  else
  begin
    var e := 0;
    var i := 1;
    while (i < field.size) and (e < numErrors) do
    begin
      if (Evaluate(field, sigma, i) = 0) then
      begin
        errorLocations[e] := field.inverse(i);
        Inc(e);
      end;
      Inc(i);
    end;
    if (e <> numErrors) then
      exit; // error locator degree does not match number of roots
  end;

  // the error magnitudes (Forney)
  var errorMagnitudes: TArray<Integer>;
  SetLength(errorMagnitudes, numErrors);
  for var i := 0 to numErrors - 1 do
  begin
    var xiInverse := field.inverse(errorLocations[i]);
    var denominator := 1;
    for var j := 0 to numErrors - 1 do
      if (i <> j) then
      begin
        var term := field.multiply(errorLocations[j], xiInverse);
        var termPlus1: Integer;
        if ((term and 1) = 0) then
          termPlus1 := term or 1
        else
          termPlus1 := term and not 1;
        denominator := field.multiply(denominator, termPlus1);
      end;
    errorMagnitudes[i] := field.multiply(Evaluate(field, omega, xiInverse),
      field.inverse(denominator));
    if (field.GeneratorBase <> 0) then
      errorMagnitudes[i] := field.multiply(errorMagnitudes[i], xiInverse);
  end;

  for var i := 0 to numErrors - 1 do
  begin
    var position := n - 1 - field.log(errorLocations[i]);
    if (position < 0) then
      exit; // bad error location
    received[position] := received[position] xor errorMagnitudes[i];
  end;

  // the corrected codeword must be a valid codeword, else there were more
  // errors than could be corrected (zxing-cpp issue #940)
  // (poly was trimmed: the whole word again)
  SetLength(poly, n);
  for var i := 0 to n - 1 do
    poly[i] := received[n - 1 - i];
  Trim(poly);
  for var i := 0 to twoS - 1 do
    if (Evaluate(field, poly, field.exp(i + field.GeneratorBase)) <> 0) then
      exit;

  Result := true;
end;

end.
