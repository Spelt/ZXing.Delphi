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
  // Hint: ZXing.Common.ReedSolomon.GenericGFPoly is implemented in GenericGF as second class
  ZXing.Common.ReedSolomon.GenericGF;

type
  /// <summary>
  /// Implements Reed-Solomon encoding, as the name implies.
  /// </summary>
  TReedSolomonDecoder = class sealed
  public
    constructor Create(field: TGenericGF);
  public
    function decode(received: TArray<Integer>; twoS: Integer): boolean;
  strict private
    function findErrorLocations(errorLocator: IGenericGFPoly): TArray<Integer>;
  strict private
    function findErrorMagnitudes(errorEvaluator: IGenericGFPoly;
      errorLocations: TArray<Integer>): TArray<Integer>;
  private
    function runEuclideanAlgorithm(a: IGenericGFPoly; b: IGenericGFPoly;
      pR: Integer): TArray<IGenericGFPoly>;

  strict private
    field: TGenericGF;
  end;

implementation

{ TReedSolomonDecoder }

constructor TReedSolomonDecoder.Create(field: TGenericGF);
begin
  self.field := field
end;
	  
function TReedSolomonDecoder.decode(received: TArray<Integer>;
  twoS: Integer): boolean;
var
  i, eval, position: Integer;
  poly, syndrome, sigma, omega: IGenericGFPoly;
  sigmaOmega: TArray<IGenericGFPoly>;
  errorLocations: TArray<Integer>;

  syndromeCoefficients, errorMagnitudes: TArray<Integer>;
  noError: boolean;
begin
  poly := TGenericGFPoly.Create(self.field, received);

  syndromeCoefficients := TArray<Integer>.Create();
  SetLength(syndromeCoefficients, twoS);

  try

    noError := true;
    i := 0;
    while ((i < twoS)) do
    begin
      eval := poly.evaluateAt(self.field.exp((i + self.field.GeneratorBase)));
      syndromeCoefficients[((Length(syndromeCoefficients) - 1) - i)] := eval;
      if (eval <> 0) then
        noError := false;
      inc(i)
    end;

    if (noError) then
    begin
      Result := true;
      Exit;
    end;

    syndrome := TGenericGFPoly.Create(self.field, syndromeCoefficients);
    sigmaOmega := self.runEuclideanAlgorithm(self.field.buildMonomial(twoS, 1),
      syndrome, twoS);

    if (sigmaOmega = nil) then
    begin
      Result := false;
      Exit
    end;

    sigma := sigmaOmega[0];
    errorLocations := self.findErrorLocations(sigma);
    if (errorLocations = nil) then
    begin
      Result := false;
      Exit
    end;

    omega := sigmaOmega[1];
    errorMagnitudes := self.findErrorMagnitudes(omega, errorLocations);
    i := 0;
    while (i < Length(errorLocations)) do
    begin
      position := ((Length(received) - 1) - self.field.log(errorLocations[i]));
      if (position < 0) then
      begin
        Result := false;
        Exit
      end;
      received[position] := TGenericGF.addOrSubtract(received[position],
        errorMagnitudes[i]);
      inc(i)
    end;

  finally
    SetLength(sigmaOmega, 0);
    SetLength(syndromeCoefficients, 0);
  end;

  Result := true;

end;

function TReedSolomonDecoder.findErrorLocations(errorLocator: IGenericGFPoly)
  : TArray<Integer>;
var
  numErrors, e, i: Integer;

begin
  numErrors := errorLocator.Degree;

  if (numErrors = 1) then
  begin
    Result := TArray<Integer>.Create(errorLocator.getCoefficient(1));
    Exit
  end;

  Result := TArray<Integer>.Create();
  SetLength(Result, numErrors);

  e := 0;
  i := 1;

  while (((i < self.field.Size) and (e < numErrors))) do
  begin

    if (errorLocator.evaluateAt(i) = 0) then
    begin
      Result[e] := self.field.inverse(i);
      inc(e)
    end;

    inc(i);

  end;

  if (e <> numErrors) then
  begin
    Result := nil;
  end;

end;

function TReedSolomonDecoder.findErrorMagnitudes(errorEvaluator: IGenericGFPoly;
  errorLocations: TArray<Integer>): TArray<Integer>;
var
  s, i, xiInverse, denominator, j, term, termPlus1: Integer;

begin
  s := Length(errorLocations);
  Result := TArray<Integer>.Create();
  SetLength(Result, s);

  i := 0;
  while ((i < s)) do
  begin
    xiInverse := self.field.inverse(errorLocations[i]);
    denominator := 1;
    j := 0;
    while ((j < s)) do
    begin
      if (i <> j) then
      begin
        term := self.field.multiply(errorLocations[j], xiInverse);

        if ((term and 1) = 0) then
        begin
          termPlus1 := (term or 1)
        end
        else
        begin
          termPlus1 := (term and (not 1));
        end;

        denominator := self.field.multiply(denominator, termPlus1)

      end;
      inc(j)
    end;

    Result[i] := self.field.multiply(errorEvaluator.evaluateAt(xiInverse),
      self.field.inverse(denominator));

    if (self.field.GeneratorBase <> 0) then
      Result[i] := self.field.multiply(Result[i], xiInverse);

    inc(i)
  end;

end;

function TReedSolomonDecoder.runEuclideanAlgorithm(a, b: IGenericGFPoly;
  pR: Integer): TArray<IGenericGFPoly>;
var
  temp, rLastLast, tLastLast, rLast, tLast, t, q, sigma, omega,
    R: IGenericGFPoly;
  denominatorLeadingTerm, dltInverse, degreeDiff, scale, inverse,
    sigmaTildeAtZero: Integer;
begin

  if (a.Degree < b.Degree) then
  begin
    temp := a;
    a := b;
    b := temp
  end;

  rLast := a;
  R := b;
  tLast := self.field.Zero;
  t := self.field.One;

  while ((R.Degree >= (pR div 2))) do
  begin
    rLastLast := rLast;
    tLastLast := tLast;
    rLast := R;
    tLast := t;

    if (rLast.isZero) then
    begin
      Result := nil;
      Exit
    end;

    R := rLastLast;
    q := self.field.Zero;
    denominatorLeadingTerm := rLast.getCoefficient(rLast.Degree);
    dltInverse := self.field.inverse(denominatorLeadingTerm);

    while (((R.Degree >= rLast.Degree) and not R.isZero)) do
    begin
      degreeDiff := (R.Degree - rLast.Degree);
      scale := self.field.multiply(R.getCoefficient(R.Degree), dltInverse);
      q := q.addOrSubtract(self.field.buildMonomial(degreeDiff, scale));
      R := R.addOrSubtract(rLast.multiplyByMonomial(degreeDiff, scale))
    end;

    t := q.multiply(tLast).addOrSubtract(tLastLast);

    if (R.Degree >= rLast.Degree) then
    begin
      Result := nil;
      Exit
    end
  end;

  sigmaTildeAtZero := t.getCoefficient(0);
  if (sigmaTildeAtZero = 0) then
  begin
    Result := nil;
    Exit
  end;

  inverse := self.field.inverse(sigmaTildeAtZero);
  sigma := t.multiply(inverse);
  omega := R.multiply(inverse);

  Result := TArray<IGenericGFPoly>.Create(sigma, omega);

end;

end.
