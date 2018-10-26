unit ZXing.Common.ReedSolomon.GenericGF;

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

// ES: two classes in one unit. This is to avoid circulair reference.

interface

uses SysUtils;

type
  TGenericGF = class;

  IGenericGFPoly = interface
    ['{9E56C626-16C5-4A5A-99B5-84559780EB9D}']

    function get_Degree: Integer;
    function get_isZero: boolean;
    function get_field: TGenericGF;
    function get_coefficients: TArray<Integer>;

    property coefficients: TArray<Integer> read get_coefficients;
    property degree: Integer read get_Degree;
    property isZero: boolean read get_isZero;
    property Field: TGenericGF read get_field;

    function addOrSubtract(other: IGenericGFPoly): IGenericGFPoly;
    function divide(other: IGenericGFPoly): TArray<IGenericGFPoly>;
    function evaluateAt(a: Integer): Integer;
    function getCoefficient(degree: Integer): Integer;

    function multiply(scalar: Integer): IGenericGFPoly; overload;
    function multiply(other: IGenericGFPoly): IGenericGFPoly; overload;

    function multiplyByMonomial(degree: Integer; coefficient: Integer)
      : IGenericGFPoly;

    function ToString: string;
  end;

  /// <summary>
  ///   <p>This class contains utility methods for performing mathematical operations over
  /// the Galois Fields. Operations use a given primitive polynomial in calculations.</p>
  ///   <p>Throughout this package, elements of the GF are represented as an {@code int}
  /// for convenience and speed (but at the cost of memory).
  ///   </p>
  /// </summary>
  TGenericGF = class sealed
  private
    FexpTable: TArray<Integer>;
    FGeneratorBase: Integer;
    FlogTable: TArray<Integer>;
    FZero: IGenericGFPoly;
    FOne: IGenericGFPoly;
    Fprimitive: Integer;
    Fsize: Integer;

    function get_GeneratorBase: Integer;
    function get_One: IGenericGFPoly;
    function get_Zero: IGenericGFPoly;
    function get_Size: Integer;

    class function get_AZTEC_DATA_10: TGenericGF; static;
    class function get_AZTEC_DATA_12: TGenericGF; static;
    class function get_AZTEC_DATA_6: TGenericGF; static;
    class function get_AZTEC_PARAM: TGenericGF; static;
    class function get_QR_CODE_FIELD_256: TGenericGF; static;
    class function get_DATA_MATRIX_FIELD_256: TGenericGF; static;
    class procedure InitializeClass(); static;
    class procedure FinalizeClass(); static;
  public
    class var AZTEC_DATA_10: TGenericGF;
    class var AZTEC_DATA_12: TGenericGF;
    class var AZTEC_DATA_6: TGenericGF;
    class var AZTEC_PARAM: TGenericGF;
    class var DATA_MATRIX_FIELD_256: TGenericGF;
    class var QR_CODE_FIELD_256: TGenericGF;
    class var MAXICODE_FIELD_64: TGenericGF;
    class var AZTEC_DATA_8: TGenericGF;

    constructor Create(primitive, size, genBase: Integer);
    destructor Destroy(); override;

    function ToString: string; override;

    class function addOrSubtract(a: Integer; b: Integer): Integer; static;
    function multiply(a: Integer; b: Integer): Integer;
    function buildMonomial(degree: Integer; coefficient: Integer)
      : IGenericGFPoly;
    function log(a: Integer): Integer;
    function exp(a: Integer): Integer;
    function inverse(a: Integer): Integer;

    property GeneratorBase: Integer read get_GeneratorBase;
    property size: Integer read get_Size;
    property One: IGenericGFPoly read get_One;
    property Zero: IGenericGFPoly read get_Zero;

  end;

  TGenericGFPoly = class sealed(TInterfacedObject, IGenericGFPoly)
  private
    Fcoefficients: TArray<Integer>;
    Ffield: TGenericGF;
  public
    function get_Degree: Integer;
    function get_isZero: boolean;
    function get_field: TGenericGF;
    function get_coefficients: TArray<Integer>;

    property coefficients: TArray<Integer> read get_coefficients;
    property degree: Integer read get_Degree;
    property isZero: boolean read get_isZero;
    property Field: TGenericGF read get_field;

    constructor Create(Field: TGenericGF; coefficients: TArray<Integer>);
    destructor Destroy(); override;
    function addOrSubtract(other: IGenericGFPoly): IGenericGFPoly;
    function divide(other: IGenericGFPoly): TArray<IGenericGFPoly>;
    function evaluateAt(a: Integer): Integer;
    function getCoefficient(degree: Integer): Integer;

    function multiply(scalar: Integer): IGenericGFPoly; overload;
    function multiply(other: IGenericGFPoly): IGenericGFPoly; overload;

    function multiplyByMonomial(degree: Integer; coefficient: Integer)
      : IGenericGFPoly;

    function ToString: string; override;

  end;

implementation

{ TGenericGF }

class procedure TGenericGF.InitializeClass();
begin
  AZTEC_DATA_10 := get_AZTEC_DATA_10();
  AZTEC_DATA_12 := get_AZTEC_DATA_12;
  AZTEC_DATA_6 := get_AZTEC_DATA_6;
  AZTEC_PARAM := get_AZTEC_PARAM;
  DATA_MATRIX_FIELD_256 := get_DATA_MATRIX_FIELD_256;
  QR_CODE_FIELD_256 := get_QR_CODE_FIELD_256;
  MAXICODE_FIELD_64 := get_AZTEC_DATA_6;
  AZTEC_DATA_8 := get_DATA_MATRIX_FIELD_256;
end;

class procedure TGenericGF.FinalizeClass;
begin
  FreeAndNil(AZTEC_DATA_10);
  FreeAndNil(AZTEC_DATA_12);
  FreeAndNil(AZTEC_DATA_6);
  FreeAndNil(AZTEC_PARAM);
  FreeAndNil(DATA_MATRIX_FIELD_256);
  FreeAndNil(QR_CODE_FIELD_256);
  FreeAndNil(MAXICODE_FIELD_64);
  FreeAndNil(AZTEC_DATA_8);
end;

constructor TGenericGF.Create(primitive, size, genBase: Integer);
var
  x, i: Integer;
begin
  self.Fprimitive := primitive;
  Fsize := size;

  FGeneratorBase := genBase;
  self.FexpTable := TArray<Integer>.Create();
  self.FlogTable := TArray<Integer>.Create();

  SetLength(self.FexpTable, size);
  SetLength(self.FlogTable, size);

  x := 1;
  i := 0;

  while ((i < size)) do
  begin
    self.FexpTable[i] := x;
    x := (x shl 1);
    if (x >= size) then
    begin
      x := (x xor primitive);
      x := (x and (size - 1))
    end;
    inc(i)
  end;

  i := 0;
  while ((i < (size - 1))) do
  begin
    self.FlogTable[self.FexpTable[i]] := i;
    inc(i)
  end;

  FZero := TGenericGFPoly.Create(self, TArray<Integer>.Create(0));
  FOne := TGenericGFPoly.Create(self, TArray<Integer>.Create(1));
end;

destructor TGenericGF.Destroy;
begin
  self.FexpTable := nil;
  self.FlogTable := nil;
  inherited;
end;

class function TGenericGF.addOrSubtract(a, b: Integer): Integer;
begin
  Result := (a xor b)
end;

function TGenericGF.buildMonomial(degree, coefficient: Integer): IGenericGFPoly;
var
  coefficients: TArray<Integer>;
begin
  if (degree < 0) then
    raise EArgumentException.Create('Wrong argument');

  if (coefficient = 0) then
  begin
    Result := self.Zero;
    exit
  end;

  coefficients := TArray<Integer>.Create();
  SetLength(coefficients, degree + 1);
  coefficients[0] := coefficient;

  Result := TGenericGFPoly.Create(self, coefficients);

end;

function TGenericGF.exp(a: Integer): Integer;
begin
  Result := self.FexpTable[a]
end;

class function TGenericGF.get_AZTEC_DATA_10: TGenericGF;
begin
  Result := TGenericGF.Create($409, $400, 1);
end;

class function TGenericGF.get_AZTEC_DATA_12: TGenericGF;
begin
  Result := TGenericGF.Create($1069, $1000, 1);
end;

class function TGenericGF.get_AZTEC_DATA_6: TGenericGF;
begin
  Result := TGenericGF.Create($43, $40, 1);
end;

class function TGenericGF.get_AZTEC_PARAM: TGenericGF;
begin
  Result := TGenericGF.Create($13, $10, 1);
end;

class function TGenericGF.get_DATA_MATRIX_FIELD_256: TGenericGF;
begin
  Result := TGenericGF.Create($12D, $100, 1);
end;

function TGenericGF.get_GeneratorBase: Integer;
begin
  Result := FGeneratorBase;
end;

function TGenericGF.get_One: IGenericGFPoly;
begin
  Result := FOne;
end;

class function TGenericGF.get_QR_CODE_FIELD_256: TGenericGF;
begin
  Result := TGenericGF.Create($11D, $100, 0);
end;

function TGenericGF.get_Size: Integer;
begin
  Result := Fsize;
end;

function TGenericGF.get_Zero: IGenericGFPoly;
begin
  Result := FZero;
end;

function TGenericGF.inverse(a: Integer): Integer;
begin
  if (a = 0) then
    raise Exception.Create('Artithmetic Exception');

  Result := self.FexpTable[((self.size - self.FlogTable[a]) - 1)];
end;

function TGenericGF.log(a: Integer): Integer;
begin
  if (a = 0) then
    raise EArgumentException.Create('wrong argument');

  Result := self.FlogTable[a];
end;

function TGenericGF.multiply(a, b: Integer): Integer;
begin
  if ((a = 0) or (b = 0)) then
  begin
    Result := 0;
    exit
  end;

  Result := self.FexpTable
    [((self.FlogTable[a] + self.FlogTable[b]) mod (self.Fsize - 1))];

end;

function TGenericGF.ToString: string;
begin
  Result := 'GF(0x' + Fprimitive.ToString() + ',' + size.ToString() + ')';
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TGenericGFPoly.Create(Field: TGenericGF;
  coefficients: TArray<Integer>);
var
  coefficientsLength, firstNonZero: Integer;

begin

  coefficientsLength := Length(coefficients);
  if (coefficientsLength = 0) then
    raise EArgumentException.Create('Wrong arguments');

  self.Ffield := Field;

  if ((coefficientsLength > 1) and (coefficients[0] = 0)) then
  begin
    firstNonZero := 1;
    while (((firstNonZero < coefficientsLength) and
      (coefficients[firstNonZero] = 0))) do
    begin
      inc(firstNonZero)
    end;

    Fcoefficients := TArray<Integer>.Create();
    if (firstNonZero = coefficientsLength) then
    begin
      SetLength(Fcoefficients, 1);
    end
    else
    begin
      SetLength(Fcoefficients, coefficientsLength - firstNonZero);
      Fcoefficients := Copy(coefficients, firstNonZero, Length(Fcoefficients));
    end
  end
  else
    Fcoefficients := coefficients
end;

destructor TGenericGFPoly.Destroy;
begin
  SetLength(Fcoefficients, 0);
  Fcoefficients := nil;

  Ffield := nil;

  inherited;
end;

function TGenericGFPoly.addOrSubtract(other: IGenericGFPoly): IGenericGFPoly;
var
  lengthDiff, i, y: Integer;
  smallerCoefficients, largerCoefficients, temp, sumDiff: TArray<Integer>;
begin

  if (not self.Ffield.Equals(other.field)) then
    raise EArgumentException.Create
      ('GenericGFPolys do not have same GenericGF field');

  if (self.isZero) then
  begin
    Result := other;
    exit
  end;

  if (other.isZero) then
  begin
    Result := self;
    exit
  end;

  smallerCoefficients := self.coefficients;
  largerCoefficients := other.coefficients;
  if (Length(smallerCoefficients) > Length(largerCoefficients)) then
  begin
    temp := smallerCoefficients;
    smallerCoefficients := largerCoefficients;
    largerCoefficients := temp
  end;

  sumDiff := TArray<Integer>.Create();
  SetLength(sumDiff, Length(largerCoefficients));
  lengthDiff := Length(largerCoefficients) - Length(smallerCoefficients);

  for y := 0 to lengthDiff - 1 do
  begin
    sumDiff[y] := largerCoefficients[y]
  end;

  i := lengthDiff;
  while (i < Length(largerCoefficients)) do
  begin
    sumDiff[i] := TGenericGF.addOrSubtract(smallerCoefficients[(i - lengthDiff)
      ], largerCoefficients[i]);
    inc(i)
  end;

  Result := TGenericGFPoly.Create(self.Ffield, sumDiff);
end;

function TGenericGFPoly.divide(other: IGenericGFPoly): TArray<IGenericGFPoly>;
var
  term, quotient, remainder, iterationQuotient: IGenericGFPoly;
  degreeDifference, denominatorLeadingTerm, scale,
    inverseDenominatorLeadingTerm: Integer;
begin
  if (not self.Ffield.Equals(other.field)) then
    raise EArgumentException.Create
      ('GenericGFPolys do not have same GenericGF field');

  if (other.isZero) then
    raise EArgumentException.Create('Divide by 0');

  quotient := self.Ffield.Zero;
  remainder := self;
  denominatorLeadingTerm := other.getCoefficient(other.degree);
  inverseDenominatorLeadingTerm := self.Ffield.inverse(denominatorLeadingTerm);

  while (((remainder.degree >= other.degree) and not remainder.isZero)) do
  begin
    degreeDifference := (remainder.degree - other.degree);
    scale := self.Ffield.multiply(remainder.getCoefficient(remainder.degree),
      inverseDenominatorLeadingTerm);
    term := other.multiplyByMonomial(degreeDifference, scale);
    iterationQuotient := self.Ffield.buildMonomial(degreeDifference, scale);
    quotient := quotient.addOrSubtract(iterationQuotient);
    remainder := remainder.addOrSubtract(term)
  end;

  Result := TArray<IGenericGFPoly>.Create(quotient, remainder);
end;

function TGenericGFPoly.evaluateAt(a: Integer): Integer;
var
  size, coefficient, i, y, l: Integer;

begin
  Result := 0;
  if (a = 0) then
  begin
    Result := self.getCoefficient(0);
    exit
  end;

  size := Length(self.coefficients);
  if (a = 1) then
  begin

    l := Length(self.coefficients) - 1;
    for y := 0 to l do
    begin
      coefficient := coefficients[y];
      Result := TGenericGF.addOrSubtract(Result, coefficient)
    end;

    exit
  end;

  Result := self.coefficients[0];
  i := 1;
  while ((i < size)) do
  begin
    Result := TGenericGF.addOrSubtract(self.Ffield.multiply(a, Result),
      self.coefficients[i]);
    inc(i)
  end;

end;

function TGenericGFPoly.getCoefficient(degree: Integer): Integer;
begin
  Result := Fcoefficients[Length(Fcoefficients) - 1 - degree]
end;

function TGenericGFPoly.get_coefficients: TArray<Integer>;
begin
  Result := fcoefficients;
end;

function TGenericGFPoly.get_Degree: Integer;
begin
  Result := Length(Fcoefficients) - 1;
end;

function TGenericGFPoly.get_field: TGenericGF;
begin
  Result := FField;
end;

function TGenericGFPoly.get_isZero: boolean;
begin
  Result := Fcoefficients[0] = 0;
end;

function TGenericGFPoly.multiply(other: IGenericGFPoly): IGenericGFPoly;
var
  product, aCoefficients, bCoefficients: TArray<Integer>;
  aLength, bLength, i, aCoeff, j: Integer;
begin

  if (not self.Ffield.Equals(other.field)) then
    raise EArgumentException.Create
      ('GenericGFPolys do not have same GenericGF field');

  if (self.isZero or other.isZero) then
  begin
    Result := self.Ffield.Zero;
    exit
  end;

  aCoefficients := self.coefficients;
  aLength := Length(aCoefficients);
  bCoefficients := other.coefficients;
  bLength := Length(bCoefficients);
  product := TArray<Integer>.Create();
  SetLength(product, (aLength + bLength) - 1);
  i := 0;

  while ((i < aLength)) do
  begin
    aCoeff := aCoefficients[i];
    j := 0;
    while ((j < bLength)) do
    begin
      product[(i + j)] := TGenericGF.addOrSubtract(product[(i + j)],
        self.Ffield.multiply(aCoeff, bCoefficients[j]));
      inc(j)
    end;
    inc(i)
  end;

  Result := TGenericGFPoly.Create(self.Ffield, product);

end;

function TGenericGFPoly.multiply(scalar: Integer): IGenericGFPoly;
var
  product: TArray<Integer>;
  size, i: Integer;
begin

  if (scalar = 0) then
  begin
    Result := self.Ffield.Zero;
    exit
  end;

  if (scalar = 1) then
  begin
    Result := self;
    exit
  end;

  size := Length(self.coefficients);
  product := TArray<Integer>.Create();
  SetLength(product, size);
  i := 0;
  while ((i < size)) do
  begin
    product[i] := self.Ffield.multiply(self.coefficients[i], scalar);
    inc(i)
  end;

  Result := TGenericGFPoly.Create(self.Ffield, product);

end;

function TGenericGFPoly.multiplyByMonomial(degree, coefficient: Integer)
  : IGenericGFPoly;
var
  product: TArray<Integer>;
  size, i: Integer;
begin
  if (degree < 0) then
    raise EArgumentException.Create('Wrong arguments');

  if (coefficient = 0) then
  begin
    Result := self.Ffield.Zero;
    exit
  end;

  size := Length(self.coefficients);
  product := TArray<Integer>.Create();
  SetLength(product, size + degree);
  i := 0;

  while ((i < size)) do
  begin
    product[i] := self.Ffield.multiply(self.coefficients[i], coefficient);
    inc(i)
  end;

  Result := TGenericGFPoly.Create(self.Ffield, product);
end;

function TGenericGFPoly.ToString: string;
var
  lResult: TStringBuilder;
  degree, coefficient, alphaPower: Integer;

begin
  lResult := TStringBuilder.Create((8 * self.degree));
  degree := self.degree;

  while ((degree >= 0)) do
  begin

    coefficient := self.getCoefficient(degree);
    if (coefficient <> 0) then
    begin

      if (coefficient < 0) then
      begin
        lResult.Append(' - ');
        coefficient := -1 * coefficient
      end
      else if (Result.Length > 0) then
        lResult.Append(' + ');

      if ((degree = 0) or (coefficient <> 1)) then
      begin
        alphaPower := self.Ffield.log(coefficient);
        if (alphaPower = 0) then
          lResult.Append('1')
        else if (alphaPower = 1) then
          lResult.Append('a')
        else
        begin
          lResult.Append('a^');
          lResult.Append(alphaPower)
        end
      end;

      if (degree <> 0) then
        if (degree = 1) then
          lResult.Append('x')
        else
        begin
          lResult.Append('x^');
          lResult.Append(degree)
        end
    end;
    dec(degree)
  end;

  Result := lResult.ToString();
end;

initialization
  TGenericGF.InitializeClass();
finalization
  TGenericGF.FinalizeClass();
end.
