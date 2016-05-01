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

unit ZXing.QrCode.Internal.ErrorCorrectionLevel;

interface

uses SysUtils;

type
  /// <summary>
  /// <p>See ISO 18004:2006, 6.5.1. This enum encapsulates the four error correction levels
  /// defined by the QR code standard.</p>
  /// </summary>
  TErrorCorrectionLevel = class sealed
  private
    Fbits: Integer;
    Fname: string;
    Fordinal_Renamed_Field: Integer;
    class var FOR_BITS: TArray<TErrorCorrectionLevel>;

    class procedure InitalizeClass; static;
    class procedure FinalizeClass; static;
  public
    constructor Create(const ordinal, bits: Integer; const name: String);

    /// <summary>
    /// Fors the bits.
    /// </summary>
    /// <param name="bits">int containing the two bits encoding a QR Code's error correction level</param>
    /// <returns>
    ///   <see cref="ErrorCorrectionLevel"/> representing the encoded error correction level
    /// </returns>
    class function forBits(const bits: Integer): TErrorCorrectionLevel; static;

    /// <summary> L = ~7% correction</summary>
    class var L: TErrorCorrectionLevel;

    /// <summary> M = ~15% correction</summary>
    class var M: TErrorCorrectionLevel;

    /// <summary> Q = ~25% correction</summary>
    class var Q: TErrorCorrectionLevel;

    /// <summary> H = ~30% correction</summary>
    class var H: TErrorCorrectionLevel;

    /// <summary>
    /// Ordinals this instance.
    /// </summary>
    /// <returns></returns>
    function ordinal: Integer;

    /// <summary>
    /// Returns a <see cref="System.String"/> that represents this instance.
    /// </summary>
    /// <returns>
    /// A <see cref="System.String"/> that represents this instance.
    /// </returns>
    function ToString: String; override;

    /// <summary>
    /// Gets the bits.
    /// </summary>
    property Bits: Integer read Fbits;

    /// <summary>
    /// Gets the name.
    /// </summary>
    property Name: String read Fname;
  end;

implementation

{ TErrorCorrectionLevel }

constructor TErrorCorrectionLevel.Create(const ordinal, bits: Integer;
  const name: String);
begin
  Fordinal_Renamed_Field := ordinal;
  Fbits := bits;
  Fname := name
end;

class procedure TErrorCorrectionLevel.InitalizeClass;
begin
  H := TErrorCorrectionLevel.Create(3, 2, 'H');
  L := TErrorCorrectionLevel.Create(0, 1, 'L');
  M := TErrorCorrectionLevel.Create(1, 0, 'M');
  Q := TErrorCorrectionLevel.Create(2, 3, 'Q');

  FOR_BITS := TArray<TErrorCorrectionLevel>.Create(M, L, H, Q);
end;

class procedure TErrorCorrectionLevel.FinalizeClass;
begin
  H.FOR_BITS := nil;
  L.FOR_BITS := nil;
  M.FOR_BITS := nil;
  Q.FOR_BITS := nil;

  FreeAndNil(H);
  FreeAndNil(L);
  FreeAndNil(M);
  FreeAndNil(Q);
  FOR_BITS := nil;
end;

class function TErrorCorrectionLevel.forBits(const bits: Integer): TErrorCorrectionLevel;
begin
  if ((bits < 0) or
      (bits >= Length(TErrorCorrectionLevel.FOR_BITS)))
  then
     raise EArgumentException.Create('Invalid argument');

  Result := TErrorCorrectionLevel.FOR_BITS[bits];
end;

function TErrorCorrectionLevel.ordinal: Integer;
begin
  Result := Fordinal_Renamed_Field;
end;

function TErrorCorrectionLevel.ToString: string;
begin
  Result := Fname
end;

initialization
  TErrorCorrectionLevel.InitalizeClass;
finalization
  TErrorCorrectionLevel.FinalizeClass;
end.
