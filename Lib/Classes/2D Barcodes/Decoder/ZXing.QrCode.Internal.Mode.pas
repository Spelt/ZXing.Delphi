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
  * Delphi Implementation by E.Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.Mode;

interface

uses
  System.SysUtils,
  ZXing.QrCode.Internal.Version;

type
  /// <summary>
  /// <p>See ISO 18004:2006, 6.4.1, Tables 2 and 3. This enum encapsulates the various modes in which
  /// data can be encoded to bits in the QR code standard.</p>
  /// </summary>
  TMode = class sealed
  private
    Fbits : Integer;
    Fname : String;
    characterCountBitsForVersions: TArray<Integer>;

    class procedure InitializeClass; static;
    class procedure FinalizeClass; static;
  public
    constructor Create(const characterCountBitsForVersions: TArray<Integer>;
      const bits: Integer; const name: String); overload;

    // No, we can't use an enum here. J2ME doesn't support it.

    /// <summary>
    ///
    /// </summary>
    class var TERMINATOR: TMode;

    /// <summary>
    ///
    /// </summary>
    class var NUMERIC: TMode;

    /// <summary>
    ///
    /// </summary>
    class var ALPHANUMERIC: TMode;

    /// <summary>
    ///
    /// </summary>
    class var STRUCTURED_APPEND: TMode;

    /// <summary>
    ///
    /// </summary>
    class var BYTE: TMode;

    /// <summary>
    ///
    /// </summary>
    class var ECI: TMode; // character counts don't apply

    /// <summary>
    ///
    /// </summary>
    class var KANJI: TMode;

    /// <summary>
    ///
    /// </summary>
    class var FNC1_FIRST_POSITION: TMode;

    /// <summary>
    ///
    /// </summary>
    class var FNC1_SECOND_POSITION: TMode;

    /// <summary>See GBT 18284-2000; "Hanzi" is a transliteration of this mode name.</summary>
    class var HANZI: TMode;

    /// <summary>
    /// Fors the bits.
    /// </summary>
    /// <param name="bits">four bits encoding a QR Code data mode</param>
    /// <returns>
    ///   <see cref="Mode"/> encoded by these bits
    /// </returns>
    /// <exception cref="ArgumentException">if bits do not correspond to a known mode</exception>
    class function forBits(const bits: Integer): TMode; static;

    /// <param name="version">version in question
    /// </param>
    /// <returns> number of bits used, in this QR Code symbol {@link Version}, to encode the
    /// count of characters that will follow encoded in this {@link Mode}
    /// </returns>
    function getCharacterCountBits(const version: TVersion): Integer;

    /// <summary>
    /// Returns a <see cref="System.String"/> that represents this instance.
    /// </summary>
    /// <returns>
    /// A <see cref="System.String"/> that represents this instance.
    /// </returns>
    function ToString: String; override;

    /// <summary>
    /// Gets the name.
    /// </summary>
    property Name: String read Fname;

    /// <summary>
    /// Gets the bits.
    /// </summary>
    property Bits: Integer read Fbits;
  end;

implementation

{ TMode }

constructor TMode.Create(const characterCountBitsForVersions: TArray<Integer>;
  const bits: Integer; const name: String);
begin
  Self.characterCountBitsForVersions := characterCountBitsForVersions;
  Self.Fbits := bits;
  Self.Fname := name
end;

class procedure TMode.InitializeClass;
begin

  TMode.TERMINATOR := TMode.Create(TArray<Integer>.Create(0, 0, 0), 0, 'TERMINATOR');
  TMode.NUMERIC := TMode.Create(TArray<Integer>.Create(10, 12, 14), 1,
    'NUMERIC');
  TMode.ALPHANUMERIC := TMode.Create(TArray<Integer>.Create(9, 11, 13), 2,
    'ALPHANUMERIC');
  TMode.STRUCTURED_APPEND := TMode.Create(TArray<Integer>.Create(0, 0, 0), 3,
    'STRUCTURED_APPEND');
  TMode.BYTE := TMode.Create(TArray<Integer>.Create(8, $10, $10), 4, 'BYTE');
  TMode.ECI := TMode.Create(nil, 7, 'ECI');
  TMode.KANJI := TMode.Create(TArray<Integer>.Create(8, 10, 12), 8, 'KANJI');
  TMode.FNC1_FIRST_POSITION := TMode.Create(nil, 5, 'FNC1_FIRST_POSITION');
  TMode.FNC1_SECOND_POSITION := TMode.Create(nil, 9, 'FNC1_SECOND_POSITION');
  TMode.HANZI := TMode.Create(TArray<Integer>.Create(8, 10, 12), 13, 'HANZI')
end;

class procedure TMode.FinalizeClass;
begin
  TMode.TERMINATOR.characterCountBitsForVersions := nil;
  TMode.NUMERIC.characterCountBitsForVersions := nil;
  TMode.ALPHANUMERIC.characterCountBitsForVersions := nil;
  TMode.STRUCTURED_APPEND.characterCountBitsForVersions := nil;
  TMode.BYTE.characterCountBitsForVersions := nil;
  TMode.ECI.characterCountBitsForVersions := nil;
  TMode.KANJI.characterCountBitsForVersions := nil;
  TMode.FNC1_FIRST_POSITION.characterCountBitsForVersions := nil;
  TMode.FNC1_SECOND_POSITION.characterCountBitsForVersions := nil;
  TMode.HANZI.characterCountBitsForVersions := nil;

  // SetLength(TMode.TERMINATOR.name, 0);
  // SetLength(TMode.NUMERIC.name, 0);
  // SetLength(TMode.ALPHANUMERIC.name, 0);
  // SetLength(TMode.STRUCTURED_APPEND.name, 0);
  // SetLength(TMode.BYTE.name, 0);
  // SetLength(TMode.ECI.name, 0);
  // SetLength(TMode.KANJI.name, 0);
  // SetLength(TMode.FNC1_FIRST_POSITION.name, 0);
  // SetLength(TMode.FNC1_SECOND_POSITION.name, 0);
  // SetLength(TMode.HANZI.name, 0);

  FreeAndNil(TMode.TERMINATOR);
  FreeAndNil(TMode.NUMERIC);
  FreeAndNil(TMode.ALPHANUMERIC);
  FreeAndNil(TMode.STRUCTURED_APPEND);
  FreeAndNil(TMode.BYTE);
  FreeAndNil(TMode.ECI);
  FreeAndNil(TMode.KANJI);
  FreeAndNil(TMode.FNC1_FIRST_POSITION);
  FreeAndNil(TMode.FNC1_SECOND_POSITION);
  FreeAndNil(TMode.HANZI);

end;

class function TMode.forBits(const bits: Integer): TMode;
begin
  case bits of
    $0: Result := TMode.TERMINATOR;
    $1: Result := TMode.NUMERIC;
    $2: Result := TMode.ALPHANUMERIC;
    $3: Result := TMode.STRUCTURED_APPEND;
    $4: Result := TMode.BYTE;
    $5: Result := TMode.FNC1_FIRST_POSITION;
    $7: Result := TMode.ECI;
    $8: Result := TMode.KANJI;
    $9: Result := TMode.FNC1_SECOND_POSITION;
    $D: Result := TMode.HANZI; // 0xD is defined in GBT 18284-2000, may not be supported in foreign country
    else
       raise EArgumentException.Create('Invalid arguments');
  end;
end;

function TMode.getCharacterCountBits(const version: TVersion): Integer;
var
  offset,
  number : Integer;
begin
  if (Self.characterCountBitsForVersions = nil)
  then
     raise EArgumentException.Create('Character count doesn''t apply to this mode');

  number := version.VersionNumber;
  if (number <= 9)
  then
     offset := 0
  else
     if (number <= 26)
     then
        offset := 1
     else
        offset := 2;

  Result := Self.characterCountBitsForVersions[offset];
end;

function TMode.ToString: string;
begin
  Result := Self.name;
end;

initialization
  TMode.InitializeClass;
finalization
  TMode.FinalizeClass;
end.
