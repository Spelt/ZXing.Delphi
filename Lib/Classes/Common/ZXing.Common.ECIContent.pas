{
  * Copyright 2022 Axel Waggershauser
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

  * The decoded bytes of a symbol with the character set (ECI) of each part,
  * like zxing-cpp's Content.
}

unit ZXing.Common.ECIContent;

interface

uses
  System.SysUtils;

type
  TECIContent = record
    Bytes: TBytes;
    // the start of each part with an ECI and its value
    Starts: TArray<Integer>;
    ECIs: TArray<Integer>;
    HasECI: Boolean;
    /// <summary>The character set of the bytes without ECI: '' to guess it
    /// when there is no ECI at all (ISO-8859-1 in front of an ECI).
    /// </summary>
    DefaultEncoding: string;
    class function Create(const defaultEncoding: string = ''): TECIContent;
      static;
    procedure Append(b: Byte); overload;
    procedure Append(const s: string); overload;
    procedure SwitchEncoding(eci: Integer);
    /// <summary>The character set (as ECI value; -1 the default) of the
    /// next part, without an ECI in the symbol (like a Kanji segment).
    /// </summary>
    procedure SwitchCharset(eci: Integer);
    procedure Erase(index, count: Integer);
    /// <summary>Inserts the characters of s (as bytes) at index.</summary>
    procedure Insert(index: Integer; const s: string);
    function IsEmpty: Boolean;
    /// <summary>The bytes as text, each part in its character set.
    /// </summary>
    function Text: string;
  end;

implementation

uses
  System.Math,
  ZXing.CharacterSetECI,
  ZXing.StringUtils;

class function TECIContent.Create(const defaultEncoding: string): TECIContent;
begin
  Result.Bytes := nil;
  Result.Starts := nil;
  Result.ECIs := nil;
  Result.HasECI := false;
  Result.DefaultEncoding := defaultEncoding;
end;

procedure TECIContent.Append(b: Byte);
begin
  var n := Length(Bytes);
  SetLength(Bytes, n + 1);
  Bytes[n] := b;
end;

procedure TECIContent.Append(const s: string);
begin
  for var c in s do
    Append(Byte(Ord(c)));
end;

procedure TECIContent.SwitchEncoding(eci: Integer);
begin
  HasECI := true;
  Starts := Starts + [Length(Bytes)];
  ECIs := ECIs + [eci];
end;

procedure TECIContent.SwitchCharset(eci: Integer);
begin
  // (the same as the last part: nothing to do)
  if (Length(ECIs) > 0) and (ECIs[High(ECIs)] = eci) then
    exit;
  if (Length(ECIs) = 0) and (eci = -1) then
    exit;
  Starts := Starts + [Length(Bytes)];
  ECIs := ECIs + [eci];
end;

procedure TECIContent.Erase(index, count: Integer);
begin
  Delete(Bytes, index, count);
  for var i := 0 to High(Starts) do
    if (Starts[i] > index) then
      Starts[i] := Max(index, Starts[i] - count);
end;

procedure TECIContent.Insert(index: Integer; const s: string);
begin
  var ins: TBytes;
  SetLength(ins, Length(s));
  for var i := 1 to Length(s) do
    ins[i - 1] := Byte(Ord(s[i]));
  System.Insert(ins, Bytes, index);
  for var i := 0 to High(Starts) do
    if (Starts[i] > index) then
      Inc(Starts[i], Length(s));
end;

function TECIContent.IsEmpty: Boolean;
begin
  Result := (Length(Bytes) = 0);
end;

function TECIContent.Text: string;
begin
  Result := '';
  // the parts: the default character set up to the first ECI
  var allStarts: TArray<Integer> := [0] + Starts;
  var allECIs: TArray<Integer> := [-1] + ECIs;
  for var i := 0 to High(allStarts) do
  begin
    var from := allStarts[i];
    var till := Length(Bytes);
    if (i < High(allStarts)) then
      till := allStarts[i + 1];
    if (till <= from) then
      continue;
    var part := Copy(Bytes, from, till - from);
    var encodingName := DefaultEncoding;
    if (encodingName = '') then
      if HasECI then
        encodingName := 'ISO-8859-1'
      else
        encodingName := TStringUtils.guessEncoding(part, nil);
    if (allECIs[i] >= 0) then
    begin
      var charset := TCharacterSetECI.getCharacterSetECIByValue(allECIs[i]);
      if (charset <> nil) then
        encodingName := charset.EncodingName
      else
        encodingName := 'ISO-8859-1';
    end;
    var text: string;
    if TStringUtils.DecodeBytes(part, encodingName, text) then
      Result := Result + text
    else
      // the bytes as Latin-1 characters
      for var b in part do
        Result := Result + Char(b);
  end;
end;

end.
