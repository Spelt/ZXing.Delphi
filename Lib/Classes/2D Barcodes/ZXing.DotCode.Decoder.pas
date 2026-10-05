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

  * The DotCode decoder for ZXing.Delphi: zxing-cpp, ZXing Java and ZXing.Net
  * have no reader. The encoding as in zint (dotcode.c, after AIM ISS DotCode
  * Rev 4.0) reversed.
}

unit ZXing.DotCode.Decoder;

interface

uses
  System.SysUtils,
  System.Math;

type
  /// <summary>The text of a DotCode and whether it is GS1 (the default of
  /// DotCode: data that starts with 2 digits, not with an FNC1).</summary>
  TDotCodeResult = record
    Text: string;
    GS1: Boolean;
    /// <summary>The number of codewords corrected.</summary>
    Errors: Integer;
  end;

/// <summary>Decodes the dots of a DotCode of width columns and height rows
/// (row major, true a dot; the dots on the places where column + row is
/// even); false when it can not be decoded.</summary>
function DecodeDotCode(const dots: TArray<Boolean>; width, height: Integer;
  out decoded: TDotCodeResult): Boolean;

/// <summary>The codewords (the mask value first) of the dots of a DotCode
/// (as DecodeDotCode), for tests: -1 for a dot pattern that is none.
/// </summary>
function DotCodeCodewords(const dots: TArray<Boolean>;
  width, height: Integer): TArray<Integer>;

implementation

uses
  ZXing.Common.ECIContent;

const
  GF = 113;
  // Annex C: the dot patterns of the codewords (9 dots, the first the
  // highest bit)
  DOT_PATTERNS: array [0 .. 112] of Word = ($155, $0AB, $0AD, $0B5, $0D5,
    $156, $15A, $16A, $1AA, $0AE, $0B6, $0BA, $0D6, $0DA, $0EA, $12B, $12D,
    $135, $14B, $14D, $153, $159, $165, $169, $195, $1A5, $1A9, $057, $05B,
    $05D, $06B, $06D, $075, $097, $09B, $09D, $0A7, $0B3, $0B9, $0CB, $0CD,
    $0D3, $0D9, $0E5, $0E9, $12E, $136, $13A, $14E, $15C, $166, $16C, $172,
    $174, $196, $19A, $1A6, $1AC, $1B2, $1B4, $1CA, $1D2, $1D4, $05E, $06E,
    $076, $07A, $09E, $0BC, $0CE, $0DC, $0E6, $0EC, $0F2, $0F4, $117, $11B,
    $11D, $127, $133, $139, $147, $163, $171, $18B, $18D, $193, $199, $1A3,
    $1B1, $1C5, $1C9, $1D1, $02F, $037, $03B, $03D, $04F, $067, $073, $079,
    $08F, $0C7, $0E3, $0F1, $11E, $13C, $178, $18E, $19C, $1B8, $1C6, $1CC);
  // the weights of the masks (added per codeword)
  MASK_WEIGHTS: array [0 .. 3] of Integer = (0, 3, 7, 17);

var
  // the codeword of each dot pattern (-1: none)
  PatternValues: array [0 .. 511] of SmallInt;

procedure InitPatternValues;
begin
  for var i := 0 to 511 do
    PatternValues[i] := -1;
  for var v := 0 to 112 do
    PatternValues[DOT_PATTERNS[v]] := v;
end;

function IsCorner(column, row, width, height: Integer): Boolean;
begin
  // top left
  if (column = 0) and (row = 0) then
    exit(true);
  // top right
  if Odd(height) then
  begin
    if (column = width - 2) and (row = 0) or (column = width - 1) and (row = 1)
    then
      exit(true);
  end
  else if (column = width - 1) and (row = 0) then
    exit(true);
  // bottom left
  if Odd(height) then
  begin
    if (column = 0) and (row = height - 1) then
      exit(true);
  end
  else if (column = 0) and (row = height - 2) or (column = 1) and
    (row = height - 1) then
    exit(true);
  // bottom right
  Result := (column = width - 2) and (row = height - 1) or
    (column = width - 1) and (row = height - 2);
end;

/// <summary>The place (row major) of each dot of the dot stream, as zint
/// folds it; nil when the size is none.</summary>
function DotPlaces(width, height: Integer): TArray<Integer>;
begin
  Result := nil;
  if (width < 5) or (height < 5) or not Odd(width + height) then
    exit;
  // the stream position of each place (-1 none), as zint fills it
  var places: TArray<Integer>;
  SetLength(places, width * height);
  for var i := 0 to High(places) do
    places[i] := -1;
  var position := 0;
  if Odd(height) then
  begin
    // horizontal folding
    for var row := 0 to height - 1 do
      for var column := 0 to width - 1 do
        if not Odd(column + row) then
        begin
          if IsCorner(column, row, width, height) then
            places[row * width + column] := -2
          else
          begin
            places[(height - row - 1) * width + column] := position;
            Inc(position);
          end;
        end;
    for var p in [width - 2, height * width - 2, width * 2 - 1,
      (height - 1) * width - 1, 0, (height - 1) * width] do
    begin
      places[p] := position;
      Inc(position);
    end;
  end
  else
  begin
    // vertical folding
    for var column := 0 to width - 1 do
      for var row := 0 to height - 1 do
        if not Odd(column + row) then
        begin
          if IsCorner(column, row, width, height) then
            places[row * width + column] := -2
          else
          begin
            places[row * width + column] := position;
            Inc(position);
          end;
        end;
    for var p in [(height - 1) * width - 1, (height - 2) * width,
      height * width - 2, (height - 1) * width + 1, width - 1, 0] do
    begin
      places[p] := position;
      Inc(position);
    end;
  end;
  SetLength(Result, position);
  for var i := 0 to High(Result) do
    Result[i] := -1;
  for var i := 0 to High(places) do
    if (places[i] >= 0) and (places[i] < position) then
      Result[places[i]] := i;
  for var i := 0 to High(Result) do
    if (Result[i] < 0) then
      exit(nil);
end;

function DotCodeCodewords(const dots: TArray<Boolean>;
  width, height: Integer): TArray<Integer>;
begin
  Result := nil;
  if (PatternValues[0] = 0) then
    InitPatternValues;
  var places := DotPlaces(width, height);
  if (places = nil) or (Length(dots) <> width * height) then
    exit;
  var bits: TArray<Boolean>;
  SetLength(bits, Length(places));
  for var i := 0 to High(places) do
    bits[i] := dots[places[i]];
  // the mask (2 dots), then codewords of 9 dots
  var count := (Length(bits) - 2) div 9;
  SetLength(Result, count + 1);
  Result[0] := 2 * Ord(bits[0]) + Ord(bits[1]);
  for var c := 0 to count - 1 do
  begin
    var pattern := 0;
    for var b := 0 to 8 do
      pattern := (pattern shl 1) or Ord(bits[2 + 9 * c + b]);
    Result[c + 1] := PatternValues[pattern];
  end;
end;

{ Reed-Solomon in GF(113): the roots of the generator 3^1 to 3^k }

function GFMul(a, b: Integer): Integer; inline;
begin
  Result := a * b mod GF;
end;

function GFPow(a, n: Integer): Integer;
begin
  Result := 1;
  for var i := 1 to n do
    Result := Result * a mod GF;
end;

function GFInv(a: Integer): Integer;
begin
  // a^(GF - 2)
  Result := GFPow(a, GF - 2);
end;

/// <summary>Corrects the block (the data first, then the check codewords,
/// the highest power first) with nc check codewords; erasures: the places
/// known to be wrong. Returns the number of corrections, -1 when it can
/// not.</summary>
function CorrectBlock(var block: TArray<Integer>; nc: Integer;
  const erasures: TArray<Integer>): Integer;
begin
  var n := Length(block);
  // (the erasures count as corrections, also when they happen to be right)
  if (Length(erasures) > nc) then
    exit(-1);
  // the syndromes S_j = c(3^j), j = 1 .. nc
  var syndromes: TArray<Integer>;
  SetLength(syndromes, nc);
  var zero := true;
  for var j := 0 to nc - 1 do
  begin
    var x := GFPow(3, j + 1);
    var s := 0;
    for var i := 0 to n - 1 do
      s := (s * x + block[i]) mod GF;
    syndromes[j] := s;
    if (s <> 0) then
      zero := false;
  end;
  if zero then
    exit(Length(erasures));

  // the locator of the erasures, then Berlekamp-Massey (the erasures
  // known): sigma(x) = 1 + ...; the locator of place i (power n-1-i) is
  // 3^(n-1-i)
  var sigma: TArray<Integer> := [1];
  for var e in erasures do
  begin
    var x := GFPow(3, n - 1 - e);
    // sigma * (1 - x z)
    var next: TArray<Integer>;
    SetLength(next, Length(sigma) + 1);
    for var i := 0 to High(next) do
      next[i] := 0;
    for var i := 0 to High(sigma) do
    begin
      next[i] := (next[i] + sigma[i]) mod GF;
      next[i + 1] := (next[i + 1] + GF - GFMul(sigma[i], x)) mod GF;
    end;
    sigma := next;
  end;
  var b: TArray<Integer> := Copy(sigma);
  var l := Length(erasures);
  var m := 1;
  var bb := 1;
  for var k := Length(erasures) to nc - 1 do
  begin
    // the discrepancy
    var d := syndromes[k];
    for var i := 1 to Min(High(sigma), k) do
      d := (d + GFMul(sigma[i], syndromes[k - i])) mod GF;
    if (d = 0) then
    begin
      Inc(m);
      continue;
    end;
    var coef := GFMul(d, GFInv(bb));
    var t := Copy(sigma);
    // sigma := sigma - coef * z^m * b
    if (Length(sigma) < Length(b) + m) then
    begin
      var old := Length(sigma);
      SetLength(sigma, Length(b) + m);
      for var i := old to High(sigma) do
        sigma[i] := 0;
    end;
    for var i := 0 to High(b) do
      sigma[i + m] := (sigma[i + m] + GF - GFMul(coef, b[i])) mod GF;
    if (2 * l <= k + Length(erasures)) then
    begin
      l := k + 1 + Length(erasures) - l;
      b := t;
      bb := d;
      m := 1;
    end
    else
      Inc(m);
  end;
  // (the degree of sigma)
  var degree := High(sigma);
  while (degree > 0) and (sigma[degree] = 0) do
    Dec(degree);
  SetLength(sigma, degree + 1);
  if (2 * (degree - Length(erasures)) + Length(erasures) > nc) then
    exit(-1);

  // the error places: sigma(3^-(n-1-i)) = 0
  var places: TArray<Integer> := [];
  for var i := 0 to n - 1 do
  begin
    var xInv := GFInv(GFPow(3, n - 1 - i));
    var v := 0;
    for var k := degree downto 0 do
      v := (v * xInv + sigma[k]) mod GF;
    if (v = 0) then
      places := places + [i];
  end;
  if (Length(places) <> degree) then
    exit(-1);

  // Forney: omega(z) = S(z) sigma(z) mod z^nc, S(z) = S_1 + S_2 z + ...;
  // e_i = -X_i omega(X_i^-1) / sigma'(X_i^-1) (the first root 3^1)
  var omega: TArray<Integer>;
  SetLength(omega, nc);
  for var i := 0 to nc - 1 do
  begin
    var v := 0;
    for var k := 0 to Min(i, degree) do
      v := (v + GFMul(sigma[k], syndromes[i - k])) mod GF;
    omega[i] := v;
  end;
  for var place in places do
  begin
    var x := GFPow(3, n - 1 - place);
    var xInv := GFInv(x);
    var num := 0;
    for var k := nc - 1 downto 0 do
      num := (num * xInv + omega[k]) mod GF;
    // sigma'(z) = sum k sigma_k z^(k-1)
    var den := 0;
    for var k := degree downto 1 do
      den := (den * xInv + GFMul(k mod GF, sigma[k])) mod GF;
    if (den = 0) then
      exit(-1);
    // (S_j = c(3^j) with j from 1: e = -num / den)
    var e := GFMul(num, GFInv(den));
    block[place] := (block[place] + e) mod GF;
  end;
  // the syndromes must be 0 now
  for var j := 0 to nc - 1 do
  begin
    var x := GFPow(3, j + 1);
    var s := 0;
    for var i := 0 to n - 1 do
      s := (s * x + block[i]) mod GF;
    if (s <> 0) then
      exit(-1);
  end;
  Result := degree;
end;

/// <summary>Corrects the codewords (the mask value, data, the check
/// codewords; interleaved as zint, in one block when single) with nd data
/// codewords; the number of corrections, -1 when they can not be corrected.
/// </summary>
function CorrectCodewords(var codewords: TArray<Integer>;
  nd: Integer; single: Boolean = false): Integer;
begin
  Result := 0;
  var nw := Length(codewords);
  var step := (nw + GF - 2) div (GF - 1);
  if single then
    step := 1;
  for var start := 0 to step - 1 do
  begin
    var nds := (nd - start + step - 1) div step;
    var nws := (nw - start + step - 1) div step;
    var block: TArray<Integer>;
    SetLength(block, nws);
    var erasures: TArray<Integer> := [];
    for var i := 0 to nws - 1 do
    begin
      block[i] := codewords[start + i * step];
      if (block[i] < 0) then
      begin
        block[i] := 0;
        erasures := erasures + [i];
      end;
    end;
    var corrected := CorrectBlock(block, nws - nds, erasures);
    if (corrected < 0) then
      exit(-1);
    // (a small block: one check codeword spare, else noise decodes too)
    var nc := nws - nds;
    if (nc <= 8) and (2 * corrected - Length(erasures) > nc - 2) then
      exit(-1);
    Inc(Result, corrected);
    for var i := 0 to nws - 1 do
      codewords[start + i * step] := block[i];
  end;
end;

/// <summary>The text of the (unmasked) data codewords (Annex F reversed).
/// </summary>
function DecodeMessage(const cws: TArray<Integer>;
  out fnc1First: Boolean): string;
type
  TMode = (mdA, mdB, mdC, mdBinary);
var
  text: string;
  i: Integer;
  // the ECIs: the place in the text, the value
  eciPlaces, eciValues: TArray<Integer>;
  // the binary codewords not decoded yet
  binary: TArray<Integer>;

  function Next: Integer;
  begin
    if (i > High(cws)) then
      raise EArgumentException.Create('DotCode');
    Result := cws[i];
    Inc(i);
  end;

  // a codeword of code set A or B (upper: + 128); false when it is no
  // character
  function CharA(c: Integer; upper: Boolean): Boolean;
  begin
    Result := true;
    if (c < 64) then
      text := text + Chr(c + 32 + 128 * Ord(upper))
    else if (c < 96) then
      text := text + Chr(c - 64 + 128 * Ord(upper))
    else
      Result := false;
  end;

  function CharB(c: Integer; upper: Boolean): Boolean;
  begin
    Result := true;
    if (c < 96) then
      text := text + Chr(c + 32 + 128 * Ord(upper))
    else if not upper and (c <= 100) then
      case c of
        96:
          text := text + #13#10;
        97:
          text := text + #9;
        98:
          text := text + #28;
        99:
          text := text + #29;
        100:
          text := text + #30;
      end
    else
      Result := false;
  end;

  procedure PairsC(count: Integer);
  begin
    for var k := 1 to count do
    begin
      var c := Next;
      if (c > 99) then
        raise EArgumentException.Create('DotCode');
      text := text + Format('%.2d', [c]);
    end;
  end;

  // an ECI from here on in the text
  procedure AddECI(value: Integer);
  begin
    eciPlaces := eciPlaces + [Length(text)];
    eciValues := eciValues + [value];
  end;

  // the ECI after FNC2: below 40 the value, else 3 codewords
  procedure ECI;
  begin
    var c := Next;
    if (c < 40) then
      AddECI(c)
    else
    begin
      var high := c - 40;
      var middle := Next;
      AddECI(high * 12769 + middle * 113 + Next + 40);
    end;
  end;

  // the text of the binary codewords so far
  procedure FlushBinary;
  begin
    // the bytes of the binary codewords: 6 in base 103 are 5 in base
    // 259 (fewer: one less)
    var bytes: TArray<Integer> := [];
    var k := 0;
    while (k < Length(binary)) do
    begin
      var count := Min(6, Length(binary) - k);
      var value: UInt64 := 0;
      for var j := 0 to count - 1 do
        value := value * 103 + UInt64(binary[k + j]);
      var part: TArray<Integer>;
      SetLength(part, count - 1);
      for var j := count - 2 downto 0 do
      begin
        part[j] := value mod 259;
        value := value div 259;
      end;
      bytes := bytes + part;
      Inc(k, count);
    end;
    var j := 0;
    while (j < Length(bytes)) do
    begin
      // 256 to 258: an ECI of 1 to 3 bytes (the highest first)
      if (bytes[j] >= 256) then
      begin
        var n := bytes[j] - 255;
        var value := 0;
        for var b := 1 to n do
          if (j + b <= High(bytes)) then
            value := 256 * value + bytes[j + b];
        AddECI(value);
        Inc(j, n);
      end
      else
        text := text + Chr(bytes[j]);
      Inc(j);
    end;
    binary := [];
  end;


begin
  text := '';
  eciPlaces := [];
  eciValues := [];
  fnc1First := false;
  i := 0;
  var mode := mdC;
  var macro := 0;
  // FNC1 first: no GS1; a macro (after Latch B first)
  if (Length(cws) > 0) and (cws[0] = 107) then
  begin
    fnc1First := true;
    i := 1;
  end
  else if (Length(cws) > 1) and (cws[0] = 106) and (cws[1] >= 97) and
    (cws[1] <= 100) then
  begin
    macro := cws[1];
    i := 2;
    mode := mdB;
    case macro of
      97:
        text := '[)>'#30'05'#29;
      98:
        text := '[)>'#30'06'#29;
      99:
        text := '[)>'#30'12'#29;
      100:
        begin
          text := '[)>'#30;
          var c1 := Next;
          var c2 := Next;
          text := text + Chr(c1 + 32) + Chr(c2 + 32);
        end;
    end;
  end;

  binary := [];
  while (i <= High(cws)) do
  begin
    var c := Next;
    case mode of
      mdC:
        if (c <= 99) then
          text := text + Format('%.2d', [c])
        else
          case c of
            100:
              begin
                // (17) YYMMDD (10)
                text := text + '17';
                PairsC(3);
                text := text + '10';
              end;
            101:
              mode := mdA;
            102 .. 105:
              for var k := 1 to c - 101 do
                if not CharB(Next, false) then
                  raise EArgumentException.Create('DotCode');
            106:
              mode := mdB;
            107:
              text := text + #29;
            108:
              ECI;
            109:
              ; // FNC3: reader initialisation
            110:
              if not CharA(Next, true) then
                raise EArgumentException.Create('DotCode');
            111:
              if not CharB(Next, true) then
                raise EArgumentException.Create('DotCode');
            112:
              mode := mdBinary;
          end;
      mdB:
        if CharB(c, false) then
          // (a character)
        else
          case c of
            101:
              if not CharA(Next, false) then
                raise EArgumentException.Create('DotCode');
            102:
              mode := mdA;
            103 .. 105:
              PairsC(c - 101);
            106:
              mode := mdC;
            107:
              text := text + #29;
            108:
              ECI;
            109:
              ;
            110:
              if not CharA(Next, true) then
                raise EArgumentException.Create('DotCode');
            111:
              if not CharB(Next, true) then
                raise EArgumentException.Create('DotCode');
            112:
              mode := mdBinary;
          end;
      mdA:
        if CharA(c, false) then
          // (a character)
        else
          case c of
            96 .. 101:
              for var k := 1 to c - 95 do
                if not CharB(Next, false) then
                  raise EArgumentException.Create('DotCode');
            102:
              mode := mdB;
            103 .. 105:
              PairsC(c - 101);
            106:
              mode := mdC;
            107:
              text := text + #29;
            108:
              ECI;
            109:
              ;
            110:
              if not CharA(Next, true) then
                raise EArgumentException.Create('DotCode');
            111:
              if not CharB(Next, true) then
                raise EArgumentException.Create('DotCode');
            112:
              mode := mdBinary;
          end;
      mdBinary:
        if (c < 103) then
          binary := binary + [c]
        else
        begin
          FlushBinary;
          case c of
            103 .. 108:
              // shift C for pairs, back to binary
              PairsC(c - 101);
            109:
              mode := mdA;
            110:
              mode := mdB;
            111:
              mode := mdC;
          else
            raise EArgumentException.Create('DotCode');
          end;
        end;
    end;
  end;
  FlushBinary;
  // the macros 05, 06 and 12 end with RS EOT
  if (macro >= 97) and (macro <= 99) then
    text := text + #30#4
  else if (macro = 100) then
    text := text + #4;
  // with ECIs: the text (bytes) in their character sets
  if (Length(eciPlaces) = 0) then
    exit(text);
  var content := TECIContent.Create('ISO-8859-1');
  var e := 0;
  for var p := 1 to Length(text) do
  begin
    while (e < Length(eciPlaces)) and (eciPlaces[e] < p) do
    begin
      content.SwitchEncoding(eciValues[e]);
      Inc(e);
    end;
    content.Append(Byte(Ord(text[p])));
  end;
  Result := content.Text;
end;

function DecodeDotCode(const dots: TArray<Boolean>; width, height: Integer;
  out decoded: TDotCodeResult): Boolean;
begin
  Result := false;
  decoded.Text := '';
  decoded.GS1 := false;
  decoded.Errors := 0;
  var all := DotCodeCodewords(dots, width, height);
  if (all = nil) then
    exit;
  var mask := all[0];
  var count := Length(all) - 1;
  // (more dot patterns that are none than check codewords: no DotCode)
  var invalid := 0;
  for var c := 1 to count do
    if (all[c] < 0) then
      Inc(invalid);
  if (invalid > (count + 6) div 3) then
    exit;
  // the number of data codewords: count = data + 3 + data div 2 (the rest
  // padding dots); a symbol with the corners forced: their dots errors
  for var nd := count downto 1 do
  begin
    var total := nd + 3 + nd div 2;
    if (total > count) then
      continue;
    if (total < count - 1) then
      break;
    // the codewords with the mask value first
    var codewords: TArray<Integer>;
    SetLength(codewords, total + 1);
    codewords[0] := mask;
    for var c := 1 to total do
      codewords[c] := all[c];
    var read := Copy(codewords);
    var corrected := CorrectCodewords(codewords, nd + 1);
    // (some encoders make one block of more than GF - 1 codewords: only
    // when it is right as read)
    if (corrected < 0) and (Length(read) > GF - 1) then
    begin
      codewords := read;
      corrected := CorrectCodewords(codewords, nd + 1, true);
      if (corrected <> 0) then
        corrected := -1;
    end;
    if (corrected < 0) or (codewords[0] > 3) then
      continue;
    // unmasked
    var data: TArray<Integer>;
    SetLength(data, nd);
    for var j := 0 to nd - 1 do
      data[j] := (codewords[j + 1] - j * MASK_WEIGHTS[codewords[0]] mod GF +
        GF) mod GF;
    try
      var fnc1First: Boolean;
      decoded.Text := DecodeMessage(data, fnc1First);
      // GS1: the data starts with 2 digits or (17)..(10) (Code Set C), no
      // FNC1 before them (other data starting with 2 digits has the FNC1)
      decoded.GS1 := not fnc1First and (nd > 0) and (data[0] <= 100);
      decoded.Errors := corrected;
      exit(true);
    except
      on EArgumentException do
        continue;
    end;
  end;
end;

end.
