unit OneDFullAsciiTest;

{
  * Tests for Code 39 (check digit, Full ASCII) and Code 93 (Full ASCII).
  * The rows are built from the character patterns of the readers, so these
  * tests need no images: a bar or space of n modules becomes n bits.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TOneDFullAsciiTest = class(TObject)
  public
    [Test]
    [TestCase('plain', 'CODE39,false,false,CODE39')]
    [TestCase('check digit', 'CODE39,true,false,CODE39')]
    [TestCase('check digit $/+%', 'A$B/C+D%,true,false,A$B/C+D%')]
    [TestCase('full ASCII lower case', '+A+B,false,true,ab')]
    [TestCase('full ASCII %', '%F%K%O%P%V%W,false,true,;[_{@`')]
    [TestCase('full ASCII /', '/A/Z,false,true,!:')]
    procedure Code39(const content: string; withCheckDigit, extended: Boolean;
      const expected: string);

    [Test]
    procedure Code39FullAsciiControlCharacters;

    [Test]
    [TestCase('plain', 'CODE93,CODE93')]
    [TestCase('lower case', 'dAdB,ab')]
    [TestCase('% characters', 'bFbKbV,;[@')]
    [TestCase('mixed', 'AdBbFC,Ab;C')]
    [TestCase('/ characters', 'cAcZ,!:')]
    procedure Code93(const content, expected: string);

    [Test]
    procedure Code93FullAsciiControlCharacters;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.Common.BitArray,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.OneD.Code39Reader,
  ZXing.OneD.Code93Reader;

const
  CODE39_ALPHABET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%';
  CODE39_PATTERNS: array [0 .. 43] of Integer = ($034, $121, $061, $160, $031,
    $130, $070, $025, $124, $064, $109, $049, $148, $019, $118, $058, $00D,
    $10C, $04C, $01C, $103, $043, $142, $013, $112, $052, $007, $106, $046,
    $016, $181, $0C1, $1C0, $091, $190, $0D0, $085, $184, $0C4, $094, $0A8,
    $0A2, $08A, $02A);
  CODE39_CHECK_DIGITS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%';
  CODE93_ALPHABET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%abcd*';
  CODE93_PATTERNS: array [0 .. 47] of Integer = ($114, $148, $144, $142, $128,
    $124, $122, $150, $112, $10A, $1A8, $1A4, $1A2, $194, $192, $18A, $168,
    $164, $162, $134, $11A, $158, $14C, $146, $12C, $116, $1B4, $1B2, $1AC,
    $1A6, $196, $19A, $16C, $166, $136, $13A, $12E, $1D4, $1D2, $1CA, $16E,
    $176, $1AE, $126, $1DA, $1D6, $132, $15E);
  QUIET_ZONE = 40;

type
  TRowBuilder = class
  private
    FBits: TList<Boolean>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(black: Boolean; count: Integer);
    function Row: IBitArray;
  end;

constructor TRowBuilder.Create;
begin
  inherited;
  FBits := TList<Boolean>.Create;
end;

destructor TRowBuilder.Destroy;
begin
  FBits.Free;
  inherited;
end;

procedure TRowBuilder.Add(black: Boolean; count: Integer);
var
  i: Integer;
begin
  for i := 1 to count do
    FBits.Add(black);
end;

function TRowBuilder.Row: IBitArray;
var
  i: Integer;
begin
  Result := TBitArrayHelpers.CreateBitArray(FBits.Count);
  for i := 0 to FBits.Count - 1 do
    if FBits[i] then
      Result[i] := true;
end;

/// <summary>Code 39 row: 9 elements per character (bar first), narrow 2 and
/// wide 6 modules, a narrow space between characters.</summary>
function Code39Row(const content: string; withCheckDigit: Boolean): IBitArray;
var
  text: string;
  c: Char;
  i, pattern, total: Integer;
  builder: TRowBuilder;
begin
  text := content;
  if withCheckDigit then
  begin
    total := 0;
    for c in content do
      Inc(total, CODE39_CHECK_DIGITS.IndexOf(c));
    text := text + CODE39_CHECK_DIGITS.Chars[total mod 43];
  end;
  text := '*' + text + '*';

  builder := TRowBuilder.Create;
  try
    builder.Add(false, QUIET_ZONE);
    for c in text do
    begin
      pattern := CODE39_PATTERNS[CODE39_ALPHABET.IndexOf(c)];
      for i := 0 to 8 do
        if ((pattern and (1 shl (8 - i))) <> 0) then
          builder.Add(not Odd(i), 6)
        else
          builder.Add(not Odd(i), 2);
      builder.Add(false, 2);
    end;
    builder.Add(false, QUIET_ZONE);
    Result := builder.Row;
  finally
    builder.Free;
  end;
end;

/// <summary>Code 93 row: 9 modules of 3 bits per character, the two check
/// characters C and K, and a termination bar.</summary>
function Code93Row(const content: string): IBitArray;
var
  text: string;
  builder: TRowBuilder;
  c: Char;

  procedure addCheckCharacter(maxWeight: Integer);
  var
    i, weight, total: Integer;
  begin
    total := 0;
    weight := 1;
    for i := Length(text) downto 1 do
    begin
      Inc(total, weight * CODE93_ALPHABET.IndexOf(text[i]));
      Inc(weight);
      if (weight > maxWeight) then
        weight := 1;
    end;
    text := text + CODE93_ALPHABET.Chars[total mod 47];
  end;

  procedure addCharacter(ch: Char);
  var
    bit, pattern: Integer;
  begin
    pattern := CODE93_PATTERNS[CODE93_ALPHABET.IndexOf(ch)];
    for bit := 8 downto 0 do
      builder.Add((pattern and (1 shl bit)) <> 0, 3);
  end;

begin
  text := content;
  addCheckCharacter(20);
  addCheckCharacter(15);

  builder := TRowBuilder.Create;
  try
    builder.Add(false, QUIET_ZONE);
    addCharacter('*');
    for c in text do
      addCharacter(c);
    addCharacter('*');
    builder.Add(true, 3);
    builder.Add(false, QUIET_ZONE);
    Result := builder.Row;
  finally
    builder.Free;
  end;
end;

function DecodeCode39(const content: string;
  withCheckDigit, extended: Boolean): string;
var
  reader: TCode39Reader;
  hints: TDictionary<TDecodeHintType, TObject>;
  r: TReadResult;
begin
  reader := TCode39Reader.Create(withCheckDigit, extended);
  hints := TDictionary<TDecodeHintType, TObject>.Create;
  try
    r := reader.decodeRow(0, Code39Row(content, withCheckDigit), hints);
    try
      Assert.IsNotNull(r, 'Nil result for ' + content);
      Result := r.Text;
    finally
      r.Free;
    end;
  finally
    hints.Free;
    reader.Free;
  end;
end;

function DecodeCode93(const content: string): string;
var
  reader: TCode93Reader;
  hints: TDictionary<TDecodeHintType, TObject>;
  r: TReadResult;
begin
  reader := TCode93Reader.Create;
  hints := TDictionary<TDecodeHintType, TObject>.Create;
  try
    r := reader.decodeRow(0, Code93Row(content), hints);
    try
      Assert.IsNotNull(r, 'Nil result for ' + content);
      Result := r.Text;
    finally
      r.Free;
    end;
  finally
    hints.Free;
    reader.Free;
  end;
end;

{ TOneDFullAsciiTest }

procedure TOneDFullAsciiTest.Code39(const content: string;
  withCheckDigit, extended: Boolean; const expected: string);
begin
  Assert.AreEqual(expected, DecodeCode39(content, withCheckDigit, extended));
end;

procedure TOneDFullAsciiTest.Code39FullAsciiControlCharacters;
begin
  // $A = SOH, %A = ESC, %T and %X..%Z = DEL, %U = NUL
  Assert.AreEqual(#1#27#127#127#0, DecodeCode39('$A%A%T%X%U', false, true));
end;

procedure TOneDFullAsciiTest.Code93(const content, expected: string);
begin
  Assert.AreEqual(expected, DecodeCode93(content));
end;

procedure TOneDFullAsciiTest.Code93FullAsciiControlCharacters;
begin
  // a = ($), b = (%): ($)A = SOH, (%)A = ESC, (%)X = DEL, (%)U = NUL
  Assert.AreEqual(#1#27#127#0, DecodeCode93('aAbAbXbU'));
end;

initialization

TDUnitX.RegisterTestFixture(TOneDFullAsciiTest);

end.
