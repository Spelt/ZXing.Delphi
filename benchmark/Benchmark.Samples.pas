unit Benchmark.Samples;

{
  * Reads the black box test samples of zxing-cpp (test/samples) and the
  * expectations that belong to them, following the rules of zxing-cpp's
  * test/blackbox/BlackboxTestRunner.cpp:
  *
  * - every folder has an optional "!defaults.toml" with defaults for all images
  * - every image has a ".toml" or ".txt" with the expected content; when there
  *   is none, the part of the name after the last '-', '_' or '!' is dropped
  *   until one is found (so "abc-2.png" uses "abc.txt")
  * - "find" lists the modes and rotations where zxing-cpp reads the symbol,
  *   "missing" the ones where it does not, "search" extra positions to try
  * - an image name ending in "!", "!f" or "!p" is not read by zxing-cpp in all
  *   modes, in fast and pure mode, or in pure mode
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  ZXing.BarcodeFormat;

type
  TTestMode = (tmSlow, tmFast, tmPure);

  /// <summary>
  /// Set of modes and rotations (0, 90, 180, 270 degrees), parsed from strings
  /// like "sa fh p0": s/f/p select the mode, a = all rotations,
  /// h = 0 and 180, v = 90 and 270, 0..3 = one rotation.
  /// </summary>
  TTestFilter = record
    Positions: array [TTestMode, 0 .. 3] of Boolean;
    class function Parse(const value: string): TTestFilter; static;
    function Contains(mode: TTestMode; rotation: Integer): Boolean;
    procedure Add(const other: TTestFilter);
    procedure Remove(const other: TTestFilter);
  end;

  /// <summary>One symbol that is expected in an image.</summary>
  TExpectedSymbol = record
    FormatName: string;
    Format: TBarcodeFormat;
    /// <summary>False when ZXing.Delphi does not support the format.</summary>
    Supported: Boolean;
    TextPlain: string;
    TextEscaped: string;
    /// <summary>False when only TextHex (or nothing) is given; then only the
    /// format is compared.</summary>
    HasText: Boolean;
    Find: TTestFilter;
    Missing: TTestFilter;
    /// <summary>zxing-cpp reads this symbol at that mode and rotation.</summary>
    function ReadByCpp(mode: TTestMode; rotation: Integer): Boolean;
  end;

  TSampleImage = class
  public
    FileName: string;
    /// <summary>Positions where the test runs.</summary>
    Search: TTestFilter;
    Symbols: TArray<TExpectedSymbol>;
    /// <summary>"formats" option of the test, e.g. "upca".</summary>
    Formats: string;
    /// <summary>"eanAddOnSymbol" option: "ignore" (default), "read" or
    /// "require".</summary>
    EanAddOnSymbol: string;
  end;

  TSampleFolder = class
  private
    FImages: TObjectList<TSampleImage>;
  public
    Name: string;
    Path: string;
    /// <summary>Format to scan for; Auto for mixed folders like multi-1.</summary>
    ScanFormat: TBarcodeFormat;
    /// <summary>The other formats of the symbols in the folder, like Code 32
    /// and PZN in code39-1 (only read when asked for).</summary>
    OtherFormats: TArray<TBarcodeFormat>;
    constructor Create;
    destructor Destroy; override;
    /// <summary>Returns nil when ZXing.Delphi does not support the folder's
    /// format.</summary>
    class function Load(const folderPath: string): TSampleFolder; static;
    property Images: TObjectList<TSampleImage> read FImages;
  end;

/// <summary>Maps a zxing-cpp format name ("DataMatrix", "Data Matrix",
/// "EAN-13", "upca", ...) to a ZXing.Delphi format.</summary>
function FormatFromName(const name: string; out format: TBarcodeFormat): Boolean;

/// <summary>Same as zxing-cpp's EscapeNonGraphical for ASCII: control
/// characters become "&lt;GS&gt;" etc.</summary>
function EscapeNonGraphical(const s: string): string;

function TestModeName(mode: TTestMode): string;

implementation

uses
  System.Character;

const
  ASCII_NAMES: array [0 .. 32] of string = ('NUL', 'SOH', 'STX', 'ETX', 'EOT',
    'ENQ', 'ACK', 'BEL', 'BS', 'HT', 'LF', 'VT', 'FF', 'CR', 'SO', 'SI', 'DLE',
    'DC1', 'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB', 'CAN', 'EM', 'SUB', 'ESC',
    'FS', 'GS', 'RS', 'US', 'DEL');

  IMAGE_EXTENSIONS: array [0 .. 4] of string = ('.webp', '.png', '.jpg',
    '.gif', '.pgm');

type
  /// <summary>Key/value pairs of one TOML record; keys are case sensitive.</summary>
  TProperties = TDictionary<string, string>;

function TestModeName(mode: TTestMode): string;
begin
  case mode of
    tmSlow: Result := 'slow';
    tmFast: Result := 'fast';
  else
    Result := 'pure';
  end;
end;

function EscapeNonGraphical(const s: string): string;
var
  sb: TStringBuilder;
  c: Char;
begin
  sb := TStringBuilder.Create(Length(s));
  try
    for c in s do
      if (Ord(c) < 32) then
        sb.Append('<').Append(ASCII_NAMES[Ord(c)]).Append('>')
      else if (Ord(c) = 127) then
        sb.Append('<DEL>')
      else
        sb.Append(c);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function FormatFromName(const name: string; out format: TBarcodeFormat): Boolean;
var
  n: string;
begin
  n := LowerCase(name).Replace(' ', '').Replace('-', '').Replace('_', '')
    .Replace('/', '');
  Result := true;
  if (n = 'datamatrix') then
    format := TBarcodeFormat.DATA_MATRIX
  else if (n = 'qrcode') then
    format := TBarcodeFormat.QR_CODE
  else if (n = 'ean13') then
    format := TBarcodeFormat.EAN_13
  else if (n = 'ean8') then
    format := TBarcodeFormat.EAN_8
  else if (n = 'upca') then
    format := TBarcodeFormat.UPC_A
  else if (n = 'upce') then
    format := TBarcodeFormat.UPC_E
  else if (n = 'code128') then
    format := TBarcodeFormat.CODE_128
  else if (n = 'code39') or (n = 'code39ext') or (n = 'code39extended') then
    format := TBarcodeFormat.CODE_39
  else if (n = 'code93') then
    format := TBarcodeFormat.CODE_93
  else if (n = 'itf') then
    format := TBarcodeFormat.ITF
  else if (n = 'aztec') or (n = 'azteccode') or (n = 'aztecrune') then
    format := TBarcodeFormat.AZTEC
  else if (n = 'databar') or (n = 'databaromni') or (n = 'databarstk') or
    (n = 'databarstacked') or (n = 'databarstkomni') or
    (n = 'databarstackedomni') or (n = 'rss14') then
    format := TBarcodeFormat.RSS_14
  else if (n = 'databarexp') or (n = 'databarexpanded') or
    (n = 'databarexpstk') or (n = 'databarexpandedstacked') or
    (n = 'rssexpanded') then
    format := TBarcodeFormat.RSS_EXPANDED
  else if (n = 'databarltd') or (n = 'databarlimited') then
    format := TBarcodeFormat.RSS_LIMITED
  else if (n = 'dxfilmedge') then
    format := TBarcodeFormat.DX_FILM_EDGE
  else if (n = 'code32') then
    format := TBarcodeFormat.CODE_32
  else if (n = 'pzn') then
    format := TBarcodeFormat.PZN
  else if (n = 'msi') then
    format := TBarcodeFormat.MSI
  else if (n = 'plessey') then
    format := TBarcodeFormat.PLESSEY
  else if (n = 'pharmacode') then
    format := TBarcodeFormat.PHARMA_CODE
  else if (n = 'kix') or (n = 'kixcode') then
    format := TBarcodeFormat.KIX
  else if (n = 'rm4scc') then
    format := TBarcodeFormat.RM4SCC
  else if (n = 'imb') or (n = 'uspsimail') then
    format := TBarcodeFormat.IMB
  else if (n = 'postnet') then
    format := TBarcodeFormat.POSTNET
  else if (n = 'planet') then
    format := TBarcodeFormat.PLANET
  else if (n = 'japanpost') then
    format := TBarcodeFormat.JAPAN_POST
  else if (n = 'auspost') or (n = 'australiapost') then
    format := TBarcodeFormat.AUSTRALIA_POST
  else if (n = 'mailmark') or (n = 'mailmark4s') then
    format := TBarcodeFormat.MAILMARK_4STATE
  else if (n = 'code11') then
    format := TBarcodeFormat.CODE_11
  else if (n = 'industrial2of5') or (n = 'c25ind') then
    format := TBarcodeFormat.INDUSTRIAL_2_OF_5
  else if (n = 'iata2of5') or (n = 'c25iata') then
    format := TBarcodeFormat.IATA_2_OF_5
  else if (n = 'matrix2of5') or (n = 'c25standard') or (n = 'c25matrix') then
    format := TBarcodeFormat.MATRIX_2_OF_5
  else if (n = 'datalogic2of5') or (n = 'c25logic') then
    format := TBarcodeFormat.DATALOGIC_2_OF_5
  else if (n = 'pharmacodetwotrack') or (n = 'pharma2') then
    format := TBarcodeFormat.PHARMA_CODE_TWO_TRACK
  else if (n = 'cepnet') then
    format := TBarcodeFormat.CEPNET
  else if (n = 'koreapost') then
    format := TBarcodeFormat.KOREA_POST
  else if (n = 'fim') then
    format := TBarcodeFormat.FIM
  else if (n = 'leitcode') or (n = 'dpleit') then
    format := TBarcodeFormat.DP_LEITCODE
  else if (n = 'identcode') or (n = 'dpident') then
    format := TBarcodeFormat.DP_IDENTCODE
  else if (n = 'codablockf') or (n = 'codablock') then
    format := TBarcodeFormat.CODABLOCK_F
  else if (n = 'code16k') then
    format := TBarcodeFormat.CODE_16K
  else if (n = 'code49') then
    format := TBarcodeFormat.CODE_49
  else if (n = 'dotcode') then
    format := TBarcodeFormat.DOTCODE
  else if (n = 'maxicode') then
    format := TBarcodeFormat.MAXICODE
  else if (n = 'microqrcode') then
    format := TBarcodeFormat.MICRO_QR_CODE
  else if (n = 'rmqrcode') then
    format := TBarcodeFormat.RMQR_CODE
  else if (n = 'pdf417') then
    format := TBarcodeFormat.PDF_417
  else if (n = 'micropdf417') then
    format := TBarcodeFormat.MICRO_PDF417
  else if (n = 'codabar') then
    format := TBarcodeFormat.CODABAR
  else if (n = 'telepen') or (n = 'telepenalpha') or (n = 'telepennumeric') then
    format := TBarcodeFormat.TELEPEN
  else
    Result := false;
end;

{ TTestFilter }

class function TTestFilter.Parse(const value: string): TTestFilter;
var
  c: Char;
  mode: TTestMode;
  hasMode: Boolean;
  r: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  hasMode := false;
  mode := tmSlow;
  for c in value do
  begin
    case c of
      ' ':
        ;
      's':
        begin
          mode := tmSlow;
          hasMode := true;
        end;
      'f':
        begin
          mode := tmFast;
          hasMode := true;
        end;
      'p':
        begin
          mode := tmPure;
          hasMode := true;
        end;
      '0' .. '3', 'h', 'v', 'a':
        begin
          if not hasMode then
            raise EArgumentException.CreateFmt('No mode before ''%s'' in ''%s''',
              [c, value]);
          case c of
            'h':
              begin
                Result.Positions[mode, 0] := true;
                Result.Positions[mode, 2] := true;
              end;
            'v':
              begin
                Result.Positions[mode, 1] := true;
                Result.Positions[mode, 3] := true;
              end;
            'a':
              for r := 0 to 3 do
                Result.Positions[mode, r] := true;
          else
            Result.Positions[mode, Ord(c) - Ord('0')] := true;
          end;
        end;
    else
      raise EArgumentException.CreateFmt('Invalid character ''%s'' in ''%s''',
        [c, value]);
    end;
  end;
end;

function TTestFilter.Contains(mode: TTestMode; rotation: Integer): Boolean;
begin
  Result := Positions[mode, rotation];
end;

procedure TTestFilter.Add(const other: TTestFilter);
var
  m: TTestMode;
  r: Integer;
begin
  for m := Low(TTestMode) to High(TTestMode) do
    for r := 0 to 3 do
      Positions[m, r] := Positions[m, r] or other.Positions[m, r];
end;

procedure TTestFilter.Remove(const other: TTestFilter);
var
  m: TTestMode;
  r: Integer;
begin
  for m := Low(TTestMode) to High(TTestMode) do
    for r := 0 to 3 do
      Positions[m, r] := Positions[m, r] and not other.Positions[m, r];
end;

{ TExpectedSymbol }

function TExpectedSymbol.ReadByCpp(mode: TTestMode; rotation: Integer): Boolean;
begin
  Result := Find.Contains(mode, rotation) and not Missing.Contains(mode, rotation);
end;

{ TOML reading }

function ParseTomlValue(const value, fileName, key: string): string;
var
  v: string;
  i, j, codePoint: Integer;
  sb: TStringBuilder;

  function hexCodePoint(digits: Integer): Integer;
  var
    k: Integer;
  begin
    Result := 0;
    for k := 1 to digits do
    begin
      Inc(i);
      if (i > Length(v) - 1) then
        raise EArgumentException.CreateFmt('%s: invalid unicode escape in %s',
          [fileName, key]);
      Result := Result * 16 + StrToInt('$' + v[i]);
    end;
  end;

begin
  v := Trim(value);
  if (Length(v) < 2) or (v[1] <> '"') or (v[Length(v)] <> '"') then
    exit(v);

  sb := TStringBuilder.Create;
  try
    i := 2;
    while (i < Length(v)) do
    begin
      if (v[i] <> '\') then
        sb.Append(v[i])
      else
      begin
        Inc(i);
        case v[i] of
          '"': sb.Append('"');
          '\': sb.Append('\');
          'b': sb.Append(#8);
          't': sb.Append(#9);
          'n': sb.Append(#10);
          'f': sb.Append(#12);
          'r': sb.Append(#13);
          'u', 'U':
            begin
              if (v[i] = 'u') then
                j := 4
              else
                j := 8;
              codePoint := hexCodePoint(j);
              sb.Append(Char.ConvertFromUtf32(codePoint));
            end;
        else
          raise EArgumentException.CreateFmt('%s: unsupported escape in %s',
            [fileName, key]);
        end;
      end;
      Inc(i);
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

/// <summary>Reads the [[symbol]] records of a TOML file. Top-level keys are
/// defaults for all records. Without the file the defaults are returned.</summary>
function ReadToml(const fileName: string; const folderDefaults: TProperties)
  : TObjectList<TProperties>;
var
  line, key: string;
  defaults, current: TProperties;
  atTopLevel: Boolean;
  equals: Integer;
begin
  Result := TObjectList<TProperties>.Create;
  if not TFile.Exists(fileName) then
  begin
    Result.Add(TProperties.Create(folderDefaults));
    exit;
  end;

  defaults := TProperties.Create(folderDefaults);
  try
    current := TProperties.Create(defaults);
    atTopLevel := true;
    for line in TFile.ReadAllLines(fileName, TEncoding.UTF8) do
    begin
      if (Trim(line) = '') or Trim(line).StartsWith('#') then
        continue;
      if (Trim(line) = '[[symbol]]') then
      begin
        if atTopLevel then
        begin
          // top-level key/value pairs are defaults for all subsequent records
          atTopLevel := false;
          defaults.Free;
          defaults := TProperties.Create(current);
        end
        else
        begin
          Result.Add(current);
          current := TProperties.Create(defaults);
        end;
        continue;
      end;
      if Trim(line).StartsWith('[') then
        raise EArgumentException.CreateFmt('%s: only [[symbol]] tables are supported',
          [fileName]);
      equals := Pos('=', line);
      if (equals = 0) then
        raise EArgumentException.CreateFmt('%s: invalid line ''%s''',
          [fileName, line]);
      key := Trim(Copy(line, 1, equals - 1));
      current.AddOrSetValue(key, ParseTomlValue(Copy(line, equals + 1, MaxInt),
        fileName, key));
    end;
    Result.Add(current);
  finally
    defaults.Free;
  end;
end;

{ TSampleFolder }

constructor TSampleFolder.Create;
begin
  inherited;
  FImages := TObjectList<TSampleImage>.Create;
end;

destructor TSampleFolder.Destroy;
begin
  FImages.Free;
  inherited;
end;

function IsImage(const fileName: string): Boolean;
var
  ext, e: string;
begin
  ext := LowerCase(ExtractFileExt(fileName));
  for e in IMAGE_EXTENSIONS do
    if (ext = e) then
      exit(true);
  Result := false;
end;

/// <summary>Finds the .toml or .txt that belongs to an image, see the unit
/// comment. Returns '' when there is none.</summary>
function FindDataFile(const imagePath: string): string;
var
  dir, stem: string;
  sep: Integer;
begin
  dir := ExtractFilePath(imagePath);
  stem := TPath.GetFileNameWithoutExtension(imagePath);
  while true do
  begin
    if TFile.Exists(dir + stem + '.toml') then
      exit(dir + stem + '.toml');
    if TFile.Exists(dir + stem + '.txt') then
      exit(dir + stem + '.txt');
    sep := stem.LastDelimiter('-_!') + 1;
    if (sep = 0) then
      exit('');
    stem := Copy(stem, 1, sep - 1);
  end;
end;

function GetProp(props: TProperties; const key: string): string;
begin
  if not props.TryGetValue(key, Result) then
    Result := '';
end;

class function TSampleFolder.Load(const folderPath: string): TSampleFolder;
var
  folderName, stem, imagePath, dataFile, imageStem, s: string;
  folderFormat, symbolFormat: TBarcodeFormat;
  isMixed, isLinear: Boolean;
  defaults, props: TProperties;
  defaultRecords, records: TObjectList<TProperties>;
  image: TSampleImage;
  symbol: TExpectedSymbol;
  imagePaths: TArray<string>;
  i: Integer;
begin
  Result := nil;
  folderName := ExtractFileName(ExcludeTrailingPathDelimiter(folderPath));
  stem := folderName;
  if (Pos('-', stem) > 0) then
    stem := Copy(stem, 1, Pos('-', stem) - 1);

  isMixed := (stem = 'multi') or (stem = 'none');
  if isMixed then
    folderFormat := TBarcodeFormat.Auto
  else if not FormatFromName(stem, folderFormat) then
    exit;

  isLinear := not isMixed and (folderFormat <> TBarcodeFormat.DATA_MATRIX) and
    (folderFormat <> TBarcodeFormat.QR_CODE);

  Result := TSampleFolder.Create;
  Result.Name := folderName;
  Result.Path := IncludeTrailingPathDelimiter(folderPath);
  Result.ScanFormat := folderFormat;

  defaults := TProperties.Create;
  try
    if isLinear then
      defaults.Add('find', 'sh fh')
    else
      defaults.Add('find', 'sa fh');
    defaults.Add('Format', stem);

    defaultRecords := ReadToml(Result.Path + '!defaults.toml', defaults);
    try
      defaults.Free;
      defaults := TProperties.Create(defaultRecords[0]);
    finally
      defaultRecords.Free;
    end;

    imagePaths := TDirectory.GetFiles(Result.Path);
    TArray.Sort<string>(imagePaths);
    for imagePath in imagePaths do
    begin
      if not IsImage(imagePath) then
        continue;

      dataFile := FindDataFile(imagePath);
      if dataFile.EndsWith('.toml', true) then
        records := ReadToml(dataFile, defaults)
      else
      begin
        records := TObjectList<TProperties>.Create;
        records.Add(TProperties.Create(defaults));
        if (dataFile <> '') then
          records[0].AddOrSetValue('TextEscaped',
            EscapeNonGraphical(TFile.ReadAllText(dataFile, TEncoding.UTF8)));
      end;

      try
        imageStem := TPath.GetFileNameWithoutExtension(imagePath);
        if imageStem.EndsWith('!') or (dataFile = '') then
          records[0].AddOrSetValue('missing', 'sa fa pa')
        else if imageStem.EndsWith('!f') then
          records[0].AddOrSetValue('missing', 'fa pa')
        else if imageStem.EndsWith('!p') then
          records[0].AddOrSetValue('missing', 'pa');

        image := TSampleImage.Create;
        Result.Images.Add(image);
        image.FileName := imagePath;
        image.Formats := GetProp(records[0], 'formats');
        image.EanAddOnSymbol := GetProp(records[0], 'eanAddOnSymbol');
        image.Search := TTestFilter.Parse(GetProp(records[0], 'find'));
        s := GetProp(records[0], 'search');
        if (s <> '') then
          image.Search.Add(TTestFilter.Parse(s));

        // An image without data file and without expectations (none-*) has
        // no symbols: everything found there is a false positive.
        if (dataFile = '') and isMixed then
          continue;

        SetLength(image.Symbols, records.Count);
        for i := 0 to records.Count - 1 do
        begin
          props := records[i];
          symbol := Default(TExpectedSymbol);
          symbol.FormatName := GetProp(props, 'Format');
          symbol.Supported := FormatFromName(symbol.FormatName, symbolFormat);
          symbol.Format := symbolFormat;
          if symbol.Supported and not isMixed and
            (symbolFormat <> folderFormat) then
          begin
            var known := false;
            for var f in Result.OtherFormats do
              known := known or (f = symbolFormat);
            if not known then
              Result.OtherFormats := Result.OtherFormats + [symbolFormat];
          end;
          symbol.TextPlain := GetProp(props, 'TextPlain');
          symbol.TextEscaped := GetProp(props, 'TextEscaped');
          symbol.HasText := props.ContainsKey('TextPlain') or
            props.ContainsKey('TextEscaped');
          symbol.Find := TTestFilter.Parse(GetProp(props, 'find'));
          s := GetProp(props, 'missing');
          if (s <> '') then
            symbol.Missing := TTestFilter.Parse(s);
          image.Symbols[i] := symbol;
        end;
      finally
        records.Free;
      end;
    end;
  finally
    defaults.Free;
  end;
end;

end.
