{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
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

  * Ported from zxing-cpp (PDFDecoder.cpp and DecodeCodewords of
  * PDFScanningDecoder.cpp): the text of the codewords of PDF417 and
  * MicroPDF417 (ISO/IEC 15438:2015, ISO/IEC 24728:2006).
}

unit ZXing.PDF417.Internal.DecodedBitStreamParser;

interface

uses
  ZXing.DecoderResult,
  ZXing.PDF417.ResultMetadata;

/// <summary>
/// Error corrects the codewords (the first is the number of data codewords)
/// and decodes them. nil when that fails, with error 'Checksum' (too many
/// errors), 'Format' or 'Unsupported'. extra gets the Macro PDF417 data
/// (Structured Append) when the codewords could be decoded.
/// </summary>
function DecodePDF417Codewords(var codewords: TArray<Integer>;
  numECCodewords: Integer; const erasures: TArray<Integer>;
  microPDF417: Boolean; out error: string;
  out extra: IPDF417ResultMetadata): TDecoderResult;

/// <summary>The number of error correction codewords of an error correction
/// level (0 to 8).</summary>
function NumECCodewords(ecLevel: Integer): Integer;

implementation

uses
  System.SysUtils,
  System.Math,
  ZXing.Common.ECIContent,
  ZXing.PDF417.Internal.ErrorCorrection;

type
  TMode = (mdAlpha, mdLower, mdMixed, mdPunct, mdAlphaShift, mdPunctShift);

  EPDF417Format = class(Exception);
  EPDF417Unsupported = class(Exception);

const
  TEXT_COMPACTION_MODE_LATCH = 900;
  BYTE_COMPACTION_MODE_LATCH = 901;
  NUMERIC_COMPACTION_MODE_LATCH = 902;
  // 903-912 reserved in PDF417; MicroPDF417 function codewords
  MODE_SHIFT_TO_BYTE_COMPACTION_MODE = 913;
  // 914-917 reserved in PDF417; MicroPDF417 function codewords
  MACRO_05 = 916;
  MACRO_06 = 917;
  LINKAGE_OTHER = 918;
  // 919 reserved
  LINKAGE_EANUCC = 920; // GS1 Composite
  READER_INIT = 921; // reader initialisation / programming
  MACRO_PDF417_TERMINATOR = 922;
  BEGIN_MACRO_PDF417_OPTIONAL_FIELD = 923;
  BYTE_COMPACTION_MODE_LATCH_6 = 924;
  ECI_USER_DEFINED = 925; // 810900-811799 (1 codeword)
  ECI_GENERAL_PURPOSE = 926; // 900-810899 (2 codewords)
  ECI_CHARSET = 927; // 0-899 (1 codeword)
  BEGIN_MACRO_PDF417_CONTROL_BLOCK = 928;

  MAX_NUMERIC_CODEWORDS = 15;

  MACRO_PDF417_OPTIONAL_FIELD_FILE_NAME = 0;
  MACRO_PDF417_OPTIONAL_FIELD_SEGMENT_COUNT = 1;
  MACRO_PDF417_OPTIONAL_FIELD_TIME_STAMP = 2;
  MACRO_PDF417_OPTIONAL_FIELD_SENDER = 3;
  MACRO_PDF417_OPTIONAL_FIELD_ADDRESSEE = 4;
  MACRO_PDF417_OPTIONAL_FIELD_FILE_SIZE = 5;
  MACRO_PDF417_OPTIONAL_FIELD_CHECKSUM = 6;

  PUNCT_CHARS = ';<>@[\]_`~!'#13#9',:'#10'-.$/"|*()?{}''';
  MIXED_CHARS = '0123456789&'#13#9',:#-.$/+%*=^';

  NUMBER_OF_SEQUENCE_CODEWORDS = 2;

  // the code page of the Macro PDF417 optional text fields (ECI 2)
  CP437 = 'IBM437';

function NumECCodewords(ecLevel: Integer): Integer;
begin
  Result := 2 shl ecLevel;
end;

function IsECI(code: Integer): Boolean; inline;
begin
  Result := (code >= ECI_USER_DEFINED) and (code <= ECI_CHARSET);
end;

/// <summary>Whether a codeword ends a compaction mode (ISO/IEC 15438:2015
/// 5.4.2.5, 5.4.3.4, 5.4.4.3).</summary>
function TerminatesCompaction(code: Integer): Boolean;
begin
  case code of
    TEXT_COMPACTION_MODE_LATCH, BYTE_COMPACTION_MODE_LATCH,
      NUMERIC_COMPACTION_MODE_LATCH, BYTE_COMPACTION_MODE_LATCH_6,
      BEGIN_MACRO_PDF417_CONTROL_BLOCK, BEGIN_MACRO_PDF417_OPTIONAL_FIELD,
      MACRO_PDF417_TERMINATOR:
      Result := true;
  else
    Result := false;
  end;
end;

function ProcessECI(const codewords: TArray<Integer>; codeIndex, length,
  code: Integer; var res: TECIContent): Integer;
begin
  Result := codeIndex;
  if not IsECI(code) or (codeIndex >= length) then
    exit;
  if (code = ECI_CHARSET) then
  begin
    res.SwitchEncoding(codewords[codeIndex]);
    Result := codeIndex + 1;
  end
  else
  begin
    var paramCount := 1;
    if (code = ECI_GENERAL_PURPOSE) then
      paramCount := 2;
    if (codeIndex + paramCount > length) then
      exit;
    // other than character set ECIs are ignored
    Result := codeIndex + paramCount;
  end;
end;

procedure DecodeTextCompaction(const textCompactionData: TArray<Integer>;
  length: Integer; var res: TECIContent; initialMode: TMode);
begin
  // the default (and after a latch) is the Alpha sub-mode
  var subMode := initialMode;
  var priorToShiftMode := mdAlpha;
  var i := 0;
  while (i < length) do
  begin
    var subModeCh := textCompactionData[i];

    // only ECIs and Shift to Byte are function codewords here
    if IsECI(subModeCh) then
    begin
      i := ProcessECI(textCompactionData, i + 1, length, subModeCh, res);
      continue;
    end;
    if (subModeCh = MODE_SHIFT_TO_BYTE_COMPACTION_MODE) then
    begin
      Inc(i);
      while (i < length) and IsECI(textCompactionData[i]) do
        i := ProcessECI(textCompactionData, i + 1, length,
          textCompactionData[i], res);
      if (i < length) then
      begin
        res.Append(Byte(textCompactionData[i]));
        Inc(i);
      end;
      continue;
    end;

    var ch: Char := #0;
    case subMode of
      mdAlpha, mdLower:
        if (subModeCh < 26) then
        begin
          if (subMode = mdAlpha) then
            ch := Char(Ord('A') + subModeCh)
          else
            ch := Char(Ord('a') + subModeCh);
        end
        else if (subModeCh = 26) then
          ch := ' '
        else if (subModeCh = 27) and (subMode = mdAlpha) then // LL
          subMode := mdLower
        else if (subModeCh = 27) and (subMode = mdLower) then // AS
        begin
          priorToShiftMode := subMode;
          subMode := mdAlphaShift;
        end
        else if (subModeCh = 28) then // ML
          subMode := mdMixed
        // 29 PS: ignored when last or followed by Shift to Byte (5.4.2.4
        // (b) (1))
        else if (i + 1 < length) and (textCompactionData[i + 1] <>
          MODE_SHIFT_TO_BYTE_COMPACTION_MODE) then
        begin
          priorToShiftMode := subMode;
          subMode := mdPunctShift;
        end;
      mdMixed:
        if (subModeCh < 25) then
          ch := MIXED_CHARS[subModeCh + 1]
        else if (subModeCh = 25) then // PL
          subMode := mdPunct
        else if (subModeCh = 26) then
          ch := ' '
        else if (subModeCh = 27) then // LL
          subMode := mdLower
        else if (subModeCh = 28) then // AL
          subMode := mdAlpha
        else if (i + 1 < length) and (textCompactionData[i + 1] <>
          MODE_SHIFT_TO_BYTE_COMPACTION_MODE) then
        begin
          // 29 PS
          priorToShiftMode := subMode;
          subMode := mdPunctShift;
        end;
      mdPunct:
        if (subModeCh < 29) then
          ch := PUNCT_CHARS[subModeCh + 1]
        else
          // 29 AL (not ignored when followed by Shift to Byte, 5.4.2.4 (b)
          // (2))
          subMode := mdAlpha;
      mdAlphaShift:
        begin
          subMode := priorToShiftMode;
          if (subModeCh < 26) then
            ch := Char(Ord('A') + subModeCh)
          else if (subModeCh = 26) then
            ch := ' ';
          // 27 LL, 28 ML, 29 PS used as padding
        end;
      mdPunctShift:
        begin
          subMode := priorToShiftMode;
          if (subModeCh < 29) then
            ch := PUNCT_CHARS[subModeCh + 1]
          else // 29 AL
            subMode := mdAlpha;
        end;
    end;
    if (ch <> #0) then
      res.Append(Byte(Ord(ch)));
    Inc(i);
  end;
end;

/// <summary>Puts an ECI codeword and its parameters in the text compaction
/// data.</summary>
function ProcessTextECI(var textCompactionData: TArray<Integer>;
  var index: Integer; const codewords: TArray<Integer>;
  codeIndex, code: Integer): Integer;
begin
  textCompactionData[index] := code;
  Inc(index);
  if (codeIndex < codewords[0]) then
  begin
    textCompactionData[index] := codewords[codeIndex];
    Inc(index);
    Inc(codeIndex);
    if (codeIndex < codewords[0]) and (code = ECI_GENERAL_PURPOSE) then
    begin
      textCompactionData[index] := codewords[codeIndex];
      Inc(index);
      Inc(codeIndex);
    end;
  end;
  Result := codeIndex;
end;

/// <summary>Text Compaction mode (5.4.1.5): 2 characters per codeword.
/// Returns the next index.</summary>
function TextCompaction(const codewords: TArray<Integer>; codeIndex: Integer;
  var res: TECIContent; initialMode: TMode = mdAlpha): Integer;
begin
  var textCompactionData: TArray<Integer>;
  // (+ 2 for a GS of Macro 06)
  SetLength(textCompactionData, Max(0, (codewords[0] - codeIndex) * 2) + 2);
  var index := 0;
  var ending := false;
  while (codeIndex < codewords[0]) and not ending do
  begin
    var code := codewords[codeIndex];
    Inc(codeIndex);
    if (code < TEXT_COMPACTION_MODE_LATCH) then
    begin
      textCompactionData[index] := code div 30;
      textCompactionData[index + 1] := code mod 30;
      Inc(index, 2);
    end
    else
      case code of
        MODE_SHIFT_TO_BYTE_COMPACTION_MODE:
          begin
            // a switch to Byte Compaction for the next codeword only
            textCompactionData[index] := MODE_SHIFT_TO_BYTE_COMPACTION_MODE;
            Inc(index);
            // ECIs are allowed anywhere, also after a Shift to Byte (5.5.3.1)
            while (codeIndex < codewords[0]) and IsECI(codewords[codeIndex]) do
              codeIndex := ProcessTextECI(textCompactionData, index, codewords,
                codeIndex + 1, codewords[codeIndex]);
            if (codeIndex < codewords[0]) then
            begin
              // the byte
              textCompactionData[index] := codewords[codeIndex];
              Inc(index);
              Inc(codeIndex);
            end;
          end;
        ECI_CHARSET, ECI_GENERAL_PURPOSE, ECI_USER_DEFINED:
          codeIndex := ProcessTextECI(textCompactionData, index, codewords,
            codeIndex, code);
        903, 904, 905:
          // a group separator (GS) in Macro 06 (MicroPDF417)
          if (initialMode = mdMixed) then
          begin
            textCompactionData[index] := MODE_SHIFT_TO_BYTE_COMPACTION_MODE;
            textCompactionData[index + 1] := 29;
            Inc(index, 2);
          end
          else
            raise EPDF417Format.Create
              ('Reserved codeword in Text Compaction mode');
      else
        if not TerminatesCompaction(code) then
          raise EPDF417Format.Create
            ('Reserved codeword in Text Compaction mode');
        Dec(codeIndex);
        ending := true;
      end;
    // (more room for the GS of Macro 06)
    if (index + 4 > Length(textCompactionData)) then
      SetLength(textCompactionData, Length(textCompactionData) + 8);
  end;
  DecodeTextCompaction(textCompactionData, index, res, initialMode);
  Result := codeIndex;
end;

/// <summary>The number of 5 codeword batches of Byte Compaction and the
/// trailing bytes, with checks of the format.</summary>
function CountByteBatches(mode: Integer; const codewords: TArray<Integer>;
  codeIndex: Integer; out trailingCount: Integer): Integer;
begin
  var count := 0;
  trailingCount := 0;
  while (codeIndex < codewords[0]) do
  begin
    var code := codewords[codeIndex];
    Inc(codeIndex);
    if (code >= TEXT_COMPACTION_MODE_LATCH) then
    begin
      if (mode = BYTE_COMPACTION_MODE_LATCH_6) and (count <> 0) and
        (count mod 5 <> 0) then
        raise EPDF417Format.Create('Format');
      if IsECI(code) then
      begin
        if (code = ECI_GENERAL_PURPOSE) then
          Inc(codeIndex, 2)
        else
          Inc(codeIndex);
        continue;
      end;
      if not TerminatesCompaction(code) then
        raise EPDF417Format.Create('Format');
      break;
    end;
    Inc(count);
  end;
  if (codeIndex > codewords[0]) then
    raise EPDF417Format.Create('Format');

  if (count = 0) then
    exit(0);

  if (mode = BYTE_COMPACTION_MODE_LATCH) then
  begin
    trailingCount := count mod 5;
    if (trailingCount = 0) then
    begin
      trailingCount := 5;
      Dec(count, 5);
    end;
  end
  else if (count mod 5 <> 0) then
    raise EPDF417Format.Create('Format');

  Result := count div 5;
end;

function ProcessByteECIs(const codewords: TArray<Integer>; codeIndex: Integer;
  var res: TECIContent): Integer;
begin
  while (codeIndex < codewords[0]) and
    (codewords[codeIndex] >= TEXT_COMPACTION_MODE_LATCH) and
    not TerminatesCompaction(codewords[codeIndex]) do
  begin
    var code := codewords[codeIndex];
    Inc(codeIndex);
    if not IsECI(code) then
      raise EPDF417Format.Create('Format');
    codeIndex := ProcessECI(codewords, codeIndex, codewords[0], code, res);
  end;
  Result := codeIndex;
end;

/// <summary>Byte Compaction mode (5.4.3): 6 bytes per 5 codewords, mode
/// 901 or 924.</summary>
function ByteCompaction(mode: Integer; const codewords: TArray<Integer>;
  codeIndex: Integer; var res: TECIContent): Integer;
begin
  var trailingCount: Integer;
  var batches := CountByteBatches(mode, codewords, codeIndex, trailingCount);

  codeIndex := ProcessByteECIs(codewords, codeIndex, res);
  for var batch := 0 to batches - 1 do
  begin
    var value: Int64 := 0;
    for var count := 0 to 4 do
    begin
      // no ECI within a group of 5 codewords (5.5.3.2)
      if (codeIndex >= codewords[0]) or
        (codewords[codeIndex] >= TEXT_COMPACTION_MODE_LATCH) then
        raise EPDF417Format.Create('Format');
      value := 900 * value + codewords[codeIndex];
      Inc(codeIndex);
    end;
    for var j := 0 to 5 do
      res.Append(Byte(value shr (8 * (5 - j))));
    codeIndex := ProcessByteECIs(codewords, codeIndex, res);
  end;

  var i := 0;
  while (i < trailingCount) and (codeIndex < codewords[0]) do
  begin
    res.Append(Byte(codewords[codeIndex]));
    Inc(codeIndex);
    codeIndex := ProcessByteECIs(codewords, codeIndex, res);
    Inc(i);
  end;
  Result := codeIndex;
end;

/// <summary>count codewords in base 900 (ending before endIndex) as decimal
/// digits, without the leading 1.</summary>
function DecodeBase900toBase10(const codewords: TArray<Integer>;
  endIndex, count: Integer): string;
begin
  // the digits from the lowest on
  var digits: TArray<Integer> := [0];
  for var i := 0 to count - 1 do
  begin
    // digits := digits * 900 + codeword
    var carry := codewords[endIndex - count + i];
    for var k := 0 to High(digits) do
    begin
      var v := digits[k] * 900 + carry;
      digits[k] := v mod 10;
      carry := v div 10;
    end;
    while (carry > 0) do
    begin
      digits := digits + [carry mod 10];
      carry := carry div 10;
    end;
  end;
  var n := Length(digits);
  while (n > 1) and (digits[n - 1] = 0) do
    Dec(n);
  if (digits[n - 1] <> 1) or ((n = 1) and (digits[0] = 0)) then
    raise EPDF417Format.Create('Format');
  Result := '';
  for var k := n - 2 downto 0 do
    Result := Result + Char(Ord('0') + digits[k]);
end;

/// <summary>Numeric Compaction mode (5.4.4).</summary>
function NumericCompaction(const codewords: TArray<Integer>;
  codeIndex: Integer; var res: TECIContent): Integer;
begin
  var count := 0;
  while (codeIndex < codewords[0]) do
  begin
    var code := codewords[codeIndex];
    if (code < TEXT_COMPACTION_MODE_LATCH) then
    begin
      Inc(count);
      Inc(codeIndex);
    end;
    if (count > 0) and ((count = MAX_NUMERIC_CODEWORDS) or
      (codeIndex = codewords[0]) or (code >= TEXT_COMPACTION_MODE_LATCH)) then
    begin
      res.Append(DecodeBase900toBase10(codewords, codeIndex, count));
      count := 0;
    end;

    if (code >= TEXT_COMPACTION_MODE_LATCH) then
    begin
      // ECIs anywhere (Basic Channel Mode)
      if IsECI(code) then
        codeIndex := ProcessECI(codewords, codeIndex + 1, codewords[0], code,
          res)
      else if TerminatesCompaction(code) then
        break
      else
        raise EPDF417Format.Create('Format');
    end;
  end;
  Result := codeIndex;
end;

function DecodeMacroOptionalTextField(const codewords: TArray<Integer>;
  codeIndex: Integer; out field: string): Integer;
begin
  // each optional field begins with an implied reset to ECI 2 (Annex
  // H.2.3): ASCII and Cp437
  var res := TECIContent.Create(CP437);
  Result := TextCompaction(codewords, codeIndex, res);
  field := res.Text;
end;

function DecodeMacroOptionalNumericField(const codewords: TArray<Integer>;
  codeIndex: Integer; out field: Int64): Integer;
begin
  var res := TECIContent.Create(CP437);
  Result := NumericCompaction(codewords, codeIndex, res);
  if not TryStrToInt64(res.Text, field) then
    raise EPDF417Format.Create('Format');
end;

function DecodeMacroBlock(const codewords: TArray<Integer>;
  codeIndex: Integer; extra: TPDF417ResultMetadata): Integer;
begin
  // at least two codewords left for the segment index
  if (codeIndex + NUMBER_OF_SEQUENCE_CODEWORDS > codewords[0]) then
    raise EPDF417Format.Create('Format');

  Inc(codeIndex, NUMBER_OF_SEQUENCE_CODEWORDS);
  extra.FSegmentIndex := StrToInt(DecodeBase900toBase10(codewords, codeIndex,
    NUMBER_OF_SEQUENCE_CODEWORDS));

  // the file id as numbers 0-899, each 3 digits (Annex H.6)
  var fileId := '';
  while (codeIndex < codewords[0]) and
    (codewords[codeIndex] <> MACRO_PDF417_TERMINATOR) and
    (codewords[codeIndex] <> BEGIN_MACRO_PDF417_OPTIONAL_FIELD) do
  begin
    fileId := fileId + Format('%.3d', [codewords[codeIndex]]);
    Inc(codeIndex);
  end;
  extra.FFileId := fileId;

  var optionalFieldsStart := -1;
  if (codeIndex < codewords[0]) and
    (codewords[codeIndex] = BEGIN_MACRO_PDF417_OPTIONAL_FIELD) then
    optionalFieldsStart := codeIndex + 1;

  while (codeIndex < codewords[0]) do
    case codewords[codeIndex] of
      BEGIN_MACRO_PDF417_OPTIONAL_FIELD:
        begin
          Inc(codeIndex);
          if (codeIndex >= codewords[0]) then
            break;
          var value: Int64;
          case codewords[codeIndex] of
            MACRO_PDF417_OPTIONAL_FIELD_FILE_NAME:
              codeIndex := DecodeMacroOptionalTextField(codewords,
                codeIndex + 1, extra.FFileName);
            MACRO_PDF417_OPTIONAL_FIELD_SENDER:
              codeIndex := DecodeMacroOptionalTextField(codewords,
                codeIndex + 1, extra.FSender);
            MACRO_PDF417_OPTIONAL_FIELD_ADDRESSEE:
              codeIndex := DecodeMacroOptionalTextField(codewords,
                codeIndex + 1, extra.FAddressee);
            MACRO_PDF417_OPTIONAL_FIELD_SEGMENT_COUNT:
              begin
                codeIndex := DecodeMacroOptionalNumericField(codewords,
                  codeIndex + 1, value);
                extra.FSegmentCount := Integer(value);
              end;
            MACRO_PDF417_OPTIONAL_FIELD_TIME_STAMP:
              codeIndex := DecodeMacroOptionalNumericField(codewords,
                codeIndex + 1, extra.FTimestamp);
            MACRO_PDF417_OPTIONAL_FIELD_CHECKSUM:
              begin
                codeIndex := DecodeMacroOptionalNumericField(codewords,
                  codeIndex + 1, value);
                extra.FChecksum := Integer(value);
              end;
            MACRO_PDF417_OPTIONAL_FIELD_FILE_SIZE:
              codeIndex := DecodeMacroOptionalNumericField(codewords,
                codeIndex + 1, extra.FFileSize);
          else
            raise EPDF417Format.Create('Format');
          end;
        end;
      MACRO_PDF417_TERMINATOR:
        begin
          Inc(codeIndex);
          extra.FIsLastSegment := true;
        end;
    else
      raise EPDF417Format.Create('Format');
    end;

  // the optional fields as they are
  if (optionalFieldsStart <> -1) then
  begin
    var optionalFieldsLength := codeIndex - optionalFieldsStart;
    if extra.FIsLastSegment then
      Dec(optionalFieldsLength); // without the terminator
    extra.FOptionalData := Copy(codewords, optionalFieldsStart,
      optionalFieldsLength);
  end;
  Result := codeIndex;
end;

function Decode(const codewords: TArray<Integer>; microPDF417: Boolean;
  out error: string; out extra: IPDF417ResultMetadata): TDecoderResult;
begin
  Result := nil;
  extra := nil;
  if (Length(codewords) = 0) or (codewords[0] < 1) or
    (codewords[0] > Length(codewords)) then
  begin
    error := 'Format';
    exit;
  end;

  var res := TECIContent.Create;
  var readerInit := false;
  var macro := false;
  var data := TPDF417ResultMetadata.Create;
  var dataIntf: IPDF417ResultMetadata := data;
  try
    var codeIndex := 1;
    while (codeIndex < codewords[0]) do
    begin
      var code := codewords[codeIndex];
      Inc(codeIndex);
      case code of
        TEXT_COMPACTION_MODE_LATCH:
          codeIndex := TextCompaction(codewords, codeIndex, res);
        // only once, when the default Text Compaction mode applies
        MODE_SHIFT_TO_BYTE_COMPACTION_MODE:
          codeIndex := TextCompaction(codewords, codeIndex - 1, res);
        BYTE_COMPACTION_MODE_LATCH, BYTE_COMPACTION_MODE_LATCH_6:
          codeIndex := ByteCompaction(code, codewords, codeIndex, res);
        NUMERIC_COMPACTION_MODE_LATCH:
          codeIndex := NumericCompaction(codewords, codeIndex, res);
        ECI_CHARSET, ECI_GENERAL_PURPOSE, ECI_USER_DEFINED:
          codeIndex := ProcessECI(codewords, codeIndex, codewords[0], code, res);
        BEGIN_MACRO_PDF417_CONTROL_BLOCK:
          begin
            if macro then
              raise EPDF417Format.Create('Format');
            codeIndex := DecodeMacroBlock(codewords, codeIndex, data);
          end;
        BEGIN_MACRO_PDF417_OPTIONAL_FIELD, MACRO_PDF417_TERMINATOR:
          // not outside a macro block
          raise EPDF417Format.Create('Format');
        READER_INIT:
          // the first codeword after the symbol length (5.4.1.4)
          if (codeIndex <> 2) then
            raise EPDF417Format.Create('Format')
          else
            readerInit := true;
        LINKAGE_EANUCC:
          // the first codeword after the symbol length (GS1 Composite
          // ISO/IEC 24723:2010 4.3)
          if (codeIndex <> 2) then
            raise EPDF417Format.Create('Format');
        LINKAGE_OTHER:
          // may be treated as invalid in Basic Channel Mode (ISO/IEC
          // 24723:2010 5.4.1.5)
          raise EPDF417Unsupported.Create('LINKAGE_OTHER');
        // MicroPDF417 function codewords (ISO/IEC 24728:2006 5.4.1.5 and
        // 5.4.1.7): 05 and 06 Macro strings, implied Numeric or Text
        // Compaction latch
        MACRO_05, MACRO_06:
          begin
            if not microPDF417 or (codeIndex <> 2) or macro then
              raise EPDF417Format.Create('Format');
            macro := true;
            if (code = MACRO_05) then
            begin
              res.Append('[)>'#30'05'#29);
              codeIndex := NumericCompaction(codewords, codeIndex, res);
            end
            else
            begin
              res.Append('[)>'#30'06'#29);
              codeIndex := TextCompaction(codewords, codeIndex, res, mdMixed);
            end;
          end;
      else
        if (code >= TEXT_COMPACTION_MODE_LATCH) then
          // reserved codewords, may be treated as invalid (5.4.6.1)
          raise EPDF417Unsupported.Create('Reserved codeword')
        else
          // the default is Text Compaction mode Alpha (5.4.2.1)
          codeIndex := TextCompaction(codewords, codeIndex - 1, res);
      end;
    end;
  except
    on EPDF417Unsupported do
    begin
      error := 'Unsupported';
      exit;
    end;
    on Exception do
    begin
      error := 'Format';
      exit;
    end;
  end;

  if macro then
    res.Append(#30#4);

  if res.IsEmpty and (data.FSegmentIndex = -1) then
  begin
    error := 'Format';
    exit;
  end;

  if (data.FSegmentIndex > -1) and (data.FSegmentCount = -1) and
    data.FIsLastSegment then
    data.FSegmentCount := data.FSegmentIndex + 1;
  data.FReaderInit := readerInit;

  Result := TDecoderResult.Create(res.Bytes, res.Text, nil, '');
  // ISO/IEC 15424: ]L2, ]L1 with ECI
  if res.HasECI then
    Result.SymbologyIdentifier := ']L1'
  else
    Result.SymbologyIdentifier := ']L2';
  extra := dataIntf;
end;

/// <summary>The codeword array must be as long as its first codeword (the
/// number of data codewords) plus the error correction codewords.</summary>
function VerifyCodewordCount(var codewords: TArray<Integer>;
  numECCodewords: Integer): Boolean;
begin
  // the count, at least one data and two error correction codewords
  if (Length(codewords) < 4) then
    exit(false);
  var numberOfCodewords := codewords[0];
  if (numberOfCodewords > Length(codewords)) then
    exit(false);
  if (numberOfCodewords + numECCodewords <> Length(codewords)) then
  begin
    // set it to the length less the error correction codewords
    if (numECCodewords < Length(codewords)) then
      codewords[0] := Length(codewords) - numECCodewords
    else
      exit(false);
  end;
  Result := true;
end;

function DecodePDF417Codewords(var codewords: TArray<Integer>;
  numECCodewords: Integer; const erasures: TArray<Integer>;
  microPDF417: Boolean; out error: string;
  out extra: IPDF417ResultMetadata): TDecoderResult;
begin
  Result := nil;
  extra := nil;
  error := '';
  if (Length(codewords) = 0) then
  begin
    error := 'Format';
    exit;
  end;

  var usedECC: Integer;
  if not PDF417ReedSolomonDecode(codewords, numECCodewords, erasures,
    usedECC) then
  begin
    error := 'Checksum';
    exit;
  end;

  if not VerifyCodewordCount(codewords, numECCodewords) then
  begin
    error := 'Format';
    exit;
  end;

  Result := Decode(codewords, microPDF417, error, extra);
  if (Result <> nil) then
    Result.ECLevel := IntToStr(numECCodewords * 100 div Length(codewords)) + '%';
end;

end.
