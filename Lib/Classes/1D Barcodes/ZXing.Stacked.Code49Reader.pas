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

  * Code 49 for ZXing.Delphi: zxing-cpp, ZXing Java and ZXing.Net have no
  * reader. The encoding and the tables as in zint (code49.c, code49.h,
  * after ANSI/AIM BC6-2000).
}

unit ZXing.Stacked.Code49Reader;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.BinaryBitmap,
  ZXing.Common.BitMatrix,
  ZXing.Common.Pattern,
  ZXing.Stacked.StackedReader;

type
  /// <summary>
  /// Reads Code 49: 2 to 8 rows of 4 symbol characters (each 2 code
  /// characters of 0 to 48), each row with its check character and a parity
  /// pattern that tells its number; the last row has the number of rows,
  /// the mode and the check characters X, Y (and Z) of the whole symbol.
  /// Horizontal, also upside down; vertical with TRY_HARDER.
  /// </summary>
  TCode49Reader = class(TStackedRowReader)
  public type
    TRow = record
      Codes: TArray<Integer>;
      // the row number, -1 the last row
      Index: Integer;
      XStart, XStop: Double;
      Y: Integer;
    end;
  private
    FRows: TList<TRow>;
  protected
    procedure ReadRows(const runs: TPatternRow; y, width: Integer;
      reversed: Boolean); override;
    procedure AddSymbols(results: TList<TReadResult>; maxCount: Integer;
      vertical: Boolean); override;
    procedure ClearRows; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

/// <summary>The text of the code characters of a Code 49 (8 per row, the
/// rows in the order of their numbers) and whether it starts with FNC1
/// (GS1); '' when they are none (row and symbol checks).</summary>
/// <summary>The 16 modules of the symbol character of value (0 to 2400) of
/// even or odd parity, the highest bit the first module (for encoders and
/// tests).</summary>
function Code49CharacterPattern(value: Integer; even: Boolean): Word;

function DecodeCode49(const codes: TArray<Integer>;
  out fnc1First: Boolean): string;

implementation

uses
  ZXing.ResultPoint;

const
  // the code characters: 0-9, A-Z, - . space $ / + %, shift 1 (!), shift 2
  // (&), FNC1 (*), FNC2, FNC3, numeric shift
  CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%!&*';
  SHIFT_1 = 43;
  SHIFT_2 = 44;
  FNC_1 = 45;
  NUMERIC_SHIFT = 48;
  // Table 7: the ASCII characters (a shift and a character, or one
  // character)
  ASCII_CHARS: array [0 .. 127] of string = (
    '! ', '!A', '!B', '!C', '!D', '!E', '!F', '!G', '!H', '!I', '!J', '!K',
    '!L', '!M', '!N', '!O', '!P', '!Q', '!R', '!S', '!T', '!U', '!V', '!W',
    '!X', '!Y', '!Z', '!1', '!2', '!3', '!4', '!5', '  ', '!6', '!7', '!8',
    '$ ', '% ', '!9', '!0', '!-', '!.', '!$', '+ ', '!/', '- ', '. ', '/ ',
    '0 ', '1 ', '2 ', '3 ', '4 ', '5 ', '6 ', '7 ', '8 ', '9 ', '!+', '&1',
    '&2', '&3', '&4', '&5', '&6', 'A ', 'B ', 'C ', 'D ', 'E ', 'F ', 'G ',
    'H ', 'I ', 'J ', 'K ', 'L ', 'M ', 'N ', 'O ', 'P ', 'Q ', 'R ', 'S ',
    'T ', 'U ', 'V ', 'W ', 'X ', 'Y ', 'Z ', '&7', '&8', '&9', '&0', '&-',
    '&.', '&A', '&B', '&C', '&D', '&E', '&F', '&G', '&H', '&I', '&J', '&K',
    '&L', '&M', '&N', '&O', '&P', '&Q', '&R', '&S', '&T', '&U', '&V', '&W',
    '&X', '&Y', '&Z', '&$', '&/', '&+', '&%', '& ');
  // Table 5: the weights of the check characters
  X_WEIGHTS: array [0 .. 31] of Integer = (1, 9, 31, 26, 2, 12, 17, 23, 37,
    18, 22, 6, 27, 44, 15, 43, 39, 11, 13, 5, 41, 33, 36, 8, 4, 32, 3, 19,
    40, 25, 29, 10);
  Y_WEIGHTS: array [0 .. 31] of Integer = (9, 31, 26, 2, 12, 17, 23, 37, 18,
    22, 6, 27, 44, 15, 43, 39, 11, 13, 5, 41, 33, 36, 8, 4, 32, 3, 19, 40, 25,
    29, 10, 24);
  Z_WEIGHTS: array [0 .. 31] of Integer = (31, 26, 2, 12, 17, 23, 37, 18, 22,
    6, 27, 44, 15, 43, 39, 11, 13, 5, 41, 33, 36, 8, 4, 32, 3, 19, 40, 25, 29,
    10, 24, 30);
  // Table 4: the parity of the symbol characters of the rows (the last row
  // even only)
  ROW_PARITY: array [0 .. 7] of string = ('OEEO', 'EOEO', 'OOEE', 'EEOO',
    'OEOE', 'EOOE', 'OOOO', 'EEEE');
  // Appendix E: the patterns of the symbol characters (even parity), the
  // highest bit the first module
  PATTERNS_EVEN: array [0 .. 2400] of Word = (
    $BE5C, $C16E, $86DC, $C126, $864C, $9EDC, $C726, $9E4C, $DF26, $82CC,
    $8244, $8ECC, $C322, $8E44, $BECC, $CF22, $BE44, $C162, $86C4, $C762,
    $9EC4, $DF62, $812E, $872E, $9F2E, $836E, $8326, $8F6E, $8F26, $BF6E,
    $8166, $8122, $8766, $8722, $9F66, $9F22, $8362, $8F62, $BF62, $A2E0,
    $E8B8, $FA2E, $D370, $F4DC, $D130, $F44C, $AEE0, $EBB8, $FAEE, $A660,
    $E998, $FA66, $A220, $E888, $FA22, $D730, $F5CC, $D310, $F4C4, $AE20,
    $EB88, $FAE2, $9170, $E45C, $D8B8, $F62E, $C9B8, $F26E, $B370, $C898,
    $F226, $B130, $EC4C, $9770, $E5DC, $9330, $E4CC, $9110, $E444, $D888,
    $F622, $CB98, $F2E6, $B730, $C988, $F262, $B310, $ECC4, $9710, $E5C4,
    $DB88, $F6E2, $88B8, $E22E, $CC5C, $B8B8, $EE2E, $C4DC, $99B8, $C44C,
    $9898, $E626, $DC4C, $8BB8, $E2EE, $8998, $E266, $BBB8, $8888, $E222,
    $B998, $CC44, $B888, $EE22, $C5CC, $9B98, $C4C4, $9988, $E662, $DCC4,
    $8B88, $E2E2, $CDC4, $BB88, $EEE2, $845C, $C62E, $9C5C, $DE2E, $C26E,
    $8CDC, $C226, $8C4C, $BCDC, $CE26, $BC4C, $85DC, $84CC, $9DDC, $8444,
    $9CCC, $C622, $9C44, $DE22, $C2E6, $8DCC, $C262, $8CC4, $BDCC, $CE62,
    $BCC4, $85C4, $C6E2, $9DC4, $DEE2, $822E, $8E2E, $866E, $8626, $9E6E,
    $9E26, $82EE, $8266, $8EEE, $8222, $8E66, $BEEE, $8E22, $BE66, $86E6,
    $8662, $9EE6, $9E62, $82E2, $8EE2, $BEE2, $A170, $E85C, $D1B8, $F46E,
    $D098, $F426, $A770, $E9DC, $A330, $E8CC, $A110, $E844, $D7B8, $F5EE,
    $D398, $F4E6, $D188, $F462, $AF30, $EBCC, $A710, $E9C4, $D788, $F5E2,
    $90B8, $E42E, $D85C, $C8DC, $B1B8, $C84C, $B098, $EC26, $93B8, $E4EE,
    $9198, $E466, $9088, $E422, $D844, $CBDC, $B7B8, $C9CC, $B398, $C8C4,
    $B188, $EC62, $9798, $E5E6, $9388, $E4E2, $D9C4, $CBC4, $B788, $EDE2,
    $885C, $CC2E, $B85C, $C46E, $98DC, $C426, $984C, $DC26, $89DC, $88CC,
    $B9DC, $8844, $B8CC, $CC22, $B844, $C5EE, $9BDC, $C4E6, $99CC, $C462,
    $98C4, $DC62, $8BCC, $89C4, $BBCC, $CCE2, $B9C4, $C5E2, $9BC4, $DDE2,
    $842E, $9C2E, $8C6E, $8C26, $BC6E, $84EE, $8466, $9CEE, $8422, $9C66,
    $9C22, $8DEE, $8CE6, $BDEE, $8C62, $BCE6, $BC62, $85E6, $84E2, $9DE6,
    $9CE2, $8DE2, $BDE2, $A0B8, $E82E, $D0DC, $D04C, $A3B8, $E8EE, $A198,
    $E866, $A088, $E822, $D3DC, $D1CC, $D0C4, $AFB8, $EBEE, $A798, $E9E6,
    $A388, $E8E2, $D7CC, $D3C4, $905C, $D82E, $C86E, $B0DC, $C826, $B04C,
    $91DC, $90CC, $9044, $D822, $C9EE, $B3DC, $C8E6, $B1CC, $C862, $B0C4,
    $97DC, $93CC, $91C4, $D8E2, $CBE6, $B7CC, $C9E2, $B3C4, $882E, $986E,
    $9826, $88EE, $8866, $B8EE, $8822, $B866, $99EE, $98E6, $9862, $8BEE,
    $89E6, $BBEE, $88E2, $B9E6, $B8E2, $9BE6, $99E2, $A05C, $D06E, $D026,
    $A1DC, $A0CC, $A044, $D1EE, $D0E6, $D062, $A7DC, $A3CC, $A1C4, $D7EE,
    $D3E6, $D1E2, $902E, $B06E, $90EE, $9066, $9022, $B1EE, $B0E6, $B062,
    $93EE, $91E6, $90E2, $B7EE, $B3E6, $B1E2, $A9C0, $EA70, $FA9C, $D460,
    $F518, $FD46, $A840, $EA10, $FA84, $ED78, $FB5E, $94E0, $E538, $F94E,
    $DA70, $F69C, $CA30, $F28C, $B460, $ED18, $FB46, $9420, $E508, $F942,
    $DA10, $F684, $9AF0, $E6BC, $DD78, $F75E, $8A70, $E29C, $CD38, $F34E,
    $BA70, $EE9C, $C518, $F146, $9A30, $E68C, $DD18, $F746, $8A10, $E284,
    $CD08, $F342, $BA10, $EE84, $8D78, $E35E, $CEBC, $BD78, $EF5E, $8538,
    $E14E, $C69C, $9D38, $E74E, $DE9C, $C28C, $8D18, $E346, $CE8C, $BD18,
    $EF46, $8508, $E142, $C684, $9D08, $E742, $DE84, $86BC, $C75E, $9EBC,
    $DF5E, $829C, $C34E, $8E9C, $CF4E, $BE9C, $C146, $868C, $C746, $9E8C,
    $DF46, $8284, $C342, $8E84, $CF42, $BE84, $835E, $8F5E, $BF5E, $814E,
    $874E, $9F4E, $8346, $8F46, $BF46, $8142, $8742, $9F42, $D2F0, $F4BC,
    $ADE0, $EB78, $FADE, $A4E0, $E938, $FA4E, $D670, $F59C, $D230, $F48C,
    $AC60, $EB18, $FAC6, $A420, $E908, $FA42, $D610, $F584, $C978, $F25E,
    $B2F0, $ECBC, $96F0, $E5BC, $9270, $E49C, $D938, $F64E, $CB38, $F2CE,
    $B670, $C918, $F246, $B230, $EC8C, $9630, $E58C, $9210, $E484, $D908,
    $F642, $CB08, $F2C2, $B610, $ED84, $C4BC, $9978, $E65E, $DCBC, $8B78,
    $E2DE, $8938, $E24E, $BB78, $CC9C, $B938, $EE4E, $C59C, $9B38, $C48C,
    $9918, $E646, $DC8C, $8B18, $E2C6, $8908, $E242, $BB18, $CC84, $B908,
    $EE42, $C584, $9B08, $E6C2, $DD84, $C25E, $8CBC, $CE5E, $BCBC, $85BC,
    $849C, $9DBC, $C64E, $9C9C, $DE4E, $C2CE, $8D9C, $C246, $8C8C, $BD9C,
    $CE46, $BC8C, $858C, $8484, $9D8C, $C642, $9C84, $DE42, $C2C2, $8D84,
    $CEC2, $BD84, $865E, $9E5E, $82DE, $824E, $8EDE, $8E4E, $BEDE, $BE4E,
    $86CE, $8646, $9ECE, $9E46, $82C6, $8242, $8EC6, $8E42, $BEC6, $BE42,
    $86C2, $9EC2, $D178, $F45E, $A6F0, $E9BC, $A270, $E89C, $D778, $F5DE,
    $D338, $F4CE, $D118, $F446, $AE70, $EB9C, $A630, $E98C, $A210, $E884,
    $D718, $F5C6, $D308, $F4C2, $AE10, $EB84, $C8BC, $B178, $EC5E, $9378,
    $E4DE, $9138, $E44E, $D89C, $CBBC, $B778, $C99C, $B338, $C88C, $B118,
    $EC46, $9738, $E5CE, $9318, $E4C6, $9108, $E442, $D884, $CB8C, $B718,
    $C984, $B308, $ECC2, $9708, $E5C2, $DB84, $C45E, $98BC, $DC5E, $89BC,
    $889C, $B9BC, $CC4E, $B89C, $C5DE, $9BBC, $C4CE, $999C, $C446, $988C,
    $DC46, $8B9C, $898C, $BB9C, $8884, $B98C, $CC42, $B884, $C5C6, $9B8C,
    $C4C2, $9984, $DCC2, $8B84, $CDC2, $BB84, $8C5E, $BC5E, $84DE, $844E,
    $9CDE, $9C4E, $8DDE, $8CCE, $BDDE, $8C46, $BCCE, $BC46, $85CE, $84C6,
    $9DCE, $8442, $9CC6, $9C42, $8DC6, $8CC2, $BDC6, $BCC2, $85C2, $9DC2,
    $D0BC, $A378, $E8DE, $A138, $E84E, $D3BC, $D19C, $D08C, $AF78, $EBDE,
    $A738, $E9CE, $A318, $E8C6, $A108, $E842, $D79C, $D38C, $D184, $AF18,
    $EBC6, $A708, $E9C2, $C85E, $B0BC, $91BC, $909C, $D84E, $C9DE, $B3BC,
    $C8CE, $B19C, $C846, $B08C, $97BC, $939C, $918C, $9084, $D842, $CBCE,
    $B79C, $C9C6, $B38C, $C8C2, $B184, $978C, $9384, $D9C2, $985E, $88DE,
    $884E, $B8DE, $B84E, $99DE, $98CE, $9846, $8BDE, $89CE, $BBDE, $88C6,
    $B9CE, $8842, $B8C6, $B842, $9BCE, $99C6, $98C2, $8BC6, $89C2, $BBC6,
    $B9C2, $D05E, $A1BC, $A09C, $D1DE, $D0CE, $D046, $A7BC, $A39C, $A18C,
    $A084, $D7DE, $D3CE, $D1C6, $D0C2, $AF9C, $A78C, $A384, $B05E, $90DE,
    $904E, $B1DE, $B0CE, $B046, $93DE, $91CE, $90C6, $9042, $B7DE, $B3CE,
    $B1C6, $B0C2, $97CE, $93C6, $91C2, $A0DE, $A04E, $A3DE, $A1CE, $A0C6,
    $A042, $AFDE, $A7CE, $A3C6, $A1C2, $D4F0, $F53C, $A8E0, $EA38, $FA8E,
    $D430, $F50C, $A820, $EA08, $FA82, $DAF8, $F6BE, $CA78, $F29E, $B4F0,
    $ED3C, $9470, $E51C, $DA38, $F68E, $CA18, $F286, $B430, $ED0C, $9410,
    $E504, $DA08, $F682, $CD7C, $BAF8, $EEBE, $C53C, $9A78, $E69E, $DD3C,
    $8A38, $E28E, $CD1C, $BA38, $EE8E, $C50C, $9A18, $E686, $DD0C, $8A08,
    $E282, $CD04, $BA08, $EE82, $C6BE, $9D7C, $DEBE, $C29E, $8D3C, $CE9E,
    $BD3C, $851C, $C68E, $9D1C, $DE8E, $C286, $8D0C, $CE86, $BD0C, $8504,
    $C682, $9D04, $DE82, $8EBE, $BEBE, $869E, $9E9E, $828E, $8E8E, $BE8E,
    $8686, $9E86, $8282, $8E82, $BE82, $E97C, $D6F8, $F5BE, $D278, $F49E,
    $ACF0, $EB3C, $A470, $E91C, $D638, $F58E, $D218, $F486, $AC30, $EB0C,
    $A410, $E904, $D608, $F582, $92F8, $E4BE, $D97C, $CB7C, $B6F8, $C93C,
    $B278, $EC9E, $9678, $E59E, $9238, $E48E, $D91C, $CB1C, $B638, $C90C,
    $B218, $EC86, $9618, $E586, $9208, $E482, $D904, $CB04, $B608, $ED82,
    $897C, $CCBE, $B97C, $C5BE, $9B7C, $C49E, $993C, $DC9E, $8B3C, $891C,
    $BB3C, $CC8E, $B91C, $C58E, $9B1C, $C486, $990C, $DC86, $8B0C, $8904,
    $BB0C, $CC82, $B904, $C582, $9B04, $DD82, $84BE, $9CBE, $8DBE, $8C9E,
    $BDBE, $BC9E, $859E, $848E, $9D9E, $9C8E, $8D8E, $8C86, $BD8E, $BC86,
    $8586, $8482, $9D86, $9C82, $8D82, $BD82, $A2F8, $E8BE, $D37C, $D13C,
    $AEF8, $EBBE, $A678, $E99E, $A238, $E88E, $D73C, $D31C, $D10C, $AE38,
    $EB8E, $A618, $E986, $A208, $E882, $D70C, $D304, $917C, $D8BE, $C9BE,
    $B37C, $C89E, $B13C, $977C, $933C, $911C, $D88E, $CB9E, $B73C, $C98E,
    $B31C, $C886, $B10C, $971C, $930C, $9104, $D882, $CB86, $B70C, $C982,
    $B304, $88BE, $B8BE, $99BE, $989E, $8BBE, $899E, $BBBE, $888E, $B99E,
    $B88E, $9B9E, $998E, $9886, $8B8E, $8986, $BB8E, $8882, $B986, $B882,
    $9B86, $9982, $A17C, $D1BE, $D09E, $A77C, $A33C, $A11C, $D7BE, $D39E,
    $D18E, $D086, $AF3C, $A71C, $A30C, $A104, $D78E, $D386, $D182, $90BE,
    $B1BE, $B09E, $93BE, $919E, $908E, $B7BE, $B39E, $B18E, $B086, $979E,
    $938E, $9186, $9082, $B78E, $B386, $B182, $A0BE, $A3BE, $A19E, $A08E,
    $AFBE, $A79E, $A38E, $A186, $A082, $A9F0, $EA7C, $D478, $F51E, $A870,
    $EA1C, $D418, $F506, $A810, $EA04, $ED7E, $94F8, $E53E, $DA7C, $CA3C,
    $B478, $ED1E, $9438, $E50E, $DA1C, $CA0C, $B418, $ED06, $9408, $E502,
    $DA04, $9AFC, $DD7E, $8A7C, $CD3E, $BA7C, $C51E, $9A3C, $DD1E, $8A1C,
    $CD0E, $BA1C, $C506, $9A0C, $DD06, $8A04, $CD02, $BA04, $8D7E, $BD7E,
    $853E, $9D3E, $8D1E, $BD1E, $850E, $9D0E, $8D06, $BD06, $8502, $9D02,
    $D2FC, $ADF8, $EB7E, $A4F8, $E93E, $D67C, $D23C, $AC78, $EB1E, $A438,
    $E90E, $D61C, $D20C, $AC18, $EB06, $A408, $E902, $C97E, $B2FC, $96FC,
    $927C, $D93E, $CB3E, $B67C, $C91E, $B23C, $963C, $921C, $D90E, $CB0E,
    $B61C, $C906, $B20C, $960C, $9204, $D902, $997E, $8B7E, $893E, $BB7E,
    $B93E, $E4A0, $F928, $D940, $F650, $FD94, $CB40, $F2D0, $EDA0, $FB68,
    $8940, $E250, $CCA0, $F328, $B940, $EE50, $FB94, $C5A0, $F168, $9B40,
    $E6D0, $F9B4, $DDA0, $F768, $FDDA, $84A0, $E128, $C650, $F194, $9CA0,
    $E728, $F9CA, $DE50, $F794, $C2D0, $8DA0, $E368, $CED0, $F3B4, $BDA0,
    $EF68, $FBDA, $8250, $C328, $8E50, $E394, $CF28, $F3CA, $BE50, $EF94,
    $C168, $86D0, $E1B4, $C768, $F1DA, $9ED0, $E7B4, $DF68, $F7DA, $8128,
    $C194, $8728, $E1CA, $C794, $9F28, $E7CA, $8368, $C3B4, $8F68, $E3DA,
    $CFB4, $BF68, $EFDA, $E8A0, $FA28, $D340, $F4D0, $FD34, $EBA0, $FAE8,
    $9140, $E450, $F914, $D8A0, $F628, $FD8A, $C9A0, $F268, $B340, $ECD0,
    $FB34, $9740, $E5D0, $F974, $DBA0, $F6E8, $FDBA, $88A0, $E228, $CC50,
    $F314, $B8A0, $EE28, $FB8A, $C4D0, $F134, $99A0, $E668, $F99A, $DCD0,
    $F734, $8BA0, $E2E8, $CDD0, $F374, $BBA0, $EEE8, $FBBA, $8450, $E114,
    $C628, $F18A, $9C50, $E714, $DE28, $F78A, $C268, $8CD0, $E334, $CE68,
    $F39A, $BCD0, $EF34, $85D0, $E174, $C6E8, $F1BA, $9DD0, $E774, $DEE8,
    $F7BA, $8228, $C314, $8E28, $E38A, $CF14, $C134, $8668, $E19A, $C734,
    $9E68, $E79A, $DF34, $82E8, $C374, $8EE8, $E3BA, $CF74, $BEE8, $EFBA,
    $8114, $C18A, $8714, $C78A, $8334, $C39A, $8F34, $CF9A, $8174, $C1BA,
    $8774, $C7BA, $9F74, $DFBA, $A140, $E850, $FA14, $D1A0, $F468, $FD1A,
    $A740, $E9D0, $FA74, $D7A0, $F5E8, $FD7A, $90A0, $E428, $F90A, $D850,
    $F614, $C8D0, $F234, $B1A0, $EC68, $FB1A, $93A0, $E4E8, $F93A, $D9D0,
    $F674, $CBD0, $F2F4, $B7A0, $EDE8, $FB7A, $8850, $E214, $CC28, $F30A,
    $B850, $EE14, $C468, $F11A, $98D0, $E634, $DC68, $F71A, $89D0, $E274,
    $CCE8, $F33A, $B9D0, $EE74, $C5E8, $F17A, $9BD0, $E6F4, $DDE8, $F77A,
    $8428, $E10A, $C614, $9C28, $E70A, $C234, $8C68, $E31A, $CE34, $BC68,
    $EF1A, $84E8, $E13A, $C674, $9CE8, $E73A, $DE74, $C2F4, $8DE8, $E37A,
    $CEF4, $BDE8, $EF7A, $8214, $C30A, $8E14, $C11A, $8634, $C71A, $9E34,
    $8274, $C33A, $8E74, $CF3A, $BE74, $C17A, $86F4, $C77A, $9EF4, $DF7A,
    $810A, $870A, $831A, $8F1A, $813A, $873A, $9F3A, $837A, $8F7A, $BF7A,
    $A0A0, $E828, $FA0A, $D0D0, $F434, $A3A0, $E8E8, $FA3A, $D3D0, $F4F4,
    $AFA0, $EBE8, $FAFA, $9050, $E414, $D828, $F60A, $C868, $F21A, $B0D0,
    $EC34, $91D0, $E474, $D8E8, $F63A, $C9E8, $F27A, $B3D0, $ECF4, $97D0,
    $E5F4, $DBE8, $F6FA, $8828, $E20A, $CC14, $C434, $9868, $E61A, $DC34,
    $88E8, $E23A, $CC74, $B8E8, $EE3A, $C4F4, $99E8, $E67A, $DCF4, $8BE8,
    $E2FA, $CDF4, $BBE8, $EEFA, $8414, $C60A, $C21A, $8C34, $CE1A, $8474,
    $C63A, $9C74, $DE3A, $C27A, $8CF4, $CE7A, $BCF4, $85F4, $C6FA, $9DF4,
    $DEFA, $820A, $861A, $823A, $8E3A, $867A, $9E7A, $82FA, $8EFA, $BEFA,
    $A050, $E814, $D068, $F41A, $A1D0, $E874, $D1E8, $F47A, $A7D0, $E9F4,
    $D7E8, $F5FA, $9028, $E40A, $C834, $B068, $EC1A, $90E8, $E43A, $D874,
    $C8F4, $B1E8, $EC7A, $93E8, $E4FA, $D9F4, $CBF4, $B7E8, $EDFA, $8814,
    $C41A, $9834, $8874, $CC3A, $B874, $C47A, $98F4, $DC7A, $89F4, $CCFA,
    $B9F4, $C5FA, $9BF4, $DDFA, $840A, $8C1A, $843A, $9C3A, $8C7A, $BC7A,
    $84FA, $9CFA, $8DFA, $BDFA, $EA40, $FA90, $ED60, $FB58, $E520, $F948,
    $DA40, $F690, $FDA4, $9AC0, $E6B0, $F9AC, $DD60, $F758, $FDD6, $8A40,
    $E290, $CD20, $F348, $BA40, $EE90, $FBA4, $8D60, $E358, $CEB0, $F3AC,
    $BD60, $EF58, $FBD6, $8520, $E148, $C690, $F1A4, $9D20, $E748, $F9D2,
    $DE90, $F7A4, $86B0, $E1AC, $C758, $F1D6, $9EB0, $E7AC, $DF58, $F7D6,
    $8290, $C348, $8E90, $E3A4, $CF48, $F3D2, $BE90, $EFA4, $8358, $C3AC,
    $8F58, $E3D6, $CFAC, $BF58, $EFD6, $8148, $C1A4, $8748, $E1D2, $C7A4,
    $9F48, $E7D2, $DFA4, $D2C0, $F4B0, $FD2C, $EB60, $FAD8, $E920, $FA48,
    $D640, $F590, $FD64, $C960, $F258, $B2C0, $ECB0, $FB2C, $96C0, $E5B0,
    $F96C, $9240, $E490, $F924, $D920, $F648, $FD92, $CB20, $F2C8, $B640,
    $ED90, $FB64, $C4B0, $F12C, $9960, $E658, $F996, $DCB0, $F72C, $8B60,
    $E2D8, $8920, $E248, $BB60, $CC90, $F324, $B920, $EE48, $FB92, $C590,
    $F164, $9B20, $E6C8, $F9B2, $DD90, $F764, $C258, $8CB0, $E32C, $CE58,
    $F396, $BCB0, $EF2C, $85B0, $E16C, $8490, $E124, $9DB0, $C648, $F192,
    $9C90, $E724, $DE48, $F792, $C2C8, $8D90, $E364, $CEC8, $F3B2, $BD90,
    $EF64, $C12C, $8658, $E196, $C72C, $9E58, $E796, $DF2C, $82D8, $8248,
    $8ED8, $C324, $8E48, $E392, $BED8, $CF24, $BE48, $EF92, $C164, $86C8,
    $E1B2, $C764, $9EC8, $E7B2, $DF64, $832C, $C396, $8F2C, $CF96, $816C,
    $8124, $876C, $C192, $8724, $9F6C, $C792, $9F24, $8364, $C3B2, $8F64,
    $CFB2, $BF64, $D160, $F458, $FD16, $A6C0, $E9B0, $FA6C, $A240, $E890,
    $FA24, $D760, $F5D8, $FD76, $D320, $F4C8, $FD32, $AE40, $EB90, $FAE4,
    $C8B0, $F22C, $B160, $EC58, $FB16, $9360, $E4D8, $F936, $9120, $E448,
    $F912, $D890, $F624, $CBB0, $F2EC, $B760, $C990, $F264, $B320, $ECC8,
    $FB32, $9720, $E5C8, $F972, $DB90, $F6E4, $C458, $F116, $98B0, $E62C,
    $DC58, $F716, $89B0, $E26C, $8890, $E224, $B9B0, $CC48, $F312, $B890,
    $EE24, $C5D8, $F176, $9BB0, $C4C8, $F132, $9990, $E664, $DCC8, $F732,
    $8B90, $E2E4, $CDC8, $F372, $BB90, $EEE4, $C22C, $8C58, $E316, $CE2C,
    $BC58, $EF16, $84D8, $E136, $8448, $E112, $9CD8, $C624, $9C48, $E712,
    $DE24, $C2EC, $8DD8, $C264, $8CC8, $E332, $BDD8, $CE64, $BCC8, $EF32,
    $85C8, $E172, $C6E4, $9DC8, $E772, $DEE4, $C116, $862C, $C716, $9E2C,
    $826C, $8224, $8E6C, $C312, $8E24, $BE6C, $CF12, $C176, $86EC, $C132,
    $8664, $9EEC, $C732, $9E64, $DF32, $82E4, $C372, $8EE4, $CF72, $BEE4,
    $8316, $8F16, $8136, $8112, $8736, $8712, $9F36, $8376, $8332, $8F76,
    $8F32, $BF76, $8172, $8772, $9F72, $D0B0, $F42C, $A360, $E8D8, $FA36,
    $A120, $E848, $FA12, $D3B0, $F4EC, $D190, $F464, $AF60, $EBD8, $FAF6,
    $A720, $E9C8, $FA72, $D790, $F5E4, $C858, $F216, $B0B0, $EC2C, $91B0,
    $E46C, $9090, $E424, $D848, $F612, $C9D8, $F276, $B3B0, $C8C8, $F232,
    $B190, $EC64, $97B0, $E5EC, $9390, $E4E4, $D9C8, $F672, $CBC8, $F2F2,
    $B790, $EDE4, $C42C, $9858, $E616, $DC2C, $88D8, $E236, $8848, $E212,
    $B8D8, $CC24, $B848, $EE12, $C4EC, $99D8, $C464, $98C8, $E632, $DC64,
    $8BD8, $E2F6, $89C8, $E272, $BBD8, $CCE4, $B9C8, $EE72, $C5E4, $9BC8,
    $E6F2, $DDE4, $C216, $8C2C, $CE16, $846C, $8424, $9C6C, $C612, $9C24,
    $C276, $8CEC, $C232, $8C64, $BCEC, $CE32, $BC64, $85EC, $84E4, $9DEC,
    $C672, $9CE4, $DE72, $C2F2, $8DE4, $CEF2, $BDE4, $8616, $8236, $8212,
    $8E36, $8E12, $8676, $8632, $9E76, $9E32, $82F6, $8272, $8EF6, $8E72,
    $BEF6, $BE72, $86F2, $9EF2, $D058, $F416, $A1B0, $E86C, $A090, $E824,
    $D1D8, $F476, $D0C8, $F432, $A7B0, $E9EC, $A390, $E8E4, $D7D8, $F5F6,
    $D3C8, $F4F2, $AF90, $EBE4, $C82C, $B058, $EC16, $90D8, $E436, $9048,
    $E412, $D824, $C8EC, $B1D8, $C864, $B0C8, $EC32, $93D8, $E4F6, $91C8,
    $E472, $D8E4, $CBEC, $B7D8, $C9E4, $B3C8, $ECF2, $97C8, $E5F2, $DBE4,
    $C416, $982C, $886C, $8824, $B86C, $CC12, $C476, $98EC, $C432, $9864,
    $DC32, $89EC, $88E4, $B9EC, $CC72, $B8E4, $C5F6, $9BEC, $C4F2, $99E4,
    $DCF2, $8BE4, $CDF2, $BBE4, $8C16, $8436, $8412, $9C36, $8C76, $8C32,
    $BC76, $84F6, $8472, $9CF6, $9C72, $8DF6, $8CF2, $BDF6, $BCF2, $85F2,
    $9DF2, $D02C, $A0D8, $E836, $A048, $E812, $D0EC, $D064, $A3D8, $E8F6,
    $A1C8, $E872, $D3EC, $D1E4, $AFD8, $EBF6, $A7C8, $E9F2, $C816, $906C,
    $9024, $C876, $B0EC, $C832, $B064, $91EC, $90E4, $D872, $C9F6, $B3EC,
    $C8F2, $B1E4, $97EC, $93E4, $D9F2, $8836, $8812, $9876, $9832, $88F6,
    $8872, $B8F6, $B872, $99F6, $98F2, $8BF6, $89F2, $BBF6, $B9F2, $D4C0,
    $F530, $FD4C, $EA20, $FA88, $DAE0, $F6B8, $FDAE, $CA60, $F298, $B4C0,
    $ED30, $FB4C, $9440, $E510, $F944, $DA20, $F688, $FDA2, $CD70, $F35C,
    $BAE0, $EEB8, $FBAE, $C530, $F14C, $9A60, $E698, $F9A6, $DD30, $F74C,
    $8A20, $E288, $CD10, $F344, $BA20, $EE88, $FBA2, $C6B8, $F1AE, $9D70,
    $E75C, $DEB8, $F7AE, $C298, $8D30, $E34C, $CE98, $F3A6, $BD30, $EF4C,
    $8510, $E144, $C688, $F1A2, $9D10, $E744, $DE88, $F7A2, $C35C, $8EB8,
    $E3AE, $CF5C, $BEB8, $EFAE, $C14C, $8698, $E1A6, $C74C, $9E98, $E7A6,
    $DF4C, $8288, $C344, $8E88, $E3A2, $CF44, $BE88, $EFA2, $C1AE, $875C,
    $C7AE, $9F5C, $DFAE, $834C, $C3A6, $8F4C, $CFA6, $BF4C, $8144, $C1A2,
    $8744, $C7A2, $9F44, $DFA2, $E970, $FA5C, $D6E0, $F5B8, $FD6E, $D260,
    $F498, $FD26, $ACC0, $EB30, $FACC, $A440, $E910, $FA44, $D620, $F588,
    $FD62, $92E0, $E4B8, $F92E, $D970, $F65C, $CB70, $F2DC, $B6E0, $C930,
    $F24C, $B260, $EC98, $FB26, $9660, $E598, $F966, $9220, $E488, $F922,
    $D910, $F644, $CB10, $F2C4, $B620, $ED88, $FB62, $8970, $E25C, $CCB8,
    $F32E, $B970, $EE5C, $C5B8, $F16E, $9B70, $C498, $F126, $9930, $E64C,
    $DC98, $F726, $8B30, $E2CC, $8910, $E244, $BB30, $CC88, $F322, $B910,
    $EE44, $C588, $F162, $9B10, $E6C4, $DD88, $F762, $84B8, $E12E, $C65C,
    $9CB8, $E72E, $DE5C, $C2DC, $8DB8, $C24C, $8C98, $E326, $BDB8, $CE4C,
    $BC98, $EF26, $8598, $E166, $8488, $E122, $9D98, $C644, $9C88, $E722,
    $DE44, $C2C4, $8D88, $E362, $CEC4, $BD88, $EF62, $825C, $C32E, $8E5C,
    $CF2E);
  // Appendix E: the patterns of the symbol characters (odd parity), the
  // highest bit the first module
  PATTERNS_ODD: array [0 .. 2400] of Word = (
    $C940, $F250, $ECA0, $FB28, $E5A0, $F968, $DB40, $F6D0, $FDB4, $C4A0,
    $F128, $9940, $E650, $F994, $DCA0, $F728, $FDCA, $8B40, $E2D0, $CDA0,
    $F368, $BB40, $EED0, $FBB4, $C250, $8CA0, $E328, $CE50, $F394, $BCA0,
    $EF28, $FBCA, $85A0, $E168, $C6D0, $F1B4, $9DA0, $E768, $F9DA, $DED0,
    $F7B4, $C128, $8650, $E194, $C728, $F1CA, $9E50, $E794, $DF28, $F7CA,
    $82D0, $C368, $8ED0, $E3B4, $CF68, $F3DA, $BED0, $EFB4, $8328, $C394,
    $8F28, $E3CA, $CF94, $8168, $C1B4, $8768, $E1DA, $C7B4, $9F68, $E7DA,
    $DFB4, $D140, $F450, $FD14, $E9A0, $FA68, $D740, $F5D0, $FD74, $C8A0,
    $F228, $B140, $EC50, $FB14, $9340, $E4D0, $F934, $D9A0, $F668, $FD9A,
    $CBA0, $F2E8, $B740, $EDD0, $FB74, $C450, $F114, $98A0, $E628, $F98A,
    $DC50, $F714, $89A0, $E268, $CCD0, $F334, $B9A0, $EE68, $FB9A, $C5D0,
    $F174, $9BA0, $E6E8, $F9BA, $DDD0, $F774, $C228, $8C50, $E314, $CE28,
    $F38A, $BC50, $EF14, $84D0, $E134, $C668, $F19A, $9CD0, $E734, $DE68,
    $F79A, $C2E8, $8DD0, $E374, $CEE8, $F3BA, $BDD0, $EF74, $C114, $8628,
    $E18A, $C714, $9E28, $E78A, $8268, $C334, $8E68, $E39A, $CF34, $BE68,
    $EF9A, $C174, $86E8, $E1BA, $C774, $9EE8, $E7BA, $DF74, $8314, $C38A,
    $8F14, $8134, $C19A, $8734, $C79A, $9F34, $8374, $C3BA, $8F74, $CFBA,
    $BF74, $D0A0, $F428, $FD0A, $A340, $E8D0, $FA34, $D3A0, $F4E8, $FD3A,
    $AF40, $EBD0, $FAF4, $C850, $F214, $B0A0, $EC28, $FB0A, $91A0, $E468,
    $F91A, $D8D0, $F634, $C9D0, $F274, $B3A0, $ECE8, $FB3A, $97A0, $E5E8,
    $F97A, $DBD0, $F6F4, $C428, $F10A, $9850, $E614, $DC28, $F70A, $88D0,
    $E234, $CC68, $F31A, $B8D0, $EE34, $C4E8, $F13A, $99D0, $E674, $DCE8,
    $F73A, $8BD0, $E2F4, $CDE8, $F37A, $BBD0, $EEF4, $C214, $8C28, $E30A,
    $CE14, $8468, $E11A, $C634, $9C68, $E71A, $DE34, $C274, $8CE8, $E33A,
    $CE74, $BCE8, $EF3A, $85E8, $E17A, $C6F4, $9DE8, $E77A, $DEF4, $C10A,
    $8614, $C70A, $8234, $C31A, $8E34, $CF1A, $C13A, $8674, $C73A, $9E74,
    $DF3A, $82F4, $C37A, $8EF4, $CF7A, $BEF4, $830A, $811A, $871A, $833A,
    $8F3A, $817A, $877A, $9F7A, $D050, $F414, $A1A0, $E868, $FA1A, $D1D0,
    $F474, $A7A0, $E9E8, $FA7A, $D7D0, $F5F4, $C828, $F20A, $B050, $EC14,
    $90D0, $E434, $D868, $F61A, $C8E8, $F23A, $B1D0, $EC74, $93D0, $E4F4,
    $D9E8, $F67A, $CBE8, $F2FA, $B7D0, $EDF4, $C414, $9828, $E60A, $8868,
    $E21A, $CC34, $B868, $EE1A, $C474, $98E8, $E63A, $DC74, $89E8, $E27A,
    $CCF4, $B9E8, $EE7A, $C5F4, $9BE8, $E6FA, $DDF4, $C20A, $8C14, $8434,
    $C61A, $9C34, $C23A, $8C74, $CE3A, $BC74, $84F4, $C67A, $9CF4, $DE7A,
    $C2FA, $8DF4, $CEFA, $BDF4, $860A, $821A, $8E1A, $863A, $9E3A, $827A,
    $8E7A, $BE7A, $86FA, $9EFA, $D028, $F40A, $A0D0, $E834, $D0E8, $F43A,
    $A3D0, $E8F4, $D3E8, $F4FA, $AFD0, $EBF4, $C814, $9068, $E41A, $D834,
    $C874, $B0E8, $EC3A, $91E8, $E47A, $D8F4, $C9F4, $B3E8, $ECFA, $97E8,
    $E5FA, $DBF4, $C40A, $8834, $CC1A, $C43A, $9874, $DC3A, $88F4, $CC7A,
    $B8F4, $C4FA, $99F4, $DCFA, $8BF4, $CDFA, $BBF4, $841A, $8C3A, $847A,
    $9C7A, $8CFA, $BCFA, $85FA, $9DFA, $F520, $FD48, $DAC0, $F6B0, $FDAC,
    $CA40, $F290, $ED20, $FB48, $CD60, $F358, $BAC0, $EEB0, $FBAC, $C520,
    $F148, $9A40, $E690, $F9A4, $DD20, $F748, $FDD2, $C6B0, $F1AC, $9D60,
    $E758, $F9D6, $DEB0, $F7AC, $C290, $8D20, $E348, $CE90, $F3A4, $BD20,
    $EF48, $FBD2, $C358, $8EB0, $E3AC, $CF58, $F3D6, $BEB0, $EFAC, $C148,
    $8690, $E1A4, $C748, $F1D2, $9E90, $E7A4, $DF48, $F7D2, $C1AC, $8758,
    $E1D6, $C7AC, $9F58, $E7D6, $DFAC, $8348, $C3A4, $8F48, $E3D2, $CFA4,
    $BF48, $EFD2, $E960, $FA58, $D6C0, $F5B0, $FD6C, $D240, $F490, $FD24,
    $EB20, $FAC8, $92C0, $E4B0, $F92C, $D960, $F658, $FD96, $CB60, $F2D8,
    $B6C0, $C920, $F248, $B240, $EC90, $FB24, $9640, $E590, $F964, $DB20,
    $F6C8, $FDB2, $8960, $E258, $CCB0, $F32C, $B960, $EE58, $FB96, $C5B0,
    $F16C, $9B60, $C490, $F124, $9920, $E648, $F992, $DC90, $F724, $8B20,
    $E2C8, $CD90, $F364, $BB20, $EEC8, $FBB2, $84B0, $E12C, $C658, $F196,
    $9CB0, $E72C, $DE58, $F796, $C2D8, $8DB0, $C248, $8C90, $E324, $BDB0,
    $CE48, $F392, $BC90, $EF24, $8590, $E164, $C6C8, $F1B2, $9D90, $E764,
    $DEC8, $F7B2, $8258, $C32C, $8E58, $E396, $CF2C, $BE58, $EF96, $C16C,
    $86D8, $C124, $8648, $E192, $9ED8, $C724, $9E48, $E792, $DF24, $82C8,
    $C364, $8EC8, $E3B2, $CF64, $BEC8, $EFB2, $812C, $C196, $872C, $C796,
    $9F2C, $836C, $8324, $8F6C, $C392, $8F24, $BF6C, $CF92, $8164, $C1B2,
    $8764, $C7B2, $9F64, $DFB2, $A2C0, $E8B0, $FA2C, $D360, $F4D8, $FD36,
    $D120, $F448, $FD12, $AEC0, $EBB0, $FAEC, $A640, $E990, $FA64, $D720,
    $F5C8, $FD72, $9160, $E458, $F916, $D8B0, $F62C, $C9B0, $F26C, $B360,
    $C890, $F224, $B120, $EC48, $FB12, $9760, $E5D8, $F976, $9320, $E4C8,
    $F932, $D990, $F664, $CB90, $F2E4, $B720, $EDC8, $FB72, $88B0, $E22C,
    $CC58, $F316, $B8B0, $EE2C, $C4D8, $F136, $99B0, $C448, $F112, $9890,
    $E624, $DC48, $F712, $8BB0, $E2EC, $8990, $E264, $BBB0, $CCC8, $F332,
    $B990, $EE64, $C5C8, $F172, $9B90, $E6E4, $DDC8, $F772, $8458, $E116,
    $C62C, $9C58, $E716, $DE2C, $C26C, $8CD8, $C224, $8C48, $E312, $BCD8,
    $CE24, $BC48, $EF12, $85D8, $E176, $84C8, $E132, $9DD8, $C664, $9CC8,
    $E732, $DE64, $C2E4, $8DC8, $E372, $CEE4, $BDC8, $EF72, $822C, $C316,
    $8E2C, $CF16, $C136, $866C, $C112, $8624, $9E6C, $C712, $9E24, $82EC,
    $8264, $8EEC, $C332, $8E64, $BEEC, $CF32, $BE64, $C172, $86E4, $C772,
    $9EE4, $DF72, $8116, $8716, $8336, $8312, $8F36, $8F12, $8176, $8132,
    $8776, $8732, $9F76, $9F32, $8372, $8F72, $BF72, $A160, $E858, $FA16,
    $D1B0, $F46C, $D090, $F424, $A760, $E9D8, $FA76, $A320, $E8C8, $FA32,
    $D7B0, $F5EC, $D390, $F4E4, $AF20, $EBC8, $FAF2, $90B0, $E42C, $D858,
    $F616, $C8D8, $F236, $B1B0, $C848, $F212, $B090, $EC24, $93B0, $E4EC,
    $9190, $E464, $D8C8, $F632, $CBD8, $F2F6, $B7B0, $C9C8, $F272, $B390,
    $ECE4, $9790, $E5E4, $DBC8, $F6F2, $8858, $E216, $CC2C, $B858, $EE16,
    $C46C, $98D8, $C424, $9848, $E612, $DC24, $89D8, $E276, $88C8, $E232,
    $B9D8, $CC64, $B8C8, $EE32, $C5EC, $9BD8, $C4E4, $99C8, $E672, $DCE4,
    $8BC8, $E2F2, $CDE4, $BBC8, $EEF2, $842C, $C616, $9C2C, $C236, $8C6C,
    $C212, $8C24, $BC6C, $CE12, $84EC, $8464, $9CEC, $C632, $9C64, $DE32,
    $C2F6, $8DEC, $C272, $8CE4, $BDEC, $CE72, $BCE4, $85E4, $C6F2, $9DE4,
    $DEF2, $8216, $8E16, $8636, $8612, $9E36, $8276, $8232, $8E76, $8E32,
    $BE76, $86F6, $8672, $9EF6, $9E72, $82F2, $8EF2, $BEF2, $A0B0, $E82C,
    $D0D8, $F436, $D048, $F412, $A3B0, $E8EC, $A190, $E864, $D3D8, $F4F6,
    $D1C8, $F472, $AFB0, $EBEC, $A790, $E9E4, $D7C8, $F5F2, $9058, $E416,
    $D82C, $C86C, $B0D8, $C824, $B048, $EC12, $91D8, $E476, $90C8, $E432,
    $D864, $C9EC, $B3D8, $C8E4, $B1C8, $EC72, $97D8, $E5F6, $93C8, $E4F2,
    $D9E4, $CBE4, $B7C8, $EDF2, $882C, $CC16, $C436, $986C, $C412, $9824,
    $88EC, $8864, $B8EC, $CC32, $B864, $C4F6, $99EC, $C472, $98E4, $DC72,
    $8BEC, $89E4, $BBEC, $CCF2, $B9E4, $C5F2, $9BE4, $DDF2, $8416, $8C36,
    $8C12, $8476, $8432, $9C76, $9C32, $8CF6, $8C72, $BCF6, $BC72, $85F6,
    $84F2, $9DF6, $9CF2, $8DF2, $BDF2, $A058, $E816, $D06C, $D024, $A1D8,
    $E876, $A0C8, $E832, $D1EC, $D0E4, $A7D8, $E9F6, $A3C8, $E8F2, $D7EC,
    $D3E4, $902C, $C836, $B06C, $C812, $90EC, $9064, $D832, $C8F6, $B1EC,
    $C872, $B0E4, $93EC, $91E4, $D8F2, $CBF6, $B7EC, $C9F2, $B3E4, $8816,
    $9836, $8876, $8832, $B876, $98F6, $9872, $89F6, $88F2, $B9F6, $B8F2,
    $9BF6, $99F2, $EA60, $FA98, $D440, $F510, $FD44, $ED70, $FB5C, $94C0,
    $E530, $F94C, $DA60, $F698, $FDA6, $CA20, $F288, $B440, $ED10, $FB44,
    $9AE0, $E6B8, $F9AE, $DD70, $F75C, $8A60, $E298, $CD30, $F34C, $BA60,
    $EE98, $FBA6, $C510, $F144, $9A20, $E688, $F9A2, $DD10, $F744, $8D70,
    $E35C, $CEB8, $F3AE, $BD70, $EF5C, $8530, $E14C, $C698, $F1A6, $9D30,
    $E74C, $DE98, $F7A6, $C288, $8D10, $E344, $CE88, $F3A2, $BD10, $EF44,
    $86B8, $E1AE, $C75C, $9EB8, $E7AE, $DF5C, $8298, $C34C, $8E98, $E3A6,
    $CF4C, $BE98, $EFA6, $C144, $8688, $E1A2, $C744, $9E88, $E7A2, $DF44,
    $835C, $C3AE, $8F5C, $CFAE, $BF5C, $814C, $C1A6, $874C, $C7A6, $9F4C,
    $DFA6, $8344, $C3A2, $8F44, $CFA2, $BF44, $D2E0, $F4B8, $FD2E, $ADC0,
    $EB70, $FADC, $A4C0, $E930, $FA4C, $D660, $F598, $FD66, $D220, $F488,
    $FD22, $AC40, $EB10, $FAC4, $C970, $F25C, $B2E0, $ECB8, $FB2E, $96E0,
    $E5B8, $F96E, $9260, $E498, $F926, $D930, $F64C, $CB30, $F2CC, $B660,
    $C910, $F244, $B220, $EC88, $FB22, $9620, $E588, $F962, $DB10, $F6C4,
    $C4B8, $F12E, $9970, $E65C, $DCB8, $F72E, $8B70, $E2DC, $8930, $E24C,
    $BB70, $CC98, $F326, $B930, $EE4C, $C598, $F166, $9B30, $C488, $F122,
    $9910, $E644, $DC88, $F722, $8B10, $E2C4, $CD88, $F362, $BB10, $EEC4,
    $C25C, $8CB8, $E32E, $CE5C, $BCB8, $EF2E, $85B8, $E16E, $8498, $E126,
    $9DB8, $C64C, $9C98, $E726, $DE4C, $C2CC, $8D98, $C244, $8C88, $E322,
    $BD98, $CE44, $BC88, $EF22, $8588, $E162, $C6C4, $9D88, $E762, $DEC4,
    $C12E, $865C, $C72E, $9E5C, $DF2E, $82DC, $824C, $8EDC, $C326, $8E4C,
    $BEDC, $CF26, $BE4C, $C166, $86CC, $C122, $8644, $9ECC, $C722, $9E44,
    $DF22, $82C4, $C362, $8EC4, $CF62, $BEC4, $832E, $8F2E, $816E, $8126,
    $876E, $8726, $9F6E, $9F26, $8366, $8322, $8F66, $8F22, $BF66, $8162,
    $8762, $9F62, $D170, $F45C, $A6E0, $E9B8, $FA6E, $A260, $E898, $FA26,
    $D770, $F5DC, $D330, $F4CC, $D110, $F444, $AE60, $EB98, $FAE6, $A620,
    $E988, $FA62, $D710, $F5C4, $C8B8, $F22E, $B170, $EC5C, $9370, $E4DC,
    $9130, $E44C, $D898, $F626, $CBB8, $F2EE, $B770, $C998, $F266, $B330,
    $C888, $F222, $B110, $EC44, $9730, $E5CC, $9310, $E4C4, $D988, $F662,
    $CB88, $F2E2, $B710, $EDC4, $C45C, $98B8, $E62E, $DC5C, $89B8, $E26E,
    $8898, $E226, $B9B8, $CC4C, $B898, $EE26, $C5DC, $9BB8, $C4CC, $9998,
    $C444, $9888, $E622, $DC44, $8B98, $E2E6, $8988, $E262, $BB98, $CCC4,
    $B988, $EE62, $C5C4, $9B88, $E6E2, $DDC4, $C22E, $8C5C, $CE2E, $BC5C,
    $84DC, $844C, $9CDC, $C626, $9C4C, $DE26, $C2EE, $8DDC, $C266, $8CCC,
    $C222, $BDDC, $8C44, $BCCC, $CE22, $BC44, $85CC, $84C4, $9DCC, $C662,
    $9CC4, $DE62, $C2E2, $8DC4, $CEE2, $BDC4, $862E, $9E2E, $826E, $8226,
    $8E6E, $8E26, $BE6E, $86EE, $8666, $9EEE, $8622, $9E66, $9E22, $82E6,
    $8262, $8EE6, $8E62, $BEE6, $BE62, $86E2, $9EE2, $D0B8, $F42E, $A370,
    $E8DC, $A130, $E84C, $D3B8, $F4EE, $D198, $F466, $D088, $F422, $AF70,
    $EBDC, $A730, $E9CC, $A310, $E8C4, $D798, $F5E6, $D388, $F4E2, $AF10,
    $EBC4, $C85C, $B0B8, $EC2E, $91B8, $E46E, $9098, $E426, $D84C, $C9DC,
    $B3B8, $C8CC, $B198, $C844, $B088, $EC22, $97B8, $E5EE, $9398, $E4E6,
    $9188, $E462, $D8C4, $CBCC, $B798, $C9C4, $B388, $ECE2, $9788, $E5E2,
    $DBC4, $C42E, $985C, $DC2E, $88DC, $884C, $B8DC, $CC26, $B84C, $C4EE,
    $99DC, $C466, $98CC, $C422, $9844, $DC22, $8BDC, $89CC, $BBDC, $88C4,
    $B9CC, $CC62, $B8C4, $C5E6, $9BCC, $C4E2, $99C4, $DCE2, $8BC4, $CDE2,
    $BBC4, $8C2E, $846E, $8426, $9C6E, $9C26, $8CEE, $8C66, $BCEE, $8C22,
    $BC66, $85EE, $84E6, $9DEE, $8462, $9CE6, $9C62, $8DE6, $8CE2, $BDE6,
    $BCE2, $85E2, $9DE2, $D05C, $A1B8, $E86E, $A098, $E826, $D1DC, $D0CC,
    $D044, $A7B8, $E9EE, $A398, $E8E6, $A188, $E862, $D7DC, $D3CC, $D1C4,
    $AF98, $EBE6, $A788, $E9E2, $C82E, $B05C, $90DC, $904C, $D826, $C8EE,
    $B1DC, $C866, $B0CC, $C822, $B044, $93DC, $91CC, $90C4, $D862, $CBEE,
    $B7DC, $C9E6, $B3CC, $C8E2, $B1C4, $97CC, $93C4, $D9E2, $982E, $886E,
    $8826, $B86E, $98EE, $9866, $9822, $89EE, $88E6, $B9EE, $8862, $B8E6,
    $B862, $9BEE, $99E6, $98E2, $8BE6, $89E2, $BBE6, $B9E2, $D02E, $A0DC,
    $A04C, $D0EE, $D066, $D022, $A3DC, $A1CC, $A0C4, $D3EE, $D1E6, $D0E2,
    $AFDC, $A7CC, $A3C4, $906E, $9026, $B0EE, $B066, $91EE, $90E6, $9062,
    $B3EE, $B1E6, $B0E2, $97EE, $93E6, $91E2, $D4E0, $F538, $FD4E, $A8C0,
    $EA30, $FA8C, $D420, $F508, $FD42, $DAF0, $F6BC, $CA70, $F29C, $B4E0,
    $ED38, $FB4E, $9460, $E518, $F946, $DA30, $F68C, $CA10, $F284, $B420,
    $ED08, $FB42, $CD78, $F35E, $BAF0, $EEBC, $C538, $F14E, $9A70, $E69C,
    $DD38, $F74E, $8A30, $E28C, $CD18, $F346, $BA30, $EE8C, $C508, $F142,
    $9A10, $E684, $DD08, $F742, $C6BC, $9D78, $E75E, $DEBC, $C29C, $8D38,
    $E34E, $CE9C, $BD38, $EF4E, $8518, $E146, $C68C, $9D18, $E746, $DE8C,
    $C284, $8D08, $E342, $CE84, $BD08, $EF42, $C35E, $8EBC, $CF5E, $BEBC,
    $C14E, $869C, $C74E, $9E9C, $DF4E, $828C, $C346, $8E8C, $CF46, $BE8C,
    $C142, $8684, $C742, $9E84, $DF42, $875E, $9F5E, $834E, $8F4E, $BF4E,
    $8146, $8746, $9F46, $8342, $8F42, $BF42, $E978, $FA5E, $D6F0, $F5BC,
    $D270, $F49C, $ACE0, $EB38, $FACE, $A460, $E918, $FA46, $D630, $F58C,
    $D210, $F484, $AC20, $EB08, $FAC2, $92F0, $E4BC, $D978, $F65E, $CB78,
    $F2DE, $B6F0, $C938, $F24E, $B270, $EC9C, $9670, $E59C, $9230, $E48C,
    $D918, $F646, $CB18, $F2C6, $B630, $C908, $F242, $B210, $EC84, $9610,
    $E584, $DB08, $F6C2, $8978, $E25E, $CCBC, $B978, $EE5E, $C5BC, $9B78,
    $C49C, $9938, $E64E, $DC9C, $8B38, $E2CE, $8918, $E246, $BB38, $CC8C,
    $B918, $EE46, $C58C, $9B18, $C484, $9908, $E642, $DC84, $8B08, $E2C2,
    $CD84, $BB08, $EEC2, $84BC, $C65E, $9CBC, $DE5E, $C2DE, $8DBC, $C24E,
    $8C9C, $BDBC, $CE4E, $BC9C, $859C, $848C, $9D9C, $C646, $9C8C, $DE46,
    $C2C6, $8D8C, $C242, $8C84, $BD8C, $CE42, $BC84, $8584, $C6C2, $9D84,
    $DEC2, $825E, $8E5E, $BE5E, $86DE, $864E, $9EDE, $9E4E, $82CE, $8246,
    $8ECE, $8E46, $BECE, $BE46, $86C6, $8642, $9EC6, $9E42, $82C2, $8EC2,
    $BEC2, $A2F0, $E8BC, $D378, $F4DE, $D138, $F44E, $AEF0, $EBBC, $A670,
    $E99C, $A230, $E88C, $D738, $F5CE, $D318, $F4C6, $D108, $F442, $AE30,
    $EB8C, $A610, $E984, $D708, $F5C2, $9178, $E45E, $D8BC, $C9BC, $B378,
    $C89C, $B138, $EC4E, $9778, $E5DE, $9338, $E4CE, $9118, $E446, $D88C,
    $CB9C, $B738, $C98C, $B318, $C884, $B108, $EC42, $9718, $E5C6, $9308,
    $E4C2, $D984, $CB84, $B708, $EDC2, $88BC, $CC5E, $B8BC, $C4DE, $99BC,
    $C44E, $989C, $DC4E, $8BBC, $899C, $BBBC, $888C, $B99C, $CC46, $B88C,
    $C5CE, $9B9C, $C4C6, $998C, $C442, $9884, $DC42, $8B8C, $8984, $BB8C,
    $CCC2, $B984, $C5C2, $9B84, $DDC2, $845E, $9C5E, $8CDE, $8C4E, $BCDE,
    $BC4E, $85DE, $84CE, $9DDE, $8446, $9CCE, $9C46, $8DCE, $8CC6, $BDCE,
    $8C42, $BCC6, $BC42, $85C6, $84C2, $9DC6, $9CC2, $8DC2, $BDC2, $A178,
    $E85E, $D1BC, $D09C, $A778, $E9DE, $A338, $E8CE, $A118, $E846, $D7BC,
    $D39C, $D18C, $D084, $AF38, $EBCE, $A718, $E9C6, $A308, $E8C2, $D78C,
    $D384, $90BC, $D85E, $C8DE, $B1BC, $C84E, $B09C, $93BC, $919C, $908C,
    $D846, $CBDE, $B7BC, $C9CE, $B39C, $C8C6, $B18C, $C842, $B084, $979C,
    $938C, $9184, $D8C2, $CBC6, $B78C, $C9C2, $B384, $885E, $B85E, $98DE,
    $984E, $89DE, $88CE, $B9DE, $8846, $B8CE, $B846, $9BDE, $99CE, $98C6,
    $9842, $8BCE, $89C6, $BBCE, $88C2, $B9C6, $B8C2, $9BC6, $99C2, $A0BC,
    $D0DE, $D04E, $A3BC, $A19C, $A08C, $D3DE, $D1CE, $D0C6, $D042, $AFBC,
    $A79C, $A38C, $A184, $D7CE, $D3C6, $D1C2, $905E, $B0DE, $B04E, $91DE,
    $90CE, $9046, $B3DE, $B1CE, $B0C6, $B042, $97DE, $93CE, $91C6, $90C2,
    $B7CE, $B3C6, $B1C2, $A05E, $A1DE, $A0CE, $A046, $A7DE, $A3CE, $A1C6,
    $A0C2, $A9E0, $EA78, $FA9E, $D470, $F51C, $A860, $EA18, $FA86, $D410,
    $F504, $ED7C, $94F0, $E53C, $DA78, $F69E, $CA38, $F28E, $B470, $ED1C,
    $9430, $E50C, $DA18, $F686, $CA08, $F282, $B410, $ED04, $9AF8, $E6BE,
    $DD7C, $8A78, $E29E, $CD3C, $BA78, $EE9E, $C51C, $9A38, $E68E, $DD1C,
    $8A18, $E286, $CD0C, $BA18, $EE86, $C504, $9A08, $E682, $DD04, $8D7C,
    $CEBE, $BD7C, $853C, $C69E, $9D3C, $DE9E, $C28E, $8D1C, $CE8E, $BD1C,
    $850C, $C686, $9D0C, $DE86, $C282, $8D04, $CE82, $BD04, $86BE, $9EBE,
    $829E, $8E9E, $BE9E, $868E, $9E8E, $8286, $8E86, $BE86, $8682, $9E82,
    $D2F8, $F4BE, $ADF0, $EB7C, $A4F0, $E93C, $D678, $F59E, $D238, $F48E,
    $AC70, $EB1C, $A430, $E90C, $D618, $F586, $D208, $F482, $AC10, $EB04,
    $C97C, $B2F8, $ECBE, $96F8, $E5BE, $9278, $E49E, $D93C, $CB3C, $B678,
    $C91C, $B238, $EC8E, $9638, $E58E, $9218, $E486, $D90C, $CB0C, $B618,
    $C904, $B208, $EC82, $9608, $E582, $DB04, $C4BE, $997C, $DCBE, $8B7C,
    $893C, $BB7C, $CC9E, $B93C, $C59E, $9B3C, $C48E, $991C, $DC8E, $8B1C,
    $890C, $BB1C, $CC86, $B90C, $C586, $9B0C, $C482, $9904, $DC82, $8B04,
    $CD82, $BB04, $8CBE, $BCBE, $85BE, $849E, $9DBE, $9C9E, $8D9E, $8C8E,
    $BD9E, $BC8E, $858E, $8486, $9D8E, $9C86, $8D86, $8C82, $BD86, $BC82,
    $8582, $9D82, $D17C, $A6F8, $E9BE, $A278, $E89E, $D77C, $D33C, $D11C,
    $AE78, $EB9E, $A638, $E98E, $A218, $E886, $D71C, $D30C, $D104, $AE18,
    $EB86, $A608, $E982, $C8BE, $B17C, $937C, $913C, $D89E, $CBBE, $B77C,
    $C99E, $B33C, $C88E, $B11C, $973C, $931C, $910C, $D886, $CB8E, $B71C,
    $C986, $B30C, $C882, $B104, $970C, $9304, $D982, $98BE, $89BE, $889E,
    $B9BE, $B89E, $9BBE, $999E, $988E, $8B9E, $898E, $BB9E, $8886, $B98E,
    $B886, $9B8E, $9986, $9882, $8B86, $8982, $BB86, $B982, $D0BE, $A37C,
    $A13C, $D3BE, $D19E, $D08E, $AF7C, $A73C, $A31C, $A10C, $D79E, $D38E,
    $D186, $D082, $AF1C, $A70C, $A304, $B0BE, $91BE, $909E, $B3BE, $B19E,
    $B08E, $97BE, $939E, $918E, $9086, $B79E, $B38E, $B186, $B082, $978E,
    $9386, $9182, $A1BE, $A09E, $A7BE, $A39E, $A18E, $A086, $AF9E, $A78E,
    $A386, $A182, $D4F8, $F53E, $A8F0, $EA3C, $D438, $F50E, $A830, $EA0C,
    $D408, $F502, $DAFC, $CA7C, $B4F8, $ED3E, $9478, $E51E, $DA3C, $CA1C,
    $B438, $ED0E, $9418, $E506, $DA0C, $CA04, $B408, $ED02, $CD7E, $BAFC,
    $C53E, $9A7C, $DD3E, $8A3C, $CD1E, $BA3C, $C50E, $9A1C, $DD0E, $8A0C,
    $CD06, $BA0C, $C502, $9A04, $DD02, $9D7E, $8D3E, $BD3E, $851E, $9D1E,
    $8D0E, $BD0E, $8506, $9D06, $8D02, $BD02, $E97E, $D6FC, $D27C, $ACF8,
    $EB3E, $A478, $E91E, $D63C, $D21C, $AC38, $EB0E, $A418, $E906, $D60C,
    $D204, $92FC, $D97E, $CB7E, $B6FC, $C93E, $B27C, $967C, $923C, $D91E,
    $CB1E, $B63C, $C90E, $B21C, $961C, $920C, $D906, $CB06, $B60C, $C902,
    $B204, $897E, $B97E, $9B7E, $993E, $8B3E, $891E, $BB3E, $B91E, $9B1E,
    $990E, $8B0E, $8906, $BB0E, $B906, $9B06, $9902, $A2FC, $D37E, $D13E,
    $AEFC);
  // start (bar, space), 4 characters of 8 elements, stop (a bar of 4)
  ROW_LEN = 35;
  // the quiet zones in modules (the specification has 10; small: the row,
  // parity and symbol checks are strong)
  QUIET_ZONE = 2;

var
  // the symbol characters (the even ones, the odd ones + 2401) by their edge
  // to edge widths (7 of 4 bits)
  Characters: TDictionary<Cardinal, TArray<Integer>>;

function Code49CharacterPattern(value: Integer; even: Boolean): Word;
begin
  if even then
    Result := PATTERNS_EVEN[value]
  else
    Result := PATTERNS_ODD[value];
end;

/// <summary>The widths of the 8 bars and spaces of the 16 modules of a
/// symbol character.</summary>
function PatternWidths(pattern: Word): TArray<Integer>;
begin
  Result := [];
  var width := 0;
  for var b := 15 downto 0 do
  begin
    Inc(width);
    if (b = 0) or ((pattern shr b) and 1 <> (pattern shr (b - 1)) and 1) then
    begin
      Result := Result + [width];
      width := 0;
    end;
  end;
end;

procedure InitCharacters;
begin
  Characters := TDictionary<Cardinal, TArray<Integer>>.Create;
  for var v := 0 to 2 * 2401 - 1 do
  begin
    var pattern: Word;
    if (v < 2401) then
      pattern := PATTERNS_EVEN[v]
    else
      pattern := PATTERNS_ODD[v - 2401];
    var widths := PatternWidths(pattern);
    var key: Cardinal := 0;
    for var e := 0 to 6 do
      key := key or (Cardinal(widths[e] + widths[e + 1]) shl (4 * e));
    var list: TArray<Integer>;
    if not Characters.TryGetValue(key, list) then
      list := [];
    Characters.AddOrSetValue(key, list + [v]);
  end;
end;

function DecodeCode49(const codes: TArray<Integer>;
  out fnc1First: Boolean): string;
begin
  Result := '';
  fnc1First := false;
  var n := Length(codes) div 8;
  if (n < 2) or (n > 8) or (Length(codes) <> 8 * n) then
    exit;
  var last := 8 * (n - 1);
  // the row checks
  for var r := 0 to n - 1 do
  begin
    var sum := 0;
    for var i := 0 to 6 do
      Inc(sum, codes[8 * r + i]);
    if (sum mod 49 <> codes[8 * r + 7]) then
      exit;
  end;
  // the number of rows and the mode
  var countMode := codes[last + 6];
  if (countMode div 7 + 2 <> n) then
    exit;
  var mode := countMode mod 7;
  // the symbol checks X, Y and (more than 6 rows) Z
  var x := countMode * 20;
  var y := countMode * 16;
  var z := countMode * 38;
  var position := 0;
  for var r := 0 to n - 2 do
    for var j := 0 to 3 do
    begin
      var value := codes[8 * r + 2 * j] * 49 + codes[8 * r + 2 * j + 1];
      Inc(x, X_WEIGHTS[position] * value);
      Inc(y, Y_WEIGHTS[position] * value);
      Inc(z, Z_WEIGHTS[position] * value);
      Inc(position);
    end;
  var value := codes[last] * 49 + codes[last + 1];
  if (n > 6) and (z mod 2401 <> value) then
    exit;
  Inc(x, X_WEIGHTS[position] * value);
  Inc(y, Y_WEIGHTS[position] * value);
  Inc(position);
  if (y mod 2401 <> codes[last + 2] * 49 + codes[last + 3]) then
    exit;
  Inc(x, X_WEIGHTS[position] * (y mod 2401));
  if (x mod 2401 <> codes[last + 4] * 49 + codes[last + 5]) then
    exit;

  // the data: 7 code characters a row, in the last row 2 (up to 6 rows)
  var data: TArray<Integer> := [];
  case mode of
    2:
      data := [NUMERIC_SHIFT];
    4:
      data := [SHIFT_1];
    5:
      data := [SHIFT_2];
  end;
  for var r := 0 to n - 2 do
    for var i := 0 to 6 do
      data := data + [codes[8 * r + i]];
  if (n <= 6) then
    data := data + [codes[last], codes[last + 1]];

  var text := '';
  var i := 0;
  while (i < Length(data)) do
  begin
    var c := data[i];
    Inc(i);
    if (c < SHIFT_1) then
      text := text + CHARS.Chars[c]
    else if (c = SHIFT_1) or (c = SHIFT_2) then
    begin
      // a shifted character (Table 7)
      if (i >= Length(data)) or (data[i] >= SHIFT_1) then
        exit;
      var pair := CHARS.Chars[c] + CHARS.Chars[data[i]];
      Inc(i);
      var found := -1;
      for var a := 0 to 127 do
        if (ASCII_CHARS[a] = pair) then
          found := a;
      if (found < 0) then
        exit;
      text := text + Chr(found);
    end
    else if (c = FNC_1) then
    begin
      if (text = '') then
        fnc1First := true
      else
        text := text + #29;
    end
    else if (c = NUMERIC_SHIFT) then
    begin
      // numeric: up to the next numeric shift, 3 code characters (base 48)
      // for 5 digits (4 when 100000 or more), 2 for 3 digits, 1 for 1
      var first := i;
      while (i < Length(data)) and (data[i] <> NUMERIC_SHIFT) do
        Inc(i);
      var k := first;
      while (k < i) do
      begin
        var rest := i - k;
        if (rest >= 3) then
        begin
          var v := data[k] * 2304 + data[k + 1] * 48 + data[k + 2];
          if (v >= 100000) then
            text := text + Format('%.4d', [v - 100000])
          else
            text := text + Format('%.5d', [v]);
          Inc(k, 3);
        end
        else if (rest = 2) then
        begin
          var v := data[k] * 48 + data[k + 1];
          if (v > 999) then
            exit;
          text := text + Format('%.3d', [v]);
          Inc(k, 2);
        end
        else
        begin
          if (data[k] > 9) then
            exit;
          text := text + Chr(Ord('0') + data[k]);
          Inc(k);
        end;
      end;
      // (the numeric shift back)
      Inc(i);
    end;
    // (FNC2, FNC3: nothing)
  end;
  Result := text;
end;

/// <summary>The rows of a Code 49 in the runs of row y: start, 4 symbol
/// characters and stop, between quiet zones.</summary>
procedure ReadRows(const runs: TPatternRow; y, width: Integer;
  reversed: Boolean; rows: TList<TCode49Reader.TRow>);
begin
  if (Characters = nil) then
    InitCharacters;
  var view := TPatternView.Create(runs);
  view := view.SubView(0, ROW_LEN);
  while view.IsValid do
  begin
    // the module (the row has a bar more than spaces: about as wide with
    // the bars wider in the image) and how much wider the bars are: from
    // the start (a bar and a space of 1) and the stop (a bar of 4)
    // (first quickly: the stop bar of 4 modules wider than the start bar
    // and space of 1, a quiet zone in front)
    var stop := view[ROW_LEN - 1];
    if (stop < 2 * view[0]) or (stop < 2 * view[1]) or
      not view.IsAtFirstBar and (2 * view.SpaceInFront < stop) then
    begin
      if not view.SkipPair then
        break;
      continue;
    end;
    var module: Double := view.Sum(ROW_LEN) / 70;
    var bias1 := view[0] - module;
    var bias2 := module - view[1];
    var bias3 := view[ROW_LEN - 1] - 4 * module;
    var bias := (bias1 + bias2 + bias3) / 3;
    if (module > 0) and (Abs(bias1 - bias) <= 0.4 * module + 0.5) and
      (Abs(bias2 - bias) <= 0.4 * module + 0.5) and
      (Abs(bias3 - bias) <= 0.4 * module + 0.5) and
      (Abs(bias) <= 0.75 * module) and
      (view.IsAtFirstBar or (view.SpaceInFront >= QUIET_ZONE * module)) and
      (view.IsAtLastBar or (view[ROW_LEN] >= QUIET_ZONE * module)) then
    begin
      var parity := '';
      var codes: TArray<Integer> := [];
      for var c := 0 to 3 do
      begin
        // the edge to edge widths (a bar and a space: as wide in the image
        // with the bars wider) in modules of the character (16 modules)
        var total := view.Sum(2 + 8 * c + 8) - view.Sum(2 + 8 * c);
        var key: Cardinal := 0;
        for var e := 0 to 6 do
        begin
          var t := Round(16 * (view[2 + 8 * c + e] + view[2 + 8 * c + e + 1]) /
            total);
          if (t < 2) or (t > 15) then
          begin
            key := 0;
            break;
          end;
          key := key or (Cardinal(t) shl (4 * e));
        end;
        var candidates: TArray<Integer>;
        if (key = 0) or not Characters.TryGetValue(key, candidates) then
          break;
        // more than one: the one with the widths nearest to the ones of the
        // image (the bars narrower, the spaces wider by the bias)
        var best := -1;
        var bestError := MaxDouble;
        for var candidate in candidates do
        begin
          var pattern: Word;
          if (candidate < 2401) then
            pattern := PATTERNS_EVEN[candidate]
          else
            pattern := PATTERNS_ODD[candidate - 2401];
          var widths := PatternWidths(pattern);
          var error := 0.0;
          for var e := 0 to 7 do
          begin
            var size: Double := view[2 + 8 * c + e] - bias;
            if Odd(e) then
              size := view[2 + 8 * c + e] + bias;
            error := error + Abs(16 * size / total - widths[e]);
          end;
          if (error < bestError) then
          begin
            best := candidate;
            bestError := error;
          end;
        end;
        var v := best;
        if (v < 2401) then
          parity := parity + 'E'
        else
        begin
          Dec(v, 2401);
          parity := parity + 'O';
        end;
        codes := codes + [v div 49, v mod 49];
      end;
      if (Length(codes) = 8) then
      begin
        var sum := 0;
        for var i := 0 to 6 do
          Inc(sum, codes[i]);
        var row: TCode49Reader.TRow;
        row.Index := -2;
        if (sum mod 49 = codes[7]) then
          for var r := 0 to 7 do
            if (ROW_PARITY[r] = parity) then
              row.Index := r;
        // even only: the last row
        if (row.Index = 7) then
          row.Index := -1;
        if (row.Index >= -1) then
        begin
          row.Codes := codes;
          row.XStart := view.PixelsInFront;
          row.XStop := view.PixelsInFront + view.Sum(ROW_LEN);
          if reversed then
          begin
            row.XStart := width - row.XStart;
            row.XStop := width - row.XStop;
          end;
          row.Y := y;
          rows.Add(row);
        end;
      end;
    end;
    if not view.SkipPair then
      break;
  end;
end;

{ TCode49Reader }

constructor TCode49Reader.Create;
begin
  inherited Create;
  FRows := TList<TRow>.Create;
end;

destructor TCode49Reader.Destroy;
begin
  FRows.Free;
  inherited;
end;

procedure TCode49Reader.ReadRows(const runs: TPatternRow; y, width: Integer;
  reversed: Boolean);
begin
  ZXing.Stacked.Code49Reader.ReadRows(runs, y, width, reversed, FRows);
end;

procedure TCode49Reader.ClearRows;
begin
  FRows.Clear;
end;

procedure TCode49Reader.AddSymbols(results: TList<TReadResult>;
  maxCount: Integer; vertical: Boolean);
begin
  for var last in FRows do
  begin
    if (last.Index <> -1) or ResultsFull(results, maxCount) then
      continue;
    var n := last.Codes[6] div 7 + 2;
    if (n > 8) then
      continue;
    var codes: TArray<Integer> := [];
    var tolerance := Abs(last.XStop - last.XStart) / 20;
    var minY := last.Y;
    var maxY := last.Y;
    var complete := true;
    for var k := 0 to n - 2 do
    begin
      var best := -1;
      var bestDistance := MaxInt;
      for var i := 0 to FRows.Count - 1 do
      begin
        var row := FRows[i];
        if (row.Index = k) and
          (Abs(row.XStart - last.XStart) <= tolerance) and
          (Abs(row.XStop - last.XStop) <= tolerance) and
          (Abs(row.Y - last.Y) < bestDistance) then
        begin
          best := i;
          bestDistance := Abs(row.Y - last.Y);
        end;
      end;
      if (best < 0) then
      begin
        complete := false;
        break;
      end;
      codes := codes + FRows[best].Codes;
      minY := Min(minY, FRows[best].Y);
      maxY := Max(maxY, FRows[best].Y);
    end;
    if not complete then
      continue;
    codes := codes + last.Codes;
    var fnc1First: Boolean;
    var text := DecodeCode49(codes, fnc1First);
    if (text = '') then
      continue;
    var x1 := Min(last.XStart, last.XStop);
    var x2 := Max(last.XStart, last.XStop);
    var points: TArray<IResultPoint>;
    if vertical then
      points := [TResultPointHelpers.CreateResultPoint(minY, x1),
        TResultPointHelpers.CreateResultPoint(maxY, x1),
        TResultPointHelpers.CreateResultPoint(maxY, x2),
        TResultPointHelpers.CreateResultPoint(minY, x2)]
    else
      points := [TResultPointHelpers.CreateResultPoint(x1, minY),
        TResultPointHelpers.CreateResultPoint(x2, minY),
        TResultPointHelpers.CreateResultPoint(x2, maxY),
        TResultPointHelpers.CreateResultPoint(x1, maxY)];
    var r := TReadResult.Create(text, nil, points,
      TBarcodeFormat.CODE_49);
    // ISO/IEC 15424: ]T0 Code 49
    r.SymbologyIdentifier := ']T0';
    if ContainsResult(results, r) then
      r.Free
    else
      results.Add(r);
  end;
end;

end.
