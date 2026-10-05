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

  * The Han Xin Code decoder for ZXing.Delphi: zxing-cpp, ZXing Java and
  * ZXing.Net have no reader. The encoding as in zint (hanxin.c, after
  * ISO/IEC 20830:2021) reversed.
}

unit ZXing.HanXin.Decoder;

interface

uses
  System.SysUtils;

type
  /// <summary>The text of a Han Xin Code symbol and its parameters.
  /// </summary>
  THanXinResult = record
    Text: string;
    Version, ECLevel, Mask: Integer;
    /// <summary>The number of codewords corrected.</summary>
    Errors: Integer;
  end;

/// <summary>The size (modules) of a symbol of version (1 to 84).</summary>
function HanXinSize(version: Integer): Integer;

/// <summary>The function modules of a symbol of version (row major, true
/// for the finder patterns, their separators, the function information and
/// the alignment patterns).</summary>
function HanXinFunctionModules(version: Integer): TArray<Boolean>;

/// <summary>The modules of the finder patterns, their separators and the
/// alignment patterns of a symbol of version (row major): 1 light, 2 dark,
/// 0 the others (data and function information).</summary>
function HanXinFunctionPattern(version: Integer): TArray<Byte>;

type
  /// <summary>Whether the module (column x, row y) of a symbol is dark.
  /// </summary>
  THanXinModule = reference to function(x, y: Integer): Boolean;

/// <summary>The version, error correction level (1 to 4) and mask (0 to 3)
/// of the function information of a symbol of size x size modules; false
/// when it can not be read.</summary>
function ReadHanXinFunctionInfo(const module: THanXinModule; size: Integer;
  out version, ecLevel, mask: Integer): Boolean; overload;

/// <summary>The version, error correction level (1 to 4) and mask (0 to 3)
/// of the function information of a symbol of size x size modules (row
/// major, true dark); false when it can not be read.</summary>
function ReadHanXinFunctionInfo(const modules: TArray<Boolean>;
  size: Integer; out version, ecLevel, mask: Integer): Boolean; overload;

/// <summary>Decodes a symbol of size x size modules (row major, true dark);
/// false when it can not be decoded.</summary>
function DecodeHanXin(const modules: TArray<Boolean>; size: Integer;
  out decoded: THanXinResult): Boolean;

implementation

uses
  System.Math,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder,
  ZXing.Common.ECIContent;

const
  // Table D1: per version and level 3 groups of blocks: number, data
  // codewords, check codewords
  BLOCKS: array [0 .. 335, 0 .. 8] of Byte = (
    (1, 21, 4, 0, 0, 0, 0, 0, 0),
    (1, 17, 8, 0, 0, 0, 0, 0, 0),
    (1, 13, 12, 0, 0, 0, 0, 0, 0),
    (1, 9, 16, 0, 0, 0, 0, 0, 0),
    (1, 31, 6, 0, 0, 0, 0, 0, 0),
    (1, 25, 12, 0, 0, 0, 0, 0, 0),
    (1, 19, 18, 0, 0, 0, 0, 0, 0),
    (1, 15, 22, 0, 0, 0, 0, 0, 0),
    (1, 42, 8, 0, 0, 0, 0, 0, 0),
    (1, 34, 16, 0, 0, 0, 0, 0, 0),
    (1, 26, 24, 0, 0, 0, 0, 0, 0),
    (1, 20, 30, 0, 0, 0, 0, 0, 0),
    (1, 46, 8, 0, 0, 0, 0, 0, 0),
    (1, 38, 16, 0, 0, 0, 0, 0, 0),
    (1, 30, 24, 0, 0, 0, 0, 0, 0),
    (1, 22, 32, 0, 0, 0, 0, 0, 0),
    (1, 57, 12, 0, 0, 0, 0, 0, 0),
    (1, 49, 20, 0, 0, 0, 0, 0, 0),
    (1, 37, 32, 0, 0, 0, 0, 0, 0),
    (1, 14, 20, 1, 13, 22, 0, 0, 0),
    (1, 70, 14, 0, 0, 0, 0, 0, 0),
    (1, 58, 26, 0, 0, 0, 0, 0, 0),
    (1, 24, 20, 1, 22, 18, 0, 0, 0),
    (1, 16, 24, 1, 18, 26, 0, 0, 0),
    (1, 84, 16, 0, 0, 0, 0, 0, 0),
    (1, 70, 30, 0, 0, 0, 0, 0, 0),
    (1, 26, 22, 1, 28, 24, 0, 0, 0),
    (2, 14, 20, 1, 12, 20, 0, 0, 0),
    (1, 99, 18, 0, 0, 0, 0, 0, 0),
    (1, 40, 18, 1, 41, 18, 0, 0, 0),
    (1, 31, 26, 1, 32, 28, 0, 0, 0),
    (2, 16, 24, 1, 15, 22, 0, 0, 0),
    (1, 114, 22, 0, 0, 0, 0, 0, 0),
    (2, 48, 20, 0, 0, 0, 0, 0, 0),
    (2, 24, 20, 1, 26, 22, 0, 0, 0),
    (2, 18, 28, 1, 18, 26, 0, 0, 0),
    (1, 131, 24, 0, 0, 0, 0, 0, 0),
    (1, 52, 22, 1, 57, 24, 0, 0, 0),
    (2, 27, 24, 1, 29, 24, 0, 0, 0),
    (2, 21, 32, 1, 19, 30, 0, 0, 0),
    (1, 135, 26, 0, 0, 0, 0, 0, 0),
    (1, 56, 24, 1, 57, 24, 0, 0, 0),
    (2, 28, 24, 1, 31, 26, 0, 0, 0),
    (2, 22, 32, 1, 21, 32, 0, 0, 0),
    (1, 153, 28, 0, 0, 0, 0, 0, 0),
    (1, 62, 26, 1, 65, 28, 0, 0, 0),
    (2, 32, 28, 1, 33, 28, 0, 0, 0),
    (3, 17, 26, 1, 22, 30, 0, 0, 0),
    (1, 86, 16, 1, 85, 16, 0, 0, 0),
    (1, 71, 30, 1, 72, 30, 0, 0, 0),
    (2, 37, 32, 1, 35, 30, 0, 0, 0),
    (3, 20, 30, 1, 21, 32, 0, 0, 0),
    (1, 94, 18, 1, 95, 18, 0, 0, 0),
    (2, 51, 22, 1, 55, 24, 0, 0, 0),
    (3, 30, 26, 1, 31, 26, 0, 0, 0),
    (4, 18, 28, 1, 17, 24, 0, 0, 0),
    (1, 104, 20, 1, 105, 20, 0, 0, 0),
    (2, 57, 24, 1, 61, 26, 0, 0, 0),
    (3, 33, 28, 1, 36, 30, 0, 0, 0),
    (4, 20, 30, 1, 19, 30, 0, 0, 0),
    (1, 115, 22, 1, 114, 22, 0, 0, 0),
    (2, 65, 28, 1, 61, 26, 0, 0, 0),
    (3, 38, 32, 1, 33, 30, 0, 0, 0),
    (5, 19, 28, 1, 14, 24, 0, 0, 0),
    (1, 126, 24, 1, 125, 24, 0, 0, 0),
    (2, 70, 30, 1, 69, 30, 0, 0, 0),
    (4, 33, 28, 1, 29, 26, 0, 0, 0),
    (5, 20, 30, 1, 19, 30, 0, 0, 0),
    (1, 136, 26, 1, 137, 26, 0, 0, 0),
    (3, 56, 24, 1, 59, 26, 0, 0, 0),
    (5, 35, 30, 0, 0, 0, 0, 0, 0),
    (6, 18, 28, 1, 21, 28, 0, 0, 0),
    (1, 148, 28, 1, 149, 28, 0, 0, 0),
    (3, 61, 26, 1, 64, 28, 0, 0, 0),
    (7, 24, 20, 1, 23, 22, 0, 0, 0),
    (6, 20, 30, 1, 21, 32, 0, 0, 0),
    (3, 107, 20, 0, 0, 0, 0, 0, 0),
    (3, 65, 28, 1, 72, 30, 0, 0, 0),
    (7, 26, 22, 1, 23, 22, 0, 0, 0),
    (7, 19, 28, 1, 20, 32, 0, 0, 0),
    (3, 115, 22, 0, 0, 0, 0, 0, 0),
    (4, 56, 24, 1, 63, 28, 0, 0, 0),
    (7, 28, 24, 1, 25, 22, 0, 0, 0),
    (8, 18, 28, 1, 21, 22, 0, 0, 0),
    (2, 116, 22, 1, 122, 24, 0, 0, 0),
    (4, 56, 24, 1, 72, 30, 0, 0, 0),
    (7, 28, 24, 1, 32, 26, 0, 0, 0),
    (8, 18, 28, 1, 24, 30, 0, 0, 0),
    (3, 127, 24, 0, 0, 0, 0, 0, 0),
    (5, 51, 22, 1, 62, 26, 0, 0, 0),
    (7, 30, 26, 1, 35, 26, 0, 0, 0),
    (8, 20, 30, 1, 21, 32, 0, 0, 0),
    (2, 135, 26, 1, 137, 26, 0, 0, 0),
    (5, 56, 24, 1, 59, 26, 0, 0, 0),
    (7, 33, 28, 1, 30, 28, 0, 0, 0),
    (11, 16, 24, 1, 19, 26, 0, 0, 0),
    (3, 105, 20, 1, 121, 22, 0, 0, 0),
    (5, 61, 26, 1, 57, 26, 0, 0, 0),
    (9, 28, 24, 1, 28, 22, 0, 0, 0),
    (10, 19, 28, 1, 18, 30, 0, 0, 0),
    (2, 157, 30, 1, 150, 28, 0, 0, 0),
    (5, 65, 28, 1, 61, 26, 0, 0, 0),
    (8, 33, 28, 1, 34, 30, 0, 0, 0),
    (10, 19, 28, 2, 15, 26, 0, 0, 0),
    (3, 126, 24, 1, 115, 22, 0, 0, 0),
    (7, 51, 22, 1, 54, 22, 0, 0, 0),
    (8, 35, 30, 1, 37, 30, 0, 0, 0),
    (15, 15, 22, 1, 10, 22, 0, 0, 0),
    (4, 105, 20, 1, 103, 20, 0, 0, 0),
    (7, 56, 24, 1, 45, 18, 0, 0, 0),
    (10, 31, 26, 1, 27, 26, 0, 0, 0),
    (10, 17, 26, 3, 20, 28, 1, 21, 28),
    (3, 139, 26, 1, 137, 28, 0, 0, 0),
    (6, 66, 28, 1, 66, 30, 0, 0, 0),
    (9, 36, 30, 1, 34, 32, 0, 0, 0),
    (13, 19, 28, 1, 17, 32, 0, 0, 0),
    (6, 84, 16, 1, 82, 16, 0, 0, 0),
    (6, 70, 30, 1, 68, 30, 0, 0, 0),
    (7, 35, 30, 3, 33, 28, 1, 32, 28),
    (13, 20, 30, 1, 20, 28, 0, 0, 0),
    (5, 105, 20, 1, 94, 18, 0, 0, 0),
    (6, 74, 32, 1, 71, 30, 0, 0, 0),
    (11, 33, 28, 1, 34, 32, 0, 0, 0),
    (13, 19, 28, 3, 16, 26, 0, 0, 0),
    (4, 127, 24, 1, 126, 24, 0, 0, 0),
    (7, 66, 28, 1, 66, 30, 0, 0, 0),
    (12, 30, 24, 1, 24, 28, 1, 24, 30),
    (15, 19, 28, 1, 17, 32, 0, 0, 0),
    (7, 84, 16, 1, 78, 16, 0, 0, 0),
    (7, 70, 30, 1, 66, 28, 0, 0, 0),
    (12, 33, 28, 1, 32, 30, 0, 0, 0),
    (14, 21, 32, 1, 24, 28, 0, 0, 0),
    (5, 117, 22, 1, 117, 24, 0, 0, 0),
    (8, 66, 28, 1, 58, 26, 0, 0, 0),
    (11, 38, 32, 1, 34, 32, 0, 0, 0),
    (15, 20, 30, 2, 17, 26, 0, 0, 0),
    (4, 148, 28, 1, 146, 28, 0, 0, 0),
    (8, 68, 30, 1, 70, 24, 0, 0, 0),
    (10, 36, 32, 3, 38, 28, 0, 0, 0),
    (16, 19, 28, 3, 16, 26, 0, 0, 0),
    (4, 126, 24, 2, 135, 26, 0, 0, 0),
    (8, 70, 28, 2, 43, 26, 0, 0, 0),
    (13, 32, 28, 2, 41, 30, 0, 0, 0),
    (17, 19, 28, 3, 15, 26, 0, 0, 0),
    (5, 136, 26, 1, 132, 24, 0, 0, 0),
    (5, 67, 30, 4, 68, 28, 1, 69, 28),
    (14, 35, 30, 1, 32, 24, 0, 0, 0),
    (18, 18, 26, 3, 16, 28, 1, 14, 28),
    (3, 142, 26, 3, 141, 28, 0, 0, 0),
    (8, 70, 30, 1, 73, 32, 1, 74, 32),
    (12, 34, 30, 3, 34, 26, 1, 35, 28),
    (18, 21, 32, 1, 27, 30, 0, 0, 0),
    (5, 116, 22, 2, 103, 20, 1, 102, 20),
    (9, 74, 32, 1, 74, 30, 0, 0, 0),
    (14, 34, 28, 2, 32, 32, 1, 32, 30),
    (19, 21, 32, 1, 25, 26, 0, 0, 0),
    (7, 116, 22, 1, 117, 22, 0, 0, 0),
    (11, 65, 28, 1, 58, 24, 0, 0, 0),
    (15, 38, 32, 1, 27, 28, 0, 0, 0),
    (20, 20, 30, 1, 20, 32, 1, 21, 32),
    (6, 136, 26, 1, 130, 24, 0, 0, 0),
    (11, 66, 28, 1, 62, 30, 0, 0, 0),
    (14, 34, 28, 3, 34, 32, 1, 30, 30),
    (18, 20, 30, 3, 20, 28, 2, 15, 26),
    (5, 105, 20, 2, 115, 22, 2, 116, 22),
    (10, 75, 32, 1, 73, 32, 0, 0, 0),
    (16, 38, 32, 1, 27, 28, 0, 0, 0),
    (22, 19, 28, 2, 16, 30, 1, 19, 30),
    (6, 147, 28, 1, 146, 28, 0, 0, 0),
    (11, 66, 28, 2, 65, 30, 0, 0, 0),
    (18, 33, 28, 2, 33, 30, 0, 0, 0),
    (22, 21, 32, 1, 28, 30, 0, 0, 0),
    (6, 116, 22, 3, 125, 24, 0, 0, 0),
    (11, 75, 32, 1, 68, 30, 0, 0, 0),
    (13, 35, 28, 6, 34, 32, 1, 30, 30),
    (23, 21, 32, 1, 26, 30, 0, 0, 0),
    (7, 105, 20, 4, 95, 18, 0, 0, 0),
    (12, 67, 28, 1, 63, 30, 1, 62, 32),
    (21, 31, 26, 2, 33, 32, 0, 0, 0),
    (23, 21, 32, 2, 24, 30, 0, 0, 0),
    (10, 116, 22, 0, 0, 0, 0, 0, 0),
    (12, 74, 32, 1, 78, 30, 0, 0, 0),
    (18, 37, 32, 1, 39, 30, 1, 41, 28),
    (25, 21, 32, 1, 27, 28, 0, 0, 0),
    (5, 126, 24, 4, 115, 22, 1, 114, 22),
    (12, 67, 28, 2, 66, 32, 1, 68, 30),
    (21, 35, 30, 1, 39, 30, 0, 0, 0),
    (26, 21, 32, 1, 28, 28, 0, 0, 0),
    (9, 126, 24, 1, 117, 22, 0, 0, 0),
    (13, 75, 32, 1, 68, 30, 0, 0, 0),
    (20, 35, 30, 3, 35, 28, 0, 0, 0),
    (27, 21, 32, 1, 28, 30, 0, 0, 0),
    (9, 126, 24, 1, 137, 26, 0, 0, 0),
    (13, 71, 30, 2, 68, 32, 0, 0, 0),
    (20, 37, 32, 1, 39, 28, 1, 38, 28),
    (24, 20, 32, 5, 25, 28, 0, 0, 0),
    (8, 147, 28, 1, 141, 28, 0, 0, 0),
    (10, 73, 32, 4, 74, 30, 1, 73, 30),
    (16, 36, 32, 6, 39, 30, 1, 37, 30),
    (27, 21, 32, 3, 20, 26, 0, 0, 0),
    (9, 137, 26, 1, 135, 26, 0, 0, 0),
    (12, 70, 30, 4, 75, 32, 0, 0, 0),
    (24, 35, 30, 1, 40, 28, 0, 0, 0),
    (23, 20, 32, 8, 24, 30, 0, 0, 0),
    (14, 95, 18, 1, 86, 18, 0, 0, 0),
    (13, 73, 32, 3, 77, 30, 0, 0, 0),
    (24, 35, 30, 2, 35, 28, 0, 0, 0),
    (26, 21, 32, 5, 21, 30, 1, 23, 30),
    (9, 147, 28, 1, 142, 28, 0, 0, 0),
    (10, 73, 30, 6, 70, 32, 1, 71, 32),
    (25, 35, 30, 2, 34, 26, 0, 0, 0),
    (29, 21, 32, 4, 22, 30, 0, 0, 0),
    (11, 126, 24, 1, 131, 24, 0, 0, 0),
    (16, 74, 32, 1, 79, 30, 0, 0, 0),
    (25, 38, 32, 1, 25, 30, 0, 0, 0),
    (33, 21, 32, 1, 28, 28, 0, 0, 0),
    (14, 105, 20, 1, 99, 18, 0, 0, 0),
    (19, 65, 28, 1, 72, 28, 0, 0, 0),
    (24, 37, 32, 2, 40, 30, 1, 41, 30),
    (31, 21, 32, 4, 24, 32, 0, 0, 0),
    (10, 147, 28, 1, 151, 28, 0, 0, 0),
    (15, 71, 30, 3, 71, 32, 1, 73, 32),
    (24, 37, 32, 3, 38, 30, 1, 39, 30),
    (36, 19, 30, 3, 29, 26, 0, 0, 0),
    (15, 105, 20, 1, 99, 18, 0, 0, 0),
    (19, 70, 30, 1, 64, 28, 0, 0, 0),
    (27, 38, 32, 2, 25, 26, 0, 0, 0),
    (38, 20, 30, 2, 18, 28, 0, 0, 0),
    (14, 105, 20, 1, 113, 22, 1, 114, 22),
    (17, 67, 30, 3, 92, 32, 0, 0, 0),
    (30, 35, 30, 1, 41, 30, 0, 0, 0),
    (36, 21, 32, 1, 26, 30, 1, 27, 30),
    (11, 146, 28, 1, 146, 26, 0, 0, 0),
    (20, 70, 30, 1, 60, 26, 0, 0, 0),
    (29, 38, 32, 1, 24, 32, 0, 0, 0),
    (40, 20, 30, 2, 17, 26, 0, 0, 0),
    (3, 137, 26, 1, 136, 26, 10, 126, 24),
    (22, 65, 28, 1, 75, 30, 0, 0, 0),
    (30, 37, 32, 1, 51, 30, 0, 0, 0),
    (42, 20, 30, 1, 21, 30, 0, 0, 0),
    (12, 126, 24, 2, 118, 22, 1, 116, 22),
    (19, 74, 32, 1, 74, 30, 1, 72, 28),
    (30, 38, 32, 2, 29, 30, 0, 0, 0),
    (39, 20, 32, 2, 37, 26, 1, 38, 26),
    (12, 126, 24, 3, 136, 26, 0, 0, 0),
    (21, 70, 30, 2, 65, 28, 0, 0, 0),
    (34, 35, 30, 1, 44, 32, 0, 0, 0),
    (42, 20, 30, 2, 19, 28, 2, 18, 28),
    (12, 126, 24, 3, 117, 22, 1, 116, 22),
    (25, 61, 26, 2, 62, 28, 0, 0, 0),
    (34, 35, 30, 1, 40, 32, 1, 41, 32),
    (45, 20, 30, 1, 20, 32, 1, 21, 32),
    (15, 105, 20, 2, 115, 22, 2, 116, 22),
    (25, 65, 28, 1, 72, 28, 0, 0, 0),
    (18, 35, 30, 17, 37, 32, 1, 50, 32),
    (42, 20, 30, 6, 19, 28, 1, 15, 28),
    (19, 105, 20, 1, 101, 20, 0, 0, 0),
    (33, 51, 22, 1, 65, 22, 0, 0, 0),
    (40, 33, 28, 1, 28, 28, 0, 0, 0),
    (49, 20, 30, 1, 18, 28, 0, 0, 0),
    (18, 105, 20, 2, 117, 22, 0, 0, 0),
    (26, 65, 28, 1, 80, 30, 0, 0, 0),
    (35, 35, 30, 3, 35, 28, 1, 36, 28),
    (52, 18, 28, 2, 38, 30, 0, 0, 0),
    (26, 84, 16, 0, 0, 0, 0, 0, 0),
    (26, 70, 30, 0, 0, 0, 0, 0, 0),
    (45, 31, 26, 1, 9, 26, 0, 0, 0),
    (52, 20, 30, 0, 0, 0, 0, 0, 0),
    (16, 126, 24, 1, 114, 22, 1, 115, 22),
    (23, 70, 30, 3, 65, 28, 1, 66, 28),
    (40, 35, 30, 1, 43, 30, 0, 0, 0),
    (46, 20, 30, 7, 19, 28, 1, 16, 28),
    (19, 116, 22, 1, 105, 22, 0, 0, 0),
    (20, 70, 30, 7, 66, 28, 1, 63, 28),
    (40, 35, 30, 1, 42, 32, 1, 43, 32),
    (54, 20, 30, 1, 19, 30, 0, 0, 0),
    (17, 126, 24, 2, 115, 22, 0, 0, 0),
    (24, 70, 30, 4, 74, 32, 0, 0, 0),
    (48, 31, 26, 2, 18, 26, 0, 0, 0),
    (54, 19, 28, 6, 15, 26, 1, 14, 26),
    (29, 84, 16, 0, 0, 0, 0, 0, 0),
    (29, 70, 30, 0, 0, 0, 0, 0, 0),
    (6, 34, 30, 3, 36, 30, 38, 33, 28),
    (58, 20, 30, 0, 0, 0, 0, 0, 0),
    (16, 147, 28, 1, 149, 28, 0, 0, 0),
    (31, 66, 28, 1, 37, 26, 0, 0, 0),
    (48, 33, 28, 1, 23, 26, 0, 0, 0),
    (53, 20, 30, 6, 19, 28, 1, 17, 28),
    (20, 115, 22, 2, 134, 24, 0, 0, 0),
    (29, 66, 28, 2, 56, 26, 2, 57, 26),
    (45, 36, 30, 2, 15, 28, 0, 0, 0),
    (59, 20, 30, 2, 21, 32, 0, 0, 0),
    (17, 147, 28, 1, 134, 26, 0, 0, 0),
    (26, 70, 30, 5, 75, 32, 0, 0, 0),
    (47, 35, 30, 1, 48, 32, 0, 0, 0),
    (64, 18, 28, 2, 33, 30, 1, 35, 30),
    (22, 115, 22, 1, 133, 24, 0, 0, 0),
    (33, 65, 28, 1, 74, 28, 0, 0, 0),
    (43, 36, 30, 5, 27, 28, 1, 30, 28),
    (57, 20, 30, 5, 21, 32, 1, 24, 32),
    (18, 136, 26, 2, 142, 26, 0, 0, 0),
    (33, 66, 28, 2, 49, 26, 0, 0, 0),
    (48, 35, 30, 2, 38, 28, 0, 0, 0),
    (64, 20, 30, 1, 20, 32, 0, 0, 0),
    (19, 126, 24, 2, 135, 26, 1, 136, 26),
    (32, 66, 28, 2, 55, 26, 2, 56, 26),
    (49, 36, 30, 2, 18, 32, 0, 0, 0),
    (65, 18, 28, 5, 27, 30, 1, 29, 30),
    (20, 137, 26, 1, 130, 26, 0, 0, 0),
    (30, 75, 32, 2, 71, 32, 0, 0, 0),
    (46, 35, 30, 6, 39, 32, 0, 0, 0),
    (3, 12, 30, 70, 19, 28, 0, 0, 0),
    (20, 147, 28, 0, 0, 0, 0, 0, 0),
    (35, 70, 30, 0, 0, 0, 0, 0, 0),
    (49, 35, 30, 5, 35, 28, 0, 0, 0),
    (70, 20, 30, 0, 0, 0, 0, 0, 0),
    (21, 136, 26, 1, 155, 28, 0, 0, 0),
    (34, 70, 30, 1, 64, 28, 1, 65, 28),
    (54, 35, 30, 1, 45, 30, 0, 0, 0),
    (68, 20, 30, 3, 18, 28, 1, 19, 28),
    (19, 126, 24, 5, 115, 22, 1, 114, 22),
    (33, 70, 30, 3, 65, 28, 1, 64, 28),
    (52, 35, 30, 3, 41, 32, 1, 40, 32),
    (67, 20, 30, 5, 21, 32, 1, 24, 32),
    (2, 150, 28, 21, 136, 26, 0, 0, 0),
    (32, 70, 30, 6, 65, 28, 0, 0, 0),
    (52, 38, 32, 2, 27, 32, 0, 0, 0),
    (73, 20, 30, 2, 22, 32, 0, 0, 0),
    (21, 126, 24, 4, 136, 26, 0, 0, 0),
    (30, 74, 32, 6, 73, 30, 0, 0, 0),
    (54, 35, 30, 4, 40, 32, 0, 0, 0),
    (75, 20, 30, 1, 20, 28, 0, 0, 0),
    (30, 105, 20, 1, 114, 22, 0, 0, 0),
    (3, 45, 22, 55, 47, 20, 0, 0, 0),
    (2, 26, 26, 62, 33, 28, 0, 0, 0),
    (79, 18, 28, 4, 33, 30, 0, 0, 0));
  MODULE_K: array [0 .. 83] of Byte = (
    0, 0, 0, 14, 16, 16, 17, 18, 19, 20, 14, 15, 16, 16, 17, 17, 18, 19, 20,
    20, 21, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 17, 17, 18, 18, 19, 19,
    19, 20, 20, 17, 17, 18, 18, 18, 19, 19, 19, 17, 17, 18, 18, 18, 18, 19,
    19, 19, 17, 17, 18, 18, 18, 18, 19, 19, 17, 17, 17, 18, 18, 18, 18, 19,
    19, 17, 17, 17, 18, 18, 18, 18, 18, 17, 17);
  MODULE_M: array [0 .. 83] of Byte = (
    0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10);
  MODULE_R: array [0 .. 83] of Byte = (
    0, 0, 0, 15, 15, 17, 18, 19, 20, 21, 15, 15, 15, 17, 17, 19, 19, 19, 19,
    21, 21, 17, 16, 18, 17, 19, 18, 20, 19, 21, 20, 17, 19, 17, 19, 17, 19,
    21, 19, 21, 18, 20, 17, 19, 21, 18, 20, 22, 17, 19, 15, 17, 19, 21, 17,
    19, 21, 18, 20, 15, 17, 19, 21, 16, 18, 17, 19, 21, 15, 17, 19, 21, 15,
    17, 18, 20, 22, 15, 17, 19, 21, 23, 17, 19);

var
  // GF(256) of x^8 + x^6 + x^5 + x + 1 (the data) and GF(16) of x^4 + x + 1
  // (the function information), the roots of the generators from a^1
  Field256, Field16: TGenericGF;

function HanXinSize(version: Integer): Integer;
begin
  Result := 2 * version + 21;
end;

function HanXinFunctionModules(version: Integer): TArray<Boolean>;
var
  size: Integer;
  grid: TArray<Boolean>;

  procedure Mark(x, y: Integer);
  begin
    if (x >= 0) and (y >= 0) and (x < size) and (y < size) then
      grid[y * size + x] := true;
  end;

  // an alignment pattern along the top and the right of the region at (x, y)
  // (its top right corner) of w x h modules
  procedure Alignment(x, y, w, h: Integer);
  begin
    Mark(x, y);
    Mark(x - 1, y + 1);
    for var i := 1 to w do
    begin
      Mark(x - i, y);
      Mark(x - i - 1, y + 1);
    end;
    for var i := 1 to h - 1 do
    begin
      Mark(x, y + i);
      Mark(x - 1, y + i + 1);
    end;
  end;

  // an assistant alignment pattern: 3 x 3 modules around (x, y)
  procedure Assistant(x, y: Integer);
  begin
    for var dy := -1 to 1 do
      for var dx := -1 to 1 do
        Mark(x + dx, y + dy);
  end;

begin
  size := HanXinSize(version);
  SetLength(grid, size * size);
  for var i := 0 to High(grid) do
    grid[i] := false;
  // the finder patterns with their separators and the function information:
  // 9 x 9 modules in each corner
  for var y := 0 to 8 do
    for var x := 0 to 8 do
    begin
      Mark(x, y);
      Mark(size - 1 - x, y);
      Mark(x, size - 1 - y);
      Mark(size - 1 - x, size - 1 - y);
    end;
  if (version > 3) then
  begin
    // the regions: m of k modules, the rest of r - 1 (from the top and
    // from the right)
    var k := MODULE_K[version - 1];
    var r := MODULE_R[version - 1];
    var m := MODULE_M[version - 1];
    // the assistant alignment patterns left and right
    var y := 0;
    var row := 0;
    repeat
      var height := k;
      if (row >= m) then
        height := r - 1;
      if not Odd(row) then
      begin
        if Odd(m) then
          Assistant(0, y);
      end
      else
      begin
        if not Odd(m) then
          Assistant(0, y);
        Assistant(size - 1, y);
      end;
      Inc(row);
      Inc(y, height);
    until (y >= size);
    // at the top and the bottom
    var x := size - 1;
    var column := 0;
    repeat
      var width := k;
      if (column >= m) then
        width := r - 1;
      if not Odd(column) then
      begin
        if Odd(m) then
          Assistant(x, size - 1);
      end
      else
      begin
        if not Odd(m) then
          Assistant(x, size - 1);
        Assistant(x, 0);
      end;
      Inc(column);
      Dec(x, width);
    until (x < 0);
    // the alignment patterns: every other region, alternating per row
    var first := true;
    y := 0;
    row := 0;
    repeat
      var height := k;
      if (row >= m) then
        height := r - 1;
      var plot := first;
      first := not first;
      x := size - 1;
      column := 0;
      repeat
        var width := k;
        if (column >= m) then
          width := r - 1;
        if plot and not ((y = 0) and (x = size - 1)) then
          Alignment(x, y, width, height);
        plot := not plot;
        Inc(column);
        Dec(x, width);
      until (x < 0);
      Inc(row);
      Inc(y, height);
    until (y >= size);
  end;
  Result := grid;
end;

function HanXinFunctionPattern(version: Integer): TArray<Byte>;
const
  LIGHT = 1;
  DARK = 2;
  // (the function information, at the end 0)
  RESERVED = 3;
  // the finder patterns: top left, top right and bottom left, bottom right
  // (rows, the bits from $40 for the left)
  FINDERS: array [0 .. 2, 0 .. 6] of Byte = (($7F, $40, $5F, $50, $57, $57,
    $57), ($7F, $01, $7D, $05, $75, $75, $75), ($75, $75, $75, $05, $7D, $01,
    $7F));
var
  size: Integer;
  grid: TArray<Byte>;

  procedure Put(x, y: Integer; value: Byte);
  begin
    grid[y * size + x] := value;
  end;

  // only where nothing is yet
  procedure Plot(x, y: Integer; value: Byte);
  begin
    if (x >= 0) and (y >= 0) and (x < size) and (y < size) and
      (grid[y * size + x] = 0) then
      grid[y * size + x] := value;
  end;

  procedure Finder(finder, x, y: Integer);
  begin
    for var yp := 0 to 6 do
      for var xp := 0 to 6 do
        if (FINDERS[finder, yp] and ($40 shr xp) <> 0) then
          Put(x + xp, y + yp, DARK)
        else
          Put(x + xp, y + yp, LIGHT);
  end;

  procedure Alignment(x, y, w, h: Integer);
  begin
    Plot(x, y, DARK);
    Plot(x - 1, y + 1, LIGHT);
    for var i := 1 to w do
    begin
      Plot(x - i, y, DARK);
      Plot(x - i - 1, y + 1, LIGHT);
    end;
    for var i := 1 to h - 1 do
    begin
      Plot(x, y + i, DARK);
      Plot(x - 1, y + i + 1, LIGHT);
    end;
  end;

  procedure Assistant(x, y: Integer);
  begin
    for var dy := -1 to 1 do
      for var dx := -1 to 1 do
        if (dx = 0) and (dy = 0) then
          Plot(x, y, DARK)
        else
          Plot(x + dx, y + dy, LIGHT);
  end;

begin
  size := HanXinSize(version);
  SetLength(grid, size * size);
  for var i := 0 to High(grid) do
    grid[i] := 0;
  Finder(0, 0, 0);
  Finder(1, 0, size - 7);
  Finder(1, size - 7, 0);
  Finder(2, size - 7, size - 7);
  // the separators, then the function information
  for var i := 0 to 7 do
  begin
    Put(i, 7, LIGHT);
    Put(7, i, LIGHT);
    Put(size - 1 - i, 7, LIGHT);
    Put(7, size - 1 - i, LIGHT);
    Put(size - 8, i, LIGHT);
    Put(i, size - 8, LIGHT);
    Put(size - 1 - i, size - 8, LIGHT);
    Put(size - 8, size - 1 - i, LIGHT);
  end;
  for var i := 0 to 8 do
  begin
    Put(i, 8, RESERVED);
    Put(8, i, RESERVED);
    Put(size - 1 - i, 8, RESERVED);
    Put(8, size - 1 - i, RESERVED);
    Put(size - 9, i, RESERVED);
    Put(i, size - 9, RESERVED);
    Put(size - 1 - i, size - 9, RESERVED);
    Put(size - 9, size - 1 - i, RESERVED);
  end;
  if (version > 3) then
  begin
    // as in HanXinFunctionModules
    var k := MODULE_K[version - 1];
    var r := MODULE_R[version - 1];
    var m := MODULE_M[version - 1];
    var y := 0;
    var row := 0;
    repeat
      var height := k;
      if (row >= m) then
        height := r - 1;
      if not Odd(row) then
      begin
        if Odd(m) then
          Assistant(0, y);
      end
      else
      begin
        if not Odd(m) then
          Assistant(0, y);
        Assistant(size - 1, y);
      end;
      Inc(row);
      Inc(y, height);
    until (y >= size);
    var x := size - 1;
    var column := 0;
    repeat
      var width := k;
      if (column >= m) then
        width := r - 1;
      if not Odd(column) then
      begin
        if Odd(m) then
          Assistant(x, size - 1);
      end
      else
      begin
        if not Odd(m) then
          Assistant(x, size - 1);
        Assistant(x, 0);
      end;
      Inc(column);
      Dec(x, width);
    until (x < 0);
    var first := true;
    y := 0;
    row := 0;
    repeat
      var height := k;
      if (row >= m) then
        height := r - 1;
      var plotted := first;
      first := not first;
      x := size - 1;
      column := 0;
      repeat
        var width := k;
        if (column >= m) then
          width := r - 1;
        if plotted and not ((y = 0) and (x = size - 1)) then
          Alignment(x, y, width, height);
        plotted := not plotted;
        Inc(column);
        Dec(x, width);
      until (x < 0);
      Inc(row);
      Inc(y, height);
    until (y >= size);
  end;
  for var i := 0 to High(grid) do
    if (grid[i] = RESERVED) then
      grid[i] := 0;
  Result := grid;
end;

/// <summary>Corrects the block (the data codewords, then the check
/// codewords) with checks check codewords in the field; the number of
/// corrections, -1 when it can not be corrected.</summary>
function CorrectBlock(var block: TArray<Integer>; checks: Integer;
  field: TGenericGF): Integer;
begin
  var before := Copy(block);
  var decoder := TReedSolomonDecoder.Create(field);
  try
    if not decoder.decode(block, checks) then
      exit(-1);
  finally
    decoder.Free;
  end;
  Result := 0;
  for var i := 0 to High(block) do
    if (block[i] <> before[i]) then
      Inc(Result);
end;

function ReadHanXinFunctionInfo(const module: THanXinModule; size: Integer;
  out version, ecLevel, mask: Integer): Boolean;
begin
  Result := false;
  version := 0;
  ecLevel := 0;
  mask := 0;
  if (size < 23) then
    exit;
  // 2 copies: around the top left and top right finder patterns, around
  // the bottom right and bottom left ones
  for var copy := 0 to 1 do
  begin
    var bits: array [0 .. 33] of Boolean;
    for var i := 0 to 8 do
      if (copy = 0) then
      begin
        bits[i] := module(i, 8);
        bits[i + 8] := module(8, 8 - i);
        bits[i + 17] := module(size - 9, i);
        bits[i + 25] := module(size - 9 + i, 8);
      end
      else
      begin
        bits[i] := module(size - 1 - i, size - 9);
        bits[i + 8] := module(size - 9, size - 9 + i);
        bits[i + 17] := module(8, size - 1 - i);
        bits[i + 25] := module(8 - i, size - 9);
      end;
    // 3 data and 4 check codewords of 4 bits
    var codewords: TArray<Integer>;
    SetLength(codewords, 7);
    for var c := 0 to 6 do
    begin
      codewords[c] := 0;
      for var b := 0 to 3 do
        codewords[c] := 2 * codewords[c] + Ord(bits[4 * c + b]);
    end;
    if (CorrectBlock(codewords, 4, Field16) < 0) then
      continue;
    var v := 16 * codewords[0] + codewords[1] - 20;
    if (v < 1) or (v > 84) then
      continue;
    version := v;
    ecLevel := codewords[2] shr 2 + 1;
    mask := codewords[2] and 3;
    exit(true);
  end;
end;

function ReadHanXinFunctionInfo(const modules: TArray<Boolean>;
  size: Integer; out version, ecLevel, mask: Integer): Boolean;
begin
  version := 0;
  ecLevel := 0;
  mask := 0;
  if (Length(modules) <> size * size) then
    exit(false);
  var grid := modules;
  Result := ReadHanXinFunctionInfo(
    function(x, y: Integer): Boolean
    begin
      Result := grid[y * size + x];
    end, size, version, ecLevel, mask);
end;

type
  /// <summary>Reads the bits of bytes, the highest first.</summary>
  TBitReader = record
    Data: TArray<Integer>;
    Position: Integer;
    function Available: Integer;
    function Read(n: Integer): Integer;
  end;

function TBitReader.Available: Integer;
begin
  Result := 8 * Length(Data) - Position;
end;

function TBitReader.Read(n: Integer): Integer;
begin
  if (n > Available) then
    raise EArgumentException.Create('Han Xin');
  Result := 0;
  for var i := 0 to n - 1 do
  begin
    Result := 2 * Result + (Data[Position shr 3] shr (7 - Position and 7))
      and 1;
    Inc(Position);
  end;
end;

/// <summary>The text of the data codewords (the data modes of ISO/IEC
/// 20830 section 5.3); raises EArgumentException when they are invalid.
/// </summary>
function DecodeData(const data: TArray<Integer>): string;
const
  GB18030_ECI = 32;
var
  bits: TBitReader;
  content: TECIContent;
  eci: Integer;
  hanzi: Boolean;

  procedure Fail;
  begin
    raise EArgumentException.Create('Han Xin');
  end;

  // a character of 2 bytes of a Chinese mode: GB 18030 (or, after an ECI, of
  // that character set)
  procedure AddDouble(first, second: Integer);
  begin
    if (eci < 0) and ((Length(content.ECIs) = 0) or
      (content.ECIs[High(content.ECIs)] <> GB18030_ECI)) then
      content.SwitchCharset(GB18030_ECI);
    hanzi := true;
    content.Append(Byte(first));
    content.Append(Byte(second));
  end;

  procedure EndHanzi;
  begin
    if (eci < 0) then
      content.SwitchCharset(eci);
  end;

  procedure Region(two: Boolean);
  begin
    repeat
      var glyph := bits.Read(12);
      if (glyph = 4095) then
        break;
      if (glyph = 4094) then
      begin
        // into the other region directly
        two := not two;
        continue;
      end;
      if two then
      begin
        if (glyph >= $5E * 32) then
          Fail;
        AddDouble(glyph div $5E + $D8, glyph mod $5E + $A1);
      end
      else if (glyph < $EB0) then
        AddDouble(glyph div $5E + $B0, glyph mod $5E + $A1)
      else if (glyph < $FCA) then
        AddDouble((glyph - $EB0) div $5E + $A1, (glyph - $EB0) mod $5E + $A1)
      else if (glyph <= $FE9) then
        AddDouble($A8, glyph - $FCA + $A1)
      else
        Fail;
    until false;
    EndHanzi;
  end;

begin
  bits.Data := data;
  bits.Position := 0;
  content := TECIContent.Create('ISO-8859-1');
  eci := -1;
  hanzi := false;
  while (bits.Available >= 4) do
  begin
    var mode := bits.Read(4);
    case mode of
      0:
        // the padding
        break;
      1:
        begin
          // numeric: groups of 3 digits, the terminator the length of the
          // last one
          var values: TArray<Integer> := [];
          var last := 0;
          repeat
            var v := bits.Read(10);
            if (v >= 1021) then
            begin
              last := v - 1020;
              break;
            end;
            if (v > 999) then
              Fail;
            values := values + [v];
          until false;
          if (Length(values) = 0) then
            Fail;
          for var i := 0 to High(values) do
            if (i < High(values)) then
              content.Append(Format('%.3d', [values[i]]))
            else
            begin
              if (values[i] >= Round(IntPower(10, last))) then
                Fail;
              content.Append(Format('%.*d', [last, values[i]]));
            end;
        end;
      2:
        begin
          // text: 2 submodes of 6 bits
          var submode := 1;
          repeat
            var v := bits.Read(6);
            if (v = 63) then
              break;
            if (v = 62) then
            begin
              submode := 3 - submode;
              continue;
            end;
            var c: Integer;
            if (submode = 1) then
              case v of
                0 .. 9:
                  c := Ord('0') + v;
                10 .. 35:
                  c := Ord('A') + v - 10;
              else
                c := Ord('a') + v - 36;
              end
            else
              case v of
                0 .. 27:
                  c := v;
                28 .. 43:
                  c := Ord(' ') + v - 28;
                44 .. 50:
                  c := Ord(':') + v - 44;
                51 .. 56:
                  c := Ord('[') + v - 51;
              else
                c := Ord('{') + v - 57;
              end;
            content.Append(Byte(c));
          until false;
        end;
      3:
        begin
          // binary: a count, then the bytes
          var count := bits.Read(13);
          for var i := 1 to count do
            content.Append(Byte(bits.Read(8)));
        end;
      4, 5:
        // Common Chinese Region One and Two
        Region(mode = 5);
      6:
        begin
          // GB 18030 2-byte region
          repeat
            var glyph := bits.Read(15);
            if (glyph = 32767) then
              break;
            if (glyph >= $BE * 126) then
              Fail;
            var second := glyph mod $BE;
            if (second < $3F) then
              Inc(second, $40)
            else
              Inc(second, $41);
            AddDouble(glyph div $BE + $81, second);
          until false;
          EndHanzi;
        end;
      7:
        begin
          // GB 18030 4-byte region: one character
          var glyph := bits.Read(21);
          var first := glyph div $3138;
          glyph := glyph mod $3138;
          if (first > $FE - $81) then
            Fail;
          AddDouble(first + $81, glyph div $4EC + $30);
          glyph := glyph mod $4EC;
          content.Append(Byte(glyph div $0A + $81));
          content.Append(Byte(glyph mod $0A + $30));
          EndHanzi;
        end;
      8:
        begin
          // ECI: 8, 16 or 24 bits (0, 10 or 110 first)
          if (bits.Read(1) = 0) then
            eci := bits.Read(7)
          else if (bits.Read(1) = 0) then
            eci := bits.Read(14)
          else if (bits.Read(1) = 0) then
            eci := bits.Read(21)
          else
            Fail;
          content.SwitchEncoding(eci);
        end;
    else
      Fail;
    end;
  end;
  // (no ECI but Chinese characters: GB 18030 throughout, as zint makes it)
  if hanzi and not content.HasECI then
    content.DefaultEncoding := 'GB18030';
  Result := content.Text;
end;

function DecodeHanXin(const modules: TArray<Boolean>; size: Integer;
  out decoded: THanXinResult): Boolean;
begin
  Result := false;
  decoded.Text := '';
  decoded.Errors := 0;
  var version, ecLevel, mask: Integer;
  if not ReadHanXinFunctionInfo(modules, size, version, ecLevel, mask) or
    (HanXinSize(version) <> size) then
    exit;
  decoded.Version := version;
  decoded.ECLevel := ecLevel;
  decoded.Mask := mask;
  // the blocks
  var row := 4 * (version - 1) + ecLevel - 1;
  var total := 0;
  for var g := 0 to 2 do
    Inc(total, BLOCKS[row, 3 * g] * (BLOCKS[row, 3 * g + 1] +
      BLOCKS[row, 3 * g + 2]));
  // the codewords: the data modules row by row, unmasked
  var functions := HanXinFunctionModules(version);
  var stream: TArray<Integer>;
  SetLength(stream, total);
  for var i := 0 to total - 1 do
    stream[i] := 0;
  var n := 0;
  for var y := 0 to size - 1 do
  begin
    if (n >= 8 * total) then
      break;
    for var x := 0 to size - 1 do
    begin
      if functions[y * size + x] then
        continue;
      if (n >= 8 * total) then
        break;
      var dark := modules[y * size + x];
      var i := y + 1;
      var j := x + 1;
      case mask of
        1:
          dark := dark xor not Odd(i + j);
        2:
          dark := dark xor not Odd((i + j) mod 3 + j mod 3);
        3:
          dark := dark xor not Odd(i mod j + j mod i + i mod 3 + j mod 3);
      end;
      if dark then
        stream[n shr 3] := stream[n shr 3] or ($80 shr (n and 7));
      Inc(n);
    end;
  end;
  // the codewords in groups of 13 ("picket fence") back in order
  var full: TArray<Integer>;
  SetLength(full, total);
  var p := 0;
  for var start := 0 to 12 do
  begin
    var i := start;
    while (i < total) do
    begin
      full[i] := stream[p];
      Inc(p);
      Inc(i, 13);
    end;
  end;
  // the blocks corrected, their data codewords
  var data: TArray<Integer> := [];
  p := 0;
  for var g := 0 to 2 do
    for var b := 1 to BLOCKS[row, 3 * g] do
    begin
      var k := BLOCKS[row, 3 * g + 1];
      var checks := BLOCKS[row, 3 * g + 2];
      var block := Copy(full, p, k + checks);
      Inc(p, k + checks);
      var corrected := CorrectBlock(block, checks, Field256);
      if (corrected < 0) then
        exit;
      Inc(decoded.Errors, corrected);
      data := data + Copy(block, 0, k);
    end;
  try
    decoded.Text := DecodeData(data);
  except
    on EArgumentException do
      exit;
  end;
  Result := true;
end;

initialization

Field256 := TGenericGF.Create($163, $100, 1);
Field16 := TGenericGF.Create($13, $10, 1);

finalization

Field256.Free;
Field16.Free;

end.
