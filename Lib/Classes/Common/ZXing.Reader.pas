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

  * Original Authors: Sean Owen and dswitkin@google.com (Daniel Switkin)
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.Reader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.BinaryBitmap,
  ZXing.ReadResult,
  ZXing.DecodeHintType;

type
  /// <summary>
  /// Implementations of this interface can decode an image of a barcode in some format into
  /// the String it encodes. For example, <see cref="ZXing.QrCode.QRCodeReader" /> can
  /// decode a QR code. The decoder may optionally receive hints from the caller which may help
  /// it decode more quickly or accurately.
  ///
  /// See <see cref="MultiFormatReader" />, which attempts to determine what barcode
  /// format is present within the image as well, and then decodes it accordingly.
  /// </summary>
  IReader = interface
    /// <summary>
    /// Locates and decodes a barcode in some format within an image.
    /// </summary>
    /// <param name="image">image of barcode to decode</param>
    /// <returns>String which the barcode encodes</returns>
    function decode(const image: TBinaryBitmap): TReadResult; overload;

    /// <summary> Locates and decodes a barcode in some format within an image. This method also accepts
    /// hints, each possibly associated to some data, which may help the implementation decode.
    /// </summary>
    /// <param name="image">image of barcode to decode</param>
    /// <param name="hints">passed as a <see cref="IDictionary{TKey, TValue}" /> from <see cref="DecodeHintType" />
    /// to arbitrary data. The
    /// meaning of the data depends upon the hint type. The implementation may or may not do
    /// anything with these hints.
    /// </param>
    /// <returns>String which the barcode encodes</returns>
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;

    /// <summary>
    /// Resets any internal state the implementation has after a decode, to prepare it
    /// for reuse.
    /// </summary>
    procedure Reset();
  end;

  /// <summary>
  /// A reader that can find several barcodes in one image.
  /// </summary>
  IMultipleReader = interface
    ['{6F1B6F0E-2C56-4D8B-9A4E-0B7E5B1C3D21}']
    /// <summary>
    /// Adds the barcodes found in image to results (the caller owns them),
    /// at most maxCount in total (0: no limit). Results that are already in
    /// results (the same text and format at the same place) are not added
    /// again.
    /// </summary>
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
  end;

/// <summary>Whether r is the same barcode as one in results: the same format
/// and text, at an overlapping place.</summary>
function ContainsResult(results: TList<TReadResult>; r: TReadResult): Boolean;
/// <summary>Whether results holds maxCount results (0: no limit).</summary>
function ResultsFull(results: TList<TReadResult>; maxCount: Integer): Boolean;
/// <summary>Whether a and b are readings of the same 1D code (from rows
/// close to each other), possibly in another format or with another text,
/// like UPC-A and EAN-13 or with and without add-on.</summary>
function IsSameLinearSymbol(a, b: TReadResult): Boolean;

implementation

uses
  System.Math,
  ZXing.BarcodeFormat,
  ZXing.ResultPoint;

/// <summary>The bounding box of the position of r, enlarged by a quarter of
/// its size (at least 10 pixels) in every direction. A line (the position of
/// a 1D code, found on one row) is enlarged by its length across, as the
/// same code is found on other rows too.</summary>
procedure GetArea(r: TReadResult; out minX, minY, maxX, maxY: Single);
begin
  var pos := r.Position;
  minX := MaxInt;
  minY := MaxInt;
  maxX := -MaxInt;
  maxY := -MaxInt;
  for var p in pos do
  begin
    if (p.x < minX) then
      minX := p.x;
    if (p.x > maxX) then
      maxX := p.x;
    if (p.y < minY) then
      minY := p.y;
    if (p.y > maxY) then
      maxY := p.y;
  end;
  var w: Single := maxX - minX;
  var h: Single := maxY - minY;
  var margin: Single := (w + h) / 4;
  if (margin < 10) then
    margin := 10;
  var marginX := margin;
  var marginY := margin;
  // a (nearly) horizontal or vertical line
  if (h < w / 4) then
    marginY := Max(marginY, w);
  if (w < h / 4) then
    marginX := Max(marginX, h);
  minX := minX - marginX;
  minY := minY - marginY;
  maxX := maxX + marginX;
  maxY := maxY + marginY;
end;

function Is2D(r: TReadResult): Boolean;
begin
  var f := r.BarcodeFormat;
  Result := (f = TBarcodeFormat.QR_CODE) or (f = TBarcodeFormat.DATA_MATRIX) or
    (f = TBarcodeFormat.AZTEC) or (f = TBarcodeFormat.PDF_417) or
    (f = TBarcodeFormat.MAXICODE);
end;

/// <summary>Whether the scanned lines of two 1D results lie on top of each
/// other: overlapping along the line and at most half its length apart
/// across (with any distance across when maxAcross is false).</summary>
function SameLine(a, b: TReadResult; maxAcross: Boolean = true): Boolean;

  procedure box(r: TReadResult; out minX, minY, maxX, maxY: Single);
  begin
    minX := MaxInt;
    minY := MaxInt;
    maxX := -MaxInt;
    maxY := -MaxInt;
    for var p in r.Position do
    begin
      minX := Min(minX, p.x);
      minY := Min(minY, p.y);
      maxX := Max(maxX, p.x);
      maxY := Max(maxY, p.y);
    end;
  end;

begin
  var aMinX, aMinY, aMaxX, aMaxY, bMinX, bMinY, bMaxX, bMaxY: Single;
  box(a, aMinX, aMinY, aMaxX, aMaxY);
  box(b, bMinX, bMinY, bMaxX, bMaxY);
  var len: Single := Max(Max(aMaxX - aMinX, aMaxY - aMinY), 10);
  var across: Single := len / 2;
  if not maxAcross then
    across := MaxInt;
  // horizontal lines: overlap in x, near in y; vertical ones the other way
  if (aMaxX - aMinX >= aMaxY - aMinY) then
    Result := (aMinX <= bMaxX) and (bMinX <= aMaxX) and
      (Abs((aMinY + aMaxY) - (bMinY + bMaxY)) / 2 <= across)
  else
    Result := (aMinY <= bMaxY) and (bMinY <= aMaxY) and
      (Abs((aMinX + aMaxX) - (bMinX + bMaxX)) / 2 <= across);
end;

function IsSameLinearSymbol(a, b: TReadResult): Boolean;
begin
  Result := not Is2D(a) and not Is2D(b) and (a.Position <> nil) and
    (b.Position <> nil) and SameLine(a, b);
end;

function ContainsResult(results: TList<TReadResult>; r: TReadResult): Boolean;
begin
  var pos := r.Position;
  var cx: Single := 0;
  var cy: Single := 0;
  for var p in pos do
  begin
    cx := cx + p.x / System.Length(pos);
    cy := cy + p.y / System.Length(pos);
  end;

  for var other in results do
  begin
    if (other.BarcodeFormat = r.BarcodeFormat) and (other.Text = r.Text) then
    begin
      if (pos = nil) or (other.Position = nil) then
        exit(true);
      // a 1D code is found on several rows of its height
      if not Is2D(r) and SameLine(other, r, false) then
        exit(true);
      var minX, minY, maxX, maxY: Single;
      GetArea(other, minX, minY, maxX, maxY);
      if (cx >= minX) and (cx <= maxX) and (cy >= minY) and (cy <= maxY) then
        exit(true);
    end
    // another reading of the same 1D code, like UPC-A and EAN-13 or with and
    // without add-on
    else if IsSameLinearSymbol(other, r) then
      exit(true);
  end;
  Result := false;
end;

function ResultsFull(results: TList<TReadResult>; maxCount: Integer): Boolean;
begin
  Result := (maxCount > 0) and (results.Count >= maxCount);
end;

end.
