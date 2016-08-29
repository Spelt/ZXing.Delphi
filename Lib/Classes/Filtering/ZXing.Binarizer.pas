unit ZXing.Binarizer;
{
  * Copyright 2009 ZXing authors
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

  * 2015-3 Adapted for Delphi/Object Pascal FireMonkey XE7 mobile by E.Spelt
}

interface

uses
  System.SysUtils,
  ZXing.LuminanceSource,
  ZXing.Common.BitArray,
  ZXing.Common.BitMatrix;

type

  /// <summary> This class hierarchy provides a set of methods to convert luminance data to 1 bit data.
  /// It allows the algorithm to vary polymorphically, for example allowing a very expensive
  /// thresholding technique for servers and a fast one for mobile. It also permits the implementation
  /// to vary, e.g. a JNI version for Android and a Java fallback version for other platforms.
  ///
  /// <author>dswitkin@google.com (Daniel Switkin)</author>
  /// </summary>
  TBinarizer = class
  private
    FSource: TLuminanceSource;
    function GetWidth: Integer;
    function GetHeight: Integer;
  protected
    /// <summary>
    /// Initializes a new instance of the <see cref="Binarizer"/> class.
    /// </summary>
    /// <param name="source">The source.</param>
    constructor Create(source: TLuminanceSource);

    /// <summary> Converts one row of luminance data to 1 bit data. May actually do the conversion, or return
    /// cached data. Callers should assume this method is expensive and call it as seldom as possible.
    /// This method is intended for decoding 1D barcodes and may choose to apply sharpening.
    /// For callers which only examine one row of pixels at a time, the same BitArray should be reused
    /// and passed in with each call for performance. However it is legal to keep more than one row
    /// at a time if needed.
    /// </summary>
    /// <param name="y">The row to fetch, which must be in [0, bitmap height)</param>
    /// <param name="row">An optional preallocated array. If null or too small, it will be ignored.
    /// If used, the Binarizer will call BitArray.clear(). Always use the returned object.
    /// </param>
    /// <returns> The array of bits for this row (true means black).</returns>
  public
    function LuminanceSource(): TLuminanceSource;
    function GetBlackRow(y: Integer; row: IBitArray): IBitArray;
      virtual; abstract;

    function BlackMatrix: TBitMatrix; virtual; abstract;
    destructor Destroy();override;

    /// <summary> Creates a new object with the same type as this Binarizer implementation, but with pristine
    /// state. This is needed because Binarizer implementations may be stateful, e.g. keeping a cache
    /// of 1 bit data. See Effective Java for why we can't use Java's clone() method.
    /// </summary>
    /// <param name="source">The LuminanceSource this Binarizer will operate on.</param>
    /// <returns> A new concrete Binarizer implementation object.</returns>
    function createBinarizer(source: TLuminanceSource): TBinarizer;
      virtual; abstract;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;



  end;

implementation

{ TBinarizer }

constructor TBinarizer.Create(source: TLuminanceSource);
begin

  if (source = nil) then
  begin
    raise EArgumentException.Create('Source must be non-null.');
  end;

  Self.FSource := source;

end;

destructor TBinarizer.Destroy;
begin
  inherited;
end;

function TBinarizer.GetHeight: Integer;
begin
  Result:= FSource.Height;
end;

function TBinarizer.GetWidth: Integer;
begin
  result := FSource.Width;
end;

function TBinarizer.LuminanceSource: TLuminanceSource;
begin
  result := Self.FSource;
end;

end.
