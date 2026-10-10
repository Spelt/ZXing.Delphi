unit CommonTest;

{
  * Tests for the data structures of the library: TBitArray, TBitMatrix,
  * the pattern rows, the Galois field and the luminance sources. They
  * compare the fast implementations with plain reference loops.
}

{$POINTERMATH ON}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCommonTest = class(TObject)
  public
    [Test]
    procedure MathUtilsShifts;
    [Test]
    procedure GeometryFloorInt;
    [Test]
    procedure BitArrayGetSetNext;
    [Test]
    procedure BitArrayReverse;
    [Test]
    procedure BitArrayRanges;
    [Test]
    procedure BitMatrixFlipAndOutside;
    [Test]
    procedure BitMatrixSetRegion;
    [Test]
    procedure PatternRowFromMatrix;
    [Test]
    procedure GaloisFieldMultiply;
    [Test]
    procedure LuminanceRotateAndCrop;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  ZXing.Common.Detector.MathUtils,
  ZXing.Common.Geometry,
  ZXing.Common.BitArray,
  ZXing.Common.BitMatrix,
  ZXing.Common.Pattern,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.LuminanceSource,
  ZXing.RGBLuminanceSource;

/// <summary>A fixed pseudo random sequence (the same on every run).</summary>
function NextRandom(var seed: Cardinal): Cardinal;
begin
  seed := seed * 1103515245 + 12345;
  Result := seed shr 8;
end;

procedure TCommonTest.MathUtilsShifts;
begin
  Assert.AreEqual(25, TMathUtils.Asr(100, 2));
  Assert.AreEqual(0, TMathUtils.Asr(31, 5));
  Assert.AreEqual(-1, TMathUtils.Asr(-1, 5));
  Assert.AreEqual(-1, TMathUtils.Asr(-32, 5));
  Assert.AreEqual(-2, TMathUtils.Asr(-33, 5));
  Assert.AreEqual(-1, TMathUtils.Asr(-1, 31));
  Assert.AreEqual(Integer($C0000000), TMathUtils.Asr(Integer($80000000), 1));
  Assert.AreEqual(-5, TMathUtils.Asr(-5, 0));
  Assert.AreEqual<Int64>(-2, TMathUtils.Asr(Int64(-33), 5));

  Assert.AreEqual(0, TMathUtils.TrailingZeros(1));
  Assert.AreEqual(3, TMathUtils.TrailingZeros(8));
  Assert.AreEqual(3, TMathUtils.TrailingZeros($FFFFFFF8));
  Assert.AreEqual(31, TMathUtils.TrailingZeros($80000000));
  for var i := 0 to 31 do
    Assert.AreEqual<Integer>(i, TMathUtils.TrailingZeros(Cardinal(1) shl i));
end;

procedure TCommonTest.GeometryFloorInt;
begin
  // the fast floor and trunc must be the same as Floor and Trunc: whole
  // numbers, halves (Round is to even), values just below and above them,
  // negative values
  for var i := -2000 to 2000 do
  begin
    var x: Double := i / 4;
    for var delta in [0.0, 1E-9, -1E-9, 0.1, -0.1, 0.49999, 0.5, 0.50001] do
    begin
      var v := x + delta;
      Assert.AreEqual(Floor(v), FloorInt(v), 'floor ' + FloatToStr(v));
      Assert.AreEqual(Trunc(v), TruncInt(v), 'trunc ' + FloatToStr(v));
    end;
  end;
  var c := Centered(PointD(3.99, -0.5));
  Assert.AreEqual(3.5, c.X, 1E-12);
  Assert.AreEqual(-0.5, c.Y, 1E-12);
  Assert.AreEqual(0, PixelX(PointD(-0.5, 0)));
  Assert.AreEqual(7, PixelY(PointD(0, 7.999)));
end;

procedure TCommonTest.BitArrayGetSetNext;
const
  SIZE = 100;
var
  expected: array [0 .. SIZE - 1] of Boolean;
begin
  var seed: Cardinal := 7;
  var bits := TBitArrayHelpers.CreateBitArray(SIZE);
  Assert.AreEqual(SIZE, bits.Size);
  for var i := 0 to SIZE - 1 do
  begin
    expected[i] := (NextRandom(seed) and 3) = 0;
    bits[i] := expected[i];
  end;
  // clearing a bit must work too
  bits[5] := true;
  bits[5] := false;
  expected[5] := false;
  bits[64] := true;
  expected[64] := true;
  for var i := 0 to SIZE - 1 do
    Assert.AreEqual(expected[i], bits[i], 'bit ' + i.ToString);

  // getNextSet / getNextUnset against a plain scan
  for var from := 0 to SIZE do
  begin
    var nextSet := SIZE;
    var nextUnset := SIZE;
    for var i := from to SIZE - 1 do
      if expected[i] then
      begin
        nextSet := i;
        break;
      end;
    for var i := from to SIZE - 1 do
      if not expected[i] then
      begin
        nextUnset := i;
        break;
      end;
    Assert.AreEqual(nextSet, bits.getNextSet(from), 'next set from ' +
      from.ToString);
    Assert.AreEqual(nextUnset, bits.getNextUnset(from), 'next unset from ' +
      from.ToString);
  end;

  // a word of all ones and a size that ends inside a word
  var ones := TBitArrayHelpers.CreateBitArray(40);
  ones.setBulk(0, -1);
  Assert.AreEqual(32, ones.getNextUnset(0));
  Assert.AreEqual(40, ones.getNextSet(32));
  ones.clear;
  Assert.AreEqual(40, ones.getNextSet(0));
  Assert.AreEqual(0, ones.getNextUnset(0));
end;

procedure TCommonTest.BitArrayReverse;
begin
  for var size in [1, 31, 32, 33, 64, 77, 100] do
  begin
    var seed: Cardinal := Cardinal(size);
    var bits := TBitArrayHelpers.CreateBitArray(size);
    var expected: TArray<Boolean>;
    SetLength(expected, size);
    for var i := 0 to size - 1 do
    begin
      expected[i] := (NextRandom(seed) and 1) = 0;
      bits[i] := expected[i];
    end;
    bits.Reverse;
    for var i := 0 to size - 1 do
      Assert.AreEqual(expected[size - 1 - i], bits[i],
        Format('size %d bit %d', [size, i]));
    // the bits beyond the size stay clear (the next word is not touched)
    var words := bits.Bits;
    if ((size and 31) <> 0) then
      Assert.AreEqual(0, Integer(Cardinal(words[High(words)]) shr (size and 31)),
        'padding of size ' + size.ToString);
  end;
end;

procedure TCommonTest.BitArrayRanges;
begin
  var bits := TBitArrayHelpers.CreateBitArray(100);
  bits.setRange(3, 70);
  for var i := 0 to 99 do
    Assert.AreEqual((i >= 3) and (i < 70), bits[i], 'bit ' + i.ToString);
  Assert.IsTrue(bits.isRange(3, 70, true));
  Assert.IsTrue(bits.isRange(0, 3, false));
  Assert.IsTrue(bits.isRange(70, 100, false));
  Assert.IsFalse(bits.isRange(2, 70, true));
  Assert.IsFalse(bits.isRange(3, 71, true));
  Assert.IsFalse(bits.isRange(0, 4, false));
  Assert.IsTrue(bits.isRange(50, 50, true)); // empty range
  // one word, inside
  bits.clear;
  bits.setRange(33, 40);
  Assert.IsTrue(bits.isRange(33, 40, true));
  Assert.IsTrue(bits.isRange(0, 33, false));
  Assert.IsTrue(bits.isRange(40, 100, false));
  Assert.AreEqual(33, bits.getNextSet(0));
  Assert.AreEqual(40, bits.getNextUnset(33));
end;

procedure TCommonTest.BitMatrixFlipAndOutside;
begin
  var m := TBitMatrix.Create(40, 10);
  try
    Assert.AreEqual(2, m.RowSize);
    m.flip(35, 2);
    Assert.IsTrue(m[35, 2]);
    Assert.IsFalse(m[3, 2]); // (35 and 31) = 3: not the bit of the first word
    Assert.IsFalse(m[35, 1]);
    m.flip(35, 2);
    Assert.IsFalse(m[35, 2]);
    m[0, 0] := true;
    m[39, 9] := true;
    Assert.IsTrue(m[0, 0]);
    Assert.IsTrue(m[39, 9]);
    // outside: false, no exception
    Assert.IsFalse(m[-1, 0]);
    Assert.IsFalse(m[40, 0]);
    Assert.IsFalse(m[0, -1]);
    Assert.IsFalse(m[0, 10]);
    Assert.IsFalse(m[-1, 9]);
    Assert.IsFalse(m[31, -1]);
    m[-1, 0] := true; // ignored
    m[40, 9] := true;
    Assert.IsFalse(m[31, 0]);
    Assert.IsFalse(m[8, 9]);
    // the words of a row
    var words := m.RowWords(9);
    Assert.AreEqual(Integer(1 shl 7), words[1]);
  finally
    m.Free;
  end;
end;

procedure TCommonTest.BitMatrixSetRegion;
begin
  var seed: Cardinal := 11;
  for var n := 1 to 40 do
  begin
    var width := 1 + Integer(NextRandom(seed) mod 100);
    var height := 1 + Integer(NextRandom(seed) mod 6);
    var left := Integer(NextRandom(seed) mod Cardinal(width));
    var top := Integer(NextRandom(seed) mod Cardinal(height));
    var w := 1 + Integer(NextRandom(seed) mod Cardinal(width - left));
    var h := 1 + Integer(NextRandom(seed) mod Cardinal(height - top));
    var m := TBitMatrix.Create(width, height);
    try
      // some bits set before, which must stay
      m[0, 0] := true;
      m[width - 1, height - 1] := true;
      m.setRegion(left, top, w, h);
      for var y := 0 to height - 1 do
        for var x := 0 to width - 1 do
        begin
          var expected := ((x >= left) and (x < left + w) and (y >= top) and
            (y < top + h)) or ((x = 0) and (y = 0)) or
            ((x = width - 1) and (y = height - 1));
          Assert.AreEqual(expected, m[x, y],
            Format('%dx%d region %d,%d %dx%d at %d,%d', [width, height, left,
            top, w, h, x, y]));
        end;
    finally
      m.Free;
    end;
  end;
end;

procedure TCommonTest.PatternRowFromMatrix;
begin
  var seed: Cardinal := 3;
  for var width in [1, 5, 32, 33, 64, 100, 131] do
  begin
    var m := TBitMatrix.Create(width, 6);
    try
      for var y := 0 to 5 do
      begin
        // runs of random length, the last row all black, the first all white
        var x := 0;
        var black := (y and 1) = 1;
        while (x < width) do
        begin
          var len := 1 + Integer(NextRandom(seed) mod 9);
          if (y = 5) then
          begin
            black := true;
            len := width;
          end;
          if (y > 0) then
            for var i := x to Min(x + len, width) - 1 do
              m[i, y] := black;
          Inc(x, len);
          black := not black;
        end;
      end;
      for var y := 0 to 5 do
      begin
        // the reference: run lengths, starting with the white run (0 when
        // the row starts black), ending with the white run
        var expected: TArray<Integer> := [];
        var color := false;
        var run := 0;
        for var x := 0 to width - 1 do
        begin
          if (m[x, y] <> color) then
          begin
            expected := expected + [run];
            run := 0;
            color := not color;
          end;
          Inc(run);
        end;
        expected := expected + [run];
        if color then
          expected := expected + [0];

        var fromMatrix: TPatternRow;
        GetPatternRow(m, y, fromMatrix);
        var fromBits: TPatternRow;
        GetPatternRow(m.getRow(y, nil), width, fromBits);
        var name := Format('width %d row %d', [width, y]);
        Assert.AreEqual(Length(expected), Length(fromMatrix), name + ' length');
        Assert.AreEqual(Length(expected), Length(fromBits), name + ' length (bits)');
        for var i := 0 to High(expected) do
        begin
          Assert.AreEqual(expected[i], fromMatrix[i], name + ' run ' + i.ToString);
          Assert.AreEqual(expected[i], fromBits[i], name + ' run ' + i.ToString + ' (bits)');
        end;
      end;
    finally
      m.Free;
    end;
  end;
end;

/// <summary>The product of a and b in GF(size) with the primitive polynomial,
/// bit by bit (the reference for the table lookup).</summary>
function SlowMultiply(a, b, primitive, size: Integer): Integer;
begin
  Result := 0;
  while (b <> 0) do
  begin
    if ((b and 1) <> 0) then
      Result := Result xor a;
    b := b shr 1;
    a := a shl 1;
    if (a >= size) then
      a := (a xor primitive) and (size - 1);
  end;
end;

procedure TCommonTest.GaloisFieldMultiply;
begin
  // QR Code and Data Matrix: GF(256); Aztec parameters: GF(16)
  var fields: TArray<TGenericGF> := [TGenericGF.QR_CODE_FIELD_256,
    TGenericGF.DATA_MATRIX_FIELD_256, TGenericGF.AZTEC_PARAM,
    TGenericGF.AZTEC_DATA_6];
  var primitives: TArray<Integer> := [$11D, $12D, $13, $43];
  for var f := 0 to High(fields) do
  begin
    var field := fields[f];
    var size := field.size;
    for var a := 0 to size - 1 do
      for var b := 0 to size - 1 do
        Assert.AreEqual(SlowMultiply(a, b, primitives[f], size),
          field.multiply(a, b), Format('%d * %d in GF(%d)', [a, b, size]));
    // exp and log are inverse, and the inverse multiplies to 1
    for var i := 0 to size - 2 do
      Assert.AreEqual(i, field.log(field.exp(i)));
    for var a := 1 to size - 1 do
      Assert.AreEqual(1, field.multiply(a, field.inverse(a)));
  end;
end;

procedure TCommonTest.LuminanceRotateAndCrop;
const
  W = 7;
  H = 5;
begin
  var pixels: TArray<Byte>;
  SetLength(pixels, W * H);
  for var i := 0 to High(pixels) do
    pixels[i] := Byte(i * 3 + 1);
  var source := TRGBLuminanceSource.Create(pixels, W, H, TBitmapFormat.Gray8);
  try
    // the source has its own copy
    pixels[0] := 200;
    Assert.AreEqual(1, Integer(source.Matrix[0]));

    var rotated := source.rotateCounterClockwise;
    try
      Assert.AreEqual(H, rotated.Width);
      Assert.AreEqual(W, rotated.Height);
      // counter clockwise: the top row of the source becomes the left
      // column, read from the bottom up
      var m := rotated.Matrix;
      for var ynew := 0 to W - 1 do
        for var xnew := 0 to H - 1 do
        begin
          var xold := W - 1 - ynew;
          var yold := xnew;
          Assert.AreEqual(Integer(source.Matrix[yold * W + xold]),
            Integer(m[ynew * H + xnew]), Format('rotated %d,%d', [xnew, ynew]));
        end;
      // rotated back (three more quarter turns) it is the source again
      var half := rotated.rotateCounterClockwise;
      var threeQuarters := half.rotateCounterClockwise;
      var back := threeQuarters.rotateCounterClockwise;
      try
        for var i := 0 to W * H - 1 do
          Assert.AreEqual(Integer(source.Matrix[i]), Integer(back.Matrix[i]));
      finally
        back.Free;
        threeQuarters.Free;
        half.Free;
      end;
      // getRow is a copy of the row
      var row := rotated.getRow(2, nil);
      Assert.AreEqual(H, Length(row));
      for var x := 0 to H - 1 do
        Assert.AreEqual(Integer(m[2 * H + x]), Integer(row[x]));
    finally
      rotated.Free;
    end;

    var cropped := source.crop(2, 1, 3, 3);
    try
      Assert.AreEqual(3, cropped.Width);
      Assert.AreEqual(3, cropped.Height);
      for var y := 0 to 2 do
        for var x := 0 to 2 do
          Assert.AreEqual(Integer(source.Matrix[(y + 1) * W + x + 2]),
            Integer(cropped.Matrix[y * 3 + x]), Format('cropped %d,%d', [x, y]));
    finally
      cropped.Free;
    end;

    // the inverted source: 255 - the value, the source itself unchanged
    var inverted := source.invert;
    try
      for var i := 0 to W * H - 1 do
        Assert.AreEqual(255 - Integer(source.Matrix[i]),
          Integer(inverted.Matrix[i]));
    finally
      inverted.Free;
    end;
  finally
    source.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TCommonTest);

end.
