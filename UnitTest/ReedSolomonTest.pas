unit ReedSolomonTest;

{
  * Tests for the Reed-Solomon decoder: more errors than can be corrected
  * must not give a "corrected" codeword (like zxing-cpp).
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TReedSolomonTest = class(TObject)
  public
    [Test]
    procedure CorrectsErrors;
    [Test]
    procedure TooManyErrorsOddECCount;
    [Test]
    procedure TooManyErrors;
  end;

implementation

uses
  System.SysUtils,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder;

const
  LENGTH = 20;

/// <summary>The number of positions where a and b differ.</summary>
function Distance(const a, b: TArray<Integer>): Integer;
begin
  Result := 0;
  for var i := 0 to High(a) do
    if (a[i] <> b[i]) then
      Inc(Result);
end;

/// <summary>A codeword of zeros (a valid codeword) with numErrors random
/// errors.</summary>
function WithErrors(numErrors: Integer): TArray<Integer>;
begin
  SetLength(Result, LENGTH);
  var count := 0;
  while (count < numErrors) do
  begin
    var p := Random(LENGTH);
    if (Result[p] = 0) then
    begin
      Result[p] := 1 + Random(255);
      Inc(count);
    end;
  end;
end;

/// <summary>Decodes random codewords with numErrors errors and checks that
/// every codeword that is decoded is a valid one at most maxChanges
/// positions away. Returns how many were decoded.</summary>
function DecodeRandom(numErrors, twoS, maxChanges: Integer): Integer;
begin
  Result := 0;
  var rs := TReedSolomonDecoder.Create(TGenericGF.QR_CODE_FIELD_256);
  try
    for var trial := 1 to 2000 do
    begin
      var received := WithErrors(numErrors);
      var original := Copy(received);
      if rs.decode(received, twoS) then
      begin
        Inc(Result);
        Assert.IsTrue(Distance(original, received) <= maxChanges,
          Format('%d positions corrected', [Distance(original, received)]));
        // a valid codeword: nothing more to correct
        var again := Copy(received);
        Assert.IsTrue(rs.decode(again, twoS), 'not a valid codeword');
        Assert.AreEqual(0, Distance(again, received), 'changed again');
      end;
    end;
  finally
    rs.Free;
  end;
end;

procedure TReedSolomonTest.CorrectsErrors;
begin
  RandSeed := 1;
  // 4 error correction codewords correct 2 errors, always to the zeros
  var rs := TReedSolomonDecoder.Create(TGenericGF.QR_CODE_FIELD_256);
  try
    for var trial := 1 to 200 do
    begin
      var received := WithErrors(2);
      Assert.IsTrue(rs.decode(received, 4), 'not corrected');
      Assert.AreEqual(0, Distance(received, WithErrors(0)), 'wrong correction');
    end;
  finally
    rs.Free;
  end;
end;

procedure TReedSolomonTest.TooManyErrorsOddECCount;
begin
  RandSeed := 2;
  // 3 error correction codewords correct 1 error: 2 errors must not be
  // "corrected" at 2 positions
  DecodeRandom(2, 3, 1);
end;

procedure TReedSolomonTest.TooManyErrors;
begin
  RandSeed := 3;
  // 3 errors with 4 error correction codewords: at most 2 positions change,
  // and the result is a valid codeword
  DecodeRandom(3, 4, 2);
end;

initialization

TDUnitX.RegisterTestFixture(TReedSolomonTest);

end.
