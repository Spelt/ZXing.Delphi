unit ZXing.Common.Detector.MathUtils;

{
  * Copyright 2008 ZXing authors
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

  * Implemented by E. Spelt for Delphi
  * Restructured by K. Gossens
}
interface

uses
  System.SysUtils;

type
  TMathUtils = class sealed
  public
    class function distance(const aX, aY, bX, bY: Double): Single; static;
    class function round(d: Single): Integer; static;
    /// <summary>The arithmetic shift right of Value (Java's >>), for
    /// ShiftBits 0 to 31. Inline: this is used in the inner loops.
    /// </summary>
    class function Asr(Value: Integer; ShiftBits: Integer): Integer; overload;
      static; inline;
    class function Asr(Value: Int64; ShiftBits: Integer): Int64; overload;
      static;
    /// <summary>The number of trailing zero bits of w, which must not be 0
    /// (de Bruijn sequence).</summary>
    class function TrailingZeros(w: Cardinal): Integer; static; inline;
  end;

const
  // de Bruijn sequence: the number of trailing zero bits of w is
  // DE_BRUIJN_BITS[((w and -w) * DE_BRUIJN) shr 27]
  DE_BRUIJN = $077CB531;
  DE_BRUIJN_BITS: array [0 .. 31] of Byte = (0, 1, 28, 2, 29, 14, 24, 3, 30,
    22, 20, 15, 25, 17, 4, 8, 31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11,
    5, 10, 9);

implementation

{ TMathUtils }

class function TMathUtils.distance(const aX, aY, bX, bY: Double): Single;
var
  xDiff, yDiff: double;
begin
  xDiff := aX - bX;
  yDiff := aY - bY;
  result := Sqrt(xDiff * xDiff + yDiff * yDiff);
end;

class function TMathUtils.round(d: Single): Integer;
begin
  if (Single.IsNaN(d)) then
  begin
    Result := 0;
    exit
  end;
  if (Single.IsPositiveInfinity(d)) then
  begin
    Result := $7fffffff;
    exit;
  end;



  if (d < 0)
  then
     Result := Trunc(d + (-0.5))
  else
     Result := Trunc(d + 0.5);
end;

class function TMathUtils.Asr(Value: Integer; ShiftBits: Integer): Integer;
begin
  // shr is a logical shift: for a negative value shift its complement,
  // which is not negative, and complement the result (the sign bits come
  // back as ones)
  if (Value >= 0) then
    Result := Value shr ShiftBits
  else
    Result := not ((not Value) shr ShiftBits);
end;

class function TMathUtils.Asr(Value: Int64; ShiftBits: Integer): Int64;
begin
  result := Value shr ShiftBits;
  if (Value and $8000000000000000) > 0 then
    result := result or ($FFFFFFFFFFFFFFFF shl (64 - ShiftBits));
end;

class function TMathUtils.TrailingZeros(w: Cardinal): Integer;
begin
{$IFOPT Q+}{$DEFINE MATHUTILS_Q}{$Q-}{$ENDIF}
  Result := DE_BRUIJN_BITS[((w and (not w + 1)) * DE_BRUIJN) shr 27];
{$IFDEF MATHUTILS_Q}{$Q+}{$UNDEF MATHUTILS_Q}{$ENDIF}
end;

end.
