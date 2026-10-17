unit ZXing.Helpers;

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
}

interface

type
  TArray = class
    /// <summary>A copy of the array, of any length.</summary>
    class function Clone(original: TArray<Integer>): TArray<Integer>; static;
    /// <summary>A copy of the whole array with the elements StartIndex to
    /// StartIndex + Len - 1 moved to the front, the rest as it was (the
    /// arraycopy of Java onto the same array).</summary>
    class function CopyInSameArray(const Input: TArray<Integer>;
      StartIndex: Integer; Len: Integer): TArray<Integer>; static;
  end;

implementation

uses
  System.SysUtils;

class function TArray.Clone(original: TArray<Integer>): TArray<Integer>;
begin
  // (the length went through a SmallInt before: an array of more than
  // 32767 elements failed or came back truncated)
  Result := Copy(original);
end;

class function TArray.CopyInSameArray(const Input: TArray<Integer>;
  StartIndex: Integer; Len: Integer): TArray<Integer>;
begin
  if (StartIndex < 0) or (Len < 0) or (StartIndex + Len > Length(Input)) then
    raise EArgumentOutOfRangeException.Create
      ('CopyInSameArray: the range is outside the array');
  Result := Copy(Input);
  if (Len > 0) then
    Move(Input[StartIndex], Result[0], Len * SizeOf(Integer));
end;

end.
