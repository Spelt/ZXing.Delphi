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

  * Original Author: Sean Owen  
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.Common.BitArray;

interface

type
  IBitArray = interface
    ['{3D65F451-E408-4015-A637-73CD05877BCB}']
     // property getters and setters
     function GetBit(i: Integer): Boolean;
     procedure SetBit(i: Integer; Value: Boolean);
     function GetBits: TArray<Integer>;

     function Size: Integer;
     function SizeInBytes: Integer;

     property Self[i: Integer]: Boolean read GetBit write SetBit; default;
     property Bits: TArray<Integer> read GetBits;

     function getNextSet(from: Integer): Integer;
     function getNextUnset(from: Integer): Integer;

     procedure setBulk(i, newBits: Integer);
     procedure setRange(start, ending: Integer);
     procedure appendBit(bit: Boolean);
     procedure Reverse();
     procedure clear();

     function isRange(start, ending: Integer; const value: Boolean): Boolean;
  end;

 TBitArrayHelpers = class
    class function CreateBitArray:IBitArray; overload;
    class function CreateBitArray(const Size: Integer):IBitArray; overload;
 end;

implementation
uses ZXing.Common.BitArrayImplementation;

class function TBitArrayHelpers.CreateBitArray:IBitArray;
begin
  result := ZXing.Common.BitArrayImplementation.NewBitArray;
end;


class function TBitArrayHelpers.CreateBitArray(const Size: Integer):IBitArray;
begin
  result := ZXing.Common.BitArrayImplementation.NewBitArray(size);
end;


end.
