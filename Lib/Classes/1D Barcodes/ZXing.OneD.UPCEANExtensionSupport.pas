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

  * Delphi Implementation by K. Gossens
}

unit ZXing.OneD.UPCEANExtensionSupport;

interface

uses 
  System.SysUtils, 
  System.Generics.Collections,
  System.Math,
  ZXing.OneD.UPCEANExtension2Support,
  ZXing.OneD.UPCEANExtension5Support,
  ZXing.Reader,
  ZXing.BinaryBitmap,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultMetadataType,
  ZXing.ResultPoint,
  ZXing.Common.BitArray,
  ZXing.Common.Detector.MathUtils;

type
  TUPCEANExtensionSupport = class sealed
  private
    class var
      EXTENSION_START_PATTERN : TArray<Integer>;

      twoSupport : TUPCEANExtension2Support;
      fiveSupport : TUPCEANExtension5Support;

    class procedure InitializeClass; static;
    class procedure FinalizeClass; static;
  public
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const rowOffset: Integer): TReadResult;
  end;

implementation

uses
  ZXing.OneD.UPCEANReader;

{ TUPCEANExtensionSupport }

class procedure TUPCEANExtensionSupport.InitializeClass();
begin
  EXTENSION_START_PATTERN := TArray<Integer>.Create(1, 1, 2);
  twoSupport := TUPCEANExtension2Support.Create();
  fiveSupport := TUPCEANExtension5Support.Create();
end;

class procedure TUPCEANExtensionSupport.FinalizeClass();
begin
  EXTENSION_START_PATTERN := nil;
  twoSupport.Free;
  fiveSupport.Free;
end;

function TUPCEANExtensionSupport.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const rowOffset: Integer): TReadResult;
var
  extensionStartRange: TArray<Integer>;
  res : TReadResult;
begin
  Result := nil;

  extensionStartRange := TUPCEANReader.findGuardPattern(row, rowOffset,
    false, EXTENSION_START_PATTERN);
  if (extensionStartRange = nil)
  then
     exit;
  res := fiveSupport.decodeRow(rowNumber, row, extensionStartRange);
  if (res = nil)
  then
     res := twoSupport.decodeRow(rowNumber, row, extensionStartRange);
  Result := res;
end;

initialization
  TUPCEANExtensionSupport.InitializeClass;
finalization
  TUPCEANExtensionSupport.FinalizeClass;
end.
