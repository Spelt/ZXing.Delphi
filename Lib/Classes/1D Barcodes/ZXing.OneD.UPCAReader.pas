unit ZXing.OneD.UPCAReader;
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

  * Original Authors: Sean Owen
  * Delphi Implementation by E. Spelt
}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Math,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.BinaryBitmap,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat,
  ZXing.Helpers,
  ZXing.OneD.EAN13Reader,
  ZXing.OneD.UPCEANReader;

type

  TUPCAReader = class(TUPCEANReader)
  private
    class var EAN13Reader: TUPCEANReader;
    class function maybeReturnResult(pResult: TReadResult): TReadResult; static;
    class procedure DoInitialize();
    class procedure DoFinalize();
  protected

  public
    class function DecodeMiddle(const row: IBitArray;
      const startRange: TArray<Integer>; const resultString: TStringBuilder)
      : Integer; override;

    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; override;

    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      overload; override;

    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const startGuardRange: TArray<Integer>;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      reintroduce; overload;

    function BarcodeFormat: TBarcodeFormat; override;
  end;

implementation

{ TUPCAReader }

class procedure TUPCAReader.DoFinalize;
begin
  EAN13Reader.Free;
end;

class procedure TUPCAReader.DoInitialize;
begin
  EAN13Reader := TEAN13Reader.Create();
end;

function TUPCAReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  result := TUPCAReader.maybeReturnResult(self.EAN13Reader.decode(image, hints))
end;

class function TUPCAReader.DecodeMiddle(const row: IBitArray;
  const startRange: TArray<Integer>;
  const resultString: TStringBuilder): Integer;
begin
  result := self.EAN13Reader.DecodeMiddle(row, startRange, resultString)
end;

function TUPCAReader.decodeRow(const rowNumber: Integer; const row: IBitArray;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  result := TUPCAReader.maybeReturnResult(self.EAN13Reader.decodeRow(rowNumber,
    row, hints))
end;

function TUPCAReader.decodeRow(const rowNumber: Integer; const row: IBitArray;
  const startGuardRange: TArray<Integer>;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  result := TUPCAReader.maybeReturnResult
    (self.EAN13Reader.doDecodeRow(rowNumber, row, startGuardRange, hints))
end;

function TUPCAReader.BarcodeFormat: TBarcodeFormat;
begin
  result := TBarcodeFormat.UPC_A;
end;

class function TUPCAReader.maybeReturnResult(pResult: TReadResult): TReadResult;
begin
  if not Assigned(pResult) then
    Exit(nil);

{$ZEROBASEDSTRINGS ON}
  if (copy(pResult.text, 0, 1) = '0') then
  begin
    pResult.text := copy(pResult.text, 2, length(pResult.text));
    pResult.BarcodeFormat := TBarcodeFormat.UPC_A;
  end;
{$ZEROBASEDSTRINGS OFF}
  result := pResult;

end;

initialization

TUPCAReader.DoInitialize;

finalization

TUPCAReader.DoFinalize;


end.
