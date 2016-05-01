unit ZXing.Common.EncodingOptions;

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

  * Implemented by K. Gossens for Delphi
}

unit ZXing.Datamatrix.DatamatrixEncodingOptions;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.EncodeHintType;
  
type
  TEncodingOptions = class
  private
    FHeight,
	FWidth,
    FMargin      : Integer;
	FHints       : IDictionary<TEncodeHintType, TObject>;
	FPureBarcode : Boolean;
  public
    constructor Create;
	
	property Height: Integer read FHeight write FHeight;
	property Hints: IDictionary<TEncodeHintType, TObject>; read FHints write FHints;
	property Margin: Integer read FMargin write FMargin;
	property PuteBarcode: Boolean read FPureBarcode write FPureBarcode;
	property Width: Integer read FWidth write FWidth;
  end;

implementation

{ TEncodingOptions }

constructor Create;
begin
  inherited;
  
  Self.Hints := TDictionary<TEncodeHintType, TObject>.Create;
end;


end.