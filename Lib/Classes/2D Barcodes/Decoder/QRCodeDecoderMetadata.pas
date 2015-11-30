unit QRCodeDecoderMetadata;

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

uses ResultPoint;

type

  TQRCodeDecoderMetaData = class sealed

  private
    FMirrored: boolean;
    function get_IsMirrored: boolean;
  public
    property IsMirrored: boolean read get_IsMirrored;
    constructor Create(mirrored: boolean);
    procedure applyMirroredCorrection(points: TArray<TResultPoint>);
  end;

implementation

constructor TQRCodeDecoderMetaData.Create(mirrored: boolean);
begin
  FMirrored := mirrored
end;

function TQRCodeDecoderMetaData.get_IsMirrored: boolean;
begin
  Result := FMirrored;
end;

procedure TQRCodeDecoderMetaData.applyMirroredCorrection
  (points: TArray<TResultPoint>);
var
  bottomLeft: TResultPoint;
begin
  if ((FMirrored and (points <> nil)) and (Length(points) >= 3)) then
  begin
    bottomLeft := points[0];
    points[0] := points[2];
    points[2] := bottomLeft
  end
end;

end.
