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

unit ZXing.Common.GridSampler;

interface

uses
  System.SysUtils,
  ZXing.Common.Bitmatrix,
  ZXing.Common.PerspectiveTransform;

type
  TGridSampler = class abstract
  private
    class var FgridSampler: TGridSampler;
    //class constructor Create;
  protected
    constructor Create;
    class function checkAndNudgePoints(image: TBitMatrix; points: TArray<Single>): boolean; static;
  public
    function sampleGrid(image: TBitMatrix; dimensionX: Integer;
      dimensionY: Integer; transform: TPerspectiveTransform): TBitMatrix; overload; virtual;
    function sampleGrid(image: TBitMatrix; dimensionX: Integer;
      dimensionY: Integer; p1ToX: Single; p1ToY: Single; p2ToX: Single;
      p2ToY: Single; p3ToX: Single; p3ToY: Single; p4ToX: Single; p4ToY: Single;
      p1FromX: Single; p1FromY: Single; p2FromX: Single; p2FromY: Single;
      p3FromX: Single; p3FromY: Single; p4FromX: Single; p4FromY: Single): TBitMatrix; overload; virtual; abstract;
    procedure setGridSampler(newGridSampler: TGridSampler); //static;

    class property Instance: TGridSampler read FgridSampler;
  end;

implementation

{ TGridSampler }

constructor TGridSampler.Create;
begin
  inherited Create;
end;

class function TGridSampler.checkAndNudgePoints(image: TBitMatrix;
  points: TArray<Single>): boolean;
var
  width,
  height : Integer;
  nudged : Boolean;
  offset, maxOffset : Integer;
  x, y   : Single;
begin
  width  := image.Width;
  height := image.Height;
  nudged := true;
  offset := 0;
  maxOffset := Length(points) - 1; // points.length must be even

  while ((offset < maxOffset) and nudged) do
  begin
    x := points[offset];
    y := points[(offset + 1)];
    if (((x < -1) or (x > width)) or ((y < -1) or (y > height))) then
    begin
      Result := false;
      exit;
    end;
    nudged := false;
    if (x = -1) then
    begin
      points[offset] := 0;
      nudged := true;
    end
    else
       if (x = width) then
       begin
         points[offset] := (width - 1);
         nudged := true;
       end;
       if (y = -1) then
       begin
         points[(offset + 1)] := 0;
         nudged := true;
       end
       else
          if (y = height) then
          begin
            points[(offset + 1)] := (height - 1);
            nudged := true;
          end;
    Inc(offset, 2)
  end;
  nudged := true;
  offset := (Length(points) - 2);
  while (((offset >= 0) and nudged)) do
  begin
    x := points[offset];
    y := points[(offset + 1)];
    if (((x < -1) or (x > width)) or ((y < -1) or (y > height))) then
    begin
      Result := false;
      exit;
    end;
    nudged := false;
    if (x = -1) then
    begin
      points[offset] := 0;
      nudged := true;
    end else
           if (x = width) then
           begin
             points[offset] := (width - 1);
             nudged := true;
           end;
    if (y = -1) then
    begin
      points[(offset + 1)] := 0;
      nudged := true
    end else
           if (y = height) then
           begin
             points[(offset + 1)] := (height - 1);
             nudged := true;
           end;
    dec(offset, 2)
  end;

  Result := true;
end;

function TGridSampler.sampleGrid(image: TBitMatrix; dimensionX: Integer;
  dimensionY: Integer; transform: TPerspectiveTransform): TBitMatrix;
begin
  raise ENotSupportedException.Create('Not yet supported');
end;

procedure TGridSampler.setGridSampler(newGridSampler: TGridSampler);
begin
  if (newGridSampler = nil)
  then
     raise EArgumentException.Create('Missing arguments');
  FgridSampler := newGridSampler;
end;

end.