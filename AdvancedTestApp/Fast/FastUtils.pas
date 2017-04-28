unit FastUtils;
{ Optimized Delphi routines for RGBA<>BGRA conversion, YV12>RGBA conversion and
  bitmap rotation }

interface

{ Swaps the Red and Blue components of pixels in a memory buffer.

  Parameters:
    ASrc: points to the source buffer containing the pixels to convert.
    ADst: points to the destination buffer for the converted pixels. May be
      the same as ASrc.
    APixelCount: the number of pixels to convert.

  The Src and Dst buffers must have room for at least PixelCount * 4 bytes. }
procedure SwapRB(const ASrc, ADst: Pointer; const APixelCount: Integer);

{ Converts a buffer from YV12 format to RGBA format.

  Parameters:
    AYPtr: pointer to the source Y (luminance) plane
    AUPtr: pointer to the source U (chrominance) plane
    AVPtr: pointer to the source V (chrominance) plane
    ARGBAPtr: pointer to the target interleaved RGBA buffer
    AYStride: number of bytes between two rows in the Y plane
    AUVStride: number of bytes between two rows in the U and V planes
    ARGBAStride: number of bytes between two rows in the RGBA buffer
    AWidth: width of the image to convert (must be even).
    AHeight: height of the image to convert (must be even).

  The buffers must be large enough to accomodate AWidth * AHeight pixels in
  their corresponding format.

  The alpha values will be set to $FF (opaque)

  NOTE: Currently, this procedure only works when AWidth is a multiple of 16 and
  AHeight is a multiple of 2. If this is not the case, then some rightmost and
  bottommost pixels will not get converted. }
procedure YV12ToRGBA(const AYPtr, AUPtr, AVPtr, ARGBAPtr: Pointer;
  const AYStride, AUVStride, ARGBAStride, AWidth, AHeight: Integer);

{ Rotates a 32-bit bitmap 0, 90, 180 or 270 degrees.

  Parameters:
    ASrc: pointer to the source bitmap data
    ADst: pointer to the target bitmap data. This must be different than ASrc.
      The ASrc and ADst buffers may not share any data.
    ASrcWidth: width of the source bitmap (must be a multiple of 4)
    ASrcHeight: height of the source bitmap (must be a multiple of 4)
    AAngle: the angle of rotation (0, 90, 180 or 270)

  When rotating 0 or 180 degrees, the dimensions of ADst must match the
  dimensions of ASrc. When rotating 90 or 270 degrees, the width of ASrc must
  match the height of ADst and the height of ASrc must match the width of ADst.

  Furthermore, this routine expects the dimensions to be a multiple of 4. If
  not, an assertion will be raised and the results may be unpredictable. }
procedure RotateBitmap(const ASrc, ADst: Pointer; const ASrcWidth,
  ASrcHeight: Integer; const AAngle: Integer);

implementation

uses
  System.UITypes;

procedure SwapRB(const ASrc, ADst: Pointer; const APixelCount: Integer);
var
  I: Integer;
  S, D: PAlphaColorRec;
  Temp: Byte;
begin
  if (ASrc = ADst) then
  begin
    D := ADst;
    for I := 0 to APixelCount - 1 do
    begin
      Temp := D.B;
      D.B := D.R;
      D.R := Temp;
      Inc(D);
    end;
  end
  else
  begin
    S := ASrc;
    D := ADst;
    for I := 0 to APixelCount - 1 do
    begin
      D.B := S.R;
      D.G := S.G;
      D.R := S.B;
      D.A := S.A;
      Inc(S);
      Inc(D);
    end;
  end;
end;

const
  SHIFT    = 6;
  MAX_VAL  = (256 shl SHIFT) - 1;

function Clip(const AValue: Integer): Integer; inline;
begin
  if (AValue < 0) then
    Result := 0
  else if (AValue > MAX_VAL) then
    Result := MAX_VAL
  else
    Result := AValue;
end;

procedure YV12ToRGBA(const AYPtr, AUPtr, AVPtr, ARGBAPtr: Pointer;
  const AYStride, AUVStride, ARGBAStride, AWidth, AHeight: Integer);
{ This version uses relative low-precision 2.6 integer arithmetic to match the
  NEON version.

  Formulas:
    R := 1.164*(Y-16)                 + 1.596*(V-128)
    G := 1.164*(Y-16) - 0.391*(U-128) - 0.813*(V-128)
    B := 1.164*(Y-16) + 2.018*(U-128)

  We use 2.6 integer arithmetic, so the formulas become:
    R := (74*(Y-16)               + 102*(V-128)) shr 6
    G := (74*(Y-16) -  25*(U-128) -  52*(V-128)) shr 6
    B := (74*(Y-16) + 129*(U-128)              ) shr 6

  NOTE: we use "127*(U-128)" instead of "129*(U-128)" to match the ARM SIMD
  version. }
const
  Y_BIAS   = 16;
  UV_BIAS  = 128;

  U_TO_G   = 25;
  U_TO_B   = 127; // Should be 129
  V_TO_R   = 102;
  V_TO_G   = 52;
  Y_TO_RGB = 74;

  R_INDEX  = 0;
  G_INDEX  = 1;
  B_INDEX  = 2;
  A_INDEX  = 3;
var
  PaddedWidth, RGBAExtra, YExtra, UVExtra, Row, Col, Y, U, V: Integer;
  VtoR, UtoB, UVtoG: Integer;
  YPtr, UPtr, VPtr, RGBA: PByte;
begin
  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  Assert(Assigned(AYPtr));
  Assert(Assigned(AUPtr));
  Assert(Assigned(AVPtr));
  Assert(Assigned(ARGBAPtr));
  Assert(AYStride > 0);
  Assert(AUVStride > 0);
  Assert(ARGBAStride > 0);
  Assert((AWidth and 1) = 0);
  Assert((AHeight and 1) = 0);

  PaddedWidth := (AWidth + 1) and (not 1);
  RGBAExtra := (ARGBAStride - (4 * PaddedWidth)) + ARGBAStride;
  YExtra := (AYStride - PaddedWidth) + AYStride;
  UVExtra := AUVStride - (PaddedWidth shr 1);

  YPtr := AYPtr;
  UPtr := AUPtr;
  VPtr := AVPtr;
  RGBA := ARGBAPtr;

  Row := 0;
  while (Row < AHeight) do
  begin
    Col := 0;
    while (Col < AWidth) do
    begin
      U := UPtr[0] - UV_BIAS;
      V := VPtr[0] - UV_BIAS;

      VtoR  := V_TO_R * V;
      UVtoG := (U_TO_G * U) + (V_TO_G * V);
      UtoB  := U_TO_B * U;

      Y := (YPtr[0] - Y_BIAS) * Y_TO_RGB;
      RGBA[R_INDEX] := Clip(Y + VtoR)  shr SHIFT;
      RGBA[G_INDEX] := Clip(Y - UVtoG) shr SHIFT;
      RGBA[B_INDEX] := Clip(Y + UtoB)  shr SHIFT;
      RGBA[A_INDEX] := $FF;

      Y := (YPtr[1] - Y_BIAS) * Y_TO_RGB;
      RGBA[R_INDEX + 4] := Clip(Y + VtoR)  shr SHIFT;
      RGBA[G_INDEX + 4] := Clip(Y - UVtoG) shr SHIFT;
      RGBA[B_INDEX + 4] := Clip(Y + UtoB)  shr SHIFT;
      RGBA[A_INDEX + 4] := $FF;

      Y := (YPtr[AYStride] - Y_BIAS) * Y_TO_RGB;
      RGBA[ARGBAStride + R_INDEX] := Clip(Y + VtoR)  shr SHIFT;
      RGBA[ARGBAStride + G_INDEX] := Clip(Y - UVtoG) shr SHIFT;
      RGBA[ARGBAStride + B_INDEX] := Clip(Y + UtoB)  shr SHIFT;
      RGBA[ARGBAStride + A_INDEX] := $FF;

      Y := (YPtr[AYStride + 1] - Y_BIAS) * Y_TO_RGB;
      RGBA[ARGBAStride + R_INDEX + 4] := Clip(Y + VtoR)  shr SHIFT;
      RGBA[ARGBAStride + G_INDEX + 4] := Clip(Y - UVtoG) shr SHIFT;
      RGBA[ARGBAStride + B_INDEX + 4] := Clip(Y + UtoB)  shr SHIFT;
      RGBA[ARGBAStride + A_INDEX + 4] := $FF;

      Inc(RGBA, 8);
      Inc(YPtr, 2);
      Inc(UPtr);
      Inc(VPtr);
      Inc(Col, 2);
    end;
    Inc(RGBA, RGBAExtra);
    Inc(YPtr, YExtra);
    Inc(UPtr, UVExtra);
    Inc(VPtr, UVExtra);
    Inc(Row, 2);
  end;
end;

procedure RotateBitmap0Degrees(const ASrc, ADst: Pointer; const ASrcWidth,
  ASrcHeight: Integer);
begin
  Move(ASrc^, ADst^, ASrcWidth * ASrcHeight * 4);
end;

procedure RotateBitmap90Degrees(const ASrc, ADst: Pointer; const ASrcWidth,
  ASrcHeight: Integer);
var
  S, D: PCardinal;
  DstX, DstY, DstDelta: Integer;
begin
  S := ASrc;
  D := ADst;
  Inc(D, ASrcHeight - 1);

  DstDelta := (ASrcWidth * ASrcHeight) + 1;
  for DstX := 0 to ASrcHeight - 1 do
  begin
    for DstY := 0 to ASrcWidth - 1 do
    begin
      D^ := S^;
      Inc(S);
      Inc(D, ASrcHeight);
    end;
    Dec(D, DstDelta);
  end;
end;

procedure RotateBitmap180Degrees(const ASrc, ADst: Pointer; const ASrcWidth,
  ASrcHeight: Integer);
var
  S, D: PCardinal;
  I: Integer;
begin
  S := ASrc;
  D := ADst;
  Inc(D, (ASrcWidth * ASrcHeight) - 1);
  for I := 0 to (ASrcWidth * ASrcHeight) - 1 do
  begin
    D^ := S^;
    Inc(S);
    Dec(D);
  end;
end;

procedure RotateBitmap270Degrees(const ASrc, ADst: Pointer; const ASrcWidth,
  ASrcHeight: Integer);
var
  S, D: PCardinal;
  DstX, DstY, SrcDelta: Integer;
begin
  S := ASrc;
  D := ADst;
  Inc(S, ASrcWidth - 1);

  SrcDelta := (ASrcHeight * ASrcWidth) + 1;
  for DstY := 0 to ASrcWidth - 1 do
  begin
    for DstX := 0 to ASrcHeight - 1 do
    begin
      D^ := S^;
      Inc(S, ASrcWidth);
      Inc(D);
    end;
    Dec(S, SrcDelta);
  end;
end;

procedure RotateBitmap(const ASrc, ADst: Pointer; const ASrcWidth,
  ASrcHeight: Integer; const AAngle: Integer);
begin
  if (ASrcWidth <= 0) or (ASrcHeight <= 0) then
    Exit;

  Assert(Assigned(ASrc));
  Assert(Assigned(ADst));
  Assert((ASrcHeight and 3) = 0);
  Assert((ASrcWidth and 3) = 0);

  case AAngle of
      0: RotateBitmap0Degrees(ASrc, ADst, ASrcWidth, ASrcHeight);
     90: RotateBitmap90Degrees(ASrc, ADst, ASrcWidth, ASrcHeight);
    180: RotateBitmap180Degrees(ASrc, ADst, ASrcWidth, ASrcHeight);
    270: RotateBitmap270Degrees(ASrc, ADst, ASrcWidth, ASrcHeight);
  else
    Assert(False);
  end;
end;

end.
