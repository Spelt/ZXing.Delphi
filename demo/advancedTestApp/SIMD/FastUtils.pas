unit FastUtils;
{ NEON/Arm64 optimized assembly routines for RGBA<>BGRA conversion, YV12>RGBA
  conversion and bitmap rotation }

interface

{$IFDEF IOS}
{ Swaps the Red and Blue components of pixels in a memory buffer.

  Parameters:
    ASrc: points to the source buffer containing the pixels to convert.
    ADst: points to the destination buffer for the converted pixels. May be
      the same as ASrc.
    APixelCount: the number of pixels to convert.

  The Src and Dst buffers must have room for at least PixelCount * 4 bytes. }
procedure SwapRB(const ASrc, ADst: Pointer; const APixelCount: Integer);
{$ENDIF}

{$IFDEF ANDROID}
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
{$ENDIF}

implementation

uses
  System.UITypes;

const
  {$IF Defined(IOS)}
  LIB_FAST_UTILS = 'libfastutils.a';
  _PU = '';
  {$ELSEIF Defined(ANDROID)}
  {$IF (RTLVersion >= 33)}
  { Delphi 10.3 Rio changed linking to Thumb mode. }
  LIB_FAST_UTILS = 'libfastutils-android-thumb.a';
  {$ELSE}
  LIB_FAST_UTILS = 'libfastutils-android.a';
  {$ENDIF}
  _PU = '_';
  {$ENDIF}

{$IFDEF IOS}
procedure swap_rb(ASrc, ADst: Pointer; ACount: Integer); cdecl;
  external LIB_FAST_UTILS name _PU + 'swap_rb';

procedure SwapRB(const ASrc, ADst: Pointer; const APixelCount: Integer);
var
  NeonCount, Remainder: Integer;
  S, D: PAlphaColorRec;
  Temp: Byte;
begin
  NeonCount := APixelCount shr 4;
  Remainder := APixelCount and 15;

  if (NeonCount > 0) then
    swap_rb(ASrc, ADst, NeonCount);

  if (Remainder > 0) then
  begin
    S := ASrc;
    D := ADst;
    Inc(S, NeonCount * 16);
    Inc(D, NeonCount * 16);
    if (ASrc = ADst) then
    begin
      while (Remainder > 0) do
      begin
        Temp := D.B;
        D.B := D.R;
        D.R := Temp;
        Inc(D);
        Dec(Remainder);
      end;
    end
    else
    begin
      while (Remainder > 0) do
      begin
        D.B := S.R;
        D.G := S.G;
        D.R := S.B;
        D.A := S.A;
        Inc(S);
        Inc(D);
        Dec(Remainder);
      end;
    end;
  end;
end;
{$ENDIF}

{$IFDEF ANDROID}
procedure yv12_to_rgba(AYPtr, AUPtr, AVPtr, ARGBAPtr: Pointer;
  AYStride, AUVStride, ARGBAStride, AWidth, AHeight: Integer); cdecl;
  external LIB_FAST_UTILS name _PU + 'yv12_to_rgba';

procedure YV12ToRGBA(const AYPtr, AUPtr, AVPtr, ARGBAPtr: Pointer;
  const AYStride, AUVStride, ARGBAStride, AWidth, AHeight: Integer);
begin
  yv12_to_rgba(AYPtr, AUPtr, AVPtr, ARGBAPtr, AYStride, AUVStride,
    ARGBAStride, AWidth and (not 15), AHeight and (not 1));
end;

procedure RotateBitmap0Degrees(const ASrc, ADst: Pointer; const ASrcWidth,
  ASrcHeight: Integer);
begin
  Move(ASrc^, ADst^, ASrcWidth * ASrcHeight * 4);
end;

procedure RotateBitmap90Degrees(ASrc, ADst: Pointer; ASrcWidth, ASrcHeight: Integer); cdecl;
  external LIB_FAST_UTILS name _PU + 'rotate_90';

procedure RotateBitmap180Degrees(ASrc, ADst: Pointer; ASrcWidth, ASrcHeight: Integer); cdecl;
  external LIB_FAST_UTILS name _PU + 'rotate_180';

procedure RotateBitmap270Degrees(ASrc, ADst: Pointer; ASrcWidth, ASrcHeight: Integer); cdecl;
  external LIB_FAST_UTILS name _PU + 'rotate_270';

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
{$ENDIF}

end.
