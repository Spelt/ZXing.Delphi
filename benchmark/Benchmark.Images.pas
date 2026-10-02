unit Benchmark.Images;

{
  * Loads png, jpg, gif and webp images and rotates them, for FMX
  * (FRAMEWORK_FMX) and VCL (FRAMEWORK_VCL), like the library itself.
  * On Windows both use WIC; webp needs the WebP codec of Windows, the
  * "Webp Image Extensions" package.
}

interface

uses
  System.SysUtils,
{$IFDEF FRAMEWORK_FMX}
  FMX.Graphics;
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  Vcl.Graphics;
{$ENDIF}

/// <summary>Loads an image; transparent pixels become white.</summary>
function LoadImage(const fileName: string): TBitmap;

/// <summary>Returns a new bitmap rotated by quarterTurns * 90 degrees
/// clockwise.</summary>
function RotateImage(const source: TBitmap; quarterTurns: Integer): TBitmap;

implementation

{$IFDEF FRAMEWORK_FMX}

uses
  System.UITypes;

type
  TPixel = packed record
    B, G, R, A: Byte;
  end;

  PPixelArray = ^TPixelArray;
  TPixelArray = array [0 .. MaxInt div SizeOf(TPixel) - 1] of TPixel;

function LoadImage(const fileName: string): TBitmap;
var
  data: TBitmapData;
  x, y: Integer;
  row: PPixelArray;
begin
  Result := TBitmap.CreateFromFile(fileName);
  try
    // FMX bitmaps are premultiplied BGRA: blend over white
    if Result.Map(TMapAccess.ReadWrite, data) then
      try
        for y := 0 to data.Height - 1 do
        begin
          row := data.GetScanline(y);
          for x := 0 to data.Width - 1 do
            if (row[x].A < 255) then
            begin
              row[x].B := row[x].B + (255 - row[x].A);
              row[x].G := row[x].G + (255 - row[x].A);
              row[x].R := row[x].R + (255 - row[x].A);
              row[x].A := 255;
            end;
        end;
      finally
        Result.Unmap(data);
      end;
  except
    Result.Free;
    raise;
  end;
end;

function RotateImage(const source: TBitmap; quarterTurns: Integer): TBitmap;
var
  src, dst: TBitmapData;
  x, y, w, h: Integer;
  srcRow: PPixelArray;
  rows: array of PPixelArray;
begin
  quarterTurns := ((quarterTurns mod 4) + 4) mod 4;
  w := source.Width;
  h := source.Height;
  if Odd(quarterTurns) then
    Result := TBitmap.Create(h, w)
  else
    Result := TBitmap.Create(w, h);

  if source.Map(TMapAccess.Read, src) then
    try
      if Result.Map(TMapAccess.Write, dst) then
        try
          SetLength(rows, dst.Height);
          for y := 0 to dst.Height - 1 do
            rows[y] := dst.GetScanline(y);
          for y := 0 to h - 1 do
          begin
            srcRow := src.GetScanline(y);
            for x := 0 to w - 1 do
              case quarterTurns of
                0: rows[y][x] := srcRow[x];
                1: rows[x][h - 1 - y] := srcRow[x];
                2: rows[h - 1 - y][w - 1 - x] := srcRow[x];
                3: rows[w - 1 - x][y] := srcRow[x];
              end;
          end;
        finally
          Result.Unmap(dst);
        end;
    finally
      source.Unmap(src);
    end;
end;

{$ENDIF}

{$IFDEF FRAMEWORK_VCL}

type
  TRGBTriple = packed record
    B, G, R: Byte;
  end;

  TRGBQuad = packed record
    B, G, R, A: Byte;
  end;

  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0 .. MaxInt div SizeOf(TRGBTriple) - 1] of TRGBTriple;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [0 .. MaxInt div SizeOf(TRGBQuad) - 1] of TRGBQuad;

function LoadImage(const fileName: string): TBitmap;
var
  wic: TWICImage;
  source: TBitmap;
  x, y: Integer;
  src: PRGBQuadArray;
  dst: PRGBTripleArray;
  a: Integer;
begin
  wic := TWICImage.Create;
  source := TBitmap.Create;
  try
    wic.LoadFromFile(fileName);
    source.Assign(wic);

    Result := TBitmap.Create;
    Result.PixelFormat := pf24bit;
    Result.SetSize(source.Width, source.Height);
    if (source.PixelFormat = pf32bit) then
    begin
      // blend over white, like a transparent image shown on a white page
      for y := 0 to source.Height - 1 do
      begin
        src := source.ScanLine[y];
        dst := Result.ScanLine[y];
        for x := 0 to source.Width - 1 do
        begin
          a := src[x].A;
          if (source.AlphaFormat = afPremultiplied) then
          begin
            dst[x].B := src[x].B + (255 - a);
            dst[x].G := src[x].G + (255 - a);
            dst[x].R := src[x].R + (255 - a);
          end
          else
          begin
            dst[x].B := (src[x].B * a + 255 * (255 - a)) div 255;
            dst[x].G := (src[x].G * a + 255 * (255 - a)) div 255;
            dst[x].R := (src[x].R * a + 255 * (255 - a)) div 255;
          end;
        end;
      end;
    end
    else
      Result.Canvas.Draw(0, 0, source);
  finally
    source.Free;
    wic.Free;
  end;
end;

function RotateImage(const source: TBitmap; quarterTurns: Integer): TBitmap;
var
  x, y, w, h: Integer;
  src: PRGBTripleArray;
  rows: array of PRGBTripleArray;
begin
  quarterTurns := ((quarterTurns mod 4) + 4) mod 4;
  w := source.Width;
  h := source.Height;

  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  if Odd(quarterTurns) then
    Result.SetSize(h, w)
  else
    Result.SetSize(w, h);

  SetLength(rows, Result.Height);
  for y := 0 to Result.Height - 1 do
    rows[y] := Result.ScanLine[y];

  for y := 0 to h - 1 do
  begin
    src := source.ScanLine[y];
    for x := 0 to w - 1 do
      case quarterTurns of
        0: rows[y][x] := src[x];
        1: rows[x][h - 1 - y] := src[x];
        2: rows[h - 1 - y][w - 1 - x] := src[x];
        3: rows[w - 1 - x][y] := src[x];
      end;
  end;
end;

{$ENDIF}

end.
