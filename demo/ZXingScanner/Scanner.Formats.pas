unit Scanner.Formats;

{
  The barcode formats the demo lets you switch on and off: their names, as
  the TBarcodeFormat values are called in code, and whether a format is read
  by default (TBarcodeFormat.Auto) or only when asked for.

  Copyright 2026 E Spelt
  Licensed under the Apache License, Version 2.0.
}

interface

uses
  ZXing.BarcodeFormat;

type
  TFormatInfo = record
    Format: TBarcodeFormat;
    Name: string;
    /// <summary>True: read with TBarcodeFormat.Auto. False: only read when
    /// the format is asked for (slower, or without a check against false
    /// positives, or a kind of another format).</summary>
    InAuto: Boolean;
  end;

const
  Formats: array [0 .. 51] of TFormatInfo = (
    // 2D
    (Format: TBarcodeFormat.QR_CODE; Name: 'QR_CODE'; InAuto: True),
    (Format: TBarcodeFormat.MICRO_QR_CODE; Name: 'MICRO_QR_CODE'; InAuto: True),
    (Format: TBarcodeFormat.RMQR_CODE; Name: 'RMQR_CODE'; InAuto: True),
    (Format: TBarcodeFormat.DATA_MATRIX; Name: 'DATA_MATRIX'; InAuto: True),
    (Format: TBarcodeFormat.AZTEC; Name: 'AZTEC'; InAuto: True),
    (Format: TBarcodeFormat.PDF_417; Name: 'PDF_417'; InAuto: True),
    (Format: TBarcodeFormat.MICRO_PDF417; Name: 'MICRO_PDF417'; InAuto: True),
    (Format: TBarcodeFormat.MAXICODE; Name: 'MAXICODE'; InAuto: True),
    // 1D
    (Format: TBarcodeFormat.CODE_128; Name: 'CODE_128'; InAuto: True),
    (Format: TBarcodeFormat.CODE_93; Name: 'CODE_93'; InAuto: True),
    (Format: TBarcodeFormat.CODE_39; Name: 'CODE_39'; InAuto: True),
    (Format: TBarcodeFormat.EAN_13; Name: 'EAN_13'; InAuto: True),
    (Format: TBarcodeFormat.EAN_8; Name: 'EAN_8'; InAuto: True),
    (Format: TBarcodeFormat.UPC_A; Name: 'UPC_A'; InAuto: True),
    (Format: TBarcodeFormat.UPC_E; Name: 'UPC_E'; InAuto: True),
    (Format: TBarcodeFormat.ITF; Name: 'ITF'; InAuto: True),
    (Format: TBarcodeFormat.CODABAR; Name: 'CODABAR'; InAuto: True),
    (Format: TBarcodeFormat.TELEPEN; Name: 'TELEPEN'; InAuto: True),
    (Format: TBarcodeFormat.RSS_14; Name: 'RSS_14'; InAuto: True),
    (Format: TBarcodeFormat.RSS_EXPANDED; Name: 'RSS_EXPANDED'; InAuto: True),
    (Format: TBarcodeFormat.RSS_LIMITED; Name: 'RSS_LIMITED'; InAuto: True),
    (Format: TBarcodeFormat.DX_FILM_EDGE; Name: 'DX_FILM_EDGE'; InAuto: True),
    // stacked and composite
    (Format: TBarcodeFormat.CODABLOCK_F; Name: 'CODABLOCK_F'; InAuto: True),
    (Format: TBarcodeFormat.CODE_16K; Name: 'CODE_16K'; InAuto: True),
    (Format: TBarcodeFormat.CODE_49; Name: 'CODE_49'; InAuto: True),
    (Format: TBarcodeFormat.GS1_COMPOSITE; Name: 'GS1_COMPOSITE'; InAuto: True),
    // only when asked for
    (Format: TBarcodeFormat.DOTCODE; Name: 'DOTCODE'; InAuto: False),
    (Format: TBarcodeFormat.HAN_XIN; Name: 'HAN_XIN'; InAuto: False),
    (Format: TBarcodeFormat.CODE_32; Name: 'CODE_32'; InAuto: False),
    (Format: TBarcodeFormat.PZN; Name: 'PZN'; InAuto: False),
    (Format: TBarcodeFormat.MSI; Name: 'MSI'; InAuto: False),
    (Format: TBarcodeFormat.PLESSEY; Name: 'PLESSEY'; InAuto: False),
    (Format: TBarcodeFormat.PHARMA_CODE; Name: 'PHARMA_CODE'; InAuto: False),
    (Format: TBarcodeFormat.PHARMA_CODE_TWO_TRACK; Name: 'PHARMA_CODE_TWO_TRACK'; InAuto: False),
    (Format: TBarcodeFormat.CODE_11; Name: 'CODE_11'; InAuto: False),
    (Format: TBarcodeFormat.INDUSTRIAL_2_OF_5; Name: 'INDUSTRIAL_2_OF_5'; InAuto: False),
    (Format: TBarcodeFormat.IATA_2_OF_5; Name: 'IATA_2_OF_5'; InAuto: False),
    (Format: TBarcodeFormat.MATRIX_2_OF_5; Name: 'MATRIX_2_OF_5'; InAuto: False),
    (Format: TBarcodeFormat.DATALOGIC_2_OF_5; Name: 'DATALOGIC_2_OF_5'; InAuto: False),
    (Format: TBarcodeFormat.DP_LEITCODE; Name: 'DP_LEITCODE'; InAuto: False),
    (Format: TBarcodeFormat.DP_IDENTCODE; Name: 'DP_IDENTCODE'; InAuto: False),
    // postal
    (Format: TBarcodeFormat.KIX; Name: 'KIX'; InAuto: False),
    (Format: TBarcodeFormat.RM4SCC; Name: 'RM4SCC'; InAuto: False),
    (Format: TBarcodeFormat.IMB; Name: 'IMB'; InAuto: False),
    (Format: TBarcodeFormat.POSTNET; Name: 'POSTNET'; InAuto: False),
    (Format: TBarcodeFormat.PLANET; Name: 'PLANET'; InAuto: False),
    (Format: TBarcodeFormat.JAPAN_POST; Name: 'JAPAN_POST'; InAuto: False),
    (Format: TBarcodeFormat.AUSTRALIA_POST; Name: 'AUSTRALIA_POST'; InAuto: False),
    (Format: TBarcodeFormat.MAILMARK_4STATE; Name: 'MAILMARK_4STATE'; InAuto: False),
    (Format: TBarcodeFormat.CEPNET; Name: 'CEPNET'; InAuto: False),
    (Format: TBarcodeFormat.KOREA_POST; Name: 'KOREA_POST'; InAuto: False),
    (Format: TBarcodeFormat.FIM; Name: 'FIM'; InAuto: False)
  );

/// <summary>The name of a format as in code (QR_CODE), also for the ones
/// not in the table above.</summary>
function FormatName(format: TBarcodeFormat): string;
/// <summary>The format of a name from FormatName; False for an unknown
/// name.</summary>
function FormatByName(const name: string; out format: TBarcodeFormat): Boolean;

implementation

uses
  System.SysUtils;

function FormatName(format: TBarcodeFormat): string;
begin
  for var info in Formats do
    if (info.Format = format) then
      exit(info.Name);
  case format of
    TBarcodeFormat.Auto:
      Result := 'Auto';
    TBarcodeFormat.All:
      Result := 'All';
    TBarcodeFormat.UPC_EAN_EXTENSION:
      Result := 'UPC_EAN_EXTENSION';
  else
    Result := 'FORMAT_' + IntToStr(Ord(format));
  end;
end;

function FormatByName(const name: string; out format: TBarcodeFormat): Boolean;
begin
  for var info in Formats do
    if SameText(info.Name, name) then
    begin
      format := info.Format;
      exit(True);
    end;
  Result := False;
end;

end.
