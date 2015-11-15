unit FormatInformation;

interface

uses Generics.Collections, ErrorCorrectionLevel, classes;

const
  FORMAT_INFO_MASK_QR: Integer = $5412;

type

  T2DIntegerArray = TArray<TArray<Integer>>;

  TFormatInformation = class
  private
    class var FORMAT_INFO_DECODE_LOOKUP: T2DIntegerArray;
    class var BITS_SET_IN_HALF_BYTE: TArray<Integer>;

  var
    DataMask: Byte;

    constructor Create(formatInfo: Integer);
    class function decodeFormatInformation(maskedFormatInfo1: Integer;
      maskedFormatInfo2: Integer): TFormatInformation; static;

    class function doDecodeFormatInformation(maskedFormatInfo1: Integer;
      maskedFormatInfo2: Integer): TFormatInformation; static;

  public
  var
    ErrorCorrectionLevel: TErrorCorrectionLevel;

    function Equals(o: TObject): boolean; override;
    function GetHashCode: Integer; override;

    class function numBitsDiffering(a: Integer; b: Integer): Integer; static;

  end;

implementation

{ TFormatInformation }

constructor TFormatInformation.Create(formatInfo: Integer);
begin
  SetLength(FORMAT_INFO_DECODE_LOOKUP, $20);
  FORMAT_INFO_DECODE_LOOKUP[0] := TArray<Integer>.Create($5412, 0);
  FORMAT_INFO_DECODE_LOOKUP[1] := TArray<Integer>.Create($5125, 1);
  FORMAT_INFO_DECODE_LOOKUP[2] := TArray<Integer>.Create($5E7C, 2);
  FORMAT_INFO_DECODE_LOOKUP[3] := TArray<Integer>.Create($5B4B, 3);
  FORMAT_INFO_DECODE_LOOKUP[4] := TArray<Integer>.Create($45F9, 4);
  FORMAT_INFO_DECODE_LOOKUP[5] := TArray<Integer>.Create($40CE, 5);
  FORMAT_INFO_DECODE_LOOKUP[6] := TArray<Integer>.Create($4F97, 6);
  FORMAT_INFO_DECODE_LOOKUP[7] := TArray<Integer>.Create($4AA0, 7);
  FORMAT_INFO_DECODE_LOOKUP[8] := TArray<Integer>.Create($77C4, 8);
  FORMAT_INFO_DECODE_LOOKUP[9] := TArray<Integer>.Create($72F3, 9);
  FORMAT_INFO_DECODE_LOOKUP[10] := TArray<Integer>.Create($7DAA, 10);
  FORMAT_INFO_DECODE_LOOKUP[11] := TArray<Integer>.Create($789D, 11);
  FORMAT_INFO_DECODE_LOOKUP[12] := TArray<Integer>.Create($662F, 12);
  FORMAT_INFO_DECODE_LOOKUP[13] := TArray<Integer>.Create($6318, 13);
  FORMAT_INFO_DECODE_LOOKUP[14] := TArray<Integer>.Create($6C41, 14);
  FORMAT_INFO_DECODE_LOOKUP[15] := TArray<Integer>.Create($6976, 15);
  FORMAT_INFO_DECODE_LOOKUP[$10] := TArray<Integer>.Create($1689, $10);
  FORMAT_INFO_DECODE_LOOKUP[$11] := TArray<Integer>.Create($13BE, $11);
  FORMAT_INFO_DECODE_LOOKUP[$12] := TArray<Integer>.Create($1CE7, $12);
  FORMAT_INFO_DECODE_LOOKUP[$13] := TArray<Integer>.Create($19D0, $13);
  FORMAT_INFO_DECODE_LOOKUP[20] := TArray<Integer>.Create($762, 20);
  FORMAT_INFO_DECODE_LOOKUP[$15] := TArray<Integer>.Create($255, $15);
  FORMAT_INFO_DECODE_LOOKUP[$16] := TArray<Integer>.Create($D0C, $16);
  FORMAT_INFO_DECODE_LOOKUP[$17] := TArray<Integer>.Create($83B, $17);
  FORMAT_INFO_DECODE_LOOKUP[$18] := TArray<Integer>.Create($355F, $18);
  FORMAT_INFO_DECODE_LOOKUP[$19] := TArray<Integer>.Create($3068, $19);
  FORMAT_INFO_DECODE_LOOKUP[$1A] := TArray<Integer>.Create($3F31, $1A);
  FORMAT_INFO_DECODE_LOOKUP[$1B] := TArray<Integer>.Create($3A06, $1B);
  FORMAT_INFO_DECODE_LOOKUP[$1C] := TArray<Integer>.Create($24B4, $1C);
  FORMAT_INFO_DECODE_LOOKUP[$1D] := TArray<Integer>.Create($2183, $1D);
  FORMAT_INFO_DECODE_LOOKUP[30] := TArray<Integer>.Create($2EDA, 30);
  FORMAT_INFO_DECODE_LOOKUP[$1F] := TArray<Integer>.Create($2BED, $1F);

  BITS_SET_IN_HALF_BYTE := TArray<Integer>.Create(0, 1, 1, 2, 1, 2, 2, 3, 1, 2,
    2, 3, 2, 3, 3, 4);
end;

class function TFormatInformation.decodeFormatInformation(maskedFormatInfo1,
  maskedFormatInfo2: Integer): TFormatInformation;
var
  formatInfo: TFormatInformation;
begin
  formatInfo := TFormatInformation.doDecodeFormatInformation(maskedFormatInfo1,
    maskedFormatInfo2);
  if (formatInfo <> nil) then
  begin
    Result := formatInfo;
    exit
  end;

  Result := TFormatInformation.doDecodeFormatInformation
    ((maskedFormatInfo1 xor $5412), (maskedFormatInfo2 xor $5412));

end;

class function TFormatInformation.doDecodeFormatInformation(maskedFormatInfo1,
  maskedFormatInfo2: Integer): TFormatInformation;
var
  bestDifference, bestFormatInfo, bitsDifference, targetInfo: Integer;
  decodeInfo: TArray<Integer>;

begin
  bestDifference := $7FFFFFFF;
  bestFormatInfo := 0;

  for decodeInfo in FORMAT_INFO_DECODE_LOOKUP do
  begin

    targetInfo := decodeInfo[0];

    if ((targetInfo = maskedFormatInfo1) or (targetInfo = maskedFormatInfo2))
    then
    begin
      Result := TFormatInformation.Create(decodeInfo[1]);
      exit
    end;

    bitsDifference := TFormatInformation.numBitsDiffering(maskedFormatInfo1,
      targetInfo);

    if (bitsDifference < bestDifference) then
    begin
      bestFormatInfo := decodeInfo[1];
      bestDifference := bitsDifference
    end;

    if (maskedFormatInfo1 <> maskedFormatInfo2) then
    begin
      bitsDifference := TFormatInformation.numBitsDiffering(maskedFormatInfo2,
        targetInfo);
      if (bitsDifference < bestDifference) then
      begin
        bestFormatInfo := decodeInfo[1];
        bestDifference := bitsDifference
      end
    end

  end;

  if (bestDifference <= 3) then
  begin
    Result := TFormatInformation.Create(bestFormatInfo);
    exit
  end;

  Result := nil;
end;

function TFormatInformation.Equals(o: TObject): boolean;
var
  other: TFormatInformation;
begin
  if (not(o is TFormatInformation)) then
  begin
    Result := false;
    exit
  end;

  other := (o as TFormatInformation);

  Result := ((self.ErrorCorrectionLevel = other.ErrorCorrectionLevel) and
    (self.DataMask = other.DataMask));
end;

function TFormatInformation.GetHashCode: Integer;
begin
  Result := ((self.ErrorCorrectionLevel.ordinal shl 3) or self.DataMask)
end;

class function TFormatInformation.numBitsDiffering(a, b: Integer): Integer;
begin
  a := (a xor b);
  Result := (((((((BITS_SET_IN_HALF_BYTE[(a and 15)] + BITS_SET_IN_HALF_BYTE
    [((a shr 4) and 15)]) + BITS_SET_IN_HALF_BYTE[((a shr 8) and 15)]) +
    BITS_SET_IN_HALF_BYTE[((a shr 12) and 15)]) + BITS_SET_IN_HALF_BYTE
    [((a shr $10) and 15)]) + BITS_SET_IN_HALF_BYTE[((a shr 20) and 15)]) +
    BITS_SET_IN_HALF_BYTE[((a shr $18) and 15)]) + BITS_SET_IN_HALF_BYTE
    [((a shr $1C) and 15)])
end;

end.
