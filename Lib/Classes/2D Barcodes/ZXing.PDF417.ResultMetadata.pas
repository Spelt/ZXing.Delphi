{
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

  * The Macro PDF417 data of a PDF417 or MicroPDF417 symbol (Structured
  * Append, ISO/IEC 15438:2015 annex H), like zxing-cpp's PDF417CustomData.
}

unit ZXing.PDF417.ResultMetadata;

interface

uses
  ZXing.ReadResult;

type
  /// <summary>
  /// The metadata TResultMetadataType.PDF417_EXTRA_METADATA of a PDF417 or
  /// MicroPDF417 result.
  /// </summary>
  IPDF417ResultMetadata = interface(IMetaData)
    ['{5B0F6A41-7E0B-4C55-9F2D-0E6C3B8A9D11}']
    /// <summary>The index (0 based) of the symbol in a Macro PDF417
    /// sequence; -1 when it is not part of one.</summary>
    function SegmentIndex: Integer;
    /// <summary>The number of symbols of the sequence; -1 when unknown.
    /// </summary>
    function SegmentCount: Integer;
    /// <summary>The id of the sequence (the codewords as 3 digits each).
    /// </summary>
    function FileId: string;
    function IsLastSegment: Boolean;
    function FileName: string;
    function Sender: string;
    function Addressee: string;
    /// <summary>-1 when not given.</summary>
    function FileSize: Int64;
    /// <summary>-1 when not given.</summary>
    function Timestamp: Int64;
    /// <summary>-1 when not given.</summary>
    function Checksum: Integer;
    /// <summary>The codewords of the optional fields.</summary>
    function OptionalData: TArray<Integer>;
    /// <summary>Whether the symbol starts with Reader Initialisation.
    /// </summary>
    function ReaderInit: Boolean;
  end;

  TPDF417ResultMetadata = class(TInterfacedObject, IMetaData,
    IPDF417ResultMetadata)
  public
    // set by the decoder
    FSegmentIndex: Integer;
    FSegmentCount: Integer;
    FFileId: string;
    FIsLastSegment: Boolean;
    FFileName: string;
    FSender: string;
    FAddressee: string;
    FFileSize: Int64;
    FTimestamp: Int64;
    FChecksum: Integer;
    FOptionalData: TArray<Integer>;
    FReaderInit: Boolean;
    constructor Create;
    function SegmentIndex: Integer;
    function SegmentCount: Integer;
    function FileId: string;
    function IsLastSegment: Boolean;
    function FileName: string;
    function Sender: string;
    function Addressee: string;
    function FileSize: Int64;
    function Timestamp: Int64;
    function Checksum: Integer;
    function OptionalData: TArray<Integer>;
    function ReaderInit: Boolean;
  end;

implementation

constructor TPDF417ResultMetadata.Create;
begin
  inherited Create;
  FSegmentIndex := -1;
  FSegmentCount := -1;
  FFileSize := -1;
  FTimestamp := -1;
  FChecksum := -1;
end;

function TPDF417ResultMetadata.SegmentIndex: Integer;
begin
  Result := FSegmentIndex;
end;

function TPDF417ResultMetadata.SegmentCount: Integer;
begin
  Result := FSegmentCount;
end;

function TPDF417ResultMetadata.FileId: string;
begin
  Result := FFileId;
end;

function TPDF417ResultMetadata.IsLastSegment: Boolean;
begin
  Result := FIsLastSegment;
end;

function TPDF417ResultMetadata.FileName: string;
begin
  Result := FFileName;
end;

function TPDF417ResultMetadata.Sender: string;
begin
  Result := FSender;
end;

function TPDF417ResultMetadata.Addressee: string;
begin
  Result := FAddressee;
end;

function TPDF417ResultMetadata.FileSize: Int64;
begin
  Result := FFileSize;
end;

function TPDF417ResultMetadata.Timestamp: Int64;
begin
  Result := FTimestamp;
end;

function TPDF417ResultMetadata.Checksum: Integer;
begin
  Result := FChecksum;
end;

function TPDF417ResultMetadata.OptionalData: TArray<Integer>;
begin
  Result := FOptionalData;
end;

function TPDF417ResultMetadata.ReaderInit: Boolean;
begin
  Result := FReaderInit;
end;

end.
