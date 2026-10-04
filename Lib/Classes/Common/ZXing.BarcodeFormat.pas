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

unit ZXing.BarcodeFormat;

interface

type
  TBarcodeFormat = (
    /// <summary>No format set. All formats will be used</summary>
    Auto = 0,

    /// <summary>Aztec 2D barcode format.</summary>
    AZTEC = 1,

    /// <summary>CODABAR 1D format.</summary>
    CODABAR = 2,

    /// <summary>Code 39 1D format.</summary>
    CODE_39 = 4,

    /// <summary>Code 93 1D format.</summary>
    CODE_93 = 8,

    /// <summary>Code 128 1D format.</summary>
    CODE_128 = 16,

    /// <summary>Data Matrix 2D barcode format.</summary>
    DATA_MATRIX = 32,

    /// <summary>EAN-8 1D format.</summary>
    EAN_8 = 64,

    /// <summary>EAN-13 1D format.</summary>
    EAN_13 = 128,

    /// <summary>ITF (Interleaved Two of Five) 1D format.</summary>
    ITF = 256,

    /// <summary>MaxiCode 2D barcode format.</summary>
    MAXICODE = 512,

    /// <summary>PDF417 format.</summary>
    PDF_417 = 1024,

    /// <summary>QR Code 2D barcode format.</summary>
    QR_CODE = 2048,

    /// <summary>GS1 DataBar (formerly RSS-14): Omnidirectional, Truncated,
    /// Stacked and Stacked Omnidirectional.</summary>
    RSS_14 = 4096,

    /// <summary>GS1 DataBar Expanded (formerly RSS Expanded), also stacked.
    /// </summary>
    RSS_EXPANDED = 8192,

    /// <summary>UPC-A 1D format.</summary>
    UPC_A = 16384,

    /// <summary>UPC-E 1D format.</summary>
    UPC_E = 32768,

    /// <summary>UPC/EAN extension format. Not a stand-alone format.</summary>
    UPC_EAN_EXTENSION = 65536,

    /// <summary>MSI (Modified Plessey): only when asked for, not in Auto.
    /// </summary>
    MSI = 131072,

    /// <summary>Plessey: only when asked for, not in Auto.</summary>
    PLESSEY = 262144,

    /// <summary>Telepen 1D format (full ASCII and compressed numeric).
    /// </summary>
    TELEPEN = 524288,

    /// <summary>GS1 DataBar Limited (formerly RSS Limited).</summary>
    RSS_LIMITED = 1048576,

    /// <summary>MicroPDF417 2D format.</summary>
    MICRO_PDF417 = 2097152,

    /// <summary>Micro QR Code 2D format.</summary>
    MICRO_QR_CODE = 4194304,

    /// <summary>rMQR Code (rectangular Micro QR Code) 2D format.</summary>
    RMQR_CODE = 8388608,

    /// <summary>DX film edge code of 35 mm film (1D).</summary>
    DX_FILM_EDGE = 16777216,

    /// <summary>Code 32 (Italian pharmacy code, a Code 39): only when asked
    /// for, Auto returns it as CODE_39.</summary>
    CODE_32 = 33554432,

    /// <summary>PZN (German Pharmazentralnummer, a Code 39): only when asked
    /// for, Auto returns it as CODE_39.</summary>
    PZN = 67108864,

    /// <summary>Pharmacode (Laetus, one track): only when asked for, not in
    /// Auto.</summary>
    PHARMA_CODE = 134217728,

    // the postal barcodes (from here on numbered, not bits): only when asked
    // for, not in Auto

    /// <summary>KIX (PostNL, 4-state).</summary>
    KIX = 268435456,

    /// <summary>RM4SCC (Royal Mail 4-State Customer Code).</summary>
    RM4SCC = 268435457,

    /// <summary>USPS Intelligent Mail Barcode (4-state).</summary>
    IMB = 268435458,

    /// <summary>POSTNET (USPS, tall and short bars).</summary>
    POSTNET = 268435459,

    /// <summary>PLANET (USPS, tall and short bars).</summary>
    PLANET = 268435460,

    /// <summary>Japan Post (Kasutama barcode, 4-state).</summary>
    JAPAN_POST = 268435461,

    /// <summary>Australia Post 4-State Customer Barcode.</summary>
    AUSTRALIA_POST = 268435462

    );

implementation

end.