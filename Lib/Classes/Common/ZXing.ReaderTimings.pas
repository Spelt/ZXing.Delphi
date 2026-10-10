unit ZXing.ReaderTimings;

{
  * Optional measurement of the time spent per reader, for the benchmark.
  * When ReaderTimingsEnabled is true, TMultiFormatReader adds the time of
  * every reader call (and of the binarization of the whole image) under
  * ReaderTimingsMode (a label the caller sets, e.g. the test mode) and the
  * class name of the reader. Off by default, then it costs nothing more
  * than a Boolean test per reader call. Not thread safe.
}

interface

type
  TReaderTiming = record
    /// <summary>The value of ReaderTimingsMode when the time was added.
    /// </summary>
    Mode: string;
    /// <summary>The class name of the reader.</summary>
    Reader: string;
    Calls: Integer;
    /// <summary>The calls that gave a result.</summary>
    Found: Integer;
    Ticks: Int64;
    function Ms: Double;
  end;

const
  /// <summary>The "reader" name of the binarization of the whole image
  /// (the black matrix the 2D readers share).</summary>
  READER_TIMING_BINARIZER = '(binarizer: black matrix)';
  /// <summary>The black rows and their pattern rows (bars and spaces) the
  /// 1D readers share: made on demand by the first reader that asks for a
  /// row, booked apart from that reader (nested timings).</summary>
  READER_TIMING_BLACK_ROWS = '(binarizer: black rows)';
  READER_TIMING_PATTERN_ROWS = '(binarizer: pattern rows)';
  /// <summary>The image rotated by 90 degrees (the luminances) and the
  /// black matrix rotated or transposed, made once and shared.</summary>
  READER_TIMING_TURNED = '(binarizer: turned image and matrices)';

var
  ReaderTimingsEnabled: Boolean = false;
  ReaderTimingsMode: string = '';
  /// <summary>The ticks of all nested timings so far: work done inside a
  /// reader call that is booked on its own (the rows). The caller of a
  /// reader subtracts the growth of this during the call from the time of
  /// the reader.</summary>
  ReaderTimingsNestedTicks: Int64 = 0;

procedure AddReaderTiming(const reader: string; ticks: Int64; found: Boolean);
/// <summary>AddReaderTiming for work inside a reader call that is booked
/// apart from the reader (see ReaderTimingsNestedTicks).</summary>
procedure AddNestedReaderTiming(const name: string; ticks: Int64);
/// <summary>The timings added so far, by mode and then by time
/// (descending).</summary>
function GetReaderTimings: TArray<TReaderTiming>;
procedure ClearReaderTimings;

implementation

uses
  System.SysUtils,
  System.Diagnostics,
  System.Generics.Defaults,
  System.Generics.Collections;

var
  timings: TDictionary<string, TReaderTiming>;

function TReaderTiming.Ms: Double;
begin
  Result := Ticks * 1000.0 / TStopwatch.Frequency;
end;

procedure AddNestedReaderTiming(const name: string; ticks: Int64);
begin
  AddReaderTiming(name, ticks, true);
  Inc(ReaderTimingsNestedTicks, ticks);
end;

procedure AddReaderTiming(const reader: string; ticks: Int64; found: Boolean);
var
  t: TReaderTiming;
begin
  if (timings = nil) then
    timings := TDictionary<string, TReaderTiming>.Create;
  var key := ReaderTimingsMode + #1 + reader;
  if not timings.TryGetValue(key, t) then
  begin
    t := Default(TReaderTiming);
    t.Mode := ReaderTimingsMode;
    t.Reader := reader;
  end;
  Inc(t.Calls);
  if found then
    Inc(t.Found);
  Inc(t.Ticks, ticks);
  timings.AddOrSetValue(key, t);
end;

function GetReaderTimings: TArray<TReaderTiming>;
begin
  Result := [];
  if (timings = nil) then
    exit;
  Result := timings.Values.ToArray;
  TArray.Sort<TReaderTiming>(Result, TComparer<TReaderTiming>.Construct(
    function(const a, b: TReaderTiming): Integer
    begin
      Result := CompareText(a.Mode, b.Mode);
      if (Result = 0) then
        if (a.Ticks > b.Ticks) then
          Result := -1
        else if (a.Ticks < b.Ticks) then
          Result := 1
        else
          Result := CompareText(a.Reader, b.Reader);
    end));
end;

procedure ClearReaderTimings;
begin
  if (timings <> nil) then
    timings.Clear;
end;

initialization

finalization
  timings.Free;

end.
