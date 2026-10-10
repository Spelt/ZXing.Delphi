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
  /// (the black matrix the 2D readers share). The black rows of the 1D
  /// readers are binarized on demand and counted with the first 1D reader
  /// that asks for them.</summary>
  READER_TIMING_BINARIZER = '(binarizer: black matrix)';

var
  ReaderTimingsEnabled: Boolean = false;
  ReaderTimingsMode: string = '';

procedure AddReaderTiming(const reader: string; ticks: Int64; found: Boolean);
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
