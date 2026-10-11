unit ZXing.Parallel;

{
  * The switch for decoding with tasks of the thread pool, where the readers
  * support it: the four directions of the edge tracing DataMatrix detector
  * with TRY_HARDER. The results are the same as without: the work is spread
  * over the processors, which shortens the time per image but not the total
  * work, so it costs battery on a mobile device.
  *
  * Delphi Implementation by E. Spelt
}

interface

var
  /// <summary>
  /// Decode with tasks where the readers support it. On by default on the
  /// desktop platforms with more than one processor, off on Android and iOS
  /// (battery); an application sets it as it likes, before scanning.
  /// </summary>
  ParallelDecoding: Boolean;

/// <summary>True when ParallelDecoding is on and there is more than one
/// processor: the readers run their tasks then.</summary>
function ParallelDecodingActive: Boolean;

implementation

uses
  System.Classes;

function ParallelDecodingActive: Boolean;
begin
  Result := ParallelDecoding and (TThread.ProcessorCount > 1);
end;

initialization

{$IF defined(ANDROID) or defined(IOS)}
  ParallelDecoding := false;
{$ELSE}
  ParallelDecoding := (TThread.ProcessorCount > 1);
{$ENDIF}

end.
