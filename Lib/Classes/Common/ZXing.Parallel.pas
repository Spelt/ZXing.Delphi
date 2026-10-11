unit ZXing.Parallel;

{
  * The switch for decoding with tasks, where the readers support it: the
  * four directions of the edge tracing DataMatrix detector with TRY_HARDER.
  * The results are the same as without: the work is spread over the
  * processors, which shortens the time per image but not the total work, so
  * it costs battery on a mobile device.
  *
  * The tasks run in a thread pool of the library with a worker thread per
  * processor from the start. The default pool of the RTL starts with two
  * workers (a quarter of the processors) and grows only when its monitor
  * sees the CPU idle, which it does not while decoding: with it the
  * directions ran one after the other.
  *
  * Delphi Implementation by E. Spelt
}

interface

uses
  System.Threading;

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

/// <summary>The thread pool of the decoding tasks: made on first use, with
/// as many worker threads as processors.</summary>
function DecodingThreadPool: TThreadPool;

implementation

uses
  System.Classes,
  System.SyncObjs;

var
  FPool: TThreadPool = nil;

function ParallelDecodingActive: Boolean;
begin
  Result := ParallelDecoding and (TThread.ProcessorCount > 1);
end;

function DecodingThreadPool: TThreadPool;
var
  pool: TThreadPool;
begin
  if (FPool = nil) then
  begin
    pool := TThreadPool.Create;
    pool.SetMaxWorkerThreads(2 * TThread.ProcessorCount);
    pool.SetMinWorkerThreads(TThread.ProcessorCount);
    // (two threads at once here: the first one wins, the other pool goes)
    if (TInterlocked.CompareExchange(Pointer(FPool), Pointer(pool), nil) <> nil)
    then
      pool.Free;
  end;
  Result := FPool;
end;

initialization

{$IF defined(ANDROID) or defined(IOS)}
  ParallelDecoding := false;
{$ELSE}
  ParallelDecoding := (TThread.ProcessorCount > 1);
{$ENDIF}

finalization

FPool.Free;

end.
