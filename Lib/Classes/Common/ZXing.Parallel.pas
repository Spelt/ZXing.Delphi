unit ZXing.Parallel;

{
  * Decoding with tasks, where the readers support it: the readers of
  * TMultiFormatReader run side by side (the result is the one of the
  * sequential order, the readers behind it are stopped), and the four
  * directions of the edge tracing DataMatrix detector with TRY_HARDER. The
  * results are the same as without: the work is spread over the processors,
  * which shortens the time per image but not the total work, so it costs
  * battery on a mobile device.
  *
  * The tasks run in a thread pool of the library with a worker thread per
  * processor from the start. The default pool of the RTL starts with two
  * workers (a quarter of the processors) and grows only when its monitor
  * sees the CPU idle, which it does not while decoding: with it the tasks
  * ran one after the other.
  *
  * Delphi Implementation by E. Spelt
}

interface

uses
  System.Threading,
  System.Generics.Collections,
  ZXing.DecodeHintType;

type
  /// <summary>
  /// The value of the hint CANCEL_TOKEN, set by TMultiFormatReader in the
  /// hints of a reader that runs in a task. When the result of the reader
  /// is not needed any more, Cancelled is set: the readers that support it
  /// stop at their next row, scan line or candidate and return nothing.
  /// </summary>
  TCancelToken = class
  public
    Cancelled: Boolean;
  end;

var
  /// <summary>
  /// Decode with tasks where the readers support it. On by default with
  /// more than one processor on the desktop platforms, and with more than
  /// two on Android and iOS (a phone with two cores keeps them for itself
  /// and its battery); an application sets it as it likes, before scanning.
  /// </summary>
  ParallelDecoding: Boolean;

threadvar
  /// <summary>True in a task that runs a reader (TMultiFormatReader): a
  /// detector does not start tasks of its own then.</summary>
  InParallelReader: Boolean;

/// <summary>True when ParallelDecoding is on and there is more than one
/// processor: the readers run their tasks then.</summary>
function ParallelDecodingActive: Boolean;

/// <summary>The thread pool of the decoding tasks: made on first use, with
/// as many worker threads as processors.</summary>
function DecodingThreadPool: TThreadPool;

/// <summary>The cancel token of the hints, nil without one.</summary>
function CancelTokenOf(hints: TDictionary<TDecodeHintType, TObject>)
  : TCancelToken;

/// <summary>The flag of the cancel token of the hints, nil without one: for
/// the loops of a detector.</summary>
function CancelFlagOf(hints: TDictionary<TDecodeHintType, TObject>): PBoolean;

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
    // The memory manager must not sleep when threads contend for it: the
    // readers in tasks allocate all the time, and with the sleeps of the
    // default (1 ms each) the parallel decoding was four times slower than
    // the sequential one. With this it spins briefly instead.
    System.NeverSleepOnMMThreadContention := true;
    pool := TThreadPool.Create;
    // (a worker per processor, not more: the readers in tasks contend for
    // the memory manager and the caches of the image)
    pool.SetMaxWorkerThreads(TThread.ProcessorCount);
    pool.SetMinWorkerThreads(TThread.ProcessorCount);
    // (two threads at once here: the first one wins, the other pool goes)
    if (TInterlocked.CompareExchange(Pointer(FPool), Pointer(pool), nil) <> nil)
    then
      pool.Free;
  end;
  Result := FPool;
end;

function CancelTokenOf(hints: TDictionary<TDecodeHintType, TObject>)
  : TCancelToken;
var
  o: TObject;
begin
  Result := nil;
  if (hints <> nil) and hints.TryGetValue(TDecodeHintType.CANCEL_TOKEN, o) and
    (o is TCancelToken) then
    Result := TCancelToken(o);
end;

function CancelFlagOf(hints: TDictionary<TDecodeHintType, TObject>): PBoolean;
var
  token: TCancelToken;
begin
  token := CancelTokenOf(hints);
  if (token = nil) then
    Result := nil
  else
    Result := @token.Cancelled;
end;

initialization

{$IF defined(ANDROID) or defined(IOS)}
  ParallelDecoding := (TThread.ProcessorCount > 2);
{$ELSE}
  ParallelDecoding := (TThread.ProcessorCount > 1);
{$ENDIF}

finalization

FPool.Free;

end.
