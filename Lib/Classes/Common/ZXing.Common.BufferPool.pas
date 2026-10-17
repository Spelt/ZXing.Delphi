unit ZXing.Common.BufferPool;

{
  * A pool of the image-sized buffers a scan allocates: the luminances of a
  * frame, its inverted copy, the words of the black matrix and the
  * downscaled layers. Without it, every camera frame pays SetLength for
  * each of them: the allocation from the OS (large blocks), the zero fill
  * and the page faults of the first touch, about a millisecond per 1080p
  * frame in all, before any decoding. The pool keeps the buffers of the
  * last frames and hands them out again when the size matches exactly.
  *
  * A buffer of the pool is NOT cleared: Acquire with cleared = false is for
  * code that writes every element (the luminance converters), with
  * cleared = true the buffer is zero like after SetLength (a FillChar, which
  * is still much cheaper than fresh pages).
  *
  * Release pools a buffer only when nothing else refers to it (the reference
  * count of the dynamic array is 1), so an array that is still shared, e.g.
  * the luminances an inverted binarizer kept from the original one, is
  * simply dropped like before. Small buffers go to the pool neither: they
  * are cheap to allocate and would push the large ones out.
  *
  * Thread safe (readers in tasks, ScanAsync): a critical section around the
  * lists. Capped: at most MAX_KEPT buffers per element type, the oldest one
  * goes when a new one arrives, so the pool follows a change of the frame
  * size instead of holding on to the old one.
  *
  * Delphi Implementation by E. Spelt
}

interface

var
  /// <summary>
  /// Pool the image-sized buffers of a scan (see the unit). On by default;
  /// an application sets it as it likes, before scanning. Off: every buffer
  /// is allocated with SetLength as before, and Release only drops it.
  /// </summary>
  BufferPooling: Boolean = true;

/// <summary>A byte buffer of exactly length elements: from the pool when it
/// has one of that size, else new. Not cleared unless cleared is true.
/// </summary>
function AcquireBytes(length: Integer; cleared: Boolean = false): TArray<Byte>;
/// <summary>Gives a byte buffer back to the pool (when nothing else refers
/// to it); buffer is nil afterwards.</summary>
procedure ReleaseBytes(var buffer: TArray<Byte>);

/// <summary>An integer buffer of exactly length elements, like AcquireBytes.
/// </summary>
function AcquireWords(length: Integer; cleared: Boolean = false)
  : TArray<Integer>;
/// <summary>Gives an integer buffer back to the pool, like ReleaseBytes.
/// </summary>
procedure ReleaseWords(var buffer: TArray<Integer>);

/// <summary>Frees the buffers kept in the pool, e.g. when the application
/// goes to the background or the camera resolution changes.</summary>
procedure ClearBufferPool;

implementation

uses
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections;

const
  /// <summary>Buffers per element type kept at most.</summary>
  MAX_KEPT = 16;
  /// <summary>Smaller buffers are not pooled (bytes).</summary>
  MIN_POOLED_BYTES = 32 * 1024;

var
  PoolLock: TCriticalSection;
  BytePool: TList<TArray<Byte>>;
  WordPool: TList<TArray<Integer>>;

/// <summary>The reference count of a dynamic array (not nil): the field
/// before the length in the header of the array.</summary>
function DynArrayRefCount(p: Pointer): Integer; inline;
begin
  Result := PInteger(PByte(p) - SizeOf(NativeInt) - SizeOf(Integer))^;
end;

function AcquireBytes(length: Integer; cleared: Boolean): TArray<Byte>;
begin
  Result := nil;
  if BufferPooling and (length >= MIN_POOLED_BYTES) then
  begin
    PoolLock.Enter;
    try
      // the most recent one of that size (the hot sizes stay at the end)
      for var i := BytePool.Count - 1 downto 0 do
        if (System.Length(BytePool[i]) = length) then
        begin
          Result := BytePool[i];
          BytePool.Delete(i);
          break;
        end;
    finally
      PoolLock.Leave;
    end;
    if (Result <> nil) then
    begin
      if cleared then
        FillChar(Result[0], length, 0);
      exit;
    end;
  end;
  // (SetLength clears the new array)
  SetLength(Result, length);
end;

procedure ReleaseBytes(var buffer: TArray<Byte>);
begin
  if (buffer = nil) then
    exit;
  if BufferPooling and (System.Length(buffer) >= MIN_POOLED_BYTES) and
    (DynArrayRefCount(buffer) = 1) then
  begin
    PoolLock.Enter;
    try
      if (BytePool.Count >= MAX_KEPT) then
        BytePool.Delete(0);
      BytePool.Add(buffer);
    finally
      PoolLock.Leave;
    end;
  end;
  buffer := nil;
end;

function AcquireWords(length: Integer; cleared: Boolean): TArray<Integer>;
begin
  Result := nil;
  if BufferPooling and (length * SizeOf(Integer) >= MIN_POOLED_BYTES) then
  begin
    PoolLock.Enter;
    try
      for var i := WordPool.Count - 1 downto 0 do
        if (System.Length(WordPool[i]) = length) then
        begin
          Result := WordPool[i];
          WordPool.Delete(i);
          break;
        end;
    finally
      PoolLock.Leave;
    end;
    if (Result <> nil) then
    begin
      if cleared then
        FillChar(Result[0], length * SizeOf(Integer), 0);
      exit;
    end;
  end;
  SetLength(Result, length);
end;

procedure ReleaseWords(var buffer: TArray<Integer>);
begin
  if (buffer = nil) then
    exit;
  if BufferPooling and
    (System.Length(buffer) * SizeOf(Integer) >= MIN_POOLED_BYTES) and
    (DynArrayRefCount(buffer) = 1) then
  begin
    PoolLock.Enter;
    try
      if (WordPool.Count >= MAX_KEPT) then
        WordPool.Delete(0);
      WordPool.Add(buffer);
    finally
      PoolLock.Leave;
    end;
  end;
  buffer := nil;
end;

procedure ClearBufferPool;
begin
  PoolLock.Enter;
  try
    BytePool.Clear;
    WordPool.Clear;
  finally
    PoolLock.Leave;
  end;
end;

initialization

PoolLock := TCriticalSection.Create;
BytePool := TList<TArray<Byte>>.Create;
WordPool := TList<TArray<Integer>>.Create;

finalization

BytePool.Free;
WordPool.Free;
PoolLock.Free;

end.
