program ZXingBenchmark;

{
  * Measures ZXing.Delphi against the black box test samples of zxing-cpp and
  * shows next to it how many of them zxing-cpp itself reads.
  *
  * Usage: ZXingBenchmark [folder prefixes] [-v] [-modes=slow,fast,pure]
  *                       [-samples=<folder>] [-thresholds] [-all]
  *
  *   folder prefixes   only these folders, e.g. "datamatrix qrcode-1"
  *   -v                list every symbol that zxing-cpp reads and Delphi not,
  *                     every wrong read and every exception
  *   -modes=...        modes to run (default all three)
  *   -samples=...      samples folder (default unitTest\Images\zxing-cpp of
  *                     this repository, a copy of zxing-cpp's test/samples)
  *   -thresholds       also write the results as limits for
  *                     unitTest\ZXingCppSamplesTest.pas
  *   -all              read all symbols of an image (TScanManager.ScanAll)
  *                     instead of one (Scan)
  *   -readers          after every folder (and in total) the time spent per
  *                     reader: ms, calls and calls with a result per mode.
  *                     The binarization of the whole image is listed
  *                     separately; the black rows of the 1D readers count
  *                     with the first 1D reader (TStackedReader in Auto)
  *
  * See Benchmark.Runner for the modes and how results are compared.
}

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  ZXing.ReaderTimings,
  Benchmark.Samples in 'Benchmark.Samples.pas',
  Benchmark.Images in 'Benchmark.Images.pas',
  Benchmark.Runner in 'Benchmark.Runner.pas';

var
  Verbose: Boolean;
  Modes: TTestModes;
  Thresholds: TStringList;
  // -readers: the timings of all folders so far, by mode and reader
  TotalTimings: TDictionary<string, TReaderTiming>;

function StatsColumns(const s: TModeStats): string;
begin
  Result := Format('%5d %5d %5d %4d %7.0f', [s.ReadByDelphi, s.ReadByCpp,
    s.Checks, s.Wrong, s.TimeMs]);
end;

procedure WriteHeader;
var
  mode: TTestMode;
  line1, line2: string;
begin
  line1 := Format('%-18s %4s', ['', '']);
  line2 := Format('%-18s %4s', ['folder', 'img']);
  for mode := Low(TTestMode) to High(TTestMode) do
    if (mode in Modes) then
    begin
      line1 := line1 + ' | ' + Format('%-30s', [TestModeName(mode)]);
      line2 := line2 + ' | ' + 'Delphi   cpp    of wrong      ms';
    end;
  Writeln(line1);
  Writeln(line2);
end;

procedure WriteStats(const name: string; images: Integer; const stats: TStats);
var
  mode: TTestMode;
  line: string;
begin
  line := Format('%-18s %4d', [name, images]);
  for mode := Low(TTestMode) to High(TTestMode) do
    if (mode in Modes) then
      line := line + ' | ' + StatsColumns(stats[mode]);
  Writeln(line);
end;

procedure AddThreshold(const folderName: string; const stats: TStats);
var
  mode: TTestMode;
  line: string;
begin
  line := Format('    (Folder: ''%s''; Limits: (', [folderName]);
  for mode := Low(TTestMode) to High(TTestMode) do
  begin
    if (mode > Low(TTestMode)) then
      line := line + ', ';
    line := line + Format('(MinRead: %d; MaxWrong: %d; MaxErrors: %d)',
      [stats[mode].ReadByDelphi, stats[mode].Wrong, stats[mode].Errors]);
  end;
  Thresholds.Add(line + ')),');
end;

/// <summary>Writes the time per reader, a row per reader (most expensive
/// first) and the modes as column groups, with a sum row.</summary>
procedure WriteReaderTimings(const title: string;
  const timings: TArray<TReaderTiming>);
type
  TRow = record
    Reader: string;
    PerMode: array [TTestMode] of TReaderTiming;
    Ticks: Int64;
  end;
var
  rows: TList<TRow>;
  row, sum: TRow;
  mode: TTestMode;
  line: string;
  index: Integer;
begin
  if (Length(timings) = 0) then
    exit;
  rows := TList<TRow>.Create;
  try
    for var t in timings do
    begin
      index := -1;
      for var i := 0 to rows.Count - 1 do
        if (rows[i].Reader = t.Reader) then
          index := i;
      if (index < 0) then
      begin
        row := Default(TRow);
        row.Reader := t.Reader;
        index := rows.Add(row);
      end;
      row := rows[index];
      for mode := Low(TTestMode) to High(TTestMode) do
        if (t.Mode = TestModeName(mode)) then
        begin
          Inc(row.PerMode[mode].Calls, t.Calls);
          Inc(row.PerMode[mode].Found, t.Found);
          Inc(row.PerMode[mode].Ticks, t.Ticks);
        end;
      Inc(row.Ticks, t.Ticks);
      rows[index] := row;
    end;
    rows.Sort(TComparer<TRow>.Construct(
      function(const a, b: TRow): Integer
      begin
        if (a.Ticks > b.Ticks) then
          Result := -1
        else if (a.Ticks < b.Ticks) then
          Result := 1
        else
          Result := CompareText(a.Reader, b.Reader);
      end));

    Writeln;
    Writeln('  time per reader, ' + title + ':');
    line := Format('  %-28s', ['reader']);
    for mode := Low(TTestMode) to High(TTestMode) do
      if (mode in Modes) then
        line := line + ' | ' + Format('%-5s   ms  calls found', [TestModeName(mode)]);
    Writeln(line);
    sum := Default(TRow);
    for row in rows do
    begin
      line := Format('  %-28s', [row.Reader]);
      for mode := Low(TTestMode) to High(TTestMode) do
        if (mode in Modes) then
        begin
          line := line + ' | ' + Format('%10.0f %6d %5d',
            [row.PerMode[mode].Ms, row.PerMode[mode].Calls,
            row.PerMode[mode].Found]);
          Inc(sum.PerMode[mode].Calls, row.PerMode[mode].Calls);
          Inc(sum.PerMode[mode].Found, row.PerMode[mode].Found);
          Inc(sum.PerMode[mode].Ticks, row.PerMode[mode].Ticks);
        end;
      Writeln(line);
    end;
    line := Format('  %-28s', ['all readers']);
    for mode := Low(TTestMode) to High(TTestMode) do
      if (mode in Modes) then
        line := line + ' | ' + Format('%10.0f %6d %5d', [sum.PerMode[mode].Ms,
          sum.PerMode[mode].Calls, sum.PerMode[mode].Found]);
    Writeln(line);
  finally
    rows.Free;
  end;
end;

/// <summary>Adds the timings of a folder to TotalTimings.</summary>
procedure AddToTotalTimings(const timings: TArray<TReaderTiming>);
var
  total: TReaderTiming;
begin
  for var t in timings do
  begin
    var key := t.Mode + #1 + t.Reader;
    if not TotalTimings.TryGetValue(key, total) then
      total := t
    else
    begin
      Inc(total.Calls, t.Calls);
      Inc(total.Found, t.Found);
      Inc(total.Ticks, t.Ticks);
    end;
    TotalTimings.AddOrSetValue(key, total);
  end;
end;

function GroupOf(const folderName: string): string;
begin
  if folderName.StartsWith('datamatrix') then
    Result := 'DataMatrix'
  else if folderName.StartsWith('qrcode') then
    Result := 'QR Code'
  else if folderName.StartsWith('aztec') then
    Result := 'Aztec'
  else if folderName.StartsWith('maxicode') then
    Result := 'MaxiCode'
  else if folderName.StartsWith('microqrcode') or
    folderName.StartsWith('rmqrcode') then
    Result := 'Micro QR'
  else if folderName.StartsWith('databar') then
    Result := 'DataBar'
  else if folderName.StartsWith('pdf417') or
    folderName.StartsWith('micropdf417') then
    Result := 'PDF417'
  else if folderName.StartsWith('multi') or folderName.StartsWith('none') then
    Result := 'mixed / none'
  else
    Result := '1D';
end;

procedure Run(const samplesDir: string; const prefixes: TArray<string>);
var
  dirs: TArray<string>;
  dir, prefix, group: string;
  folder: TSampleFolder;
  mode: TTestMode;
  folderStats, total, groupTotal: TStats;
  groupStats: TDictionary<string, TStats>;
  groupImages: TDictionary<string, Integer>;
  groups, skipped: TStringList;
  include: Boolean;
  totalImages: Integer;
  log: TProc<string>;
begin
  dirs := TDirectory.GetDirectories(samplesDir);
  TArray.Sort<string>(dirs);

  if Verbose then
    log := procedure(line: string)
      begin
        Writeln('  ', line);
      end
  else
    log := nil;

  groupStats := TDictionary<string, TStats>.Create;
  groupImages := TDictionary<string, Integer>.Create;
  groups := TStringList.Create;
  skipped := TStringList.Create;
  try
    FillChar(total, SizeOf(total), 0);
    totalImages := 0;
    WriteHeader;

    for dir in dirs do
    begin
      include := (Length(prefixes) = 0);
      for prefix in prefixes do
        if ExtractFileName(dir).StartsWith(prefix, true) then
          include := true;
      if not include then
        continue;

      folder := TSampleFolder.Load(dir);
      if (folder = nil) then
      begin
        skipped.Add(ExtractFileName(dir));
        continue;
      end;

      try
        folderStats := RunFolder(folder, Modes, log);

        WriteStats(folder.Name, folder.Images.Count, folderStats);
        AddThreshold(folder.Name, folderStats);
        if ReaderTimingsEnabled then
        begin
          var timings := GetReaderTimings;
          WriteReaderTimings(folder.Name, timings);
          AddToTotalTimings(timings);
          ClearReaderTimings;
          Writeln;
        end;
        AddStats(total, folderStats);
        Inc(totalImages, folder.Images.Count);

        group := GroupOf(folder.Name);
        if not groupStats.ContainsKey(group) then
        begin
          groups.Add(group);
          groupStats.Add(group, Default(TStats));
          groupImages.Add(group, 0);
        end;
        groupTotal := groupStats[group];
        AddStats(groupTotal, folderStats);
        groupStats[group] := groupTotal;
        groupImages[group] := groupImages[group] + folder.Images.Count;
      finally
        folder.Free;
      end;
    end;

    Writeln;
    for group in groups do
      WriteStats(group, groupImages[group], groupStats[group]);
    WriteStats('total', totalImages, total);
    if ReaderTimingsEnabled then
      WriteReaderTimings('all folders', TotalTimings.Values.ToArray);

    for mode := Low(TTestMode) to High(TTestMode) do
      if (mode in Modes) and (total[mode].Errors > 0) then
        Writeln(Format('%s: %d exceptions during decoding',
          [TestModeName(mode), total[mode].Errors]));
    if (skipped.Count > 0) then
      Writeln('Skipped (format not supported by ZXing.Delphi): ' +
        skipped.CommaText);
  finally
    skipped.Free;
    groups.Free;
    groupImages.Free;
    groupStats.Free;
  end;
end;

var
  samplesDir, arg, m: string;
  prefixes: TArray<string>;
  writeThresholds: Boolean;
  i: Integer;

begin
  Thresholds := TStringList.Create;
  TotalTimings := TDictionary<string, TReaderTiming>.Create;
  try
    try
      CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
      SetConsoleOutputCP(CP_UTF8);
      SetTextCodePage(Output, CP_UTF8);
      Verbose := false;
      writeThresholds := false;
      Modes := [tmSlow, tmFast, tmPure];
      samplesDir := TPath.GetFullPath(ExtractFilePath(ParamStr(0)) +
        '..\..\..\unitTest\Images\zxing-cpp');
      for i := 1 to ParamCount do
      begin
        arg := ParamStr(i);
        if SameText(arg, '-v') then
          Verbose := true
        else if SameText(arg, '-thresholds') then
          writeThresholds := true
        else if SameText(arg, '-errors') then
          UseReturnErrors := true
        else if SameText(arg, '-all') then
          UseScanAll := true
        else if SameText(arg, '-readers') then
          ReaderTimingsEnabled := true
        else if arg.StartsWith('-samples=', true) then
          samplesDir := Copy(arg, 10, MaxInt)
        else if arg.StartsWith('-modes=', true) then
        begin
          Modes := [];
          for m in Copy(arg, 8, MaxInt).Split([',']) do
            if SameText(m, 'slow') then
              Include(Modes, tmSlow)
            else if SameText(m, 'fast') then
              Include(Modes, tmFast)
            else if SameText(m, 'pure') then
              Include(Modes, tmPure);
        end
        else if arg.StartsWith('-') then
        begin
          Writeln('Unknown option ' + arg);
          ExitCode := 1;
          exit;
        end
        else
          prefixes := prefixes + [arg];
      end;

      if not TDirectory.Exists(samplesDir) then
      begin
        Writeln('Samples folder not found: ' + samplesDir);
        Writeln('Usage: ZXingBenchmark [folder prefixes] [-v] [-modes=slow,fast,pure] [-samples=<folder>] [-thresholds] [-all] [-readers]');
        ExitCode := 1;
        exit;
      end;

      Writeln('ZXing.Delphi benchmark against the zxing-cpp samples in ' + samplesDir);
      Writeln('Delphi = read correctly by ZXing.Delphi, cpp = read by zxing-cpp (from the samples),');
      Writeln('of = expected symbols at the tested positions, wrong = results matching no expected symbol, ms = decode time.');
      Writeln;
      Run(samplesDir, prefixes);

      if writeThresholds then
      begin
        Writeln;
        Writeln('Limits (MinRead = Delphi, MaxWrong = wrong, MaxErrors = exceptions) per folder, slow/fast/pure:');
        for arg in Thresholds do
          Writeln(arg);
      end;
    except
      on E: Exception do
      begin
        Writeln(E.ClassName, ': ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    TotalTimings.Free;
    Thresholds.Free;
  end;
end.
