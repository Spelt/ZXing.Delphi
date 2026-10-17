# ZXing Scanner

The demo app of ZXing.Delphi: a barcode scanner with two screens, the scan
screen and its settings. It shows how the library is used in a real app, and
how little is needed for that: one `TScanManager`, one call per camera frame,
and the result back in the main thread.

The app is a FireMonkey application for Windows, macOS, iOS and Android,
built with the standard components only. There is no third party code and no
custom style: the dark look is made of rectangles, circles, paths and labels.

## The scan screen

The camera fills the screen. A code that is found gets an outline at its
corners, in the color of your choice, and its text comes in the sheet at the
bottom: the format (as the library names it, `QR_CODE`), when it was found,
the time the scan took, and the text. For GS1 codes the readable form with
the application identifiers is shown below the text. Tapping the text, or the
pill under the outline, copies it to the clipboard.

With the *Multi result reader* on, every code in the frame is found: the
outlines are numbered, and the sheet lists them, with a copy button per code
and *Copy all*.

A code that is there but can not be read (too far, not sharp, moving) gets a
white outline and the sheet asks to hold the camera still; once it is read,
the same outline takes the outline color.

On the desktop the button at the top left opens an image instead of the
camera; an image can also be dropped on the window or given on the command
line:

```
ZXingScanner.exe "C:\images\label.png"
```

On a phone that button is the torch.

## The settings screen

What the scanner does, in the words of the library:

| Setting | In the library |
| ------- | -------------- |
| Also inverted images | the hint `ENABLE_INVERSION`: light codes on a dark background too |
| Try harder | the hint `TRY_HARDER`: more accurate on damaged, tiny or rotated codes, a bit slower |
| Multi result reader | `ScanAllAsync` instead of `ScanAsync`: every code in the frame |
| Downscaled copies | `TScanManager.TryDownscale`: when nothing is found, smaller copies of the frame are scanned too (large, blurry and dot-peen codes) |
| Parallel decoding | `ZXing.Parallel.ParallelDecoding`: the readers decode side by side in the tasks of a thread pool |
| Show unreadable codes | `TScanManager.ReturnErrors`: a code that was found but not read is returned with its position |
| Formats | `TBarcodeFormat.Auto`, `TBarcodeFormat.All`, or the chosen ones as the hint `POSSIBLE_FORMATS` |

The formats come in two groups: the ones read by default (`Auto`), and the
ones only read when asked for, because they are slow, have no check against
false positives, or are a kind of another format (Code 32 is a Code 39). With
all of the first group and none of the second the scanner runs with `Auto`,
with all of both with `All`, else with the list of the chosen formats.

*On a hit*: the color of the outline, a short vibration and a beep.

The settings are kept in `ZXingScanner.json` in the documents folder of the
app, so the app starts as it was left.

## The code

| Unit | What it is |
| ---- | ---------- |
| `Scanner.Main` | the scan screen: the camera, the scans, the outlines and the sheet |
| `Scanner.Settings` | the settings screen; it edits a `TScannerConfig` and reports back with `OnDone` |
| `Scanner.Config` | the settings: a class that makes the scan manager for them (`CreateScanManager`) and keeps itself in JSON |
| `Scanner.Formats` | the table of formats: their names and whether they are in `Auto` |
| `Scanner.Theme` | the colors, the fonts, the icons (SVG path data) and a few helpers for the controls |
| `Scanner.Feedback` | the beep and the vibration, per platform |

### The scan of a frame

The camera (`TCameraComponent`) calls `OnSampleBufferReady` from its own
thread; the frame is taken as a bitmap in the main thread and scanned:

```pascal
procedure TMainForm.ScanFrame;
begin
  // a scan still running: this frame is skipped
  if fScanManager.Scanning then
    exit;
  fScanManager.ScanAsync(fFrame,
    procedure(r: TReadResult)
    begin
      try
        if r = nil then
          ShowResults([])
        else
          ShowResults([r]);
      finally
        r.Free;
      end;
    end, ShowError);
end;
```

`ScanAsync` takes the pixels of the bitmap in the calling thread, so the
bitmap is free again when it returns, decodes in a task of the thread pool,
and calls back in the main thread with the first barcode (`nil` when there is
none). While a scan runs, `Scanning` is true and the frames that arrive are
skipped, so the app stays responsive whatever the device. `ScanAllAsync` does
the same for every code in the frame, with a list.

The scan manager is held as `IScanManager`: it is freed with the last
reference, after a scan that may still run in it. One scan manager is kept
for all frames; a new one is made when the settings change
(`TScannerConfig.CreateScanManager`), with the format and the hints:

```pascal
hints := TDictionary<TDecodeHintType, TObject>.Create;
hints.Add(TDecodeHintType.TRY_HARDER, nil);
scanner := TScanManager.Create(TBarcodeFormat.Auto, hints); // the hints are its now
scanner.TryDownscale := True;
scanner.ReturnErrors := True;
```

### The outline

`TReadResult.Position` gives the 4 corners of a code in the frame (for a 1D
code: the scanned line widened to the height of the bars). The scan screen
does not draw them as they come: the hits of the camera jump a little from
frame to frame, and a frame may miss a code. `Scanner.Tracking` keeps a
`TTrackedCode` per code on the screen, fed with the hits of every frame: a
known code gets the corners of the hit as its target (unless they moved less
than a few pixels: then the outline stays), a new code comes in
where it was seen and fades in, and a code not seen for 2 seconds fades out.
Every frame of the display (a `TAnimation` of FMX, which runs on the display
link of the platform) the drawn corners ease towards the target: a part of
the distance that is left per frame, small when the target hardly moves (so
the noise of the detection is averaged out) and large when it moves fast (so
the outline follows a moving camera at once), and it settles softly. The outline is drawn 20 pixels
outside the corners, in `OnPaint` of the paint box that also draws the
frame; the pill with the text follows its code, as visible as the outline.

### The look without a style

The design is dark, with rounded cards and pills; all of it is done with the
standard controls and their properties: `TRectangle` with `XRadius` and
`Corners` for the cards and the sheet, `TCircle` for the buttons and badges,
`TPath` for the icons, filled with the SVG path data of the [Material Design
icons](https://github.com/google/material-design-icons) of Google (Apache
License 2.0; the paths are in `Scanner.Theme`), `TLabel` with
`StyledSettings = []` for the colors and fonts, `TFlowLayout` for the chips
of the formats, `TFloatAnimation` for the pulsing dot. The only control that
takes its look from the style of the platform is the `TSwitch`; a custom
style would be needed to color it like the design, nothing else.

The monospace font of the texts is the one of the platform (Consolas, Menlo,
monospace).

## The icon

The icon of the app (the QR code between the brackets of the outline) is
`resourceszxing-delphi-icon.svg`; the sets for Android, iOS, macOS and Windows
that the project uses are in `ZXingScanner.Artwork`.

## Platforms

The app renders with Skia (`GlobalUseSkia`), on Metal on iOS and macOS and on
Vulkan on Android (`GlobalUseMetal`, `GlobalUseVulkan` in the project source),
and runs in portrait.

- **Windows and macOS**: the camera of the computer, or an image (the
  button, drag and drop, the command line). No vibration.
- **Android**: the permissions `CAMERA` and `VIBRATE` are set in the project
  options; the camera permission is asked for at the start, as the platform
  wants it. The torch is the button at the top left; the back button of the
  device closes the settings.
- **iOS**: `NSCameraUsageDescription` is set in the version info of the
  project. The beep and the vibration are system sounds of AudioToolbox.

Requires Delphi 10.4 Sydney or newer. Open `ZXingScanner.dproj` in the IDE,
or build the Windows version from the command line with `build.cmd`
(Delphi 13 in its default location, or `%BDS%`).
