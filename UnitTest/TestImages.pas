unit TestImages;

{
  Where the test images are. On Windows they are in the repository, next
  to the test project (unitTest\images). On a device (Android, iOS) the
  test app brings them along as images.zip (made by images.cmd, deployed to
  the documents folder of the app) and unpacks it there the first time, or
  again when a newer zip was deployed.
}

interface

/// <summary>The folder with the test images, with a trailing path
/// delimiter.</summary>
function ImagesFolder: string;

/// <summary>The full path of an image in that folder; a sub folder with
/// '\' or '/'.</summary>
function ImagePath(const name: string): string;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Zip;

var
  Folder: string;

function ImagesFolder: string;
begin
  if Folder <> '' then
    exit(Folder);
{$IFDEF MSWINDOWS}
  Folder := ExtractFileDir(ParamStr(0)) + '\..\..\images\';
{$ELSE}
  // the device: images.zip in the documents folder, unpacked once (and
  // again after a newer zip was deployed)
  var documents := TPath.GetDocumentsPath;
  Folder := TPath.Combine(documents, 'images');
  var zip := TPath.Combine(documents, 'images.zip');
  if TFile.Exists(zip) and (not TDirectory.Exists(Folder) or
    (TFile.GetLastWriteTime(zip) > TFile.GetLastWriteTime(Folder))) then
  begin
    if TDirectory.Exists(Folder) then
      TDirectory.Delete(Folder, True);
    // (the zip holds the folder images\ itself)
    TZipFile.ExtractZipFile(zip, documents);
    TFile.SetLastWriteTime(Folder, Now);
  end;
  Folder := IncludeTrailingPathDelimiter(Folder);
{$ENDIF}
  Result := Folder;
end;

function ImagePath(const name: string): string;
begin
  Result := ImagesFolder + name.Replace('\', TPath.DirectorySeparatorChar)
    .Replace('/', TPath.DirectorySeparatorChar);
end;

end.
