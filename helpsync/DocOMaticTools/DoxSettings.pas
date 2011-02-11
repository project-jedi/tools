unit DoxSettings;

interface

uses
  SysUtils,
  Classes,
  IniFiles;

type
  TDoxSettings = class(TMemIniFile)
  private
    FMasterFile: TDoxSettings;
    FConfiguration: string;
  public
    constructor Create(const FileName, AConfiguration: string);
    destructor Destroy; override;
    function GetDoxDBFileName: string;
    procedure GetDtxFiles(Files: TStrings);
    function GetListBulletCharacters: string;
    function GetParameterDelimiterCharacters: string;
    function GetSectionDelimiterCharacters: string;
    function GetTopicSeparator: string;
    function GetTrailerCharacters: string;
    function GetWallCharacters: string;
    function GetXmlOutputFileName: string;
    function ReadString(const Section, Ident, Default: string): string; override;
    property Configuration: string read FConfiguration;
  end;

implementation

uses
  JclFileUtils;

const
  XmlGUID: string = '{6E42A4FE-6D16-4DEA-802D-17D788DA6C4D}';

//=== { TDoxSettings } =======================================================

constructor TDoxSettings.Create(const FileName, AConfiguration: string);
var
  MasterFileName: string;
begin
  inherited Create(FileName);
  if not FileExists(FileName) then
    raise Exception.CreateFmt('File not found "%s"', [FileName]);
  FConfiguration := AConfiguration;
  MasterFileName := ReadString('*Control*', 'MasterFile', '');
  if MasterFileName <> '' then
    FMasterFile := TDoxSettings.Create(PathAddSeparator(ExtractFilePath(FileName)) + MasterFileName, Configuration);
  
  if ReadString(Format('Configurations\%s\OutputFormat', [Configuration]), 'ID', '') <> XmlGUID then
    raise Exception.Create('XML configuration');
end;

destructor TDoxSettings.Destroy;
begin
  FMasterFile.Free;
  inherited Destroy;
end;

function TDoxSettings.GetDoxDBFileName: string;
begin
  Result := ChangeFileExt(FileName, '.doxdb');
end;

procedure TDoxSettings.GetDtxFiles(Files: TStrings);
var
  Index: Integer;
  DtxFileName: string;
begin
  Files.Clear;
  Files.BeginUpdate;
  try
    ReadSectionValues('Source Files', Files);
    Files.Values['Count'] := '';
    for Index := Files.Count - 1 downto 0 do
    begin
      DtxFileName := Files.ValueFromIndex[Index];
      if SameText(ExtractFileExt(DtxFileName), '.dtx') then
      begin
        if not PathIsAbsolute(DtxFileName) then
          DtxFileName := PathGetRelativePath(ExtractFilePath(FileName), DtxFileName);
        Files.Strings[Index] := DtxFileName;
      end
      else
        Files.Delete(Index);
    end;
  finally
    Files.EndUpdate;
  end;
end;

function TDoxSettings.GetListBulletCharacters: string;
begin
  Result := ReadString('Parsing', 'ListBulletChars', '');
end;

function TDoxSettings.GetParameterDelimiterCharacters: string;
begin
  Result := ReadString('Parsing', 'ParameterDelimiterChars', '');
end;

function TDoxSettings.GetSectionDelimiterCharacters: string;
begin
  Result := ReadString('Parsing', 'SectionDelimiterChars', '');
end;

function TDoxSettings.GetTopicSeparator: string;
begin
  Result := ReadString('Parsing', 'TopicSeparator', '');
end;

function TDoxSettings.GetTrailerCharacters: string;
begin
  Result := ReadString('Parsing', 'TrailerChars', '');
end;

function TDoxSettings.GetWallCharacters: string;
begin
  Result := ReadString('Parsing', 'WallCharacters', '');
end;

function TDoxSettings.GetXmlOutputFileName: string;
var
  Directory, Section: string;
begin
  Section := Format('Configurations\%s\%s\Basic', [Configuration, XmlGUID]);
  Directory := inherited ReadString(Section, 'OutputDir', '');
  if Directory <> '' then
  begin
    if not PathIsAbsolute(Directory) then
      Directory := PathCanonicalize(PathGetRelativePath(ExtractFilePath(FileName), Directory));
    Result := ReadString(Section, 'OutputFilename', '');
    if Result = '' then
      Result := ChangeFileExt(ExtractFileName(FileName), '.xml');
    Result := PathAddSeparator(Directory) + Result;
  end
  else
  if Assigned(FMasterFile) then
  begin
    Result := FMasterFile.GetXmlOutputFileName;
    Result := ExtractFilePath(Result) + ChangeFileExt(ExtractFileName(FileName), '.xml');
  end
  else
    Result := ChangeFileExt(ExtractFileName(FileName), '.xml');
end;

function TDoxSettings.ReadString(const Section, Ident, Default: string): string;
begin
  Result := inherited ReadString(Section, Ident, Default);
  if Assigned(FMasterFile) and (Result = Default) then
    Result := FMasterFile.ReadString(Section, Ident, Default);
end;

end.

