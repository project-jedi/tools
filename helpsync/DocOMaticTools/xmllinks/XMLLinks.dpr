program XMLLinks;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  JclStrings,
  JclFileUtils,
  JclSimpleXml,
  DoxSettings in '..\DoxSettings.pas';

type
  TArguments = record
    DoxFileName: string;
    Configuration: string;
    LinkFormat: string;
  end;

function ParseArguments(out Arguments: TArguments): Boolean;
begin
  Result := ParamCount = 3;
  if Result then
  begin
    Arguments.DoxFileName := ParamStr(1);
    Arguments.Configuration := ParamStr(2);
    Arguments.LinkFormat := ParamStr(3);
    if not PathIsAbsolute(Arguments.DoxFileName) then
      Arguments.DoxFileName := PathCanonicalize(PathAddSeparator(GetCurrentDir) + Arguments.DoxFileName);
  end;
end;

procedure DisplayHelp;
begin
  WriteLn('usage: XMLLinks project.dox configuration link_format');
  WriteLn('example: XMLLinks help.dox XML %id@%xml');
  WriteLn;
  WriteLn('arguments:');
  WriteLn(' - project.dox: Doc-O-Matic project file');
  WriteLn(' - configuration: Name of the Doc-O-Matic configuration');
  WriteLn(' - link_format: format pattern for link targets');
  WriteLn('     This format pattern supports the following format specifiers:');
  WriteLn('       * %title: Topic title');
  WriteLn('       * %id: Topic ID');
  WriteLn('       * %file: Project file name (without .dox extension)');
  WriteLn('       * %doxfile: Project file name (with .dox extension)');
  WriteLn('       * %xml: XML file name');
end;

procedure ParseXml(const XMLFileName: string; Topics: TStrings);
  procedure ParseNode(Elem: TJclSimpleXMLElem);
  var
    IDProp: TJclSimpleXMLProp;
    TitleElem: TJclSimpleXMLElem;
    I: Integer;
  begin
    if Elem.Name = 'topic' then
    begin
      IDProp := Elem.Properties.ItemNamed['id'];
      TitleElem := Elem.Items.ItemNamed['title'];
      if Assigned(IDProp) and Assigned(TitleElem) and (Copy(IDProp.Value, 1, 2) <> '!!') then
        Topics.Values[IDProp.Value] := TitleElem.Value;
    end;
    for I := 0 to Elem.ItemCount - 1 do
      ParseNode(Elem.Items.Item[I]);
  end;
var
  XML: TJclSimpleXML;
begin
  XML := TJclSimpleXML.Create;
  try
    XML.LoadFromFile(XMLFileName);
    XML.Options := XML.Options - [sxoAutoCreate];
    ParseNode(XML.Root);
  finally
    XML.Free;
  end;
end;

procedure FillDoxDB(const DoxDBFileName, LinkFormat: string; LinkParams: array of TVarRec;
  Topics: TStrings);
var
  XML: TJclSimpleXML;
  LinkDatabase, Link, Target: TJclSimpleXMLElem;
  TopicIDProp, DisplayStringProp, IDProp: TJclSimpleXMLProp;
  I, J: Integer;
  Found: Boolean;
begin
  XML := TJclSimpleXML.Create;
  try
    XML.LoadFromFile(DoxDBFileName);
    XML.Options := XML.Options - [sxoAutoCreate];

    LinkDatabase := XML.Root.Items.ItemNamed['linkdatabase'];
    if not Assigned(LinkDatabase) then
      LinkDatabase := XML.Root.Items.Add('linkdatabase');

    // update existing links first
    for I := 0 to LinkDatabase.ItemCount - 1 do
    begin
      Link := LinkDatabase.Items.Item[I];
      TopicIDProp := Link.Properties.ItemNamed['topicid'];
      DisplayStringProp := Link.Properties.ItemNamed['displaystring'];
      if Assigned(TopicIDProp) and Assigned(DisplayStringProp) then
      begin
        if Topics.Values[TopicIDProp.Value] <> '' then
        begin
          Found := False;
          LinkParams[0].VPChar := PChar(DisplayStringProp.Value);
          LinkParams[1].VPChar := PChar(TopicIDProp.Value);
          for J := 0 to Link.ItemCount - 1 do
          begin
            Target := Link.Items.Item[J];
            IDProp := Target.Properties.ItemNamed['id'];
            if not Assigned(IDProp) then
              raise Exception.Create('invalid target');
            if IDProp.Value = 'xml' then
            begin
              Found := True;
              Target.Value := Format(LinkFormat, LinkParams);
            end;
          end;
          if not Found then
          begin
            Target := Link.Items.Add('target');
            Target.Properties.Add('id', 'xml');
            Target.Value := Format(LinkFormat, LinkParams);
          end;
          Topics.Values[TopicIDProp.Value] := '';
        end;
      end
      else
        raise Exception.Create('invalid link');
    end;

    for I := 0 to Topics.Count - 1 do
    begin
      Link := LinkDatabase.Items.Add('link');
      TopicIDProp := Link.Properties.Add('topicid', Topics.Names[I]);
      DisplayStringProp := Link.Properties.Add('displaystring', Topics.ValueFromIndex[I]);
      Target := Link.Items.Add('target');
      Target.Properties.Add('id', 'xml');
      LinkParams[0].VPChar := PChar(DisplayStringProp.Value);
      LinkParams[1].VPChar := PChar(TopicIDProp.Value);
      Target.Value := Format(LinkFormat, LinkParams);
    end;
    XML.SaveToFile(DoxDBFileName);
  finally
    XML.Free;
  end;
end;

procedure ExecuteArguments(const Arguments: TArguments);
var
  DoxSettings: TDoxSettings;
  XMLFileName, DoxDBFileName: string;
  LinkParams: array of TVarRec;
  LinkFormat: string;
  Topics: TStrings;
begin
  DoxSettings := TDoxSettings.Create(Arguments.DoxFileName, Arguments.Configuration);
  try
    XMLFileName := DoxSettings.GetXmlOutputFileName;
    DoxDBFileName := DoxSettings.GetDoxDBFileName;
  finally
    DoxSettings.Free;
  end;

  // setup link format
  LinkFormat := Arguments.LinkFormat;
  StrReplace(LinkFormat, '%title', '%0s');
  StrReplace(LinkFormat, '%id', '%1s');
  StrReplace(LinkFormat, '%file', '%2s');
  StrReplace(LinkFormat, '%doxfile', '%3s');
  StrReplace(LinkFormat, '%xml', '%4s');

  // setup link params
  SetLength(LinkParams, 5);
  LinkParams[0].VType := vtPChar; // topic title
  LinkParams[0].VPChar := '';
  LinkParams[1].VType := vtPChar; // topic ID
  LinkParams[1].VPChar := '';
  LinkParams[2].VType := vtPChar; // project file name without dox
  LinkParams[2].VPChar := PChar(ChangeFileExt(Arguments.DoxFileName, ''));
  LinkParams[3].VType := vtPChar; // project file name with dox
  LinkParams[3].VPChar := PChar(Arguments.DoxFileName);
  LinkParams[4].VType := vtPChar; // xml file name
  LinkParams[4].VPChar := PChar(XMLFileName);

  Topics := TStringList.Create;
  try
    ParseXml(XMLFileName, Topics);
    WriteLn(Topics.Count, ' topics found.');
    FillDoxDB(DoxDBFileName, LinkFormat, LinkParams, Topics);
  finally
    Topics.Free;
  end;
end;

procedure Execute;
var
  Arguments: TArguments;
begin
  WriteLn('XMLLinks 1.0: fix Doc-O-Matic XML link databases');
  WriteLn('Copyright Florent Ouchet, 2011, All right reserved');
  WriteLn;
  if ParseArguments(Arguments) then
    ExecuteArguments(Arguments)
  else
    DisplayHelp;
end;

begin
  try
    Execute;
    ExitCode := 0;
  except
    on E:Exception do
    begin
      Writeln(E.Classname, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.

