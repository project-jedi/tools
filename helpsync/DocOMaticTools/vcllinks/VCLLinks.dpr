program VCLLinks;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JclStrings,
  JclSimpleXml,
  MediaWikiApi in '..\..\MediaWikiApi\MediaWikiApi.pas',
  MediaWikiUtils in '..\..\MediaWikiApi\MediaWikiUtils.pas';

procedure ExecuteQuery(const ResultFile: string);
var
  API: TMediaWikiApi;
  XML: TJclSimpleXml;
  Login, Password: string;
  I, Count: Integer;
  ContinueInfo, OldContinueInfo: TMediaWikiContinueInfo;
  AllPageInfos: TMediaWikiAllPageInfos;
  PageElem: TJclSimpleXmlElem;
begin
  Count := 0;
  API := TMediaWikiApi.Create;
  XML := TJclSimpleXML.Create;
  try
    XML.Root.Name := 'pages';
    API.HttpCli.URL := 'http://docwiki.embarcadero.com/VCL/e/api.php';
    API.HttpCli.Agent := 'MediaWiki JEDI bot';
    API.HttpCli.FollowRelocation := False;
    Write('login: ');
    ReadLn(Login);
    Write('password: ');
    ReadLn(Password);
    API.Login(Login, Password, True);
    try
      ContinueInfo.ParameterName := '';
      ContinueInfo.ParameterValue := '';
      repeat
        try
          OldContinueInfo := ContinueInfo;
          API.QueryAllPageInfo(AllPageInfos, ContinueInfo, '', 5000);
        except
          SetLength(AllPageInfos, 0);
          ContinueInfo := OldContinueInfo;
        end;
        for I := Low(AllPageInfos) to High(AllPageInfos) do
        begin
          PageElem := XML.Root.Items.Add('page');
          PageElem.Properties.Add('title', AllPageInfos[I].PageTitle);
          PageElem.Properties.Add('id', AllPageInfos[I].PageID);
          PageElem.Properties.Add('ns', AllPageInfos[I].PageNamespace);
        end;
        Inc(Count, Length(AllPageInfos));
        WriteLn(Count);
        Sleep(5000);
      until ContinueInfo.ParameterValue = '';
    finally
      API.Logout;
    end;
    XML.SaveToFile(ResultFile);
  finally
    API.Free;
    XML.Free;
  end;
end;

procedure ExecuteConvert(const ResultFile, DoxDBFile: string);
(*const
  DocSuffixes: array [0..8] of string =
    ( ' Members',
      ' Constructors and Destructors',
      ' Functions',
      ' Properties',
      ' Inherited',
      ' Events',
      ' Fields',
      ' Types',
      ' Constants' );*)
var
  XML, DoxDB: TJclSimpleXml;
  I, J: Integer;
  Pages, Page, ProjectDatabase, LinkDatabase, Link, Target: TJclSimpleXmlElem;
  Title: TJclSimpleXmlProp;
  ID, DisplayString: string;
begin
  XML := TJclSimpleXML.Create;
  DoxDB := TJclSimpleXML.Create;
  try
    XML.LoadFromFile(ResultFile);
    XML.Options := XML.Options - [sxoAutoCreate];

    DoxDB.Options := XML.Options - [sxoAutoCreate];
    ProjectDatabase := DoxDB.Root;
    ProjectDatabase.Name := 'projectdatabase';
    LinkDatabase := ProjectDatabase.Items.Add('linkdatabase');

    Pages := XML.Root;
    for I := 0 to Pages.Items.Count - 1 do
    begin
      Page := Pages.Items.Item[I];
      Title := Page.Properties.ItemNamed['title'];
      if not Assigned(Title) then
        raise Exception.Create('missing title');
      ID := Title.Value;
      DisplayString:= Title.Value;

      // remove all known suffixes
      //repeat
      //  J := StrSuffixIndex(ID, DocSuffixes);
      //  if J >= 0 then
      //    ID := StrLeft(ID, Length(ID) - Length(DocSuffixes[J]));
      //until J < 0;

      // fix wiki implicit renames
      ID := StrReplaceChar(ID, ' ', '_');

      // remove namespace prefix if any
      J := Pos('.', DisplayString);
      if J > 0 then
        DisplayString := Copy(DisplayString, J + 1, Length(DisplayString) - J);

      // save the link
      Link := LinkDatabase.Items.Add('link');
      Link.Properties.Add('topicid', ID);
      Link.Properties.Add('displaystring', DisplayString);
      Target := Link.Items.Add('target');
      Target.Properties.Add('id', 'xml');
      Target.Value := 'VCL:' + Title.Value;
    end;
    DoxDB.SaveToFile(DoxDBFile);
  finally
    XML.Free;
  end;
end;

begin
  try
    WriteLn('VCLLinks: query wiki pages and generate Doc-o-Matic doxdb file');
    WriteLn('copyright (c) Florent Ouchet 2011');
    WriteLn('');
    if (ParamCount = 2) and (ParamStr(1) = 'query') then
      ExecuteQuery(ParamStr(2))
    else
    if (ParamCount = 3) and (ParamStr(1) = 'convert') then
      ExecuteConvert(ParamStr(2), ParamStr(3))
    else
    begin
      WriteLn('usage: VCLLinks query result.xml');
      WriteLn('    query all pages from Embarcadero RTL/VCL wiki');
      WriteLn('');
      WriteLn('usage: VCLLinks convert result.xml result.doxdb');
      WriteLn('    convert result.xml to a Doc-o-Matic doxdb file');
      WriteLn('');
    end;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.

