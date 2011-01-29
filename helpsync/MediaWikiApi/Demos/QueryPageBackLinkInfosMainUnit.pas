unit QueryPageBackLinkInfosMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  MediaWikiUtils,
  MediaWikiApi;

type
  TMainForm = class(TForm)
    MemoResult: TMemo;
    ButtonQueryAsync: TButton;
    ButtonQuerySync: TButton;
    EditMaxLinks: TEdit;
    LabelMaxLinks: TLabel;
    EditPage: TEdit;
    LabelPage: TLabel;
    LabelStartLink: TLabel;
    EditStartLink: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiAllPageBackLinkDone(Sender: TMediaWikiApi; const BackLinkInfos: TMediaWikiBackLinkInfos;
      const ContinueInfo: TMediaWikiContinueInfo);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryBackLinkInfoDone := MediaWikiAllPageBackLinkDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryBackLinkInfoAsync(EditPage.Text, FContinueInfo, -1, StrToInt(EditMaxLinks.Text), [mwfIncludeBackLinksFromRedirect]);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  BackLinkInfos: TMediaWikiBackLinkInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryBackLinkInfoDone := nil;
  FMediaWikiApi.QueryBackLinkInfo(EditPage.Text, BackLinkInfos, FContinueInfo, -1, StrToInt(EditMaxLinks.Text), [mwfIncludeBackLinksFromRedirect]);
  for Index := Low(BackLinkInfos) to High(BackLinkInfos) do
  begin
    MemoResult.Lines.Add('source page title = ' + BackLinkInfos[Index].BackLinkPageBasics.PageTitle);
    MemoResult.Lines.Add('source page ID = ' + IntToStr(BackLinkInfos[Index].BackLinkPageBasics.PageID));
    MemoResult.Lines.Add('source page namespace = ' + IntToStr(BackLinkInfos[Index].BackLinkPageBasics.PageNamespace));
    if mwfBackLinkIsRedirect in BackLinkInfos[Index].BackLinkFlags then
      MemoResult.Lines.Add('redirect');
    if mwfBackLinkToRedirect in BackLinkInfos[Index].BackLinkFlags then
    begin
      MemoResult.Lines.Add('from page title = ' + BackLinkInfos[Index].BackLinkRedirFromPageBasics.PageTitle);
      MemoResult.Lines.Add('from page ID = ' + IntToStr(BackLinkInfos[Index].BackLinkRedirFromPageBasics.PageID));
      MemoResult.Lines.Add('from page namespace = ' + IntToStr(BackLinkInfos[Index].BackLinkRedirFromPageBasics.PageNamespace));
    end;
    MemoResult.Lines.Add('');
  end;
  EditStartLink.Text := FContinueInfo.ParameterValue;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  // logout is not required
  // FMediaWikiApi.Logout;
  FMediaWikiApi.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMediaWikiApi := TMediaWikiApi.Create;
  FMediaWikiApi.HttpCli.URL := 'http://wiki.delphi-jedi.org/w/api.php';
  FMediaWikiApi.HttpCli.Agent := 'MediaWiki JEDI bot';
  FMediaWikiApi.HttpCli.FollowRelocation := False;
  // login is not mandatory
  // FMediaWikiApi.Login();
end;

procedure TMainForm.MediaWikiAllPageBackLinkDone(Sender: TMediaWikiApi;
  const BackLinkInfos: TMediaWikiBackLinkInfos; const ContinueInfo: TMediaWikiContinueInfo);
var
  Index: Integer;
begin
  for Index := Low(BackLinkInfos) to High(BackLinkInfos) do
  begin
    MemoResult.Lines.Add('source page title = ' + BackLinkInfos[Index].BackLinkPageBasics.PageTitle);
    MemoResult.Lines.Add('source page ID = ' + IntToStr(BackLinkInfos[Index].BackLinkPageBasics.PageID));
    MemoResult.Lines.Add('source page namespace = ' + IntToStr(BackLinkInfos[Index].BackLinkPageBasics.PageNamespace));
    if mwfBackLinkIsRedirect in BackLinkInfos[Index].BackLinkFlags then
      MemoResult.Lines.Add('redirect');
    if mwfBackLinkToRedirect in BackLinkInfos[Index].BackLinkFlags then
    begin
      MemoResult.Lines.Add('from page title = ' + BackLinkInfos[Index].BackLinkRedirFromPageBasics.PageTitle);
      MemoResult.Lines.Add('from page ID = ' + IntToStr(BackLinkInfos[Index].BackLinkRedirFromPageBasics.PageID));
      MemoResult.Lines.Add('from page namespace = ' + IntToStr(BackLinkInfos[Index].BackLinkRedirFromPageBasics.PageNamespace));
    end;
    MemoResult.Lines.Add('');
  end;
  FContinueInfo := ContinueInfo;
  EditStartLink.Text := FContinueInfo.ParameterValue;
end;

end.

