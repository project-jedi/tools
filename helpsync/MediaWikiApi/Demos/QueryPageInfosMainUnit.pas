unit QueryPageInfosMainUnit;

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
    EditStartLink: TEdit;
    LabelStartLink: TLabel;
    LabelPageTitle: TLabel;
    EditPageTitle: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiPageInfosDone(Sender: TMediaWikiApi; const PageInfos: TMediaWikiPageInfos);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageInfoDone := MediaWikiPageInfosDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageInfoAsync(EditPageTitle.Text, False, [mwfIncludeProtection, mwfIncludeTalkID, mwfIncludeSubjectID, mwfIncludeURL, mwfIncludeEditToken, mwfIncludeMoveToken]);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  PageInfos: TMediaWikiPageInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageInfoDone := nil;
  FMediaWikiApi.QueryPageInfo(EditPageTitle.Text, False, [mwfIncludeProtection, mwfIncludeTalkID, mwfIncludeSubjectID, mwfIncludeURL, mwfIncludeEditToken, mwfIncludeMoveToken], PageInfos);
  for Index := Low(PageInfos) to High(PageInfos) do
  begin
    MemoResult.Lines.Add('title = ' + PageInfos[Index].PageBasics.PageTitle);
    MemoResult.Lines.Add('namespace = ' + IntToStr(PageInfos[Index].PageBasics.PageNamespace));
    MemoResult.Lines.Add('page ID = ' + IntToStr(PageInfos[Index].PageBasics.PageID));
    MemoResult.Lines.Add('last touched = ' + DateTimeToStr(PageInfos[Index].PageLastTouched));
    MemoResult.Lines.Add('revision ID = ' + IntToStr(PageInfos[Index].PageRevisionID));
    MemoResult.Lines.Add('views = ' + IntToStr(PageInfos[Index].PageViews));
    MemoResult.Lines.Add('size = ' + IntToStr(PageInfos[Index].PageSize));
    if mwfPageIsNew in PageInfos[Index].PageFlags then
      MemoResult.Lines.Add('flags = new');
    if mwfPageIsRedirect in PageInfos[Index].PageFlags then
      MemoResult.Lines.Add('flags = redirect');
    MemoResult.Lines.Add('protection # = ' + IntToStr(Length(PageInfos[Index].PageProtections)));
    MemoResult.Lines.Add('talk ID = ' + IntToStr(PageInfos[Index].PageTalkID));
    MemoResult.Lines.Add('subject ID = ' + IntToStr(PageInfos[Index].PageSubjectID));
    MemoResult.Lines.Add('full URL = ' + PageInfos[Index].PageFullURL);
    MemoResult.Lines.Add('edit URL = ' + PageInfos[Index].PageEditURL);
    MemoResult.Lines.Add('edit token = ' + PageInfos[Index].PageEditToken);
    MemoResult.Lines.Add('move token = ' + PageInfos[Index].PageMoveToken);
    MemoResult.Lines.Add('');
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  // logout is not required
  //FMediaWikiApi.Logout;
  FMediaWikiApi.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMediaWikiApi := TMediaWikiApi.Create;
  FMediaWikiApi.HttpCli.URL := 'http://wiki.delphi-jedi.org/w/api.php';
  FMediaWikiApi.HttpCli.Agent := 'MediaWiki JEDI bot';
  FMediaWikiApi.HttpCli.FollowRelocation := False;
  // login is not mandatory
  //FMediaWikiApi.Login()
end;

procedure TMainForm.MediaWikiPageInfosDone(Sender: TMediaWikiApi;
  const PageInfos: TMediaWikiPageInfos);
var
  Index: Integer;
begin
  for Index := Low(PageInfos) to High(PageInfos) do
  begin
    MemoResult.Lines.Add('title = ' + PageInfos[Index].PageBasics.PageTitle);
    MemoResult.Lines.Add('namespace = ' + IntToStr(PageInfos[Index].PageBasics.PageNamespace));
    MemoResult.Lines.Add('page ID = ' + IntToStr(PageInfos[Index].PageBasics.PageID));
    MemoResult.Lines.Add('last touched = ' + DateTimeToStr(PageInfos[Index].PageLastTouched));
    MemoResult.Lines.Add('revision ID = ' + IntToStr(PageInfos[Index].PageRevisionID));
    MemoResult.Lines.Add('views = ' + IntToStr(PageInfos[Index].PageViews));
    MemoResult.Lines.Add('size = ' + IntToStr(PageInfos[Index].PageSize));
    if mwfPageIsNew in PageInfos[Index].PageFlags then
      MemoResult.Lines.Add('flags = new');
    if mwfPageIsRedirect in PageInfos[Index].PageFlags then
      MemoResult.Lines.Add('flags = redirect');
    MemoResult.Lines.Add('protection # = ' + IntToStr(Length(PageInfos[Index].PageProtections)));
    MemoResult.Lines.Add('talk ID = ' + IntToStr(PageInfos[Index].PageTalkID));
    MemoResult.Lines.Add('subject ID = ' + IntToStr(PageInfos[Index].PageSubjectID));
    MemoResult.Lines.Add('full URL = ' + PageInfos[Index].PageFullURL);
    MemoResult.Lines.Add('edit URL = ' + PageInfos[Index].PageEditURL);
    MemoResult.Lines.Add('edit token = ' + PageInfos[Index].PageEditToken);
    MemoResult.Lines.Add('move token = ' + PageInfos[Index].PageMoveToken);
    MemoResult.Lines.Add('');
  end;
end;

end.

