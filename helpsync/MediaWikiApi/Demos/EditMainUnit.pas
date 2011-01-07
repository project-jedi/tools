unit EditMainUnit;

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
    EditSummary: TEdit;
    LabelSummary: TLabel;
    EditToken: TEdit;
    LabelEditToken: TLabel;
    EditPage: TEdit;
    LabelPage: TLabel;
    EditRevID: TEdit;
    LabelRevID: TLabel;
    ButtonEditAsync: TButton;
    ButtonEditSync: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonEditAsyncClick(Sender: TObject);
    procedure ButtonEditSyncClick(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi; const PageInfos: TMediaWikiPageInfos);
    procedure MediaWikiQueryPageRevisionInfoDone(Sender: TMediaWikiApi; const PageRevisionInfos: TMediaWikiPageRevisionInfos;
      const ContinueInfo: TMediaWikiContinueInfo);
    procedure MediaWikiEditDone(Sender: TMediaWikiApi; const EditInfo: TMediaWikiEditInfo);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonEditAsyncClick(Sender: TObject);
begin
  FMediaWikiApi.OnEditDone := MediaWikiEditDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.EditAsync(EditPage.Text, '', MemoResult.Lines.Text, '', '', EditToken.Text, EditSummary.Text);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonEditSyncClick(Sender: TObject);
var
  EditInfo: TMediaWikiEditInfo;
begin
  FMediaWikiApi.OnEditDone := nil;
  FMediaWikiApi.Edit(EditPage.Text, '', MemoResult.Lines.Text, '', '', EditToken.Text, EditSummary.Text, EditInfo);
  if not EditInfo.EditSuccess then
    raise Exception.Create('edit failure');
  EditRevID.Text := IntToStr(EditInfo.EditNewRevID);
end;

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageInfoDone := MediaWikiQueryPageInfoDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageInfoAsync(EditPage.Text, False, [mwfIncludeEditToken]);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  PageInfos: TMediaWikiPageInfos;
  PageRevisionInfos: TMediaWikiPageRevisionInfos;
  RevisionID: TMediaWikiID;
  Unused: TMediaWikiContinueInfo;
begin
  MemoResult.Lines.Clear;

  FMediaWikiApi.OnQueryPageInfoDone := nil;
  FMediaWikiApi.QueryPageInfo(EditPage.Text, False, [mwfIncludeEditToken], PageInfos);
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');
  EditPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageEditToken;
  RevisionID := PageInfos[0].PageRevisionID;
  EditRevID.Text := IntToStr(RevisionID);

  FMediaWikiApi.OnQueryPageRevisionInfoDone := nil;
  FMediaWikiApi.QueryPageRevisionInfo(EditPage.Text, False, [mwfIncludeRevisionContent], PageRevisionInfos, Unused, 1, -1, RevisionID, RevisionID);
  if Length(PageRevisionInfos) <> 1 then
    raise Exception.Create('length error');
  MemoResult.Lines.Text := PageRevisionInfos[0].PageRevisionInfoContent;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  // logout is required
  FMediaWikiApi.Logout;
  FMediaWikiApi.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  LoginName, LoginPassword: string;
begin
  FMediaWikiApi := TMediaWikiApi.Create;
  FMediaWikiApi.URL := 'http://wiki.delphi-jedi.org/w/api.php';
  FMediaWikiApi.UserAgent := 'MediaWiki JEDI bot';
  FMediaWikiApi.FollowRelocation := False;
  // login is mandatory
  InputQuery('Login information', 'login name', LoginName);
  InputQuery('Login information', 'login password', LoginPassword);
  FMediaWikiApi.Login(LoginName, LoginPassword, True);
end;

procedure TMainForm.MediaWikiEditDone(Sender: TMediaWikiApi;
  const EditInfo: TMediaWikiEditInfo);
begin
  if not EditInfo.EditSuccess then
    raise Exception.Create('failure');
  EditRevID.Text := IntToStr(EditInfo.EditNewRevID);
end;

procedure TMainForm.MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi;
  const PageInfos: TMediaWikiPageInfos);
var
  RevisionID: TMediaWikiID;
  Unused: TMediaWikiContinueInfo;
begin
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');

  EditPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageEditToken;
  RevisionID := PageInfos[0].PageRevisionID;
  EditRevID.Text := IntToStr(RevisionID);

  FMediaWikiApi.OnQueryPageRevisionInfoDone := MediaWikiQueryPageRevisionInfoDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageRevisionInfoAsync(EditPage.Text, False, [mwfIncludeRevisionContent], Unused, 1, -1, RevisionID - 1, RevisionID);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.MediaWikiQueryPageRevisionInfoDone(Sender: TMediaWikiApi;
  const PageRevisionInfos: TMediaWikiPageRevisionInfos; const ContinueInfo: TMediaWikiContinueInfo);
begin
  if Length(PageRevisionInfos) <> 1 then
    raise Exception.Create('length error');
  MemoResult.Lines.Text := PageRevisionInfos[0].PageRevisionInfoContent;
end;

end.

