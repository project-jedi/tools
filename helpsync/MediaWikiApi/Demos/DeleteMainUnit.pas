unit DeleteMainUnit;

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
    EditToken: TEdit;
    LabelToken: TLabel;
    EditPage: TEdit;
    LabelPage: TLabel;
    EditReason: TEdit;
    LabelReason: TLabel;
    ButtonDeleteAsync: TButton;
    ButtonDeleteSync: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonDeleteAsyncClick(Sender: TObject);
    procedure ButtonDeleteSyncClick(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi; const PageInfos: TMediaWikiPageInfos);
    procedure MediaWikiDeleteDone(Sender: TMediaWikiApi; const DeleteInfo: TMediaWikiDeleteInfo);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonDeleteAsyncClick(Sender: TObject);
begin
  FMediaWikiApi.OnDeleteDone := MediaWikiDeleteDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.DeleteAsync(EditPage.Text, EditToken.Text, EditReason.Text, -1);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonDeleteSyncClick(Sender: TObject);
var
  DeleteInfo: TMediaWikiDeleteInfo;
begin
  FMediaWikiApi.OnDeleteDone := nil;
  FMediaWikiApi.Delete(EditPage.Text, EditToken.Text, EditReason.Text, -1, DeleteInfo);
  if not DeleteInfo.DeleteSuccess then
    raise Exception.Create('Delete failure');
  EditPage.Text := DeleteInfo.DeletePage;
end;

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageInfoDone := MediaWikiQueryPageInfoDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageInfoAsync(EditPage.Text, False, [mwfIncludeDeleteToken]);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  PageInfos: TMediaWikiPageInfos;
begin
  MemoResult.Lines.Clear;

  FMediaWikiApi.OnQueryPageInfoDone := nil;
  FMediaWikiApi.QueryPageInfo(EditPage.Text, False, [mwfIncludeDeleteToken], PageInfos);
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');
  EditPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageDeleteToken;
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

procedure TMainForm.MediaWikiDeleteDone(Sender: TMediaWikiApi;
  const DeleteInfo: TMediaWikiDeleteInfo);
begin
  if not DeleteInfo.DeleteSuccess then
    raise Exception.Create('failure');
  EditPage.Text := DeleteInfo.DeletePage;
end;

procedure TMainForm.MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi;
  const PageInfos: TMediaWikiPageInfos);
begin
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');

  EditPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageDeleteToken;
end;

end.

