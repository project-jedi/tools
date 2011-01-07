unit MoveMainUnit;

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
    LabelMoveToken: TLabel;
    EditFromPage: TEdit;
    LabelFromPage: TLabel;
    EditReason: TEdit;
    LabelReason: TLabel;
    ButtonMoveAsync: TButton;
    ButtonMoveSync: TButton;
    LabelToPage: TLabel;
    EditToPage: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonMoveAsyncClick(Sender: TObject);
    procedure ButtonMoveSyncClick(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi; const PageInfos: TMediaWikiPageInfos);
    procedure MediaWikiMoveDone(Sender: TMediaWikiApi; const MoveInfo: TMediaWikiMoveInfo);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonMoveAsyncClick(Sender: TObject);
begin
  FMediaWikiApi.OnMoveDone := MediaWikiMoveDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.MoveAsync(EditFromPage.Text, EditToPage.Text, EditToken.Text, EditReason.Text, -1, [mwfMoveTalk, mwfMoveNoRedirect]);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonMoveSyncClick(Sender: TObject);
var
  MoveInfo: TMediaWikiMoveInfo;
begin
  FMediaWikiApi.OnMoveDone := nil;
  FMediaWikiApi.Move(EditFromPage.Text, EditToPage.Text, EditToken.Text, EditReason.Text, -1, [mwfMoveTalk, mwfMoveNoRedirect], MoveInfo);
  if not MoveInfo.MoveSuccess then
    raise Exception.Create('Move failure');
  EditFromPage.Text := MoveInfo.MoveToPage;
  EditToPage.Text := MoveInfo.MoveFromPage;
end;

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageInfoDone := MediaWikiQueryPageInfoDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageInfoAsync(EditFromPage.Text, False, [mwfIncludeMoveToken]);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  PageInfos: TMediaWikiPageInfos;
begin
  MemoResult.Lines.Clear;

  FMediaWikiApi.OnQueryPageInfoDone := nil;
  FMediaWikiApi.QueryPageInfo(EditFromPage.Text, False, [mwfIncludeMoveToken], PageInfos);
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');
  EditFromPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageMoveToken;
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

procedure TMainForm.MediaWikiMoveDone(Sender: TMediaWikiApi;
  const MoveInfo: TMediaWikiMoveInfo);
begin
  if not MoveInfo.MoveSuccess then
    raise Exception.Create('failure');
  EditFromPage.Text := MoveInfo.MoveToPage;
  EditToPage.Text := MoveInfo.MoveFromPage;
end;

procedure TMainForm.MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi;
  const PageInfos: TMediaWikiPageInfos);
begin
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');

  EditFromPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageMoveToken;
end;

end.

