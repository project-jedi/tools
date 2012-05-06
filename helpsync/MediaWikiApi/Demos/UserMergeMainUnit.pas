unit UserMergeMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  MediaWikiUtils,
  MediaWikiApi;

type
  TMainForm = class(TForm)
    ButtonQueryAsync: TButton;
    ButtonQuerySync: TButton;
    EditToUser: TEdit;
    LabelPage: TLabel;
    ButtonMergeAsync: TButton;
    ButtonMergeSync: TButton;
    ListBoxUserList: TListBox;
    CheckBoxDeleteUser: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonMergeAsyncClick(Sender: TObject);
    procedure ButtonMergeSyncClick(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiQueryAllUserInfoDone(Sender: TMediaWikiApi; const UserInfos: TMediaWikiAllUserInfos;
      const ContinueInfo: TMediaWikiContinueInfo);
    procedure MediaWikiUserMergeDone(Sender: TMediaWikiApi; const UserMergeInfo: TMediaWikiUserMergeInfo);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonMergeAsyncClick(Sender: TObject);
begin
  FMediaWikiApi.OnUserMergeDone := MediaWikiUserMergeDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.UserMergeAsync(ListBoxUserList.Items[ListBoxUserList.ItemIndex], EditToUser.Text, CheckBoxDeleteUser.Checked);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonMergeSyncClick(Sender: TObject);
var
  UserMergeInfo: TMediaWikiUserMergeInfo;
begin
  FMediaWikiApi.OnUserMergeDone := nil;
  FMediaWikiApi.UserMerge(ListBoxUserList.Items[ListBoxUserList.ItemIndex], EditToUser.Text, CheckBoxDeleteUser.Checked, UserMergeInfo);
  if not UserMergeInfo.UserMergeSuccess then
    raise Exception.Create('User Merge failure');
  EditToUser.Text := UserMergeInfo.UserMergeNewUser;
end;

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  FMediaWikiApi.OnQueryAllUserInfoDone := MediaWikiQueryAllUserInfoDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryAllUserInfoAsync(FContinueInfo, '', '', 100);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  AllUserInfos: TMediaWikiAllUserInfos;
  I: Integer;
begin
  FMediaWikiApi.OnQueryAllUserInfoDone := nil;
  FMediaWikiApi.QueryAllUserInfo(AllUserInfos, FContinueInfo, '', '', 100);
  for I := Low(AllUserInfos) to High(AllUserInfos) do
    ListBoxUserList.Items.Add(AllUserInfos[I].UserName);
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
  FMediaWikiApi.HttpCli.URL := 'http://wiki.delphi-jedi.org/w/api.php';
  FMediaWikiApi.HttpCli.Agent := 'MediaWiki JEDI bot';
  FMediaWikiApi.HttpCli.FollowRelocation := False;
  // login is mandatory
  InputQuery('Login information', 'login name', LoginName);
  InputQuery('Login information', 'login password', LoginPassword);
  FMediaWikiApi.Login(LoginName, LoginPassword, True);
end;

procedure TMainForm.MediaWikiUserMergeDone(Sender: TMediaWikiApi;
  const UserMergeInfo: TMediaWikiUserMergeInfo);
begin
  if not UserMergeInfo.UserMergeSuccess then
    raise Exception.Create('failure');
  EditToUser.Text := UserMergeInfo.UserMergeNewUser;
end;

procedure TMainForm.MediaWikiQueryAllUserInfoDone(Sender: TMediaWikiApi;
  const UserInfos: TMediaWikiAllUserInfos; const ContinueInfo: TMediaWikiContinueInfo);
var
  I: Integer;
begin
  for I := Low(UserInfos) to High(UserInfos) do
    ListBoxUserList.Items.Add(UserInfos[I].UserName);
  FContinueInfo := ContinueInfo;
end;

end.

