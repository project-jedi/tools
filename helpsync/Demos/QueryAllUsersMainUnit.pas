unit QueryAllUsersMainUnit;

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
    EditMaxUsers: TEdit;
    LabelMaxUsers: TLabel;
    EditStartUser: TEdit;
    LabelStartUser: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiAllUserDone(Sender: TMediaWikiApi; const UserInfos: TMediaWikiAllUserInfos);
    procedure MediaWikiAllUserContinue(Sender: TMediaWikiApi; const Start: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryAllUserInfoDone := MediaWikiAllUserDone;
  FMediaWikiApi.OnQueryAllUserInfoContinue := MediaWikiAllUserContinue;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryAllUserInfoAsync(EditStartUser.Text, '', '', StrToInt(EditMaxUsers.Text), []);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  UserInfos: TMediaWikiAllUserInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryAllUserInfoDone := nil;
  FMediaWikiApi.OnQueryAllUserInfoContinue := nil;
  FMediaWikiApi.QueryAllUserInfo(UserInfos, EditStartUser.Text, '', '', StrToInt(EditMaxUsers.Text), []);
  for Index := Low(UserInfos) to High(UserInfos) do
    MemoResult.Lines.Add(UserInfos[Index].UserName);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMediaWikiApi := TMediaWikiApi.Create;
  FMediaWikiApi.URL := 'http://wiki.delphi-jedi.org/api.php';
  FMediaWikiApi.UserAgent := 'MediaWiki JEDI bot';
  FMediaWikiApi.FollowRelocation := False;
  // login is not mandatory
  //FMediaWikiApi.Login()
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // logout is not required
  //FMediaWikiApi.Logout;
  FMediaWikiApi.Free;
end;

procedure TMainForm.MediaWikiAllUserContinue(Sender: TMediaWikiApi;
  const Start: string);
begin
  EditStartUser.Text := Start;
end;

procedure TMainForm.MediaWikiAllUserDone(Sender: TMediaWikiApi;
  const UserInfos: TMediaWikiAllUserInfos);
var
  Index: Integer;
begin
  for Index := Low(UserInfos) to High(UserInfos) do
    MemoResult.Lines.Add(UserInfos[Index].UserName);
end;

end.

