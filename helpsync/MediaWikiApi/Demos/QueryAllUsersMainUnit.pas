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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiAllUserDone(Sender: TMediaWikiApi; const UserInfos: TMediaWikiAllUserInfos;
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
  FMediaWikiApi.OnQueryAllUserInfoDone := MediaWikiAllUserDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryAllUserInfoAsync(FContinueInfo, '', '', StrToInt(EditMaxUsers.Text), []);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  UserInfos: TMediaWikiAllUserInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryAllUserInfoDone := nil;
  FMediaWikiApi.QueryAllUserInfo(UserInfos, FContinueInfo, '', '', StrToInt(EditMaxUsers.Text), []);
  for Index := Low(UserInfos) to High(UserInfos) do
    MemoResult.Lines.Add(UserInfos[Index].UserName);
  EditStartUser.Text := FContinueInfo.ParameterValue;
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
  FMediaWikiApi.URL := 'http://wiki.delphi-jedi.org/w/api.php';
  FMediaWikiApi.UserAgent := 'MediaWiki JEDI bot';
  FMediaWikiApi.FollowRelocation := False;
  // login is not mandatory
  //FMediaWikiApi.Login()
end;

procedure TMainForm.MediaWikiAllUserDone(Sender: TMediaWikiApi;
  const UserInfos: TMediaWikiAllUserInfos; const ContinueInfo: TMediaWikiContinueInfo);
var
  Index: Integer;
begin
  for Index := Low(UserInfos) to High(UserInfos) do
    MemoResult.Lines.Add(UserInfos[Index].UserName);
  FContinueInfo := ContinueInfo;
  EditStartUser.Text := FContinueInfo.ParameterValue;
end;

end.

