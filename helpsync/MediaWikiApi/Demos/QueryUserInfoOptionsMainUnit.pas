unit QueryUserInfoOptionsMainUnit;

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
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiUserInfoOptionsDone(Sender: TMediaWikiApi; Infos: TStrings);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryUserInfoOptionsDone := MediaWikiUserInfoOptionsDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryUserInfoOptionsAsync;
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryUserInfoOptionsDone := nil;
  FMediaWikiApi.QueryUserInfoOptions(MemoResult.Lines);
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
  FMediaWikiApi.URL := 'http://wiki.delphi-jedi.org/api.php';
  FMediaWikiApi.UserAgent := 'MediaWiki JEDI bot';
  FMediaWikiApi.FollowRelocation := False;
  // login is mandatory
  InputQuery('Login information', 'login name', LoginName);
  InputQuery('Login information', 'login password', LoginPassword);
  FMediaWikiApi.Login(LoginName, LoginPassword, True);
end;

procedure TMainForm.MediaWikiUserInfoOptionsDone(Sender: TMediaWikiApi; Infos: TStrings);
begin
  MemoResult.Lines.Assign(Infos);
end;

end.

