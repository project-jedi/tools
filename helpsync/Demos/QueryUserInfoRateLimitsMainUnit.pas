unit QueryUserInfoRateLimitsMainUnit;

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
    procedure MediaWikiUserInfoRateLimitsDone(Sender: TMediaWikiApi; const Infos: TMediaWikiRateLimits);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryUserInfoRateLimitsDone := MediaWikiUserInfoRateLimitsDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryUserInfoRateLimitsAsync;
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  Infos: TMediaWikiRateLimits;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryUserInfoRateLimitsDone := nil;
  FMediaWikiApi.QueryUserInfoRateLimits(Infos);
  for Index := Low(Infos) to High(Infos) do
  begin
    MemoResult.Lines.Add('action = ' + Infos[Index].RateLimitAction);
    MemoResult.Lines.Add('group = ' + Infos[Index].RateLimitGroup);
    MemoResult.Lines.Add('hits = ' + IntToStr(Infos[Index].RateLimitHits));
    MemoResult.Lines.Add('seconds = ' + IntToStr(Infos[Index].RateLimitSeconds));
    MemoResult.Lines.Add('');
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Action = caFree then
  begin
    // logout is not required
    //FMediaWikiApi.Logout;
    FMediaWikiApi.Free;
  end;
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

procedure TMainForm.MediaWikiUserInfoRateLimitsDone(Sender: TMediaWikiApi; const Infos: TMediaWikiRateLimits);
var
  Index: Integer;
begin
  for Index := Low(Infos) to High(Infos) do
  begin
    MemoResult.Lines.Add('action = ' + Infos[Index].RateLimitAction);
    MemoResult.Lines.Add('group = ' + Infos[Index].RateLimitGroup);
    MemoResult.Lines.Add('hits = ' + IntToStr(Infos[Index].RateLimitHits));
    MemoResult.Lines.Add('seconds = ' + IntToStr(Infos[Index].RateLimitSeconds));
    MemoResult.Lines.Add('');
  end;
end;

end.

