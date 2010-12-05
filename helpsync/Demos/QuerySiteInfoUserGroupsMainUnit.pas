unit QuerySiteInfoUserGroupsMainUnit;

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
    CheckBoxUserCount: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiSiteInfoUserGroupsDone(Sender: TMediaWikiApi; Infos: TStrings);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoUserGroupsDone := MediaWikiSiteInfoUserGroupsDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QuerySiteInfoUserGroupsAsync(CheckBoxUserCount.Checked);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoUserGroupsDone := nil;
  FMediaWikiApi.QuerySiteInfoUserGroups(CheckBoxUserCount.Checked, MemoResult.Lines);
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

procedure TMainForm.MediaWikiSiteInfoUserGroupsDone(Sender: TMediaWikiApi; Infos: TStrings);
begin
  MemoResult.Lines.Assign(Infos);
end;

end.

