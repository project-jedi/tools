unit QuerySiteInfoDBReplLagMainUnit;

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
    CheckBoxAllDB: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiSiteInfoDBReplLagDone(Sender: TMediaWikiApi; Infos: TStrings);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoDBReplLagDone := MediaWikiSiteInfoDBReplLagDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QuerySiteInfoDBReplLagAsync(CheckBoxAllDB.Checked);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoDBReplLagDone := nil;
  FMediaWikiApi.QuerySiteInfoDBReplLag(CheckBoxAllDB.Checked, MemoResult.Lines);
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

procedure TMainForm.MediaWikiSiteInfoDBReplLagDone(Sender: TMediaWikiApi; Infos: TStrings);
begin
  MemoResult.Lines.Assign(Infos);
end;

end.

