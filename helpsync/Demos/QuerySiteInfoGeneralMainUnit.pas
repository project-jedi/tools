unit QuerySiteInfoGeneralMainUnit;

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
    procedure FormDestroy(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiSiteInfoGeneralDone(Sender: TMediaWikiApi; Infos: TStrings);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoGeneralDone := MediaWikiSiteInfoGeneralDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QuerySiteInfoGeneralAsync;
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoGeneralDone := nil;
  FMediaWikiApi.QuerySiteInfoGeneral(MemoResult.Lines);
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

procedure TMainForm.MediaWikiSiteInfoGeneralDone(Sender: TMediaWikiApi; Infos: TStrings);
begin
  MemoResult.Lines.Assign(Infos);
end;

end.

