unit QuerySiteInfoInterWikiMapMainUnit;

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
    CheckBoxLocal: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiSiteInfoInterWikiMapDone(Sender: TMediaWikiApi; Infos: TStrings);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoInterWikiMapDone := MediaWikiSiteInfoInterWikiMapDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QuerySiteInfoInterWikiMapAsync(CheckBoxLocal.Checked);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoInterWikiMapDone := nil;
  FMediaWikiApi.QuerySiteInfoInterWikiMap(CheckBoxLocal.Checked, MemoResult.Lines);
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

procedure TMainForm.MediaWikiSiteInfoInterWikiMapDone(Sender: TMediaWikiApi; Infos: TStrings);
begin
  MemoResult.Lines.Assign(Infos);
end;

end.

