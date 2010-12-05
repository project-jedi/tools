unit QueryBackLinksMainUnit;

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
    EditMaxLinks: TEdit;
    LabelMaxLinks: TLabel;
    EditStartLink: TEdit;
    LabelStartLink: TLabel;
    LabelPageTitle: TLabel;
    EditPageTitle: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiBackLinkDone(Sender: TMediaWikiApi; const BackLinkInfos: TMediaWikiBackLinkInfos);
    procedure MediaWikiBackLinkContinue(Sender: TMediaWikiApi; const Start: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryBackLinkInfoDone := MediaWikiBackLinkDone;
  FMediaWikiApi.OnQueryBackLinkInfoContinue := MediaWikiBackLinkContinue;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryBackLinkInfoAsync(EditPageTitle.Text, -1, StrToInt(EditMaxLinks.Text), EditStartLink.Text, []);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  BackLinkInfos: TMediaWikiBackLinkInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryBackLinkInfoDone := nil;
  FMediaWikiApi.OnQueryBackLinkInfoContinue := nil;
  FMediaWikiApi.QueryBackLinkInfo(EditPageTitle.Text, BackLinkInfos, -1, StrToInt(EditMaxLinks.Text), EditStartLink.Text, []);
  for Index := Low(BackLinkInfos) to High(BackLinkInfos) do
    MemoResult.Lines.Add(BackLinkInfos[Index].BackLinkPageBasics.PageTitle);
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

procedure TMainForm.MediaWikiBackLinkContinue(Sender: TMediaWikiApi;
  const Start: string);
begin
  EditStartLink.Text := Start;
end;

procedure TMainForm.MediaWikiBackLinkDone(Sender: TMediaWikiApi;
  const BackLinkInfos: TMediaWikiBackLinkInfos);
var
  Index: Integer;
begin
  for Index := Low(BackLinkInfos) to High(BackLinkInfos) do
    MemoResult.Lines.Add(BackLinkInfos[Index].BackLinkPageBasics.PageTitle);
end;

end.

