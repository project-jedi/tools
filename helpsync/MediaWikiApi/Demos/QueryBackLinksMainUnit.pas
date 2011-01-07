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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiBackLinkDone(Sender: TMediaWikiApi; const BackLinkInfos: TMediaWikiBackLinkInfos;
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
  FMediaWikiApi.OnQueryBackLinkInfoDone := MediaWikiBackLinkDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryBackLinkInfoAsync(EditPageTitle.Text, FContinueInfo, -1, StrToInt(EditMaxLinks.Text), []);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  BackLinkInfos: TMediaWikiBackLinkInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryBackLinkInfoDone := nil;
  FMediaWikiApi.QueryBackLinkInfo(EditPageTitle.Text, BackLinkInfos, FContinueInfo, -1, StrToInt(EditMaxLinks.Text), []);
  for Index := Low(BackLinkInfos) to High(BackLinkInfos) do
    MemoResult.Lines.Add(BackLinkInfos[Index].BackLinkPageBasics.PageTitle);
  EditStartLink.Text := FContinueInfo.ParameterValue;
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

procedure TMainForm.MediaWikiBackLinkDone(Sender: TMediaWikiApi;
  const BackLinkInfos: TMediaWikiBackLinkInfos; const ContinueInfo: TMediaWikiContinueInfo);
var
  Index: Integer;
begin
  for Index := Low(BackLinkInfos) to High(BackLinkInfos) do
    MemoResult.Lines.Add(BackLinkInfos[Index].BackLinkPageBasics.PageTitle);
  FContinueInfo := ContinueInfo;
  EditStartLink.Text := FContinueInfo.ParameterValue;
end;

end.

