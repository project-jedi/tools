unit QueryAllPagesMainUnit;

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
    EditMaxPages: TEdit;
    LabelMaxPages: TLabel;
    EditStartPage: TEdit;
    LabelStartPage: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiAllPageDone(Sender: TMediaWikiApi; const PageInfos: TMediaWikiAllPageInfos);
    procedure MediaWikiAllPageContinue(Sender: TMediaWikiApi; const Start: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryAllPageInfoDone := MediaWikiAllPageDone;
  FMediaWikiApi.OnQueryAllPageInfoContinue := MediaWikiAllPageContinue;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryAllPageInfoAsync(EditStartPage.Text, '', StrToInt(EditMaxPages.Text),
    -1, mwfAllPageFilterAll, mwfAllPageLangAll, -1, -1, mwfAllPageProtectionNone, mwfAllPageLevelNone, mwfAllPageAscending);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  PageInfos: TMediaWikiAllPageInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryAllPageInfoDone := nil;
  FMediaWikiApi.OnQueryAllPageInfoContinue := nil;
  FMediaWikiApi.QueryAllPageInfo(PageInfos, EditStartPage.Text, '', StrToInt(EditMaxPages.Text),
    -1, mwfAllPageFilterAll, mwfAllPageLangAll, -1, -1, mwfAllPageProtectionNone, mwfAllPageLevelNone, mwfAllPageAscending);
  for Index := Low(PageInfos) to High(PageInfos) do
    MemoResult.Lines.Add(PageInfos[Index].PageTitle);
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
  FMediaWikiApi.URL := 'http://wiki.delphi-jedi.org/api.php';
  FMediaWikiApi.UserAgent := 'MediaWiki JEDI bot';
  FMediaWikiApi.FollowRelocation := False;
  // login is not mandatory
  //FMediaWikiApi.Login()
end;

procedure TMainForm.MediaWikiAllPageContinue(Sender: TMediaWikiApi;
  const Start: string);
begin
  EditStartPage.Text := Start;
end;

procedure TMainForm.MediaWikiAllPageDone(Sender: TMediaWikiApi;
  const PageInfos: TMediaWikiAllPageInfos);
var
  Index: Integer;
begin
  for Index := Low(PageInfos) to High(PageInfos) do
    MemoResult.Lines.Add(PageInfos[Index].PageTitle);
end;

end.

