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
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiAllPageDone(Sender: TMediaWikiApi; const PageInfos: TMediaWikiAllPageInfos;
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
  FMediaWikiApi.OnQueryAllPageInfoDone := MediaWikiAllPageDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryAllPageInfoAsync(FContinueInfo, '', StrToInt(EditMaxPages.Text),
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
  FMediaWikiApi.QueryAllPageInfo(PageInfos, FContinueInfo, '', StrToInt(EditMaxPages.Text),
    -1, mwfAllPageFilterAll, mwfAllPageLangAll, -1, -1, mwfAllPageProtectionNone, mwfAllPageLevelNone, mwfAllPageAscending);
  for Index := Low(PageInfos) to High(PageInfos) do
    MemoResult.Lines.Add(PageInfos[Index].PageTitle);
  EditStartPage.Text := FContinueInfo.ParameterValue;
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

procedure TMainForm.MediaWikiAllPageDone(Sender: TMediaWikiApi;
  const PageInfos: TMediaWikiAllPageInfos; const ContinueInfo: TMediaWikiContinueInfo);
var
  Index: Integer;
begin
  for Index := Low(PageInfos) to High(PageInfos) do
    MemoResult.Lines.Add(PageInfos[Index].PageTitle);
  FContinueInfo := ContinueInfo;
  EditStartPage.Text := ContinueInfo.ParameterValue;
end;

end.

