unit QueryPageExternalLinkInfosMainUnit;

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
    EditPage: TEdit;
    LabelPage: TLabel;
    LabelStartLink: TLabel;
    EditStartLink: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiAllPageExternalLinkDone(Sender: TMediaWikiApi; const PageExternalLinkInfos: TMediaWikiPageExtLinkInfos;
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
  FMediaWikiApi.OnQueryPageExtLinkInfoDone := MediaWikiAllPageExternalLinkDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageExtLinkInfoAsync(EditPage.Text, False, FContinueInfo, StrToInt(EditMaxLinks.Text));
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  ExternalLinkInfos: TMediaWikiPageExtLinkInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageLinkInfoDone := nil;
  FMediaWikiApi.QueryPageExtLinkInfo(EditPage.Text, False, ExternalLinkInfos, FContinueInfo, StrToInt(EditMaxLinks.Text));
  for Index := Low(ExternalLinkInfos) to High(ExternalLinkInfos) do
  begin
    MemoResult.Lines.Add('source page title = ' + ExternalLinkInfos[Index].ExtLinkPageBasics.PageTitle);
    MemoResult.Lines.Add('source page ID = ' + IntToStr(ExternalLinkInfos[Index].ExtLinkPageBasics.PageID));
    MemoResult.Lines.Add('source page namespace = ' + IntToStr(ExternalLinkInfos[Index].ExtLinkPageBasics.PageNamespace));
    MemoResult.Lines.Add('target = ' + ExternalLinkInfos[Index].ExtLinkTarget);
    MemoResult.Lines.Add('');
  end;
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
  FMediaWikiApi.HttpCli.URL := 'http://wiki.delphi-jedi.org/w/api.php';
  FMediaWikiApi.HttpCli.Agent := 'MediaWiki JEDI bot';
  FMediaWikiApi.HttpCli.FollowRelocation := False;
  // login is not mandatory
  //FMediaWikiApi.Login()
end;

procedure TMainForm.MediaWikiAllPageExternalLinkDone(Sender: TMediaWikiApi;
  const PageExternalLinkInfos: TMediaWikiPageExtLinkInfos; const ContinueInfo: TMediaWikiContinueInfo);
var
  Index: Integer;
begin
  for Index := Low(PageExternalLinkInfos) to High(PageExternalLinkInfos) do
  begin
    MemoResult.Lines.Add('source page title = ' + PageExternalLinkInfos[Index].ExtLinkPageBasics.PageTitle);
    MemoResult.Lines.Add('source page ID = ' + IntToStr(PageExternalLinkInfos[Index].ExtLinkPageBasics.PageID));
    MemoResult.Lines.Add('source page namespace = ' + IntToStr(PageExternalLinkInfos[Index].ExtLinkPageBasics.PageNamespace));
    MemoResult.Lines.Add('target = ' + PageExternalLinkInfos[Index].ExtLinkTarget);
    MemoResult.Lines.Add('');
  end;
  FContinueInfo := ContinueInfo;
  EditStartLink.Text := FContinueInfo.ParameterValue;
end;

end.

