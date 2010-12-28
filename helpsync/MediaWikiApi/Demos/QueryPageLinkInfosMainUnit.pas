unit QueryPageLinkInfosMainUnit;

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
    procedure MediaWikiAllPageLinkDone(Sender: TMediaWikiApi; const PageLinkInfos: TMediaWikiPageLinkInfos);
    procedure MediaWikiAllPageLinkContinue(Sender: TMediaWikiApi; const Start: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageLinkInfoDone := MediaWikiAllPageLinkDone;
  FMediaWikiApi.OnQueryPageLinkInfoContinue := MediaWikiAllPageLinkContinue;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageLinkInfoAsync(EditPage.Text, False, StrToInt(EditMaxLinks.Text), -1, EditStartLink.Text);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  LinkInfos: TMediaWikiPageLinkInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageLinkInfoDone := nil;
  FMediaWikiApi.OnQueryPageLinkInfoContinue := nil;
  FMediaWikiApi.QueryPageLinkInfo(EditPage.Text, False, LinkInfos, StrToInt(EditMaxLinks.Text), -1, EditStartLink.Text);
  for Index := Low(LinkInfos) to High(LinkInfos) do
  begin
    MemoResult.Lines.Add('source page title = ' + LinkInfos[Index].LinkSourceBasics.PageTitle);
    MemoResult.Lines.Add('source page ID = ' + IntToStr(LinkInfos[Index].LinkSourceBasics.PageID));
    MemoResult.Lines.Add('source page namespace = ' + IntToStr(LinkInfos[Index].LinkSourceBasics.PageNamespace));
    MemoResult.Lines.Add('target page title = ' + LinkInfos[Index].LinkTargetTitle);
    MemoResult.Lines.Add('target page namespace = ' + IntToStr(LinkInfos[Index].LinkTargetNameSpace));
    MemoResult.Lines.Add('');
  end;
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

procedure TMainForm.MediaWikiAllPageLinkContinue(Sender: TMediaWikiApi;
  const Start: string);
begin
  EditStartLink.Text := Start;
end;

procedure TMainForm.MediaWikiAllPageLinkDone(Sender: TMediaWikiApi;
  const PageLinkInfos: TMediaWikiPageLinkInfos);
var
  Index: Integer;
begin
  for Index := Low(PageLinkInfos) to High(PageLinkInfos) do
  begin
    MemoResult.Lines.Add('source page title = ' + PageLinkInfos[Index].LinkSourceBasics.PageTitle);
    MemoResult.Lines.Add('source page ID = ' + IntToStr(PageLinkInfos[Index].LinkSourceBasics.PageID));
    MemoResult.Lines.Add('source page namespace = ' + IntToStr(PageLinkInfos[Index].LinkSourceBasics.PageNamespace));
    MemoResult.Lines.Add('target page title = ' + PageLinkInfos[Index].LinkTargetTitle);
    MemoResult.Lines.Add('target page namespace = ' + IntToStr(PageLinkInfos[Index].LinkTargetNameSpace));
    MemoResult.Lines.Add('');
  end;
end;

end.

