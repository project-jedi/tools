unit UploadMainUnit;

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
    EditSummary: TEdit;
    LabelSummary: TLabel;
    EditToken: TEdit;
    LabelEditToken: TLabel;
    EditPage: TEdit;
    LabelPage: TLabel;
    EditFileName: TEdit;
    LabelFileName: TLabel;
    ButtonUploadAsync: TButton;
    ButtonUploadSync: TButton;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonUploadAsyncClick(Sender: TObject);
    procedure ButtonUploadSyncClick(Sender: TObject);
    procedure EditFileNameClick(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    FFileStream: TFileStream;
    procedure MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi; const PageInfos: TMediaWikiPageInfos);
    procedure MediaWikiUploadDone(Sender: TMediaWikiApi; const UploadInfo: TMediaWikiUploadInfo);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonUploadAsyncClick(Sender: TObject);
begin
  FreeAndNil(FFileStream);
  FFileStream := TFileStream.Create(EditFileName.Text, fmOpenRead);
  FMediaWikiApi.OnUploadDone := MediaWikiUploadDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.UploadAsync(ExtractFileName(EditFileName.Text), EditSummary.Text, '', EditToken.Text, [], FFileStream, '');
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonUploadSyncClick(Sender: TObject);
var
  UploadInfo: TMediaWikiUploadInfo;
begin
  FreeAndNil(FFileStream);
  FFileStream := TFileStream.Create(EditFileName.Text, fmOpenRead);
  try
    FMediaWikiApi.OnUploadDone := nil;
    FMediaWikiApi.Upload(ExtractFileName(EditFileName.Text), EditSummary.Text, '', EditToken.Text, [], FFileStream, '', UploadInfo);
    if not UploadInfo.UploadSuccess then
      raise Exception.Create('Upload failure');
  finally
    FreeAndNil(FFileStream);
  end;
end;

procedure TMainForm.EditFileNameClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    EditFileName.Text := OpenDialog1.FileName;
end;

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageInfoDone := MediaWikiQueryPageInfoDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageInfoAsync(EditPage.Text, False, [mwfIncludeEditToken]);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  PageInfos: TMediaWikiPageInfos;
begin
  MemoResult.Lines.Clear;

  FMediaWikiApi.OnQueryPageInfoDone := nil;
  FMediaWikiApi.QueryPageInfo(EditPage.Text, False, [mwfIncludeEditToken], PageInfos);
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');
  EditPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageEditToken;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Action = caFree then
  begin
    FreeAndNil(FFileStream);
    // logout is required
    FMediaWikiApi.Logout;
    FMediaWikiApi.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  LoginName, LoginPassword: string;
begin
  FMediaWikiApi := TMediaWikiApi.Create;
  FMediaWikiApi.URL := 'http://wiki.delphi-jedi.org/api.php';
  FMediaWikiApi.UserAgent := 'MediaWiki JEDI bot';
  FMediaWikiApi.FollowRelocation := False;
  // login is mandatory
  InputQuery('Login information', 'login name', LoginName);
  InputQuery('Login information', 'login password', LoginPassword);
  FMediaWikiApi.Login(LoginName, LoginPassword, True);
end;

procedure TMainForm.MediaWikiUploadDone(Sender: TMediaWikiApi;
  const UploadInfo: TMediaWikiUploadInfo);
begin
  if not UploadInfo.UploadSuccess then
    raise Exception.Create('failure');
end;

procedure TMainForm.MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi;
  const PageInfos: TMediaWikiPageInfos);
begin
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');

  EditPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageEditToken;
end;

end.

