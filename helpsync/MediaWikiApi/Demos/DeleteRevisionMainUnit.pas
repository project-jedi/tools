unit DeleteRevisionMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  MediaWikiUtils,
  MediaWikiApi;

type
  TMainForm = class(TForm)
    MemoResult: TMemo;
    ButtonQueryPageAsync: TButton;
    ButtonQueryPageSync: TButton;
    EditToken: TEdit;
    LabelToken: TLabel;
    EditPage: TEdit;
    LabelPage: TLabel;
    EditReason: TEdit;
    LabelReason: TLabel;
    ButtonDeleteRevisionAsync: TButton;
    ButtonDeleteRevisionSync: TButton;
    EditRevision: TEdit;
    EditNextRevision: TEdit;
    LabelRevision: TLabel;
    LabelNextRevision: TLabel;
    ButtonQueryRevisionAsync: TButton;
    ButtonQueryRevisionSync: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQueryPageSyncClick(Sender: TObject);
    procedure ButtonQueryPageAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonDeleteRevisionAsyncClick(Sender: TObject);
    procedure ButtonDeleteRevisionSyncClick(Sender: TObject);
    procedure ButtonQueryRevisionAsyncClick(Sender: TObject);
    procedure ButtonQueryRevisionSyncClick(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi; const PageInfos: TMediaWikiPageInfos);
    procedure MediaWikiQueryPageRevisionInfoDone(Sender: TMediaWikiApi; const PageRevisionInfos: TMediaWikiPageRevisionInfos; const ContinueInfo: TMediaWikiContinueInfo);
    procedure MediaWikiDeleteRevisionDone(Sender: TMediaWikiApi; const DeleteRevisionInfo: TMediaWikiDeleteRevisionInfo);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonDeleteRevisionAsyncClick(Sender: TObject);
begin
  FMediaWikiApi.OnDeleteRevisionDone := MediaWikiDeleteRevisionDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.DeleteRevisionAsync(EditPage.Text, EditToken.Text, EditReason.Text, -1, StrToInt(EditRevision.Text));
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonDeleteRevisionSyncClick(Sender: TObject);
var
  DeleteRevisionInfo: TMediaWikiDeleteRevisionInfo;
begin
  FMediaWikiApi.OnDeleteRevisionDone := nil;
  FMediaWikiApi.DeleteRevision(EditPage.Text, EditToken.Text, EditReason.Text, -1, StrToInt(EditRevision.Text), DeleteRevisionInfo);
  if not DeleteRevisionInfo.DeleteRevisionSuccess then
    raise Exception.Create('Delete failure');
end;

procedure TMainForm.ButtonQueryPageAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageInfoDone := MediaWikiQueryPageInfoDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageInfoAsync(EditPage.Text, False, [mwfIncludeDeleteToken]);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQueryPageSyncClick(Sender: TObject);
var
  PageInfos: TMediaWikiPageInfos;
begin
  MemoResult.Lines.Clear;

  FMediaWikiApi.OnQueryPageInfoDone := nil;
  FMediaWikiApi.QueryPageInfo(EditPage.Text, False, [mwfIncludeDeleteToken], PageInfos);
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');
  EditPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageDeleteToken;
end;

procedure TMainForm.ButtonQueryRevisionAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryPageRevisionInfoDone := MediaWikiQueryPageRevisionInfoDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryPageRevisionInfoAsync(EditPage.Text,
                                           False,
                                           [mwfIncludeRevisionID, mwfIncludeRevisionFlags, mwfIncludeRevisionTimeStamp, mwfIncludeRevisionAuthor, mwfIncludeRevisionComment, mwfIncludeRevisionSize],
                                           FContinueInfo,
                                           1);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQueryRevisionSyncClick(Sender: TObject);
var
  PageRevisionInfos: TMediaWikiPageRevisionInfos;
begin
  MemoResult.Lines.Clear;

  FMediaWikiApi.OnQueryPageRevisionInfoDone := nil;
  FMediaWikiApi.QueryPageRevisionInfo(EditPage.Text,
                                      False,
                                      [mwfIncludeRevisionID, mwfIncludeRevisionFlags, mwfIncludeRevisionTimeStamp, mwfIncludeRevisionAuthor, mwfIncludeRevisionComment, mwfIncludeRevisionSize],
                                      PageRevisionInfos,
                                      FContinueInfo,
                                      1);
  if Length(PageRevisionInfos) <> 1 then
    raise Exception.Create('length error');

  EditPage.Text := PageRevisionInfos[0].PageRevisionInfoPageBasics.PageTitle;
  EditRevision.Text := IntToStr(PageRevisionInfos[0].PageRevisionInfoID);
  EditNextRevision.Text := FContinueInfo.ParameterValue;

  MemoResult.Lines.Clear;
  MemoResult.Lines.Add('RevisionID = ' + IntToStr(PageRevisionInfos[0].PageRevisionInfoID));
  MemoResult.Lines.Add('Date = ' + DateTimeToStr(PageRevisionInfos[0].PageRevisionInfoDateTime));
  MemoResult.Lines.Add('Author = ' + PageRevisionInfos[0].PageRevisionInfoAuthor);
  MemoResult.Lines.Add('Comment = ' + PageRevisionInfos[0].PageRevisionInfoComment);
  MemoResult.Lines.Add('Size = ' + IntToStr(PageRevisionInfos[0].PageRevisionInfoSize));
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  // logout is required
  FMediaWikiApi.Logout;
  FMediaWikiApi.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  LoginName, LoginPassword: string;
begin
  FMediaWikiApi := TMediaWikiApi.Create;
  FMediaWikiApi.HttpCli.URL := 'http://wiki.delphi-jedi.org/w/api.php';
  FMediaWikiApi.HttpCli.Agent := 'MediaWiki JEDI bot';
  FMediaWikiApi.HttpCli.FollowRelocation := False;
  // login is mandatory
  InputQuery('Login information', 'login name', LoginName);
  InputQuery('Login information', 'login password', LoginPassword);
  FMediaWikiApi.Login(LoginName, LoginPassword, True);
end;

procedure TMainForm.MediaWikiDeleteRevisionDone(Sender: TMediaWikiApi;
  const DeleteRevisionInfo: TMediaWikiDeleteRevisionInfo);
begin
  if not DeleteRevisionInfo.DeleteRevisionSuccess then
    raise Exception.Create('failure');
end;

procedure TMainForm.MediaWikiQueryPageInfoDone(Sender: TMediaWikiApi;
  const PageInfos: TMediaWikiPageInfos);
begin
  if Length(PageInfos) <> 1 then
    raise Exception.Create('length error');

  EditPage.Text := PageInfos[0].PageBasics.PageTitle;
  EditToken.Text := PageInfos[0].PageDeleteToken;
end;

procedure TMainForm.MediaWikiQueryPageRevisionInfoDone(Sender: TMediaWikiApi;
  const PageRevisionInfos: TMediaWikiPageRevisionInfos;
  const ContinueInfo: TMediaWikiContinueInfo);
begin
  FContinueInfo := ContinueInfo;
  
  if Length(PageRevisionInfos) <> 1 then
    raise Exception.Create('length error');

  EditPage.Text := PageRevisionInfos[0].PageRevisionInfoPageBasics.PageTitle;
  EditRevision.Text := IntToStr(PageRevisionInfos[0].PageRevisionInfoID);
  EditNextRevision.Text := ContinueInfo.ParameterValue;

  MemoResult.Lines.Clear;
  MemoResult.Lines.Add('RevisionID = ' + IntToStr(PageRevisionInfos[0].PageRevisionInfoID));
  MemoResult.Lines.Add('Date = ' + DateTimeToStr(PageRevisionInfos[0].PageRevisionInfoDateTime));
  MemoResult.Lines.Add('Author = ' + PageRevisionInfos[0].PageRevisionInfoAuthor);
  MemoResult.Lines.Add('Comment = ' + PageRevisionInfos[0].PageRevisionInfoComment);
  MemoResult.Lines.Add('Size = ' + IntToStr(PageRevisionInfos[0].PageRevisionInfoSize));
end;

end.

