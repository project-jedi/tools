unit QueryCategoryMembersMainUnit;

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
    EditMaxCategoryMembers: TEdit;
    LabelMaxCategoryMembers: TLabel;
    EditStartCategoryMember: TEdit;
    LabelStartCategoryMember: TLabel;
    EditCategory: TEdit;
    LabelCategory: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiCategoryMemberDone(Sender: TMediaWikiApi; const CategoryMemberInfos: TMediaWikiCategoryMemberInfos;
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
  FMediaWikiApi.OnQueryCategoryMemberInfoDone := MediaWikiCategoryMemberDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryCategoryMemberInfoAsync(EditCategory.Text, FContinueInfo, -1, 0.0, 0.0, '', '', StrToInt(EditMaxCategoryMembers.Text), []);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  CategoryMemberInfos: TMediaWikiCategoryMemberInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryCategoryMemberInfoDone := nil;
  FMediaWikiApi.QueryCategoryMemberInfo(EditCategory.Text, CategoryMemberInfos, FContinueInfo, -1, 0.0, 0.0, '', '', StrToInt(EditMaxCategoryMembers.Text), []);
  for Index := Low(CategoryMemberInfos) to High(CategoryMemberInfos) do
    MemoResult.Lines.Add(CategoryMemberInfos[Index].CategoryMemberPageBasics.PageTitle);
  EditStartCategoryMember.Text := FContinueInfo.ParameterValue;
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

procedure TMainForm.MediaWikiCategoryMemberDone(Sender: TMediaWikiApi;
  const CategoryMemberInfos: TMediaWikiCategoryMemberInfos; const ContinueInfo: TMediaWikiContinueInfo);
var
  Index: Integer;
begin
  for Index := Low(CategoryMemberInfos) to High(CategoryMemberInfos) do
    MemoResult.Lines.Add(CategoryMemberInfos[Index].CategoryMemberPageBasics.PageTitle);
  FContinueInfo := ContinueInfo;
  EditStartCategoryMember.Text := FContinueInfo.ParameterValue;
end;

end.

