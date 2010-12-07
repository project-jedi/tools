unit QueryAllCategoriesMainUnit;

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
    EditMaxCategories: TEdit;
    LabelMaxCategories: TLabel;
    EditStartCategory: TEdit;
    LabelStartCategory: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiAllCategorieDone(Sender: TMediaWikiApi; CategorieInfos: TStrings);
    procedure MediaWikiAllCategorieContinue(Sender: TMediaWikiApi; const Start: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryAllCategoryInfoDone := MediaWikiAllCategorieDone;
  FMediaWikiApi.OnQueryAllCategoryInfoContinue := MediaWikiAllCategorieContinue;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryAllCategoryInfoAsync(EditStartCategory.Text, '', StrToInt(EditMaxCategories.Text), []);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryAllCategoryInfoDone := nil;
  FMediaWikiApi.OnQueryAllCategoryInfoContinue := nil;
  FMediaWikiApi.QueryAllCategoryInfo(MemoResult.Lines, EditStartCategory.Text, '', StrToInt(EditMaxCategories.Text), []);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Action = caFree then
  begin
    // logout is not required
    //FMediaWikiApi.Logout;
    FMediaWikiApi.Free;
  end;
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

procedure TMainForm.MediaWikiAllCategorieContinue(Sender: TMediaWikiApi;
  const Start: string);
begin
  EditStartCategory.Text := Start;
end;

procedure TMainForm.MediaWikiAllCategorieDone(Sender: TMediaWikiApi;
  CategorieInfos: TStrings);
begin
  MemoResult.Lines.Assign(CategorieInfos);
end;

end.

