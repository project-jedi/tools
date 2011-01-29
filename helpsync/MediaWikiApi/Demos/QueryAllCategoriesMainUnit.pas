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
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiAllCategorieDone(Sender: TMediaWikiApi; CategorieInfos: TStrings;
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
  FMediaWikiApi.OnQueryAllCategoryInfoDone := MediaWikiAllCategorieDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryAllCategoryInfoAsync(FContinueInfo, '', StrToInt(EditMaxCategories.Text), []);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryAllCategoryInfoDone := nil;
  FMediaWikiApi.QueryAllCategoryInfo(MemoResult.Lines, FContinueInfo, '', StrToInt(EditMaxCategories.Text), []);
  EditStartCategory.Text := FContinueInfo.ParameterValue;
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

procedure TMainForm.MediaWikiAllCategorieDone(Sender: TMediaWikiApi;
  CategorieInfos: TStrings; const ContinueInfo: TMediaWikiContinueInfo);
begin
  MemoResult.Lines.Assign(CategorieInfos);
  FContinueInfo := ContinueInfo;
  EditStartCategory.Text := FContinueInfo.ParameterValue;
end;

end.

