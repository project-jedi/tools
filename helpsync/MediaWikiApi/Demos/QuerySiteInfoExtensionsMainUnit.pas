unit QuerySiteInfoExtensionsMainUnit;

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
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiSiteInfoExtensionsDone(Sender: TMediaWikiApi; const Infos: TMediaWikiExtensions);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoExtensionsDone := MediaWikiSiteInfoExtensionsDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QuerySiteInfoExtensionsAsync;
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  Infos: TMediaWikiExtensions;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQuerySiteInfoExtensionsDone := nil;
  FMediaWikiApi.QuerySiteInfoExtensions(Infos);
  for Index := Low(Infos) to High(Infos) do
  begin
    MemoResult.Lines.Add('Type = ' + Infos[Index].ExtensionType);
    MemoResult.Lines.Add('Name = ' + Infos[Index].ExtensionName);
    MemoResult.Lines.Add('Description = ' + Infos[Index].ExtensionDescription);
    MemoResult.Lines.Add('Description message = ' + Infos[Index].ExtensionDescriptionMsg);
    MemoResult.Lines.Add('Author = ' + Infos[Index].ExtensionAuthor);
    MemoResult.Lines.Add('Version = ' + Infos[Index].ExtensionVersion);
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
  FMediaWikiApi.URL := 'http://wiki.delphi-jedi.org/w/api.php';
  FMediaWikiApi.UserAgent := 'MediaWiki JEDI bot';
  FMediaWikiApi.FollowRelocation := False;
  // login is not mandatory
  //FMediaWikiApi.Login()
end;

procedure TMainForm.MediaWikiSiteInfoExtensionsDone(Sender: TMediaWikiApi; const Infos: TMediaWikiExtensions);
var
  Index: Integer;
begin
  for Index := Low(Infos) to High(Infos) do
  begin
    MemoResult.Lines.Add('Type = ' + Infos[Index].ExtensionType);
    MemoResult.Lines.Add('Name = ' + Infos[Index].ExtensionName);
    MemoResult.Lines.Add('Description = ' + Infos[Index].ExtensionDescription);
    MemoResult.Lines.Add('Description message = ' + Infos[Index].ExtensionDescriptionMsg);
    MemoResult.Lines.Add('Author = ' + Infos[Index].ExtensionAuthor);
    MemoResult.Lines.Add('Version = ' + Infos[Index].ExtensionVersion);
    MemoResult.Lines.Add('');
  end;
end;

end.

