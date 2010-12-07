program QueryAllCategoriesProject;

uses
  Forms,
  QueryAllCategoriesMainUnit in 'QueryAllCategoriesMainUnit.pas' {MainForm},
  MediaWikiApi in '..\MediaWikiApi.pas',
  MediaWikiUtils in '..\MediaWikiUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

