program QueryPageExternalLinkInfosProject;

uses
  Forms,
  QueryPageExternalLinkInfosMainUnit in 'QueryPageExternalLinkInfosMainUnit.pas' {MainForm},
  MediaWikiUtils in '..\MediaWikiUtils.pas',
  MediaWikiApi in '..\MediaWikiApi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

