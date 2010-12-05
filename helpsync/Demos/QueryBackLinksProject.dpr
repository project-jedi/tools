program QueryBackLinksProject;

uses
  Forms,
  QueryBackLinksMainUnit in 'QueryBackLinksMainUnit.pas' {MainForm},
  MediaWikiUtils in '..\MediaWikiApi\MediaWikiUtils.pas',
  MediaWikiApi in '..\MediaWikiApi\MediaWikiApi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

