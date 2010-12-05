program QueryAllUsersProject;

uses
  Forms,
  QueryAllUsersMainUnit in 'QueryAllUsersMainUnit.pas' {MainForm},
  MediaWikiApi in '..\MediaWikiApi\MediaWikiApi.pas',
  MediaWikiUtils in '..\MediaWikiApi\MediaWikiUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

