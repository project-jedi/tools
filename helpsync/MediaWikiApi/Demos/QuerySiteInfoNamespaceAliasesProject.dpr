program QuerySiteInfoNamespaceAliasesProject;

uses
  Forms,
  QuerySiteInfoNamespaceAliasesMainUnit in 'QuerySiteInfoNamespaceAliasesMainUnit.pas' {MainForm},
  MediaWikiUtils in '..\MediaWikiUtils.pas',
  MediaWikiApi in '..\MediaWikiApi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

