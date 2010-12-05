program QuerySiteInfoNamespaceAliasesProject;

uses
  Forms,
  QuerySiteInfoNamespaceAliasesMainUnit in 'QuerySiteInfoNamespaceAliasesMainUnit.pas' {MainForm},
  MediaWikiUtils in '..\MediaWikiApi\MediaWikiUtils.pas',
  MediaWikiApi in '..\MediaWikiApi\MediaWikiApi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

