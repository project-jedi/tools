program QuerySiteInfoNamespacesProject;

uses
  Forms,
  MediaWikiApi in '..\MediaWikiApi\MediaWikiApi.pas',
  MediaWikiUtils in '..\MediaWikiApi\MediaWikiUtils.pas',
  QuerySiteInfoNamespacesMainUnit in 'QuerySiteInfoNamespacesMainUnit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

