program Merge3Demo;

uses
  Forms,
  SimpleDiff in '..\SimpleDiff.pas',
  Merge3MainUnit in 'Merge3MainUnit.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

