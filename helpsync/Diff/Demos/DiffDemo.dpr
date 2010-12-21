program DiffDemo;

uses
  Forms,
  SimpleDiff in '..\SimpleDiff.pas',
  DiffMainUnit in 'DiffMainUnit.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

