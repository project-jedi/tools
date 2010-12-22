program MergeAfterDemo;

uses
  Forms,
  SimpleDiff in '..\SimpleDiff.pas',
  MergeAfterMainUnit in 'MergeAfterMainUnit.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

