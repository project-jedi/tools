program MergeDemo;

uses
  Forms,
  MergeMainUnit in 'MergeMainUnit.pas' {FormMain},
  SimpleDiff in '..\SimpleDiff.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

