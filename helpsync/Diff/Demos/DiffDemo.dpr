program DiffDemo;

uses
  Forms,
  UMain in 'UMain.pas' {Form1},
  SimpleDiff in '..\SimpleDiff.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

